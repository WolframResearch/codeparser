//! Parser types and customization.
//!
//! This module providesj:
//!
//! * The enums of the [`operators`] module
//! * The [`ParseBuilder`] trait, used to implement custom parsing output.
//!
//! ###### General Terminology
//!
//! * **token** — any atomic piece of input
//!
//!   The [`TokenKind`] enum defines the set of tokens used by this parser.
//!
//!   Tokens are further broken down into three classes:
//!
//!   - **trivia token** — any token that doesn't semantically effect parsing.
//!
//!     Examples: ` ` (whitespace), `(* comments *)`
//!
//!   - **syntax token** — any token whose presence semantically affects
//!     parsing, but which does not have a meaningful intrinsic value.
//!
//!     Examples: `{`, `&`, `+`, `->`, `#` in `#2`, ...
//!
//!   - **operand token** — any token that on its own can be an operand to
//!     a parsed expression (including error tokens, and implicit tokens).
//!
//!     *Examples:* `3`, `4.2`, `foo`, `"hello"`, `2` in `#2`, ...
//!
//! ###### Parser Implementation Documentation
//!
//! To view the internal parser implementation documentation, build the
//! documentation locally with:
//!
//! ```shell
//! $ cargo doc -p wolfram-parser --document-private-items --open
//! ```
//!
//! and navigate to the [`wolfram_parser::parse::parser_docs`][self::parser_docs] module.

//
// Main internal parser documentation.
//
// Build with `cargo doc --document-private-items` to view this documentation.
//
pub(crate) mod parser_docs;

pub mod operators;

pub(crate) mod parselet;
pub(crate) mod token_parselets;


#[cfg(test)]
mod parse_tests {
    mod test_parselet;
}


use std::fmt::Debug;

use crate::{
    create_parse_result,
    cst::TriviaSeq,
    feature, panic_if_aborted,
    parse::parselet::PrefixToplevelCloserParselet,
    precedence::Precedence,
    quirks,
    tokenize::{
        token_kind::Closer, tokenizer::Tokenizer_currentToken_stringifyAsFile,
        TokenKind, TokenRef, TokenStr, Tokenizer,
    },
    ParseOptions, ParseResult, QuirkSettings,
};

// Import types used only in doc comments in this module.
#[allow(unused_imports)]
use crate::cst::Cst;

use self::{
    operators::{
        BinaryOperator, CompoundOperator, GroupOperator, InfixOperator,
        PostfixOperator, PrefixBinaryOperator, PrefixOperator, TernaryOperator,
    },
    parselet::{InfixParselet, PrefixParselet},
};

//======================================
// API
//======================================

/// Parse Wolfram Language input using the specified [`ParseBuilder`].
pub(crate) fn parse<'i, B: ParseBuilder<'i> + 'i>(
    input: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<B::Output> {
    let builder: B = B::new_builder();

    let (builder, result): (B, ParseResult<()>) =
        do_parse(input, builder, opts);

    let exprs = builder.finish(input, opts);

    ParseResult {
        syntax: exprs,
        unsafe_character_encoding: result.unsafe_character_encoding,
        fatal_issues: result.fatal_issues,
        non_fatal_issues: result.non_fatal_issues,
        tracked: result.tracked,
    }
}

fn do_parse<'i, B: ParseBuilder<'i> + 'i>(
    input: &'i [u8],
    builder: B,
    opts: &ParseOptions,
) -> (B, ParseResult<()>) {
    let mut session = ParserSession::new(&*input, builder, opts);

    quirks::set_quirks(session.quirk_settings);

    #[cfg(feature = "DIAGNOSTICS")]
    {
        DiagnosticsLog("enter parseExpressions");
        DiagnosticsMarkTime();
    }

    //
    // Collect all expressions
    //

    loop {
        if feature::CHECK_ABORT && crate::abortQ() {
            break;
        }

        let peek: TokenRef = session.tokenizer.peek_token();

        if peek.tok == TokenKind::EndOfFile {
            break;
        }

        if peek.tok.isTrivia() {
            session.builder.finish_top_level_trivia(peek);

            peek.skip(&mut session.tokenizer);

            continue;
        }

        //
        // special top-level handling of stray closers
        //
        if peek.tok.isCloser() {
            let node = (PrefixToplevelCloserParselet {})
                .parse_prefix(&mut session, peek);

            session.builder.finish_top_level_expr(node);

            assert!(session.is_quiescent());

            continue;
        }

        let node = session.parse_prefix(peek);

        session.builder.finish_top_level_expr(node);

        assert!(session.is_quiescent());
    } // while (true)

    #[cfg(feature = "DIAGNOSTICS")]
    {
        DiagnosticsLog("exit parseExpressions");
        DiagnosticsLogTime();
    }

    return (session.builder, create_parse_result(&session.tokenizer, ()));
}


//======================================
// Types
//======================================

/// A parser session
#[derive(Debug)]
pub(crate) struct ParserSession<'i, B: ParseBuilder<'i> + 'i> {
    tokenizer: Tokenizer<'i>,

    builder: B,

    context_stack: Vec<Context<B::ContextData>>,

    quirk_settings: QuirkSettings,
}

#[derive(Debug)]
pub(crate) struct Context<D> {
    pub(crate) prec: Option<Precedence>,

    /// Data stored in this context by the current [`ParseBuilder`]
    /// implementation.
    builder_data: D,
}

//
// Used mainly for collecting trivia that has been eaten
//
pub(crate) type TriviaSeqRef<'i> = TriviaSeq<TokenStr<'i>>;


/// Handler for parse events to build up a representation of the parsed input.
///
/// Types that implement this trait receive parsing "events" in the form of
/// calls to the methods defined by this trait, and use those events to build up
/// some representation of the parsed input, typically a [`Cst`] or Wolfram
/// Language expression.
///
/// **None of the methods on this trait should ever be called directly by code
/// outside of wolfram-parser.** To initiate parsing using a [`ParseBuilder`]
/// implementation, call [`wolfram_parser::parse::parse()`][crate::parse::parse].
///
/// # Background
///
/// Prior to the introduction of this trait, the parser could only produce
/// [`Cst`] values as output. However, that limited the utility of this parser
/// for the performant construction of Wolfram Language expressions, because
/// the required pipeline was `Input &str => Cst => Expr` — the intermediate
/// `Cst` and all its allocations and overhead was superfluous to the ultimate
/// goal of creating an `Expr`.
///
/// This abstraction allows `Input &str => Expr` with no intermediate stage,
/// while also preserving the ability to do `Input &str => Cst`, or any other
/// type buildable from parsing.
///
/// # API Design
///
/// ## Method Overview
///
/// * **Node Construction:** [Push and Reduce Methods](#push-and-reduce-methods)
///   - [`push_leaf()`][ParseBuilder::push_leaf]
///   - [`push_syntax()`][ParseBuilder::push_syntax]
///   - [`push_compound_blank()`][ParseBuilder::push_compound_blank]
///   - [`push_compound_pattern_blank()`][ParseBuilder::push_compound_pattern_blank]
///   - [`push_compound_pattern_optional()`][ParseBuilder::push_compound_pattern_optional]
///   - [`push_compound_slot()`][ParseBuilder::push_compound_slot]
///   - [`push_compound_out()`][ParseBuilder::push_compound_out]
///   - [`reduce_prefix()`][ParseBuilder::reduce_prefix]
///   - [`reduce_prefix_get()`][ParseBuilder::reduce_prefix_get]
///   - [`reduce_postfix()`][ParseBuilder::reduce_postfix]
///   - [`reduce_binary()`][ParseBuilder::reduce_binary]
///   - [`reduce_binary_unset()`][ParseBuilder::reduce_binary_unset]
///   - [`reduce_ternary()`][ParseBuilder::reduce_ternary]
///   - [`reduce_ternary_tag_unset()`][ParseBuilder::reduce_ternary_tag_unset]
///   - [`reduce_infix()`][ParseBuilder::reduce_infix]
///     * [`begin_infix()`][ParseBuilder::begin_infix]
///     * [`infix_add()`][ParseBuilder::infix_add]
///   - [`reduce_group()`][ParseBuilder::reduce_group]
///     * [`begin_group()`][ParseBuilder::begin_group]
///     * [`group_add()`][ParseBuilder::group_add]
///   - [`reduce_call()`][ParseBuilder::reduce_call]
///
///   Errors:
///
///   - [`reduce_syntax_error()`][ParseBuilder::reduce_syntax_error]
///   - [`reduce_unterminated_group()`][ParseBuilder::reduce_unterminated_group]
///   - [`reduce_group_missing_closer()`][ParseBuilder::reduce_group_missing_closer]
///
/// * **Trivia token management:**
///
///   - [`resettable_trivia_begin()`][ParseBuilder::resettable_trivia_begin]
///   - [`resettable_trivia_push()`][ParseBuilder::resettable_trivia_push]
///   - [`resettable_trivia_end()`][ParseBuilder::resettable_trivia_end]
///   - [`reset_trivia()`][ParseBuilder::reset_trivia_seq]
///   - [`push_trivia_seq()`][ParseBuilder::push_trivia_seq]
///
///
/// ## Push and Reduce Methods
///
/// This section describes the `push_*()` and `reduce_*()` methods of the
/// [`ParseBuilder`] trait. These methods are the primary way the parser
/// transforms parsed input into specific output types like [`Cst`] or an
/// expression.
///
///
///
/// ### Push Methods
///
/// This parser is designed for producing tree-like parsed output. At the leaves
/// of those parse trees are the nodes created by the `push_*()` family of
/// methods. A **`push_*()` method** is called by a parselet when it finishes
/// parsing some "atomic" piece of input.
///
/// Examples of parser "atomic" input include literally AtomQ input like `foo` or
/// `3.78`, as well as simple compound forms like `head_` or `#7`. These forms
/// are considered parser atomic because their parsing is not affected by
/// precedence, and they cannot contain interior whitespace.
///
/// | Sample Input     | Push method                                    | Comment
/// |------------------|------------------------------------------------|------------
/// | `foo`, `3.7`     | [`ParseBuilder::push_leaf`]                    |
/// | `_Integer`       | [`ParseBuilder::push_compound_blank`]          | Unnamed Blank pattern
/// | `expr_`, `f_Foo` | [`ParseBuilder::push_compound_pattern_blank`]  | Named Blank pattern
///
/// ### Reduce Methods
///
/// Reduce methods are closely linked with parser contexts: a parser context
/// ends when a `reduce_*()` method call is made, combining the nodes "owned" by
/// the top-most parser context into a single new node. This means that the
/// context begun by a call to [`begin_context()`][ParseBuilder::begin_context]
/// always ends with a corresponding call to a `reduce_*()` method.
///
/// Typically an individual parser context is managed by a single parselet
/// implementation, which recursively parses any expected arguments or
/// additional required syntax. Once all required arguments and syntax are
/// accounted for, the typical parselet implementation then ends by calling a
/// `reduce_*()` method to emit a new node.
///
/// In this way parsed subexpressions are composed into higher-level parent
/// expressions by the interaction of parser contexts and reduce methods.
///
/// | Sample Input | Reduce method                   | Comment
/// |--------------|---------------------------------|--------
/// | `!x`         | [`ParseBuilder::reduce_prefix`] | Takes three args: `!` token, whitespace, and `x` node
/// | `a -> b`     | [`ParseBuilder::reduce_binary`] |
/// | `a + b + c`  | [`ParseBuilder::reduce_infix`]  | Takes arbitrary number of args; see also [`ParseBuilder::begin_infix`]
/// | `f[...]`     | [`ParseBuilder::reduce_call`]   |
/// | `{...}`      | [`ParseBuilder::reduce_group`]  |
///
///
///
/// ## Optimizing parsing
///
/// [`ParseBuilder`] is designed to allow the implementor very fine-grained
/// control what information made available by the parser is used, and what is
/// otherwise optimized way. This flexibilty allows this parser to efficiently
/// support a variety of use-cases, including:
///
/// | Use case        | Operand nodes               | [*Trivia token*][term] info | [*Syntax token*][term] info | line:column positions |
/// |-----------------|-----------------------------|-----------------------------|-----------------------------|-----------------------|
/// | Code formatting | Yes                         | Yes                         | Yes                         | Yes                   |
/// | Code linting    | Yes                         | No                          | Yes                         | Yes                   |
/// | Parsing exprs   | Yes                         | No                          | No                          | No                    |
/// | SyntaxQ         | No                          | No                          | No                          | No                    |
///
/// For each use-case, the relevant feature of the parser can be enabled or
/// disabled by setting appropriate values for the [`ParseBuilder`] associated
/// types.
///
///
///
/// ### Guarantees
///
/// The wolfram-parser implementation makes several important guarantees
/// regarding its invocation of `push_*()` methods:
///
/// * Every token appearing in the input will be passed to a push method exactly one
/// * Tokens will be passed to push methods in order
///
/// These guarantees allow more optimized [`ParseBuilder`] implementations to
/// avoid separate intermediate allocations for every node and trivia sequence,
/// and instead use a single flat intermediate buffer for staging node
/// construction (an optimization used by [`parse_cst()`][crate::parse_cst()]).
/// Implementations optimized in this way can then set their
/// [`ParseBuilder::Node`] and [`ParseBuilder::TriviaHandle`] types to [`()`] or
/// some equivalent zero-sized type, minimizing the amount of data the parser
/// moves around on the stack.
///
/// Alternatively, other [`ParseBuilder`] implementations can opt for easier
/// but slightly less efficient implementation strategy and choose to pass
/// values through the `Node` and `TriviaHandle` associated types, and
/// consequently through the parser implementation itself.
///
///
/// ### Controlling trivia tokens
///
/// When e.g. parsing input into expressions, it is valid and efficient to ignore
/// [*trivia tokens*][term]. But if the original arrangement of trivia is
/// important to know (e.g. when implementing a formatter), [`ParseBuilder`]
/// makes trivia information available.
///
/// The [`ParseBuilder`] trivia API is designed with the following constraints
/// in mind:
///
/// * There are two
///       different situations in which the parser might process trivia:
///       when it knows the current parselet is going to process the
///       next syntax token, and when it is merely peeking the next
///       syntax token to decide if it should process it or not.
///       In the former case, trivia is pushed immediately (i.e.
///       non-resettable). In the latter case, the consumption of that
///       might need to be undone.
///
/// TODO: Document that it is possible to have multiple resettable
///       trivia sequences active at one time. See e.g. SemiSemiParselet.
///
/// TODO: Document that there are two outcomes for resettable trivia:
///       pushing it ([`push_trivia_seq()`][ParseBuilder::push_trivia_seq]), and resetting it
///      ([`trivia_reset()`][ParseBuilder::reset_trivia_seq]).
///

///
/// ### Controlling syntax tokens
///
/// To enable syntax tokens, set [`ParseBuilder::SyntaxTokenNode`] to:
///
/// ```ignore
/// type SyntaxTokenNode = TokenRef<'i>;
/// ```
///
/// to disable them, set:
///
/// ```ignore
/// type SyntaxTokenNode = ();
/// ```
///
/// and define [`ParseBuilder::push_syntax()`] to be empty.
///
/// [term]: crate::parse#general-terminology
pub(crate) trait ParseBuilder<'i>: Sized + Debug
where
    Self: 'i,
{
    /// An intermediate node in the parse tree structure constructed by the
    /// parser.
    ///
    /// Nodes are typically sub-expressions of the parsed input, but may also
    /// represent e.g. syntax errors.
    type Node;

    /// A node constructed from a [*syntax token*][term] by
    /// [`push_syntax()`][ParseBuilder::push_syntax].
    ///
    /// Should typically either by:
    ///
    /// ```ignore
    /// type SyntaxTokenNode = TokenRef<'i>;
    /// ```
    ///
    /// or:
    ///
    /// ```ignore
    /// type SyntaxTokenNode = ();
    /// ```
    ///
    /// [term]: crate::parse#general-terminology
    type SyntaxTokenNode;

    type Output;

    /// Extra data returned by [`Self::begin_context()`] when a new context
    /// starts, and passed to the `reduce_*()` method call made when a parsing
    /// context completes.
    type ContextData: Debug;

    type InfixParseState: Debug;
    type GroupParseState: Debug;

    //==================================
    // Lifecycle
    //==================================

    fn new_builder() -> Self;

    /// Complete the parse and return the parsed output.
    fn finish(self, input: &'i [u8], opts: &ParseOptions) -> Self::Output;

    /// Apply the [`PrefixParselet`] implementation associated with the given
    /// [`TokenKind`].
    fn with_prefix_parselet<R, F>(kind: TokenKind, callback: F) -> R
    where
        F: FnOnce(&dyn PrefixParselet<'i, Self>) -> R;

    /// Apply the [`InfixParselet`] implementation associated with the given
    /// [`TokenKind`].
    fn with_infix_parselet<R, F: FnOnce(&dyn InfixParselet<'i, Self>) -> R>(
        kind: TokenKind,
        callback: F,
    ) -> R;

    //==================================
    // Trivia handling
    //==================================

    // TODO(cleanup): Add default impls for these trivia methods?

    type ResettableTriviaAccumulator;
    type ResettableTriviaHandle;

    type TriviaHandle;

    fn empty_trivia() -> Self::TriviaHandle;

    fn resettable_trivia_begin(&mut self) -> Self::ResettableTriviaAccumulator;

    fn resettable_trivia_push(
        &mut self,
        accum: &mut Self::ResettableTriviaAccumulator,
        trivia: TokenRef<'i>,
    );

    fn resettable_trivia_end(
        &mut self,
        accum: Self::ResettableTriviaAccumulator,
    ) -> Self::ResettableTriviaHandle;

    /// Get the first piece of trivia in a set of trivia.
    ///
    /// Used to reset the position of the underlying
    /// [`Reader`][crate::read::Reader] when a potential parse fails.
    fn reset_trivia_seq(
        &mut self,
        handle: Self::ResettableTriviaHandle,
    ) -> Option<TokenRef<'i>>;

    fn push_trivia_seq(
        &mut self,
        trivia: Self::ResettableTriviaHandle,
    ) -> Self::TriviaHandle;

    //==================================
    // Context management
    //==================================

    /// Called when the parser begins parsing a new subexpression.
    ///
    /// The [`ContextData`][Self::ContextData] value returned from this
    /// method is passed back into the
    /// [`reduce_*()` method][Self#reduce-methods] called when this context is
    /// finished parsing.
    ///
    /// Every `begin_context()` call must have a corresponding call to
    /// a `reduce_*()` method. However that is the responsibility of the
    /// wolfram-parser implementation, and not implementors of the
    /// [`ParseBuilder`] trait.
    fn begin_context(&mut self) -> Self::ContextData;

    //==================================
    // Push
    //==================================

    fn push_leaf(&mut self, token: TokenRef<'i>) -> Self::Node;

    /// Push a [*syntax token*][term].
    ///
    /// Parser implementations that want to locate non-value tokens like e.g.
    /// `[` or `+` should capture them with this method.
    ///
    /// [term]: crate::parse#general-terminology
    fn push_syntax(&mut self, token: TokenRef<'i>) -> Self::SyntaxTokenNode;

    /// `name_` or `name_head`
    fn push_compound_pattern_blank(
        &mut self,
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under: UnderParseData<'i>,
    ) -> Self::Node;

    /// `_` or `_head`
    fn push_compound_blank(&mut self, under: UnderParseData<'i>) -> Self::Node;

    fn push_compound_pattern_optional(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        // TODO(cleanup): SyntaxTokenNode here and related push_compound methods?
        under_dot: TokenRef<'i>,
    ) -> Self::Node;

    // TODO(cleanup): Same signature as push_compound_pattern_optional?
    fn push_compound_slot(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: CompoundOperator,
        hash: TokenRef<'i>,
        arg: TokenRef<'i>,
    ) -> Self::Node;

    // TODO(cleanup): Same signature as push_compound_pattern_optional and push_compound_slot?
    fn push_compound_out(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: CompoundOperator,
        percent: TokenRef<'i>,
        integer: TokenRef<'i>,
    ) -> Self::Node;

    //==================================
    // Reduce
    //==================================

    //----------------------------------
    // Reduce normal
    //----------------------------------

    fn reduce_prefix(
        &mut self,
        ctx_data: Self::ContextData,
        op: PrefixOperator,
        op_token: Self::SyntaxTokenNode,
        trivia: Self::TriviaHandle,
        operand: Self::Node,
    ) -> Self::Node;

    // TODO(cleanup): Better name
    fn reduce_prefix_get(
        &mut self,
        ctx_data: Self::ContextData,
        // TODO(cleanup): Can this only ever have one value?
        op: PrefixOperator,
        op_token: Self::SyntaxTokenNode,
        trivia: Self::TriviaHandle,
        stringify_token: TokenRef<'i>,
    ) -> Self::Node;

    fn reduce_postfix(
        &mut self,
        ctx_data: Self::ContextData,
        op: PostfixOperator,
        operand: Self::Node,
        trivia: Self::TriviaHandle,
        op_token: Self::SyntaxTokenNode,
    ) -> Self::Node;

    fn reduce_binary(
        &mut self,
        ctx_data: Self::ContextData,
        op: BinaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        op_token: Self::SyntaxTokenNode,
        trivia2: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node;

    fn reduce_binary_unset(
        &mut self,
        ctx_data: Self::ContextData,
        op: BinaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        op_token: Self::SyntaxTokenNode,
        trivia2: Self::TriviaHandle,
        dot_token: Self::SyntaxTokenNode,
    ) -> Self::Node;

    fn reduce_ternary(
        &mut self,
        ctx_data: Self::ContextData,
        op: TernaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        first_op_token: Self::SyntaxTokenNode,
        trivia2: Self::TriviaHandle,
        middle_node: Self::Node,
        trivia3: Self::TriviaHandle,
        second_op_token: Self::SyntaxTokenNode,
        trivia4: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node;

    fn reduce_ternary_tag_unset(
        &mut self,
        ctx_data: Self::ContextData,
        // TODO(cleanup): Always the same operator?
        op: TernaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        slash_colon_token: Self::SyntaxTokenNode,
        trivia2: Self::TriviaHandle,
        middle_node: Self::Node,
        trivia3: Self::TriviaHandle,
        equal_token: Self::SyntaxTokenNode,
        trivia4: Self::TriviaHandle,
        dot_token: Self::SyntaxTokenNode,
    ) -> Self::Node;

    fn reduce_prefix_binary(
        &mut self,
        ctx_data: Self::ContextData,
        op: PrefixBinaryOperator,
        prefix_op_token: Self::SyntaxTokenNode,
        trivia1: Self::TriviaHandle,
        lhs_node: Self::Node,
        trivia2: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node;

    /// Build up an infix parse.
    ///
    /// ```text
    /// a + b + c
    /// a[ + b][ + c]
    /// ```
    fn begin_infix(
        &mut self,
        op: InfixOperator,
        first_node: Self::Node,
    ) -> Self::InfixParseState;

    fn infix_add(
        &mut self,
        infix_state: &mut Self::InfixParseState,
        trivia1: Self::TriviaHandle,
        op_token: Self::SyntaxTokenNode,
        trivia2: Self::TriviaHandle,
        operand: Self::Node,
    );

    /// Parselet implementations should not call this method directly, and
    /// instead call [`ParserSession::reduce_infix()`].
    fn reduce_infix(
        &mut self,
        ctx_data: Self::ContextData,
        infix_state: Self::InfixParseState,
    ) -> Self::Node;

    /// Build up an group parse.
    ///
    /// TODO: Will only have multiple elems if there's an error?
    fn begin_group(
        &mut self,
        opener_tok: Self::SyntaxTokenNode,
    ) -> Self::GroupParseState;

    fn group_add(
        &mut self,
        group_state: &mut Self::GroupParseState,
        trivia: Self::TriviaHandle,
        operand: Self::Node,
    );

    fn reduce_group(
        &mut self,
        ctx_data: Self::ContextData,
        op: GroupOperator,
        group_state: Self::GroupParseState,
        trailing_trivia: Self::TriviaHandle,
        closer_tok: Self::SyntaxTokenNode,
    ) -> Self::Node;

    fn reduce_call(
        &mut self,
        ctx_data: Self::ContextData,
        head: Self::Node,
        head_trivia: Self::TriviaHandle,
        group: Self::Node,
    ) -> Self::Node;

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(
        &mut self,
        ctx_data: Self::ContextData,
        data: SyntaxErrorData<
            Self::Node,
            Self::TriviaHandle,
            Self::SyntaxTokenNode,
        >,
    ) -> Self::Node;

    fn reduce_unterminated_group(
        &mut self,
        ctx_data: Self::ContextData,
        input: &'i str,
        tab_width: usize,
        op: GroupOperator,
        group_state: Self::GroupParseState,
        trailing_trivia: Self::TriviaHandle,
    ) -> Self::Node;

    fn reduce_group_missing_closer(
        &mut self,
        ctx_data: Self::ContextData,
        op: GroupOperator,
        group_state: Self::GroupParseState,
    ) -> Self::Node;

    //==================================
    // Pop
    //==================================

    fn finish_top_level_trivia(&mut self, trivia: TokenRef<'i>);

    fn finish_top_level_expr(&mut self, node: Self::Node);

    //==================================
    // Properties
    //==================================
    // TODO(cleanup): Find a way to store enough in ParserSession so that
    //                ParseBuilder impls don't need to provide these
    //                specialized methods.

    fn is_quiescent(&self) -> bool;

    fn check_colon_lhs(&self, lhs: &Self::Node) -> ColonLHS;

    fn top_node_is_span(&self) -> bool;
}

pub(crate) enum UnderParseData<'i> {
    /// `_`, `__`, or `___`.
    Under(TokenRef<'i>),
    /// `_head`, `__head`, or `___head`.
    UnderSymbol {
        op: CompoundOperator,
        under: TokenRef<'i>,
        symbol: TokenRef<'i>,
    },
}

/// A syntax error encountered during parsing.
///
/// Values of this type are passed to [`ParseBuilder::reduce_syntax_error()`]
/// by the parser.
#[derive(Debug)]
pub(crate) enum SyntaxErrorData<N, TRV, STN> {
    /// E.g. `5:_` -- occurs when a symbol is required to appear as the
    /// left-hand operand of Pattern (`:`).
    ///
    /// # Example
    ///
    /// ```
    /// # use wolfram_parser::{
    /// #     parse_cst, NodeSeq,
    /// #     parse::SyntaxErrorKind,
    /// #     cst::{Cst, SyntaxErrorNode},
    /// #     macros::token
    /// # };
    /// assert_eq!(
    ///     parse_cst("5:_", &Default::default()).syntax,
    ///     Cst::SyntaxError(SyntaxErrorNode {
    ///         err: SyntaxErrorKind::ExpectedSymbol,
    ///         children: NodeSeq(vec![
    ///             Cst::Token(token!(Integer, "5", 1:1-2)),
    ///             Cst::Token(token!(Colon, ":", 1:2-3)),
    ///             Cst::Token(token!(Under, "_", 1:3-4))
    ///         ])
    ///     })
    /// );
    /// ```
    #[allow(dead_code)]
    ExpectedSymbol {
        lhs_node: N,
        trivia1: TRV,
        tok_in: STN,
        trivia2: TRV,
        rhs_node: N,
    },

    /// E.g. `a /: b ;` -- occurs when a `=`, `:=`, or `=.` was expected after
    /// the 2nd operand in a `/:` ternary expression.
    ///
    /// # Example
    ///
    /// ```
    /// # use wolfram_parser::{
    /// #     parse_cst, NodeSeq,
    /// #     parse::{SyntaxErrorKind, operators::InfixOperator},
    /// #     cst::{Cst, SyntaxErrorNode, OperatorNode, InfixNode},
    /// #     macros::token
    /// # };
    /// assert_eq!(
    ///     parse_cst("a /: b ;", &Default::default()).syntax,
    ///     Cst::Infix(InfixNode(OperatorNode {
    ///         op: InfixOperator::CompoundExpression,
    ///         children: NodeSeq(vec![
    ///             Cst::SyntaxError(SyntaxErrorNode {
    ///                 err: SyntaxErrorKind::ExpectedSet,
    ///                 children: NodeSeq(vec![
    ///                     Cst::Token(token!(Symbol, "a", 1:1-2)),
    ///                     Cst::Token(token!(Whitespace, " ", 1:2-3)),
    ///                     Cst::Token(token!(SlashColon, "/:", 1:3-5)),
    ///                     Cst::Token(token!(Whitespace, " ", 1:5-6)),
    ///                     Cst::Token(token!(Symbol, "b", 1:6-7)),
    ///                 ])
    ///             }),
    ///             Cst::Token(token!(Whitespace, " ", 1:7-8)),
    ///             Cst::Token(token!(Semi, ";", 1:8-9)),
    ///             Cst::Token(token!(Fake_ImplicitNull, "", 1:9-9)),
    ///         ])
    ///     }))
    /// );
    /// ```
    ExpectedSet,

    /// E.g. `a~f` -- occurs when a second `~` was expected after the 2nd
    /// operand in a ternary Infix expression.
    ///
    /// # Example
    ///
    /// ```
    /// # use wolfram_parser::{
    /// #     parse_cst, NodeSeq,
    /// #     parse::SyntaxErrorKind,
    /// #     cst::{Cst, SyntaxErrorNode},
    /// #     macros::token
    /// # };
    /// assert_eq!(
    ///     parse_cst("a~f", &Default::default()).syntax,
    ///     Cst::SyntaxError(SyntaxErrorNode {
    ///         err: SyntaxErrorKind::ExpectedTilde,
    ///         children: NodeSeq(vec![
    ///             Cst::Token(token!(Symbol, "a", 1:1-2)),
    ///             Cst::Token(token!(Tilde, "~", 1:2-3)),
    ///             Cst::Token(token!(Symbol, "f", 1:3-4))
    ///         ])
    ///      })
    /// );
    /// ```
    #[allow(dead_code)]
    ExpectedTilde {
        lhs_node: N,
        trivia1: TRV,
        first_op_token: STN,
        trivia2: TRV,
        middle_node: N,
    },
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SyntaxErrorKind {
    ExpectedSymbol,
    ExpectedSet,
    ExpectedTilde,
}

#[derive(Debug)]
pub(crate) enum ColonLHS {
    Pattern,
    Optional,
    Error,
}

impl<D> Context<D> {
    pub fn new(prec: Option<Precedence>, builder_data: D) -> Self {
        Context { prec, builder_data }
    }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(
        &mut self,
        prec: P,
    ) {
        self.prec = prec.into();
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> ParserSession<'i, B> {
    pub fn new(
        input: &'i [u8],
        builder: B,
        opts: &ParseOptions,
    ) -> ParserSession<'i, B> {
        let ParseOptions {
            first_line_behavior: _,
            src_convention: _,
            encoding_mode: _,
            tab_width: _,
            check_issues: _,
            compute_oob: _,
            quirk_settings,
        } = *opts;

        ParserSession {
            tokenizer: Tokenizer::new(input, opts),
            builder,
            context_stack: Vec::new(),
            quirk_settings,
        }
    }

    /// Returns the complete input [`Buffer`][crate::source::Buffer].
    pub fn input(&self) -> &'i [u8] {
        self.tokenizer.input
    }

    /// Lookup and apply the [`PrefixParselet`] implementation associated
    /// with the [`TokenKind`] of `token`.
    // TODO(cleanup): Rename to avoid ambiguity with PrefixParselet::parse_prefix()?
    pub(crate) fn parse_prefix(&mut self, token: TokenRef<'i>) -> B::Node {
        B::with_prefix_parselet(token.tok, |parselet| {
            // MUSTTAIL
            parselet.parse_prefix(self, token)
        })
    }

    /// Lookup and apply the [`InfixParselet`] implementation associated
    /// with the [`TokenKind`] of `token`.
    // TODO(cleanup): Rename to avoid ambiguity with PrefixParselet::parse_prefix()?
    ///
    /// `node` is the previous node that just finished parsing.
    fn parse_infix(
        &mut self,
        finished: B::Node,
        trivia1: B::TriviaHandle,
        token: TokenRef<'i>,
    ) -> B::Node {
        B::with_infix_parselet(token.tok, |parselet| {
            // MUSTTAIL
            parselet.parse_infix(self, finished, trivia1, token)
        })
    }

    fn do_process_implicit_times(
        &mut self,
        token: TokenRef<'i>,
    ) -> TokenRef<'i> {
        B::with_infix_parselet(token.tok, |parselet| {
            parselet.process_implicit_times(self, token)
        })
    }

    pub(crate) fn push_and_climb(&mut self, leaf: TokenRef<'i>) -> B::Node {
        let node = self.push_leaf(leaf);

        self.parse_climb(node)
    }

    /// A complete sub-expression was just finished being parsed, so "climb" up
    /// by parsing the next "infix" token in the input.
    #[must_use]
    pub(crate) fn parse_climb(&mut self, finished: B::Node) -> B::Node {
        //
        // Check isAbort() inside loops
        //
        panic_if_aborted!();

        //
        // not in the middle of parsing anything, so toplevel newlines will delimit
        //
        let (trivia1, mut token) =
            self.current_token_eat_trivia_but_not_toplevel_newlines_into();

        token = self.do_process_implicit_times(token);

        let TokenPrecedence = B::with_infix_parselet(token.tok, |parselet| {
            parselet.getPrecedence(self)
        });

        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   break;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   break;
        //

        if Precedence::greater(self.top_precedence(), TokenPrecedence) {
            self.trivia_reset(trivia1);

            return finished;
        }

        self.push_context(TokenPrecedence);

        let trivia1 = self.builder.push_trivia_seq(trivia1);

        // MUSTTAIL
        return self.parse_infix(finished, trivia1, token);
    }

    //==================================
    // Node and trivia management
    //==================================

    /// Push an [*operand token][term].
    ///
    /// [term]: crate::parse#general-terminology
    // TODO(cleanup): Rename
    #[must_use]
    pub(crate) fn push_leaf(&mut self, token: TokenRef<'i>) -> B::Node {
        self.builder.push_leaf(token)
    }

    /// Push an [*operand token][term].
    ///
    /// [term]: crate::parse#general-terminology
    // TODO(cleanup): Rename
    #[must_use]
    pub(crate) fn push_leaf_and_next(
        &mut self,
        token: TokenRef<'i>,
    ) -> B::Node {
        let node = self.push_leaf(token);

        token.skip(&mut self.tokenizer);

        node
    }

    /// Push a [*syntax token*][term].
    ///
    /// [term]: crate::parse#general-terminology
    #[must_use]
    pub(crate) fn push_syntax_and_next(
        &mut self,
        token: TokenRef<'i>,
    ) -> B::SyntaxTokenNode {
        let node = self.builder.push_syntax(token);

        token.skip(&mut self.tokenizer);

        node
    }

    /// Consume the resettable trivia in `trivia` and advance the read cursor
    /// past `token`.
    fn commit_syntax_and_next(
        &mut self,
        trivia: B::ResettableTriviaHandle,
        token: TokenRef<'i>,
    ) -> (B::TriviaHandle, B::SyntaxTokenNode) {
        let trivia = self.builder.push_trivia_seq(trivia);

        let token = self.push_syntax_and_next(token);

        (trivia, token)
    }

    /// Move the underlying [`Reader`][crate::read::Reader] cursor to before
    /// `trivia`.
    fn trivia_reset(&mut self, trivia: B::ResettableTriviaHandle) {
        //
        // Just need to reset the global buffer to the buffer of the first token in the sequence
        //
        if let Some(first) = self.builder.reset_trivia_seq(trivia) {
            first.reset(&mut self.tokenizer)
        }
    }

    /// Get the next non-trivia token, eating trivia tokens.
    ///
    /// If the current token is already a non-trivia token, it will be returned.
    ///
    /// Otherwise, repeatedly eat the current token and append it to
    /// `container` until the current token is no longer a trivia token.
    ///
    /// This function always returns a non-trivia token
    /// ([`TokenKind::isTrivia()`] is false).
    pub(crate) fn current_token_eat_trivia(
        &mut self,
    ) -> (B::TriviaHandle, TokenRef<'i>) {
        let (trivia, tok) = self.current_token();

        // Commit this trivia.
        let trivia = self.builder.push_trivia_seq(trivia);

        (trivia, tok)
    }

    /// Get the next non-trivia token in the input, with the option to reset
    /// the read head.
    ///
    /// To reset the read head to its former position, use
    /// [`trivia_reset`][ParserSession::trivia_reset].
    //
    // TODO(cleanup): Make this return a special type, e.g. PeekedToken?
    pub(crate) fn current_token(
        &mut self,
    ) -> (B::ResettableTriviaHandle, TokenRef<'i>) {
        let mut tok = self.tokenizer.peek_token();

        let mut trivia = self.builder.resettable_trivia_begin();

        while tok.tok.isTrivia() {
            self.builder.resettable_trivia_push(&mut trivia, tok);

            tok.skip(&mut self.tokenizer);

            tok = self.tokenizer.peek_token();
        }

        let trivia = self.builder.resettable_trivia_end(trivia);

        debug_assert!(!tok.tok.isTrivia());

        (trivia, tok)
    }

    pub(crate) fn current_syntax_token_stringify_as_file(
        &mut self,
    ) -> (B::TriviaHandle, TokenRef<'i>) {
        let mut token =
            Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);

        let mut trivia = self.builder.resettable_trivia_begin();

        while token.tok.isTrivia() {
            self.builder.resettable_trivia_push(&mut trivia, token);

            token.skip(&mut self.tokenizer);

            token = Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);
        }

        debug_assert!(!token.tok.isTrivia());

        let trivia = self.builder.resettable_trivia_end(trivia);

        // Commit this trivia.
        let trivia = self.builder.push_trivia_seq(trivia);

        (trivia, token)
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines(
        &mut self,
    ) -> (B::TriviaHandle, TokenRef<'i>) {
        let (trivia, tok) =
            self.current_token_eat_trivia_but_not_toplevel_newlines_into();

        // Commit this trivia.
        let trivia = self.builder.push_trivia_seq(trivia);

        (trivia, tok)
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines_into(
        &mut self,
    ) -> (B::ResettableTriviaHandle, TokenRef<'i>) {
        let mut tok = self.tokenizer.peek_token();

        //
        // CompoundExpression should not cross toplevel newlines
        //
        let mut trivia = self.builder.resettable_trivia_begin();

        while tok.tok.isTriviaButNotToplevelNewline() {
            self.builder.resettable_trivia_push(&mut trivia, tok);

            tok.skip(&mut self.tokenizer);

            tok = self.tokenizer.peek_token();
        }

        (self.builder.resettable_trivia_end(trivia), tok)
    }

    //==================================
    // ParseBuilder method wrappers
    //==================================

    fn reduce_prefix(
        &mut self,
        op: PrefixOperator,
        op_token: B::SyntaxTokenNode,
        trivia: B::TriviaHandle,
        operand: B::Node,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder
            .reduce_prefix(ctx_data, op, op_token, trivia, operand)
    }

    // TODO(cleanup): Better name
    fn reduce_prefix_get(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: PrefixOperator,
        op_token: B::SyntaxTokenNode,
        trivia: B::TriviaHandle,
        stringify_token: TokenRef<'i>,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_prefix_get(
            ctx_data,
            op,
            op_token,
            trivia,
            stringify_token,
        )
    }

    fn begin_infix(
        &mut self,
        op: InfixOperator,
        first_node: B::Node,
    ) -> B::InfixParseState {
        self.builder.begin_infix(op, first_node)
    }

    fn reduce_infix(&mut self, state: B::InfixParseState) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_infix(ctx_data, state)
    }

    fn reduce_postfix(
        &mut self,
        op: PostfixOperator,
        operand: B::Node,
        trivia: B::TriviaHandle,
        op_token: B::SyntaxTokenNode,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder
            .reduce_postfix(ctx_data, op, operand, trivia, op_token)
    }

    fn reduce_binary(
        &mut self,
        op: BinaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        op_token: B::SyntaxTokenNode,
        trivia2: B::TriviaHandle,
        rhs_node: B::Node,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_binary(
            ctx_data, op, lhs_node, trivia1, op_token, trivia2, rhs_node,
        )
    }

    fn reduce_binary_unset(
        &mut self,
        op: BinaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        op_token: B::SyntaxTokenNode,
        trivia2: B::TriviaHandle,
        dot_token: B::SyntaxTokenNode,
    ) -> B::Node {
        debug_assert_eq!(op, BinaryOperator::Unset);
        // debug_assert_eq!(dot_token.tok, TokenKind::Dot);

        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_binary_unset(
            ctx_data, op, lhs_node, trivia1, op_token, trivia2, dot_token,
        )
    }

    // a /: b := c
    fn reduce_ternary(
        &mut self,
        op: TernaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        first_op_token: B::SyntaxTokenNode,
        trivia2: B::TriviaHandle,
        middle_node: B::Node,
        trivia3: B::TriviaHandle,
        second_op_token: B::SyntaxTokenNode,
        trivia4: B::TriviaHandle,
        rhs_node: B::Node,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_ternary(
            ctx_data,
            op,
            lhs_node,
            trivia1,
            first_op_token,
            trivia2,
            middle_node,
            trivia3,
            second_op_token,
            trivia4,
            rhs_node,
        )
    }

    fn reduce_ternary_tag_unset(
        &mut self,
        op: TernaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        slash_colon_token: B::SyntaxTokenNode,
        trivia2: B::TriviaHandle,
        middle_node: B::Node,
        trivia3: B::TriviaHandle,
        equal_token: B::SyntaxTokenNode,
        trivia4: B::TriviaHandle,
        dot_token: B::SyntaxTokenNode,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        debug_assert_eq!(op, TernaryOperator::TagUnset);

        self.builder.reduce_ternary_tag_unset(
            ctx_data,
            op,
            lhs_node,
            trivia1,
            slash_colon_token,
            trivia2,
            middle_node,
            trivia3,
            equal_token,
            trivia4,
            dot_token,
        )
    }

    fn reduce_prefix_binary(
        &mut self,
        op: PrefixBinaryOperator,
        prefix_op_token: B::SyntaxTokenNode,
        trivia1: B::TriviaHandle,
        lhs_node: B::Node,
        trivia2: B::TriviaHandle,
        rhs_node: B::Node,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_prefix_binary(
            ctx_data,
            op,
            prefix_op_token,
            trivia1,
            lhs_node,
            trivia2,
            rhs_node,
        )
    }

    fn reduce_group(
        &mut self,
        op: GroupOperator,
        group_state: B::GroupParseState,
        trailing_trivia: B::TriviaHandle,
        closer_tok: B::SyntaxTokenNode,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.pop_group();

        self.builder.reduce_group(
            ctx_data,
            op,
            group_state,
            trailing_trivia,
            closer_tok,
        )
    }

    fn reduce_call(
        &mut self,
        head: B::Node,
        head_trivia: B::TriviaHandle,
        group: B::Node,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_call(ctx_data, head, head_trivia, group)
    }

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(
        &mut self,
        data: SyntaxErrorData<B::Node, B::TriviaHandle, B::SyntaxTokenNode>,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.builder.reduce_syntax_error(ctx_data, data)
    }

    fn reduce_unterminated_group(
        &mut self,
        op: GroupOperator,
        group_state: B::GroupParseState,
        trailing_trivia: B::TriviaHandle,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.pop_group();

        // The input MUST be valid UTF-8, because we only reduce an *unterminated*
        // group node if we've read an EOF (which is how we know it must be
        // unterminated: we've read all the input).
        let input = std::str::from_utf8(self.input()).expect(
            "cannot reparse unterminated group node: input is not valid UTF-8",
        );

        let tab_width = self.tokenizer.tab_width as usize;

        self.builder.reduce_unterminated_group(
            ctx_data,
            input,
            tab_width,
            op,
            group_state,
            trailing_trivia,
        )
    }

    fn reduce_group_missing_closer(
        &mut self,
        op: GroupOperator,
        group_state: B::GroupParseState,
    ) -> B::Node {
        let ctx_data = self.context_stack.pop().unwrap().builder_data;

        self.pop_group();

        self.builder
            .reduce_group_missing_closer(ctx_data, op, group_state)
    }

    //----------------------------------
    // Properties
    //----------------------------------

    fn check_colon_lhs(&self, lhs: &B::Node) -> ColonLHS {
        self.builder.check_colon_lhs(lhs)
    }

    //==================================
    // Context management
    //==================================

    /// Push a new context with associated precedence value.
    ///
    /// The most recently emitted node is included in the new context.
    pub(crate) fn push_context<'s, P: Into<Option<Precedence>>>(
        &'s mut self,
        prec: P,
    ) -> &'s mut Context<B::ContextData> {
        let prec = prec.into();

        let data = self.builder.begin_context();

        self.context_stack.push(Context::new(prec, data));

        return self.context_stack.last_mut().unwrap();
    }

    fn top_context<'s>(&'s mut self) -> &'s mut Context<B::ContextData> {
        return self
            .context_stack
            .last_mut()
            .expect("top_context: no contexts set");
    }

    //==================================
    // Precedence management
    //==================================

    pub(crate) fn top_precedence(&self) -> Option<Precedence> {
        match self.context_stack.last() {
            Some(ctxt) => ctxt.prec,
            None => None,
        }
    }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(
        &mut self,
        prec: P,
    ) {
        let prec = prec.into();

        self.top_context().prec = prec;
    }

    //==================================
    // Group stack management
    //==================================

    pub(crate) fn push_group(&mut self, closer: Closer) {
        self.tokenizer.GroupStack.push(closer);
    }

    pub(crate) fn pop_group(&mut self) {
        assert!(!self.tokenizer.GroupStack.is_empty());

        self.tokenizer.GroupStack.pop();
    }

    pub(crate) fn check_group(&self, closer: Closer) -> bool {
        for value in self.tokenizer.GroupStack.iter().rev() {
            if *value == closer {
                return true;
            }
        }

        return false;
    }

    //===================================
    // Assorted parselet helper functions
    //===================================

    fn check_pattern_precedence(&self) -> bool {
        for ctxt in self.context_stack.iter().rev() {
            let Some(prec) = ctxt.prec else {
                // Equivalent to a precedence of zero.
                return false;
            };

            if prec > Precedence::FAKE_PATTERNCOLON {
                continue;
            }

            if prec < Precedence::FAKE_PATTERNCOLON {
                return false;
            }

            assert!(prec == Precedence::FAKE_PATTERNCOLON);

            return true;
        }

        return false;
    }

    pub fn is_quiescent(&mut self) -> bool {
        assert!(self.builder.is_quiescent());
        assert!(
            self.context_stack.is_empty(),
            "expected empty context stack, got: {:#?}",
            self.context_stack
        );
        assert!(self.tokenizer.GroupStack.is_empty());

        return true;
    }
}
