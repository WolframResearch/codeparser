//! Parser implementation.
//!
//! # Parser Design
//!
//! Each parse of a Wolfram input is managed by a [`ParserSession`] instance.
//!
//! Parsing logic is structured into individual "modules" calls *parselets*.
//!
//! There are two kinds of parselet:
//!
//! * [`PrefixParselet`] — invoked when there is no previous expression in the
//!   current context.
//! * [`InfixParselet`] — invoked when there is a previous expression in the
//!   current context.
//!
//! Every token is associated with one [`PrefixParselet`] instance
//! ([`TokenKind::prefix_parselet()`]) and one
//! [`InfixParselet`] instance ([`TokenKind::infix_parselet()`]), which are
//! invoked, respectively, when that token is encountered in "prefix" or "infix"
//! position.
//!
//! Parselet implementations will typically read the current or next token,
//! do a bit of logic, optionally push a node onto the node stack, and then
//! either:
//!
//! 1. Call [`parse_prefix()`][ParserSession::parse_prefix] on the next token in the input
//! 2. Call [`parse_infix()`][ParserSession::parse_infix] on the next token in the input
//! 3. Call a `reduce_XXX()` method to push a completed parsed subexpression
//!    onto the node stack followed by [`parse_climb()`][ParserSession::parse_climb]
//!    to parse the next token using `parse_infix()`.
//! 4. Call [`try_continue()`][ParserSession::try_continue] to invoke the
//!    continuation function from the top context on the context stack.

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

    context_stack: Vec<Context>,

    quirk_settings: QuirkSettings,
}

pub(crate) struct Context {
    pub(crate) prec: Option<Precedence>,
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
    type Output;

    type InfixParseState: Debug;

    //==================================
    // Lifecycle
    //==================================

    fn new_builder() -> Self;

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

    /// Complete the parse and return the parsed output.
    fn finish(self, input: &'i [u8], opts: &ParseOptions) -> Self::Output;

    //==================================
    // Trivia handling
    //==================================

    type TriviaAccumulator;
    type TriviaHandle;

    fn trivia_begin(&mut self) -> Self::TriviaAccumulator;

    fn trivia_push(
        &mut self,
        accum: &mut Self::TriviaAccumulator,
        trivia: TokenRef<'i>,
    );

    fn trivia_end(
        &mut self,
        accum: Self::TriviaAccumulator,
    ) -> Self::TriviaHandle;

    fn empty_trivia() -> Self::TriviaHandle;

    /// Get the first piece of trivia in a set of trivia.
    ///
    /// Used to reset the position of the underlying [`Reader`] when a
    /// potential parse fails.
    fn trivia_first(&self, trivia: Self::TriviaHandle) -> Option<TokenRef<'i>>;

    //==================================
    // Context management
    //==================================

    fn begin_context(&mut self);

    //==================================
    // Push
    //==================================

    fn push_leaf(&mut self, token: TokenRef<'i>) -> Self::Node;

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

    // TODO(cleanup): Better name
    fn push_prefix_get(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: PrefixOperator,
        op_token: TokenRef<'i>,
        trivia: Self::TriviaHandle,
        stringify_token: TokenRef<'i>,
    ) -> Self::Node;

    //==================================
    // Reduce
    //==================================

    //----------------------------------
    // Reduce normal
    //----------------------------------

    fn reduce_prefix(
        &mut self,
        op: PrefixOperator,
        op_token: TokenRef<'i>,
        trivia: Self::TriviaHandle,
        operand: Self::Node,
    ) -> Self::Node;

    fn reduce_postfix(
        &mut self,
        op: PostfixOperator,
        operand: Self::Node,
        trivia: Self::TriviaHandle,
        op_tok: TokenRef<'i>,
    ) -> Self::Node;

    fn reduce_binary(
        &mut self,
        op: BinaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node;

    fn reduce_binary_unset(
        &mut self,
        op: BinaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        dot_token: TokenRef<'i>,
    ) -> Self::Node;

    fn reduce_ternary(
        &mut self,
        op: TernaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        first_op_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        middle_node: Self::Node,
        trivia3: Self::TriviaHandle,
        second_op_token: TokenRef<'i>,
        trivia4: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node;

    fn reduce_ternary_tag_unset(
        &mut self,
        // TODO(cleanup): Always the same operator?
        op: TernaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        slash_colon_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        middle_node: Self::Node,
        trivia3: Self::TriviaHandle,
        equal_token: TokenRef<'i>,
        trivia4: Self::TriviaHandle,
        dot_token: TokenRef<'i>,
    ) -> Self::Node;

    fn reduce_prefix_binary(
        &mut self,
        op: PrefixBinaryOperator,
        prefix_op_token: TokenRef<'i>,
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
        op_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        operand: Self::Node,
    );

    /// Get the last parsed node contained in this infix expression.
    ///
    /// Needed specially to support custom context-dependent
    /// [`SemiSemiParselet::process_implicit_times()`][self::parselet::SemiSemiParselet::process_implicit_times]
    /// implementation.
    fn infix_last_node<'s>(
        &self,
        infix_state: &'s Self::InfixParseState,
    ) -> &'s Self::Node;

    /// Parselet implementations should not call this method directly, and
    /// instead call [`ParserSession::reduce_infix()`].
    fn infix_finish(
        &mut self,
        infix_state: Self::InfixParseState,
    ) -> Self::Node;

    fn reduce_group(
        &mut self,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(Self::TriviaHandle, Self::Node)>,
        trailing_trivia: Self::TriviaHandle,
        closer_tok: TokenRef<'i>,
    ) -> Self::Node;

    fn reduce_call(
        &mut self,
        head: Self::Node,
        head_trivia: Self::TriviaHandle,
        group: Self::Node,
    ) -> Self::Node;

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(
        &mut self,
        data: SyntaxErrorData<'i, Self::Node, Self::TriviaHandle>,
    ) -> Self::Node;

    fn reduce_unterminated_group(
        &mut self,
        input: &'i str,
        tab_width: usize,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(Self::TriviaHandle, Self::Node)>,
        trailing_trivia: Self::TriviaHandle,
    ) -> Self::Node;

    fn reduce_group_missing_closer(
        &mut self,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(Self::TriviaHandle, Self::Node)>,
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

    fn top_node_is_span(&self, top_node: &Self::Node) -> bool;
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
pub(crate) enum SyntaxErrorData<'i, N, TRV> {
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
    ExpectedSymbol {
        lhs_node: N,
        trivia1: TRV,
        tok_in: TokenRef<'i>,
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
    ///                 // FIXME: This should include the "a /: b" portion
    ///                 //        of the input.
    ///                 children: NodeSeq(vec![])
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
    ExpectedTilde {
        lhs_node: N,
        trivia1: TRV,
        first_op_token: TokenRef<'i>,
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

impl Context {
    pub fn new(prec: Option<Precedence>) -> Self {
        Context { prec }
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
        prev_node: &B::Node,
        token: TokenRef<'i>,
    ) -> TokenRef<'i> {
        B::with_infix_parselet(token.tok, |parselet| {
            parselet.process_implicit_times(self, prev_node, token)
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

        token = self.do_process_implicit_times(&finished, token);

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

        // MUSTTAIL
        return self.parse_infix(finished, trivia1, token);
    }

    //======================================
    // Get current token after eating trivia
    //======================================

    pub(crate) fn skip(&mut self, token: TokenRef<'i>) {
        token.skip(&mut self.tokenizer)
    }

    /// Move the underlying [`Reader`][crate::read::Reader] cursor to before
    /// `trivia`.
    fn trivia_reset(&mut self, trivia: B::TriviaHandle) {
        //
        // Just need to reset the global buffer to the buffer of the first token in the sequence
        //
        if let Some(first) = self.builder.trivia_first(trivia) {
            first.reset(&mut self.tokenizer)
        }
    }

    /// Get the current token, eating trivia tokens.
    ///
    /// If the current token is already a non-trivia token, it will be returned.
    ///
    /// Otherwise, repeatedly eat the current token and append it to
    /// `container` until the current token is no longer a trivia token.
    ///
    /// This function always returns a non-trivia token
    /// ([`TokenKind::isTrivia()`] is false).
    pub(crate) fn current_token_eat_trivia_into(
        &mut self,
    ) -> (B::TriviaHandle, TokenRef<'i>) {
        let mut tok = self.tokenizer.peek_token();

        let mut trivia = self.builder.trivia_begin();

        while tok.tok.isTrivia() {
            self.builder.trivia_push(&mut trivia, tok);

            tok.skip(&mut self.tokenizer);

            tok = self.tokenizer.peek_token();
        }

        let trivia = self.builder.trivia_end(trivia);

        debug_assert!(!tok.tok.isTrivia());

        (trivia, tok)
    }

    pub(crate) fn current_syntax_token_stringify_as_file(
        &mut self,
    ) -> (B::TriviaHandle, TokenRef<'i>) {
        let mut token =
            Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);

        let mut trivia = self.builder.trivia_begin();

        while token.tok.isTrivia() {
            self.builder.trivia_push(&mut trivia, token);

            token.skip(&mut self.tokenizer);

            token = Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);
        }

        debug_assert!(!token.tok.isTrivia());

        (self.builder.trivia_end(trivia), token)
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines_into(
        &mut self,
    ) -> (B::TriviaHandle, TokenRef<'i>) {
        let mut tok = self.tokenizer.peek_token();

        //
        // CompoundExpression should not cross toplevel newlines
        //
        let mut trivia = self.builder.trivia_begin();

        while tok.tok.isTriviaButNotToplevelNewline() {
            self.builder.trivia_push(&mut trivia, tok);

            tok.skip(&mut self.tokenizer);

            tok = self.tokenizer.peek_token();
        }

        (self.builder.trivia_end(trivia), tok)
    }

    //==================================
    // ParseBuilder method wrappers
    //==================================

    fn reduce_prefix(
        &mut self,
        op: PrefixOperator,
        op_token: TokenRef<'i>,
        trivia: B::TriviaHandle,
        operand: B::Node,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_prefix(op, op_token, trivia, operand)
    }

    fn begin_infix(
        &mut self,
        op: InfixOperator,
        first_node: B::Node,
    ) -> B::InfixParseState {
        self.builder.begin_infix(op, first_node)
    }

    fn reduce_infix(&mut self, state: B::InfixParseState) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder.infix_finish(state)
    }

    fn reduce_postfix(
        &mut self,
        op: PostfixOperator,
        operand: B::Node,
        trivia: B::TriviaHandle,
        op_tok: TokenRef<'i>,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_postfix(op, operand, trivia, op_tok)
    }

    fn reduce_binary(
        &mut self,
        op: BinaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: B::TriviaHandle,
        rhs_node: B::Node,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder
            .reduce_binary(op, lhs_node, trivia1, op_token, trivia2, rhs_node)
    }

    fn reduce_binary_unset(
        &mut self,
        op: BinaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: B::TriviaHandle,
        dot_token: TokenRef<'i>,
    ) -> B::Node {
        debug_assert_eq!(op, BinaryOperator::Unset);
        debug_assert_eq!(dot_token.tok, TokenKind::Dot);

        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_binary_unset(
            op, lhs_node, trivia1, op_token, trivia2, dot_token,
        )
    }

    // a /: b := c
    fn reduce_ternary(
        &mut self,
        op: TernaryOperator,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        first_op_token: TokenRef<'i>,
        trivia2: B::TriviaHandle,
        middle_node: B::Node,
        trivia3: B::TriviaHandle,
        second_op_token: TokenRef<'i>,
        trivia4: B::TriviaHandle,
        rhs_node: B::Node,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_ternary(
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
        slash_colon_token: TokenRef<'i>,
        trivia2: B::TriviaHandle,
        middle_node: B::Node,
        trivia3: B::TriviaHandle,
        equal_token: TokenRef<'i>,
        trivia4: B::TriviaHandle,
        dot_token: TokenRef<'i>,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        debug_assert_eq!(op, TernaryOperator::TagUnset);

        self.builder.reduce_ternary_tag_unset(
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
        prefix_op_token: TokenRef<'i>,
        trivia1: B::TriviaHandle,
        lhs_node: B::Node,
        trivia2: B::TriviaHandle,
        rhs_node: B::Node,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_prefix_binary(
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
        opener_tok: TokenRef<'i>,
        group_children: Vec<(B::TriviaHandle, B::Node)>,
        trailing_trivia: B::TriviaHandle,
        closer_tok: TokenRef<'i>,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.pop_group();

        self.builder.reduce_group(
            op,
            opener_tok,
            group_children,
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
        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_call(head, head_trivia, group)
    }

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(
        &mut self,
        data: SyntaxErrorData<'i, B::Node, B::TriviaHandle>,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.builder.reduce_syntax_error(data)
    }

    fn reduce_unterminated_group(
        &mut self,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(B::TriviaHandle, B::Node)>,
        trailing_trivia: B::TriviaHandle,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.pop_group();

        // The input MUST be valid UTF-8, because we only reduce an *unterminated*
        // group node if we've read an EOF (which is how we know it must be
        // unterminated: we've read all the input).
        let input = std::str::from_utf8(self.input()).expect(
            "cannot reparse unterminated group node: input is not valid UTF-8",
        );

        let tab_width = self.tokenizer.tab_width as usize;

        self.builder.reduce_unterminated_group(
            input,
            tab_width,
            op,
            opener_tok,
            group_children,
            trailing_trivia,
        )
    }

    fn reduce_group_missing_closer(
        &mut self,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(B::TriviaHandle, B::Node)>,
    ) -> B::Node {
        let _ = self.context_stack.pop().unwrap();

        self.pop_group();

        self.builder
            .reduce_group_missing_closer(op, opener_tok, group_children)
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
    /// The top node in the [`node_stack`][ParserSession::node_stack] is included
    /// in the new context.
    pub(crate) fn push_context<'s, P: Into<Option<Precedence>>>(
        &'s mut self,
        prec: P,
    ) -> &'s mut Context {
        let prec = prec.into();

        let () = self.builder.begin_context();

        self.context_stack.push(Context::new(prec));

        return self.context_stack.last_mut().unwrap();
    }

    fn top_context<'s>(&'s mut self) -> &'s mut Context {
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
    // Node stack management
    //==================================

    // TODO(cleanup): Rename
    #[must_use]
    pub(crate) fn push_leaf(&mut self, token: TokenRef<'i>) -> B::Node {
        self.builder.push_leaf(token)
    }

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

//======================================
// Format Impls
//======================================

impl Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Context { prec } = self;
        f.debug_struct("Context").field("prec", prec).finish()
    }
}
