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


pub(crate) mod parselet;
pub(crate) mod operators;
mod token_parselets;


use std::fmt::Debug;

use crate::{
    create_parse_result,
    cst::{
        BinaryOperator, CompoundOperator, Cst, CstSeq, SyntaxErrorKind,
        TernaryOperator, TriviaSeq,
    },
    feature, panic_if_aborted,
    parse::parselet::PrefixToplevelCloserParselet,
    parse_cst::ParseCst,
    precedence::Precedence,
    quirks,
    tokenize::{
        token_kind::Closer, tokenizer::Tokenizer_currentToken_stringifyAsFile,
        TokenKind, TokenRef, TokenStr, Tokenizer,
    },
    NodeSeq, ParseOptions, ParseResult, QuirkSettings,
};

use self::{
    operators::{
        GroupOperator, InfixOperator, PostfixOperator, PrefixBinaryOperator,
        PrefixOperator,
    },
    parselet::{InfixParselet, PrefixParselet},
    token_parselets::{INFIX_PARSELETS, PREFIX_PARSELETS},
};

//======================================
// Types
//======================================

/// A parser session
#[derive(Debug)]
pub(crate) struct ParserSession<'i, B: ParseBuilder<'i> = ParseCst<'i>> {
    pub(crate) tokenizer: Tokenizer<'i>,

    #[cfg(not(test))]
    pub(super) builder: B,

    #[cfg(test)]
    pub(crate) builder: B,

    pub(crate) quirk_settings: QuirkSettings,
}

pub(crate) struct Context<'i> {
    continue_parse: Option<Box<dyn FnOnce(&mut ParserSession<'i>) + 'i>>,

    /// The position in [`ParserSession.node_stack`][ParserSession::node_stack]
    /// that marks the first node associated with this [`Context`].
    pub(crate) index: usize,

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
pub(crate) trait ParseBuilder<'i>: Debug {
    type Output;

    //==================================
    // Context management
    //==================================

    fn push_context(
        &mut self,
        precedence: Option<Precedence>,
    ) -> &mut Context<'i>;

    fn top_context(&mut self) -> Option<&mut Context<'i>>;

    fn is_quiescent(&self) -> bool;

    //==================================
    // Push
    //==================================

    fn push_leaf(&mut self, token: TokenRef<'i>);

    fn push_trivia(&mut self, trivia: TokenRef<'i>);

    fn push_trivia_seq(&mut self, seq: TriviaSeqRef<'i>);

    /// `name_` or `name_head`
    fn push_compound_pattern_blank(
        &mut self,
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under: UnderParseData<'i>,
    );

    /// `_` or `_head`
    fn push_compound_blank(&mut self, under: UnderParseData<'i>);

    fn push_compound_pattern_optional(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under_dot: TokenRef<'i>,
    );

    // TODO(cleanup): Same signature as push_compound_pattern_optional?
    fn push_compound_slot(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: CompoundOperator,
        hash: TokenRef<'i>,
        arg: TokenRef<'i>,
    );

    // TODO(cleanup): Same signature as push_compound_pattern_optional and push_compound_slot?
    fn push_compound_out(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: CompoundOperator,
        percent: TokenRef<'i>,
        integer: TokenRef<'i>,
    );

    // TODO(cleanup): Better name
    fn push_prefix_get(
        &mut self,
        // TODO(cleanup): Can this only ever have one value?
        op: PrefixOperator,
        op_token: TokenRef<'i>,
        trivia: TriviaSeqRef<'i>,
        stringify_token: TokenRef<'i>,
    );

    //==================================
    // Reduce
    //==================================

    //----------------------------------
    // Reduce normal
    //----------------------------------

    fn reduce_prefix(&mut self, op: PrefixOperator);

    fn reduce_infix(&mut self, op: InfixOperator);

    fn reduce_postfix(&mut self, op: PostfixOperator);

    fn reduce_binary(&mut self, op: BinaryOperator);

    fn reduce_ternary(&mut self, op: TernaryOperator);

    fn reduce_prefix_binary(&mut self, op: PrefixBinaryOperator);

    fn reduce_group(&mut self, op: GroupOperator);

    fn reduce_call(&mut self);

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(&mut self, kind: SyntaxErrorKind);

    fn reduce_unterminated_group(
        &mut self,
        op: GroupOperator,
        input: &'i str,
        tab_width: usize,
    );

    fn reduce_group_missing_closer(&mut self, op: GroupOperator);

    //==================================
    // Pop
    //==================================

    fn pop_finished_expr(&mut self) -> Self::Output;

    //==================================
    // Properties
    //==================================
    // TODO(cleanup): Find a way to store enough in ParserSession so that
    //                ParseBuilder impls don't need to provide these
    //                specialized methods.

    fn check_pattern_precedence(&self) -> bool;

    fn check_colon_lhs(&self) -> ColonLHS;

    fn top_non_trivia_node_is_tilde(&self) -> bool;

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

pub(crate) enum ColonLHS {
    Pattern,
    Optional,
    Error,
}

impl<'i> Context<'i> {
    pub fn new(index: usize, prec: Option<Precedence>) -> Self {
        Context {
            continue_parse: None,
            index,
            prec,
        }
    }

    pub(crate) fn init_callback(&mut self, func: fn(&mut ParserSession)) {
        debug_assert!(matches!(self.continue_parse, None));

        self.continue_parse = Some(Box::new(func));
    }

    pub(crate) fn init_callback_with_state<
        F: FnOnce(&mut ParserSession<'i>) + 'i,
    >(
        &mut self,
        func: F,
    ) {
        debug_assert!(matches!(self.continue_parse, None));

        self.continue_parse = Some(Box::new(func))
    }

    pub(crate) fn init_identity(&mut self) {
        debug_assert!(matches!(self.continue_parse, None));

        self.continue_parse = None;
    }

    pub(crate) fn set_callback(&mut self, func: fn(&mut ParserSession)) {
        self.continue_parse = Some(Box::new(func));
    }

    pub(crate) fn set_callback_with_state<
        F: FnOnce(&mut ParserSession<'i>) + 'i,
    >(
        &mut self,
        func: F,
    ) {
        // TODO: Should `f` already have some value in this case?
        self.continue_parse = Some(Box::new(func));
    }

    // pub(crate) fn is_identity(&self) -> bool {
    //     self.f == Some(Parser_identity)
    // }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(
        &mut self,
        prec: P,
    ) {
        self.prec = prec.into();
    }
}

impl TokenKind {
    /// Get the [`PrefixParselet`] implementation associated with this token.
    fn prefix_parselet(&self) -> &'static dyn PrefixParselet {
        let index = usize::from(self.id());

        PREFIX_PARSELETS[index]
    }

    /// Get the [`InfixParselet`] implementation associated with this token.
    fn infix_parselet(&self) -> &'static dyn InfixParselet {
        let index = usize::from(self.id());

        INFIX_PARSELETS[index]
    }
}

impl<'i> ParserSession<'i> {
    pub fn new(input: &'i [u8], opts: &ParseOptions) -> ParserSession<'i> {
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
            builder: ParseCst::new(),
            quirk_settings,
        }
    }

    /// Returns the complete input [`Buffer`][crate::source::Buffer].
    pub fn input(&self) -> &'i [u8] {
        self.tokenizer.input
    }

    pub fn concrete_parse_expressions(
        &mut self,
    ) -> ParseResult<CstSeq<TokenStr<'i>>> {
        quirks::set_quirks(self.quirk_settings);

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("enter parseExpressions");
            DiagnosticsMarkTime();
        }

        //
        // Collect all expressions
        //

        let mut exprs: CstSeq<TokenStr<'i>> = NodeSeq::new();

        loop {
            if feature::CHECK_ABORT && crate::abortQ() {
                break;
            }

            let peek: TokenRef = self.tokenizer.peek_token();

            if peek.tok == TokenKind::EndOfFile {
                break;
            }

            if peek.tok.isTrivia() {
                exprs.push(Cst::Token(peek));

                peek.skip(&mut self.tokenizer);

                continue;
            }

            //
            // special top-level handling of stray closers
            //
            if peek.tok.isCloser() {
                (PrefixToplevelCloserParselet {}).parse_prefix(self, peek);

                exprs.push(self.builder.pop_finished_expr());

                assert!(self.is_quiescent());

                continue;
            }

            self.parse_prefix(peek);

            exprs.push(self.builder.pop_finished_expr());

            assert!(self.is_quiescent());
        } // while (true)

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("exit parseExpressions");
            DiagnosticsLogTime();
        }

        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            exprs = crate::error::reparse_unterminated(
                exprs,
                input,
                usize::try_from(self.tokenizer.tab_width).unwrap(),
            );
        }

        return create_parse_result(&self.tokenizer, exprs);
    }

    /// Lookup and apply the [`PrefixParselet`] implementation associated
    /// with the [`TokenKind`] of `token`.
    // TODO(cleanup): Rename to avoid ambiguity with PrefixParselet::parse_prefix()?
    pub(crate) fn parse_prefix(&mut self, token: TokenRef<'i>) {
        // MUSTTAIL
        token.tok.prefix_parselet().parse_prefix(self, token)
    }

    /// Lookup and apply the [`InfixParselet`] implementation associated
    /// with the [`TokenKind`] of `token`.
    // TODO(cleanup): Rename to avoid ambiguity with PrefixParselet::parse_prefix()?
    fn parse_infix(&mut self, token: TokenRef<'i>) {
        token.tok.infix_parselet().parse_infix(self, token)
    }

    fn do_process_implicit_times(
        &mut self,
        token: TokenRef<'i>,
    ) -> TokenRef<'i> {
        token
            .tok
            .infix_parselet()
            .process_implicit_times(self, token)
    }

    pub(crate) fn push_and_climb(&mut self, leaf: TokenRef<'i>) {
        self.builder.push_leaf(leaf);
        self.parse_climb();
    }

    /// A complete sub-expression was just finished being parsed, so "climb" up
    /// by parsing the next "infix" token in the input.
    pub(crate) fn parse_climb(&mut self) {
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

        let TokenPrecedence = token.tok.infix_parselet().getPrecedence(self);

        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   break;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   break;
        //

        if Precedence::greater(self.top_precedence(), TokenPrecedence) {
            trivia1.reset(&mut self.tokenizer);

            // MUSTTAIL
            return self.try_continue();
        }

        self.push_context(TokenPrecedence);

        self.push_trivia_seq(trivia1);

        // MUSTTAIL
        return self.parse_infix(token);
    }

    /// Apply the continuation function from the top context to
    /// attempt to continue parsing.
    pub(crate) fn try_continue(&mut self) {
        if self.builder.top_context().is_none() {
            // no call needed here
            return;
        }

        let ctxt: &mut Context = self.top_context();

        let Some(continue_parse) =
            std::mem::replace(&mut ctxt.continue_parse, None)
        else {
            return;
        };

        (continue_parse)(self)
    }

    //======================================
    // Get current token after eating trivia
    //======================================

    /// Get the current token, eating trivia tokens.
    ///
    /// If the current token is already a non-trivia token, it will be returned.
    ///
    /// Otherwise, repeatedly eat the current token and append it to
    /// [`ParserSession::node_stack`] until the current token is no longer a
    /// trivia token.
    ///
    /// This function always returns a non-trivia token
    /// ([`TokenKind::isTrivia()`] is false).
    pub(crate) fn current_token_eat_trivia(&mut self) -> TokenRef<'i> {
        let mut tok = self.tokenizer.peek_token();

        self.eat_trivia(&mut tok);

        debug_assert!(!tok.tok.isTrivia());

        tok
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
    ) -> (TriviaSeqRef<'i>, TokenRef<'i>) {
        let mut trivia = TriviaSeqRef::new();
        let mut tok = self.tokenizer.peek_token();

        while tok.tok.isTrivia() {
            trivia.push(tok.clone());

            tok.skip(&mut self.tokenizer);

            tok = self.tokenizer.peek_token();
        }

        debug_assert!(!tok.tok.isTrivia());

        (trivia, tok)
    }

    pub(crate) fn current_syntax_token_stringify_as_file(
        &mut self,
    ) -> (TriviaSeqRef<'i>, TokenRef<'i>) {
        let mut token =
            Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);

        let mut trivia = Vec::new();

        while token.tok.isTrivia() {
            trivia.push(token);

            token.skip(&mut self.tokenizer);

            token = Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);
        }

        debug_assert!(!token.tok.isTrivia());

        (TriviaSeq(trivia), token)
    }

    pub(crate) fn current_token_stringify_as_file_eat_trivia(
        &mut self,
    ) -> TokenRef<'i> {
        let mut tok =
            Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);

        self.eat_trivia_stringify_as_file(&mut tok);

        debug_assert!(!tok.tok.isTrivia());

        tok
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines(
        &mut self,
    ) -> TokenRef<'i> {
        let mut tok = self.tokenizer.peek_token();

        //
        // CompoundExpression should not cross toplevel newlines
        //
        self.eat_trivia_but_not_toplevel_newlines(&mut tok);

        tok
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines_into(
        &mut self,
    ) -> (TriviaSeqRef<'i>, TokenRef<'i>) {
        let mut tok = self.tokenizer.peek_token();

        //
        // CompoundExpression should not cross toplevel newlines
        //
        let mut trivia = TriviaSeq::new();

        while tok.tok.isTriviaButNotToplevelNewline() {
            trivia.push(tok.clone().into());

            tok.skip(&mut self.tokenizer);

            tok = self.tokenizer.peek_token();
        }

        (trivia, tok)
    }

    //----------------------------------
    // Eat trivia helpers
    //----------------------------------
    // TODO(cleanup): Inline these functions into their currently only callsite?

    fn eat_trivia(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTrivia() {
            self.builder.push_trivia(*token);

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    fn eat_trivia_stringify_as_file(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTrivia() {
            self.builder.push_trivia(*token);

            token.skip(&mut self.tokenizer);

            *token =
                Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);
        }
    }

    fn eat_trivia_but_not_toplevel_newlines(
        &mut self,
        token: &mut TokenRef<'i>,
    ) {
        while token.tok.isTriviaButNotToplevelNewline() {
            self.builder.push_trivia(*token);

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
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
    ) -> &'s mut Context<'i> {
        let prec = prec.into();

        return self.builder.push_context(prec);
    }

    pub(crate) fn top_context<'s>(&'s mut self) -> &'s mut Context<'i> {
        return self
            .builder
            .top_context()
            .expect("top_context: no contexts set");
    }

    //==================================
    // Precedence management
    //==================================

    pub(crate) fn top_precedence(&mut self) -> Option<Precedence> {
        match self.builder.top_context() {
            Some(ctxt) => ctxt.prec,
            None => None,
        }
    }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(
        &mut self,
        prec: P,
    ) {
        let prec = prec.into();

        self.builder
            .top_context()
            .expect("set_precedence: no contexts set")
            .prec = prec;
    }

    //==================================
    // Node stack management
    //==================================

    pub(crate) fn push_leaf(&mut self, token: TokenRef<'i>) {
        self.builder.push_leaf(token);
    }

    pub(crate) fn push_leaf_and_next(&mut self, token: TokenRef<'i>) {
        self.builder.push_leaf(token);

        token.skip(&mut self.tokenizer);
    }

    pub(crate) fn push_trivia_seq(&mut self, seq: TriviaSeqRef<'i>) {
        self.builder.push_trivia_seq(seq);
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

    pub fn is_quiescent(&mut self) -> bool {
        assert!(self.builder.is_quiescent());
        assert!(self.tokenizer.GroupStack.is_empty());

        return true;
    }

    #[cfg(test)]
    pub(crate) fn fatal_issues(&self) -> &Vec<crate::issue::Issue> {
        &self.tokenizer.fatal_issues
    }

    #[cfg(test)]
    pub(crate) fn non_fatal_issues(&self) -> &Vec<crate::issue::Issue> {
        &self.tokenizer.non_fatal_issues
    }
}

//======================================
// TriviaSeq
//======================================

impl<'i> TriviaSeq<TokenStr<'i>> {
    pub(crate) fn new() -> Self {
        TriviaSeq(Vec::new())
    }

    pub(crate) fn reset(self, session: &mut Tokenizer) {
        let TriviaSeq(vec) = self;

        //
        // Just need to reset the global buffer to the buffer of the first token in the sequence
        //

        if vec.is_empty() {
            return;
        }

        let T = &vec[0];

        T.reset(session);
    }

    pub(crate) fn push(&mut self, token: TokenRef<'i>) {
        let TriviaSeq(vec) = self;
        vec.push(token);
    }
}

//======================================
// Format Impls
//======================================

impl<'i> Debug for Context<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("continue_parse", &"<continuation function>")
            .field("index", &self.index)
            .field("prec", &self.prec)
            .finish()
    }
}
