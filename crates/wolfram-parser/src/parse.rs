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
//! 3. Call [`reduce_and_climb()`][ParserSession::reduce_and_climb] to push a
//!    completed parsed expression onto the node stack and parse the next token
//!    using `parse_infix()`.
//! 4. Call [`try_continue()`][ParserSession::try_continue] to invoke the
//!    continuation function from the top context on the context stack.


pub(crate) mod parselet;
pub(crate) mod operators;
mod token_parselets;
mod parser_session;


use std::fmt::Debug;

use crate::{
    cst::{
        BinaryNode, BinaryOperator, CompoundNode, CompoundOperator, Cst,
        CstSeq, TernaryNode, TernaryOperator, TriviaSeq,
    },
    feature,
    panic_if_aborted,
    // parselet::Parselet,
    precedence::Precedence,

    tokenize::{
        token_kind::Closer,
        tokenizer::{Tokenizer, Tokenizer_currentToken_stringifyAsFile},
        TokenKind, TokenRef, TokenStr,
    },
    FirstLineBehavior,
    NodeSeq,
};

use self::{
    parselet::{InfixParselet, ParseFunction, ParseletPtr, PrefixParselet},
    parser_session::TriviaSeqRef,
    token_parselets::{INFIX_PARSELETS, PREFIX_PARSELETS},
};

pub use self::parser_session::ParseResult;

pub(crate) use self::parser_session::ParserSession;


#[derive(Debug)]
pub(crate) struct Context {
    f: Option<ParseFunction>,
    p: Option<ParseletPtr>,

    /// The position in [`ParserSession.NodeStack`][ParserSession::NodeStack]
    /// that marks the first node associated with this [`Context`].
    index: usize,

    prec: Option<Precedence>,
}

pub(crate) enum ColonLHS {
    Pattern,
    Optional,
    Error,
}

impl Context {
    pub fn new(index: usize, prec: Option<Precedence>) -> Self {
        Context {
            f: None,
            p: None,
            index,
            prec,
        }
    }

    pub(crate) fn init_callback(&mut self, func: ParseFunction) {
        assert!(self.f.is_none());
        assert!(self.p.is_none());

        self.f = Some(func);
        self.p = None;
    }

    pub(crate) fn init_callback_with_parselet(
        &mut self,
        func: ParseFunction,
        parselet: ParseletPtr,
    ) {
        assert!(self.f.is_none());
        assert!(self.p.is_none());

        self.f = Some(func);
        self.p = Some(parselet);
    }

    pub(crate) fn init_identity(&mut self) {
        self.init_callback(Parser_identity);
    }

    pub(crate) fn set_callback(&mut self, func: ParseFunction) {
        assert!(self.f.is_some());
        self.f = Some(func);
    }

    pub(crate) fn set_callback_2(
        &mut self,
        func: ParseFunction,
        parselet: ParseletPtr,
    ) {
        // TODO: Should `f` already have some value in this case?
        self.f = Some(func);
        self.p = Some(parselet);
    }

    pub(crate) fn is_identity(&self) -> bool {
        self.f == Some(Parser_identity)
    }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(
        &mut self,
        prec: P,
    ) {
        self.prec = prec.into();
    }
}


fn Parser_identity<'i>(_: &mut ParserSession<'i>, _: ParseletPtr) {
    return;
}

pub(crate) fn Parser_handleFirstLine<'i>(session: &mut Tokenizer<'i>) {
    let firstLineBehavior = session.firstLineBehavior;

    match firstLineBehavior {
        FirstLineBehavior::NotScript => {
            return;
        },
        FirstLineBehavior::Check => {
            //
            // Handle the optional #! shebang
            //

            let mut peek = session.peek_token();

            if peek.tok != TokenKind::Hash {
                // not #!

                return;
            }

            peek.skip(session);

            peek = session.peek_token();

            if peek.tok != TokenKind::Bang {
                // not #!

                return;
            }

            //
            // Definitely a shebang
            //

            peek.skip(session);

            loop {
                if feature::CHECK_ABORT && crate::abortQ() {
                    break;
                }

                let peek = session.peek_token();

                if peek.tok == TokenKind::EndOfFile {
                    break;
                }

                if peek.tok == TokenKind::ToplevelNewline {
                    peek.skip(session);

                    break;
                }

                peek.skip(session);
            } // while (true)

            //
            // TODO: if anyone ever asks, then consider providing the shebang as a token
            // but only after BIGCODEMERGE!!
            //
        },
        FirstLineBehavior::Script => {
            //
            // Handle the #! shebang
            //

            let mut peek = session.peek_token();

            if peek.tok != TokenKind::Hash {
                //
                // TODO: add to Issues
                //

                return;
            }

            peek.skip(session);

            peek = session.peek_token();

            if peek.tok != TokenKind::Bang {
                //
                // TODO: add to Issues
                //

                return;
            }

            peek.skip(session);

            loop {
                if feature::CHECK_ABORT && crate::abortQ() {
                    break;
                }

                let peek = session.peek_token();

                if peek.tok == TokenKind::EndOfFile {
                    break;
                }

                if peek.tok == TokenKind::ToplevelNewline {
                    peek.skip(session);

                    break;
                }

                peek.skip(session);
            } // while (true)
        },
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

    /// Pop the top context and push a new node constructed by `func`, then
    /// call [`ParserSession::parse_climb()`].
    pub(crate) fn reduce_and_climb<N, F>(&mut self, func: F)
    where
        N: Into<Cst<TokenStr<'i>>>,
        F: FnOnce(CstSeq<TokenStr<'i>>) -> N,
    {
        self.reduce(func);

        // MUSTTAIL
        return self.parse_climb();
    }

    /// Pop the top context and push a new node constructed by `func`.
    pub(crate) fn reduce<N, F>(&mut self, func: F)
    where
        N: Into<Cst<TokenStr<'i>>>,
        F: FnOnce(CstSeq<TokenStr<'i>>) -> N,
    {
        // Remove the top context
        let ctxt = self
            .ContextStack
            .pop()
            .expect("context stack was unexpectedly empty");

        // Remove nodes associated with `ctxt` from back of NodeStack
        let nodes = Vec::from_iter(self.NodeStack.drain(ctxt.index..));

        debug_assert_eq!(self.NodeStack.len(), ctxt.index);

        // "Reduce" the nodes associated with the popped context using
        // the provided callback.
        let node = func(NodeSeq(nodes));

        self.push_node(node);
    }

    pub(crate) fn push_and_climb<T: Into<Cst<TokenStr<'i>>>>(
        &mut self,
        node: T,
    ) {
        self.NodeStack.push(node.into());
        self.parse_climb();
    }

    /// A complete expression was just finished being parsed, so "climb" up by
    /// parsing the next "infix" token in the input.
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
        if self.ContextStack.is_empty() {
            // no call needed here
            return;
        }

        let ctxt: &mut Context = self.top_context();

        let F = ctxt.f.expect("Ctxt.f is unexpectedly None");
        let P = ctxt
            .p
            .unwrap_or_else(|| &self::parselet::PrefixAssertFalseParselet {});

        // MUSTTAIL
        return F(self, P);
    }

    //======================================
    // Get current token after eating trivia
    //======================================

    /// Get the current token, eating trivia tokens.
    ///
    /// If the current token is already a non-trivia token, it will be returned.
    ///
    /// Otherwise, repeatedly eat the current token and append it to
    /// [`ParserSession::NodeStack`] until the current token is no longer a
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
            self.NodeStack.push(Cst::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    fn eat_trivia_stringify_as_file(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTrivia() {
            self.NodeStack.push(Cst::Token(token.clone()));

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
            self.NodeStack.push(Cst::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    //==================================
    // Context management
    //==================================

    /// Push a new context with associated precedence value.
    ///
    /// The top node in the [`NodeStack`][ParserSession::NodeStack] is included
    /// in the new context.
    pub(crate) fn push_context<'s, P: Into<Option<Precedence>>>(
        &'s mut self,
        prec: P,
    ) -> &'s mut Context {
        let prec = prec.into();

        assert!(!self.NodeStack.is_empty());

        self.ContextStack
            .push(Context::new(self.NodeStack.len() - 1, prec));

        return self.ContextStack.last_mut().unwrap();
    }

    pub(crate) fn top_context<'s>(&'s mut self) -> &'s mut Context {
        assert!(!self.ContextStack.is_empty());

        return self.ContextStack.last_mut().unwrap();
    }

    //==================================
    // Precedence management
    //==================================

    pub(crate) fn top_precedence(&mut self) -> Option<Precedence> {
        match self.ContextStack.last() {
            Some(ctxt) => ctxt.prec,
            None => None,
        }
    }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(
        &mut self,
        prec: P,
    ) {
        let prec = prec.into();

        assert!(!self.ContextStack.is_empty());

        let ctxt: &mut _ = self.ContextStack.last_mut().unwrap();

        ctxt.prec = prec;
    }

    //==================================
    // Node stack management
    //==================================

    pub(crate) fn push_leaf(&mut self, token: TokenRef<'i>) {
        self.NodeStack.push(Cst::Token(token));
    }

    pub(crate) fn push_leaf_and_next(&mut self, token: TokenRef<'i>) {
        self.NodeStack.push(Cst::Token(token));

        token.skip(&mut self.tokenizer);
    }

    pub(crate) fn push_trivia_seq(&mut self, seq: TriviaSeqRef<'i>) {
        //
        // Move all trivia from Seq to back of ArgsStack
        //
        let TriviaSeq(vec) = seq;

        self.NodeStack.extend(vec.into_iter().map(Cst::Token));
    }

    pub(crate) fn push_node<N>(&mut self, node: N)
    where
        N: Into<Cst<TokenStr<'i>>>,
    {
        let node = node.into();
        self.NodeStack.push(node);
    }

    pub(crate) fn pop_node(&mut self) -> Cst<TokenStr<'i>> {
        assert!(!self.NodeStack.is_empty());

        let top = self.NodeStack.pop().unwrap();

        return top;
    }

    #[cfg(test)]
    pub(crate) fn top_node<'s>(&'s mut self) -> &'s mut Cst<TokenStr<'i>> {
        assert!(!self.NodeStack.is_empty());

        return self.NodeStack.last_mut().unwrap();
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

    pub(crate) fn check_pattern_precedence(&self) -> bool {
        for ctxt in self.ContextStack.iter().rev() {
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

    pub(crate) fn check_colon_lhs(&self) -> ColonLHS {
        //
        // work backwards, looking for a symbol or something that is a pattern
        //
        // skip any trivia
        //

        let ctxt = self.ContextStack.last().unwrap();

        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        let top_non_trivia_in_context = (&self.NodeStack[ctxt.index..])
            .iter()
            .rev()
            .find(
                |cst| !matches!(cst, Cst::Token(token) if token.tok.isTrivia()),
            )
            .expect(
                "unable to check colon LHS: no non-trivia token in top context",
            );

        match top_non_trivia_in_context {
            Cst::Binary(BinaryNode(op)) => {
                //
                // Something like  a:b:c
                //                  ^ Pattern
                //                    ^ Optional
                //

                if op.op == BinaryOperator::Pattern {
                    return ColonLHS::Optional;
                }

                return ColonLHS::Error;
            },

            Cst::Compound(CompoundNode(op)) => {
                //
                // Something like  a_:b
                //                   ^ Optional
                //

                match op.op {
                    CompoundOperator::CodeParser_PatternBlank
                    | CompoundOperator::CodeParser_PatternBlankSequence
                    | CompoundOperator::CodeParser_PatternBlankNullSequence
                    | CompoundOperator::Blank
                    | CompoundOperator::BlankSequence
                    | CompoundOperator::BlankNullSequence => {
                        return ColonLHS::Optional;
                    },
                    _ => return ColonLHS::Error,
                }
            },

            Cst::Token(tok) => {
                match tok.tok {
                    TokenKind::Symbol => {
                        //
                        // Something like  a:b
                        //                  ^ Pattern
                        //

                        return ColonLHS::Pattern;
                    },
                    TokenKind::Under
                    | TokenKind::UnderUnder
                    | TokenKind::UnderUnderUnder => {
                        //
                        // Something like  _:b
                        //                  ^ Optional
                        //

                        return ColonLHS::Optional;
                    },
                    TokenKind::Colon => {
                        panic!("Fix at call site")
                    },
                    _ => (),
                }

                if tok.tok.isError() {
                    //
                    // allow errors to be on LHS of :
                    //
                    // This is a bit confusing. The thinking is that since there is already an error, then we do not need to introduce another error.
                    //
                    return ColonLHS::Pattern;
                }

                return ColonLHS::Error;
            },
            _ => return ColonLHS::Error,
        }
    }

    pub(crate) fn top_non_trivia_node_is_tilde(&self) -> bool {
        //
        // work backwards, looking for ~
        //

        if self.ContextStack.is_empty() {
            return false;
        }

        let ctxt = self.ContextStack.last().unwrap();

        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        let top_non_trivia_in_context = (&self.NodeStack[ctxt.index..])
            .iter()
            .rev()
            // Skip past top
            .skip(1)
            .find(
                |cst| !matches!(cst, Cst::Token(token) if token.tok.isTrivia()),
            )
            .expect(
                "unable to check tilde: no non-trivia token in top context",
            );

        if let Cst::Token(tok) = top_non_trivia_in_context {
            if tok.tok == TokenKind::Tilde {
                return true;
            }
        }

        return false;
    }

    pub(crate) fn top_node_is_span(&self) -> bool {
        assert!(!self.NodeStack.is_empty());

        let top_node: &Cst<_> = self.NodeStack.last().unwrap();

        // Note: This method should only be called in process_implicit_times(),
        //       which itself should only be called after non-newline trivia has
        //       been eaten.
        debug_assert!(
            !matches!(top_node, Cst::Token(tok) if tok.tok.isTriviaButNotToplevelNewline())
        );

        match top_node {
            // This is a BinaryNode of Span
            Cst::Binary(BinaryNode(node))
                if node.op == BinaryOperator::Span =>
            {
                true
            },
            // This is a TernaryNode of Span
            Cst::Ternary(TernaryNode(node))
                if node.op == TernaryOperator::Span =>
            {
                true
            },
            _ => false,
        }
    }

    pub fn is_quiescent(&mut self) -> bool {
        assert!(self.NodeStack.is_empty());
        assert!(self.ContextStack.is_empty());
        assert!(self.tokenizer.GroupStack.is_empty());

        return true;
    }
}
