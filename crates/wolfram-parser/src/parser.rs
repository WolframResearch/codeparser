mod parser_session;


use std::fmt::{self, Debug};

use crate::{
    cst::{
        BinaryNode, BinaryOperator, CompoundNode, CompoundOperator, CstNode, CstNodeSeq, Node,
        TernaryNode, TernaryOperator,
    },
    feature,
    panic_if_aborted,
    parselet::{InfixParselet, ParseFunction, ParseletPtr, PrefixParselet},
    parselet_registration::INFIX_PARSELETS,
    // parselet::Parselet,
    precedence::Precedence,

    token::{BorrowedTokenInput, TokenKind, TokenRef},
    token_enum::Closer,
    tokenizer::{Tokenizer, Tokenizer_currentToken_stringifyAsFile},
    FirstLineBehavior,
    NodeSeq,
};

use self::parser_session::TriviaSeq;


pub use self::parser_session::ParseResult;

pub(crate) use self::parser_session::ParserSession;


pub(crate) struct Context {
    f: Option<ParseFunction>,
    p: Option<ParseletPtr>,

    /// The position in [`ParserSession.NodeStack`][ParserSession::NodeStack]
    /// that marks the first node associated with this [`Context`].
    index: usize,

    prec: Option<Precedence>,
}

impl Debug for Context {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let Context { f, p, index, prec } = self;

        fmt.debug_struct("Context")
            .field("f", &f.is_some())
            .field("p", p)
            .field("index", index)
            .field("prec", prec)
            .finish()
    }
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

    pub(crate) fn init_callback(&mut self, func: ParseFunction, parselet: Option<ParseletPtr>) {
        assert!(self.f.is_none());
        assert!(self.p.is_none());

        self.f = Some(func);
        self.p = parselet;
    }

    pub(crate) fn set_callback(&mut self, func: ParseFunction) {
        assert!(self.f.is_some());
        self.f = Some(func);
    }

    pub(crate) fn set_callback_2(&mut self, func: ParseFunction, parselet: ParseletPtr) {
        // TODO: Should `f` already have some value in this case?
        self.f = Some(func);
        self.p = Some(parselet);
    }

    pub(crate) fn is_identity(&self) -> bool {
        self.f == Some(Parser_identity)
    }

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(&mut self, prec: P) {
        self.prec = prec.into();
    }
}


pub(crate) fn Parser_identity<'i>(_: &mut ParserSession<'i>, _: ParseletPtr) {
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

impl<'i> ParserSession<'i> {
    /// Lookup and apply the [`PrefixParselet`] implementation associated
    /// with the [`TokenKind`] of `token`.
    // TODO(cleanup): Rename to avoid ambiguity with PrefixParselet::parse_prefix()?
    pub(crate) fn parse_prefix(&mut self, token: TokenRef<'i>) {
        let parselet: &dyn PrefixParselet = crate::parselet::prefix_parselet(token.tok);

        // MUSTTAIL
        parselet.parse_prefix(self, token)
    }

    /// Pop the top context and push a new node constructed by `func`, then
    /// call [`ParserSession::parse_climb()`].
    pub(crate) fn reduce_and_climb<N, F>(&mut self, func: F)
    where
        N: Into<CstNode<BorrowedTokenInput<'i>>>,
        F: FnOnce(CstNodeSeq<BorrowedTokenInput<'i>>) -> N,
    {
        let context = self.pop_context();
        let node = func(context).into();
        self.push_node(node);

        // MUSTTAIL
        return self.parse_climb();
    }

    pub(crate) fn parse_climb(&mut self) {
        //
        // Check isAbort() inside loops
        //
        panic_if_aborted!();

        let Trivia1 = self.trivia1.clone();

        //
        // not in the middle of parsing anything, so toplevel newlines will delimit
        //
        let mut token =
            self.current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia1.borrow_mut());

        let mut I: &dyn InfixParselet = INFIX_PARSELETS[usize::from(token.tok.value())];

        token = I.process_implicit_times(self, token);

        I = INFIX_PARSELETS[usize::from(token.tok.value())];

        let TokenPrecedence = I.getPrecedence(self);

        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   break;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   break;
        //

        if Precedence::greater(self.top_precedence(), TokenPrecedence) {
            Trivia1.borrow_mut().reset(&mut self.tokenizer);

            // MUSTTAIL
            return self.try_continue();
        }

        self.push_context(TokenPrecedence);

        self.push_trivia_seq(&mut Trivia1.borrow_mut());

        // MUSTTAIL
        return I.parse_infix(self, token);
    }

    pub(crate) fn try_continue(&mut self) {
        if self.is_context_stack_empty() {
            // no call needed here
            return;
        }

        let ctxt: &mut Context = self.top_context();

        let F = ctxt.f.expect("Ctxt.f is unexpectedly None");
        let P = ctxt
            .p
            .unwrap_or_else(|| &crate::parselet::PrefixAssertFalseParselet {});

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
        container: &mut TriviaSeq<'i>,
    ) -> TokenRef<'i> {
        let mut tok = self.tokenizer.peek_token();

        self.eat_trivia_into(&mut tok, container);

        debug_assert!(!tok.tok.isTrivia());

        tok
    }

    pub(crate) fn current_token_stringify_as_file_eat_trivia(&mut self) -> TokenRef<'i> {
        let mut tok = Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);

        self.eat_trivia_stringify_as_file(&mut tok);

        debug_assert!(!tok.tok.isTrivia());

        tok
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines(&mut self) -> TokenRef<'i> {
        let mut tok = self.tokenizer.peek_token();

        //
        // CompoundExpression should not cross toplevel newlines
        //
        self.eat_trivia_but_not_toplevel_newlines(&mut tok);

        tok
    }

    pub(crate) fn current_token_eat_trivia_but_not_toplevel_newlines_into(
        &mut self,
        container: &mut TriviaSeq<'i>,
    ) -> TokenRef<'i> {
        let mut tok = self.tokenizer.peek_token();

        //
        // CompoundExpression should not cross toplevel newlines
        //
        self.eat_trivia_but_not_toplevel_newlines_into(&mut tok, container);

        tok
    }

    //----------------------------------
    // Eat trivia helpers
    //----------------------------------
    // TODO(cleanup): Inline these functions into their currently only callsite?

    fn eat_trivia(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTrivia() {
            self.NodeStack.push(Node::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    fn eat_trivia_into(&mut self, token: &mut TokenRef<'i>, Args: &mut TriviaSeq<'i>) {
        while token.tok.isTrivia() {
            Args.push(token.clone());

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    fn eat_trivia_stringify_as_file(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTrivia() {
            self.NodeStack.push(Node::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);
        }
    }

    fn eat_trivia_but_not_toplevel_newlines(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTriviaButNotToplevelNewline() {
            self.NodeStack.push(Node::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    fn eat_trivia_but_not_toplevel_newlines_into(
        &mut self,
        token: &mut TokenRef<'i>,
        Args: &mut TriviaSeq<'i>,
    ) {
        while token.tok.isTriviaButNotToplevelNewline() {
            Args.push(token.clone().into());

            token.skip(&mut self.tokenizer);

            *token = self.tokenizer.peek_token();
        }
    }

    //==================================
    // Context management
    //==================================

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

    /// Removes and returns the sequence of nodes associated with the top context.
    pub(crate) fn pop_context(&mut self) -> CstNodeSeq<BorrowedTokenInput<'i>> {
        assert!(!self.ContextStack.is_empty());

        //
        // get the top Context
        //

        let ctxt = self.ContextStack.pop().unwrap();

        //
        // Remove args from back of NodeStack
        //

        let vec = Vec::from_iter(self.NodeStack.drain(ctxt.index..));

        debug_assert_eq!(self.NodeStack.len(), ctxt.index);

        return NodeSeq(vec);
    }

    fn is_context_stack_empty(&mut self) -> bool {
        return self.ContextStack.is_empty();
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

    pub(crate) fn set_precedence<P: Into<Option<Precedence>>>(&mut self, prec: P) {
        let prec = prec.into();

        assert!(!self.ContextStack.is_empty());

        let ctxt: &mut _ = self.ContextStack.last_mut().unwrap();

        ctxt.prec = prec;
    }

    //==================================
    // Node stack management
    //==================================

    pub(crate) fn push_leaf(&mut self, token: TokenRef<'i>) {
        self.NodeStack.push(Node::Token(token));
    }

    pub(crate) fn push_leaf_and_next(&mut self, token: TokenRef<'i>) {
        self.NodeStack.push(Node::Token(token));

        token.skip(&mut self.tokenizer);
    }

    pub(crate) fn push_trivia_seq(&mut self, seq: &mut TriviaSeq<'i>) {
        //
        // Move all trivia from Seq to back of ArgsStack
        //
        let TriviaSeq { vec } = seq;

        self.NodeStack.extend(vec.drain(0..).map(Node::Token));

        //
        // Forget about Seq
        //

        seq.clear();
    }

    pub(crate) fn push_node<N>(&mut self, node: N)
    where
        N: Into<Node<BorrowedTokenInput<'i>>>,
    {
        let node = node.into();
        self.NodeStack.push(node);
    }

    pub(crate) fn pop_node(&mut self) -> Node<BorrowedTokenInput<'i>> {
        assert!(!self.NodeStack.is_empty());

        let top = self.NodeStack.pop().unwrap();

        return top;
    }

    #[cfg(test)]
    pub(crate) fn top_node<'s>(&'s mut self) -> &'s mut Node<BorrowedTokenInput<'i>> {
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
            .find(|cst| !matches!(cst, CstNode::Token(token) if token.tok.isTrivia()))
            .expect("unable to check colon LHS: no non-trivia token in top context");

        match top_non_trivia_in_context {
            Node::Binary(BinaryNode(op)) => {
                //
                // Something like  a:b:c
                //                  ^ Pattern
                //                    ^ Optional
                //

                if op.getOp() == BinaryOperator::Pattern {
                    return ColonLHS::Optional;
                }

                return ColonLHS::Error;
            },

            Node::Compound(CompoundNode(op)) => {
                //
                // Something like  a_:b
                //                   ^ Optional
                //

                match op.getOp() {
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

            Node::Token(tok) => {
                match tok.tok {
                    TokenKind::Symbol => {
                        //
                        // Something like  a:b
                        //                  ^ Pattern
                        //

                        return ColonLHS::Pattern;
                    },
                    TokenKind::Under | TokenKind::UnderUnder | TokenKind::UnderUnderUnder => {
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
            .find(|cst| !matches!(cst, CstNode::Token(token) if token.tok.isTrivia()))
            .expect("unable to check tilde: no non-trivia token in top context");

        if let Node::Token(tok) = top_non_trivia_in_context {
            if tok.tok == TokenKind::Tilde {
                return true;
            }
        }

        return false;
    }

    pub(crate) fn top_node_is_span(&self) -> bool {
        assert!(!self.NodeStack.is_empty());

        let top_node: &CstNode<_> = self.NodeStack.last().unwrap();

        match top_node {
            // This is a BinaryNode of Span
            CstNode::Binary(BinaryNode(node)) if node.getOp() == BinaryOperator::Span => true,
            // This is a TernaryNode of Span
            CstNode::Ternary(TernaryNode(node)) if node.getOp() == TernaryOperator::Span => true,
            _ => false,
        }
    }

    pub fn is_quiescent(&mut self) -> bool {
        assert!(self.NodeStack.is_empty());
        assert!(self.ContextStack.is_empty());
        assert!(self.tokenizer.GroupStack.is_empty());
        assert!(self.trivia1.borrow().is_empty());
        assert!(self.trivia2.borrow().is_empty());

        return true;
    }
}
