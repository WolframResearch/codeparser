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
    parser_session::{NodeStack, ParserSession, TriviaSeq},
    precedence::{Precedence, *},
    source::{NextPolicy, TOPLEVEL},

    token::{BorrowedTokenInput, TokenKind, TokenRef},
    token_enum::Closer,
    tokenizer::{Tokenizer, Tokenizer_currentToken, Tokenizer_currentToken_stringifyAsFile},
    FirstLineBehavior,
    NodeSeq,
};

pub struct Context {
    pub(crate) f: Option<ParseFunction>,
    pub(crate) p: Option<ParseletPtr>,

    /// The position in [`ParserSession.NodeStack`][ParserSession::NodeStack]
    /// that marks the first node associated with this [`Context`].
    index: usize,

    pub(crate) prec: Precedence,
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
    pub fn new(index: usize, prec: Precedence) -> Self {
        Context {
            f: None,
            p: None,
            index,
            prec,
        }
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

            let mut peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TokenKind::Hash {
                // not #!

                //
                // reset
                //
                peek.reset(session);

                return;
            }

            peek.skip(session);

            peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TokenKind::Bang {
                // not #!

                //
                // reset
                //
                peek.reset(session);

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

                let peek = Tokenizer_currentToken(session, TOPLEVEL);

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

            let mut peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TokenKind::Hash {
                //
                // TODO: add to Issues
                //

                return;
            }

            peek.skip(session);

            peek = Tokenizer_currentToken(session, TOPLEVEL);

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

                let peek = Tokenizer_currentToken(session, TOPLEVEL);

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

        let mut token = Tokenizer_currentToken(&mut self.tokenizer, TOPLEVEL);

        //
        // not in the middle of parsing anything, so toplevel newlines will delimit
        //
        self.eat_trivia_but_not_toplevel_newlines_2(
            &mut token,
            TOPLEVEL,
            &mut Trivia1.borrow_mut(),
        );

        let mut I: &dyn InfixParselet = INFIX_PARSELETS[usize::from(token.tok.value())];

        token = I.processImplicitTimes(self, token);

        I = INFIX_PARSELETS[usize::from(token.tok.value())];

        let TokenPrecedence = I.getPrecedence(self);

        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   break;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   break;
        //

        if (self.top_precedence() | 0x1) > TokenPrecedence {
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

    pub(crate) fn eat_trivia(&mut self, token: &mut TokenRef<'i>, policy: NextPolicy) {
        while token.tok.isTrivia() {
            self.NodeStack.push(Node::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = Tokenizer_currentToken(&mut self.tokenizer, policy);
        }
    }

    /// [`ParserSession::eat_trivia`], but the fields of [`ParserSession`] used are
    /// transparent to the borrow checker.
    pub(crate) fn eat_trivia_transparent(
        node_stack: &mut NodeStack<'i>,
        tokenizer: &mut Tokenizer<'i>,
        token: &mut TokenRef<'i>,
        policy: NextPolicy,
    ) {
        while token.tok.isTrivia() {
            node_stack.push(Node::Token(token.clone()));

            token.skip(tokenizer);

            *token = Tokenizer_currentToken(tokenizer, policy);
        }
    }

    pub(crate) fn eat_trivia_2(
        &mut self,
        token: &mut TokenRef<'i>,
        policy: NextPolicy,
        Args: &mut TriviaSeq<'i>,
    ) {
        while token.tok.isTrivia() {
            Args.push(token.clone());

            token.skip(&mut self.tokenizer);

            *token = Tokenizer_currentToken(&mut self.tokenizer, policy);
        }
    }

    pub(crate) fn eat_trivia_stringify_as_file(&mut self, token: &mut TokenRef<'i>) {
        while token.tok.isTrivia() {
            self.NodeStack.push(Node::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = Tokenizer_currentToken_stringifyAsFile(&mut self.tokenizer);
        }
    }

    pub(crate) fn eat_trivia_but_not_toplevel_newlines(
        &mut self,
        token: &mut TokenRef<'i>,
        policy: NextPolicy,
    ) {
        while token.tok.isTriviaButNotToplevelNewline() {
            self.NodeStack.push(Node::Token(token.clone()));

            token.skip(&mut self.tokenizer);

            *token = Tokenizer_currentToken(&mut self.tokenizer, policy);
        }
    }

    pub(crate) fn eat_trivia_but_not_toplevel_newlines_2(
        &mut self,
        token: &mut TokenRef<'i>,
        policy: NextPolicy,
        Args: &mut TriviaSeq<'i>,
    ) {
        while token.tok.isTriviaButNotToplevelNewline() {
            Args.push(token.clone().into());

            token.skip(&mut self.tokenizer);

            *token = Tokenizer_currentToken(&mut self.tokenizer, policy);
        }
    }

    pub(crate) fn push_context<'s>(&'s mut self, prec: Precedence) -> &'s mut Context {
        assert!(!self.NodeStack.is_empty());

        self.ContextStack
            .push(Context::new(self.NodeStack.len() - 1, prec));

        return self.ContextStack.last_mut().unwrap();
    }

    /// Push a context, transparently to the borrow checker.
    pub(crate) fn push_context_transparent<'c>(
        node_stack: &mut NodeStack,
        context_stack: &'c mut Vec<Context>,
        prec: Precedence,
    ) -> &'c mut Context {
        assert!(!node_stack.is_empty());

        context_stack.push(Context::new(node_stack.len() - 1, prec));

        return context_stack.last_mut().unwrap();
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

    pub(crate) fn top_precedence(&mut self) -> Precedence {
        if self.ContextStack.is_empty() {
            return PRECEDENCE_LOWEST;
        }

        return self.ContextStack.last().unwrap().prec;
    }

    pub(crate) fn set_precedence(&mut self, prec: Precedence) {
        assert!(!self.ContextStack.is_empty());

        let ctxt: &mut _ = self.ContextStack.last_mut().unwrap();

        ctxt.prec = prec;
    }

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

    pub(crate) fn push_group(&mut self, closer: Closer) {
        self.tokenizer.GroupStack.push(closer);
    }

    pub(crate) fn pop_group(&mut self) {
        assert!(!self.tokenizer.GroupStack.is_empty());

        self.tokenizer.GroupStack.pop();
    }

    pub(crate) fn check_group(&mut self, closer: Closer) -> bool {
        for value in self.tokenizer.GroupStack.iter().rev() {
            if *value == closer {
                return true;
            }
        }

        return false;
    }

    pub(crate) fn check_pattern_precedence(&mut self) -> bool {
        for ctxt in self.ContextStack.iter().rev() {
            let prec = ctxt.prec;

            if prec > PRECEDENCE_FAKE_PATTERNCOLON {
                continue;
            }

            if prec < PRECEDENCE_FAKE_PATTERNCOLON {
                return false;
            }

            assert!(prec == PRECEDENCE_FAKE_PATTERNCOLON);

            return true;
        }

        return false;
    }

    pub(crate) fn check_colon_lhs(&mut self) -> ColonLHS {
        //
        // work backwards, looking for a symbol or something that is a pattern
        //

        //
        // skip any trivia
        //

        let ctxt = self.ContextStack.last().unwrap();

        let mut i: usize = self.NodeStack.len() - 1;
        while i >= ctxt.index {
            if let Node::Token(tok) = self.NodeStack[i] {
                if tok.tok.isTrivia() {
                    i -= 1;
                    continue;
                }

                break;
            }

            break;
        }

        // if i == ctxt.index.checked_sub(1).expect("subtracted from 0 unsigned number") {
        //     panic!();
        // }

        if Some(i) == ctxt.index.checked_sub(1) {
            panic!();
        }

        match &self.NodeStack[i] {
            Node::Binary(BinaryNode(op)) => {
                //
                // Something like  a:b:c
                //                  ^ Pattern
                //                    ^ Optional
                //

                let op = op.getOp();

                if op == BinaryOperator::Pattern {
                    return ColonLHS::Optional;
                }

                return ColonLHS::Error;
            },

            Node::Compound(CompoundNode(op)) => {
                //
                // Something like  a_:b
                //                   ^ Optional
                //

                let op = op.getOp();

                match op {
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

    pub(crate) fn check_tilde(&mut self) -> bool {
        //
        // work backwards, looking for ~
        //

        if self.ContextStack.is_empty() {
            return false;
        }

        let ctxt = self.ContextStack.last().unwrap();

        let mut i: usize = self.NodeStack.len() - 1;

        //
        // skip past top
        //
        i -= 1;

        //
        // skip any trivia
        //
        while i >= ctxt.index {
            if let Node::Token(tok) = self.NodeStack[i] {
                if tok.tok.isTrivia() {
                    i -= 1;
                    continue;
                }

                break;
            }

            break;
        }

        // if i == ctxt.index - 1 {
        //     panic!();
        // }

        if Some(i) == ctxt.index.checked_sub(1) {
            panic!();
        }

        if let Node::Token(tok) = self.NodeStack[i] {
            if tok.tok == TokenKind::Tilde {
                return true;
            }
        }

        return false;
    }

    pub(crate) fn check_span(&mut self) -> bool {
        assert!(!self.NodeStack.is_empty());

        let N: &mut Node<_> = self.NodeStack.last_mut().unwrap();

        {
            let NN = N;

            if let Node::Binary(BinaryNode(B)) = NN {
                let op = B.getOp();

                if op == BinaryOperator::Span {
                    return true;
                }

                //
                // there is a BinaryNode, but it is not a Span
                //

                return false;
            }

            if let Node::Ternary(TernaryNode(op)) = NN {
                let op = op.getOp();

                if op == TernaryOperator::Span {
                    return true;
                }

                //
                // there is a TernaryNode, but it is not a Span
                //

                return false;
            }
        }

        return false;
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
