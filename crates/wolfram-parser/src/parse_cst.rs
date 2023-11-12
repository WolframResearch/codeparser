use crate::{
    cst::{
        BinaryNode, BinaryOperator, CallBody, CallNode, CompoundNode,
        CompoundOperator, Cst, CstSeq, GroupMissingCloserNode, GroupNode,
        GroupOperator, InfixNode, InfixOperator, OperatorNode, PostfixNode,
        PostfixOperator, PrefixBinaryNode, PrefixBinaryOperator, PrefixNode,
        PrefixOperator, SyntaxErrorKind, SyntaxErrorNode, TernaryNode,
        TernaryOperator, TriviaSeq,
    },
    parse::{ColonLHS, ParseBuilder, TriviaSeqRef, UnderParseData},
    tokenize::{TokenKind, TokenRef, TokenStr},
    utils::debug_assert_matches,
    NodeSeq,
};

#[derive(Debug)]
pub(crate) struct ParseCst<'i> {
    node_stack: Vec<Cst<TokenStr<'i>>>,
    context_stack_mirror: Vec<ParseCstContext>,
}

#[derive(Debug)]
struct ParseCstContext {
    /// The position in [`ParseCst.node_stack`][ParseCst::node_stack]
    /// that marks the first node associated with this context.
    index: usize,
}

impl<'i> ParseCst<'i> {
    pub(crate) fn new() -> Self {
        ParseCst {
            node_stack: Vec::new(),
            context_stack_mirror: Vec::new(),
        }
    }

    /// Pop the top context and push a new node constructed by `func`.
    fn reduce<N, F>(&mut self, func: F)
    where
        N: Into<Cst<TokenStr<'i>>>,
        F: FnOnce(CstSeq<TokenStr<'i>>) -> N,
    {
        let ctxt = self
            .context_stack_mirror
            .pop()
            .expect("reduce: context stack is empty");

        // Remove nodes associated with `ctxt` from back of node_stack
        let nodes = Vec::from_iter(self.node_stack.drain(ctxt.index..));

        debug_assert_eq!(self.node_stack.len(), ctxt.index);

        // "Reduce" the nodes associated with the popped context using
        // the provided callback.
        let node = func(NodeSeq(nodes));

        let node = node.into();

        self.node_stack.push(node);
    }

    fn push_node<N>(&mut self, node: N)
    where
        N: Into<Cst<TokenStr<'i>>>,
    {
        let node = node.into();
        self.node_stack.push(node);
    }

    fn pop_node(&mut self) -> Cst<TokenStr<'i>> {
        debug_assert!(!self.node_stack.is_empty());

        let top = self.node_stack.pop().unwrap();

        return top;
    }

    #[cfg(test)]
    pub(crate) fn top_node<'s>(&'s mut self) -> &'s mut Cst<TokenStr<'i>> {
        assert!(!self.node_stack.is_empty());

        return self.node_stack.last_mut().unwrap();
    }

    /// Returns a last-in first-out iterator over nodes in the top
    /// context.
    fn top_context_nodes(&self) -> impl Iterator<Item = &Cst<TokenStr<'i>>> {
        let ctxt = self
            .context_stack_mirror
            .last()
            .expect("top_context_nodes: empty context stack");

        let index = ctxt.index;

        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        (&self.node_stack[index..]).iter().rev()
    }
}

impl<'i> ParseBuilder<'i> for ParseCst<'i> {
    type Output = Cst<TokenStr<'i>>;

    //==================================
    // Context management
    //==================================

    /// Push a new context with associated precedence value.
    ///
    /// The top node in the [`node_stack`][ParserSession::node_stack] is included
    /// in the new context.
    fn begin_context<'s>(&'s mut self) {
        assert!(!self.node_stack.is_empty());

        let data = ParseCstContext {
            index: self.node_stack.len() - 1,
        };

        self.context_stack_mirror.push(data);
    }

    fn is_quiescent(&self) -> bool {
        self.node_stack.is_empty()
    }

    //==================================
    // Push
    //==================================

    fn push_leaf(&mut self, token: TokenRef<'i>) {
        debug_assert!(!token.tok.isTrivia());

        self.node_stack.push(Cst::Token(token))
    }

    fn push_trivia(&mut self, token: TokenRef<'i>) {
        debug_assert!(token.tok.isTrivia());

        self.node_stack.push(Cst::Token(token))
    }

    fn push_trivia_seq(&mut self, seq: TriviaSeqRef<'i>) {
        //
        // Move all trivia from Seq to back of ArgsStack
        //
        let TriviaSeq(vec) = seq;

        self.node_stack.extend(vec.into_iter().map(Cst::Token));
    }

    fn push_compound_pattern_blank(
        &mut self,
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under: UnderParseData<'i>,
    ) {
        debug_assert_eq!(symbol.tok, TokenKind::Symbol);
        // debug_assert!(matches!(under.tok, TokenKind::Under | TokenKind::UnderUnder | TokenKind::UnderUnderUnder));

        let under = under.into_cst();

        let node = CompoundNode::new3(op, symbol, under);

        self.node_stack.push(Cst::Compound(node))
    }

    fn push_compound_blank(&mut self, under: UnderParseData<'i>) {
        let cst: Cst<_> = under.into_cst();

        self.push_node(cst);
    }

    fn push_compound_pattern_optional(
        &mut self,
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under_dot: TokenRef<'i>,
    ) {
        debug_assert_eq!(symbol.tok, TokenKind::Symbol);
        debug_assert_eq!(under_dot.tok, TokenKind::UnderDot);

        let node = CompoundNode::new2(op, symbol, under_dot);

        self.node_stack.push(Cst::Compound(node));
    }

    fn push_compound_slot(
        &mut self,
        op: CompoundOperator,
        hash: TokenRef<'i>,
        arg: TokenRef<'i>,
    ) {
        debug_assert_matches!(
            op,
            CompoundOperator::Slot | CompoundOperator::SlotSequence
        );
        debug_assert_eq!(hash.tok, TokenKind::Hash);
        debug_assert_matches!(arg.tok, TokenKind::Integer | TokenKind::String);

        let node = CompoundNode::new2(op, hash, arg);

        self.node_stack.push(Cst::Compound(node));
    }

    fn push_compound_out(
        &mut self,
        op: CompoundOperator,
        percent: TokenRef<'i>,
        integer: TokenRef<'i>,
    ) {
        debug_assert_eq!(op, CompoundOperator::Out);
        debug_assert_eq!(percent.tok, TokenKind::Percent);
        debug_assert_matches!(integer.tok, TokenKind::Integer);

        let node = CompoundNode::new2(op, percent, integer);

        self.node_stack.push(Cst::Compound(node));
    }

    fn push_prefix_get(
        &mut self,
        op: PrefixOperator,
        tok1: TokenRef<'i>,
        trivia: TriviaSeqRef<'i>,
        tok2: TokenRef<'i>,
    ) {
        debug_assert_eq!(op, PrefixOperator::Get);

        let node = PrefixNode::new2(op, tok1, trivia, tok2);

        self.node_stack.push(Cst::Prefix(node));
    }

    //==================================
    // Reduce
    //==================================

    fn reduce_prefix(&mut self, op: PrefixOperator) {
        self.reduce(|ctx| PrefixNode::new(op, ctx))
    }

    fn reduce_infix(&mut self, op: InfixOperator) {
        self.reduce(|ctx| InfixNode::new(op, ctx));
    }

    fn reduce_postfix(&mut self, op: PostfixOperator) {
        self.reduce(|ctx| PostfixNode::new(op, ctx));
    }

    fn reduce_binary(&mut self, op: BinaryOperator) {
        self.reduce(|ctx| BinaryNode::new(op, ctx))
    }

    fn reduce_ternary(&mut self, op: TernaryOperator) {
        self.reduce(|ctx| TernaryNode::new(op, ctx))
    }

    fn reduce_prefix_binary(&mut self, op: PrefixBinaryOperator) {
        self.reduce(|ctx| PrefixBinaryNode::new(op, ctx))
    }

    fn reduce_group(&mut self, op: GroupOperator) {
        self.reduce(|ctx| GroupNode::new(op, ctx))
    }

    fn reduce_call(&mut self) {
        let body = self.pop_node();

        let body: CallBody<_> = match body {
            Cst::Group(group) => {
                let GroupNode(OperatorNode { op, children}) = group;

                let op = op.try_to_call_operator().expect("expected call group to be a valid CallOperator");

                let group = GroupNode(OperatorNode {
                    op, children
                });

                CallBody::Group(group)
            },
            Cst::GroupMissingCloser(group) => {
                let GroupMissingCloserNode(OperatorNode { op, children }) = group;

                let op = op.try_to_call_operator().expect("expected call group to be a valid CallOperator");

                let group = GroupMissingCloserNode(OperatorNode {
                    op, children
                });

                CallBody::GroupMissingCloser(group)
            },
            other => panic!(
                "expected CallParselet body to reduce to a Group or GroupMissingCloser node; got: {:#?}",
                other
            ),
        };

        self.reduce(|ctx| CallNode::concrete(ctx, body))
    }

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(&mut self, kind: SyntaxErrorKind) {
        self.reduce(|ctx| SyntaxErrorNode::new(kind, ctx));
    }

    fn reduce_unterminated_group(
        &mut self,
        op: GroupOperator,
        input: &'i str,
        tab_width: usize,
    ) {
        self.reduce(|ctx| {
            crate::error::reparse_unterminated_group_node(
                (op, ctx),
                input,
                tab_width,
            )
        });
    }

    fn reduce_group_missing_closer(&mut self, op: GroupOperator) {
        self.reduce(|ctx| GroupMissingCloserNode::new(op, ctx));
    }

    //==================================
    // Pop
    //==================================

    fn pop_finished_expr(&mut self) -> Self::Output {
        let node = self.pop_node();

        debug_assert!(self.is_quiescent());

        node
    }

    //==================================
    // Properties
    //==================================

    fn check_colon_lhs(&self) -> ColonLHS {
        //
        // work backwards, looking for a symbol or something that is a pattern
        //
        // skip any trivia
        //

        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        let top_non_trivia_in_context = self
            .top_context_nodes()
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

    fn top_non_trivia_node_is_tilde(&self) -> bool {
        //
        // work backwards, looking for ~
        //

        if self.context_stack_mirror.is_empty() {
            return false;
        }

        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        let top_non_trivia_in_context = self
            .top_context_nodes()
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


    fn top_node_is_span(&self) -> bool {
        assert!(!self.node_stack.is_empty());

        let top_node: &Cst<_> = self.node_stack.last().unwrap();

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
}

impl<'i> UnderParseData<'i> {
    pub(crate) fn into_cst(self) -> Cst<TokenStr<'i>> {
        match self {
            UnderParseData::Under(under) => Cst::Token(under),
            UnderParseData::UnderSymbol { op, under, symbol } => {
                Cst::Compound(CompoundNode::new2(op, under, symbol))
            },
        }
    }
}
