use crate::{
    cst::{
        BinaryNode, CallBody, CallNode, CompoundNode, Cst, CstSeq,
        GroupMissingCloserNode, GroupNode, InfixNode, OperatorNode,
        PostfixNode, PrefixBinaryNode, PrefixNode, SyntaxErrorNode,
        TernaryNode, TriviaSeq,
    },
    parse::{
        operators::{
            BinaryOperator, CompoundOperator, GroupOperator, InfixOperator,
            PostfixOperator, PrefixBinaryOperator, PrefixOperator,
            TernaryOperator,
        },
        parselet::{InfixParselet, PrefixParselet},
        token_parselets::{
            token_kind_to_infix_parselet, token_kind_to_prefix_parselet,
        },
        ColonLHS, ParseBuilder, SyntaxErrorData, SyntaxErrorKind, TriviaSeqRef,
        UnderParseData,
    },
    tokenize::{TokenKind, TokenRef, TokenStr},
    utils::debug_assert_matches,
    NodeSeq, ParseOptions,
};

#[derive(Debug)]
pub(crate) struct ParseCst<'i> {
    node_stack: Vec<Cst<TokenStr<'i>>>,

    finished: Vec<Cst<TokenStr<'i>>>,
}

#[derive(Debug)]
pub(crate) struct Context {
    /// Index in [`ParseCst::node_stack`] at which nodes associated with the
    /// current context begin.
    start_index: usize,
}

#[derive(Debug)]
pub(crate) struct InfixParseCst {
    op: InfixOperator,
}

#[derive(Debug)]
pub(crate) struct InfixParseGroup {}

impl<'i> ParseBuilder<'i> for ParseCst<'i> {
    type Node = ();
    type SyntaxTokenNode = ();

    type Output = CstSeq<TokenStr<'i>>;

    type ContextData = Context;

    type InfixParseState = InfixParseCst;
    type GroupParseState = InfixParseGroup;

    //==================================
    // Trivia handling
    //==================================

    type ResettableTriviaAccumulator = Vec<TokenRef<'i>>;
    type ResettableTriviaHandle = TriviaSeqRef<'i>;

    type TriviaHandle = ();

    fn resettable_trivia_begin(&mut self) -> Self::ResettableTriviaAccumulator {
        Vec::new()
    }

    fn resettable_trivia_push(
        &mut self,
        state: &mut Self::ResettableTriviaAccumulator,
        trivia: TokenRef<'i>,
    ) {
        state.push(trivia);
    }

    fn resettable_trivia_end(
        &mut self,
        state: Self::ResettableTriviaAccumulator,
    ) -> Self::ResettableTriviaHandle {
        TriviaSeq(state)
    }

    fn empty_trivia() -> Self::TriviaHandle {}

    fn reset_trivia_seq(
        &mut self,
        trivia: Self::ResettableTriviaHandle,
    ) -> Option<TokenRef<'i>> {
        trivia.0.first().copied()
    }

    fn push_trivia_seq(
        &mut self,
        seq: Self::ResettableTriviaHandle,
    ) -> Self::TriviaHandle {
        //
        // Move all trivia from `seq` to back of `node_stack`
        //
        let TriviaSeq(vec) = seq;

        self.node_stack.extend(vec.into_iter().map(Cst::Token));
    }

    //==================================
    // Lifecycle
    //==================================

    fn new_builder() -> Self {
        ParseCst {
            node_stack: Vec::new(),
            finished: Vec::new(),
        }
    }

    fn with_prefix_parselet<R, F>(kind: TokenKind, callback: F) -> R
    where
        F: FnOnce(&dyn PrefixParselet<'i, Self>) -> R,
    {
        const PREFIX_PARSELETS: [&dyn for<'ii> PrefixParselet<
            'ii,
            ParseCst<'ii>,
        >; TokenKind::COUNT] = crate::utils::from_fn!(
            [&'static dyn for<'ii> PrefixParselet<'ii, ParseCst<'ii>>, TokenKind::COUNT],
            |index: usize| {
                let kind = TokenKind::VARIANTS[index];

                token_kind_to_prefix_parselet!(
                    &dyn for<'ii> PrefixParselet<'ii, ParseCst<'ii>>;
                    kind
                )
            }
        );

        let parselet = &*PREFIX_PARSELETS[usize::from(kind.id())];

        callback(parselet)
    }

    fn with_infix_parselet<R, F: FnOnce(&dyn InfixParselet<'i, Self>) -> R>(
        kind: TokenKind,
        callback: F,
    ) -> R {
        const INFIX_PARSELETS: [&dyn for<'ii> InfixParselet<
            'ii,
            ParseCst<'ii>,
        >; TokenKind::COUNT] = crate::utils::from_fn!(
            [&'static dyn for<'ii> InfixParselet<'ii, ParseCst<'ii>>, TokenKind::COUNT],
            |index: usize| {
                let kind = TokenKind::VARIANTS[index];

                token_kind_to_infix_parselet!(
                    &dyn for<'ii> InfixParselet<'ii, ParseCst<'ii>>;
                    kind
                )
            }
        );

        let parselet = &*INFIX_PARSELETS[usize::from(kind.id())];

        callback(parselet)
    }

    fn finish(self, input: &'i [u8], opts: &ParseOptions) -> Self::Output {
        let ParseCst {
            node_stack,
            finished,
        } = self;

        debug_assert!(
            node_stack.is_empty(),
            "expected empty node stack, got: {node_stack:#?}"
        );

        let mut exprs = NodeSeq(finished);

        if let Ok(input) = std::str::from_utf8(input) {
            exprs = crate::error::reparse_unterminated(
                exprs,
                input,
                usize::try_from(opts.tab_width).unwrap(),
            );
        }

        exprs
    }

    //==================================
    // Context management
    //==================================

    fn begin_context<'s>(&'s mut self) -> Self::ContextData {
        debug_assert!(!self.node_stack.is_empty(),);

        let ctx = Context {
            start_index: self.node_stack.len() - 1,
        };

        ctx
    }

    fn is_quiescent(&self) -> bool {
        let ParseCst {
            node_stack: _,
            finished: _,
        } = self;

        true
    }

    //==================================
    // Push
    //==================================

    fn push_leaf(&mut self, token: TokenRef<'i>) -> Self::Node {
        debug_assert!(!token.tok.isTrivia());

        self.push_node(Cst::Token(token))
    }

    fn push_syntax(&mut self, token: TokenRef<'i>) -> Self::Node {
        debug_assert!(!token.tok.isTrivia());

        self.push_node(Cst::Token(token))
    }

    fn push_compound_pattern_blank(
        &mut self,
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under: UnderParseData<'i>,
    ) -> Self::Node {
        debug_assert_eq!(symbol.tok, TokenKind::Symbol);
        // debug_assert!(matches!(under.tok, TokenKind::Under | TokenKind::UnderUnder | TokenKind::UnderUnderUnder));

        let under = under.into_cst();

        let node = CompoundNode::new3(op, symbol, under);

        self.push_node(Cst::Compound(node))
    }

    fn push_compound_blank(&mut self, under: UnderParseData<'i>) -> Self::Node {
        let cst: Cst<_> = under.into_cst();

        self.push_node(cst)
    }

    fn push_compound_pattern_optional(
        &mut self,
        op: CompoundOperator,
        symbol: TokenRef<'i>,
        under_dot: TokenRef<'i>,
    ) -> Self::Node {
        debug_assert_eq!(symbol.tok, TokenKind::Symbol);
        debug_assert_eq!(under_dot.tok, TokenKind::UnderDot);

        let node = CompoundNode::new2(op, symbol, under_dot);

        self.push_node(Cst::Compound(node))
    }

    fn push_compound_slot(
        &mut self,
        op: CompoundOperator,
        hash: TokenRef<'i>,
        arg: TokenRef<'i>,
    ) -> Self::Node {
        debug_assert_matches!(
            op,
            CompoundOperator::Slot | CompoundOperator::SlotSequence
        );
        debug_assert_matches!(hash.tok, TokenKind::Hash | TokenKind::HashHash);
        debug_assert_matches!(arg.tok, TokenKind::Integer | TokenKind::String);

        let node = CompoundNode::new2(op, hash, arg);

        self.push_node(Cst::Compound(node))
    }

    fn push_compound_out(
        &mut self,
        op: CompoundOperator,
        percent: TokenRef<'i>,
        integer: TokenRef<'i>,
    ) -> Self::Node {
        debug_assert_eq!(op, CompoundOperator::Out);
        debug_assert_eq!(percent.tok, TokenKind::Percent);
        debug_assert_matches!(integer.tok, TokenKind::Integer);

        let node = CompoundNode::new2(op, percent, integer);

        self.push_node(Cst::Compound(node))
    }

    //==================================
    // Reduce
    //==================================

    fn reduce_prefix(
        &mut self,
        ctx_data: Self::ContextData,
        op: PrefixOperator,
        _op_token: Self::SyntaxTokenNode,
        _trivia: Self::TriviaHandle,
        _operand: Self::Node,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = PrefixNode::new(op, children);

        self.push_node(Cst::Prefix(node))
    }

    fn reduce_prefix_get(
        &mut self,
        ctx_data: Self::ContextData,
        op: PrefixOperator,
        _tok1: Self::SyntaxTokenNode,
        _trivia: Self::TriviaHandle,
        _tok2: TokenRef<'i>,
    ) -> Self::Node {
        debug_assert_eq!(op, PrefixOperator::Get);

        let children = self.reduce(ctx_data);

        let node = PrefixNode::new(op, children);

        self.push_node(Cst::Prefix(node))
    }


    fn reduce_postfix(
        &mut self,
        ctx_data: Self::ContextData,
        op: PostfixOperator,
        _operand: Self::Node,
        _trivia: Self::TriviaHandle,
        _op_tok: Self::SyntaxTokenNode,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = PostfixNode::new(op, children);

        self.push_node(Cst::Postfix(node))
    }

    fn reduce_binary(
        &mut self,
        ctx_data: Self::ContextData,
        op: BinaryOperator,
        _lhs_node: Self::Node,
        _trivia1: Self::TriviaHandle,
        _op_token: Self::SyntaxTokenNode,
        _trivia2: Self::TriviaHandle,
        _rhs_node: Self::Node,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = BinaryNode::new(op, children);

        self.push_node(Cst::Binary(node))
    }

    /// Special-case alternative to [`reduce_binary()`][ParseBuilder] for
    /// `Unset`.
    ///
    /// `foo[bar] =.` is binary-like, but the RHS is the syntax `.`,
    /// not a normal arbitrary expression operand.
    fn reduce_binary_unset(
        &mut self,
        ctx_data: Self::ContextData,
        op: BinaryOperator,
        _lhs_node: Self::Node,
        _trivia1: Self::TriviaHandle,
        _op_token: Self::SyntaxTokenNode,
        _trivia2: Self::TriviaHandle,
        _dot_token: Self::SyntaxTokenNode,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = BinaryNode::new(op, children);

        self.push_node(Cst::Binary(node))
    }

    fn reduce_ternary(
        &mut self,
        ctx_data: Self::ContextData,
        op: TernaryOperator,
        _lhs_node: Self::Node,
        _trivia1: Self::TriviaHandle,
        _first_op_token: Self::SyntaxTokenNode,
        _trivia2: Self::TriviaHandle,
        _middle_node: Self::Node,
        _trivia3: Self::TriviaHandle,
        _second_op_token: Self::SyntaxTokenNode,
        _trivia4: Self::TriviaHandle,
        _rhs_node: Self::Node,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = TernaryNode::new(op, children);

        self.push_node(Cst::Ternary(node))
    }

    /// Special-case alternative to [`reduce_ternary()`][ParseBuilder] for
    /// `TagUnset`.
    ///
    /// `bar /: foo[bar] =.` is ternary-like, but the RHS is the syntax `.`,
    /// not a normal arbitrary expression operand.
    fn reduce_ternary_tag_unset(
        &mut self,
        ctx_data: Self::ContextData,
        // TODO(cleanup): Always the same operator?
        op: TernaryOperator,
        _lhs_node: Self::Node,
        _trivia1: Self::TriviaHandle,
        _slash_colon_token: Self::SyntaxTokenNode,
        _trivia2: Self::TriviaHandle,
        _middle_node: Self::Node,
        _trivia3: Self::TriviaHandle,
        _equal_token: Self::SyntaxTokenNode,
        _trivia4: Self::TriviaHandle,
        _dot_token: Self::SyntaxTokenNode,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = TernaryNode::new(op, children);

        self.push_node(Cst::Ternary(node))
    }

    fn reduce_prefix_binary(
        &mut self,
        ctx_data: Self::ContextData,
        op: PrefixBinaryOperator,
        _prefix_op_token: Self::SyntaxTokenNode,
        _trivia1: Self::TriviaHandle,
        _lhs_node: Self::Node,
        _trivia2: Self::TriviaHandle,
        _rhs_node: Self::Node,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = PrefixBinaryNode::new(op, children);

        self.push_node(Cst::PrefixBinary(node))
    }

    //----------------------------------
    // Infix parsing
    //----------------------------------

    fn begin_infix(
        &mut self,
        // TODO(cleanup): Add this at the end, to avoid having to
        //                pass it around?
        op: InfixOperator,
        _first_node: Self::Node,
    ) -> Self::InfixParseState {
        // `first_node` will already have been pushed to `node_stack`

        InfixParseCst { op }
    }

    fn infix_add(
        &mut self,
        _infix_state: &mut Self::InfixParseState,
        _trivia1: Self::TriviaHandle,
        _op_token: Self::SyntaxTokenNode,
        _trivia2: Self::TriviaHandle,
        _operand: Self::Node,
    ) {
        // Do nothing, because all the arguments should already have been
        // added to `node_stack` when they were originally processed.
    }

    fn reduce_infix(
        &mut self,
        ctx_data: Self::ContextData,
        state: Self::InfixParseState,
    ) -> Self::Node {
        let InfixParseCst { op } = state;

        let children = self.reduce(ctx_data);

        let node = InfixNode::new(op, children);

        self.push_node(Cst::Infix(node))
    }

    fn begin_group(
        &mut self,
        _opener_tok: Self::SyntaxTokenNode,
    ) -> Self::GroupParseState {
        // Do nothing, because all the arguments should already have been
        // added to `node_stack` when they were originally processed.
        InfixParseGroup {}
    }

    fn group_add(
        &mut self,
        _infix_state: &mut Self::GroupParseState,
        _trivia: Self::TriviaHandle,
        _operand: Self::Node,
    ) {
        // Do nothing, because all the arguments should already have been
        // added to `node_stack` when they were originally processed.
    }

    fn reduce_group(
        &mut self,
        ctx_data: Self::ContextData,
        op: GroupOperator,
        _group_state: Self::GroupParseState,
        _trailing_trivia: Self::TriviaHandle,
        _closer_tok: Self::SyntaxTokenNode,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = GroupNode::new(op, children);

        self.push_node(Cst::Group(node))
    }

    fn reduce_call(
        &mut self,
        ctx_data: Self::ContextData,
        _head: Self::Node,
        _head_trivia: Self::TriviaHandle,
        _body: Self::Node,
    ) -> Self::Node {
        let body = self.pop_node();

        let head_children = self.reduce(ctx_data);

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

        let node = CallNode::concrete(head_children, body);

        self.push_node(Cst::Call(node))
    }

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
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let kind = match data {
            SyntaxErrorData::ExpectedSymbol {
                lhs_node: _,
                trivia1: _,
                tok_in: _,
                trivia2: _,
                rhs_node: _,
            } => SyntaxErrorKind::ExpectedSymbol,
            SyntaxErrorData::ExpectedSet => SyntaxErrorKind::ExpectedSet,
            SyntaxErrorData::ExpectedTilde {
                lhs_node: _,
                trivia1: _,
                first_op_token: _,
                trivia2: _,
                middle_node: _,
            } => SyntaxErrorKind::ExpectedTilde,
        };

        let node = SyntaxErrorNode::new(kind, children);

        self.push_node(Cst::SyntaxError(node))
    }

    fn reduce_unterminated_group(
        &mut self,
        ctx_data: Self::ContextData,
        input: &'i str,
        tab_width: usize,
        op: GroupOperator,
        _state: Self::GroupParseState,
        _trailing_trivia: Self::TriviaHandle,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = crate::error::reparse_unterminated_group_node(
            (op, children),
            input,
            tab_width,
        );

        self.push_node(Cst::GroupMissingCloser(node))
    }

    fn reduce_group_missing_closer(
        &mut self,
        ctx_data: Self::ContextData,
        op: GroupOperator,
        _state: Self::GroupParseState,
    ) -> Self::Node {
        let children = self.reduce(ctx_data);

        let node = GroupMissingCloserNode::new(op, children);

        self.push_node(Cst::GroupMissingCloser(node))
    }

    //==================================
    // Pop
    //==================================

    fn finish_top_level_trivia(&mut self, token: TokenRef<'i>) {
        debug_assert!(token.tok.isTrivia());

        self.finished.push(Cst::Token(token));
    }

    fn finish_top_level_expr(&mut self, node: Self::Node) {
        let () = node;

        debug_assert!(self.is_quiescent());

        let node = self.pop_node();

        self.finished.push(node);
    }

    //==================================
    // Properties
    //==================================

    fn check_colon_lhs(&self, lhs: &Self::Node) -> ColonLHS {
        let () = lhs;

        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        let top_node = self
            .node_stack
            .iter()
            .rev()
            .find(
                |cst| !matches!(cst, Cst::Token(token) if token.tok.isTrivia()),
            )
            .expect(
                "unable to check colon LHS: no non-trivia token in top context",
            );

        match top_node {
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

    fn top_node_is_span(&self) -> bool {
        // Of the nodes owned by `ctxt`, get the top (last) one that
        // is not trivia.
        let top_node = self
            .node_stack
            .iter()
            .rev()
            .find(
                |cst| !matches!(cst, Cst::Token(token) if token.tok.isTrivia()),
            )
            .expect(
                "unable to check colon LHS: no non-trivia token in top context",
            );

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

impl<'i> ParseCst<'i> {
    fn push_node(&mut self, node: Cst<TokenStr<'i>>) {
        self.node_stack.push(node)
    }

    fn pop_node(&mut self) -> Cst<TokenStr<'i>> {
        self.node_stack.pop().unwrap()
    }

    #[cfg(test)]
    pub(crate) fn top_node<'s>(&'s mut self) -> &'s mut Cst<TokenStr<'i>> {
        assert!(!self.node_stack.is_empty());

        return self.node_stack.last_mut().unwrap();
    }

    fn reduce(&mut self, ctx_data: Context) -> CstSeq<TokenStr<'i>> {
        let Context { start_index } = ctx_data;

        let children = self.node_stack.drain(start_index..).collect();

        NodeSeq(children)
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
