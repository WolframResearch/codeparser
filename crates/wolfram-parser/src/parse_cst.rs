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
        ColonLHS, InfixParseBuilder, ParseBuilder, SyntaxErrorData,
        SyntaxErrorKind, TriviaSeqRef, UnderParseData,
    },
    tokenize::{TokenKind, TokenRef, TokenStr},
    utils::debug_assert_matches,
    NodeSeq, ParseOptions,
};

#[derive(Debug)]
pub(crate) struct ParseCst<'i> {
    finished: Vec<Cst<TokenStr<'i>>>,
}

impl<'i> ParseBuilder<'i> for ParseCst<'i> {
    type Node = Cst<TokenStr<'i>>;

    type Output = CstSeq<TokenStr<'i>>;

    type InfixParseBuilder = InfixParseCst<'i>;

    type TriviaAccumulator = Vec<TokenRef<'i>>;
    type TriviaHandle = TriviaSeqRef<'i>;

    //==================================
    // Trivia handling
    //==================================

    fn trivia_begin(&mut self) -> Self::TriviaAccumulator {
        Vec::new()
    }

    fn trivia_push(
        &mut self,
        accum: &mut Vec<TokenRef<'i>>,
        trivia: TokenRef<'i>,
    ) {
        accum.push(trivia);
    }

    fn trivia_end(&mut self, accum: Vec<TokenRef<'i>>) -> Self::TriviaHandle {
        TriviaSeq(accum)
    }

    fn empty_trivia() -> Self::TriviaHandle {
        TriviaSeq(Vec::new())
    }

    fn trivia_first(&self, trivia: Self::TriviaHandle) -> Option<TokenRef<'i>> {
        let TriviaSeq(vec) = trivia;

        vec.first().copied()
    }

    //==================================
    // Lifecycle
    //==================================

    fn new_builder() -> Self {
        ParseCst {
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
        let ParseCst { finished } = self;

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

    fn begin_context<'s>(&'s mut self) {
        // Do nothing.
    }

    fn is_quiescent(&self) -> bool {
        let ParseCst { finished: _ } = self;

        true
    }

    //==================================
    // Push
    //==================================

    fn push_leaf(&mut self, token: TokenRef<'i>) -> Self::Node {
        debug_assert!(!token.tok.isTrivia());

        Cst::Token(token)
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

        Cst::Compound(node)
    }

    fn push_compound_blank(&mut self, under: UnderParseData<'i>) -> Self::Node {
        let cst: Cst<_> = under.into_cst();

        cst
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

        Cst::Compound(node)
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

        Cst::Compound(node)
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

        Cst::Compound(node)
    }

    fn push_prefix_get(
        &mut self,
        op: PrefixOperator,
        tok1: TokenRef<'i>,
        trivia: Self::TriviaHandle,
        tok2: TokenRef<'i>,
    ) -> Self::Node {
        debug_assert_eq!(op, PrefixOperator::Get);

        let mut children = Vec::with_capacity(trivia.0.len() + 2);
        children.push(Cst::Token(tok1));
        children.extend(trivia.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(tok2));

        Cst::Prefix(PrefixNode::new(op, NodeSeq(children)))
    }

    //==================================
    // Reduce
    //==================================

    fn reduce_prefix(
        &mut self,
        op: PrefixOperator,
        op_token: TokenRef<'i>,
        trivia: Self::TriviaHandle,
        operand: Self::Node,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(Cst::Token(op_token));
        children.extend(trivia.0.into_iter().map(Cst::Token));
        children.push(operand);

        Cst::Prefix(PrefixNode::new(op, NodeSeq(children)))
    }

    fn begin_infix(
        &mut self,
        op: InfixOperator,
        first_node: Self::Node,
    ) -> Self::InfixParseBuilder {
        // TODO(optimize): Use a single Vec allocation for efficiency?
        InfixParseCst {
            op,
            children: vec![first_node],
        }
    }

    fn reduce_postfix(
        &mut self,
        op: PostfixOperator,
        operand: Self::Node,
        trivia: Self::TriviaHandle,
        op_tok: TokenRef<'i>,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(operand);
        children.extend(trivia.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(op_tok));

        Cst::Postfix(PostfixNode::new(op, NodeSeq(children)))
    }

    fn reduce_binary(
        &mut self,
        op: BinaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(lhs_node);
        children.extend(trivia1.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(op_token));
        children.extend(trivia2.0.into_iter().map(Cst::Token));
        children.push(rhs_node);

        Cst::Binary(BinaryNode::new(op, NodeSeq(children)))
    }

    /// Special-case alternative to [`reduce_binary()`][ParseBuilder] for
    /// `Unset`.
    ///
    /// `foo[bar] =.` is binary-like, but the RHS is the syntax `.`,
    /// not a normal arbitrary expression operand.
    fn reduce_binary_unset(
        &mut self,
        op: BinaryOperator,
        lhs_node: Self::Node,
        trivia1: Self::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: Self::TriviaHandle,
        dot_token: TokenRef<'i>,
    ) -> Self::Node {
        debug_assert_eq!(op, BinaryOperator::Unset);
        debug_assert_eq!(dot_token.tok, TokenKind::Dot);

        let mut children = Vec::new();
        children.push(lhs_node);
        children.extend(trivia1.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(op_token));
        children.extend(trivia2.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(dot_token));

        Cst::Binary(BinaryNode::new(op, NodeSeq(children)))
    }

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
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(lhs_node);
        children.extend(trivia1.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(first_op_token));
        children.extend(trivia2.0.into_iter().map(Cst::Token));
        children.push(middle_node);
        children.extend(trivia3.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(second_op_token));
        children.extend(trivia4.0.into_iter().map(Cst::Token));
        children.push(rhs_node);

        Cst::Ternary(TernaryNode::new(op, NodeSeq(children)))
    }

    /// Special-case alternative to [`reduce_ternary()`][ParseBuilder] for
    /// `TagUnset`.
    ///
    /// `bar /: foo[bar] =.` is ternary-like, but the RHS is the syntax `.`,
    /// not a normal arbitrary expression operand.
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
    ) -> Self::Node {
        debug_assert_eq!(slash_colon_token.tok, TokenKind::SlashColon);
        debug_assert_eq!(equal_token.tok, TokenKind::Equal);
        debug_assert_eq!(dot_token.tok, TokenKind::Dot);

        let mut children = Vec::new();
        children.push(lhs_node);
        children.extend(trivia1.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(slash_colon_token));
        children.extend(trivia2.0.into_iter().map(Cst::Token));
        children.push(middle_node);
        children.extend(trivia3.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(equal_token));
        children.extend(trivia4.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(dot_token));

        Cst::Ternary(TernaryNode::new(op, NodeSeq(children)))
    }

    fn reduce_prefix_binary(
        &mut self,
        op: PrefixBinaryOperator,
        prefix_op_token: TokenRef<'i>,
        trivia1: Self::TriviaHandle,
        lhs_node: Self::Node,
        trivia2: Self::TriviaHandle,
        rhs_node: Self::Node,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(Cst::Token(prefix_op_token));
        children.extend(trivia1.0.into_iter().map(Cst::Token));
        children.push(lhs_node);
        children.extend(trivia2.0.into_iter().map(Cst::Token));
        children.push(rhs_node);

        Cst::PrefixBinary(PrefixBinaryNode::new(op, NodeSeq(children)))
    }

    fn reduce_group(
        &mut self,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(Self::TriviaHandle, Self::Node)>,
        TriviaSeq(trailing_trivia): Self::TriviaHandle,
        closer_tok: TokenRef<'i>,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(Cst::Token(opener_tok));
        for (TriviaSeq(trivia), node) in group_children {
            children.extend(trivia.into_iter().map(Cst::Token));
            children.push(node);
        }
        children.extend(trailing_trivia.into_iter().map(Cst::Token));
        children.push(Cst::Token(closer_tok));

        Cst::Group(GroupNode::new(op, NodeSeq(children)))
    }

    fn reduce_call(
        &mut self,
        head: Self::Node,
        head_trivia: Self::TriviaHandle,
        body: Self::Node,
    ) -> Self::Node {
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

        Cst::Call(CallNode::concrete(head, head_trivia, body))
    }

    //----------------------------------
    // Reduce errors
    //----------------------------------

    fn reduce_syntax_error(
        &mut self,
        data: SyntaxErrorData<'i, Self::Node, Self::TriviaHandle>,
    ) -> Self::Node {
        let (kind, children) = match data {
            SyntaxErrorData::ExpectedSymbol {
                lhs_node,
                trivia1: TriviaSeq(trivia1),
                tok_in,
                trivia2: TriviaSeq(trivia2),
                rhs_node,
            } => {
                let mut children = Vec::new();
                children.push(lhs_node);
                children.extend(trivia1.into_iter().map(Cst::Token));
                children.push(Cst::Token(tok_in));
                children.extend(trivia2.into_iter().map(Cst::Token));
                children.push(rhs_node);

                (SyntaxErrorKind::ExpectedSymbol, children)
            },
            SyntaxErrorData::ExpectedSet => {
                (SyntaxErrorKind::ExpectedSet, Vec::new())
            },
            SyntaxErrorData::ExpectedTilde {
                lhs_node,
                trivia1: TriviaSeq(trivia1),
                first_op_token,
                trivia2: TriviaSeq(trivia2),
                middle_node,
            } => {
                let mut children = Vec::new();
                children.push(lhs_node);
                children.extend(trivia1.into_iter().map(Cst::Token));
                children.push(Cst::Token(first_op_token));
                children.extend(trivia2.into_iter().map(Cst::Token));
                children.push(middle_node);

                (SyntaxErrorKind::ExpectedTilde, children)
            },
        };

        Cst::SyntaxError(SyntaxErrorNode::new(kind, NodeSeq(children)))
    }

    fn reduce_unterminated_group(
        &mut self,
        input: &'i str,
        tab_width: usize,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(Self::TriviaHandle, Self::Node)>,
        TriviaSeq(trailing_trivia): Self::TriviaHandle,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(Cst::Token(opener_tok));
        for (TriviaSeq(trivia), node) in group_children {
            children.extend(trivia.into_iter().map(Cst::Token));
            children.push(node);
        }
        children.extend(trailing_trivia.into_iter().map(Cst::Token));

        let node = crate::error::reparse_unterminated_group_node(
            (op, NodeSeq(children)),
            input,
            tab_width,
        );

        Cst::GroupMissingCloser(node)
    }

    fn reduce_group_missing_closer(
        &mut self,
        op: GroupOperator,
        opener_tok: TokenRef<'i>,
        group_children: Vec<(Self::TriviaHandle, Self::Node)>,
    ) -> Self::Node {
        let mut children = Vec::new();
        children.push(Cst::Token(opener_tok));
        for (TriviaSeq(trivia), node) in group_children {
            children.extend(trivia.into_iter().map(Cst::Token));
            children.push(node);
        }

        Cst::GroupMissingCloser(GroupMissingCloserNode::new(
            op,
            NodeSeq(children),
        ))
    }

    //==================================
    // Pop
    //==================================

    fn finish_top_level_trivia(&mut self, token: TokenRef<'i>) {
        debug_assert!(token.tok.isTrivia());

        self.finished.push(Cst::Token(token));
    }

    fn finish_top_level_expr(&mut self, node: Self::Node) {
        debug_assert!(self.is_quiescent());

        self.finished.push(node);
    }

    //==================================
    // Properties
    //==================================

    fn check_colon_lhs(&self, lhs: &Self::Node) -> ColonLHS {
        match lhs {
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

    fn top_node_is_span(&self, top_node: &Self::Node) -> bool {
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

#[derive(Debug)]
pub(crate) struct InfixParseCst<'i> {
    op: InfixOperator,
    children: Vec<Cst<TokenStr<'i>>>,
}

impl<'i> InfixParseBuilder<'i, ParseCst<'i>> for InfixParseCst<'i>
where
    ParseCst<'i>: ParseBuilder<
        'i,
        Node = Cst<TokenStr<'i>>,
        TriviaHandle = TriviaSeqRef<'i>,
    >,
{
    fn add(
        &mut self,
        trivia1: <ParseCst<'i> as ParseBuilder<'i>>::TriviaHandle,
        op_token: TokenRef<'i>,
        trivia2: <ParseCst<'i> as ParseBuilder<'i>>::TriviaHandle,
        operand: Cst<TokenStr<'i>>,
    ) {
        let InfixParseCst { op: _, children } = self;

        children.extend(trivia1.0.into_iter().map(Cst::Token));
        children.push(Cst::Token(op_token));
        children.extend(trivia2.0.into_iter().map(Cst::Token));
        children.push(operand)
    }

    fn last_node(&self) -> &Cst<TokenStr<'i>> {
        let InfixParseCst { op: _, children } = self;

        children
            .last()
            .expect("InfixParseCst::last_node: node list is empty")
    }

    fn finish(self) -> Cst<TokenStr<'i>> {
        let InfixParseCst { op, children } = self;

        Cst::Infix(InfixNode::new(op, NodeSeq(children)))
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
