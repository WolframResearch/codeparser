mod abstract_call_node;

use std::fmt::Debug;

use crate::{
    agg::{AggNodeSeq, LHS},
    ast::{AbstractSyntaxError, Ast, AstCall, AstMetadata},
    cst::{
        BinaryNode, BoxKind, BoxNode, CallHead, CallNode, CodeNode,
        CompoundNode, Cst, CstSeq, GroupMissingCloserNode,
        GroupMissingOpenerNode, GroupNode, InfixNode, OperatorNode,
        PostfixNode, PrefixBinaryNode, PrefixNode, SyntaxErrorNode,
        TernaryNode,
    },
    issue::{Issue, IssueTag, Severity},
    macros::leaf,
    parse::{
        operators::{
            BinaryOperator, CompoundOperator, GroupOperator, InfixOperator,
            Operator, PostfixOperator, PrefixBinaryOperator, PrefixOperator,
            TernaryOperator,
        },
        SyntaxErrorKind,
    },
    quirks::{self, Quirk},
    symbol::Symbol,
    symbols as st,
    tokenize::{
        Token, TokenInput,
        TokenKind::{self, self as TK},
        TokenSource, TokenString,
    },
    utils::{append, join},
    NodeSeq, QuirkSettings,
};

//==========================================================
// Aggregate
//==========================================================

pub fn aggregate_cst_seq<I: Debug, S: Debug>(
    agg: CstSeq<I, S>,
) -> AggNodeSeq<I, S> {
    let NodeSeq(agg) = agg;

    let agg_children = agg.into_iter().flat_map(aggregate_cst).collect();

    NodeSeq(agg_children)
}

pub fn aggregate_cst<I: Debug, S: Debug>(node: Cst<I, S>) -> Option<Cst<I, S>> {
    let node: Cst<_, _> = match node {
        // Remove comments, whitespace, and newlines
        Cst::Token(Token {
            tok:
                TokenKind::Comment
                | TokenKind::InternalNewline
                | TokenKind::ToplevelNewline
                | TokenKind::Boxes_MultiWhitespace
                | TokenKind::Whitespace,
            ..
        }) => return None,
        Cst::Token(_) => return Some(node),
        // Remove comments.
        Cst::Group(GroupNode(OperatorNode {
            op: GroupOperator::Token_Comment,
            ..
        })) => return None,

        // Multiple implicit Times tokens may have been inserted when parsing boxes, so remove them here
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(children),
        })) => {
            let aggregated_children: Vec<_> =
                children.into_iter().flat_map(aggregate_cst).collect();

            // FIXME: Translate this line
            //aggregatedChildren = First /@ Split[aggregatedChildren, (MatchQ[#1, LeafNode[Token`Fake`ImplicitTimes, _, _]] && MatchQ[#2, LeafNode[Token`Fake`ImplicitTimes, _, _]])&];

            Cst::Infix(InfixNode(OperatorNode {
                op: InfixOperator::Times,
                children: NodeSeq(aggregated_children),
            }))
        },

        Cst::Call(CallNode { head, body }) => {
            let head = match head {
                CallHead::Concrete(head) => {
                    let NodeSeq(head) = aggregate_cst_seq(head);

                    // Aggregating the head of a call should reduce to a seqeuence with
                    // just one node.
                    debug_assert!(head.len() == 1);
                    let head = head.into_iter().next().unwrap();

                    head
                },
                CallHead::Aggregate(_) => {
                    panic!("unexpected CallHead::Aggregate(..)")
                },
            };

            let body = body.map_op(aggregate_op);

            Cst::Call(CallNode {
                head: CallHead::Aggregate(Box::new(head)),
                body,
            })
        },

        // Do not descend into CodeNode
        //    aggregate[n:CodeNode[_, _, _]] := n
        Cst::Code(node) => Cst::Code(node),

        //---------------------------------------------
        //  aggregate[node_[tag_, children_, data_]] :=
        //      node[tag, aggregate /@ children, data]
        //---------------------------------------------
        Cst::SyntaxError(SyntaxErrorNode { err, children }) => {
            Cst::SyntaxError(SyntaxErrorNode {
                err,
                children: aggregate_cst_seq(children),
            })
        },
        Cst::Group(GroupNode(op)) => Cst::Group(GroupNode(aggregate_op(op))),
        Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
            Cst::GroupMissingCloser(GroupMissingCloserNode(aggregate_op(op)))
        },
        Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
            Cst::GroupMissingOpener(GroupMissingOpenerNode(aggregate_op(op)))
        },
        Cst::Box(BoxNode {
            kind,
            children,
            src,
        }) => Cst::Box(BoxNode {
            kind,
            children: aggregate_cst_seq(children),
            src,
        }),

        Cst::Infix(InfixNode(op)) => Cst::Infix(InfixNode(aggregate_op(op))),
        Cst::Prefix(PrefixNode(op)) => {
            Cst::Prefix(PrefixNode(aggregate_op(op)))
        },
        Cst::Postfix(PostfixNode(op)) => {
            Cst::Postfix(PostfixNode(aggregate_op(op)))
        },
        Cst::Binary(BinaryNode(op)) => {
            Cst::Binary(BinaryNode(aggregate_op(op)))
        },
        Cst::Ternary(TernaryNode(op)) => {
            Cst::Ternary(TernaryNode(aggregate_op(op)))
        },
        Cst::PrefixBinary(PrefixBinaryNode(op)) => {
            Cst::PrefixBinary(PrefixBinaryNode(aggregate_op(op)))
        },
        Cst::Compound(CompoundNode(op)) => {
            Cst::Compound(CompoundNode(aggregate_op(op)))
        },
    };

    /* FIXME: Port these:
        (*
            BoxNode[RowBox] and BoxNode[GridBox] have lists as children
        *)
        aggregate[l_List] := aggregate /@ l

        aggregate[BoxNode[RowBox, childrenIn_, dataIn_]] :=
            Catch[
                Module[{children, aggChildren, data},
                    children = childrenIn;
                    data = dataIn;

                    aggChildren = aggregate /@ children;

                    If[MatchQ[aggChildren, {{_}}],
                        (*
                        children is now a single node, so collapse the RowBox
                        *)
                        Throw[aggChildren[[1, 1]]]
                    ];

                    BoxNode[RowBox, aggChildren, data]
                ]
            ]
    */

    Some(node)
}

fn aggregate_op<I: Debug, S: Debug, O>(
    op: OperatorNode<I, S, O>,
) -> OperatorNode<I, S, O> {
    let OperatorNode { op, children } = op;

    OperatorNode {
        op,
        children: aggregate_cst_seq(children),
    }
}

//==========================================================
// Abstract
//==========================================================

macro_rules! expect_children {
    ($children:ident, {_, $name:ident:_}) => {
        let [_, $name] = expect_children($children);
    };
    ($children:ident, {$name1:ident:_, $name2:ident:_}) => {
        let [$name1, $name2] = expect_children($children);
    };
    ($children:ident, {_, $name1:ident:_, _}) => {
        let [_, $name1, _] = expect_children($children);
    };
    ($children:ident, {$name1:ident, _, $name2:ident:_}) => {
        let [$name1, _, $name2] = expect_children($children);
    };

    ($children:ident, {$name:ident:_, LeafNode[$token_kind:ident, _, $data:ident:_]}) => {
        let [$name, leaf] = expect_children($children);

        let $data = match leaf {
            Cst::Token(Token {
                tok: TokenKind::$token_kind,
                input: _,
                src,
            }) => src,
            _ => unhandled(),
        };
    };
    ($children:ident, {_, $name:ident:LeafNode[$token_kind:ident, _, _]}) => {
        let [_, $name] = expect_children($children);

        if !matches!(
            $name,
            Cst::Token(Token {
                tok: TokenKind::$token_kind,
                ..
            })
        ) {
            unhandled()
        }
    };
}

//--------------------------------------
// Functions
//--------------------------------------

pub fn abstract_cst<I: TokenInput + Debug, S: TokenSource + Debug>(
    cst: Cst<I, S>,
    quirks: QuirkSettings,
) -> Ast {
    // FIXME: Just pass this as a normal argument through the abstraction
    //        logic.
    crate::quirks::set_quirks(quirks);

    abstract_(cst)
}

// TODO(cleanup): Should also take quirks if made public.
fn abstract_cst_seq<I: TokenInput + Debug, S: TokenSource + Debug>(
    agg: AggNodeSeq<I, S>,
) -> Vec<Ast> {
    let NodeSeq(agg) = agg;

    let ast_children = agg.into_iter().map(abstract_).collect();

    ast_children
}

fn abstract_<I: TokenInput + Debug, S: TokenSource + Debug>(
    node: Cst<I, S>,
) -> Ast {
    let data: S = node.get_source();

    #[cfg(debug_assertions)]
    let (node_2, data_2) = (node.clone(), data.clone().into_general());

    let ast_node = match node {
        Cst::Token(token) => return abstract_replace_token(token),
        Cst::Compound(CompoundNode(OperatorNode { op, children })) => {
            match op {
                // CompoundNode[Blank, {_, sym2_}, data_]
                CompoundOperator::Blank => {
                    expect_children!(children, {_, sym2:_});
                    Ast::call(st::Blank, vec![abstract_(sym2)], data)
                },
                // CompoundNode[BlankSequence, {_, sym2_}, data_]
                CompoundOperator::BlankSequence => {
                    expect_children!(children, {_, sym2:_});
                    Ast::call(st::BlankSequence, vec![abstract_(sym2)], data)
                },
                // CompoundNode[BlankNullSequence, {_, sym2_}, data_]
                CompoundOperator::BlankNullSequence => {
                    expect_children!(children, {_, sym2:_});
                    Ast::call(
                        st::BlankNullSequence,
                        vec![abstract_(sym2)],
                        data,
                    )
                },
                // CompoundNode[PatternBlank, {sym1_, blank_}, data_]
                CompoundOperator::CodeParser_PatternBlank => {
                    expect_children!(children, {sym1:_, blank:_});
                    Ast::call(
                        st::Pattern,
                        vec![abstract_(sym1), abstract_(blank)],
                        data,
                    )
                },
                // CompoundNode[PatternBlankSequence,     {sym1_, blankSeq_}, data_]
                CompoundOperator::CodeParser_PatternBlankSequence => {
                    expect_children!(children, {sym1:_, blankSeq:_});
                    Ast::call(
                        st::Pattern,
                        vec![abstract_(sym1), abstract_(blankSeq)],
                        data,
                    )
                },
                // CompoundNode[PatternBlankNullSequence, {sym1_, blankNullSeq_}, data_]
                CompoundOperator::CodeParser_PatternBlankNullSequence => {
                    expect_children!(children, {sym1:_, blankNullSeq:_});
                    Ast::call(
                        st::Pattern,
                        vec![abstract_(sym1), abstract_(blankNullSeq)],
                        data,
                    )
                },
                // CompoundNode[PatternOptionalDefault, {sym1_, LeafNode[Token`UnderDot, _, optionalDefaultData_]}, data_]
                CompoundOperator::CodeParser_PatternOptionalDefault => {
                    expect_children!(children, {sym1:_, LeafNode[UnderDot, _, optionalDefaultData:_]});

                    Ast::call(
                        st::Optional,
                        vec![Ast::call(
                            st::Pattern,
                            vec![
                                abstract_(sym1),
                                Ast::call(
                                    st::Blank,
                                    vec![],
                                    optionalDefaultData,
                                ),
                            ],
                            data.clone(),
                        )],
                        data,
                    )
                },
                // CompoundNode[Slot, {_, arg:LeafNode[Integer, _, data1_]}, data_]
                // CompoundNode[Slot, {_, arg:LeafNode[Symbol, s_, data1_]}, data_]
                // CompoundNode[Slot, {_, arg:LeafNode[String, s_, data1_]}, data_]
                CompoundOperator::Slot => {
                    let [_, arg] = expect_children(children);

                    match arg {
                        Cst::Token(arg) => match arg.tok {
                            TokenKind::Integer => Ast::call(
                                st::Slot,
                                vec![abstract_replace_token(arg)],
                                data,
                            ),
                            TokenKind::Symbol => {
                                let Token {
                                    tok: _,
                                    input: s,
                                    src: data1,
                                } = arg;
                                Ast::call(
                                    st::Slot,
                                    vec![Ast::string(
                                        escapeString_of_abstractSymbolString(
                                            s.as_str(),
                                        ),
                                        data1,
                                    )],
                                    data,
                                )
                            },
                            TokenKind::String => {
                                let Token {
                                    tok: _,
                                    input: s,
                                    src: data1,
                                } = arg;
                                Ast::call(
                                    st::Slot,
                                    vec![Ast::string(
                                        escapeString_of_abstractSymbolString(
                                            s.as_str(),
                                        ),
                                        data1,
                                    )],
                                    data,
                                )
                            },
                            _ => unhandled(),
                        },
                        _ => unhandled(),
                    }
                },

                // CompoundNode[SlotSequence, {_, arg:LeafNode[Integer, _, _]}, data_]
                CompoundOperator::SlotSequence => {
                    expect_children!(children, {_, arg:LeafNode[Integer, _, _]});

                    Ast::call(st::SlotSequence, vec![abstract_(arg)], data)
                },

                // CompoundNode[Out, {_, arg:LeafNode[Integer, _, _]}, data_]
                CompoundOperator::Out => {
                    expect_children!(children, {_, arg:LeafNode[Integer, _, _]});

                    Ast::call(st::Out, vec![abstract_(arg)], data)
                },
            }
        },

        //============
        // PrefixNode
        //============
        Cst::Prefix(PrefixNode(OperatorNode { op, children })) => match op {
            // PrefixNode[Minus, {_, rand_}, data_]
            PrefixOperator::Minus => {
                expect_children!(children, {_, rand:_});

                negate(rand).into_ast(data)
            },

            // PrefixNode[Plus, {_, rand_}, _], data_
            PrefixOperator::Plus => {
                let [_, rand] = expect_children(children);

                abstractPrefixPlus(rand, data)
            },

            // PrefixNode[PrefixNot2, {notNotTok_, rand_}, data_]
            PrefixOperator::CodeParser_PrefixNot2 => {
                let [notNotTok, rand] = expect_children(children);

                abstractNot2(rand, notNotTok, data)
            },

            PrefixOperator::CodeParser_PrefixLinearSyntaxBang => {
                let NodeSeq(children) = children;

                match children.as_slice() {
                    // PrefixNode[PrefixLinearSyntaxBang, {rator_, rand:LeafNode[Token`LinearSyntaxBlob, _, _]}, data_]
                    [_, Cst::Token(Token {
                        tok: TK::LinearSyntaxBlob,
                        ..
                    })] => {
                        let [rator, rand] = expect_children(NodeSeq(children));

                        // FIXME: keep linear syntax for now
                        // PrefixNode[PrefixLinearSyntaxBang, {rator, abstract[rand]}, data]
                        Ast::PrefixNode_PrefixLinearSyntaxBang(
                            Box::new([abstract_(rator), abstract_(rand)]),
                            AstMetadata::from(data),
                        )
                    },
                    // PrefixNode[PrefixLinearSyntaxBang, {_, rand_}, data_]
                    [_, rand] => Ast::abstract_syntax_error(
                        AbstractSyntaxError::LinearSyntaxBang,
                        vec![abstract_(rand.clone())],
                        data,
                    ),
                    _ => unhandled(),
                }
            },

            // strings may be quoted
            //
            // concrete syntax: <<a
            // abstract syntax Get["a"]
            //
            // concrete syntax: <<"a"
            // abstract syntax Get["a"]
            //
            // PrefixNode[Get, {_, LeafNode[String, str_, data1_]}, data_]
            PrefixOperator::Get => {
                // TODO(test): Add test case for prefix get (there doesn't seem
                //             to be one now).
                let [_, rand] = expect_children(children);

                match rand {
                    Cst::Token(Token {
                        tok: TK::String,
                        input: str,
                        src: data1,
                    }) => Ast::call(
                        st::Get,
                        vec![Ast::string(
                            escapeString_of_abstractFileString(str.as_str()),
                            data1,
                        )],
                        data,
                    ),
                    _ => unhandled(),
                }
            },

            // PrefixNode[op_, {_, operand_}, data_]
            op => {
                let [_, operand] = expect_children(children);

                Ast::call(op.to_symbol(), vec![abstract_(operand)], data)
            },
        },

        //============
        // PostfixNode
        //============

        // PostfixNode[op_, {operand_, rator_}, data_]
        Cst::Postfix(PostfixNode(OperatorNode { op, children })) => {
            let [operand, rator] = expect_children(children);

            match op {
                // PostfixNode[System`HermitianConjugate, {rand_, _}, data_]
                // TODO(test): Add test case for this case.
                PostfixOperator::HermitianConjugate => Ast::call(
                    st::ConjugateTranspose,
                    vec![abstract_(operand)],
                    data,
                ),
                PostfixOperator::Derivative => {
                    match rator {
                        // PostfixNode[Derivative, {rand_, LeafNode[Token`SingleQuote, _, _]}, _]
                        Cst::Token(Token {
                            tok: TK::SingleQuote,
                            ..
                        }) => {
                            let (order, abstractedBody) =
                                derivativeOrderAndAbstractedBody(operand);

                            Ast::call2(
                                Ast::call(
                                    st::Derivative,
                                    vec![Ast::usize(order + 1)],
                                    AstMetadata::empty(),
                                ),
                                vec![abstractedBody],
                                AstMetadata::empty(),
                            )
                        },
                        // PostfixNode[Derivative, {rand_, LeafNode[Token`Boxes`MultiSingleQuote, quoteStr_, _]}, data_]
                        Cst::Token(Token {
                            tok: TK::Boxes_MultiSingleQuote,
                            input: quoteStr,
                            ..
                        }) => {
                            let order = quoteStr.as_str().len();

                            Ast::call2(
                                Ast::call(
                                    st::Derivative,
                                    vec![Ast::usize(order)],
                                    AstMetadata::empty(),
                                ),
                                vec![abstract_(operand)],
                                AstMetadata::empty(),
                            )
                        },
                        _ => unhandled(),
                    }
                },
                op => Ast::call(op.to_symbol(), vec![abstract_(operand)], data),
            }
        },

        //============
        // BinaryNode
        //============

        // BinaryNode[Divide, { left_, _, right_ }, data_]
        Cst::Binary(BinaryNode(OperatorNode { op, children })) => {
            let [left, middle, right] = expect_children(children);

            match op {
                BinaryOperator::Divide => {
                    abstractTimes_BinaryNode_Divide([left, right], data)
                },

                BinaryOperator::CodeParser_BinaryAt => {
                    Ast::call2(abstract_(left), vec![abstract_(right)], data)
                },

                BinaryOperator::MapApply => {
                    // TID:231104/1: OldAtAtAt quirk cases
                    if quirks::is_quirk_enabled(Quirk::OldAtAtAt) {
                        let level = Ast::call(
                            st::List,
                            vec![Ast::i64(1)],
                            AstMetadata::empty(),
                        );

                        Ast::call(
                            st::Apply,
                            vec![abstract_(left), abstract_(right), level],
                            data,
                        )
                    } else {
                        Ast::call(
                            st::MapApply,
                            vec![abstract_(left), abstract_(right)],
                            data,
                        )
                    }
                },

                BinaryOperator::CodeParser_BinarySlashSlash => Ast::call2(
                    // Make sure to reverse the arguments
                    abstract_(right),
                    vec![abstract_(left)],
                    data,
                ),
                BinaryOperator::Put | BinaryOperator::PutAppend => {
                    let (str, data1) = match right {
                        // {left_, _, LeafNode[String, str_, data1_]}
                        Cst::Token(Token {
                            tok: TokenKind::String,
                            ref input,
                            src: data1,
                        }) => (input.as_str(), data1),
                        _ => unhandled(),
                    };

                    Ast::call(
                        op.to_symbol(),
                        vec![
                            abstract_(left),
                            Ast::string(
                                escapeString_of_abstractFileString(str),
                                data1,
                            ),
                        ],
                        data,
                    )
                },

                BinaryOperator::Pattern => Ast::call(
                    st::Pattern,
                    vec![abstract_(left), abstract_(right)],
                    data,
                ),

                // BinaryNode[Unset, {left_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]
                BinaryOperator::Unset => {
                    if !matches!(
                        middle,
                        Cst::Token(Token { tok: TK::Equal, .. })
                    ) {
                        unhandled()
                    }

                    if !matches!(right, Cst::Token(Token { tok: TK::Dot, .. }))
                    {
                        unhandled()
                    }

                    Ast::call(st::Unset, vec![abstract_(left)], data)
                },

                // Abstract NonAssociative errors
                //
                // a ? b ? c being NonAssociative is alluded to being a bug in bug report 206938
                // Related bugs: 206938
                //
                // BinaryNode[PatternTest, {left:BinaryNode[PatternTest, _, _], _, right_}, data_]
                BinaryOperator::PatternTest
                    if matches!(
                        left,
                        Cst::Binary(BinaryNode(OperatorNode {
                            op: BinaryOperator::PatternTest,
                            ..
                        }))
                    ) =>
                {
                    Ast::abstract_syntax_error(
                        AbstractSyntaxError::NonAssociativePatternTest,
                        vec![abstract_(left), abstract_(right)],
                        data,
                    )
                },

                op => Ast::call(
                    op.to_symbol(),
                    vec![abstract_(left), abstract_(right)],
                    data,
                ),
            }
        },

        //============
        // InfixNode
        //============
        Cst::Infix(InfixNode(OperatorNode {
            op,
            children: NodeSeq(children),
        })) => {
            match op {
                // InfixNode[InfixInequality, children_, data_]
                InfixOperator::CodeParser_InfixInequality => {
                    abstractInfixInequality(children, data)
                },

                // Handle SameQ and UnsameQ specially because they do not
                // participate in the InfixBinaryAt quirk (TID:231010/3)
                //
                // InfixNode[op:SameQ | UnsameQ, children_ /; OddQ[Length[children]], data_]
                InfixOperator::SameQ | InfixOperator::UnsameQ
                    if is_odd(children.len()) =>
                {
                    let children = part_span_even_children(children, None);

                    let children =
                        children.into_iter().map(abstract_).collect();

                    Ast::call(op.to_symbol(), children, data)
                },

                // InfixNode[Plus, children_, data_]
                InfixOperator::Plus => {
                    // Do not do children[[;;;;2]]
                    // need to remember whether Token`Plus or Token`Minus
                    abstractPlus(children, data)
                },

                // InfixNode[Times, children_, data_]
                InfixOperator::Times => {
                    // Skip every other child, which are Star tokens.
                    //   children[[;; ;; 2]]
                    let children = part_span_even_children(children, None);

                    abstractTimes_InfixNode(
                        InfixNode(OperatorNode {
                            op: InfixOperator::Times,
                            children: NodeSeq(children),
                        }),
                        data,
                    )
                },

                // InfixNode[Divisible, children_, data_]
                InfixOperator::Divisible => {
                    let children = part_span_even_children(children, None);

                    let processed = children
                        .into_iter()
                        .map(|node| {
                            processInfixBinaryAtQuirk(node, "Divisible")
                        })
                        // make sure to reverse children of Divisible
                        .rev()
                        .map(abstract_)
                        .collect();

                    Ast::call(st::Divisible, processed, data)
                },

                // InfixNode[CompoundExpression, children_, data_]
                InfixOperator::CompoundExpression => {
                    // Skip every other child, which are Semi tokens.
                    //   children[[;; ;; 2]]
                    let children = part_span_even_children(
                        children,
                        Some(TokenKind::Semi),
                    );

                    let children = children
                        .into_iter()
                        // abstractCompoundExpressionChild
                        .map(|node| match node {
                            Cst::Token(Token {
                                tok: TK::Fake_ImplicitNull,
                                input: _,
                                src: data,
                            }) => Ast::symbol_with_data(st::Null, data.clone()),
                            node => abstract_(node),
                        })
                        .collect();

                    Ast::call(st::CompoundExpression, children, data)
                },

                // InfixNode[MessageName, children_, data_]
                InfixOperator::MessageName => {
                    let children =
                        part_span_even_children(children, Some(TK::ColonColon));

                    abstractMessageName(children, data)
                },

                InfixOperator::CodeParser_InfixTilde => {
                    // children[[;; ;;2]]
                    let children = part_span_even_children(
                        children,
                        Some(TokenKind::Tilde),
                    );

                    abstractInfixTilde(children, AstMetadata::from_src(data))
                },

                // abstract[InfixNode[op_, children_ /; OddQ[Length[children]], data_]] :=
                //   CallNode[ToNode[op], abstract /@ (processInfixBinaryAtQuirk[#, ToString[op]]& /@ children[[;;;;2]]), data]
                _ => {
                    if !is_odd(children.len()) {
                        unhandled()
                    }

                    // TODO(robust): Add and use Operator::as_token_kind(), and
                    //               pass that as the second arg to
                    //               part_span_even_children()?
                    let children = part_span_even_children(children, None);

                    let children = children
                        .into_iter()
                        .map(|child| {
                            processInfixBinaryAtQuirk(
                                child,
                                op.to_symbol().symbol_name().as_str(),
                            )
                        })
                        .map(abstract_)
                        .collect();

                    Ast::call(op.to_symbol(), children, data)
                },
            }
        },

        //============
        // TernaryNode
        //============

        // TernaryNode[TagSet, {left_, _, middle_, _, right_}, data_]
        Cst::Ternary(TernaryNode(OperatorNode { op, children })) => {
            let [left, _, middle, middle_right, right] =
                expect_children(children);

            match op {
                TernaryOperator::CodeParser_TernaryTilde => {
                    // handle  a ~f,~ b
                    //
                    // Cannot have  (f,)[a, b]
                    if matches!(
                        middle,
                        Cst::Infix(InfixNode(OperatorNode {
                            op: InfixOperator::CodeParser_Comma,
                            ..
                        }))
                    ) {
                        // TODO(test): Add test case for this case.
                        // TernaryNode[TernaryTilde, {left_, _, middle:InfixNode[Comma, _, _], _, right_}, data_]
                        let abstractedMiddle = abstract_(middle);

                        let (abstractedMiddle_2, abstractedMiddle_3) =
                            abstractedMiddle.into_children_and_source();

                        Ast::call2(
                            Ast::abstract_syntax_error(
                                AbstractSyntaxError::CommaTopLevel,
                                abstractedMiddle_2,
                                abstractedMiddle_3,
                            ),
                            vec![abstract_(left), abstract_(right)],
                            data,
                        )
                    } else {
                        // TernaryNode[TernaryTilde, {left_, _, middle_, _, right_}, data_]
                        Ast::call2(
                            abstract_(middle),
                            vec![abstract_(left), abstract_(right)],
                            data,
                        )
                    }
                },
                // Allow non-Symbols for left; not a syntax error
                TernaryOperator::TagSet => Ast::call(
                    st::TagSet,
                    vec![abstract_(left), abstract_(middle), abstract_(right)],
                    data,
                ),
                // Allow non-Symbols for left; not a syntax error
                TernaryOperator::TagSetDelayed => Ast::call(
                    st::TagSetDelayed,
                    vec![abstract_(left), abstract_(middle), abstract_(right)],
                    data,
                ),
                // Allow non-Symbols for left; not a syntax error
                // TernaryNode[TagUnset, {left_, _, middle_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]
                TernaryOperator::TagUnset => {
                    if !matches!(
                        middle_right,
                        Cst::Token(Token { tok: TK::Equal, .. })
                    ) {
                        unhandled()
                    }

                    if !matches!(right, Cst::Token(Token { tok: TK::Dot, .. }))
                    {
                        unhandled()
                    }

                    Ast::call(
                        st::TagUnset,
                        vec![abstract_(left), abstract_(middle)],
                        data,
                    )
                },
                TernaryOperator::Span => Ast::call(
                    st::Span,
                    vec![abstract_(left), abstract_(middle), abstract_(right)],
                    data,
                ),
                // TernaryOptionalPattern comes from boxes
                TernaryOperator::CodeParser_TernaryOptionalPattern => {
                    Ast::call(
                        st::Optional,
                        vec![
                            Ast::call(
                                st::Pattern,
                                vec![abstract_(left), abstract_(middle)],
                                AstMetadata::empty(),
                            ),
                            abstract_(right),
                        ],
                        data,
                    )
                },
            }
        },

        Cst::Call(call) => abstract_call_node::abstract_call_node(call),

        //============
        // GroupNode
        //============
        Cst::Group(GroupNode(OperatorNode { op, children })) => {
            match op {
                GroupOperator::CodeParser_GroupParen => {
                    let NodeSeq(children) = children;

                    let children: Result<[_; 3], _> = children.try_into();

                    match children {
                        // GroupNode[GroupParen, { _, InfixNode[Comma, commaChildren, _], _ }, data_]
                        Ok(
                            [_, Cst::Infix(InfixNode(OperatorNode {
                                op: InfixOperator::CodeParser_Comma,
                                children: NodeSeq(comma_children),
                                ..
                            })), _],
                        ) => {
                            let comma_children =
                                part_span_even_children(comma_children, None);
                            let comma_children =
                                abstract_cst_seq(NodeSeq(comma_children));

                            Ast::abstract_syntax_error(
                                AbstractSyntaxError::OpenParen,
                                comma_children,
                                data,
                            )
                        },

                        // GroupNode[GroupParen, { _, child_, _}, data_]
                        Ok([_, child, _]) => abstract_(child),

                        // GroupNode[GroupParen, children_, data_]
                        Err(children) => {
                            // children[[2 ;; -2]]
                            let children =
                                part_span_drop_first_and_last(children);

                            let children = abstract_cst_seq(NodeSeq(children));

                            Ast::abstract_syntax_error(
                                AbstractSyntaxError::OpenParen,
                                children,
                                data,
                            )
                        },
                    }
                },

                // GroupNode errors
                //
                // naked []
                //
                // naked ::[]
                //
                // naked \[LeftDoubleBracket]\[RightDoubleBracket]

                // GroupNode[GroupSquare, children_, data_]
                GroupOperator::CodeParser_GroupSquare => {
                    match children.0.as_slice() {
                        // GroupNode[GroupSquare, {_, InfixNode[Comma, commaChildren_, _], _}, data_]
                        [_, Cst::Infix(InfixNode(OperatorNode {
                            op: InfixOperator::CodeParser_Comma,
                            children: NodeSeq(comma_children),
                            ..
                        })), _] => {
                            let comma_children = part_span_even_children(
                                comma_children.clone(),
                                None,
                            );
                            let comma_children =
                                abstract_cst_seq(NodeSeq(comma_children));

                            Ast::abstract_syntax_error(
                                AbstractSyntaxError::OpenSquare,
                                comma_children,
                                data,
                            )
                        },
                        // GroupNode[GroupSquare, {_, child_, _}, data_]
                        [_, child, _] => Ast::abstract_syntax_error(
                            AbstractSyntaxError::OpenSquare,
                            vec![abstract_(child.clone())],
                            data,
                        ),
                        // GroupNode[GroupSquare, {_, _}, data_]
                        [_, _] => Ast::abstract_syntax_error(
                            AbstractSyntaxError::OpenSquare,
                            vec![],
                            data,
                        ),
                        _ => unhandled(),
                    }
                },

                // GroupNode[GroupTypeSpecifier, children_, data_]
                GroupOperator::CodeParser_GroupTypeSpecifier => {
                    match children.0.as_slice() {
                        // GroupNode[GroupTypeSpecifier, {_, InfixNode[Comma, commaChildren_, _], _}, data_]
                        [_, Cst::Infix(InfixNode(OperatorNode {
                            op: InfixOperator::CodeParser_Comma,
                            children: NodeSeq(comma_children),
                            ..
                        })), _] => {
                            let comma_children = part_span_even_children(
                                comma_children.clone(),
                                None,
                            );
                            let comma_children =
                                abstract_cst_seq(NodeSeq(comma_children));

                            Ast::abstract_syntax_error(
                                AbstractSyntaxError::ColonColonOpenSquare,
                                comma_children,
                                data,
                            )
                        },
                        // GroupNode[GroupTypeSpecifier, {_, child_, _}, data_]
                        [_, child, _] => Ast::abstract_syntax_error(
                            AbstractSyntaxError::ColonColonOpenSquare,
                            vec![abstract_(child.clone())],
                            data,
                        ),
                        // GroupNode[GroupTypeSpecifier, {_, _}, data_]
                        [_, _] => Ast::abstract_syntax_error(
                            AbstractSyntaxError::ColonColonOpenSquare,
                            vec![],
                            data,
                        ),
                        _ => unhandled(),
                    }
                },

                // GroupNode[GroupDoubleBracket, children_, data_]
                GroupOperator::CodeParser_GroupDoubleBracket => {
                    match children.0.as_slice() {
                        // GroupNode[GroupDoubleBracket, {_, InfixNode[Comma, commaChildren_, _], _}, data_]
                        [_, Cst::Infix(InfixNode(OperatorNode {
                            op: InfixOperator::CodeParser_Comma,
                            children: NodeSeq(comma_children),
                            ..
                        })), _] => {
                            let comma_children = part_span_even_children(
                                comma_children.clone(),
                                None,
                            );
                            let comma_children =
                                abstract_cst_seq(NodeSeq(comma_children));

                            Ast::abstract_syntax_error(
                                AbstractSyntaxError::LeftDoubleBracket,
                                comma_children,
                                data,
                            )
                        },
                        // GroupNode[GroupDoubleBracket, {_, child_, _}, data_]
                        [_, child, _] => Ast::abstract_syntax_error(
                            AbstractSyntaxError::LeftDoubleBracket,
                            vec![abstract_(child.clone())],
                            data,
                        ),
                        // GroupNode[GroupDoubleBracket, {_, _}, data_]
                        [_, _] => Ast::abstract_syntax_error(
                            AbstractSyntaxError::LeftDoubleBracket,
                            vec![],
                            data,
                        ),
                        _ => unhandled(),
                    }
                },

                // GroupNode[tag_, children_, data_]
                _ => Ast::from(abstractGroupNode(GroupNode(OperatorNode {
                    op,
                    children,
                }))),
            }
        },

        //==============================
        // GroupMissingCloserNode
        //==============================
        Cst::GroupMissingCloser(node) => {
            let (op, abstracted_children, data) =
                abstractGroupNode_GroupMissingCloserNode(node);

            Ast::GroupMissingCloser {
                kind: op,
                children: abstracted_children,
                data,
            }
        },

        //==============================
        // GroupMissingOpenerNode
        //==============================
        Cst::GroupMissingOpener(node) => {
            abstractGroupNode_GroupMissingOpenerNode(node)
        },

        //=================
        // PrefixBinaryNode
        //=================
        Cst::PrefixBinary(PrefixBinaryNode(OperatorNode { op, children })) => {
            let [_, operand1, operand2] = expect_children(children);

            match (op, operand2) {
                // PrefixBinaryNode[
                //     Integrate | etc.,
                //     {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]},
                //     data_
                // ]
                (
                    PrefixBinaryOperator::Integrate
                    | PrefixBinaryOperator::ContourIntegral
                    | PrefixBinaryOperator::DoubleContourIntegral
                    | PrefixBinaryOperator::ClockwiseContourIntegral
                    | PrefixBinaryOperator::CounterClockwiseContourIntegral,
                    //
                    Cst::Prefix(PrefixNode(OperatorNode {
                        op:
                            PrefixOperator::DifferentialD
                            | PrefixOperator::CapitalDifferentialD,
                        children,
                    })),
                ) => {
                    let [_, var] = expect_children(children);

                    Ast::call(
                        op.to_symbol(),
                        vec![abstract_(operand1), abstract_(var)],
                        data,
                    )
                },
                // TODO: Is this case reachable? Are there any legal
                //       PrefixBinaryNode's other than the Op::*Integral
                //       variants listed above?
                //
                // PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]
                (_, operand2) => Ast::call(
                    op.to_symbol(),
                    vec![abstract_(operand1), abstract_(operand2)],
                    data,
                ),
            }
        },

        // Do not touch CodeNodes
        Cst::Code(CodeNode { first, second, src }) => Ast::Code {
            first,
            second,
            data: AstMetadata::from_src(src),
        },

        //==============================
        // BoxNode
        //==============================

        // FIXME: keep boxes for now
        //
        // Abstract any child boxes
        Cst::Box(box_node) => abstract_box_node(box_node),

        //==============================
        // SyntaxErrorNode
        //==============================
        Cst::SyntaxError(syntax_error_node) => {
            let data = syntax_error_node.get_source();

            let SyntaxErrorNode {
                err,
                children: NodeSeq(children),
            } = syntax_error_node;

            match (err, children.as_slice()) {
                (SyntaxErrorKind::ExpectedTilde, [left, _, middle]) => {
                    Ast::syntax_error(
                        SyntaxErrorKind::ExpectedTilde,
                        vec![
                            abstract_(left.clone()),
                            abstract_(middle.clone()),
                        ],
                        data,
                    )
                },
                (SyntaxErrorKind::ExpectedSet, [left, _, middle]) => {
                    Ast::syntax_error(
                        SyntaxErrorKind::ExpectedSet,
                        vec![
                            abstract_(left.clone()),
                            abstract_(middle.clone()),
                        ],
                        data,
                    )
                },
                /*
                abstract[SyntaxErrorNode[SyntaxError`OldFESyntax, children_, data_]] :=
                    SyntaxErrorNode[SyntaxError`OldFESyntax, abstract /@ children, data]

                abstract[SyntaxErrorNode[SyntaxError`BuggyFESyntax, children_, data_]] :=
                    SyntaxErrorNode[SyntaxError`BuggyFESyntax, abstract /@ children, data]

                abstract[SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, {left_, _, _, right_}, data_]] :=
                    SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, {abstract[left], abstract[right]}, data]

                abstract[SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, {left_, _, middle_, _}, data_]] :=
                    SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, {abstract[left], abstract[middle]}, data]
                */
                (SyntaxErrorKind::ExpectedSymbol, [left, _, right]) => {
                    Ast::syntax_error(
                        SyntaxErrorKind::ExpectedSymbol,
                        vec![abstract_(left.clone()), abstract_(right.clone())],
                        data,
                    )
                },
                _ => todo!(
                "unhandled SyntaxErrorNode content: ({err:?}, {children:?})"
            ),
            }
        },
    };

    // Sanity check that the new Ast node doesn't have a different source
    // region than the original Cst.
    #[cfg(debug_assertions)]
    if &ast_node.metadata().source != &data_2 {
        // Note:
        //   One way the Ast can legitimately have a different source span is if
        //   the original Cst was a `(expr)` parenthesized expression, in
        //   which case the Ast has the span only of the interior `expr`.
        if !matches!(
            node_2,
            Cst::Group(GroupNode(OperatorNode {
                op: GroupOperator::CodeParser_GroupParen,
                ..
            }))
        ) {
            assert_eq!(
                ast_node.metadata().source,
                data_2,
                "Ast source does not match Cst source:\n\tCst = {:#?}, ast_node = {:#?}",
                node_2,
                ast_node
            );
        }
    }

    ast_node
}

fn abstract_replace_token<I: TokenInput, S: TokenSource>(
    token: Token<I, S>,
) -> Ast {
    let Token {
        tok: kind,
        input,
        src: data,
    } = token;

    let node: Ast = match kind {
        TokenKind::PercentPercent => {
            let str = input.as_str();

            // count = StringCount[s, "%" | "\\.25" | "\\:0025" | "\\|000025" | "\\045" | "\\[RawPercent]"];
            let mut count: usize = 0;
            count += str.matches("%").count();
            count += str.matches("\\.25").count();
            count += str.matches("\\:0025").count();
            count += str.matches("\\|000025").count();
            count += str.matches("\\045").count();
            count += str.matches("\\[RawPercent]").count();

            let count =
                i64::try_from(count).expect("Out[..] %-sequence overflows i64");

            Ast::call(
                CompoundOperator::Out.to_symbol(),
                vec![Ast::i64(-count)],
                data,
            )
        },
        TokenKind::Under => Ast::call(st::Blank, vec![], data),
        TokenKind::UnderUnder => Ast::call(st::BlankSequence, vec![], data),
        TokenKind::UnderUnderUnder => {
            Ast::call(st::BlankNullSequence, vec![], data)
        },
        TokenKind::UnderDot => Ast::call(
            st::Optional,
            vec![Ast::call(st::Blank, vec![], data.clone())],
            data,
        ),
        TokenKind::Hash => Ast::call(st::Slot, vec![Ast::i64(1)], data),
        TokenKind::HashHash => {
            Ast::call(st::SlotSequence, vec![Ast::i64(1)], data)
        },
        TokenKind::Percent => Ast::call(st::Out, vec![], data),

        TokenKind::Fake_ImplicitOne => Ast::i64_with_data(1, data),
        // FIXME: This should be "System`All", so that "All" doesn't resolve
        //        into the wrong context if System` is not on $ContextPath?
        TokenKind::Fake_ImplicitAll => Ast::symbol_with_data(st::All, data),

        TokenKind::Error_PrefixImplicitNull
        | TokenKind::Error_InfixImplicitNull => {
            Ast::symbol_with_data(st::Null, data)
        },

        kind if kind.isError() => Ast::Error {
            kind,
            input: input.into_owned(),
            data: AstMetadata::from_src(data),
        },

        TokenKind::Symbol => {
            match input.as_str() {
                // "\[Pi]"
                "\u{03c0}" | "\\[Pi]" | "\\:03c0" | "\\|0003c0" => {
                    Ast::symbol_with_data(st::Pi, data)
                },

                // "\[Degree]"
                "\u{00b0}" | "\\[Degree]" | "\\:00b0" | "\\.b0" | "\\260"
                | "\\|0000b0" => Ast::symbol_with_data(st::Degree, data),

                // "\[Infinity]"
                "\u{221e}" | "\\[Infinity]" | "\\:221e" | "\\|00221e" => {
                    Ast::symbol_with_data(st::Infinity, data)
                },

                // "\[ExponentialE]"
                "\u{f74d}" | "\\[ExponentialE]" | "\\:f74d" | "\\|00f74d" => {
                    Ast::symbol_with_data(st::E, data)
                },

                // "\[ImaginaryI]"
                "\u{f74e}" | "\\[ImaginaryI]" | "\\:f74e" | "\\|00f74e" => {
                    Ast::symbol_with_data(st::I, data)
                },

                // NOTE: It is NOT a bug that \[ImaginaryJ] turns into the same
                //       thing as \[ImaginaryI]. (Or, at the very least, its a
                //       bug so old its not going to change now.)
                // "\[ImaginaryJ]"
                "\u{f74f}" | "\\[ImaginaryJ]" | "\\:f74f" | "\\|00f74f" => {
                    Ast::symbol_with_data(st::I, data)
                },

                _ => Ast::Leaf {
                    kind,
                    input: input.into_owned(),
                    data: AstMetadata::from_src(data),
                },
            }
        },

        // Symbols, Strings, Integers, Reals, and Rationals just get passed
        // through
        //
        // Also, LinearSyntaxBlob just gets passed through
        kind => Ast::Leaf {
            kind,
            input: input.into_owned(),
            data: AstMetadata::from_src(data),
        },
    };

    node
}

//======================================
// Helper Functions
//======================================

pub(crate) fn expect_children<I: Debug, S: Debug, const N: usize>(
    children: AggNodeSeq<I, S>,
) -> [Cst<I, S>; N] {
    let NodeSeq(children) = children;

    let children: [Cst<I, S>; N] = match children.try_into() {
        Ok(children) => children,
        Err(children) => panic!(
            "node did not have the expected number of children (expected {N}, got {})",
            children.len()
        ),
    };

    children
}


/// Ported composition of `escapeString(abstractSymbolString(s))`.
fn escapeString_of_abstractSymbolString(s: &str) -> String {
    // FIXME: This is made up. Consult original WL source and finish porting
    //        this function.
    if s.starts_with('"') {
        format!("{s}")
    } else if s.starts_with(r"\[") {
        format!("\"{s}\"")
    } else {
        format!("{s:?}")
    }
}

/// Ported composition of `escapeString(abstractFileString(s))`.
fn escapeString_of_abstractFileString(s: &str) -> String {
    // FIXME: This is made up. Consult original WL source and finish porting
    //        this function.
    if s.starts_with('"') {
        format!("{s}")
    } else {
        format!("{s:?}")
    }
}

/*
========================================
Original, unported definitions of
  - abstractSymbolString
  - abstractFileString
These are subtle string handling functions,
so I'm recording them here until a more
careful port and testing of them can be
completed.
========================================
(*

String "a" -> a
String a -> a

for handling the various stringification operators
#a
#"a"
a::b
a::"b"
*)
abstractSymbolString[str_String /; StringStartsQ[str, "\""]] :=
  Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

abstractSymbolString[str_String] :=
  Quiet[ToExpression["\""<>str<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

(*
a>>b
a>>"b"

The strings might be something like:
b\c => b\\c
b\f => b\\f

FIXME: once the semantics are completely understood, move this to library
*)
abstractFileString[str_String /; StringStartsQ[str, "\""]] :=
Module[{},
  Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
]

abstractFileString[str_String] :=
Module[{replaced},

  (*
  convert to the language that is understood by quoted strings, to be given to ToExpression
  *)
  replaced = StringReplace[str, {
      (*
      single character escapes
      *)
      "\\b" -> "\\\\b",
      "\\f" -> "\\\\f",
      "\\n" -> "\\\\n",
      "\\r" -> "\\\\r",
      "\\t" -> "\\\\t",
      (*
      and double quote
      *)
      "\"" -> "\\\"",
      (*
      and backslash
      *)
      "\\" -> "\\\\"
    }];

  Quiet[ToExpression["\""<>replaced<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
]
*/

// fn escapeString(s: &str) -> String {
//     // FIXME: In WL this was defined as:
//     //   ToString[s, InputForm, CharacterEncoding -> "ASCII"]
//     //
//     // What *exactly* does ToString do in that situation? What special
//     // characters are escaped or not?

//     // TODO: Use some combination of str::escape_debug()/std::escape_default()/
//     //       custom escaping logic here?

//     format!("{s:?}")
// }

// /// String "a" -> a
// /// String a -> a
// ///
// /// for handling the various stringification operators
// ///
// /// * `#a`
// /// * `#"a"`
// /// * `a::b`
// /// * `a::"b"`
// fn abstractSymbolString(s: &str) -> String {
//     // abstractSymbolString[str_String /; StringStartsQ[str, "\""]] :=
//     //   Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

//     // abstractSymbolString[str_String] :=
//     //   Quiet[ToExpression["\""<>str<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

//     if s.starts_with('\"') {
//     }
// }

#[track_caller]
fn unhandled() -> ! {
    // TODO: Turn this into an Err(..) instead of a panic?
    // Failure["Unhandled", ..]
    todo!("unhandled case")
}

//--------------------------------------
// negate
//--------------------------------------

fn parenthesizedIntegerOrRealQ<I: Debug, S: Debug>(node: &Cst<I, S>) -> bool {
    match node {
        // parenthesizedIntegerOrRealQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
        //     parenthesizedIntegerOrRealQ[child]
        Cst::Group(GroupNode(OperatorNode {
            op: GroupOperator::CodeParser_GroupParen,
            children,
        })) => {
            let [_, child, _]: &[_; 3] =
                children.0.as_slice().try_into().expect(
                    "GroupParen Group node has unexpected number of children",
                );

            parenthesizedIntegerOrRealQ(child)
        },
        // parenthesizedIntegerOrRealQ[LeafNode[Integer, _, _]] := True
        // parenthesizedIntegerOrRealQ[LeafNode[Real, _, _]] := True
        Cst::Token(Token {
            tok: TokenKind::Integer | TokenKind::Real,
            input: _,
            src: _,
        }) => true,
        // parenthesizedIntegerOrRealQ[_] := False
        _ => false,
    }
}

fn extractParenthesizedIntegerOrRealQ<I: Debug, S: Debug>(
    node: Cst<I, S>,
) -> Cst<I, S> {
    debug_assert!(parenthesizedIntegerOrRealQ(&node));

    match node {
        // parenthesizedIntegerOrRealQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
        //     parenthesizedIntegerOrRealQ[child]
        Cst::Group(GroupNode(OperatorNode {
            op: GroupOperator::CodeParser_GroupParen,
            children: NodeSeq(children),
        })) => {
            let [_, child, _]: [_; 3] = children.try_into().expect(
                "GroupParen Group node has unexpected number of children",
            );

            child
        },
        _ => panic!("unable to extract inner parenthesized Integer or Real"),
    }
}

// TODO(optimization): Make this take a `&Cst`, so we don't have to
//                     clone at the callsite to this function.
fn possiblyNegatedZeroQ<I: TokenInput + Debug, S: Debug>(
    node: Cst<I, S>,
) -> bool {
    match node {
        // possiblyNegatedZeroQ[LeafNode[Integer, "0", _]] := True
        Cst::Token(Token {
            tok: TokenKind::Integer,
            input,
            src: _,
        }) if input.as_str() == "0" => true,
        // possiblyNegatedZeroQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
        //     possiblyNegatedZeroQ[child]
        Cst::Group(GroupNode(OperatorNode {
            op: GroupOperator::CodeParser_GroupParen,
            children,
        })) => {
            expect_children!(children, {_, child:_, _});

            possiblyNegatedZeroQ(child)
        },
        // possiblyNegatedZeroQ[PrefixNode[Minus, { _, child_}, _]] :=
        //     possiblyNegatedZeroQ[child]
        Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Minus,
            children,
        })) => {
            expect_children!(children, {_, child:_});

            possiblyNegatedZeroQ(child)
        },
        // possiblyNegatedZeroQ[_] := False
        _ => false,
    }
}

/// Form that behaves specially when being abstracted due to special processing
/// for things like the infix binary at quirk, Times flattening, prefix plus
/// flattening, etc.
enum Operand<I, S> {
    Cst(Cst<I, S>),
    Reciprocate(Reciprocate<I, S>),
    NegativeOne,
    Negated(Negated<I, S>, S),
}

#[must_use]
#[derive(Debug)]
enum Negated<I, S> {
    Integer0,
    IntegerNegated(I),
    RealNegated(I),
    InfixTimesSeq(NodeSeq<Cst<I, S>>),
}

impl<I: TokenInput + Debug, S: TokenSource + Debug> Negated<I, S> {
    fn into_ast(self, data: S) -> Ast {
        match self {
            Negated::Integer0 => {
                leaf!(Integer, "0", data)
            },
            Negated::IntegerNegated(input) => {
                let str = input.as_str();

                Ast::Leaf {
                    kind: TokenKind::Integer,
                    input: TokenString::from_string(format!("-{str}")),
                    data: AstMetadata::from_src(data),
                }
            },
            Negated::RealNegated(input) => {
                let str = input.as_str();

                Ast::Leaf {
                    kind: TokenKind::Real,
                    input: TokenString::from_string(format!("-{str}")),
                    data: AstMetadata::from_src(data),
                }
            },
            Negated::InfixTimesSeq(NodeSeq(children)) => {
                let children =
                    part_span_even_children(children, Some(TK::Star));

                let infix = InfixNode(OperatorNode {
                    op: InfixOperator::Times,
                    children: NodeSeq(children),
                });

                let Ast::Call {
                    head,
                    mut args,
                    data,
                } = abstractTimes_InfixNode(infix, data)
                else {
                    panic!("expected InfixNode after abstract Times")
                };

                args = join([leaf!(Integer, "-1", <||>)], args);

                Ast::Call { head, args, data }
            },
        }
    }
}

// concrete syntax does not have negated numbers
// abstract syntax is allowed to have negated numbers
//
// This can happen with:  a-EOF
//
// negate[node:ErrorNode[Token`Error`ExpectedOperand, _, _], _] :=
//   node
fn negate<I: TokenInput + Debug, S: TokenSource + Debug>(
    node: Cst<I, S>,
) -> Negated<I, S> {
    if let Cst::Token(Token {
        tok: TokenKind::Integer,
        input,
        src: _,
    }) = node
    {
        let str = input.as_str();

        if str == "0" {
            return Negated::Integer0;
        } else {
            return Negated::IntegerNegated(input);
        }
    };

    if let Cst::Token(Token {
        tok: TokenKind::Real,
        input,
        src: _,
    }) = node
    {
        return Negated::RealNegated(input);
    }

    // dig down into parens
    //
    // something like  -(1.2)  is still parsed as  -1.2
    //
    // TODO: maybe this is a kernel quirk?
    if let Cst::Group(GroupNode(OperatorNode {
        op: GroupOperator::CodeParser_GroupParen,
        children: NodeSeq(mut children),
    })) = node.clone()
    {
        // TODO(optimization): Avoid this clone().
        if possiblyNegatedZeroQ(children[1].clone()) {
            return negate(children.remove(1));
        }
    }

    if let Cst::Prefix(PrefixNode(OperatorNode {
        op: PrefixOperator::Minus,
        children: NodeSeq(mut children),
        // TODO(optimization): Avoid this clone()
    })) = node.clone()
    {
        // TODO(optimization): Avoid this clone().
        if possiblyNegatedZeroQ(children[1].clone()) {
            return negate(children.remove(1));
        }
    }

    if parenthesizedIntegerOrRealQ(&node) {
        let child = extractParenthesizedIntegerOrRealQ(node);
        return negate(child);
    }

    //
    // NOT ABSTRACTED YET!
    //
    // Important to use InfixNode[Times and not just CallNode[Times,
    //
    // This allows these nodes to be merged later e.g., 1-a/b
    //
    // TID:231012/1 -- negating an Infix Times node
    // TID:231012/2 -- negating an Infix Times node that is later flattened (quirk)
    if let Cst::Infix(InfixNode(OperatorNode {
        op: InfixOperator::Times,
        children: NodeSeq(children),
    })) = node.clone()
    {
        return Negated::InfixTimesSeq(NodeSeq(children));
    }

    //------------------------------------------------
    // Otherwise, returns a Times[-1, node] expression
    //------------------------------------------------

    let children = NodeSeq(vec![node]);

    Negated::InfixTimesSeq(children)
}

//======================================

/// Represents a [`Cst`] value that should be reciprocated once it is
/// abstracted.
///
/// This avoids the need to construct a fake [`Cst`] nodes with "unknown" source
/// location values--but the eventual [`Ast`] value does have interior nodes
/// with unknown source locations.
#[derive(Debug)]
struct Reciprocate<I, S>(Cst<I, S>, S);

impl<I: TokenInput + Debug, S: TokenSource + Debug> Reciprocate<I, S> {
    fn into_ast(self) -> Ast {
        let Reciprocate(node, data) = self;

        // Power[node, -1]
        Ast::Call {
            head: Box::new(Ast::symbol(crate::symbols::Power)),
            args: vec![abstract_(node), Ast::i64(-1)],
            data: AstMetadata::from_src(data),
        }
    }
}

//======================================

/// Collect all of the `'` in `f'''[x]`
fn derivativeOrderAndAbstractedBody<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    node: Cst<I, S>,
) -> (usize, Ast) {
    match node {
        Cst::Postfix(PostfixNode(OperatorNode {
            op: PostfixOperator::Derivative,
            children,
        })) => {
            let [rand, _] = expect_children(children);

            let (order, body) = derivativeOrderAndAbstractedBody(rand);

            (order + 1, body)
        },
        _ => (0, abstract_(node)),
    }
}

//======================================

fn processPlusPair<I: TokenInput + Debug, S: TokenSource + Debug>(
    pair: [Cst<I, S>; 2],
) -> Operand<I, S> {
    match pair {
        // {LeafNode[Token`Plus | Token`LongName`ImplicitPlus, _, _], rand_}
        [Cst::Token(Token {
            tok: TK::Plus | TK::LongName_ImplicitPlus,
            input: _,
            src: _,
        }), rand] => Operand::Cst(rand),
        // {LeafNode[Token`Minus | Token`LongName`Minus, _, opData_], rand_}
        [Cst::Token(Token {
            tok: TK::Minus | TK::LongName_Minus,
            input: _,
            src: opData,
        }), rand] => {
            // When parsing a - b + c, make sure to give the abstracted Times expression the correct Span.
            // That is, the source of  - b
            //
            //   synthesizedData = <| Source -> { opData[[Key[Source], 1]], rand[[3, Key[Source], 2]] } |>;

            // TID:231110/2 — Plus pair synthetic "between" BoxPosition
            let source: S = S::between(opData, rand.source());

            Operand::Negated(negate(rand), source)
        },
        _ => unhandled(),
    }
}

// is it a quirk that  a + + b  is parsed as  a + b  ?
// The prefix + is eaten
// TODO: add to kernel quirks mode
fn flattenPrefixPlus<I: Debug, S: Debug>(node: Cst<I, S>) -> Cst<I, S> {
    match node {
        Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Plus,
            children,
        })) => {
            let [_, rand] = expect_children(children);

            flattenPrefixPlus(rand)
        },
        rand => rand,
    }
}

/// abstract syntax of  +a + b - c \[ImplicitPlus] d  is a single Plus expression
///
/// except when it's not
///
/// TID:231104/2
///
/// Related bugs: 365287
///
/// TODO: add 365287 to kernel quirks mode
fn abstractPlus<I: TokenInput + Debug, S: TokenSource + Debug>(
    children: Vec<Cst<I, S>>,
    data: S,
) -> Ast {
    debug_assert!(children.len() > 0 && is_odd(children.len()));

    let pairs: Vec<_> = children[1..]
        .chunks(2)
        .map(|chunk: &[Cst<I, S>]| {
            let array: [Cst<I, S>; 2] = chunk.to_vec().try_into().unwrap();
            array
        })
        .collect();

    let processedPairs = pairs.into_iter().map(processPlusPair);

    let children = std::iter::once(Operand::Cst(children[0].clone()))
        .chain(processedPairs)
        .map(|node| match node {
            Operand::Cst(node) => abstract_(flattenPrefixPlus(
                processInfixBinaryAtQuirk(node, "Plus"),
            )),
            // NOTE: These cases wouldn't be effected by the flatten prefix
            //       plus or process infix binary at quirk because their
            //       heads are never Plus.
            Operand::NegativeOne => leaf!(Integer, "-1", <||>),
            Operand::Reciprocate(reciprocated) => reciprocated.into_ast(),
            Operand::Negated(negated, data) => negated.into_ast(data),
        })
        .collect();

    Ast::call(st::Plus, children, data)
}

/// + +a  parses the same as  +a
///
/// is it a quirk that  + +a  is parsed as  +a  ?
///
/// The first + is eaten
///
/// TODO: add to kernel quirks mode
fn abstractPrefixPlus<I: TokenInput + Debug, S: TokenSource + Debug>(
    rand: Cst<I, S>,
    data: S,
) -> Ast {
    match rand {
        // PrefixNode[Plus, {_, rand_}, _], data_
        Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Plus,
            children,
        })) => {
            let [_, rand] = expect_children(children);

            abstractPrefixPlus(rand, data)
        },
        // rand_, data_
        _ => Ast::call(st::Plus, vec![abstract_(rand)], data),
    }
}

//======================================
// Times handling
//======================================

/// abstract syntax of  `-a * b / c d \[InvisibleTimes] e \[Times] f`  is a single Times expression
///
/// The sequence returned from this function will be the arguments
/// to a `Times[...]` expression.
fn flattenTimes<I: TokenInput + Debug, S: TokenSource + Debug>(
    nodes: Vec<Cst<I, S>>,
    data: S,
) -> Vec<Operand<I, S>> {
    nodes
        .into_iter()
        .flat_map(|node| flatten_times_cst(node, data.clone()))
        .collect()
}

fn flatten_times_cst<I, S>(node: Cst<I, S>, data: S) -> Vec<Operand<I, S>>
where
    I: TokenInput + Debug,
    S: TokenSource + Debug,
{
    let flattenTimesQuirk = quirks::is_quirk_enabled(Quirk::FlattenTimes);

    match node {
        // These rules for PrefixNode illustrate the difference between the FE and kernel
        // Related bugs: 139531
        //
        // TODO: add to kernel quirks mode
        // TODO: add to frontend quirks mode
        Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Minus,
            ref children,
        })) => {
            let [_, operand] = expect_children(children.clone());

            match operand {
                // PrefixNode[Minus, { _, LeafNode[Integer | Real, _, _] }, _]
                Cst::Token(Token {
                    tok: TK::Integer | TK::Real,
                    input: _,
                    src: _,
                }) => vec![Operand::Negated(negate(operand), data.clone())],
                // PrefixNode[Minus, { _, _?parenthesizedIntegerOrRealQ }, _]
                _ if parenthesizedIntegerOrRealQ(&operand) => {
                    vec![Operand::Negated(negate(operand), data.clone())]
                },
                // PrefixNode[Minus, {_, _}, _]
                _ => {
                    if flattenTimesQuirk {
                        // (*
                        // it is possible to have nested prefix Minus, e.g., - - a
                        // so must call recursively into flattenTimes
                        //
                        // TID:231012/3 -- "- - a"
                        // TID:231012/4 -- "- - a * b"
                        //     Nested prefix minus with enclosing Times. Only
                        //     testing "- - a" doesn't actually test that we
                        //     recursed here.
                        // *)
                        join(
                            [Operand::NegativeOne],
                            flatten_times_cst(operand, data),
                        )
                    } else {
                        vec![Operand::Cst(node)]
                    }
                },
            }
        },
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(children),
        })) => {
            let children = part_span_even_children(children, Some(TK::Star));

            flattenTimes(children, data.clone())
        },
        // This rule for BinaryNode[Divide] illustrates the difference between the FE and kernel
        //
        // TODO: add to kernel quirks mode
        // TODO: add to frontend quirks mode
        Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Divide,
            ref children,
        })) => {
            if flattenTimesQuirk {
                let [left, _, right] = expect_children(children.clone());

                // TID:231010/1
                append(
                    flatten_times_cst(left, data.clone()),
                    Operand::Reciprocate(Reciprocate(right, data.clone())),
                )
            } else {
                vec![Operand::Cst(node)]
            }
        },
        _ => vec![Operand::Cst(node)],
    }
}

// InfixNode[Times, children_, data_]
fn abstractTimes_InfixNode<I: TokenInput + Debug, S: TokenSource + Debug>(
    infix: InfixNode<I, S>,
    data: S,
) -> Ast {
    let InfixNode(OperatorNode {
        op,
        children: NodeSeq(children),
    }) = infix;

    debug_assert!(op == InfixOperator::Times);

    let flattened = flattenTimes(children, data.clone());

    let children: Vec<Ast> = flattened
        .into_iter()
        .map(|node| match node {
            Operand::Cst(node) => {
                abstract_(processInfixBinaryAtQuirk(node, "Times"))
            },
            Operand::NegativeOne => leaf!(Integer, "-1", <||>),
            Operand::Negated(negated, data) => negated.into_ast(data),
            Operand::Reciprocate(reciprocate) => reciprocate.into_ast(),
        })
        .collect();

    Ast::call(st::Times, children, data)
}

// BinaryNode[Divide, {left_, right_}, data_]
fn abstractTimes_BinaryNode_Divide<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    [left, right]: [Cst<I, S>; 2],
    data: S,
) -> Ast {
    // TID:231010/4 -- flatten times through Divide numerator
    // TID:231010/5 -- do NOT do infix binary at quirk here
    let children = append(
        flatten_times_cst(left, data.clone())
            .into_iter()
            .map(|node| match node {
                Operand::Cst(node) => abstract_(node),
                Operand::NegativeOne => leaf!(Integer, "-1", <||>),
                Operand::Negated(negated, data) => negated.into_ast(data),
                Operand::Reciprocate(reciprocate) => reciprocate.into_ast(),
            })
            .collect(),
        Reciprocate(right, data.clone()).into_ast(),
    );

    Ast::call(st::Times, children, data)
}

//======================================

pub(crate) fn processInfixBinaryAtQuirk<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    node: Cst<I, S>,
    symName: &str,
) -> Cst<I, S> {
    if !quirks::is_quirk_enabled(Quirk::InfixBinaryAt) {
        return node;
    }

    // Check if this is a `left @ right` node
    let Cst::Binary(BinaryNode(OperatorNode {
        op: BinaryOperator::CodeParser_BinaryAt,
        ref children,
    })) = node
    else {
        return node;
    };

    let [left, middle, rhs] = expect_children(children.clone());

    if !matches!(
        left,
        Cst::Token(Token {
            tok: TK::Symbol,
            input,
            ..
        }) if input.as_str() == symName
    ) {
        return node;
    }

    // TODO: Make this a debug_assert!?
    assert!(matches!(middle, Cst::Token(Token { tok: TK::At, .. })));

    // let data = rhs.source();

    /* FIXME: Port this issue handling logic.
        issues = Lookup[data, AbstractSyntaxIssues, {}];

        synthesizedSource = {symData[[Key[Source], 1]], atData[[Key[Source], 2]]};

        AppendTo[
            issues,
            SyntaxIssue[
                "InfixBinaryAtQuirk", "Unexpected parse.", "Remark",
                <| Source -> synthesizedSource, ConfidenceLevel -> 1.0 |>
            ]
        ];

        AssociateTo[data, AbstractSyntaxIssues -> issues];

        rhs[[3]] = data;
    */

    // TID:231010/2
    rhs
}

//======================================

// (*
// strings may be quoted
//
// concrete syntax: a::b
// abstract syntax MessageName[a, "b"]
//
// concrete syntax: a::"b"
// abstract syntax MessageName[a, "b"]
// *)
fn abstractMessageName<I: TokenInput + Debug, S: TokenSource + Debug>(
    mut children: Vec<Cst<I, S>>,
    data: S,
) -> Ast {
    let (left, rest) = (children.remove(0), children);

    // FIXME: Port this issues code
    let mut issues = Vec::new();

    // a::b::c::d
    if rest.len() > 2 {
        let source = data.clone().into_general();

        issues.push(Issue::syntax(
            IssueTag::SyntaxUndocumentedMessageName,
            "This syntax is not documented.".to_owned(),
            Severity::Error,
            source,
            1.0,
        ));
    };

    if !issues.is_empty() {
        // TODO: Port this? At the moment, Cst's can't contain syntax
        //       issues anyway, so there is nothing to merge.
        // issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
        // AssociateTo[data, AbstractSyntaxIssues -> issues];
    };

    let data = AstMetadata {
        source: data.into_general(),
        issues,
    };

    let mut children = vec![abstract_(left)];
    children.extend(rest.into_iter().map(|node| match node {
        // LeafNode[String, str_, data_]
        Cst::Token(Token {
            tok: TK::String,
            input: str,
            src: data,
        }) => Ast::string(
            escapeString_of_abstractSymbolString(str.as_str()),
            data,
        ),
        // FIXME: The original WL here didn't call abstract[..] here. Is that
        //        because it is not possible for `children` to contain anything
        //        but LeafNode's?
        child => abstract_(child),
    }));

    Ast::call(st::MessageName, children, data)
}


/// attempt to simplify e.g. Inequality[a, Less, b, Less, c] to Less[a, b, c]
///
/// Also integrate the newer VectorInequality functionality
fn abstractInfixInequality<I: TokenInput + Debug, S: TokenSource + Debug>(
    children: Vec<Cst<I, S>>,
    data: S,
) -> Ast {
    let first = children[0].clone();
    let first = abstract_(first);

    let mut processed: (Ast, Vec<(Symbol, Ast)>) = (first, vec![]);

    let pairs: Vec<(Token<I, S>, Cst<I, S>)> = children[1..]
        .chunks(2)
        .map(|chunk: &[Cst<I, S>]| {
            let [left, right]: [Cst<I, S>; 2] = chunk.to_vec().try_into().unwrap();

            let left = match left {
                Cst::Token(token) => token,
                other => panic!(
                    "abstractInfixInequality: expected odd index to contain Token; got: {other:?}"
                ),
            };

            (left, right)
        })
        .collect();

    //
    // affinity is purposely not True nor False when starting
    //
    let mut affinity = None;

    for (rator, rand) in pairs {
        let rator: Symbol = inequalityOperatorToSymbol(rator.tok);
        let rand = abstract_(rand);

        if vectorInequalityAffinity(rator) == Some(false) {
            if affinity == Some(true) {
                // (*
                // affinity is True, so all operators up to now are 1 sub node
                // *)
                processed = (
                    simplifyInfixInequality(processed, affinity, data.clone()),
                    vec![(rator, rand)],
                );
            } else {
                processed.1.push((rator, rand));
            };
            //
            // affinity is definitely False now
            //
            affinity = Some(false);
        } else if vectorInequalityAffinity(rator) == Some(true) {
            if affinity == Some(false) {
                //
                // affinity is True, so all operators up to now are 1 sub node
                //
                processed = (
                    simplifyInfixInequality(processed, affinity, data.clone()),
                    vec![(rator, rand)],
                );
            } else {
                processed.1.push((rator, rand));
            };
            //
            // affinity is definitely True now
            //
            affinity = Some(true);
        } else {
            processed.1.push((rator, rand));
        }
    }

    simplifyInfixInequality(processed, affinity, data)
}

fn simplifyInfixInequality<S: TokenSource>(
    processed: (Ast, Vec<(Symbol, Ast)>),
    affinity: Option<bool>,
    data: S,
) -> Ast {
    // rators = processed[[2;;-2;;2]];
    // rands = processed[[1;;-1;;2]];

    // let rators = part_span_even_children(part_span_drop_first_and_last(processed), None);
    // let rands = part_span_even_children(processed, None);

    let rators: Vec<Symbol> =
        processed.1.iter().map(|(rator, _)| *rator).collect();
    // TODO(optimization): Refactor to remove clone()'s
    let rands: Vec<Ast> = std::iter::once(processed.0.clone())
        .chain(processed.1.clone().into_iter().map(|(_, rand)| rand))
        .collect();

    let all_rators = |needle: Symbol| -> bool {
        rators.iter().all(|rator| *rator == needle)
    };

    //
    // Try simple cases of all the same operator first
    //
    match rators {
        _ if all_rators(st::Equal) => Ast::call(st::Equal, rands, data),
        _ if all_rators(st::Unequal) => Ast::call(st::Unequal, rands, data),
        _ if all_rators(st::Greater) => Ast::call(st::Greater, rands, data),
        _ if all_rators(st::Less) => Ast::call(st::Less, rands, data),
        _ if all_rators(st::GreaterEqual) => {
            Ast::call(st::GreaterEqual, rands, data)
        },
        _ if all_rators(st::GreaterEqualLess) => {
            Ast::call(st::GreaterEqualLess, rands, data)
        },
        _ if all_rators(st::GreaterFullEqual) => {
            Ast::call(st::GreaterFullEqual, rands, data)
        },
        _ if all_rators(st::GreaterGreater) => {
            Ast::call(st::GreaterGreater, rands, data)
        },
        _ if all_rators(st::GreaterLess) => {
            Ast::call(st::GreaterLess, rands, data)
        },
        _ if all_rators(st::GreaterTilde) => {
            Ast::call(st::GreaterTilde, rands, data)
        },
        _ if all_rators(st::LessEqual) => Ast::call(st::LessEqual, rands, data),
        _ if all_rators(st::LessEqualGreater) => {
            Ast::call(st::LessEqualGreater, rands, data)
        },
        _ if all_rators(st::LessFullEqual) => {
            Ast::call(st::LessFullEqual, rands, data)
        },
        _ if all_rators(st::LessGreater) => {
            Ast::call(st::LessGreater, rands, data)
        },
        _ if all_rators(st::LessLess) => Ast::call(st::LessLess, rands, data),
        _ if all_rators(st::LessTilde) => Ast::call(st::LessTilde, rands, data),
        _ if all_rators(st::NestedGreaterGreater) => {
            Ast::call(st::NestedGreaterGreater, rands, data)
        },
        _ if all_rators(st::NestedLessLess) => {
            Ast::call(st::NestedLessLess, rands, data)
        },
        _ if all_rators(st::NotGreater) => {
            Ast::call(st::NotGreater, rands, data)
        },
        _ if all_rators(st::NotGreaterEqual) => {
            Ast::call(st::NotGreaterEqual, rands, data)
        },
        _ if all_rators(st::NotGreaterFullEqual) => {
            Ast::call(st::NotGreaterFullEqual, rands, data)
        },
        _ if all_rators(st::NotGreaterGreater) => {
            Ast::call(st::NotGreaterGreater, rands, data)
        },
        _ if all_rators(st::NotGreaterLess) => {
            Ast::call(st::NotGreaterLess, rands, data)
        },
        _ if all_rators(st::NotGreaterSlantEqual) => {
            Ast::call(st::NotGreaterSlantEqual, rands, data)
        },
        _ if all_rators(st::NotGreaterTilde) => {
            Ast::call(st::NotGreaterTilde, rands, data)
        },
        _ if all_rators(st::NotLess) => Ast::call(st::NotLess, rands, data),
        _ if all_rators(st::NotLessEqual) => {
            Ast::call(st::NotLessEqual, rands, data)
        },
        _ if all_rators(st::NotLessFullEqual) => {
            Ast::call(st::NotLessFullEqual, rands, data)
        },
        _ if all_rators(st::NotLessGreater) => {
            Ast::call(st::NotLessGreater, rands, data)
        },
        _ if all_rators(st::NotLessLess) => {
            Ast::call(st::NotLessLess, rands, data)
        },
        _ if all_rators(st::NotLessSlantEqual) => {
            Ast::call(st::NotLessSlantEqual, rands, data)
        },
        _ if all_rators(st::NotLessTilde) => {
            Ast::call(st::NotLessTilde, rands, data)
        },
        _ if all_rators(st::NotNestedGreaterGreater) => {
            Ast::call(st::NotNestedGreaterGreater, rands, data)
        },
        _ if all_rators(st::NotNestedLessLess) => {
            Ast::call(st::NotNestedLessLess, rands, data)
        },
        _ if all_rators(st::VectorLess) => {
            //
            // Yes, make sure that it is VectorLess[{a, b, c}] and not VectorLess[a, b, c]
            //
            Ast::call(
                st::VectorLess,
                vec![Ast::call(st::List, rands, AstMetadata::empty())],
                data,
            )
        },
        _ if all_rators(st::VectorGreater) => Ast::call(
            st::VectorGreater,
            vec![Ast::call(st::List, rands, AstMetadata::empty())],
            data,
        ),
        _ if all_rators(st::VectorLessEqual) => Ast::call(
            st::VectorLessEqual,
            vec![Ast::call(st::List, rands, AstMetadata::empty())],
            data,
        ),
        _ if all_rators(st::VectorGreaterEqual) => Ast::call(
            st::VectorGreaterEqual,
            vec![Ast::call(st::List, rands, AstMetadata::empty())],
            data,
        ),
        _ => {
            let children = {
                let mut children = vec![processed.0];

                for (rator, rand) in processed.1 {
                    children.push(Ast::symbol(rator));
                    children.push(rand);
                }

                children
            };

            match affinity {
                Some(true) => {
                    //
                    // Anything containing a combination inequality and Vector inequality operators is abstracted to VectorInequality
                    // Related bugs: 385771
                    //
                    Ast::call(st::Developer::VectorInequality, children, data)
                },
                Some(false) => Ast::call(st::Inequality, children, data),
                None => Ast::call(st::Inequality, children, data),
            }
        },
    }
}

fn inequalityOperatorToSymbol(tok: TokenKind) -> Symbol {
    use crate::symbols as sym;

    match tok {
        TK::EqualEqual | TK::LongName_Equal | TK::LongName_LongEqual => sym::Equal,
        TK::BangEqual | TK::LongName_NotEqual => sym::Unequal,
        TK::Less => sym::Less,
        TK::Greater => sym::Greater,
        TK::LessEqual | TK::LongName_LessEqual => sym::LessEqual,
        TK::GreaterEqual | TK::LongName_GreaterEqual => sym::GreaterEqual,
        TK::LongName_GreaterEqualLess => sym::GreaterEqualLess,
        TK::LongName_GreaterFullEqual => sym::GreaterFullEqual,
        TK::LongName_GreaterGreater => sym::GreaterGreater,
        TK::LongName_GreaterLess => sym::GreaterLess,
        //
        // GreaterSlantEqual parses to GreaterEqual
        // Related bugs: 78439
        //
        TK::LongName_GreaterSlantEqual => sym::GreaterEqual,
        TK::LongName_GreaterTilde => sym::GreaterTilde,
        TK::LongName_LessEqualGreater => sym::LessEqualGreater,
        TK::LongName_LessFullEqual => sym::LessFullEqual,
        TK::LongName_LessGreater => sym::LessGreater,
        TK::LongName_LessLess => sym::LessLess,
        //
        // LessSlantEqual parses to LessEqual
        // Related bugs: 78439
        //
        TK::LongName_LessSlantEqual => sym::LessEqual,
        TK::LongName_LessTilde => sym::LessTilde,
        TK::LongName_NestedGreaterGreater => sym::NestedGreaterGreater,
        TK::LongName_NestedLessLess => sym::NestedLessLess,
        TK::LongName_NotGreater => sym::NotGreater,
        TK::LongName_NotGreaterEqual => sym::NotGreaterEqual,
        TK::LongName_NotGreaterFullEqual => sym::NotGreaterFullEqual,
        TK::LongName_NotGreaterGreater => sym::NotGreaterGreater,
        TK::LongName_NotGreaterLess => sym::NotGreaterLess,
        TK::LongName_NotGreaterSlantEqual => sym::NotGreaterSlantEqual,
        TK::LongName_NotGreaterTilde => sym::NotGreaterTilde,
        TK::LongName_NotLess => sym::NotLess,
        TK::LongName_NotLessEqual => sym::NotLessEqual,
        TK::LongName_NotLessFullEqual => sym::NotLessFullEqual,
        TK::LongName_NotLessGreater => sym::NotLessGreater,
        TK::LongName_NotLessLess => sym::NotLessLess,
        TK::LongName_NotLessSlantEqual => sym::NotLessSlantEqual,
        TK::LongName_NotLessTilde => sym::NotLessTilde,
        TK::LongName_NotNestedGreaterGreater => sym::NotNestedGreaterGreater,
        TK::LongName_NotNestedLessLess => sym::NotNestedLessLess,

        TK::LongName_VectorLess => sym::VectorLess,
        TK::LongName_VectorGreater => sym::VectorGreater,
        TK::LongName_VectorLessEqual => sym::VectorLessEqual,
        TK::LongName_VectorGreaterEqual => sym::VectorGreaterEqual,

        other => panic!(
            "inequalityOperatorToSymbol(): token '{other:?}' is not a known inequality operator"
        ),
    }
}


fn vectorInequalityAffinity(op: Symbol) -> Option<bool> {
    use crate::symbols as sym;

    let boole = match op {
        //
        // Just these operators do not have an affinity, neither True nor False
        //
        sym::Equal
        | sym::Unequal
        | sym::Less
        | sym::Greater
        | sym::LessEqual
        | sym::GreaterEqual => return None,

        //
        // Definitely NOT a VectorInequality
        //
        sym::GreaterEqualLess
        | sym::GreaterFullEqual
        | sym::GreaterGreater
        | sym::GreaterLess
        | sym::GreaterSlantEqual
        | sym::GreaterTilde
        | sym::LessEqualGreater
        | sym::LessFullEqual
        | sym::LessGreater
        | sym::LessLess
        | sym::LessSlantEqual
        | sym::LessTilde
        | sym::NestedGreaterGreater
        | sym::NestedLessLess
        | sym::NotGreater
        | sym::NotGreaterEqual
        | sym::NotGreaterFullEqual
        | sym::NotGreaterGreater
        | sym::NotGreaterLess
        | sym::NotGreaterSlantEqual
        | sym::NotGreaterTilde
        | sym::NotLess
        | sym::NotLessEqual
        | sym::NotLessFullEqual
        | sym::NotLessGreater
        | sym::NotLessLess
        | sym::NotLessSlantEqual
        | sym::NotLessTilde
        | sym::NotNestedGreaterGreater
        | sym::NotNestedLessLess => false,

        //
        // Definitely a VectorInequality
        //
        sym::VectorLess
        | sym::VectorGreater
        | sym::VectorLessEqual
        | sym::VectorGreaterEqual => true,

        _ => panic!("vectorInequalityAffinity(): unrecognized symbol: {op:?}"),
    };

    Some(boole)
}

//======================================

//
// only from boxes
//

fn abstractInfixTilde<I: TokenInput + Debug, S: TokenSource + Debug>(
    children: Vec<Cst<I, S>>,
    data: AstMetadata,
) -> Ast {
    // TODO:
    match children.as_slice() {
        [_, _] => {
            let [left, middle] = expect_children(NodeSeq(children));

            let left = abstract_(left);
            let middle = abstract_(middle);

            Ast::AbstractSyntaxError {
                kind: AbstractSyntaxError::ExpectedTilde,
                args: vec![left, middle],
                data,
            }
        },
        [_, _, _] => {
            let [left, middle, right] = expect_children(NodeSeq(children));

            abstractInfixTildeLeftAlreadyAbstracted(
                abstract_(left),
                vec![middle, right],
                data,
            )
        },
        [left, middle, right, rest @ ..] => {
            // TODO(optimization): Refactor to remove these clone()'s/to_vec().
            let left = left.clone();
            let middle = middle.clone();
            let right = right.clone();
            let rest = rest.to_vec();

            abstractInfixTildeLeftAlreadyAbstracted(
                abstractInfixTilde(
                    vec![left, middle, right],
                    AstMetadata::empty(),
                ),
                rest,
                data,
            )
        },
        _ => panic!(
            "abstractInfixTilde: invalid number of children: {children:?}"
        ),
    }
}

fn abstractInfixTildeLeftAlreadyAbstracted<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    left: Ast,
    rest: Vec<Cst<I, S>>,
    data: AstMetadata,
) -> Ast {
    match rest.as_slice() {
        [_] => {
            let [middle] = expect_children(NodeSeq(rest));
            let middle = abstract_(middle);

            Ast::AbstractSyntaxError {
                kind: AbstractSyntaxError::ExpectedTilde,
                args: vec![left, middle],
                data
            }
        },
        [_, _] => {
            let [middle, right] = expect_children(NodeSeq(rest));

            Ast::call2(abstract_(middle), vec![left, abstract_(right)], data)
        },
        [middle, right, rest @ ..] => {
            let middle = middle.clone();
            let right = right.clone();
            let rest = rest.to_vec();

            abstractInfixTildeLeftAlreadyAbstracted(
                abstractInfixTildeLeftAlreadyAbstracted(left, vec![middle, right], AstMetadata::empty()),
                rest,
                data,
            )
        },
        _ => panic!("abstractInfixTildeLeftAlreadyAbstracted: invalid number of rest: {rest:?}"),
    }
}


//======================================
// abstractGroupNode
//======================================

/// Precondition: Opener and Closer are still present
/// Precondition: Commas are still present
///
/// Removes all commas
///
/// Fills in Nulls and gives SyntaxIssues for e.g. {1,,2}
fn abstractGroupNode<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
    O: Operator,
>(
    group: GroupNode<I, S, O>,
) -> AstCall {
    let data = group.0.get_source();

    let GroupNode(OperatorNode {
        op: tag,
        children: NodeSeq(children),
    }) = group;

    // children = children[[2 ;; -2]];
    let children = part_span_drop_first_and_last(children);

    // abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];
    let abstracted_children = children
        .into_iter()
        .map(abstract_)
        .flat_map(|child: Ast| selectChildren(child))
        .collect();

    /*  FIXME: Port this issues handling code
        issues = Lookup[data, AbstractSyntaxIssues, {}];

        If issues != {} {
            AssociateTo[data, AbstractSyntaxIssues -> issues];
        };
    */

    // CallNode[ToNode[tag], abstractedChildren, data]
    AstCall {
        // TODO(clean): Is this head field read anywhere in the callers of
        //              abstractGroupNode()? I don't think so, since the
        //              ToNode_Op(tag) where tag is CodeParser`* are not valid
        //              abstract syntax nodes anyway.
        head: Box::new(Ast::symbol(tag.to_symbol())),
        args: abstracted_children,
        data: data.into_general(),
    }
}

fn abstractGroupNode_GroupMissingCloserNode<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
    O,
>(
    group: GroupMissingCloserNode<I, S, O>,
) -> (O, Vec<Ast>, AstMetadata) {
    let data = group.0.get_source();

    let GroupMissingCloserNode(OperatorNode {
        op,
        children: NodeSeq(mut children),
    }) = group;

    // children[[2;;]]
    children.remove(0);

    let abstractedChildren = children
        .into_iter()
        .map(abstract_)
        .flat_map(selectChildren)
        .collect();

    (op, abstractedChildren, AstMetadata::from_src(data))
}

fn abstractGroupNode_GroupMissingOpenerNode<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    group: GroupMissingOpenerNode<I, S>,
) -> Ast {
    let data = group.0.get_source();

    let GroupMissingOpenerNode(OperatorNode {
        op,
        children: NodeSeq(mut children),
    }) = group;

    // children[[;;-2]]
    children.pop();

    let abstracted_children = children
        .into_iter()
        .map(abstract_)
        .flat_map(selectChildren)
        .collect();

    Ast::GroupMissingOpener {
        kind: op,
        children: abstracted_children,
        data: AstMetadata::from_src(data),
    }
}


fn selectChildren(node: Ast) -> Vec<Ast> {
    // selectChildren[CallNode[ToNode[Comma], children_, _]] := children
    //
    // selectChildren[n_] := n
    match node {
        // TODO(cleanup): Refactor how Comma nodes are abstracted so that
        //                fake Ast::Call of CodeParser`Comma heads is not
        //                necessary.
        Ast::Call {
            ref head,
            ref args,
            data: _,
        } => {
            if let Ast::Leaf {
                kind: TK::Symbol,
                input,
                data: _,
            } = &**head
            {
                if input.as_str() == "CodeParser`Comma" {
                    // TODO(optimization): Refactor to remove this clone()
                    args.clone()
                } else {
                    vec![node]
                }
            } else {
                vec![node]
            }
        },
        _ => vec![node],
    }
}

//======================================

fn abstractNot2<I: TokenInput + Debug, S: TokenSource + Debug>(
    rand: Cst<I, S>,
    notNotTok: Cst<I, S>,
    data: S,
) -> Ast {
    // notNotData = notNotTok[[3]];
    let notNotData = notNotTok.source();

    // let mut issues = Lookup[data, AbstractSyntaxIssues, {}];
    let mut issues = Vec::new();

    let notNotData_source = notNotData.into_general();

    issues.push(Issue::syntax(
        IssueTag::PrefixNotNot,
        "Unexpected parse.".to_owned(),
        Severity::Warning,
        notNotData_source,
        1.0,
    ));

    let data = AstMetadata {
        source: data.into_general(),
        issues,
    };

    Ast::call(
        st::Not,
        vec![Ast::call(
            st::Not,
            vec![abstract_(rand)],
            AstMetadata::empty(),
        )],
        data,
    )
}

//======================================

fn abstract_box_node<I: TokenInput + Debug, S: TokenSource + Debug>(
    box_node: BoxNode<I, S>,
) -> Ast {
    // FIXME: Add test cases for and finish porting the todo!(..) cases below.
    match box_node.kind {
        //
        // `a` is a List of boxes
        //
        // BoxNode[RowBox, {a_}, data_]
        BoxKind::RowBox => {
            todo!("RowBox[..] abstracting logic")
            // BoxNode[RowBox, {abstract /@ a}, data]
        },
        //
        // `a` is a List of Lists
        //
        // BoxNode[GridBox, {a_, rest___}, data_]
        BoxKind::GridBox => {
            todo!("GridBox[..] abstracting logic");
            // WL!(BoxNode[GridBox, {Map[abstract, a, {2}]} ~Join~ (abstract /@ {rest}), data])
        },
        _ => (),
    }

    let box_node = match try_subscript_box_part_special_cases(box_node) {
        Ok(ast) => return ast,
        Err(node) => node,
    };

    let box_node = match try_superscript_box_derivative_special_case(box_node) {
        Ok(ast) => return ast,
        Err(node) => node,
    };

    //-----------------------------------------
    // Now handle the BoxNode[..] general case:
    //   BoxNode[kind_, children_, data_]
    //-----------------------------------------

    let BoxNode {
        kind,
        children: NodeSeq(children),
        src: data,
    } = box_node;

    let children = children.into_iter().map(abstract_).collect();

    Ast::Box {
        kind,
        args: children,
        data: data.into(),
    }
}

/// Handle special form of `[[x]]` in subscript
///
/// Keep the `[[]]` structure un-abstracted
//
// FIXME: when things like SuperscriptBox[] -> Power[] and
//        FractionBox[] -> Divide, then also do
//        SubscriptBox[..., [[]] ] -> Part
fn try_subscript_box_part_special_cases<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    box_node: BoxNode<I, S>,
) -> Result<Ast, BoxNode<I, S>> {
    /* Original WL pattern:
    BoxNode[
        SubscriptBox,
        {
            a_,
            GroupNode[GroupSquare, {
                o1:LeafNode[Token`OpenSquare, _, _],
                GroupNode[GroupSquare, {
                    o2:LeafNode[Token`OpenSquare, _, _],
                    b_,
                    c2:LeafNode[Token`CloseSquare, _, _]
                },
                    data2_
                ],
                c1:LeafNode[Token`CloseSquare, _, _]
            },
                data1_
            ],
            ___
        },
        data_
    ]
    */
    let (children, data) = match Cst::from(box_node.clone()) {
        LHS!(BoxNode[
            SubscriptBox,
            children:_,
            data:_
        ]) => (children, data),
        _ => return Err(box_node),
    };

    let (a, middle) = match children.0.as_slice() {
        [a, middle, ..] => (a.clone(), middle.clone()),
        _ => return Err(box_node),
    };

    let data1 = middle.get_source();

    match middle {
        LHS!(GroupNode[
            CodeParser_GroupSquare,
            children1:_
        ]) => {
            let [o1, group, c1] = expect_children(children1);

            match o1 {
                LHS!(LeafNode[OpenSquare, _, _]) => (),
                _ => return Err(box_node),
            };

            match c1 {
                LHS!(LeafNode[CloseSquare, _, _]) => (),
                _ => return Err(box_node),
            }

            let data2 = group.get_source();

            match group {
                LHS!(GroupNode[
                    CodeParser_GroupSquare,
                    children2:_
                ]) => {
                    let [o2, b, c2] = expect_children(children2);

                    match o2 {
                        LHS!(LeafNode[OpenSquare, _, _]) => (),
                        _ => return Err(box_node),
                    };

                    match c2 {
                        LHS!(LeafNode[CloseSquare, _, _]) => (),
                        _ => return Err(box_node),
                    }

                    let (o1, o2) = (abstract_(o1), abstract_(o2));
                    let (c1, c2) = (abstract_(c1), abstract_(c2));

                    let ast = Ast::Box {
                        kind: BoxKind::SubscriptBox,
                        args: vec![
                            abstract_(a),
                            Ast::Group {
                                kind: GroupOperator::CodeParser_GroupSquare,
                                children: Box::new((
                                    o1,
                                    Ast::Group{
                                        kind: GroupOperator::CodeParser_GroupSquare,
                                        children: Box::new((
                                            o2,
                                            abstract_(b),
                                            c2
                                        )),
                                        data: AstMetadata::from_src(data2)
                                    },
                                    c1
                                )),
                                data: AstMetadata::from_src(data1)
                            }
                        ],
                        data: data.into()
                    };

                    return Ok(ast);
                },
                _ => return Err(box_node),
            }
        },
        LHS!(GroupNode[
            CodeParser_GroupDoubleBracket,
            children1:_
        ]) => {
            let [o, b, c] = expect_children(children1);

            match o {
                LHS!(LeafNode[LongName_LeftDoubleBracket, _, _]) => (),
                _ => return Err(box_node),
            };

            match c {
                LHS!(LeafNode[LongName_RightDoubleBracket, _, _]) => (),
                _ => return Err(box_node),
            }

            // BoxNode[SubscriptBox, {abstract[a], GroupNode[GroupDoubleBracket, {o, abstract[b], c}, data1]}, data]
            let ast = Ast::Box {
                kind: BoxKind::SubscriptBox,
                args: vec![
                    abstract_(a),
                    Ast::Group {
                        kind: GroupOperator::CodeParser_GroupDoubleBracket,
                        children: Box::new((
                            abstract_(o),
                            abstract_(b),
                            abstract_(c),
                        )),
                        data: AstMetadata::from_src(data1),
                    },
                ],
                data: data.into(),
            };

            return Ok(ast);
        },
        _ => Err(box_node),
    }
}

/// Handle special form of `TagBox[(), Derivative]` in superscript
///
/// Keep the `TagBox[(), Derivative]` structure un-abstracted
///
/// TagBox is considered "easier" than say, FormBox
///
/// Contents of TagBox are largely valid boxes
///
// FIXME: when things like SuperscriptBox[] -> Power[] and
//        FractionBox[] -> Divide, then also do
//        SuperscriptBox[..., TagBox[(), Derivative] ] -> Derivative
//
// FIXME: maybe first arg of TagBox should be treated as a CodeNode and not parsed at all
fn try_superscript_box_derivative_special_case<
    I: TokenInput + Debug,
    S: TokenSource + Debug,
>(
    box_node: BoxNode<I, S>,
) -> Result<Ast, BoxNode<I, S>> {
    /* Original WL pattern that the nested Rust match/if let statements below
       are unpacking. If this pattern matches, an `Ok(Ast)` is returned. If
       this pattern does not match, then the original BoxNode is returned as
       the `Err(_)` value.

    BoxNode[
        SuperscriptBox,
        {
            a_,
            BoxNode[TagBox, {
                GroupNode[GroupParen, {
                    o:LeafNode[Token`OpenParen, _, _],
                    b_,
                    c:LeafNode[Token`CloseParen, _, _]
                },
                    data2_
                ],
                t:CodeNode[Evaluated, Derivative, _]},
                data1_
            ],
            ___
        },
        data_
    ]
        => BoxNode[SuperscriptBox, {
            abstract[a],
            BoxNode[TagBox, {
                GroupNode[GroupParen, {o, abstract[b], c}, data2],
                t
            }, data1]
        }, data]
    */
    let LHS!(BoxNode[
        SuperscriptBox,
        children:_
        ,
        data:_
    ]) = Cst::from(box_node.clone())
    else {
        return Err(box_node);
    };

    let (a, middle) = match children.0.as_slice() {
        [a, middle, ..] => (a.clone(), middle.clone()),
        _ => todo!("Error?"),
    };

    let LHS!(BoxNode[
        TagBox,
        children1:_,
        data1:_
    ]) = middle
    else {
        return Err(box_node);
    };

    let [left, t] = expect_children(children1);

    let data2 = left.get_source();

    let LHS!(GroupNode[
        CodeParser_GroupParen,
        children2:_
    ]) = left
    else {
        return Err(box_node);
    };

    match t {
        // CodeNode[Null, Derivative, _]
        Cst::Code(
            ref t @ CodeNode {
                ref first,
                ref second,
                src: _,
            },
        ) if first.try_as_symbol().map(wolfram_expr::Symbol::as_str)
            == Some("System`Evaluated")
            && second.try_as_symbol().map(wolfram_expr::Symbol::as_str)
                == Some("System`Derivative") =>
        {
            let [o, b, c] = expect_children(children2);

            let [o @ LHS!(LeafNode[OpenParen, _, _]), b @ _, c @ LHS!(LeafNode[CloseParen, _, _])] =
                [o, b, c]
            else {
                return Err(box_node);
            };

            let o = abstract_(o);
            let b = abstract_(b);
            let c = abstract_(c);

            let t = {
                let CodeNode { first, second, src } = t.clone();
                CodeNode {
                    first,
                    second,
                    src: src.into_general(),
                }
            };

            let ast = Ast::Box {
                kind: BoxKind::SuperscriptBox,
                args: vec![
                    abstract_(a),
                    // GroupNode[
                    //     CodeParser_GroupParen,
                    //     NodeSeq(vec![o, abstract_(b), c]),
                    //     data2
                    // ]
                    Ast::TagBox_GroupParen {
                        group: Box::new((o, b, c, data2.into_general())),
                        tag: t,
                        data: AstMetadata::from_src(data1),
                    },
                ],
                data: data.into(),
            };

            Ok(ast)
        },
        _ => Err(box_node),
    }
}

//======================================

fn is_even(x: usize) -> bool {
    x % 2 == 0
}

fn is_odd(x: usize) -> bool {
    x % 2 == 1
}

/// `children[[;; ;; 2]]`
fn part_span_even_children<I: Debug, S: Debug>(
    children: Vec<Cst<I, S>>,
    debug_expected_separator: Option<TokenKind>,
) -> Vec<Cst<I, S>> {
    children
        .into_iter()
        .enumerate()
        .filter(|(index, child)| {
            debug_assert!(
                is_even(*index) || is_expected_separator(child, debug_expected_separator),
                "child with is_even == {}, expected to be {debug_expected_separator:?}, was: {child:?}",
                is_even(*index)
            );

            is_even(*index)
        })
        .map(|(_, child)| child)
        .collect()
}

fn is_expected_separator<I, S>(
    child: &Cst<I, S>,
    debug_expected_separator: Option<TokenKind>,
) -> bool {
    if let Some(sep) = debug_expected_separator {
        match child {
            Cst::Token(Token { tok, .. }) if *tok == sep => true,
            _ => false,
        }
    } else {
        true
    }
}

/// `children[[2 ;; -2]]`
fn part_span_drop_first_and_last<I, S>(
    mut children: Vec<Cst<I, S>>,
) -> Vec<Cst<I, S>> {
    children.remove(0);
    children.pop().unwrap();
    children
}
