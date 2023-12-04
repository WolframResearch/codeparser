use crate::{
    abstract_cst::{abstract_cst, aggregate_cst},
    ast::Ast,
    cst::{
        BinaryNode, CallBody, CallHead, CallNode, CompoundNode,
        Cst::{
            Binary, Call, Compound, Group, GroupMissingCloser, Infix, Postfix,
            Prefix, PrefixBinary, SyntaxError, Ternary, Token,
        },
        GroupMissingCloserNode, GroupNode, InfixNode, OperatorNode,
        PostfixNode, PrefixBinaryNode, PrefixNode, SyntaxErrorNode,
        TernaryNode,
    },
    macros::{leaf, src, token},
    parse::{
        operators::{
            BinaryOperator as BinaryOp, CallOperator as CallOp,
            CompoundOperator as CompoundOp, GroupOperator as GroupOp,
            InfixOperator as InfixOp, PostfixOperator as PostfixOp,
            PrefixBinaryOperator, PrefixOperator, PrefixOperator as PrefixOp,
            TernaryOperator as TernaryOp,
        },
        SyntaxErrorKind,
    },
    parse_ast, parse_cst, parse_cst_seq,
    tests::assert_src,
    tokenize::{TokenKind, TokenString},
    NodeSeq, QuirkSettings,
};

use pretty_assertions::assert_eq;

#[test]
fn ternary_tag_set() {
    //==================================
    // Typical TagSet (TID:231105/3)
    //==================================

    assert_eq!(
        parse_cst("a /: b[a] = c", &Default::default()).syntax,
        assert_src!(1:1-14 => Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::TagSet,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(SlashColon, "/:", 1:3-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                assert_src!(1:6-10 => Call(CallNode {
                    head: CallHead::Concrete(NodeSeq(vec![Token(
                        token!(Symbol, "b", 1:6-7),
                    )])),
                    body: assert_src!(1:7-10 => CallBody::Group(GroupNode(OperatorNode {
                        op: CallOp::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:7-8)),
                            Token(token!(Symbol, "a", 1:8-9)),
                            Token(token!(CloseSquare, "]", 1:9-10)),
                        ]),
                    }))),
                })),
                Token(token!(Whitespace, " ", 1:10-11)),
                Token(token!(Equal, "=", 1:11-12)),
                Token(token!(Whitespace, " ", 1:12-13)),
                Token(token!(Symbol, "c", 1:13-14)),
            ]),
        })))
    );

    assert_eq!(
        parse_cst("a /: b = (c = d)", &Default::default()).syntax,
        assert_src!(1:1-17 => Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::TagSet,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Token(token!(SlashColon, "/:", 1:3-5),),
                Token(token!(Whitespace, " ", 1:5-6),),
                Token(token!(Symbol, "b", 1:6-7),),
                Token(token!(Whitespace, " ", 1:7-8),),
                Token(token!(Equal, "=", 1:8-9),),
                Token(token!(Whitespace, " ", 1:9-10),),
                assert_src!(1:10-17 => Group(GroupNode(OperatorNode {
                    op: GroupOp::CodeParser_GroupParen,
                    children: NodeSeq(vec![
                        Token(token!(OpenParen, "(", 1:10-11),),
                        assert_src!(1:11-16 => Binary(BinaryNode(OperatorNode {
                            op: BinaryOp::Set,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "c", 1:11-12),),
                                Token(token!(Whitespace, " ", 1:12-13),),
                                Token(token!(Equal, "=", 1:13-14),),
                                Token(token!(Whitespace, " ", 1:14-15),),
                                Token(token!(Symbol, "d", 1:15-16),),
                            ]),
                        }))),
                        Token(token!(CloseParen, ")", 1:16-17),),
                    ]),
                }))),
            ]),
        })))
    );
}

#[test]
fn ternary_tag_unset() {
    //==================================
    // Typical TagUnset (TID:231105/1)
    //==================================

    assert_eq!(
        parse_cst("a /: b[a] =.", &Default::default()).syntax,
        assert_src!(1:1-13 => Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::TagUnset,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(SlashColon, "/:", 1:3-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                assert_src!(1:6-10 => Call(CallNode {
                    head: CallHead::Concrete(NodeSeq(vec![Token(
                        token!(Symbol, "b", 1:6-7),
                    )])),
                    body: assert_src!(1:7-10 => CallBody::Group(GroupNode(OperatorNode {
                        op: CallOp::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:7-8)),
                            Token(token!(Symbol, "a", 1:8-9)),
                            Token(token!(CloseSquare, "]", 1:9-10)),
                        ]),
                    }))),
                })),
                Token(token!(Whitespace, " ", 1:10-11)),
                Token(token!(Equal, "=", 1:11-12)),
                Token(token!(Dot, ".", 1:12-13)),
            ]),
        })))
    );

    //=====================================
    // TagUnset with interior trivia ("= .") (TID:231105/2)
    //=====================================

    assert_eq!(
        parse_cst("a /: b[a] = .", &Default::default()).syntax,
        assert_src!(1:1-14 => Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::TagUnset,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(SlashColon, "/:", 1:3-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                assert_src!(1:6-10 => Call(CallNode {
                    head: CallHead::Concrete(NodeSeq(vec![Token(
                        token!(Symbol, "b", 1:6-7),
                    )])),
                    body: assert_src!(1:7-10 => CallBody::Group(GroupNode(OperatorNode {
                        op: CallOp::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:7-8)),
                            Token(token!(Symbol, "a", 1:8-9)),
                            Token(token!(CloseSquare, "]", 1:9-10)),
                        ]),
                    }))),
                })),
                Token(token!(Whitespace, " ", 1:10-11)),
                Token(token!(Equal, "=", 1:11-12)),
                Token(token!(Whitespace, " ", 1:12-13)),
                Token(token!(Dot, ".", 1:13-14)),
            ]),
        })))
    )
}

#[test]
fn test_prefix_binary_integral() {
    //==================================
    // TID:231113/1: "\[Integral] f \[DifferentialD] x"
    //==================================

    assert_eq!(
        parse_cst(r#"\[Integral] f \[DifferentialD] x"#, &Default::default())
            .syntax,
        PrefixBinary(PrefixBinaryNode(OperatorNode {
            op: PrefixBinaryOperator::Integrate,
            children: NodeSeq(vec![
                Token(token!(LongName_Integral, "\\[Integral]", 1:1-12)),
                Token(token!(Whitespace, " ", 1:12-13)),
                Token(token!(Symbol, "f", 1:13-14)),
                Token(token!(Whitespace, " ", 1:14-15)),
                Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::DifferentialD,
                    children: NodeSeq(vec![
                        Token(
                            token!(LongName_DifferentialD, "\\[DifferentialD]", 1:15-31)
                        ),
                        Token(token!(Whitespace, " ", 1:31-32)),
                        Token(token!(Symbol, "x", 1:32-33))
                    ])
                })),
            ]),
        }))
    );

    //==================================
    // TID:231113/2: "\[Integral] \[DifferentialD] x"
    //==================================

    assert_eq!(
        parse_cst(r#"\[Integral] \[DifferentialD] x"#, &Default::default())
            .syntax,
        PrefixBinary(PrefixBinaryNode(OperatorNode {
            op: PrefixBinaryOperator::Integrate,
            children: NodeSeq(vec![
                Token(token!(LongName_Integral, "\\[Integral]", 1:1-12)),
                Token(token!(Whitespace, " ", 1:12-13)),
                Token(token!(Fake_ImplicitOne, "", 1:13-13)),
                Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::DifferentialD,
                    children: NodeSeq(vec![
                        Token(
                            token!(LongName_DifferentialD, "\\[DifferentialD]", 1:13-29)
                        ),
                        Token(token!(Whitespace, " ", 1:29-30)),
                        Token(token!(Symbol, "x", 1:30-31)),
                    ])
                })),
            ]),
        }))
    );

    //==================================
    // TID:231113/3: "\[Integral] f"
    //==================================
    // In the FE, this is a syntax error. In the Kernel, it is Integral[f]

    let cst = parse_cst(r#"\[Integral] f"#, &Default::default()).syntax;

    assert_eq!(
        cst,
        Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Integral,
            children: NodeSeq(vec![
                Token(token!(LongName_Integral, "\\[Integral]", 1:1-12)),
                Token(token!(Whitespace, " ", 1:12-13)),
                Token(token!(Symbol, "f", 1:13-14)),
            ])
        }))
    );

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Integral", <||>)),
            args: vec![leaf!(Symbol, "f", 1:13-14),],
            data: src!(1:1-14).into(),
        }
    );
}

#[test]
fn test_span() {
    assert_eq!(
        parse_cst(";; ;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1)),
                        Token(token!(SemiSemi, ";;", 1:1-3)),
                        Token(token!(Whitespace, " ", 1:3-4)),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4)),
                    ])
                })),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4)),
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4)),
                        Token(token!(SemiSemi, ";;", 1:4-6)),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6)),
                    ])
                })),
            ]),
        }))
    );

    //==============================
    // Implicit times and span
    //==============================

    use crate::parse::operators::{BinaryOperator::Span, InfixOperator::Times};

    assert_eq!(
        parse_cst(";; ;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Whitespace, " ", 1:3-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;", &Default::default()).syntax,
        Binary(BinaryNode(OperatorNode {
            op: Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(SemiSemi, ";;", 1:2-4),),
                Token(token!(Fake_ImplicitAll, "", 1:4-4),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b", &Default::default()).syntax,
        Binary(BinaryNode(OperatorNode {
            op: Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(SemiSemi, ";;", 1:2-4),),
                Token(token!(Symbol, "b", 1:4-5),),
            ]),
        },),)
    );


    assert_eq!(
        parse_cst("a;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "a", 1:3-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;a", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                Token(token!(SemiSemi, ";;", 1:1-3),),
                Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                Token(token!(SemiSemi, ";;", 1:3-5),),
                Token(token!(Symbol, "a", 1:5-6),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                Token(token!(SemiSemi, ";;", 1:1-3),),
                Token(token!(Symbol, "a", 1:3-4),),
                Token(token!(SemiSemi, ";;", 1:4-6),),
                Token(token!(Symbol, "b", 1:6-7),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;;;b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(SemiSemi, ";;", 1:2-4),),
                Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                Token(token!(SemiSemi, ";;", 1:4-6),),
                Token(token!(Symbol, "b", 1:6-7),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b;;c", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(SemiSemi, ";;", 1:2-4),),
                Token(token!(Symbol, "b", 1:4-5),),
                Token(token!(SemiSemi, ";;", 1:5-7),),
                Token(token!(Symbol, "c", 1:7-8),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;b;;c;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "b", 1:3-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Symbol, "c", 1:6-7),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Fake_ImplicitAll, "", 1:9-9),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b;;c;;d", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Symbol, "b", 1:4-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "c", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "d", 1:10-11),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "a", 1:3-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                    ]),
                },),),
            ]),
        },),)
    );
    //==============================
    assert_eq!(
        parse_cst(";;;;a;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Symbol, "a", 1:5-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;a", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "a", 1:7-8),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Fake_ImplicitAll, "", 1:9-9),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;;;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Fake_ImplicitAll, "", 1:10-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "a", 1:3-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Fake_ImplicitAll, "", 1:10-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;a;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Symbol, "a", 1:5-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Fake_ImplicitAll, "", 1:10-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;a;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "a", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Fake_ImplicitAll, "", 1:10-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;;;a", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Symbol, "a", 1:9-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Symbol, "b", 1:8-9),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "a", 1:3-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Symbol, "b", 1:8-9),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;a;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Symbol, "a", 1:5-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Symbol, "b", 1:8-9),),
                    ]),
                },),),
            ]),
        },),)
    );


    assert_eq!(
        parse_cst(";;;;;;a", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "a", 1:7-8),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Symbol, "b", 1:9-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;;;;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "b", 1:10-11),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "a", 1:3-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "b", 1:10-11),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;a;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Symbol, "a", 1:5-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Fake_ImplicitAll, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "b", 1:10-11),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;a;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "a", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "b", 1:10-11),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;;;a", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Symbol, "a", 1:9-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Symbol, "b", 1:9-10),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;c;;;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Symbol, "c", 1:4-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Fake_ImplicitAll, "", 1:9-9),),
                        Token(token!(SemiSemi, ";;", 1:9-11),),
                        Token(token!(Symbol, "b", 1:11-12),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;c;;;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Symbol, "a", 1:3-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Symbol, "c", 1:6-7),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Fake_ImplicitAll, "", 1:9-9),),
                        Token(token!(SemiSemi, ";;", 1:9-11),),
                        Token(token!(Symbol, "b", 1:11-12),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;a;;c;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Symbol, "a", 1:5-6),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Symbol, "c", 1:8-9),),
                        Token(token!(SemiSemi, ";;", 1:9-11),),
                        Token(token!(Symbol, "b", 1:11-12),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("c;;;;;;a;;b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "c", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                        Token(token!(SemiSemi, ";;", 1:6-8),),
                        Token(token!(Symbol, "a", 1:8-9),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:9-9),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:9-9),),
                        Token(token!(SemiSemi, ";;", 1:9-11),),
                        Token(token!(Symbol, "b", 1:11-12),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("{ ;;\n;; }", &Default::default()).syntax,
        Group(GroupNode(OperatorNode {
            op: GroupOp::List,
            children: NodeSeq(vec![
                Token(token!(OpenCurly, "{", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Infix(InfixNode(OperatorNode {
                    op: Times,
                    children: NodeSeq(vec![
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                                Token(token!(SemiSemi, ";;", 1:3-5),),
                                Token(token!(InternalNewline, "\n", 1:5-2:1),),
                                Token(token!(Fake_ImplicitAll, "", 2:1-1),),
                            ]),
                        },),),
                        Token(token!(Fake_ImplicitTimes, "", 2:1-1),),
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 2:1-1),),
                                Token(token!(SemiSemi, ";;", 2:1-3),),
                                Token(token!(Whitespace, " ", 2:3-4),),
                                Token(token!(Fake_ImplicitAll, "", 2:4-4),),
                            ]),
                        },),),
                    ]),
                },),),
                Token(token!(CloseCurly, "}", 2:4-5),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("{ ;;\n;;a }", &Default::default()).syntax,
        Group(GroupNode(OperatorNode {
            op: GroupOp::List,
            children: NodeSeq(vec![
                Token(token!(OpenCurly, "{", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(InternalNewline, "\n", 1:5-2:1),),
                        Token(token!(Fake_ImplicitAll, "", 2:1-1),),
                        Token(token!(SemiSemi, ";;", 2:1-3),),
                        Token(token!(Symbol, "a", 2:3-4),),
                    ]),
                },),),
                Token(token!(Whitespace, " ", 2:4-5),),
                Token(token!(CloseCurly, "}", 2:5-6),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;;;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Fake_ImplicitAll, "", 1:3-3),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:3-3),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:3-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Fake_ImplicitAll, "", 1:5-5),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Fake_ImplicitAll, "", 1:9-9),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b;;c;;d;;e", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Symbol, "b", 1:4-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "c", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "d", 1:10-11),),
                        Token(token!(SemiSemi, ";;", 1:11-13),),
                        Token(token!(Symbol, "e", 1:13-14),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b;;c;;d;;e;;f", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Symbol, "b", 1:4-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "c", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "d", 1:10-11),),
                        Token(token!(SemiSemi, ";;", 1:11-13),),
                        Token(token!(Symbol, "e", 1:13-14),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:14-14),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:14-14),),
                        Token(token!(SemiSemi, ";;", 1:14-16),),
                        Token(token!(Symbol, "f", 1:16-17),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b;;c;;d;;e;;f;;g", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Symbol, "b", 1:4-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "c", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "d", 1:10-11),),
                        Token(token!(SemiSemi, ";;", 1:11-13),),
                        Token(token!(Symbol, "e", 1:13-14),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:14-14),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:14-14),),
                        Token(token!(SemiSemi, ";;", 1:14-16),),
                        Token(token!(Symbol, "f", 1:16-17),),
                        Token(token!(SemiSemi, ";;", 1:17-19),),
                        Token(token!(Symbol, "g", 1:19-20),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;b;;c;;d;;e;;f;;g;;h", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(SemiSemi, ";;", 1:2-4),),
                        Token(token!(Symbol, "b", 1:4-5),),
                        Token(token!(SemiSemi, ";;", 1:5-7),),
                        Token(token!(Symbol, "c", 1:7-8),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:8-8),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:8-8),),
                        Token(token!(SemiSemi, ";;", 1:8-10),),
                        Token(token!(Symbol, "d", 1:10-11),),
                        Token(token!(SemiSemi, ";;", 1:11-13),),
                        Token(token!(Symbol, "e", 1:13-14),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:14-14),),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:14-14),),
                        Token(token!(SemiSemi, ";;", 1:14-16),),
                        Token(token!(Symbol, "f", 1:16-17),),
                        Token(token!(SemiSemi, ";;", 1:17-19),),
                        Token(token!(Symbol, "g", 1:19-20),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:20-20),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:20-20),),
                        Token(token!(SemiSemi, ";;", 1:20-22),),
                        Token(token!(Symbol, "h", 1:22-23),),
                    ]),
                },),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("a;;;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::CompoundExpression,
            children: NodeSeq(vec![
                Infix(InfixNode(OperatorNode {
                    op: Times,
                    children: NodeSeq(vec![
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:1-2),),
                                Token(token!(SemiSemi, ";;", 1:2-4),),
                                Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                            ]),
                        },),),
                        Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                                Token(token!(SemiSemi, ";;", 1:4-6),),
                                Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                            ]),
                        },),),
                    ]),
                },),),
                Token(token!(Semi, ";", 1:6-7),),
                Token(token!(Fake_ImplicitNull, "", 1:7-7),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst("b;;a;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::CompoundExpression,
            children: NodeSeq(vec![
                Infix(InfixNode(OperatorNode {
                    op: Times,
                    children: NodeSeq(vec![
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "b", 1:1-2),),
                                Token(token!(SemiSemi, ";;", 1:2-4),),
                                Token(token!(Symbol, "a", 1:4-5),),
                            ]),
                        },),),
                        Token(token!(Fake_ImplicitTimes, "", 1:5-5),),
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:5-5),),
                                Token(token!(SemiSemi, ";;", 1:5-7),),
                                Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                            ]),
                        },),),
                    ]),
                },),),
                Token(token!(Semi, ";", 1:7-8),),
                Token(token!(Fake_ImplicitNull, "", 1:8-8),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst(";;a;;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::CompoundExpression,
            children: NodeSeq(vec![
                Infix(InfixNode(OperatorNode {
                    op: Times,
                    children: NodeSeq(vec![
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                                Token(token!(SemiSemi, ";;", 1:1-3),),
                                Token(token!(Symbol, "a", 1:3-4),),
                            ]),
                        },),),
                        Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                        Binary(BinaryNode(OperatorNode {
                            op: Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                                Token(token!(SemiSemi, ";;", 1:4-6),),
                                Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                            ]),
                        },),),
                    ]),
                },),),
                Token(token!(Semi, ";", 1:6-7),),
                Token(token!(Fake_ImplicitNull, "", 1:7-7),),
            ]),
        },),)
    );

    assert_eq!(
        parse_ast("a;;!", &Default::default()).syntax,
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Span", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Not", <||>)),
                    args: vec![Ast::Error {
                        kind: TokenKind::Error_ExpectedOperand,
                        input: TokenString::new(""),
                        data: src!(1:5-5).into(),
                    },],
                    data: src!(1:4-5).into(),
                },
            ],
            data: src!(1:1-5).into(),
        }
    );

    //
    // verify that nested ImplicitTimes are not created
    //

    assert_eq!(
        parse_cst(";; ;; ;;", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:1-1),),
                        Token(token!(SemiSemi, ";;", 1:1-3),),
                        Token(token!(Whitespace, " ", 1:3-4),),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:4-4),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:4-4),),
                        Token(token!(SemiSemi, ";;", 1:4-6),),
                        Token(token!(Whitespace, " ", 1:6-7),),
                        Token(token!(Fake_ImplicitAll, "", 1:7-7),),
                    ]),
                },),),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Fake_ImplicitOne, "", 1:7-7),),
                        Token(token!(SemiSemi, ";;", 1:7-9),),
                        Token(token!(Fake_ImplicitAll, "", 1:9-9),),
                    ]),
                },),),
            ]),
        },),)
    );


    //==================================


    assert_eq!(
        parse_cst("a ;; &", &Default::default()).syntax,
        Postfix(PostfixNode(OperatorNode {
            op: PostfixOp::Function,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2),),
                        Token(token!(Whitespace, " ", 1:2-3),),
                        Token(token!(SemiSemi, ";;", 1:3-5),),
                        Token(token!(Whitespace, " ", 1:5-6),),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                    ]),
                },),),
                Token(token!(Amp, "&", 1:6-7),),
            ]),
        },),)
    );

    assert_eq!(
        parse_cst_seq("a ;; \\t", &Default::default()).syntax,
        NodeSeq(vec![
            Binary(BinaryNode(OperatorNode {
                op: Span,
                children: NodeSeq(vec![
                    Token(token!(Symbol, "a", 1:1-2),),
                    Token(token!(Whitespace, " ", 1:2-3),),
                    Token(token!(SemiSemi, ";;", 1:3-5),),
                    Token(token!(Whitespace, " ", 1:5-6),),
                    Token(token!(Fake_ImplicitAll, "", 1:6-6),),
                ]),
            },),),
            Token(token!(Error_UnhandledCharacter, "\\t", 1:6-8),),
        ])
    );

    assert_eq!(
        parse_cst("a ;; b ;; c", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Token(token!(SemiSemi, ";;", 1:3-5),),
                Token(token!(Whitespace, " ", 1:5-6),),
                Token(token!(Symbol, "b", 1:6-7),),
                Token(token!(Whitespace, " ", 1:7-8),),
                Token(token!(SemiSemi, ";;", 1:8-10),),
                Token(token!(Whitespace, " ", 1:10-11),),
                Token(token!(Symbol, "c", 1:11-12),),
            ]),
        },),)
    );
}

#[test]
fn test_tilde() {
    assert_eq!(
        parse_cst("a~f~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Token(token!(Symbol, "f", 1:3-4)),
                Token(token!(Tilde, "~", 1:4-5)),
                Token(token!(Symbol, "b", 1:5-6)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f~", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Token(token!(Symbol, "f", 1:3-4)),
                Token(token!(Tilde, "~", 1:4-5)),
                Token(token!(Error_ExpectedOperand, "", 1:5-5)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~ ~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Token(token!(Whitespace, " ", 1:3-4)),
                Token(token!(Error_ExpectedOperand, "", 1:4-4)),
                Token(token!(Tilde, "~", 1:4-5)),
                Token(token!(Symbol, "b", 1:5-6)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("~ ~", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Error_ExpectedOperand, "", 1:1-1)),
                Token(token!(Tilde, "~", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(Error_ExpectedOperand, "", 1:3-3)),
                Token(token!(Tilde, "~", 1:3-4)),
                Token(token!(Error_ExpectedOperand, "", 1:4-4)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("~~", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::StringExpression,
            children: NodeSeq(vec![
                Token(token!(Error_ExpectedOperand, "", 1:1-1)),
                Token(token!(TildeTilde, "~~", 1:1-3)),
                Token(token!(Error_ExpectedOperand, "", 1:3-3)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~x~f~y~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::CodeParser_TernaryTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "x", 1:3-4)),
                        Token(token!(Tilde, "~", 1:4-5)),
                        Token(token!(Symbol, "f", 1:5-6)),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "y", 1:7-8)),
                Token(token!(Tilde, "~", 1:8-9)),
                Token(token!(Symbol, "b", 1:9-10)),
            ]),
        }))
    );

    //==================================
    // Tilde precedence
    //==================================

    // Input         | FrontEnd | Kernel   | CodeParser | Comment
    // --------------|----------|----------|------------|-------------------------
    // "x*a~f~c"     | ✅       | ✅        | ✅         |
    // "a~(x*f)~b"   | ✅       | ✅        | ✅         |
    //
    // Operators with LOWER precedence than ~ inside of a ~ sequence
    //
    // "a~x*f~b"     | ❌       | ❌        | ❌         | syntax error because * has lower precedence than ~
    // "a~x f~b"     | ❌       | ❌        | ❌         | syntax error because implicit Times has lower precedence than ~
    // "a ~f x"      | ❌       | ❌        | ❌         |
    // "a~f;;~b"     | ❌       | ✅        | ❌         | valid, despite ;; having lower precedence than ~
    // "a~f;;x~b"    | ❌       | ❌        | ❌         |
    // "a~f;;x;;~b"  | ❌       | ✅        | ❌         | valid, despite ;; having lower precedence than ~
    // "a ~f;~ b"    | ❌       | ✅        | ❌         | valid, despite ;  having lower precedence than ~
    // "a ~f!~ b"    | ❌       | ✅        | ❌         | valid, despite !  having lower precedence than ~
    // "a~f+x&~b"    | ❌       | ✅        | ❌         | valid, despite + and & both having lower prec than ~
    // "a~f+x!~b"    | ❌       | ❌        | ❌         |
    // "a~f?x!~b"    | ❌       | ✅        | ❌         |
    //
    // "a ~f,~ b"   -- syntax error
    // "a~f,x~b"    -- syntax error
    // "{a ~f,~ b}" -- syntax error
    //
    // Operators with HIGHER precedence than ~ inside of a ~ sequence
    //
    // "a~f@x~b"     | ✅       | ✅        | ✅         |
    // "a~f/*g~b"    | ✅       | ✅        | ✅         |
    // "a~f@*g~b"    | ✅       | ✅        | ✅         |
    // "a~++f~b"     | ✅       | ✅        | ✅         |
    // "a~f--~b"     | ✅       | ✅        | ✅         |
    // "a~f[x]~b"    | ✅       | ✅        | ✅         |
    // "a~f?h~b"     | ✅       | ✅        | ✅         |
    // "a~<<g@x~b"   | ✅       | ❌        | ✅         | Kernel requires a space after `<<g` due to file ops weirdness
    // "a~<<g @x~b"  | ✅       | ✅        | ✅         | Space after g is significant to the Kernel parser
    // "a~f::x~b"    | ✅       | ✅        | ✅         |
    // "a~f_x~b"     | ✅       | ✅        | ✅         |
    //
    // Operators in tilde first operand
    //
    // "a;;~b~c"     | ❌       | ✅        | ✅         |
    // "a;;b~c~d"    | ✅       | ✅        | ✅         |
    // "a;;b;;~d~e"  | ❌       | ✅        | ✅⚠️       | * CodeParser expr is different from Kernel
    // "a;;b;;c~d~e" | ✅       | ✅        | ✅         |
    // "a!~b~c"      | ✅       | ✅        | ✅         |

    //--------------------------------------------------------------
    // Operators with LOWER precedence than ~ inside of a ~ sequence
    //--------------------------------------------------------------

    assert_eq!(
        parse_cst("x*a~f~c", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "x", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::CodeParser_TernaryTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:3-4)),
                        Token(token!(Tilde, "~", 1:4-5)),
                        Token(token!(Symbol, "f", 1:5-6)),
                        Token(token!(Tilde, "~", 1:6-7)),
                        Token(token!(Symbol, "c", 1:7-8)),
                    ]),
                })),
            ]),
        }))
    );

    // Explicit Times (*) operator, which has lower precedence than ~
    assert_eq!(
        parse_cst("a~x*f~b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "x", 1:3-4)),
                    ]),
                }),
                Token(token!(Star, "*", 1:4-5)),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:5-6)),
                        Token(token!(Tilde, "~", 1:6-7)),
                        Token(token!(Symbol, "b", 1:7-8)),
                    ]),
                }),
            ]),
        }))
    );

    // Implicit times
    assert_eq!(
        parse_cst("a~x f~b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "x", 1:3-4)),
                    ]),
                }),
                Token(token!(Whitespace, " ", 1:4-5)),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5)),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:5-6)),
                        Token(token!(Tilde, "~", 1:6-7)),
                        Token(token!(Symbol, "b", 1:7-8)),
                    ]),
                }),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~(x*f)~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Group(GroupNode(OperatorNode {
                    op: GroupOp::CodeParser_GroupParen,
                    children: NodeSeq(vec![
                        Token(token!(OpenParen, "(", 1:3-4)),
                        Infix(InfixNode(OperatorNode {
                            op: InfixOp::Times,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "x", 1:4-5)),
                                Token(token!(Star, "*", 1:5-6)),
                                Token(token!(Symbol, "f", 1:6-7)),
                            ]),
                        })),
                        Token(token!(CloseParen, ")", 1:7-8)),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:8-9)),
                Token(token!(Symbol, "b", 1:9-10)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a ~f x", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Whitespace, " ", 1:2-3)),
                        Token(token!(Tilde, "~", 1:3-4)),
                        Token(token!(Symbol, "f", 1:4-5)),
                    ]),
                }),
                Token(token!(Whitespace, " ", 1:5-6)),
                Token(token!(Fake_ImplicitTimes, "", 1:6-6)),
                Token(token!(Symbol, "x", 1:6-7)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f;;~b", &Default::default()).syntax,
        SyntaxError(SyntaxErrorNode {
            err: SyntaxErrorKind::ExpectedTilde,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::Span,
                    children: NodeSeq(vec![
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:1-2)),
                                Token(token!(Tilde, "~", 1:2-3)),
                                Token(token!(Symbol, "f", 1:3-4)),
                            ]),
                        }),
                        Token(token!(SemiSemi, ";;", 1:4-6)),
                        Token(token!(Fake_ImplicitAll, "", 1:6-6)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "b", 1:7-8)),
            ]),
        })
    );

    assert_eq!(
        parse_cst("a~f;;x~b", &Default::default()).syntax,
        Binary(BinaryNode(OperatorNode {
            op: BinaryOp::Span,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "f", 1:3-4)),
                    ]),
                }),
                Token(token!(SemiSemi, ";;", 1:4-6)),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "x", 1:6-7)),
                        Token(token!(Tilde, "~", 1:7-8)),
                        Token(token!(Symbol, "b", 1:8-9)),
                    ]),
                }),
            ])
        }))
    );

    // FIXME:
    // Per the current Kernel parser behavior, this should parse into:
    //     Times[Span[f, x], Span[1, All]][a, b]
    assert_eq!(
        parse_cst("a~f;;x;;~b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::Span,
                    children: NodeSeq(vec![
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:1-2)),
                                Token(token!(Tilde, "~", 1:2-3)),
                                Token(token!(Symbol, "f", 1:3-4)),
                            ]),
                        }),
                        Token(token!(SemiSemi, ";;", 1:4-6)),
                        Token(token!(Symbol, "x", 1:6-7)),
                    ])
                })),
                Token(token!(Fake_ImplicitTimes, "", 1:7-7),),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Binary(BinaryNode(OperatorNode {
                            op: BinaryOp::Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:7-7)),
                                Token(token!(SemiSemi, ";;", 1:7-9)),
                                Token(token!(Fake_ImplicitAll, "", 1:9-9)),
                            ])
                        })),
                        Token(token!(Tilde, "~", 1:9-10)),
                        Token(token!(Symbol, "b", 1:10-11)),
                    ]),
                }),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a ~f;~ b", &Default::default()).syntax,
        SyntaxError(SyntaxErrorNode {
            err: SyntaxErrorKind::ExpectedTilde,
            children: NodeSeq(vec![
                Infix(InfixNode(OperatorNode {
                    op: InfixOp::CompoundExpression,
                    children: NodeSeq(vec![
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:1-2)),
                                Token(token!(Whitespace, " ", 1:2-3)),
                                Token(token!(Tilde, "~", 1:3-4)),
                                Token(token!(Symbol, "f", 1:4-5)),
                            ]),
                        }),
                        Token(token!(Semi, ";", 1:5-6)),
                        Token(token!(Fake_ImplicitNull, "", 1:6-6)),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Whitespace, " ", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        })
    );

    assert_eq!(
        parse_cst("a ~f!~ b", &Default::default()).syntax,
        SyntaxError(SyntaxErrorNode {
            err: SyntaxErrorKind::ExpectedTilde,
            children: NodeSeq(vec![
                Postfix(PostfixNode(OperatorNode {
                    op: PostfixOp::Factorial,
                    children: NodeSeq(vec![
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:1-2)),
                                Token(token!(Whitespace, " ", 1:2-3)),
                                Token(token!(Tilde, "~", 1:3-4)),
                                Token(token!(Symbol, "f", 1:4-5)),
                            ]),
                        }),
                        Token(token!(Bang, "!", 1:5-6)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Whitespace, " ", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        })
    );

    assert_eq!(
        parse_cst("a~f+x&~b", &Default::default()).syntax,
        SyntaxError(SyntaxErrorNode {
            err: SyntaxErrorKind::ExpectedTilde,
            children: NodeSeq(vec![
                Postfix(PostfixNode(OperatorNode {
                    op: PostfixOp::Function,
                    children: NodeSeq(vec![
                        Infix(InfixNode(OperatorNode {
                            op: InfixOp::Plus,
                            children: NodeSeq(vec![
                                SyntaxError(SyntaxErrorNode {
                                    err: SyntaxErrorKind::ExpectedTilde,
                                    children: NodeSeq(vec![
                                        Token(token!(Symbol, "a", 1:1-2)),
                                        Token(token!(Tilde, "~", 1:2-3)),
                                        Token(token!(Symbol, "f", 1:3-4)),
                                    ]),
                                }),
                                Token(token!(Plus, "+", 1:4-5)),
                                Token(token!(Symbol, "x", 1:5-6)),
                            ]),
                        })),
                        Token(token!(Amp, "&", 1:6-7)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        })
    );


    assert_eq!(
        parse_cst("a~f+x!~b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Plus,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "f", 1:3-4)),
                    ]),
                }),
                Token(token!(Plus, "+", 1:4-5)),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Postfix(PostfixNode(OperatorNode {
                            op: PostfixOp::Factorial,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "x", 1:5-6)),
                                Token(token!(Bang, "!", 1:6-7)),
                            ]),
                        })),
                        Token(token!(Tilde, "~", 1:7-8)),
                        Token(token!(Symbol, "b", 1:8-9)),
                    ]),
                },),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f+x!~b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Plus,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "f", 1:3-4)),
                    ]),
                },),
                Token(token!(Plus, "+", 1:4-5),),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Postfix(PostfixNode(OperatorNode {
                            op: PostfixOp::Factorial,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "x", 1:5-6)),
                                Token(token!(Bang, "!", 1:6-7)),
                            ])
                        })),
                        Token(token!(Tilde, "~", 1:7-8)),
                        Token(token!(Symbol, "b", 1:8-9)),
                    ]),
                }),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f?x!~b", &Default::default()).syntax,
        SyntaxError(SyntaxErrorNode {
            err: SyntaxErrorKind::ExpectedTilde,
            children: NodeSeq(vec![
                Postfix(PostfixNode(OperatorNode {
                    op: PostfixOp::Factorial,
                    children: NodeSeq(vec![
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:1-2)),
                                Token(token!(Tilde, "~", 1:2-3)),
                                Binary(BinaryNode(OperatorNode {
                                    op: BinaryOp::PatternTest,
                                    children: NodeSeq(vec![
                                        Token(token!(Symbol, "f", 1:3-4)),
                                        Token(token!(Question, "?", 1:4-5)),
                                        Token(token!(Symbol, "x", 1:5-6)),
                                    ])
                                })),
                            ]),
                        }),
                        Token(token!(Bang, "!", 1:6-7)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        })
    );

    assert_eq!(
        parse_cst("a ~f,~ b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::CodeParser_Comma,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Whitespace, " ", 1:2-3)),
                        Token(token!(Tilde, "~", 1:3-4)),
                        Token(token!(Symbol, "f", 1:4-5)),
                    ]),
                }),
                Token(token!(Comma, ",", 1:5-6)),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Error_ExpectedOperand, "", 1:6-6)),
                        Token(token!(Tilde, "~", 1:6-7)),
                        Token(token!(Whitespace, " ", 1:7-8)),
                        Token(token!(Symbol, "b", 1:8-9)),
                    ]),
                }),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f,x~b", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::CodeParser_Comma,
            children: NodeSeq(vec![
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Tilde, "~", 1:2-3)),
                        Token(token!(Symbol, "f", 1:3-4)),
                    ]),
                }),
                Token(token!(Comma, ",", 1:4-5)),
                SyntaxError(SyntaxErrorNode {
                    err: SyntaxErrorKind::ExpectedTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "x", 1:5-6)),
                        Token(token!(Tilde, "~", 1:6-7)),
                        Token(token!(Symbol, "b", 1:7-8)),
                    ]),
                }),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("{a ~f,~ b}", &Default::default()).syntax,
        Group(GroupNode(OperatorNode {
            op: GroupOp::List,
            children: NodeSeq(vec![
                Token(token!(OpenCurly, "{", 1:1-2)),
                Infix(InfixNode(OperatorNode {
                    op: InfixOp::CodeParser_Comma,
                    children: NodeSeq(vec![
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "a", 1:2-3)),
                                Token(token!(Whitespace, " ", 1:3-4)),
                                Token(token!(Tilde, "~", 1:4-5)),
                                Token(token!(Symbol, "f", 1:5-6)),
                            ]),
                        }),
                        Token(token!(Comma, ",", 1:6-7)),
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Error_ExpectedOperand, "", 1:7-7)),
                                Token(token!(Tilde, "~", 1:7-8)),
                                Token(token!(Whitespace, " ", 1:8-9)),
                                Token(token!(Symbol, "b", 1:9-10)),
                            ]),
                        }),
                    ]),
                })),
                Token(token!(CloseCurly, "}", 1:10-11)),
            ]),
        }))
    );

    //---------------------------------------------------------------
    // Operators with HIGHER precedence than ~ inside of a ~ sequence
    //---------------------------------------------------------------

    assert_eq!(
        parse_cst("a~f@x~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Token(token!(At, "@", 1:4-5)),
                        Token(token!(Symbol, "x", 1:5-6)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "b", 1:7-8)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f/*g~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Infix(InfixNode(OperatorNode {
                    op: InfixOp::RightComposition,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Token(token!(SlashStar, "/*", 1:4-6)),
                        Token(token!(Symbol, "g", 1:6-7)),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f@*g~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Infix(InfixNode(OperatorNode {
                    op: InfixOp::Composition,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Token(token!(AtStar, "@*", 1:4-6)),
                        Token(token!(Symbol, "g", 1:6-7)),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~++f~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Prefix(PrefixNode(OperatorNode {
                    op: PrefixOp::PreIncrement,
                    children: NodeSeq(vec![
                        Token(token!(PlusPlus, "++", 1:3-5)),
                        Token(token!(Symbol, "f", 1:5-6)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "b", 1:7-8)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f--~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Postfix(PostfixNode(OperatorNode {
                    op: PostfixOp::Decrement,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Token(token!(MinusMinus, "--", 1:4-6)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "b", 1:7-8)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f[x]~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Call(CallNode {
                    head: CallHead::Concrete(NodeSeq(vec![Token(
                        token!(Symbol, "f", 1:3-4),
                    ),])),
                    body: CallBody::Group(GroupNode(OperatorNode {
                        op: CallOp::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:4-5)),
                            Token(token!(Symbol, "x", 1:5-6)),
                            Token(token!(CloseSquare, "]", 1:6-7)),
                        ]),
                    })),
                }),
                Token(token!(Tilde, "~", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f?h~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::PatternTest,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Token(token!(Question, "?", 1:4-5)),
                        Token(token!(Symbol, "h", 1:5-6)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "b", 1:7-8)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~<<g@x~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Prefix(PrefixNode(OperatorNode {
                            op: PrefixOp::Get,
                            children: NodeSeq(vec![
                                Token(token!(LessLess, "<<", 1:3-5)),
                                Token(token!(String, "g", 1:5-6)),
                            ])
                        })),
                        Token(token!(At, "@", 1:6-7)),
                        Token(token!(Symbol, "x", 1:7-8)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:8-9)),
                Token(token!(Symbol, "b", 1:9-10)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~<<g @x~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Prefix(PrefixNode(OperatorNode {
                            op: PrefixOp::Get,
                            children: NodeSeq(vec![
                                Token(token!(LessLess, "<<", 1:3-5)),
                                Token(token!(String, "g", 1:5-6)),
                            ])
                        })),
                        Token(token!(Whitespace, " ", 1:6-7)),
                        Token(token!(At, "@", 1:7-8)),
                        Token(token!(Symbol, "x", 1:8-9)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:9-10)),
                Token(token!(Symbol, "b", 1:10-11)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f::x~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Infix(InfixNode(OperatorNode {
                    op: InfixOp::MessageName,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Token(token!(ColonColon, "::", 1:4-6)),
                        Token(token!(String, "x", 1:6-7)),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:7-8)),
                Token(token!(Symbol, "b", 1:8-9)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a~f_x~b", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Tilde, "~", 1:2-3)),
                Compound(CompoundNode(OperatorNode {
                    op: CompoundOp::CodeParser_PatternBlank,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "f", 1:3-4)),
                        Compound(CompoundNode(OperatorNode {
                            op: CompoundOp::Blank,
                            children: NodeSeq(vec![
                                Token(token!(Under, "_", 1:4-5)),
                                Token(token!(Symbol, "x", 1:5-6)),
                            ]),
                        })),
                    ]),
                })),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "b", 1:7-8)),
            ]),
        }))
    );

    //----------------------------------
    // Operators in tilde first operand
    //----------------------------------

    assert_eq!(
        parse_cst("a;;~b~c", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(SemiSemi, ";;", 1:2-4)),
                        Token(token!(Fake_ImplicitAll, "", 1:4-4)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:4-5)),
                Token(token!(Symbol, "b", 1:5-6)),
                Token(token!(Tilde, "~", 1:6-7)),
                Token(token!(Symbol, "c", 1:7-8)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a;;b~c~d", &Default::default()).syntax,
        Binary(BinaryNode(OperatorNode {
            op: BinaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(SemiSemi, ";;", 1:2-4)),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::CodeParser_TernaryTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "b", 1:4-5)),
                        Token(token!(Tilde, "~", 1:5-6)),
                        Token(token!(Symbol, "c", 1:6-7)),
                        Token(token!(Tilde, "~", 1:7-8)),
                        Token(token!(Symbol, "d", 1:8-9)),
                    ]),
                })),
            ])
        }))
    );

    // FIXME:
    // CodeParser currently parses this incorrectly. The correct parse,
    // theoretically and to match current Kernel behavior, is:
    //     d[Times[Span[a, b], Span[1, All]], e]
    // However, the expr represented by the Cst parsed by CodeParser below is:
    //     Times[Span[a, b], d[Span[1, All], e]]
    assert_eq!(
        parse_cst("a;;b;;~d~e", &Default::default()).syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOp::Times,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOp::Span,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(SemiSemi, ";;", 1:2-4)),
                        Token(token!(Symbol, "b", 1:4-5)),
                    ])
                })),
                Token(token!(Fake_ImplicitTimes, "", 1:5-5)),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::CodeParser_TernaryTilde,
                    children: NodeSeq(vec![
                        Binary(BinaryNode(OperatorNode {
                            op: BinaryOp::Span,
                            children: NodeSeq(vec![
                                Token(token!(Fake_ImplicitOne, "", 1:5-5)),
                                Token(token!(SemiSemi, ";;", 1:5-7)),
                                Token(token!(Fake_ImplicitAll, "", 1:7-7)),
                            ])
                        })),
                        Token(token!(Tilde, "~", 1:7-8)),
                        Token(token!(Symbol, "d", 1:8-9)),
                        Token(token!(Tilde, "~", 1:9-10)),
                        Token(token!(Symbol, "e", 1:10-11)),
                    ]),
                })),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a;;b;;c~d~e", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::Span,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(SemiSemi, ";;", 1:2-4)),
                Token(token!(Symbol, "b", 1:4-5)),
                Token(token!(SemiSemi, ";;", 1:5-7)),
                Ternary(TernaryNode(OperatorNode {
                    op: TernaryOp::CodeParser_TernaryTilde,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "c", 1:7-8)),
                        Token(token!(Tilde, "~", 1:8-9)),
                        Token(token!(Symbol, "d", 1:9-10)),
                        Token(token!(Tilde, "~", 1:10-11)),
                        Token(token!(Symbol, "e", 1:11-12)),
                    ]),
                })),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a!~b~c", &Default::default()).syntax,
        Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::CodeParser_TernaryTilde,
            children: NodeSeq(vec![
                Postfix(PostfixNode(OperatorNode {
                    op: PostfixOp::Factorial,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Bang, "!", 1:2-3)),
                    ])
                })),
                Token(token!(Tilde, "~", 1:3-4)),
                Token(token!(Symbol, "b", 1:4-5)),
                Token(token!(Tilde, "~", 1:5-6)),
                Token(token!(Symbol, "c", 1:6-7)),
            ]),
        }))
    );
}

#[test]
fn test_regressions() {
    assert_eq!(
        parse_cst("a=.", &Default::default()).syntax,
        Binary(BinaryNode(OperatorNode {
            op: BinaryOp::Unset,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Equal, "=", 1:2-3)),
                Token(token!(Dot, ".", 1:3-4)),
            ])
        }))
    );

    assert_eq!(
        parse_cst("{ ( a }", &Default::default()).syntax,
        Group(GroupNode(OperatorNode {
            op: GroupOp::List,
            children: NodeSeq(vec![
                Token(token!(OpenCurly, "{", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                GroupMissingCloser(GroupMissingCloserNode(OperatorNode {
                    op: GroupOp::CodeParser_GroupParen,
                    children: NodeSeq(vec![
                        Token(token!(OpenParen, "(", 1:3-4)),
                        Token(token!(Whitespace, " ", 1:4-5)),
                        Token(token!(Symbol, "a", 1:5-6)),
                    ]),
                })),
                Token(token!(Whitespace, " ", 1:6-7)),
                Token(token!(CloseCurly, "}", 1:7-8)),
            ]),
        }))
    );

    assert_eq!(
        parse_cst("a>>b\\1c", &Default::default()).syntax,
        Binary(BinaryNode(OperatorNode {
            op: BinaryOp::Put,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(GreaterGreater, ">>", 1:2-4)),
                Token(token!(String, "b\\1c", 1:4-8)),
            ])
        }))
    );

    assert_eq!(
        parse_cst("f[a : b]", &Default::default()).syntax,
        Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(Symbol, "f", 1:1-2)
            )])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOp::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    Token(token!(OpenSquare, "[", 1:2-3)),
                    Binary(BinaryNode(OperatorNode {
                        op: BinaryOp::Pattern,
                        children: NodeSeq(vec![
                            Token(token!(Symbol, "a", 1:3-4)),
                            Token(token!(Whitespace, " ", 1:4-5)),
                            Token(token!(Colon, ":", 1:5-6)),
                            Token(token!(Whitespace, " ", 1:6-7)),
                            Token(token!(Symbol, "b", 1:7-8)),
                        ]),
                    })),
                    Token(token!(CloseSquare, "]", 1:8-9),),
                ]),
            })),
        })
    );
}
