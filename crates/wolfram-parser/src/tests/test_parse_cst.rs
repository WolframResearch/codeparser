use crate::{
    abstract_cst::{abstract_cst, aggregate_cst},
    ast::Ast,
    cst::{
        BinaryNode, CallBody, CallHead, CallNode,
        Cst::{Binary, Call, Group, Prefix, PrefixBinary, Ternary, Token},
        GroupNode, OperatorNode, PrefixBinaryNode, PrefixNode, TernaryNode,
        TriviaSeq,
    },
    macros::{leaf, src, token},
    parse::operators::{
        BinaryOperator as BinaryOp, CallOperator as CallOp,
        GroupOperator as GroupOp, PrefixBinaryOperator, PrefixOperator,
        TernaryOperator as TernaryOp,
    },
    parse_cst,
    tests::assert_src,
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
