use crate::{
    abstract_::{abstract_cst, aggregate_cst},
    ast::Ast,
    cst::{
        BinaryNode, BinaryOperator,
        Cst::{self, Token},
        InfixNode, InfixOperator, OperatorNode, PrefixNode, PrefixOperator,
    },
    macros::{leaf, src, token},
    parse_cst, NodeSeq, QuirkSettings,
};

use pretty_assertions::assert_eq;

#[test]
fn test_abstract_flatten_times_quirk() {
    let cst = parse_cst(
        r#"-a * b / c d \[InvisibleTimes] e \[Times] f"#,
        &Default::default(),
    );

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:1-2)),
                        Token(token!(Symbol, "a", 1:2-3)),
                    ]),
                    src: src!(1:1-3).into(),
                })),
                Token(token!(Whitespace, " ", 1:3-4)),
                Token(token!(Star, "*", 1:4-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::Divide,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "b", 1:6-7)),
                        Token(token!(Whitespace, " ", 1:7-8)),
                        Token(token!(Slash, "/", 1:8-9)),
                        Token(token!(Whitespace, " ", 1:9-10)),
                        Token(token!(Symbol, "c", 1:10-11)),
                    ]),
                    src: src!(1:6-11).into(),
                })),
                Token(token!(Whitespace, " ", 1:11-12)),
                Token(token!(Fake_ImplicitTimes, "", 1:12-12)),
                Token(token!(Symbol, "d", 1:12-13)),
                Token(token!(Whitespace, " ", 1:13-14)),
                Token(
                    token!(LongName_InvisibleTimes, "\\[InvisibleTimes]", 1:14-31)
                ),
                Token(token!(Whitespace, " ", 1:31-32)),
                Token(token!(Symbol, "e", 1:32-33)),
                Token(token!(Whitespace, " ", 1:33-34)),
                Token(token!(LongName_Times, "\\[Times]", 1:34-42)),
                Token(token!(Whitespace, " ", 1:42-43)),
                Token(token!(Symbol, "f", 1:43-44)),
            ]),
            src: src!(1:1-44).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    assert_eq!(
        abstract_cst(agg.clone(), Default::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        leaf!(Symbol, "a", 1:2-3),
                    ],
                    data: src!(1:1-3).into(),
                },
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Symbol, "b", 1:6-7),
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Power", <||>)),
                            args: vec![
                                leaf!(Symbol, "c", 1:10-11),
                                leaf!(Integer, "-1", <||>),
                            ],
                            data: src!(1:6-11).into(),
                        },
                    ],
                    data: src!(1:6-11).into(),
                },
                leaf!(Symbol, "d", 1:12-13),
                leaf!(Symbol, "e", 1:32-33),
                leaf!(Symbol, "f", 1:43-44),
            ],
            data: src!(1:1-44).into()
        }
    );

    //
    // Test the same input, but this time with the 'flatten Times' parsing
    // quirk enabled.
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Integer, "-1", <||>),
                leaf!(Symbol, "a", 1:2-3),
                leaf!(Symbol, "b", 1:6-7),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Power", <||>)),
                    args: vec![
                        leaf!(Symbol, "c", 1:10-11),
                        leaf!(Integer, "-1", <||>),
                    ],
                    // TODO(polish): Any better source span than this pretty
                    //               broad one?
                    data: src!(1:1-44).into(),
                },
                leaf!(Symbol, "d", 1:12-13),
                leaf!(Symbol, "e", 1:32-33),
                leaf!(Symbol, "f", 1:43-44),
            ],
            data: src!(1:1-44).into()
        }
    )
}

#[test]
fn test_abstract_infix_binary_at_quirk() {
    //==================================
    // With StringJoin
    //==================================

    let cst = parse_cst("a<>StringJoin@b", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::StringJoin,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(LessGreater, "<>", 1:2-4),),
                Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "StringJoin", 1:4-14),),
                        Token(token!(At, "@", 1:14-15),),
                        Token(token!(Symbol, "b", 1:15-16),),
                    ]),
                    src: src!(1:4-16).into(),
                })),
            ]),
            src: src!(1:1-16).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // Test abstract with the 'infix_binary_at' quirk ENABLED (the default)
    //

    #[rustfmt::skip]
    assert_eq!(
        abstract_cst(agg.clone(), Default::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "StringJoin", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Symbol, "b", 1:15-16),
            ],
            data: src!(1:1-16).into(),
        }
    );

    //
    // Test the same input, but now with 'infix_binary_at' quirk DISABLED.
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().infix_binary_at(false)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "StringJoin", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "StringJoin", 1:4-14)),
                    args: vec![leaf!(Symbol, "b", 1:15-16),],
                    data: src!(1:4-16).into(),
                },
            ],
            data: src!(1:1-16).into(),
        }
    );

    //==================================
    // With Plus
    //==================================

    let cst = parse_cst(r#"a + Plus @ b"#, &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Token(token!(Plus, "+", 1:3-4),),
                Token(token!(Whitespace, " ", 1:4-5),),
                Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "Plus", 1:5-9),),
                        Token(token!(Whitespace, " ", 1:9-10),),
                        Token(token!(At, "@", 1:10-11),),
                        Token(token!(Whitespace, " ", 1:11-12),),
                        Token(token!(Symbol, "b", 1:12-13),),
                    ]),
                    src: src!(1:5-13).into(),
                })),
            ]),
            src: src!(1:1-13).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().infix_binary_at(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Plus", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Symbol, "b", 1:12-13),
            ],
            data: src!(1:1-13).into(),
        }
    );

    //
    // Test the same input, but now with 'infix_binary_at' quirk DISABLED.
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().infix_binary_at(false)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Plus", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Plus", 1:5-9)),
                    args: vec![leaf!(Symbol, "b", 1:12-13)],
                    data: src!(1:5-13).into(),
                },
            ],
            data: src!(1:1-13).into(),
        }
    );

    //==================================
    // With SameQ
    //==================================

    //
    // NOTE: Unlike the previous cases covering <> and +, === DOES NOT
    //       support this infix binary at quirky parsing behavior. That
    //       may or may not be a bug, but it is the behavior of the
    //       Kernel parser as of 13.3.
    //

    let cst = parse_cst("a === SameQ @ b", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::SameQ,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Token(token!(EqualEqualEqual, "===", 1:3-6),),
                Token(token!(Whitespace, " ", 1:6-7),),
                Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "SameQ", 1:7-12),),
                        Token(token!(Whitespace, " ", 1:12-13),),
                        Token(token!(At, "@", 1:13-14),),
                        Token(token!(Whitespace, " ", 1:14-15),),
                        Token(token!(Symbol, "b", 1:15-16),),
                    ]),
                    src: src!(1:7-16).into(),
                })),
            ]),
            src: src!(1:1-16).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    // Test that even with `infix_binary_at` quirk enabled, this DOES NOT
    // parse as a flag `SameQ[a, b]` expression.
    #[rustfmt::skip]
    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().infix_binary_at(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "SameQ", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "SameQ", 1:7-12)),
                    args: vec![
                        leaf!(Symbol, "b", 1:15-16)
                    ],
                    data: src!(1:7-16).into()
                }
            ],
            data: src!(1:1-16).into(),
        }
    );
}
