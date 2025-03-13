use crate::{
    abstract_::{abstract_cst, aggregate_cst},
    ast::Ast,
    cst::{
        BinaryNode, BinaryOperator,
        Cst::{self, Token},
        GroupNode, GroupOperator, InfixNode, InfixOperator, OperatorNode,
        PrefixNode, PrefixOperator,
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
    );

    //==================================

    let cst = parse_cst("- - a", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Minus,
            children: NodeSeq(vec![
                Token(token!(Minus, "-", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:3-4)),
                        Token(token!(Whitespace, " ", 1:4-5)),
                        Token(token!(Symbol, "a", 1:5-6)),
                    ]),
                    src: src!(1:3-6).into(),
                })),
            ]),
            src: src!(1:1-6).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    assert_eq!(
        abstract_cst(agg.clone(), Default::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Integer, "-1", <||>),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        leaf!(Symbol, "a", 1:5-6),
                    ],
                    data: src!(1:3-6).into(),
                },
            ],
            data: src!(1:1-6).into()
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
                leaf!(Integer, "-1", <||>),
                leaf!(Symbol, "a", 1:5-6),
            ],
            data: src!(1:1-6).into()
        }
    );

    //==================================
    // Test basic flatten times through Binary Divide numerator (TID:231010/4)
    //==================================

    let cst = parse_cst("-a/b", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Divide,
            children: NodeSeq(vec![
                Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:1-2)),
                        Token(token!(Symbol, "a", 1:2-3))
                    ]),
                    src: src!(1:1-3).into()
                })),
                Token(token!(Slash, "/", 1:3-4)),
                Token(token!(Symbol, "b", 1:4-5)),
            ]),
            src: src!(1:1-5).into()
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // flatten_times = true
    //

    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Integer, "-1", <||>),
                leaf!(Symbol, "a", 1:2-3),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Power", <||>)),
                    args: vec![
                        leaf!(Symbol, "b", 1:4-5),
                        leaf!(Integer, "-1", <||>),
                    ],
                    data: src!(1:1-5).into(),
                },
            ],
            data: src!(1:1-5).into()
        }
    );

    //
    // flatten_times = false
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(false)),
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
                    head: Box::new(leaf!(Symbol, "Power", <||>)),
                    args: vec![
                        leaf!(Symbol, "b", 1:4-5),
                        leaf!(Integer, "-1", <||>),
                    ],
                    data: src!(1:1-5).into(),
                },
            ],
            data: src!(1:1-5).into()
        }
    );

    //==================================
    // Test nested flatten times through Binary Divide numerator (TID:231010/1)
    //==================================

    // The key point and what is uncommon about this case is that we're
    // constructing a Divide[..] whose numerator is a Times[..] expression
    // with higher precedence than `*`, so it is a separate Times from
    // Times[a, ..].
    // TODO: Are the parens really necessary for what is being tested here?
    let cst = parse_cst("a*-(b)/c", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::Divide,
                    children: NodeSeq(vec![
                        Cst::Prefix(PrefixNode(OperatorNode {
                            op: PrefixOperator::Minus,
                            children: NodeSeq(vec![
                                Token(token!(Minus, "-", 1:3-4)),
                                Cst::Group(GroupNode(OperatorNode {
                                    op: GroupOperator::CodeParser_GroupParen,
                                    children: NodeSeq(vec![
                                        Token(token!(OpenParen, "(", 1:4-5)),
                                        Token(token!(Symbol, "b", 1:5-6)),
                                        Token(token!(CloseParen, ")", 1:6-7)),
                                    ]),
                                    src: src!(1:4-7).into(),
                                }))
                            ]),
                            src: src!(1:3-7).into(),
                        })),
                        Token(token!(Slash, "/", 1:7-8)),
                        Token(token!(Symbol, "c", 1:8-9)),
                    ]),
                    src: src!(1:3-9).into(),
                })),
            ]),
            src: src!(1:1-9).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // flatten_times = true
    //

    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Integer, "-1", <||>),
                leaf!(Symbol, "b", 1:5-6),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Power", <||>)),
                    args: vec![
                        leaf!(Symbol, "c", 1:8-9),
                        leaf!(Integer, "-1", <||>),
                    ],
                    data: src!(1:1-9).into(),
                },
            ],
            data: src!(1:1-9).into()
        }
    );

    //
    // flatten_times = false
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(false)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Times", <||>)),
                            args: vec![
                                leaf!(Integer, "-1", <||>),
                                leaf!(Symbol, "b", 1:5-6),
                            ],
                            data: src!(1:3-7).into(),
                        },
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Power", <||>)),
                            args: vec![
                                leaf!(Symbol, "c", 1:8-9),
                                leaf!(Integer, "-1", <||>),
                            ],
                            data: src!(1:3-9).into(),
                        },
                    ],
                    data: src!(1:3-9).into(),
                },
            ],
            data: src!(1:1-9).into()
        }
    );

    //=======================================================
    // Test nested flatten times through Prefix Minus operand
    //=======================================================

    //
    // These two variants test that the flatten_times quirks
    // DOES NOT flatten through explicit parenthesis groups (...).
    //

    //----------------------------------------------
    // Variant A: Nested prefix minus "-(-b)"
    //----------------------------------------------

    let cst = parse_cst("a*-(-b)", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:3-4)),
                        Cst::Group(GroupNode(OperatorNode {
                            op: GroupOperator::CodeParser_GroupParen,
                            children: NodeSeq(vec![
                                Token(token!(OpenParen, "(", 1:4-5)),
                                Cst::Prefix(PrefixNode(OperatorNode {
                                    op: PrefixOperator::Minus,
                                    children: NodeSeq(vec![
                                        Token(token!(Minus, "-", 1:5-6)),
                                        Token(token!(Symbol, "b", 1:6-7)),
                                    ]),
                                    src: src!(1:5-7).into(),
                                })),
                                Token(token!(CloseParen, ")", 1:7-8)),
                            ]),
                            src: src!(1:4-8).into(),
                        }))
                    ]),
                    src: src!(1:3-8).into(),
                })),
            ]),
            src: src!(1:1-8).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // flatten_times = true
    //

    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Integer, "-1", <||>),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        leaf!(Symbol, "b", 1:6-7),
                    ],
                    data: src!(1:5-7).into(),
                },
            ],
            data: src!(1:1-8).into()
        }
    );

    //
    // flatten_times = false
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(false)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Times", <||>)),
                            args: vec![
                                leaf!(Integer, "-1", <||>),
                                leaf!(Symbol, "b", 1:6-7),
                            ],
                            data: src!(1:5-7).into(),
                        }
                    ],
                    data: src!(1:3-8).into(),
                },
            ],
            data: src!(1:1-8).into()
        }
    );

    //------------------------------------------------------------------
    // Variant B: Prefix minus before normal paren Times -(b*c)
    //------------------------------------------------------------------

    let cst = parse_cst("a*-(b*c)", &Default::default());

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:3-4)),
                        Cst::Group(GroupNode(OperatorNode {
                            op: GroupOperator::CodeParser_GroupParen,
                            children: NodeSeq(vec![
                                Token(token!(OpenParen, "(", 1:4-5)),
                                Cst::Infix(InfixNode(OperatorNode {
                                    op: InfixOperator::Times,
                                    children: NodeSeq(vec![
                                        Token(token!(Symbol, "b", 1:5-6)),
                                        Token(token!(Star, "*", 1:6-7)),
                                        Token(token!(Symbol, "c", 1:7-8)),
                                    ]),
                                    src: src!(1:5-8).into(),
                                })),
                                Token(token!(CloseParen, ")", 1:8-9)),
                            ]),
                            src: src!(1:4-9).into(),
                        }))
                    ]),
                    src: src!(1:3-9).into(),
                })),
            ]),
            src: src!(1:1-9).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // flatten_times = true    --    Times[a, -1, Times[b, c]]
    //

    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Integer, "-1", <||>),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Symbol, "b", 1:5-6),
                        leaf!(Symbol, "c", 1:7-8),
                    ],
                    data: src!(1:5-8).into(),
                },
            ],
            data: src!(1:1-9).into()
        }
    );

    //
    // flatten_times = false    --    Times[a, Times[-1, Times[b, c]]]
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(false)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Times", <||>)),
                            args: vec![
                                leaf!(Symbol, "b", 1:5-6),
                                leaf!(Symbol, "c", 1:7-8),
                            ],
                            data: src!(1:5-8).into(),
                        }
                    ],
                    data: src!(1:3-9).into(),
                },
            ],
            data: src!(1:1-9).into()
        }
    );
}

#[test]
fn test_abstract_infix_binary_at_quirk() {
    //==================================
    // With StringJoin
    //==================================

    // TID:231010/2
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
    // With SameQ (TID:231010/3)
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

#[test]
fn test_abstract_flatten_times_combined_with_infix_binary_at_quirk() {
    let cst = parse_cst("a * Times @ -b", &Default::default());

    //
    // Four different ways to abstract this:
    //
    //            | infix_binary_at | flatten_times | Unevaluated full form
    // -----------|-----------------|---------------|----------------------
    // Variant A: | true            | true          | Times[a, Times[-1, b]]
    // Variant B: | true            | false         | Times[a, Times[-1, b]]
    // Variant C: | false           | true          | Times[a, Times[Times[-1, b]]]
    // Variant D: | false           | false         | Times[a, Times[Times[-1, b]]]
    //
    // TODO: Is there a short input string that makes the full form of all
    //       four variants different?

    let [cst]: &[_; 1] = cst.nodes().try_into().unwrap();

    assert_eq!(
        *cst,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(Star, "*", 1:3-4)),
                Token(token!(Whitespace, " ", 1:4-5)),
                Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "Times", 1:5-10)),
                        Token(token!(Whitespace, " ", 1:10-11)),
                        Token(token!(At, "@", 1:11-12)),
                        Token(token!(Whitespace, " ", 1:12-13)),
                        Cst::Prefix(PrefixNode(OperatorNode {
                            op: PrefixOperator::Minus,
                            children: NodeSeq(vec![
                                Token(token!(Minus, "-", 1:13-14),),
                                Token(token!(Symbol, "b", 1:14-15),),
                            ]),
                            src: src!(1:13-15).into(),
                        })),
                    ]),
                    src: src!(1:5-15).into(),
                })),
            ]),
            src: src!(1:1-15).into(),
        }))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // Variant A: true, true
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default()
                .infix_binary_at(true)
                .flatten_times(true)
        ),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        leaf!(Symbol, "b", 1:14-15),
                    ],
                    data: src!(1:13-15).into(),
                }
            ],
            data: src!(1:1-15).into(),
        }
    );

    //
    // Variant B: true, false
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default()
                .infix_binary_at(true)
                .flatten_times(false)
        ),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        leaf!(Symbol, "b", 1:14-15),
                    ],
                    data: src!(1:13-15).into(),
                }
            ],
            data: src!(1:1-15).into(),
        }
    );

    //
    // Variant C: false, true
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default()
                .infix_binary_at(false)
                .flatten_times(true)
        ),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", 1:5-10)),
                    args: vec![Ast::Call {
                        head: Box::new(leaf!(Symbol, "Times", <||>)),
                        args: vec![
                            leaf!(Integer, "-1", <||>),
                            leaf!(Symbol, "b", 1:14-15),
                        ],
                        data: src!(1:13-15).into(),
                    }],
                    data: src!(1:5-15).into(),
                }
            ],
            data: src!(1:1-15).into(),
        }
    );

    //
    // Variant D: false, false
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default()
                .infix_binary_at(false)
                .flatten_times(false)
        ),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", 1:5-10)),
                    args: vec![Ast::Call {
                        head: Box::new(leaf!(Symbol, "Times", <||>)),
                        args: vec![
                            leaf!(Integer, "-1", <||>),
                            leaf!(Symbol, "b", 1:14-15),
                        ],
                        data: src!(1:13-15).into(),
                    }],
                    data: src!(1:5-15).into(),
                }
            ],
            data: src!(1:1-15).into(),
        }
    );
}
