use crate::{
    abstract_cst::{abstract_cst, aggregate_cst},
    ast::{Ast, AstMetadata},
    cst::{
        BinaryNode, CallBody, CallHead, CallNode, CompoundNode,
        Cst::{self, Call, Compound, Group, Infix, Token},
        GroupNode, InfixNode, OperatorNode, PrefixNode,
    },
    issue::{Issue, IssueTag, Severity},
    macros::{leaf, src, token},
    parse::operators::{
        BinaryOperator, CallOperator, CompoundOperator, GroupOperator,
        InfixOperator, PrefixOperator,
    },
    parse_cst, symbols as st,
    tests::assert_src,
    NodeSeq, QuirkSettings,
};

use pretty_assertions::assert_eq;


#[test]
fn test_negate_infix_times() {
    //
    // TID:231012/1
    //

    let cst = parse_cst("a-b*c", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-6 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Minus, "-", 1:2-3),),
                assert_src!(1:3-6 => Cst::Infix(InfixNode(OperatorNode {
                    op: InfixOperator::Times,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "b", 1:3-4),),
                        Token(token!(Star, "*", 1:4-5),),
                        Token(token!(Symbol, "c", 1:5-6),),
                    ],),
                }))),
            ]),
        })))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    let common_expected_ast = Ast::Call {
        head: Box::new(leaf!(Symbol, "Plus", <||>)),
        args: vec![
            leaf!(Symbol, "a", 1:1-2),
            Ast::Call {
                head: Box::new(leaf!(Symbol, "Times", <||>)),
                args: vec![
                    leaf!(Integer, "-1", <||>),
                    leaf!(Symbol, "b", 1:3-4),
                    leaf!(Symbol, "c", 1:5-6),
                ],
                data: src!(1:2-6).into(),
            },
        ],
        data: src!(1:1-6).into(),
    };

    //
    // flatten_times = false
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default().flatten_times(false)
        ),
        common_expected_ast
    );

    //
    // flatten_times = true
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(true)),
        common_expected_ast
    );

    //==================================
    // Flatten times after negate TID:231012/2
    //==================================

    let cst = parse_cst("a-b/c", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-6 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Minus, "-", 1:2-3),),
                assert_src!(1:3-6 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::Divide,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "b", 1:3-4)),
                        Token(token!(Slash, "/", 1:4-5)),
                        Token(token!(Symbol, "c", 1:5-6)),
                    ]),
                })))
            ]),
        })))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // flatten_times = false
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default().flatten_times(false)
        ),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Plus", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Times", <||>)),
                            args: vec![
                                leaf!(Symbol, "b", 1:3-4),
                                Ast::Call {
                                    head: Box::new(
                                        leaf!(Symbol, "Power", <||>)
                                    ),
                                    args: vec![
                                        leaf!(Symbol, "c", 1:5-6),
                                        leaf!(Integer, "-1", <||>),
                                    ],
                                    data: src!(1:3-6).into(),
                                },
                            ],
                            data: src!(1:3-6).into(),
                        },
                    ],
                    data: src!(1:2-6).into(),
                },
            ],
            data: src!(1:1-6).into(),
        }
    );

    //
    // flatten_times = true
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Plus", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        leaf!(Symbol, "b", 1:3-4),
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Power", <||>)),
                            args: vec![
                                leaf!(Symbol, "c", 1:5-6),
                                leaf!(Integer, "-1", <||>),
                            ],
                            data: src!(1:2-6).into(),
                        },
                    ],
                    data: src!(1:2-6).into(),
                },
            ],
            data: src!(1:1-6).into(),
        }
    );
}

//==========================================================
// Quirks
//==========================================================

#[test]
fn test_abstract_flatten_times_quirk() {
    let cst = parse_cst(
        r#"-a * b / c d \[InvisibleTimes] e \[Times] f"#,
        &Default::default(),
    )
    .syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-44 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Cst::Prefix(assert_src!(1:1-3 => PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:1-2)),
                        Token(token!(Symbol, "a", 1:2-3)),
                    ]),
                }))),
                Token(token!(Whitespace, " ", 1:3-4)),
                Token(token!(Star, "*", 1:4-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                assert_src!(1:6-11 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::Divide,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "b", 1:6-7)),
                        Token(token!(Whitespace, " ", 1:7-8)),
                        Token(token!(Slash, "/", 1:8-9)),
                        Token(token!(Whitespace, " ", 1:9-10)),
                        Token(token!(Symbol, "c", 1:10-11)),
                    ]),
                }))),
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
        })))
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
    // Nested prefix minus *without* enclosing Times (TID:231012/3)
    //==================================

    let cst = parse_cst("- - a", &Default::default()).syntax;

    assert_eq!(
        cst,
        Cst::Prefix(assert_src!(1:1-6 => PrefixNode(OperatorNode {
            op: PrefixOperator::Minus,
            children: NodeSeq(vec![
                Token(token!(Minus, "-", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Cst::Prefix(assert_src!(1:3-6 => PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:3-4)),
                        Token(token!(Whitespace, " ", 1:4-5)),
                        Token(token!(Symbol, "a", 1:5-6)),
                    ]),
                }))),
            ]),
        })))
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
    // Nested prefix minus *with* enclosing Times (TID:231012/4)
    //==================================

    let cst = parse_cst("- - a * b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-10 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Cst::Prefix(assert_src!(1:1-6 => PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:1-2)),
                        Token(token!(Whitespace, " ", 1:2-3)),
                        assert_src!(1:3-6 => Cst::Prefix(PrefixNode(OperatorNode {
                            op: PrefixOperator::Minus,
                            children: NodeSeq(vec![
                                Token(token!(Minus, "-", 1:3-4)),
                                Token(token!(Whitespace, " ", 1:4-5)),
                                Token(token!(Symbol, "a", 1:5-6)),
                            ]),
                        }))),
                    ]),
                }))),
                Token(token!(Whitespace, " ", 1:6-7)),
                Token(token!(Star, "*", 1:7-8)),
                Token(token!(Whitespace, " ", 1:8-9)),
                Token(token!(Symbol, "b", 1:9-10)),
            ]),
        })))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //
    // flatten_times = false
    //

    assert_eq!(
        abstract_cst(
            agg.clone(),
            QuirkSettings::default().flatten_times(false)
        ),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
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
                    data: src!(1:1-6).into(),
                },
                leaf!(Symbol, "b", 1:9-10),
            ],
            data: src!(1:1-10).into(),
        }
    );

    //
    // flatten_times = true
    //

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default().flatten_times(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                leaf!(Integer, "-1", <||>),
                leaf!(Integer, "-1", <||>),
                leaf!(Symbol, "a", 1:5-6),
                leaf!(Symbol, "b", 1:9-10),
            ],
            data: src!(1:1-10).into(),
        }
    );

    //==================================
    // Test basic flatten times through Binary Divide numerator (TID:231010/4)
    //==================================

    let cst = parse_cst("-a/b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-5 => Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Divide,
            children: NodeSeq(vec![
                Cst::Prefix(assert_src!(1:1-3 => PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:1-2)),
                        Token(token!(Symbol, "a", 1:2-3))
                    ]),
                }))),
                Token(token!(Slash, "/", 1:3-4)),
                Token(token!(Symbol, "b", 1:4-5)),
            ]),
        })))
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
    let cst = parse_cst("a*-(b)/c", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-9 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                assert_src!(1:3-9 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::Divide,
                    children: NodeSeq(vec![
                        Cst::Prefix(assert_src!(1:3-7 => PrefixNode(OperatorNode {
                            op: PrefixOperator::Minus,
                            children: NodeSeq(vec![
                                Token(token!(Minus, "-", 1:3-4)),
                                assert_src!(1:4-7 => Cst::Group(GroupNode(OperatorNode {
                                    op: GroupOperator::CodeParser_GroupParen,
                                    children: NodeSeq(vec![
                                        Token(token!(OpenParen, "(", 1:4-5)),
                                        Token(token!(Symbol, "b", 1:5-6)),
                                        Token(token!(CloseParen, ")", 1:6-7)),
                                    ]),
                                })))
                            ]),
                        }))),
                        Token(token!(Slash, "/", 1:7-8)),
                        Token(token!(Symbol, "c", 1:8-9)),
                    ]),
                }))),
            ]),
        })))
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

    let cst = parse_cst("a*-(-b)", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-8 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                Cst::Prefix(assert_src!(1:3-8 => PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:3-4)),
                        assert_src!(1:4-8 => Cst::Group(GroupNode(OperatorNode {
                            op: GroupOperator::CodeParser_GroupParen,
                            children: NodeSeq(vec![
                                Token(token!(OpenParen, "(", 1:4-5)),
                                Cst::Prefix(assert_src!(1:5-7 => PrefixNode(OperatorNode {
                                    op: PrefixOperator::Minus,
                                    children: NodeSeq(vec![
                                        Token(token!(Minus, "-", 1:5-6)),
                                        Token(token!(Symbol, "b", 1:6-7)),
                                    ]),
                                }))),
                                Token(token!(CloseParen, ")", 1:7-8)),
                            ]),
                        })))
                    ]),
                }))),
            ]),
        })))
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

    let cst = parse_cst("a*-(b*c)", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-9 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Star, "*", 1:2-3)),
                Cst::Prefix(assert_src!(1:3-9 => PrefixNode(OperatorNode {
                    op: PrefixOperator::Minus,
                    children: NodeSeq(vec![
                        Token(token!(Minus, "-", 1:3-4)),
                        assert_src!(1:4-9 => Cst::Group(GroupNode(OperatorNode {
                            op: GroupOperator::CodeParser_GroupParen,
                            children: NodeSeq(vec![
                                Token(token!(OpenParen, "(", 1:4-5)),
                                assert_src!(1:5-8 => Cst::Infix(InfixNode(OperatorNode {
                                    op: InfixOperator::Times,
                                    children: NodeSeq(vec![
                                        Token(token!(Symbol, "b", 1:5-6)),
                                        Token(token!(Star, "*", 1:6-7)),
                                        Token(token!(Symbol, "c", 1:7-8)),
                                    ]),
                                }))),
                                Token(token!(CloseParen, ")", 1:8-9)),
                            ]),
                        })))
                    ]),
                }))),
            ]),
        })))
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
    let cst = parse_cst("a<>StringJoin@b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-16 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::StringJoin,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(LessGreater, "<>", 1:2-4),),
                assert_src!(1:4-16 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "StringJoin", 1:4-14),),
                        Token(token!(At, "@", 1:14-15),),
                        Token(token!(Symbol, "b", 1:15-16),),
                    ]),
                }))),
            ]),
        })))
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

    let cst = parse_cst(r#"a + Plus @ b"#, &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-13 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Token(token!(Plus, "+", 1:3-4),),
                Token(token!(Whitespace, " ", 1:4-5),),
                assert_src!(1:5-13 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "Plus", 1:5-9),),
                        Token(token!(Whitespace, " ", 1:9-10),),
                        Token(token!(At, "@", 1:10-11),),
                        Token(token!(Whitespace, " ", 1:11-12),),
                        Token(token!(Symbol, "b", 1:12-13),),
                    ]),
                }))),
            ]),
        })))
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

    let cst = parse_cst("a === SameQ @ b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-16 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::SameQ,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2),),
                Token(token!(Whitespace, " ", 1:2-3),),
                Token(token!(EqualEqualEqual, "===", 1:3-6),),
                Token(token!(Whitespace, " ", 1:6-7),),
                assert_src!(1:7-16 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "SameQ", 1:7-12),),
                        Token(token!(Whitespace, " ", 1:12-13),),
                        Token(token!(At, "@", 1:13-14),),
                        Token(token!(Whitespace, " ", 1:14-15),),
                        Token(token!(Symbol, "b", 1:15-16),),
                    ]),
                }))),
            ]),
        })))
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

    //==================================
    // With Divide (aka Times) (TID:231010/5)
    //==================================

    //
    // NOTE: Divide also DOES NOT support the infix binary at quirky parsing
    //       behavior. Two cases are tested: "Times@a/b" and "Divide@a/b".
    //

    //
    // Variant A: "Times@..."
    //

    let cst = parse_cst("Times@a/b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-10 => Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Divide,
            children: NodeSeq(vec![
                assert_src!(1:1-8 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "Times", 1:1-6),),
                        Token(token!(At, "@", 1:6-7),),
                        Token(token!(Symbol, "a", 1:7-8),),
                    ]),
                }))),
                Token(token!(Slash, "/", 1:8-9),),
                Token(token!(Symbol, "b", 1:9-10),),
            ]),
        }))),
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    // Test that even with `infix_binary_at` quirk enabled, this DOES NOT
    // parse as a flag `SameQ[a, b]` expression.
    #[rustfmt::skip]
    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().infix_binary_at(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", 1:1-6)),
                    args: vec![
                        leaf!(Symbol, "a", 1:7-8),
                    ],
                    data: src!(1:1-8).into(),
                },
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Power", <||>)),
                    args: vec![
                        leaf!(Symbol, "b", 1:9-10),
                        leaf!(Integer, "-1", <||>),
                    ],
                    data: src!(1:1-10).into(),
                },
            ],
            data: src!(1:1-10).into(),
        }
    );

    //
    // Variant B: "Divide@..."
    //

    let cst = parse_cst("Divide@a/b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-11 => Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Divide,
            children: NodeSeq(vec![
                assert_src!(1:1-9 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "Divide", 1:1-7),),
                        Token(token!(At, "@", 1:7-8),),
                        Token(token!(Symbol, "a", 1:8-9),),
                    ]),
                }))),
                Token(token!(Slash, "/", 1:9-10),),
                Token(token!(Symbol, "b", 1:10-11),),
            ]),
        }))),
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    // Test that even with `infix_binary_at` quirk enabled, this DOES NOT
    // parse as a flag `SameQ[a, b]` expression.
    #[rustfmt::skip]
    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().infix_binary_at(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Times", <||>)),
            args: vec![
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Divide", 1:1-7)),
                    args: vec![
                        leaf!(Symbol, "a", 1:8-9),
                    ],
                    data: src!(1:1-9).into(),
                },
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Power", <||>)),
                    args: vec![
                        leaf!(Symbol, "b", 1:10-11),
                        leaf!(Integer, "-1", <||>),
                    ],
                    data: src!(1:1-11).into(),
                },
            ],
            data: src!(1:1-11).into(),
        }
    );
}

#[test]
fn test_abstract_flatten_times_combined_with_infix_binary_at_quirk() {
    let cst = parse_cst("a * Times @ -b", &Default::default()).syntax;

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

    assert_eq!(
        cst,
        assert_src!(1:1-15 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(Star, "*", 1:3-4)),
                Token(token!(Whitespace, " ", 1:4-5)),
                assert_src!(1:5-15 => Cst::Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::CodeParser_BinaryAt,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "Times", 1:5-10)),
                        Token(token!(Whitespace, " ", 1:10-11)),
                        Token(token!(At, "@", 1:11-12)),
                        Token(token!(Whitespace, " ", 1:12-13)),
                        assert_src!(1:13-15 => Cst::Prefix(PrefixNode(OperatorNode {
                            op: PrefixOperator::Minus,
                            children: NodeSeq(vec![
                                Token(token!(Minus, "-", 1:13-14),),
                                Token(token!(Symbol, "b", 1:14-15),),
                            ]),
                        }))),
                    ]),
                }))),
            ]),
        })))
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

/// TID:231104/1: OldAtAtAt quirk cases
#[test]
fn test_abstract_old_at_at_at_quirk() {
    let cst = parse_cst("a @@@ b", &Default::default()).syntax;

    assert_eq!(
        cst,
        assert_src!(1:1-8 => Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::MapApply,
            children: NodeSeq(vec![
                Cst::Token(token!(Symbol, "a", 1:1-2)),
                Cst::Token(token!(Whitespace, " ", 1:2-3)),
                Cst::Token(token!(AtAtAt, "@@@", 1:3-6)),
                Cst::Token(token!(Whitespace, " ", 1:6-7)),
                Cst::Token(token!(Symbol, "b", 1:7-8)),
            ]),
        })))
    );

    let agg = aggregate_cst(cst.clone()).unwrap();

    //----------------------------------
    // OldAtAtAt Quirk: Disabled
    //----------------------------------

    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().old_at_at_at(false)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "MapApply", <||>)),
            args: vec![leaf!(Symbol, "a", 1:1-2), leaf!(Symbol, "b", 1:7-8),],
            data: src!(1:1-8).into(),
        }
    );

    //----------------------------------
    // OldAtAtAt Quirk: Enabled
    //----------------------------------

    assert_eq!(
        abstract_cst(agg.clone(), QuirkSettings::default().old_at_at_at(true)),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Apply", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Symbol, "b", 1:7-8),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "List", <||>)),
                    args: vec![leaf!(Integer, "1", <||>)],
                    data: AstMetadata::empty()
                }
            ],
            data: src!(1:1-8).into(),
        }
    );
}

#[test]
fn test_abstract_plus() {
    // TID:231104/2: "+a + b - c \[ImplicitPlus] d" is a single Plus expression (?)
    let cst =
        parse_cst("+a + b - c \\[ImplicitPlus] d", &Default::default()).syntax;

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        agg,
        assert_src!(1:1-29 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                assert_src!(1:1-3 => Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Plus,
                    children: NodeSeq(vec![
                        Token(token!(Plus, "+", 1:1-2)),
                        Token(token!(Symbol, "a", 1:2-3)),
                    ]),
                }))),
                Cst::Token(token!(Plus, "+", 1:4-5)),
                Cst::Token(token!(Symbol, "b", 1:6-7)),
                Cst::Token(token!(Minus, "-", 1:8-9)),
                assert_src!(1:10-29 => Cst::Infix(InfixNode(OperatorNode {
                    op: InfixOperator::Plus,
                    children: NodeSeq(vec![
                        Cst::Token(token!(Symbol, "c", 1:10-11)),
                        Cst::Token(
                            token!(LongName_ImplicitPlus, "\\[ImplicitPlus]", 1:12-27),
                        ),
                        Cst::Token(token!(Symbol, "d", 1:28-29)),
                    ]),
                }))),
            ]),
        })))
    );

    let ast = abstract_cst(agg, QuirkSettings::default());

    assert_eq!(
        ast,
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Plus", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:2-3),
                leaf!(Symbol, "b", 1:6-7),
                Ast::Call {
                    head: Box::new(leaf!(Symbol, "Times", <||>)),
                    args: vec![
                        leaf!(Integer, "-1", <||>),
                        Ast::Call {
                            head: Box::new(leaf!(Symbol, "Plus", <||>)),
                            args: vec![
                                leaf!(Symbol, "c", 1:10-11),
                                leaf!(Symbol, "d", 1:28-29),
                            ],
                            data: src!(1:10-29).into(),
                        },
                    ],
                    data: src!(1:8-29).into(),
                },
            ],
            data: src!(1:1-29).into(),
        }
    );
}

#[test]
fn test_abstract_box_sources() {
    //
    // TID:231110/2 — Plus pair synthetic "between" BoxPosition
    //

    // From: RowBox[{"{", RowBox[{"E", "-", "1"}], "}"}]
    let cst = assert_src!(src!({}) => Cst::Group(GroupNode(OperatorNode {
        op: GroupOperator::List,
        children: NodeSeq(vec![
            Cst::Token(token!(OpenCurly, "{", {1, 1})),
            assert_src!(src!({1, 2}) => Cst::Infix(InfixNode(OperatorNode {
                op: InfixOperator::Plus,
                children: NodeSeq(vec![
                    Cst::Token(token![Symbol, "E", {1, 2, 1, 1}]),
                    Cst::Token(token![Minus, "-", {1, 2, 1, 2}]),
                    Cst::Token(token![Integer, "1", {1, 2, 1, 3}])
                ]),
            }))),
            Cst::Token(token![CloseCurly, "}", {1, 3}])
        ])
    })));

    assert_eq!(
        abstract_cst(cst, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(leaf![Symbol, "List", <||>]),
            args: vec![Ast::Call {
                head: Box::new(leaf![Symbol, "Plus", <||>]),
                args: vec![
                    leaf![Symbol, "E", {1, 2, 1, 1}],
                    // NOTE: The source location of this node has to be computed
                    //       from the separate `-` and `1` tokens in the
                    //       original box input.
                    leaf![Integer, "-1", src!({1, 2, 1, (2 ;; 3)})]
                ],
                data: AstMetadata::from(src!({1, 2}))
            }],
            data: AstMetadata::from(src!({}))
        }
    );
}

#[test]
fn test_abstract_call() {
    //==================================
    // Test Part syntax
    // TID:231112/1: "f[[x]]"
    //==================================

    let cst = parse_cst("f[[x]]", &Default::default()).syntax;

    assert_eq!(
        cst,
        Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(Symbol, "f", 1:1-2),
            )])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    Token(token!(OpenSquare, "[", 1:2-3)),
                    Group(GroupNode(OperatorNode {
                        op: GroupOperator::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:3-4)),
                            Token(token!(Symbol, "x", 1:4-5)),
                            Token(token!(CloseSquare, "]", 1:5-6)),
                        ]),
                    })),
                    Token(token!(CloseSquare, "]", 1:6-7)),
                ]),
            })),
        })
    );

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Part", <||>)),
            args: vec![leaf!(Symbol, "f", 1:1-2), leaf!(Symbol, "x", 1:4-5)],
            data: src!(1:1-7).into()
        }
    );

    //==================================
    // Test suspicious call of SlotSequence
    // TID:231112/2: "##2[arg]"
    //==================================

    let cst = parse_cst("##2[arg]", &Default::default()).syntax;

    assert_eq!(
        cst,
        Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Compound(CompoundNode(
                OperatorNode {
                    op: CompoundOperator::SlotSequence,
                    children: NodeSeq(vec![
                        Token(token!(HashHash, "##", 1:1-3)),
                        Token(token!(Integer, "2", 1:3-4)),
                    ]),
                },
            ))])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    Token(token!(OpenSquare, "[", 1:4-5)),
                    Token(token!(Symbol, "arg", 1:5-8)),
                    Token(token!(CloseSquare, "]", 1:8-9)),
                ]),
            })),
        })
    );

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(Ast::Call {
                head: Box::new(leaf!(Symbol, "SlotSequence", <||>)),
                args: vec![leaf!(Integer, "2", 1:3-4),],
                data: src!(1:1-4).into(),
            }),
            args: vec![leaf!(Symbol, "arg", 1:5-8),],
            data: AstMetadata {
                source: src!(1:1-9).into(),
                issues: vec![Issue {
                    make_sym: st::CodeParser_SyntaxIssue,
                    tag: IssueTag::StrangeCallSlotSequence,
                    msg: "Unexpected call.".to_owned(),
                    sev: Severity::Error,
                    src: src!(1:4-5).into(),
                    val: 1.0,
                    actions: vec![],
                    additional_descriptions: vec![],
                    additional_sources: vec![src!(1:8-9).into()],
                }],
            },
        }
    );

    //==================================
    // Test TypeSpecifier syntax
    // TID:231112/3: "foo"::[arg]
    //==================================

    let cst = parse_cst(r#""foo"::[arg]"#, &Default::default()).syntax;

    assert_eq!(
        cst,
        Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(String, "\"foo\"", 1:1-6),
            )])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupTypeSpecifier,
                children: NodeSeq(vec![
                    Token(token!(ColonColonOpenSquare, "::[", 1:6-9)),
                    Token(token!(Symbol, "arg", 1:9-12)),
                    Token(token!(CloseSquare, "]", 1:12-13)),
                ]),
            })),
        })
    );

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(Ast::Call {
                head: Box::new(leaf!(Symbol, "TypeSpecifier", <||>)),
                args: vec![leaf!(String, "\"foo\"", 1:1-6),],
                data: AstMetadata::empty(),
            }),
            args: vec![leaf!(Symbol, "arg", 1:9-12),],
            data: src!(1:1-13).into(),
        }
    );

    //==================================
    // Test double bracket long name syntax
    // TID:231112/4: "a\[LeftDoubleBracket]2\[RightDoubleBracket]"
    //==================================

    let cst = parse_cst(
        r#"a\[LeftDoubleBracket]2\[RightDoubleBracket]"#,
        &Default::default(),
    )
    .syntax;

    assert_eq!(
        cst,
        Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(Symbol, "a", 1:1-2),
            )])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupDoubleBracket,
                children: NodeSeq(vec![
                    Token(
                        token!(LongName_LeftDoubleBracket, "\\[LeftDoubleBracket]", 1:2-22)
                    ),
                    Token(token!(Integer, "2", 1:22-23)),
                    Token(
                        token!(LongName_RightDoubleBracket, "\\[RightDoubleBracket]", 1:23-44)
                    ),
                ]),
            })),
        })
    );

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Part", <||>)),
            args: vec![leaf!(Symbol, "a", 1:1-2), leaf!(Integer, "2", 1:22-23),],
            data: src!(1:1-44).into(),
        }
    );

    //==================================
    // Test double bracket long name syntax with multiple args
    // TID:231112/5: "a\[LeftDoubleBracket]1, 2, 3\[RightDoubleBracket]"
    //==================================

    let cst = parse_cst(
        r#"a\[LeftDoubleBracket]1, 2, 3\[RightDoubleBracket]"#,
        &Default::default(),
    )
    .syntax;

    assert_eq!(
        cst,
        Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(Symbol, "a", 1:1-2),
            )])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupDoubleBracket,
                children: NodeSeq(vec![
                    Token(
                        token!(LongName_LeftDoubleBracket, "\\[LeftDoubleBracket]", 1:2-22),
                    ),
                    Infix(InfixNode(OperatorNode {
                        op: InfixOperator::CodeParser_Comma,
                        children: NodeSeq(vec![
                            Token(token!(Integer, "1", 1:22-23)),
                            Token(token!(Comma, ",", 1:23-24)),
                            Token(token!(Whitespace, " ", 1:24-25)),
                            Token(token!(Integer, "2", 1:25-26)),
                            Token(token!(Comma, ",", 1:26-27)),
                            Token(token!(Whitespace, " ", 1:27-28)),
                            Token(token!(Integer, "3", 1:28-29)),
                        ]),
                    })),
                    Token(
                        token!(LongName_RightDoubleBracket, "\\[RightDoubleBracket]", 1:29-50),
                    ),
                ]),
            })),
        })
    );

    let agg = aggregate_cst(cst).unwrap();

    assert_eq!(
        abstract_cst(agg, QuirkSettings::default()),
        Ast::Call {
            head: Box::new(leaf!(Symbol, "Part", <||>)),
            args: vec![
                leaf!(Symbol, "a", 1:1-2),
                leaf!(Integer, "1", 1:22-23),
                leaf!(Integer, "2", 1:25-26),
                leaf!(Integer, "3", 1:28-29),
            ],
            data: src!(1:1-50).into(),
        }
    );
}
