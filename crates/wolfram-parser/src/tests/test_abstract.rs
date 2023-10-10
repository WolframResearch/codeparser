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
        abstract_cst(agg, QuirkSettings::default().flatten_times()),
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
