mod test_api;
mod test_byte_decoder;
mod test_character_decoder;
mod test_crashers;
mod test_ffi;
mod test_node;
mod test_parselet;
mod test_source_character;
mod test_token_enum;
mod test_tokenizer;
mod test_wl_character;

use pretty_assertions::assert_eq;

use crate::{
    ast::{Ast, AstMetadata},
    cst::{
        BinaryNode, BinaryOperator, CallBody, CallHead, CallNode, CallOperator,
        Cst, Cst::Token as NVToken, GroupMissingCloserNode, GroupNode,
        GroupOperator, InfixNode, InfixOperator, OperatorNode,
    },
    macros::{src, token},
    parse::ParserSession,
    parse_bytes_cst,
    source::{Source, SourceConvention},
    tokenize,
    tokenize::{Token, TokenInput, TokenKind as TK, TokenStr, TokenString},
    FirstLineBehavior, NodeSeq, ParseOptions, ParseResult, Tokens,
};

pub(crate) fn nodes(input: &str) -> Vec<Cst<TokenStr>> {
    let mut session =
        ParserSession::new(input.as_bytes(), &ParseOptions::default());

    let result = session.concrete_parse_expressions();

    let NodeSeq(nodes) = NodeSeq(result.nodes().to_vec());

    nodes
}

pub(crate) fn tokens(input: &str) -> Vec<Token<TokenStr>> {
    let Tokens(tokens) = tokenize(input, &ParseOptions::default());

    tokens
}

fn concrete_exprs(input: &str, opts: ParseOptions) -> Vec<Cst<TokenStr>> {
    let mut session = ParserSession::new(input.as_bytes(), &opts);

    let ParseResult { nodes, .. } = session.concrete_parse_expressions();

    let NodeSeq(nodes) = nodes;

    nodes
}

fn concrete_exprs_character_index(input: &str) -> Vec<Cst<TokenStr>> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        &ParseOptions::default()
            .source_convention(SourceConvention::CharacterIndex),
    );

    let ParseResult { nodes, .. } = session.concrete_parse_expressions();

    let NodeSeq(nodes) = nodes;

    nodes
}

#[test]
fn test_1p2() {
    assert_eq!(
        tokens("1+2"),
        vec![
            token![Integer, "1", src!(1:1-1:2)],
            token![Plus, "+", src!(1:2-1:3)],
            token![Integer, "2", src!(1:3-1:4)],
        ]
    );
}

#[test]
fn test_something() {
    assert_eq!(tokens("123"), vec![token![Integer, "123", src!(1:1-1:4)]]);

    assert_eq!(
        nodes("foo"),
        vec![NVToken(token![Symbol, "foo", src!(1:1-1:4)])]
    );

    assert_eq!(
        tokens("a+b"),
        vec![
            token![Symbol, "a", src!(1:1-1:2)],
            token![Plus, "+", src!(1:2-1:3)],
            token![Symbol, "b", src!(1:3-1:4)],
        ]
    );

    assert_eq!(
        tokens("!a"),
        vec![
            token![Bang, "!", src!(1:1-1:2)],
            token![Symbol, "a", src!(1:2-1:3)]
        ]
    );

    assert_eq!(
        tokens("2 + 2"),
        vec![
            token![Integer, "2", src!(1:1-1:2)],
            token![Whitespace, " ", src!(1:2-1:3)],
            token![Plus, "+", src!(1:3-1:4)],
            token![Whitespace, " ", src!(1:4-1:5)],
            token![Integer, "2", src!(1:5-1:6)],
        ]
    );

    assert_eq!(
        nodes("2 + 2"),
        vec![Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                NVToken(token![Integer, "2", src!(1:1-1:2)]),
                NVToken(token![Whitespace, " ", src!(1:2-1:3)]),
                NVToken(token![Plus, "+", src!(1:3-1:4)]),
                NVToken(token![Whitespace, " ", src!(1:4-1:5)]),
                NVToken(token![Integer, "2", src!(1:5-1:6)]),
            ],),
            src: src!(1:1-1:6).into(),
        }))]
    );
}

#[test]
fn test_call_head_seq() {
    assert_eq!(
        nodes("f[x]"),
        vec![Cst::Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Cst::Token(token![
                Symbol,
                "f",
                src!(1:1-1:2)
            ])])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    NVToken(token![OpenSquare, "[", src!(1:2-1:3)]),
                    NVToken(token![Symbol, "x", src!(1:3-1:4)]),
                    NVToken(token![CloseSquare, "]", src!(1:4-1:5)]),
                ]),
                src: src!(1:2-1:5).into(),
            })),
            src: src!(1:1-1:5).into(),
        })]
    );

    // Test parsing call node where the head is a sequence with more than one
    // Cst element.
    assert_eq!(
        nodes("f (* hello *)[x]"),
        vec![Cst::Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![
                Cst::Token(token!(Symbol, "f", 1:1-2)),
                Cst::Token(token!(Whitespace, " ", 1:2-3)),
                Cst::Token(token!(Comment, "(* hello *)", 1:3-14)),
            ])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    Cst::Token(token![OpenSquare, "[", src!(1:14-15)]),
                    Cst::Token(token![Symbol, "x", src!(1:15-16)]),
                    Cst::Token(token![CloseSquare, "]", src!(1:16-17)]),
                ]),
                src: src!(1:14-17).into(),
            })),
            src: src!(1:1-17).into(),
        })]
    );

    // Sanity test what a top-level comment before a function head groups with.
    assert_eq!(
        nodes("(* hello *) f[x]"),
        vec![
            Cst::Token(token!(Comment, "(* hello *)", 1:1-12)),
            Cst::Token(token!(Whitespace, " ", 1:12-13)),
            Cst::Call(CallNode {
                head: CallHead::Concrete(NodeSeq(vec![Cst::Token(
                    token!(Symbol, "f", 1:13-14)
                ),])),
                body: CallBody::Group(GroupNode(OperatorNode {
                    op: CallOperator::CodeParser_GroupSquare,
                    children: NodeSeq(vec![
                        Cst::Token(token![OpenSquare, "[", src!(1:14-15)]),
                        Cst::Token(token![Symbol, "x", src!(1:15-16)]),
                        Cst::Token(token![CloseSquare, "]", src!(1:16-17)]),
                    ]),
                    src: src!(1:14-17).into(),
                })),
                src: src!(1:13-17).into(),
            })
        ]
    );

    // Test what an interior comment before a function head groups with.
    assert_eq!(
        nodes("foo[(* hello *) bar[x]]"),
        vec![Cst::Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Cst::Token(
                token!(Symbol, "foo", 1:1-4)
            ),])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    Cst::Token(token![OpenSquare, "[", 1:4-5]),
                    // NOTE: This comment groups with the *enclosing group*,
                    //       NOT the head of the interior function.
                    Cst::Token(token!(Comment, "(* hello *)", 1:5-16)),
                    Cst::Token(token!(Whitespace, " ", 1:16-17)),
                    Cst::Call(CallNode {
                        head: CallHead::Concrete(NodeSeq(vec![Cst::Token(
                            token!(Symbol, "bar", 1:17-20)
                        )])),
                        body: CallBody::Group(GroupNode(OperatorNode {
                            op: CallOperator::CodeParser_GroupSquare,
                            children: NodeSeq(vec![
                                Cst::Token(token![OpenSquare, "[", 1:20-21]),
                                Cst::Token(token![Symbol, "x", 1:21-22]),
                                Cst::Token(token![CloseSquare, "]", 1:22-23]),
                            ]),
                            src: src!(1:20-23).into(),
                        })),
                        // NOTE: Call source span does NOT include the comment.
                        src: src!(1:17-23).into()
                    }),
                    Cst::Token(token![CloseSquare, "]", 1:23-24]),
                ]),
                src: src!(1:4-24).into(),
            })),
            src: src!(1:1-24).into(),
        })]
    );
}

#[test]
fn test_ast_src() {
    let ast = crate::parse_ast("a/", &Default::default());
    let [ast]: &[_; 1] = ast.nodes().try_into().unwrap();

    assert_eq!(ast.span(), src!(1:1-3).into());
}

#[test]
fn test_box_source_recovery() {
    // Copied from from TestID -> "Abstract-20210504-G2K4C0" concrete parse.
    let cst: CallNode<_, Source> = CallNode {
        head: CallHead::Concrete(NodeSeq(vec![Cst::Token(
            token![Symbol, "Begin", {1, 1, 1}],
        )])),
        body: CallBody::Group(GroupNode(OperatorNode {
            op: CallOperator::CodeParser_GroupSquare,
            children: NodeSeq(vec![
                Cst::Token(token![OpenSquare, "[", {1, 1, 2}]),
                Cst::Token(token![String, "\"FindMinimumTrek`\"", {1, 1, 3}]),
                Cst::Token(token![CloseSquare, "]", {1, 1, 4}]),
            ]),
            src: Source::unknown(),
        })),
        src: src!({ 1 }), // <|Source -> {1} |>
    };

    assert_eq!(cst.src, Source::BoxPosition(vec![1]))
}


#[test]
pub fn test_tokenize_is_not_idempotent() {
    let mut session = ParserSession::new(
        "2+2".as_bytes(),
        &ParseOptions::default()
            .source_convention(SourceConvention::CharacterIndex),
    );

    assert_eq!(
        session.tokenize().unwrap().0,
        vec![
            token![Integer, "2", src!(1 - 2)],
            token![Plus, "+", src!(2 - 3)],
            token![Integer, "2", src!(3 - 4)]
        ]
    );

    // Test that ParserSession::tokenize() is NOT idempotent.
    assert_eq!(session.tokenize().unwrap().0, Vec::<Token<TokenStr>>::new())
}

#[test]
fn test_character_index_source() {
    assert_eq!(
        concrete_exprs_character_index("2+2"),
        &[Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                NVToken(token![Integer, "2", src!(1..2)]),
                NVToken(token![Plus, "+", src!(2..3)]),
                NVToken(token![Integer, "2", src!(3..4)])
            ]),
            src: src!(1 - 4).into(),
        }))]
    );
}

/// Tests the heuristics used to simplify the included tokens and source span
/// in error nodes representing groups that were opened but have no matching
/// closer.
#[test]
fn test_unterminated_group_reparse() {
    assert_eq!(
        concrete_exprs("{", ParseOptions::default()),
        &[Cst::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: GroupOperator::List,
                children: NodeSeq(vec![Cst::Token(token![
                    OpenCurly,
                    [123],
                    src!(1:1-1:2)
                ])]),
                src: src!(1:1-1:2).into(),
            },
        ),)]
    );

    // assert_eq!(concrete_exprs("f["), &[]);

    assert_eq!(
        concrete_exprs("\"\n", ParseOptions::default()),
        &[Cst::Token(token![
            Error_UnterminatedString,
            "\"",
            src!(1:1-1:2)
        ])]
    );


    assert_eq!(
        concrete_exprs_character_index("\"\n"),
        &[NVToken(token![
            Error_UnterminatedString,
            "\"\n",
            src!(1..3)
        ])]
    );

    assert_eq!(
        // <| ?
        // 123456
        //   ^ \t
        concrete_exprs("<|\t?", ParseOptions::default().tab_width(1)),
        &[Cst::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: GroupOperator::Association,
                children: NodeSeq(vec![
                    NVToken(token![LessBar, "<|", src!(1:1-1:3)]),
                    NVToken(token![Whitespace, "\t", src!(1:3-1:4)]),
                    NVToken(token![Error_ExpectedOperand, "", src!(1:4-1:4)]),
                    NVToken(token![Question, "?", src!(1:4-1:5)]),
                    NVToken(token![Error_ExpectedOperand, "", src!(1:5-1:5)]),
                ]),
                src: src!(1:1-1:5).into(),
            }
        ))]
    );

    // Same test as above, but using a tab width of 4 instead of 1.
    assert_eq!(
        // <|  ?
        // 123456
        //   ^^ \t
        concrete_exprs("<|\t?", ParseOptions::default()),
        &[Cst::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: GroupOperator::Association,
                children: NodeSeq(vec![
                    NVToken(token![LessBar, "<|", src!(1:1-1:3)]),
                    NVToken(token![Whitespace, "\t", src!(1:3-1:5)]),
                    NVToken(token![Error_ExpectedOperand, "", src!(1:5-1:5)]),
                    NVToken(token![Question, "?", src!(1:5-1:6)]),
                    NVToken(token![Error_ExpectedOperand, "", src!(1:6-1:6)])
                ]),
                src: src!(1:1-1:6).into()
            }
        ))]
    );

    //==================================

    #[rustfmt::skip]
    assert_eq!(nodes("(a  "), &[
        Cst::GroupMissingCloser(GroupMissingCloserNode(OperatorNode {
            op: GroupOperator::CodeParser_GroupParen,
            children: NodeSeq(vec![
                Cst::Token(token!(OpenParen, "(", src!(1:1-1:2))),
                Cst::Token(token!(Symbol, "a", src!(1:2-1:3))),
                Cst::Token(token!(Whitespace, " ", src!(1:3-1:4))),
                Cst::Token(token!(Whitespace, " ", src!(1:4-1:5))),
            ],),
            src: src!(1:1-1:5).into(),
        },),),
    ]);

    //==================================

    let unterminated_paren: &str = r#"
global = (a
+ b

nextLooksLikeToplevelStatement = foo

"#;

    // Test that any tokens associated with `nextLooksLikeToplevelStatement`
    // are removed from the unterminated paren node.
    #[rustfmt::skip]
    assert_eq!(nodes(unterminated_paren), &[
        Cst::Token(token!(ToplevelNewline, "\n", src!(1:1-2:1))),
        Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Set,
            children: NodeSeq(vec![
                Cst::Token(token!(Symbol, "global", src!(2:1-2:7))),
                Cst::Token(token!(Whitespace, " ", src!(2:7-2:8))),
                Cst::Token(token!(Equal, "=", src!(2:8-2:9)),),
                Cst::Token(token!(Whitespace, " ", src!(2:9-2:10)),),
                Cst::GroupMissingCloser(GroupMissingCloserNode(
                    OperatorNode {
                        op: GroupOperator::CodeParser_GroupParen,
                        children: NodeSeq(vec![
                            Cst::Token(token!(OpenParen, "(", src!(2:10-2:11))),
                            Cst::Token(token!(Symbol, "a", src!(2:11-2:12))),
                            Cst::Token(token!(InternalNewline, "\n", src!(2:12-3:1))),
                            Cst::Token(token!(Plus, "+", src!(3:1-3:2))),
                            Cst::Token(token!(Whitespace, " ", src!(3:2-3:3))),
                            Cst::Token(token!(Symbol, "b", src!(3:3-3:4))),
                            Cst::Token(token!(InternalNewline, "\n", src!(3:4-4:1))),
                        ]),
                        src: src!(2:10-3:4).into(),
                    },
                )),
            ]),
            src: src!(2:1-3:4).into(),
        })),
    ]);

    //==================================

    let unterminated_paren: &str = r#"
global = (a
+ b

nextStatement

"#;

    // Test that any tokens associated with `nextStatement` ARE included in the
    // unterminated paren node.
    #[rustfmt::skip]
    assert_eq!(nodes(unterminated_paren), &[
        Cst::Token(token!(ToplevelNewline, "\n", src!(1:1-2:1))),
        Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Set,
            children: NodeSeq(vec![
                Cst::Token(token!(Symbol, "global", src!(2:1-2:7))),
                Cst::Token(token!(Whitespace, " ", src!(2:7-2:8))),
                Cst::Token(token!(Equal, "=", src!(2:8-2:9)),),
                Cst::Token(token!(Whitespace, " ", src!(2:9-2:10)),),
                Cst::GroupMissingCloser(GroupMissingCloserNode(
                    OperatorNode {
                        op: GroupOperator::CodeParser_GroupParen,
                        children: NodeSeq(vec![
                            Cst::Token(token!(OpenParen, "(", src!(2:10-2:11))),
                            Cst::Token(token!(Symbol, "a", src!(2:11-2:12))),
                            Cst::Token(token!(InternalNewline, "\n", src!(2:12-3:1))),
                            Cst::Token(token!(Plus, "+", src!(3:1-3:2))),
                            Cst::Token(token!(Whitespace, " ", src!(3:2-3:3))),
                            Cst::Token(token!(Symbol, "b", src!(3:3-3:4))),
                            Cst::Token(token!(InternalNewline, "\n", src!(3:4-4:1))),
                            Cst::Token(token!(InternalNewline, "\n", src!(4:1-5:1))),
                            Cst::Token(token!(Fake_ImplicitTimes, "", src!(5:1-1))),
                            Cst::Token(token!(Symbol, "nextStatement", src!(5:1-14))),
                            Cst::Token(token!(InternalNewline, "\n", src!(5:14-6:1))),
                        ]),
                        src: src!(2:10-5:14).into(),
                    },
                )),
            ]),
            src: src!(2:1-5:14).into(),
        })),
    ]);

    //==================================

    let unterminated_paren: &str = r#"
global = {(a
+ b


}

nextStatement

"#;

    // Test that *internal* missing closers are NOT reparsed.
    #[rustfmt::skip]
    assert_eq!(nodes(unterminated_paren), &[
        Cst::Token(token!(ToplevelNewline, "\n", 1:1-2:1)),
        Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Set,
            children: NodeSeq(vec![
                Cst::Token(token!(Symbol, "global", 2:1-7)),
                Cst::Token(token!(Whitespace, " ", 2:7-8)),
                Cst::Token(token!(Equal, "=", 2:8-9),),
                Cst::Token(token!(Whitespace, " ", 2:9-10),),
                Cst::Group(GroupNode(
                    OperatorNode {
                        op: GroupOperator::List,
                        children: NodeSeq(vec![
                            Cst::Token(token![OpenCurly, "{", 2:10-11]),
                            Cst::GroupMissingCloser(GroupMissingCloserNode(
                                OperatorNode {
                                    op: GroupOperator::CodeParser_GroupParen,
                                    children: NodeSeq(vec![
                                        Cst::Token(token!(OpenParen, "(", 2:11-12)),
                                        Cst::Infix(InfixNode(OperatorNode {
                                            op: InfixOperator::Plus,
                                            children: NodeSeq(vec![
                                                Cst::Token(token!(Symbol, "a", 2:12-13)),
                                                Cst::Token(token!(InternalNewline, "\n", 2:13-3:1)),
                                                Cst::Token(token!(Plus, "+", 3:1-2)),
                                                Cst::Token(token!(Whitespace, " ", 3:2-3)),
                                                Cst::Token(token!(Symbol, "b", 3:3-4)),
                                            ]),
                                            src: src!(2:12-3:4).into()
                                        })),
                                    ]),
                                    src: src!(2:11-3:4).into(),
                                },
                            )),
                            Cst::Token(token!(InternalNewline, "\n", 3:4-4:1)),
                            Cst::Token(token!(InternalNewline, "\n", 4:1-5:1)),
                            Cst::Token(token!(InternalNewline, "\n", 5:1-6:1)),
                            Cst::Token(token![CloseCurly, "}", 6:1-2])
                        ]),
                        src: src!(2:10-6:2).into()
                    }
                )),
            ]),
            src: src!(2:1-6:2).into(),
        })),
        Cst::Token(token!(ToplevelNewline, "\n", 6:2-7:1)),
        Cst::Token(token!(ToplevelNewline, "\n", 7:1-8:1)),
        Cst::Token(token!(Symbol, "nextStatement", 8:1-14)),
        Cst::Token(token!(ToplevelNewline, "\n", 8:14-9:1)),
        Cst::Token(token!(ToplevelNewline, "\n", 9:1-10:1)),
    ]);
}

#[test]
fn test_invalid_utf8_in_middle_of_parse() {
    // This tests what the parse result is where there are invalid UTF-8 bytes
    // in the input followed by valid UTF-8 bytes.
    //
    // The presence of invalid UTF-8 shouldn't halt the parsing process.
    let result = parse_bytes_cst(
        &[b'1', b'+', 0xE2, 0x9A, b'1'],
        &ParseOptions::default(),
    );

    assert_eq!(
        result.nodes.0,
        &[Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Cst::Infix(InfixNode(OperatorNode {
                    op: InfixOperator::Plus,
                    children: NodeSeq(vec![
                        NVToken(token![Integer, "1", src!(1:1-1:2)]),
                        NVToken(token![Plus, "+", src!(1:2-1:3)]),
                        NVToken(token![
                            Error_UnsafeCharacterEncoding,
                            [0xE2, 0x9A],
                            src!(1:3-1:4)
                        ])
                    ]),
                    src: src!(1:1-1:4).into()
                })),
                NVToken(token!(Fake_ImplicitTimes, "", src!(1:4-1:4))),
                NVToken(token!(Integer, "1", src!(1:4-1:5)))
            ]),
            src: src!(1:1-1:5).into()
        }))]
    );
}

#[test]
fn test_first_line_behavior() {
    // FirstLineBehavior::Check without shebang
    assert_eq!(
        tokenize(
            "1+2",
            &ParseOptions::default()
                .first_line_behavior(FirstLineBehavior::Check)
        ),
        Tokens(vec![
            token![Integer, "1", src!(1:1-1:2)],
            token![Plus, "+", src!(1:2-1:3)],
            token![Integer, "2", src!(1:3-1:4)],
        ])
    );

    // FirstLineBehavior::Check with shebang
    assert_eq!(
        tokenize(
            "#!/usr/bin/env blah \
           \n1+2",
            &ParseOptions::default()
                .first_line_behavior(FirstLineBehavior::Check)
        ),
        Tokens(vec![
            token![Integer, "1", src!(2:1-2:2)],
            token![Plus, "+", src!(2:2-2:3)],
            token![Integer, "2", src!(2:3-2:4)],
        ])
    );
}

//==========================================================
// Abstract
//==========================================================

#[test]
fn test_abstract_parse() {
    let result = crate::parse_ast("2 + 2", &ParseOptions::default());

    assert_eq!(
        result.nodes(),
        &[Ast::Call {
            head: Box::new(Ast::Leaf {
                kind: TK::Symbol,
                input: TokenString::fake("Plus"),
                data: AstMetadata::empty()
            }),
            args: vec![
                Ast::Leaf {
                    kind: TK::Integer,
                    input: TokenString::fake("2"),
                    data: AstMetadata {
                        source: Source::Span(src!(1:1-1:2).into()),
                        issues: vec![],
                    },
                },
                Ast::Leaf {
                    kind: TK::Integer,
                    input: TokenString::fake("2"),
                    data: AstMetadata {
                        source: Source::Span(src!(1:5-1:6).into()),
                        issues: vec![],
                    },
                },
            ],
            data: AstMetadata {
                source: Source::Span(src!(1:1-1:6).into()),
                issues: vec![],
            },
        },]
    )
}
