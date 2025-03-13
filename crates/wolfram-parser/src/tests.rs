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
    ast::{AstMetadata, AstNode},
    cst::{
        CallBody, CallHead, CallNode, CallOperator, CstNode,
        CstNode::Token as NVToken, GroupMissingCloserNode, GroupNode,
        GroupOperator, InfixNode, InfixOperator, Node, OperatorNode,
    },
    macros::{src, token},
    parse::ParserSession,
    parse_bytes_to_cst,
    source::{Source, SourceConvention},
    tokenize,
    tokenize::{
        BorrowedTokenInput, OwnedTokenInput, Token, TokenInput, TokenKind as TK,
    },
    FirstLineBehavior, NodeSeq, ParseOptions, ParseResult, Tokens,
};

pub(crate) fn nodes(input: &str) -> Vec<Node<BorrowedTokenInput>> {
    let mut session =
        ParserSession::new(input.as_bytes(), &ParseOptions::default());

    let result = session.concrete_parse_expressions();

    let NodeSeq(nodes) = NodeSeq(result.nodes().to_vec());

    nodes
}

pub(crate) fn tokens(input: &str) -> Vec<Token<BorrowedTokenInput>> {
    let Tokens(tokens) = tokenize(input, &ParseOptions::default());

    tokens
}

fn concrete_exprs(
    input: &str,
    opts: ParseOptions,
) -> Vec<Node<BorrowedTokenInput>> {
    let mut session = ParserSession::new(input.as_bytes(), &opts);

    let ParseResult { nodes, .. } = session.concrete_parse_expressions();

    let NodeSeq(nodes) = nodes;

    nodes
}

fn concrete_exprs_character_index(
    input: &str,
) -> Vec<Node<BorrowedTokenInput>> {
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
fn test_something() {
    assert_eq!(
        tokens("123"),
        vec![token![Integer, "123" @ 0, src!(1:1-1:4)]]
    );

    assert_eq!(
        nodes("foo"),
        vec![NVToken(token![Symbol, "foo" @ 0, src!(1:1-1:4)])]
    );

    assert_eq!(
        tokens("a+b"),
        vec![
            token![Symbol, "a" @ 0, src!(1:1-1:2)],
            token![Plus, "+" @ 1, src!(1:2-1:3)],
            token![Symbol, "b" @ 2, src!(1:3-1:4)],
        ]
    );

    assert_eq!(
        tokens("!a"),
        vec![
            token![Bang, "!" @ 0, src!(1:1-1:2)],
            token![Symbol, "a" @ 1, src!(1:2-1:3)]
        ]
    );

    assert_eq!(
        tokens("2 + 2"),
        vec![
            token![Integer, "2" @ 0, src!(1:1-1:2)],
            token![Whitespace, " " @ 1, src!(1:2-1:3)],
            token![Plus, "+" @ 2, src!(1:3-1:4)],
            token![Whitespace, " " @ 3, src!(1:4-1:5)],
            token![Integer, "2" @ 4, src!(1:5-1:6)],
        ]
    );

    assert_eq!(
        nodes("2 + 2"),
        vec![Node::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                NVToken(token![Integer, "2" @ 0, src!(1:1-1:2)]),
                NVToken(token![Whitespace, " " @ 1, src!(1:2-1:3)]),
                NVToken(token![Plus, "+" @ 2, src!(1:3-1:4)]),
                NVToken(token![Whitespace, " " @ 3, src!(1:4-1:5)]),
                NVToken(token![Integer, "2" @ 4, src!(1:5-1:6)]),
            ],),
            src: src!(1:1-1:6).into(),
        }))]
    );

    assert_eq!(
        nodes("f[x]"),
        vec![Node::Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Node::Token(token![
                Symbol,
                "f" @ 0,
                src!(1:1-1:2)
            ])])),
            body: CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    NVToken(token![OpenSquare, "[" @ 1, src!(1:2-1:3)]),
                    NVToken(token![Symbol, "x" @ 2, src!(1:3-1:4)]),
                    NVToken(token![CloseSquare, "]" @ 3, src!(1:4-1:5)]),
                ]),
                src: src!(1:2-1:5).into(),
            })),
            src: src!(1:1-1:5).into(),
        })]
    );
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
            token![Integer, "2" @ 0, src!(1-2)],
            token![Plus, "+" @ 1, src!(2-3)],
            token![Integer, "2" @ 2, src!(3-4)]
        ]
    );

    // Test that ParserSession::tokenize() is NOT idempotent.
    assert_eq!(
        session.tokenize().unwrap().0,
        Vec::<Token<BorrowedTokenInput>>::new()
    )
}

#[test]
fn test_character_index_source() {
    assert_eq!(
        concrete_exprs_character_index("2+2"),
        &[Node::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Plus,
            children: NodeSeq(vec![
                NVToken(token![Integer, "2" @ 0, src!(1..2)]),
                NVToken(token![Plus, "+" @ 1, src!(2..3)]),
                NVToken(token![Integer, "2" @ 2, src!(3..4)])
            ]),
            src: src!(1 - 4).into(),
        }))]
    );
}

#[test]
fn test_unterminated_group_reparse() {
    assert_eq!(
        concrete_exprs("{", ParseOptions::default()),
        &[Node::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: GroupOperator::List,
                children: NodeSeq(vec![Node::Token(token![
                    OpenCurly,
                    [123] @ 0,
                    src!(1:1-1:2)
                ])]),
                src: src!(1:1-1:2).into(),
            },
        ),)]
    );

    // assert_eq!(concrete_exprs("f["), &[]);

    assert_eq!(
        concrete_exprs("\"\n", ParseOptions::default()),
        &[Node::Token(token![
            Error_UnterminatedString,
            "\"" @ 0,
            src!(1:1-1:2)
        ])]
    );


    assert_eq!(
        concrete_exprs_character_index("\"\n"),
        &[NVToken(token![
            Error_UnterminatedString,
            "\"\n" @ 0,
            src!(1..3)
        ])]
    );

    assert_eq!(
        // <| ?
        // 123456
        //   ^ \t
        concrete_exprs("<|\t?", ParseOptions::default().tab_width(1)),
        &[Node::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: GroupOperator::Association,
                children: NodeSeq(vec![
                    NVToken(token![LessBar, "<|" @ 0, src!(1:1-1:3)]),
                    NVToken(token![Whitespace, "\t" @ 2, src!(1:3-1:4)]),
                    NVToken(
                        token![Error_ExpectedOperand, "" @ 3, src!(1:4-1:4)]
                    ),
                    NVToken(token![Question, "?" @ 3, src!(1:4-1:5)]),
                    NVToken(
                        token![Error_ExpectedOperand, "" @ 4, src!(1:5-1:5)]
                    ),
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
        &[Node::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: GroupOperator::Association,
                children: NodeSeq(vec![
                    NVToken(token![LessBar, "<|" @ 0, src!(1:1-1:3)]),
                    NVToken(token![Whitespace, "\t" @ 2, src!(1:3-1:5)]),
                    NVToken(
                        token![Error_ExpectedOperand, "" @ 3, src!(1:5-1:5)]
                    ),
                    NVToken(token![Question, "?" @ 3, src!(1:5-1:6)]),
                    NVToken(
                        token![Error_ExpectedOperand, "" @ 4, src!(1:6-1:6)]
                    )
                ]),
                src: src!(1:1-1:6).into()
            }
        ))]
    );
}

#[test]
fn test_invalid_utf8_in_middle_of_parse() {
    // This tests what the parse result is where there are invalid UTF-8 bytes
    // in the input followed by valid UTF-8 bytes.
    //
    // The presence of invalid UTF-8 shouldn't halt the parsing process.
    let result = parse_bytes_to_cst(
        &[b'1', b'+', 0xE2, 0x9A, b'1'],
        &ParseOptions::default(),
    );

    assert_eq!(
        result.nodes.0,
        &[CstNode::Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                CstNode::Infix(InfixNode(OperatorNode {
                    op: InfixOperator::Plus,
                    children: NodeSeq(vec![
                        NVToken(token![Integer, "1" @ 0, src!(1:1-1:2)]),
                        NVToken(token![Plus, "+" @ 1, src!(1:2-1:3)]),
                        NVToken(
                            token![Error_UnsafeCharacterEncoding, [0xE2, 0x9A] @ 2, src!(1:3-1:4)]
                        )
                    ]),
                    src: src!(1:1-1:4).into()
                })),
                NVToken(token!(Fake_ImplicitTimes, "" @ 4, src!(1:4-1:4))),
                NVToken(token!(Integer, "1" @ 4, src!(1:4-1:5)))
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
            token![Integer, "1" @ 0, src!(1:1-1:2)],
            token![Plus, "+" @ 1, src!(1:2-1:3)],
            token![Integer, "2" @ 2, src!(1:3-1:4)],
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
            token![Integer, "1" @ 21, src!(2:1-2:2)],
            token![Plus, "+" @ 22, src!(2:2-2:3)],
            token![Integer, "2" @ 23, src!(2:3-2:4)],
        ])
    );
}

//==========================================================
// Abstract
//==========================================================

#[test]
fn test_abstract_parse() {
    let result = crate::parse_to_ast("2 + 2", &ParseOptions::default());

    assert_eq!(
        result.nodes(),
        &[AstNode::Call {
            head: Box::new(AstNode::Leaf {
                kind: TK::Symbol,
                input: OwnedTokenInput::fake("Plus"),
                data: AstMetadata::empty()
            }),
            args: vec![
                AstNode::Leaf {
                    kind: TK::Integer,
                    input: OwnedTokenInput::fake("2"),
                    data: AstMetadata {
                        source: Source::Span(src!(1:1-1:2).into()),
                        issues: vec![],
                    },
                },
                AstNode::Leaf {
                    kind: TK::Integer,
                    input: OwnedTokenInput::fake("2"),
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
