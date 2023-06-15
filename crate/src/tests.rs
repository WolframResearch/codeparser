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
    node::{
        CallNode, GroupMissingCloserNode, GroupNode, InfixNode, Node, Node::Token as NVToken,
        NodeSeq, Operator, OperatorNode, UnterminatedGroupNode,
    },
    parser_session::ParserSession,
    source::SourceConvention,
    src, token,
    token::{BorrowedTokenInput, Token},
    EncodingMode, FirstLineBehavior, ParseOptions, ParseResult, Tokens, DEFAULT_TAB_WIDTH,
};

fn nodes(input: &str) -> Vec<Node<BorrowedTokenInput>> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::LineColumn,
        4,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let result = session.concrete_parse_expressions();

    let NodeSeq(nodes) = NodeSeq(result.nodes().to_vec());

    nodes
}

fn tokens(input: &str) -> Vec<Token<BorrowedTokenInput>> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::LineColumn,
        4,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let tokens: Tokens<BorrowedTokenInput> = session.tokenize().unwrap();

    let Tokens(tokens) = tokens;

    tokens
}

fn concrete_exprs(input: &str, opts: ParseOptions) -> Vec<Node<BorrowedTokenInput>> {
    let ParseOptions {
        first_line_behavior,
        src_convention,
        encoding_mode,
        tab_width,
    } = opts;

    let mut session = ParserSession::new(
        input.as_bytes(),
        src_convention,
        tab_width,
        first_line_behavior,
        encoding_mode,
    );

    let ParseResult { nodes, .. } = session.concrete_parse_expressions();

    let NodeSeq(nodes) = nodes;

    nodes
}

fn concrete_exprs_character_index(input: &str) -> Vec<Node<BorrowedTokenInput>> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::CharacterIndex,
        4,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
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
            op: Operator::Plus,
            children: NodeSeq(vec![
                NVToken(token![Integer, "2" @ 0, src!(1:1-1:2)]),
                NVToken(token![Whitespace, " " @ 1, src!(1:2-1:3)]),
                NVToken(token![Plus, "+" @ 2, src!(1:3-1:4)]),
                NVToken(token![Whitespace, " " @ 3, src!(1:4-1:5)]),
                NVToken(token![Integer, "2" @ 4, src!(1:5-1:6)]),
            ],),
            src: src!(1:1-1:6),
        }))]
    );

    assert_eq!(
        nodes("f[x]"),
        vec![Node::Call(CallNode {
            head: NodeSeq(vec![Node::Token(token![
                Symbol,
                "f" @ 0,
                src!(1:1-1:2)
            ])]),
            body: Box::new(Node::Group(GroupNode(OperatorNode {
                op: Operator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    NVToken(token![OpenSquare, "[" @ 1, src!(1:2-1:3)]),
                    NVToken(token![Symbol, "x" @ 2, src!(1:3-1:4)]),
                    NVToken(token![CloseSquare, "]" @ 3, src!(1:4-1:5)]),
                ]),
                src: src!(1:2-1:5),
            }))),
            src: src!(1:1-1:5),
        })]
    );
}

#[test]
pub fn test_tokenize_is_not_idempotent() {
    let mut session = ParserSession::new(
        "2+2".as_bytes(),
        SourceConvention::CharacterIndex,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    assert_eq!(
        session.tokenize().unwrap().0,
        vec![
            token![Integer, "2" @ 0, src!(0:1-0:2)],
            token![Plus, "+" @ 1, src!(0:2-0:3)],
            token![Integer, "2" @ 2, src!(0:3-0:4)]
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
            op: Operator::Plus,
            children: NodeSeq(vec![
                NVToken(token![Integer, "2" @ 0, src!(1..2)]),
                NVToken(token![Plus, "+" @ 1, src!(2..3)]),
                NVToken(token![Integer, "2" @ 2, src!(3..4)])
            ]),
            src: src!(0:1-0:4),
        }))]
    );
}

#[test]
fn test_unterminated_group_reparse() {
    assert_eq!(
        concrete_exprs("{", ParseOptions::default()),
        &[Node::GroupMissingCloser(GroupMissingCloserNode(
            OperatorNode {
                op: Operator::List,
                children: NodeSeq(vec![Node::Token(token![
                    OpenCurly,
                    [123] @ 0,
                    src!(1:1-1:2)
                ])]),
                src: src!(1:1-1:2),
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
                op: Operator::Association,
                children: NodeSeq(vec![
                    NVToken(token![LessBar, "<|" @ 0, src!(1:1-1:3)]),
                    NVToken(token![Whitespace, "\t" @ 2, src!(1:3-1:4)]),
                    NVToken(token![Error_ExpectedOperand, "" @ 3, src!(1:4-1:4)]),
                    NVToken(token![Question, "?" @ 3, src!(1:4-1:5)]),
                    NVToken(token![Error_ExpectedOperand, "" @ 4, src!(1:5-1:5)]),
                ]),
                src: src!(1:1-1:5),
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
                op: Operator::Association,
                children: NodeSeq(vec![
                    NVToken(token![LessBar, "<|" @ 0, src!(1:1-1:3)]),
                    NVToken(token![Whitespace, "\t" @ 2, src!(1:3-1:5)]),
                    NVToken(token![Error_ExpectedOperand, "" @ 3, src!(1:5-1:5)]),
                    NVToken(token![Question, "?" @ 3, src!(1:5-1:6)]),
                    NVToken(token![Error_ExpectedOperand, "" @ 4, src!(1:6-1:6)])
                ]),
                src: src!(1:1-1:6)
            }
        ))]
    );
}
