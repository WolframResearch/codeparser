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
        CallNode, GroupNode, InfixNode, Node, Node::Token as NVToken, NodeContainer, NodeSeq,
        Operator, OperatorNode,
    },
    parser_session::ParserSession,
    source::SourceConvention,
    src, token,
    token::BorrowedTokenInput,
    EncodingMode, FirstLineBehavior, DEFAULT_TAB_WIDTH,
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

// TODO: Change this to return Vec<Token>.
fn tokens(input: &str) -> Vec<Node<BorrowedTokenInput>> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::LineColumn,
        4,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let NodeContainer { nodes } = session.tokenize();

    let NodeSeq(nodes) = nodes;

    nodes
}


#[test]
fn test_something() {
    assert_eq!(
        tokens("123"),
        vec![NVToken(token![Integer, "123" @ 0, src!(1:1-1:4)])]
    );

    assert_eq!(
        nodes("foo"),
        vec![NVToken(token![Symbol, "foo" @ 0, src!(1:1-1:4)])]
    );

    assert_eq!(
        tokens("a+b"),
        vec![
            NVToken(token![Symbol, "a" @ 0, src!(1:1-1:2)]),
            NVToken(token![Plus, "+" @ 1, src!(1:2-1:3)]),
            NVToken(token![Symbol, "b" @ 2, src!(1:3-1:4)]),
        ]
    );

    assert_eq!(
        tokens("!a"),
        vec![
            NVToken(token![Bang, "!" @ 0, src!(1:1-1:2)]),
            NVToken(token![Symbol, "a" @ 1, src!(1:2-1:3)])
        ]
    );

    assert_eq!(
        tokens("2 + 2"),
        vec![
            NVToken(token![Integer, "2" @ 0, src!(1:1-1:2)]),
            NVToken(token![Whitespace, " " @ 1, src!(1:2-1:3)]),
            NVToken(token![Plus, "+" @ 2, src!(1:3-1:4)]),
            NVToken(token![Whitespace, " " @ 3, src!(1:4-1:5)]),
            NVToken(token![Integer, "2" @ 4, src!(1:5-1:6)]),
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
        session.tokenize().nodes.0,
        vec![
            NVToken(token![Integer, "2" @ 0, src!(0:1-0:2)]),
            NVToken(token![Plus, "+" @ 1, src!(0:2-0:3)]),
            NVToken(token![Integer, "2" @ 2, src!(0:3-0:4)])
        ]
    );

    // Test that ParserSession::tokenize() is NOT idempotent.
    assert_eq!(session.tokenize().nodes.0, vec![])
}
