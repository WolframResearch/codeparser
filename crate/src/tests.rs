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
        OperatorNode,
    },
    parser_session::ParserSession,
    source::{ByteSpan, SourceConvention},
    src,
    symbol::Symbol,
    symbol_registration::{SYMBOL_CODEPARSER_GROUPNODE, SYMBOL_CODEPARSER_GROUPSQUARE},
    token::{Token, TokenKind},
    EncodingMode, FirstLineBehavior, Source, DEFAULT_TAB_WIDTH,
};

fn nodes(input: &str) -> Vec<Node> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::LineColumn,
        4,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let result = session.concrete_parse_expressions();
    let nodes = result.nodes();

    nodes.to_owned()
}

fn tokens(input: &str) -> Vec<Node> {
    let mut session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::LineColumn,
        4,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let NodeContainer {
        nodes: NodeSeq(nodes),
    } = session.tokenize();

    nodes.clone()
}


#[test]
fn test_something() {
    assert_eq!(
        tokens("123"),
        vec![NVToken(Token {
            tok: TokenKind::Integer,
            src: src!(1:1-1:4),
            span: ByteSpan::new(0, 3)
        })]
    );

    assert_eq!(
        nodes("foo"),
        vec![NVToken(Token {
            tok: TokenKind::Symbol,
            src: src!(1:1-1:4),
            span: ByteSpan::new(0, 3)
        })]
    );

    assert_eq!(
        tokens("a+b"),
        vec![
            NVToken(Token {
                tok: TokenKind::Symbol,
                src: src!(1:1-1:2),
                span: ByteSpan::new(0, 1),
            }),
            NVToken(Token {
                tok: TokenKind::Plus,
                src: src!(1:2-1:3),
                span: ByteSpan::new(1, 1),
            }),
            NVToken(Token {
                tok: TokenKind::Symbol,
                src: src!(1:3-1:4),
                span: ByteSpan::new(2, 1),
            }),
        ]
    );

    assert_eq!(
        tokens("!a"),
        vec![
            NVToken(Token {
                tok: TokenKind::Bang,
                src: src!(1:1-1:2),
                span: ByteSpan { offset: 0, len: 1 },
            },),
            NVToken(Token {
                tok: TokenKind::Symbol,
                src: src!(1:2-1:3),
                span: ByteSpan::new(1, 1)
            },),
        ]
    );

    assert_eq!(
        tokens("2 + 2"),
        vec![
            NVToken(Token {
                tok: TokenKind::Integer,
                src: src!(1:1-1:2),
                span: ByteSpan::new(0, 1)
            },),
            NVToken(Token {
                tok: TokenKind::Whitespace,
                src: src!(1:2-1:3),
                span: ByteSpan::new(1, 1)
            },),
            NVToken(Token {
                tok: TokenKind::Plus,
                src: src!(1:3-1:4),
                span: ByteSpan::new(2, 1)
            },),
            NVToken(Token {
                tok: TokenKind::Whitespace,
                src: src!(1:4-1:5),
                span: ByteSpan::new(3, 1)
            },),
            NVToken(Token {
                tok: TokenKind::Integer,
                src: src!(1:5-1:6),
                span: ByteSpan::new(4, 1)
            },),
        ]
    );

    assert_eq!(
        nodes("2 + 2"),
        vec![Node::Infix(InfixNode(OperatorNode {
            // Op: Symbol { name: "Plus", id: 514 },
            op: crate::symbol_registration::SYMBOL_PLUS,
            make_sym: crate::symbol_registration::SYMBOL_CODEPARSER_INFIXNODE,
            children: NodeSeq(vec![
                NVToken(Token {
                    tok: TokenKind::Integer,
                    src: src!(1:1-1:2),
                    span: ByteSpan::new(0, 1),
                },),
                NVToken(Token {
                    tok: TokenKind::Whitespace,
                    src: src!(1:2-1:3),
                    span: ByteSpan::new(1, 1)
                },),
                NVToken(Token {
                    tok: TokenKind::Plus,
                    src: src!(1:3-1:4),
                    span: ByteSpan::new(2, 1),
                },),
                NVToken(Token {
                    tok: TokenKind::Whitespace,
                    src: src!(1:4-1:5),
                    span: ByteSpan::new(3, 1),
                },),
                NVToken(Token {
                    tok: TokenKind::Integer,
                    src: src!(1:5-1:6),
                    span: ByteSpan::new(4, 1),
                },),
            ],),
            src: src!(1:1-1:6),
        }))]
    );

    assert_eq!(
        nodes("f[x]"),
        vec![Node::Call(CallNode {
            head: NodeSeq(vec![Node::Token(Token {
                tok: TokenKind::Symbol,
                src: src!(1:1-1:2),
                span: ByteSpan::new(0, 1),
            })]),
            body: Box::new(Node::Group(GroupNode(OperatorNode {
                op: SYMBOL_CODEPARSER_GROUPSQUARE,
                make_sym: SYMBOL_CODEPARSER_GROUPNODE,
                children: NodeSeq(vec![
                    NVToken(Token {
                        tok: TokenKind::OpenSquare,
                        src: src!(1:2-1:3),
                        span: ByteSpan::new(1, 1),
                    }),
                    NVToken(Token {
                        tok: TokenKind::Symbol,
                        src: src!(1:3-1:4),
                        span: ByteSpan::new(2, 1),
                    }),
                    NVToken(Token {
                        tok: TokenKind::CloseSquare,
                        src: src!(1:4-1:5),
                        span: ByteSpan::new(3, 1),
                    }),
                ]),
                src: src!(1:2-1:5),
            },))),
            src: src!(1:1-1:5),
        })]
    );
}

fn token(kind: TokenKind, src: Source, span: ByteSpan) -> Node {
    Node::Token(Token {
        tok: kind,
        src,
        span,
    })
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
            token(TokenKind::Integer, src!(0:1-0:2), ByteSpan::new(0, 1)),
            token(TokenKind::Plus, src!(0:2-0:3), ByteSpan::new(1, 1)),
            token(TokenKind::Integer, src!(0:3-0:4), ByteSpan::new(2, 1))
        ]
    );

    // Test that ParserSession::tokenize() is NOT idempotent.
    assert_eq!(session.tokenize().nodes.0, vec![])
}
