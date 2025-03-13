use crate::{
    node::{CompoundNode, Node, NodeSeq},
    source::{ByteSpan, SourceLocation},
    src,
    symbol_registration::*,
    token::{Token, TokenKind},
    EncodingMode, FirstLineBehavior, ParserSession, SourceConvention, DEFAULT_TAB_WIDTH,
};


#[test]
fn NodeTest_Bug1() {
    let mut Args = NodeSeq::new();

    let input = "a_.";

    let session = ParserSession::new(
        input.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let T1 = Token::new3(TokenKind::Symbol, ByteSpan::new(0, 1), src!(1:1-1:2));
    Args.push(T1);

    let T2 = Token::new3(TokenKind::UnderDot, ByteSpan::new(1, 2), src!(1:2-1:4));
    Args.push(T2);

    let N = CompoundNode::new(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, Args);

    let NSource = Node::Compound(N).getSource();

    assert_eq!(NSource.start, SourceLocation::new(1, 1));
    assert_eq!(NSource.end, SourceLocation::new(1, 4));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
