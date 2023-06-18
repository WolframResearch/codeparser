use crate::{
    node::Node, parselet::prefix_parselet, parser::Parser_topNode, source::TOPLEVEL,
    tokenizer::Tokenizer_currentToken, EncodingMode, FirstLineBehavior, ParserSession,
    QuirkSettings, SourceConvention, DEFAULT_TAB_WIDTH,
};


#[test]
fn ParseletTest_Bug1() {
    let strIn = "a /: b := c";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    prefix_parselet(Tok.tok).parse_prefix(&mut session, Tok);

    let P: &mut Node<_> = Parser_topNode(&mut session);

    assert!(matches!(P, Node::Ternary(_)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn ParseletTest_Bug2() {
    //
    let strIn = "a<b ";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    prefix_parselet(Tok.tok).parse_prefix(&mut session, Tok);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn ParseletTest_Bug3() {
    let strIn = "a\\[Integral]b\\[Integral]c ";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    prefix_parselet(Tok.tok).parse_prefix(&mut session, Tok);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn ParseletTest_Bug4() {
    let strIn = "\\[RawLeftBrace]*\\[RawRightBrace]";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    prefix_parselet(Tok.tok).parse_prefix(&mut session, Tok);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
