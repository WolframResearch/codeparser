use crate::{cst::CstNode, ParseOptions, ParserSession};


#[test]
fn ParseletTest_Bug1() {
    let strIn = "a /: b := c";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let tok = session.tokenizer.peek_token();

    session.parse_prefix(tok);

    let P: &mut CstNode<_> = session.top_node();

    assert!(matches!(P, CstNode::Ternary(_)));

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

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let tok = session.tokenizer.peek_token();

    session.parse_prefix(tok);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn ParseletTest_Bug3() {
    let strIn = "a\\[Integral]b\\[Integral]c ";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let tok = session.tokenizer.peek_token();

    session.parse_prefix(tok);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn ParseletTest_Bug4() {
    let strIn = "\\[RawLeftBrace]*\\[RawRightBrace]";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let tok = session.tokenizer.peek_token();

    session.parse_prefix(tok);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
