use crate::{cst::Cst, ParseOptions, ParserSession};


#[test]
fn ParseletTest_Bug1() {
    let strIn = "a /: b := c";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let tok = session.tokenizer.peek_token();

    session.parse_prefix(tok);

    let P: &mut Cst<_> = session.builder.top_node();

    assert!(matches!(P, Cst::Ternary(_)));

    assert_eq!(session.tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(session.tokenizer.fatal_issues.len(), 0);
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

    assert_eq!(session.tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(session.tokenizer.fatal_issues.len(), 0);
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

    assert_eq!(session.tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(session.tokenizer.fatal_issues.len(), 0);
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

    assert_eq!(session.tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(session.tokenizer.fatal_issues.len(), 0);
}
