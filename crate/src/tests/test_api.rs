use crate::{
    EncodingMode, FirstLineBehavior, ParserSession, SourceConvention, StringifyMode,
    DEFAULT_TAB_WIDTH,
};


//
// this used to assert
//
#[test]
fn APITest_Bug1() {
    let strIn = "abc[]";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.concreteParseLeaf(StringifyMode::Normal);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to hang
//
#[test]
fn APITest_Hang1() {
    let strIn = "<<rr[R";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
#[test]
fn APITest_Crash1() {
    let strIn = "0^^";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
#[test]
fn APITest_Crash2() {
    let strIn = ".2^^0";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
#[test]
fn APITest_Crash3() {
    let strIn = "12^^a.a";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
#[test]
fn APITest_Crash4() {
    let strIn = "12..";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
#[test]
fn APITest_Crash5() {
    let strIn = "123\\\n.45";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
#[test]
fn APITest_Crash6() {
    let strIn = "\\0560";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// this used to crash
//
// NOTE: This test was part of the C++ version, but is not possible in the
//       Rust version of CodeParser, which does not allow invalid values
//       for the SourceConvention.
#[test]
fn APITest_Crash7() {
    //let strIn = "1+1";

    //
    // this was originally using SOURCECONVENTION_UNKNOWN, which was 0
    // but 0 is now SourceConvention::LineColumn
    // so make up a bogus SourceConvention of 2
    //

    //let mut session = ParserSession::new(strIn.as_bytes(),static_cast<SourceConvention>(2), DEFAULT_TAB_WIDTH, FirstLineBehavior::NotScript, EncodingMode::Normal);
    //assert_eq!(res, PARSERSESSIONINIT_ERROR);
}

//
// this used to crash
//
// CODETOOLS-62
//
#[test]
fn APITest_Crash8() {
    const arr: &[u8] = &[b'(', b'*', b'\r', b'\n', b'*', b')'];

    let bufAndLen = arr;

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::CharacterIndex,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.parseExpressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
