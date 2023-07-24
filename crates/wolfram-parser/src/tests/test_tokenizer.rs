use crate::{
    macros::{src, token},
    source::{Location, NextPolicyBits::RETURN_TOPLEVELNEWLINE, TOPLEVEL},
    ParseOptions, ParserSession,
};

use pretty_assertions::assert_eq;


//
// This was asserting
//
#[test]
fn TokenizerTest_Bug1() {
    let strIn = "\\.GG";

    let session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn TokenizerTest_Bug2() {
    let strIn = "<<<";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok = session.tokenizer.peek_token();

    Tok.skip(&mut session.tokenizer);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn TokenizerTest_Bug3() {
    let strIn = "\\\r";

    let session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Bug4() {
    let strIn = "\\[";

    let session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Bug5() {
    let strIn = "\"a\\\\\r\nb\"";

    let session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_IntegerRealMixup() {
    let strIn = "0..";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok1 = session.tokenizer.peek_token();

    assert_eq!(Tok1, token!(Integer, "0" @ 0, src!(1:1-1:2)));

    Tok1.skip(&mut session.tokenizer);

    let Tok2 = session.tokenizer.peek_token();

    assert_eq!(Tok2, token!(DotDot, ".." @ 1, src!(1:2-1:4)));

    Tok2.skip(&mut session.tokenizer);

    let Tok3 = session.tokenizer.peek_token();

    assert_eq!(Tok3, token!(EndOfFile, "" @ 3, src!(1:4-1:4)));

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic2() {
    let strIn = "\\[Alpha]bc+1";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok1 = session.tokenizer.peek_token();

    assert_eq!(Tok1, token!(Symbol, "\\[Alpha]bc" @ 0, src!(1:1-1:11)));

    Tok1.skip(&mut session.tokenizer);

    let Tok2 = session.tokenizer.peek_token();

    assert_eq!(Tok2, token!(Plus, "+" @ 10, src!(1:11-1:12)));

    Tok2.skip(&mut session.tokenizer);

    let Tok3 = session.tokenizer.peek_token();

    assert_eq!(Tok3, token!(Integer, "1" @ 11, src!(1:12-1:13)));

    Tok3.skip(&mut session.tokenizer);

    let Tok4 = session.tokenizer.peek_token();

    assert_eq!(Tok4, token!(EndOfFile, "" @ 12, src!(1:13-1:13)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_OldAssert1() {
    let strIn = "8*";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Integer, "8" @ 0, src!(1:1-1:2)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic3() {
    let strIn = "{\n}";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(OpenCurly, "{" @ 0, src!(1:1-1:2)));

    Tok.skip(&mut session.tokenizer);

    //
    // Clear 0x100 because we are inside a group now
    //
    Tok = session
        .tokenizer
        .peek_token_with(TOPLEVEL & !(RETURN_TOPLEVELNEWLINE));

    assert_eq!(Tok, token!(InternalNewline, "\n" @ 1, src!(1:2-2:1)));

    Tok.skip(&mut session.tokenizer);

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(CloseCurly, "}" @ 2, src!(2:1-2:2)));

    Tok.skip(&mut session.tokenizer);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic4() {
    let arr = &[0xff];

    let mut session = ParserSession::new(arr, &ParseOptions::default());

    assert_eq!(session.tokenizer.SrcLoc, Location::new(1, 1));

    assert_eq!(session.tokenizer.wasEOF, false);

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(
        Tok,
        token!(Error_UnsafeCharacterEncoding, [0xff] @ 0, src!(1:1-1:2))
    );

    assert_eq!(session.tokenizer.SrcLoc, Location::new(1, 1));

    assert_eq!(session.tokenizer.wasEOF, false);

    Tok.skip(&mut session.tokenizer);

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "" @ 1, src!(1:2-1:2)));

    Tok.skip(&mut session.tokenizer);

    assert_eq!(session.tokenizer.SrcLoc, Location::new(1, 2));

    assert_eq!(session.tokenizer.wasEOF, true);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

#[test]
fn TokenizerTest_Crash1() {
    let arr = &[b'6', b'`', b'5', b'.', b'.'];

    let mut session = ParserSession::new(arr, &ParseOptions::default());

    let _ = session.tokenizer.peek_token();

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation1() {
    let strIn = "ab\\\ncd";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\ncd" @ 0, src!(1:1-2:3)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "" @ 6, src!(2:3-2:3)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation2() {
    let strIn = "ab\\\r\ncd";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\r\ncd" @ 0, src!(1:1-2:3)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "" @ 7, src!(2:3-2:3)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation3() {
    let strIn = "ab\\\rcd";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\rcd" @ 0, src!(1:1-2:3)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "" @ 6, src!(2:3-2:3)));

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation4() {
    let strIn = "1\\\n";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Integer, "1" @ 0, src!(1:1-1:2)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "\\\n" @ 1, src!(1:2-2:1)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
