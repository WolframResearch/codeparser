use crate::{
    macros::{src, token},
    source::{Location, NextPolicyBits::RETURN_TOPLEVELNEWLINE, TOPLEVEL},
    tests::tokens,
    ParseOptions, ParserSession, Tokens,
};

use pretty_assertions::assert_eq;


//
// This was asserting
//
#[test]
fn TokenizerTest_Bug1() {
    let strIn = "\\.GG";

    let session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn TokenizerTest_Bug2() {
    let strIn = "<<<";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

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

    let session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Bug4() {
    let strIn = "\\[";

    let session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Bug5() {
    let strIn = "\"a\\\\\r\nb\"";

    let session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_IntegerRealMixup() {
    let strIn = "0..";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok1 = session.tokenizer.peek_token();

    assert_eq!(Tok1, token!(Integer, "0", src!(1:1-1:2)));

    Tok1.skip(&mut session.tokenizer);

    let Tok2 = session.tokenizer.peek_token();

    assert_eq!(Tok2, token!(DotDot, "..", src!(1:2-1:4)));

    Tok2.skip(&mut session.tokenizer);

    let Tok3 = session.tokenizer.peek_token();

    assert_eq!(Tok3, token!(EndOfFile, "", src!(1:4-1:4)));

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic2() {
    let strIn = "\\[Alpha]bc+1";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok1 = session.tokenizer.peek_token();

    assert_eq!(Tok1, token!(Symbol, "\\[Alpha]bc", src!(1:1-1:11)));

    Tok1.skip(&mut session.tokenizer);

    let Tok2 = session.tokenizer.peek_token();

    assert_eq!(Tok2, token!(Plus, "+", src!(1:11-1:12)));

    Tok2.skip(&mut session.tokenizer);

    let Tok3 = session.tokenizer.peek_token();

    assert_eq!(Tok3, token!(Integer, "1", src!(1:12-1:13)));

    Tok3.skip(&mut session.tokenizer);

    let Tok4 = session.tokenizer.peek_token();

    assert_eq!(Tok4, token!(EndOfFile, "", src!(1:13-1:13)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_OldAssert1() {
    let strIn = "8*";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Integer, "8", src!(1:1-1:2)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic3() {
    let strIn = "{\n}";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(OpenCurly, "{", src!(1:1-1:2)));

    Tok.skip(&mut session.tokenizer);

    //
    // Clear 0x100 because we are inside a group now
    //
    Tok = session
        .tokenizer
        .peek_token_with(TOPLEVEL.without(RETURN_TOPLEVELNEWLINE));

    assert_eq!(Tok, token!(InternalNewline, "\n", src!(1:2-2:1)));

    Tok.skip(&mut session.tokenizer);

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(CloseCurly, "}", src!(2:1-2:2)));

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
        token!(Error_UnsafeCharacterEncoding, [0xff], src!(1:1-1:2))
    );

    assert_eq!(session.tokenizer.SrcLoc, Location::new(1, 1));

    assert_eq!(session.tokenizer.wasEOF, false);

    Tok.skip(&mut session.tokenizer);

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(1:2-1:2)));

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

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\ncd", src!(1:1-2:3)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(2:3-2:3)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation2() {
    let strIn = "ab\\\r\ncd";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\r\ncd", src!(1:1-2:3)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(2:3-2:3)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation3() {
    let strIn = "ab\\\rcd";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\rcd", src!(1:1-2:3)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(2:3-2:3)));

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation4() {
    let strIn = "1\\\n";

    let mut session =
        ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(Integer, "1", src!(1:1-1:2)));

    let _ = session.tokenizer.next_token();

    Tok = session.tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "\\\n", src!(1:2-2:1)));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn test_escaped_ascii_del() {
    let Tokens(tokens) =
        crate::tokenize_bytes(&[b'\\', 127], &ParseOptions::default()).unwrap();

    assert_eq!(
        tokens.as_slice(),
        [token![
            Error_UnhandledCharacter,
            [b'\\', 127],
            src!(1:1-1:3)
        ]]
    );
}

#[test]
fn test_tokenizing_escaped_chars() {
    // Test that escaped chars can be used outside of strings
    assert_eq!(
        // 0x33 is ASCII '3'
        tokens(r#"12\.33"#),
        [token![Integer, r#"12\.33"#, src!(1:1-1:7)]]
    );

    assert_eq!(
        // 0x0A is ASCII '3'
        tokens(r#"12\.0A3"#),
        [
            token![Integer, r#"12"#, src!(1:1-1:3)],
            token![ToplevelNewline, r#"\.0A"#, src!(1:3-1:7)],
            // An escaped newline doesn't increment the Location line number
            token![Integer, r#"3"#, src!(1:7-1:8)]
        ]
    );
}
