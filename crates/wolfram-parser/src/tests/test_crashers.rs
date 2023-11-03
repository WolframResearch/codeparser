use crate::{
    macros::{src, token},
    ParseOptions, ParserSession, SourceConvention,
};

use pretty_assertions::assert_eq;


#[test]
fn CrashTest_Crash0_tokens() {
    let bufAndLen: &[u8] = b"1\\\n";

    let mut session = ParserSession::new(bufAndLen, &ParseOptions::default());

    let mut tok = session.tokenizer.peek_token();

    assert_eq!(tok, token!(Integer, "1", src!(1:1-1:2)));

    let _ = session.tokenizer.next_token();

    tok = session.tokenizer.peek_token();

    assert_eq!(tok, token!(EndOfFile, "\\\n", src!(1:2-2:1)));

    assert_eq!(session.non_fatal_issues().len(), 0);
    assert_eq!(session.fatal_issues().len(), 0);
}

#[test]
fn CrashTest_Crash1() {
    let bufAndLen = b"1::*\\\r\n";

    let mut session = ParserSession::new(bufAndLen, &ParseOptions::default());

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

/*
#if 0
#[test]
fn CrashTest_StackOverflow1() {

    unsigned char arr[1600];
    for (let i = 0; i < 1600 ; i++){
        arr[i] = '(';
    }

    let bufAndLen = BufferAndLength(arr, 1600);

    TheParserSession->init(bufAndLen, nullptr, SourceConvention::LineColumn);

    let N = TheParserSession->parseExpressions();

    TheParserSession->releaseNode(N);

    assert_eq!(session.non_fatal_issues().len(), 0);
    assert_eq!(session.fatal_issues().len(), 0);

    TheParserSession->deinit();
}
#endif // #if 0
*/

#[test]
fn CrashTest_Crash2() {
    let bufAndLen = b"\\:feff";

    let mut session = ParserSession::new(bufAndLen, &ParseOptions::default());

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash3() {
    let bufAndLen = b"a:b~1:2";

    let mut session = ParserSession::new(bufAndLen, &ParseOptions::default());

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash4() {
    let arr: &[u8] = &[
        b'\\', b'[', b'I', b'n', b't', b'e', b'g', b'r', b'a', b'l', b']',
        b'\\', b'[', b'S', b'u', b'm', b']',
    ];

    let bufAndLen = arr;

    let mut session = ParserSession::new(bufAndLen, &ParseOptions::default());

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash5() {
    let bufAndLen = b"{\t1\\\n^";

    let mut session = ParserSession::new(
        bufAndLen,
        &ParseOptions::default()
            .source_convention(SourceConvention::CharacterIndex),
    );

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
