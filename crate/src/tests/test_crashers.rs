use crate::{
    source::{ByteSpan, TOPLEVEL},
    src,
    token::Token,
    token_enum::TokenEnum::*,
    tokenizer::{Tokenizer_currentToken, Tokenizer_nextToken},
    EncodingMode, FirstLineBehavior, ParserSession, SourceConvention, DEFAULT_TAB_WIDTH,
};


#[test]
fn CrashTest_Crash0_tokens() {
    let bufAndLen: &[u8] = &[b'1', b'\\', b'\n'];

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let policy = TOPLEVEL;

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, policy);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_INTEGER, ByteSpan::new(0, 1), src!(1:1-1:2))
    );

    Tokenizer_nextToken(&mut session.tokenizer, policy);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, policy);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(1, 2), src!(1:2-2:1))
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash1() {
    let bufAndLen = &[b'1', b':', b':', b'*', b'\\', b'\r', b'\n'];

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

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

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);

    TheParserSession->deinit();
}
#endif // #if 0
*/

#[test]
fn CrashTest_Crash2() {
    let arr: &[u8] = &[b'\\', b':', b'f', b'e', b'f', b'f'];

    let bufAndLen = arr;

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash3() {
    let arr: &[u8] = &[b'a', b':', b'b', b'~', b'1', b':', b'2'];

    let bufAndLen = arr;

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash4() {
    let arr: &[u8] = &[
        b'\\', b'[', b'I', b'n', b't', b'e', b'g', b'r', b'a', b'l', b']', b'\\', b'[', b'S', b'u',
        b'm', b']',
    ];

    let bufAndLen = arr;

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CrashTest_Crash5() {
    let arr: &[u8] = &[b'{', b'\t', b'1', b'\\', b'\n', b'^'];

    let bufAndLen = arr;

    let mut session = ParserSession::new(
        bufAndLen,
        SourceConvention::CharacterIndex,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let _ = session.concrete_parse_expressions();

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
