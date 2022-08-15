use crate::{
    source::{ByteSpan, NextPolicyBits::RETURN_TOPLEVELNEWLINE, SourceLocation, TOPLEVEL},
    token::Token,
    token_enum::TokenEnum::*,
    tokenizer::{Tokenizer_currentToken, Tokenizer_nextToken},
    EncodingMode, FirstLineBehavior, ParserSession, SourceConvention, DEFAULT_TAB_WIDTH,
};


//
// This was asserting
//
#[test]
fn TokenizerTest_Bug1() {
    let strIn = "\\.GG";

    let session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// This used to assert
//
#[test]
fn TokenizerTest_Bug2() {
    let strIn = "<<<";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

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

    let session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Bug4() {
    let strIn = "\\[";

    let session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Bug5() {
    let strIn = "\"a\\\\\r\nb\"";

    let session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_IntegerRealMixup() {
    let strIn = "0..";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok1,
        Token::new3(TOKEN_INTEGER, ByteSpan::new(0, 1), src!(1:1-1:2))
    );

    Tok1.skip(&mut session.tokenizer);

    let Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok2,
        Token::new3(TOKEN_DOTDOT, ByteSpan::new(1, 2), src!(1:2-1:4))
    );

    Tok2.skip(&mut session.tokenizer);

    let Tok3 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok3,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(3, 0), src!(1:4-1:4))
    );

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic2() {
    let strIn = "\\[Alpha]bc+1";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok1,
        Token::new3(TOKEN_SYMBOL, ByteSpan::new(0, 10), src!(1:1-1:11))
    );

    Tok1.skip(&mut session.tokenizer);

    let Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok2,
        Token::new3(TOKEN_PLUS, ByteSpan::new(10, 1), src!(1:11-1:12))
    );

    Tok2.skip(&mut session.tokenizer);

    let Tok3 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok3,
        Token::new3(TOKEN_INTEGER, ByteSpan::new(11, 1), src!(1:12-1:13))
    );

    Tok3.skip(&mut session.tokenizer);

    let Tok4 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok4,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(12, 0), src!(1:13-1:13))
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_OldAssert1() {
    let strIn = "8*";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_INTEGER, ByteSpan::new(0, 1), src!(1:1-1:2))
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic3() {
    let strIn = "{\n}";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_OPENCURLY, ByteSpan::new(0, 1), src!(1:1-1:2))
    );

    Tok.skip(&mut session.tokenizer);

    //
    // Clear 0x100 because we are inside a group now
    //
    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL & !(RETURN_TOPLEVELNEWLINE));

    assert_eq!(
        Tok,
        Token::new3(TOKEN_INTERNALNEWLINE, ByteSpan::new(1, 1), src!(1:2-2:1))
    );

    Tok.skip(&mut session.tokenizer);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_CLOSECURLY, ByteSpan::new(2, 1), src!(2:1-2:2))
    );

    Tok.skip(&mut session.tokenizer);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_Basic4() {
    let arr = &[0xff];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    assert_eq!(session.tokenizer.SrcLoc, SourceLocation::new(1, 1));

    assert_eq!(session.tokenizer.wasEOF, false);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(
            TOKEN_ERROR_UNSAFECHARACTERENCODING,
            ByteSpan::new(0, 1),
            src!(1:1-1:2)
        )
    );

    assert_eq!(session.tokenizer.SrcLoc, SourceLocation::new(1, 1));

    assert_eq!(session.tokenizer.wasEOF, false);

    Tok.skip(&mut session.tokenizer);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(1, 0), src!(1:2-1:2))
    );

    Tok.skip(&mut session.tokenizer);

    assert_eq!(session.tokenizer.SrcLoc, SourceLocation::new(1, 2));

    assert_eq!(session.tokenizer.wasEOF, true);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

#[test]
fn TokenizerTest_Crash1() {
    let arr = &[b'6', b'`', b'5', b'.', b'.'];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation1() {
    let strIn = "ab\\\ncd";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_SYMBOL, ByteSpan::new(0, 6), src!(1:1-2:3))
    );

    Tokenizer_nextToken(&mut session.tokenizer, TOPLEVEL);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(6, 0), src!(2:3-2:3))
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation2() {
    let strIn = "ab\\\r\ncd";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_SYMBOL, ByteSpan::new(0, 7), src!(1:1-2:3))
    );

    Tokenizer_nextToken(&mut session.tokenizer, TOPLEVEL);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(7, 0), src!(2:3-2:3))
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation3() {
    let strIn = "ab\\\rcd";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_SYMBOL, ByteSpan::new(0, 6), src!(1:1-2:3))
    );

    Tokenizer_nextToken(&mut session.tokenizer, TOPLEVEL);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(6, 0), src!(2:3-2:3))
    );

    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation4() {
    let strIn = "1\\\n";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_INTEGER, ByteSpan::new(0, 1), src!(1:1-1:2))
    );

    Tokenizer_nextToken(&mut session.tokenizer, TOPLEVEL);

    Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(
        Tok,
        Token::new3(TOKEN_ENDOFFILE, ByteSpan::new(1, 2), src!(1:2-2:1))
    );

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}
