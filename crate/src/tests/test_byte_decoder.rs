use pretty_assertions::assert_eq;

use crate::{
    byte_decoder::{ByteDecoder_currentSourceCharacter, ByteDecoder_nextSourceCharacter},
    code_point::CodePoint,
    source::TOPLEVEL,
    EncodingMode, FirstLineBehavior, ParserSession, QuirkSettings, SourceConvention,
    DEFAULT_TAB_WIDTH,
};


#[test]
fn ByteDecoderTest_Basic1() {
    let strIn = "1+2";

    let mut session = ParserSession::new(
        strIn.as_bytes(),
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '2');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
#[test]
fn ByteDecoderTest_Basic2() {
    let arr = &[b'1', b'+', 206, 177];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c: CodePoint = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c.as_i32(), 0x03b1);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    //
    // Issue: Non-ASCII character: ``"α" (\[Alpha])``
    //
    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
#[test]
fn ByteDecoderTest_Basic3() {
    let arr = &[b'1', b'+', 0xE2, 0x9A, 0xA1];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c.as_i32(), 0x26A1);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    //
    // Issue: Non-ASCII character: ``"⚡" (\:26a1)``
    //
    assert_eq!(session.nonFatalIssues().len(), 1);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn ByteDecoderTest_Invalid1() {
    let arr = &[b'1', b'+', 0xf8];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::Unsafe1ByteUtf8Sequence);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
// So test with only first byte
//
#[test]
fn ByteDecoderTest_Invalid2() {
    let arr = &[b'1', b'+', 206];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::Unsafe1ByteUtf8Sequence);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
#[test]
fn ByteDecoderTest_Invalid3() {
    let arr = &[b'1', b'+', 0xE2];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    // from 0xE2 byte
    assert_eq!(c, CodePoint::Unsafe1ByteUtf8Sequence);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
#[test]
fn ByteDecoderTest_Invalid4() {
    let arr = &[b'1', b'+', 0xE2, 0x9A];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    // from 0xE2 byte
    assert_eq!(c, CodePoint::Unsafe2ByteUtf8Sequence);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

//
// Surrogates
//
#[test]
fn ByteDecoderTest_Surrogate1() {
    let arr = &[b'1', b'+', 0xed, 0xa0, 0x80];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    assert_eq!(session.tokenizer.buffer().slice, &arr[0..]);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    assert_eq!(session.tokenizer.buffer().slice, &arr[1..]);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    // from 0xED byte
    assert_eq!(c, CodePoint::Unsafe3ByteUtf8Sequence);

    assert_eq!(session.tokenizer.buffer().slice, &arr[2..]);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    // from 0xA0 byte
    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.tokenizer.buffer().slice, &arr[5..]);

    assert_eq!(session.nonFatalIssues().len(), 0);
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    assert_eq!(session.fatalIssues().len(), 1);
}

//
// Surrogates
//
#[test]
fn ByteDecoderTest_Surrogate2() {
    let arr = &[b'1', b'+', 0xed, 0xb0, 0x80];

    let mut session = ParserSession::new(
        arr,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    let mut c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '1');

    assert_eq!(session.tokenizer.buffer().slice, &arr[0..]);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    assert_eq!(c, '+');

    assert_eq!(session.tokenizer.buffer().slice, &arr[1..]);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    // from 0xED byte
    assert_eq!(c, CodePoint::Unsafe3ByteUtf8Sequence);

    assert_eq!(session.tokenizer.buffer().slice, &arr[2..]);

    ByteDecoder_nextSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(&mut session.tokenizer, TOPLEVEL);

    // from 0xB0 byte
    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(session.tokenizer.buffer().slice, &arr[5..]);

    assert_eq!(session.nonFatalIssues().len(), 0);
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    assert_eq!(session.fatalIssues().len(), 1);
}
