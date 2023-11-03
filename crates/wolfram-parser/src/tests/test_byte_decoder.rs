use pretty_assertions::assert_eq;

use crate::{
    read::code_point::CodePoint, read::Reader, source::TOPLEVEL, ParseOptions,
};

#[test]
fn ByteDecoderTest_Empty() {
    let strIn = "";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}


#[test]
fn ByteDecoderTest_Basic1() {
    let strIn = "1+2";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '2');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
#[test]
fn ByteDecoderTest_Basic2() {
    let arr = &[b'1', b'+', 206, 177];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c: CodePoint = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c.as_i32(), 0x03b1);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    //
    // Issue: Non-ASCII character: ``"α" (\[Alpha])``
    //
    assert_eq!(reader.non_fatal_issues.len(), 1);
    assert_eq!(reader.fatal_issues.len(), 0);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
#[test]
fn ByteDecoderTest_Basic3() {
    let arr = &[b'1', b'+', 0xE2, 0x9A, 0xA1];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c.as_i32(), 0x26A1);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    //
    // Issue: Non-ASCII character: ``"⚡" (\:26a1)``
    //
    assert_eq!(reader.non_fatal_issues.len(), 1);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn ByteDecoderTest_Invalid1() {
    let arr = &[b'1', b'+', 0xf8];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::Unsafe1ByteUtf8Sequence);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
// So test with only first byte
//
#[test]
fn ByteDecoderTest_Invalid2() {
    let arr = &[b'1', b'+', 206];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::Unsafe1ByteUtf8Sequence);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
#[test]
fn ByteDecoderTest_Invalid3() {
    let arr = &[b'1', b'+', 0xE2];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    // from 0xE2 byte
    assert_eq!(c, CodePoint::Unsafe1ByteUtf8Sequence);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
#[test]
fn ByteDecoderTest_Invalid4() {
    let arr = &[b'1', b'+', 0xE2, 0x9A];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    // from 0xE2 byte
    assert_eq!(c, CodePoint::Unsafe2ByteUtf8Sequence);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

//
// Surrogates
//
#[test]
fn ByteDecoderTest_Surrogate1() {
    let arr = &[b'1', b'+', 0xed, 0xa0, 0x80];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    assert_eq!(reader.buffer(), &arr[0..]);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    assert_eq!(reader.buffer(), &arr[1..]);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    // from 0xED byte
    assert_eq!(c, CodePoint::Unsafe3ByteUtf8Sequence);

    assert_eq!(reader.buffer(), &arr[2..]);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    // from 0xA0 byte
    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.buffer(), &arr[5..]);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    assert_eq!(reader.fatal_issues.len(), 1);
}

//
// Surrogates
//
#[test]
fn ByteDecoderTest_Surrogate2() {
    let arr = &[b'1', b'+', 0xed, 0xb0, 0x80];

    let mut reader = Reader::new(arr, &ParseOptions::default());

    let mut c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '1');

    assert_eq!(reader.buffer(), &arr[0..]);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    assert_eq!(c, '+');

    assert_eq!(reader.buffer(), &arr[1..]);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    // from 0xED byte
    assert_eq!(c, CodePoint::Unsafe3ByteUtf8Sequence);

    assert_eq!(reader.buffer(), &arr[2..]);

    reader.next_source_char(TOPLEVEL);

    c = reader.peek_source_char(TOPLEVEL);

    // from 0xB0 byte
    assert_eq!(c, CodePoint::EndOfFile);

    assert_eq!(reader.buffer(), &arr[5..]);

    assert_eq!(reader.non_fatal_issues.len(), 0);
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    assert_eq!(reader.fatal_issues.len(), 1);
}
