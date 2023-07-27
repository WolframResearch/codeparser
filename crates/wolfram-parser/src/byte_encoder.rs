use std::fmt::{self, Write};

use crate::read::code_point::CodePoint::{self, Char, *};

pub(crate) fn encodeBytes(
    stream: &mut fmt::Formatter,
    val: CodePoint,
) -> Result<(), fmt::Error> {
    match val {
        Char(c) => stream.write_char(c),
        CodePoint::CRLF => {
            stream.write_char('\r')?;
            stream.write_char('\n')?;

            return Ok(());
        },
        //
        // e.g., GTest was trying to print
        //
        Unsafe1ByteUtf8Sequence
        | Unsafe2ByteUtf8Sequence
        | Unsafe3ByteUtf8Sequence => {
            //
            // Print U+FFFD (REPLACEMENT CHARACTER)
            //

            stream.write_char('\u{FFFD}')?;

            return Ok(());
        },
        _ => panic!("unable to encode special code point as UTF-8: {val:?}"),
    }
}


//======================================
// Custom Encoding Logic (Old)
//======================================

/*

//
// Encode a code point into a sequence of UTF-8 bytes
//

pub(crate) fn size(val: CodePoint) -> usize {
    if val == Special(CRLF) {
        return 2;
    }

    if val == Special(Unsafe1ByteUtf8Sequence) {
        return 1;
    }

    if val == Special(Unsafe2ByteUtf8Sequence) {
        return 2;
    }

    if val == Special(Unsafe3ByteUtf8Sequence) {
        return 3;
    }

    let val = val.as_i32();

    assert!(val >= 0);

    if val <= 0x7f {
        return 1;
    }

    if val <= 0x7ff {
        return 2;
    }

    if val <= 0xffff {
        assert!(val != CODEPOINT_BOM as i32);
        assert!(!utils::isStraySurrogate(val));

        return 3;
    }

    assert!(val <= 0x10ffff);

    return 4;
}

pub(crate) fn encodeBytes(stream: &mut fmt::Formatter, val: CodePoint) -> Result<(), fmt::Error> {
    match val {
        Special(CRLF) => {
            stream.write_char('\r')?;
            stream.write_char('\n')?;

            return Ok(());
        },
        //
        // e.g., GTest was trying to print
        //
        Special(Unsafe1ByteUtf8Sequence | Unsafe2ByteUtf8Sequence | Unsafe3ByteUtf8Sequence) => {
            //
            // Print U+FFFD (REPLACEMENT CHARACTER)
            //

            stream.write_char(0xEF);
            stream.write_char(0xBF);
            stream.write_char(0xBD);

            return Ok(());
        },
        _ => (),
    }

    let val = val.as_i32();

    assert!(val >= 0);

    if val <= 0x7f {
        //
        // 1 byte UTF-8 sequence
        //

        let firstByte = to_byte(((val >> 0) & 0x7f) | 0x00);

        stream.write_char(firstByte);

        return;
    }

    if val <= 0x7ff {
        //
        // 2 byte UTF-8 sequence
        //

        let firstByte = to_byte(((val >> 6) & 0x1f) | 0xc0);
        let secondByte = to_byte(((val >> 0) & 0x3f) | 0x80);

        stream.write_char(firstByte);
        stream.write_char(secondByte);

        return;
    }

    if val <= 0xffff {
        //
        // 3 byte UTF-8 sequence
        //

        assert!(val != CODEPOINT_BOM as i32);
        assert!(!utils::isStraySurrogate(val));

        let firstByte = to_byte(((val >> 12) & 0x0f) | 0xe0);
        let secondByte = to_byte(((val >> 6) & 0x3f) | 0x80);
        let thirdByte = to_byte(((val >> 0) & 0x3f) | 0x80);

        stream.write_char(firstByte);
        stream.write_char(secondByte);
        stream.write_char(thirdByte);

        return;
    }

    //
    // 4 byte UTF-8 sequence
    //

    assert!(val <= 0x10ffff);

    let firstByte = to_byte(((val >> 18) & 0x07) | 0xf0);
    let secondByte = to_byte(((val >> 12) & 0x3f) | 0x80);
    let thirdByte = to_byte(((val >> 6) & 0x3f) | 0x80);
    let fourthByte = to_byte(((val >> 0) & 0x3f) | 0x80);

    stream.write_char(firstByte);
    stream.write_char(secondByte);
    stream.write_char(thirdByte);
    stream.write_char(fourthByte);
}

fn encodeBytes_to_array(arr: [u8; 4], val: CodePoint) {
    match val {
        Special(CRLF) => {
            arr[0] = '\r';
            arr[1] = '\n';

            return;
        },
        //
        // e.g., GTest was trying to print
        //
        Special(Unsafe1ByteUtf8Sequence | Unsafe2ByteUtf8Sequence | Unsafe3ByteUtf8Sequence) => {
            //
            // Print U+FFFD (REPLACEMENT CHARACTER)
            //

            arr[0] = 0xEF;
            arr[1] = 0xBF;
            arr[2] = 0xBD;

            return;
        },
        _ => (),
    }

    let val = val.as_i32();

    assert!(val >= 0);

    if val <= 0x7f {
        //
        // 1 byte UTF-8 sequence
        //

        let firstByte = to_byte(((val >> 0) & 0x7f) | 0x00);

        arr[0] = firstByte;

        return;
    }

    if val <= 0x7ff {
        //
        // 2 byte UTF-8 sequence
        //

        let firstByte = to_byte(((val >> 6) & 0x1f) | 0xc0);
        let secondByte = to_byte(((val >> 0) & 0x3f) | 0x80);

        arr[0] = firstByte;
        arr[1] = secondByte;

        return;
    }

    if val <= 0xffff {
        //
        // 3 byte UTF-8 sequence
        //

        assert!(val != CODEPOINT_BOM as i32);
        assert!(!utils::isStraySurrogate(val));

        let firstByte = to_byte(((val >> 12) & 0x0f) | 0xe0);
        let secondByte = to_byte(((val >> 6) & 0x3f) | 0x80);
        let thirdByte = to_byte(((val >> 0) & 0x3f) | 0x80);

        arr[0] = firstByte;
        arr[1] = secondByte;
        arr[2] = thirdByte;

        return;
    }

    //
    // 4 byte UTF-8 sequence
    //

    assert!(val <= 0x10ffff);

    let firstByte = to_byte(((val >> 18) & 0x07) | 0xf0);
    let secondByte = to_byte(((val >> 12) & 0x3f) | 0x80);
    let thirdByte = to_byte(((val >> 6) & 0x3f) | 0x80);
    let fourthByte = to_byte(((val >> 0) & 0x3f) | 0x80);

    arr[0] = firstByte;
    arr[1] = secondByte;
    arr[2] = thirdByte;
    arr[3] = fourthByte;
}

fn to_byte(val: i32) -> u8 {
    u8::try_from(val).expect("unable to convert encoded i32 to u8")
}
*/
