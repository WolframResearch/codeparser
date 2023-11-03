use crate::{
    read::Reader,
    read::{code_point::CodePoint, Escape, WLCharacter},
    source::TOPLEVEL,
    ParseOptions,
};


#[test]
fn CharacterDecoderTest_Basic1() {
    let strIn = "1+2";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('2'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_LongName() {
    let strIn = "1+\\[Alpha]";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(
        c,
        WLCharacter::new_with_escape(
            CodePoint::from_u32(0x03b1).unwrap(),
            Escape::LongName
        )
    );

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_4Hex() {
    let strIn = "1+\\:03b1";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(
        c,
        WLCharacter::new_with_escape(
            CodePoint::from_u32(0x03b1).unwrap(),
            Escape::Hex4
        )
    );

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_2Hex() {
    let strIn = "1+\\.f2";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape(242, Escape::Hex2));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_Octal() {
    let strIn = "1+\\333";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape(219, Escape::Octal));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_6Hex() {
    let strIn = "1+\\|0000f2";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape(242, Escape::Hex6));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_Raw() {
    let strIn = "1+\\[RawWedge]";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape('^', Escape::Raw));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 0);
}

#[test]
fn CharacterDecoderTest_LongNameError1() {
    let strIn = "1+\\[Alpha+2";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('\\'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('['));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('A'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('l'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('p'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('h'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('a'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('2'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

#[test]
fn CharacterDecoderTest_LongNameError2() {
    let strIn = "1+\\[Alpa]+2";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('\\'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('['));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('A'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('l'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('p'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('a'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(']'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('2'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

#[test]
fn CharacterDecoderTest_4HexError1() {
    let strIn = "1+\\:03b+1";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('\\'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(':'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('0'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('3'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('b'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.non_fatal_issues.len(), 0);
    assert_eq!(reader.fatal_issues.len(), 1);
}

//
// There was a bug where UnexpectedEscapeSequence issues were being added by mistake
//
#[test]
fn CharacterDecoderTest_UnexpectedEscapeSequence() {
    let strIn = "\"\\[Alpha]\"";

    let mut reader = Reader::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('"'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(
        c,
        WLCharacter::new_with_escape(
            CodePoint::from_u32(0x03b1).unwrap(),
            Escape::LongName
        )
    );

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('"'));

    reader.next_wolfram_char(TOPLEVEL);

    c = reader.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(reader.fatal_issues.len(), 0);
    assert_eq!(reader.non_fatal_issues.len(), 0);
}
