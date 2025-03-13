use crate::{
    code_point::CodePoint,
    source::TOPLEVEL,
    wl_character::{EscapeStyle, WLCharacter},
    ParseOptions, ParserSession,
};


#[test]
fn CharacterDecoderTest_Basic1() {
    let strIn = "1+2";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('2'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_LongName() {
    let strIn = "1+\\[Alpha]";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(
        c,
        WLCharacter::new_with_escape(CodePoint::from_u32(0x03b1).unwrap(), EscapeStyle::LongName)
    );

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_4Hex() {
    let strIn = "1+\\:03b1";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(
        c,
        WLCharacter::new_with_escape(CodePoint::from_u32(0x03b1).unwrap(), EscapeStyle::Hex4)
    );

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_2Hex() {
    let strIn = "1+\\.f2";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape(242, EscapeStyle::Hex2));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_Octal() {
    let strIn = "1+\\333";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape(219, EscapeStyle::Octal));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_6Hex() {
    let strIn = "1+\\|0000f2";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape(242, EscapeStyle::Hex6));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_Raw() {
    let strIn = "1+\\[RawWedge]";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new_with_escape('^', EscapeStyle::Raw));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn CharacterDecoderTest_LongNameError1() {
    let strIn = "1+\\[Alpha+2";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('\\'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('['));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('A'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('l'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('p'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('h'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('a'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('2'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

#[test]
fn CharacterDecoderTest_LongNameError2() {
    let strIn = "1+\\[Alpa]+2";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('\\'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('['));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('A'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('l'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('p'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('a'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(']'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('2'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

#[test]
fn CharacterDecoderTest_4HexError1() {
    let strIn = "1+\\:03b+1";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('\\'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(':'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('0'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('3'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('b'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('+'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('1'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 1);
}

//
// There was a bug where UnexpectedEscapeSequence issues were being added by mistake
//
#[test]
fn CharacterDecoderTest_UnexpectedEscapeSequence() {
    let strIn = "\"\\[Alpha]\"";

    let mut session = ParserSession::new(strIn.as_bytes(), &ParseOptions::default());

    let mut c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('"'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(
        c,
        WLCharacter::new_with_escape(CodePoint::from_u32(0x03b1).unwrap(), EscapeStyle::LongName)
    );

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new('"'));

    session.tokenizer.next_wolfram_char(TOPLEVEL);

    c = session.tokenizer.peek_wolfram_char(TOPLEVEL);

    assert_eq!(c, WLCharacter::new(CodePoint::EndOfFile));

    assert_eq!(session.fatalIssues().len(), 0);
    assert_eq!(session.nonFatalIssues().len(), 0);
}
