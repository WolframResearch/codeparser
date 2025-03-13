use crate::{
    long_names_registration::*,
    read::wl_character::{EscapeStyle, WLCharacter},
};



#[test]
fn WLCharacterTest_Bug1() {
    let C = WLCharacter::new('\t');

    assert_eq!(C.graphicalString(), "\\t");
}

#[test]
fn WLCharacterTest_Bug2() {
    let C = WLCharacter::new(' ');

    assert_eq!(C.graphicalString(), " ");
}

#[test]
fn WLCharacterTest_RawGraphical() {
    let mut c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWTAB, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "\\t");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_NEWLINE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "\\n");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWRETURN, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "\\r");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWESCAPE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "\\[RawEscape]");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSPACE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), " ");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWEXCLAMATION, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "!");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDOUBLEQUOTE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "\\\"");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWNUMBERSIGN, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "#");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDOLLAR, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "$");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWPERCENT, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "%");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWAMPERSAND, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "&");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWQUOTE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "'");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWLEFTPARENTHESIS, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "(");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWRIGHTPARENTHESIS, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), ")");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSTAR, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "*");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWPLUS, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "+");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWCOMMA, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), ",");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDASH, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "-");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDOT, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), ".");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSLASH, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "/");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWCOLON, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), ":");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSEMICOLON, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), ";");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWLESS, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "<");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWEQUAL, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "=");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWGREATER, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), ">");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWQUESTION, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "?");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWAT, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "@");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWLEFTBRACKET, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "[");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWBACKSLASH, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "\\\\");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWRIGHTBRACKET, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "]");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWWEDGE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "^");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWUNDERSCORE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "_");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWBACKQUOTE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "`");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWLEFTBRACE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "{");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWVERTICALBAR, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "|");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWRIGHTBRACE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "}");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWTILDE, EscapeStyle::Raw);

    assert_eq!(c.graphicalString(), "~");
}
