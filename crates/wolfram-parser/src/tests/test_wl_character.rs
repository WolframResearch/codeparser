use crate::{
    generated::long_names_registration::*,
    read::{Escape, WLCharacter},
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
    let mut c =
        WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWTAB, Escape::Raw);

    assert_eq!(c.graphicalString(), "\\t");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_NEWLINE, Escape::Raw);

    assert_eq!(c.graphicalString(), "\\n");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWRETURN, Escape::Raw);

    assert_eq!(c.graphicalString(), "\\r");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWESCAPE, Escape::Raw);

    assert_eq!(c.graphicalString(), "\\[RawEscape]");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSPACE, Escape::Raw);

    assert_eq!(c.graphicalString(), " ");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWEXCLAMATION,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "!");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWDOUBLEQUOTE,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "\\\"");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWNUMBERSIGN,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "#");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDOLLAR, Escape::Raw);

    assert_eq!(c.graphicalString(), "$");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWPERCENT,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "%");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWAMPERSAND,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "&");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWQUOTE, Escape::Raw);

    assert_eq!(c.graphicalString(), "'");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWLEFTPARENTHESIS,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "(");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWRIGHTPARENTHESIS,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), ")");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSTAR, Escape::Raw);

    assert_eq!(c.graphicalString(), "*");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWPLUS, Escape::Raw);

    assert_eq!(c.graphicalString(), "+");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWCOMMA, Escape::Raw);

    assert_eq!(c.graphicalString(), ",");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDASH, Escape::Raw);

    assert_eq!(c.graphicalString(), "-");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWDOT, Escape::Raw);

    assert_eq!(c.graphicalString(), ".");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWSLASH, Escape::Raw);

    assert_eq!(c.graphicalString(), "/");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWCOLON, Escape::Raw);

    assert_eq!(c.graphicalString(), ":");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWSEMICOLON,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), ";");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWLESS, Escape::Raw);

    assert_eq!(c.graphicalString(), "<");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWEQUAL, Escape::Raw);

    assert_eq!(c.graphicalString(), "=");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWGREATER,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), ">");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWQUESTION,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "?");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWAT, Escape::Raw);

    assert_eq!(c.graphicalString(), "@");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWLEFTBRACKET,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "[");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWBACKSLASH,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "\\\\");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWRIGHTBRACKET,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "]");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWWEDGE, Escape::Raw);

    assert_eq!(c.graphicalString(), "^");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWUNDERSCORE,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "_");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWBACKQUOTE,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "`");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWLEFTBRACE,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "{");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWVERTICALBAR,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "|");

    c = WLCharacter::new_with_escape(
        CODEPOINT_LONGNAME_RAWRIGHTBRACE,
        Escape::Raw,
    );

    assert_eq!(c.graphicalString(), "}");

    c = WLCharacter::new_with_escape(CODEPOINT_LONGNAME_RAWTILDE, Escape::Raw);

    assert_eq!(c.graphicalString(), "~");
}
