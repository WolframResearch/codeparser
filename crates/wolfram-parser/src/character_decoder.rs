use crate::{
    code_point::{CodePoint::*, *},
    feature,
    issue::{CodeAction, IssueTag, Severity, SyntaxIssue},
    long_names as LongNames,
    long_names_registration::{
        LONGNAME_TO_CODE_POINT_MAP__NAMES, LONGNAME_TO_CODE_POINT_MAP__POINTS,
    },
    read::{ByteDecoder_currentSourceCharacter, ByteDecoder_nextSourceCharacter},
    source::{
        BufferAndLength, NextPolicy,
        NextPolicyBits::{ENABLE_CHARACTER_DECODING_ISSUES, SCAN_FOR_UNRECOGNIZEDLONGNAMES},
        Source, SourceCharacter, SourceLocation, STRING_OR_COMMENT,
    },
    tokenizer::Tokenizer,
    utils,
    wl_character::{EscapeStyle, WLCharacter},
};

//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences of Source Characters such as \[Alpha] into a single WL character
//

type HandlerFunction = for<'i, 's> fn(
    session: &'s mut Tokenizer<'i>,
    startBuf: usize,
    startLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter;

macro_rules! U {
    () => {
        CharacterDecoder_handleUncommon
    };
}

macro_rules! A {
    () => {
        CharacterDecoder_handleAssertFalse
    };
}

/// Lookup table for handling ASCII byte values preceeded by a backslash.
///
/// This lookup table is equivalent to the following `match` statement:
///
/// ```ignore
/// let handler = match point_u8 {
///     00..=31 => CharacterDecoder_handleAssertFalse,
///     32 => CharacterDecoder_handleUncommon,
///     33 => CharacterDecoder_handleUncommon,
///     34 => CharacterDecoder_handleStringMetaDoubleQuote,
///     35..=59 => CharacterDecoder_handleUncommon,
///     60 => CharacterDecoder_handleStringMetaOpen,
///     61 => CharacterDecoder_handleUncommon,
///     62 => CharacterDecoder_handleStringMetaClose,
///     63..=91 => CharacterDecoder_handleUncommon,
///     92 => CharacterDecoder_handleStringMetaBackslash,
///     93..=126 => CharacterDecoder_handleUncommon,
///     127 => CharacterDecoder_handleAssertFalse,
///     128..=255 => panic!("invalid ASCII byte value: {point_u8}"),
/// };
/// ```
///
/// However, the lookup table-based implementation performs ~10-15% better than
/// the `match` statement version on some of the large benchmarks.
#[rustfmt::skip]
const CHARACTER_DECODER_HANDLER_TABLE: [HandlerFunction; 128] = [
    // 00-31: handleAssertFalse
    A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(),
    A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(), A!(),
    A!(), A!(), A!(), A!(), A!(), A!(),
    // 32-33: handleUncommon
    U!(), U!(),
    // 34:
    CharacterDecoder_handleStringMetaDoubleQuote,
    // 35-59: handleUncommon
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    // 60:
    CharacterDecoder_handleStringMetaOpen,
    // 61:
    U!(),
    // 62:
    CharacterDecoder_handleStringMetaClose,
    // 63-91:
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    U!(), U!(), U!(),
    // 92:
    CharacterDecoder_handleStringMetaBackslash,
    // 93-126: handleUncommon
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    U!(), U!(), U!(), U!(), U!(), U!(), U!(), U!(),
    // 127: handleAssertFalse
    A!(),
];

/// Precondition: buffer is pointing to current WLCharacter
/// Postcondition: buffer is pointing to next WLCharacter
///
/// Example:
/// memory: 1+\[Alpha]-2
///           ^
///           buffer
///
/// after calling nextWLCharacter:
/// memory: 1+\[Alpha]-2
///                   ^
///                   buffer
/// return \[Alpha]
///
pub(crate) fn CharacterDecoder_nextWLCharacter(
    session: &mut Tokenizer,
    policy: NextPolicy,
) -> WLCharacter {
    let mut curSource = ByteDecoder_nextSourceCharacter(session, policy);

    let mut point = curSource;

    if point != '\\' {
        incr_diagnostic!(CharacterDecoder_UnescapedCount);

        return WLCharacter::new(point);
    }

    //
    // Handle \
    //
    // handle escapes like special characters
    //

    //
    // There was a \
    //

    let escaped_offset = session.offset;
    let escapedLoc = session.SrcLoc;

    curSource = ByteDecoder_currentSourceCharacter(session, policy);

    point = curSource;

    if !(0x20 <= point.as_i32() && point.as_i32() <= 0x7e) {
        // MUSTTAIL
        return CharacterDecoder_handleUncommon(session, escaped_offset, escapedLoc, policy);
    }

    let point_u8 =
        u8::try_from(point.as_i32()).expect("unable to convert digit character to u8 value");

    return CHARACTER_DECODER_HANDLER_TABLE[usize::from(point_u8)](
        session,
        escaped_offset,
        escapedLoc,
        policy,
    );
}

#[allow(dead_code)]
pub(crate) fn CharacterDecoder_currentWLCharacter(
    session: &mut Tokenizer,
    policy: NextPolicy,
) -> WLCharacter {
    let mark = session.mark();

    let c = CharacterDecoder_nextWLCharacter(session, policy);

    session.seek(mark);

    return c;
}

fn CharacterDecoder_handleStringMetaDoubleQuote(
    session: &mut Tokenizer,
    _: usize,
    _: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaDoubleQuoteCount);

    ByteDecoder_nextSourceCharacter(session, policy);

    return WLCharacter::new_with_escape(StringMeta_DoubleQuote, EscapeStyle::Single);
}

//
// \\ \" \< \>
//
// String meta characters
// What are \< and \> ?
// https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
// https://stackoverflow.com/q/6065887
//
fn CharacterDecoder_handleStringMetaOpen(
    session: &mut Tokenizer,
    _escapedBuf: usize,
    escapedLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaOpenCount);

    ByteDecoder_nextSourceCharacter(session, policy);

    let c = WLCharacter::new_with_escape(StringMeta_Open, EscapeStyle::Single);

    if feature::CHECK_ISSUES {
        let graphicalStr = c.graphicalString();

        let currentWLCharacterStartLoc = escapedLoc.previous();

        let currentWLCharacterEndLoc = session.SrcLoc;

        let Src = Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc);

        //
        // matched reduced severity of unexpected characters inside strings or comments
        //

        let I = SyntaxIssue(
            IssueTag::UnexpectedCharacter,
            format!("Unexpected string meta character: ``{}``.", graphicalStr),
            Severity::Remark,
            Src,
            0.95,
            vec![],
            vec![format!(
                "The kernel parses ``\"{graphicalStr}\"`` as an empty string."
            )],
        );

        session.addIssue(I);
    }

    return c;
}

fn CharacterDecoder_handleStringMetaClose(
    session: &mut Tokenizer,
    _escapedBuf: usize,
    escapedLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaCloseCount);

    ByteDecoder_nextSourceCharacter(session, policy);

    let c = WLCharacter::new_with_escape(StringMeta_Close, EscapeStyle::Single);

    if feature::CHECK_ISSUES {
        let graphicalStr = c.graphicalString();

        let currentWLCharacterStartLoc = escapedLoc.previous();

        let currentWLCharacterEndLoc = session.SrcLoc;

        let Src = Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc);

        //
        // matched reduced severity of unexpected characters inside strings or comments
        //

        let I = SyntaxIssue(
            IssueTag::UnexpectedCharacter,
            format!("Unexpected string meta character: ``{graphicalStr}``."),
            Severity::Remark,
            Src,
            0.95,
            vec![],
            vec![format!(
                "The kernel parses ``\"{graphicalStr}\"`` as an empty string."
            )],
        );

        session.addIssue(I);
    }

    return c;
}

fn CharacterDecoder_handleStringMetaBackslash(
    session: &mut Tokenizer,
    _: usize,
    _: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaBackslashCount);

    ByteDecoder_nextSourceCharacter(session, policy);

    //    MUSTTAIL
    return CharacterDecoder_handleBackslash(session, policy);
}

fn CharacterDecoder_handleLongName(
    session: &mut Tokenizer,
    openSquareBuf: usize,
    openSquareLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    // assert!(openSquareBuf[0] == b'[');
    assert!(session.input[openSquareBuf] == b'[');

    incr_diagnostic!(CharacterDecoder_LongNameCount);

    //
    // Do not write leading \[ or trailing ] to LongName
    //
    let longNameStartBuf = session.buffer();

    let mut curSource = ByteDecoder_currentSourceCharacter(session, policy);

    let mut wellFormed = false;

    let mut atleast1DigitOrAlpha = false;

    //
    // Read at least 1 alnum before entering loop
    //
    // Must start with upper
    //
    if curSource.isUpper() {
        atleast1DigitOrAlpha = true;

        ByteDecoder_nextSourceCharacter(session, policy);

        curSource = ByteDecoder_currentSourceCharacter(session, policy);

        loop {
            if curSource.isAlphaOrDigit() {
                ByteDecoder_nextSourceCharacter(session, policy);

                curSource = ByteDecoder_currentSourceCharacter(session, policy);
            } else if curSource == ']' {
                wellFormed = true;

                break;
            } else {
                //
                // Something like \[A!] which is not a long name
                //

                break;
            }
        }
    } else if curSource == ']' {
        //
        // Handle \[]
        //

        ByteDecoder_nextSourceCharacter(session, policy);

        curSource = ByteDecoder_currentSourceCharacter(session, policy);
    }

    if !wellFormed {
        //
        // Not well-formed
        //

        if feature::CHECK_ISSUES
            && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
        {
            let currentWLCharacterStartLoc = openSquareLoc.previous();

            let currentWLCharacterEndBuf = session.buffer();
            let currentWLCharacterEndLoc = session.SrcLoc;

            let longNameEndBuf = currentWLCharacterEndBuf;

            // let longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
            let longNameBufAndLen = BufferAndLength::between(longNameStartBuf, longNameEndBuf);
            let longNameStr = longNameBufAndLen.as_str();

            if atleast1DigitOrAlpha {
                //
                // Something like \[Alpha
                //
                // Make the warning message a little more relevant
                //

                let suggestion = CharacterDecoder_longNameSuggestion(longNameStr);

                let mut Actions: Vec<CodeAction> = Vec::new();

                let found = LONGNAME_TO_CODE_POINT_MAP__NAMES
                    .binary_search(&longNameStr)
                    .is_ok();

                if found {
                    Actions.push(CodeAction::insert_text(
                        format!("Insert ``]`` to form ``\\[{suggestion}]``"),
                        Source::from_location(currentWLCharacterEndLoc),
                        "]".into(),
                    ));
                }

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\[{longNameStr}``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            } else {
                //
                // Malformed some other way
                //
                // Something like \[!
                // Something like \[*
                //

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\[{longNameStr}``"),
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    format!("\\\\[{longNameStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\[{}``.", longNameStr),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }
        }

        session.offset = openSquareBuf;
        session.SrcLoc = openSquareLoc;

        return WLCharacter::new('\\');
    }

    //
    // Well-formed
    //

    //
    // if unlikelyEscapeChecking, then make sure to append all of the Source characters again
    //

    let longNameEndBuf = session.buffer();

    let longNameBufAndLen = BufferAndLength::between(longNameStartBuf, longNameEndBuf);
    // let longNameBufAndLen = BufferAndLength {
    //     buf: longNameStartBuf,
    //     len: longNameEndBuf.as_ptr().addr() - longNameStartBuf.as_ptr().addr(),
    // };

    // let longNameStr = std::string(reinterpret_cast::<*const i8>(longNameBufAndLen.Buf), longNameBufAndLen.length());
    let longNameStr = longNameBufAndLen.as_str();

    debug_assert!(utils::is_sorted(&LONGNAME_TO_CODE_POINT_MAP__NAMES));
    let found: Option<usize> = LONGNAME_TO_CODE_POINT_MAP__NAMES
        .binary_search(&longNameStr)
        .ok();

    if found == None {
        //
        // Name not found
        //

        if feature::CHECK_ISSUES
            && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
        {
            let longNameEndLoc = session.SrcLoc;

            let currentWLCharacterStartLoc = openSquareLoc.previous();

            //
            // Accomodate the ] character
            //
            let currentWLCharacterEndLoc = longNameEndLoc.next();

            let suggestion = CharacterDecoder_longNameSuggestion(longNameStr);

            let mut Actions: Vec<CodeAction> = Vec::new();

            // if !suggestion.is_empty() {
            //     Actions.push(CodeAction::replace_text(
            //         format!("Replace with ``\\[{suggestion}]``"),
            //         Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
            //         format!("\\[{suggestion}]"),
            //     ));
            // }

            // //
            // // More specifically: Unrecognized
            // //
            // let I = SyntaxIssue(
            //     STRING_UNHANDLEDCHARACTER,
            //     format!("Unhandled character: ``\\[{longNameStr}]``."),
            //     STRING_FATAL,
            //     Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
            //     1.0,
            //     Actions,
            //     vec![format!("``{longNameStr}`` is not a recognized long name.")],
            // );

            // session.addIssue(I);

            if (policy & SCAN_FOR_UNRECOGNIZEDLONGNAMES) == SCAN_FOR_UNRECOGNIZEDLONGNAMES {
                let currentUnrecognizedStartLoc = currentWLCharacterStartLoc.previous();

                if !suggestion.is_empty() {
                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\\\[{suggestion}]``"),
                        Source::new(currentUnrecognizedStartLoc, currentWLCharacterEndLoc),
                        format!("\\\\[{suggestion}]"),
                    ));
                }

                let I = SyntaxIssue(
                    IssueTag::UnrecognizedLongName,
                    format!("Unrecognized longname: ``\\\\[{longNameStr}]``."),
                    Severity::Error,
                    Source::new(currentUnrecognizedStartLoc, currentWLCharacterEndLoc),
                    0.75,
                    Actions,
                    vec![format!("``{longNameStr}`` is not a valid long name.")],
                );

                session.addIssue(I);
            } else {
                if !suggestion.is_empty() {
                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\[{suggestion}]``"),
                        Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                        format!("\\[{suggestion}]"),
                    ));
                }

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\[{longNameStr}]``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![format!("``{longNameStr}`` is not a valid long name.")],
                );

                session.addIssue(I);
            }
        }

        session.offset = openSquareBuf;
        session.SrcLoc = openSquareLoc;

        return WLCharacter::new('\\');
    }

    let found: usize = found.unwrap();

    //
    // Success!
    //

    ByteDecoder_nextSourceCharacter(session, policy);

    let point: CodePoint = LONGNAME_TO_CODE_POINT_MAP__POINTS[found];

    if feature::CHECK_ISSUES
        && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
    {
        // let longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
        let longNameBufAndLen = BufferAndLength::between(longNameStartBuf, longNameEndBuf);
        let longNameStr = longNameBufAndLen.as_str();

        check_strange_syntax_issue(
            session,
            policy,
            point,
            openSquareLoc,
            if LongNames::isRaw(longNameStr) {
                EscapeStyle::Raw
            } else {
                EscapeStyle::LongName
            },
        );
    }

    if LongNames::isRaw(longNameStr) {
        return WLCharacter::new_with_escape(point, EscapeStyle::Raw);
    }

    return WLCharacter::new_with_escape(point, EscapeStyle::LongName);
}

fn CharacterDecoder_handle4Hex(
    session: &mut Tokenizer,
    colon_offset: usize,
    colonLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(session.input[colon_offset] == b':');

    incr_diagnostic!(CharacterDecoder_4HexCount);

    let hexStartBuf = session.buffer();

    for _ in 0..4 {
        let curSource = ByteDecoder_currentSourceCharacter(session, policy);

        if curSource.isHex() {
            ByteDecoder_nextSourceCharacter(session, policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \:z
            //

            if feature::CHECK_ISSUES
                && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
            {
                let currentWLCharacterStartLoc = colonLoc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let hexEndBuf = currentWLCharacterEndBuf;

                // let hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                let hexBufAndLen = BufferAndLength::between(hexStartBuf, hexEndBuf);
                let hexStr = hexBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\:{hexStr}``"),
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    format!("\\\\:{hexStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\:{hexStr}``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.offset = colon_offset;
            session.SrcLoc = colonLoc;

            return WLCharacter::new('\\');
        }
    }

    //
    // Success!
    //

    let d3 = u32::from(utils::toDigit(hexStartBuf[0]));
    let d2 = u32::from(utils::toDigit(hexStartBuf[1]));
    let d1 = u32::from(utils::toDigit(hexStartBuf[2]));
    let d0 = u32::from(utils::toDigit(hexStartBuf[3]));
    let point: u32 = d3 << 12 | d2 << 8 | d1 << 4 | d0;
    let mut point = CodePoint::from_u32(point).unwrap();

    match point {
        CodePoint::Char(CODEPOINT_ACTUAL_DOUBLEQUOTE) => {
            point = CodePoint::StringMeta_DoubleQuote;
        },
        CodePoint::Char(CODEPOINT_ACTUAL_BACKSLASH) => {
            point = CodePoint::StringMeta_Backslash;
        },
        _ => (),
    }

    #[cfg(feature = "CHECK_ISSUES")]
    check_strange_syntax_issue(session, policy, point, colonLoc, EscapeStyle::Hex4);

    return WLCharacter::new_with_escape(point, EscapeStyle::Hex4);
}

fn CharacterDecoder_handle2Hex(
    session: &mut Tokenizer,
    dot_offset: usize,
    dotLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(session.input[dot_offset] == b'.');

    incr_diagnostic!(CharacterDecoder_2HexCount);

    let hexStartBuf = session.buffer();

    for _ in 0..2 {
        let curSource = ByteDecoder_currentSourceCharacter(session, policy);

        if curSource.isHex() {
            ByteDecoder_nextSourceCharacter(session, policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \.z
            //

            if feature::CHECK_ISSUES
                && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
            {
                let currentWLCharacterStartLoc = dotLoc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let hexEndBuf = currentWLCharacterEndBuf;

                // let hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                let hexBufAndLen = BufferAndLength::between(hexStartBuf, hexEndBuf);
                let hexStr = hexBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\.{hexStr}``"),
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    format!("\\\\.{hexStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\.{hexStr}``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.offset = dot_offset;
            session.SrcLoc = dotLoc;

            return WLCharacter::new('\\');
        }
    }

    //
    // Success!
    //

    let d1 = utils::toDigit(hexStartBuf[0]);
    let d0 = utils::toDigit(hexStartBuf[1]);
    let mut point: CodePoint = CodePoint::Char(char::from(d1 << 4 | d0));

    match point {
        CodePoint::Char(CODEPOINT_ACTUAL_DOUBLEQUOTE) => {
            point = CodePoint::StringMeta_DoubleQuote;
        },
        CodePoint::Char(CODEPOINT_ACTUAL_BACKSLASH) => {
            point = CodePoint::StringMeta_Backslash;
        },
        _ => (),
    }

    #[cfg(feature = "CHECK_ISSUES")]
    check_strange_syntax_issue(session, policy, point, dotLoc, EscapeStyle::Hex2);

    return WLCharacter::new_with_escape(point, EscapeStyle::Hex2);
}

fn CharacterDecoder_handleOctal(
    session: &mut Tokenizer,
    firstOctalBuf: usize,
    firstOctalLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(SourceCharacter::from(char::from(session.input[firstOctalBuf])).isOctal());

    incr_diagnostic!(CharacterDecoder_OctalCount);

    let octalStartBuf = session.buffer_at(firstOctalBuf);

    for _ in 0..3 - 1 {
        let curSource = ByteDecoder_currentSourceCharacter(session, policy);

        if curSource.isOctal() {
            ByteDecoder_nextSourceCharacter(session, policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \1z
            //

            if feature::CHECK_ISSUES
                && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
            {
                let currentWLCharacterStartLoc = firstOctalLoc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let octalEndBuf = currentWLCharacterEndBuf;

                // let octalBufAndLen = BufferAndLength(octalStartBuf, octalEndBuf - octalStartBuf);
                let octalBufAndLen = BufferAndLength::between(octalStartBuf, octalEndBuf);
                let octalStr = octalBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\{octalStr}``"),
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    format!("\\\\{octalStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\{octalStr}``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.offset = firstOctalBuf;
            session.SrcLoc = firstOctalLoc;

            // FIXME: Why return a backslash if the character is not well-formed?
            return WLCharacter::new('\\');
        }
    }

    //
    // Success!
    //

    let d2 = utils::toDigit(octalStartBuf[0]);
    let d1 = utils::toDigit(octalStartBuf[1]);
    let d0 = utils::toDigit(octalStartBuf[2]);
    let point: u32 = u32::from(d2) << 6 | u32::from(d1) << 3 | u32::from(d0);
    let mut point = CodePoint::from_u32(point).unwrap();

    // FIXME: This match to do canonicalization is repeated in several places.
    //        Refactor this into a method on CodePoint. Perhaps this should even
    //        be done automatically in CodePoint::from_u32?
    match point {
        CodePoint::Char(CODEPOINT_ACTUAL_DOUBLEQUOTE) => {
            point = CodePoint::StringMeta_DoubleQuote;
        },
        CodePoint::Char(CODEPOINT_ACTUAL_BACKSLASH) => {
            point = CodePoint::StringMeta_Backslash;
        },
        _ => (),
    }

    #[cfg(feature = "CHECK_ISSUES")]
    check_strange_syntax_issue(session, policy, point, firstOctalLoc, EscapeStyle::Octal);

    return WLCharacter::new_with_escape(point, EscapeStyle::Octal);
}

fn CharacterDecoder_handle6Hex(
    session: &mut Tokenizer,
    bar_offset: usize,
    barLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(session.input[bar_offset] == b'|');

    incr_diagnostic!(CharacterDecoder_6HexCount);

    let hexStartBuf = session.buffer();

    for _ in 0..6 {
        let curSource = ByteDecoder_currentSourceCharacter(session, policy);

        if curSource.isHex() {
            ByteDecoder_nextSourceCharacter(session, policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \|z
            //

            if feature::CHECK_ISSUES
                && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
            {
                let currentWLCharacterStartLoc = barLoc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let hexEndBuf = currentWLCharacterEndBuf;

                // let hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                let hexBufAndLen = BufferAndLength::between(hexStartBuf, hexEndBuf);
                let hexStr = hexBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\|{hexStr}``"),
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    format!("\\\\|{hexStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\|{hexStr}``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.offset = bar_offset;
            session.SrcLoc = barLoc;

            return WLCharacter::new('\\');
        }
    }

    //
    // Well-formed
    //

    let d5 = utils::toDigit(hexStartBuf[0]);
    let d4 = utils::toDigit(hexStartBuf[1]);
    let d3 = utils::toDigit(hexStartBuf[2]);
    let d2 = utils::toDigit(hexStartBuf[3]);
    let d1 = utils::toDigit(hexStartBuf[4]);
    let d0 = utils::toDigit(hexStartBuf[5]);
    // let point: u32 = d5 << 20 | d4 << 16 | d3 << 12 | d2 << 8 | d1 << 4 | d0;
    let point: u32 = u32::from_be_bytes([0, d5 << 4 | d4, d3 << 4 | d2, d1 << 4 | d0]);

    // TODO: Is this logic here correct? Why always return a \ if point is out
    //       of range?
    if point > 0x10ffff {
        session.offset = bar_offset;
        session.SrcLoc = barLoc;

        return WLCharacter::new('\\');
    }

    let mut point = CodePoint::from_u32(point).unwrap();

    //
    // Success!
    //

    match point {
        CodePoint::Char(CODEPOINT_ACTUAL_DOUBLEQUOTE) => {
            point = CodePoint::StringMeta_DoubleQuote;
        },
        CodePoint::Char(CODEPOINT_ACTUAL_BACKSLASH) => {
            point = CodePoint::StringMeta_Backslash;
        },
        _ => (),
    }

    #[cfg(feature = "CHECK_ISSUES")]
    check_strange_syntax_issue(session, policy, point, barLoc, EscapeStyle::Hex6);

    return WLCharacter::new_with_escape(point, EscapeStyle::Hex6);
}

fn CharacterDecoder_handleBackslash(session: &mut Tokenizer, policy: NextPolicy) -> WLCharacter {
    //
    // test whether this \ is the result of the "feature" of
    // converting "\[Alpa]" into "\\[Alpa]", copying that, and then never giving any further warnings
    // when dealing with "\\[Alpa]"
    //
    if feature::CHECK_ISSUES {
        let resetBuf = session.offset;
        let resetLoc = session.SrcLoc;

        //
        // will be resetting any way, so just use nextSourceCharacter here
        //
        let mut c = ByteDecoder_nextSourceCharacter(session, policy);

        if c == '[' {
            //
            // Try to reconstruct \[XXX]
            //
            // If well-formed, then warn
            //

            let longNameStartBuf = session.offset;
            let longNameStartLoc = session.SrcLoc;

            c = ByteDecoder_nextSourceCharacter(session, policy);

            let mut wellFormed = false;

            if c.isUpper() {
                c = ByteDecoder_nextSourceCharacter(session, policy);

                loop {
                    if c.isAlphaOrDigit() {
                        c = ByteDecoder_nextSourceCharacter(session, policy);

                        continue;
                    }

                    if c == ']' {
                        wellFormed = true;
                    }

                    break;
                }
            }

            if wellFormed {
                let mut tmpPolicy = policy;

                tmpPolicy |= ENABLE_CHARACTER_DECODING_ISSUES;
                tmpPolicy |= SCAN_FOR_UNRECOGNIZEDLONGNAMES;

                session.offset = longNameStartBuf;
                session.SrcLoc = longNameStartLoc;

                CharacterDecoder_handleLongName(session, resetBuf, resetLoc, tmpPolicy);
            }
        }

        session.offset = resetBuf;
        session.SrcLoc = resetLoc;
    }

    return WLCharacter::new_with_escape(StringMeta_Backslash, EscapeStyle::Single);
}

fn CharacterDecoder_handleUnhandledEscape(
    session: &mut Tokenizer,
    unhandled_offset: usize,
    unhandledLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    //
    // Anything else
    //
    // Something like  \A
    //

    let escapedChar = ByteDecoder_currentSourceCharacter(session, policy);

    ByteDecoder_nextSourceCharacter(session, policy);

    //
    // Make the warnings a little more relevant
    //

    if feature::CHECK_ISSUES
        && (policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES
    {
        let currentWLCharacterStartLoc = unhandledLoc.previous();

        let mut currentWLCharacterEndLoc = session.SrcLoc;

        if escapedChar.isUpper() {
            //
            // Attempt to read \Alpha] and report a missing [
            //

            let mut alnumRun: String = String::new();

            alnumRun.push(escapedChar.as_char().unwrap());

            let mut curSource = ByteDecoder_currentSourceCharacter(session, policy);

            let mut wellFormed = false;

            loop {
                if curSource.isAlphaOrDigit() {
                    alnumRun.push(curSource.as_char().unwrap());

                    ByteDecoder_nextSourceCharacter(session, policy);

                    curSource = ByteDecoder_currentSourceCharacter(session, policy);
                } else if curSource == ']' {
                    ByteDecoder_nextSourceCharacter(session, policy);

                    wellFormed = true;

                    break;
                } else {
                    //
                    // Unrecognized
                    //
                    // Something like \A!] which is not a long name
                    //

                    break;
                }
            }

            let mut wellFormedAndFound = false;

            if wellFormed {
                wellFormedAndFound = LONGNAME_TO_CODE_POINT_MAP__NAMES
                    .binary_search(&alnumRun.as_str())
                    .is_ok();
            }

            if wellFormedAndFound {
                //
                // Something like \Alpha]
                //

                currentWLCharacterEndLoc = session.SrcLoc;

                let curSourceGraphicalStr = format!("{alnumRun}]");

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::insert_text(
                    format!("Insert ``[`` to form ``\\[{alnumRun}]``"),
                    Source::from_location(currentWLCharacterStartLoc.next()),
                    "[".into(),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character ``\\{curSourceGraphicalStr}``."),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            } else {
                if escapedChar.isHex() {
                    let curSourceGraphicalStr = WLCharacter::new(escapedChar).graphicalString();

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\[{}XXX]``", curSourceGraphicalStr),
                        Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                        format!("\\[{}XXX]", curSourceGraphicalStr),
                    ));

                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\:{}XXX``", curSourceGraphicalStr),
                        Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                        format!("\\:{}XXX", curSourceGraphicalStr),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::UnhandledCharacter,
                        format!("Unhandled character ``\\{}``.", curSourceGraphicalStr),
                        Severity::Fatal,
                        Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                } else {
                    let curSourceGraphicalStr = WLCharacter::new(escapedChar).graphicalString();

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\[{}XXX]``", curSourceGraphicalStr),
                        Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                        format!("\\[{}XXX]", curSourceGraphicalStr),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::UnhandledCharacter,
                        format!("Unhandled character ``\\{}``.", curSourceGraphicalStr),
                        Severity::Fatal,
                        Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }
        } else if escapedChar.isHex() {
            let curSourceGraphicalStr = WLCharacter::new(escapedChar).graphicalString();

            let mut Actions: Vec<CodeAction> = Vec::new();

            Actions.push(CodeAction::replace_text(
                format!("Replace with ``\\:{}xxx``", curSourceGraphicalStr),
                Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                format!("\\:{}xxx", curSourceGraphicalStr),
            ));

            let I = SyntaxIssue(
                IssueTag::UnhandledCharacter,
                format!("Unhandled character ``\\{}``.", curSourceGraphicalStr),
                Severity::Fatal,
                Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                1.0,
                Actions,
                vec![],
            );

            session.addIssue(I);
        } else if escapedChar.isEndOfFile() {

            //
            // Do not know what a good suggestion would be for \<EOF>
            //
        } else if escapedChar.isMBUnsafeUTF8Sequence() {

            //
            // Do not know what a good suggestion would be for \<0xa9>
            //
        } else {
            //
            // Anything else
            //

            let curSourceGraphicalStr = WLCharacter::new(escapedChar).graphicalString();

            if curSourceGraphicalStr.len() > 1 {
                //
                // Something like \<tab>
                //
                // curSourceGraphicalStr is now the 2 characters '\' 't'
                //
                // This can now be confusing when reporting the issue.
                // The correct number of backslashes is required.
                //
                // Do the simple thing: No actions, and report the character with all escaped backslashes now
                //

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character ``\\\\{}``.", curSourceGraphicalStr),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    vec![],
                    vec![],
                );

                session.addIssue(I);
            } else {
                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\{}``", curSourceGraphicalStr),
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    format!("\\\\{}", curSourceGraphicalStr),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character ``\\{}``.", curSourceGraphicalStr),
                    Severity::Fatal,
                    Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }
        }
    }

    //
    // Keep these treated as 2 characters. This is how bad escapes are handled in WL strings.
    // And has the nice benefit of the single \ still giving an error at top-level
    //
    // Currently, past the bad character
    //
    // Must remember to reset to the bad character
    //
    // The tokenizer will use the bad character to decide what to do
    //

    session.offset = unhandled_offset;
    session.SrcLoc = unhandledLoc;

    return WLCharacter::new('\\');
}

fn CharacterDecoder_handleAssertFalse(
    _session: &mut Tokenizer,
    _escapedBuf: usize,
    _escapedLoc: SourceLocation,
    _policy: NextPolicy,
) -> WLCharacter {
    panic!();
}

fn CharacterDecoder_handleUncommon<'i, 's>(
    session: &'s mut Tokenizer<'i>,
    escapedBuf: usize,
    escapedLoc: SourceLocation,
    policy: NextPolicy,
) -> WLCharacter {
    let curSource = ByteDecoder_currentSourceCharacter(session, policy);

    match curSource {
        Char('\n') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(LineContinuation_LineFeed, EscapeStyle::Single);
        },
        Char('\r') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(
                LineContinuation_CarriageReturn,
                EscapeStyle::Single,
            );
        },
        CodePoint::CRLF => {
            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(LineContinuation_CRLF, EscapeStyle::Single);
        },
        Char('[') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            //            MUSTTAIL
            return CharacterDecoder_handleLongName(session, escapedBuf, escapedLoc, policy);
        },
        Char(':') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            //            MUSTTAIL
            return CharacterDecoder_handle4Hex(session, escapedBuf, escapedLoc, policy);
        },
        Char('.') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            //            MUSTTAIL
            return CharacterDecoder_handle2Hex(session, escapedBuf, escapedLoc, policy);
        },
        Char('|') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            //            MUSTTAIL
            return CharacterDecoder_handle6Hex(session, escapedBuf, escapedLoc, policy);
        },
        Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7') => {
            ByteDecoder_nextSourceCharacter(session, policy);

            //            MUSTTAIL
            return CharacterDecoder_handleOctal(session, escapedBuf, escapedLoc, policy);
        },

        //
        // Simple escaped characters
        // \b \f \n \r \t
        //
        Char('b') => {
            incr_diagnostic!(CharacterDecoder_StringMetaBackspaceCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            let c = WLCharacter::new_with_escape(StringMeta_Backspace, EscapeStyle::Single);

            if feature::CHECK_ISSUES {
                let graphicalStr = c.graphicalString();

                let currentWLCharacterStartLoc = escapedLoc.previous();

                let currentWLCharacterEndLoc = session.SrcLoc;

                let Src = Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc);

                //
                // matched reduced severity of unexpected characters inside strings or comments
                //

                let I = SyntaxIssue(
                    IssueTag::UnexpectedCharacter,
                    format!("Unexpected character: ``{graphicalStr}``."),
                    Severity::Remark,
                    Src,
                    0.95,
                    vec![],
                    vec![],
                );

                session.addIssue(I);
            }

            return c;
        },

        Char('f') => {
            //
            // \f is NOT a space character (but inside of strings, it does have special meaning)
            //

            incr_diagnostic!(CharacterDecoder_StringMetaFormFeedCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            let c = WLCharacter::new_with_escape(StringMeta_FormFeed, EscapeStyle::Single);

            if feature::CHECK_ISSUES {
                let graphicalStr = c.graphicalString();

                let currentWLCharacterStartLoc = escapedLoc.previous();

                let currentWLCharacterEndLoc = session.SrcLoc;

                let Src = Source::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc);

                //
                // matched reduced severity of unexpected characters inside strings or comments
                //

                let I = SyntaxIssue(
                    IssueTag::UnexpectedCharacter,
                    format!("Unexpected character: ``{graphicalStr}``."),
                    Severity::Remark,
                    Src,
                    0.95,
                    vec![],
                    vec![],
                );

                session.addIssue(I);
            }

            return c;
        },

        Char('n') => {
            //
            // \n is NOT a newline character (but inside of strings, it does have special meaning)
            //

            incr_diagnostic!(CharacterDecoder_StringMetaLineFeedCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(StringMeta_LineFeed, EscapeStyle::Single);
        },

        Char('r') => {
            //
            // \r is NOT a newline character (but inside of strings, it does have special meaning)
            //

            incr_diagnostic!(CharacterDecoder_StringMetaCarriageReturnCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(StringMeta_CarriageReturn, EscapeStyle::Single);
        },

        Char('t') => {
            //
            // \t is NOT a space character (but inside of strings, it does have special meaning)
            //

            incr_diagnostic!(CharacterDecoder_StringMetaTabCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(StringMeta_Tab, EscapeStyle::Single);
        },
        //
        // Linear syntax characters
        // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
        //
        Char('!') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxBangCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_BANG, EscapeStyle::Single);
        },
        Char('%') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxPercentCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_PERCENT,
                EscapeStyle::Single,
            );
        },
        Char('&') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxAmpCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_AMP, EscapeStyle::Single);
        },
        Char('(') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxOpenParenCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_OPENPAREN,
                EscapeStyle::Single,
            );
        },
        Char(')') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxCloseParenCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_CLOSEPAREN,
                EscapeStyle::Single,
            );
        },
        Char('*') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxStarCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_STAR, EscapeStyle::Single);
        },
        Char('+') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxPlusCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_PLUS, EscapeStyle::Single);
        },
        Char('/') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxSlashCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_SLASH, EscapeStyle::Single);
        },
        Char('@') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxAtCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_AT, EscapeStyle::Single);
        },
        Char('^') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxCaretCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_CARET, EscapeStyle::Single);
        },
        Char('_') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxUnderscoreCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_UNDER, EscapeStyle::Single);
        },
        Char('`') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxBacktickCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_BACKTICK,
                EscapeStyle::Single,
            );
        },
        Char(' ') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxSpaceCount);

            ByteDecoder_nextSourceCharacter(session, policy);

            return WLCharacter::new_with_escape(LinearSyntax_Space, EscapeStyle::Single);
        },
        _ => (),
    } // switch

    //
    // Anything else
    //
    // Something like \A or \{
    //
    incr_diagnostic!(CharacterDecoder_UnhandledCount);

    //    MUSTTAIL
    return CharacterDecoder_handleUnhandledEscape(session, escapedBuf, escapedLoc, policy);
}

//
// example:
// input: Alpa
// return Alpha
//
// Return empty string if no suggestion.
//
#[cfg(feature = "USE_EXPR_LIB")]
fn CharacterDecoder_longNameSuggestion(input: String) -> String {
    let InputExpr = Expr_UTF8BytesToStringExpr(input.c_str(), input.len());

    let e = Expr_LongNameSuggestion(InputExpr);

    let buffer: Buffer;
    let len: usize;

    Expr_StringExprToUTF8Bytes(e, &buffer, reinterpret_cast::<*mut mint>(&len));

    let suggestion = std::string(reinterpret_cast::<*const i8>(buffer), len);

    Expr_Release(e);

    return suggestion;
}

fn CharacterDecoder_longNameSuggestion(input: &str) -> String {
    use crate::long_names_registration::CODE_POINT_TO_LONGNAME_MAP__NAMES;
    use edit_distance::edit_distance;

    let closest: Option<&&str> = CODE_POINT_TO_LONGNAME_MAP__NAMES
        .iter()
        .min_by_key(|name| edit_distance(input, name));

    match closest {
        Some(closest) => {
            // If the `closest` long name isn't within an edit distance of 2,
            // then we aren't confident enough that it might have been the users
            // intent to write `closest`, so don't suggest it.
            if edit_distance(input, closest) <= 2 {
                closest.to_string()
            } else {
                String::new()
            }
        },
        // TODO: Return None?
        _ => String::new(),
    }
}

/// Add an [`Issue`][crate::issue::Issue] if the specified [`CodePoint`] is
/// a "strange" character.
pub(crate) fn check_strange_syntax_issue(
    session: &mut Tokenizer,
    policy: NextPolicy,
    point: CodePoint,
    start_loc: SourceLocation,
    escape_style: EscapeStyle,
) {
    let c = WLCharacter::new_with_escape(point, escape_style);

    let issue_value: f64 = if utils::isStrange(point) {
        if c.isStrangeWhitespace() {
            return;
        } else {
            0.95
        }
    } else if utils::isMBStrange(point) {
        if c.isMBStrangeWhitespace() {
            return;
        } else {
            0.85
        }
    } else {
        return;
    };

    //
    // Just generally strange character is in the code
    //

    let currentWLCharacterStartLoc = start_loc.previous();

    let currentSourceCharacterEndLoc = session.SrcLoc;

    let graphicalStr = c.graphicalString();

    let Src = Source::new(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);

    let mut Actions: Vec<CodeAction> = Vec::new();

    for A in utils::certainCharacterReplacementActions(c, Src) {
        Actions.push(A);
    }

    //
    // do not recommend replacing graphical character with literal version
    //

    //
    // any ASCII replacements
    //
    for r in LongNames::asciiReplacements(point) {
        Actions.push(CodeAction::replace_text(
            format!(
                "Replace with ``{}``",
                LongNames::replacementGraphical(r.clone())
            ),
            Src,
            r,
        ));
    }

    //
    // reduce severity of unexpected characters inside strings or comments
    //
    let severity = if (policy & STRING_OR_COMMENT) == STRING_OR_COMMENT {
        Severity::Remark
    } else {
        Severity::Warning
    };

    let issue = SyntaxIssue(
        IssueTag::UnexpectedCharacter,
        format!("Unexpected character: ``{graphicalStr}``."),
        severity,
        Src,
        issue_value,
        Actions,
        vec![],
    );

    session.addIssue(issue);
}
