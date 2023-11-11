use crate::{
    generated::long_names_registration::CODEPOINT_TO_LONGNAME_MAP,
    issue::{CodeAction, IssueTag, Severity, SyntaxIssue},
    long_names::{self as LongNames, self},
    read::{
        code_point::{CodePoint::*, *},
        wl_character::{Escape, WLCharacter},
        Reader,
    },
    source::{
        BufferAndLength, Location, NextPolicy,
        NextPolicyBits::{
            ENABLE_CHARACTER_DECODING_ISSUES, SCAN_FOR_UNRECOGNIZEDLONGNAMES,
        },
        SourceCharacter, Span, STRING_OR_COMMENT,
    },
    utils::{self, from_fn},
};

use super::InputMark;

//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences of Source Characters such as \[Alpha] into a single WL character
//

type HandlerFunction = for<'i, 's> fn(
    session: &'s mut Reader<'i>,
    start: InputMark,
    policy: NextPolicy,
) -> WLCharacter;

/// Lookup table for handling ASCII byte values preceeded by a backslash.
///
/// This lookup table is equivalent to the match statement used in this
/// `const` definition to populate the table.
///
/// However, the lookup table-based implementation performs ~10-15% better than
/// the `match` statement version on some of the large benchmarks.
#[rustfmt::skip]
const CHARACTER_DECODER_HANDLER_TABLE: [HandlerFunction; 128] = from_fn!(
    [HandlerFunction, 128],
    |index: usize| {
        let index = index as u8;

        match index {
            00..=31 => CharacterDecoder_handleUncommon,
            32 => CharacterDecoder_handleUncommon,              // SPACE
            33 => CharacterDecoder_handleUncommon,              // !
            34 => CharacterDecoder_handleStringMetaDoubleQuote, // "
            35..=59 => CharacterDecoder_handleUncommon,
            60 => CharacterDecoder_handleStringMetaOpen,        // <
            61 => CharacterDecoder_handleUncommon,              // =
            62 => CharacterDecoder_handleStringMetaClose,       // >
            63..=91 => CharacterDecoder_handleUncommon,
            92 => CharacterDecoder_handleStringMetaBackslash,   // \
            93..=127 => CharacterDecoder_handleUncommon,
            // "invalid ASCII byte value: {i}"
            128..=255 => panic!(),
        }
    }
);


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
pub(super) fn CharacterDecoder_nextWLCharacter(
    session: &mut Reader,
    policy: NextPolicy,
) -> WLCharacter {
    let mut point = session.next_source_char(policy);

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

    let escaped = session.mark();

    point = session.peek_source_char(policy);

    if !point.is_ascii() {
        // MUSTTAIL
        return CharacterDecoder_handleUncommon(session, escaped, policy);
    }

    let index = usize::try_from(point.as_i32()).unwrap();

    return CHARACTER_DECODER_HANDLER_TABLE[index](session, escaped, policy);
}

fn CharacterDecoder_handleStringMetaDoubleQuote(
    session: &mut Reader,
    _: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaDoubleQuoteCount);

    session.next_source_char(policy);

    return WLCharacter::new_with_escape(
        StringMeta_DoubleQuote,
        Escape::Single,
    );
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
    session: &mut Reader,
    escaped: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaOpenCount);

    session.next_source_char(policy);

    let c = WLCharacter::new_with_escape(StringMeta_Open, Escape::Single);

    if session.check_issues {
        let graphicalStr = c.graphicalString();

        let currentWLCharacterStartLoc = escaped.src_loc.previous();

        let currentWLCharacterEndLoc = session.SrcLoc;

        let Src =
            Span::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc);

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
    session: &mut Reader,
    escaped: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaCloseCount);

    session.next_source_char(policy);

    let c = WLCharacter::new_with_escape(StringMeta_Close, Escape::Single);

    if session.check_issues {
        let graphicalStr = c.graphicalString();

        let currentWLCharacterStartLoc = escaped.src_loc.previous();

        let currentWLCharacterEndLoc = session.SrcLoc;

        let Src =
            Span::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc);

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
    session: &mut Reader,
    _: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(CharacterDecoder_StringMetaBackslashCount);

    session.next_source_char(policy);

    //    MUSTTAIL
    return CharacterDecoder_handleBackslash(session, policy);
}

fn CharacterDecoder_handleLongName(
    session: &mut Reader,
    open_square: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    // assert!(openSquareBuf[0] == b'[');
    assert!(session.input[open_square.offset] == b'[');

    incr_diagnostic!(CharacterDecoder_LongNameCount);

    //
    // Do not write leading \[ or trailing ] to LongName
    //
    let longNameStartBuf = session.buffer();

    let mut curSource = session.peek_source_char(policy);

    let mut wellFormed = false;

    let mut atleast1DigitOrAlpha = false;

    //
    // Read at least 1 alnum before entering loop
    //
    // Must start with upper
    //
    if curSource.isUpper() {
        atleast1DigitOrAlpha = true;

        session.next_source_char(policy);

        curSource = session.peek_source_char(policy);

        loop {
            if curSource.isAlphaOrDigit() {
                session.next_source_char(policy);

                curSource = session.peek_source_char(policy);
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

        session.next_source_char(policy);

        curSource = session.peek_source_char(policy);
    }

    if !wellFormed {
        //
        // Not well-formed
        //

        if session.check_issues
            && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
        {
            let currentWLCharacterStartLoc = open_square.src_loc.previous();

            let currentWLCharacterEndBuf = session.buffer();
            let currentWLCharacterEndLoc = session.SrcLoc;

            let longNameEndBuf = currentWLCharacterEndBuf;

            // let longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
            let longNameBufAndLen =
                BufferAndLength::between(longNameStartBuf, longNameEndBuf);
            let longNameStr = longNameBufAndLen.as_str();

            if atleast1DigitOrAlpha {
                //
                // Something like \[Alpha
                //
                // Make the warning message a little more relevant
                //

                let suggestion =
                    CharacterDecoder_longNameSuggestion(longNameStr);

                let mut Actions: Vec<CodeAction> = Vec::new();

                let found =
                    long_names::longname_to_codepoint(longNameStr).is_some();

                if found {
                    Actions.push(CodeAction::insert_text(
                        format!("Insert ``]`` to form ``\\[{suggestion}]``"),
                        Span::at(currentWLCharacterEndLoc),
                        "]".into(),
                    ));
                }

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\[{longNameStr}``."),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
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
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    format!("\\\\[{longNameStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\[{}``.", longNameStr),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }
        }

        session.seek(open_square);

        return WLCharacter::new('\\');
    }

    //
    // Well-formed
    //

    //
    // if unlikelyEscapeChecking, then make sure to append all of the Source characters again
    //

    let longNameEndBuf = session.buffer();

    let longNameBufAndLen =
        BufferAndLength::between(longNameStartBuf, longNameEndBuf);
    // let longNameBufAndLen = BufferAndLength {
    //     buf: longNameStartBuf,
    //     len: longNameEndBuf.as_ptr().addr() - longNameStartBuf.as_ptr().addr(),
    // };

    // let longNameStr = std::string(reinterpret_cast::<*const i8>(longNameBufAndLen.Buf), longNameBufAndLen.length());
    let longNameStr = longNameBufAndLen.as_str();

    let found: Option<CodePoint> =
        long_names::longname_to_codepoint(longNameStr);

    if found == None {
        //
        // Name not found
        //

        if session.check_issues
            && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
        {
            let longNameEndLoc = session.SrcLoc;

            let currentWLCharacterStartLoc = open_square.src_loc.previous();

            //
            // Accomodate the ] character
            //
            let currentWLCharacterEndLoc = longNameEndLoc.next();

            let suggestion = CharacterDecoder_longNameSuggestion(longNameStr);

            let mut Actions: Vec<CodeAction> = Vec::new();

            // if !suggestion.is_empty() {
            //     Actions.push(CodeAction::replace_text(
            //         format!("Replace with ``\\[{suggestion}]``"),
            //         Span::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
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
            //     Span::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
            //     1.0,
            //     Actions,
            //     vec![format!("``{longNameStr}`` is not a recognized long name.")],
            // );

            // session.addIssue(I);

            if policy.contains(SCAN_FOR_UNRECOGNIZEDLONGNAMES) {
                let currentUnrecognizedStartLoc =
                    currentWLCharacterStartLoc.previous();

                if !suggestion.is_empty() {
                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\\\[{suggestion}]``"),
                        Span::new(
                            currentUnrecognizedStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        format!("\\\\[{suggestion}]"),
                    ));
                }

                let I = SyntaxIssue(
                    IssueTag::UnrecognizedLongName,
                    format!("Unrecognized longname: ``\\\\[{longNameStr}]``."),
                    Severity::Error,
                    Span::new(
                        currentUnrecognizedStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    0.75,
                    Actions,
                    vec![format!(
                        "``{longNameStr}`` is not a valid long name."
                    )],
                );

                session.addIssue(I);
            } else {
                if !suggestion.is_empty() {
                    Actions.push(CodeAction::replace_text(
                        format!("Replace with ``\\[{suggestion}]``"),
                        Span::new(
                            currentWLCharacterStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        format!("\\[{suggestion}]"),
                    ));
                }

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\[{longNameStr}]``."),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![format!(
                        "``{longNameStr}`` is not a valid long name."
                    )],
                );

                session.addIssue(I);
            }
        }

        session.seek(open_square);

        return WLCharacter::new('\\');
    }

    //
    // Success!
    //

    let point: CodePoint = found.unwrap();

    session.next_source_char(policy);

    if session.check_issues && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
    {
        // let longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
        let longNameBufAndLen =
            BufferAndLength::between(longNameStartBuf, longNameEndBuf);
        let longNameStr = longNameBufAndLen.as_str();

        check_strange_syntax_issue(
            session,
            policy,
            point,
            open_square.src_loc,
            if LongNames::isRaw(longNameStr) {
                Escape::Raw
            } else {
                Escape::LongName
            },
        );
    }

    if LongNames::isRaw(longNameStr) {
        return WLCharacter::new_with_escape(point, Escape::Raw);
    }

    return WLCharacter::new_with_escape(point, Escape::LongName);
}

fn CharacterDecoder_handle4Hex(
    session: &mut Reader,
    colon: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(session.input[colon.offset] == b':');

    incr_diagnostic!(CharacterDecoder_4HexCount);

    let hexStartBuf = session.buffer();

    for _ in 0..4 {
        let curSource = session.peek_source_char(policy);

        if curSource.isHex() {
            session.next_source_char(policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \:z
            //

            if session.check_issues
                && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
            {
                let currentWLCharacterStartLoc = colon.src_loc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let hexEndBuf = currentWLCharacterEndBuf;

                // let hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                let hexBufAndLen =
                    BufferAndLength::between(hexStartBuf, hexEndBuf);
                let hexStr = hexBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\:{hexStr}``"),
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    format!("\\\\:{hexStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\:{hexStr}``."),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.seek(colon);

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

    check_strange_syntax_issue(
        session,
        policy,
        point,
        colon.src_loc,
        Escape::Hex4,
    );

    return WLCharacter::new_with_escape(point, Escape::Hex4);
}

fn CharacterDecoder_handle2Hex(
    session: &mut Reader,
    dot: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(session.input[dot.offset] == b'.');

    incr_diagnostic!(CharacterDecoder_2HexCount);

    let hexStartBuf = session.buffer();

    for _ in 0..2 {
        let curSource = session.peek_source_char(policy);

        if curSource.isHex() {
            session.next_source_char(policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \.z
            //

            if session.check_issues
                && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
            {
                let currentWLCharacterStartLoc = dot.src_loc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let hexEndBuf = currentWLCharacterEndBuf;

                // let hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                let hexBufAndLen =
                    BufferAndLength::between(hexStartBuf, hexEndBuf);
                let hexStr = hexBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\.{hexStr}``"),
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    format!("\\\\.{hexStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\.{hexStr}``."),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.seek(dot);

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

    check_strange_syntax_issue(
        session,
        policy,
        point,
        dot.src_loc,
        Escape::Hex2,
    );

    return WLCharacter::new_with_escape(point, Escape::Hex2);
}

fn CharacterDecoder_handleOctal(
    session: &mut Reader,
    first_octal: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(SourceCharacter::from(char::from(
        session.input[first_octal.offset]
    ))
    .isOctal());

    incr_diagnostic!(CharacterDecoder_OctalCount);

    let octalStartBuf = session.buffer_at(first_octal.offset);

    for _ in 0..3 - 1 {
        let curSource = session.peek_source_char(policy);

        if curSource.isOctal() {
            session.next_source_char(policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \1z
            //

            if session.check_issues
                && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
            {
                let currentWLCharacterStartLoc = first_octal.src_loc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let octalEndBuf = currentWLCharacterEndBuf;

                // let octalBufAndLen = BufferAndLength(octalStartBuf, octalEndBuf - octalStartBuf);
                let octalBufAndLen =
                    BufferAndLength::between(octalStartBuf, octalEndBuf);
                let octalStr = octalBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\{octalStr}``"),
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    format!("\\\\{octalStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\{octalStr}``."),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.seek(first_octal);

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

    check_strange_syntax_issue(
        session,
        policy,
        point,
        first_octal.src_loc,
        Escape::Octal,
    );

    return WLCharacter::new_with_escape(point, Escape::Octal);
}

fn CharacterDecoder_handle6Hex(
    session: &mut Reader,
    bar: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(session.input[bar.offset] == b'|');

    incr_diagnostic!(CharacterDecoder_6HexCount);

    let hexStartBuf = session.buffer();

    for _ in 0..6 {
        let curSource = session.peek_source_char(policy);

        if curSource.isHex() {
            session.next_source_char(policy);
        } else {
            //
            // Not well-formed
            //
            // Something like \|z
            //

            if session.check_issues
                && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
            {
                let currentWLCharacterStartLoc = bar.src_loc.previous();

                let currentWLCharacterEndBuf = session.buffer();
                let currentWLCharacterEndLoc = session.SrcLoc;

                let hexEndBuf = currentWLCharacterEndBuf;

                // let hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                let hexBufAndLen =
                    BufferAndLength::between(hexStartBuf, hexEndBuf);
                let hexStr = hexBufAndLen.as_str();

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\|{hexStr}``"),
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    format!("\\\\|{hexStr}"),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!("Unhandled character: ``\\|{hexStr}``."),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            session.seek(bar);

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
    let point: u32 =
        u32::from_be_bytes([0, d5 << 4 | d4, d3 << 4 | d2, d1 << 4 | d0]);

    // TODO: Is this logic here correct? Why always return a \ if point is out
    //       of range?
    if point > 0x10ffff {
        session.seek(bar);

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

    check_strange_syntax_issue(
        session,
        policy,
        point,
        bar.src_loc,
        Escape::Hex6,
    );

    return WLCharacter::new_with_escape(point, Escape::Hex6);
}

fn CharacterDecoder_handleBackslash(
    session: &mut Reader,
    policy: NextPolicy,
) -> WLCharacter {
    //
    // test whether this \ is the result of the "feature" of
    // converting "\[Alpa]" into "\\[Alpa]", copying that, and then never giving any further warnings
    // when dealing with "\\[Alpa]"
    //
    if session.check_issues {
        let reset_mark = session.mark();

        //
        // will be resetting any way, so just use nextSourceCharacter here
        //
        let mut c = session.next_source_char(policy);

        if c == '[' {
            //
            // Try to reconstruct \[XXX]
            //
            // If well-formed, then warn
            //

            let longNameStartBuf = session.offset;
            let longNameStartLoc = session.SrcLoc;

            c = session.next_source_char(policy);

            let mut wellFormed = false;

            if c.isUpper() {
                c = session.next_source_char(policy);

                loop {
                    if c.isAlphaOrDigit() {
                        c = session.next_source_char(policy);

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

                CharacterDecoder_handleLongName(session, reset_mark, tmpPolicy);
            }
        }

        session.seek(reset_mark);
    }

    return WLCharacter::new_with_escape(StringMeta_Backslash, Escape::Single);
}

fn CharacterDecoder_handleUnhandledEscape(
    session: &mut Reader,
    unhandled: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    //
    // Anything else
    //
    // Something like  \A
    //

    let escapedChar = session.peek_source_char(policy);

    session.next_source_char(policy);

    //
    // Make the warnings a little more relevant
    //

    if session.check_issues && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES)
    {
        let currentWLCharacterStartLoc = unhandled.src_loc.previous();

        let mut currentWLCharacterEndLoc = session.SrcLoc;

        if escapedChar.isUpper() {
            //
            // Attempt to read \Alpha] and report a missing [
            //

            let mut alnumRun: String = String::new();

            alnumRun.push(escapedChar.as_char().unwrap());

            let mut curSource = session.peek_source_char(policy);

            let mut wellFormed = false;

            loop {
                if curSource.isAlphaOrDigit() {
                    alnumRun.push(curSource.as_char().unwrap());

                    session.next_source_char(policy);

                    curSource = session.peek_source_char(policy);
                } else if curSource == ']' {
                    session.next_source_char(policy);

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
                wellFormedAndFound =
                    long_names::longname_to_codepoint(alnumRun.as_str())
                        .is_some();
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
                    Span::at(currentWLCharacterStartLoc.next()),
                    "[".into(),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!(
                        "Unhandled character ``\\{curSourceGraphicalStr}``."
                    ),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            } else {
                if escapedChar.isHex() {
                    let curSourceGraphicalStr =
                        WLCharacter::new(escapedChar).graphicalString();

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::replace_text(
                        format!(
                            "Replace with ``\\[{}XXX]``",
                            curSourceGraphicalStr
                        ),
                        Span::new(
                            currentWLCharacterStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        format!("\\[{}XXX]", curSourceGraphicalStr),
                    ));

                    Actions.push(CodeAction::replace_text(
                        format!(
                            "Replace with ``\\:{}XXX``",
                            curSourceGraphicalStr
                        ),
                        Span::new(
                            currentWLCharacterStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        format!("\\:{}XXX", curSourceGraphicalStr),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::UnhandledCharacter,
                        format!(
                            "Unhandled character ``\\{}``.",
                            curSourceGraphicalStr
                        ),
                        Severity::Fatal,
                        Span::new(
                            currentWLCharacterStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                } else {
                    let curSourceGraphicalStr =
                        WLCharacter::new(escapedChar).graphicalString();

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::replace_text(
                        format!(
                            "Replace with ``\\[{}XXX]``",
                            curSourceGraphicalStr
                        ),
                        Span::new(
                            currentWLCharacterStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        format!("\\[{}XXX]", curSourceGraphicalStr),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::UnhandledCharacter,
                        format!(
                            "Unhandled character ``\\{}``.",
                            curSourceGraphicalStr
                        ),
                        Severity::Fatal,
                        Span::new(
                            currentWLCharacterStartLoc,
                            currentWLCharacterEndLoc,
                        ),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }
        } else if escapedChar.isHex() {
            let curSourceGraphicalStr =
                WLCharacter::new(escapedChar).graphicalString();

            let mut Actions: Vec<CodeAction> = Vec::new();

            Actions.push(CodeAction::replace_text(
                format!("Replace with ``\\:{}xxx``", curSourceGraphicalStr),
                Span::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
                format!("\\:{}xxx", curSourceGraphicalStr),
            ));

            let I = SyntaxIssue(
                IssueTag::UnhandledCharacter,
                format!("Unhandled character ``\\{}``.", curSourceGraphicalStr),
                Severity::Fatal,
                Span::new(currentWLCharacterStartLoc, currentWLCharacterEndLoc),
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

            let curSourceGraphicalStr =
                WLCharacter::new(escapedChar).graphicalString();

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
                    format!(
                        "Unhandled character ``\\\\{}``.",
                        curSourceGraphicalStr
                    ),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    1.0,
                    vec![],
                    vec![],
                );

                session.addIssue(I);
            } else {
                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::replace_text(
                    format!("Replace with ``\\\\{}``", curSourceGraphicalStr),
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
                    format!("\\\\{}", curSourceGraphicalStr),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnhandledCharacter,
                    format!(
                        "Unhandled character ``\\{}``.",
                        curSourceGraphicalStr
                    ),
                    Severity::Fatal,
                    Span::new(
                        currentWLCharacterStartLoc,
                        currentWLCharacterEndLoc,
                    ),
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

    session.seek(unhandled);

    return WLCharacter::new('\\');
}

fn CharacterDecoder_handleUncommon<'i, 's>(
    session: &'s mut Reader<'i>,
    escaped: InputMark,
    policy: NextPolicy,
) -> WLCharacter {
    let curSource = session.peek_source_char(policy);

    match curSource {
        Char('\n') => {
            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                LineContinuation_LineFeed,
                Escape::Single,
            );
        },
        Char('\r') => {
            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                LineContinuation_CarriageReturn,
                Escape::Single,
            );
        },
        CodePoint::CRLF => {
            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                LineContinuation_CRLF,
                Escape::Single,
            );
        },
        Char('[') => {
            session.next_source_char(policy);

            //            MUSTTAIL
            return CharacterDecoder_handleLongName(session, escaped, policy);
        },
        Char(':') => {
            session.next_source_char(policy);

            //            MUSTTAIL
            return CharacterDecoder_handle4Hex(session, escaped, policy);
        },
        Char('.') => {
            session.next_source_char(policy);

            //            MUSTTAIL
            return CharacterDecoder_handle2Hex(session, escaped, policy);
        },
        Char('|') => {
            session.next_source_char(policy);

            //            MUSTTAIL
            return CharacterDecoder_handle6Hex(session, escaped, policy);
        },
        Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7') => {
            session.next_source_char(policy);

            //            MUSTTAIL
            return CharacterDecoder_handleOctal(session, escaped, policy);
        },

        //
        // Simple escaped characters
        // \b \f \n \r \t
        //
        Char('b') => {
            incr_diagnostic!(CharacterDecoder_StringMetaBackspaceCount);

            session.next_source_char(policy);

            let c = WLCharacter::new_with_escape(
                StringMeta_Backspace,
                Escape::Single,
            );

            if session.check_issues {
                let graphicalStr = c.graphicalString();

                let currentWLCharacterStartLoc = escaped.src_loc.previous();

                let currentWLCharacterEndLoc = session.SrcLoc;

                let Src = Span::new(
                    currentWLCharacterStartLoc,
                    currentWLCharacterEndLoc,
                );

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

            session.next_source_char(policy);

            let c = WLCharacter::new_with_escape(
                StringMeta_FormFeed,
                Escape::Single,
            );

            if session.check_issues {
                let graphicalStr = c.graphicalString();

                let currentWLCharacterStartLoc = escaped.src_loc.previous();

                let currentWLCharacterEndLoc = session.SrcLoc;

                let Src = Span::new(
                    currentWLCharacterStartLoc,
                    currentWLCharacterEndLoc,
                );

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

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                StringMeta_LineFeed,
                Escape::Single,
            );
        },

        Char('r') => {
            //
            // \r is NOT a newline character (but inside of strings, it does have special meaning)
            //

            incr_diagnostic!(CharacterDecoder_StringMetaCarriageReturnCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                StringMeta_CarriageReturn,
                Escape::Single,
            );
        },

        Char('t') => {
            //
            // \t is NOT a space character (but inside of strings, it does have special meaning)
            //

            incr_diagnostic!(CharacterDecoder_StringMetaTabCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                StringMeta_Tab,
                Escape::Single,
            );
        },
        //
        // Linear syntax characters
        // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
        //
        Char('!') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxBangCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_BANG,
                Escape::Single,
            );
        },
        Char('%') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxPercentCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_PERCENT,
                Escape::Single,
            );
        },
        Char('&') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxAmpCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_AMP,
                Escape::Single,
            );
        },
        Char('(') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxOpenParenCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_OPENPAREN,
                Escape::Single,
            );
        },
        Char(')') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxCloseParenCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_CLOSEPAREN,
                Escape::Single,
            );
        },
        Char('*') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxStarCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_STAR,
                Escape::Single,
            );
        },
        Char('+') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxPlusCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_PLUS,
                Escape::Single,
            );
        },
        Char('/') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxSlashCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_SLASH,
                Escape::Single,
            );
        },
        Char('@') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxAtCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_AT,
                Escape::Single,
            );
        },
        Char('^') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxCaretCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_CARET,
                Escape::Single,
            );
        },
        Char('_') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxUnderscoreCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_UNDER,
                Escape::Single,
            );
        },
        Char('`') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxBacktickCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                CODEPOINT_LINEARSYNTAX_BACKTICK,
                Escape::Single,
            );
        },
        Char(' ') => {
            incr_diagnostic!(CharacterDecoder_LinearSyntaxSpaceCount);

            session.next_source_char(policy);

            return WLCharacter::new_with_escape(
                LinearSyntax_Space,
                Escape::Single,
            );
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
    return CharacterDecoder_handleUnhandledEscape(session, escaped, policy);
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
    use edit_distance::edit_distance;

    let closest: Option<&&str> = CODEPOINT_TO_LONGNAME_MAP
        .iter()
        .map(|(_, longname): &(CodePoint, &str)| longname)
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
    session: &mut Reader,
    policy: NextPolicy,
    point: CodePoint,
    start_loc: Location,
    escape_style: Escape,
) {
    if !session.check_issues {
        // Don't add any issues
        return;
    }

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

    let Src =
        Span::new(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);

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
    let severity = if policy.contains(STRING_OR_COMMENT) {
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
