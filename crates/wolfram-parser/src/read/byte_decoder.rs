//! Decode a sequence of UTF-8 encoded bytes into Source characters

//
// https://unicodebook.readthedocs.io/issues.html#strict-utf8-decoder
//
// Table 3.1B. Legal UTF-8 Byte Sequences
// http://www.unicode.org/versions/corrigendum1.html
//
// CODE POINTS         1ST BYTE    2ND BYTE    3RD BYTE    4TH BYTE
//   U+0000....U+007F    00..7F
//   U+0080....U+07FF    C2..DF      80..BF
//   U+0800....U+0FFF        E0      A0..BF      80..BF
//   U+1000....U+FFFF    E1..EF      80..BF      80..BF
//  U+10000...U+3FFFF        F0      90..BF      80..BF      80..BF
//  U+40000...U+FFFFF    F1..F3      80..BF      80..BF      80..BF
// U+100000..U+10FFFF        F4      80..8F      80..BF      80..BF
//
//
// stray surrogates:
//   U+D800....U+DFFF        ED      A0..BF      80..BF
//
// BOM:
//   U+FEFF                  EF      BB          BF
//

use std::num::NonZeroU32;

use crate::{
    feature,
    issue::{CodeAction, EncodingIssue, IssueTag, Severity},
    read::{
        byte_buffer::{ByteBuffer_currentByte, ByteBuffer_nextByte},
        Reader, SourceManager,
    },
    read::{
        code_point::{CodePoint, CODEPOINT_BOM},
        wl_character::{Escape, WLCharacter},
    },
    source::{
        LineColumn, Location, NextPolicy, NextPolicyBits::*, SourceCharacter,
        SourceConvention, Span,
    },
    tokenize::tokenizer::UnsafeCharacterEncoding,
    utils::{self, non_zero_u32_incr},
    EncodingMode,
};

//
// Precondition: buffer is pointing to current SourceCharacter
// Postcondition: buffer is pointing to next SourceCharacter
//
// return the SourceCharacter that was current
//
// Decode UTF-8 byte sequences
//
// Also decode \r\n into a single SourceCharacter
//
// \r\n is akin to a 2-byte UTF-8 sequence
//
// Also warn about \r line endings
//
// Do not decode unsafe character encodings: incomplete sequences, stray surrogates, or BOM
//
pub(super) fn ByteDecoder_nextSourceCharacter(
    session: &mut Reader,
    policy: NextPolicy,
) -> SourceCharacter {
    let firstByte = ByteBuffer_currentByte(session);

    if 0x20 <= firstByte && firstByte <= 0x7e {
        //
        // Valid
        //

        incr_diagnostic!(ByteDecoder_PrintableCount);

        ByteBuffer_nextByte(session);

        if feature::COMPUTE_SOURCE {
            session.src().increment();
        }

        return SourceCharacter::from_u8(firstByte);
    }

    if firstByte == 0x0a {
        //
        // Handle LF specially
        //

        incr_diagnostic!(ByteDecoder_LineFeedCount);

        ByteBuffer_nextByte(session);

        if feature::COMPUTE_SOURCE {
            session.src().newline();
        }

        return SourceCharacter::from_u8(firstByte);
    }

    return ByteDecoder_nextSourceCharacter_uncommon(session, policy);
}

fn ByteDecoder_nextSourceCharacter_uncommon(
    session: &mut Reader,
    policy: NextPolicy,
) -> SourceCharacter {
    let firstByte = ByteBuffer_nextByte(session);

    match firstByte {
        b'\t' => {

            //
            // Handle TAB specially
            //

            incr_diagnostic!(ByteDecoder_TabCount);

            if feature::COMPUTE_SOURCE {
                session.src().tab();
            }

            return SourceCharacter::from_u8(firstByte);

        }
            //
            // Handle CR specially
            //
        b'\r' => {

            incr_diagnostic!(ByteDecoder_CarriageReturnCount);

            if ByteBuffer_currentByte(session) == b'\n' {

                ByteBuffer_nextByte(session);

                if feature::COMPUTE_SOURCE {
                    session.src().windowsNewline();
                }

                return SourceCharacter::from(CodePoint::CRLF);
            }

            if feature::COMPUTE_SOURCE {
                session.src().newline();
            }

            if session.check_issues && policy.contains(ENABLE_CHARACTER_DECODING_ISSUES) {

                //
                // No CodeAction here
                //

                //
                // FIXME: no way to do endOfPreviousLine()
                //
                let currentSourceCharacterStartLoc = session.SrcLoc;

                let I = EncodingIssue(
                    IssueTag::UnexpectedCarriageReturn,
                    format!("Unexpected ``\\r`` character."),
                    Severity::Warning,
                    Span::new(currentSourceCharacterStartLoc, session.SrcLoc),
                    1.0,
                    vec![],
                    vec![]
                );

                session.addIssue(I);
            }

            return SourceCharacter::from('\r');
        }
            //
            // 1 byte UTF-8 sequence
            //
        0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 |
        0x08 | /*   TAB*/ /*    LF*/ 0x0b | 0x0c | /*    CR*/ 0x0e | 0x0f |
        0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 |
        0x18 | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e | 0x1f |
        0x7f => {

            incr_diagnostic!(ByteDecoder_1ByteCount);

            //
            // Valid
            //

            let firstByte = CodePoint::from(char::from(firstByte));

            return ByteDecoder_validStrange(session, firstByte, policy);
        }

            //
            // 2 byte UTF-8 sequence
            //
        /*                 */ 0xc2 | 0xc3 | 0xc4 | 0xc5 | 0xc6 | 0xc7 |
        0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf |
        0xd0 | 0xd1 | 0xd2 | 0xd3 | 0xd4 | 0xd5 | 0xd6 | 0xd7 |
        0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf => {

            incr_diagnostic!(ByteDecoder_2ByteCount);

            //
            // Buffer is possibly already pointing to EOF
            //

            let mark = session.mark();

            let tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let secondByte = tmp;

            if !(0x80 <= secondByte && secondByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            //
            // Valid
            //

            let decoded = ((u32::from(firstByte) & 0x1f) << 6) | (u32::from(secondByte) & 0x3f);
            let decoded = CodePoint::from_u32(decoded).unwrap();

            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        0xe0 => {

            incr_diagnostic!(ByteDecoder_3ByteCount);

            //
            // Buffer is possibly already pointing to EOF
            //

            let mut mark = session.mark();

            let mut tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let secondByte = tmp;

            if !(0xa0 <= secondByte && secondByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let thirdByte = tmp;

            if !(0x80 <= thirdByte && thirdByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            //
            // Valid
            //

            let decoded =
                ((u32::from(firstByte) & 0x0f) << 12)
                | ((u32::from(secondByte) & 0x3f) << 6)
                | (u32::from(thirdByte) & 0x3f);
            let decoded = CodePoint::from_u32(decoded).unwrap();

            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        /*      */ 0xe1 | 0xe2 | 0xe3 | 0xe4 | 0xe5 | 0xe6 | 0xe7 |
        0xe8 | 0xe9 | 0xea | 0xeb | 0xec | 0xed | 0xee | 0xef => {

            incr_diagnostic!(ByteDecoder_3ByteCount);

            //
            // Buffer is possibly already pointing to EOF
            //

            let mut mark = session.mark();

            let mut tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let secondByte = tmp;

            if !(0x80 <= secondByte && secondByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let thirdByte = tmp;

            if !(0x80 <= thirdByte && thirdByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            //
            // Valid
            //

            let decoded =
                ((u32::from(firstByte) & 0x0f) << 12)
                | ((u32::from(secondByte) & 0x3f) << 6)
                | (u32::from(thirdByte) & 0x3f);

            if utils::isStraySurrogate(decoded) {

                //
                // Stray surrogate
                //

                return ByteDecoder_straySurrogate(session, mark.src_loc, policy);
            }

            let decoded = CodePoint::from_u32(decoded).unwrap();

            if decoded == CODEPOINT_BOM {

                //
                // BOM
                //

                return ByteDecoder_bom(session, mark.src_loc, policy);
            }

            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        0xf0 => {

            incr_diagnostic!(ByteDecoder_4ByteCount);

            //
            // Buffer is possibly already pointing to EOF
            //

            let mut mark = session.mark();

            let mut tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let secondByte = tmp;

            if !(0x90 <= secondByte && secondByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let thirdByte = tmp;

            if !(0x80 <= thirdByte && thirdByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete3ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let fourthByte = tmp;

            if !(0x80 <= fourthByte && fourthByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete3ByteSequence(session, mark.src_loc, policy);
            }

            //
            // Valid
            //

            let decoded =
                ((u32::from(firstByte) & 0x07) << 18)
                | ((u32::from(secondByte) & 0x3f) << 12)
                | ((u32::from(thirdByte) & 0x3f) << 6)
                | ((u32::from(fourthByte) & 0x3f));
            let decoded = CodePoint::from_u32(decoded).unwrap();

            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        0xf1 | 0xf2 | 0xf3 => {

            incr_diagnostic!(ByteDecoder_4ByteCount);

            //
            // Buffer is possibly already pointing to EOF
            //

            let mut mark = session.mark();

            let mut tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let secondByte = tmp;

            if !(0x80 <= secondByte && secondByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let thirdByte = tmp;

            if !(0x80 <= thirdByte && thirdByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete3ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let fourthByte = tmp;

            if !(0x80 <= fourthByte && fourthByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete3ByteSequence(session, mark.src_loc, policy);
            }

            //
            // Valid
            //

            let decoded: u32 =
                ( (u32::from(firstByte) & 0x07) << 18)
                | ((u32::from(secondByte) & 0x3f) << 12)
                | ((u32::from(thirdByte) & 0x3f) << 6)
                | ((u32::from(fourthByte) & 0x3f));
            let decoded = CodePoint::from_u32(decoded).unwrap();

            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        0xf4 => {

            incr_diagnostic!(ByteDecoder_4ByteCount);

            //
            // Buffer is possibly already pointing to EOF
            //

            let mut mark = session.mark();

            let mut tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let secondByte = tmp;

            if !(0x80 <= secondByte && secondByte <= 0x8f) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete1ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let thirdByte = tmp;

            if !(0x80 <= thirdByte && thirdByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete2ByteSequence(session, mark.src_loc, policy);
            }

            mark = session.mark();

            tmp = ByteBuffer_nextByte(session);

            if session.wasEOF {

                //
                // EOF
                //

                session.seek(mark);

                return ByteDecoder_incomplete3ByteSequence(session, mark.src_loc, policy);
            }

            // Continue

            let fourthByte = tmp;

            if !(0x80 <= fourthByte && fourthByte <= 0xbf) {

                //
                // Incomplete
                //

                session.seek(mark);

                return ByteDecoder_incomplete3ByteSequence(session, mark.src_loc, policy);
            }

            //
            // Valid
            //

            let decoded: u32 = ((u32::from(firstByte) & 0x07) << 18)
                | ((u32::from(secondByte) & 0x3f) << 12)
                | ((u32::from(thirdByte) & 0x3f) << 6)
                | ((u32::from(fourthByte) & 0x3f));
            let decoded = CodePoint::from_u32(decoded).unwrap();

            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // Not a valid UTF-8 start, handle specially
            //
        0xff => {

            incr_diagnostic!(ByteDecoder_FFCount);

            if session.wasEOF {

                //
                // Do not increment Column
                //

                return SourceCharacter::from(CodePoint::EndOfFile);
            }

            //
            // Incomplete
            //

            return ByteDecoder_incomplete1ByteSequence(session, session.SrcLoc, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        _ => {

            incr_diagnostic!(ByteDecoder_Incomplete1ByteCount);

            //
            // Incomplete
            //

            return ByteDecoder_incomplete1ByteSequence(session, session.SrcLoc, policy);
        }
    }
}

fn ByteDecoder_strangeWarning(
    session: &mut Reader,
    decoded: CodePoint,
    currentSourceCharacterStartLoc: Location,
    policy: NextPolicy,
) {
    let currentSourceCharacterEndLoc = session.SrcLoc;

    let safeAndGraphicalStr = decoded.safeAndGraphicalString();
    let graphicalStr = decoded.graphicalString();

    let Src =
        Span::new(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);

    let mut Actions: Vec<CodeAction> = Vec::new();

    let c = WLCharacter::new_with_escape(decoded, Escape::None);

    let certainCharacterActions =
        utils::certainCharacterReplacementActions(c, Src);

    for A in certainCharacterActions {
        Actions.push(A);
    }

    //
    // graphical version
    //
    Actions.push(CodeAction::replace_text(
        format!("Replace with ``{graphicalStr}``"),
        Src,
        graphicalStr,
    ));

    //
    // any ASCII replacements
    //
    for r in crate::long_names::asciiReplacements(decoded) {
        Actions.push(CodeAction::replace_text(
            format!(
                "Replace with ``{}``",
                crate::long_names::replacementGraphical(r.clone())
            ),
            Src,
            r,
        ));
    }

    if policy.contains(STRING_OR_COMMENT) {
        //
        // reduce severity of unexpected characters inside strings or comments
        //

        let I = EncodingIssue(
            IssueTag::UnexpectedCharacter,
            format!("Unexpected character: ``{safeAndGraphicalStr}``."),
            Severity::Remark,
            Src,
            0.95,
            Actions,
            Vec::new(),
        );

        session.addIssue(I);
    } else if c.isStrangeWhitespace() || c.isMBStrangeWhitespace() {

        // Do nothing.
    } else {
        let I = EncodingIssue(
            IssueTag::UnexpectedCharacter,
            format!("Unexpected character: ``{safeAndGraphicalStr}``."),
            Severity::Warning,
            Src,
            0.95,
            Actions,
            Vec::new(),
        );

        session.addIssue(I);
    }
}

fn ByteDecoder_nonASCIIWarning(
    session: &mut Reader,
    decoded: CodePoint,
    currentSourceCharacterStartLoc: Location,
) {
    let currentSourceCharacterEndLoc = session.SrcLoc;

    let safeAndGraphicalStr = decoded.safeAndGraphicalString();
    let graphicalStr = decoded.graphicalString();

    let Src =
        Span::new(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);

    let mut Actions: Vec<CodeAction> = Vec::new();

    Actions.push(CodeAction::replace_text(
        format!("Replace with ``{graphicalStr}``"),
        Src,
        graphicalStr,
    ));

    for r in crate::long_names::asciiReplacements(decoded) {
        Actions.push(CodeAction::replace_text(
            format!(
                "Replace with ``{}``",
                crate::long_names::replacementGraphical(r.clone())
            ),
            Src,
            r,
        ));
    }

    let I = EncodingIssue(
        IssueTag::NonASCIICharacter,
        format!("Non-ASCII character: ``{safeAndGraphicalStr}``."),
        Severity::Remark,
        Src,
        1.0,
        Actions,
        Vec::new(),
    );

    session.addIssue(I);
}

fn ByteDecoder_validStrange(
    session: &mut Reader,
    decoded: CodePoint,
    policy: NextPolicy,
) -> SourceCharacter {
    if feature::COMPUTE_SOURCE {
        session.src().increment();
    }

    if session.check_issues {
        let currentSourceCharacterStartLoc = session.SrcLoc.previous();

        ByteDecoder_strangeWarning(
            session,
            decoded,
            currentSourceCharacterStartLoc,
            policy,
        );
    }

    return decoded;
}

fn ByteDecoder_validMB(
    session: &mut Reader,
    decoded: CodePoint,
    policy: NextPolicy,
) -> SourceCharacter {
    if feature::COMPUTE_SOURCE {
        session.src().increment();
    }

    if session.check_issues {
        let currentSourceCharacterStartLoc = session.SrcLoc.previous();

        if crate::utils::isMBStrange(decoded) {
            ByteDecoder_strangeWarning(
                session,
                decoded,
                currentSourceCharacterStartLoc,
                policy,
            );
        } else if session.encoding_mode == EncodingMode::Normal {
            ByteDecoder_nonASCIIWarning(
                session,
                decoded,
                currentSourceCharacterStartLoc,
            );
        }
    }

    return decoded;
}

fn ByteDecoder_incomplete1ByteSequence(
    session: &mut Reader,
    errSrcLoc: Location,
    _policy: NextPolicy,
) -> SourceCharacter {
    ByteDecoder_incompleteByteSequence(
        session,
        errSrcLoc,
        CodePoint::Unsafe1ByteUtf8Sequence,
    )
}

fn ByteDecoder_incomplete2ByteSequence(
    session: &mut Reader,
    errSrcLoc: Location,
    _policy: NextPolicy,
) -> SourceCharacter {
    ByteDecoder_incompleteByteSequence(
        session,
        errSrcLoc,
        CodePoint::Unsafe2ByteUtf8Sequence,
    )
}

fn ByteDecoder_incomplete3ByteSequence(
    session: &mut Reader,
    errSrcLoc: Location,
    _policy: NextPolicy,
) -> SourceCharacter {
    ByteDecoder_incompleteByteSequence(
        session,
        errSrcLoc,
        CodePoint::Unsafe3ByteUtf8Sequence,
    )
}

fn ByteDecoder_incompleteByteSequence(
    session: &mut Reader,
    errSrcLoc: Location,
    errChar: SourceCharacter,
) -> SourceCharacter {
    if feature::COMPUTE_SOURCE {
        session.src().increment();
    }

    if session.check_issues {
        //
        // No CodeAction here
        //

        let I = EncodingIssue(
            IssueTag::IncompleteUTF8Sequence,
            "Incomplete UTF-8 sequence.".into(),
            Severity::Fatal,
            Span::new(errSrcLoc, errSrcLoc.next()),
            1.0,
            vec![],
            vec![],
        );

        session.addIssue(I);
    }

    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //

    session.setUnsafeCharacterEncodingFlag(
        UnsafeCharacterEncoding::IncompleteUTF8Sequence,
    );

    return errChar;
}

//
// Related bugs: 376155
//
fn ByteDecoder_straySurrogate(
    session: &mut Reader,
    errSrcLoc: Location,
    _policy: NextPolicy,
) -> SourceCharacter {
    if feature::COMPUTE_SOURCE {
        session.src().increment();
    }

    if session.check_issues {
        //
        // No CodeAction here
        //

        let I = EncodingIssue(
            IssueTag::StraySurrogate,
            "Stray surrogate.".into(),
            Severity::Fatal,
            Span::new(errSrcLoc, errSrcLoc.next()),
            1.0,
            vec![],
            vec![],
        );

        session.addIssue(I);
    }

    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //

    session.setUnsafeCharacterEncodingFlag(
        UnsafeCharacterEncoding::StraySurrogate,
    );

    return CodePoint::from(CodePoint::Unsafe3ByteUtf8Sequence);
}

fn ByteDecoder_bom(
    session: &mut Reader,
    errSrcLoc: Location,
    _policy: NextPolicy,
) -> SourceCharacter {
    if feature::COMPUTE_SOURCE {
        session.src().increment();
    }

    if session.check_issues {
        //
        // No CodeAction here
        //

        let I = EncodingIssue(
            IssueTag::BOM,
            "BOM.".into(),
            Severity::Fatal,
            Span::new(errSrcLoc, errSrcLoc.next()),
            1.0,
            vec![],
            vec![],
        );

        session.addIssue(I);
    }

    session.setUnsafeCharacterEncodingFlag(UnsafeCharacterEncoding::BOM);

    return SourceCharacter::from(CodePoint::Unsafe3ByteUtf8Sequence);
}

impl SourceConvention {
    pub fn newSourceLocation(&self) -> Location {
        match self {
            SourceConvention::LineColumn => Location::LineColumn(LineColumn(
                NonZeroU32::MIN,
                NonZeroU32::MIN,
            )),
            SourceConvention::CharacterIndex => Location::CharacterIndex(1),
        }
    }
}

/// How to manage advancing through [`Location`]s
impl<'t> SourceManager<'t> {
    fn newline(&mut self) {
        match self.loc {
            Location::LineColumn(LineColumn(line, column)) => {
                *line = non_zero_u32_incr(*line);
                *column = NonZeroU32::MIN;
            },
            Location::CharacterIndex(index) => {
                *index += 1;
            },
        }
    }

    fn windowsNewline(&mut self) {
        match self.loc {
            Location::LineColumn(LineColumn(line, column)) => {
                *line = non_zero_u32_incr(*line);
                *column = NonZeroU32::MIN;
            },
            Location::CharacterIndex(index) => {
                *index += 2;
            },
        }
    }

    fn tab(&mut self) {
        match self.loc {
            Location::LineColumn(LineColumn(_, column)) => {
                let currentTabStop =
                    self.tab_width * ((column.get() - 1) / self.tab_width) + 1;

                *column =
                    NonZeroU32::new(currentTabStop + self.tab_width).unwrap();
            },
            Location::CharacterIndex(index) => {
                *index += 1;
            },
        }
    }

    fn increment(&mut self) {
        match self.loc {
            Location::LineColumn(LineColumn(_, column)) => {
                *column = non_zero_u32_incr(*column)
            },
            Location::CharacterIndex(index) => *index += 1,
        }
    }
}
