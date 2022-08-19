
#include "ByteDecoder.h"

#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for isMBStrange, etc.
#include "CodePoint.h" // for CODEPOINT_CRLF, etc.
#include "LongNames.h"
#include "MyStringRegistration.h"
#include "ParserSession.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS


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


SourceCharacter ByteDecoder_nextSourceCharacter_uncommon(ParserSessionPtr session, NextPolicy policy);

void ByteDecoder_strangeWarning(ParserSessionPtr session, codepoint decoded, NextPolicy policy);
void ByteDecoder_nonASCIIWarning(ParserSessionPtr session, codepoint decoded);

SourceCharacter ByteDecoder_validStrange(ParserSessionPtr session, codepoint decoded, NextPolicy policy);
SourceCharacter ByteDecoder_validMB(ParserSessionPtr session, codepoint decoded, NextPolicy policy);

SourceCharacter ByteDecoder_incomplete1ByteSequence(ParserSessionPtr session, NextPolicy policy);
SourceCharacter ByteDecoder_incomplete2ByteSequence(ParserSessionPtr session, NextPolicy policy);
SourceCharacter ByteDecoder_incomplete3ByteSequence(ParserSessionPtr session, NextPolicy policy);
SourceCharacter ByteDecoder_straySurrogate(ParserSessionPtr session, NextPolicy policy);
SourceCharacter ByteDecoder_bom(ParserSessionPtr session, NextPolicy policy);


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
SourceCharacter ByteDecoder_nextSourceCharacter(ParserSessionPtr session, NextPolicy policy) {
    
    auto firstByte = ByteBuffer_currentByte(session);
    
    if (0x20 <= firstByte && firstByte <= 0x7e) {

        //
        // Valid
        //

#if DIAGNOSTICS
        ByteDecoder_PrintableCount++;
#endif // DIAGNOSTICS
        
        ByteBuffer_nextByte(session);
        
#if COMPUTE_SOURCE
        session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
        
        return SourceCharacter(firstByte);
    }
    
    if (firstByte == 0x0a) {

        //
        // Handle LF specially
        //

#if DIAGNOSTICS
        ByteDecoder_LineFeedCount++;
#endif // DIAGNOSTICS
        
        ByteBuffer_nextByte(session);
        
#if COMPUTE_SOURCE
        session->srcConventionManager->newline(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
        
        return SourceCharacter(firstByte);
    }
        
    return ByteDecoder_nextSourceCharacter_uncommon(session, policy);
}

SourceCharacter ByteDecoder_nextSourceCharacter_uncommon(ParserSessionPtr session, NextPolicy policy) {
    
    auto firstByte = ByteBuffer_nextByte(session);
    
    switch (firstByte) {
        case 0x09: {

            //
            // Handle TAB specially
            //

#if DIAGNOSTICS
            ByteDecoder_TabCount++;
#endif // DIAGNOSTICS
                
            
#if COMPUTE_SOURCE
            session->srcConventionManager->tab(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
            
            return SourceCharacter(firstByte);

        }
            //
            // Handle CR specially
            //
        case 0x0d: {
            
#if DIAGNOSTICS
            ByteDecoder_CarriageReturnCount++;
#endif // DIAGNOSTICS
            
            if (ByteBuffer_currentByte(session) == 0x0a) {
                
                ByteBuffer_nextByte(session);
                
#if COMPUTE_SOURCE
                session->srcConventionManager->windowsNewline(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
                
                return SourceCharacter(CODEPOINT_CRLF);
            }
            
#if COMPUTE_SOURCE
            session->srcConventionManager->newline(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
            
#if CHECK_ISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                //
                // No CodeAction here
                //
                
                //
                // FIXME: no way to do endOfPreviousLine()
                //
                auto currentSourceCharacterStartLoc = session->SrcLoc;
                
                auto I = new EncodingIssue(STRING_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", STRING_WARNING, Source(currentSourceCharacterStartLoc, session->SrcLoc), 1.0, {}, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES
            
            return SourceCharacter('\r');
        }
            //
            // 1 byte UTF-8 sequence
            //
        case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
        case 0x08: /*   TAB*/ /*    LF*/ case 0x0b: case 0x0c: /*    CR*/ case 0x0e: case 0x0f:
        case 0x10: case 0x11: case 0x12: case 0x13: case 0x14: case 0x15: case 0x16: case 0x17:
        case 0x18: case 0x19: case 0x1a: case 0x1b: case 0x1c: case 0x1d: case 0x1e: case 0x1f:
        case 0x7f: {
            
#if DIAGNOSTICS
            ByteDecoder_1ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Valid
            //
            
            return ByteDecoder_validStrange(session, firstByte, policy);
        }
            
            //
            // 2 byte UTF-8 sequence
            //
        /*                 */ case 0xc2: case 0xc3: case 0xc4: case 0xc5: case 0xc6: case 0xc7:
        case 0xc8: case 0xc9: case 0xca: case 0xcb: case 0xcc: case 0xcd: case 0xce: case 0xcf:
        case 0xd0: case 0xd1: case 0xd2: case 0xd3: case 0xd4: case 0xd5: case 0xd6: case 0xd7:
        case 0xd8: case 0xd9: case 0xda: case 0xdb: case 0xdc: case 0xdd: case 0xde: case 0xdf: {
            
#if DIAGNOSTICS
            ByteDecoder_2ByteCount++;
#endif // DIAGNOSTICS
            
            auto secondByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete1ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            //
            // Valid
            //
            
            auto decoded = (((firstByte & 0x1f) << 6) | (secondByte & 0x3f));
            
            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        case 0xe0: {
            
#if DIAGNOSTICS
            ByteDecoder_3ByteCount++;
#endif // DIAGNOSTICS
            
            auto secondByte = ByteBuffer_currentByte(session);
            
            if (!(0xa0 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete1ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto thirdByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete2ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            //
            // Valid
            //
            
            auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (thirdByte & 0x3f));
            
            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        /*      */ case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
        case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: case 0xed: case 0xee: case 0xef: {
            
#if DIAGNOSTICS
            ByteDecoder_3ByteCount++;
#endif // DIAGNOSTICS
            
            auto secondByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete1ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto thirdByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete2ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            //
            // Valid
            //
            
            auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (thirdByte & 0x3f));
            
            if (Utils::isStraySurrogate(decoded)) {

                //
                // Stray surrogate
                //

                return ByteDecoder_straySurrogate(session, policy);
            }
            
            if (decoded == CODEPOINT_BOM) {
                
                //
                // BOM
                //
                
                return ByteDecoder_bom(session, policy);
            }
            
            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf0: {
            
#if DIAGNOSTICS
            ByteDecoder_4ByteCount++;
#endif // DIAGNOSTICS
            
            auto secondByte = ByteBuffer_currentByte(session);
            
            if (!(0x90 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete1ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto thirdByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete2ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto fourthByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= fourthByte && fourthByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete3ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            //
            // Valid
            //
            
            auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((fourthByte & 0x3f)));
            
            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf1: case 0xf2: case 0xf3: {
            
#if DIAGNOSTICS
            ByteDecoder_4ByteCount++;
#endif // DIAGNOSTICS
            
            auto secondByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete1ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto thirdByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete2ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto fourthByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= fourthByte && fourthByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete3ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            //
            // Valid
            //
            
            auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((fourthByte & 0x3f)));
            
            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf4: {
            
#if DIAGNOSTICS
            ByteDecoder_4ByteCount++;
#endif // DIAGNOSTICS
            
            auto secondByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= secondByte && secondByte <= 0x8f)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete1ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto thirdByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete2ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            auto fourthByte = ByteBuffer_currentByte(session);
            
            if (!(0x80 <= fourthByte && fourthByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                return ByteDecoder_incomplete3ByteSequence(session, policy);
            }
            
            //
            // Continue
            //
            
            ByteBuffer_nextByte(session);
            
            //
            // Valid
            //
            
            auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((fourthByte & 0x3f)));
            
            return ByteDecoder_validMB(session, decoded, policy);
        }
            //
            // Not a valid UTF-8 start, handle specially
            //
        case 0xff: {
            
#if DIAGNOSTICS
            ByteDecoder_FFCount++;
#endif // DIAGNOSTICS
            
            if (session->buffer == session->end) {
                
                //
                // EOF
                //
                // Do not increment Column
                //
                
                return SourceCharacter(CODEPOINT_ENDOFFILE);
            }
            
            //
            // Incomplete
            //
            
            return ByteDecoder_incomplete1ByteSequence(session, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        default: {
            
#if DIAGNOSTICS
            ByteDecoder_Incomplete1ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Incomplete
            //
            
            return ByteDecoder_incomplete1ByteSequence(session, policy);
        }
    }
}

//
// Postcondition: lastBuf is set to the last value of buffer
// Postcondition: lastLoc is set to the last value of SrcLoc
//
SourceCharacter ByteDecoder_currentSourceCharacter(ParserSessionPtr session, NextPolicy policy) {
    
    auto resetBuf = session->buffer;
    auto resetLoc = session->SrcLoc;
    
    auto c = ByteDecoder_nextSourceCharacter(session, policy);
    
    session->buffer = resetBuf;
    session->SrcLoc = resetLoc;
    
    return c;
}

void ByteDecoder_strangeWarning(ParserSessionPtr session, codepoint decoded, SourceLocation currentSourceCharacterStartLoc, NextPolicy policy) {
    
    auto currentSourceCharacterEndLoc = session->SrcLoc;
    
    auto safeAndGraphicalStr = SourceCharacter(decoded).safeAndGraphicalString();
    auto graphicalStr = SourceCharacter(decoded).graphicalString();
    
    auto Src = Source(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);
    
    CodeActionPtrVector Actions;
    
    auto c = WLCharacter(decoded, ESCAPE_NONE);
    
    auto certainCharacterActions = Utils::certainCharacterReplacementActions(c, Src);
    
    for (auto& A : certainCharacterActions) {
        Actions.push_back(A);
    }
    
    //
    // graphical version
    //
    Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + graphicalStr + "``", Src, graphicalStr));
    
    //
    // any ASCII replacements
    //
    for (const auto& r : LongNames::asciiReplacements(decoded)) {
        Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
    }
    
    if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
        
        //
        // reduce severity of unexpected characters inside strings or comments
        //
        
        auto I = new EncodingIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + safeAndGraphicalStr + "``.", STRING_REMARK, Src, 0.95, Actions, {});
        
        session->addIssue(I);
        
    } else if (c.isStrangeWhitespace() || c.isMBStrangeWhitespace()) {
        
        ;
        
    } else {
        
        auto I = new EncodingIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + safeAndGraphicalStr + "``.", STRING_WARNING, Src, 0.95, Actions, {});
        
        session->addIssue(I);
    }
}

void ByteDecoder_nonASCIIWarning(ParserSessionPtr session, codepoint decoded, SourceLocation currentSourceCharacterStartLoc) {
    
    auto currentSourceCharacterEndLoc = session->SrcLoc;
    
    auto safeAndGraphicalStr = SourceCharacter(decoded).safeAndGraphicalString();
    auto graphicalStr = SourceCharacter(decoded).graphicalString();
    
    auto Src = Source(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);
    
    CodeActionPtrVector Actions;
    
    Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + graphicalStr + "``", Src, graphicalStr));
    
    for (const auto& r : LongNames::asciiReplacements(decoded)) {
        Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
    }
    
    auto I = new EncodingIssue(STRING_NONASCIICHARACTER, "Non-ASCII character: ``" + safeAndGraphicalStr + "``.", STRING_REMARK, Src, 1.0, Actions, {});
    
    session->addIssue(I);
}

inline SourceCharacter ByteDecoder_validStrange(ParserSessionPtr session, codepoint decoded, NextPolicy policy) {
    
    auto currentSourceCharacterStartLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        ByteDecoder_strangeWarning(session, decoded, currentSourceCharacterStartLoc, policy);
    }
#endif // CHECK_ISSUES
    
    return SourceCharacter(decoded);
}

inline SourceCharacter ByteDecoder_validMB(ParserSessionPtr session, codepoint decoded, NextPolicy policy) {
    
    auto currentSourceCharacterStartLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        if (Utils::isMBStrange(decoded)) {
            
            ByteDecoder_strangeWarning(session, decoded, currentSourceCharacterStartLoc, policy);
            
        } else if (session->opts.encodingMode == ENCODINGMODE_NORMAL) {
            
            ByteDecoder_nonASCIIWarning(session, decoded, currentSourceCharacterStartLoc);
        }
    }
#endif // CHECK_ISSUES
    
    return SourceCharacter(decoded);
}

SourceCharacter ByteDecoder_incomplete1ByteSequence(ParserSessionPtr session, NextPolicy policy) {
    
    auto startLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = new EncodingIssue(STRING_INCOMPLETEUTF8SEQUENCE, "Incomplete UTF-8 sequence.", STRING_FATAL, Source(startLoc, session->SrcLoc), 1.0, {}, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    session->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE);
}

SourceCharacter ByteDecoder_incomplete2ByteSequence(ParserSessionPtr session, NextPolicy policy) {
    
    auto startLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = new EncodingIssue(STRING_INCOMPLETEUTF8SEQUENCE, "Incomplete UTF-8 sequence.", STRING_FATAL, Source(startLoc, session->SrcLoc), 1.0, {}, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    session->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE);
}

SourceCharacter ByteDecoder_incomplete3ByteSequence(ParserSessionPtr session, NextPolicy policy) {
    
    auto startLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = new EncodingIssue(STRING_INCOMPLETEUTF8SEQUENCE, "Incomplete UTF-8 sequence.", STRING_FATAL, Source(startLoc, session->SrcLoc), 1.0, {}, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    session->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE);
}

//
// Related bugs: 376155
//
SourceCharacter ByteDecoder_straySurrogate(ParserSessionPtr session, NextPolicy policy) {
    
    auto startLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = new EncodingIssue(STRING_STRAYSURROGATE, "Stray surrogate.", STRING_FATAL, Source(startLoc, session->SrcLoc), 1.0, {}, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    session->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_STRAYSURROGATE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE);
}

SourceCharacter ByteDecoder_bom(ParserSessionPtr session, NextPolicy policy) {
    
    auto startLoc = session->SrcLoc;
    
#if COMPUTE_SOURCE
    session->srcConventionManager->increment(session, session->SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = new EncodingIssue(STRING_BOM, "BOM.", STRING_FATAL, Source(startLoc, session->SrcLoc), 1.0, {}, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    session->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_BOM);
    
    return SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE);
}


void SourceConventionManager::increment(ParserSessionPtr session, SourceLocation& loc) {
    loc.second++;
}


SourceLocation LineColumnManager::newSourceLocation() {
    return SourceLocation(1, 1);
}

void LineColumnManager::newline(ParserSessionPtr session, SourceLocation& loc) {
    
    loc.first++;
    
    loc.second = 1;
}

void LineColumnManager::windowsNewline(ParserSessionPtr session, SourceLocation& loc) {
    
    loc.first++;
    
    loc.second = 1;
}

void LineColumnManager::tab(ParserSessionPtr session, SourceLocation& loc) {
    
    auto currentTabStop = session->opts.tabWidth * ((loc.second - 1) / session->opts.tabWidth) + 1;
    
    loc.second = currentTabStop + session->opts.tabWidth;
}


SourceLocation SourceCharacterIndexManager::newSourceLocation() {
    return SourceLocation(0, 1);
}

void SourceCharacterIndexManager::newline(ParserSessionPtr session, SourceLocation& loc) {
    loc.second++;
}

void SourceCharacterIndexManager::windowsNewline(ParserSessionPtr session, SourceLocation& loc) {
    loc.second+=2;
}

void SourceCharacterIndexManager::tab(ParserSessionPtr session, SourceLocation& loc) {
    loc.second++;
}
