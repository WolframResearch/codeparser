
#include "ByteDecoder.h"

#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for isMBStrange, etc.
#include "CodePoint.h" // for CODEPOINT_CRLF, etc.
#include "LongNames.h"
#include "MyString.h"
#include "ParserSession.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

ByteDecoder::ByteDecoder() : srcConventionManager(), lastBuf(), lastLoc(), SrcLoc() {}

void ByteDecoder::init() {
    
    lastBuf = nullptr;
    lastLoc = SourceLocation();
    
#if COMPUTE_SOURCE
    switch (TheParserSession->srcConvention) {
        case SOURCECONVENTION_LINECOLUMN: {
            srcConventionManager = SourceConventionManagerPtr(new LineColumnManager());
            break;
        }
        case SOURCECONVENTION_SOURCECHARACTERINDEX: {
            srcConventionManager = SourceConventionManagerPtr(new SourceCharacterIndexManager());
            break;
        }
        default: {
            assert(false);
            break;
        }
    }
    
    SrcLoc = srcConventionManager->newSourceLocation();
#endif // COMPUTE_SOURCE
}

void ByteDecoder::deinit() {
    
}

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
SourceCharacter ByteDecoder::nextSourceCharacter0(NextPolicy policy) {
    
    auto firstByte = TheByteBuffer->currentByte();
    
    if (0x20 <= firstByte && firstByte <= 0x7e) {

        //
        // Valid
        //

#if DIAGNOSTICS
        ByteDecoder_PrintableCount++;
#endif // DIAGNOSTICS
        
        TheByteBuffer->nextByte0();
        
#if COMPUTE_SOURCE
        srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
        
        return SourceCharacter(firstByte);

    } else if (firstByte == 0x0a) {

        //
        // Handle LF specially
        //

#if DIAGNOSTICS
        ByteDecoder_LineFeedCount++;
#endif // DIAGNOSTICS
        
        TheByteBuffer->nextByte0();
        
#if COMPUTE_SOURCE
        srcConventionManager->newline(SrcLoc);
#endif // COMPUTE_SOURCE
        
        return SourceCharacter(firstByte);

    } else {
        
        return nextSourceCharacter0_uncommon(policy);
    }
}

SourceCharacter ByteDecoder::nextSourceCharacter0_uncommon(NextPolicy policy) {
    
    auto firstByte = TheByteBuffer->nextByte0();
    
    switch (firstByte) {
        case 0x09: {

            //
            // Handle TAB specially
            //

#if DIAGNOSTICS
            ByteDecoder_TabCount++;
#endif // DIAGNOSTICS
                
            
#if COMPUTE_SOURCE
            srcConventionManager->tab(SrcLoc);
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
            
            if (TheByteBuffer->currentByte() == 0x0a) {
                
                TheByteBuffer->nextByte();
                
#if COMPUTE_SOURCE
                srcConventionManager->windowsNewline(SrcLoc);
#endif // COMPUTE_SOURCE
                
                return SourceCharacter(CODEPOINT_CRLF);
            }
            
#if COMPUTE_SOURCE
            srcConventionManager->newline(SrcLoc);
#endif // COMPUTE_SOURCE
            
#if CHECK_ISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                //
                // No CodeAction here
                //
                
                //
                // FIXME: no way to do endOfPreviousLine()
                //
                auto currentSourceCharacterStartLoc = SrcLoc;
                
                auto I = IssuePtr(new EncodingIssue(STRING_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", STRING_WARNING, Source(currentSourceCharacterStartLoc, SrcLoc), 1.0, {}, {}));
                
                TheParserSession->addIssue(std::move(I));
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
            
            return validStrange(firstByte, policy);
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
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
                
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            //
            // Valid
            //
            
            const auto decoded = (((firstByte & 0x1f) << 6) | (secondByte & 0x3f));
            
            return validMB(decoded, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        case 0xe0: {
            
#if DIAGNOSTICS
            ByteDecoder_3ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0xa0 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            //
            // Valid
            //
            
            const auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (thirdByte & 0x3f));
            
            return validMB(decoded, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        /*      */ case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
        case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: case 0xed: case 0xee: case 0xef: {
            
#if DIAGNOSTICS
            ByteDecoder_3ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            //
            // Valid
            //
            
            const auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (thirdByte & 0x3f));
            
            if (Utils::isStraySurrogate(decoded)) {

                //
                // Stray surrogate
                //

                return straySurrogate(resetLoc, policy);
            }
            
            if (decoded == CODEPOINT_BOM) {
                
                //
                // BOM
                //
                
                return bom(resetLoc, policy);
            }
            
            return validMB(decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf0: {
            
#if DIAGNOSTICS
            ByteDecoder_4ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x90 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete3ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto fourthByte = tmp;
            
            if (!(0x80 <= fourthByte && fourthByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete3ByteSequence(resetLoc, policy);
            }
            
            //
            // Valid
            //
            
            const auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((fourthByte & 0x3f)));
            
            return validMB(decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf1: case 0xf2: case 0xf3: {
            
#if DIAGNOSTICS
            ByteDecoder_4ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete3ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto fourthByte = tmp;
            
            if (!(0x80 <= fourthByte && fourthByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete3ByteSequence(resetLoc, policy);
            }
            
            //
            // Valid
            //
            
            const auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((fourthByte & 0x3f)));
            
            return validMB(decoded, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf4: {
            
#if DIAGNOSTICS
            ByteDecoder_4ByteCount++;
#endif // DIAGNOSTICS
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
                
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0x8f)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete1ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
                
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete2ByteSequence(resetLoc, policy);
            }
            
            resetBuf = TheByteBuffer->buffer;
            resetEOF = TheByteBuffer->wasEOF;
            resetLoc = SrcLoc;
            
            tmp = TheByteBuffer->nextByte0();
                
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete3ByteSequence(resetLoc, policy);
            }
            
            // Continue
            
            auto fourthByte = tmp;
            
            if (!(0x80 <= fourthByte && fourthByte <= 0xbf)) {
                
                //
                // Incomplete
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return incomplete3ByteSequence(resetLoc, policy);
            }
            
            //
            // Valid
            //
            
            const auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((fourthByte & 0x3f)));
            
            return validMB(decoded, policy);
        }
            //
            // Not a valid UTF-8 start, handle specially
            //
        case 0xff: {
            
#if DIAGNOSTICS
            ByteDecoder_FFCount++;
#endif // DIAGNOSTICS
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // Do not increment Column
                //
                
                return SourceCharacter(CODEPOINT_ENDOFFILE);
            }
            
            //
            // Incomplete
            //
            
            return incomplete1ByteSequence(SrcLoc, policy);
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
            
            return incomplete1ByteSequence(SrcLoc, policy);
        }
    }
}


SourceCharacter ByteDecoder::currentSourceCharacter(NextPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = SrcLoc;
    
    auto c = nextSourceCharacter0(policy);
    
    lastBuf = TheByteBuffer->buffer;
    lastLoc = SrcLoc;
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    SrcLoc = resetLoc;
    
    return c;
}

void ByteDecoder::strangeWarning(codepoint decoded, SourceLocation currentSourceCharacterStartLoc, NextPolicy policy) {
    
    auto currentSourceCharacterEndLoc = SrcLoc;
    
    auto safeAndGraphicalStr = SourceCharacter(decoded).safeAndGraphicalString();
    auto graphicalStr = SourceCharacter(decoded).graphicalString();
    
    auto Src = Source(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);
    
    CodeActionPtrVector Actions;
    
    auto c = WLCharacter(decoded, ESCAPE_NONE);
    
    auto certainCharacterActions = Utils::certainCharacterReplacementActions(c, Src);
    
    for (auto& A : certainCharacterActions) {
        Actions.push_back(std::move(A));
    }
    
    //
    // graphical version
    //
    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + graphicalStr + "``", Src, graphicalStr)));
    
    //
    // any ASCII replacements
    //
    for (const auto& r : LongNames::asciiReplacements(decoded)) {
        Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
    }
    
    if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
        
        //
        // reduce severity of unexpected characters inside strings or comments
        //
        
        auto I = IssuePtr(new EncodingIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + safeAndGraphicalStr + "``.", STRING_REMARK, Src, 0.95, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
        
    } else if (c.isStrangeWhitespace() || c.isMBStrangeWhitespace()) {
        
        ;
        
    } else {
        
        auto I = IssuePtr(new EncodingIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + safeAndGraphicalStr + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
}

void ByteDecoder::nonASCIIWarning(codepoint decoded, SourceLocation currentSourceCharacterStartLoc) {
    
    auto currentSourceCharacterEndLoc = SrcLoc;
    
    auto safeAndGraphicalStr = SourceCharacter(decoded).safeAndGraphicalString();
    auto graphicalStr = SourceCharacter(decoded).graphicalString();
    
    auto Src = Source(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);
    
    CodeActionPtrVector Actions;
    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + graphicalStr + "``", Src, graphicalStr)));
    
    for (const auto& r : LongNames::asciiReplacements(decoded)) {
        Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
    }
    
    auto I = IssuePtr(new EncodingIssue(STRING_NONASCIICHARACTER, "Non-ASCII character: ``" + safeAndGraphicalStr + "``.", STRING_REMARK, Src, 1.0, std::move(Actions), {}));
    
    TheParserSession->addIssue(std::move(I));
}

inline SourceCharacter ByteDecoder::validStrange(codepoint decoded, NextPolicy policy) {
    
#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        auto currentSourceCharacterStartLoc = SrcLoc.previous();
        
        strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
    }
#endif // CHECK_ISSUES
    
    return SourceCharacter(decoded);
}

inline SourceCharacter ByteDecoder::validMB(codepoint decoded, NextPolicy policy) {
    
#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        auto currentSourceCharacterStartLoc = SrcLoc.previous();
        
        if (Utils::isMBStrange(decoded)) {
            strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
        } else if (TheParserSession->encodingMode == ENCODINGMODE_NORMAL) {
            nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
        }
    }
#endif // CHECK_ISSUES
    
    return SourceCharacter(decoded);
}

SourceCharacter ByteDecoder::incomplete1ByteSequence(SourceLocation errSrcLoc, NextPolicy policy) {
    
#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(STRING_INCOMPLETEUTF8SEQUENCE, "Incomplete UTF-8 sequence.", STRING_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0, {}, {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    TheParserSession->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE);
}

SourceCharacter ByteDecoder::incomplete2ByteSequence(SourceLocation errSrcLoc, NextPolicy policy) {
    
#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(STRING_INCOMPLETEUTF8SEQUENCE, "Incomplete UTF-8 sequence.", STRING_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0, {}, {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    TheParserSession->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE);
}

SourceCharacter ByteDecoder::incomplete3ByteSequence(SourceLocation errSrcLoc, NextPolicy policy) {
    
#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(STRING_INCOMPLETEUTF8SEQUENCE, "Incomplete UTF-8 sequence.", STRING_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0, {}, {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    TheParserSession->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE);
}

//
// Related bugs: 376155
//
SourceCharacter ByteDecoder::straySurrogate(SourceLocation errSrcLoc, NextPolicy policy) {
    
#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(STRING_STRAYSURROGATE, "Stray surrogate.", STRING_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0, {}, {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // CHECK_ISSUES
    
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    TheParserSession->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_STRAYSURROGATE);
    
    return SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE);
}

SourceCharacter ByteDecoder::bom(SourceLocation errSrcLoc, NextPolicy policy) {

#if COMPUTE_SOURCE
    srcConventionManager->increment(SrcLoc);
#endif // COMPUTE_SOURCE
    
#if CHECK_ISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(STRING_BOM, "BOM.", STRING_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0, {}, {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // CHECK_ISSUES
    
    TheParserSession->setUnsafeCharacterEncodingFlag(UNSAFECHARACTERENCODING_BOM);
    
    return SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE);
}

ByteDecoderPtr TheByteDecoder = nullptr;


void SourceConventionManager::increment(SourceLocation& loc) {
    loc.second++;
};


SourceLocation LineColumnManager::newSourceLocation() {
    return SourceLocation(1, 1);
};

void LineColumnManager::newline(SourceLocation& loc) {
    
    loc.first++;
    
    loc.second = 1;
};

void LineColumnManager::windowsNewline(SourceLocation& loc) {
    
    loc.first++;
    
    loc.second = 1;
};

void LineColumnManager::tab(SourceLocation& loc) {
    
    auto currentTabStop = TheParserSession->tabWidth * ((loc.second - 1) / TheParserSession->tabWidth) + 1;
    
    loc.second = currentTabStop + TheParserSession->tabWidth;
};


SourceLocation SourceCharacterIndexManager::newSourceLocation() {
    return SourceLocation(0, 1);
};

void SourceCharacterIndexManager::newline(SourceLocation& loc) {
    loc.second++;
};

void SourceCharacterIndexManager::windowsNewline(SourceLocation& loc) {
    loc.second+=2;
};

void SourceCharacterIndexManager::tab(SourceLocation& loc) {
    loc.second++;
};
