
#include "ByteDecoder.h"

#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for isMBNonCharacter, etc.
#include "CodePoint.h" // for CODEPOINT_REPLACEMENT_CHARACTER, CODEPOINT_CRLF, etc.
#include "LongNames.h"
#include "API.h" // for ENCODINGMODE

ByteDecoder::ByteDecoder() : Issues(), status(), srcConventionManager(), encodingMode(), lastBuf(), lastLoc(), SrcLoc() {}

void ByteDecoder::init(SourceConvention srcConvention, uint32_t TabWidth, EncodingMode encodingModeIn) {
    
    Issues.clear();
    
    status = UTF8STATUS_NORMAL;
    
    lastBuf = nullptr;
    lastLoc = SourceLocation();
    
    switch (srcConvention) {
        case SOURCECONVENTION_LINECOLUMN:
            srcConventionManager = SourceConventionManagerPtr(new LineColumnManager(TabWidth));
            break;
        case SOURCECONVENTION_SOURCECHARACTERINDEX:
            srcConventionManager = SourceConventionManagerPtr(new SourceCharacterIndexManager());
            break;
        case SOURCECONVENTION_UNKNOWN:
            assert(false);
            break;
    }
    
    SrcLoc = srcConventionManager->newSourceLocation();

    encodingMode = encodingModeIn;
}

void ByteDecoder::deinit() {
    
    Issues.clear();
}

//
// https://unicodebook.readthedocs.io/issues.html#strict-utf8-decoder
//
// Table 3.1B. Legal UTF-8 Byte Sequences
// http://www.unicode.org/versions/corrigendum1.html
//
// with surrogates disallowed
//
// CODE POINTS         1ST BYTE    2ND BYTE    3RD BYTE    4TH BYTE
//   U+0000....U+007F    00..7F
//   U+0080....U+07FF    C2..DF      80..BF
//   U+0800....U+0FFF        E0      A0..BF      80..BF
//   U+1000....U+CFFF    E1..EC      80..BF      80..BF
//   U+D000....U+D7FF        ED      80..9F      80..BF
//
// disallowed surrogates:
//   U+D800....U+DFFF        ED      A0..BF      80..BF
//
//   U+E000....U+FFFF    EE..EF      80..BF      80..BF
//  U+10000...U+3FFFF        F0      90..BF      80..BF      80..BF
//  U+40000...U+FFFFF    F1..F3      80..BF      80..BF      80..BF
// U+100000..U+10FFFF        F4      80..8F      80..BF      80..BF
//
SourceCharacter ByteDecoder::nextSourceCharacter0(NextPolicy policy) {

#if !NISSUES
    auto currentSourceCharacterStartLoc = SrcLoc;
#endif // !NISSUES
    
    auto firstByte = TheByteBuffer->nextByte0();
    
    switch (firstByte) {
            //
            // Handle CR specially
            //
        case 0x0d: {
            
            if (TheByteBuffer->currentByte() == 0x0a) {
                
                TheByteBuffer->nextByte();
                
                srcConventionManager->windowsNewline(SrcLoc);
                
                return SourceCharacter(CODEPOINT_CRLF);
            }
            
            srcConventionManager->newline(SrcLoc);
            
#if !NISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                //
                // No CodeAction here
                //
                
                auto I = IssuePtr(new EncodingIssue(ENCODINGISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", ENCODINGISSUESEVERITY_WARNING, Source(currentSourceCharacterStartLoc), 1.0));
                
                addIssue(std::move(I));
            }
#endif // !NISSUES
            
            return SourceCharacter('\r');
        }
            //
            // Handle LF also
            //
        case 0x0a:
            
            srcConventionManager->newline(SrcLoc);
            
            return SourceCharacter('\n');
        case 0x09:
            
            // Handle TAB specially
            
            srcConventionManager->tab(SrcLoc);
            
            return SourceCharacter(firstByte);
            
            //
            // 1 byte UTF-8 sequence
            //
        case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
        case 0x08: /*   TAB*/ /*    LF*/ case 0x0b: case 0x0c: /*    CR*/ case 0x0e: case 0x0f:
        case 0x10: case 0x11: case 0x12: case 0x13: case 0x14: case 0x15: case 0x16: case 0x17:
        case 0x18: case 0x19: case 0x1a: case 0x1b: case 0x1c: case 0x1d: case 0x1e: case 0x1f:
        case 0x7f: {
            
            // Valid
            
            const auto decoded = firstByte;
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            
            //
            // 1 byte UTF-8 sequence
            //
        case 0x20: case 0x21: case 0x22: case 0x23: case 0x24: case 0x25: case 0x26: case 0x27:
        case 0x28: case 0x29: case 0x2a: case 0x2b: case 0x2c: case 0x2d: case 0x2e: case 0x2f:
        case 0x30: case 0x31: case 0x32: case 0x33: case 0x34: case 0x35: case 0x36: case 0x37:
        case 0x38: case 0x39: case 0x3a: case 0x3b: case 0x3c: case 0x3d: case 0x3e: case 0x3f:
        case 0x40: case 0x41: case 0x42: case 0x43: case 0x44: case 0x45: case 0x46: case 0x47:
        case 0x48: case 0x49: case 0x4a: case 0x4b: case 0x4c: case 0x4d: case 0x4e: case 0x4f:
        case 0x50: case 0x51: case 0x52: case 0x53: case 0x54: case 0x55: case 0x56: case 0x57:
        case 0x58: case 0x59: case 0x5a: case 0x5b: case 0x5c: case 0x5d: case 0x5e: case 0x5f:
        case 0x60: case 0x61: case 0x62: case 0x63: case 0x64: case 0x65: case 0x66: case 0x67:
        case 0x68: case 0x69: case 0x6a: case 0x6b: case 0x6c: case 0x6d: case 0x6e: case 0x6f:
        case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76: case 0x77:
        case 0x78: case 0x79: case 0x7a: case 0x7b: case 0x7c: case 0x7d: case 0x7e: /*   DEL*/ {
            
            // Valid
            
            srcConventionManager->increment(SrcLoc);
            
            const auto decoded = firstByte;
            
            return SourceCharacter(decoded);
        }
            
            //
            // 2 byte UTF-8 sequence
            //
        /*                 */ case 0xc2: case 0xc3: case 0xc4: case 0xc5: case 0xc6: case 0xc7:
        case 0xc8: case 0xc9: case 0xca: case 0xcb: case 0xcc: case 0xcd: case 0xce: case 0xcf:
        case 0xd0: case 0xd1: case 0xd2: case 0xd3: case 0xd4: case 0xd5: case 0xd6: case 0xd7:
        case 0xd8: case 0xd9: case 0xda: case 0xdb: case 0xdc: case 0xdd: case 0xde: case 0xdf: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            //
            // Continue
            //
            
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x1f) << 6) | (tmp & 0x3f));
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // 3 byte UTF-8 sequence
            //
        case 0xe0: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0xa0 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
                
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
            
            if (Utils::isBMPNonCharacter(decoded) || Utils::isNonBMPNonCharacter(decoded)) {
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            }
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // 3 byte UTF-8 sequence
            //
        /*      */ case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
        case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: /*      */ case 0xee: case 0xef: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
            
            if (decoded == CODEPOINT_ACTUAL_BOM) {
                
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            
                srcConventionManager->increment(SrcLoc);
                
                return SourceCharacter(CODEPOINT_VIRTUAL_BOM);
            }
            
            if (Utils::isBMPNonCharacter(decoded) || Utils::isNonBMPNonCharacter(decoded)) {
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            }
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // 3 byte UTF-8 sequence
            //
            // Possibly a surrogate
            //
        case 0xed: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0x9f)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                if (0xa0 <= secondByte && secondByte <= 0xbf) {
                    return surrogateReturn(resetLoc, policy);
                }
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
                
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
            
            if (Utils::isBMPNonCharacter(decoded) || Utils::isNonBMPNonCharacter(decoded)) {
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            }
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf0: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x90 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
            
            //
            // Manual test for code points that are too large
            //
            assert(decoded <= 0x10ffff);
            
            if (Utils::isBMPNonCharacter(decoded) || Utils::isNonBMPNonCharacter(decoded)) {
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            }
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf1: case 0xf2: case 0xf3: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
            
            //
            // Manual test for code points that are too large
            //
            assert(decoded <= 0x10ffff);
            
            if (Utils::isBMPNonCharacter(decoded) || Utils::isNonBMPNonCharacter(decoded)) {
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            }
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf4: {
            
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
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            if (!(0x80 <= secondByte && secondByte <= 0x8f)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
                
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            if (!(0x80 <= thirdByte && thirdByte <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            tmp = TheByteBuffer->nextByte0();
                
            if (TheByteBuffer->wasEOF) {
                
                //
                // EOF
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            if (!(0x80 <= tmp && tmp <= 0xbf)) {
                
                //
                // Invalid
                //
                
                TheByteBuffer->buffer = resetBuf;
                TheByteBuffer->wasEOF = resetEOF;
                SrcLoc = resetLoc;
                
                return invalidReturn(resetLoc, policy);
            }
            
            // Valid
            
            const auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
            
            //
            // Manual test for code points that are too large
            //
            assert(decoded <= 0x10ffff);
            
            if (Utils::isBMPNonCharacter(decoded) || Utils::isNonBMPNonCharacter(decoded)) {
                status = UTF8STATUS_NONCHARACTER_OR_BOM;
            }
            
            srcConventionManager->increment(SrcLoc);
            
#if !NISSUES
            {
                if (Utils::isMBStrange(decoded)) {
                    strangeWarning(decoded, currentSourceCharacterStartLoc, policy);
                } else if (encodingMode == ENCODINGMODE_NORMAL) {
                    nonASCIIWarning(decoded, currentSourceCharacterStartLoc);
                }
            }
#endif // !NISSUES
            
            return SourceCharacter(decoded);
        }
            //
            // Not a valid UTF-8 start, handle specially
            //
        case 0xff: {
            
            if (TheByteBuffer->wasEOF) {
                
                //
                // Do not increment Column
                //
                
                return SourceCharacter(CODEPOINT_ENDOFFILE);
            }
            
            //
            // Invalid
            //
            
            return invalidReturn(SrcLoc, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        default: {
            
            //
            // Invalid
            //
            
            return invalidReturn(SrcLoc, policy);
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
    
    auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
    
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
    
    std::string severity;
    if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
        //
        // reduce severity of unexpected characters inside strings or comments
        //
        severity = SYNTAXISSUESEVERITY_REMARK;
    } else {
        severity = SYNTAXISSUESEVERITY_WARNING;
    }
    
    auto I = IssuePtr(new EncodingIssue(ENCODINGISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + safeAndGraphicalStr + "``.", severity, Src, 0.95, std::move(Actions)));
    
    Issues.insert(std::move(I));
}

void ByteDecoder::nonASCIIWarning(codepoint decoded, SourceLocation currentSourceCharacterStartLoc) {
    
    auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
    
    auto safeAndGraphicalStr = SourceCharacter(decoded).safeAndGraphicalString();
    auto graphicalStr = SourceCharacter(decoded).graphicalString();
    
    auto Src = Source(currentSourceCharacterStartLoc, currentSourceCharacterEndLoc);
    
    CodeActionPtrVector Actions;
    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + graphicalStr + "``", Src, graphicalStr)));
    
    for (const auto& r : LongNames::asciiReplacements(decoded)) {
        Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
    }
    
    auto I = IssuePtr(new EncodingIssue(ENCODINGISSUETAG_NONASCIICHARACTER, "Non-ASCII character: ``" + safeAndGraphicalStr + "``.", ENCODINGISSUESEVERITY_REMARK, Src, 1.0, std::move(Actions)));
    
    Issues.insert(std::move(I));
}

//
// Technically, the 3rd byte was not tested for true surrogate or just invalid UTF-8
//
// So for example, this sequence
// 0xed 0xad 0x00
//
// gives the same warning as true surrogte sequence 0xed 0xad 0x80
//
// But it results in different text in the warning message.
//
// Related bugs: 376155
//
SourceCharacter ByteDecoder::surrogateReturn(SourceLocation errSrcLoc, NextPolicy policy) {
    
    status = UTF8STATUS_INVALID;
    
    srcConventionManager->increment(SrcLoc);
    
#if !NISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(ENCODINGISSUETAG_INVALIDCHARACTERENCODING_SURROGATE, "Invalid UTF-8 sequence: Probable surrogate", ENCODINGISSUESEVERITY_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0));
        
        Issues.insert(std::move(I));
    }
#endif // !NISSUES
    
    //
    // Return \[UnknownGlyph] character when there is bad UTF-8 encoding
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    return SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER);
}

SourceCharacter ByteDecoder::invalidReturn(SourceLocation errSrcLoc, NextPolicy policy) {
    
    status = UTF8STATUS_INVALID;
    
    srcConventionManager->increment(SrcLoc);
    
#if !NISSUES
    {
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new EncodingIssue(ENCODINGISSUETAG_INVALIDCHARACTERENCODING, "Invalid UTF-8 sequence.", ENCODINGISSUESEVERITY_FATAL, Source(errSrcLoc, errSrcLoc.next()), 1.0));
        
        Issues.insert(std::move(I));
    }
#endif // !NISSUES
    
    //
    // Return \[UnknownGlyph] character when there is bad UTF-8 encoding
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    // Related bugs: 366106, 376155
    //
    
    return SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER);
}


#if !NISSUES
void ByteDecoder::addIssue(IssuePtr I) {
    Issues.insert(std::move(I));
}

IssuePtrSet& ByteDecoder::getIssues() {
    return Issues;
}
#endif // !NISSUES

void ByteDecoder::setStatus(UTF8Status stat) {
    status = stat;
}

UTF8Status ByteDecoder::getStatus() const {
    return status;
}

void ByteDecoder::clearStatus() {
    status = UTF8STATUS_NORMAL;
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
    auto currentTabStop = TabWidth * ((loc.second - 1) / TabWidth) + 1;
    loc.second = currentTabStop + TabWidth;
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
