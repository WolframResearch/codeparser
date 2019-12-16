
#include "ByteDecoder.h"

#include "CodePoint.h" // for CODEPOINT_REPLACEMENT_CHARACTER

ByteDecoder::ByteDecoder() : Issues(), error(), lastBuf(), lastLoc(), SrcLoc() {}

void ByteDecoder::init() {
    
    Issues.clear();
    
    error = false;
    
    SrcLoc = SourceLocation(1, 1);
}

void ByteDecoder::deinit() {
    
    Issues.clear();
}

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
// https://unicodebook.readthedocs.io/issues.html#strict-utf8-decoder
//
SourceCharacter ByteDecoder::nextSourceCharacter0(NextCharacterPolicy policy) {

#if !NISSUES
    auto currentSourceCharacterLoc = SrcLoc;
#endif // NISSUES
    
    auto firstByte = TheByteBuffer->nextByte0();
    
    switch (firstByte) {
            //
            // Handle CR specially
            //
        case 0x0d: {
            
            if (TheByteBuffer->currentByte() == '\n') {
                
                TheByteBuffer->nextByte();
                
                SrcLoc.Line++;
                SrcLoc.Column = 1;
                
                return SourceCharacter(CODEPOINT_CRLF);
            }
            
            SrcLoc.Line++;
            SrcLoc.Column = 1;
            
#if !NISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                //
                // No CodeAction here
                //
                
                auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(currentSourceCharacterLoc), 0.0, {}));
                
                addIssue(std::move(I));
            }
#endif // NISSUES
            
            return SourceCharacter('\r');
        }
        case 0x0a: {
            
            SrcLoc.Line++;
            SrcLoc.Column = 1;
            
            return SourceCharacter('\n');
        }
            //
            // 1 byte UTF-8 sequence
            //
        case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
        case 0x08: case 0x09: /*    LF*/ case 0x0b: case 0x0c: /*    CR*/  case 0x0e: case 0x0f:
        case 0x10: case 0x11: case 0x12: case 0x13: case 0x14: case 0x15: case 0x16: case 0x17:
        case 0x18: case 0x19: case 0x1a: case 0x1b: case 0x1c: case 0x1d: case 0x1e: case 0x1f:
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
        case 0x78: case 0x79: case 0x7a: case 0x7b: case 0x7c: case 0x7d: case 0x7e: case 0x7f:
            
            SrcLoc.Column++;
            
            return SourceCharacter(firstByte);
            //
            // 2 byte UTF-8 sequence
            //
        case 0xc0: case 0xc1: case 0xc2: case 0xc3: case 0xc4: case 0xc5: case 0xc6: case 0xc7:
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
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    error = true;
                    
                    auto srcCharStartLoc = resetLoc;
                    
                    SrcLoc.Column++;
                    
                    return invalid(srcCharStartLoc, policy);
                }
            }
            
            //
            // Continue
            //
            
            //
            // Manual test for code points that are over long
            //
            if (0xc2 <= firstByte && firstByte <= 0xdf) {
                
                if (0x80 <= tmp && tmp <= 0xbf) {
                    
                    // Valid
                    
                    auto decoded = (((firstByte & 0x1f) << 6) | (tmp & 0x3f));
                    
                    SrcLoc.Column++;
                    
                    return SourceCharacter(decoded);
                }
            }
            
            TheByteBuffer->buffer = resetBuf;
            TheByteBuffer->wasEOF = resetEOF;
            SrcLoc = resetLoc;
            
            error = true;
            
            auto srcCharStartLoc = resetLoc;
            
            SrcLoc.Column++;
            
            return invalid(srcCharStartLoc, policy);
        }
            //
            // 3 byte UTF-8 sequence
            //
        case 0xe0: case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
        case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: case 0xed: case 0xee: case 0xef: {
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    error = true;
                    
                    auto srcCharStartLoc = resetLoc;
                    
                    SrcLoc.Column++;
                    
                    return invalid(srcCharStartLoc, policy);
                }
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteBuffer->wasEOF = resetEOF;
                    SrcLoc = resetLoc;
                    
                    error = true;
                    
                    auto srcCharStartLoc = resetLoc;
                    
                    SrcLoc.Column++;
                    
                    return invalid(srcCharStartLoc, policy);
                }
            }
            
            //
            // Manual test for code points that are over long
            //
            if (0xe0 <= firstByte && firstByte <= 0xe0) {
                
                if (0xa0 <= secondByte && secondByte <= 0xbf) {
                    
                    if (0x80 <= tmp && tmp <= 0xbf) {
                        
                        // Valid
                        
                        auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
                        
                        //
                        // Manual test for code points that are surrogates
                        //
                        assert(!(0xd800 <= decoded && decoded <= 0xdfff));
                        
                        SrcLoc.Column++;
                        
                        return SourceCharacter(decoded);
                    }
                }
                
            } else if (0xe1 <= firstByte && firstByte <= 0xec) {
                
                if (0x80 <= secondByte && secondByte <= 0xbf) {
                    
                    if (0x80 <= tmp && tmp <= 0xbf) {
                        
                        // Valid
                        
                        auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
                        
                        //
                        // Manual test for code points that are surrogates
                        //
                        assert(!(0xd800 <= decoded && decoded <= 0xdfff));
                        
                        SrcLoc.Column++;
                        
                        return SourceCharacter(decoded);
                    }
                }
                
            } else if (0xed <= firstByte && firstByte <= 0xed) {
                
                if (0x80 <= secondByte && secondByte <= 0x9f) {
                    
                    if (0x80 <= tmp && tmp <= 0xbf) {
                        
                        // Valid
                        
                        auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
                        
                        //
                        // Manual test for code points that are surrogates
                        //
                        assert(!(0xd800 <= decoded && decoded <= 0xdfff));
                        
                        SrcLoc.Column++;
                        
                        return SourceCharacter(decoded);
                    }
                }
                
            } else if (0xee <= firstByte && firstByte <= 0xef) {
                
                if (0x80 <= secondByte && secondByte <= 0xbf) {
                    
                    if (0x80 <= tmp && tmp <= 0xbf) {
                        
                        // Valid
                        
                        auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
                        
                        //
                        // Manual test for code points that are surrogates
                        //
                        assert(!(0xd800 <= decoded && decoded <= 0xdfff));
                        
                        SrcLoc.Column++;
                        
                        return SourceCharacter(decoded);
                    }
                }
            }
            
            TheByteBuffer->buffer = resetBuf;
            TheByteBuffer->wasEOF = resetEOF;
            SrcLoc = resetLoc;
            
            error = true;
            
            auto srcCharStartLoc = resetLoc;
            
            SrcLoc.Column++;
            
            return invalid(srcCharStartLoc, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf0: case 0xf1: case 0xf2: case 0xf3: case 0xf4: case 0xf5: case 0xf6: case 0xf7: {
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetEOF = TheByteBuffer->wasEOF;
            auto resetLoc = SrcLoc;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    error = true;
                    
                    auto srcCharStartLoc = resetLoc;
                    
                    SrcLoc.Column++;
                    
                    return invalid(srcCharStartLoc, policy);
                }
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteBuffer->wasEOF = resetEOF;
                    SrcLoc = resetLoc;
                    
                    error = true;
                    
                    auto srcCharStartLoc = resetLoc;
                    
                    SrcLoc.Column++;
                    
                    return invalid(srcCharStartLoc, policy);
                }
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteBuffer->wasEOF = resetEOF;
                    SrcLoc = resetLoc;
                    
                    error = true;
                    
                    auto srcCharStartLoc = resetLoc;
                    
                    SrcLoc.Column++;
                    
                    return invalid(srcCharStartLoc, policy);
                }
            }
            
            //
            // Manual test for code points that are over long
            //
            if (0xf0 <= firstByte && firstByte <= 0xf0) {
                
                if (0x90 <= secondByte && secondByte <= 0xbf) {
                    
                    if (0x80 <= thirdByte && thirdByte <= 0xbf) {
                        
                        if (0x80 <= tmp && tmp <= 0xbf) {
                            
                            // Valid
                            
                            auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
                            
                            //
                            // Manual test for code points that are too large
                            //
                            assert(decoded <= 0x10ffff);
                            
                            SrcLoc.Column++;
                            
                            return SourceCharacter(decoded);
                        }
                    }
                }
                
            } else if (0xf1 <= firstByte && firstByte <= 0xf3) {
                
                if (0x80 <= secondByte && secondByte <= 0xbf) {
                    
                    if (0x80 <= thirdByte && thirdByte <= 0xbf) {
                        
                        if (0x80 <= tmp && tmp <= 0xbf) {
                            
                            // Valid
                            
                            auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
                            
                            //
                            // Manual test for code points that are too large
                            //
                            assert(decoded <= 0x10ffff);
                            
                            SrcLoc.Column++;
                            
                            return SourceCharacter(decoded);
                        }
                    }
                }
                
            } else if (0xf4 <= firstByte && firstByte <= 0xf4) {
                
                if (0x80 <= secondByte && secondByte <= 0x8f) {
                    
                    if (0x80 <= thirdByte && thirdByte <= 0xbf) {
                        
                        if (0x80 <= tmp && tmp <= 0xbf) {
                            
                            // Valid
                            
                            auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
                            
                            //
                            // Manual test for code points that are too large
                            //
                            assert(decoded <= 0x10ffff);
                            
                            SrcLoc.Column++;
                            
                            return SourceCharacter(decoded);
                        }
                    }
                }
            }
            
            TheByteBuffer->buffer = resetBuf;
            TheByteBuffer->wasEOF = resetEOF;
            SrcLoc = resetLoc;
            
            error = true;
            
            auto srcCharStartLoc = resetLoc;
            
            SrcLoc.Column++;
            
            return invalid(srcCharStartLoc, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        case 0xff: {
            
            //
            // could be EOF
            //
            if (TheByteBuffer->wasEOF) {
                
                //
                // Do not increment Column
                //
                
                return SourceCharacter(CODEPOINT_ENDOFFILE);
            }
            
            error = true;
            
            auto srcCharStartLoc = SrcLoc;
            
            SrcLoc.Column++;
            
            return invalid(srcCharStartLoc, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        default: {
            
            error = true;
            
            auto srcCharStartLoc = SrcLoc;
            
            SrcLoc.Column++;
            
            return invalid(srcCharStartLoc, policy);
        }
    }
}


SourceCharacter ByteDecoder::currentSourceCharacter(NextCharacterPolicy policy) {
    
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

//
//
//
SourceCharacter ByteDecoder::invalid(SourceLocation errSrcLoc, NextCharacterPolicy policy) {
    
#if !NISSUES
    if ((policy & ENABLE_BYTE_DECODING_ISSUES) == ENABLE_BYTE_DECODING_ISSUES) {
        
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(errSrcLoc, errSrcLoc + 1), 0.0, {}));
        
        Issues.push_back(std::move(I));
    }
#endif // !NISSUES
    
    //
    // Return \[UnknownGlyph] character when there is bad UTF-8 encoding
    //
    // http://www.unicode.org/faq/utf_bom.html
    // Are there any byte sequences that are not generated by a UTF? How should I interpret them?
    //
    
    return SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER);
}


#if !NISSUES
void ByteDecoder::addIssue(IssuePtr I) {
    Issues.push_back(std::move(I));
}

std::vector<IssuePtr>& ByteDecoder::getIssues() {
    return Issues;
}
#endif // !NISSUES

void ByteDecoder::setError(bool err) {
    error = err;
}

bool ByteDecoder::getError() const {
    return error;
}

void ByteDecoder::clearError() {
    error = false;
}

ByteDecoderPtr TheByteDecoder = nullptr;
