
#include "ByteDecoder.h"

#include "ByteBuffer.h"
#include "CodePoint.h"

ByteDecoder::ByteDecoder() : SrcLoc(), state(), Issues() {}

void ByteDecoder::init(SourceStyle style) {
    
    Issues.clear();
    
    state.reset();
    
    SrcLoc = SourceLocation(style);
}

void ByteDecoder::deinit() {
    
    Issues.clear();
}

SourceCharacter ByteDecoder::nextSourceCharacter() {
    
    unsigned char eof = 0;
    
    auto b = TheByteBuffer->nextByte(&eof);
    
    auto c = decodeBytes(b, eof);
    
    SrcLoc = state.advance(c, SrcLoc);
    
    return c;
}

SourceCharacter ByteDecoder::invalid(unsigned char first, size_t backup) {
    
    auto idx = TheByteBuffer->getDataByteIndex();
    
    auto loc = getSourceLocation();
    
    AdvancementState state;
    
    // Has not advanced yet at this point
    loc = state.advance(SourceCharacter(first), loc);
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(loc)));
    
    Issues.push_back(std::move(I));
    
    TheByteBuffer->setDataByteIndex(idx - backup);
    
    return SourceCharacter(first);
}

//
// https://unicodebook.readthedocs.io/issues.html#strict-utf8-decoder
//
SourceCharacter ByteDecoder::decodeBytes(unsigned char cIn, bool eof) {
    
    switch (cIn) {
        //
        // ASCII character
        //
        case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
        case 0x08: case 0x09: case 0x0a: case 0x0b: case 0x0c: case 0x0d: case 0x0e: case 0x0f:
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
            
            return SourceCharacter(cIn);
        //
        // 2 byte UTF 8 sequence
        //
        case 0xc0: case 0xc1: case 0xc2: case 0xc3: case 0xc4: case 0xc5: case 0xc6: case 0xc7:
        case 0xc8: case 0xc9: case 0xca: case 0xcb: case 0xcc: case 0xcd: case 0xce: case 0xcf:
        case 0xd0: case 0xd1: case 0xd2: case 0xd3: case 0xd4: case 0xd5: case 0xd6: case 0xd7:
        case 0xd8: case 0xd9: case 0xda: case 0xdb: case 0xdc: case 0xdd: case 0xde: case 0xdf: {
            
            auto firstByte = cIn;
            
            unsigned char eof = 0;
            
            auto tmp = TheByteBuffer->nextByte(&eof);
            
            if (eof) {
                return invalid(firstByte, 0);
            }
            
            //
            // Manual test for code points that are over long
            //
            if (0xc2 <= firstByte && firstByte <= 0xdf) {
                
                if (0x80 <= tmp && tmp <= 0xbf) {
                    
                    // Valid
                    
                    auto decoded = (((firstByte & 0x1f) << 6) | (tmp & 0x3f));
                    
                    return SourceCharacter(decoded);
                }
            }
            
            return invalid(firstByte, 1);
        }
        //
        // 3 byte UTF 8 sequence
        //
        case 0xe0: case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
        case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: case 0xed: case 0xee: case 0xef: {
            auto firstByte = cIn;
            
            unsigned char eof = 0;
            
            auto tmp = TheByteBuffer->nextByte(&eof);
            
            if (eof) {
                return invalid(firstByte, 0);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            tmp = TheByteBuffer->nextByte(&eof);
            
            if (eof) {
                return invalid(firstByte, 1);
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
                        
                        return SourceCharacter(decoded);
                    }
                }
            }
            
            return invalid(firstByte, 2);
        }
        //
        // 4 byte UTF 8 sequence
        //
        case 0xf0: case 0xf1: case 0xf2: case 0xf3: case 0xf4: case 0xf5: case 0xf6: case 0xf7: {
            
            auto firstByte = cIn;
            
            unsigned char eof = 0;
            
            auto tmp = TheByteBuffer->nextByte(&eof);
            
            if (eof) {
                return invalid(firstByte, 0);
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            tmp = TheByteBuffer->nextByte(&eof);
            
            if (eof) {
                return invalid(firstByte, 1);
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            tmp = TheByteBuffer->nextByte(&eof);
            
            if (eof) {
                return invalid(firstByte, 2);
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
                            
                            return SourceCharacter(decoded);
                        }
                    }
                }
            }
            
            return invalid(firstByte, 3);
        }
        //
        // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
        //
        // And also check for EOF
        //
        case 0xff: {
            if (eof) {
                return SourceCharacter(CODEPOINT_ENDOFFILE);
            }
            
            return invalid(cIn, 0);
        }
        //
        // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
        //
        default:
            return invalid(cIn, 0);
    }
}

void ByteDecoder::setSourceLocation(SourceLocation Loc) {
    SrcLoc = Loc;
}

SourceLocation ByteDecoder::getSourceLocation() const {
    return SrcLoc;
}

std::vector<std::unique_ptr<Issue>>& ByteDecoder::getIssues() {
    return Issues;
}


void AdvancementState::reset() {
    
    lastCharacterWasCarriageReturn = false;
}

SourceLocation AdvancementState::advance(SourceCharacter c, SourceLocation SrcLoc) {
    
    switch (c.to_point()) {
        case CODEPOINT_ENDOFFILE: {
            //
            // It can happen that single \r occurs.
            // Then make sure to treat it as a newline.
            //
            if (lastCharacterWasCarriageReturn) {
                auto Loc = SrcLoc;
                
                //
                // Do not need to advance Col here
                //
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                
                TheByteDecoder->addIssue(std::move(I));
            }
            
            lastCharacterWasCarriageReturn = false;
            
            return SrcLoc.nextLine();
        }
        case '\n': {
            //
            // if lastCharacterWasCarriageReturn, then newline was already handled
            //
            if (lastCharacterWasCarriageReturn) {
                
                lastCharacterWasCarriageReturn = false;
                
                return SrcLoc;
            }
            
            return SrcLoc.nextLine();
        }
        case '\r': {
            //
            // It can happen that single \r occurs.
            // Then make sure to treat it as a newline.
            //
            if (lastCharacterWasCarriageReturn) {
                
                auto Loc = SrcLoc;
                
                //
                // Do not need to advance Col here
                //
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                
                TheByteDecoder->addIssue(std::move(I));
            }
            
            lastCharacterWasCarriageReturn = true;
            
            return SrcLoc.nextLine();
        }
        default: {
            //
            // It can happen that single \r occurs.
            // Then make sure to treat it as a newline.
            //
            if (lastCharacterWasCarriageReturn) {
                
                auto Loc = SrcLoc;
                
                //
                // Do not need to advance Col here
                //
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                
                TheByteDecoder->addIssue(std::move(I));
            }
            
            lastCharacterWasCarriageReturn = false;
            
            return SrcLoc+1;
        }
    }
}

//
// Only to be used by AdvancementState
//
void ByteDecoder::addIssue(std::unique_ptr<Issue> I) {
    Issues.push_back(std::move(I));
}

std::unique_ptr<ByteDecoder> TheByteDecoder = nullptr;
