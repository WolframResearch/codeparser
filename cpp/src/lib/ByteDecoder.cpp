
#include "ByteDecoder.h"

#include "CodePoint.h" // for CODEPOINT_REPLACEMENT_CHARACTER


ByteDecoder::ByteDecoder() : offsetLineMap(), Issues() {}

void ByteDecoder::init() {
    
    Issues.clear();
    
    error = false;
    
    //
    // calculate offset line map
    //
    // Example: given the list { 'a', 'b', '\n', 'c', 'd', 'e', '\n', 'f'}
    // the result would be < 0, 2, 6 >
    //
    // Example: given the list { 'a', 'b', '\r', '\n', 'c', 'd', 'e', '\r', '\n', 'f'}
    // the result would be < 0, 3, 8 >
    //
    // Example: given the list { '\n', 'c', 'd', 'e', '\n', 'f'}
    // the result would be < 0, 0, 4 >
    //
    // Example: given the list { '\r', '\n', 'c', 'd', 'e', '\r', '\n', 'f'}
    // the result would be < 0, 1, 6 >
    //
    // With \r\n pairs, store the pointer to \n.
    // This makes it easier to compute the offsets later in that line.
    // And only the preceding \r has to be handled specially.
    //
    // converting to start locations would look like this: { (1, 1), (1, 2), (2, 0), (2, 1), (2, 2), (2, 3), (3, 0), (3, 1) }
    //
    // converting to end locations would look like this:   { (1, 1), (1, 2), (1, 3), (2, 2), (2, 2), (2, 3), (2, 4), (3, 1) }
    //
    
    auto start = TheByteBuffer->buffer;
    auto end = TheByteBuffer->end;
    
    if (start == end) {
        //
        // Empty
        //
        
        offsetLineMap = std::vector<Buffer>(0);
        
        return;
    }
    
    //
    // Initial scan to get count of \r and \n
    //
    size_t newlineCount = 0;
    for (auto p = start; p < end; ++p) {
        auto b = *p;
        //
        // Might overshoot the number of newlines because of
        // double-counting \r and \n inside \r\n, oh well
        //
        if (b == '\n' || b == '\r') {
            ++newlineCount;
        }
    }
    
    offsetLineMap = std::vector<Buffer>(2 + newlineCount);
    
#if !NISSUES
    SourceLocation Loc(1, 1);
#endif // NISSUES
    
    offsetLineMap[0] = start;
    actualOffsetLineMapSize = 1;
    
    bool lastCharacterWasCR = false;
    
    if (*start == '\r') {
        offsetLineMap[1] = start;
        ++actualOffsetLineMapSize;
        lastCharacterWasCR = true;
#if !NISSUES
        Loc = SourceLocation(2, 0);
#endif // NISSUES
        ++start;
    }
    
    //
    // This is where unexpected \r are reported
    //
    
    for (auto p = start; p < end; ++p) {
        
        switch (*p) {
            case '\r':
#if !NISSUES
                Loc.Line++;
                Loc.Column = 0;
                
                //
                // It can happen that single \r occurs.
                // Then make sure to treat it as a newline.
                //
                if (lastCharacterWasCR) {
                    
                    //
                    // No CodeAction here
                    //
                    
                    auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc), 0.0, {}));
                    
                    TheByteDecoder->addIssue(std::move(I));
                }
#endif // NISSUES
                //
                // advanced
                //
                
                offsetLineMap[actualOffsetLineMapSize] = p;
                ++actualOffsetLineMapSize;
                
                lastCharacterWasCR = true;
                
                break;
                
            case '\n':
#if !NISSUES
                if (!lastCharacterWasCR) {
                    Loc.Line++;
                    Loc.Column = 0;
                }
#endif // NISSUES
                //
                // advanced
                //
                
                if (lastCharacterWasCR) {
                    ++offsetLineMap[actualOffsetLineMapSize-1];
                } else {
                    //                    offsetLineMap.push_back(p);
                    offsetLineMap[actualOffsetLineMapSize] = p;
                    ++actualOffsetLineMapSize;
                }
                
                lastCharacterWasCR = false;
                
                break;
            default:
#if !NISSUES
                Loc.Column++;
                    
                //
                // It can happen that single \r occurs.
                // Then make sure to treat it as a newline.
                //
                if (lastCharacterWasCR) {
                    
                    //
                    // No CodeAction here
                    //
                    
                    auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc), 0.0, {}));
                    
                    TheByteDecoder->addIssue(std::move(I));
                }
                
#endif // !NISSUES
                lastCharacterWasCR = false;
                break;
        }
        
    } // for
    
    offsetLineMap[actualOffsetLineMapSize] = end;
    ++actualOffsetLineMapSize;
}

void ByteDecoder::deinit() {
    
    Issues.clear();
    
    offsetLineMap.clear();
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
    
    auto firstByte = TheByteBuffer->nextByte0();
    
    switch (firstByte) {
            //
            // Handle CR specially
            //
        case 0x0d: {
            
            if (TheByteBuffer->currentByte() == '\n') {
                
                TheByteBuffer->nextByte();
                
                return SourceCharacter(CODEPOINT_CRLF);
            }
            
            return SourceCharacter('\r');
        }
            //
            // 1 byte UTF-8 sequence
            //
        case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
        case 0x08: case 0x09: case 0x0a: case 0x0b: case 0x0c: /*^CR^^*/  case 0x0e: case 0x0f:
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
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    error = true;
                    
                    auto srcCharStartBuf = resetBuf - 1;
                    
                    return invalid(srcCharStartBuf, policy);
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
                    
                    return SourceCharacter(decoded);
                }
            }
            
            TheByteBuffer->buffer = resetBuf;
            
            error = true;
            
            auto srcCharStartBuf = resetBuf - 1;
            
            return invalid(srcCharStartBuf, policy);
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
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    error = true;
                    
                    auto srcCharStartBuf = resetBuf - 1;
                    
                    return invalid(srcCharStartBuf, policy);
                }
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    TheByteBuffer->buffer = resetBuf;
                    
                    error = true;
                    
                    auto srcCharStartBuf = resetBuf - 1;
                    
                    return invalid(srcCharStartBuf, policy);
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
            
            TheByteBuffer->buffer = resetBuf;
            
            error = true;
            
            auto srcCharStartBuf = resetBuf - 1;
            
            return invalid(srcCharStartBuf, policy);
        }
            //
            // 4 byte UTF-8 sequence
            //
        case 0xf0: case 0xf1: case 0xf2: case 0xf3: case 0xf4: case 0xf5: case 0xf6: case 0xf7: {
            
            //
            // Buffer is possibly already pointing to EOF
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            
            auto tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    error = true;
                    
                    auto srcCharStartBuf = resetBuf - 1;
                    
                    return invalid(srcCharStartBuf, policy);
                }
            }
            
            // Continue
            
            auto secondByte = tmp;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    TheByteBuffer->buffer = resetBuf;
                    
                    error = true;
                    
                    auto srcCharStartBuf = resetBuf - 1;
                    
                    return invalid(srcCharStartBuf, policy);
                }
            }
            
            // Continue
            
            auto thirdByte = tmp;
            
            tmp = TheByteBuffer->nextByte0();
            
            if (tmp == 0xff) {
                
                if (TheByteBuffer->wasEOF) {
                    
                    TheByteBuffer->buffer = resetBuf;
                    
                    error = true;
                    
                    auto srcCharStartBuf = resetBuf - 1;
                    
                    return invalid(srcCharStartBuf, policy);
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
            
            TheByteBuffer->buffer = resetBuf;
            
            error = true;
            
            auto srcCharStartBuf = resetBuf - 1;
            
            return invalid(srcCharStartBuf, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        case 0xff: {
            
            auto resetBuf = TheByteBuffer->buffer;
            
            //
            // could be EOF
            //
            if (TheByteBuffer->eof()) {
                
                return SourceCharacter(CODEPOINT_ENDOFFILE);
            }
            
            error = true;
            
            auto srcCharStartBuf = resetBuf - 1;
            
            return invalid(srcCharStartBuf, policy);
        }
            //
            // Not a valid UTF-8 start
            //
        default: {
            
            auto resetBuf = TheByteBuffer->buffer;
            
            error = true;
            
            auto srcCharStartBuf = resetBuf - 1;
            
            return invalid(srcCharStartBuf, policy);
        }
    }
}


SourceCharacter ByteDecoder::currentSourceCharacter(NextCharacterPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    
    auto c = nextSourceCharacter0(policy);
    
    lastBuf = TheByteBuffer->buffer;
    
    TheByteBuffer->buffer = resetBuf;
    
    return c;
}

//
//
//
SourceCharacter ByteDecoder::invalid(Buffer errReportingBuf, NextCharacterPolicy policy) {
    
#if !NISSUES
    if ((policy & ENABLE_BYTE_DECODING_ISSUES) == ENABLE_BYTE_DECODING_ISSUES) {
        
        auto loc1 = convertBufferToStart(errReportingBuf);
        auto loc2 = convertBufferToEnd(errReportingBuf + 1);
        
        //
        // No CodeAction here
        //
        
        auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(loc1, loc2), 0.0, {}));
        
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

//
// input: buf
// output: SourceLocation of buf
//
// if buf is pointer to newline at SourceLocation(2, 0), then this will return SourceLocation(2, 0)
//
SourceLocation ByteDecoder::convertBufferToStart(Buffer buf) const {
    
    assert(!offsetLineMap.empty());
    
    assert(offsetLineMap[0] <= buf || (buf[0] == '\r' && offsetLineMap[0] == buf + 1));
    assert(buf <= TheByteBuffer->end);
    
    //
    // Handle end
    //
    if (offsetLineMap[actualOffsetLineMapSize-1] == buf) {
        
        if (actualOffsetLineMapSize > 2) {
            
            auto p = offsetLineMap[actualOffsetLineMapSize-2];
            
            auto diff = buf - p;
            return SourceLocation(actualOffsetLineMapSize-1, diff);
        }
        
        auto diff = buf - offsetLineMap[0];
        return SourceLocation(1, diff + 1);
    }
    
    //
    // if *buf is \r and *(buf+1) is '\n', then adjust to use the \n
    //
    auto adjustedBuf = buf;
    if (*buf == '\r') {
        if (buf < offsetLineMap[actualOffsetLineMapSize-1]) {
            if (*(buf+1) == '\n') {
                adjustedBuf = buf+1;
            }
        }
    }
    
    //
    // Handle start
    //
    if (adjustedBuf < offsetLineMap[1]) {
        
        auto diff = adjustedBuf - offsetLineMap[0];
        return SourceLocation(1, diff + 1);
    }
    
    //
    // Now do binary search
    //
    
    size_t a = 1;
    size_t b = actualOffsetLineMapSize-1;
    
    size_t good;
    
    while (true) {
        
        if (a + 1 == b) {
            good = a;
            break;
        }
        
        auto mid = (a + b) / 2;
        
        if (adjustedBuf < offsetLineMap[mid]) {
            
            b = mid;
            
        } else {
            assert(offsetLineMap[mid] <= adjustedBuf);
            
            a = mid;
            
        }
        
    }
    
    auto diff = adjustedBuf - offsetLineMap[good];
    return SourceLocation(good + 1, diff);
}

//
// input: buf
// output: SourceLocation of buf
//
// if buf is pointer to newline at SourceLocation(2, 0), then this will return SourceLocation(1, x+1), where x is the length of line 1
//
SourceLocation ByteDecoder::convertBufferToEnd(Buffer buf) const {

    assert(!offsetLineMap.empty());
    
    assert(offsetLineMap[0] <= buf || (buf[0] == '\r' && offsetLineMap[0] == buf + 1));
    assert(buf <= TheByteBuffer->end);
    
    //
    // Handle end
    //
    if (offsetLineMap[actualOffsetLineMapSize-1] == buf) {
        
        if (actualOffsetLineMapSize > 2) {
            auto o = offsetLineMap[actualOffsetLineMapSize - 2];
            auto diff = buf - o;
            return SourceLocation(actualOffsetLineMapSize - 1, diff);
        }
        
        auto diff = buf - offsetLineMap[0];
        return SourceLocation(1, diff + 1);
    }
    
    //
    // if *buf is \n and *(buf-1) is '\r', then adjust to use the \r
    //
    auto adjustedBuf = buf;
    if (*buf == '\n') {
        if (offsetLineMap[0] < buf) {
            if (*(buf-1) == '\r') {
                adjustedBuf = buf-1;
            }
        }
    }
    
    //
    // Handle start
    //
    
    if (adjustedBuf <= offsetLineMap[1]) {
        
        auto diff = adjustedBuf - offsetLineMap[0];
        return SourceLocation(1, diff + 1);
    }
    
    //
    // Now do binary search
    //
    
    size_t a = 1;
    size_t b = actualOffsetLineMapSize-1;
    
    size_t good;
    
    while (true) {
        
        if (a + 1 == b) {
            good = a;
            break;
        }
        
        auto mid = (a + b) / 2;
        
        if (adjustedBuf <= offsetLineMap[mid]) {
            
            b = mid;
            
        } else {
            assert(offsetLineMap[mid] < adjustedBuf);
            
            a = mid;
            
        }
        
    }
    
    auto diff = adjustedBuf - offsetLineMap[good];
    return SourceLocation(good + 1, diff);
}



#if !NISSUES
//
// Only to be used by AdvancementState
//
void ByteDecoder::addIssue(IssuePtr I) {
    Issues.push_back(std::move(I));
}

std::vector<IssuePtr>& ByteDecoder::getIssues() {
    return Issues;
}
#endif // !NISSUES

std::vector<Buffer> ByteDecoder::getOffsetLineMap() const {
    return offsetLineMap;
}

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
