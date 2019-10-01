
#include "ByteDecoder.h"

#include "SourceManager.h"
#include "CodePoint.h"

ByteDecoder::ByteDecoder() : eof(false), byteQueue(), Issues() {}

void ByteDecoder::init() {
    eof = false;
    byteQueue.clear();
    Issues.clear();
}

void ByteDecoder::deinit() {
    byteQueue.clear();
    Issues.clear();
}

SourceCharacter ByteDecoder::nextSourceCharacter() {
    
    //
    // handle the queue before anything else
    //
    // We know only single-byte SourceCharacters are queued
    //
    if (!byteQueue.empty()) {
        
        auto p = byteQueue[0];
        
        // erase first
        byteQueue.erase(byteQueue.begin());
        
        auto b = p.first;
        auto loc = p.second;
        
        auto c = SourceCharacter(b);
        
        TheSourceManager->setSourceLocation(loc);
        
        return c;
    }
    
    if (TheSourceManager->isEndOfFile()) {
        
        TheSourceManager->advanceSourceLocation(SourceCharacter(CODEPOINT_ENDOFFILE));
        
        return SourceCharacter(CODEPOINT_ENDOFFILE);
    }
    
    auto b = TheSourceManager->nextByte();
    
    auto c = decodeBytes(b);
    
    TheSourceManager->advanceSourceLocation(c);
    
    return c;
}

void ByteDecoder::append(unsigned char b, SourceLocation loc) {
    byteQueue.push_back(std::make_pair(b, loc));
}

SourceCharacter ByteDecoder::invalid(unsigned char first) {
    
    auto loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    loc++;
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(loc)));
    
    Issues.push_back(std::move(I));
    
    return SourceCharacter(first);
}

SourceCharacter ByteDecoder::invalid(unsigned char first, unsigned char second) {
    
    auto loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    loc++;
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(loc)));
    
    Issues.push_back(std::move(I));
    
    append(second, loc+1);
    
    return SourceCharacter(first);
}

SourceCharacter ByteDecoder::invalid(unsigned char first, unsigned char second, unsigned char third) {
    
    auto loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    loc++;
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(loc)));
    
    Issues.push_back(std::move(I));
    
    append(second, loc+1);
    append(third, loc+2);
    
    return SourceCharacter(first);
}

SourceCharacter ByteDecoder::invalid(unsigned char first, unsigned char second, unsigned char third, unsigned char fourth) {
    
    auto loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    loc++;
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.", FORMATISSUESEVERITY_FORMATTING, Source(loc)));
    
    Issues.push_back(std::move(I));
    
    append(second, loc+1);
    append(third, loc+2);
    append(fourth, loc+3);
    
    return SourceCharacter(first);
}

//
// https://unicodebook.readthedocs.io/issues.html#strict-utf8-decoder
//
SourceCharacter ByteDecoder::decodeBytes(unsigned char cIn) {
    
    if ((cIn & 0x80) == 0x00) {
        
        //
        // ASCII character
        //
        
        return SourceCharacter(cIn);
        
    } else if ((cIn & 0xe0) == 0xc0) {
        
        //
        // 2 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        if (TheSourceManager->isEndOfFile()) {
            return invalid(firstByte);
        }
        
        auto tmp = TheSourceManager->nextByte();
        
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
        
        return invalid(firstByte, tmp);
        
    } else if ((cIn & 0xf0) == 0xe0) {
        
        //
        // 3 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        if (TheSourceManager->isEndOfFile()) {
            return invalid(firstByte);
        }
        
        auto tmp = TheSourceManager->nextByte();
        
        // Continue
        
        auto secondByte = tmp;
        
        if (TheSourceManager->isEndOfFile()) {
            return invalid(firstByte, secondByte);
        }
        
        tmp = TheSourceManager->nextByte();
        
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
        
        return invalid(firstByte, secondByte, tmp);
        
    } else if ((cIn & 0xf8) == 0xf0) {
        
        //
        // 4 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        if (TheSourceManager->isEndOfFile()) {
            return invalid(firstByte);
        }
        
        auto tmp = TheSourceManager->nextByte();
        
        // Continue
        
        auto secondByte = tmp;
        
        if (TheSourceManager->isEndOfFile()) {
            return invalid(firstByte, secondByte);
        }
        
        tmp = TheSourceManager->nextByte();
        
        // Continue
        
        auto thirdByte = tmp;
        
        if (TheSourceManager->isEndOfFile()) {
            return invalid(firstByte, secondByte, thirdByte);
        }
        
        tmp = TheSourceManager->nextByte();
        
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
        
        return invalid(firstByte, secondByte, thirdByte, tmp);
    }
    
    //
    // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
    //
    
    return invalid(cIn);
}

std::vector<std::unique_ptr<Issue>>& ByteDecoder::getIssues() {
    return Issues;
}

std::unique_ptr<ByteDecoder> TheByteDecoder = nullptr;

