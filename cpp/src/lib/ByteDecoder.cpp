
#include "ByteDecoder.h"
#include "Utils.h"

ByteDecoder::ByteDecoder() : eof(false), byteQueue(), Issues(), totalTimeMicros() {}

void ByteDecoder::init() {
    eof = false;
    byteQueue.clear();
    Issues.clear();
    totalTimeMicros = std::chrono::microseconds::zero();
}

void ByteDecoder::deinit() {
    byteQueue.clear();
    Issues.clear();
}

SourceCharacter ByteDecoder::nextSourceCharacter() {
    TimeScoper Scoper(&totalTimeMicros);
    
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
        auto location = p.second;
        
        auto c = SourceCharacter(b);
        
        TheSourceManager->setSourceLocation(location);
        
        return c;
    }
    
    auto b = nextByte();
    
    if (eof) {
        
        TheSourceManager->advanceSourceLocation(SOURCECHARACTER_ENDOFFILE);
        
        return SOURCECHARACTER_ENDOFFILE;
    }
    
    auto c = decodeBytes(b);
    
    TheSourceManager->advanceSourceLocation(c);
    
    return c;
}

unsigned char ByteDecoder::nextByte() {
    
    if (eof) {
        return EOF;
    }
    
    auto b = TheInputStream->get();
    
    if (TheInputStream->eof()) {
        eof = true;
    }
    
    if (b == EOF) {
        eof = true;
    }
    
    return b;
}

void ByteDecoder::append(unsigned char b, SourceLocation location) {
    byteQueue.push_back(std::make_pair(b, location));
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
        
        auto tmp = nextByte();
        
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
        
        // Invalid UTF8
    
        auto Loc = TheSourceManager->getSourceLocation();
    
        // Has not advanced yet at this point
        Loc = SourceLocation(Loc.Line, Loc.Col+1);
    
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
    
        Issues.push_back(Issue);
    
    
        append(tmp, Loc+1);
    
        return SourceCharacter(firstByte);
        
    } else if ((cIn & 0xf0) == 0xe0) {
        
        //
        // 3 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        auto tmp = nextByte();
        
        // Continue
        
        auto secondByte = tmp;
        
        tmp = nextByte();
        
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
        
        // Invalid UTF8
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        // Has not advanced yet at this point
        Loc = SourceLocation(Loc.Line, Loc.Col+1);
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
        
        Issues.push_back(Issue);
        
        
        append(secondByte, Loc+1);
        append(tmp, Loc+2);
        
        return SourceCharacter(firstByte);
        
    } else if ((cIn & 0xf8) == 0xf0) {
        
        //
        // 4 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        auto tmp = nextByte();
        
        // Continue
        
        auto secondByte = tmp;
        
        tmp = nextByte();
        
        // Continue
        
        auto thirdByte = tmp;
        
        tmp = nextByte();
        
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
        
        // Invalid UTF8
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        // Has not advanced yet at this point
        Loc = SourceLocation(Loc.Line, Loc.Col+1);
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
        
        Issues.push_back(Issue);
        
        
        append(secondByte, Loc+1);
        append(thirdByte, Loc+2);
        append(tmp, Loc+3);
        
        return SourceCharacter(firstByte);
    }
        
    //
    // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
    //
    
    auto Loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    Loc = SourceLocation(Loc.Line, Loc.Col+1);
    
    auto Issue = SyntaxIssue(SYNTAXISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
    
    Issues.push_back(Issue);
    
    
    return SourceCharacter(cIn);
}

std::vector<SyntaxIssue> ByteDecoder::getIssues() const {
    return Issues;
}

std::vector<Metadata> ByteDecoder::getMetadatas() const {
    
    std::vector<Metadata> Metadatas;
    
    auto totalTimeMillis = std::chrono::duration_cast<std::chrono::milliseconds>(totalTimeMicros);
    
    Metadatas.push_back(Metadata("ByteDecoderTotalTimeMillis", std::to_string(totalTimeMillis.count())));
    
    return Metadatas;
}

std::istream *TheInputStream = nullptr;
ByteDecoder *TheByteDecoder = nullptr;
