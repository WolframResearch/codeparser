
#include "ByteDecoder.h"

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
        
        auto b = byteQueue[0];
        
        // erase first
        byteQueue.erase(byteQueue.begin());
        
        return SourceCharacter(b);
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

SourceCharacter ByteDecoder::leaveAlone(std::vector<unsigned char> bytes) {

    assert(!bytes.empty());

    auto Loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    Loc = SourceLocation(Loc.Line, Loc.Col+1);

    auto Issue = SyntaxIssue(SYNTAXISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence.\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
        
    Issues.push_back(Issue);

    
    auto first = bytes[0];

    for (size_t i = 1; i < bytes.size(); i++) {
        auto b = bytes[i];
        byteQueue.push_back(b);
    }

    return SourceCharacter(first);
}

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
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            // Invalid UTF8

            return leaveAlone({firstByte, tmp});
        }
        
        // Valid
        
        auto secondByte = tmp;
        
        auto decoded = (((firstByte & 0x1f) << 6) | (secondByte & 0x3f));
        
        return SourceCharacter(decoded);
        
    } else if ((cIn & 0xf0) == 0xe0) {
        
        //
        // 3 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        auto tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            // Invalid UTF8

            return leaveAlone({firstByte, tmp});
        }
        
        // Continue
        
        auto secondByte = tmp;
        
        tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            // Invalid UTF8

            return leaveAlone({firstByte, secondByte, tmp});
        }
        
        // Valid
        
        auto decoded = (((firstByte & 0x0f) << 12) | ((secondByte & 0x3f) << 6) | (tmp & 0x3f));
        
        return SourceCharacter(decoded);
        
    } else if ((cIn & 0xf8) == 0xf0) {
        
        //
        // 4 byte UTF 8 sequence
        //
        
        auto firstByte = cIn;
        
        auto tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            // Invalid UTF8

            return leaveAlone({firstByte, tmp});
        }
        
        // Continue
        
        auto secondByte = tmp;
        
        tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            // Invalid UTF8

            return leaveAlone({firstByte, secondByte, tmp});
        }
        
        // Continue
        
        auto thirdByte = tmp;
        
        tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            // Invalid UTF8

            return leaveAlone({firstByte, secondByte, thirdByte, tmp});
        }
        
        // Valid
        
        auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
        
        //
        // Manual test for code points that are too large
        //
        if (decoded > 0x10ffff) {
            
            // Invalid UTF8
            
            return leaveAlone({firstByte, secondByte, thirdByte, tmp});
        }
        
        return SourceCharacter(decoded);
        
    } else {
        
        //
        // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
        //
        
        return leaveAlone({cIn});
    }
}

std::vector<SyntaxIssue> ByteDecoder::getIssues() const {
    return Issues;
}

std::istream *TheInputStream = nullptr;
ByteDecoder *TheByteDecoder = nullptr;
