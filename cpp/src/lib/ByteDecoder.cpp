
#include "ByteDecoder.h"

ByteDecoder::ByteDecoder(std::istream &In) : In(In), eof(false), byteQueue(), Issues() {}

SourceCharacter ByteDecoder::nextSourceCharacter() {
    
    assert(Issues.empty());
    
    auto b = nextByte();
    
    if (eof) {
        
        auto SourceManagerIssues = TheSourceManager->getIssues();
        
        std::copy(SourceManagerIssues.begin(), SourceManagerIssues.end(), std::back_inserter(Issues));
        
        
        TheSourceManager->advanceSourceLocation(SourceCharacter(EOF));
        
        return SourceCharacter(EOF);
    }
    
    auto c = decodeBytes(b);
    
    auto SourceManagerIssues = TheSourceManager->getIssues();
    
    std::copy(SourceManagerIssues.begin(), SourceManagerIssues.end(), std::back_inserter(Issues));
    
    
    TheSourceManager->advanceSourceLocation(c);
    
    return c;
}

unsigned char ByteDecoder::nextByte() {
    
    if (eof) {
        return EOF;
    }
    
    if (!byteQueue.empty()) {
        
        auto b = byteQueue[0];
        
        // erase first
        byteQueue.erase(byteQueue.begin());
        
        return b;
    }
    
    auto b = In.get();
    
    if (In.eof()) {
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
    Loc.Col++;
    

    auto Issue = SyntaxIssue(SYNTAXISSUETAG_CHARACTERENCODING, "Invalid UTF-8 sequence. Try resaving the file.", SYNTAXISSUESEVERITY_REMARK, SourceSpan{Loc, Loc});
        
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

std::vector<SyntaxIssue> ByteDecoder::getIssues() {
    
    auto Tmp = Issues;
    
    Issues.clear();
    
    return Tmp;
}

ByteDecoder *TheByteDecoder = nullptr;
