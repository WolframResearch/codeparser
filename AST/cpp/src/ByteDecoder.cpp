
#include "ByteDecoder.h"

#include "Utils.h"
#include "LongNameDefines.h"

#include <sstream>
#include <iomanip>
#include <iostream>
#include <cassert>

ByteDecoder::ByteDecoder(std::istream &In, bool interactive) : In(In), interactive(interactive), eof(false), byteQueue() {}

//
// if interactive is true, then only allow single lines to be read (such as on the command-line)
//
SourceCharacter ByteDecoder::nextSourceCharacter() {
    
    auto b = nextByte();
    
    if (eof) {
        
        TheSourceManager->advanceSourceLocation(EOF);
        
        return EOF;
    }
    
    auto c = decodeBytes(b);
    
    TheSourceManager->advanceSourceLocation(c);
    
    if (interactive && TheSourceManager->getSourceLocation().Line > 1) {
        eof = true;
        return EOF;
    }
    
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

unsigned char ByteDecoder::leaveAlone(std::vector<unsigned char> bytes) {

    assert(!bytes.empty());

    auto first = bytes[0];

    for (size_t i = 1; i < bytes.size(); i++) {
        auto b = bytes[i];
        byteQueue.push_back(b);
    }

    return first;
}

//
// Sets Unicode to the text of the decoded character, if it was not ASCII
//
SourceCharacter ByteDecoder::decodeBytes(unsigned char cIn) {
    
    if ((cIn & 0x80) == 0x00) {
        
        //
        // ASCII character
        //
        
        return cIn;
        
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
        
        return decoded;
        
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
        
        return decoded;
        
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
        
        return decoded;
        
    } else {
        
        //
        // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
        //
        
        return leaveAlone({cIn});
    }
}

ByteDecoder *TheByteDecoder = nullptr;
