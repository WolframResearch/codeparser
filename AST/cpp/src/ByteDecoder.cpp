
#include "ByteDecoder.h"

#include "Utils.h"
#include "LongNameDefines.h"

#include <sstream>
#include <iomanip>
#include <iostream>

ByteDecoder::ByteDecoder(std::istream &In, bool singleLine) : In(In), singleLine(singleLine), eof(false), byteQueue() {}

SourceCharacter ByteDecoder::nextSourceCharacter() {
    
    auto b = nextByte();
    
    if (eof) {
        
        TheSourceManager->advanceSourceLocation(EOF);
        
        return EOF;
    }
    
    auto c = decodeBytes(b);
    
    TheSourceManager->advanceSourceLocation(c);
    
    if (singleLine && TheSourceManager->getSourceLocation().Line > 1) {
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
    
    //
    // Discard all \r that are read in
    //
    while (b == '\r') {
        b = In.get();
    }
    
    if (In.eof()) {
        eof = true;
    }
    
    if (b == EOF) {
        eof = true;
    }
    
    return b;
}

unsigned char ByteDecoder::invalidUTF8(unsigned char tmp) {
    
    auto Loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    Loc.Col++;
    
//    WARNING_MACRO("Invalid UTF8 sequence '" + makePrintable(tmp) + "'  Treating bytes as extended ASCII characters.", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));
    
    return tmp;
}

unsigned char ByteDecoder::invalidUTF8(unsigned char firstByte, unsigned char tmp) {
    
    auto Loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    Loc.Col++;
    
//    WARNING_MACRO("Invalid UTF8 sequence '" + makePrintable(firstByte) + "' '" + makePrintable(tmp) + "'  Treating bytes as extended ASCII characters.", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));
    
    byteQueue.push_back(tmp);
    
    return firstByte;
}

unsigned char ByteDecoder::invalidUTF8(unsigned char firstByte, unsigned char secondByte, unsigned char tmp) {

    auto Loc = TheSourceManager->getSourceLocation();

    // Has not advanced yet at this point
    Loc.Col++;

//    WARNING_MACRO("Invalid UTF8 sequence '" + makePrintable(firstByte) + "' '" + makePrintable(secondByte) + "' '" + makePrintable(tmp) + "'  Treating bytes as extended ASCII characters.", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));

    byteQueue.push_back(secondByte);
    byteQueue.push_back(tmp);

    return firstByte;
}

unsigned char ByteDecoder::invalidUTF8(unsigned char firstByte, unsigned char secondByte, unsigned char thirdByte, unsigned char tmp) {
    
    auto Loc = TheSourceManager->getSourceLocation();
    
    // Has not advanced yet at this point
    Loc.Col++;
    
//    WARNING_MACRO("Invalid UTF8 sequence '" + makePrintable(firstByte) + "' '" + makePrintable(secondByte) + "' '" + makePrintable(thirdByte) + "' '" + makePrintable(tmp) + "'  Treating bytes as extended ASCII characters.", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));
    
    byteQueue.push_back(secondByte);
    byteQueue.push_back(thirdByte);
    byteQueue.push_back(tmp);
    
    return firstByte;
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
            
            return invalidUTF8(firstByte, tmp);
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
            
            return invalidUTF8(firstByte, tmp);
        }
        
        // Continue
        
        auto secondByte = tmp;
        
        tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            return invalidUTF8(firstByte, secondByte, tmp);
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
            
            return invalidUTF8(firstByte, tmp);
        }
        
        // Continue
        
        auto secondByte = tmp;
        
        tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            return invalidUTF8(firstByte, secondByte, tmp);
        }
        
        // Continue
        
        auto thirdByte = tmp;
        
        tmp = nextByte();
        
        // UTF8 encoding
        if (!((tmp & 0xc0) == 0x80)) {
            
            return invalidUTF8(firstByte, secondByte, thirdByte, tmp);
        }
        
        // Valid
        
        auto decoded = (((firstByte & 0x07) << 18) | ((secondByte & 0x3f) << 12) | ((thirdByte & 0x3f) << 6) | ((tmp & 0x3f)));
        
        return decoded;
        
    } else {
        
        //
        // Not a valid UTF8 prefix, so just assume 8-bit extended ASCII
        //
        
        return invalidUTF8(cIn);
    }
}

ByteDecoder *TheByteDecoder = nullptr;
