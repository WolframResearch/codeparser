
#include "ByteEncoder.h"

#include <cassert>

void ByteEncoder::encodeBytes(std::ostream& stream, int val) {
    
    assert(val >= 0);
    
    if (val <= 0x7f) {
        
        //
        // ASCII character
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 0) & 0x7f) | 0x00);
        
        stream << firstByte;
        
    } else if (val <= 0x7ff) {
        
        //
        // 2 byte UTF 8 sequence
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 6) & 0x1f) | 0xc0);
        auto secondByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        stream << firstByte;
        stream << secondByte;
        
    } else if (val <= 0xffff) {
        
        //
        // 3 byte UTF 8 sequence
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 12) & 0x0f) | 0xe0);
        auto secondByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        stream << firstByte;
        stream << secondByte;
        stream << thirdByte;
        
    } else {
        
        //
        // 4 byte UTF 8 sequence
        //
        
        assert(val <= 0x10ffff);
        
        auto firstByte = static_cast<unsigned char>(((val >> 18) & 0x07) | 0xf0);
        auto secondByte = static_cast<unsigned char>(((val >> 12) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto fourthByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        stream << firstByte;
        stream << secondByte;
        stream << thirdByte;
        stream << fourthByte;
    }
    
}

ByteEncoder *TheByteEncoder = nullptr;

