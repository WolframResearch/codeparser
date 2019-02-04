
#include "ByteEncoder.h"

#include "Utils.h"
#include "LongNameDefines.h"

#include <sstream>
#include <iomanip>
#include <iostream>
#include <cassert>

ByteEncoder::ByteEncoder() {}

std::vector<unsigned char> ByteEncoder::encodeBytes(int val) {

    assert(val > 0);

    if (val <= 0x7f) {

        //
        // ASCII character
        //

        auto firstByte = static_cast<unsigned char>(((val >> 0) & 0x7f) | 0x00);

        return { firstByte };

    } else if (val <= 0x7ff) {

        //
        // 2 byte UTF 8 sequence
        //

        auto firstByte = static_cast<unsigned char>(((val >> 6) & 0x1f) | 0xc0);
        auto secondByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        return { firstByte, secondByte };

    } else if (val <= 0xffff) {

        //
        // 3 byte UTF 8 sequence
        //

        auto firstByte = static_cast<unsigned char>(((val >> 12) & 0x0f) | 0xe0);
        auto secondByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        return { firstByte, secondByte, thirdByte };
        
    } else if (val <= 0x10ffff) {

        //
        // 4 byte UTF 8 sequence
        //

        auto firstByte = static_cast<unsigned char>(((val >> 18) & 0x07) | 0xf0);
        auto secondByte = static_cast<unsigned char>(((val >> 12) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto fourthByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        return { firstByte, secondByte, thirdByte, fourthByte };
        
    } else {
        assert(false);
        return {};
    }

}

ByteEncoder *TheByteEncoder = nullptr;
