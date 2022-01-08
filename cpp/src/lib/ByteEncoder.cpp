
#include "ByteEncoder.h"

#include "Utils.h" // for isStraySurrogate

#include <cassert>

size_t ByteEncoder::size(codepoint val) {

    if (val == CODEPOINT_CRLF) {

        return 2;
    }
    
    if (val == CODEPOINT_UNSAFE_1_BYTE_SEQUENCE) {
        
        return 1;
    }
    
    if (val == CODEPOINT_UNSAFE_2_BYTE_SEQUENCE) {
        
        return 2;
    }
    
    if (val == CODEPOINT_UNSAFE_3_BYTE_SEQUENCE) {
        
        return 3;
    }
    
    assert(val >= 0);
    
    if (val <= 0x7f) {

        return 1;

    } else if (val <= 0x7ff) {

        return 2;

    } else if (val <= 0xffff) {
        
        assert(val != CODEPOINT_BOM);
        assert(!Utils::isStraySurrogate(val));
        
        return 3;

    } else {
        
        assert(val <= 0x10ffff);
        
        return 4;
    }
}

void ByteEncoder::encodeBytes(std::ostream& stream, codepoint val, ByteEncoderState *state) {
    
    if (val == CODEPOINT_CRLF) {
        
        stream.put('\r');
        stream.put('\n');
        
        return;
    }
    
    //
    // e.g., GTest was trying to print
    //
    if (val == CODEPOINT_UNSAFE_1_BYTE_SEQUENCE || val == CODEPOINT_UNSAFE_2_BYTE_SEQUENCE || val == CODEPOINT_UNSAFE_3_BYTE_SEQUENCE) {
        
        //
        // Print U+FFFD (REPLACEMENT CHARACTER)
        //
        
        stream.put('\xef');
        stream.put('\xbf');
        stream.put('\xbd');
        
        return;
    }
    
    assert(val >= 0);
    
    if (val <= 0x7f) {
        
        //
        // 1 byte UTF-8 sequence
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 0) & 0x7f) | 0x00);
        
        stream.put(firstByte);
        
    } else if (val <= 0x7ff) {
        
        //
        // 2 byte UTF-8 sequence
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 6) & 0x1f) | 0xc0);
        auto secondByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        stream.put(firstByte);
        stream.put(secondByte);
        
    } else if (val <= 0xffff) {
        
        //
        // 3 byte UTF-8 sequence
        //
        
        assert(val != CODEPOINT_BOM);
        assert(!Utils::isStraySurrogate(val));
        
        auto firstByte = static_cast<unsigned char>(((val >> 12) & 0x0f) | 0xe0);
        auto secondByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        stream.put(firstByte);
        stream.put(secondByte);
        stream.put(thirdByte);
        
    } else {
        
        //
        // 4 byte UTF-8 sequence
        //
        
        assert(val <= 0x10ffff);
        
        auto firstByte = static_cast<unsigned char>(((val >> 18) & 0x07) | 0xf0);
        auto secondByte = static_cast<unsigned char>(((val >> 12) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto fourthByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        stream.put(firstByte);
        stream.put(secondByte);
        stream.put(thirdByte);
        stream.put(fourthByte);
    }
}

void ByteEncoder::encodeBytes(std::array<unsigned char, 4>& arr, codepoint val, ByteEncoderState *state) {
    
    if (val == CODEPOINT_CRLF) {
        
        arr[0] = '\r';
        arr[1] = '\n';
        
        return;
    }
    
    //
    // e.g., GTest was trying to print
    //
    if (val == CODEPOINT_UNSAFE_1_BYTE_SEQUENCE || val == CODEPOINT_UNSAFE_2_BYTE_SEQUENCE || val == CODEPOINT_UNSAFE_3_BYTE_SEQUENCE) {
        
        //
        // Print U+FFFD (REPLACEMENT CHARACTER)
        //
        
        arr[0] = '\xef';
        arr[1] = '\xbf';
        arr[2] = '\xbd';
        
        return;
    }
    
    assert(val >= 0);
    
    if (val <= 0x7f) {
        
        //
        // 1 byte UTF-8 sequence
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 0) & 0x7f) | 0x00);
        
        arr[0] = firstByte;
        
    } else if (val <= 0x7ff) {
        
        //
        // 2 byte UTF-8 sequence
        //
        
        auto firstByte = static_cast<unsigned char>(((val >> 6) & 0x1f) | 0xc0);
        auto secondByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        arr[0] = firstByte;
        arr[1] = secondByte;
        
    } else if (val <= 0xffff) {
        
        //
        // 3 byte UTF-8 sequence
        //
        
        assert(val != CODEPOINT_BOM);
        assert(!Utils::isStraySurrogate(val));
        
        auto firstByte = static_cast<unsigned char>(((val >> 12) & 0x0f) | 0xe0);
        auto secondByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        arr[0] = firstByte;
        arr[1] = secondByte;
        arr[2] = thirdByte;
        
    } else {
        
        //
        // 4 byte UTF-8 sequence
        //
        
        assert(val <= 0x10ffff);
        
        auto firstByte = static_cast<unsigned char>(((val >> 18) & 0x07) | 0xf0);
        auto secondByte = static_cast<unsigned char>(((val >> 12) & 0x3f) | 0x80);
        auto thirdByte = static_cast<unsigned char>(((val >> 6) & 0x3f) | 0x80);
        auto fourthByte = static_cast<unsigned char>(((val >> 0) & 0x3f) | 0x80);
        
        arr[0] = firstByte;
        arr[1] = secondByte;
        arr[2] = thirdByte;
        arr[3] = fourthByte;
    }
}
