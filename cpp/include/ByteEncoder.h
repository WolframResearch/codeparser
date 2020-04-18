
#pragma once

#include "CodePoint.h" // for codepoint

#include <ostream>
#include <array>

//
//
//
struct ByteEncoderState {
    
};

//
// Encode a code point into a sequence of UTF-8 bytes
//
class ByteEncoder {
public:
    
    static size_t size(codepoint val);
    
    static void encodeBytes(std::ostream&, codepoint val, ByteEncoderState *state);
    
    static void encodeBytes(std::array<unsigned char, 4>& arr, codepoint val, ByteEncoderState *state);
};
