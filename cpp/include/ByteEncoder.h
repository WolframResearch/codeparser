
#pragma once

#include "CodePoint.h" // for codepoint

#include <ostream>
#include <array>
#include <cstddef> // for size_t


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
    
    static void encodeBytes(std::ostream& s, codepoint val, ByteEncoderState *state);
    
    static void encodeBytes(std::array<unsigned char, 4>& arr, codepoint val, ByteEncoderState *state);
};
