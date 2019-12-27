
#pragma once

#include <ostream>
#include <array>

struct ByteEncoderState {
    
};

//
// Encode a code point into a sequence of UTF-8 bytes
//
class ByteEncoder {
public:
    
    static size_t size(int32_t val);
    
    static void encodeBytes(std::ostream&, int32_t val, ByteEncoderState *state);
    
    static void encodeBytes(std::array<unsigned char, 4>& arr, int32_t val, ByteEncoderState *state);
};
