
#pragma once

#include <ostream>
#include <array>

//
// Encode a code point into a sequence of UTF-8 bytes
//
class ByteEncoder {
public:
    
    static size_t size(int val);
    
    static void encodeBytes(std::ostream&, int val);
    
    static void encodeBytes(std::array<unsigned char, 4>& arr, int val);
};
