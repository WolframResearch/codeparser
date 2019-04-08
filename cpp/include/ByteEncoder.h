
#pragma once

#include <vector>

//
// Encode a code point into a sequence of UTF-8 bytes
//
class ByteEncoder {
public:
    
    ByteEncoder();

    static std::vector<unsigned char> encodeBytes(int val);
};

extern ByteEncoder *TheByteEncoder;
