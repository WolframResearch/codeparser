
#pragma once

#include <ostream>

//
// Encode a code point into a sequence of UTF-8 bytes
//
class ByteEncoder {
public:
    
    static void encodeBytes(std::ostream&, int val);
};

extern ByteEncoder *TheByteEncoder;

