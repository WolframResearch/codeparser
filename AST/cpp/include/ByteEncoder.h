
#pragma once

#include <vector>

class ByteEncoder {
    
public:
    
    ByteEncoder();

    static std::vector<unsigned char> encodeBytes(int val);
};

extern ByteEncoder *TheByteEncoder;
