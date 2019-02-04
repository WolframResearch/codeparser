
#pragma once

#include "SourceManager.h"

#include <vector>
#include <istream>

class ByteEncoder {
    
public:
    
    ByteEncoder();

    static std::vector<unsigned char> encodeBytes(int val);
};

extern ByteEncoder *TheByteEncoder;
