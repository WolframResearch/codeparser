
#pragma once

#include "SourceManager.h"

#include <vector>
#include <istream>

class ByteDecoder {
    
    std::istream &In;
    bool interactive;
    bool eof;
    
    std::vector<unsigned char> byteQueue;
    
    unsigned char nextByte();
    
    SourceCharacter decodeBytes(unsigned char);

    SourceCharacter leaveAlone(std::vector<unsigned char> bytes);
    
public:
    
    ByteDecoder(std::istream &In, bool interactive);
    
    SourceCharacter nextSourceCharacter();
};

extern ByteDecoder *TheByteDecoder;
