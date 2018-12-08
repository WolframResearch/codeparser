
#pragma once

#include <vector>
#include <istream>

typedef int SourceCharacter;

class ByteDecoder {
    
    std::istream &In;
    bool interactive;
    bool eof;
    
    std::vector<unsigned char> byteQueue;
    
    unsigned char nextByte();
    
    SourceCharacter decodeBytes(unsigned char);

    unsigned char leaveAlone(std::vector<unsigned char> bytes);
    
public:
    
    ByteDecoder(std::istream &In, bool interactive);
    
    SourceCharacter nextSourceCharacter();
};

extern ByteDecoder *TheByteDecoder;
