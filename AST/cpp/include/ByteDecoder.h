
#pragma once

#include <vector>

typedef int SourceCharacter;

class ByteDecoder {
    
    std::istream &In;
    bool singleLine;
    
    bool eof;
    
    std::vector<unsigned char> byteQueue;
    
    unsigned char nextByte();
    
    SourceCharacter decodeBytes(unsigned char);
    
    unsigned char invalidUTF8(unsigned char tmp);
    unsigned char invalidUTF8(unsigned char firstByte, unsigned char tmp);
    unsigned char invalidUTF8(unsigned char firstByte, unsigned char secondByte, unsigned char tmp);
    unsigned char invalidUTF8(unsigned char firstByte, unsigned char secondByte, unsigned char thirdByte, unsigned char tmp);
    
public:
    
    ByteDecoder(std::istream &In, bool singleLine);
    
    SourceCharacter nextSourceCharacter();
};

extern ByteDecoder *TheByteDecoder;
