
#pragma once

#include "Source.h"

#include <vector>
#include <istream>

//
// Decode a sequences of UTF-8 encoded bytes into characters
//
class ByteDecoder {
private:
    std::istream &In;
    bool eof;
    std::vector<unsigned char> byteQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    unsigned char nextByte();
    
    SourceCharacter decodeBytes(unsigned char);

    SourceCharacter leaveAlone(std::vector<unsigned char> bytes);
    
public:
    
    ByteDecoder(std::istream &In);
    
    SourceCharacter nextSourceCharacter();
    
    std::vector<SyntaxIssue> getIssues();
};

extern ByteDecoder *TheByteDecoder;
