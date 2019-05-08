
#pragma once

#include "Source.h"

#include <vector>
#include <istream>

//
// Decode a sequences of UTF-8 encoded bytes into characters
//
class ByteDecoder {
private:
    bool eof;
    std::vector<unsigned char> byteQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    unsigned char nextByte();
    
    SourceCharacter decodeBytes(unsigned char);

    SourceCharacter leaveAlone(std::vector<unsigned char> bytes);
    
public:
    
    ByteDecoder();
    
    void init();
    
    void deinit();
    
    SourceCharacter nextSourceCharacter();
    
    std::vector<SyntaxIssue> getIssues() const;
};

extern std::istream *TheInputStream;
extern ByteDecoder *TheByteDecoder;
