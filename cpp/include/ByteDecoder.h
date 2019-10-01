
#pragma once

#include "Source.h"

#include <vector>
#include <memory> // for unique_ptr

//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    bool eof;
    std::vector<std::pair<unsigned char, SourceLocation>> byteQueue;
    
    std::vector<std::unique_ptr<Issue>> Issues;
    
    SourceCharacter decodeBytes(unsigned char);
    
    SourceCharacter invalid(unsigned char);
    SourceCharacter invalid(unsigned char, unsigned char);
    SourceCharacter invalid(unsigned char, unsigned char, unsigned char);
    SourceCharacter invalid(unsigned char, unsigned char, unsigned char, unsigned char);
    
public:
    
    ByteDecoder();
    
    void init();
    
    void deinit();
    
    SourceCharacter nextSourceCharacter();
    
    void append(unsigned char, SourceLocation);
    
    std::vector<std::unique_ptr<Issue>>& getIssues();
};

extern std::unique_ptr<ByteDecoder> TheByteDecoder;

