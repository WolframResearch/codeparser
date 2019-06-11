
#pragma once

#include "Source.h"

#include <vector>
#include <istream>
#include <chrono>

//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    bool eof;
    std::vector<std::pair<unsigned char, SourceLocation>> byteQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    std::chrono::microseconds totalTimeMicros;
    
    
    unsigned char nextByte();
    
    SourceCharacter decodeBytes(unsigned char);
    
public:
    
    ByteDecoder();
    
    void init();
    
    void deinit();
    
    SourceCharacter nextSourceCharacter();
    
    void append(unsigned char, SourceLocation);
    
    std::vector<SyntaxIssue> getIssues() const;
    
    std::vector<Metadata> getMetadatas() const;
};

extern std::istream *TheInputStream;
extern ByteDecoder *TheByteDecoder;
