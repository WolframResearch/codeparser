
#pragma once

#include "Source.h"

#include <vector>
#include <memory> // for unique_ptr

//
// Contain the state needed for advancing to newlines and EOF
//
// i.e., need to keep track of \r\n
//
struct AdvancementState {
    
    bool lastCharacterWasCarriageReturn;
    
    AdvancementState() : lastCharacterWasCarriageReturn() {}
    
    void reset();
    
    SourceLocation advance(SourceCharacter c, SourceLocation SrcLoc);
};

//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    
    SourceLocation SrcLoc;
    
    AdvancementState state;
    
    std::vector<std::unique_ptr<Issue>> Issues;
    
    
    SourceCharacter decodeBytes(unsigned char, bool eof);
    
    SourceCharacter invalid(unsigned char, size_t backup);
    
public:
    
    ByteDecoder();
    
    void init(SourceStyle style);
    
    void deinit();
    
    SourceCharacter nextSourceCharacter();
    
    std::vector<std::unique_ptr<Issue>>& getIssues();
    
    void setSourceLocation(SourceLocation Loc);
    SourceLocation getSourceLocation() const;
    
    void addIssue(std::unique_ptr<Issue>);
};

extern std::unique_ptr<ByteDecoder> TheByteDecoder;

