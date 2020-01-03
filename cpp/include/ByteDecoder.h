
#pragma once

#include "Source.h" // for IssuePtr, UTF8Status, etc.

#include <vector>
#include <memory> // for unique_ptr

class ByteDecoder;
using ByteDecoderPtr = std::unique_ptr<ByteDecoder>;


//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    
    std::vector<IssuePtr> Issues;
    
    UTF8Status status;
    
    SourceCharacter invalid(SourceLocation errSrcLoc, NextCharacterPolicy policy);
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    SourceLocation SrcLoc;
    
    
    ByteDecoder();
    
    void init();
    
    void deinit();
    
    SourceCharacter nextSourceCharacter0(NextCharacterPolicy policy);
    
    SourceCharacter currentSourceCharacter(NextCharacterPolicy policy);
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();
    
    void addIssue(IssuePtr);
#endif // !NISSUES
    
    void setStatus(UTF8Status status);
    
    UTF8Status getStatus() const;
    
    void clearStatus();
};

extern ByteDecoderPtr TheByteDecoder;

