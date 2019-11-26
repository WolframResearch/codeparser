
#pragma once

#include "ByteBuffer.h"
#include "Source.h"

#include <vector>
#include <memory> // for unique_ptr

class ByteDecoder;
using ByteDecoderPtr = std::unique_ptr<ByteDecoder>;


//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    
    std::vector<Buffer> offsetLineMap;
    
    std::vector<IssuePtr> Issues;
    
    bool error;
    
    SourceCharacter invalid(Buffer errBuf, NextCharacterPolicy policy);
    
    
public:
    
    Buffer lastBuf;
    
    size_t actualOffsetLineMapSize;
    
    
    
    ByteDecoder();
    
    void init();
    
    void deinit();
    
    SourceCharacter nextSourceCharacter0(NextCharacterPolicy policy);
    
    SourceCharacter currentSourceCharacter(NextCharacterPolicy policy);
    
    
    SourceLocation convertBufferToStart(Buffer ) const;
    
    SourceLocation convertBufferToEnd(Buffer ) const;
    
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();
    
    void addIssue(IssuePtr);
#endif // !NISSUES
    
    std::vector<Buffer> getOffsetLineMap() const;
    
    void setError(bool err);
    
    bool getError() const;
    
    void clearError();
};

extern ByteDecoderPtr TheByteDecoder;

