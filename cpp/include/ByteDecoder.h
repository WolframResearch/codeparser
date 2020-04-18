
#pragma once

#include "Source.h" // for IssuePtr, UTF8Status, etc.

#include <vector>
#include <memory> // for unique_ptr

class ByteDecoder;
class SourceConventionManager;
using ByteDecoderPtr = std::unique_ptr<ByteDecoder>;
using SourceConventionManagerPtr = std::unique_ptr<SourceConventionManager>;

//
//
//
class SourceConventionManager {
public:
    
    virtual SourceLocation newSourceLocation() = 0;
    
    virtual void newline(SourceLocation& loc) = 0;
    
    void increment(SourceLocation& loc);
    
    virtual ~SourceConventionManager() {}
};

//
//
//
class LineColumnManager : public SourceConventionManager {
    
    SourceLocation newSourceLocation() override;
    
    void newline(SourceLocation& loc) override;
};

//
//
//
class SourceCharacterIndexManager : public SourceConventionManager {
    
    SourceLocation newSourceLocation() override;
    
    void newline(SourceLocation& loc) override;
};


//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    
    std::vector<IssuePtr> Issues;
    
    UTF8Status status;
    
    SourceConventionManagerPtr srcConventionManager;
    
    
    SourceCharacter invalid(SourceLocation errSrcLoc, NextPolicy policy);
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    SourceLocation SrcLoc;
    
    
    ByteDecoder();
    
    void init(SourceConvention srcConvention);
    
    void deinit();
    
    //
    // Precondition: buffer is pointing to current SourceCharacter
    // Postcondition: buffer is pointing to next SourceCharacter
    //
    // return the SourceCharacter that was current
    //
    // Decode UTF-8 byte sequences
    //
    // Also decode \r\n into a single SourceCharacter
    //
    // \r\n is akin to a 2-byte UTF-8 sequence
    //
    // Also warn about \r line endings
    //
    // Do not decode invalid sequences or surrogates.
    //
    SourceCharacter nextSourceCharacter0(NextPolicy policy);
    
    SourceCharacter currentSourceCharacter(NextPolicy policy);
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();
    
    void addIssue(IssuePtr);
#endif // !NISSUES
    
    void setStatus(UTF8Status status);
    
    UTF8Status getStatus() const;
    
    void clearStatus();
};

extern ByteDecoderPtr TheByteDecoder;

