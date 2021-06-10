
#pragma once

#include "Source.h" // for IssuePtr, UTF8Status, etc.

#include <set>
#include <memory> // for unique_ptr

class ByteDecoder;
class SourceConventionManager;
using ByteDecoderPtr = std::unique_ptr<ByteDecoder>;
using SourceConventionManagerPtr = std::unique_ptr<SourceConventionManager>;

//
// How to manage advancing through SourceLocations
//
class SourceConventionManager {
public:
    
    virtual SourceLocation newSourceLocation() = 0;
    
    virtual void newline(SourceLocation& loc) = 0;
    
    virtual void windowsNewline(SourceLocation& loc) = 0;
    
    void increment(SourceLocation& loc);
    
    virtual void tab(SourceLocation& loc) = 0;
    
    virtual ~SourceConventionManager() {}
};

//
// Handle next (non-newline) SourceLocation by incrementing column.
// Handle next newline by incrementing line.
//
class LineColumnManager : public SourceConventionManager {
    
    //
    // Use uint32_t here to match SourceLocation members
    //
    uint32_t TabWidth;
    
public:
    
    LineColumnManager(uint32_t TabWidth) : TabWidth(TabWidth) {}
    
    
    SourceLocation newSourceLocation() override;
    
    void newline(SourceLocation& loc) override;
    
    void windowsNewline(SourceLocation& loc) override;
    
    void tab(SourceLocation& loc) override;
};

//
// Handle next (non-newline) SourceLocation by incrementing index.
// Handle next newline by incrementing index.
//
class SourceCharacterIndexManager : public SourceConventionManager {
    
    SourceLocation newSourceLocation() override;
    
    void newline(SourceLocation& loc) override;
    
    void windowsNewline(SourceLocation& loc) override;
    
    void tab(SourceLocation& loc) override;
};

//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//
class ByteDecoder {
private:
    
    IssuePtrSet Issues;
    
    UTF8Status status;
    
    SourceConventionManagerPtr srcConventionManager;

    int encodingMode;
    
    
    void strangeWarning(codepoint decoded, SourceLocation currentSourceCharacterStartLoc, double confidence);
    
    void nonASCIIWarning(codepoint decoded, SourceLocation currentSourceCharacterStartLoc, double confidence);
    
    SourceCharacter invalidReturn(SourceLocation errSrcLoc, NextPolicy policy);
    
    SourceCharacter surrogateReturn(SourceLocation errSrcLoc, NextPolicy policy);
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    SourceLocation SrcLoc;
    
    
    ByteDecoder();
    
    void init(SourceConvention srcConvention, uint32_t TabWidth, int encodingMode);
    
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
    
    //
    // Postcondition: lastBuf is set to the last value of buffer
    // Postcondition: lastLoc is set to the last value of SrcLoc
    //
    SourceCharacter currentSourceCharacter(NextPolicy policy);
    
#if !NISSUES
    IssuePtrSet& getIssues();
    
    void addIssue(IssuePtr);
#endif // !NISSUES
    
    void setStatus(UTF8Status status);
    
    UTF8Status getStatus() const;
    
    void clearStatus();
};

extern ByteDecoderPtr TheByteDecoder;

