
#pragma once

#include "Source.h"

#include "WolframLibrary.h"

#undef True

#undef False

#include <cstddef>
#include <vector>
#include <istream>

class SourceManager {
    
    char* buffer;
    size_t length;
    size_t idx;
    
    bool lastCharacterWasCarriageReturn;
    
    bool advancedToEOF;
    
    std::vector<SyntaxIssue> Issues;
    
    SourceLocation SourceLoc;
    
    SourceLocation TokenStartLoc;
    
    SourceLocation WLCharacterStartLoc;
    SourceLocation WLCharacterEndLoc;
    
    SourceLocation PrevWLCharacterStartLoc;
    SourceLocation PrevWLCharacterEndLoc;
    
    WolframLibraryData libData;
    
public:
    SourceManager();
    
    void init(std::istream& is, WolframLibraryData libData);
    
    void deinit();
    
    void advanceSourceLocation(SourceCharacter c);
    
    unsigned char nextByte();
    
    bool isEndOfFile() const;
    
    void setTokenStart();
    
    void setWLCharacterStart();
    void setWLCharacterEnd();
    
    Source getTokenSpan() const;
    
    SourceLocation getWLCharacterStart() const;
    
    Source getWLCharacterSpan() const;
    
    SourceLocation getTokenStart() const;
    
    void setSourceLocation(SourceLocation Loc);
    SourceLocation getSourceLocation() const;
    
    std::vector<SyntaxIssue> getIssues() const;
};

extern SourceManager *TheSourceManager;

