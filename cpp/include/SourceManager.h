
#pragma once

#include "Source.h"

#include "WolframLibrary.h"

#undef True

#undef False

#include <cstddef>
#include <vector>
#include <istream>
#include <memory> // for unique_ptr

class SourceManager {
    
    std::unique_ptr<char[]> buffer;
    size_t length;
    size_t idx;
    
    bool lastCharacterWasCarriageReturn;
    
    bool advancedToEOF;
    
    std::vector<SyntaxIssue> Issues;
    
    SourceLocation SrcLoc;
    
    SourceLocation TokenStartLoc;
    
    SourceLocation WLCharacterStartLoc;
    SourceLocation WLCharacterEndLoc;
    
    SourceLocation PrevWLCharacterStartLoc;
    SourceLocation PrevWLCharacterEndLoc;
    
    WolframLibraryData libData;
    
public:
    SourceManager();
    
    void init(SourceStyle style, std::istream& is, WolframLibraryData libData);
    
    void deinit();
    
    void advanceSourceLocation(SourceCharacter c);
    
    unsigned char nextByte();
    
    bool isEndOfFile() const;
    
    void setTokenStart();
    
    void setWLCharacterStart();
    void setWLCharacterEnd();
    
    Source getTokenSource() const;
    
    SourceLocation getWLCharacterStart() const;
    
    Source getWLCharacterSource() const;
    
    SourceLocation getTokenStart() const;
    
    void setSourceLocation(SourceLocation Loc);
    SourceLocation getSourceLocation() const;
    
    std::vector<SyntaxIssue> getIssues() const;
};

extern std::unique_ptr<SourceManager> TheSourceManager;

