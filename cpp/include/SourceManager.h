
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
    
    const char *data;
    size_t dataLength;
    size_t idx;
    
    bool lastCharacterWasCarriageReturn;
    
    std::vector<std::unique_ptr<Issue>> Issues;
    
    SourceLocation SrcLoc;
    
    SourceLocation TokenStartLoc;
    
    SourceLocation WLCharacterStartLoc;
    SourceLocation WLCharacterEndLoc;
    
    SourceLocation PrevWLCharacterStartLoc;
    SourceLocation PrevWLCharacterEndLoc;
    
    WolframLibraryData libData;
    
public:
    SourceManager();
    
    void init(const char *data, size_t dataLength, SourceStyle style, WolframLibraryData libData);
    
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
    
    std::vector<std::unique_ptr<Issue>>& getIssues();
};

extern std::unique_ptr<SourceManager> TheSourceManager;

