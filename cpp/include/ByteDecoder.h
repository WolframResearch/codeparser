
#pragma once

#include "Source.h"

#include <set>

class SourceConventionManager;
class ParserSession;

using SourceConventionManagerPtr = SourceConventionManager *;
using ParserSessionPtr = ParserSession *;


//
// How to manage advancing through SourceLocations
//
class SourceConventionManager {
public:
    
    virtual SourceLocation newSourceLocation() = 0;
    
    virtual void newline(ParserSessionPtr session, SourceLocation& loc) = 0;
    
    virtual void windowsNewline(ParserSessionPtr session, SourceLocation& loc) = 0;
    
    void increment(ParserSessionPtr session, SourceLocation& loc);
    
    virtual void tab(ParserSessionPtr session, SourceLocation& loc) = 0;
    
    virtual ~SourceConventionManager() {}
};

//
// Handle next (non-newline) SourceLocation by incrementing column.
// Handle next newline by incrementing line.
//
class LineColumnManager : public SourceConventionManager {
public:
    
    LineColumnManager() {}
    
    SourceLocation newSourceLocation() override;
    
    void newline(ParserSessionPtr session, SourceLocation& loc) override;
    
    void windowsNewline(ParserSessionPtr session, SourceLocation& loc) override;
    
    void tab(ParserSessionPtr session, SourceLocation& loc) override;
};

//
// Handle next (non-newline) SourceLocation by incrementing index.
// Handle next newline by incrementing index.
//
class SourceCharacterIndexManager : public SourceConventionManager {
public:
    
    SourceLocation newSourceLocation() override;
    
    void newline(ParserSessionPtr session, SourceLocation& loc) override;
    
    void windowsNewline(ParserSessionPtr session, SourceLocation& loc) override;
    
    void tab(ParserSessionPtr session, SourceLocation& loc) override;
};


//
// Decode a sequence of UTF-8 encoded bytes into Source characters
//

SourceCharacter ByteDecoder_nextSourceCharacter(ParserSessionPtr session, NextPolicy policy);
SourceCharacter ByteDecoder_nextSourceCharacter_uncommon(ParserSessionPtr session, NextPolicy policy);
SourceCharacter ByteDecoder_currentSourceCharacter(ParserSessionPtr session, NextPolicy policy);

void ByteDecoder_strangeWarning(ParserSessionPtr session, codepoint decoded, SourceLocation currentSourceCharacterStartLoc, NextPolicy policy);
void ByteDecoder_nonASCIIWarning(ParserSessionPtr session, codepoint decoded, SourceLocation currentSourceCharacterStartLoc);

SourceCharacter ByteDecoder_validStrange(ParserSessionPtr session, codepoint decoded, NextPolicy policy);
SourceCharacter ByteDecoder_validMB(ParserSessionPtr session, codepoint decoded, NextPolicy policy);

SourceCharacter ByteDecoder_incomplete1ByteSequence(ParserSessionPtr session, SourceLocation errSrcLoc, NextPolicy policy);
SourceCharacter ByteDecoder_incomplete2ByteSequence(ParserSessionPtr session, SourceLocation errSrcLoc, NextPolicy policy);
SourceCharacter ByteDecoder_incomplete3ByteSequence(ParserSessionPtr session, SourceLocation errSrcLoc, NextPolicy policy);
SourceCharacter ByteDecoder_straySurrogate(ParserSessionPtr session, SourceLocation errSrcLoc, NextPolicy policy);
SourceCharacter ByteDecoder_bom(ParserSessionPtr session, SourceLocation errSrcLoc, NextPolicy policy);
