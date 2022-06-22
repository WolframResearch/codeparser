
#pragma once

#include "Source.h" // for SourceLocation
#include "WLCharacter.h" // for WLCharacter

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include "WolframLibrary.h"
#undef True
#undef False

#include <set>
#include <memory> // for unique_ptr


class CharacterDecoder;

using CharacterDecoderPtr = std::unique_ptr<CharacterDecoder>;


//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences of Source Characters such as \[Alpha] into a single WL character
//
class CharacterDecoder {
private:
    
    std::set<SourceLocation> SimpleLineContinuations;
    std::set<SourceLocation> ComplexLineContinuations;
    std::set<SourceLocation> EmbeddedTabs;
    
    WLCharacter handleLongName(Buffer openSquareBuf, SourceLocation openSquareLoc, NextPolicy policy);
    WLCharacter handle2Hex(Buffer dotBuf, SourceLocation dotLoc, NextPolicy policy);
    WLCharacter handle4Hex(Buffer colonBuf, SourceLocation colonLoc, NextPolicy policy);
    WLCharacter handle6Hex(Buffer barBuf, SourceLocation barLoc, NextPolicy policy);
    WLCharacter handleOctal(Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextPolicy policy);
    WLCharacter handleUncommon(Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
    
    //
    // Handling line continuations belongs in some layer strictly above CharacterDecoder and below Tokenizer.
    //
    // Some middle layer that deals with "parts" of a token.
    //
    SourceCharacter handleLineContinuation(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);
    
    WLCharacter handleBackslash(Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
    
    WLCharacter handleUnhandledEscape(Buffer unhandledBuf, SourceLocation unhandledLoc, NextPolicy policy);
    
    //
    // example:
    // input: Alpa
    // return Alpha
    //
    // Return empty string if no suggestion.
    //
    std::string longNameSuggestion(std::string input);
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    CharacterDecoder();
    
    void init();
    
    void deinit();
    
    // Precondition: buffer is pointing to current WLCharacter
    // Postcondition: buffer is pointing to next WLCharacter
    //
    // Example:
    // memory: 1+\[Alpha]-2
    //           ^
    //           buffer
    //
    // after calling nextWLCharacter:
    // memory: 1+\[Alpha]-2
    //                   ^
    //                   buffer
    // return \[Alpha]
    //
    WLCharacter nextWLCharacter0(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);
    
    //
    // Postcondition: lastBuf is set to the last value of buffer
    // Postcondition: lastLoc is set to the last value of SrcLoc
    //
    WLCharacter currentWLCharacter(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);
    
    std::set<SourceLocation>& getSimpleLineContinuations();
    
    std::set<SourceLocation>& getComplexLineContinuations();
    
    std::set<SourceLocation>& getEmbeddedTabs();
};

extern CharacterDecoderPtr TheCharacterDecoder;
