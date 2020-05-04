
#pragma once

#include "Source.h" // for IssuePtr
#include "WLCharacter.h" // for WLCharacter

#include "WolframLibrary.h"
#undef True
#undef False

#include <vector>
#include <memory> // for unique_ptr


class CharacterDecoder;

using CharacterDecoderPtr = std::unique_ptr<CharacterDecoder>;


//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences of Source Characters such as \[Alpha] into a single WL character
//
class CharacterDecoder {
    
    std::vector<IssuePtr> Issues;
    
    WolframLibraryData libData;
    
    
    WLCharacter handleLongName(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer openSquareBuf, SourceLocation openSquareLoc, NextPolicy policy);
    WLCharacter handle2Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer dotBuf, SourceLocation dotLoc, NextPolicy policy);
    WLCharacter handle4Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer colonBuf, SourceLocation colonLoc, NextPolicy policy);
    WLCharacter handle6Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer barBuf, SourceLocation barLoc, NextPolicy policy);
    WLCharacter handleOctal(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextPolicy policy);
    
    //
    // Handling line continuations belongs in some layer strictly above CharacterDecoder and below Tokenizer.
    //
    // Some middle layer that deals with "parts" of a token.
    //
    SourceCharacter handleLineContinuation(SourceCharacter c, NextPolicy policy);
    
    WLCharacter handleBackSlash(Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
    
    WLCharacter handleUnhandledEscape(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer unhandledBuf, SourceLocation unhandledLoc, SourceCharacter escapedChar, NextPolicy policy);
    
    //
    // example:
    // input: Alpa
    // return Alpha
    //
    // Return empty string if no suggestion.
    //
    std::string longNameSuggestion(BufferAndLength );
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    CharacterDecoder();
    
    void init(WolframLibraryData libData);
    
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
    WLCharacter nextWLCharacter0(NextPolicy policy);
    
    //
    // Postcondition: lastBuf is set to the last value of buffer
    // Postcondition: lastLoc is set to the last value of SrcLoc
    //
    WLCharacter currentWLCharacter(NextPolicy policy);
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();
#endif // !NISSUES
};

extern CharacterDecoderPtr TheCharacterDecoder;
