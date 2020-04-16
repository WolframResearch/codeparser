
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
    
    WLCharacter handleLineContinuation(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer escapedBuf, SourceLocation escapedLoc, SourceCharacter firstChar, NextPolicy policy);
    
    WLCharacter handleBackSlash(Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
    
    WLCharacter handleUnhandledEscape(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer unhandledBuf, SourceLocation unhandledLoc, SourceCharacter escapedChar, NextPolicy policy);
    
    std::string longNameSuggestion(BufferAndLength );
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    CharacterDecoder();
    
    void init(WolframLibraryData libData);
    
    void deinit();
    
    WLCharacter nextWLCharacter0(NextPolicy policy);
    
    WLCharacter currentWLCharacter(NextPolicy policy);
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();
#endif // !NISSUES
};

extern CharacterDecoderPtr TheCharacterDecoder;
