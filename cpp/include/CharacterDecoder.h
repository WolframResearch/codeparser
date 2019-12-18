
#pragma once

#include "API.h"
#include "ByteDecoder.h"
#include "Source.h"
#include "WLCharacter.h"

#include "WolframLibrary.h"

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
    
    
    WLCharacter handleLongName(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer openSquareBuf, SourceLocation openSquareLoc, NextCharacterPolicy policy);
    WLCharacter handle2Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer dotBuf, SourceLocation dotLoc, NextCharacterPolicy policy);
    WLCharacter handle4Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer colonBuf, SourceLocation colonLoc, NextCharacterPolicy policy);
    WLCharacter handle6Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer barBuf, SourceLocation barLoc, NextCharacterPolicy policy);
    WLCharacter handleOctal(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextCharacterPolicy policy);
    
    WLCharacter handleLineContinuation(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer escapedBuf, SourceLocation escapedLoc, SourceCharacter firstChar, NextCharacterPolicy policy);
    
    WLCharacter handleBackSlash(Buffer escapedBuf, SourceLocation escapedLoc, NextCharacterPolicy policy);
    
    WLCharacter handleUnhandledEscape(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer unhandledBuf, SourceLocation unhandledLoc, SourceCharacter escapedChar, NextCharacterPolicy policy);
    
    std::string longNameSuggestion(BufferAndLength );
    
public:
    
    Buffer lastBuf;
    SourceLocation lastLoc;
    
    CharacterDecoder();
    
    void init(WolframLibraryData libData);
    
    void deinit();
    
    WLCharacter nextWLCharacter0(NextCharacterPolicy policy);
    
    WLCharacter currentWLCharacter(NextCharacterPolicy policy);
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();
#endif // !NISSUES
};

extern CharacterDecoderPtr TheCharacterDecoder;
