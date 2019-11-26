
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
    
    
    WLCharacter handleLongName(NextCharacterPolicy policy);
    WLCharacter handle2Hex(NextCharacterPolicy policy);
    WLCharacter handle4Hex(NextCharacterPolicy policy);
    WLCharacter handle6Hex(NextCharacterPolicy policy);
    WLCharacter handleOctal(SourceCharacter firstDigit, NextCharacterPolicy policy);
    
    WLCharacter handleLineContinuation(SourceCharacter firstChar, NextCharacterPolicy policy);
    
    WLCharacter handleBackSlash(NextCharacterPolicy policy);
    
    WLCharacter handleUnhandledEscape(Buffer currentWLCharacterStart, SourceCharacter curSource, NextCharacterPolicy policy);
    
    std::string longNameSuggestion(BufferAndLength );
    
public:
    
    Buffer lastBuf;
    
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
