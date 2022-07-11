
#pragma once

#include "Source.h" // for SourceLocation
#include "WLCharacter.h" // for WLCharacter

#include <set>

class ParserSession;

using ParserSessionPtr = ParserSession *;


//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences of Source Characters such as \[Alpha] into a single WL character
//

WLCharacter CharacterDecoder_nextWLCharacter(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);
WLCharacter CharacterDecoder_currentWLCharacter(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);

WLCharacter CharacterDecoder_handleLongName(ParserSessionPtr session, Buffer openSquareBuf, SourceLocation openSquareLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handle2Hex(ParserSessionPtr session, Buffer dotBuf, SourceLocation dotLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handle4Hex(ParserSessionPtr session, Buffer colonBuf, SourceLocation colonLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handle6Hex(ParserSessionPtr session, Buffer barBuf, SourceLocation barLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleOctal(ParserSessionPtr session, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleUnhandledEscape(ParserSessionPtr session, Buffer unhandledBuf, SourceLocation unhandledLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleBackslash(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);

WLCharacter CharacterDecoder_handleUncommon(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleAssertFalse(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaDoubleQuote(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaOpen(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaClose(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaBackslash(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);

SourceCharacter CharacterDecoder_handleLineContinuation(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);

std::string CharacterDecoder_longNameSuggestion(ParserSessionPtr session, std::string input);
