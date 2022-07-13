
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

WLCharacter CharacterDecoder_nextWLCharacter(ParserSessionPtr session, NextPolicy policy);
WLCharacter CharacterDecoder_currentWLCharacter(ParserSessionPtr session, NextPolicy policy);
