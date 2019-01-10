
#pragma once

#include "Utils.h"
#include "ByteDecoder.h"
#include "SyntaxIssue.h"

#include <sstream>
#include <vector>

typedef int WLCharacter;

#define WLCHARACTER_EOF -1
//
// These are the actual code points for linear syntax
//
#define WLCHARACTER_LINEARSYNTAX_BANG 0xf7c1
#define WLCHARACTER_LINEARSYNTAX_PERCENT 0xf7c5
#define WLCHARACTER_LINEARSYNTAX_AMP 0xf7c7
#define WLCHARACTER_LINEARSYNTAX_OPENPAREN 0xf7c9
#define WLCHARACTER_LINEARSYNTAX_CLOSEPAREN 0xf7c0
#define WLCHARACTER_LINEARSYNTAX_STAR 0xf7c8
#define WLCHARACTER_LINEARSYNTAX_PLUS 0xf7cb
#define WLCHARACTER_LINEARSYNTAX_SLASH 0xf7cc
#define WLCHARACTER_LINEARSYNTAX_AT 0xf7c2
#define WLCHARACTER_LINEARSYNTAX_CARET 0xf7c6
#define WLCHARACTER_LINEARSYNTAX_UNDER 0xf7ca
#define WLCHARACTER_LINEARSYNTAX_BACKTICK 0xf7cd
//
// LINEARSYNTAX_SPACE does not have a dedicated code point in WL
// So invent one here.
//
#define WLCHARACTER_LINEARSYNTAX_SPACE -2
//
// Something like 1 + \[Bad] would be:
// '1', ' ', '+', ' ', CHARACTER_ERROR_UNRECOGNIZED
//
#define WLCHARACTER_ERROR_UNRECOGNIZED -3
//
// Something like 1 + \:123 would be:
// '1', ' ', '+', ' ', CHARACTER_ERROR_MALFORMED
//
#define WLCHARACTER_ERROR_MALFORMED -4


bool isLinearSyntax(WLCharacter);

std::string WLCharacterToString(WLCharacter c);



enum NextCharacterPolicyBits {
    
    //
    // Preserve whitespace after line continuation
    //
    // ToExpression["0.\\\n  6"] evaluates to 0.6 (whitespace is NOT preserved)
    //
    // But ToExpression["\"0.\\\n  6\""] evalautes to "0.  6" (whitespace IS preserved)
    //
    PRESERVE_WS_AFTER_LC = 0x01,

    //
    // Convert character escapes to a single character
    //
    // Given the 8 bytes \ [ A l p h a ], should the next character be:
    // 0x03b1 (the code point for Alpha character)
    // or 0x005c (the code point for backslash character) ?
    //
    CONVERT_ESCAPES_TO_SINGLE = 0x02
};

typedef int NextCharacterPolicy;

const NextCharacterPolicy TOPLEVEL       = CONVERT_ESCAPES_TO_SINGLE | (PRESERVE_WS_AFTER_LC & 0);
const NextCharacterPolicy INSIDE_NUMBER = TOPLEVEL;
const NextCharacterPolicy INSIDE_STRING  = (CONVERT_ESCAPES_TO_SINGLE & 0) | PRESERVE_WS_AFTER_LC;
const NextCharacterPolicy INSIDE_COMMENT = (CONVERT_ESCAPES_TO_SINGLE & 0) | PRESERVE_WS_AFTER_LC;


//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences such as \[Alpha] into a single WL character
//
class CharacterDecoder {

    WLCharacter c;

    std::vector<std::pair<SourceCharacter, SourceLocation>> characterQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    void handleLongName(SourceLocation CharacterStart, NextCharacterPolicy policy);
    void handle2Hex(SourceLocation CharacterStart, NextCharacterPolicy policy);
    void handle4Hex(SourceLocation CharacterStart, NextCharacterPolicy policy);
    void handle6Hex(SourceLocation CharacterStart, NextCharacterPolicy policy);
    void handleOctal(SourceLocation CharacterStart, NextCharacterPolicy policy);

    void leaveAlone(SourceLocation CharacterStart, std::vector<WLCharacter>);
    
public:
    CharacterDecoder();

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter();

    std::vector<SyntaxIssue> getIssues();
};

extern CharacterDecoder *TheCharacterDecoder;
