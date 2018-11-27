
#pragma once

#include "Utils.h"
#include "ByteDecoder.h"
#include "SyntaxIssue.h"

#include <sstream>
#include <vector>

typedef int WLCharacter;

//
// These are the actual code points for linear syntax
//
// LINEARSYNTAX_SPACE does not have a dedicated code point in WL
//
#define LINEARSYNTAX_BANG 0xf7c1
#define LINEARSYNTAX_PERCENT 0xf7c5
#define LINEARSYNTAX_AMP 0xf7c7
#define LINEARSYNTAX_OPENPAREN 0xf7c9
#define LINEARSYNTAX_CLOSEPAREN 0xf7c0
#define LINEARSYNTAX_STAR 0xf7c8
#define LINEARSYNTAX_PLUS 0xf7cb
#define LINEARSYNTAX_SLASH 0xf7cc
#define LINEARSYNTAX_AT 0xf7c2
#define LINEARSYNTAX_CARET 0xf7c6
#define LINEARSYNTAX_UNDER 0xf7ca
#define LINEARSYNTAX_BACKTICK 0xf7cd
#define LINEARSYNTAX_SPACE -2

bool isLinearSyntax(WLCharacter);

std::string WLCharacterToString(WLCharacter c);

enum NextCharacterPolicy {

    POLICY_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION,
    
    POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION,
};

//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences such as \[Alpha] into a single WL character
//
class CharacterDecoder {

    WLCharacter c;

    std::vector<std::pair<SourceCharacter, SourceLocation>> characterQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    void handleLongName(SourceLocation CharacterStart);
    void handle4Hex(SourceLocation CharacterStart);
    void handle2Hex(SourceLocation CharacterStart);
    void handleOctal(SourceLocation CharacterStart);
    void handle6Hex(SourceLocation CharacterStart);
    
public:
    CharacterDecoder();
    
    void init();

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = POLICY_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);

    WLCharacter currentWLCharacter();

    std::vector<SyntaxIssue> getIssues();
};

extern CharacterDecoder *TheCharacterDecoder;
