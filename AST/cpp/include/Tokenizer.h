
#pragma once

#include "CharacterDecoder.h"

#include <sstream>

//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
    
    bool _symbolifyNextToken;
    bool _fileifyNextToken;
    Token cur;
    bool currentCached;

    std::vector<std::pair<WLCharacter, SourceLocation>> characterQueue;
    
    WLCharacter _currentWLCharacter;
    SourceLocation _currentSourceLocation;

    std::ostringstream String;

    std::vector<SyntaxIssue> Issues;
    
    bool expectDigits();
    void handleDigits();
    bool handleDigitsOrAlpha(int base);
    void handleDigitsOrAlphaOrDollar();
    
    Token handleComment();
    Token handleFileOpsBrackets();
    Token handleString();
    
    void handleSymbolSegment();
    Token handleSymbol();
    
    Token handleNumber();
    bool handleFractionalPart(int base);
    
    Token handleOperator();
    
    Token handleLinearSyntax();
    
    Token handleDot();

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter();
    void setCurrentWLCharacter(WLCharacter, SourceLocation);
    
public:
    Tokenizer();
    
    void init(bool skipFirstLine);
    void deinit();

    Token nextToken();
    
    Token currentToken();
    
    std::string getString();

    std::vector<SyntaxIssue> getIssues();
};

extern Tokenizer *TheTokenizer;
