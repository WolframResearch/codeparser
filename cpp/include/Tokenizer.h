
#pragma once

#include "CharacterDecoder.h"

#include <sstream>

struct TokenizerContext {
    //
    // If inside #, then give syntax warnings for #"123" and #a`b syntax (which are undocumented)
    //
    bool SlotFlag;
    
    //
    // Some tokens are "stringified",  b in  a::b
    //
    // This behavior can be controlled with this flag.
    //
    bool EnableStringifyNextToken;
};

//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
    
    bool stringifyNextToken_symbol;
    bool stringifyNextToken_file;
    Token cur;
    bool currentCached;

    std::vector<std::pair<WLCharacter, SourceLocation>> characterQueue;
    
    WLCharacter _currentWLCharacter;
    SourceLocation _currentSourceLocation;

    std::ostringstream String;

    std::vector<SyntaxIssue> Issues;
    
    bool expectDigits(TokenizerContext Ctxt);
    size_t handleDigits(TokenizerContext Ctxt);
    bool handleDigitsOrAlpha(TokenizerContext Ctxt, int base);
    
    Token handleComment(TokenizerContext Ctxt);
    Token handleFileOpsBrackets(TokenizerContext Ctxt);
    Token handleString(TokenizerContext Ctxt);
    
    void handleSymbolSegment(TokenizerContext Ctxt);
    Token handleSymbol(TokenizerContext Ctxt);
    
    Token handleNumber(TokenizerContext Ctxt);
    bool handleFractionalPart(TokenizerContext Ctxt, int base);
    
    Token handleOperator(TokenizerContext Ctxt);
    
    Token handleLinearSyntax(TokenizerContext Ctxt);
    
    Token handleDot(TokenizerContext Ctxt);

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter();
    void setCurrentWLCharacter(WLCharacter, SourceLocation);
    
public:
    Tokenizer();
    
    void init(bool skipFirstLine);
    void deinit();

    Token nextToken(TokenizerContext Ctxt);
    
    Token currentToken();
    
    std::string getString();

    std::vector<SyntaxIssue> getIssues();
};

extern Tokenizer *TheTokenizer;
