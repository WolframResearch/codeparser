
#pragma once

#include "CharacterDecoder.h"

#include <sstream>

struct TokenizerContext {
    //
    // If inside #, then give syntax warnings for #"123" and #a`b syntax (which is undocumented syntax)
    //
    // But obviously "123" and a`b are fine outside of #
    //
    bool SlotFlag;
    
    //
    // Some tokens are "stringified",  b in  a::b
    //
    // This behavior can be controlled with this flag.
    //
    // This is used inside linear syntax.
    //
    bool EnableStringifyNextToken;
    
    //
    //
    //
    bool StringifyCurrentLine;
    
    
    TokenizerContext() : SlotFlag(false), EnableStringifyNextToken(true), StringifyCurrentLine(false) {}
    
    TokenizerContext(bool SlotFlag, bool EnableStringifyNextToken, bool StringifyCurrentLine) : SlotFlag(SlotFlag), EnableStringifyNextToken(EnableStringifyNextToken), StringifyCurrentLine(StringifyCurrentLine) {}
    
};

//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
    
    bool stringifyNextToken_symbol;
    bool stringifyNextToken_file;
    Token cur;

    WLCharacter _currentWLCharacter;
    
    std::vector<std::pair<WLCharacter, Source>> characterQueue;

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

    void enqueue(WLCharacter, Source);
    
    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter() const;
    
public:
    Tokenizer();
    
    void init(bool skipFirstLine);
    void deinit();

    Token nextToken(TokenizerContext Ctxt);
    
    Token currentToken() const;
    
    std::string getString() const;

    std::vector<SyntaxIssue> getIssues() const;
};

extern Tokenizer *TheTokenizer;
