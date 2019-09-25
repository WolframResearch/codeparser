
#pragma once

#include "CharacterDecoder.h"
#include "Source.h"
#include "WLCharacter.h"
#include "Token.h"

#include <sstream>
#include <vector>
#include <memory> // for unique_ptr

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
    Token _currentToken;
    
    WLCharacter _currentWLCharacter;
    
    std::vector<std::pair<WLCharacter, Source>> wlCharacterQueue;
    
    std::ostringstream String;
    
    std::vector<SyntaxIssue> Issues;
    
    
    bool expectDigits(TokenizerContext Ctxt);
    size_t handleDigits(TokenizerContext Ctxt);
    int handleDigitsOrAlpha(TokenizerContext Ctxt, int base);
    
    Token handleComment(TokenizerContext Ctxt);
    Token handleFileOpsBrackets(TokenizerContext Ctxt);
    Token handleString(TokenizerContext Ctxt);
    
    Token handleSymbol(TokenizerContext Ctxt);
    void handleSymbolSegment(TokenizerContext Ctxt);
    
    Token handleNumber(TokenizerContext Ctxt);
    int handleFractionalPart(TokenizerContext Ctxt, int base);
    
    Token handleOperator(TokenizerContext Ctxt);
    
    Token handleLinearSyntax(TokenizerContext Ctxt);
    
    void append(WLCharacter, Source);
    
    WLCharacter nextWLCharacter(NextWLCharacterPolicy policy);
    
    WLCharacter currentWLCharacter() const;
    
public:
    Tokenizer();
    
    void init(bool skipFirstLine);
    void deinit();
    
    Token nextToken(TokenizerContext Ctxt);
    
    Token currentToken() const;
    
    std::vector<SyntaxIssue> getIssues() const;
};

extern std::unique_ptr<Tokenizer> TheTokenizer;

