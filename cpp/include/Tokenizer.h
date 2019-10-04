
#pragma once

#include "CharacterDecoder.h"
#include "Source.h"
#include "WLCharacter.h"
#include "Token.h"

#include <sstream>
#include <vector>
#include <memory> // for unique_ptr

enum TokenizerContextBits : uint8_t {
    
    //
    // If inside #, then give syntax warnings for #"123" and #a`b syntax (which is undocumented syntax)
    //
    // But obviously "123" and a`b are fine outside of #
    //
    TOKENIZER_SLOT = 0x01,
    
    //
    // Some tokens are "stringified",  b in  a::b
    //
    // This behavior can be controlled with this flag.
    //
    // This is used inside linear syntax.
    //
    TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN = 0x02,
    
    //
    //
    //
    TOKENIZER_STRINGIFY_CURRENT_LINE = 0x04,
};

class TokenizerContext {
    uint8_t val;
public:
    constexpr TokenizerContext() : val(TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) {}
    constexpr TokenizerContext(uint8_t val) : val(val) {}
    
    TokenizerContextBits operator&(const TokenizerContextBits bits) const {
        return static_cast<TokenizerContextBits>(val & bits);
    }
    
    TokenizerContextBits operator|(const TokenizerContextBits bits) const {
        return static_cast<TokenizerContextBits>(val | bits);
    }
    
    void operator|=(const TokenizerContextBits bits) {
        val |= bits;
    }
    
    void clear(const TokenizerContextBits bits) {
        val &= ~bits;
    }
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
    
    std::vector<std::unique_ptr<Issue>> Issues;
    
    
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
    
    void init(SourceStyle style, bool stringifyNextTokenSymbol, bool skipFirstLine);
    void deinit();
    
    void nextToken(TokenizerContext Ctxt);
    
    Token currentToken();
    
    std::vector<std::unique_ptr<Issue>>& getIssues();
};

extern std::unique_ptr<Tokenizer> TheTokenizer;

