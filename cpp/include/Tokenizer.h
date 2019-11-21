
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
};

class TokenizerContext {
    uint8_t val;
public:
    constexpr TokenizerContext() : val() {}
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

class sbuffer {
    
    std::ostringstream Str;
    
public:
    
    sbuffer() : Str() {}
    
    void clear();
    
    std::string str();
    
    void operator<<(WLCharacter c);
};

enum class WarningPosition {
    NONE,
    BEFORE,
    AFTER
};

//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
    
    Token _currentToken;
    
    WLCharacter _currentWLCharacter;
    
    std::vector<std::pair<WLCharacter, Source>> wlCharacterQueue;
    
    sbuffer String;
    
    std::vector<std::unique_ptr<Issue>> Issues;
    
    SourceLocation TokenStartLoc;
    
    void backup(WLCharacter Char1, SourceLocation Loc1, WLCharacter c, WarningPosition pos);
    
    void handleEndOfFile();
    void handleLineFeed();
    void handleCarriageReturn();
    void handleSpace();
    void handleTab();
    void handleStrangeSpace();
    void handleLineContinuation();
    
    void handleSpaceCharacter();
    void handleNewlineCharacter();
    
    bool expectDigits(int *leadingZeroCount);
    size_t handleDigits(int *leadingZeroCount);
    int handleDigitsOrAlpha(int base);
    
    void handleComment();
    void handleFileOpsBrackets();
    void handleString();
    void handleString_stringifyCurrentLine();
    void handleString_stringifyNextToken_symbol();
    void handleString_stringifyNextToken_file();
    
    void handleSymbol(TokenizerContext Ctxt);
    void handleSymbolSegment(TokenizerContext Ctxt);
    
    void handleNumber();
    int handlePossibleFractionalPart(int base);
    int handlePossibleFractionalPartPastDot(int base, WLCharacter DotChar, SourceLocation DotLoc);
    
    void handleColon();
    void handleOpenParen();
    void handleCloseParen();
    void handleOpenSquare();
    void handleCloseSquare();
    void handleComma();
    void handleOpenCurly();
    void handleCloseCurly();
    void handleDot();
    void handleEqual();
    void handleUnder();
    void handleLess();
    void handleGreater();
    void handleMinus();
    void handleBar();
    void handleSemi();
    void handleBang();
    void handleHash();
    void handlePercent();
    void handleAmp();
    void handleSlash();
    void handleAt();
    void handlePlus();
    void handleTilde();
    void handleQuestion();
    void handleStar();
    void handleCaret();
    void handleSingleQuote();
    
    void handlePunctuationCharacter();
    
    void handleLinearSyntax();
    
    void handleUnhandledBackSlash();
    void handleUninterpretable();
    
    void append(WLCharacter, Source);
    
    WLCharacter nextWLCharacter(NextWLCharacterPolicy policy);
    
    WLCharacter currentWLCharacter() const;
    
    Source getTokenSource() const;
    
public:
    Tokenizer();
    
    void init(SourceStyle style, int mode);

    void deinit();
    
    void nextToken();
    void nextToken_stringifyCurrentLine();
    void nextToken_stringifyNextToken_symbol();
    void nextToken_stringifyNextToken_file();
    
    Token currentToken();

#if !NISSUES
    void addIssue(std::unique_ptr<Issue>);
#endif
    
#if !NISSUES
    std::vector<std::unique_ptr<Issue>>& getIssues();
#endif
    
};

extern std::unique_ptr<Tokenizer> TheTokenizer;

