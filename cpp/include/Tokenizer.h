
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Token.h" // for Token
#include "API.h" // for ENCODINGMODE

#include <set>
#include <memory> // for unique_ptr

class Tokenizer;
using TokenizerPtr = std::unique_ptr<Tokenizer>;

struct NumberTokenizationContext {
    bool InvalidBase;
    bool UnrecognizedDigit;
    bool NegativeExponent;
    bool Real;
    int NonZeroExponentDigitCount;
    //
    // Use the convention that base of 0 means the default, unspecified base
    //
    int Base;
    
    NumberTokenizationContext() : InvalidBase(false), UnrecognizedDigit(false), NegativeExponent(false), Real(false), NonZeroExponentDigitCount(0), Base(0) {}
    
    TokenEnum computeTok();
};


//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
private:
    
    std::set<SourceLocation> EmbeddedNewlines;
    std::set<SourceLocation> EmbeddedTabs;
    
    
    void backupAndWarn(Buffer resetBuf, SourceLocation resetLoc);
    
    Token handleStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleMBStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleMBStrangeNewline(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleComment(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy);
    
    Token handleMBLinearSyntaxBlob(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    //
    // Handle parsing the brackets in:
    // a >> foo[[]]
    //
    // tutorial/OperatorInputForms
    //
    // File Names
    //
    // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
    //
    SourceCharacter handleFileOpsBrackets(SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy, int *handled);
    Token handleString(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleString_stringifyAsTag(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleString_stringifyAsFile(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy);
    
    Token handleSymbol(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    //
    // Precondition: currentWLCharacter is letterlike
    // Postcondition: buffer is pointing to first NON-SYMBOLSEGMENT character after all symbol segment characters
    //
    // return: the first NON-SYMBOLSEGMENT character after all symbol segment characters
    //
    WLCharacter handleSymbolSegment(Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer firstCharBuf, SourceLocation firstCharLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleNumber(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    //
    // Precondition: currentWLCharacter is 0
    // Postcondition: buffer is pointing to first NON-ZERO character after all zeros
    //
    // return: the first NON-ZERO character after all digits
    //
    WLCharacter handleZeros(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter firstChar, int *count);
    
    //
    // Precondition: currentWLCharacter is a digit
    // Postcondition: buffer is pointing to first NON-DIGIT character after all digits
    //
    // return: the first NON-DIGIT character after all digits
    //
    WLCharacter handleDigits(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter firstChar, int *count);
    
    //
    // Precondition: currentWLCharacter is NOT in String
    // Postcondition: currentWLCharacter is the first WLCharacter AFTER all good digits or alphas
    //
    // Return: number of digits handled, possibly 0, or -1 if error
    //
    WLCharacter handleAlphaOrDigits(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt);
    
    //
    // Precondition: currentWLCharacter is NOT in String
    //
    // Return: number of digits handled after ., possibly 0, or -1 if error
    //
    WLCharacter handlePossibleFractionalPart(Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt);
    
    //
    // Precondition: currentWLCharacter is NOT in String
    //
    // Return: number of digits handled after ., possibly 0
    //         UNRECOGNIZED_DIGIT if base error
    //         BAILOUT if not a radix point (and also backup before dot)
    //
    WLCharacter handlePossibleFractionalPartPastDot(Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt);
    
    Token handleColon(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleOpenParen(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleDot(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleEqual(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleUnder(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleLess(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleGreater(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleMinus(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleBar(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleSemi(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleBang(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleHash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handlePercent(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleAmp(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleSlash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleAt(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handlePlus(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleTilde(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleQuestion(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleStar(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleCaret(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleMBPunctuation(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleNakedMBLinearSyntax(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleUnhandledBackslash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    
    Source getTokenSource(SourceLocation tokStartLoc) const;
    
    BufferAndLength getTokenBufferAndLength(Buffer tokStartBuf) const;
    
    
public:
    Tokenizer();
    
    void init();

    void deinit();
    
    void nextToken(Token Tok);
    
    Token nextToken0(NextPolicy policy);
    
    Token nextToken0_stringifyAsTag();
    Token nextToken0_stringifyAsFile();
    
    Token currentToken(NextPolicy policy);
    
    Token currentToken_stringifyAsTag();
    Token currentToken_stringifyAsFile();
    
    std::set<SourceLocation>& getEmbeddedNewlines();
    
    std::set<SourceLocation>& getEmbeddedTabs();
    
};

extern TokenizerPtr TheTokenizer;

