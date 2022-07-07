
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Token.h" // for Token

#include <set>
#include <cstddef> // for size_t

class ParserSession;

using ParserSessionPtr = ParserSession *;


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

void Tokenizer_nextToken(ParserSessionPtr session, Token Tok);
Token Tokenizer_nextToken0(ParserSessionPtr session, NextPolicy policy);
Token Tokenizer_nextToken0_uncommon(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy);
Token Tokenizer_nextToken0_stringifyAsTag(ParserSessionPtr session);
Token Tokenizer_nextToken0_stringifyAsFile(ParserSessionPtr session);

Token Tokenizer_currentToken(ParserSessionPtr session, NextPolicy policy);
Token Tokenizer_currentToken_stringifyAsTag(ParserSessionPtr session);
Token Tokenizer_currentToken_stringifyAsFile(ParserSessionPtr session);

Token Tokenizer_handleColon(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleOpenParen(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleDot(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleEqual(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleUnder(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleLess(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleGreater(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleBar(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleSemi(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleBang(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleHash(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handlePercent(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleAmp(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleSlash(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleAt(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handlePlus(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleTilde(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleQuestion(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleStar(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleCaret(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);

Token Tokenizer_handleStrangeWhitespace(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleMBLinearSyntaxBlob(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleNakedMBLinearSyntax(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleMBStrangeWhitespace(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleMBStrangeNewline(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleMBPunctuation(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);

Token Tokenizer_handleUnhandledBackslash(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);

WLCharacter Tokenizer_handleSymbolSegment(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer firstCharBuf, SourceLocation firstCharLoc, WLCharacter firstChar, NextPolicy policy);

WLCharacter Tokenizer_handleZeros(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter firstChar, int *count);
WLCharacter Tokenizer_handleDigits(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter firstChar, int *count);
WLCharacter Tokenizer_handleAlphaOrDigits(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt);
WLCharacter Tokenizer_handlePossibleFractionalPart(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt);
WLCharacter Tokenizer_handlePossibleFractionalPartPastDot(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt);

Token Tokenizer_handleComment(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy);
SourceCharacter Tokenizer_handleFileOpsBrackets(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy, int *handled);

Token Tokenizer_handleString_stringifyAsTag(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleString_stringifyAsFile(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy);

Token Tokenizer_handleComma(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleLineFeed(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleSpace(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleSymbol(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleString(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleNumber(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleOpenSquare(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleCloseSquare(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleOpenCurly(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleCloseCurly(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
Token Tokenizer_handleMinus(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);

void Tokenizer_backupAndWarn(ParserSessionPtr session, Buffer resetBuf, SourceLocation resetLoc);

Source Tokenizer_getTokenSource(ParserSessionPtr session, SourceLocation tokStartLoc);

BufferAndLength Tokenizer_getTokenBufferAndLength(ParserSessionPtr session, Buffer tokStartBuf);
