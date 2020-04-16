
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Token.h" // for Token

#include <vector>
#include <memory> // for unique_ptr


class Tokenizer;
using TokenizerPtr = std::unique_ptr<Tokenizer>;



//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
    
    std::vector<IssuePtr> Issues;
    
    
    void backup(Buffer resetBuf, SourceLocation resetLoc, bool warn);
    
    Token handleStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleMBStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleMBStrangeNewline(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleComment(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy);
    
    SourceCharacter handleFileOpsBrackets(SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy, int *handled);
    Token handleString(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleString_stringifySymbol(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    Token handleString_stringifyFile(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter firstChar, NextPolicy policy);
    
    Token handleSymbol(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    WLCharacter handleSymbolSegment(Buffer tokenStartBuf, SourceLocation firstCharLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleNumber(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    WLCharacter handleDigits(NextPolicy policy, WLCharacter firstChar, size_t *count);
    WLCharacter handleAlphaOrDigits(WLCharacter firstChar, size_t base, NextPolicy policy, int *handled);
    WLCharacter handlePossibleFractionalPart(Buffer dotBuf, SourceLocation dotLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled);
    WLCharacter handlePossibleFractionalPartPastDot(Buffer dotBuf, SourceLocation dotLoc, WLCharacter firstChar, size_t base, NextPolicy policy, int *handled);
    
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
    
    Token handleMBLinearSyntax(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    Token handleUnhandledBackSlash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy);
    
    
    Source getTokenSource(SourceLocation tokStartLoc) const;
    
    BufferAndLength getTokenBufferAndLength(Buffer tokStartBuf) const;
    
    
public:
    Tokenizer();
    
    void init();

    void deinit();
    
    void nextToken(Token Tok);
    
    void nextToken_stringifySymbol();
    void nextToken_stringifyFile();
    
    Token nextToken0(NextPolicy policy);
    
    Token nextToken0_stringifySymbol();
    Token nextToken0_stringifyFile();
    
    Token currentToken(NextPolicy policy);
    
    Token currentToken_stringifySymbol();
    Token currentToken_stringifyFile();

#if !NISSUES
    void addIssue(IssuePtr);

    std::vector<IssuePtr>& getIssues();
#endif // !NISSUES
    
};

extern TokenizerPtr TheTokenizer;

