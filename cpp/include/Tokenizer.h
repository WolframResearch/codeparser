
#pragma once

#include "CharacterDecoder.h"
#include "Source.h"
#include "WLCharacter.h"
#include "Token.h"

#include <vector>
#include <memory> // for unique_ptr


class Tokenizer;
using TokenizerPtr = std::unique_ptr<Tokenizer>;


enum TokenizerContextBits : uint8_t {

    //
    // If inside #, then give syntax warnings for #"123" and #a`b syntax (which is undocumented syntax)
    //
    // But obviously "123" and a`b are fine outside of #
    //
    TOKENIZER_SLOT = 0x01,
    
    //
    // This code:
    // { a, \
    //   b }
    //
    // would give a line continuation warning (line continuation is NOT meaningful)
    //
    // This code:
    // { a, "x\
    //   y", b }
    //
    // would NOT give a warning (line continuation IS meaningful)
    //
//    LC_IS_MEANINGFUL = 0x02,
};

using TokenizerContext = uint8_t;

//
// Tokenizer takes a stream of WL characters and tokenizes them
//
class Tokenizer {
    
    std::vector<IssuePtr> Issues;
    
    
    void backup(Buffer resetBuf, bool warn);
    
    Token handleStrangeSpace(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
    Token handleMBStrangeSpace(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleMBStrangeNewline(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
    Token handleComment(Buffer tokenStartBuf, SourceCharacter firstChar, NextCharacterPolicy policy);
    
    SourceCharacter handleFileOpsBrackets(Buffer tokenStartBuf, SourceCharacter firstChar, NextCharacterPolicy policy, int *handled);
    Token handleString(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
#if STARTOFLINE
    Token handleString_stringifyLine(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
#endif // STARTOFLINE
    
    Token handleString_stringifySymbol(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleString_stringifyFile(Buffer tokenStartBuf, SourceCharacter firstChar, NextCharacterPolicy policy);
    
    Token handleSymbol(Buffer symbolStartBuf, WLCharacter firstChar, NextCharacterPolicy policy, TokenizerContext Ctxt);
    WLCharacter handleSymbolSegment(Buffer firstCharBuf, WLCharacter firstChar, NextCharacterPolicy policy, TokenizerContext Ctxt);
    
    Token handleNumber(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    WLCharacter handleDigits(NextCharacterPolicy policy, WLCharacter firstChar, size_t *count);
    WLCharacter handleAlphaOrDigits(WLCharacter firstChar, size_t base, NextCharacterPolicy policy, int *handled);
    WLCharacter handlePossibleFractionalPart(Buffer dotBuf, WLCharacter firstChar, int base, NextCharacterPolicy policy, int *handled);
    WLCharacter handlePossibleFractionalPartPastDot(Buffer dotBuf, WLCharacter firstChar, int base, NextCharacterPolicy policy, int *handled);
    
    Token handleColon(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleOpenParen(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleDot(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleEqual(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleUnder(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleLess(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleGreater(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleMinus(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleBar(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleSemi(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleBang(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleHash(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handlePercent(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleAmp(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleSlash(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleAt(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handlePlus(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleTilde(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleQuestion(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleStar(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    Token handleCaret(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
    Token handleMBPunctuation(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
    Token handleMBLinearSyntax(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
    Token handleUnhandledBackSlash(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy);
    
    
    Source getTokenSource(SourceLocation tokStartLoc) const;
    
    BufferAndLength getTokenBufferAndLength(Buffer tokStartBuf) const;
    
    
public:
    Tokenizer();
    
    void init();

    void deinit();
    
    void nextToken(NextCharacterPolicy policy);
    
#if STARTOFLINE
    void nextToken_stringifyLine();
#endif // STARTOFLINE
    
    void nextToken_stringifySymbol();
    void nextToken_stringifyFile();
    
    Token nextToken0(NextCharacterPolicy policy);
    
#if STARTOFLINE
    Token nextToken0_stringifyLine();
#endif // STARTOFLINE
    
    Token nextToken0_stringifySymbol();
    Token nextToken0_stringifyFile();
    
    Token currentToken(NextCharacterPolicy policy);
    
#if STARTOFLINE
    Token currentToken_stringifyLine();
#endif // STARTOFLINE
    
    Token currentToken_stringifySymbol();
    Token currentToken_stringifyFile();

#if !NISSUES
    void addIssue(IssuePtr);

    std::vector<IssuePtr>& getIssues();
#endif // !NISSUES
    
};

extern TokenizerPtr TheTokenizer;

