
#include "Tokenizer.h"

#include "CharacterDecoder.h" // for TheCharacterDecoder
#include "ByteDecoder.h" // for TheByteDecoder
#include "Utils.h" // for strangeLetterlikeWarning
#include "MyStringRegistration.h"
#include "ParserSession.h"
#include "TokenEnumRegistration.h"
#include "WLCharacter.h" // for WLCharacter
#include "CodePoint.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <cstring> // for memchr
#include <array>
#include <cassert>

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


struct NumberTokenizationContext {
    
    bool InvalidBase;
    
    bool UnrecognizedDigit;
    
    bool NegativeExponent;
    
    bool Real;
    
    int NonZeroExponentDigitCount;
    //
    // Use the convention that base of 0 means the default, unspecified base
    //
    size_t Base;
    
    NumberTokenizationContext() : InvalidBase(false), UnrecognizedDigit(false), NegativeExponent(false), Real(false), NonZeroExponentDigitCount(0), Base(0) {}
    
    TokenEnum computeTok();
};

Token Tokenizer_nextToken_uncommon(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy);

WLCharacter Tokenizer_nextWLCharacter(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);
WLCharacter Tokenizer_currentWLCharacter(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy);

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


using HandlerFunction = Token(ParserSessionPtr session, Buffer startBuf, SourceLocation startLoc, WLCharacter c, NextPolicy policy);
using HandlerFunctionPtr = HandlerFunction *;

#define U Tokenizer_nextToken_uncommon
#define S Tokenizer_handleSymbol
#define N Tokenizer_handleNumber

std::array<HandlerFunctionPtr, 128> TokenizerHandlerTable = {
    U, U, U, U, U, U, U, U, U, U, Tokenizer_handleLineFeed, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U,
    Tokenizer_handleSpace, U, Tokenizer_handleString, U, S, U, U, U, U, U, U, U, Tokenizer_handleComma, Tokenizer_handleMinus, U, U, N, N, N, N, N, N, N, N, N, N, U, U, U, U, U, U,
    U, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, Tokenizer_handleOpenSquare, U, Tokenizer_handleCloseSquare, U, U,
    U, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, Tokenizer_handleOpenCurly, U, Tokenizer_handleCloseCurly, U, U,
};

#undef U
#undef S
#undef N


// Precondition: buffer is pointing to current token
// Postcondition: buffer is pointing to next token
//
// Example:
// memory: 1+\[Alpha]-2
//           ^
//           buffer
//
// after calling nextToken0:
// memory: 1+\[Alpha]-2
//                   ^
//                   buffer
// return \[Alpha]
//
Token Tokenizer_nextToken(ParserSessionPtr session, NextPolicy policy) {
    
    auto tokenStartBuf = session->buffer;
    auto tokenStartLoc = session->SrcLoc;
    
    auto c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    auto point = c.to_point();
    
    if (!(0x00 <= point && point <= 0x7f)) {
        return Tokenizer_nextToken_uncommon(session, tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    return (TokenizerHandlerTable[point])(session, tokenStartBuf, tokenStartLoc, c, policy);
}


Token Tokenizer_nextToken_uncommon(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
            
    switch (c.to_point()) {
            
        //
        // all single-byte characters
        //
        // most control characters are letterlike
        // jessef: There may be such a thing as *too* binary-safe...
        //
        case '`':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': /*    \x07*/
        case '\x08': /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f': {
            
            MUSTTAIL
            return Tokenizer_handleSymbol(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case CODEPOINT_BEL: case CODEPOINT_DEL: {
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '\t': {
            return Token(TOKEN_WHITESPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '\v': case '\f': {
            
//            MUSTTAIL
            return Tokenizer_handleStrangeWhitespace(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '\r': {
            
#if DIAGNOSTICS
            Tokenizer_NewlineCount++;
#endif // DIAGNOSTICS
            
            //
            // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
            //
            return Token((policy & RETURN_TOPLEVELNEWLINE) ? TOKEN_TOPLEVELNEWLINE : TOKEN_INTERNALNEWLINE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '(': {
            
//            MUSTTAIL
            return Tokenizer_handleOpenParen(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case ')': {
            
#if DIAGNOSTICS
            Tokenizer_CloseParenCount++;
#endif // DIAGNOSTICS
            
            return Token(TOKEN_CLOSEPAREN, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '+': {
            
//            MUSTTAIL
            return Tokenizer_handlePlus(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '^': {
            
//            MUSTTAIL
            return Tokenizer_handleCaret(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '=': {
            
//            MUSTTAIL
            return Tokenizer_handleEqual(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case ';': {
            
//            MUSTTAIL
            return Tokenizer_handleSemi(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case ':': {
            
//            MUSTTAIL
            return Tokenizer_handleColon(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '#': {
            
//            MUSTTAIL
            return Tokenizer_handleHash(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '&': {
            
//            MUSTTAIL
            return Tokenizer_handleAmp(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '!': {
            
//            MUSTTAIL
            return Tokenizer_handleBang(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '%': {
            
//            MUSTTAIL
            return Tokenizer_handlePercent(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '\'': {
            
            return Token(TOKEN_SINGLEQUOTE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '*': {
            
//            MUSTTAIL
            return Tokenizer_handleStar(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '.': {
            
//            MUSTTAIL
            return Tokenizer_handleDot(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '/': {
            
//            MUSTTAIL
            return Tokenizer_handleSlash(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '<': {
            
//            MUSTTAIL
            return Tokenizer_handleLess(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '>': {
            
//            MUSTTAIL
            return Tokenizer_handleGreater(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '?': {
            
//            MUSTTAIL
            return Tokenizer_handleQuestion(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '@': {
            
//            MUSTTAIL
            return Tokenizer_handleAt(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '\\': {
            
//            MUSTTAIL
            return Tokenizer_handleUnhandledBackslash(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '_': {
            
//            MUSTTAIL
            return Tokenizer_handleUnder(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '|': {
            
//            MUSTTAIL
            return Tokenizer_handleBar(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case '~': {
            
//            MUSTTAIL
            return Tokenizer_handleTilde(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case CODEPOINT_ENDOFFILE: {
            return Token(TOKEN_ENDOFFILE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_BANG: {
            return Token(TOKEN_LINEARSYNTAX_BANG, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_OPENPAREN: {
            
//            MUSTTAIL
            return Tokenizer_handleMBLinearSyntaxBlob(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        case CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE:
        case CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE:
        case CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE: {
            
            //
            // This will be disposed before the user sees it
            //
            
            return Token(TOKEN_ERROR_UNSAFECHARACTERENCODING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
            
    if (c.isMBLinearSyntax()) {
        
//        MUSTTAIL
        return Tokenizer_handleNakedMBLinearSyntax(session, tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    if (c.isMBUninterpretable()) {
        return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    if (c.isMBStrangeWhitespace()) {
        
//        MUSTTAIL
        return Tokenizer_handleMBStrangeWhitespace(session, tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    if (c.isMBWhitespace()) {
        return Token(TOKEN_WHITESPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    if (c.isMBStrangeNewline()) {
        
//        MUSTTAIL
        return Tokenizer_handleMBStrangeNewline(session, tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    if (c.isMBNewline()) {
        
        //
        // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
        //
        return Token((policy & RETURN_TOPLEVELNEWLINE) ? TOKEN_TOPLEVELNEWLINE : TOKEN_INTERNALNEWLINE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    if (c.isMBPunctuation()) {
        
//        MUSTTAIL
        return Tokenizer_handleMBPunctuation(session, tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    if (c.isMBStringMeta()) {
        return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
        
    //
    // if nothing else, then it is letterlike
    //
    
    assert(c.isMBLetterlike());
    
//    MUSTTAIL
    return Tokenizer_handleSymbol(session, tokenStartBuf, tokenStartLoc, c, policy);
}


Token Tokenizer_nextToken_stringifyAsTag(ParserSessionPtr session) {
    
    auto tokenStartBuf = session->buffer;
    auto tokenStartLoc = session->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_AS_TAG;
    
    auto c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case CODEPOINT_ENDOFFILE: {
            
            //
            // EndOfFile is special, so invent source
            //
            
            return Token(TOKEN_ERROR_EXPECTEDTAG, tokenStartBuf, 0, Source(tokenStartLoc));
        }
        case '\n': case '\r': case CODEPOINT_CRLF: {
            
            //
            // Newline is special, so invent source
            //
            
            return Token(TOKEN_ERROR_EXPECTEDTAG, tokenStartBuf, 0, Source(tokenStartLoc));
        }
        case '"': {
            return Tokenizer_handleString(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
    }
    
    return Tokenizer_handleString_stringifyAsTag(session, tokenStartBuf, tokenStartLoc, c, policy);
}

//
// Use SourceCharacters here, not WLCharacters
//
Token Tokenizer_nextToken_stringifyAsFile(ParserSessionPtr session) {
    
    auto tokenStartBuf = session->buffer;
    auto tokenStartLoc = session->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_AS_FILE;
    
    auto c = ByteDecoder_nextSourceCharacter(session, policy);
    
    switch (c.to_point()) {
        case CODEPOINT_ENDOFFILE: {
            return Token(TOKEN_ERROR_EXPECTEDFILE, tokenStartBuf, 0, Source(tokenStartLoc));
        }
        case '\n': case '\r': case CODEPOINT_CRLF: {
            
            //
            // Stringifying as a file can span lines
            //
            // Something like  a >>
            //                    b
            //
            // should work
            //
            // Do not use TOKEN_ERROR_EMPTYSTRING here
            //
            
            //
            // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
            //
            return Token((policy & RETURN_TOPLEVELNEWLINE) ? TOKEN_TOPLEVELNEWLINE : TOKEN_INTERNALNEWLINE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case ' ': case '\t': {
            
            //
            // There could be space, something like  << abc
            //
            // or something like:
            // a >>
            //   b
            //
            return Token(TOKEN_WHITESPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '"': {
            return Tokenizer_handleString(session, tokenStartBuf, tokenStartLoc, WLCharacter(c.to_point()), policy);
        }
    }
    
    return Tokenizer_handleString_stringifyAsFile(session, tokenStartBuf, tokenStartLoc, c, policy);
}


Token Tokenizer_currentToken(ParserSessionPtr session, NextPolicy policy) {
    
    auto insideGroup = !session->GroupStack.empty();
    
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    
    policy &= ~(returnInternalNewlineMask);
    
    auto resetBuf = session->buffer;
    auto resetLoc = session->SrcLoc;
    
    auto Tok = Tokenizer_nextToken(session, policy);
    
    session->buffer = resetBuf;
    session->SrcLoc = resetLoc;
    
    return Tok;
}


Token Tokenizer_currentToken_stringifyAsTag(ParserSessionPtr session) {
    
    auto resetBuf = session->buffer;
    auto resetLoc = session->SrcLoc;
    
    auto Tok = Tokenizer_nextToken_stringifyAsTag(session);
    
    session->buffer = resetBuf;
    session->SrcLoc = resetLoc;
    
    return Tok;
}

Token Tokenizer_currentToken_stringifyAsFile(ParserSessionPtr session) {
    
    auto resetBuf = session->buffer;
    auto resetLoc = session->SrcLoc;
    
    auto Tok = Tokenizer_nextToken_stringifyAsFile(session);
    
    session->buffer = resetBuf;
    session->SrcLoc = resetLoc;
    
    return Tok;
}


//
// Handling line continuations belongs in some layer strictly above CharacterDecoder and below Tokenizer.
//
// Some middle layer that deals with "parts" of a token.
//
WLCharacter Tokenizer_nextWLCharacter(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_LineContinuationCount++;
#endif // DIAGNOSTICS
    
    auto c = CharacterDecoder_nextWLCharacter(session, policy);
    
    auto point = c.to_point();
    
    while (true) {

        //
        // this is a negative range, so remember to test with >=
        //
        if (!(CODEPOINT_LINECONTINUATION_LINEFEED >= point && point >= CODEPOINT_LINECONTINUATION_CRLF)) {
            return c;
        }
        
        c = CharacterDecoder_currentWLCharacter(session, policy);
        
        point = c.to_point();
        
        //
        // Even though strings preserve the whitespace after a line continuation, and
        // e.g., integers do NOT preserve the whitespace after a line continuation,
        // we do not need to worry about that here.
        //
        // There are no choices to be made here.
        // All whitespace after a line continuation can be ignored for the purposes of tokenization
        //
        while (c.isWhitespace()) {

#if COMPUTE_OOB
            if (point == '\t') {
                
                if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {

                    //
                    // It is possible to have e.g.:
                    //
                    //"a<backslash>
                    //<tab>b"
                    //
                    // where the embedded tab gets consumed by the whitespace loop after the line continuation.
                    //
                    // Must still count the embedded tab

                    session->addEmbeddedTab(tokenStartLoc);
                }
            }
#endif // COMPUTE_OOB

            CharacterDecoder_nextWLCharacter(session, policy);
            
            c = CharacterDecoder_currentWLCharacter(session, policy);
            
            point = c.to_point();
        }

#if COMPUTE_OOB
        if ((policy & TRACK_LC) == TRACK_LC) {
            
            if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
                
                session->addComplexLineContinuation(tokenStartLoc);
                
            } else {
                
                session->addSimpleLineContinuation(tokenStartLoc);
            }
        }
#endif // COMPUTE_OOB
    
        CharacterDecoder_nextWLCharacter(session, policy);
        
    } // while(true)
}

WLCharacter Tokenizer_currentWLCharacter(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy) {
    
    auto resetBuf = session->buffer;
    auto resetLoc = session->SrcLoc;
    
    //
    //
    //
    policy &= ~(TRACK_LC);
    
    auto c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    session->buffer = resetBuf;
    session->SrcLoc = resetLoc;
    
    return c;
}


Token Tokenizer_handleComma(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_CommaCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_COMMA, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

Token Tokenizer_handleLineFeed(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_NewlineCount++;
#endif // DIAGNOSTICS
            
    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return Token((policy & RETURN_TOPLEVELNEWLINE) ? TOKEN_TOPLEVELNEWLINE : TOKEN_INTERNALNEWLINE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

Token Tokenizer_handleOpenSquare(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_OpenSquareCount++;
#endif // DIAGNOSTICS

    return Token(TOKEN_OPENSQUARE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

Token Tokenizer_handleOpenCurly(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_OpenCurlyCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_OPENCURLY, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

Token Tokenizer_handleSpace(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_WhitespaceCount++;
#endif // DIAGNOSTICS

    return Token(TOKEN_WHITESPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

Token Tokenizer_handleCloseSquare(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_CloseSquareCount++;
#endif // DIAGNOSTICS
            
    return Token(TOKEN_CLOSESQUARE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

Token Tokenizer_handleCloseCurly(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {

#if DIAGNOSTICS
    Tokenizer_CloseCurlyCount++;
#endif // DIAGNOSTICS
            
    return Token(TOKEN_CLOSECURLY, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}


inline Token Tokenizer_handleStrangeWhitespace(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isStrangeWhitespace());
    
#if CHECK_ISSUES
    {
        auto Src = Source(tokenStartLoc, session->SrcLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDSPACECHARACTER, "Unexpected space character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.95, Actions, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    return Token(TOKEN_WHITESPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

//
// Use SourceCharacters here, not WLCharacters
//
// Comments deal with (**) SourceCharacters
// Escaped characters do not work
//
// Important to process SourceCharacters here: (* \\.28\\.2a *)
//
inline Token Tokenizer_handleComment(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy) {
    
    //
    // comment is already started
    //
    
    assert(c.to_point() == '*');
    
    ByteDecoder_nextSourceCharacter(session, policy);
    
#if DIAGNOSTICS
    Tokenizer_CommentCount++;
#endif // DIAGNOSTICS
    
    policy |= STRING_OR_COMMENT;
    
    auto depth = 1;
    
    c = ByteDecoder_currentSourceCharacter(session, policy);
    
    while (true) {
        
        //
        // No need to check for comment length
        //
        
        switch (c.to_point()) {
            case '(': {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                if (c.to_point() == '*') {
                    
                    ByteDecoder_nextSourceCharacter(session, policy);
                    
                    depth = depth + 1;
                    
                    c = ByteDecoder_currentSourceCharacter(session, policy);
                }
                
                break;
            }
            case '*': {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                if (c.to_point() == ')') {
                    
                    ByteDecoder_nextSourceCharacter(session, policy);
                    
                    // This comment is closing
                    
                    depth = depth - 1;
                    
                    if (depth == 0) {
                        return Token(TOKEN_COMMENT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                    
                    c = ByteDecoder_currentSourceCharacter(session, policy);
                }
                
                break;
            }
            case CODEPOINT_ENDOFFILE: {
                return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            case '\n': case '\r': case CODEPOINT_CRLF: {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
#if COMPUTE_OOB
                session->addEmbeddedNewline(tokenStartLoc);
#endif // COMPUTE_OOB
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                break;
            }
            case '\t': {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
#if COMPUTE_OOB
                session->addEmbeddedTab(tokenStartLoc);
#endif // COMPUTE_OOB
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                break;
            }
            default: {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                break;
            }
        }
        
    } // while
}

inline Token Tokenizer_handleMBLinearSyntaxBlob(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == CODEPOINT_LINEARSYNTAX_OPENPAREN);
    
    auto depth = 1;
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        switch (c.to_point()) {
            case CODEPOINT_LINEARSYNTAX_OPENPAREN: {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                depth = depth + 1;
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                break;
            }
            case CODEPOINT_LINEARSYNTAX_CLOSEPAREN: {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                depth = depth - 1;
                
                if (depth == 0) {
                    return Token(TOKEN_LINEARSYNTAXBLOB, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                break;
            }
            case CODEPOINT_ENDOFFILE: {
                return Token(TOKEN_ERROR_UNTERMINATEDLINEARSYNTAXBLOB, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            default: {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                break;
            }
        }
    } // while
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
inline Token Tokenizer_handleSymbol(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '`' || c.isLetterlike() || c.isMBLetterlike());
    
#if DIAGNOSTICS
    Tokenizer_SymbolCount++;
#endif // DIAGNOSTICS
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        c = Tokenizer_handleSymbolSegment(session, tokenStartBuf, tokenStartLoc, tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    //
    // if c == '`', then buffer is pointing past ` now
    //
    
    while (true) {
        
        if (c.to_point() != '`') {
            break;
        }
        
#if CHECK_ISSUES
        if ((policy & INSIDE_SLOT) == INSIDE_SLOT) {
            
            //
            // Something like  #`a
            //
            // It's hard to keep track of the ` characters, so just report the entire symbol. Oh well
            //
            
            auto I = new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the **`** character.", STRING_WARNING, Source(tokenStartLoc, session->SrcLoc), 0.33, {}, {});
            
            session->addIssue(I);
        }
#endif // CHECK_ISSUES
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        if (c.isLetterlike() || c.isMBLetterlike()) {
            
            auto letterlikeBuf = session->buffer;
            auto letterlikeLoc = session->SrcLoc;
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_handleSymbolSegment(session, tokenStartBuf, tokenStartLoc, letterlikeBuf, letterlikeLoc, c, policy);
            
        } else {
            
            //
            // Something like  a`1
            //
            
            return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        
    } // while
        
    return Token(((policy & INSIDE_SLOT) == INSIDE_SLOT) ? TOKEN_STRING : TOKEN_SYMBOL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

//
// Precondition: currentWLCharacter is letterlike
// Postcondition: buffer is pointing to first NON-SYMBOLSEGMENT character after all symbol segment characters
//
// return: the first NON-SYMBOLSEGMENT character after all symbol segment characters
//
inline WLCharacter Tokenizer_handleSymbolSegment(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer charBuf, SourceLocation charLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isLetterlike() || c.isMBLetterlike());
    
#if CHECK_ISSUES
    if (c.to_point() == '$') {
        
        if ((policy & INSIDE_SLOT) == INSIDE_SLOT) {
            
            //
            // Something like  #$a
            //
            
            auto I = new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``$`` character.", STRING_WARNING, Source(charLoc, session->SrcLoc), 0.33, {}, {});
            
            session->addIssue(I);
        }
        
    } else if (c.isStrangeLetterlike()) {
        
        auto Src = Source(charLoc, session->SrcLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.85, Actions, {});
        
        session->addIssue(I);
        
    } else if (c.isMBStrangeLetterlike()) {
        
        auto Src = Source(charLoc, session->SrcLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.80, Actions, {});
        
        session->addIssue(I);
        
    } else if (!c.isAlpha()) {
        
        if ((policy & INSIDE_STRINGIFY_AS_TAG) == INSIDE_STRINGIFY_AS_TAG) {

            //
            // Something like  a::\[Beta]
            //

            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "The tag has non-alphanumeric source characters.", STRING_WARNING, Source(charLoc, session->SrcLoc), 0.85, {}, {});

            session->addIssue(I);
        }
    }
#endif // CHECK_ISSUES
    
    charLoc = session->SrcLoc;
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        if (c.isDigit()) {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            charLoc = session->SrcLoc;
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
        } else if (c.isLetterlike() || c.isMBLetterlike()) {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
#if CHECK_ISSUES
            if (c.to_point() == '$') {
                
                if ((policy & INSIDE_SLOT) == INSIDE_SLOT) {
                    
                    //
                    // Something like  #$a
                    //
                    
                    auto I = new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``$`` character.", STRING_WARNING, Source(charLoc, session->SrcLoc), 0.33, {}, {});
                    
                    session->addIssue(I);
                }
                
            } else if (c.isStrangeLetterlike()) {
                
                auto Src = Source(charLoc, session->SrcLoc);
                
                CodeActionPtrVector Actions;
                
                for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                    Actions.push_back(A);
                }
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.85, Actions, {});
                
                session->addIssue(I);
                
            } else if (c.isMBStrangeLetterlike()) {
                
                auto Src = Source(charLoc, session->SrcLoc);
                
                CodeActionPtrVector Actions;
                
                for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                    Actions.push_back(A);
                }
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.80, Actions, {});
                
                session->addIssue(I);
                
            } else if (!c.isAlphaOrDigit()) {
                
                if ((policy & INSIDE_STRINGIFY_AS_TAG) == INSIDE_STRINGIFY_AS_TAG) {
                    
                    //
                    // Something like  a::b\[Beta]
                    //
                    
                    auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "The tag has non-alphanumeric source characters.", STRING_WARNING, Source(charLoc, session->SrcLoc), 0.85, {}, {});
                    
                    session->addIssue(I);
                }
            }
#endif // CHECK_ISSUES
            
            charLoc = session->SrcLoc;
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
        } else if (c.to_point() == '`') {
            
            //
            // Advance past trailing `
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            break;
            
        } else {
            break;
        }
        
    } // while
    
    return c;
}

inline Token Tokenizer_handleString(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '"');
    
#if CHECK_ISSUES
    if ((policy & INSIDE_SLOT) == INSIDE_SLOT) {
        
        //
        // Something like  #"a"
        //
        
        auto I = new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``\"`` character.", STRING_WARNING, Source(tokenStartLoc, session->SrcLoc), 0.33, {}, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    Buffer quotPtr = nullptr;
    bool fast = false;
    bool terminated = false;
    
#if !COMPUTE_OOB && !CHECK_ISSUES && !COMPUTE_SOURCE && FAST_STRING_SCAN
    
    //
    // !CHECK_ISSUES (so do not need to warn about strange SourceCharacters)
    // !COMPUTE_OOB (so do not need to care about embedded newlines or tabs)
    // !COMPUTE_SOURCE (so do not need to keep track of line and column information)
    //
    // FAST_STRING_SCAN (as a final check that skipping bad SourceCharacters and WLCharacters is ok)
    //
    
    //
    // The idea is to use memchr to scan for the next '"' character byte and then just jump to it.
    //
    // This is faster than explicitly calling TheCharacterDecoder->nextWLCharacter0 over and over again.
    //
    // Diagnostics that count SourceCharacters and WLCharacters will not be accurate inside of fast strings.
    //
    // Bad SourceCharacters will not be detected. This means that incomplete sequences, stray surrogates, and BOM will not be reported.
    //
    // Bad WLCharacters will not be detected. This means that badly escaped characters will not be reported.
    //
    
    quotPtr = static_cast<Buffer>(std::memchr(session->buffer, '"', session->end - session->buffer));
    
    if (quotPtr) {
        
        if (*(quotPtr - 1) != '\\') {
            
            //
            // first double-quote character is NOT preceded by a backslash character
            //
            
            fast = true;
            terminated = true;
            
        } else {
            
            //
            // there is a backslash character, so fall-through to SLOW
            //
            
            fast = false;
            terminated = true;
        }
        
    } else {
        
        //
        // unterminated, so fall-through to SLOW
        //
        
        fast = false;
        terminated = false;
    }
    
#else
    
    fast = false;
    
#endif // !COMPUTE_OOB && !CHECK_ISSUES && !COMPUTE_SOURCE && FAST_STRING_SCAN
    
    if (fast) {
        
#if DIAGNOSTICS
        Tokenizer_StringFastCount++;
#endif // DIAGNOSTICS
        
        //
        // just set buffer to quotPtr + 1
        //
        
        if (terminated) {
            
            session->buffer = quotPtr + 1;
            
            return Token(TOKEN_STRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            
        } else {
            
            session->buffer = session->end;
            
            return Token(TOKEN_ERROR_UNTERMINATEDSTRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // SLOW FALL-THROUGH
    //
    
#if DIAGNOSTICS
    Tokenizer_StringSlowCount++;
#endif // DIAGNOSTICS
    
    policy |= STRING_OR_COMMENT;
    
    while (true) {
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        switch (c.to_point()) {
            case '"': {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_STRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            case CODEPOINT_ENDOFFILE: {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
#if COMPUTE_OOB
            case '\n': case '\r': case CODEPOINT_CRLF: {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                session->addEmbeddedNewline(tokenStartLoc);
                
                break;
            }
            case '\t': {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                session->addEmbeddedTab(tokenStartLoc);
                
                break;
            }
#endif // COMPUTE_OOB
            default: {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                break;
            }
        }
        
    } // while
}


inline Token Tokenizer_handleString_stringifyAsTag(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    //
    // Nothing to assert
    //
    
    //
    // magically turn into a string
    //
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        auto letterlikeBuf = session->buffer;
        auto letterlikeLoc = session->SrcLoc;
        
        Tokenizer_handleSymbolSegment(session, tokenStartBuf, tokenStartLoc, letterlikeBuf, letterlikeLoc, c, policy);
        
        return Token(TOKEN_STRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
        
    //
    // Something like  a::5
    //
    
    return Token(TOKEN_ERROR_EXPECTEDTAG, tokenStartBuf, 0, Source(tokenStartLoc));
}


const int UNTERMINATED_FILESTRING = -1;

//
// Use SourceCharacters here, not WLCharacters
//
inline Token Tokenizer_handleString_stringifyAsFile(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy) {
    
    //
    // Nothing to assert
    //
        
    //
    // magically turn into a string
    //
    
    //
    // sync-up with current character
    //
    
    switch (c.to_point()) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
        case '$': case '`': case '/': case '.': case '\\': case '!': case '-': case '_': case ':': case '*': case '~': case '?': {
            
            c = ByteDecoder_currentSourceCharacter(session, policy);
            
            break;
        }
        case '[': {

            // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines

            int handled;

            c = Tokenizer_handleFileOpsBrackets(session, tokenStartBuf, tokenStartLoc, c, policy, &handled);

            switch (handled) {
                case UNTERMINATED_FILESTRING: {
                    return Token(TOKEN_ERROR_UNTERMINATEDFILESTRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
            }

            break;
        }
        default: {
            
            //
            // Something like  <<EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            
            return Token(TOKEN_ERROR_EXPECTEDFILE, tokenStartBuf, 0, Source(tokenStartLoc));
        }
    }
    
    while (true) {
        
        //
        // tutorial/OperatorInputForms
        //
        // File Names
        //
        // Any file name can be given in quotes after <<, >>, and >>>.
        // File names can also be given without quotes if they contain only alphanumeric
        // characters and the characters `, /, ., \[Backslash], !, -, _, :, $, *, ~, and ?, together with
        // matched pairs of square brackets enclosing any characters other than spaces, tabs, and newlines.
        // Note that file names given without quotes can be followed only by spaces, tabs, or newlines, or
        // by the characters ), ], or }, as well as semicolons and commas.
        //
        
        switch (c.to_point()) {
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
            case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
            case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            case '$': case '`': case '/': case '.': case '\\': case '!': case '-': case '_': case ':': case '*': case '~': case '?': {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                break;
            }
            case '[': {
                
                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                int handled;
                
                c = Tokenizer_handleFileOpsBrackets(session, tokenStartBuf, tokenStartLoc, c, policy, &handled);
                
                switch (handled) {
                    case UNTERMINATED_FILESTRING: {
                        return Token(TOKEN_ERROR_UNTERMINATEDFILESTRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
                
                break;
            }
            default: {
                return Token(TOKEN_STRING, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
        }
        
    } // while
}

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
// Use SourceCharacters here, not WLCharacters
//
inline SourceCharacter Tokenizer_handleFileOpsBrackets(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy, int *handled) {
    
    assert(c.to_point() == '[');
    
    //
    // sync-up with current character
    //
    
    c = ByteDecoder_currentSourceCharacter(session, policy);
    
    auto depth = 1;
    
    while (true) {
        
        switch (c.to_point()) {
                //
                // Spaces and Newlines
                //
            case ' ': case '\t': case '\v': case '\f':
            case '\n': case '\r': case CODEPOINT_CRLF: {
                
                //
                // Cannot have spaces in the string here, so bail out
                //
                
                *handled = UNTERMINATED_FILESTRING;
                
                return c;
            }
            case CODEPOINT_ENDOFFILE: {
                
                *handled = UNTERMINATED_FILESTRING;
                
                return c;
            }
            case '[': {
                
                depth = depth + 1;
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                break;
            }
            case ']': {
                
                depth = depth - 1;
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                if (depth == 0) {
                    
                    *handled = 0;
                    
                    return c;
                }
                
                break;
            }
            default: {
                
                if (c.isMBWhitespace() || c.isMBNewline()) {
                    
                    *handled = UNTERMINATED_FILESTRING;
                    
                    return c;
                }
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                c = ByteDecoder_currentSourceCharacter(session, policy);
                
                break;
            }
        }
        
    } // while
}


const int BAILOUT = -1;

//
//digits                  integer
//digits.digits           approximate number
//base^^digits            integer in specified base
//base^^digits.digits     approximate number in specified base
//mantissa*^n             scientific notation (mantissa*10^n)
//base^^mantissa*^n       scientific notation in specified base (mantissa*base^n)
//number`                 machine-precision approximate number
//number`s                arbitrary-precision number with precision s
//number``s               arbitrary-precision number with accuracy s
//
// base = (digits^^)?
// approximate = digits(.digits?)?|.digits
// precision = `(-?approximate)?
// accuracy = ``-?approximate
// mantissa = approximate+(precision|accuracy)?
// exponent = (*^-?digits)?
//
// numer = base+mantissa+exponent
//
inline Token Tokenizer_handleNumber(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isDigit() || c.to_point() == '.');
    
#if DIAGNOSTICS
    Tokenizer_NumberCount++;
#endif // DIAGNOSTICS
    
    NumberTokenizationContext Ctxt;
    
    auto leadingDigitsCount = 0;
    
    //
    // leadingDigitsEnd will point to the first character after all leading digits and ^^
    //
    // 16^^0.F
    //      ^leadingDigitsEnd
    //
    // 16^^.F
    //     ^leadingDigitsEnd
    //
    // 0.123
    //  ^leadingDigitsEnd
    //
    auto leadingDigitsEndBuf = tokenStartBuf;
    auto leadingDigitsEndLoc = tokenStartLoc;
    
    Buffer caret1Buf;
    SourceLocation caret1Loc;
    
    Buffer starBuf;
    SourceLocation starLoc;
    
    if (c.isDigit()) {
        
//        leadingDigitsCount++;
        
        //
        // Count leading zeros
        //
        
        //
        // 002^^111
        //   ^nonZeroStartBuf
        //
        auto nonZeroStartBuf = tokenStartBuf;
        auto nonZeroStartLoc = tokenStartLoc;
        
        if (c.to_point() == '0') {
            
            int leadingZeroCount;
            c = Tokenizer_handleZeros(session, tokenStartBuf, tokenStartLoc, policy, c, &leadingZeroCount);
            
            leadingDigitsCount += leadingZeroCount;
            
            nonZeroStartBuf = session->buffer;
            nonZeroStartLoc = session->SrcLoc;
        }
        
        
        //
        // Count the rest of the leading digits
        //
        
        leadingDigitsEndBuf = session->buffer;
        leadingDigitsEndLoc = session->SrcLoc;
        
        if (c.isDigit()) {
            
            int count;
            c = Tokenizer_handleDigits(session, tokenStartBuf, tokenStartLoc, policy, c, &count);
            
            leadingDigitsCount += count;
            
            leadingDigitsEndBuf = session->buffer;
            leadingDigitsEndLoc = session->SrcLoc;
        }
        
        if ((policy & INTEGER_SHORT_CIRCUIT) == INTEGER_SHORT_CIRCUIT) {
            
#if CHECK_ISSUES
            if (c.to_point() == '.') {
                
                //
                // Something like  #2.a
                //
                
                auto dotLoc = session->SrcLoc;
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new InsertTextCodeAction("Insert space", Source(dotLoc), " "));
                
                auto I = new FormatIssue(STRING_AMBIGUOUS, "Ambiguous syntax.", STRING_FORMATTING, Source(dotLoc, session->SrcLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES
            
            //
            // Success!
            //
            
            return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        
        switch (c.to_point()) {
                //
                // These are the possible next characters for a number
                //
            case '^': case '*': case '.': case '`': {
                
                if (c.to_point() == '^') {
                    
                    caret1Buf = session->buffer;
                    caret1Loc = session->SrcLoc;
                    
                    assert(Utils::ifASCIIWLCharacter(*caret1Buf, '^'));
                    
                } else if (c.to_point() == '*') {
                    
                    starBuf = session->buffer;
                    starLoc = session->SrcLoc;
                    
                    assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                }
                
                //
                // Preserve c, but advance buffer to next character
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                break;
            }
            default: {
                //
                // Something else
                //
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
        }
        
        if (c.to_point() == '^') {
            
            //
            // Could be 16^^blah
            //
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() != '^') {
                
                //
                // Something like  2^a
                //
                // Must now do surgery and back up
                //
                
                session->buffer = caret1Buf;
                session->SrcLoc = caret1Loc;
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            assert(c.to_point() == '^');
            
            //
            // Something like  2^^
            //
            // Must be a number
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (nonZeroStartBuf == caret1Buf) {
                
                //
                // Something like  0^^2
                //
                
                Ctxt.InvalidBase = true;
                
            } else {
                
                auto baseStrLen = caret1Buf - nonZeroStartBuf;
                
                //
                // bases can only be between 2 and 36, so we know they can only be 1 or 2 characters
                //
                if (baseStrLen > 2) {
                    
                    Ctxt.InvalidBase = true;
                    
                } else if (baseStrLen == 2) {
                    
                    auto d1 = Utils::toDigit(nonZeroStartBuf[0]);
                    auto d0 = Utils::toDigit(nonZeroStartBuf[1]);
                    Ctxt.Base = d1 * 10 + d0;
                    
                } else {
                    
                    assert(baseStrLen == 1);
                    
                    auto d0 = Utils::toDigit(nonZeroStartBuf[0]);
                    Ctxt.Base = d0;
                }
                
                if (!(2 <= Ctxt.Base && Ctxt.Base <= 36)) {
                    
                    Ctxt.InvalidBase = true;
                }
            }
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            //
            // What can come after ^^ ?
            //
            
            leadingDigitsCount = 0;
            
            switch (c.to_point()) {
                case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
                case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
                case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                    
                    //
                    // Something like  16^^A
                    //
                    
                    c = Tokenizer_handleAlphaOrDigits(session, tokenStartBuf, tokenStartLoc, c, Ctxt.Base, policy, &leadingDigitsCount, &Ctxt);
                    
                    switch (leadingDigitsCount) {
                        case BAILOUT: {
                            
                            assert(false);
                            
                            break;
                        }
                    }
                    
                    leadingDigitsEndBuf = session->buffer;
                    leadingDigitsEndLoc = session->SrcLoc;
                    
                    switch (c.to_point()) {
                            //
                            // These are the possible next characters for a number
                            //
                        case '*': case '.': case '`': {
                            
                            if (c.to_point() == '*') {
                                
                                starBuf = session->buffer;
                                starLoc = session->SrcLoc;
                                
                                assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                            }
                            
                            //
                            // Preserve c, but advance buffer to next character
                            //

                            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                            
                            break;
                        }
                        default: {
                            //
                            // Something else
                            //
                            
                            //
                            // Success!
                            //
                            
                            return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        }
                    }
                    
                    break;
                }
                case '.': {
                    
                    //
                    // Something like  2^^.0
                    //
                    
                    leadingDigitsEndBuf = session->buffer;
                    leadingDigitsEndLoc = session->SrcLoc;
                    
                    //
                    // Preserve c, but advance buffer to next character
                    //

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    break;
                }
                default: {
                    
                    //
                    // Something like  2^^@
                    //
                    
                    // nee TOKEN_ERROR_UNRECOGNIZEDDIGIT
                    return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
            }
            
        } // if (c.to_point() == '^')
        
    } // if (c.isDigit())
    
    if (c.to_point() == '.') {
        
        assert(Utils::ifASCIIWLCharacter(*(session->buffer - 1), '.'));
        
        int handled;
        c = Tokenizer_handlePossibleFractionalPart(session, tokenStartBuf, tokenStartLoc, leadingDigitsEndBuf, leadingDigitsEndLoc, c, Ctxt.Base, policy, &handled, &Ctxt);
        switch (handled) {
            case BAILOUT: {
                
                if (leadingDigitsCount == 0) {
                    
                    //
                    // Something like  2^^..
                    //
                    
                    // nee TOKEN_ERROR_UNHANDLEDDOT
                    return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
                
                //
                // Something like  0..
                //
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            case 0: {
                
                if (leadingDigitsCount == 0) {

                    //
                    // Something like  2^^.
                    //
                    
                    // nee TOKEN_ERROR_UNHANDLEDDOT
                    return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
                
                //
                // Something like  0.
                //
                
                Ctxt.Real = true;
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '`': case '*': {
                        
                        if (c.to_point() == '*') {
                            
                            starBuf = session->buffer;
                            starLoc = session->SrcLoc;
                            
                            assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                        }
                        
                        //
                        // Preserve c, but advance buffer to next character
                        //
                        
                        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                        
                        break;
                    }
                    default: {
                        
                        //
                        // Something like  123.
                        //
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
                
                break;
            }
            default: {
                
                //
                // Something like  123.456
                //
                
                Ctxt.Real = true;
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '`': case '*': {
                        
                        if (c.to_point() == '*') {
                            
                            starBuf = session->buffer;
                            starLoc = session->SrcLoc;
                            
                            assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                        }
                        
                        //
                        // Preserve c, but advance buffer to next character
                        //
                        
                        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                        
                        break;
                    }
                    default: {
                        
                        //
                        // Something like  123.456
                        //
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
                
                break;
            }
        }
    }
    
    assert(c.to_point() == '`' || c.to_point() == '*');
    
    //
    // Handle all ` logic here
    //
    // foo`
    // foo`bar
    // foo``bar
    //
    if (c.to_point() == '`') {
        
        Ctxt.Real = true;
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        bool accuracy = false;
        bool sign = false;
        bool precOrAccSupplied = false;
        
        Buffer signBuf;
        SourceLocation signLoc;
        
        if (c.to_point() == '`') {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            accuracy = true;
        }
        
        switch (c.to_point()) {
            case '-': case '+': {
                
                //
                // Something like  1.2`-
                //
                
                signBuf = session->buffer;
                signLoc = session->SrcLoc;
                
                assert(Utils::ifASCIIWLCharacter(*signBuf, '-')  || Utils::ifASCIIWLCharacter(*signBuf, '+'));
                
                //
                // Eat the sign
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                        
                        //
                        // Something like  1.2`-3
                        //
                        
                        sign = true;

#if CHECK_ISSUES
                        {
                            if (accuracy) {

                                //
                                // do not warn about 1.2``+3 for now
                                //

                            } else {

                                auto I = new SyntaxIssue(STRING_UNEXPECTEDSIGN, "The real number has a ``" + std::string(reinterpret_cast<const char *>(signBuf), 1) + "`` sign in its precision specification.", STRING_WARNING, Source(signLoc), 0.95, {}, {"This is usually unintentional."});

                                session->addIssue(I);
                            }
                        }
#endif // CHECK_ISSUES

                        break;
                    }
                    case '.': {
                        
                        //
                        // Something like  1.2`-.3
                        //
                        
                        sign = true;
#if CHECK_ISSUES
                        {
                            if (accuracy) {

                                //
                                // do not warn about 1.2``+.3 for now
                                //

                            } else {

                                auto I = new SyntaxIssue(STRING_UNEXPECTEDSIGN, "The real number has a ``" + std::string(reinterpret_cast<const char *>(signBuf), 1) + "`` sign in its precision specification.", STRING_WARNING, Source(signLoc), 0.95, {}, {"This is usually unintentional."});

                                session->addIssue(I);
                            }
                        }
#endif // CHECK_ISSUES
                        break;
                    }
                    default: {
                        
                        //
                        // Something like  1.2`->
                        //
                        
                        if (accuracy) {
                            
                            //
                            // Something like  1.2``->3
                            //
                            
                            // nee TOKEN_ERROR_EXPECTEDACCURACY
                            return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        }
                        
                        //
                        // Something like  1.2`->3  or  1`+#
                        //
                        // Must now do surgery and back up
                        //
                        Tokenizer_backupAndWarn(session, signBuf, signLoc);
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
                
                break;
                
            } // case '-': case '+'
        }
        
        switch(c.to_point()) {
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                
                int count;
                
                c = Tokenizer_handleDigits(session, tokenStartBuf, tokenStartLoc, policy, c, &count);
                
                if (count > 0) {
                    precOrAccSupplied = true;
                }
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '.': {
                        break;
                    }
                    case '*': {
                        break;
                    }
                    default: {
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
            }
        }
        
        switch (c.to_point()) {
            case '.': {
                
                auto dotBuf = session->buffer;
                auto dotLoc = session->SrcLoc;
                
                assert(Utils::ifASCIIWLCharacter(*dotBuf, '.'));
                
                bool tentativeActualDecimalPoint = false;
                
                //
                // If there was already a sign, or if the leading digits have already been supplied,
                // then this is an actual decimal point
                //
                if (sign || precOrAccSupplied) {
                    tentativeActualDecimalPoint = true;
                }
                
                //
                // Need to decide if the  .  here is actual radix point, or something like
                // the . in  123`.xxx  (which is Dot)
                //
                
                if (!tentativeActualDecimalPoint) {
                    
                    //
                    // Need to peek ahead
                    //
                    // Something like  123`.xxx
                    //
                    
                    // look ahead
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    auto NextChar = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    if (!NextChar.isDigit()) {
                        
                        if (accuracy) {
                            
                            //
                            // Something like  123``.EOF
                            //
                            
                            // TOKEN_ERROR_EXPECTEDDIGIT
                            return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        }
                        
                        if (NextChar.isSign()) {
                            
                            //
                            // Something like  123`.+4
                            //
                            
                            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                            
                            // nee TOKEN_ERROR_EXPECTEDDIGIT
                            return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        }
                        
                        //
                        // Something like  123`.xxx  where the . could be a Dot operator
                        //
                        // Number stops at `
                        //
                        // NOT actual decimal point
                        //
                        // Must now do surgery and back up
                        //
                        
                        Tokenizer_backupAndWarn(session, dotBuf, dotLoc);
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        
                    } else {
                        
                        //
                        // digit
                        //
                        
                        c = NextChar;
                    }
                    
                } else {
                    
                    //
                    // actual decimal point
                    //
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                }
                
                //
                // actual decimal point
                //
                
                int handled;
                //
                // The base to use inside of precision/accuracy processing is 0, i.e., implied 10
                //
                size_t baseToUse = 0;
                
                c = Tokenizer_handlePossibleFractionalPartPastDot(session, tokenStartBuf, tokenStartLoc, dotBuf, dotLoc, c, baseToUse, policy, &handled, &Ctxt);
                
                switch (handled) {
                    case BAILOUT: {
                        
                        if (precOrAccSupplied) {
                            
                            //
                            // Something like  6`5..
                            //
                            
                            //
                            // Success!
                            //
                            
                            return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        }
                        
                        if (sign) {
                            
                            //
                            // Something like  1`+..
                            //
                            
                            Tokenizer_backupAndWarn(session, signBuf, signLoc);
                            
                            //
                            // Success!
                            //
                            
                            return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                        }
                        
                        assert(false);
                        
                        break;
                    }
                    case 0: {
                        break;
                    }
                    default: {
                        
                        precOrAccSupplied = true;
                        
                        break;
                    }
                }
                
                if (!precOrAccSupplied) {
                    
                    //
                    // Something like  1`+.a
                    //
                    
                    // nee TOKEN_ERROR_EXPECTEDDIGIT
                    return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
                
                break;
            } // case '.'
                
        } // switch (c.to_point())
        
        switch (c.to_point()) {
                
                //
                // These are the possible next characters for a number
                //
            case '*': {
                
                if (accuracy) {
                    
                    if (!precOrAccSupplied) {
                     
                        //
                        // Something like  123.45``*^6
                        //
                        
                        // nee TOKEN_ERROR_EXPECTEDACCURACY
                        return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
                
                starBuf = session->buffer;
                starLoc = session->SrcLoc;
                
                assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                
                //
                // Preserve c, but advance buffer to next character
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                break;
            }
            default: {
                
                if (accuracy) {
                    
                    if (!precOrAccSupplied) {
                     
                        //
                        // Something like  123``EOF
                        //
                        
                        // nee TOKEN_ERROR_EXPECTEDACCURACY
                        return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                    }
                }
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
        }
    } // if (c.to_point() == '`')
    
    assert(c.to_point() == '*');
    
    assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() != '^') {
        
        //
        // Something like  1*a
        //
        // Must now do surgery and back up
        //
        
        session->buffer = starBuf;
        session->SrcLoc = starLoc;
        
        //
        // Success!
        //
        
        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    assert(c.to_point() == '^');
    
    //
    // c is '^'
    //
    // So now examine *^ notation
    //
    
    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '-': {
            Ctxt.NegativeExponent = true;
        }
            //
            // FALL THROUGH
            //
        case '+': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            break;
        }
    }
    
    if (!c.isDigit()) {
        
        //
        // Something like  123*^-EOF
        //
        
        // TOKEN_ERROR_EXPECTEDEXPONENT
        return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    assert(c.isDigit());
    
    //
    // Count leading zeros in exponent
    //
    if (c.to_point() == '0') {
        
        int exponentLeadingZeroCount;
        
        c = Tokenizer_handleZeros(session, tokenStartBuf, tokenStartLoc, policy, c, &exponentLeadingZeroCount);
    }
    
    if (c.isDigit()) {
        c = Tokenizer_handleDigits(session, tokenStartBuf, tokenStartLoc, policy, c, &Ctxt.NonZeroExponentDigitCount);
    }
    
    if (c.to_point() != '.') {
        
        //
        // Success!
        //
        
        return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    assert(c.to_point() == '.');
    
    auto dotBuf = session->buffer;
    auto dotLoc = session->SrcLoc;
    
    assert(Utils::ifASCIIWLCharacter(*dotBuf, '.'));
    
    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    int handled;
    c = Tokenizer_handlePossibleFractionalPartPastDot(session, tokenStartBuf, tokenStartLoc, dotBuf, dotLoc, c, Ctxt.Base, policy, &handled, &Ctxt);
    
    switch (handled) {
        case BAILOUT: {
            
            //
            // Something like  123*^2..
            //
            // The first . is not actually a radix point
            //
            
            //
            // Success!
            //
            
            return Token(Ctxt.computeTok(), tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // Something like  123*^0.5
    //
    // Make this an error; do NOT make this Dot[123*^0, 5]
    //
    
    // nee TOKEN_ERROR_EXPECTEDEXPONENT
    return Token(TOKEN_ERROR_NUMBER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}


TokenEnum NumberTokenizationContext::computeTok() {
    
    //
    // We wait until returning to handle these errors because we do not want invalid base or unrecognized digit to prevent further parsing
    //
    // e.g., we want  0^^1.2``3  to parse completely before returning the error
    //
    
    if (InvalidBase) {
        
        // nee TOKEN_ERROR_INVALIDBASE
        return TOKEN_ERROR_NUMBER;
    }
    
    if (UnrecognizedDigit) {
        
        // nee TOKEN_ERROR_UNRECOGNIZEDDIGIT
        return TOKEN_ERROR_NUMBER;
    }
    
    if (Real) {
        return TOKEN_REAL;
    }
    
    if (NegativeExponent && NonZeroExponentDigitCount != 0) {
        
        //
        // Something like  1*^-2..
        //
        
        return TOKEN_RATIONAL;
    }
        
    return TOKEN_INTEGER;
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
inline WLCharacter Tokenizer_handlePossibleFractionalPart(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter c, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt) {
    
    assert(c.to_point() == '.');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    MUSTTAIL
    return Tokenizer_handlePossibleFractionalPartPastDot(session, tokenStartBuf, tokenStartLoc, dotBuf, dotLoc, c, base, policy, handled, Ctxt);
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0
//         UNRECOGNIZED_DIGIT if base error
//         BAILOUT if not a radix point (and also backup before dot)
//
inline WLCharacter Tokenizer_handlePossibleFractionalPartPastDot(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter c, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt) {
    
    //
    // Nothing to assert
    //

    if (c.to_point() == '.') {
        
        //
        // Something like  0..
        //
        // The first . is not actually a radix point
        //
        // Must now do surgery and back up
        //
        
        Tokenizer_backupAndWarn(session, dotBuf, dotLoc);
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        *handled = BAILOUT;

        return c;
    }
    
    if (c.isAlphaOrDigit()) {
        
        c = Tokenizer_handleAlphaOrDigits(session, tokenStartBuf, tokenStartLoc, c, base, policy, handled, Ctxt);
        
        switch (*handled) {
            case BAILOUT: {
                
                assert(false);
                
                break;
            }
            case 0: {
                return c;
            }
            default: {
                
#if CHECK_ISSUES
                if (c.to_point() == '.') {
                    
                    //
                    // Something like  1.2.3
                    //
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new InsertTextCodeAction("Insert ``*``", Source(dotLoc), "*"));
                    
                    auto I = new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Suspicious syntax.", STRING_ERROR, Source(dotLoc), 0.99, Actions, {});
                    
                    session->addIssue(I);
                }
#endif // CHECK_ISSUES
                
                return c;
            }
        }
    }

    *handled = 0;
    
    return c;
}
        
void Tokenizer_backupAndWarn(ParserSessionPtr session, Buffer resetBuf, SourceLocation resetLoc) {
    
#if CHECK_ISSUES
    {
        CodeActionPtrVector Actions;
        
        Actions.push_back(new InsertTextCodeAction("Insert space", Source(resetLoc), " "));
        
        auto I = new FormatIssue(STRING_AMBIGUOUS, "Ambiguous syntax.", STRING_FORMATTING, Source(resetLoc), 1.0, Actions, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    session->buffer = resetBuf;
    session->SrcLoc = resetLoc;
}

//
// Precondition: currentWLCharacter is 0
// Postcondition: buffer is pointing to first NON-ZERO character after all zeros
//
// return: the first NON-ZERO character after all digits
//
inline WLCharacter Tokenizer_handleZeros(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter c, int *countP) {
    
    assert(c.to_point() == '0');
    
    auto count = 1;
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        if (c.to_point() != '0') {
            break;
        }
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        count++;
        
    } // while
    
    *countP = count;
    
    return c;
}

//
// Precondition: currentWLCharacter is a digit
// Postcondition: buffer is pointing to first NON-DIGIT character after all digits
//
// return: the first NON-DIGIT character after all digits
//
inline WLCharacter Tokenizer_handleDigits(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter c, int *countP) {
    
    assert(c.isDigit());
    
    auto count = 1;
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        if (!c.isDigit()) {
            break;
        }
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        count++;
        
    } // while
    
    *countP = count;

    return c;
}

//
// Precondition: currentWLCharacter is NOT in String
// Postcondition: currentWLCharacter is the first WLCharacter AFTER all good digits or alphas
//
// Return: number of digits handled, possibly 0, or -1 if error
//
inline WLCharacter Tokenizer_handleAlphaOrDigits(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt) {
    
    assert(c.isAlphaOrDigit());
    
    auto count = 0;
    
    while (true) {
        
        if (!c.isAlphaOrDigit()) {
            break;
        }
        
        if (base == 0) {
            
            if (!c.isDigit()) {
                break;
            }
            
        } else {
            
            auto dig = Utils::toDigit(c.to_point());
            
            if (base <= dig) {
                Ctxt->UnrecognizedDigit = true;
            }
            
        }
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        count++;
        
    } // while
    
    *handled = count;
    
    return c;
}

inline Token Tokenizer_handleColon(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == ':');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case ':': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '[') {
                
                //
                // ::[
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_COLONCOLONOPENSQUARE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            //
            // ::
            //
            
            return Token(TOKEN_COLONCOLON, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // :=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_COLONEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '>': {
            
            //
            // :>
            //
            
#if DIAGNOSTICS
            Tokenizer_ColonGreaterCount++;
#endif // DIAGNOSTICS
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_COLONGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // :
    //
    
    return Token(TOKEN_COLON, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleOpenParen(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '(');
    
    auto secondChar = ByteDecoder_currentSourceCharacter(session, policy);
    
    //
    // Comments must start literally with (*
    // Escaped characters do not work
    //
    if ((c.to_point() == '(' && c.escape() == ESCAPE_NONE) &&
        (secondChar.to_point() == '*')) {
        
        //
        // secondChar is a SourceCharacter, so cannot MUSTTAIL
        //
//        MUSTTAIL
        return Tokenizer_handleComment(session, tokenStartBuf, tokenStartLoc, secondChar, policy);
    }
    
    //
    // (
    //
    
#if DIAGNOSTICS
    Tokenizer_OpenParenCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_OPENPAREN, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleDot(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {
    
    auto c = firstChar;
    
    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //
    
    assert(c.to_point() == '.');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.isDigit()) {
        
//        MUSTTAIL
        return Tokenizer_handleNumber(session, tokenStartBuf, tokenStartLoc, firstChar, policy);
    }
    
    if (c.to_point() == '.') {
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        if (c.to_point() == '.') {
            
            //
            // ...
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_DOTDOTDOT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        
        //
        // ..
        //
        
        return Token(TOKEN_DOTDOT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // .
    //
    
    return Token(TOKEN_DOT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleEqual(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '=');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '=': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '=') {
                
                //
                // ===
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_EQUALEQUALEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            //
            // ==
            //
            
            return Token(TOKEN_EQUALEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '!': {
            
            auto bangBuf = session->buffer;
            auto bangLoc = session->SrcLoc;
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '=') {
                
                //
                // =!=
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_EQUALBANGEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
                
            //
            // Something like  x=!y
            //
            // Must now do surgery and back up
            //
            
            Tokenizer_backupAndWarn(session, bangBuf, bangLoc);
            
            return Token(TOKEN_EQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // =
    //
    
    return Token(TOKEN_EQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleUnder(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '_');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '_': {
            
            //
            // __
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '_') {
                
                //
                // ___
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_UNDERUNDERUNDER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            return Token(TOKEN_UNDERUNDER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '.': {
            
            //
            // _.
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
#if CHECK_ISSUES
            {
                auto afterLoc = session->SrcLoc;
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '.') {
                    
                    //
                    // Something like  a_..b  or  _...
                    //
                    // Prior to 12.2,  a_..b  was parsed as Times[(a_).., b]
                    //
                    // 12.2 and onward,  a_..b  is parsed as Dot[a_., b]
                    //
                    // Related bugs: 390755
                    //
                    
                    auto dotLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new InsertTextCodeAction("Insert space", Source(dotLoc), " "));
                    
                    auto I = new SyntaxIssue(STRING_UNEXPECTEDDOT, "Suspicious syntax.", STRING_ERROR, Source(dotLoc), 0.95, Actions, {});
                    
                    session->addIssue(I);
                }
            }
#endif // CHECK_ISSUES
            
            return Token(TOKEN_UNDERDOT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // _
    //
    
    return Token(TOKEN_UNDER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleLess(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '<');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '|': {
            
            //
            // <|
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_LESSBAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '<': {
            
            //
            // <<
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_LESSLESS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '>': {
            
            //
            // <>
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_LESSGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // <=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_LESSEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '-': {
            
            auto minusBuf = session->buffer;
            auto minusLoc = session->SrcLoc;
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '>') {
                
                //
                // <->
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_LESSMINUSGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
                
            //
            // Something like  a<-4
            //
            // Must now do surgery and back up
            //
            
            Tokenizer_backupAndWarn(session, minusBuf, minusLoc);
            
            return Token(TOKEN_LESS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // <
    //
    
    return Token(TOKEN_LESS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleGreater(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '>');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '>': {
            
            //
            // >>
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '>') {
                
                //
                // >>>
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_GREATERGREATERGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            return Token(TOKEN_GREATERGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // >=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_GREATEREQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // >
    //
    
    return Token(TOKEN_GREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleMinus(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '-');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    //
    // Do not lex as a number here
    // Makes it easier to handle implicit times later
    //
    // Because if we lexed - as a number here, then it is
    // harder to know that b-1 is Plus[b, -1] instead of
    // b<invisiblespace>-1 which is Times[b, -1]
    //
    
    switch (c.to_point()) {
        case '>': {
            
            //
            // ->
            //
            
#if DIAGNOSTICS
            Tokenizer_MinusGreaterCount++;
#endif // DIAGNOSTICS
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_MINUSGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '-': {
            
            //
            // --
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
#if CHECK_ISSUES
            {
                auto afterLoc = session->SrcLoc;
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '>') {
                    
                    //
                    // Something like  a-->0
                    //
                    // Was originally just a FormatIssue
                    //
                    // But a real-world example was demonstrated and this is now considered a real thing that could happen
                    //
                    // https://stash.wolfram.com/projects/WA/repos/alphasource/pull-requests/30963/overview
                    //
                    
                    auto greaterLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    
                    //
                    // HACK: little bit of a hack here
                    // would like to replace  -->  with  ->
                    // but the current token is only -- and I would prefer to not read past the > just for this action
                    //
                    // So actually just replace the -- with -
                    //
                    Actions.push_back(new ReplaceTextCodeAction("Replace with ``->``", Source(tokenStartLoc, afterLoc), "-"));
                    
                    Actions.push_back(new InsertTextCodeAction("Insert space", Source(greaterLoc), " "));
                    
                    auto I = new SyntaxIssue(STRING_AMBIGUOUS, "``-->`` is ambiguous syntax.", STRING_ERROR, Source(tokenStartLoc, afterLoc), 0.95, Actions, {});
                    
                    session->addIssue(I);
                    
                } else if (c.to_point() == '=') {
                    
                    //
                    // Something like  a--=0
                    //
                    
                    auto equalLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new InsertTextCodeAction("Insert space", Source(equalLoc), " "));
                    
                    auto I = new FormatIssue(STRING_AMBIGUOUS, "Put a space between ``--`` and ``=`` to reduce ambiguity", STRING_FORMATTING, Source(equalLoc), 1.0, Actions, {});
                    
                    session->addIssue(I);
                }
            }
#endif // CHECK_ISSUES
            
            return Token(TOKEN_MINUSMINUS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // -=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_MINUSEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // -
    //
    
#if DIAGNOSTICS
    Tokenizer_MinusCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_MINUS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleBar(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '|');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '>': {
            
            //
            // |>
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
#if CHECK_ISSUES
            {
                auto afterLoc = session->SrcLoc;
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '=') {
                    
                    //
                    // Something like  <||>=0
                    //
                    
                    auto equalLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new InsertTextCodeAction("Insert space", Source(equalLoc), " "));
                    
                    auto I = new FormatIssue(STRING_AMBIGUOUS, "Put a space between ``|>`` and ``=`` to reduce ambiguity", STRING_FORMATTING, Source(equalLoc), 1.0, Actions, {});
                    
                    session->addIssue(I);
                }
            }
#endif // CHECK_ISSUES
            
            return Token(TOKEN_BARGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '|': {
            
            //
            // ||
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_BARBAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '-': {
            
            auto barBuf = session->buffer;
            auto barLoc = session->SrcLoc;
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '>') {
                
                //
                // |->
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_BARMINUSGREATER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
                
            //
            // Something like  x|-y
            //
            // Must now do surgery and back up
            //
            
            Tokenizer_backupAndWarn(session, barBuf, barLoc);
            
            return Token(TOKEN_BAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // |
    //
    
    return Token(TOKEN_BAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleSemi(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == ';');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() == ';') {
        
        //
        // ;;
        //
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        return Token(TOKEN_SEMISEMI, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // ;
    //
    
    return Token(TOKEN_SEMI, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleBang(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '!');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '=': {
            
            //
            // !=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_BANGEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '!': {
            
            //
            // !!
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_BANGBANG, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // !
    //
    
    return Token(TOKEN_BANG, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleHash(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '#');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() == '#') {
        
        //
        // ##
        //
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        return Token(TOKEN_HASHHASH, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // #
    //
    
#if DIAGNOSTICS
    Tokenizer_HashCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_HASH, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handlePercent(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '%');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() == '%') {
        
        //
        // %%
        //
        
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        while (true) {
            
            if (c.to_point() != '%') {
                break;
            }
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
        } // while
        
        return Token(TOKEN_PERCENTPERCENT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // %
    //
    
    return Token(TOKEN_PERCENT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleAmp(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '&');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() == '&') {
        
        //
        // &&
        //
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        return Token(TOKEN_AMPAMP, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // &
    //
    
#if DIAGNOSTICS
    Tokenizer_AmpCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_AMP, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleSlash(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '/');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '@': {
            
            //
            // /@
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_SLASHAT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case ';': {
            
            //
            // /;
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_SLASHSEMI, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '.': {
            
            auto dotBuf = session->buffer;
            auto dotLoc = session->SrcLoc;
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (!c.isDigit()) {
                
                //
                // /.
                //
                
                return Token(TOKEN_SLASHDOT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            //
            // Something like  t/.3
            //
            // Must now do surgery and back up
            //
            
            Tokenizer_backupAndWarn(session, dotBuf, dotLoc);
            
            return Token(TOKEN_SLASH, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '/': {
            
            //
            // //
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            switch (c.to_point()) {
                case '.': {
                    
                    //
                    // //.
                    //
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    return Token(TOKEN_SLASHSLASHDOT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
                case '@': {
                    
                    //
                    // //@
                    //
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    return Token(TOKEN_SLASHSLASHAT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
                case '=': {
                    
                    //
                    // //=
                    //
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    return Token(TOKEN_SLASHSLASHEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
                }
            }
            
            //
            // //
            //
            
            return Token(TOKEN_SLASHSLASH, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case ':': {
            
            //
            // /:
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_SLASHCOLON, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // /=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_SLASHEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '*': {
            
            //
            // /*
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_SLASHSTAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // /
    //
    
    return Token(TOKEN_SLASH, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleAt(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '@');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '@': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '@') {
                
                //
                // @@@
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_ATATAT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            //
            // @@
            //
            
            return Token(TOKEN_ATAT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '*': {
            
            //
            // @*
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_ATSTAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // @
    //
    
    return Token(TOKEN_AT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handlePlus(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '+');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '+': {
            
            //
            // ++
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
#if CHECK_ISSUES
            {
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '=') {
                    
                    //
                    // Something like  a++=0
                    //
                    
                    auto loc = session->SrcLoc;
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new InsertTextCodeAction("Insert space", Source(loc), " "));
                    
                    auto I = new FormatIssue(STRING_AMBIGUOUS, "Put a space between ``++`` and ``=`` to reduce ambiguity", STRING_FORMATTING, Source(loc), 1.0, Actions, {});
                    
                    session->addIssue(I);
                }
            }
#endif // CHECK_ISSUES
            
            return Token(TOKEN_PLUSPLUS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // +=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_PLUSEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // +
    //
    
#if DIAGNOSTICS
    Tokenizer_PlusCount++;
#endif // DIAGNOSTICS
    
    return Token(TOKEN_PLUS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleTilde(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '~');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() == '~') {
        
        //
        // ~~
        //
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        return Token(TOKEN_TILDETILDE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // ~
    //
    
    return Token(TOKEN_TILDE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleQuestion(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '?');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() == '?') {
        
        //
        // ??
        //
        
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        
        return Token(TOKEN_QUESTIONQUESTION, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
    }
    
    //
    // ?
    //
    
    return Token(TOKEN_QUESTION, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleStar(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '*');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '=': {
            
            //
            // *=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_STAREQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '*': {
            
            //
            // **
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_STARSTAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case ')': {
            
            //
            // *)
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_ERROR_UNEXPECTEDCOMMENTCLOSER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // *
    //
    
    return Token(TOKEN_STAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleCaret(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '^');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case ':': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '=') {
                
                //
                // ^:=
                //
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                return Token(TOKEN_CARETCOLONEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            //
            // Has to be ^:=
            //
            
            return Token(TOKEN_ERROR_EXPECTEDEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '=': {
            
            //
            // ^=
            //
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            return Token(TOKEN_CARETEQUAL, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    //
    // ^
    //
    
    return Token(TOKEN_CARET, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleUnhandledBackslash(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    //
    // Unhandled backslash
    //
    // Something like  \A  or  \{  or  \<EOF>
    //
    // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
    // This duplicates some logic in CharacterDecoder
    //
    
    assert(c.to_point() == '\\');
    
    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '[': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            //
            // Try to reconstruct \[XXX]
            //
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            auto wellFormed = false;
            
            if (c.isUpper()) {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                
                while (true) {
                    
                    if (c.isAlphaOrDigit()) {
                        
                        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                        
                        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                        
                        continue;
                    }
                    
                    if (c.to_point() == ']') {
                        
                        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                        
                        wellFormed = true;
                    }
                    
                    break;
                }
                
            } else if (c.to_point() == ']') {
                
                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            }
            
            if (wellFormed) {
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case ':': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            //
            // Try to reconstruct \:XXXX
            //
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 4; i++) {
                
                if (c.isHex()) {
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    continue;
                }
                
                break;
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '.': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            //
            // Try to reconstruct \.XX
            //
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 2; i++) {
                
                if (c.isHex()) {
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    continue;
                }
                
                break;
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            //
            // Try to reconstruct \XXX
            //
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 3; i++) {
                
                if (c.isOctal()) {
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    continue;
                }
                
                break;
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case '|': {
            
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            //
            // Try to reconstruct \|XXXXXX
            //
            
            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 6; i++) {
                
                if (c.isHex()) {
                    
                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    
                    continue;
                }
                
                break;
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_ENDOFFILE: {
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    } // switch
    
    //
    // Nothing special, just read next single character
    //
    
    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    
    return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleMBStrangeNewline(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBStrangeNewline());
    
#if CHECK_ISSUES
    {
        auto Src = Source(tokenStartLoc, session->SrcLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDNEWLINECHARACTER, "Unexpected newline character: ``" + c.graphicalString() + "``.", STRING_WARNING, Src, 0.85, Actions, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return Token((policy & RETURN_TOPLEVELNEWLINE) ? TOKEN_TOPLEVELNEWLINE : TOKEN_INTERNALNEWLINE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleMBStrangeWhitespace(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBStrangeWhitespace());
    
#if CHECK_ISSUES
    {
        auto Src = Source(tokenStartLoc, session->SrcLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDSPACECHARACTER, "Unexpected space character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.85, Actions, {});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
    
    return Token(TOKEN_WHITESPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleMBPunctuation(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBPunctuation());
    
    auto Operator = LongNameCodePointToOperator(c.to_point());
    
    return Token(Operator, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
}

inline Token Tokenizer_handleNakedMBLinearSyntax(ParserSessionPtr session, Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBLinearSyntax());
    
    switch (c.to_point()) {
        case CODEPOINT_LINEARSYNTAX_CLOSEPAREN: {
            return Token(TOKEN_LINEARSYNTAX_CLOSEPAREN, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_AT: {
            return Token(TOKEN_LINEARSYNTAX_AT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_PERCENT: {
            return Token(TOKEN_LINEARSYNTAX_PERCENT, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_CARET: {
            return Token(TOKEN_LINEARSYNTAX_CARET, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_AMP: {
            return Token(TOKEN_LINEARSYNTAX_AMP, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_STAR: {
            return Token(TOKEN_LINEARSYNTAX_STAR, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_UNDER: {
            return Token(TOKEN_LINEARSYNTAX_UNDER, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_PLUS: {
            return Token(TOKEN_LINEARSYNTAX_PLUS, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_SLASH: {
            return Token(TOKEN_LINEARSYNTAX_SLASH, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_BACKTICK: {
            return Token(TOKEN_LINEARSYNTAX_BACKTICK, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
        case CODEPOINT_LINEARSYNTAX_SPACE: {
            return Token(TOKEN_LINEARSYNTAX_SPACE, tokenStartBuf, session->buffer - tokenStartBuf, Source(tokenStartLoc, session->SrcLoc));
        }
    }
    
    assert(false);
    
    return Token();
}
