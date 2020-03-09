
#include "Tokenizer.h"

#include "CharacterDecoder.h" // for TheCharacterDecoder
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for strangeLetterlikeWarning


Tokenizer::Tokenizer() : Issues() {}

void Tokenizer::init() {
    
    Issues.clear();
}

void Tokenizer::deinit() {
    
    Issues.clear();
}

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
Token Tokenizer::nextToken0(NextCharacterPolicy policy) {
    
    TokenizerContext Ctxt = 0;
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto c = TheCharacterDecoder->nextWLCharacter0(policy);
    
    switch (c.to_point()) {
        
        //
        // all single-byte characters
        //
        // most control characters are letterlike
        // jessef: There may be such a thing as *too* binary-safe...
        //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': /*    \x07*/
        case '\x08': /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        case '$':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '`':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            return handleSymbol(tokenStartBuf, tokenStartLoc, c, policy, Ctxt);
        case CODEPOINT_BEL: case CODEPOINT_DEL:
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '\t':
            return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '\n': case '\r':
            
            //
            // Return TOPLEVELNEWLINE or INTERNALNEWLINE, depending on policy
            //
            return Token(TOKEN_TOPLEVELNEWLINE.t() | (policy & RETURN_INTERNALNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '\v': case '\f':
            return handleStrangeWhitespace(tokenStartBuf, tokenStartLoc, c, policy);
        case ' ':
            return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '!':
            return handleBang(tokenStartBuf, tokenStartLoc, c, policy);
        case '"':
            return handleString(tokenStartBuf, tokenStartLoc, c, policy);
        case '#':
            return handleHash(tokenStartBuf, tokenStartLoc, c, policy);
        case '%':
            return handlePercent(tokenStartBuf, tokenStartLoc, c, policy);
        case '&':
            return handleAmp(tokenStartBuf, tokenStartLoc, c, policy);
        case '\'':
            return Token(TOKEN_SINGLEQUOTE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '(':
            return handleOpenParen(tokenStartBuf, tokenStartLoc, c, policy);
        case ')':
            return Token(TOKEN_CLOSEPAREN, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '*':
            return handleStar(tokenStartBuf, tokenStartLoc, c, policy);
        case '+':
            return handlePlus(tokenStartBuf, tokenStartLoc, c, policy);
        case ',':
            return Token(TOKEN_COMMA, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '-':
            return handleMinus(tokenStartBuf, tokenStartLoc, c, policy);
        case '.':
            return handleDot(tokenStartBuf, tokenStartLoc, c, policy);
        case '/':
            return handleSlash(tokenStartBuf, tokenStartLoc, c, policy);
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            return handleNumber(tokenStartBuf, tokenStartLoc, c, policy);
        case ':':
            return handleColon(tokenStartBuf, tokenStartLoc, c, policy);
        case ';':
            return handleSemi(tokenStartBuf, tokenStartLoc, c, policy);
        case '<':
            return handleLess(tokenStartBuf, tokenStartLoc, c, policy);
        case '=':
            return handleEqual(tokenStartBuf, tokenStartLoc, c, policy);
        case '>':
            return handleGreater(tokenStartBuf, tokenStartLoc, c, policy);
        case '?':
            return handleQuestion(tokenStartBuf, tokenStartLoc, c, policy);
        case '@':
            return handleAt(tokenStartBuf, tokenStartLoc, c, policy);
        case '[':
            return Token(TOKEN_OPENSQUARE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '\\':
            return handleUnhandledBackSlash(tokenStartBuf, tokenStartLoc, c, policy);
        case ']':
            return Token(TOKEN_CLOSESQUARE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '^':
            return handleCaret(tokenStartBuf, tokenStartLoc, c, policy);
        case '_':
            return handleUnder(tokenStartBuf, tokenStartLoc, c, policy);
        case '{':
            return Token(TOKEN_OPENCURLY, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '|':
            return handleBar(tokenStartBuf, tokenStartLoc, c, policy);
        case '}':
            return Token(TOKEN_CLOSECURLY, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '~':
            return handleTilde(tokenStartBuf, tokenStartLoc, c, policy);
        default: {
            
            //
            // Everything else involving multi-byte characters
            //
            
            if (c.to_point() == CODEPOINT_ENDOFFILE) {
                
                return Token(TOKEN_ENDOFFILE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBLinearSyntax()) {
                
                return handleMBLinearSyntax(tokenStartBuf, tokenStartLoc, c, policy);
                
            } else if (c.isMBLineContinuation()) {
                
                return Token(TOKEN_LINECONTINUATION, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBUninterpretable()) {
                
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBStrangeWhitespace()) {
                
                return handleMBStrangeWhitespace(tokenStartBuf, tokenStartLoc, c, policy);
                
            } else if (c.isMBWhitespace()) {
                
                return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBStrangeNewline()) {
                
                return handleMBStrangeNewline(tokenStartBuf, tokenStartLoc, c, policy);
                
            } else if (c.isMBNewline()) {
                
                //
                // Return TOPLEVELNEWLINE or INTERNALNEWLINE, depending on policy
                //
                return Token(TOKEN_TOPLEVELNEWLINE.t() | (policy & RETURN_INTERNALNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBPunctuation()) {
                
                return handleMBPunctuation(tokenStartBuf, tokenStartLoc, c, policy);
                
            } else if (c.isMBStringMeta()) {
                
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBUnsupported()) {
                
                return Token(TOKEN_ERROR_UNSUPPORTEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else {
                
                //
                // if nothing else, then it is letterlike
                //
                
                assert(c.isMBLetterlike());
                
                return handleSymbol(tokenStartBuf, tokenStartLoc, c, policy, Ctxt);
            }
        }
    }
}


#if STARTOFLINE

Token Tokenizer::nextToken0_stringifyLine() {

    auto tokenStartBuf = TheByteBuffer->getBuffer();
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_LINE;

    auto c = TheCharacterDecoder->nextWLCharacter0(policy);

    if (c.to_point() == CODEPOINT_ENDOFFILE) {

        //
        // EndOfFile is special, so invent source
        //

        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));

    } else if () {
        
        xx;
        
    } else if (c.isNewline()) {

        //
        // Newline is special, so invent source
        //

        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));
    }

    return handleString_stringifyLine(tokenStartBuf, c, policy);
}

#endif // STARTOFLINE


Token Tokenizer::nextToken0_stringifySymbol() {
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_SYMBOL;
    
    auto c = TheCharacterDecoder->nextWLCharacter0(policy);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        //
        // EndOfFile is special, so invent source
        //
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        
    } else if (c.to_point() == CODEPOINT_CRLF) {
        
        //
        // Newline is special, so invent source
        //
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        
    } else if (c.isNewline()) {
        
        //
        // Newline is special, so invent source
        //
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
    }
    
    return handleString_stringifySymbol(tokenStartBuf, tokenStartLoc, c, policy);
}

//
// Use SourceCharacters here, not WLCharacters
//
Token Tokenizer::nextToken0_stringifyFile() {
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_FILE;
    
    auto c = TheByteDecoder->nextSourceCharacter0(policy);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        
    } else if (c.to_point() == CODEPOINT_CRLF) {
        
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
        // Return TOPLEVELNEWLINE or INTERNALNEWLINE, depending on policy
        //
        return Token(TOKEN_TOPLEVELNEWLINE.t() | (policy & RETURN_INTERNALNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        
    } else if (c.isNewline()) {
        
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
        // Return TOPLEVELNEWLINE or INTERNALNEWLINE, depending on policy
        //
        return Token(TOKEN_TOPLEVELNEWLINE.t() | (policy & RETURN_INTERNALNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    //
    // There could be space, something like  << abc
    //
    // or something like:
    // a >>
    //   b
    //
    
    if (c.isWhitespace()) {
        
        return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    return handleString_stringifyFile(tokenStartBuf, tokenStartLoc, c, policy);
}


void Tokenizer::nextToken(Token Tok) {
    
    TheByteBuffer->buffer = Tok.BufLen.end;
    TheByteDecoder->SrcLoc = Tok.Src.End;
    
    if (Tok.Tok == TOKEN_ENDOFFILE) {
        TheByteBuffer->wasEOF = true;
    } else {
        TheByteBuffer->wasEOF = false;
    }
    
    TheByteDecoder->clearStatus();
}

#if STARTOFLINE

void Tokenizer::nextToken_stringifyLine() {

    nextToken0_stringifyLine();

    TheByteDecoder->clearStatus();
}

#endif // STARTOFLINE

void Tokenizer::nextToken_stringifySymbol() {
    
    nextToken0_stringifySymbol();
    
    TheByteDecoder->clearStatus();
}

void Tokenizer::nextToken_stringifyFile() {
    
    nextToken0_stringifyFile();
    
    TheByteDecoder->clearStatus();
}


//
//
//
Token Tokenizer::currentToken(NextCharacterPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0(policy);
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    TheByteDecoder->clearStatus();
    
    return Tok;
}

#if STARTOFLINE

Token Tokenizer::currentToken_stringifyLine() {

    auto resetBuf = TheByteBuffer->getBuffer();
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0_stringifyLine();

    TheByteBuffer->setBuffer(resetBuf);
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    TheByteDecoder->clearError();

    return Tok;
}

#endif // STARTOFLINE

Token Tokenizer::currentToken_stringifySymbol() {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0_stringifySymbol();
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    TheByteDecoder->clearStatus();
    
    return Tok;
}

Token Tokenizer::currentToken_stringifyFile() {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0_stringifyFile();
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    TheByteDecoder->clearStatus();
    
    return Tok;
}

inline Token Tokenizer::handleStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.isStrangeWhitespace());
    
#if !NISSUES
    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected space character.", SYNTAXISSUESEVERITY_WARNING, getTokenSource(tokenStartLoc), 0.95, {}));
    
    Issues.push_back(std::move(I));
#endif // !NISSUES
    
    return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

//
// Use SourceCharacters here, not WLCharacters
//
// Comments deal with (**) SourceCharacters
// Escaped characters do not work
//
inline Token Tokenizer::handleComment(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextCharacterPolicy policy) {
    
    //
    // comment is already started
    //
    
    assert(c.to_point() == '*');
    
    auto depth = 1;
    
    c = TheByteDecoder->currentSourceCharacter(policy);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        //
        // No need to check for comment length
        //
        
        switch (c.to_point()) {
            case '(':
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
                
                if (c.to_point() == '*') {
                    
                    depth = depth + 1;
                    
                    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                    
                    c = TheByteDecoder->currentSourceCharacter(policy);
                }
                
                break;
            case '*':
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
                
                if (c.to_point() == ')') {
                    
                    // This comment is closing
                    
                    depth = depth - 1;
                    
                    if (depth == 0) {
                        
                        TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                        TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                        
                        return Token(TOKEN_COMMENT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                    
                    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                    
                    c = TheByteDecoder->currentSourceCharacter(policy);
                }
                break;
            case CODEPOINT_ENDOFFILE:
                return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            default:
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
                break;
        }
        
    } // while
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
inline Token Tokenizer::handleSymbol(Buffer symbolStartBuf, SourceLocation symbolStartLoc, WLCharacter c, NextCharacterPolicy policy, TokenizerContext Ctxt) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '`' || c.isLetterlike() || c.isMBLetterlike());
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        c = handleSymbolSegment(symbolStartBuf, symbolStartLoc, c, policy, Ctxt);
    }
    
    //
    // if c == '`', then buffer is pointing past ` now
    //
    
    while (true) {
        
        if (c.to_point() != '`') {
            break;
        }
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
#if !NISSUES
        if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
            
            //
            // It's hard to keep track of the ` characters, so just report the entire symbol. Oh well
            //
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the **`** character.", SYNTAXISSUESEVERITY_REMARK, getTokenSource(symbolStartLoc), 0.33, {}));
            
            Issues.push_back(std::move(I));
        }
#endif // !NISSUES
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        if (c.isLetterlike() || c.isMBLetterlike()) {
            
            auto letterlikeBuf = TheByteBuffer->buffer;
            auto letterlikeLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = handleSymbolSegment(letterlikeBuf, letterlikeLoc, c, policy, Ctxt);
            
        } else {
            
            //
            // Something like  a`1
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, getTokenBufferAndLength(symbolStartBuf), getTokenSource(symbolStartLoc));
        }
        
    } // while
    
    return Token(TOKEN_SYMBOL, getTokenBufferAndLength(symbolStartBuf), getTokenSource(symbolStartLoc));
}

//
// Precondition: currentWLCharacter is letterlike
// Postcondition: buffer is pointing to first NON-SYMBOLSEGMENT character after all symbol segment characters
//
// return: the first NON-SYMBOLSEGMENT character after all symbol segment characters
//
inline WLCharacter Tokenizer::handleSymbolSegment(Buffer charBuf, SourceLocation charLoc, WLCharacter c, NextCharacterPolicy policy, TokenizerContext Ctxt) {
    
    assert(c.isLetterlike() || c.isMBLetterlike());
    
#if !NISSUES
    if (c.to_point() == '$') {
        
        if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``$`` character.", SYNTAXISSUESEVERITY_REMARK, getTokenSource(charLoc), 0.33, {}));
            
            Issues.push_back(std::move(I));
        }
    } else if (c.isStrangeLetterlike()) {
        Utils::strangeLetterlikeWarning(getTokenSource(charLoc), c);
    } else if (c.isMBStrangeLetterlike()) {
        Utils::mbStrangeLetterlikeWarning(getTokenSource(charLoc), c);
    }
#endif // !NISSUES
    
    charLoc = TheByteDecoder->SrcLoc;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isDigit()) {
            ;
        } else if (c.isLetterlike() || c.isMBLetterlike()) {
            
#if !NISSUES
            if (c.to_point() == '$') {
                
                if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``$`` character.", SYNTAXISSUESEVERITY_REMARK, getTokenSource(charLoc), 0.33, {}));
                    
                    Issues.push_back(std::move(I));
                }
            } else if (c.isStrangeLetterlike()) {
                
                auto loc = TheByteDecoder->SrcLoc;
                
                auto strangeLoc = loc.previous();
                
                Utils::strangeLetterlikeWarning(getTokenSource(strangeLoc), c);
                
            } else if (c.isMBStrangeLetterlike()) {
                
                auto loc = TheByteDecoder->SrcLoc;
                
                auto strangeLoc = loc.previous();
                
                Utils::mbStrangeLetterlikeWarning(getTokenSource(strangeLoc), c);
            }
#endif // !NISSUES
            
        } else if (c.to_point() == '`') {
            
            //
            // Advance past trailing `
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            break;
            
        } else {
            
            break;
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        charLoc = TheByteDecoder->SrcLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
    } // while
    
    return c;
}

inline Token Tokenizer::handleString(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
        
    assert(c.to_point() == '"');
    
    policy |= PRESERVE_WS_AFTER_LC | LC_IS_MEANINGFUL;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = TheCharacterDecoder->nextWLCharacter0(policy);
        
        switch (c.to_point()) {
            case '"':
                return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            case CODEPOINT_ENDOFFILE:
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        
    } // while
}

#if STARTOFLINE

inline Token Tokenizer::handleString_stringifyLine(Buffer tokenStartBuf, WLCharacter c, NextCharacterPolicy policy) {
    
    //
    // Nothing to assert
    //
    
    policy &= ~PRESERVE_WS_AFTER_LC;
    
    auto lastGoodBuffer = TheByteBuffer->getBuffer();
    xx;
    
    auto empty = true;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            break;
            
        } else if (c.to_point() == CODEPOINT_CRLF) {
            
            break;
            
        } else if (c.isNewline()) {
            
            break;
            
        } else if (c.isMBLineContinuation()) {
            
            break;
        }
        
        empty = false;
        
        lastGoodBuffer = TheByteBuffer->getBuffer();
        xx;
        
        TheCharacterDecoder->nextWLCharacter(policy);
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
    } // while
    
    if (empty) {
        
        //
        // Something like   ?<EOF>
        //
        // EndOfFile is special because there is no source
        //
        // So invent source
        //
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));
    }
    
    //
    // ?? syntax is special because we want to ignore the newline that was read.
    //
    // So invent source
    //
    
    return Token(TOKEN_STRING, BufferAndLength(tokenStartBuf, lastGoodBuffer - tokenStartBuf + 1, false));
}

#endif // STARTOFLINE

inline Token Tokenizer::handleString_stringifySymbol(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    //
    // Nothing to assert
    //
    
    if (c.to_point() == '"') {
        
        return handleString(tokenStartBuf, tokenStartLoc, c, policy);
    }
    
    //
    // magically turn into a string
    //
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        TokenizerContext Ctxt = 0;
        
        auto letterlikeBuf = TheByteBuffer->buffer;
        auto letterlikeLoc = TheByteDecoder->SrcLoc;
        
        handleSymbolSegment(letterlikeBuf, letterlikeLoc, c, policy, Ctxt);
        
        return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
        
    //
    // Something like   a::5
    //
    
    return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

//
// Use SourceCharacters here, not WLCharacters
//
inline Token Tokenizer::handleString_stringifyFile(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextCharacterPolicy policy) {
    
    //
    // Nothing to assert
    //
    
    if (c.to_point() == '"') {
        
        return handleString(tokenStartBuf, tokenStartLoc, WLCharacter('"'), policy);
    }
        
    //
    // magically turn into a string
    //
    
    switch (c.to_point()) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
        case '$': case '`': case '/': case '.': case '\\': case '!': case '-': case '_': case ':': case '*': case '~': case '?': {
            
            c = TheByteDecoder->currentSourceCharacter(policy);
        }
            break;
        case '[': {
            
            // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
            
            int handled;
            c = handleFileOpsBrackets(tokenStartLoc, c, policy, &handled);
            
            if (handled < 0) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
        }
            break;
        default: {
            //
            // Something like   <<EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            
            return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        }
            break;
    }
    
    auto breakWhile = false;
    while (true) {
        
        if (breakWhile) {
            break;
        }
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
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
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
            }
                break;
            case '[': {
                
                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                int handled;
                c = handleFileOpsBrackets(tokenStartLoc, c, policy, &handled);
                
                if (handled < 0) {
                    return Token(TOKEN_ERROR_UNTERMINATEDSTRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
            }
                break;
            default: {
                breakWhile = true;
            }
                break;
        }
        
    } // while
    
    return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}



const int UNTERMINATED_STRING = -1;

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
inline SourceCharacter Tokenizer::handleFileOpsBrackets(SourceLocation tokenStartLoc, SourceCharacter c, NextCharacterPolicy policy, int *handled) {
    
    assert(c.to_point() == '[');
    
    auto depth = 1;
    
    c = TheByteDecoder->currentSourceCharacter(policy);
    
    bool breakWhile = false;
    while (true) {
        
        if (breakWhile) {
            break;
        }
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        switch (c.to_point()) {
                //
                // Spaces and Newlines
                //
            case ' ': case '\t': case '\v': case '\f':
            case '\n': case '\r': case CODEPOINT_CRLF:
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                //
                // Cannot have spaces in the string here, so bail out
                //
                
                *handled = UNTERMINATED_STRING;
                
                return c;
            case CODEPOINT_ENDOFFILE:
                
                *handled = UNTERMINATED_STRING;
                
                return c;
            case '[':
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                depth = depth + 1;
                
                break;
            case ']':
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    breakWhile = true;
                }
                break;
            default:
                
                if (c.isMBWhitespace() || c.isMBNewline()) {
                    
                    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                    
                    *handled = UNTERMINATED_STRING;
                    
                    return c;
                    
                }
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                break;
        }
        
        c = TheByteDecoder->currentSourceCharacter(policy);
        
    } // while
    
    *handled = 0;
    
    return c;
}



const int UNRECOGNIZED_DIGIT = -1;
const int BAILOUT = -2;

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
inline Token Tokenizer::handleNumber(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.isDigit() || c.to_point() == '.');
    
    policy |= LC_IS_MEANINGFUL;
    
    //
    // given 16^^0.F, leadingDigitsEnd will point to .
    // given 16^^.F, leadingDigitsEnd will point to .
    // given 0.123, leadingDigitsEnd will point to .
    //
    auto leadingDigitsEndBuf = tokenStartBuf;
    auto leadingDigitsEndLoc = tokenStartLoc;
    
    //
    // Use the convention that base of 0 means the default, unspecified base
    //
    size_t base = 0;
    bool real = false;
    
    if (c.isDigit()) {
        
        //
        // With input of 002^^111, this will point to '2'
        //
        auto nonZeroStartBuf = tokenStartBuf;
        auto nonZeroStartLoc = tokenStartLoc;
        
        //
        // Count leading zeros
        //
        if (c.to_point() == '0') {
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            while (true) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.to_point() != '0') {
                    
                    nonZeroStartBuf = TheByteBuffer->buffer;
                    nonZeroStartLoc = TheByteDecoder->SrcLoc;
                    
                    break;
                }
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
                
            } // while
        }
        
        leadingDigitsEndBuf = TheByteBuffer->buffer;
        leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
        
        if (c.isDigit()) {
            
            size_t count;
            c = handleDigits(policy, c, &count);
            
            leadingDigitsEndBuf = TheByteBuffer->buffer;
            leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
        }
        
        if (c.to_point() == '.') {
            
            //
            // Eat the dot
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
        
        if (c.to_point() == '^') {
            
            //
            // Could be 16^^blah
            //
            
            auto caret1Buf = TheByteBuffer->buffer;
            auto caret1Loc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() != '^') {
                
                //
                // Something like  2^a
                //
                // Must now do surgery and back up
                //
                
                backup(caret1Buf, caret1Loc, false);
                
                //
                // Success!
                //
                
                return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
            
            // c is '^'
            
            //
            // Something like 2^^
            //
            // Must be a number
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            if (nonZeroStartBuf == caret1Buf) {
                
                //
                // Something like 0^^2
                //
                
                return Token(TOKEN_ERROR_INVALIDBASE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
            
            auto baseStrLen = caret1Buf - nonZeroStartBuf;
            
            //
            // bases can only be between 2 and 36, so we know they can only be 1 or 2 characters
            //
            if (baseStrLen > 2) {
                
                return Token(TOKEN_ERROR_INVALIDBASE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (baseStrLen == 2) {
                
                auto d1 = Utils::toDigit(nonZeroStartBuf[0]);
                auto d0 = Utils::toDigit(nonZeroStartBuf[1]);
                base = d1 * 10 + d0;
                
            } else {
                
                assert(baseStrLen == 1);
                
                auto d0 = Utils::toDigit(nonZeroStartBuf[0]);
                base = d0;
            }
            
            if (!(2 <= base && base <= 36)) {
                
                return Token(TOKEN_ERROR_INVALIDBASE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            //
            // What can come after ^^ ?
            //
            
            switch (c.to_point()) {
                case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
                case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
                case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                    
                    int handled;
                    c = handleAlphaOrDigits(c, base, policy, &handled);
                    switch (handled) {
                        case BAILOUT:
                            assert(false);
                            break;
                        case UNRECOGNIZED_DIGIT:
                            return Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                    
                    leadingDigitsEndBuf = TheByteBuffer->buffer;
                    leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
                    
                    if (c.to_point() == '.') {
                        
                        //
                        // Eat the dot
                        //
                        
                        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                        
                    }
                }
                    break;
                case '.': {
                    
                    leadingDigitsEndBuf = TheByteBuffer->buffer;
                    leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
                    
                    //
                    // Eat the dot
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                }
                    break;
                default: {
                    
                    //
                    // Something like 2^^@
                    //
                    
                    //
                    // Make sure that bad character is read
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    return Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
            }
        }
    }
    
    if (c.to_point() == '.') {
        
        //
        // assert(*(TheByteBuffer->buffer - 1) == '.');
        //
        // The dot could be a sequence of SourceCharacters, e.g. \056, so can't really assert here
        //
        
        int handled;
        c = handlePossibleFractionalPart(leadingDigitsEndBuf, leadingDigitsEndLoc, c, base, policy, &handled);
        switch (handled) {
            case BAILOUT:
                real = false;
                break;
            case UNRECOGNIZED_DIGIT:
                return Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            case 0:
                real = true;
                break;
            default:
                real = true;
                break;
        }
    }
    
    //
    // Handle all ` logic here
    //
    // foo`
    // foo`bar
    // foo``bar
    //
    if (c.to_point() == '`') {
        
        real = true;
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        bool accuracy = false;
        bool sign = false;
        bool supplied = false;
        if (c.to_point() == '`') {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            accuracy = true;
        }
        
        bool sawDot = false;
        switch (c.to_point()) {
            case '-': case '+': {
                
                //
                // Something like 1.2`-
                //
                
                auto signBuf = TheByteBuffer->buffer;
                auto signLoc = TheByteDecoder->SrcLoc;
                
                //
                // Eat the sign
                //
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
                
                switch (c.to_point()) {
                    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                        sign = true;
                        break;
                    case '.':
                        sawDot = true;
                        sign = true;
                        break;
                    default: {
                        
                        //
                        // Something like 1.2`->
                        //
                        
                        if (accuracy) {
                            
                            //
                            // Something like 1.2``->3
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            
                            return Token(TOKEN_ERROR_EXPECTEDACCURACY, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        }
                        
                        //
                        // Something like 1.2`->3
                        //
                        // Must now do surgery and back up
                        //
                        backup(signBuf, signLoc, true);
                        
                        // Success!
                        
                        return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                }
            }
            //
            // fall-through
            //
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                
                //
                // might jump here or fall-through
                //
                // Need to test whether last character was . or not
                //
                if (!sawDot) {
                    size_t count;
                    c = handleDigits(policy, c, &count);
                    if (count > 0) {
                        supplied = true;
                    }
                    
                    if (c.to_point() != '.') {
                        break;
                    }
                }
            }
            //
            // fall-through
            //
            case '.': {
                
                auto dotBuf = TheByteBuffer->buffer;
                auto dotLoc = TheByteDecoder->SrcLoc;
                
                bool actualDecimalPoint = false;
                
                //
                // If there was already a sign, or if the leading digits have already been supplied,
                // then this is an actual decimal point
                //
                if (sign || supplied) {
                    actualDecimalPoint = true;
                }
                
                //
                // Need to decide if the  .  here is actual radix point, or something like
                // the . in  123`.xxx  (which is Dot)
                //
                
                if (!actualDecimalPoint) {
                    
                    //
                    // Need to peek ahead
                    //
                    // Something like 123`.xxx
                    //
                    
                    // look ahead
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    auto NextChar = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    if (!NextChar.isDigit()) {
                        
                        if (accuracy) {
                            
                            //
                            // Something like  123``.EOF
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            
                            return Token(TOKEN_ERROR_EXPECTEDDIGIT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        }
                        
                        if (NextChar.isSign()) {
                            
                            //
                            // Something like  123`.+4
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            
                            return Token(TOKEN_ERROR_EXPECTEDDIGIT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
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
                        backup(dotBuf, dotLoc, true);
                        
                        //
                        // Success!
                        //
                        return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        
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
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                }
                
                //
                // actual decimal point
                //
                
                int handled;
                //
                // The base to use inside of precision/accuracy processing is 0, i.e., implied 10
                //
                size_t baseToUse = 0;
                c = handlePossibleFractionalPartPastDot(dotBuf, dotLoc, c, baseToUse, policy, &handled);
                switch (handled) {
                    case BAILOUT:
                        break;
                    case UNRECOGNIZED_DIGIT:
                        assert(false);
                        break;
                    case 0:
                        break;
                    default:
                        supplied = true;
                        break;
                }
                
                if (!supplied) {
                    
                    //
                    // Something like 1`+.a
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    return Token(TOKEN_ERROR_EXPECTEDDIGIT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
            } // case '.'
                break;
            default: {
                
                if (accuracy) {
                    
                    //
                    // Something like  123``EOF
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    return Token(TOKEN_ERROR_EXPECTEDACCURACY, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
            }
                break;
        } // switch (c.to_point())
    }
    
    if (c.to_point() != '*') {
        
        //
        // Success!
        //
        
        if (real) {
            return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        } else {
            return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
    }
    
    // c is '*'
    
    auto starBuf = TheByteBuffer->buffer;
    auto starLoc = TheByteDecoder->SrcLoc;
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.to_point() != '^') {
        
        //
        // Something like 1*a
        //
        // Must now do surgery and back up
        //

        auto resetBuf = starBuf;
        auto resetLoc = starLoc;
        
        backup(resetBuf, resetLoc, false);
        
        //
        // Success!
        //
        
        if (real) {
            return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        } else {
            return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
    }
    
    //
    // c is '^'
    //
    // So now examine *^ notation
    //
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.isSign()) {
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
    }
    
    if (!c.isDigit()) {
        
        //
        // Something like 123*^-EOF
        //
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        return Token(TOKEN_ERROR_EXPECTEDEXPONENT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    size_t count;
    c = handleDigits(policy, c, &count);
    
    if (c.to_point() != '.') {
        
        //
        // Success!
        //
        
        if (real) {
            return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        } else {
            return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
    }
    
    // c is '.'
        
    //
    // Something like 123*^0.5
    //
    // Make this an error, do NOT make this Dot[123*^0, 5]
    //
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    
    return Token(TOKEN_ERROR_EXPECTEDEXPONENT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
inline WLCharacter Tokenizer::handlePossibleFractionalPart(Buffer dotBuf, SourceLocation dotLoc, WLCharacter c, size_t base, NextCharacterPolicy policy, int *handled) {
    
    assert(c.to_point() == '.');
    
    assert(*dotBuf == '.' || *dotBuf == '\\'/* line continuation */);
    
    assert(((*dotBuf == '.') && (TheByteBuffer->buffer == dotBuf + 1)) || (*dotBuf == '\\'));
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    return handlePossibleFractionalPartPastDot(dotBuf, dotLoc, c, base, policy, handled);
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0
//         UNRECOGNIZED_DIGIT if base error
//         BAILOUT if not a radix point (and also backup before dot)
//
inline WLCharacter Tokenizer::handlePossibleFractionalPartPastDot(Buffer dotBuf, SourceLocation dotLoc, WLCharacter c, size_t base, NextCharacterPolicy policy, int *handled) {
    
    //
    // Nothing to assert
    //

    if (c.to_point() == '.') {
        
        //
        // Something like 0..
        //
        // The first . is not actually a radix point
        //
        // Must now do surgery and back up
        //
        
#if !NISSUES
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(dotLoc), " ")));
        
        auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, getTokenSource(dotLoc), 0.25, std::move(Actions)));
        
        Issues.push_back(std::move(I));
#endif // !NISSUES
        
        backup(dotBuf, dotLoc, false);
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        *handled = BAILOUT;

        return c;
    }
    
    if (c.isAlphaOrDigit()) {
        
        c = handleAlphaOrDigits(c, base, policy, handled);
        switch (*handled) {
            case BAILOUT:
                assert(false);
                break;
            case UNRECOGNIZED_DIGIT:
                return c;
            case 0:
                return c;
            default:
                
#if !NISSUES
                if (c.to_point() == '.') {
                    
                    //
                    // Something like 1.2.3
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert *", Source(dotLoc), "*")));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDIMPLICITTIMES, "Suspicious syntax.", SYNTAXISSUESEVERITY_ERROR, Source(dotLoc), 0.99, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
#endif // !NISSUES
                
                return c;
        }
    }

    *handled = 0;
    
    return c;
}
        
void Tokenizer::backup(Buffer resetBuf, SourceLocation resetLoc, bool warn) {
    
#if !NISSUES
    
    if (warn) {
        
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(resetLoc), " ")));
        
        auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, Source(resetLoc), 0.25, std::move(Actions)));
        
        Issues.push_back(std::move(I));
    }
#endif // !NISSUES
    
    TheByteBuffer->buffer = resetBuf;
    TheByteDecoder->SrcLoc = resetLoc;
}

//
// Precondition: currentWLCharacter is a digit
// Postcondition: buffer is pointing to first NON-DIGIT character after all digits
//
// return: the first NON-DIGIT character after all digits
//
inline WLCharacter Tokenizer::handleDigits(NextCharacterPolicy policy, WLCharacter c, size_t *countP) {
    
    assert(c.isDigit());
    
    size_t count = 1;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (!c.isDigit()) {
            
            break;
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
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
inline WLCharacter Tokenizer::handleAlphaOrDigits(WLCharacter c, size_t base, NextCharacterPolicy policy, int *handled) {
    
    assert(c.isAlphaOrDigit());
    
    int count = 0;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
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
                
                *handled = UNRECOGNIZED_DIGIT;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                return c;
            }
            
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        count++;
        
    } // while
    
    *handled = count;
    
    return c;
}

inline Token Tokenizer::handleColon(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == ':');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_COLON; // :
    
    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_COLONCOLON; // ::
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '=': {
            Operator = TOKEN_COLONEQUAL; // :=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '>': {
            Operator = TOKEN_COLONGREATER; // :>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleOpenParen(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.to_point() == '(');
    
    auto ParenChar = c;
    
    auto Operator = TOKEN_OPENPAREN; // (
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    //
    // Comments must start literally with (*
    // Escaped characters do not work
    //
    if ((ParenChar.to_point() == '(' && ParenChar.escape() == ESCAPE_NONE) &&
        (c.to_point() == '*' && c.escape() == ESCAPE_NONE)) {
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        return handleComment(tokenStartBuf, tokenStartLoc, SourceCharacter(c.to_point()), policy);
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleDot(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '.');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.isDigit()) {
        
        return handleNumber(tokenStartBuf, tokenStartLoc, firstChar, policy);
    }
    
    auto Operator = TOKEN_DOT; // .
    
    if (c.to_point() == '.') {
        
        Operator = TOKEN_DOTDOT; // ..
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        if (c.to_point() == '.') {
            
            Operator = TOKEN_DOTDOTDOT; // ...
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleEqual(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '=');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_EQUAL; // =
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_EQUALEQUAL; // ==
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALEQUALEQUAL; // ===
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
        }
            break;
        case '.': {
            
            //
            // Could be  =.  or  =..  or  =...  or  =....  or  =.0
            //
            
            auto dotBuf = TheByteBuffer->buffer;
            auto dotLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.isDigit()) {
                
                //
                // Something like x=.0
                //
                // Must now do surgery and back up
                //
                
                backup(dotBuf, dotLoc, true);
                
            } else if (c.to_point() == '.') {
                
                //
                // =..  is a syntax error
                //
                
                Operator = TOKEN_ERROR_UNHANDLEDDOT;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
            } else {
                
                Operator = TOKEN_EQUALDOT; // =.
            }
            
        }
            break;
        case '!': {
            
            auto bangBuf = TheByteBuffer->buffer;
            auto bangLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALBANGEQUAL; // =!=
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
            } else {
                
                //
                // Something like x=!y
                //
                // Must now do surgery and back up
                //
                
                backup(bangBuf, bangLoc, true);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleUnder(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '_');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_UNDER; // _
    
    switch (c.to_point()) {
        case '_': {
            
            Operator = TOKEN_UNDERUNDER; // __
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '_') {
                
                Operator = TOKEN_UNDERUNDERUNDER; // ___
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
            break;
        }
        case '.': {
            
            auto dotBuf = TheByteBuffer->buffer;
            auto dotLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '.') {
                
                //
                // Something like  _...
                //
                // Must now do surgery and back up
                //
                
                auto shouldWarn = true;
                backup(dotBuf, dotLoc, shouldWarn);
                
            } else {
                
                Operator = TOKEN_UNDERDOT; // _.
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleLess(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '<');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_LESS; // <
    
    switch (c.to_point()) {
        case '|': {
            
            Operator = TOKEN_LESSBAR; // <|
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '<': {
            
            Operator = TOKEN_LESSLESS; // <<
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '>': {
            
            Operator = TOKEN_LESSGREATER; // <>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '=': {
            
            Operator = TOKEN_LESSEQUAL; // <=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '-': {
            
            auto minusBuf = TheByteBuffer->buffer;
            auto minusLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_LESSMINUSGREATER; // <->
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
            } else {
                
                //
                // Something like  a<-4
                //
                // Must now do surgery and back up
                //
                
                backup(minusBuf, minusLoc, true);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleGreater(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '>');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_GREATER; // >
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_GREATERGREATER; // >>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_GREATERGREATERGREATER; // >>>
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
        }
            break;
        case '=': {
            
            Operator = TOKEN_GREATEREQUAL; // >=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleMinus(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '-');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_MINUS; // -
    
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
            
            Operator = TOKEN_MINUSGREATER; // ->
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '-': {
            
            Operator = TOKEN_MINUSMINUS; // --
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            auto afterLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '>') {
                
                //
                // Something like a-->0
                //
                
                auto greaterLoc = afterLoc;
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(greaterLoc), " ")));
                
                auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``>`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(greaterLoc), 0.25, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
            } else if (c.to_point() == '=') {
                
                //
                // Something like a--=0
                //
                
                auto equalLoc = afterLoc;
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(equalLoc), " ")));
                
                auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(equalLoc), 0.25, std::move(Actions)));
                
                Issues.push_back(std::move(I));
            }
#endif // !NISSUES
        }
            break;
        case '=': {
            
            Operator = TOKEN_MINUSEQUAL; // -=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleBar(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '|');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_BAR; // |
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_BARGREATER; // |>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            auto afterLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                //
                // Something like <||>=0
                //
                
                auto equalLoc = afterLoc;
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(equalLoc), " ")));
                
                auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``>`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(equalLoc), 0.25, std::move(Actions)));
                
                Issues.push_back(std::move(I));
            }
#endif // !NISSUES
            
        }
            break;
        case '|': {
            
            Operator = TOKEN_BARBAR; // ||
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleSemi(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == ';');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_SEMI; // ;
    
    if (c.to_point() == ';') {
        
        Operator = TOKEN_SEMISEMI; // ;;
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleBang(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '!');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_BANG; // !
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_BANGEQUAL; // !=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '!': {
            
            Operator = TOKEN_BANGBANG; // !!
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleHash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '#');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_UNKNOWN;
    
    //
    // From Slot documentation:
    //
    // In the form #name, the characters in name can be any combination of alphanumeric characters not beginning with digits.
    //
    //
    // A slot that starts with a digit goes down one path
    // And a slot that starts with a letter goes down another path
    //
    // Make sure e.g.  #1a is not parsed as SlotNode["#1a"]
    //
    
    switch (c.to_point()) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            
            Operator = TOKEN_HASH; // #
            
            size_t count;
            c = handleDigits(policy, c, &count);
        }
            break;
        //
        // letterlike
        //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': /*    \x07*/
        case '\x08': /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        case '$':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '`':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': {
            
            auto symbolStartBuf = TheByteBuffer->buffer;
            auto symbolStartLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            Operator = TOKEN_HASH; // #
            
            TokenizerContext Ctxt = 0;
            
            Ctxt |= TOKENIZER_SLOT;
            
            handleSymbol(symbolStartBuf, symbolStartLoc, c, policy, Ctxt);
        }
            break;
        case '"': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            Operator = TOKEN_HASH; // #
            
            handleString(tokenStartBuf, tokenStartLoc, c, policy);
            
#if !NISSUES
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``\"`` character.", SYNTAXISSUESEVERITY_REMARK, getTokenSource(tokenStartLoc), 0.33, {}));
            
            Issues.push_back(std::move(I));
#endif // !NISSUES
        }
            break;
        case '#': {
            
            Operator = TOKEN_HASHHASH; // ##
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.isDigit()) {
                
                size_t count;
                handleDigits(policy, c, &count);
            }
        }
            break;
        default: {
            
            //
            // All other multi-byte characters
            //
            
            if (c.isMBLetterlike()) {
                
                auto symbolStartBuf = TheByteBuffer->buffer;
                auto symbolStartLoc = TheByteDecoder->SrcLoc;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                Operator = TOKEN_HASH; // #
                
                TokenizerContext Ctxt = 0;
                
                Ctxt |= TOKENIZER_SLOT;
                
                handleSymbol(symbolStartBuf, symbolStartLoc, c, policy, Ctxt);
                
            } else {
                
                Operator = TOKEN_HASH; // #
            }
            
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handlePercent(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '%');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_PERCENT; // %
    
    if (c.to_point() == '%') {
        
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            if (c.to_point() != '%') {
                break;
            }
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
        }
        
    } else if (c.isDigit()) {
        
        size_t count;
        handleDigits(policy, c, &count);
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleAmp(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '&');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_AMP; // &
    
    if (c.to_point() == '&') {
        
        Operator = TOKEN_AMPAMP; // &&
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleSlash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '/');
    
    auto Operator = TOKEN_SLASH; // /
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    switch (c.to_point()) {
        case '@': {
            
            Operator = TOKEN_SLASHAT; // /@
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case ';': {
            
            Operator = TOKEN_SLASHSEMI; // /;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '.': {
            
            auto nextBuf = TheByteBuffer->buffer;
            auto nextLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.isDigit()) {
                
                //
                // Something like t/.3
                //
                // Must now do surgery and back up
                //
                
                backup(nextBuf, nextLoc, true);
                
            } else {
                
                Operator = TOKEN_SLASHDOT; // /.
            }
        }
            break;
        case '/': {
            
            Operator = TOKEN_SLASHSLASH; // //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            switch (c.to_point()) {
                case '.': {
                    
                    Operator = TOKEN_SLASHSLASHDOT; // //.
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                }
                    break;
                case '@': {
                    
                    Operator = TOKEN_SLASHSLASHAT; // //@
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                }
                    break;
            }
        }
            break;
        case ':': {
            
            Operator = TOKEN_SLASHCOLON; // /:
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '=': {
            
            Operator = TOKEN_SLASHEQUAL; // /=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '*': {
            
            Operator = TOKEN_SLASHSTAR; // /*
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleAt(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '@');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_AT; // @
    
    switch (c.to_point()) {
        case '@': {
            
            Operator = TOKEN_ATAT; // @@
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '@') {
                
                Operator = TOKEN_ATATAT; // @@@
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
        }
            break;
        case '*': {
            
            Operator = TOKEN_ATSTAR; // @*
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handlePlus(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '+');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_PLUS; // +
    
    switch (c.to_point()) {
        case '+': {
            Operator = TOKEN_PLUSPLUS; // ++
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                //
                // Something like a++=0
                //
                
                auto loc = TheByteDecoder->SrcLoc;
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(loc), " ")));
                
                auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``+`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(loc), 0.25, std::move(Actions)));
                
                Issues.push_back(std::move(I));
            }
#endif // !NISSUES
        }
            break;
        case '=': {
            Operator = TOKEN_PLUSEQUAL; // +=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleTilde(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '~');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_TILDE; // ~
    
    if (c.to_point() == '~') {
        
        Operator = TOKEN_TILDETILDE; // ~~
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleQuestion(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '?');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_QUESTION; // ?
    
    if (c.to_point() == '?') {
        
        Operator = TOKEN_QUESTIONQUESTION; // ??
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleStar(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '*');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_STAR; // *
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_STAREQUAL; // *=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
        case '*': {
            
            Operator = TOKEN_STARSTAR; // **
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleCaret(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    policy |= LC_IS_MEANINGFUL;
    
    assert(c.to_point() == '^');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_CARET;
    
    switch (c.to_point()) {
        case ':': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_CARETCOLONEQUAL; // ^:=
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
            } else {
                
                //
                // Has to be ^:=
                //
                
                Operator = TOKEN_ERROR_EXPECTEDEQUAL;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
        }
            break;
        case '=': {
            Operator = TOKEN_CARETEQUAL; // ^=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleUnhandledBackSlash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    //
    // Unhandled \
    //
    // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
    // This duplicates some logic in CharacterDecoder
    //
    
    assert(c.to_point() == '\\');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    switch (c.to_point()) {
        case '[': {
            
            //
            // Try to reconstruct \[XXX]
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            while (true) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isAlphaOrDigit()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case ':': {
            
            //
            // Try to reconstruct \:XXXX
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 4; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case '.': {
            
            //
            // Try to reconstruct \.XX
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 2; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            //
            // Try to reconstruct \XXX
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 3; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isOctal()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case '|': {
            
            //
            // Try to reconstruct \|XXXXXX
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 6; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case CODEPOINT_ENDOFFILE: {
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        default: {
            
            //
            // Nothing special, just read next character
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
    }
}

inline Token Tokenizer::handleMBStrangeNewline(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.isMBStrangeNewline());
    
#if !NISSUES
    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected newline character.", SYNTAXISSUESEVERITY_WARNING, getTokenSource(tokenStartLoc), 0.85, {}));
    
    Issues.push_back(std::move(I));
#endif // !NISSUES
    
    //
    // Return TOPLEVELNEWLINE or INTERNALNEWLINE, depending on policy
    //
    return Token(TOKEN_TOPLEVELNEWLINE.t() | (policy & RETURN_INTERNALNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleMBStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.isMBStrangeWhitespace());
    
#if !NISSUES
    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected space character.", SYNTAXISSUESEVERITY_WARNING, getTokenSource(tokenStartLoc), 0.85, {}));
    
    Issues.push_back(std::move(I));
#endif // !NISSUES
    
    return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleMBPunctuation(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.isMBPunctuation());
    
    auto Operator = LongNameCodePointToOperator(c.to_point());
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleMBLinearSyntax(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextCharacterPolicy policy) {
    
    assert(c.isMBLinearSyntax());
    
    TokenEnum Operator;
    
    switch (c.to_point()) {
        case CODEPOINT_LINEARSYNTAX_BANG:
            Operator = TOKEN_LINEARSYNTAX_BANG;
            break;
        case CODEPOINT_LINEARSYNTAX_OPENPAREN:
            Operator = TOKEN_LINEARSYNTAX_OPENPAREN;
            break;
        case CODEPOINT_LINEARSYNTAX_STAR:
            Operator = TOKEN_LINEARSYNTAX_STAR;
            break;
        case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
            Operator = TOKEN_LINEARSYNTAX_CLOSEPAREN;
            break;
        case CODEPOINT_LINEARSYNTAX_AT:
            Operator = TOKEN_LINEARSYNTAX_AT;
            break;
        case CODEPOINT_LINEARSYNTAX_CARET:
            Operator = TOKEN_LINEARSYNTAX_CARET;
            break;
        case CODEPOINT_LINEARSYNTAX_UNDER:
            Operator = TOKEN_LINEARSYNTAX_UNDER;
            break;
        case CODEPOINT_LINEARSYNTAX_PERCENT:
            Operator = TOKEN_LINEARSYNTAX_PERCENT;
            break;
        case CODEPOINT_LINEARSYNTAX_AMP:
            Operator = TOKEN_LINEARSYNTAX_AMP;
            break;
        case CODEPOINT_LINEARSYNTAX_SLASH:
            Operator = TOKEN_LINEARSYNTAX_SLASH;
            break;
        case CODEPOINT_LINEARSYNTAX_PLUS:
            Operator = TOKEN_LINEARSYNTAX_PLUS;
            break;
        case CODEPOINT_LINEARSYNTAX_BACKTICK:
            Operator = TOKEN_LINEARSYNTAX_BACKTICK;
            break;
        case CODEPOINT_LINEARSYNTAX_SPACE:
            Operator = TOKEN_LINEARSYNTAX_SPACE;
            break;
        default:
            assert(false);
            Operator = TOKEN_UNKNOWN;
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}



Source Tokenizer::getTokenSource(SourceLocation tokStartLoc) const {
    auto loc = TheByteDecoder->SrcLoc;
    return Source(tokStartLoc, loc);
}

BufferAndLength Tokenizer::getTokenBufferAndLength(Buffer tokStartBuf) const {
    auto buf = TheByteBuffer->buffer;
    auto status = TheByteDecoder->getStatus();
    return BufferAndLength(tokStartBuf, buf - tokStartBuf, status);
}

#if !NISSUES
void Tokenizer::addIssue(IssuePtr I) {
    Issues.push_back(std::move(I));
}

std::vector<IssuePtr>& Tokenizer::getIssues() {
    return Issues;
}
#endif // !NISSUES

TokenizerPtr TheTokenizer = nullptr;
