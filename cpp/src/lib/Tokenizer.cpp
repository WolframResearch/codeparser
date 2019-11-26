
#include "Tokenizer.h"

#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "Utils.h"
#include "CodePoint.h"

size_t toDigit(int val);

#if STARTOFLINE
const NextCharacterPolicy INSIDE_STRINGIFY_LINE = ENABLE_BYTE_DECODING_ISSUES | ENABLE_CHARACTER_DECODING_ISSUES | LC_IS_MEANINGFUL | ENABLE_STRANGE_CHARACTER_CHECKING;
#endif // STARTOFLINE
const NextCharacterPolicy INSIDE_STRINGIFY_SYMBOL = ENABLE_BYTE_DECODING_ISSUES | PRESERVE_WS_AFTER_LC | ENABLE_CHARACTER_DECODING_ISSUES | LC_IS_MEANINGFUL | ENABLE_STRANGE_CHARACTER_CHECKING;
const NextCharacterPolicy INSIDE_STRINGIFY_FILE = ENABLE_BYTE_DECODING_ISSUES | LC_IS_MEANINGFUL | ENABLE_STRANGE_CHARACTER_CHECKING;


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
// after calling nextWLCharacter:
// memory: 1+\[Alpha]-2
//                   ^
//                   buffer
// return \[Alpha]
//
Token Tokenizer::nextToken0(NextCharacterPolicy policy) {
    
    TokenizerContext Ctxt = 0;
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    
    auto c = TheCharacterDecoder->nextWLCharacter0(policy);
    
    switch (c.to_point()) {
        
        //
        // all single-byte characters
        //
        
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06':
        case '\b':
        case '\x0e': case '\x0f': case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17': case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        case '$':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '`':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            return handleSymbol(tokenStartBuf, c, policy, Ctxt);
        case '\x07':
        case '\x7f':
            return Token(TOKEN_ERROR_UNINTERPRETABLECHARACTER, getTokenBufferAndLength(tokenStartBuf));
        case '\t':
            return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf));
        case '\n':
            return Token(TOKEN_NEWLINE, getTokenBufferAndLength(tokenStartBuf));
        case '\v': case '\f':
            return handleStrangeSpace(tokenStartBuf, c, policy);
        case '\r':
            return Token(TOKEN_NEWLINE, getTokenBufferAndLength(tokenStartBuf));
        case ' ':
            return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf));
        case '!':
            return handleBang(tokenStartBuf, c, policy);
        case '"':
            return handleString(tokenStartBuf, c, policy);
        case '#':
            return handleHash(tokenStartBuf, c, policy);
        case '%':
            return handlePercent(tokenStartBuf, c, policy);
        case '&':
            return handleAmp(tokenStartBuf, c, policy);
        case '\'':
            return Token(TOKEN_SINGLEQUOTE, getTokenBufferAndLength(tokenStartBuf));
        case '(':
            return handleOpenParen(tokenStartBuf, c, policy);
        case ')':
            return Token(TOKEN_CLOSEPAREN, getTokenBufferAndLength(tokenStartBuf));
        case '*':
            return handleStar(tokenStartBuf, c, policy);
        case '+':
            return handlePlus(tokenStartBuf, c, policy);
        case ',':
            return Token(TOKEN_COMMA, getTokenBufferAndLength(tokenStartBuf));
        case '-':
            return handleMinus(tokenStartBuf, c, policy);
        case '.':
            return handleDot(tokenStartBuf, c, policy);
        case '/':
            return handleSlash(tokenStartBuf, c, policy);
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            return handleNumber(tokenStartBuf, c, policy);
        case ':':
            return handleColon(tokenStartBuf, c, policy);
        case ';':
            return handleSemi(tokenStartBuf, c, policy);
        case '<':
            return handleLess(tokenStartBuf, c, policy);
        case '=':
            return handleEqual(tokenStartBuf, c, policy);
        case '>':
            return handleGreater(tokenStartBuf, c, policy);
        case '?':
            return handleQuestion(tokenStartBuf, c, policy);
        case '@':
            return handleAt(tokenStartBuf, c, policy);
        case '[':
            return Token(TOKEN_OPENSQUARE, getTokenBufferAndLength(tokenStartBuf));
        case '\\':
            return handleUnhandledBackSlash(tokenStartBuf, c, policy);
        case ']':
            return Token(TOKEN_CLOSESQUARE, getTokenBufferAndLength(tokenStartBuf));
        case '^':
            return handleCaret(tokenStartBuf, c, policy);
        case '_':
            return handleUnder(tokenStartBuf, c, policy);
        case '{':
            return Token(TOKEN_OPENCURLY, getTokenBufferAndLength(tokenStartBuf));
        case '|':
            return handleBar(tokenStartBuf, c, policy);
        case '}':
            return Token(TOKEN_CLOSECURLY, getTokenBufferAndLength(tokenStartBuf));
        case '~':
            return handleTilde(tokenStartBuf, c, policy);
        default: {
            
            //
            // Everything else involving multi-byte characters
            //
            
            if (c.to_point() == CODEPOINT_ENDOFFILE) {
                
                return Token(TOKEN_ENDOFFILE, getTokenBufferAndLength(tokenStartBuf));
                
            } else if (c.to_point() == CODEPOINT_CRLF) {
                
                return Token(TOKEN_NEWLINE, getTokenBufferAndLength(tokenStartBuf));
                
            } else if (c.isMBLinearSyntax()) {
                
                return handleMBLinearSyntax(tokenStartBuf, c, policy);
                
            } else if (c.isMBLineContinuation()) {
                
                return Token(TOKEN_LINECONTINUATION, getTokenBufferAndLength(tokenStartBuf));
                
            } else if (c.isMBUninterpretable()) {
                
                return Token(TOKEN_ERROR_UNINTERPRETABLECHARACTER, getTokenBufferAndLength(tokenStartBuf));
                
            } else if (c.isMBStrangeSpace()) {
                
                return handleMBStrangeSpace(tokenStartBuf, c, policy);
                
            } else if (c.isMBSpace()) {
                
                return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf));
                
            } else if (c.isMBStrangeNewline()) {
                
                return handleMBStrangeNewline(tokenStartBuf, c, policy);
                
            } else if (c.isMBNewline()) {
                
                return Token(TOKEN_NEWLINE, BufferAndLength(tokenStartBuf, 0, false));
                
            } else if (c.isMBPunctuation()) {
                
                return handleMBPunctuation(tokenStartBuf, c, policy);
                
            } else if (c.isMBStringMeta()) {
                
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
                
            } else if (c.isMBUnsupported()) {
                
                return Token(TOKEN_ERROR_UNSUPPORTEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
                
            } else {
                
                //
                // if nothing else, then it is letterlike
                //
                
                assert(c.isMBLetterlike());
                
                return handleSymbol(tokenStartBuf, c, policy, Ctxt);
            }
        }
    }
}


#if STARTOFLINE

Token Tokenizer::nextToken0_stringifyLine() {

    auto tokenStartBuf = TheByteBuffer->getBuffer();

    auto policy = INSIDE_STRINGIFY_LINE;

    auto c = TheCharacterDecoder->nextWLCharacter0(policy);

    if (c.to_point() == CODEPOINT_ENDOFFILE) {

        //
        // EndOfFile is special, so invent source
        //

        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));

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
    
    auto policy = INSIDE_STRINGIFY_SYMBOL;
    
    auto c = TheCharacterDecoder->nextWLCharacter0(policy);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        //
        // EndOfFile is special, so invent source
        //
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));
        
    } else if (c.isNewline()) {
        
        //
        // Newline is special, so invent source
        //
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));
    }
    
    return handleString_stringifySymbol(tokenStartBuf, c, policy);
}

Token Tokenizer::nextToken0_stringifyFile() {
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    
    auto policy = INSIDE_STRINGIFY_FILE;
    
    auto c = TheByteDecoder->nextSourceCharacter0(policy);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));
        
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
        
        return Token(TOKEN_NEWLINE, getTokenBufferAndLength(tokenStartBuf));
    }
    
    //
    // There could be space, something like  << abc
    //
    // or something like:
    // a >>
    //   b
    //
    
    if (c.isSpace()) {
        
        return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf));
    }
    
    return handleString_stringifyFile(tokenStartBuf, c, policy);
}

void Tokenizer::nextToken(NextCharacterPolicy policy) {
    
    nextToken0(policy);
    
    TheByteDecoder->clearError();
}

#if STARTOFLINE

void Tokenizer::nextToken_stringifyLine() {

    nextToken0_stringifyLine();

    TheByteDecoder->clearError();
}

#endif // STARTOFLINE

void Tokenizer::nextToken_stringifySymbol() {
    
    nextToken0_stringifySymbol();
    
    TheByteDecoder->clearError();
}

void Tokenizer::nextToken_stringifyFile() {
    
    nextToken0_stringifyFile();
    
    TheByteDecoder->clearError();
}


//
//
//
Token Tokenizer::currentToken(NextCharacterPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    
    auto newPolicy = policy;
    
    newPolicy = newPolicy & DISABLE_CHECKS_MASK;
    
    auto Tok = nextToken0(newPolicy);
    
    TheByteBuffer->buffer = resetBuf;
    
    TheByteDecoder->clearError();
    
    return Tok;
}

#if STARTOFLINE

Token Tokenizer::currentToken_stringifyLine() {

    auto resetBuf = TheByteBuffer->getBuffer();

    auto Tok = nextToken0_stringifyLine();

    TheByteBuffer->setBuffer(resetBuf);

    TheByteDecoder->clearError();

    return Tok;
}

#endif // STARTOFLINE

Token Tokenizer::currentToken_stringifySymbol() {
    
    auto resetBuf = TheByteBuffer->buffer;
    
    auto Tok = nextToken0_stringifySymbol();
    
    TheByteBuffer->buffer = resetBuf;
    
    TheByteDecoder->clearError();
    
    return Tok;
}

Token Tokenizer::currentToken_stringifyFile() {
    
    auto resetBuf = TheByteBuffer->buffer;
    
    auto Tok = nextToken0_stringifyFile();
    
    TheByteBuffer->buffer = resetBuf;
    
    TheByteDecoder->clearError();
    
    return Tok;
}

inline Token Tokenizer::handleStrangeSpace(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    assert(firstChar.isStrangeSpace());
    
#if !NISSUES
    if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
        
        auto tokenStartLoc = TheByteDecoder->convertBufferToStart(tokenStartBuf);
        
        auto Src = getTokenSource(tokenStartLoc);
        
        auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        Issues.push_back(std::move(I));
    }
#endif // !NISSUES
    
    return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleComment(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    //
    // comment is already started
    //
    // Comments deal with literal (**) characters
    // Escaped characters do not work
    //
    
    auto c = firstChar;
    
    assert(c.to_point() == '*');
    
    auto depth = 1;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, getTokenBufferAndLength(tokenStartBuf));
    }
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        //
        // No need to check for comment length
        //
        
        if (c == WLCharacter('(')) {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c == WLCharacter('*')) {
                
                depth = depth + 1;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
            }
            
        } else if (c == WLCharacter('*')) {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c == WLCharacter(')')) {
                
                // This comment is closing
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    break;
                }
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
            }
            
        } else if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, getTokenBufferAndLength(tokenStartBuf));
            
        } else {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
        }
        
    } // while
    
    return Token(TOKEN_COMMENT, getTokenBufferAndLength(tokenStartBuf));
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
inline Token Tokenizer::handleSymbol(Buffer symbolStartBuf, WLCharacter firstChar, NextCharacterPolicy policyIn, TokenizerContext Ctxt) {
    
    auto policy = policyIn;
    policy = policy | LC_IS_MEANINGFUL;
    
    auto c = firstChar;
    
    assert(c.to_point() == '`' || c.isLetterlike() || c.isMBLetterlike());
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        c = handleSymbolSegment(symbolStartBuf, c, policy, Ctxt);
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
        if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
            
            if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
                
                //
                // It's hard to keep track of the ` characters, so just report the entire symbol. Oh well
                //
                
                auto symbolStartLoc = TheByteDecoder->convertBufferToStart(symbolStartBuf);
                
                auto Src = getTokenSource(symbolStartLoc);
                
                auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the **`** character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
                
                Issues.push_back(std::move(I));
            }
        }
#endif // !NISSUES
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        if (c.isLetterlike() || c.isMBLetterlike()) {
            
            auto letterlikeBuf = TheByteBuffer->buffer;
            
            c = handleSymbolSegment(letterlikeBuf, c, policy, Ctxt);
            
        } else {
            
            //
            // Something like  a`1
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, getTokenBufferAndLength(symbolStartBuf));
        }
        
    } // while
    
    return Token(TOKEN_SYMBOL, getTokenBufferAndLength(symbolStartBuf));
}

//
// Precondition: currentWLCharacter is letterlike
// Postcondition: buffer is pointing to first NON-SYMBOLSEGMENT character after all symbol segment characters
//
// return: the first NON-SYMBOLSEGMENT character after all symbol segment characters
//
inline WLCharacter Tokenizer::handleSymbolSegment(Buffer firstCharBuf, WLCharacter firstChar, NextCharacterPolicy policy, TokenizerContext Ctxt) {
    
    auto charBuf = firstCharBuf;
    auto c = firstChar;
    
    assert(c.isLetterlike() || c.isMBLetterlike());
    
#if !NISSUES
    if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
        
        if (c.to_point() == '$') {
            
            if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
                
                auto charLoc = TheByteDecoder->convertBufferToStart(charBuf);
                
                auto Src = getTokenSource(charLoc);
                
                auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the ``$`` character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
                
                Issues.push_back(std::move(I));
            }
        } else if (c.isStrangeLetterlike() || c.isMBStrangeLetterlike()) {
            Utils::strangeLetterlikeWarning(getTokenBufferAndLength(firstCharBuf), c);
        }
    }
#endif // !NISSUES
    
    charBuf = TheByteBuffer->buffer;
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isDigit()) {
            ;
        } else if (c.isLetterlike() || c.isMBLetterlike()) {
            
#if !NISSUES
            if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
                
                if (c.to_point() == '$') {
                    
                    if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
                        
                        auto charLoc = TheByteDecoder->convertBufferToStart(charBuf);
                        
                        auto Src = getTokenSource(charLoc);
                        
                        auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the ``$`` character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
                        
                        Issues.push_back(std::move(I));
                    }
                } else if (c.isStrangeLetterlike() || c.isMBStrangeLetterlike()) {
                    
                    auto buf = TheByteBuffer->buffer;
                    auto strangeBuf = buf - 1;
                    
                    Utils::strangeLetterlikeWarning(getTokenBufferAndLength(strangeBuf), c);
                }
            }
#endif // !NISSUES
            
        } else if (c.to_point() == '`') {
            
            //
            // Advance past trailing `
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            break;
            
        } else {
            
            break;
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        
        charBuf = TheByteBuffer->buffer;
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
    } // while
    
    return c;
}

inline Token Tokenizer::handleString(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
        
    assert(c.to_point() == '"');
    
    auto newPolicy = policy;
    
    newPolicy = newPolicy | PRESERVE_WS_AFTER_LC | LC_IS_MEANINGFUL;
//    newPolicy = newPolicy & ~(LC_UNDERSTANDS_CRLF);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = TheCharacterDecoder->nextWLCharacter0(newPolicy);
        
        switch (c.to_point()) {
            case '"':
                return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf));
            case CODEPOINT_ENDOFFILE:
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, getTokenBufferAndLength(tokenStartBuf));
        }
        
    } // while
}

#if STARTOFLINE

inline Token Tokenizer::handleString_stringifyLine(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    //
    // Nothing to assert
    //
    
    auto newPolicy = policy & ~PRESERVE_WS_AFTER_LC;
    
    auto lastGoodBuffer = TheByteBuffer->getBuffer();
    
    auto empty = true;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c == WLCharacter(CODEPOINT_ENDOFFILE)) {
            
            break;
            
        } else if (c.isNewline() || c.isMBNewline()) {
            
            break;
            
        } else if (c.isMBLineContinuation()) {
            
            break;
        }
        
        empty = false;
        
        lastGoodBuffer = TheByteBuffer->getBuffer();
        
        TheCharacterDecoder->nextWLCharacter(newPolicy);
        
        c = TheCharacterDecoder->currentWLCharacter(newPolicy);
        
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

inline Token Tokenizer::handleString_stringifySymbol(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    //
    // Nothing to assert
    //
    
    if (c.to_point() == '"') {
        
        return handleString(tokenStartBuf, c, policy);
    }
    
    //
    // magically turn into a string
    //
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        TokenizerContext Ctxt = 0;
        
        auto letterlikeBuf = TheByteBuffer->buffer;
        
        handleSymbolSegment(letterlikeBuf, c, policy, Ctxt);
        
        return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf));
    }
        
    //
    // Something like   a::5
    //
    
    nextToken(policy);
    
    return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleString_stringifyFile(Buffer tokenStartBuf, SourceCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    //
    // Nothing to assert
    //
    
    if (c.to_point() == '"') {
        
        return handleString(tokenStartBuf, WLCharacter('"'), policy);
    }
        
    //
    // magically turn into a string
    //
    
    switch (c.to_point()) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
        case '$': case '`': case '/': case '.': case '\\': case '!': case '-': case '_': case ':': case '*': case '~': case '?': {
            
            c = TheByteDecoder->currentSourceCharacter(policy);
        }
            break;
        case '[': {
            
            // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
            
            int handled;
            c = handleFileOpsBrackets(tokenStartBuf, c, policy, &handled);
            
            if (handled < 0) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, BufferAndLength(tokenStartBuf, 0, false));
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
            
            return Token(TOKEN_ERROR_EMPTYSTRING, BufferAndLength(tokenStartBuf, 0, false));
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
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            case '$': case '`': case '/': case '.': case '\\': case '!': case '-': case '_': case ':': case '*': case '~': case '?': {
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
            }
                break;
            case '[': {
                
                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                
                int handled;
                c = handleFileOpsBrackets(tokenStartBuf, c, policy, &handled);
                
                if (handled < 0) {
                    return Token(TOKEN_ERROR_UNTERMINATEDSTRING, getTokenBufferAndLength(tokenStartBuf));
                }
                
            }
                break;
            default: {
                breakWhile = true;
            }
                break;
        }
        
    } // while
    
    return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf));
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
inline SourceCharacter Tokenizer::handleFileOpsBrackets(Buffer tokenStartBuf, SourceCharacter firstChar, NextCharacterPolicy policy, int *handled) {
    
    auto c = firstChar;
    
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
                
                depth = depth + 1;
                
                break;
            case ']':
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    breakWhile = true;
                }
                break;
            default:
                
                if (c.isMBSpace() || c.isMBNewline()) {
                    
                    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                    
                    *handled = UNTERMINATED_STRING;
                    
                    return c;
                    
                }
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
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
inline Token Tokenizer::handleNumber(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policyIn) {
    
    auto c = firstChar;
    
    assert(c.isDigit() || c.to_point() == '.');
    
    auto policy = policyIn;
    policy = policy | LC_IS_MEANINGFUL;
    
    //
    // given 16^^0.F, leadingDigitsEnd will point to .
    // given 16^^.F, leadingDigitsEnd will point to .
    // given 0.123, leadingDigitsEnd will point to .
    //
    auto leadingDigitsEnd = tokenStartBuf;
    
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
                    
                    break;
                }
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
                
            } // while
        }
        
        leadingDigitsEnd = TheByteBuffer->buffer;
        
        if (c.isDigit()) {
            
            size_t count;
            c = handleDigits(policy, c, &count);
            
            leadingDigitsEnd = TheByteBuffer->buffer;
        }
        
        if (c.to_point() == '.') {
            
            //
            // Eat the dot
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
        
        if (c.to_point() == '^') {
            
            //
            // Could be 16^^blah
            //
            
            auto caret1Buffer = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() != '^') {
                
                //
                // Something like  2^a
                //
                // Must now do surgery and back up
                //
                
                backup(caret1Buffer, false);
                
                //
                // Success!
                //
                
                return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf));
            }
            
            // c is '^'
            
            //
            // Something like 2^^
            //
            // Must be a number
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            if (nonZeroStartBuf == caret1Buffer) {
                
                //
                // Something like 0^^2
                //
                
                return Token(TOKEN_ERROR_INVALIDBASE, getTokenBufferAndLength(tokenStartBuf));
            }
            
            auto baseStr = std::string(reinterpret_cast<const char *>(nonZeroStartBuf), caret1Buffer - nonZeroStartBuf);
            
            //
            // bases can only be between 2 and 36, so we know they can only be 1 or 2 characters
            //
            if (baseStr.size() > 2) {
                
                return Token(TOKEN_ERROR_INVALIDBASE, getTokenBufferAndLength(tokenStartBuf));
            }
            
            base = Utils::parseInteger(baseStr, 10);
            
            if (!(2 <= base && base <= 36)) {
                
                return Token(TOKEN_ERROR_INVALIDBASE, getTokenBufferAndLength(tokenStartBuf));
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
                            return Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, getTokenBufferAndLength(tokenStartBuf));
                    }
                    
                    leadingDigitsEnd = TheByteBuffer->buffer;
                    
                    if (c.to_point() == '.') {
                        
                        //
                        // Eat the dot
                        //
                        
                        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                        
                    }
                }
                    break;
                case '.': {
                    
                    leadingDigitsEnd = TheByteBuffer->buffer;
                    
                    //
                    // Eat the dot
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
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
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    return Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, getTokenBufferAndLength(tokenStartBuf));
                }
            }
        }
    }
    
    if (c.to_point() == '.') {
        
        // Could be \056, so can't really assert here
//        assert(*(TheByteBuffer->buffer - 1) == '.');
        
        int handled;
        c = handlePossibleFractionalPart(leadingDigitsEnd, c, base, policy, &handled);
        switch (handled) {
            case BAILOUT:
                real = false;
                break;
            case UNRECOGNIZED_DIGIT:
                return Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, getTokenBufferAndLength(tokenStartBuf));
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
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        bool accuracy = false;
        bool sign = false;
        bool supplied = false;
        if (c.to_point() == '`') {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            accuracy = true;
        }
        
        bool sawDot{};
        switch (c.to_point()) {
            case '-': case '+': {
                
                //
                // Something like 1.2`-
                //
                
                auto SignBuffer = TheByteBuffer->buffer;
                
                //
                // Eat the sign
                //
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
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
                            
                            return Token(TOKEN_ERROR_EXPECTEDACCURACY, getTokenBufferAndLength(tokenStartBuf));
                        }
                        
                        //
                        // Something like 1.2`->3
                        //
                        // Must now do surgery and back up
                        //
                        backup(SignBuffer, true);
                        
                        // Success!
                        
                        return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf));
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
                
                auto DotBuffer = TheByteBuffer->buffer;
                
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
                    
                    auto NextChar = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    if (!NextChar.isDigit()) {
                        
                        if (accuracy) {
                            
                            //
                            // Something like  123``.EOF
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            
                            return Token(TOKEN_ERROR_EXPECTEDDIGIT, getTokenBufferAndLength(tokenStartBuf));
                        }
                        
                        if (NextChar.isSign()) {
                            
                            //
                            // Something like  123`.+4
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            
                            return Token(TOKEN_ERROR_EXPECTEDDIGIT, getTokenBufferAndLength(tokenStartBuf));
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
                        backup(DotBuffer, true);
                        
                        //
                        // Success!
                        //
                        return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf));
                    }
                    
                } else {
                    
                    //
                    // actual decimal point
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
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
                c = handlePossibleFractionalPartPastDot(DotBuffer, c, baseToUse, policy, &handled);
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
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    return Token(TOKEN_ERROR_EXPECTEDDIGIT, getTokenBufferAndLength(tokenStartBuf));
                }
                
            } // case '.'
                break;
            default: {
                
                if (accuracy) {
                    
                    //
                    // Something like  123``EOF
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                    return Token(TOKEN_ERROR_EXPECTEDACCURACY, getTokenBufferAndLength(tokenStartBuf));
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
            return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf));
        } else {
            return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf));
        }
    }
    
    // c is '*'
    
    auto starBuf = TheByteBuffer->buffer;
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.to_point() != '^') {
        
        //
        // Something like 1*a
        //
        // Must now do surgery and back up
        //

        auto resetBuf = starBuf;
        
        backup(resetBuf, false);
        
        //
        // Success!
        //
        
        if (real) {
            return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf));
        } else {
            return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf));
        }
    }
    
    //
    // c is '^'
    //
    // So now examine *^ notation
    //
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.isSign()) {
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
    }
    
    if (!c.isDigit()) {
        
        //
        // Something like 123*^-EOF
        //
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        
        return Token(TOKEN_ERROR_EXPECTEDEXPONENT, getTokenBufferAndLength(tokenStartBuf));
    }
    
    size_t count;
    c = handleDigits(policy, c, &count);
    
    if (c.to_point() != '.') {
        
        //
        // Success!
        //
        
        if (real) {
            return Token(TOKEN_REAL, getTokenBufferAndLength(tokenStartBuf));
        } else {
            return Token(TOKEN_INTEGER, getTokenBufferAndLength(tokenStartBuf));
        }
    }
    
    // c is '.'
        
    //
    // Something like 123*^0.5
    //
    // Make this an error, do NOT make this Dot[123*^0, 5]
    //
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    
    return Token(TOKEN_ERROR_EXPECTEDEXPONENT, getTokenBufferAndLength(tokenStartBuf));
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
inline WLCharacter Tokenizer::handlePossibleFractionalPart(Buffer dotBuf, WLCharacter firstChar, int base, NextCharacterPolicy policy, int *handled) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '.');
    
    assert(*dotBuf == '.' || *dotBuf == '\\'/* line continuation */);
    
    assert(((*dotBuf == '.') && (TheByteBuffer->buffer == dotBuf + 1)) || (*dotBuf == '\\'));
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    return handlePossibleFractionalPartPastDot(dotBuf, c, base, policy, handled);
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0
//         UNRECOGNIZED_DIGIT if base error
//         BAILOUT if not a radix point (and also backup before dot)
//
inline WLCharacter Tokenizer::handlePossibleFractionalPartPastDot(Buffer dotBuf, WLCharacter firstChar, int base, NextCharacterPolicy policy, int *handled) {
    
    auto c = firstChar;
    
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
        if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
            
            auto dotLoc = TheByteDecoder->convertBufferToStart(dotBuf);
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(dotLoc), " ")));
            
            auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, Source(dotLoc), 0.25, std::move(Actions)));
            
            Issues.push_back(std::move(I));
        }
#endif // !NISSUES
        
        backup(dotBuf, false);
        
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
        }
    }
    
#if !NISSUES
    if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
        
        if (c.to_point() == '.') {
            
            //
            // Something like 1.2.3
            //
            
            auto dotLoc = TheByteDecoder->convertBufferToStart(dotBuf);
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert *", getTokenSource(dotLoc), "*")));
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMES, "Suspicious syntax.", SYNTAXISSUESEVERITY_ERROR, Source(dotLoc), 0.99, std::move(Actions)));
            
            Issues.push_back(std::move(I));
        }
    }
#endif // !NISSUES

    *handled = 0;
    
    return c;
}
        
void Tokenizer::backup(Buffer resetBuf, bool warn) {
    
#if !NISSUES
    
    if (warn) {

        auto loc = TheByteDecoder->convertBufferToStart(resetBuf);
        
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(loc), " ")));
        
        auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, Source(loc), 0.25, std::move(Actions)));
        
        Issues.push_back(std::move(I));
    }
#endif // !NISSUES
    
    TheByteBuffer->buffer = resetBuf;
}

//
// Precondition: currentWLCharacter is a digit
// Postcondition: buffer is pointing to first NON-DIGIT character after all digits
//
// return: the first NON-DIGIT character after all digits
//
inline WLCharacter Tokenizer::handleDigits(NextCharacterPolicy policy, WLCharacter firstChar, size_t *countP) {
    
    auto c = firstChar;
    
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
inline WLCharacter Tokenizer::handleAlphaOrDigits(WLCharacter firstChar, size_t base, NextCharacterPolicy policy, int *handled) {
    
    auto c = firstChar;
    
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
            
            auto dig = toDigit(c.to_point());
            
            if (base <= dig) {
                
                *handled = UNRECOGNIZED_DIGIT;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
                return c;
            }
            
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        count++;
        
    } // while
    
    *handled = count;
    
    return c;
}

inline Token Tokenizer::handleColon(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == ':');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_COLON; // :
    
    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_COLONCOLON; // ::
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '=': {
            Operator = TOKEN_COLONEQUAL; // :=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '>': {
            Operator = TOKEN_COLONGREATER; // :>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleOpenParen(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
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
        
        return handleComment(tokenStartBuf, c, policy);
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleDot(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //
    
    auto c = firstChar;
    
    assert(c.to_point() == '.');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    if (c.isDigit()) {
        
//        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        
        return handleNumber(tokenStartBuf, firstChar, policy);
    }
    
    auto Operator = TOKEN_DOT; // .
    
    if (c.to_point() == '.') {
        
        Operator = TOKEN_DOTDOT; // ..
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        
        c = TheCharacterDecoder->currentWLCharacter(policy);
        
        if (c.to_point() == '.') {
            
            Operator = TOKEN_DOTDOTDOT; // ...
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleEqual(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '=');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_EQUAL; // =
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_EQUALEQUAL; // ==
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALEQUALEQUAL; // ===
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            }
        }
            break;
        case '.': {
            
            //
            // Could be  =.  or  =..  or  =...  or  =....  or  =.0
            //
            
            auto dotBuf = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.isDigit()) {
                
                //
                // Something like x=.0
                //
                // Must now do surgery and back up
                //
                
                backup(dotBuf, true);
                
            } else if (c.to_point() == '.') {
                
                //
                // =..  is a syntax error
                //
                
                Operator = TOKEN_ERROR_UNHANDLEDDOT;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
            } else {
                
                Operator = TOKEN_EQUALDOT; // =.
            }
            
        }
            break;
        case '!': {
            
            auto bangBuf = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALBANGEQUAL; // =!=
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
            } else {
                
                //
                // Something like x=!y
                //
                // Must now do surgery and back up
                //
                
                backup(bangBuf, true);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleUnder(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '_');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_UNDER; // _
    
    switch (c.to_point()) {
        case '_': {
            
            Operator = TOKEN_UNDERUNDER; // __
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '_') {
                
                Operator = TOKEN_UNDERUNDERUNDER; // ___
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            }
        }
            break;
        case '.': {
            
            auto dotBuf = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '.') {
                
                //
                // Something like  _...
                //
                // Must now do surgery and back up
                //
                
                //
                // Only warn if enabled
                //
                auto shouldWarn = ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING);
                
                backup(dotBuf, shouldWarn);
                
            } else {
                
                Operator = TOKEN_UNDERDOT; // _.
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleLess(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '<');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_LESS; // <
    
    switch (c.to_point()) {
        case '|': {
            
            Operator = TOKEN_LESSBAR; // <|
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '<': {
            
            Operator = TOKEN_LESSLESS; // <<
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '>': {
            
            Operator = TOKEN_LESSGREATER; // <>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '=': {
            
            Operator = TOKEN_LESSEQUAL; // <=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '-': {
            
            auto minusBuf = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_LESSMINUSGREATER; // <->
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
            } else {
                
                //
                // Something like  a<-4
                //
                // Must now do surgery and back up
                //
                
                backup(minusBuf, true);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleGreater(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '>');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_GREATER; // >
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_GREATERGREATER; // >>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_GREATERGREATERGREATER; // >>>
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            }
        }
            break;
        case '=': {
            
            Operator = TOKEN_GREATEREQUAL; // >=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleMinus(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
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
        }
            break;
        case '-': {
            
            Operator = TOKEN_MINUSMINUS; // --
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
#if !NISSUES
            if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
                
                auto afterBuf = TheByteBuffer->buffer;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
                
                if (c.to_point() == '>') {
                    
                    //
                    // Something like a-->0
                    //
                    
                    auto greaterLoc = TheByteDecoder->convertBufferToStart(afterBuf);
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(greaterLoc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``>`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(greaterLoc), 0.25, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else if (c.to_point() == '=') {
                    
                    //
                    // Something like a--=0
                    //
                    
                    auto equalLoc = TheByteDecoder->convertBufferToStart(afterBuf);
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(equalLoc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(equalLoc), 0.25, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
            }
#endif // !NISSUES
        }
            break;
        case '=': {
            
            Operator = TOKEN_MINUSEQUAL; // -=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleBar(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '|');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_BAR; // |
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_BARGREATER; // |>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
#if !NISSUES
            if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
                
                auto afterBuf = TheByteBuffer->buffer;
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
                
                if (c.to_point() == '=') {
                    
                    //
                    // Something like <||>=0
                    //
                    
                    auto equalLoc = TheByteDecoder->convertBufferToStart(afterBuf);
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(equalLoc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``>`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(equalLoc), 0.25, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
            }
#endif // !NISSUES
            
        }
            break;
        case '|': {
            
            Operator = TOKEN_BARBAR; // ||
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleSemi(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == ';');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_SEMI; // ;
    
    if (c.to_point() == ';') {
        
        Operator = TOKEN_SEMISEMI; // ;;
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleBang(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '!');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_BANG; // !
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_BANGEQUAL; // !=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '!': {
            
            Operator = TOKEN_BANGBANG; // !!
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleHash(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
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
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06':
        case '\b':
        case '\x0e': case '\x0f': case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17': case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        case '$':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '`':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': {
            
            auto symbolStartBuf = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            Operator = TOKEN_HASH; // #
            
            TokenizerContext Ctxt = 0;
            
            Ctxt |= TOKENIZER_SLOT;
            
            handleSymbol(symbolStartBuf, c, policy, Ctxt);
        }
            break;
        case '"': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            Operator = TOKEN_HASH; // #
            
            handleString(tokenStartBuf, c, policy);
            
#if !NISSUES
            if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
                
                auto tokenStartLoc = TheByteDecoder->convertBufferToStart(tokenStartBuf);
                
                auto Src = getTokenSource(tokenStartLoc);
                
                auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the ``\"`` character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
                
                Issues.push_back(std::move(I));
            }
#endif // !NISSUES
        }
            break;
        case '#': {
            
            Operator = TOKEN_HASHHASH; // ##
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
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
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
                Operator = TOKEN_HASH; // #
                
                TokenizerContext Ctxt = 0;
                
                Ctxt |= TOKENIZER_SLOT;
                
                handleSymbol(symbolStartBuf, c, policy, Ctxt);
                
            } else {
                
                Operator = TOKEN_HASH; // #
            }
            
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handlePercent(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
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
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
        }
        
    } else if (c.isDigit()) {
        
        size_t count;
        handleDigits(policy, c, &count);
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleAmp(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '&');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_AMP; // &
    
    if (c.to_point() == '&') {
        
        Operator = TOKEN_AMPAMP; // &&
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleSlash(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '/');
    
    auto Operator = TOKEN_SLASH; // /
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    switch (c.to_point()) {
        case '@': {
            
            Operator = TOKEN_SLASHAT; // /@
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case ';': {
            
            Operator = TOKEN_SLASHSEMI; // /;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '.': {
            
            auto nextBuf = TheByteBuffer->buffer;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.isDigit()) {
                
                //
                // Something like t/.3
                //
                // Must now do surgery and back up
                //
                
                backup(nextBuf, true);
                
            } else {
                
                Operator = TOKEN_SLASHDOT; // /.
            }
        }
            break;
        case '/': {
            
            Operator = TOKEN_SLASHSLASH; // //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            switch (c.to_point()) {
                case '.': {
                    
                    Operator = TOKEN_SLASHSLASHDOT; // //.
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                }
                    break;
                case '@': {
                    
                    Operator = TOKEN_SLASHSLASHAT; // //@
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                }
                    break;
            }
        }
            break;
        case ':': {
            
            Operator = TOKEN_SLASHCOLON; // /:
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '=': {
            
            Operator = TOKEN_SLASHEQUAL; // /=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '*': {
            
            Operator = TOKEN_SLASHSTAR; // /*
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleAt(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '@');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_AT; // @
    
    switch (c.to_point()) {
        case '@': {
            
            Operator = TOKEN_ATAT; // @@
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '@') {
                
                Operator = TOKEN_ATATAT; // @@@
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            }
        }
            break;
        case '*': {
            
            Operator = TOKEN_ATSTAR; // @*
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handlePlus(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '+');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_PLUS; // +
    
    switch (c.to_point()) {
        case '+': {
            Operator = TOKEN_PLUSPLUS; // ++
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
#if !NISSUES
            
            if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
                
                c = TheCharacterDecoder->currentWLCharacter(policy);
                
                if (c.to_point() == '=') {
                    
                    //
                    // Something like a++=0
                    //
                    
                    auto buf = TheByteBuffer->buffer;
                    auto loc = TheByteDecoder->convertBufferToStart(buf);
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(loc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``+`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(loc), 0.25, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
            }
#endif // !NISSUES
        }
            break;
        case '=': {
            Operator = TOKEN_PLUSEQUAL; // +=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleTilde(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '~');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_TILDE; // ~
    
    if (c.to_point() == '~') {
        
        Operator = TOKEN_TILDETILDE; // ~~
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleQuestion(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '?');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_QUESTION; // ?
    
    if (c.to_point() == '?') {
        
        Operator = TOKEN_QUESTIONQUESTION; // ??
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleStar(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '*');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_STAR; // *
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_STAREQUAL; // *=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
        case '*': {
            
            Operator = TOKEN_STARSTAR; // **
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleCaret(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.to_point() == '^');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    auto Operator = TOKEN_CARET;
    
    switch (c.to_point()) {
        case ':': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_CARETCOLONEQUAL; // ^:=
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                
            } else {
                
                //
                // Has to be ^:=
                //
                
                Operator = TOKEN_ERROR_EXPECTEDEQUAL;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            }
        }
            break;
        case '=': {
            Operator = TOKEN_CARETEQUAL; // ^=
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleUnhandledBackSlash(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    //
    // Unhandled \
    //
    // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
    // This duplicates some logic in CharacterDecoder
    //
    
    auto c = firstChar;
    
    assert(c.to_point() == '\\');
    
    c = TheCharacterDecoder->currentWLCharacter(policy);
    
    switch (c.to_point()) {
        case '[': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            while (true) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isAlphaOrDigit()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
        case ':': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 4; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
        case '.': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 2; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 3; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isOctal()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
        case '|': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            c = TheCharacterDecoder->currentWLCharacter(policy);
            
            for (auto i = 0; i < 6; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    c = TheCharacterDecoder->currentWLCharacter(policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
        case CODEPOINT_ENDOFFILE: {
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
        default: {
            
            //
            // Nothing special, just read next character
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf));
        }
    }
}

inline Token Tokenizer::handleMBStrangeNewline(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    assert(firstChar.isMBStrangeNewline());
    
#if !NISSUES
    if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
        
        auto tokenStartLoc = TheByteDecoder->convertBufferToStart(tokenStartBuf);
        
        auto Src = getTokenSource(tokenStartLoc);
        
        auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        Issues.push_back(std::move(I));
    }
#endif // !NISSUES
    
    return Token(TOKEN_NEWLINE, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleMBStrangeSpace(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    assert(firstChar.isMBStrangeSpace());
    
#if !NISSUES
    if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
        
        auto tokenStartLoc = TheByteDecoder->convertBufferToStart(tokenStartBuf);
        
        auto Src = getTokenSource(tokenStartLoc);
        
        auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        Issues.push_back(std::move(I));
    }
#endif // !NISSUES
    
    return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleMBPunctuation(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
    assert(c.isMBPunctuation());
    
    auto Operator = LongNameCodePointToOperator(c.to_point());
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}

inline Token Tokenizer::handleMBLinearSyntax(Buffer tokenStartBuf, WLCharacter firstChar, NextCharacterPolicy policy) {
    
    auto c = firstChar;
    
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
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf));
}



Source Tokenizer::getTokenSource(SourceLocation tokStartLoc) const {
    auto buf = TheByteBuffer->buffer;
    auto loc = TheByteDecoder->convertBufferToEnd(buf);
    return Source(tokStartLoc, loc);
}

BufferAndLength Tokenizer::getTokenBufferAndLength(Buffer tokStartBuf) const {
    auto buf = TheByteBuffer->buffer;
    auto error = TheByteDecoder->getError();
    return BufferAndLength(tokStartBuf, buf - tokStartBuf, error);
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


//
// Convert val to the digit that it represents
//
size_t toDigit(int val) {
    switch (val) {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        case 'a': case 'A': return 10;
        case 'b': case 'B': return 11;
        case 'c': case 'C': return 12;
        case 'd': case 'D': return 13;
        case 'e': case 'E': return 14;
        case 'f': case 'F': return 15;
        case 'g': case 'G': return 16;
        case 'h': case 'H': return 17;
        case 'i': case 'I': return 18;
        case 'j': case 'J': return 19;
        case 'k': case 'K': return 20;
        case 'l': case 'L': return 21;
        case 'm': case 'M': return 22;
        case 'n': case 'N': return 23;
        case 'o': case 'O': return 24;
        case 'p': case 'P': return 25;
        case 'q': case 'Q': return 26;
        case 'r': case 'R': return 27;
        case 's': case 'S': return 28;
        case 't': case 'T': return 29;
        case 'u': case 'U': return 30;
        case 'v': case 'V': return 31;
        case 'w': case 'W': return 32;
        case 'x': case 'X': return 33;
        case 'y': case 'Y': return 34;
        case 'z': case 'Z': return 35;
        default:
            assert(false);
            return 99;
    }
}
