
#include "Tokenizer.h"

#include "CharacterDecoder.h" // for TheCharacterDecoder
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for strangeLetterlikeWarning
#include "MyString.h"


Tokenizer::Tokenizer() : EmbeddedNewlines(), EmbeddedTabs() {}

void Tokenizer::init() {
    
    EmbeddedNewlines.clear();
    EmbeddedTabs.clear();
}

void Tokenizer::deinit() {
    
    EmbeddedNewlines.clear();
    EmbeddedTabs.clear();
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
Token Tokenizer::nextToken0(NextPolicy policy) {
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
    
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
            return handleSymbol(tokenStartBuf, tokenStartLoc, c, policy);
        case CODEPOINT_BEL: case CODEPOINT_DEL:
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '\t':
            return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case '\n': case '\r':
            
            //
            // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
            //
            return Token(TOKEN_INTERNALNEWLINE.t() | (policy & RETURN_TOPLEVELNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
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
            return handleUnhandledBackslash(tokenStartBuf, tokenStartLoc, c, policy);
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
                
            } else if (c.to_point() == CODEPOINT_LINEARSYNTAX_BANG) {
                
                return Token(TOKEN_LINEARSYNTAX_BANG, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.to_point() == CODEPOINT_LINEARSYNTAX_OPENPAREN) {
                
                return handleMBLinearSyntaxBlob(tokenStartBuf, tokenStartLoc, c, policy);
                
            } else if (c.isMBLinearSyntax()) {
                
                return handleNakedMBLinearSyntax(tokenStartBuf, tokenStartLoc, c, policy);
                
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
                // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
                //
                return Token(TOKEN_INTERNALNEWLINE.t() | (policy & RETURN_TOPLEVELNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.isMBPunctuation()) {
                
                return handleMBPunctuation(tokenStartBuf, tokenStartLoc, c, policy);
                
            } else if (c.isMBStringMeta()) {
                
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else if (c.to_point() == CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE || c.to_point() == CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE || c.to_point() == CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE) {
                
                //
                // This will be disposed before the user sees it
                //
                
                return Token(TOKEN_ERROR_UNSAFECHARACTERENCODING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            } else {
                
                //
                // if nothing else, then it is letterlike
                //
                
                assert(c.isMBLetterlike());
                
                return handleSymbol(tokenStartBuf, tokenStartLoc, c, policy);
            }
        }
    }
}


Token Tokenizer::nextToken0_stringifyAsTag() {
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_AS_TAG;
    
    auto c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case CODEPOINT_ENDOFFILE:
            //
            // EndOfFile is special, so invent source
            //
            
            return Token(TOKEN_ERROR_EXPECTEDTAG, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        case '\n': case '\r': case CODEPOINT_CRLF:
            //
            // Newline is special, so invent source
            //
            
            return Token(TOKEN_ERROR_EXPECTEDTAG, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        default:
            return handleString_stringifyAsTag(tokenStartBuf, tokenStartLoc, c, policy);
    }
}

//
// Use SourceCharacters here, not WLCharacters
//
Token Tokenizer::nextToken0_stringifyAsFile() {
    
    auto tokenStartBuf = TheByteBuffer->buffer;
    auto tokenStartLoc = TheByteDecoder->SrcLoc;
    
    auto policy = INSIDE_STRINGIFY_AS_FILE;
    
    auto c = TheByteDecoder->nextSourceCharacter0(policy);
    
    switch (c.to_point()) {
        case CODEPOINT_ENDOFFILE:
            return Token(TOKEN_ERROR_EXPECTEDFILE, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
        case '\n': case '\r': case CODEPOINT_CRLF:
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
            return Token(TOKEN_INTERNALNEWLINE.t() | (policy & RETURN_TOPLEVELNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case ' ': case '\t':
            //
            // There could be space, something like  << abc
            //
            // or something like:
            // a >>
            //   b
            //
            return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        default:
            return handleString_stringifyAsFile(tokenStartBuf, tokenStartLoc, c, policy);
    }
}


void Tokenizer::nextToken(Token Tok) {
    
    TheByteBuffer->buffer = Tok.BufLen.end;
    TheByteBuffer->wasEOF = (Tok.Tok == TOKEN_ENDOFFILE);
    
    TheByteDecoder->SrcLoc = Tok.Src.End;
}


Token Tokenizer::currentToken(NextPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0(policy);
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    return Tok;
}


Token Tokenizer::currentToken_stringifyAsTag() {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0_stringifyAsTag();
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    return Tok;
}

Token Tokenizer::currentToken_stringifyAsFile() {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto Tok = nextToken0_stringifyAsFile();
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    return Tok;
}

inline Token Tokenizer::handleStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isStrangeWhitespace());
    
#if !NISSUES
    {
        auto Src = getTokenSource(tokenStartLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDSPACECHARACTER, "Unexpected space character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

//
// Use SourceCharacters here, not WLCharacters
//
// Comments deal with (**) SourceCharacters
// Escaped characters do not work
//
inline Token Tokenizer::handleComment(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy) {
    
    //
    // comment is already started
    //
    
    assert(c.to_point() == '*');
    
    policy |= STRING_OR_COMMENT;
    
    auto depth = 1;
    
    c = TheByteDecoder->currentSourceCharacter(policy);
    
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
            case '\n': case '\r': case CODEPOINT_CRLF:
                
                EmbeddedNewlines.insert(tokenStartLoc);
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
                
                break;
            
            case '\t':
                
                EmbeddedTabs.insert(tokenStartLoc);
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
                
                break;
                
            default:
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                c = TheByteDecoder->currentSourceCharacter(policy);
                
                break;
        }
        
    } // while
}

inline Token Tokenizer::handleMBLinearSyntaxBlob(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter cIn, NextPolicy policy) {
    
    assert(cIn.to_point() == CODEPOINT_LINEARSYNTAX_OPENPAREN);
    
    auto depth = 1;
    
    auto c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        switch (c.to_point()) {
            case CODEPOINT_LINEARSYNTAX_OPENPAREN:
                
                depth = depth + 1;
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                break;
            case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    return Token(TOKEN_LINEARSYNTAXBLOB, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                break;
            case CODEPOINT_ENDOFFILE:
                return Token(TOKEN_ERROR_UNTERMINATEDLINEARSYNTAXBLOB, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            default:
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                break;
        }
        
    } // while
    
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
inline Token Tokenizer::handleSymbol(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '`' || c.isLetterlike() || c.isMBLetterlike());
    
    if (c.isLetterlike() || c.isMBLetterlike()) {
        
        c = handleSymbolSegment(tokenStartBuf, tokenStartLoc, tokenStartBuf, tokenStartLoc, c, policy);
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
        if ((policy & SLOT_BEHAVIOR_FOR_STRINGS) == SLOT_BEHAVIOR_FOR_STRINGS) {
            
            //
            // Something like  #`a
            //
            // It's hard to keep track of the ` characters, so just report the entire symbol. Oh well
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the **`** character.", STRING_WARNING, getTokenSource(tokenStartLoc), 0.33, {}, {}));
            
            TheParserSession->addIssue(std::move(I));
        }
#endif // !NISSUES
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        if (c.isLetterlike() || c.isMBLetterlike()) {
            
            auto letterlikeBuf = TheByteBuffer->buffer;
            auto letterlikeLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = handleSymbolSegment(tokenStartBuf, tokenStartLoc, letterlikeBuf, letterlikeLoc, c, policy);
            
        } else {
            
            //
            // Something like  a`1
            //
            
            return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        
    } // while
    
    if ((policy & SLOT_BEHAVIOR_FOR_STRINGS) == SLOT_BEHAVIOR_FOR_STRINGS) {
        return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    } else {
        return Token(TOKEN_SYMBOL, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
}


inline WLCharacter Tokenizer::handleSymbolSegment(Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer charBuf, SourceLocation charLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isLetterlike() || c.isMBLetterlike());
    
#if !NISSUES
    if (c.to_point() == '$') {
        
        if ((policy & SLOT_BEHAVIOR_FOR_STRINGS) == SLOT_BEHAVIOR_FOR_STRINGS) {
            
            //
            // Something like  #$a
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``$`` character.", STRING_WARNING, getTokenSource(charLoc), 0.33, {}, {}));
            
            TheParserSession->addIssue(std::move(I));
        }
    } else if (c.isStrangeLetterlike()) {
        
        auto Src = getTokenSource(charLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
        
    } else if (c.isMBStrangeLetterlike()) {
        
        auto Src = getTokenSource(charLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.80, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    charLoc = TheByteDecoder->SrcLoc;
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isDigit()) {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            charLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
        } else if (c.isLetterlike() || c.isMBLetterlike()) {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            if (c.to_point() == '$') {
                
                if ((policy & SLOT_BEHAVIOR_FOR_STRINGS) == SLOT_BEHAVIOR_FOR_STRINGS) {
                    
                    //
                    // Something like  #$a
                    //
                    
                    auto I = IssuePtr(new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``$`` character.", STRING_WARNING, getTokenSource(charLoc), 0.33, {}, {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
                
            } else if (c.isStrangeLetterlike()) {
                
                auto Src = getTokenSource(charLoc);
                
                CodeActionPtrVector Actions;
                
                for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                    Actions.push_back(std::move(A));
                }
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
                
            } else if (c.isMBStrangeLetterlike()) {
                
                auto Src = getTokenSource(charLoc);
                
                CodeActionPtrVector Actions;
                
                for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                    Actions.push_back(std::move(A));
                }
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDLETTERLIKECHARACTER, "Unexpected letterlike character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.80, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
            
            charLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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
        
    } // while
    
    return c;
}

inline Token Tokenizer::handleString(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '"');
    
#if !NISSUES
    if ((policy & SLOT_BEHAVIOR_FOR_STRINGS) == SLOT_BEHAVIOR_FOR_STRINGS) {
        
        //
        // Something like  #"a"
        //
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNDOCUMENTEDSLOTSYNTAX, "The name following ``#`` is not documented to allow the ``\"`` character.", STRING_WARNING, getTokenSource(tokenStartLoc), 0.33, {}, {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    policy |= STRING_OR_COMMENT;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
        
        switch (c.to_point()) {
            case '"':
                return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            case CODEPOINT_ENDOFFILE:
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            case '\n': case '\r': case CODEPOINT_CRLF:
                
                EmbeddedNewlines.insert(tokenStartLoc);
                
                break;
            case '\t':
                
                EmbeddedTabs.insert(tokenStartLoc);
                
                break;
        }
        
    } // while
}


inline Token Tokenizer::handleString_stringifyAsTag(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
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
        
        auto letterlikeBuf = TheByteBuffer->buffer;
        auto letterlikeLoc = TheByteDecoder->SrcLoc;
        
        handleSymbolSegment(tokenStartBuf, tokenStartLoc, letterlikeBuf, letterlikeLoc, c, policy);
        
        return Token(TOKEN_STRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
        
    //
    // Something like  a::5
    //
    
    return Token(TOKEN_ERROR_EXPECTEDTAG, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
}


const int UNTERMINATED_FILESTRING = -1;

//
// Use SourceCharacters here, not WLCharacters
//
inline Token Tokenizer::handleString_stringifyAsFile(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy) {
    
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
            switch (handled) {
                case UNTERMINATED_FILESTRING:
                    return Token(TOKEN_ERROR_UNTERMINATEDFILESTRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
        }
            break;
        default: {
            //
            // Something like  <<EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            
            return Token(TOKEN_ERROR_EXPECTEDFILE, BufferAndLength(tokenStartBuf), Source(tokenStartLoc));
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
                switch (handled) {
                    case UNTERMINATED_FILESTRING:
                        return Token(TOKEN_ERROR_UNTERMINATEDFILESTRING, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
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


//
// Use SourceCharacters here, not WLCharacters
//
inline SourceCharacter Tokenizer::handleFileOpsBrackets(SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy, int *handled) {
    
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
                
                *handled = UNTERMINATED_FILESTRING;
                
                return c;
            case CODEPOINT_ENDOFFILE:
                
                *handled = UNTERMINATED_FILESTRING;
                
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
                    
                    *handled = UNTERMINATED_FILESTRING;
                    
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
inline Token Tokenizer::handleNumber(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isDigit() || c.to_point() == '.');
    
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
            c = handleZeros(tokenStartBuf, tokenStartLoc, policy, c, &leadingZeroCount);
            
            leadingDigitsCount += leadingZeroCount;
            
            nonZeroStartBuf = TheByteBuffer->buffer;
            nonZeroStartLoc = TheByteDecoder->SrcLoc;
        }
        
        
        //
        // Count the rest of the leading digits
        //
        
        leadingDigitsEndBuf = TheByteBuffer->buffer;
        leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
        
        if (c.isDigit()) {
            
            int count;
            c = handleDigits(tokenStartBuf, tokenStartLoc, policy, c, &count);
            
            leadingDigitsCount += count;
            
            leadingDigitsEndBuf = TheByteBuffer->buffer;
            leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
        }
        
        if ((policy & INTEGER_SHORT_CIRCUIT) == INTEGER_SHORT_CIRCUIT) {
            
#if !NISSUES
            if (c.to_point() == '.') {
                
                //
                // Something like  #2.a
                //
                
                auto dotLoc = TheByteDecoder->SrcLoc;
                
                CodeActionPtrVector Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(dotLoc), " ")));
                
                auto I = IssuePtr(new FormatIssue(STRING_INSERTSPACE, "Suspicious syntax.", STRING_FORMATTING, getTokenSource(dotLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
            
            //
            // Success!
            //
            
            return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        
        switch (c.to_point()) {
                //
                // These are the possible next characters for a number
                //
            case '^': case '*': case '.': case '`':
                
                if (c.to_point() == '^') {
                    
                    caret1Buf = TheByteBuffer->buffer;
                    caret1Loc = TheByteDecoder->SrcLoc;
                    
                    assert(Utils::ifASCIIWLCharacter(*caret1Buf, '^'));
                    
                } else if (c.to_point() == '*') {
                    
                    starBuf = TheByteBuffer->buffer;
                    starLoc = TheByteDecoder->SrcLoc;
                    
                    assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                }
                
                //
                // Preserve c, but advance buffer to next character
                //
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                break;
                
            default:
                //
                // Something else
                //
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        
        if (c.to_point() == '^') {
            
            //
            // Could be 16^^blah
            //
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() != '^') {
                
                //
                // Something like  2^a
                //
                // Must now do surgery and back up
                //
                
                TheByteBuffer->buffer = caret1Buf;
                TheByteDecoder->SrcLoc = caret1Loc;
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
            
            assert(c.to_point() == '^');
            
            //
            // Something like  2^^
            //
            // Must be a number
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
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
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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
                    
                    c = handleAlphaOrDigits(tokenStartBuf, tokenStartLoc, c, Ctxt.Base, policy, &leadingDigitsCount, &Ctxt);
                    switch (leadingDigitsCount) {
                        case BAILOUT:
                            assert(false);
                            break;
                    }
                    
                    leadingDigitsEndBuf = TheByteBuffer->buffer;
                    leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
                    
                    switch (c.to_point()) {
                            //
                            // These are the possible next characters for a number
                            //
                        case '*': case '.': case '`':
                            
                            if (c.to_point() == '*') {
                                
                                starBuf = TheByteBuffer->buffer;
                                starLoc = TheByteDecoder->SrcLoc;
                                
                                assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                            }
                            
                            //
                            // Preserve c, but advance buffer to next character
                            //
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            break;
                        default:
                            //
                            // Something else
                            //
                            
                            //
                            // Success!
                            //
                            
                            return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                }
                    break;
                case '.': {
                    
                    //
                    // Something like  2^^.0
                    //
                    
                    leadingDigitsEndBuf = TheByteBuffer->buffer;
                    leadingDigitsEndLoc = TheByteDecoder->SrcLoc;
                    
                    //
                    // Preserve c, but advance buffer to next character
                    //
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                }
                    break;
                case CODEPOINT_ENDOFFILE: {
                    
                    //
                    // Something like  2^^<EOF>
                    //
                    
                    //
                    // Make sure that bad character is read
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                    
                    // nee TOKEN_ERROR_EXPECTEDDIGIT
                    return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                    break;
                default: {
                    
                    //
                    // Something like  2^^@
                    //
                    
                    //
                    // Make sure that bad character is read
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                    
                    // nee TOKEN_ERROR_UNRECOGNIZEDDIGIT
                    return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
            }
            
        } // if (c.to_point() == '^')
        
    } // if (c.isDigit())
    
    if (c.to_point() == '.') {
        
        assert(Utils::ifASCIIWLCharacter(*(TheByteBuffer->buffer - 1), '.'));
        
        int handled;
        c = handlePossibleFractionalPart(tokenStartBuf, tokenStartLoc, leadingDigitsEndBuf, leadingDigitsEndLoc, c, Ctxt.Base, policy, &handled, &Ctxt);
        switch (handled) {
            case BAILOUT:
                
                if (leadingDigitsCount == 0) {
                    
                    //
                    // Something like  2^^..
                    //
                    
                    // nee TOKEN_ERROR_UNHANDLEDDOT
                    return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
                //
                // Something like  0..
                //
                
                // Success!
                
                return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                
            case 0:
                
                if (leadingDigitsCount == 0) {

                    //
                    // Something like  2^^.
                    //
                    
                    // nee TOKEN_ERROR_UNHANDLEDDOT
                    return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
                //
                // Something like  0.
                //
                
                Ctxt.Real = true;
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '`': case '*':
                        
                        if (c.to_point() == '*') {
                            
                            starBuf = TheByteBuffer->buffer;
                            starLoc = TheByteDecoder->SrcLoc;
                            
                            assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                        }
                        
                        //
                        // Preserve c, but advance buffer to next character
                        //
                        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                        break;
                    default:
                        
                        //
                        // Something like  123.
                        //
                        
                        // Success!
                        
                        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
                break;
            default:
                
                //
                // Something like  123.456
                //
                
                Ctxt.Real = true;
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '`': case '*':
                        
                        if (c.to_point() == '*') {
                            
                            starBuf = TheByteBuffer->buffer;
                            starLoc = TheByteDecoder->SrcLoc;
                            
                            assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                        }
                        
                        //
                        // Preserve c, but advance buffer to next character
                        //
                        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                        break;
                    default:
                        
                        //
                        // Something like  123.456
                        //
                        
                        // Success!
                        
                        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                break;
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
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        bool accuracy = false;
        bool sign = false;
        bool precOrAccSupplied = false;
        
        Buffer signBuf;
        SourceLocation signLoc;
        
        if (c.to_point() == '`') {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            accuracy = true;
        }
        
        switch (c.to_point()) {
            case '-': case '+': {
                
                //
                // Something like  1.2`-
                //
                
                signBuf = TheByteBuffer->buffer;
                signLoc = TheByteDecoder->SrcLoc;
                
                assert(Utils::ifASCIIWLCharacter(*signBuf, '-')  || Utils::ifASCIIWLCharacter(*signBuf, '+'));
                
                //
                // Eat the sign
                //
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                        
                        //
                        // Something like  1.2`-3
                        //
                        
                        sign = true;

#if !NISSUES
                        {
                            if (accuracy) {

                                //
                                // do not warn about 1.2``+3 for now
                                //

                            } else {

                                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDSIGN, "The real number has a ``" + std::string(reinterpret_cast<const char *>(signBuf), 1) + "`` sign in its precision specification.", STRING_WARNING, Source(signLoc), 0.95, {}, {"This is usually unintentional."}));

                                TheParserSession->addIssue(std::move(I));
                            }
                        }
#endif // !NISSUES

                        break;
                    case '.':
                        
                        //
                        // Something like  1.2`-.3
                        //
                        
                        sign = true;

#if !NISSUES
                        {
                            if (accuracy) {

                                //
                                // do not warn about 1.2``+.3 for now
                                //

                            } else {

                                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDSIGN, "The real number has a ``" + std::string(reinterpret_cast<const char *>(signBuf), 1) + "`` sign in its precision specification.", STRING_WARNING, Source(signLoc), 0.95, {}, {"This is usually unintentional."}));

                                TheParserSession->addIssue(std::move(I));
                            }
                        }
#endif // !NISSUES

                        break;
                    default: {
                        
                        //
                        // Something like  1.2`->
                        //
                        
                        if (accuracy) {
                            
                            //
                            // Something like  1.2``->3
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            
                            // nee TOKEN_ERROR_EXPECTEDACCURACY
                            return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        }
                        
                        //
                        // Something like  1.2`->3  or  1`+#
                        //
                        // Must now do surgery and back up
                        //
                        backupAndWarn(signBuf, signLoc);
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                }
                
            } // case '-': case '+'
        }
        
        switch(c.to_point()) {
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                
                int count;
                c = handleDigits(tokenStartBuf, tokenStartLoc, policy, c, &count);
                if (count > 0) {
                    precOrAccSupplied = true;
                }
                
                switch (c.to_point()) {
                        //
                        // These are the possible next characters for a number
                        //
                    case '.':
                        break;
                    case '*':
                        break;
                        
                    default:
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
            }
        }
        
        switch (c.to_point()) {
            case '.': {
                
                auto dotBuf = TheByteBuffer->buffer;
                auto dotLoc = TheByteDecoder->SrcLoc;
                
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
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    auto NextChar = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                    
                    if (!NextChar.isDigit()) {
                        
                        if (accuracy) {
                            
                            //
                            // Something like  123``.EOF
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            
                            // TOKEN_ERROR_EXPECTEDDIGIT
                            return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        }
                        
                        if (NextChar.isSign()) {
                            
                            //
                            // Something like  123`.+4
                            //
                            
                            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                            
                            // nee TOKEN_ERROR_EXPECTEDDIGIT
                            return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
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
                        backupAndWarn(dotBuf, dotLoc);
                        
                        //
                        // Success!
                        //
                        
                        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        
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
                    
                    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                }
                
                //
                // actual decimal point
                //
                
                int handled;
                //
                // The base to use inside of precision/accuracy processing is 0, i.e., implied 10
                //
                size_t baseToUse = 0;
                c = handlePossibleFractionalPartPastDot(tokenStartBuf, tokenStartLoc, dotBuf, dotLoc, c, baseToUse, policy, &handled, &Ctxt);
                switch (handled) {
                    case BAILOUT:
                        
                        if (precOrAccSupplied) {
                            
                            //
                            // Something like  6`5..
                            //
                            
                            // Success!
                            
                            return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        }
                        
                        if (sign) {
                            
                            //
                            // Something like  1`+..
                            //
                            
                            backupAndWarn(signBuf, signLoc);
                            
                            // Success!
                            
                            return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                        }
                        
                        assert(false);
                        break;
                        
                    case 0:
                        break;
                    default:
                        precOrAccSupplied = true;
                        break;
                }
                
                if (!precOrAccSupplied) {
                    
                    //
                    // Something like  1`+.a
                    //
                    
                    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                    
                    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                    
                    // nee TOKEN_ERROR_EXPECTEDDIGIT
                    return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                }
                
            } // case '.'
                
        } // switch (c.to_point())
        
        switch (c.to_point()) {
                
                //
                // These are the possible next characters for a number
                //
            case '*':
                
                if (accuracy) {
                    
                    if (!precOrAccSupplied) {
                     
                        //
                        // Something like  123.45``*^6
                        //
                        
                        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                        
                        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                        
                        // nee TOKEN_ERROR_EXPECTEDACCURACY
                        return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                }
                
                starBuf = TheByteBuffer->buffer;
                starLoc = TheByteDecoder->SrcLoc;
                
                assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
                
                //
                // Preserve c, but advance buffer to next character
                //
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                break;
            default: {
                
                if (accuracy) {
                    
                    if (!precOrAccSupplied) {
                     
                        //
                        // Something like  123``EOF
                        //
                        
                        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                        
                        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                        
                        // nee TOKEN_ERROR_EXPECTEDACCURACY
                        return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
                    }
                }
                
                //
                // Success!
                //
                
                return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
                
        }
        
    } // if (c.to_point() == '`')
    
    assert(c.to_point() == '*');
    
    assert(Utils::ifASCIIWLCharacter(*starBuf, '*'));
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    if (c.to_point() != '^') {
        
        //
        // Something like  1*a
        //
        // Must now do surgery and back up
        //
        
        TheByteBuffer->buffer = starBuf;
        TheByteDecoder->SrcLoc = starLoc;
        
        //
        // Success!
        //
        
        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    assert(c.to_point() == '^');
    
    //
    // c is '^'
    //
    // So now examine *^ notation
    //
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '-':
            Ctxt.NegativeExponent = true;
            //
            // FALL THROUGH
            //
        case '+': {
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        }
            break;
    }
    
    if (!c.isDigit()) {
        
        //
        // Something like  123*^-<EOF>
        //
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        // TOKEN_ERROR_EXPECTEDEXPONENT
        return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    assert(c.isDigit());
    
    //
    // Count leading zeros in exponent
    //
    if (c.to_point() == '0') {
        
        int exponentLeadingZeroCount;
        c = handleZeros(tokenStartBuf, tokenStartLoc, policy, c, &exponentLeadingZeroCount);
    }
    
    if (c.isDigit()) {
        
        c = handleDigits(tokenStartBuf, tokenStartLoc, policy, c, &Ctxt.NonZeroExponentDigitCount);
    }
    
    if (c.to_point() != '.') {
        
        //
        // Success!
        //
        
        return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
    }
    
    assert(c.to_point() == '.');
    
    auto dotBuf = TheByteBuffer->buffer;
    auto dotLoc = TheByteDecoder->SrcLoc;
    
    assert(Utils::ifASCIIWLCharacter(*dotBuf, '.'));
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;

    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    int handled;
    c = handlePossibleFractionalPartPastDot(tokenStartBuf, tokenStartLoc, dotBuf, dotLoc, c, Ctxt.Base, policy, &handled, &Ctxt);
    
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
            
            return Token(Ctxt.computeTok(), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        default: {
            
            //
            // Something like  123*^0.5
            //
            // Make this an error; do NOT make this Dot[123*^0, 5]
            //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            // nee TOKEN_ERROR_EXPECTEDEXPONENT
            return Token(TOKEN_ERROR_NUMBER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
    }
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
        
    } else if (UnrecognizedDigit) {
        
        // nee TOKEN_ERROR_UNRECOGNIZEDDIGIT
        return TOKEN_ERROR_NUMBER;
        
    } else if (Real) {
        
        return TOKEN_REAL;
        
    } else if (NegativeExponent && NonZeroExponentDigitCount != 0) {
        
        //
        // Something like  1*^-2..
        //
        
        return TOKEN_RATIONAL;
        
    } else {
        
        return TOKEN_INTEGER;
    }
}


inline WLCharacter Tokenizer::handlePossibleFractionalPart(Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter c, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt) {
    
    assert(c.to_point() == '.');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    return handlePossibleFractionalPartPastDot(tokenStartBuf, tokenStartLoc, dotBuf, dotLoc, c, base, policy, handled, Ctxt);
}


inline WLCharacter Tokenizer::handlePossibleFractionalPartPastDot(Buffer tokenStartBuf, SourceLocation tokenStartLoc, Buffer dotBuf, SourceLocation dotLoc, WLCharacter c, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt) {
    
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
        
        backupAndWarn(dotBuf, dotLoc);
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        *handled = BAILOUT;

        return c;
    }
    
    if (c.isAlphaOrDigit()) {
        
        c = handleAlphaOrDigits(tokenStartBuf, tokenStartLoc, c, base, policy, handled, Ctxt);
        switch (*handled) {
            case BAILOUT:
                assert(false);
                break;
            case 0:
                return c;
            default:
                
#if !NISSUES
                if (c.to_point() == '.') {
                    
                    //
                    // Something like  1.2.3
                    //
                    
                    CodeActionPtrVector Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert ``*``", Source(dotLoc), "*")));
                    
                    auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Suspicious syntax.", STRING_ERROR, Source(dotLoc), 0.99, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
#endif // !NISSUES
                
                return c;
        }
    }

    *handled = 0;
    
    return c;
}
        
void Tokenizer::backupAndWarn(Buffer resetBuf, SourceLocation resetLoc) {
    
#if !NISSUES
    {
        CodeActionPtrVector Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(resetLoc), " ")));
        
        auto I = IssuePtr(new FormatIssue(STRING_INSERTSPACE, "Suspicious syntax.", STRING_FORMATTING, Source(resetLoc), 1.0, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    TheByteBuffer->buffer = resetBuf;
    TheByteDecoder->SrcLoc = resetLoc;
}


inline WLCharacter Tokenizer::handleZeros(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter c, int *countP) {
    
    assert(c.to_point() == '0');
    
    auto count = 1;
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.to_point() != '0') {
            
            break;
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        count++;
        
    } // while
    
    *countP = count;
    
    return c;
}


inline WLCharacter Tokenizer::handleDigits(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy, WLCharacter c, int *countP) {
    
    assert(c.isDigit());
    
    auto count = 1;
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (!c.isDigit()) {
            
            break;
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        count++;
        
    } // while
    
    *countP = count;

    return c;
}


inline WLCharacter Tokenizer::handleAlphaOrDigits(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, size_t base, NextPolicy policy, int *handled, NumberTokenizationContext *Ctxt) {
    
    assert(c.isAlphaOrDigit());
    
    auto count = 0;
    
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
                Ctxt->UnrecognizedDigit = true;
            }
            
        }
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        count++;
        
    } // while
    
    *handled = count;
    
    return c;
}

inline Token Tokenizer::handleColon(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == ':');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_COLON; // :
    
    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_COLONCOLON; // ::
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '[') {
                
                Operator = TOKEN_COLONCOLONOPENSQUARE; // ::[
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
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

inline Token Tokenizer::handleOpenParen(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '(');
    
    auto ParenChar = c;
    
    auto Operator = TOKEN_OPENPAREN; // (
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
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

inline Token Tokenizer::handleDot(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter firstChar, NextPolicy policy) {
    
    auto c = firstChar;
    
    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //
    
    assert(c.to_point() == '.');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    if (c.isDigit()) {
        
        return handleNumber(tokenStartBuf, tokenStartLoc, firstChar, policy);
    }
    
    auto Operator = TOKEN_DOT; // .
    
    if (c.to_point() == '.') {
        
        Operator = TOKEN_DOTDOT; // ..
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        if (c.to_point() == '.') {
            
            Operator = TOKEN_DOTDOTDOT; // ...
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
        }
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleEqual(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '=');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_EQUAL; // =
    
    switch (c.to_point()) {
        case '=': {
            
            Operator = TOKEN_EQUALEQUAL; // ==
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALEQUALEQUAL; // ===
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
        }
            break;
        case '!': {
            
            auto bangBuf = TheByteBuffer->buffer;
            auto bangLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALBANGEQUAL; // =!=
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
            } else {
                
                //
                // Something like  x=!y
                //
                // Must now do surgery and back up
                //
                
                backupAndWarn(bangBuf, bangLoc);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleUnder(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '_');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_UNDER; // _
    
    switch (c.to_point()) {
        case '_': {
            
            Operator = TOKEN_UNDERUNDER; // __
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '_') {
                
                Operator = TOKEN_UNDERUNDERUNDER; // ___
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            }
            break;
        }
        case '.': {
            
            Operator = TOKEN_UNDERDOT; // _.
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            {
                auto afterLoc = TheByteDecoder->SrcLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
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
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(dotLoc), " ")));
                    
                    auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDDOT, "Suspicious syntax.", STRING_ERROR, Source(dotLoc), 0.95, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
            }
#endif // !NISSUES
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleLess(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '<');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
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
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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
                
                backupAndWarn(minusBuf, minusLoc);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleGreater(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '>');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_GREATER; // >
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_GREATERGREATER; // >>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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

inline Token Tokenizer::handleMinus(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '-');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
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
            {
                auto afterLoc = TheByteDecoder->SrcLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '>') {
                    
                    //
                    // Something like  a-->0
                    //
                    
                    auto greaterLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(greaterLoc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(STRING_INSERTSPACE, "Put a space between ``-`` and ``>`` to reduce ambiguity", STRING_FORMATTING, Source(greaterLoc), 1.0, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                    
                } else if (c.to_point() == '=') {
                    
                    //
                    // Something like  a--=0
                    //
                    
                    auto equalLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(equalLoc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(STRING_INSERTSPACE, "Put a space between ``-`` and ``=`` to reduce ambiguity", STRING_FORMATTING, Source(equalLoc), 1.0, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
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

inline Token Tokenizer::handleBar(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '|');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_BAR; // |
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_BARGREATER; // |>
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            {
                auto afterLoc = TheByteDecoder->SrcLoc;
                
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '=') {
                    
                    //
                    // Something like  <||>=0
                    //
                    
                    auto equalLoc = afterLoc;
                    
                    CodeActionPtrVector Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(equalLoc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(STRING_INSERTSPACE, "Put a space between ``>`` and ``=`` to reduce ambiguity", STRING_FORMATTING, Source(equalLoc), 1.0, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
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
            
        case '-': {
            
            auto barBuf = TheByteBuffer->buffer;
            auto barLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_BARMINUSGREATER; // |->
                
                TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
                
            } else {
                
                //
                // Something like  x|-y
                //
                // Must now do surgery and back up
                //
                
                backupAndWarn(barBuf, barLoc);
            }
        }
            break;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleSemi(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == ';');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_SEMI; // ;
    
    if (c.to_point() == ';') {
        
        Operator = TOKEN_SEMISEMI; // ;;
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleBang(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '!');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
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

inline Token Tokenizer::handleHash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '#');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_HASH; // #
    
    if (c.to_point() == '#') {
        
        Operator = TOKEN_HASHHASH; // ##
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handlePercent(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '%');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_PERCENT; // %
    
    if (c.to_point() == '%') {
        
        c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
        
        Operator = TOKEN_PERCENTPERCENT; // %%
        
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            if (c.to_point() != '%') {
                
                break;
            }
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
        } // while
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleAmp(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '&');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_AMP; // &
    
    if (c.to_point() == '&') {
        
        Operator = TOKEN_AMPAMP; // &&
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleSlash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '/');
    
    auto Operator = TOKEN_SLASH; // /
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
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
            
            auto dotBuf = TheByteBuffer->buffer;
            auto dotLoc = TheByteDecoder->SrcLoc;
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
            if (c.isDigit()) {
                
                //
                // Something like  t/.3
                //
                // Must now do surgery and back up
                //
                
                backupAndWarn(dotBuf, dotLoc);
                
            } else {
                
                Operator = TOKEN_SLASHDOT; // /.
            }
        }
            break;
        case '/': {
            
            Operator = TOKEN_SLASHSLASH; // //
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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
                case '=': {
                    
                    Operator = TOKEN_SLASHSLASHEQUAL; // //=
                    
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

inline Token Tokenizer::handleAt(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '@');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_AT; // @
    
    switch (c.to_point()) {
        case '@': {
            
            Operator = TOKEN_ATAT; // @@
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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

inline Token Tokenizer::handlePlus(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '+');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_PLUS; // +
    
    switch (c.to_point()) {
        case '+': {
            Operator = TOKEN_PLUSPLUS; // ++
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
#if !NISSUES
            {
                c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
                
                if (c.to_point() == '=') {
                    
                    //
                    // Something like  a++=0
                    //
                    
                    auto loc = TheByteDecoder->SrcLoc;
                    
                    CodeActionPtrVector Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(loc), " ")));
                    
                    auto I = IssuePtr(new FormatIssue(STRING_INSERTSPACE, "Put a space between ``+`` and ``=`` to reduce ambiguity", STRING_FORMATTING, Source(loc), 1.0, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
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

inline Token Tokenizer::handleTilde(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '~');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_TILDE; // ~
    
    if (c.to_point() == '~') {
        
        Operator = TOKEN_TILDETILDE; // ~~
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleQuestion(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '?');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_QUESTION; // ?
    
    if (c.to_point() == '?') {
        
        Operator = TOKEN_QUESTIONQUESTION; // ??
        
        TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    }
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleStar(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '*');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
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

inline Token Tokenizer::handleCaret(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '^');
    
    c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
    
    auto Operator = TOKEN_CARET;
    
    switch (c.to_point()) {
        case ':': {
            
            TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
            
            c = TheCharacterDecoder->currentWLCharacter(tokenStartBuf, tokenStartLoc, policy);
            
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

inline Token Tokenizer::handleUnhandledBackslash(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    //
    // Unhandled \
    //
    // Something like  \A  or  \{  or  \<EOF>
    //
    // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
    // This duplicates some logic in CharacterDecoder
    //
    
    assert(c.to_point() == '\\');
    
    c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
    
    switch (c.to_point()) {
        case '[': {
            
            //
            // Try to reconstruct \[XXX]
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
            
            auto wellFormed = false;
            
            if (c.isUpper()) {
                
                resetBuf = TheByteBuffer->buffer;
                resetLoc = TheByteDecoder->SrcLoc;
                
                c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
                
                while (true) {
                    
                    //
                    // No need to check isAbort() inside tokenizer loops
                    //
                    
                    if (c.isAlphaOrDigit()) {
                        
                        resetBuf = TheByteBuffer->buffer;
                        resetLoc = TheByteDecoder->SrcLoc;
                        
                        c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
                        
                    } else if (c.to_point() == ']') {
                        
                        wellFormed = true;
                        
                        break;
                        
                    } else {
                        
                        TheByteBuffer->buffer = resetBuf;
                        TheByteDecoder->SrcLoc = resetLoc;
                        
                        break;
                    }
                }
            }
            
            if (wellFormed) {
                //
                // More specifically: Unrecognized
                //
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            } else {
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
            }
        }
        case ':': {
            
            //
            // Try to reconstruct \:XXXX
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 4; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    resetBuf = TheByteBuffer->buffer;
                    resetLoc = TheByteDecoder->SrcLoc;
                    
                    c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteDecoder->SrcLoc = resetLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case '.': {
            
            //
            // Try to reconstruct \.XX
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 2; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    resetBuf = TheByteBuffer->buffer;
                    resetLoc = TheByteDecoder->SrcLoc;
                    
                    c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteDecoder->SrcLoc = resetLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            //
            // Try to reconstruct \XXX
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 3; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isOctal()) {
                    
                    resetBuf = TheByteBuffer->buffer;
                    resetLoc = TheByteDecoder->SrcLoc;
                    
                    c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteDecoder->SrcLoc = resetLoc;
                    
                    break;
                }
            }
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
        case '|': {
            
            //
            // Try to reconstruct \|XXXXXX
            //
            
            auto resetBuf = TheByteBuffer->buffer;
            auto resetLoc = TheByteDecoder->SrcLoc;
            
            c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
            
            for (auto i = 0; i < 6; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    resetBuf = TheByteBuffer->buffer;
                    resetLoc = TheByteDecoder->SrcLoc;
                    
                    c = TheCharacterDecoder->nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
                    
                } else {
                    
                    TheByteBuffer->buffer = resetBuf;
                    TheByteDecoder->SrcLoc = resetLoc;
                    
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
            // Nothing special, just read next single character
            //
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        }
    }
}

inline Token Tokenizer::handleMBStrangeNewline(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBStrangeNewline());
    
#if !NISSUES
    {
        auto Src = getTokenSource(tokenStartLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDNEWLINECHARACTER, "Unexpected newline character: ``" + c.graphicalString() + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return Token(TOKEN_INTERNALNEWLINE.t() | (policy & RETURN_TOPLEVELNEWLINE), getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleMBStrangeWhitespace(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBStrangeWhitespace());
    
#if !NISSUES
    {
        auto Src = getTokenSource(tokenStartLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDSPACECHARACTER, "Unexpected space character: ``" + c.safeAndGraphicalString() + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    return Token(TOKEN_WHITESPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleMBPunctuation(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBPunctuation());
    
    auto Operator = LongNameCodePointToOperator(c.to_point());
    
    return Token(Operator, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
}

inline Token Tokenizer::handleNakedMBLinearSyntax(Buffer tokenStartBuf, SourceLocation tokenStartLoc, WLCharacter c, NextPolicy policy) {
    
    assert(c.isMBLinearSyntax());
    
    switch (c.to_point()) {
        case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
            return Token(TOKEN_LINEARSYNTAX_CLOSEPAREN, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_AT:
            return Token(TOKEN_LINEARSYNTAX_AT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_PERCENT:
            return Token(TOKEN_LINEARSYNTAX_PERCENT, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_CARET:
            return Token(TOKEN_LINEARSYNTAX_CARET, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_AMP:
            return Token(TOKEN_LINEARSYNTAX_AMP, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_STAR:
            return Token(TOKEN_LINEARSYNTAX_STAR, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_UNDER:
            return Token(TOKEN_LINEARSYNTAX_UNDER, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_PLUS:
            return Token(TOKEN_LINEARSYNTAX_PLUS, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_SLASH:
            return Token(TOKEN_LINEARSYNTAX_SLASH, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_BACKTICK:
            return Token(TOKEN_LINEARSYNTAX_BACKTICK, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        case CODEPOINT_LINEARSYNTAX_SPACE:
            return Token(TOKEN_LINEARSYNTAX_SPACE, getTokenBufferAndLength(tokenStartBuf), getTokenSource(tokenStartLoc));
        default:
            assert(false);
            return Token();
    }
    
}

Source Tokenizer::getTokenSource(SourceLocation tokStartLoc) const {
    auto loc = TheByteDecoder->SrcLoc;
    return Source(tokStartLoc, loc);
}

BufferAndLength Tokenizer::getTokenBufferAndLength(Buffer tokStartBuf) const {
    auto buf = TheByteBuffer->buffer;
    return BufferAndLength(tokStartBuf, buf - tokStartBuf);
}

std::set<SourceLocation>& Tokenizer::getEmbeddedNewlines() {
    return EmbeddedNewlines;
}

std::set<SourceLocation>& Tokenizer::getEmbeddedTabs() {
    return EmbeddedTabs;
}


TokenizerPtr TheTokenizer = nullptr;
