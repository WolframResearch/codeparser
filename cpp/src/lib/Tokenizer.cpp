
#include "Tokenizer.h"

#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "Utils.h"
#include "CodePoint.h"

int toDigit(int val);


void sbuffer::clear() {
    Str.str("");
}

std::string sbuffer::str() {
    return Str.str();
}

void sbuffer::operator<<(WLCharacter c) {
    Str << c;
}

Source Tokenizer::getTokenSource() const {
    return Source(TokenStartLoc, TheCharacterDecoder->getPrevWLCharacterEndLoc());
}



Tokenizer::Tokenizer() : _currentToken(Token(TOKEN_UNKNOWN, "", Source())), _currentWLCharacter(0), wlCharacterQueue(), String(), Issues(), TokenStartLoc() {}

void Tokenizer::init(SourceStyle style, bool stringifyNextTokenSymbol, bool stringifyNextTokenFile) {
    
    _currentToken = Token(TOKEN_UNKNOWN, "", Source(style));
    
    _currentWLCharacter = WLCharacter(0);
    wlCharacterQueue.clear();
    
    String.clear();
    
    Issues.clear();
    
    _currentWLCharacter = TheCharacterDecoder->nextWLCharacter();
    
    TokenStartLoc = SourceLocation(style);
    
    if (stringifyNextTokenSymbol) {
        nextToken_stringifyNextToken_symbol();
    } else if (stringifyNextTokenFile) {
        nextToken_stringifyNextToken_file();
    } else {
        nextToken();
    }
}

void Tokenizer::deinit() {
    
    wlCharacterQueue.clear();
    
    String.clear();
    
    Issues.clear();
}


void Tokenizer::nextToken() {
    
    assert((_currentToken.Tok() != TOKEN_ENDOFFILE) && "Must handle at call site");
    
    //
    // Too complicated to clear string when calling getString and assert here
    //
    // assert(String.str().empty());
    //
    String.clear();
    
    TokenizerContext Ctxt;
    
    TokenStartLoc = TheCharacterDecoder->getWLCharacterStartLoc();
    
    auto c = currentWLCharacter();
    
    switch (c.to_point()) {
        
        //
        // all ASCII characters
        //
        
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06':
        case '\b':
        case '\x0e': case '\x0f': case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17': case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        case '$':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '`':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            handleSymbol(Ctxt); break;
        case '\x07':
        case '\x7f':
            handleUninterpretable(); break;
        case '\t':
            handleTab(); break;
        case '\n':
            handleLineFeed(); break;
        case '\v': case '\f':
            handleStrangeSpace(); break;
        case '\r':
            handleCarriageReturn(); break;
        case ' ':
            handleSpace(); break;
        case '!':
            handleBang(); break;
        case '\"':
            handleString(); break;
        case '#':
            handleHash(); break;
        case '%':
            handlePercent(); break;
        case '&':
            handleAmp(); break;
        case '\'':
            handleSingleQuote(); break;
        case '(':
            handleOpenParen(); break;
        case ')':
            handleCloseParen(); break;
        case '*':
            handleStar(); break;
        case '+':
            handlePlus(); break;
        case ',':
            handleComma(); break;
        case '-':
            handleMinus(); break;
        case '.':
            handleDot(); break;
        case '/':
            handleSlash(); break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            handleNumber(); break;
        case ':':
            handleColon(); break;
        case ';':
            handleSemi(); break;
        case '<':
            handleLess(); break;
        case '=':
            handleEqual(); break;
        case '>':
            handleGreater(); break;
        case '?':
            handleQuestion(); break;
        case '@':
            handleAt(); break;
        case '[':
            handleOpenSquare(); break;
        case '\\':
            handleUnhandledBackSlash(); break;
        case ']':
            handleCloseSquare(); break;
        case '^':
            handleCaret(); break;
        case '_':
            handleUnder(); break;
        case '{':
            handleOpenCurly(); break;
        case '|':
            handleBar(); break;
        case '}':
            handleCloseCurly(); break;
        case '~':
            handleTilde(); break;
        default: {
            
            //
            // Everything else involving non-ASCII
            //
            
            if (c.to_point() == CODEPOINT_ENDOFFILE) {
                
                handleEndOfFile();
                
            } else if (c.isLineContinuation()) {
                
                handleLineContinuation();
                
            } else if (c.isLetterlikeCharacter()) {
                
                handleSymbol(Ctxt);
                
            } else if (c.isSpaceCharacter()) {
                
                handleSpaceCharacter();
                
            } else if (c.isNewlineCharacter()) {
                
                handleNewlineCharacter();
                
            } else if (c.isLinearSyntax()) {
                
                handleLinearSyntax();
                
            } else if (c.isPunctuationCharacter()) {
                
                handlePunctuationCharacter();
                
            } else {
                handleUninterpretable();
            }
        }
    }
}

void Tokenizer::nextToken_stringifyCurrentLine() {
    
    assert((_currentToken.Tok() != TOKEN_ENDOFFILE) && "Must handle at call site");
    
    String.clear();
    
    TokenStartLoc = TheCharacterDecoder->getWLCharacterStartLoc();
    
    auto c = currentWLCharacter();
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        //
        // EndOfFile is special, so invent source
        //
        
        auto Start = TokenStartLoc;
        
        _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        
        return;
        
    } else if (c.isNewline()) {
        
        //
        // Newline is special, so invent source
        //
        
        auto Start = TokenStartLoc;
        
        _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        
        return;
    }
    
    handleString_stringifyCurrentLine();
}

void Tokenizer::nextToken_stringifyNextToken_symbol() {
    
    assert((_currentToken.Tok() != TOKEN_ENDOFFILE) && "Must handle at call site");
    
    //
    // Too complicated to clear string when calling getString and assert here
    //
    // assert(String.str().empty());
    //
    String.clear();
    
    TokenStartLoc = TheCharacterDecoder->getWLCharacterStartLoc();
    
    auto c = currentWLCharacter();
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        //
        // EndOfFile is special, so invent source
        //
        
        auto Start = TokenStartLoc;
        
        _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        
        return;
        
    } else if (c.isNewline()) {
        
        //
        // Newline is special, so invent source
        //
        
        auto Start = TokenStartLoc;
        
        _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        
        return;
    }
    
    handleString_stringifyNextToken_symbol();
}

void Tokenizer::nextToken_stringifyNextToken_file() {
    
    assert((_currentToken.Tok() != TOKEN_ENDOFFILE) && "Must handle at call site");
    
    //
    // Too complicated to clear string when calling getString and assert here
    //
    // assert(String.str().empty());
    //
    String.clear();
    
    TokenStartLoc = TheCharacterDecoder->getWLCharacterStartLoc();
    
    auto c = currentWLCharacter();
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        //
        // EndOfFile is special, so invent source
        //
        
        auto Start = TokenStartLoc;
        
        _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        
        return;
        
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
        //        else if (stringifyNextToken_file) {
        //
        //            stringifyNextToken_file = false;
        //
        //            _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        //        }
//        else {
        
        //
        // Regular newline
        //
        
        switch (c.to_point()) {
            case '\n': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                _currentToken = Token(TOKEN_NEWLINE, String.str(), getTokenSource());
            }
                break;
            case '\r': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                if (c.to_point() == '\n') {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                }
                
                _currentToken = Token(TOKEN_NEWLINE, String.str(), getTokenSource());
            }
                break;
            default:
                assert(false);
                break;
        }
//        }
        return;
    }
    
    //
    // There could be space, something like  << abc
    //
    // or something like:
    // a >>
    //   b
    //
    
    if (c.isSpace() || c.isNewline() || c.isSpaceCharacter() || c.isNewlineCharacter()) {
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_WHITESPACE, String.str(), getTokenSource());
        
        return;
    }
    
    handleString_stringifyNextToken_file();
}

WLCharacter Tokenizer::nextWLCharacter(NextWLCharacterPolicy policy) {
    
    //
    // handle the queue before anything else
    //
    // the WLCharacters in the queue may be part of a Token with multiple WLCharacters
    //
    if (!wlCharacterQueue.empty()) {
        
        auto p = wlCharacterQueue[0];
        
        //
        // Make sure to set source information
        //
        TheByteDecoder->setSourceLocation(p.second.start());
        TheCharacterDecoder->setWLCharacterStart();
        TheByteDecoder->setSourceLocation(p.second.end());
        TheCharacterDecoder->setWLCharacterEnd();
        
        // erase first
        wlCharacterQueue.erase(wlCharacterQueue.begin());
        
        _currentWLCharacter = p.first;
        
        return _currentWLCharacter;
    }
    
    _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
    
    return _currentWLCharacter;
}

void Tokenizer::append(WLCharacter c, Source Span) {
    wlCharacterQueue.push_back(std::make_pair(c, Span));
}

WLCharacter Tokenizer::currentWLCharacter() const {
    
    return _currentWLCharacter;
}

Token Tokenizer::currentToken() {
    
    assert(_currentToken.Tok() != TOKEN_UNKNOWN);
    
    return _currentToken;
}

inline void Tokenizer::handleEndOfFile() {
    
    //
    // EndOfFile is special, so invent source
    //
    
    auto Start = TokenStartLoc;
    
    _currentToken = Token(TOKEN_ENDOFFILE, String.str(), Source(Start));
}

inline void Tokenizer::handleLineFeed() {
    
    auto c = WLCharacter('\n');
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_NEWLINE, String.str(), getTokenSource());
}

inline void Tokenizer::handleCarriageReturn() {
    
    auto c = WLCharacter('\r');
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    if (c.to_point() == '\n') {
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
    }
    
    _currentToken = Token(TOKEN_NEWLINE, String.str(), getTokenSource());
}

inline void Tokenizer::handleNewlineCharacter() {
    
    auto c = currentWLCharacter();
    
    if (c.isStrangeNewlineCharacter()) {
        
        auto Src = TheCharacterDecoder->getWLCharacterSource();
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        Issues.push_back(std::move(I));
    }
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_NEWLINE, String.str(), getTokenSource());
}

inline void Tokenizer::handleSpace() {
    
    //
    // Could be <space> or \[RawSpace]
    //
    
    auto c = currentWLCharacter();
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_WHITESPACE, String.str(), getTokenSource());
}

inline void Tokenizer::handleTab() {
    
    //
    // Could be \t or \[RawTab]
    //
    
    auto c = currentWLCharacter();
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_WHITESPACE, String.str(), getTokenSource());
}

inline void Tokenizer::handleStrangeSpace() {
    
    auto c = currentWLCharacter();
        
    auto Src = TheCharacterDecoder->getWLCharacterSource();
    
    auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
    
    Issues.push_back(std::move(I));
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_WHITESPACE, String.str(), getTokenSource());
}

inline void Tokenizer::handleSpaceCharacter() {
    
    auto c = currentWLCharacter();
    
    if (c.isStrangeSpaceCharacter()) {
        
        auto Src = TheCharacterDecoder->getWLCharacterSource();
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        Issues.push_back(std::move(I));
    }
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_WHITESPACE, String.str(), getTokenSource());
}

inline void Tokenizer::handleLineContinuation() {
    
    auto c = currentWLCharacter();
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_LINECONTINUATION, String.str(), getTokenSource());
}

inline void Tokenizer::handleLinearSyntax() {
    
    auto c = currentWLCharacter();
    
    String << c;
    
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
            Operator = TOKEN_ERROR_UNHANDLEDCHARACTER;
            break;
    }
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleComment() {
    
    //
    // comment is already started
    //
    // Comments deal with literal (**) characters
    // Escaped characters do not work
    //
    
    auto c = TheCharacterDecoder->currentWLCharacter();
    
    assert(c == WLCharacter('*'));
    
    String << c;
    
    auto depth = 1;
    
    c = nextWLCharacter(INSIDE_COMMENT);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        _currentToken = Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, String.str(), getTokenSource());
        
        return;
    }
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        //
        // No need to check for comment length
        //
        
        if (c == WLCharacter('(')) {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_COMMENT);
            
            if (c == WLCharacter('*')) {
                
                depth = depth + 1;
                
                String << c;
                
                c = nextWLCharacter(INSIDE_COMMENT);
            }
            
        } else if (c == WLCharacter('*')) {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_COMMENT);
            
            if (c == WLCharacter(')')) {
                
                String << c;
                
                // This comment is closing
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    // Leaving comments, make sure to grab next character
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    break;
                }
                
                c = nextWLCharacter(INSIDE_COMMENT);
            }
            
        } else if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            _currentToken = Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, String.str(), getTokenSource());
            
            return;
            
        } else {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_COMMENT);
        }
        
    } // while
    
    _currentToken = Token(TOKEN_COMMENT, String.str(), getTokenSource());
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
void Tokenizer::handleSymbol(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    assert(c.to_point() == '`' || c.isLetterlike() || c.isLetterlikeCharacter());
    
    if (c.isLetterlike() || c.isLetterlikeCharacter()) {
        
        handleSymbolSegment(Ctxt);
    }
    
    c = currentWLCharacter();
    
    while (c.to_point() == '`') {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
            
            auto Src = TheCharacterDecoder->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the **`** character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
            
            Issues.push_back(std::move(I));
        }
        
        String << c;
        
        c = nextWLCharacter(INSIDE_SYMBOL);
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            handleSymbolSegment(Ctxt);
            
        } else {
            
            _currentToken = Token(TOKEN_OTHER, String.str(), getTokenSource());
            
            return;
        }
        
        c = currentWLCharacter();
        
    } // while
    
    _currentToken = Token(TOKEN_SYMBOL, String.str(), getTokenSource());
}

void Tokenizer::handleSymbolSegment(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    assert(c.isLetterlike() || c.isLetterlikeCharacter());
    
    if (c.to_point() == '$') {
        
        if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
            
            auto Src = TheCharacterDecoder->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the ``$`` character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
            
            Issues.push_back(std::move(I));
        }
    }
    
    if (c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter()) {
        Utils::strangeLetterlikeWarning(c);
    }
    
    String << c;
    
    c = nextWLCharacter(INSIDE_SYMBOL);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isDigit()) {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_SYMBOL);
            
        } else if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            if (c.to_point() == '$') {
                
                if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
                    
                    auto Src = TheCharacterDecoder->getWLCharacterSource();
                    
                    auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the ``$`` character.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
                    
                    Issues.push_back(std::move(I));
                }
            }
            
            if (c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter()) {
                Utils::strangeLetterlikeWarning(c);
            }
            
            String << c;
            
            c = nextWLCharacter(INSIDE_SYMBOL);
            
        } else {
            break;
        }
        
    } // while
}

void Tokenizer::handleString() {
    
    auto c = currentWLCharacter();
        
    assert(c.to_point() == '"');
    
    String << c;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = nextWLCharacter(INSIDE_STRING);
        
        if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            _currentToken = Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), getTokenSource());
            
            return;
            
        } else if (c.to_point() == '"') {
            
            //
            // OK to check c == WLCharacter('"') here because we only care about un-escaped "
            //
            
            break;
        }
        
        String << c;
        
    } // while
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_STRING, String.str(), getTokenSource());
}

void Tokenizer::handleString_stringifyCurrentLine() {
    
    auto c = currentWLCharacter();
    
    auto lastGoodLocation = TheByteDecoder->getSourceLocation();
    
    auto empty = true;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = currentWLCharacter();
        
        if (c == WLCharacter(CODEPOINT_ENDOFFILE)) {
            
            break;
            
        } else if (c.isNewline() || c.isNewlineCharacter()) {
            
            break;
            
        } else if (c.isLineContinuation()) {
            
            break;
        }
        
        String << c;
        
        empty = false;
        
        lastGoodLocation = TheByteDecoder->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_STRINGIFY_LINE);
        
    } // while
    
    if (empty) {
        
        //
        // Something like   ?<EOF>
        //
        // EndOfFile is special because there is no source
        //
        // So invent source
        //
        
        auto Start = TokenStartLoc;
        
        _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        
        return;
    }
    
    //
    // ?? syntax is special because we want to ignore the newline that was read.
    //
    // So invent source
    //
    
    _currentToken = Token(TOKEN_STRING, String.str(), Source(TokenStartLoc, lastGoodLocation));
}

void Tokenizer::handleString_stringifyNextToken_symbol() {
    
    auto c = currentWLCharacter();
    
    if (c.to_point() != '"') {
        
        //
        // magically turn into a string
        //
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            TokenizerContext Ctxt;
            
            handleSymbolSegment(Ctxt);
            
            _currentToken = Token(TOKEN_STRING, String.str(), getTokenSource());
            
            return;
        }
            
        //
        // Something like   a::5
        //
        
        nextToken();
        
        _currentToken = Token(TOKEN_OTHER, String.str(), getTokenSource());
        
        return;
    }
        
    assert(c.to_point() == '"');
    
    String << c;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = nextWLCharacter(INSIDE_STRING);
        
        if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            _currentToken = Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), getTokenSource());
            
            return;
            
        } else if (c.to_point() == '"') {
            
            //
            // OK to check c == WLCharacter('"') here because we only care about un-escaped "
            //
            
            break;
        }
        
        String << c;
        
    } // while
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_STRING, String.str(), getTokenSource());
}

void Tokenizer::handleString_stringifyNextToken_file() {
    
    auto c = currentWLCharacter();
    
    if (c.to_point() != '"') {
        
        //
        // magically turn into a string
        //
        
        auto empty = true;
        while (true) {
            
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
            
            if (c.isDigit() || c.isAlpha() || c.to_point() == '$' || c.to_point() == '`' || c.to_point() == '/' ||
                c.to_point() == '.' || c.to_point() == '\\' || c.to_point() == '!' || c.to_point() == '-' ||
                c.to_point() == '_' || c.to_point() == ':' || c.to_point() == '*' || c.to_point() == '~' ||
                c.to_point() == '?') {
                
                empty = false;
                
                String << c;
                
                c = nextWLCharacter(INSIDE_STRINGIFY_FILE);
                
            } else if (c.to_point() == '[') {
                
                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
                
                empty = false;
                
                handleFileOpsBrackets();
                if (_currentToken.Tok() != TOKEN_STRING) {
                    return;
                }
                
                c = currentWLCharacter();
                
            } else {
                
                break;
            }
            
        } // while
        
        if (empty) {
            
            //
            // Something like   <<EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            
            auto Start = TokenStartLoc;
            
            _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
            
            return;
        }
        
        _currentToken = Token(TOKEN_STRING, String.str(), getTokenSource());
        
        return;
    }
        
    assert(c.to_point() == '"');
    
    String << c;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        c = nextWLCharacter(INSIDE_STRING);
        
        if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            _currentToken = Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), getTokenSource());
            
            return;
            
        } else if (c.to_point() == '"') {
            
            //
            // OK to check c == WLCharacter('"') here because we only care about un-escaped "
            //
            
            break;
        }
        
        String << c;
        
    } // while
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_STRING, String.str(), getTokenSource());
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
void Tokenizer::handleFileOpsBrackets() {
    
    auto c = currentWLCharacter();
    
    assert(c.to_point() == '[');
    
    String << c;
    
    auto depth = 1;
    
    auto lastGoodLocation = TheByteDecoder->getSourceLocation();
    
    c = nextWLCharacter(INSIDE_STRINGIFY_FILE);
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isSpace() || c.isNewline() || c.isSpaceCharacter() || c.isNewlineCharacter()) {
            
            //
            // Cannot have spaces in the string here, so bail out
            //
            // Not going to include the space here, so invent source
            //
            
            _currentToken = Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), Source(TokenStartLoc, lastGoodLocation));
            
            return;
        }
        if (c.to_point() == CODEPOINT_ENDOFFILE) {
            
            _currentToken = Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), getTokenSource());
            
            return;
        }
        
        String << c;
        
        if (c.to_point() == '[') {
            
            depth = depth + 1;
            
        } else if (c.to_point() == ']') {
            
            depth = depth - 1;
            
            if (depth == 0) {
                
                // Leaving brackets, make sure to grab next character
                
                c = nextWLCharacter(INSIDE_STRINGIFY_FILE);
                
                break;
            }
        }
        
        lastGoodLocation = TheByteDecoder->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_STRINGIFY_FILE);
        
    } // while
    
    _currentToken = Token(TOKEN_STRING, String.str(), getTokenSource());
}

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
inline void Tokenizer::handleNumber() {
    
    int base = 0;
    
    handleDigits();
    
    auto c = currentWLCharacter();
    
    //
    // Could be 16^^blah
    //
    if (c.to_point() == '^') {
        
        auto Caret1Char = c;
        
        auto Caret1Loc = TheByteDecoder->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c.to_point() != '^') {
            
            //
            // Something like  2^a
            //
            // Must now do surgery and back up
            //
            
            auto Src = TheCharacterDecoder->getWLCharacterSource();
            
            //
            // FIXME: CaretLoc-1 is not correct because of something like this:
            //
            // 2\
            // ^a
            //
            TheByteDecoder->setSourceLocation(Caret1Loc-1);
            TheCharacterDecoder->setWLCharacterStart();
            TheCharacterDecoder->setWLCharacterEnd();
            
            TheByteDecoder->setSourceLocation(Caret1Loc);
            TheCharacterDecoder->setWLCharacterStart();
            TheCharacterDecoder->setWLCharacterEnd();
            _currentWLCharacter = Caret1Char;
            
            append(c, Src);
            
            _currentToken = Token(TOKEN_INTEGER, String.str(), getTokenSource());
            
            return;
        }
        
        // c is '^'
            
        //
        // Something like 2^^
        //
        // Must be a number
        //
        
        auto S = String.str();
        
        //
        // Only parse integer if we know it can possibly be a valid base
        //
        if (S.size() > 2) {
            
            //
            // Too large
            //
            
            base = -1;
            
        } else {
            
            //
            // parseInteger is only safe to call if we know S is parseable
            //
            
            base = Utils::parseInteger(S, 10);
        }
        
        if (base < 2 || base > 36) {
            
            //
            // Something like 37^^2, which is an invalid base
            //
            
            String << Caret1Char;
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            _currentToken = Token(TOKEN_ERROR_INVALIDBASE, String.str(), getTokenSource());
            
            return;
        }
        
        String << Caret1Char;
        String << c;
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c.isDigit() || c.isAlpha()) {
            
            auto handle = handleDigitsOrAlpha(base);
            if (handle == -1) {
                
                _currentToken = Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, String.str(), getTokenSource());
                
                return;
            }
            
        } else if (c.to_point() != '.') {
            
            //
            // Make sure that bad character is added to String
            //
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            _currentToken = Token(TOKEN_ERROR_UNRECOGNIZEDDIGIT, String.str(), getTokenSource());
            
            return;
        }
    }
    
    c = currentWLCharacter();
    
    bool real = false;
    
    if (c.to_point() == '.') {
        
        auto handle = handleFractionalPart(base);
        if (handle == -1) {
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
            
            return;
        }
        
        real = true;
    }
    
    c = currentWLCharacter();
    
    //
    // foo`
    // foo`bar
    // foo``bar
    //
    if (c.to_point() == '`') {
        
        real = true;
        
        String << c;
        
        auto TickLoc = TheByteDecoder->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        bool accuracy = false;
        if (c.to_point() == '`') {
            
            String << c;
            
            TickLoc = TheByteDecoder->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            accuracy = true;
        }
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            //
            // Something like 1.2`a
            //
            
            auto Loc2 = TheByteDecoder->getSourceLocation();
            
            auto cGraphicalStr = c.graphicalString();
            
            //
            // Use ** markup syntax here because of ` character
            //
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(Loc2), " ")));
            
            auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between **`** and ``" + cGraphicalStr + "`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc2), 1.0, std::move(Actions)));
            
            Issues.push_back(std::move(I));
        }
        
        if (accuracy || c.isDigit() || c.to_point() == '-' || c.to_point() == '+' || c.to_point() == '.') {
            
            if (c.to_point() == '-' || c.to_point() == '+') {
                
                auto s = c;
                
                auto SignLoc = TheByteDecoder->getSourceLocation();
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                if (c.isDigit()) {
                    
                    String << s;
                    
                } else if (c.to_point() == '.') {
                    
                    String << s;
                    
                } else if (accuracy) {
                    
                    //
                    // Something like 1.2``->3
                    //
                    
                    String << s;
                    String << c;
                    
                    c = nextWLCharacter(INSIDE_NUMBER);
                    
                    _currentToken = Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), getTokenSource());
                    
                    return;
                    
                } else {
                    
                    //
                    // Something like 1.2`->3
                    //
                    // Must now do surgery and back up
                    //
                    // Cannot use SignLoc as source of FormatIssue, because it is expected to be the source for the entire token,
                    // which we do not have yet
                    //
                    // So use the source for number itself and insert space after that
                    //
                    
                    auto NumStartLoc = TokenStartLoc;
                    auto NumEndLoc = SignLoc - 1;
                    
                    std::string msg;
                    if (s.to_point() == '-') {
                        msg = "Put a space between **`** and ``-`` to reduce ambiguity";
                    } else {
                        msg = "Put a space between **`** and ``+`` to reduce ambiguity";
                    }
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextAfterCodeAction("Insert space after", Source(NumStartLoc, NumEndLoc), " ")));
                    
                    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACEAFTER, msg, FORMATISSUESEVERITY_FORMATTING, Source(NumStartLoc, NumEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                    auto Src = TheCharacterDecoder->getWLCharacterSource();
                    
                    //
                    // FIXME: SignLoc-1 is not correct because of something like this:
                    //
                    // 1.2`\
                    // ->3
                    //
                    TheByteDecoder->setSourceLocation(SignLoc-1);
                    TheCharacterDecoder->setWLCharacterStart();
                    TheCharacterDecoder->setWLCharacterEnd();
                    
                    TheByteDecoder->setSourceLocation(SignLoc);
                    TheCharacterDecoder->setWLCharacterStart();
                    TheCharacterDecoder->setWLCharacterEnd();
                    _currentWLCharacter = s;
                    
                    append(c, Src);
                    
                    _currentToken = Token(TOKEN_REAL, String.str(), getTokenSource());
                    
                    return;
                }
            }
            
            bool supplied = false;
            
            if (c.isDigit()) {
                
                handleDigits();
                
                supplied = true;
            }
            
            c = currentWLCharacter();
            
            if (c.to_point() == '.') {
                
                //
                // Need to decide if the  .  here is actual decimal point, or something like
                // the . in  123`.xxx  (which is Dot)
                //
                
                if (!supplied) {
                    
                    //
                    // Something like 123`.xxx
                    //
                    
                    auto DotChar = c;
                    auto DotLoc = TheByteDecoder->getSourceLocation();
                    
                    // look ahead
                    auto NextChar = nextWLCharacter(INSIDE_NUMBER);
                    
                    auto Src = TheCharacterDecoder->getWLCharacterSource();
                    
                    //
                    // Must now do surgery and back up
                    //
                    
                    TheByteDecoder->setSourceLocation(DotLoc-1);
                    TheCharacterDecoder->setWLCharacterStart();
                    TheCharacterDecoder->setWLCharacterEnd();
                    
                    TheByteDecoder->setSourceLocation(DotLoc);
                    TheCharacterDecoder->setWLCharacterStart();
                    TheCharacterDecoder->setWLCharacterEnd();
                    _currentWLCharacter = DotChar;
                    
                    append(NextChar, Src);
                    
                    if (!NextChar.isDigit()) {
                        
                        if (!accuracy) {
                            
                            //
                            // Something like  123`.xxx  where the . could be a Dot operator
                            //
                            
                            _currentToken = Token(TOKEN_REAL, String.str(), getTokenSource());
                            
                            return;
                        }
                            
                        //
                        // Something like  123``.EOF
                        //
                        
                        String << DotChar;
                        String << NextChar;
                        
                        // this grabs the Dot character
                        c = nextWLCharacter(INSIDE_NUMBER);
                        // this grabs the next character
                        c = nextWLCharacter(INSIDE_NUMBER);
                        
                        _currentToken = Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), getTokenSource());
                        
                        return;
                    }
                }
                
                // actual decimal point
                
                auto handle = handleFractionalPart(0);
                if (handle != 0) {
                    
                    assert(handle > 0);
                    
                    supplied = true;
                }
            }
            
            if (accuracy) {
                if (!supplied) {
                    
                    //
                    // Something like  123``EOF
                    //
                    
                    String << c;
                    
                    c = nextWLCharacter(INSIDE_NUMBER);
                    
                    _currentToken = Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), getTokenSource());
                    
                    return;
                }
            }
        }
    }
    
    c = currentWLCharacter();
    
    if (c.to_point() != '*') {
        
        if (real) {
            _currentToken = Token(TOKEN_REAL, String.str(), getTokenSource());
        } else {
            _currentToken = Token(TOKEN_INTEGER, String.str(), getTokenSource());
        }
        
        return;
    }
    
    // c is '*'
        
    auto StarChar = c;
    
    auto StarLoc = TheByteDecoder->getSourceLocation();
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    if (c.to_point() != '^') {
        
        //
        // Something like 1*a
        //
        // Must now do surgery and back up
        //
        
        auto Src = TheCharacterDecoder->getWLCharacterSource();
        
        //
        // FIXME: StarLoc-1 is not correct because of something like this:
        //
        // 1\
        // *a
        //
        TheByteDecoder->setSourceLocation(StarLoc-1);
        TheCharacterDecoder->setWLCharacterStart();
        TheCharacterDecoder->setWLCharacterEnd();
        
        TheByteDecoder->setSourceLocation(StarLoc);
        TheCharacterDecoder->setWLCharacterStart();
        TheCharacterDecoder->setWLCharacterEnd();
        _currentWLCharacter = StarChar;
        
        append(c, Src);
        
        if (real) {
            _currentToken = Token(TOKEN_REAL, String.str(), getTokenSource());
        } else {
            _currentToken = Token(TOKEN_INTEGER, String.str(), getTokenSource());
        }
        
        return;
    }
    
    // c is '^'
        
    String << StarChar;
    String << c;
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    if (c.to_point() == '-' || c.to_point() == '+') {
        
        String << c;
        
        c = nextWLCharacter(INSIDE_NUMBER);
    }
    
    if (!expectDigits()) {
        
        //
        // Something like 123*^EOF
        //
        
        String << c;
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        _currentToken = Token(TOKEN_ERROR_EXPECTEDEXPONENT, String.str(), getTokenSource());
        
        return;
    }
    
    c = currentWLCharacter();
    
    if (c.to_point() != '.') {
        
        if (real) {
            _currentToken = Token(TOKEN_REAL, String.str(), getTokenSource());
        } else {
            _currentToken = Token(TOKEN_INTEGER, String.str(), getTokenSource());
        }
        
        return;
    }
    
    // c is '.'
        
    //
    // Something like 123*^0.5
    //
    
    String << c;
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    _currentToken = Token(TOKEN_ERROR_EXPECTEDEXPONENT, String.str(), getTokenSource());
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
// Note: if 0 digits, then the . is also not added to String
//
int Tokenizer::handleFractionalPart(int base) {
    
    auto c = currentWLCharacter();
    
    assert(c.to_point() == '.');
    
    auto DotChar1 = c;
    
    auto DotLoc1 = TheByteDecoder->getSourceLocation();
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    if (c.to_point() == '.') {
        
        //
        // Something like 0..
        //
        // Must now do surgery and back up
        //
        
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(DotLoc1), " ")));
        
        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, Source(DotLoc1), 0.90, std::move(Actions)));
        
        Issues.push_back(std::move(I));
        
        
        auto Loc = TheByteDecoder->getSourceLocation();
        
        //
        // FIXME: DotLoc-1 is not correct because of something like this:
        //
        // 0\
        // ..
        //
        TheByteDecoder->setSourceLocation(DotLoc1-1);
        TheCharacterDecoder->setWLCharacterStart();
        TheCharacterDecoder->setWLCharacterEnd();
        
        TheByteDecoder->setSourceLocation(DotLoc1);
        TheCharacterDecoder->setWLCharacterStart();
        TheCharacterDecoder->setWLCharacterEnd();
        _currentWLCharacter = DotChar1;
        
        append(c, Source(Loc, Loc));
        
        return false;
    }
    
    String << DotChar1;
    
    auto handle = 0;
    if (c.isDigit() || c.isAlpha()) {
        
        handle = handleDigitsOrAlpha(base);
        if (handle == -1) {
            return -1;
        } else if (handle == 0) {
            return 0;
        }
    }
    
    c = currentWLCharacter();
    
    if (c.to_point() == '.') {
        
        //
        // Something like 1.2.3
        //
        
        auto Loc2 = TheByteDecoder->getSourceLocation();
        
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert *", Source(Loc2), "*")));
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMES, "Suspicious syntax.", SYNTAXISSUESEVERITY_ERROR, Source(Loc2), 0.99, std::move(Actions)));
        
        Issues.push_back(std::move(I));
    }
    
    return handle;
}

bool Tokenizer::expectDigits() {
    
    auto c = currentWLCharacter();
    
    if (c.isDigit()) {
        
        handleDigits();
        
        return true;
    }
    
    return false;
}

//
// Return length of digits
//
// Precondition: currentWLCharacter MAY NOT be a digit
// Postcondition: currentWLCharacter is the first WLCharacter AFTER all good digits
//
size_t Tokenizer::handleDigits() {
    
    auto c = currentWLCharacter();
    
    auto len = 0;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isDigit()) {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
        } else {
            break;
        }
        
        len++;
    } // while
    
    return len;
}

//
// Precondition: currentWLCharacter is NOT in String
// Postcondition: currentWLCharacter is the first WLCharacter AFTER all good digits or alphas
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
// Note: if base == 0, then it is not possible to return an error
//
int Tokenizer::handleDigitsOrAlpha(int base) {
    
    auto c = currentWLCharacter();
    
    auto handled = 0;
    auto error = false;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.isDigit() || c.isAlpha()) {
            
            int baseDigit = toDigit(c.to_point());
            
            assert(baseDigit >= 0);
            
            if (base == 0) {
                
                //
                // base is the unspecified default i.e., 10
                //
                
                if (baseDigit >= 10) {
                    
                    //
                    // Not an error if no base is specified
                    //
                    // It's the difference between 2.Pi and 10^^2.Pi
                    //
                    
                    break;
                }
                
            } else {
                
                if (baseDigit >= base) {
                    error = true;
                }
            }
            
            String << c;
            
            handled++;
            
        } else {
            break;
        }
        
        c = nextWLCharacter(INSIDE_NUMBER);
    }
    
    if (error) {
        return -1;
    }
    
    return handled;
}

inline void Tokenizer::handleColon() {
    
    //
    // Could be : or \[RawColon]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_COLON; // :
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_COLONCOLON; // ::
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '=': {
            Operator = TOKEN_COLONEQUAL; // :=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '>': {
            Operator = TOKEN_COLONGREATER; // :>
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleOpenParen() {
    
    //
    // Could be ( or \[RawLeftParenthesis]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_OPENPAREN; // (
    
    auto ParenChar = c;
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    //
    // Comments must start literally with (*
    // Escaped characters do not work
    //
    if (ParenChar == WLCharacter('(') &&
        c == WLCharacter('*')) {
        
        return handleComment();
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleCloseParen() {
    
    //
    // Could be ) or \[RawRightParenthesis]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_CLOSEPAREN; // )
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleOpenSquare() {
    
    //
    // Could be [ or \[RawLeftBracket]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_OPENSQUARE; // [
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleCloseSquare() {
    
    //
    // Could be ] or \[RawRightBracket]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_CLOSESQUARE; // ]
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleComma() {
    
    //
    // Could be , or \[RawComma]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_COMMA; // ,
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleOpenCurly() {
    
    //
    // Could be { or \[RawLeftBrace]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_OPENCURLY; // {
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleCloseCurly() {
    
    //
    // Could be } or \[RawRightBrace]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_CLOSECURLY; // }
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleDot() {
    
    //
    // Could be . or \[RawDot]
    //
    
    auto c = currentWLCharacter();
    
    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //
    
    auto DotChar = c;
    
    auto DotLoc = TheByteDecoder->getSourceLocation();
    
    c = nextWLCharacter(TOPLEVEL);
    
    if (c.isDigit()) {
        
        //
        // Something like .0
        //
        // Must now do surgery and back up (and go to handleNumber instead)
        //
        
        auto Loc = TheByteDecoder->getSourceLocation();
        
        //
        // FIXME: DotLoc-1 is not correct because of something like this:
        //
        // .0
        //
        TheByteDecoder->setSourceLocation(DotLoc-1);
        TheCharacterDecoder->setWLCharacterStart();
        TheCharacterDecoder->setWLCharacterEnd();
        
        TheByteDecoder->setSourceLocation(DotLoc);
        TheCharacterDecoder->setWLCharacterStart();
        TheCharacterDecoder->setWLCharacterEnd();
        _currentWLCharacter = DotChar;
        
        append(c, Source(Loc));
        
        return handleNumber();
    }
    
    String << DotChar;
    
    auto Operator = TOKEN_DOT; // .
    
    if (c.to_point() == '.') {
        
        Operator = TOKEN_DOTDOT; // ..
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        if (c.to_point() == '.') {
            
            Operator = TOKEN_DOTDOTDOT; // ...
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleEqual() {
    
    //
    // Could be = or \[RawEqual]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_EQUAL; // =
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '=': {
            Operator = TOKEN_EQUALEQUAL; // ==
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALEQUALEQUAL; // ===
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '.': {
            
            //
            // handleDot
            //
            // Could be  =.  or  =..  or  =...  or  =....  or  =.0
            //
            
            auto DotChar = c;
            
            auto DotLoc = TheByteDecoder->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.isDigit()) {
                
                //
                // Something like x=.0
                //
                // Must now do surgery and back up
                //
                
                auto Src = TheCharacterDecoder->getWLCharacterSource();
                
                //
                // FIXME: DotLoc-1 is not correct because of something like this:
                //
                // x=.\
                // 0
                //
                TheByteDecoder->setSourceLocation(DotLoc-1);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                
                TheByteDecoder->setSourceLocation(DotLoc);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                _currentWLCharacter = DotChar;
                
                append(c, Src);
                
                Operator = TOKEN_EQUAL;
                
            } else if (c.to_point() == '.') {
                
                //
                // =..  is a syntax error
                //
                
                String << DotChar;
                String << c;
                
                Operator = TOKEN_ERROR_UNHANDLEDDOT;
                
                c = nextWLCharacter(INSIDE_OPERATOR);
                
            } else {
                
                Operator = TOKEN_EQUALDOT; // =.
                
                String << DotChar;
            }
            
        }
            break;
        case '!': {
            
            auto BangChar = c;
            
            auto BangLoc = TheByteDecoder->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_EQUALBANGEQUAL; // =!=
                
                String << BangChar;
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
            } else {
                
                //
                // Something like x=!y
                //
                // Must now do surgery and back up
                //
                
                auto Src = TheCharacterDecoder->getWLCharacterSource();
                
                //
                // FIXME: BangLoc-1 is not correct because of something like this:
                //
                // x=\
                // !y
                //
                TheByteDecoder->setSourceLocation(BangLoc-1);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                
                TheByteDecoder->setSourceLocation(BangLoc);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                _currentWLCharacter = BangChar;
                
                append(c, Src);
                
                Operator = TOKEN_EQUAL;
            }
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleUnder() {
    
    //
    // Could be _ or \[RawUnderscore]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_UNDER; // _
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '_': {
            Operator = TOKEN_UNDERUNDER; // __
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '_') {
                
                Operator = TOKEN_UNDERUNDERUNDER; // ___
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '.': {
            
            auto DotChar = c;
            
            auto DotLoc = TheByteDecoder->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '.') {
                
                //
                // Something like  _...
                //
                // Must now do surgery and back up
                //
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(DotLoc), " ")));
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, Source(DotLoc), 0.95, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
                
                auto Src = TheCharacterDecoder->getWLCharacterSource();
                
                //
                // FIXME: DotLoc-1 is not correct because of something like this:
                //
                // _\
                // ...
                //
                TheByteDecoder->setSourceLocation(DotLoc-1);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                
                TheByteDecoder->setSourceLocation(DotLoc);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                _currentWLCharacter = DotChar;
                
                append(c, Src);
                
                Operator = TOKEN_UNDER; // _
                
            } else {
                
                String << DotChar;
                
                if (c.isDigit()) {
                    
                    //
                    // Something like _.0
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(DotLoc), " ")));
                    
                    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Suspicious syntax.", FORMATISSUESEVERITY_FORMATTING, Source(DotLoc), 0.90, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
                
                Operator = TOKEN_UNDERDOT; // _.
            }
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleLess() {
    
    //
    // Could be < or \[RawLess]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_LESS; // <
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '|': {
            Operator = TOKEN_LESSBAR; // <|
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '<': {
            Operator = TOKEN_LESSLESS; // <<
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '>': {
            Operator = TOKEN_LESSGREATER; // <>
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '=': {
            Operator = TOKEN_LESSEQUAL; // <=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '-': {
            
            auto MinusChar = c;
            
            auto MinusLoc = TheByteDecoder->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_LESSMINUSGREATER; // <->
                
                String << MinusChar;
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
            } else {
                
                //
                // Something like  a<-4
                //
                // Must now do surgery and back up
                //
                
                auto Src = TheCharacterDecoder->getWLCharacterSource();
                
                //
                // FIXME: MinusLoc-1 is not correct because of something like this:
                //
                // a<\
                // -4
                //
                TheByteDecoder->setSourceLocation(MinusLoc-1);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                
                TheByteDecoder->setSourceLocation(MinusLoc);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                _currentWLCharacter = MinusChar;
                
                append(c, Src);
                
                Operator = TOKEN_LESS;
            }
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleGreater() {
    
    //
    // Could be > or \[RawGreater]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_GREATER; // >
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '>': {
            
            Operator = TOKEN_GREATERGREATER; // >>
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '>') {
                
                Operator = TOKEN_GREATERGREATERGREATER; // >>>
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '=': {
            Operator = TOKEN_GREATEREQUAL; // >=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleMinus() {
    
    //
    // Could be - or \[RawDash]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_MINUS; // -
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
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
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '-': {
            Operator = TOKEN_MINUSMINUS; // --
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '>') {
                
                //
                // Something like a-->0
                //
                
                auto Loc = TheByteDecoder->getSourceLocation();
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(Loc), " ")));
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``>`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc), 1.0, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
            } else if (c.to_point() == '=') {
                
                //
                // Something like a--=0
                //
                
                auto Loc = TheByteDecoder->getSourceLocation();
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(Loc), " ")));
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc), 1.0, std::move(Actions)));
                
                Issues.push_back(std::move(I));
            }
        }
            break;
        case '=': {
            Operator = TOKEN_MINUSEQUAL; // -=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleBar() {
    
    //
    // Could be | or \[RawVerticalBar]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_BAR; // |
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '>': {
            Operator = TOKEN_BARGREATER; // |>
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '=') {
                
                //
                // Something like <||>=0
                //
                
                auto Loc = TheByteDecoder->getSourceLocation();
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(Loc), " ")));
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``>`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc), 1.0, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
            }
            
        }
            break;
        case '|': {
            Operator = TOKEN_BARBAR; // ||
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleSemi() {
    
    //
    // Could be ; or \[RawSemicolon]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_SEMI; // ;
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    if (c.to_point() == ';') {
        
        Operator = TOKEN_SEMISEMI; // ;;
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleBang() {
    
    //
    // Could be ! or \[RawExclamation]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_BANG; // !
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '=': {
            Operator = TOKEN_BANGEQUAL; // !=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '!': {
            Operator = TOKEN_BANGBANG; // !!
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleHash() {
    
    //
    // Could be # or \[RawNumberSign]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_UNKNOWN;
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
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
    
    if (c.isDigit()) {
        
        Operator = TOKEN_HASH; // #
        
        handleDigits();
        
    } else if (c.isLetterlike() || c.isLetterlikeCharacter()) {
        
        Operator = TOKEN_HASH; // #
        
        TokenizerContext Ctxt;
        
        Ctxt |= TOKENIZER_SLOT;
        
        handleSymbol(Ctxt);
        
    } else if (c.to_point() == '`') {
        
        Operator = TOKEN_HASH; // #
        
        TokenizerContext Ctxt;
        
        Ctxt |= TOKENIZER_SLOT;
        
        handleSymbol(Ctxt);
        
    } else if (c.to_point() == '"') {
        
        auto Loc = TheByteDecoder->getSourceLocation();
        
        Operator = TOKEN_HASH; // #
        
        handleString();
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "The name following ``#`` is not documented to allow the ``\"`` character.", SYNTAXISSUESEVERITY_REMARK, Source(Loc), 0.33, {}));
        
        Issues.push_back(std::move(I));
        
    } else if (c.to_point() == '#') {
        
        Operator = TOKEN_HASHHASH; // ##
        
        String << c;
        
        c = nextWLCharacter(INSIDE_OPERATOR);
        
        if (c.isDigit()) {
            
            handleDigits();
        }
        
    } else if (c.to_point() == '!') {
        
        if (TokenStartLoc.style == SOURCESTYLE_LINECOL &&
            TokenStartLoc.lineCol == LineCol(1, 1)) {
            
            Operator = TOKEN_HASHBANG; // #!
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
        } else {
            
            Operator = TOKEN_HASH; // #
        }
        
    } else {
        
        Operator = TOKEN_HASH; // #
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handlePercent() {
    
    //
    // Could be % or \[RawPercent]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_PERCENT; // %
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    if (c.to_point() == '%') {
        
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            if (c.to_point() != '%') {
                break;
            }
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
        }
        
    } else if (c.isDigit()) {
        
        handleDigits();
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleAmp() {
    
    //
    // Could be & or \[RawAmpersand]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_AMP; // &
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    if (c.to_point() == '&') {
        
        Operator = TOKEN_AMPAMP; // &&
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleSlash() {
    
    //
    // Could be / or \[RawSlash]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_SLASH; // /
    
    String << c;
    
    c = nextWLCharacter(INSIDE_OPERATOR);
    
    switch (c.to_point()) {
        case '@': {
            Operator = TOKEN_SLASHAT; // /@
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case ';': {
            Operator = TOKEN_SLASHSEMI; // /;
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '.': {
            
            auto DotChar = c;
            
            auto DotLoc = TheByteDecoder->getSourceLocation();
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.isDigit()) {
                
                //
                // Something like t/.3
                //
                // Must now do surgery and back up
                //
                // Cannot use DotLoc as source of FormatIssue, because it is expected to be the source for the entire token,
                // which we do not have yet
                //
                // So use the source for Slash itself and insert space after that
                //
                
                auto SlashStartLoc = TokenStartLoc;
                auto SlashEndLoc = DotLoc - 1;
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextAfterCodeAction("Insert space", Source(SlashStartLoc, SlashEndLoc), " ")));
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACEAFTER, "Put a space between ``/`` and ``.`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(SlashStartLoc, SlashEndLoc), 1.0, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
                
                auto Loc = TheByteDecoder->getSourceLocation();
                
                //
                // FIXME: DotLoc-1 is not correct because of something like this:
                //
                // t/\
                // .3
                //
                TheByteDecoder->setSourceLocation(DotLoc-1);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                
                TheByteDecoder->setSourceLocation(DotLoc);
                TheCharacterDecoder->setWLCharacterStart();
                TheCharacterDecoder->setWLCharacterEnd();
                _currentWLCharacter = DotChar;
                
                append(c, Source(Loc, Loc));
                
            } else {
                
                Operator = TOKEN_SLASHDOT; // /.
                
                String << DotChar;
            }
        }
            break;
        case '/': {
            Operator = TOKEN_SLASHSLASH; // //
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            switch (c.to_point()) {
                case '.': {
                    Operator = TOKEN_SLASHSLASHDOT; // //.
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                }
                    break;
                case '@': {
                    Operator = TOKEN_SLASHSLASHAT; // //@
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                }
                    break;
            }
        }
            break;
        case ':': {
            Operator = TOKEN_SLASHCOLON; // /:
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '=': {
            Operator = TOKEN_SLASHEQUAL; // /=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '*': {
            Operator = TOKEN_SLASHSTAR; // /*
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleAt() {
    
    //
    // Could be @ or \[RawAt]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_AT; // @
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    switch (c.to_point()) {
        case '@': {
            Operator = TOKEN_ATAT; // @@
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '@') {
                
                Operator = TOKEN_ATATAT; // @@@
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '*': {
            Operator = TOKEN_ATSTAR; // @*
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handlePlus() {
    
    //
    // Could be + or \[RawPlus]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_PLUS; // +
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    switch (c.to_point()) {
        case '+': {
            Operator = TOKEN_PLUSPLUS; // ++
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '=') {
                
                //
                // Something like a++=0
                //
                
                auto Loc = TheByteDecoder->getSourceLocation();
                
                std::vector<CodeActionPtr> Actions;
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(Loc), " ")));
                
                auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``+`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc), 1.0, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
            }
        }
            break;
        case '=': {
            Operator = TOKEN_PLUSEQUAL; // +=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleTilde() {
    
    //
    // Could be ~ or \[RawTilde]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_TILDE; // ~
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    if (c.to_point() == '~') {
        
        Operator = TOKEN_TILDETILDE; // ~~
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleQuestion() {
    
    //
    // Could be ? or \[RawQuestion]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_QUESTION; // ?
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    if (c.to_point() == '?') {
        
        Operator = TOKEN_QUESTIONQUESTION; // ??
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleStar() {
    
    //
    // Could be * or \[RawStar]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_STAR; // *
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    switch (c.to_point()) {
        case '=': {
            Operator = TOKEN_STAREQUAL; // *=
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '*': {
            Operator = TOKEN_STARSTAR; // **
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleCaret() {
    
    //
    // Could be ^ or \[RawWedge]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_UNKNOWN;
    
    auto CaretChar = c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    switch (c.to_point()) {
        case ':': {
            
            auto ColonChar = c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '=') {
                
                Operator = TOKEN_CARETCOLONEQUAL; // ^:=
                
                String << CaretChar;
                String << ColonChar;
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
            } else {
                
                Operator = TOKEN_ERROR_EXPECTEDEQUAL;
                
                String << CaretChar;
                String << ColonChar;
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '=': {
            Operator = TOKEN_CARETEQUAL; // ^=
            
            String << CaretChar;
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        default: {
            Operator = TOKEN_CARET; // ^
            
            String << CaretChar;
        }
            break;
    }
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleSingleQuote() {
    
    //
    // Could be ' or \[RawQuote]
    //
    
    auto c = currentWLCharacter();
    
    auto Operator = TOKEN_SINGLEQUOTE; // '
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handlePunctuationCharacter() {
    
    auto c = currentWLCharacter();
    
    String << c;
    
    auto Operator = LongNameCodePointToOperator(c.to_point());
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(Operator, String.str(), getTokenSource());
}

inline void Tokenizer::handleUnhandledBackSlash() {
    
    //
    // Unhandled \
    //
    // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
    // This duplicates some logic in CharacterDecoder
    //
    
    auto c = currentWLCharacter();
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    switch (c.to_point()) {
        case '[': {
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            while (true) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isAlphaOrDigit()) {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                } else {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    break;
                }
            }
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
        case ':': {
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            for (auto i = 0; i < 4; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                } else {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    break;
                }
            }
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
        case '.': {
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            for (auto i = 0; i < 2; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                } else {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    break;
                }
            }
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            for (auto i = 0; i < 3; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isOctal()) {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                } else {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    break;
                }
            }
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
        case '|': {
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            for (auto i = 0; i < 6; i++) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                if (c.isHex()) {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                } else {
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    break;
                }
            }
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
        case CODEPOINT_ENDOFFILE: {
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
        default: {
            
            //
            // Nothing special, just read next character
            //
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
        }
    }
}

inline void Tokenizer::handleUninterpretable() {
    
    //
    // Uninterpretable
    //
    // Something like \[ErrorIndicator]
    //
    
    auto c = currentWLCharacter();
    
    String << c;
    
    c = nextWLCharacter(TOPLEVEL);
    
    _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), getTokenSource());
}

void Tokenizer::addIssue(std::unique_ptr<Issue> I) {
    Issues.push_back(std::move(I));
}

std::vector<std::unique_ptr<Issue>>& Tokenizer::getIssues() {
    return Issues;
}

std::unique_ptr<Tokenizer> TheTokenizer = nullptr;



// Convert value_ to the digit that it represents
//
int toDigit(int val) {
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
            return -1;
    }
}





