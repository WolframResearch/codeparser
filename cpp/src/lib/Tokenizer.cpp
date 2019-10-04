
#include "Tokenizer.h"

#include "CharacterDecoder.h"
#include "SourceManager.h"
#include "Utils.h"
#include "CodePoint.h"

int toDigit(int val);


Tokenizer::Tokenizer() : stringifyNextToken_symbol(false), stringifyNextToken_file(false), _currentToken(Token(TOKEN_UNKNOWN, "", Source())), _currentWLCharacter(0), wlCharacterQueue(), String(), Issues() {}

void Tokenizer::init(SourceStyle style, bool stringifyNextTokenSymbol, bool skipFirstLine) {
    
    stringifyNextToken_symbol = stringifyNextTokenSymbol;
    stringifyNextToken_file = false;
    _currentToken = Token(TOKEN_UNKNOWN, "", Source(style));
    
    _currentWLCharacter = WLCharacter(0);
    wlCharacterQueue.clear();
    
    String.str("");
    
    Issues.clear();
    
    auto c = TheCharacterDecoder->nextWLCharacter();
    
    if (skipFirstLine) {
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            //
            // What line-like characters?
            //
            if (c.isNewline() || c.isNewlineCharacter() ||
                c.to_point() == CODEPOINT_ENDOFFILE ||
                c.isLineContinuation()) {
                
                c = TheCharacterDecoder->nextWLCharacter();
                break;
            }
            
            c = TheCharacterDecoder->nextWLCharacter();
        }
    }
    
    _currentWLCharacter = c;
    
    TokenizerContext tokenizerCtxt;
    nextToken(tokenizerCtxt);
}

void Tokenizer::deinit() {
    
    wlCharacterQueue.clear();
    
    String.str("");
    
    Issues.clear();
}


void Tokenizer::nextToken(TokenizerContext CtxtIn) {
    
    if (_currentToken.Tok == TOKEN_ENDOFFILE) {
        return;
    }
    
    //
    // Too complicated to clear string when calling getString and assert here
    //
    // assert(String.str().empty());
    //
    String.str("");
    
    auto Ctxt = CtxtIn;
    
    TheSourceManager->setTokenStart();
    
    auto c = currentWLCharacter();
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        
        //
        // EndOfFile is special because there is no source
        //
        // So invent source
        //
        
        auto Start = TheSourceManager->getTokenStart();
        
        if ((Ctxt & TOKENIZER_STRINGIFY_CURRENT_LINE) == TOKENIZER_STRINGIFY_CURRENT_LINE) {
            
            _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
            
        } else if (stringifyNextToken_symbol) {
            
            stringifyNextToken_symbol = false;
            
            _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
            
        } else if (stringifyNextToken_file) {
            
            stringifyNextToken_file = false;
            
            _currentToken = Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
            
        } else {
            
            _currentToken = Token(TOKEN_ENDOFFILE, String.str(), Source(Start));
        }
        return;
        
    } else if ((Ctxt & TOKENIZER_STRINGIFY_CURRENT_LINE) == TOKENIZER_STRINGIFY_CURRENT_LINE) {
        
        _currentToken = handleString(Ctxt);
        
        return;
        
    } else if (stringifyNextToken_symbol) {
        
        _currentToken = handleString(Ctxt);
        
        return;
        
    } else if (stringifyNextToken_file) {
        
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
            
            _currentToken = Token(TOKEN_WHITESPACE, String.str(), TheSourceManager->getTokenSource());
            
            return;
        }
        
        _currentToken = handleString(Ctxt);
        
        return;
    }
    
    //
    // All of stringification happened above.
    // Now for actual tokens.
    //
    
    if (c.isLetterlike()) {
        
        _currentToken = handleSymbol(Ctxt);
        
    } else if (c.to_point() == '`') {
        
        _currentToken = handleSymbol(Ctxt);
        
    } else if (c.to_point() == '"') {
        
        _currentToken = handleString(Ctxt);
        
    } else if (c.isDigit()) {
        
        _currentToken = handleNumber(Ctxt);
        
    } else if (c.isNewline()) {
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_NEWLINE, String.str(), TheSourceManager->getTokenSource());
        
    } else if (c.isSpace()) {
        
        if (c.isStrangeSpace()) {
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange space character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
            
            Issues.push_back(std::move(I));
        }
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_WHITESPACE, String.str(), TheSourceManager->getTokenSource());
        
    } else if (c.isPunctuation() && c.to_point() != '\\') {
        
        assert(c.to_point() != '$'); // handled in handleSymbol
        assert(c.to_point() != '`'); // handled in handleSymbol
        assert(c.to_point() != '"'); // handled in handleString
        
        _currentToken = handleOperator(Ctxt);
        
    } else if (c.isLineContinuation()) {
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_LINECONTINUATION, String.str(), TheSourceManager->getTokenSource());
        
    }
    //
    // Everything else involving Unicode or errors
    //
    else if (c.isLetterlikeCharacter()) {
        
        _currentToken = handleSymbol(Ctxt);
        
    } else if (c.isSpaceCharacter()) {
        
        if (c.isStrangeSpaceCharacter()) {
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange space character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
            
            Issues.push_back(std::move(I));
        }
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_WHITESPACE, String.str(), TheSourceManager->getTokenSource());
        
    } else if (c.isNewlineCharacter()) {
        
        if (c.isStrangeNewlineCharacter()) {
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange newline character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
            
            Issues.push_back(std::move(I));
        }
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_NEWLINE, String.str(), TheSourceManager->getTokenSource());
        
    } else if (c.isLinearSyntax()) {
        
        _currentToken = handleLinearSyntax(Ctxt);
        
    } else if (c.isPunctuationCharacter()) {
        
        _currentToken = handleOperator(Ctxt);
        
    } else if (c.to_point() == '\\') {
        
        //
        // Unhandled \
        //
        // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
        // This duplicates some logic in CharacterDecoder
        //
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        switch (c.to_point()) {
            case '[': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                while (true) {
                    
                    //
                    // No need to check isAbort() inside decoder loops
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
                
                _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
                break;
            case ':': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                for (auto i = 0; i < 4; i++) {
                    
                    //
                    // No need to check isAbort() inside decoder loops
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
                
                _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
                break;
            case '.': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                for (auto i = 0; i < 2; i++) {
                    
                    //
                    // No need to check isAbort() inside decoder loops
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
                
                _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
                break;
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                for (auto i = 0; i < 3; i++) {
                    
                    //
                    // No need to check isAbort() inside decoder loops
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
                
                _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
                break;
            case '|': {
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                for (auto i = 0; i < 6; i++) {
                    
                    //
                    // No need to check isAbort() inside decoder loops
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
                
                _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
                break;
            default: {
                
                //
                // Nothing special, just read next character
                //
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
                
                _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
                break;
        }
        
    } else {
        
        //
        // Unhandled
        //
        // Something like \[ErrorIndicator]
        //
        
        String << c;
        
        c = nextWLCharacter(TOPLEVEL);
        
        _currentToken = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
    }
}

WLCharacter Tokenizer::nextWLCharacter(NextWLCharacterPolicy policy) {
    
    //
    // handle the queue before anything else
    //
    // Unlike ByteDecoder and CharacterDecoder, the WLCharacters in the queue may be part of a Token with multiple WLCharacters
    //
    if (!wlCharacterQueue.empty()) {
        
        auto p = wlCharacterQueue[0];
        
        //
        // Make sure to set source information
        //
        TheSourceManager->setSourceLocation(p.second.start());
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setSourceLocation(p.second.end());
        TheSourceManager->setWLCharacterEnd();
        
        // erase first
        wlCharacterQueue.erase(wlCharacterQueue.begin());
        
        _currentWLCharacter = p.first;
        
        return _currentWLCharacter;
    }
    
    _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
    
    while (_currentWLCharacter.isLineContinuation()) {
        
        if ((policy & LC_IS_MEANINGFUL) != LC_IS_MEANINGFUL) {
            
            //
            // Line continuation is NOT meaningful, so warn and break out of loop
            //
            
            auto CharacterStart = TheSourceManager->getWLCharacterStart();
            
            auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_STRAYLINECONTINUATION, std::string("Stray line continuation."), FORMATISSUESEVERITY_FORMATTING, Source(CharacterStart)));
            
            Issues.push_back(std::move(I));
            
            if ((policy & PRESERVE_WS_AFTER_LC) != PRESERVE_WS_AFTER_LC) {
                
                _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
                while (_currentWLCharacter.isSpace()) {
                    _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
                }
            }
            
            break;
            
        } else {
            
            //
            // Line continuation IS meaningful, so save in current String and continue
            //
            
            String << _currentWLCharacter;
            
            _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
            
            if ((policy & PRESERVE_WS_AFTER_LC) != PRESERVE_WS_AFTER_LC) {
                
                while (_currentWLCharacter.isSpace()) {
                    _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
                }
            }
        }
    }
    
    return _currentWLCharacter;
}

void Tokenizer::append(WLCharacter c, Source Span) {
    wlCharacterQueue.push_back(std::make_pair(c, Span));
}

WLCharacter Tokenizer::currentWLCharacter() const {
    
    return _currentWLCharacter;
}

Token Tokenizer::currentToken() {
    
    assert(_currentToken.Tok != TOKEN_UNKNOWN);
    
    return _currentToken;
}

Token Tokenizer::handleLinearSyntax(TokenizerContext Ctxt) {
    
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
    
    return Token(Operator, String.str(), TheSourceManager->getTokenSource());
}

Token Tokenizer::handleComment(TokenizerContext Ctxt) {
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
        return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, String.str(), TheSourceManager->getTokenSource());
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
            
            return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, String.str(), TheSourceManager->getTokenSource());
            
        } else {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_COMMENT);
        }
        
    } // while
    
    return Token(TOKEN_COMMENT, String.str(), TheSourceManager->getTokenSource());
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
Token Tokenizer::handleSymbol(TokenizerContext Ctxt) {
    
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
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow **`** characters.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
            
            Issues.push_back(std::move(I));
        }
        
        String << c;
        
        c = nextWLCharacter(INSIDE_SYMBOL);
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            handleSymbolSegment(Ctxt);
            
        } else {
            return Token(TOKEN_OTHER, String.str(), TheSourceManager->getTokenSource());
        }
        
        c = currentWLCharacter();
        
    } // while
    
    return Token(TOKEN_SYMBOL, String.str(), TheSourceManager->getTokenSource());
}

void Tokenizer::handleSymbolSegment(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    assert(c.isLetterlike() || c.isLetterlikeCharacter());
    
    if (c.to_point() == '$') {
        
        if ((Ctxt & TOKENIZER_SLOT) == TOKENIZER_SLOT) {
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow ``$`` characters.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
            
            Issues.push_back(std::move(I));
        }
    }
    
    if (c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter()) {
        
        auto Src = TheSourceManager->getWLCharacterSource();
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange character in symbol: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        Issues.push_back(std::move(I));
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
                    
                    auto Src = TheSourceManager->getWLCharacterSource();
                    
                    auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow ``$`` characters.", SYNTAXISSUESEVERITY_REMARK, Src, 0.33, {}));
                    
                    Issues.push_back(std::move(I));
                }
            }
            
            if (c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter()) {
                
                auto Src = TheSourceManager->getWLCharacterSource();
                
                auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange character in symbol: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
                
                Issues.push_back(std::move(I));
            }
            
            String << c;
            
            c = nextWLCharacter(INSIDE_SYMBOL);
            
        } else {
            break;
        }
        
    } // while
}

Token Tokenizer::handleString(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    if ((Ctxt & TOKENIZER_STRINGIFY_CURRENT_LINE) == TOKENIZER_STRINGIFY_CURRENT_LINE) {
        
        auto lastGoodLocation = TheSourceManager->getSourceLocation();
        
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
            
            lastGoodLocation = TheSourceManager->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_STRING);
            
        } // while
        
        nextWLCharacter(TOPLEVEL);
        
        stringifyNextToken_symbol = false;
        stringifyNextToken_file = false;
        
        if (empty) {
            
            //
            // Something like   ?EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            
            auto Start = TheSourceManager->getTokenStart();
            
            return Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        }
        
        //
        // ?? syntax is special because we want to ignore the newline that was read.
        //
        // So invent source
        //
        
        return Token(TOKEN_STRING, String.str(), Source(TheSourceManager->getTokenStart(), lastGoodLocation));
        
    } else if (stringifyNextToken_symbol && c.to_point() != '"') {
        
        //
        // magically turn into a string
        //
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            stringifyNextToken_symbol = false;
            
            handleSymbolSegment(Ctxt);
            
            return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSource());
            
        } else {
            
            //
            // Something like   a::5
            //
            
            stringifyNextToken_symbol = false;
            
            nextToken(Ctxt);
            
            return Token(TOKEN_OTHER, String.str(), TheSourceManager->getTokenSource());
        }
        
    } else if (stringifyNextToken_file && c.to_point() != '"') {
        
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
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
            } else if (c.to_point() == '[') {
                
                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
                
                empty = false;
                
                auto res = handleFileOpsBrackets(Ctxt);
                if (res.Tok != TOKEN_STRING) {
                    return res;
                }
                
                c = currentWLCharacter();
                
            } else {
                
                break;
            }
            
        } // while
        
        stringifyNextToken_file = false;
        
        if (empty) {
            
            //
            // Something like   <<EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            
            auto Start = TheSourceManager->getTokenStart();
            
            return Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start));
        }
        
        return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSource());
        
    } else {
        
        assert(c.to_point() == '"');
        
        String << c;
        
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            c = nextWLCharacter(INSIDE_STRING);
            
            if (c.to_point() == CODEPOINT_ENDOFFILE) {
                
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSource());
                
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
        
        stringifyNextToken_symbol = false;
        stringifyNextToken_file = false;
        
        return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSource());
    }
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
Token Tokenizer::handleFileOpsBrackets(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    assert(c.to_point() == '[');
    
    String << c;
    
    auto depth = 1;
    
    c = nextWLCharacter(INSIDE_STRING_FILEIFY);
    
    if (c.to_point() == CODEPOINT_ENDOFFILE) {
        return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSource());
    }
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (c.to_point() == '[') {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_STRING_FILEIFY);
            
            if (c.to_point() == CODEPOINT_ENDOFFILE) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSource());
            }
            
            depth = depth + 1;
            
        } else if (c.to_point() == ']') {
            
            String << c;
            
            depth = depth - 1;
            
            if (depth == 0) {
                
                // Leaving brackets, make sure to grab next character
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
                break;
                
            } else {
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
                if (c.to_point() == CODEPOINT_ENDOFFILE) {
                    return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSource());
                }
            }
            
        } else {
            
            String << c;
            
            if (c.isSpace() || c.isNewline() || c.isSpaceCharacter() || c.isNewlineCharacter()) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSource());
            }
            
            c = nextWLCharacter(INSIDE_STRING_FILEIFY);
            
            if (c.to_point() == CODEPOINT_ENDOFFILE) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSource());
            }
        }
        
    } // while
    
    return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSource());
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
Token Tokenizer::handleNumber(TokenizerContext Ctxt) {
    
    int base = 0;
    
    handleDigits(Ctxt);
    
    auto c = currentWLCharacter();
    
    //
    // Could be 16^^blah
    //
    if (c.to_point() == '^') {
        
        auto Caret1Char = c;
        
        auto Caret1Loc = TheSourceManager->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c.to_point() == '^') {
            
            //
            // Something like 2^^
            //
            // Must be a number
            //
            
            base = Utils::parseInteger(String.str(), 10);
            
            if (base < 2 || base > 36) {
                
                //
                // Something like 37^^2, which is an invalid base
                //
                
                String << Caret1Char;
                String << c;
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                return Token(TOKEN_ERROR_INVALIDBASE, String.str(), TheSourceManager->getTokenSource());
            }
            
            String << Caret1Char;
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            if (c.isDigit() || c.isAlpha()) {
                
                auto handle = handleDigitsOrAlpha(Ctxt, base);
                if (handle == -1) {
                    
                    return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
                }
                
            } else if (c.to_point() != '.') {
                
                //
                // Make sure that bad character is added to String
                //
                String << c;
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
            }
            
        } else {
            
            //
            // Something like  2^a
            //
            // Must now do surgery and back up
            //
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            //
            // FIXME: CaretLoc-1 is not correct because of something like this:
            //
            // 2\
            // ^a
            //
            TheSourceManager->setSourceLocation(Caret1Loc-1);
            TheSourceManager->setWLCharacterStart();
            TheSourceManager->setWLCharacterEnd();
            
            TheSourceManager->setSourceLocation(Caret1Loc);
            TheSourceManager->setWLCharacterStart();
            TheSourceManager->setWLCharacterEnd();
            _currentWLCharacter = Caret1Char;
            
            append(c, Src);
            
            return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSource());
        }
    }
    
    c = currentWLCharacter();
    
    bool real = false;
    
    if (c.to_point() == '.') {
        
        auto handle = handleFractionalPart(Ctxt, base);
        if (handle == -1) {
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSource());
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
        
        auto TickLoc = TheSourceManager->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        bool accuracy = false;
        if (c.to_point() == '`') {
            
            String << c;
            
            TickLoc = TheSourceManager->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            accuracy = true;
        }
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            //
            // Something like 1.2`a
            //
            
            auto Loc2 = TheSourceManager->getSourceLocation();
            
            auto cGraphicalStr = c.graphicalString();
            
            //
            // Use ** markup syntax here because of ` character
            //
            auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between **`** and ``" + cGraphicalStr + "`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc2)));
            
            Issues.push_back(std::move(I));
        }
        
        if (accuracy || c.isDigit() || c.to_point() == '-' || c.to_point() == '+' || c.to_point() == '.') {
            
            if (c.to_point() == '-' || c.to_point() == '+') {
                
                auto s = c;
                
                auto SignLoc = TheSourceManager->getSourceLocation();
                
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
                    
                    return Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), TheSourceManager->getTokenSource());
                    
                } else {
                    
                    //
                    // Something like 1.2`->3
                    //
                    // Must now do surgery and back up
                    //
                    
                    std::string msg;
                    if (s.to_point() == '-') {
                        msg = "Put a space between **`** and ``-`` to reduce ambiguity";
                    } else {
                        msg = "Put a space between **`** and ``+`` to reduce ambiguity";
                    }
                    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, msg, FORMATISSUESEVERITY_FORMATTING, Source(SignLoc)));
                    
                    Issues.push_back(std::move(I));
                    
                    
                    
                    auto Src = TheSourceManager->getWLCharacterSource();
                    
                    //
                    // FIXME: SignLoc-1 is not correct because of something like this:
                    //
                    // 1.2`\
                    // ->3
                    //
                    TheSourceManager->setSourceLocation(SignLoc-1);
                    TheSourceManager->setWLCharacterStart();
                    TheSourceManager->setWLCharacterEnd();
                    
                    TheSourceManager->setSourceLocation(SignLoc);
                    TheSourceManager->setWLCharacterStart();
                    TheSourceManager->setWLCharacterEnd();
                    _currentWLCharacter = s;
                    
                    append(c, Src);
                    
                    return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSource());
                }
            }
            
            bool supplied = false;
            
            if (c.isDigit()) {
                
                handleDigits(Ctxt);
                
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
                    auto DotLoc = TheSourceManager->getSourceLocation();
                    
                    // look ahead
                    auto NextChar = nextWLCharacter(INSIDE_NUMBER);
                    
                    auto Src = TheSourceManager->getWLCharacterSource();
                    
                    //
                    // Must now do surgery and back up
                    //
                    
                    TheSourceManager->setSourceLocation(DotLoc-1);
                    TheSourceManager->setWLCharacterStart();
                    TheSourceManager->setWLCharacterEnd();
                    
                    TheSourceManager->setSourceLocation(DotLoc);
                    TheSourceManager->setWLCharacterStart();
                    TheSourceManager->setWLCharacterEnd();
                    _currentWLCharacter = DotChar;
                    
                    append(NextChar, Src);
                    
                    if (!NextChar.isDigit()) {
                        
                        if (accuracy) {
                            
                            //
                            // Something like  123``.EOF
                            //
                            
                            String << DotChar;
                            String << NextChar;
                            
                            // this grabs the Dot character
                            c = nextWLCharacter(INSIDE_NUMBER);
                            // this grabs the next character
                            c = nextWLCharacter(INSIDE_NUMBER);
                            
                            return Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), TheSourceManager->getTokenSource());
                            
                        } else {
                            
                            //
                            // Something like  123`.xxx  where the . could be a Dot operator
                            //
                            
                            return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSource());
                        }
                    }
                }
                
                // actual decimal point
                
                auto handle = handleFractionalPart(Ctxt, 0);
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
                    
                    return Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), TheSourceManager->getTokenSource());
                }
            }
        }
    }
    
    c = currentWLCharacter();
    
    if (c.to_point() == '*') {
        
        auto StarChar = c;
        
        auto StarLoc = TheSourceManager->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c.to_point() == '^') {
            
            String << StarChar;
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            if (c.to_point() == '-' || c.to_point() == '+') {
                
                String << c;
                
                c = nextWLCharacter(INSIDE_NUMBER);
            }
            
            if (!expectDigits(Ctxt)) {
                
                //
                // Something like 123*^EOF
                //
                
                String << c;
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                return Token(TOKEN_ERROR_EXPECTEDEXPONENT, String.str(), TheSourceManager->getTokenSource());
            }
            
            c = currentWLCharacter();
            
            if (c.to_point() == '.') {
                
                //
                // Something like 123*^0.5
                //
                
                String << c;
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                return Token(TOKEN_ERROR_EXPECTEDEXPONENT, String.str(), TheSourceManager->getTokenSource());
            }
            
        } else {
            
            //
            // Something like 1*a
            //
            // Must now do surgery and back up
            //
            
            auto Src = TheSourceManager->getWLCharacterSource();
            
            //
            // FIXME: StarLoc-1 is not correct because of something like this:
            //
            // 1\
            // *a
            //
            TheSourceManager->setSourceLocation(StarLoc-1);
            TheSourceManager->setWLCharacterStart();
            TheSourceManager->setWLCharacterEnd();
            
            TheSourceManager->setSourceLocation(StarLoc);
            TheSourceManager->setWLCharacterStart();
            TheSourceManager->setWLCharacterEnd();
            _currentWLCharacter = StarChar;
            
            append(c, Src);
            
            if (real) {
                return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSource());
            } else {
                return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSource());
            }
        }
    }
    
    if (real) {
        return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSource());
    } else {
        return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSource());
    }
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
// Note: if 0 digits, then the . is also not added to String
//
int Tokenizer::handleFractionalPart(TokenizerContext Ctxt, int base) {
    
    auto c = currentWLCharacter();
    
    assert(c.to_point() == '.');
    
    auto DotChar1 = c;
    
    auto DotLoc1 = TheSourceManager->getSourceLocation();
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    if (c.to_point() == '.') {
        
        //
        // Something like 0..
        //
        // Must now do surgery and back up
        //
        
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(DotLoc1), " ")));
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SPACE, "Suspicious syntax", SYNTAXISSUESEVERITY_REMARK, Source(DotLoc1), 0.90, std::move(Actions)));
        
        Issues.push_back(std::move(I));
        
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        //
        // FIXME: DotLoc-1 is not correct because of something like this:
        //
        // 0\
        // ..
        //
        TheSourceManager->setSourceLocation(DotLoc1-1);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        TheSourceManager->setSourceLocation(DotLoc1);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        _currentWLCharacter = DotChar1;
        
        append(c, Source(Loc, Loc));
        
        return false;
    }
    
    String << DotChar1;
    
    auto handle = 0;
    if (c.isDigit() || c.isAlpha()) {
        
        handle = handleDigitsOrAlpha(Ctxt, base);
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
        
        auto Loc2 = TheSourceManager->getSourceLocation();
        
        std::vector<CodeActionPtr> Actions;
        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(Loc2), " ")));
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SPACE, "Suspicious syntax", SYNTAXISSUESEVERITY_ERROR, Source(Loc2), 0.99, std::move(Actions)));
        
        Issues.push_back(std::move(I));
    }
    
    return handle;
}

bool Tokenizer::expectDigits(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    if (c.isDigit()) {
        
        handleDigits(Ctxt);
        
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
size_t Tokenizer::handleDigits(TokenizerContext Ctxt) {
    
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
int Tokenizer::handleDigitsOrAlpha(TokenizerContext Ctxt, int base) {
    
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

Token Tokenizer::handleOperator(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    TokenEnum Operator = TOKEN_UNKNOWN;
    
    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_COLON; // :
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            switch (c.to_point()) {
                case ':': {
                    Operator = TOKEN_COLONCOLON; // ::
                    
                    String << c;
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    if ((Ctxt & TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) == TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) {
                        stringifyNextToken_symbol = true;
                    }
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
        }
            break;
        case '(': {
            Operator = TOKEN_OPENPAREN; // (
            
            auto ParenChar = c;
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            //
            // Comments must start literally with (*
            // Escaped characters do not work
            //
            if (ParenChar == WLCharacter('(') &&
                c == WLCharacter('*')) {
                
                return handleComment(Ctxt);
            }
        }
            break;
        case ')': {
            Operator = TOKEN_CLOSEPAREN; // )
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '[': {
            
            Operator = TOKEN_OPENSQUARE; // [
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case ']': {
            Operator = TOKEN_CLOSESQUARE; // ]
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case ',': {
            Operator = TOKEN_COMMA; // ,
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '{': {
            Operator = TOKEN_OPENCURLY; // {
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '}': {
            Operator = TOKEN_CLOSECURLY; // }
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        case '.': {
            
            //
            // handleDot
            // Could be  .  or  ..  or ...  or  .0
            //
            
            auto DotChar = c;
            
            auto DotLoc = TheSourceManager->getSourceLocation();
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.isDigit()) {
                
                //
                // Something like .0
                //
                // Must now do surgery and back up (and go to handleNumber instead)
                //
                
                auto Loc = TheSourceManager->getSourceLocation();
                
                //
                // FIXME: DotLoc-1 is not correct because of something like this:
                //
                // .0
                //
                TheSourceManager->setSourceLocation(DotLoc-1);
                TheSourceManager->setWLCharacterStart();
                TheSourceManager->setWLCharacterEnd();
                
                TheSourceManager->setSourceLocation(DotLoc);
                TheSourceManager->setWLCharacterStart();
                TheSourceManager->setWLCharacterEnd();
                _currentWLCharacter = DotChar;
                
                append(c, Source(Loc));
                
                return handleNumber(Ctxt);
            }
            
            String << DotChar;
            
            Operator = TOKEN_DOT; // .
            
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
        }
            break;
        case '=': {
            Operator = TOKEN_EQUAL; // =
            
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
                    
                    auto DotLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter(INSIDE_OPERATOR);
                    
                    if (c.isDigit()) {
                        
                        //
                        // Something like x=.0
                        //
                        // Must now do surgery and back up
                        //
                        
                        auto Src = TheSourceManager->getWLCharacterSource();
                        
                        //
                        // FIXME: DotLoc-1 is not correct because of something like this:
                        //
                        // x=.\
                        // 0
                        //
                        TheSourceManager->setSourceLocation(DotLoc-1);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        
                        TheSourceManager->setSourceLocation(DotLoc);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
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
                    
                    auto BangLoc = TheSourceManager->getSourceLocation();
                    
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
                        
                        auto Src = TheSourceManager->getWLCharacterSource();
                        
                        //
                        // FIXME: BangLoc-1 is not correct because of something like this:
                        //
                        // x=\
                        // !y
                        //
                        TheSourceManager->setSourceLocation(BangLoc-1);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        
                        TheSourceManager->setSourceLocation(BangLoc);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        _currentWLCharacter = BangChar;
                        
                        append(c, Src);
                        
                        Operator = TOKEN_EQUAL;
                    }
                }
                    break;
            }
        }
            break;
        case '_': {
            Operator = TOKEN_UNDER; // _
            
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
                    
                    auto DotLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter(INSIDE_OPERATOR);
                    
                    if (c.to_point() == '.') {
                        
                        //
                        // Something like  _...
                        //
                        // Must now do surgery and back up
                        //
                        
                        std::vector<CodeActionPtr> Actions;
                        Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert space", Source(DotLoc), " ")));
                        
                        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SPACE, "Suspicious syntax", SYNTAXISSUESEVERITY_REMARK, Source(DotLoc), 0.95, std::move(Actions)));
                        
                        Issues.push_back(std::move(I));
                        
                        
                        auto Src = TheSourceManager->getWLCharacterSource();
                        
                        //
                        // FIXME: DotLoc-1 is not correct because of something like this:
                        //
                        // _\
                        // ...
                        //
                        TheSourceManager->setSourceLocation(DotLoc-1);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        
                        TheSourceManager->setSourceLocation(DotLoc);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
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
                            
                            auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SPACE, "Suspicious syntax", SYNTAXISSUESEVERITY_WARNING, Source(DotLoc), 0.90, std::move(Actions)));
                            
                            Issues.push_back(std::move(I));
                        }
                        
                        Operator = TOKEN_UNDERDOT; // _.
                    }
                }
                    break;
            }
            
            return Token(Operator, String.str(), TheSourceManager->getTokenSource());
        }
            break;
        case '<': {
            Operator = TOKEN_LESS; // <
            
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
                    
                    if ((Ctxt & TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) == TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) {
                        stringifyNextToken_file = true;
                    }
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
                    
                    auto MinusLoc = TheSourceManager->getSourceLocation();
                    
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
                        
                        auto Src = TheSourceManager->getWLCharacterSource();
                        
                        //
                        // FIXME: MinusLoc-1 is not correct because of something like this:
                        //
                        // a<\
                        // -4
                        //
                        TheSourceManager->setSourceLocation(MinusLoc-1);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        
                        TheSourceManager->setSourceLocation(MinusLoc);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        _currentWLCharacter = MinusChar;
                        
                        append(c, Src);
                        
                        Operator = TOKEN_LESS;
                    }
                }
                    break;
            }
        }
            break;
        case '>': {
            Operator = TOKEN_GREATER; // >
            
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
                    
                    if ((Ctxt & TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) == TOKENIZER_ENABLE_STRINGIFY_NEXT_TOKEN) {
                        stringifyNextToken_file = true;
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
        }
            break;
        case '-': {
            Operator = TOKEN_MINUS; // -
            
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
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``>`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                        
                        Issues.push_back(std::move(I));
                        
                    } else if (c.to_point() == '=') {
                        
                        //
                        // Something like a--=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``-`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                        
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
        }
            break;
        case '|': {
            Operator = TOKEN_BAR; // |
            
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
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``>`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                        
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
        }
            break;
        case ';': {
            Operator = TOKEN_SEMI; // ;
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == ';') {
                
                Operator = TOKEN_SEMISEMI; // ;;
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '!': {
            Operator = TOKEN_BANG; // !
            
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
        }
            break;
        case '#': {
            
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
                
                handleDigits(Ctxt);
                
            } else if (c.isLetterlike() || c.isLetterlikeCharacter()) {
                
                Operator = TOKEN_HASH; // #
                
                Ctxt |= TOKENIZER_SLOT;
                
                handleSymbol(Ctxt);
                
                Ctxt.clear(TOKENIZER_SLOT);
                
            } else if (c.to_point() == '`') {
                
                Operator = TOKEN_HASH; // #
                
                Ctxt |= TOKENIZER_SLOT;
                
                handleSymbol(Ctxt);
                
                Ctxt.clear(TOKENIZER_SLOT);
                
            } else if (c.to_point() == '"') {
                
                auto Loc = TheSourceManager->getSourceLocation();
                
                Operator = TOKEN_HASH; // #
                
                handleString(Ctxt);
                
                auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow ``\"`` characters.", SYNTAXISSUESEVERITY_REMARK, Source(Loc), 0.33, {}));
                
                Issues.push_back(std::move(I));
                
            } else if (c.to_point() == '#') {
                
                Operator = TOKEN_HASHHASH; // ##
                
                String << c;
                
                c = nextWLCharacter(INSIDE_OPERATOR);
                
                if (c.isDigit()) {
                    
                    handleDigits(Ctxt);
                }
                
            } else {
                
                Operator = TOKEN_HASH; // #
            }
        }
            break;
        case '%': {
            Operator = TOKEN_PERCENT; // %
            
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
                
                handleDigits(Ctxt);
            }
        }
            break;
        case '&': {
            Operator = TOKEN_AMP; // &
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c.to_point() == '&') {
                
                Operator = TOKEN_AMPAMP; // &&
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '/': {
            Operator = TOKEN_SLASH; // /
            
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
                    
                    auto DotLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter(TOPLEVEL);
                    
                    if (c.isDigit()) {
                        
                        //
                        // Something like t/.3
                        //
                        // Must now do surgery and back up
                        //
                        
                        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``/`` and ``.`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(DotLoc)));
                        
                        Issues.push_back(std::move(I));
                        
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        //
                        // FIXME: DotLoc-1 is not correct because of something like this:
                        //
                        // t/\
                        // .3
                        //
                        TheSourceManager->setSourceLocation(DotLoc-1);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        
                        TheSourceManager->setSourceLocation(DotLoc);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
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
        }
            break;
        case '@': {
            Operator = TOKEN_AT; // @
            
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
        }
            break;
        case '+': {
            
            Operator = TOKEN_PLUS; // +
            
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
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_SPACE, "Put a space between ``+`` and ``=`` to reduce ambiguity", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
                        
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
        }
            break;
        case '~': {
            Operator = TOKEN_TILDE; // ~
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '~') {
                
                Operator = TOKEN_TILDETILDE; // ~~
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '?': {
            Operator = TOKEN_QUESTION; // ?
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
            
            if (c.to_point() == '?') {
                
                Operator = TOKEN_QUESTIONQUESTION; // ??
                
                String << c;
                
                c = nextWLCharacter(TOPLEVEL);
            }
        }
            break;
        case '*': {
            Operator = TOKEN_STAR; // *
            
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
        }
            break;
        case '^': {
            
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
        }
            break;
        case '\'': {
            Operator = TOKEN_SINGLEQUOTE; // '
            
            String << c;
            
            c = nextWLCharacter(TOPLEVEL);
        }
            break;
        default: {
            
            assert(c.isPunctuationCharacter());
            
            String << c;
            
            Operator = LongNameCodePointToOperator(c.to_point());
            
            c = nextWLCharacter(TOPLEVEL);
        }
    }
    
    return Token(Operator, String.str(), TheSourceManager->getTokenSource());
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





