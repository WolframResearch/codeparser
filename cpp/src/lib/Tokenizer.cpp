
#include "Tokenizer.h"

#include "Utils.h"

Tokenizer::Tokenizer() : stringifyNextToken_symbol(false), stringifyNextToken_file(false), cur(Token(TOKEN_UNKNOWN, "", Source())), _currentWLCharacter(0), characterQueue(), String(), Issues(), totalTimeMicros() {}

void Tokenizer::init(bool skipFirstLine) {

    stringifyNextToken_symbol = false;
    stringifyNextToken_file = false;
    cur = Token(TOKEN_UNKNOWN, "", Source(SourceLocation(), SourceLocation()));

    _currentWLCharacter = WLCharacter(0);
    characterQueue.clear();

    String.str("");

    Issues.clear();
    totalTimeMicros = std::chrono::microseconds::zero();
    
    auto c = TheCharacterDecoder->nextWLCharacter();

    if (skipFirstLine) {
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            if (c == WLCharacter('\n')) {
                c = TheCharacterDecoder->nextWLCharacter();
                break;
            } else if (c == WLCHARACTER_ENDOFFILE) {
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
    
    characterQueue.clear();

    String.str("");

    Issues.clear();
}


Token Tokenizer::nextToken(TokenizerContext CtxtIn) {
    TimeScoper Scoper(&totalTimeMicros);
    
    //
    // Too complicated to clear string when calling getString and assert here
    //
    // assert(String.str().empty());
    //
    String.str("");
    
    auto Ctxt = CtxtIn;
    Ctxt.SlotFlag = false;

    TheSourceManager->setTokenStart();
    
    auto c = currentWLCharacter();
    
    if (Ctxt.StringifyCurrentLine) {
        
        cur = handleString(Ctxt);
        
        return cur;
        
    } else if (stringifyNextToken_symbol) {
        
        cur = handleString(Ctxt);
        
        return cur;
        
    } else if (stringifyNextToken_file) {
        
        //
        // There could be space, something like  << abc
        //
        // or something like:
        // a >>
        //   b
        //
        
        if (c.isSpace() || c.isNewline() || c.isSpaceCharacter() || c.isNewlineCharacter()) {
            
            while (c.isSpace() || c.isNewline() || c.isSpaceCharacter() || c.isNewlineCharacter()) {
                
                //
                // No need to check isAbort() inside tokenizer loops
                //
                
                String << c;
                
                c = nextWLCharacter();
            }
            
            cur = Token(TOKEN_WHITESPACE, String.str(), TheSourceManager->getTokenSpan());
            
            return cur;
        }
        
        cur = handleString(Ctxt);
        
        return cur;
    }
    
    
    
    if (c.isLetterlike()) {
        
        cur = handleSymbol(Ctxt);
        
    } else if (c == WLCharacter('`')) {
        
        cur = handleSymbol(Ctxt);
        
    } else if (c == WLCharacter('"')) {
        
        cur = handleString(Ctxt);
        
    } else if (c.isDigit()) {
        
        cur = handleNumber(Ctxt);
        
    } else if (c.isNewline()) {
        
        String << c;
        
        c = nextWLCharacter();
        
        cur = Token(TOKEN_NEWLINE, String.str(), TheSourceManager->getTokenSpan());
        
    } else if (c.isSpace()) {
        
        //
        // Handle whitespace
        //
        
        while (c.isSpace()) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            String << c;
            
            c = nextWLCharacter();
        }
        
        cur = Token(TOKEN_WHITESPACE, String.str(), TheSourceManager->getTokenSpan());
        
    } else if (c == WLCharacter('.')) {
        
        cur = handleDot(Ctxt);
        
    } else if (c == WLCharacter('\\')) {
        
        String << c;
        
        c = nextWLCharacter();
        
        cur = Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSpan());
        
    } else if (c.isPunctuation()) {
        
        //
        // These are punctuation, but they are handled elsewhere
        //
        assert(c != WLCharacter('$')); // handled in handleSymbol
        assert(c != WLCharacter('`')); // handled in handleSymbol
        assert(c != WLCharacter('"')); // handled in handleString
        assert(c != WLCharacter('.')); // handled in handleDot
        assert(c != WLCharacter('\\')); // handled above
        
        cur = handleOperator(Ctxt);
        
    } else if (c.isEndOfFile()) {
        
        //
        // EndOfFile is special because there is no source
        //
        // So invent source
        //
        auto Start = TheSourceManager->getTokenStart();
        
        cur = Token(TOKEN_ENDOFFILE, String.str(), Source(Start, Start));
    }
    //
    // Everything else involving Unicode or errors
    //
    else if (c.isLetterlikeCharacter()) {
        
        cur = handleSymbol(Ctxt);
        
    } else if (c.isSpaceCharacter()) {
        
        String << c;
        
        c = nextWLCharacter();
        
        cur = Token(TOKEN_WHITESPACE, String.str(), TheSourceManager->getTokenSpan());
        
    } else if (c.isNewlineCharacter()) {
        
        String << c;
        
        c = nextWLCharacter();
        
        cur = Token(TOKEN_NEWLINE, String.str(), TheSourceManager->getTokenSpan());
        
    } else if (c.isLinearSyntax()) {
        
        cur = handleLinearSyntax(Ctxt);
        
    } else if (c.isPunctuationCharacter()) {
        
        cur = handleOperator(Ctxt);
        
    } else {
        
        //
        // Unhandled
        //
        
        String << c;
        
        c = nextWLCharacter();
        
        cur = Token(TOKEN_UNHANDLED, String.str(), TheSourceManager->getTokenSpan());
    }
    
    return cur;
}

WLCharacter Tokenizer::nextWLCharacter(NextCharacterPolicy policy) {

    //
    // handle the queue before anything else
    //
    // Unlike ByteDecoder and CharacterDecoder, the WLCharacters in the queue may be part of a Token with multiple WLCharacters
    //
    if (!characterQueue.empty()) {

        auto p = characterQueue[0];

        //
        // Make sure to set source information
        //
        TheSourceManager->setSourceLocation(p.second.lines.start);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setSourceLocation(p.second.lines.end);
        TheSourceManager->setWLCharacterEnd();

        // erase first
        characterQueue.erase(characterQueue.begin());
        
        _currentWLCharacter = p.first;
        
    } else {
        
        _currentWLCharacter = TheCharacterDecoder->nextWLCharacter(policy);
    }

    return _currentWLCharacter;
}

void Tokenizer::append(WLCharacter c, Source Span) {
    characterQueue.push_back(std::make_pair(c, Span));
}

WLCharacter Tokenizer::currentWLCharacter() const {

    return _currentWLCharacter;
}

Token Tokenizer::currentToken() const {
    
    assert(cur.Tok != TOKEN_UNKNOWN);
    
    return cur;
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
    
    c = nextWLCharacter();

    return Token(Operator, String.str(), TheSourceManager->getTokenSpan());
}

Token Tokenizer::handleComment(TokenizerContext Ctxt) {
    //
    // comment is already started
    //
    
    auto c = TheCharacterDecoder->currentWLCharacter();

    assert(c == WLCharacter('*'));
    
    String << WLCharacter('*');
    
    auto depth = 1;

    c = nextWLCharacter(INSIDE_COMMENT);
    
    if (c == WLCHARACTER_ENDOFFILE) {
        return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, String.str(), TheSourceManager->getTokenSpan());
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
                    
                    c = nextWLCharacter();
                    
                    break;
                }
                    
                c = nextWLCharacter(INSIDE_COMMENT);
            }
            
        } else if (c.isEndOfFile()) {
            
            return Token(TOKEN_ERROR_UNTERMINATEDCOMMENT, String.str(), TheSourceManager->getTokenSpan());
            
        } else {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_COMMENT);
        }

    } // while

    return Token(TOKEN_COMMENT, String.str(), TheSourceManager->getTokenSpan());
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
Token Tokenizer::handleSymbol(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('`') || c.isLetterlike() || c.isLetterlikeCharacter());
    
    if (c.isLetterlike() || c.isLetterlikeCharacter()) {

        handleSymbolSegment(Ctxt);
    }
    
    c = currentWLCharacter();
    
    while (c == WLCharacter('`')) {

        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (Ctxt.SlotFlag) {

            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow **`** characters.", SYNTAXISSUESEVERITY_REMARK, Span);

            Issues.push_back(Issue);
        }

        String << c;
        
        c = nextWLCharacter(INSIDE_SYMBOL);
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            handleSymbolSegment(Ctxt);
            
        } else {
            return Token(TOKEN_ERROR_EXPECTEDLETTERLIKE, String.str(), TheSourceManager->getTokenSpan());
        }
        
        c = currentWLCharacter();
        
    } // while
    
    return Token(TOKEN_SYMBOL, String.str(), TheSourceManager->getTokenSpan());
}

void Tokenizer::handleSymbolSegment(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c.isLetterlike() || c.isLetterlikeCharacter());

    if (c.isDollar()) {
        
        if (Ctxt.SlotFlag) {
            
            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow ``$`` characters.", SYNTAXISSUESEVERITY_REMARK, Span);
            
            Issues.push_back(Issue);
        }
    }
    
    if (c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter()) {
        
        auto Span = TheSourceManager->getWLCharacterSpan();
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange character in symbol: " + Utils::makeGraphical(c.string()), SYNTAXISSUESEVERITY_WARNING, Span);
        
        Issues.push_back(Issue);
    }
    
    String << c;
    
    c = nextWLCharacter(INSIDE_SYMBOL);
    
    auto len = 1;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //

        if (c.isDigit()) {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_SYMBOL);
            
        } else if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            if (c.isDollar()) {
                
                if (Ctxt.SlotFlag) {
                    
                    auto Span = TheSourceManager->getWLCharacterSpan();
                    
                    auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow ``$`` characters.", SYNTAXISSUESEVERITY_REMARK, Span);
                    
                    Issues.push_back(Issue);
                }
            }
            
            if (c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter()) {
                
                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange character in symbol: ``" + Utils::makeGraphical(c.string()) + "``.", SYNTAXISSUESEVERITY_WARNING, Span);
                
                Issues.push_back(Issue);
            }
            
            String << c;
            
            c = nextWLCharacter(INSIDE_SYMBOL);
            
        } else {
            break;
        }
        
        len++;

    } // while
}

Token Tokenizer::handleString(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    if (Ctxt.StringifyCurrentLine) {
        ;
    }
    //
    // Do not check c == WLCharacter('"') here because we want to know if it was escaped
    //
    else if (c.to_point() == '"') {

        //
        // If the beginning " is escaped, then error out
        //
        if (c.isEscaped()) {
            
            String << c;
            
            c = nextWLCharacter();
            
            return Token(TOKEN_ERROR_UNHANDLEDCHARACTER, String.str(), TheSourceManager->getTokenSpan());
        }
        
    } else if (stringifyNextToken_file) {
        ;
    } else if (stringifyNextToken_symbol) {
        ;
    } else {
        assert(false);
    }
    
    if (Ctxt.StringifyCurrentLine) {
        
        auto lastGoodLocation = TheSourceManager->getSourceLocation();
        
        auto len = 0;
        auto empty = true;
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //
            
            c = currentWLCharacter();
            
            if (c.isEndOfFile()) {
                
                break;
                
            } else if (c.isNewline() || c.isNewlineCharacter()) {
                    
                break;
            }
            
            String << c;
            
            empty = false;
            
            len++;
            
            lastGoodLocation = TheSourceManager->getSourceLocation();
            
            c = nextWLCharacter(INSIDE_STRING);
            
        } // while
        
        nextWLCharacter();
        
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
            
            return Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start, Start));
        }
        
        //
        // ?? syntax is special because we want to ignore the newline that was read.
        //
        // So invent source
        //
        
        return Token(TOKEN_STRING, String.str(), Source(TheSourceManager->getTokenStart(), lastGoodLocation));
        
    } else if (stringifyNextToken_symbol && c != WLCharacter('"')) {
        
        //
        // magically turn into a string
        //
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {
            
            handleSymbolSegment(Ctxt);
            
            stringifyNextToken_symbol = false;
            
            return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSpan());
            
        } else {
            
            //
            // Something like   a::EOF
            //
            
            stringifyNextToken_symbol = false;
            
            //
            // EmptyString is special because there is no source
            //
            // So invent source
            //
            auto Start = TheSourceManager->getTokenStart();
            
            return Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start, Start));
        }
        
    } else if (stringifyNextToken_file && c != WLCharacter('"')) {
        
        //
        // magically turn into a string
        //
        
        auto empty = true;
        auto len = 1;
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
            
            if (c.isDigit() || c.isAlpha() || c.isDollar() || c == WLCharacter('`') || c == WLCharacter('/') || c == WLCharacter('.') ||
                c == WLCharacter('\\') || c == WLCharacter('!') || c == WLCharacter('-') || c == WLCharacter('_') ||
                c == WLCharacter(':') || c == WLCharacter('*') || c == WLCharacter('~') || c == WLCharacter('?')) {
                
                empty = false;
                
                String << c;
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
            } else if (c == WLCharacter('[')) {

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
            
            len++;

        } // while
        
        stringifyNextToken_file = false;
        
        if (empty) {
            
            //
            // Something like   a>>EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //
            auto Start = TheSourceManager->getTokenStart();
            
            return Token(TOKEN_ERROR_EMPTYSTRING, String.str(), Source(Start, Start));
        }
        
        return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSpan());
        
    } else {
        
        assert(c == WLCharacter('"'));
        
        String << WLCharacter('"');

        auto len = 0;
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //

            c = nextWLCharacter(INSIDE_STRING);
            
            if (c == WLCHARACTER_ENDOFFILE) {
                
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSpan());
                
            } else
                //
                // OK to check c == WLCharacter('"') here because we only care about un-escaped "
                //
                if (c == WLCharacter('"')) {
                
                break;
                
            }
            
            String << c;
            
            len++;

        } // while
        
        String << WLCharacter('"');
        
        c = nextWLCharacter();
        
        stringifyNextToken_symbol = false;
        stringifyNextToken_file = false;

        return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSpan());
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

    assert(c == WLCharacter('['));
    
    String << WLCharacter('[');
    
    auto depth = 1;

    c = nextWLCharacter(INSIDE_STRING_FILEIFY);
    
    if (c == WLCHARACTER_ENDOFFILE) {
        return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSpan());
    }
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //

        if (c == WLCharacter('[')) {
            
            String << c;
            
            c = nextWLCharacter(INSIDE_STRING_FILEIFY);
            
            if (c == WLCHARACTER_ENDOFFILE) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSpan());
            }
            
            depth = depth + 1;
            
        } else if (c == WLCharacter(']')) {
                
            String << c;
            
            depth = depth - 1;
            
            if (depth == 0) {
                
                // Leaving brackets, make sure to grab next character
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
                break;
                
            } else {
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
                if (c == WLCHARACTER_ENDOFFILE) {
                    return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSpan());
                }
            }

        } else {

            String << c;
            
            if (c.isSpace() || c.isNewline() || c.isSpaceCharacter() || c.isNewlineCharacter()) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSpan());
            }

            c = nextWLCharacter(INSIDE_STRING_FILEIFY);
            
            if (c == WLCHARACTER_ENDOFFILE) {
                return Token(TOKEN_ERROR_UNTERMINATEDSTRING, String.str(), TheSourceManager->getTokenSpan());
            }
        }
        
    } // while

    return Token(TOKEN_STRING, String.str(), TheSourceManager->getTokenSpan());
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
    
    int base = 10;
    
    handleDigits(Ctxt);
    
    auto c = currentWLCharacter();
    
    //
    // Could be 16^^blah
    //
    if (c == WLCharacter('^')) {
        
        auto CaretLoc = TheSourceManager->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c == WLCharacter('^')) {
            
            //
            // Something like 2^^
            //
            // Must be a number
            //
            
            auto Caret2Loc = TheSourceManager->getSourceLocation();
            
            base = Utils::parseInteger(String.str(), 10);
            
            if (base < 2 || base > 36) {
                
                //
                // Something like 37^^2
                //
                
                //
                // FIXME: CaretLoc-1 is not correct because of something like this:
                //
                // 37\
                // ^^a
                //
                TheSourceManager->setSourceLocation(CaretLoc-1);
                TheSourceManager->setWLCharacterStart();
                TheSourceManager->setWLCharacterEnd();
                
                TheSourceManager->setSourceLocation(CaretLoc);
                TheSourceManager->setWLCharacterStart();
                TheSourceManager->setWLCharacterEnd();
                _currentWLCharacter = WLCharacter('^');
                
                append(WLCharacter('^'), Source(Caret2Loc, Caret2Loc));
                
                return Token(TOKEN_ERROR_INVALIDBASE, String.str(), TheSourceManager->getTokenSpan());
            }
            
            String << WLCharacter('^');
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            if (c.isDigit()) {
                
                if (!handleDigitsOrAlpha(Ctxt, base)) {
                    
                    return Token(TOKEN_ERROR_EXPECTEDDIGITORALPHA, String.str(), TheSourceManager->getTokenSpan());
                }
                
            } else if (c.isAlpha()) {
                
                if (!handleDigitsOrAlpha(Ctxt, base)) {
                    
                    return Token(TOKEN_ERROR_EXPECTEDDIGITORALPHA, String.str(), TheSourceManager->getTokenSpan());
                }
                
            } else {
                
                return Token(TOKEN_ERROR_EXPECTEDDIGITORALPHA, String.str(), TheSourceManager->getTokenSpan());
            }
            
        } else {
            
            //
            // Something like  2^a
            //
            // Must now do surgery and back up
            //
            
            auto Span = TheSourceManager->getWLCharacterSpan();
            
            //
            // FIXME: CaretLoc-1 is not correct because of something like this:
            //
            // 2\
            // ^a
            //
            TheSourceManager->setSourceLocation(CaretLoc-1);
            TheSourceManager->setWLCharacterStart();
            TheSourceManager->setWLCharacterEnd();
            
            TheSourceManager->setSourceLocation(CaretLoc);
            TheSourceManager->setWLCharacterStart();
            TheSourceManager->setWLCharacterEnd();
            _currentWLCharacter = WLCharacter('^');
            
            append(c, Span);
            
            return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSpan());
        }
    }
    
    c = currentWLCharacter();
    
    bool real = false;

    if (c == WLCharacter('.')) {
        
        if (!handleFractionalPart(Ctxt, base)) {
            
            return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSpan());
        }

        real = true;
    }
    
    c = currentWLCharacter();
    
    //
    // foo`
    // foo`bar
    // foo``bar
    //
    if (c == WLCharacter('`')) {
        
        real = true;
        
        String << WLCharacter('`');
        
        auto TickLoc = TheSourceManager->getSourceLocation();

        c = nextWLCharacter(INSIDE_NUMBER);
        
        bool accuracy = false;
        if (c == WLCharacter('`')) {
            
            String << WLCharacter('`');
            
            TickLoc = TheSourceManager->getSourceLocation();

            c = nextWLCharacter(INSIDE_NUMBER);
            
            accuracy = true;
        }
        
        if (c.isLetterlike() || c.isLetterlikeCharacter()) {

            //
            // Something like 1.2`a
            //

            auto Loc2 = TheSourceManager->getSourceLocation();

            //
            // Use ** markup syntax here because of ` character
            //
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between **`** and ``" + c.string() + "`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(TickLoc, Loc2));
                
            Issues.push_back(Issue);
        }

        if (accuracy || c.isDigit() || c == WLCharacter('-') || c == WLCharacter('+') || c == WLCharacter('.')) {
            
            if (c == WLCharacter('-') || c == WLCharacter('+')) {
                
                auto s = c;
                
                auto SignLoc = TheSourceManager->getSourceLocation();
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                if (c.isDigit()) {
                    
                    String << s;
                    
                } else if (c == WLCharacter('.')) {
                    
                    String << s;
                    
                } else if (accuracy) {
                    
                    //
                    // Something like 1.2``->3
                    //
                    
                    String << s;
                    
                    return Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), TheSourceManager->getTokenSpan());
                    
                } else {

                    //
                    // Something like 1.2`->3
                    //
                    // Must now do surgery and back up
                    //

                    std::string msg;
                    if (s == WLCharacter('-')) {
                        msg = "Put a space between **`** and ``-`` to reduce ambiguity";
                    } else {
                        msg = "Put a space between **`** and ``+`` to reduce ambiguity";
                    }
                    auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, msg, SYNTAXISSUESEVERITY_REMARK, Source(TickLoc, SignLoc));
                
                    Issues.push_back(Issue);
                    
                    
                    
                    auto Span = TheSourceManager->getWLCharacterSpan();
                    
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
                    
                    append(c, Span);
                    
                    return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSpan());
                }
            }
            
            bool handled = false;
            
            if (c.isDigit()) {

                handleDigits(Ctxt);
                
                handled = true;
            }
            
            c = currentWLCharacter();
            
            if (c == WLCharacter('.')) {
                
                if (handleFractionalPart(Ctxt, 10)) {
                    
                    handled = true;
                    
                } else if (!accuracy) {
                    
                    //
                    // Something like   123`.xxx  where the . could be a Dot operator
                    //
                    
                    return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSpan());
                }
            }
            
            if (accuracy) {
                if (!handled) {
                    
                    //
                    // Something like   123``EOF
                    //
                    
                    return Token(TOKEN_ERROR_EXPECTEDACCURACY, String.str(), TheSourceManager->getTokenSpan());
                }
            }
        }
    }
    
    c = currentWLCharacter();
    
    if (c == WLCharacter('*')) {
        
        auto StarLoc = TheSourceManager->getSourceLocation();
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c == WLCharacter('^')) {
            
            String << WLCharacter('*');
            
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            if (c == WLCharacter('-') || c == WLCharacter('+')) {
                
                String << c;
                
                c = nextWLCharacter(INSIDE_NUMBER);
            }
            
            if (!expectDigits(Ctxt)) {

                //
                // Something like 123*^
                //

                return Token(TOKEN_ERROR_EXPECTEDEXPONENT, String.str(), TheSourceManager->getTokenSpan());
            }
            
            if (c == WLCharacter('.')) {
                
                //
                // Something like 123*^.5
                //

                c = nextWLCharacter(INSIDE_NUMBER);
                
                return Token(TOKEN_ERROR_EXPECTEDEXPONENT, String.str(), TheSourceManager->getTokenSpan());
            }
            
        } else {
            
            //
            // Something like 1*a
            //
            // Must now do surgery and back up
            //
            
            auto Span = TheSourceManager->getWLCharacterSpan();
            
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
            _currentWLCharacter = WLCharacter('*');
            
            append(c, Span);
            
            if (real) {
                return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSpan());
            } else {
                return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSpan());
            }
        }
    }
    
    if (real) {
        return Token(TOKEN_REAL, String.str(), TheSourceManager->getTokenSpan());
    } else {
        return Token(TOKEN_INTEGER, String.str(), TheSourceManager->getTokenSpan());
    }
}

bool Tokenizer::handleFractionalPart(TokenizerContext Ctxt, int base) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('.'));
    
    auto DotLoc = TheSourceManager->getSourceLocation();
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    if (c == WLCharacter('.')) {
        
        //
        // Something like 0..
        //
        // Must now do surgery and back up
        //
        
        auto DigitLoc = DotLoc-1;

        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space before the ``.`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(DigitLoc,DotLoc));
        
        Issues.push_back(Issue);

        
        auto Loc = TheSourceManager->getSourceLocation();

        //
        // FIXME: DotLoc-1 is not correct because of something like this:
        //
        // 0\
        // ..
        //
        TheSourceManager->setSourceLocation(DotLoc-1);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        TheSourceManager->setSourceLocation(DotLoc);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        _currentWLCharacter = WLCharacter('.');
        
        append(c, Source(Loc, Loc));
        
        return false; 
    }
    
    String << WLCharacter('.');

    if (c.isDigit()) {
        
        if (!handleDigitsOrAlpha(Ctxt, base)) {
            return false;
        }
    } else if (c.isAlpha()) {
        
        if (!handleDigitsOrAlpha(Ctxt, base)) {
            return false;
        }
    }
    
    c = currentWLCharacter();

    if (c == WLCharacter('.')) {

        //
        // Something like 1.2.3
        //

        auto Loc2 = TheSourceManager->getSourceLocation();
        auto Loc1 = Loc2-1;

        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space before the ``.`` to reduce ambiguity", SYNTAXISSUESEVERITY_ERROR, Source(Loc1,Loc2));
        
        Issues.push_back(Issue);
    }

    return true;
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

bool Tokenizer::handleDigitsOrAlpha(TokenizerContext Ctxt, int base) {
    
    auto c = currentWLCharacter();

    auto len = 1;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //

        if (c.isDigit()) {
            
            int baseDigit = Utils::toDigit(c.to_point());
            
            if (baseDigit == -1 || baseDigit >= base) {
                return false;
            }
            
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
        } else if (c.isAlpha()) {
            
            int baseDigit = Utils::toDigit(c.to_point());
            
            if (baseDigit == -1 || baseDigit >= base) {
                return false;
            }
            
            String << c;
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
        } else {
            break;
        }

        len++;
    }
    
    return true;
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
                    
                    c = nextWLCharacter();
                    
                    if (Ctxt.EnableStringifyNextToken) {
                        stringifyNextToken_symbol = true;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_COLONEQUAL; // :=
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '>': {
                    Operator = TOKEN_COLONGREATER; // :>
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '(': {
            Operator = TOKEN_OPENPAREN; // (
            
            String << c;
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('*')) {
                
                return handleComment(Ctxt);
            }
        }
            break;
        case ')': {
            Operator = TOKEN_CLOSEPAREN; // )
            
            String << c;
            
            c = nextWLCharacter();
        }
            break;
        case '[': {
            
            Operator = TOKEN_OPENSQUARE; // [
            
            String << c;
            
            c = nextWLCharacter();
        }
            break;
        case ']': {
            Operator = TOKEN_CLOSESQUARE; // ]
            
            String << c;
            
            c = nextWLCharacter();
        }
            break;
        case ',': {
            Operator = TOKEN_COMMA; // ,
            
            String << c;
            
            c = nextWLCharacter();
        }
            break;
        case '{': {
            Operator = TOKEN_OPENCURLY; // {
            
            String << c;
            
            c = nextWLCharacter();
        }
            break;
        case '}': {
            Operator = TOKEN_CLOSECURLY; // }
            
            String << c;
            
            c = nextWLCharacter();
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
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_EQUALEQUALEQUAL; // ===
                        
                        String << c;
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '!': {
                    
                    auto BangLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter(INSIDE_OPERATOR);
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_EQUALBANGEQUAL; // =!=
                        
                        String << WLCharacter('!');
                        String << c;
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        //
                        // Something like x=!y
                        //
                        // Must now do surgery and back up
                        //
                        
                        auto Span = TheSourceManager->getWLCharacterSpan();
                        
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
                        _currentWLCharacter = WLCharacter('!');
                        
                        append(c, Span);
                        
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
                    
                    if (c == WLCharacter('_')) {
                        
                        Operator = TOKEN_UNDERUNDERUNDER; // ___
                        
                        String << c;
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '.': {
                    
                    auto FirstDotLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter(INSIDE_OPERATOR);
                    
                    if (c == WLCharacter('.')) {
                        
                        //
                        // Something like  _...
                        //
                        // Must now do surgery and back up
                        //
                        
                        auto UnderLoc = FirstDotLoc-1;

                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``_`` and ``.`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(UnderLoc,FirstDotLoc));
                        
                        Issues.push_back(Issue);

                        
                        auto Span = TheSourceManager->getWLCharacterSpan();
                        
                        //
                        // FIXME: FirstDotLoc-1 is not correct because of something like this:
                        //
                        // _\
                        // ...
                        //
                        TheSourceManager->setSourceLocation(FirstDotLoc-1);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        
                        TheSourceManager->setSourceLocation(FirstDotLoc);
                        TheSourceManager->setWLCharacterStart();
                        TheSourceManager->setWLCharacterEnd();
                        _currentWLCharacter = WLCharacter('.');
                        
                        append(c, Span);
                        
                        Operator = TOKEN_UNDER; // _
                        
                    } else {
                        
                        String << WLCharacter('.');
                        
                        if (c.isDigit()) {
                            
                            //
                            // Something like _.0
                            //
                            
                            auto UnderLoc = FirstDotLoc-1;
                            
                            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``.`` and number to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(UnderLoc,FirstDotLoc));
                            
                            Issues.push_back(Issue);
                        }
                        
                        Operator = TOKEN_UNDERDOT; // _.
                    }
                }
                    break;
            }
            
            return Token(Operator, String.str(), TheSourceManager->getTokenSpan());
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
                    
                    c = nextWLCharacter();
                }
                    break;
                case '<': {
                    Operator = TOKEN_LESSLESS; // <<
                    
                    String << c;
                    
                    c = nextWLCharacter();
                    
                    if (Ctxt.EnableStringifyNextToken) {
                        stringifyNextToken_file = true;
                    }
                }
                    break;
                case '>': {
                    Operator = TOKEN_LESSGREATER; // <>
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_LESSEQUAL; // <=
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    
                    auto MinusLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter(INSIDE_OPERATOR);
                    
                    if (c == WLCharacter('>')) {
                        
                        Operator = TOKEN_LESSMINUSGREATER; // <->
                        
                        String << WLCharacter('-');
                        String << c;
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        //
                        // Something like  a<-4
                        //
                        // Must now do surgery and back up
                        //
                        
                        auto Span = TheSourceManager->getWLCharacterSpan();
                        
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
                        _currentWLCharacter = WLCharacter('-');
                        
                        append(c, Span);
                        
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
                        
                        c = nextWLCharacter();
                    }
                    
                    if (Ctxt.EnableStringifyNextToken) {
                        stringifyNextToken_file = true;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_GREATEREQUAL; // >=
                    
                    String << c;
                    
                    c = nextWLCharacter();
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
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    Operator = TOKEN_MINUSMINUS; // --
                    
                    String << c;
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('>')) {
                        
                        //
                        // Something like a-->0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto MinusLoc = Loc-1;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``-`` and ``>`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(MinusLoc,Loc));
                        
                        Issues.push_back(Issue);
                        
                    } else if (c == WLCharacter('=')) {
                        
                        //
                        // Something like a--=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto MinusLoc = Loc-1;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``-`` and ``=`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(MinusLoc,Loc));
                        
                        Issues.push_back(Issue);
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_MINUSEQUAL; // -=
                    
                    String << c;
                    
                    c = nextWLCharacter();
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
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        //
                        // Something like <||>=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto GreaterLoc = Loc-1;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``>`` and ``=`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(GreaterLoc,Loc));
                        
                        Issues.push_back(Issue);
                        
                    }
                    
                }
                    break;
                case '|': {
                    Operator = TOKEN_BARBAR; // ||
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case ';': {
            Operator = TOKEN_SEMI; // ;
            
            String << c;
            
            c = nextWLCharacter(INSIDE_OPERATOR);
            
            if (c == WLCharacter(';')) {
                
                Operator = TOKEN_SEMISEMI; // ;;
                
                String << c;
                
                c = nextWLCharacter();
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
                    
                    c = nextWLCharacter();
                }
                    break;
                case '!': {
                    Operator = TOKEN_BANGBANG; // !!
                    
                    String << c;
                    
                    c = nextWLCharacter();
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
                
                Ctxt.SlotFlag = true;
                
                handleSymbol(Ctxt);
                
            } else if (c == WLCharacter('`')) {
                
                Operator = TOKEN_HASH; // #
                
                Ctxt.SlotFlag = true;

                handleSymbol(Ctxt);
                
            } else if (c == WLCharacter('"')) {
                
                auto Loc = TheSourceManager->getSourceLocation();

                Operator = TOKEN_HASH; // #
                
                handleString(Ctxt);

                auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.\n``#`` is not documented to allow ``\"`` characters.", SYNTAXISSUESEVERITY_REMARK, Source(Loc,Loc));
                        
                Issues.push_back(Issue);
                
            } else if (c == WLCharacter('#')) {
                
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
            
            if (c == WLCharacter('%')) {
                
                while (true) {
                    
                    //
                    // No need to check isAbort() inside tokenizer loops
                    //
                    
                    if (c != WLCharacter('%')) {
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
            
            if (c == WLCharacter('&')) {
                
                Operator = TOKEN_AMPAMP; // &&
                
                String << c;
                
                c = nextWLCharacter();
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
                    
                    c = nextWLCharacter();
                }
                    break;
                case ';': {
                    Operator = TOKEN_SLASHSEMI; // /;
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '.': {
                    
                    auto DotLoc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter();
                    
                    if (c.isDigit()) {
                        
                        //
                        // Something like t/.3
                        //
                        // Must now do surgery and back up
                        //
                        
                        auto SlashLoc = DotLoc-1;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``/`` and ``.`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(SlashLoc,DotLoc));
                        
                        Issues.push_back(Issue);

                        
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
                        _currentWLCharacter = WLCharacter('.');
                        
                        append(c, Source(Loc, Loc));
                        
                    } else {
                        
                        Operator = TOKEN_SLASHDOT; // /.
                        
                        String << WLCharacter('.');
                    }
                }
                    break;
                case '/': {
                    Operator = TOKEN_SLASHSLASH; // //
                    
                    String << c;
                    
                    c = nextWLCharacter();
                    
                    switch (c.to_point()) {
                        case '.': {
                            Operator = TOKEN_SLASHSLASHDOT; // //.
                            
                            String << c;
                            
                            c = nextWLCharacter();
                        }
                            break;
                        case '@': {
                            Operator = TOKEN_SLASHSLASHAT; // //@
                            
                            String << c;
                            
                            c = nextWLCharacter();
                        }
                            break;
                    }
                }
                    break;
                case ':': {
                    Operator = TOKEN_SLASHCOLON; // /:
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_SLASHEQUAL; // /=
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = TOKEN_SLASHSTAR; // /*
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '@': {
            Operator = TOKEN_AT; // @
            
            String << c;
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '@': {
                    Operator = TOKEN_ATAT; // @@
                    
                    String << c;
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('@')) {
                        
                        Operator = TOKEN_ATATAT; // @@@
                        
                        String << c;
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '*': {
                    Operator = TOKEN_ATSTAR; // @*
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '+': {

            Operator = TOKEN_PLUS; // +
            
            String << c;
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '+': {
                    Operator = TOKEN_PLUSPLUS; // ++
                    
                    String << c;
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        //
                        // Something like a++=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto PlusLoc = Loc-1;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ``+`` and ``=`` to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, Source(PlusLoc,Loc));
                        
                        Issues.push_back(Issue);
                        
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_PLUSEQUAL; // +=
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '~': {
            Operator = TOKEN_TILDE; // ~
            
            String << c;
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('~')) {
                
                Operator = TOKEN_TILDETILDE; // ~~
                
                String << c;
                
                c = nextWLCharacter();
            }
        }
            break;
        case '?': {
            Operator = TOKEN_QUESTION; // ?
            
            String << c;
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('?')) {
                
                Operator = TOKEN_QUESTIONQUESTION; // ??
                
                String << c;
                
                c = nextWLCharacter();
            }
        }
            break;
        case '*': {
            Operator = TOKEN_STAR; // *
            
            String << c;
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_STAREQUAL; // *=
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = TOKEN_STARSTAR; // **
                    
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '^': {
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case ':': {
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_CARETCOLONEQUAL; // ^:=
                        
                        String << WLCharacter('^');
                        String << WLCharacter(':');
                        String << c;
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        String << WLCharacter('^');
                        String << WLCharacter(':');
                        
                        Operator = TOKEN_ERROR_EXPECTEDEQUAL;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_CARETEQUAL; // ^=
                    
                    String << WLCharacter('^');
                    String << c;
                    
                    c = nextWLCharacter();
                }
                    break;
                default: {
                    Operator = TOKEN_CARET; // ^
                    
                    String << WLCharacter('^');
                }
                    break;
            }
        }
            break;
        case '\'': {
            Operator = TOKEN_SINGLEQUOTE; // '
            
            String << c;
            
            c = nextWLCharacter();
        }
            break;
        default: {
            
            assert(c.isPunctuationCharacter());
            
            String << c;
            
            Operator = LongNameCodePointToOperator(c.to_point());
            
            c = nextWLCharacter();
        }
    }
    
    return Token(Operator, String.str(), TheSourceManager->getTokenSpan());
}

//
// Could be . or .. or ... or .0
// Have to lookahead to decide
//
Token Tokenizer::handleDot(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('.'));
    
    auto DotLoc = TheSourceManager->getSourceLocation();
    
    c = nextWLCharacter();
    
    if (c.isDigit()) {
        
        //
        // Something like .0
        //
        // Must now do surgery and back up
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
        _currentWLCharacter = WLCharacter('.');
        
        append(c, Source(Loc, Loc));
        
        return handleNumber(Ctxt);
    }
    
    String << WLCharacter('.');
    
    auto Operator = TOKEN_DOT; // .
    
    if (c == WLCharacter('.')) {
        
        Operator = TOKEN_DOTDOT; // ..
        
        String << c;
        
        c = nextWLCharacter();
        
        if (c == WLCharacter('.')) {
            
            Operator = TOKEN_DOTDOTDOT; // ...
            
            String << c;
            
            c = nextWLCharacter();
        }
    }
    
    return Token(Operator, String.str(), TheSourceManager->getTokenSpan());
}

std::string Tokenizer::getString() const {
    return String.str();
}

std::vector<SyntaxIssue> Tokenizer::getIssues() const {
    return Issues;
}

std::vector<Metadata> Tokenizer::getMetadatas() const {
    
    std::vector<Metadata> Metadatas;
    
    auto totalTimeMillis = std::chrono::duration_cast<std::chrono::milliseconds>(totalTimeMicros);
    
    Metadatas.push_back(Metadata("TokenizerTotalTimeMillis", std::to_string(totalTimeMillis.count())));
    
    return Metadatas;
}

Tokenizer *TheTokenizer = nullptr;
