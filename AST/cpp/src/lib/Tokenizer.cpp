
#include "Tokenizer.h"

#include "Utils.h"


Tokenizer::Tokenizer() : _symbolifyNextToken(false), _fileifyNextToken(false), cur(TOKEN_UNKNOWN), currentCached(false), characterQueue(),
    _currentWLCharacter(0), _currentSourceLocation{0, 0}, String(), Issues() {}

void Tokenizer::init(bool skipFirstLine) {

    _symbolifyNextToken = false;
    _fileifyNextToken = false;
    cur = TOKEN_UNKNOWN;
    currentCached = false;

    characterQueue.clear();

    String.str("");

    Issues.clear();

    auto c = nextWLCharacter();

    if (skipFirstLine) {
        while (true) {
            if (c == WLCharacter('\n')) {
                c = nextWLCharacter();
                break;
            } else if (c == WLCHARACTER_EOF) {
                c = nextWLCharacter();
                break;
            } else {
                c = nextWLCharacter();
            }
        }
    }
}

void Tokenizer::deinit() {
    
    characterQueue.clear();

    String.str("");

    Issues.clear();
}


Token Tokenizer::nextToken() {
    
    assert(String.str().empty());
    
    TheSourceManager->setTokenStart();
    
    if (_symbolifyNextToken || _fileifyNextToken) {
        
        cur = handleString();
        
        TheSourceManager->setTokenEnd();
        
        return cur;
    }
    
    auto c = currentWLCharacter();
    
    if (c.isAlpha()) {
        
        cur = handleSymbol();
        
    } else if (c == WLCharacter('$') || c == WLCharacter('`')) {
        
        cur = handleSymbol();
        
    } else if (c == WLCharacter('"')) {
        
        cur = handleString();
        
    } else if (c.isDigit()) {
        
        cur = handleNumber();
        
    } else if (c == WLCharacter('\n')) {
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
        cur = TOKEN_NEWLINE;
        
    } else if (c == WLCharacter(' ') || c == WLCharacter('\t') || c == WLCharacter('\r')) {
        
        while (c == WLCharacter(' ') || c == WLCharacter('\t') || c == WLCharacter('\r')) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
        
        cur = TOKEN_SPACE;
        
    } else if (c == WLCharacter('.')) {
        
        cur = handleDot();
        
    } else if (c.isPunctuation() && c != WLCharacter('\\')) {
        
        cur = handleOperator();
        
    } else if (c.isLinearSyntax()) {
        
        cur = handleLinearSyntax();
        
    } else if (c == WLCharacter(EOF)) {
        
        TheSourceManager->setEOF();
        
        cur = TOKEN_EOF;
    }
    //
    // Everything else involving Unicode or errors
    //
    else if (c.isLetterlikeCharacter()) {
        
        cur = handleSymbol();
        
    } else if (c.isSpaceCharacter()) {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        String << c;
        
        c = nextWLCharacter();
        
        cur = TOKEN_SPACE;
        
    } else if (c.isNewlineCharacter()) {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        String << c;
        
        c = nextWLCharacter();
        
        cur = TOKEN_NEWLINE;
        
    } else if (c.isCommaCharacter()) {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        String << c;
        
        c = nextWLCharacter();
        
        cur = TOKEN_OPERATOR_COMMA;
        
    } else if (c.isOperatorCharacter()) {
        
        cur = handleOperator();
        
    } else {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        String << c;
        
        // Clear Issues
        auto Tmp = TheCharacterDecoder->getIssues();
        
        std::copy(Tmp.begin(), Tmp.end(), std::back_inserter(Issues));
        
        c = nextWLCharacter();
        
        cur = TOKEN_ERROR_UNHANDLEDCHARACTER;
    }
    
    TheSourceManager->setTokenEnd();
    
    return cur;
}

WLCharacter Tokenizer::nextWLCharacter(NextCharacterPolicy policy) {
    
    currentCached = false;
    
    if (!characterQueue.empty()) {
        
        auto p = characterQueue[0];
        
        auto c = p.first;
        
        // erase first
        characterQueue.erase(characterQueue.begin());
        
        return c;
    }
    
    return TheCharacterDecoder->nextWLCharacter(policy);
}

WLCharacter Tokenizer::currentWLCharacter() {

    if (currentCached) {
        
        return _currentWLCharacter;
    }

   auto c = TheCharacterDecoder->currentWLCharacter();

   return c;
}

void Tokenizer::setCurrentWLCharacter(WLCharacter current, SourceLocation Loc) {
    
    _currentWLCharacter = current;
    _currentSourceLocation = Loc;
    currentCached = true;
}

Token Tokenizer::currentToken() {
    
    assert(cur != TOKEN_UNKNOWN);
    
    return cur;
}

Token Tokenizer::handleLinearSyntax() {
    
    auto c = currentWLCharacter();

    //
    // Do not use String.put(c.to_char()); here because c may not be a char
    //
    String << c;
    
    Token Operator;
    
    switch (c.to_point()) {
        case CODEPOINT_LINEARSYNTAX_BANG:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_BANG;
            break;
        case CODEPOINT_LINEARSYNTAX_OPENPAREN:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_OPENPAREN;
            break;
        case CODEPOINT_LINEARSYNTAX_STAR:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_STAR;
            break;
        case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_CLOSEPAREN;
            break;
        case CODEPOINT_LINEARSYNTAX_AT:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_AT;
            break;
        case CODEPOINT_LINEARSYNTAX_CARET:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_CARET;
            break;
        case CODEPOINT_LINEARSYNTAX_UNDER:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_UNDER;
            break;
        case CODEPOINT_LINEARSYNTAX_PERCENT:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_PERCENT;
            break;
        case CODEPOINT_LINEARSYNTAX_AMP:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_AMP;
            break;
        case CODEPOINT_LINEARSYNTAX_SLASH:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_SLASH;
            break;
        case CODEPOINT_LINEARSYNTAX_PLUS:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_PLUS;
            break;
        case CODEPOINT_LINEARSYNTAX_BACKTICK:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_BACKTICK;
            break;
        case CODEPOINT_LINEARSYNTAX_SPACE:
            Operator = TOKEN_OPERATOR_LINEARSYNTAX_SPACE;
            break;
        default:
            assert(false);
            Operator = TOKEN_ERROR_UNHANDLEDCHARACTER;
            break;
    }
    
    c = nextWLCharacter();

    return Operator;
}

Token Tokenizer::handleComment() {
    // comment is already started
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('*'));
    
    String.put('*');
    
    auto depth = 1;

    c = nextWLCharacter(INSIDE_COMMENT);
    
    if (c == WLCHARACTER_EOF) {
        return TOKEN_ERROR_UNTERMINATEDCOMMENT;
    }
    
    while (true) {
        
        if (c == WLCharacter('(')) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter(INSIDE_COMMENT);
            
            if (c == WLCHARACTER_EOF) {
                return TOKEN_ERROR_UNTERMINATEDCOMMENT;
            }
            
            if (c == WLCharacter('*')) {
                
                String.put(c.to_char());
                
                c = nextWLCharacter(INSIDE_COMMENT);
                
                if (c == WLCHARACTER_EOF) {
                    return TOKEN_ERROR_UNTERMINATEDCOMMENT;
                }
                
                depth = depth + 1;
            }
            
        } else if (c == WLCharacter('*')) {
            
            String.put(c.to_char());

            c = nextWLCharacter(INSIDE_COMMENT);
            
            if (c == WLCHARACTER_EOF) {
                return TOKEN_ERROR_UNTERMINATEDCOMMENT;
            }
            
            if (c == WLCharacter(')')) {
                
                String.put(c.to_char());

                // This comment is closing
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    // Leaving comments, make sure to grab next character
                    
                    c = nextWLCharacter();
                    
                    break;
                    
                } else {
                    
                    c = nextWLCharacter(INSIDE_COMMENT);
                    
                    if (c == WLCHARACTER_EOF) {
                        return TOKEN_ERROR_UNTERMINATEDCOMMENT;
                    }
                }
            }
            
        } else {
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            
            String << c;
            
            // Clear Issues
            // We do not care about issues inside of comments
            TheCharacterDecoder->getIssues();
            
            c = nextWLCharacter(INSIDE_COMMENT);
            
            if (c == WLCHARACTER_EOF) {
                return TOKEN_ERROR_UNTERMINATEDCOMMENT;
            }
        }
        
    } // while

    return TOKEN_COMMENT;
}

//
// a segment is: [a-z$]([a-z]$[0-9])*
// a symbol is: (segment)?(`segment)*
//
Token Tokenizer::handleSymbol() {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('`') || c.isAlphaOrDollar() || c.isLetterlikeCharacter());
    
    if (c.isAlphaOrDollar() || c.isLetterlikeCharacter()) {
        handleSymbolSegment();
    }
    
    c = currentWLCharacter();
    
    while (c == WLCharacter('`')) {
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
        if (c.isAlphaOrDollar() || c.isLetterlikeCharacter()) {
            handleSymbolSegment();
        } else {
            
            return TOKEN_ERROR_UNHANDLEDCHARACTER;
        }
        
        c = currentWLCharacter();
        
    } // while
    
    return TOKEN_SYMBOL;
}

void Tokenizer::handleSymbolSegment() {
    
    auto c = currentWLCharacter();

    assert(c.isAlphaOrDollar() || c.isLetterlikeCharacter());
    
    if (c.isAlphaOrDollar()) {
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
    } else {
        
        if (c.isStrangeLetterlikeCharacter()) {
            
            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(TAG_STRANGECHARACTER, "Strange character in symbol: " + c.string(), SEVERITY_ERROR, Span);

            Issues.push_back(Issue);
        }
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        
        String << c;
        
        c = nextWLCharacter();
    }
    
    while (true) {
        
        if (c.isDigitOrAlphaOrDollar()) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
        } else if (c.isLetterlikeCharacter()) {
            
            if (c.isStrangeLetterlikeCharacter()) {
                
                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(TAG_STRANGECHARACTER, "Strange character in symbol: " + c.string(), SEVERITY_ERROR, Span);

                Issues.push_back(Issue);
            }
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            
            String << c;
            
            c = nextWLCharacter();
            
        } else {
            break;
        }
        
    } // while
}

Token Tokenizer::handleString() {
    
    auto c = currentWLCharacter();
    
    if (c.to_point() == '"') {
        if (c.isEscaped()) {
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            
            String << c;
            
            c = nextWLCharacter();
            
            return TOKEN_ERROR_UNHANDLEDCHARACTER;
        }
    } else if (_fileifyNextToken) {
        // first eat the whitespace
        while (c == WLCharacter(' ') || c == WLCharacter('\t') || c == WLCharacter('\n')) {
            c = nextWLCharacter(INSIDE_STRING);
        }
    } else if (_symbolifyNextToken) {
        ;
    } else {
        assert(false);
    }
    
    if (_symbolifyNextToken && c != WLCharacter('"')) {
        
        //
        // magically turn into a string
        //
        
        if (c.isAlphaOrDollar() || c.isLetterlikeCharacter()) {
            
            handleSymbolSegment();
            
            _symbolifyNextToken = false;
            
            return TOKEN_STRING;
            
        } else {
            
            //
            // Something like a::EOF
            //
            
            _symbolifyNextToken = false;
            
            return TOKEN_ERROR_EMPTYSTRING;
        }
        
    } else if (_fileifyNextToken && c != WLCharacter('"')) {
        
        //
        // magically turn into a string
        //
        
        auto empty = true;
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
            
            if (c.isDigitOrAlphaOrDollar() || c == WLCharacter('`') || c == WLCharacter('/') || c == WLCharacter('.') ||
                c == WLCharacter('\\') || c == WLCharacter('!') || c == WLCharacter('-') || c == WLCharacter('_') ||
                c == WLCharacter(':') || c == WLCharacter('*') || c == WLCharacter('~') || c == WLCharacter('?')) {
                
                empty = false;
                
                String.put(c.to_char());
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
            } else if (c == WLCharacter('[')) {

                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines

                empty = false;

                auto res = handleFileOpsBrackets();
                if (res != TOKEN_STRING) {
                    return res;
                }
                
                c = currentWLCharacter();
                
            } else {
                
                break;
            }
            
        } // while
        
        _fileifyNextToken = false;
        
        if (empty) {
            
            //
            // Something like a>>EOF
            //
            
            return TOKEN_ERROR_EMPTYSTRING;
        }
        
        return TOKEN_STRING;
        
    } else {
        
        String.put('"');

        while (true) {
            
            auto Tmp = TheCharacterDecoder->getIssues();
            
            std::copy(Tmp.begin(), Tmp.end(), std::back_inserter(Issues));
            
            c = nextWLCharacter(INSIDE_STRING);
            
            if (c == WLCHARACTER_EOF) {
                
                return TOKEN_ERROR_UNTERMINATEDSTRING;
                
            } else if (c == WLCharacter('"')) {
                
                break;
                
            } else if (c == WLCharacter('\\')) {
                
                String.put(c.to_char());
                
                auto Tmp = TheCharacterDecoder->getIssues();
                
                std::copy(Tmp.begin(), Tmp.end(), std::back_inserter(Issues));
                
                c = nextWLCharacter(INSIDE_STRING);
                
                //
                // c could be EOF, so just let string() handle stringifying
                //
                //
                // Do not use String.put(c.to_char()); here because c may not be a char
                //
                
                String << c;
                
            } else if (c == WLCharacter('\r')) {

                //
                // Skip \r in strings
                //
                ;

            } else {

                // too noisy
                // if (c == '\n') {

                //     auto Span = TheSourceManager->getWLCharacterSpan();

                //     auto CurrentLineNumber = Span.start.Line;

                //     auto LineWidth = TheSourceManager->getCurrentLineWidth();

                //     auto Issue = SyntaxIssue(TAG_NEWLINE, std::string("Newline in string") + "\n" + MSG_NEWLINE2, SEVERITY_REMARK,
                //         SourceSpan{SourceLocation{CurrentLineNumber-1, LineWidth+1}, Span.end});

                //     Issues.push_back(Issue);
                // }

                //
                // Do not use String.put(c.to_char()); here because c may not be a char
                //
                
                String << c;
            }
            
        } // while
        
        String.put('"');
        
        c = nextWLCharacter();
        
        _symbolifyNextToken = false;
        _fileifyNextToken = false;

        return TOKEN_STRING;
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
Token Tokenizer::handleFileOpsBrackets() {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('['));
    
    String.put('[');
    
    auto depth = 1;

    c = nextWLCharacter(INSIDE_STRING_FILEIFY);
    
    if (c == WLCHARACTER_EOF) {
        return TOKEN_ERROR_UNTERMINATEDSTRING;
    }
    
    while (true) {
        
        if (c == WLCharacter('[')) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter(INSIDE_STRING_FILEIFY);
            
            if (c == WLCHARACTER_EOF) {
                return TOKEN_ERROR_UNTERMINATEDSTRING;
            }
            
            depth = depth + 1;
            
        } else if (c == WLCharacter(']')) {
                
            String.put(c.to_char());
            
            depth = depth - 1;
            
            if (depth == 0) {
                
                // Leaving brackets, make sure to grab next character
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
                break;
                
            } else {
                
                c = nextWLCharacter(INSIDE_STRING_FILEIFY);
                
                if (c == WLCHARACTER_EOF) {
                    return TOKEN_ERROR_UNTERMINATEDSTRING;
                }
            }

        } else {

            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //

            String << c;
            
            if (c.to_point() == ' ' || c.to_point() == '\t' || c.to_point() == '\n') {
                return TOKEN_ERROR_UNTERMINATEDSTRING;
            }

            c = nextWLCharacter(INSIDE_STRING_FILEIFY);
            
            if (c == WLCHARACTER_EOF) {
                return TOKEN_ERROR_UNTERMINATEDSTRING;
            }
        }
        
    } // while

    return TOKEN_STRING;
}

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
Token Tokenizer::handleNumber() {
    
    auto c = currentWLCharacter();

    int base = 10;
    
    handleDigits();
    
    c = currentWLCharacter();
    
    //
    // Could be 16^^blah
    //
    if (c == WLCharacter('^')) {
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c == WLCharacter('^')) {
            
            base = parseInteger(String.str(), 10);
            
            if (base < 2 || base > 36) {
                
                return TOKEN_ERROR_INVALIDBASE;
            }
            
            String.put('^');
            String.put(c.to_char());
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            if (c.isDigitOrAlpha()) {
                
                if (!handleDigitsOrAlpha(base)) {
                    
                    return TOKEN_ERROR_UNHANDLEDCHARACTER;
                }
                
            } else {
                
                String.str("");
                
                return TOKEN_ERROR_EXPECTEDDIGITORALPHA;
            }
            
        } else {
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            characterQueue.push_back(std::make_pair(c, Loc));
            setCurrentWLCharacter(WLCharacter('^'), Loc);
            
            return TOKEN_NUMBER;
        }
    }
    
    c = currentWLCharacter();
    
    if (c == WLCharacter('.')) {
        
        if (!handleFractionalPart(base)) {
            
            return TOKEN_NUMBER;
        }
    }
    
    c = currentWLCharacter();
    
    //
    // foo`
    // foo`bar
    // foo``bar
    //
    if (c == WLCharacter('`')) {
        
        String.put('`');
        
        auto Loc = TheSourceManager->getSourceLocation();

        c = nextWLCharacter(INSIDE_NUMBER);
        
        bool accuracy = false;
        if (c == WLCharacter('`')) {
            
            String.put('`');
            
            Loc = TheSourceManager->getSourceLocation();

            c = nextWLCharacter(INSIDE_NUMBER);
            
            accuracy = true;
        }
        
        if (c.isAlphaOrDollar() || c.isLetterlikeCharacter()) {

            //
            // Something like 1.2`a
            //

            auto Loc2 = TheSourceManager->getSourceLocation();

            auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, "Put a space between ` and " + c.string() + " to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{Loc, Loc2}));
                
            Issues.push_back(Issue);
        }

        if (accuracy || c.isDigit() || c == WLCharacter('-') || c == WLCharacter('+') || c == WLCharacter('.')) {
            
            if (c == WLCharacter('-') || c == WLCharacter('+')) {
                
                auto s = c;
                
                auto Loc2 = TheSourceManager->getSourceLocation();
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                if (c.isDigit()) {
                    
                    String.put(s.to_char());
                    
                } else if (c == WLCharacter('.')) {
                    
                    String.put(s.to_char());
                    
                } else if (accuracy) {
                    
                    //
                    // Something like 1.2``->3
                    //
                    
                    String.put(s.to_char());
                    
                    return TOKEN_ERROR_EXPECTEDACCURACY;
                    
                } else {

                    //
                    // Something like 1.2`->3
                    //

                    std::string msg;
                    if (s == WLCharacter('-')) {
                        msg = "Put a space between ` and - to reduce ambiguity";
                    } else {
                        msg = "Put a space between ` and + to reduce ambiguity";
                    }
                    auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, msg, SEVERITY_REMARK, (SourceSpan{Loc, Loc2}));
                
                    Issues.push_back(Issue);
                    
                    
                    Loc = TheSourceManager->getSourceLocation();
                    
                    characterQueue.push_back(std::make_pair(c, Loc));
                    setCurrentWLCharacter(s, Loc);
                    
                    return TOKEN_NUMBER;
                }
            }
            
            bool handled = false;
            
            if (c.isDigit()) {
                
                handleDigits();
                
                handled = true;
            }
            
            c = currentWLCharacter();
            
            if (c == WLCharacter('.')) {
                
                if (handleFractionalPart(10)) {
                    
                    handled = true;
                    
                } else if (!accuracy) {
                    
                    return TOKEN_NUMBER;
                }
            }
            
            if (accuracy) {
                if (!handled) {
                    return TOKEN_ERROR_EXPECTEDACCURACY;
                }
            }
        }
    }
    
    c = currentWLCharacter();
    
    if (c == WLCharacter('*')) {
        
        c = nextWLCharacter(INSIDE_NUMBER);
        
        if (c == WLCharacter('^')) {
            
            String.put('*');
            String.put(c.to_char());
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
            if (c == WLCharacter('-') || c == WLCharacter('+')) {
                
                String.put(c.to_char());
                
                c = nextWLCharacter(INSIDE_NUMBER);
            }
            
            if (!expectDigits()) {
                
                return TOKEN_ERROR_UNHANDLEDCHARACTER;
            }
            
            if (c == WLCharacter('.')) {
                
                c = nextWLCharacter(INSIDE_NUMBER);
                
                return TOKEN_ERROR_EXPONENT;
            }
            
        } else {
            
            //
            // Something like 1*a
            //
            // Back out of treating * as part of the number
            //
            
            auto Loc = TheSourceManager->getSourceLocation();
            Loc = Loc - 1;
            
            characterQueue.push_back(std::make_pair(c, Loc));
            setCurrentWLCharacter(WLCharacter('*'), Loc);
            
            return TOKEN_NUMBER;
        }
    }
    
    return TOKEN_NUMBER;
}

bool Tokenizer::handleFractionalPart(int base) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('.'));
    
    auto Loc = TheSourceManager->getSourceLocation();
    
    c = nextWLCharacter(INSIDE_NUMBER);
    
    if (c == WLCharacter('.')) {
        
        //
        // Something like 0..
        //
        
        auto DigitLoc = Loc;
        DigitLoc.Col--;

        auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, "Put a space before the . to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{DigitLoc,Loc}));
        
        Issues.push_back(Issue);

        
        Loc = TheSourceManager->getSourceLocation();
        
        characterQueue.push_back(std::make_pair(c, Loc));
        setCurrentWLCharacter(WLCharacter('.'), Loc);
        
        
        return false; 
    }

    String.put('.');

    if (c.isDigitOrAlpha()) {
        
        if (!handleDigitsOrAlpha(base)) {
            return false;
        }
    }
    
    c = currentWLCharacter();

    if (c == WLCharacter('.')) {

        //
        // Something like 1.2.3
        //

        auto Loc2 = TheSourceManager->getSourceLocation();
        auto Loc1 = Loc2;
        Loc1.Col--;

        auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, "Put a space before the . to reduce ambiguity", SEVERITY_ERROR, (SourceSpan{Loc1,Loc2}));
        
        Issues.push_back(Issue);
    }

    return true;
}

bool Tokenizer::expectDigits() {
    
    auto c = currentWLCharacter();

    if (c.isDigit()) {
        
        handleDigits();
        
        return true;
        
    } else {
        
        return false;
    }
}

void Tokenizer::handleDigits() {
    
    auto c = currentWLCharacter();

    while (true) {
        
        if (c.isDigit()) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
        } else {
            
            break;
        }
    }
}

bool Tokenizer::handleDigitsOrAlpha(int base) {
    
    auto c = currentWLCharacter();

    while (true) {
        
        if (c.isDigitOrAlpha()) {
            
            int baseDigit = c.toDigit();
            
            if (baseDigit == -1 || baseDigit >= base) {
                return false;
            }
            
            String.put(c.to_char());
            
            c = nextWLCharacter(INSIDE_NUMBER);
            
        } else {
            break;
        }
    }
    
    return true;
}

void Tokenizer::handleDigitsOrAlphaOrDollar() {
    
    auto c = currentWLCharacter();

    while (true) {
        
        if (c.isDigitOrAlphaOrDollar()) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
        } else {
            break;
        }
    }
}

Token Tokenizer::handleOperator() {
    
    auto c = currentWLCharacter();

    Token Operator = TOKEN_OPERATOR_UNKNOWN;
    
    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_OPERATOR_COLON;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case ':': {
                    Operator = TOKEN_OPERATOR_COLONCOLON;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    _symbolifyNextToken = true;
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_COLONEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '>': {
                    Operator = TOKEN_OPERATOR_COLONGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '(': {
            Operator = TOKEN_OPERATOR_OPENPAREN;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('*')) {
                
                return handleComment();
            }
        }
            break;
        case ')': {
            Operator = TOKEN_OPERATOR_CLOSEPAREN;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '[': {
            
            Operator = TOKEN_OPERATOR_OPENSQUARE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case ']': {
            Operator = TOKEN_OPERATOR_CLOSESQUARE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case ',': {
            Operator = TOKEN_OPERATOR_COMMA;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '{': {
            Operator = TOKEN_OPERATOR_OPENCURLY;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '}': {
            Operator = TOKEN_OPERATOR_CLOSECURLY;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '=': {
            Operator = TOKEN_OPERATOR_EQUAL;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_OPERATOR_EQUALEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_OPERATOR_EQUALEQUALEQUAL;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '!': {
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_OPERATOR_EQUALBANGEQUAL;
                        
                        String.put('!');
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('!'), Loc);
                        
                        Operator = TOKEN_OPERATOR_EQUAL;
                    }
                }
                    break;
            }
        }
            break;
        case '_': {
            Operator = TOKEN_OPERATOR_UNDER;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '_': {
                    Operator = TOKEN_OPERATOR_UNDERUNDER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('_')) {
                        
                        Operator = TOKEN_OPERATOR_UNDERUNDERUNDER;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '.': {
                    
                    auto Loc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('.')) {
                        
                        //
                        // Could be _... which should parse as _ ...
                        //
                        
                        auto UnderLoc = Loc;
                        UnderLoc.Col--;

                        auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, "Put a space between _ and . to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{UnderLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        
                        Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('.'), Loc);
                        
                        
                        Operator = TOKEN_OPERATOR_UNDER;
                        
                    } else if (c.isDigit()) {
                        
                        String.put('.');
                        
                        //
                        // Something like _.0
                        //
                        
                        auto UnderLoc = Loc;
                        UnderLoc.Col--;
                        
                        auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, "Put a space between . and number to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{UnderLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        Operator = TOKEN_OPERATOR_UNDERDOT;
                        
                    } else {
                        
                        String.put('.');
                        
                        Operator = TOKEN_OPERATOR_UNDERDOT;
                    }
                }
                    break;
            }
            
            return Operator;
        }
            break;
        case '<': {
            Operator = TOKEN_OPERATOR_LESS;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '|': {
                    Operator = TOKEN_OPERATOR_LESSBAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '<': {
                    Operator = TOKEN_OPERATOR_LESSLESS;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    _fileifyNextToken = true;
                }
                    break;
                case '>': {
                    Operator = TOKEN_OPERATOR_LESSGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_LESSEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('>')) {
                        
                        Operator = TOKEN_OPERATOR_LESSMINUSGREATER;
                        
                        String.put('-');
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('-'), Loc);
                        
                        Operator = TOKEN_OPERATOR_LESS;
                    }
                }
                    break;
            }
        }
            break;
        case '>': {
            Operator = TOKEN_OPERATOR_GREATER;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '>': {
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c.to_point() == '>') {
                        
                        Operator = TOKEN_OPERATOR_GREATERGREATERGREATER;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                        _fileifyNextToken = true;
                        
                    } else {
                        
                        Operator = TOKEN_OPERATOR_GREATERGREATER;
                        
                        _fileifyNextToken = true;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_GREATEREQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '-': {
            Operator = TOKEN_OPERATOR_MINUS;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
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
                    Operator = TOKEN_OPERATOR_MINUSGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    Operator = TOKEN_OPERATOR_MINUSMINUS;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_MINUSEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '|': {
            Operator = TOKEN_OPERATOR_BAR;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '>': {
                    Operator = TOKEN_OPERATOR_BARGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '|': {
                    Operator = TOKEN_OPERATOR_BARBAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case ';': {
            Operator = TOKEN_OPERATOR_SEMI;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter(';')) {
                
                Operator = TOKEN_OPERATOR_SEMISEMI;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
            }
        }
            break;
        case '!': {
            Operator = TOKEN_OPERATOR_BANG;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_OPERATOR_BANGEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '!': {
                    Operator = TOKEN_OPERATOR_BANGBANG;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '#': {
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            //
            // A slot that starts with a digit goes down one path
            // And a slot that starts with a letter does down another path
            //
            // Make sure e.g. #1a is not parsd as SlotNode["#1a"]
            //
            
            if (c.isDigit()) {
                
                Operator = TOKEN_OPERATOR_HASH;
                
                handleDigits();
                
            } else if (c.isAlphaOrDollar()) {
                
                Operator = TOKEN_OPERATOR_HASH;
                
                handleDigitsOrAlphaOrDollar();
                
            } else if (c == WLCharacter('#')) {
                
                Operator = TOKEN_OPERATOR_HASHHASH;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
                
                if (c.isDigit()) {
                    
                    handleDigits();
                }

            } else {
                
                Operator = TOKEN_OPERATOR_HASH;
            }
        }
            break;
        case '%': {
            Operator = TOKEN_OPERATOR_PERCENT;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('%')) {
                
                while (true) {
                    
                    if (c != WLCharacter('%')) {
                        break;
                    }
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                
            } else if (c.isDigit()) {
                
                handleDigits();
            }
        }
            break;
        case '&': {
            Operator = TOKEN_OPERATOR_AMP;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('&')) {
                
                Operator = TOKEN_OPERATOR_AMPAMP;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
            }
        }
            break;
        case '/': {
            Operator = TOKEN_OPERATOR_SLASH;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '@': {
                    Operator = TOKEN_OPERATOR_SLASHAT;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case ';': {
                    Operator = TOKEN_OPERATOR_SLASHSEMI;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '.': {
//                    Operator = TOKEN_OPERATOR_UNKNOWN;
                    
                    auto Loc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter();
                    
                    if (c.isDigit()) {
                        
                        //
                        // Something like Round[t/.03 + 1]
                        //
                        
                        auto SlashLoc = Loc;
                        SlashLoc.Col--;
                        
                        auto Issue = SyntaxIssue(TAG_SYNTAXAMBIGUITY, "Put a space between / and . to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{SlashLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        
                        Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('.'), Loc);
                        
                        
                        Operator = TOKEN_OPERATOR_SLASH;
                        
                    } else {
                        
                        String.put('.');
                        
                        Operator = TOKEN_OPERATOR_SLASHDOT;
                    }
                }
                    break;
                case '/': {
                    Operator = TOKEN_OPERATOR_SLASHSLASH;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    switch (c.to_point()) {
                        case '.': {
                            Operator = TOKEN_OPERATOR_SLASHSLASHDOT;
                            
                            String.put(c.to_char());
                            
                            c = nextWLCharacter();
                        }
                            break;
                        case '@': {
                            Operator = TOKEN_OPERATOR_SLASHSLASHAT;
                            
                            String.put(c.to_char());
                            
                            c = nextWLCharacter();
                        }
                            break;
                    }
                }
                    break;
                case ':': {
                    Operator = TOKEN_OPERATOR_SLASHCOLON;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_SLASHEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = TOKEN_OPERATOR_SLASHSTAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '@': {
            Operator = TOKEN_OPERATOR_AT;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '@': {
                    Operator = TOKEN_OPERATOR_ATAT;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('@')) {
                        
                        Operator = TOKEN_OPERATOR_ATATAT;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '*': {
                    Operator = TOKEN_OPERATOR_ATSTAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '+': {

            Operator = TOKEN_OPERATOR_PLUS;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '+': {
                    Operator = TOKEN_OPERATOR_PLUSPLUS;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_PLUSEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '~': {
            Operator = TOKEN_OPERATOR_TILDE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('~')) {
                
                Operator = TOKEN_OPERATOR_TILDETILDE;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
            }
        }
            break;
        case '?': {
            Operator = TOKEN_OPERATOR_QUESTION;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '*': {
            Operator = TOKEN_OPERATOR_STAR;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_OPERATOR_STAREQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = TOKEN_OPERATOR_STARSTAR;
                    
                    String.put(c.to_char());
                    
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
                        
                        Operator = TOKEN_OPERATOR_CARETCOLONEQUAL;
                        
                        String.put('^');
                        String.put(':');
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        String.put('^');
                        String.put(':');
                        
                        Operator = TOKEN_ERROR_UNHANDLEDCHARACTER;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_OPERATOR_CARETEQUAL;
                    
                    String.put('^');
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                default: {
                    Operator = TOKEN_OPERATOR_CARET;
                    
                    String.put('^');
                }
                    break;
            }
        }
            break;
        case '\'': {
            Operator = TOKEN_OPERATOR_SINGLEQUOTE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        default: {
            
            assert(c.isOperatorCharacter());
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            String << c;
            
            Operator = LongNameCodePointToOperator(c.to_point());
            
            c = nextWLCharacter();
        }
    }
    
    return Operator;
}

//
// Could be . or .. or ... or .0
// Have to lookahead to decide
//
Token Tokenizer::handleDot() {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('.'));
    
    c = nextWLCharacter();
    
    if (c.isDigit()) {
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        characterQueue.push_back(std::make_pair(c, Loc));
        setCurrentWLCharacter(WLCharacter('.'), Loc);
        
        return handleNumber();
    }
    
    String.put('.');
    
    auto Operator = TOKEN_OPERATOR_DOT;
    
    if (c == WLCharacter('.')) {
        
        Operator = TOKEN_OPERATOR_DOTDOT;
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
        if (c == WLCharacter('.')) {
            
            Operator = TOKEN_OPERATOR_DOTDOTDOT;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
    }
    
    return Operator;
}

std::string Tokenizer::getString() {
    
    auto StringStr = String.str();
    
    String.str("");
    
    return StringStr;
}

std::vector<SyntaxIssue> Tokenizer::getIssues() {

    auto Tmp = Issues;

    Issues.clear();
    
    auto CharacterIssues = TheCharacterDecoder->getIssues();
    
    std::copy(CharacterIssues.begin(), CharacterIssues.end(), std::back_inserter(Tmp));
    
    return Tmp;
}

Tokenizer *TheTokenizer = nullptr;
