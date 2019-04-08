
#include "Tokenizer.h"

#include "Utils.h"


Tokenizer::Tokenizer() : stringifyNextToken_symbol(false), stringifyNextToken_file(false), cur(TOKEN_UNKNOWN), currentCached(false), characterQueue(),
    _currentWLCharacter(0), _currentSourceLocation{0, 0}, String(), Issues() {}

void Tokenizer::init(bool skipFirstLine) {

    stringifyNextToken_symbol = false;
    stringifyNextToken_file = false;
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


Token Tokenizer::nextToken(TokenizerContext CtxtIn) {
    
    assert(String.str().empty());
    assert(Issues.empty());
    
    auto Ctxt = CtxtIn;
    Ctxt.SlotFlag = false;

    TheSourceManager->setTokenStart();
    
    if (stringifyNextToken_symbol || stringifyNextToken_file) {
        
        cur = handleString(Ctxt);
        
        TheSourceManager->setTokenEnd();
        
        return cur;
    }
    
    auto c = currentWLCharacter();
    
    if (c.isAlpha()) {
        
        cur = handleSymbol(Ctxt);
        
    } else if (c == WLCharacter('$') || c == WLCharacter('`')) {
        
        cur = handleSymbol(Ctxt);
        
    } else if (c == WLCharacter('"')) {
        
        cur = handleString(Ctxt);
        
    } else if (c.isDigit()) {
        
        cur = handleNumber(Ctxt);
        
    } else if (c == WLCharacter('\n') || c == WLCharacter('\r')) {
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
        cur = TOKEN_NEWLINE;
        
    } else if (c == WLCharacter(' ') || c == WLCharacter('\t')) {
        
        while (c == WLCharacter(' ') || c == WLCharacter('\t')) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
        
        cur = TOKEN_WHITESPACE;
        
    } else if (c == WLCharacter('.')) {
        
        cur = handleDot(Ctxt);
        
    } else if (c.isPunctuation() && c != WLCharacter('\\')) {
        
        cur = handleOperator(Ctxt);
        
    } else if (c.isLinearSyntax()) {
        
        cur = handleLinearSyntax(Ctxt);
        
    } else if (c == WLCharacter(EOF)) {
        
        TheSourceManager->setEOF();
        
        cur = TOKEN_EOF;
    }
    //
    // Everything else involving Unicode or errors
    //
    else if (c.isLetterlikeCharacter()) {
        
        cur = handleSymbol(Ctxt);
        
    } else if (c.isSpaceCharacter()) {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        auto p = c.preferredSource();
        String << p;
        
        c = nextWLCharacter();
        
        cur = TOKEN_WHITESPACE;
        
    } else if (c.isNewlineCharacter()) {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        auto p = c.preferredSource();
        String << p;
        
        c = nextWLCharacter();
        
        cur = TOKEN_NEWLINE;
        
    } else if (c.isCommaCharacter()) {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        auto p = c.preferredSource();
        String << p;
        
        c = nextWLCharacter();
        
        cur = TOKEN_COMMA;
        
    } else if (c.isOperatorCharacter()) {
        
        cur = handleOperator(Ctxt);
        
    } else {
        
        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        auto p = c.preferredSource();
        String << p;
        
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
    
    auto CharacterDecoderIssues = TheCharacterDecoder->getIssues();
    
    std::copy(CharacterDecoderIssues.begin(), CharacterDecoderIssues.end(), std::back_inserter(Issues));
    
    
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

Token Tokenizer::handleLinearSyntax(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    //
    // Do not use String.put(c.to_char()); here because c may not be a char
    //
    auto p = c.preferredSource();
    String << p;
    
    Token Operator;
    
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

    return Operator;
}

Token Tokenizer::handleComment(TokenizerContext Ctxt) {
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
        
        //
        // No need to check isAbort() inside tokenizer loops
        //

        //
        // No need to check for comment length
        //

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
            // Clear Issues
            // We do not care about issues within comments
            //
            getIssues();
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            
            auto p = c.preferredSource();
            String << p;
            
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
Token Tokenizer::handleSymbol(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('`') || c.isAlphaOrDollar() || c.isLetterlikeCharacter());
    
    if (c.isAlpha()) {
        handleSymbolSegment(Ctxt);
    } else if (c.isDollar()) {

        if (Ctxt.SlotFlag) {

            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

            Issues.push_back(Issue);
        }

        handleSymbolSegment(Ctxt);
    } else if (c.isLetterlikeCharacter()) {

        if (Ctxt.SlotFlag) {

            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

            Issues.push_back(Issue);
        }

        handleSymbolSegment(Ctxt);
    }
    
    c = currentWLCharacter();
    
    while (c == WLCharacter('`')) {

        if (Ctxt.SlotFlag) {

            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

            Issues.push_back(Issue);
        }

        String.put(c.to_char());
        
        c = nextWLCharacter();
        
        if (c.isAlpha()) {
            handleSymbolSegment(Ctxt);
        } else if (c.isDollar()) {

            if (Ctxt.SlotFlag) {

                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

                Issues.push_back(Issue);
            }
            
            handleSymbolSegment(Ctxt);

        } else if (c.isLetterlikeCharacter()) {

            if (Ctxt.SlotFlag) {

                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

                Issues.push_back(Issue);
            }

            handleSymbolSegment(Ctxt);
        } else {
            return TOKEN_ERROR_EXPECTEDALPHAORDOLLAR;
        }
        
        c = currentWLCharacter();
        
    } // while
    
    return TOKEN_SYMBOL;
}

void Tokenizer::handleSymbolSegment(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c.isAlphaOrDollar() || c.isLetterlikeCharacter());
    
    auto Loc = TheSourceManager->getSourceLocation();

    if (c.isAlpha()) {
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
    } else if (c.isDollar()) {
        
        if (Ctxt.SlotFlag) {

            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

            Issues.push_back(Issue);
        }

        String.put(c.to_char());
        
        c = nextWLCharacter();
        
    } else {
        
        if (c.isStrangeLetterlikeCharacter()) {
            
            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange character in symbol: " + makeGraphical(c.actualString()), SYNTAXISSUESEVERITY_WARNING, Span);

            Issues.push_back(Issue);
        }
        
        if (Ctxt.SlotFlag) {

            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

            Issues.push_back(Issue);
        }

        //
        // Do not use String.put(c.to_char()); here because c may not be a char
        //
        
        auto p = c.preferredSource();
        String << p;
        
        c = nextWLCharacter();
    }
    
    auto len = 1;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //

        if (len == MAX_SYMBOL_LENGTH) {
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXSYMBOLLENGTH, std::string("Max symbol length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
        
            Issues.push_back(Issue);
        }

        if (c.isDigitOrAlpha()) {
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
        } else if (c.isDollar()) {
            
            if (Ctxt.SlotFlag) {

                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

                Issues.push_back(Issue);
            }

            String.put(c.to_char());
            
            c = nextWLCharacter();
            
        } else if (c.isLetterlikeCharacter()) {
            
            if (c.isStrangeLetterlikeCharacter()) {
                
                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_STRANGECHARACTER, "Strange character in symbol: " + makeGraphical(c.actualString()), SYNTAXISSUESEVERITY_WARNING, Span);

                Issues.push_back(Issue);
            }
            
            if (Ctxt.SlotFlag) {

                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, Span);

                Issues.push_back(Issue);
            }
        
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            
            auto p = c.preferredSource();
            String << p;
            
            c = nextWLCharacter();
            
        } else {
            break;
        }
        
        len++;

    } // while
}

Token Tokenizer::handleString(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();
    
    auto Loc = TheSourceManager->getSourceLocation();

    //
    // Do not check c == WLCharacter('"') here because we want to know if it was escaped
    //
    if (c.to_point() == '"') {

        //
        // If the beginning " is escaped, then error out
        //
        if (c.isEscaped()) {
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            
            auto p = c.preferredSource();
            String << p;
            
            c = nextWLCharacter();
            
            return TOKEN_ERROR_UNHANDLEDCHARACTER;
        }
    } else if (stringifyNextToken_file) {
        // first eat the whitespace
        while (c == WLCharacter(' ') || c == WLCharacter('\t') || c == WLCharacter('\n')) {
            c = nextWLCharacter(INSIDE_STRING);
        }
    } else if (stringifyNextToken_symbol) {
        ;
    } else {
        assert(false);
    }
    
    if (stringifyNextToken_symbol && c != WLCharacter('"')) {
        
        //
        // magically turn into a string
        //
        
        if (c.isAlphaOrDollar() || c.isLetterlikeCharacter()) {
            
            handleSymbolSegment(Ctxt);
            
            stringifyNextToken_symbol = false;
            
            return TOKEN_STRING;
            
        } else {
            
            //
            // Something like a::EOF
            //
            
            stringifyNextToken_symbol = false;
            
            return TOKEN_ERROR_EMPTYSTRING;
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

            if (len == MAX_STRING_LENGTH) {
            
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXSTRINGLENGTH, std::string("Max string length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
            
                Issues.push_back(Issue);
            }

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

                auto res = handleFileOpsBrackets(Ctxt);
                if (res != TOKEN_STRING) {
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
            // Something like a>>EOF
            //
            // There is expected to be a string after >>
            //
            
            return TOKEN_ERROR_EMPTYSTRING;
        }
        
        return TOKEN_STRING;
        
    } else {
        
        String.put('"');

        auto len = 0;
        while (true) {
            
            //
            // No need to check isAbort() inside tokenizer loops
            //

            if (len == MAX_STRING_LENGTH) {
            
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXSTRINGLENGTH, std::string("Max string length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
            
                Issues.push_back(Issue);
            }

            c = nextWLCharacter(INSIDE_STRING);
            
            if (c == WLCHARACTER_EOF) {
                
                return TOKEN_ERROR_UNTERMINATEDSTRING;
                
            } else
                //
                // OK to check c == WLCharacter('"') here because we only care about un-escaped "
                //
                if (c == WLCharacter('"')) {
                
                break;
                
            } else if (c == WLCharacter('\\')) {
                
                String.put(c.to_char());
                
                c = nextWLCharacter(INSIDE_STRING);
                
                //
                // c could be EOF, so just let string() handle stringifying
                //
                //
                // Do not use String.put(c.to_char()); here because c may not be a char
                //
                
                auto p = c.preferredSource();
                String << p;
                
            } else if (c == WLCharacter('\r')) {

                //
                // Skip \r in strings
                //
                ;

            } else {

                //
                // Do not use String.put(c.to_char()); here because c may not be a char
                //
                
                auto p = c.preferredSource();
                String << p;
            }
            
            len++;

        } // while
        
        String.put('"');
        
        c = nextWLCharacter();
        
        stringifyNextToken_symbol = false;
        stringifyNextToken_file = false;

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
Token Tokenizer::handleFileOpsBrackets(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('['));
    
    String.put('[');
    
    auto depth = 1;

    c = nextWLCharacter(INSIDE_STRING_FILEIFY);
    
    if (c == WLCHARACTER_EOF) {
        return TOKEN_ERROR_UNTERMINATEDSTRING;
    }
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //

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

            auto p = c.preferredSource();
            String << p;
            
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
Token Tokenizer::handleNumber(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    int base = 10;
    
    handleDigits(Ctxt);
    
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
                
                if (!handleDigitsOrAlpha(Ctxt, base)) {
                    
                    return TOKEN_ERROR_EXPECTEDDIGITORALPHA;
                }
                
            } else {
                
                String.str("");
                
                return TOKEN_ERROR_EXPECTEDDIGITORALPHA;
            }
            
        } else {
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            characterQueue.push_back(std::make_pair(c, Loc));
            setCurrentWLCharacter(WLCharacter('^'), Loc);
            
            return TOKEN_INTEGER;
        }
    }
    
    c = currentWLCharacter();
    
    bool real = false;

    if (c == WLCharacter('.')) {
        
        if (!handleFractionalPart(Ctxt, base)) {
            
            return TOKEN_INTEGER;
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

            auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between ` and " + c.preferredString() + " to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc, Loc2}));
                
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
                    auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, msg, SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc, Loc2}));
                
                    Issues.push_back(Issue);
                    
                    
                    Loc = TheSourceManager->getSourceLocation();
                    
                    characterQueue.push_back(std::make_pair(c, Loc));
                    setCurrentWLCharacter(s, Loc);
                    
                    return TOKEN_REAL;
                }
            }
            
            bool handled = false;
            
            if (c.isDigit()) {
                
                auto Loc = TheSourceManager->getSourceLocation();

                auto len = handleDigits(Ctxt);

                if (accuracy) {

                    //
                    // Just look at integer value of accuracy
                    //
                    if (len >= MAX_ACCURACY_LENGTH) {
                    
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXACCURACYLENGTH, std::string("Max accuracy length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
                    
                        Issues.push_back(Issue);
                    }
                } else {

                    //
                    // Just look at integer value of precision
                    //
                    if (len >= MAX_PRECISION_LENGTH) {
                    
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXPRECISIONLENGTH, std::string("Max precision length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
                    
                        Issues.push_back(Issue);
                    }
                }
                
                handled = true;
            }
            
            c = currentWLCharacter();
            
            if (c == WLCharacter('.')) {
                
                if (handleFractionalPart(Ctxt, 10)) {
                    
                    handled = true;
                    
                } else if (!accuracy) {
                    
                    //
                    // Something like 123`.xxx  where the . could be a Dot operator
                    //
                    
                    return TOKEN_REAL;
                }
            }
            
            if (accuracy) {
                if (!handled) {
                    
                    //
                    // Something like 123``EOF
                    //
                    
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
            
            if (!expectDigits(Ctxt)) {

                //
                // Something like 123*^
                //

                return TOKEN_ERROR_EXPECTEDEXPONENT;
            }
            
            if (c == WLCharacter('.')) {
                
                //
                // Something like 123*^.5
                //

                c = nextWLCharacter(INSIDE_NUMBER);
                
                return TOKEN_ERROR_EXPECTEDEXPONENT;
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
            
            if (real) {
                return TOKEN_REAL;
            } else {
                return TOKEN_INTEGER;
            }
        }
    }
    
    if (real) {
        return TOKEN_REAL;
    } else {
        return TOKEN_INTEGER;
    }
}

bool Tokenizer::handleFractionalPart(TokenizerContext Ctxt, int base) {
    
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

        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space before the . to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{DigitLoc,Loc}));
        
        Issues.push_back(Issue);

        
        Loc = TheSourceManager->getSourceLocation();
        
        characterQueue.push_back(std::make_pair(c, Loc));
        setCurrentWLCharacter(WLCharacter('.'), Loc);
        
        
        return false; 
    }

    String.put('.');

    if (c.isDigitOrAlpha()) {
        
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
        auto Loc1 = Loc2;
        Loc1.Col--;

        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space before the . to reduce ambiguity", SYNTAXISSUESEVERITY_ERROR, (SourceSpan{Loc1,Loc2}));
        
        Issues.push_back(Issue);
    }

    return true;
}

bool Tokenizer::expectDigits(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    if (c.isDigit()) {
        
        handleDigits(Ctxt);
        
        return true;
        
    } else {
        
        return false;
    }
}

//
// Return length of digits
//
size_t Tokenizer::handleDigits(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    auto Loc = TheSourceManager->getSourceLocation();

    auto len = 0;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (len == MAX_DIGITS_LENGTH) {
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXDIGITSLENGTH, std::string("Max digits length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
        
            Issues.push_back(Issue);
        }

        if (c.isDigit()) {
            
            String.put(c.to_char());
            
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

    auto Loc = TheSourceManager->getSourceLocation();

    auto len = 1;
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        if (len == MAX_DIGITS_LENGTH) {
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXDIGITSLENGTH, std::string("Max digits length reached."), SYNTAXISSUESEVERITY_REMARK, (SourceSpan{Loc,Loc}));
        
            Issues.push_back(Issue);
        }

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

        len++;
    }
    
    return true;
}

Token Tokenizer::handleOperator(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    Token Operator = TOKEN_UNKNOWN;

    switch (c.to_point()) {
        case ':': {
            Operator = TOKEN_COLON;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case ':': {
                    Operator = TOKEN_COLONCOLON;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (Ctxt.EnableStringifyNextToken) {
                        stringifyNextToken_symbol = true;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_COLONEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '>': {
                    Operator = TOKEN_COLONGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '(': {
            Operator = TOKEN_OPENPAREN;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('*')) {
                
                return handleComment(Ctxt);
            }
        }
            break;
        case ')': {
            Operator = TOKEN_CLOSEPAREN;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '[': {
            
            Operator = TOKEN_OPENSQUARE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case ']': {
            Operator = TOKEN_CLOSESQUARE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case ',': {
            Operator = TOKEN_COMMA;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '{': {
            Operator = TOKEN_OPENCURLY;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '}': {
            Operator = TOKEN_CLOSECURLY;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '=': {
            Operator = TOKEN_EQUAL;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_EQUALEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_EQUALEQUALEQUAL;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '!': {
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        Operator = TOKEN_EQUALBANGEQUAL;
                        
                        String.put('!');
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('!'), Loc);
                        
                        Operator = TOKEN_EQUAL;
                    }
                }
                    break;
            }
        }
            break;
        case '_': {
            Operator = TOKEN_UNDER;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '_': {
                    Operator = TOKEN_UNDERUNDER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('_')) {
                        
                        Operator = TOKEN_UNDERUNDERUNDER;
                        
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

                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between _ and . to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{UnderLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        
                        Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('.'), Loc);
                        
                        
                        Operator = TOKEN_UNDER;
                        
                    } else if (c.isDigit()) {
                        
                        String.put('.');
                        
                        //
                        // Something like _.0
                        //
                        
                        auto UnderLoc = Loc;
                        UnderLoc.Col--;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between . and number to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{UnderLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        Operator = TOKEN_UNDERDOT;
                        
                    } else {
                        
                        String.put('.');
                        
                        Operator = TOKEN_UNDERDOT;
                    }
                }
                    break;
            }
            
            return Operator;
        }
            break;
        case '<': {
            Operator = TOKEN_LESS;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '|': {
                    Operator = TOKEN_LESSBAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '<': {
                    Operator = TOKEN_LESSLESS;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (Ctxt.EnableStringifyNextToken) {
                        stringifyNextToken_file = true;
                    }
                }
                    break;
                case '>': {
                    Operator = TOKEN_LESSGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_LESSEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('>')) {
                        
                        Operator = TOKEN_LESSMINUSGREATER;
                        
                        String.put('-');
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('-'), Loc);
                        
                        Operator = TOKEN_LESS;
                    }
                }
                    break;
            }
        }
            break;
        case '>': {
            Operator = TOKEN_GREATER;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '>': {
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c.to_point() == '>') {
                        
                        Operator = TOKEN_GREATERGREATERGREATER;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                        if (Ctxt.EnableStringifyNextToken) {
                            stringifyNextToken_file = true;
                        }
                        
                    } else {
                        
                        Operator = TOKEN_GREATERGREATER;
                        
                        if (Ctxt.EnableStringifyNextToken) {
                            stringifyNextToken_file = true;
                        }
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_GREATEREQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '-': {
            Operator = TOKEN_MINUS;
            
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
                    Operator = TOKEN_MINUSGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    Operator = TOKEN_MINUSMINUS;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('>')) {
                        
                        //
                        // Something like a-->0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto MinusLoc = Loc;
                        MinusLoc.Col--;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between - and > to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{MinusLoc,Loc}));
                        
                        Issues.push_back(Issue);
                        
                    } else if (c == WLCharacter('=')) {
                        
                        //
                        // Something like a--=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto MinusLoc = Loc;
                        MinusLoc.Col--;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between - and = to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{MinusLoc,Loc}));
                        
                        Issues.push_back(Issue);
                        
                    }
                    
                }
                    break;
                case '=': {
                    Operator = TOKEN_MINUSEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '|': {
            Operator = TOKEN_BAR;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '>': {
                    Operator = TOKEN_BARGREATER;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        //
                        // Something like <||>=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto GreaterLoc = Loc;
                        GreaterLoc.Col--;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between > and = to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{GreaterLoc,Loc}));
                        
                        Issues.push_back(Issue);
                        
                    }
                    
                }
                    break;
                case '|': {
                    Operator = TOKEN_BARBAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case ';': {
            Operator = TOKEN_SEMI;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter(';')) {
                
                Operator = TOKEN_SEMISEMI;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
            }
        }
            break;
        case '!': {
            Operator = TOKEN_BANG;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_BANGEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '!': {
                    Operator = TOKEN_BANGBANG;
                    
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
            // From Slot documentation:
            //
            // In the form #name, the characters in name can be any combination of alphanumeric characters not beginning with digits.
            //
            //
            // A slot that starts with a digit goes down one path
            // And a slot that starts with a letter does down another path
            //
            // Make sure e.g. #1a is not parsed as SlotNode["#1a"]
            //
            
            if (c.isDigit()) {
                
                Operator = TOKEN_HASH;
                
                handleDigits(Ctxt);
                
            } else if (c.isAlpha()) {
        
                Operator = TOKEN_HASH;
                
                Ctxt.SlotFlag = true;

                handleSymbol(Ctxt);
                
            } else if (c == WLCharacter('$') || c == WLCharacter('`')) {
                
                Operator = TOKEN_HASH;
                
                Ctxt.SlotFlag = true;

                handleSymbol(Ctxt);
                
            } else if (c == WLCharacter('"')) {
                
                auto Loc = TheSourceManager->getSourceLocation();

                Operator = TOKEN_HASH;
                
                handleString(Ctxt);

                auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT, "This syntax is not documented.", SYNTAXISSUESEVERITY_WARNING, (SourceSpan{Loc,Loc}));
                        
                Issues.push_back(Issue);
                
            } else if (c == WLCharacter('#')) {
                
                Operator = TOKEN_HASHHASH;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
                
                if (c.isDigit()) {
                    
                    handleDigits(Ctxt);
                }

            } else {
                
                Operator = TOKEN_HASH;
            }
        }
            break;
        case '%': {
            Operator = TOKEN_PERCENT;
            
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
                
                handleDigits(Ctxt);
            }
        }
            break;
        case '&': {
            Operator = TOKEN_AMP;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('&')) {
                
                Operator = TOKEN_AMPAMP;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
            }
        }
            break;
        case '/': {
            Operator = TOKEN_SLASH;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '@': {
                    Operator = TOKEN_SLASHAT;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case ';': {
                    Operator = TOKEN_SLASHSEMI;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '.': {
                    
                    auto Loc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter();
                    
                    if (c.isDigit()) {
                        
                        //
                        // Something like Round[t/.03 + 1]
                        //
                        
                        auto SlashLoc = Loc;
                        SlashLoc.Col--;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between / and . to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{SlashLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        
                        Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter(WLCharacter('.'), Loc);
                        
                        
                        Operator = TOKEN_SLASH;
                        
                    } else {
                        
                        String.put('.');
                        
                        Operator = TOKEN_SLASHDOT;
                    }
                }
                    break;
                case '/': {
                    Operator = TOKEN_SLASHSLASH;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    switch (c.to_point()) {
                        case '.': {
                            Operator = TOKEN_SLASHSLASHDOT;
                            
                            String.put(c.to_char());
                            
                            c = nextWLCharacter();
                        }
                            break;
                        case '@': {
                            Operator = TOKEN_SLASHSLASHAT;
                            
                            String.put(c.to_char());
                            
                            c = nextWLCharacter();
                        }
                            break;
                    }
                }
                    break;
                case ':': {
                    Operator = TOKEN_SLASHCOLON;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = TOKEN_SLASHEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = TOKEN_SLASHSTAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '@': {
            Operator = TOKEN_AT;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '@': {
                    Operator = TOKEN_ATAT;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('@')) {
                        
                        Operator = TOKEN_ATATAT;
                        
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '*': {
                    Operator = TOKEN_ATSTAR;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '+': {

            Operator = TOKEN_PLUS;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '+': {
                    Operator = TOKEN_PLUSPLUS;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                    
                    if (c == WLCharacter('=')) {
                        
                        //
                        // Something like a++=0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        auto PlusLoc = Loc;
                        PlusLoc.Col--;
                        
                        auto Issue = SyntaxIssue(SYNTAXISSUETAG_SYNTAXAMBIGUITY, "Put a space between + and = to reduce ambiguity", SYNTAXISSUESEVERITY_REMARK, (SourceSpan{PlusLoc,Loc}));
                        
                        Issues.push_back(Issue);
                        
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_PLUSEQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '~': {
            Operator = TOKEN_TILDE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            if (c == WLCharacter('~')) {
                
                Operator = TOKEN_TILDETILDE;
                
                String.put(c.to_char());
                
                c = nextWLCharacter();
            }
        }
            break;
        case '?': {
            Operator = TOKEN_QUESTION;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        case '*': {
            Operator = TOKEN_STAR;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
            
            switch (c.to_point()) {
                case '=': {
                    Operator = TOKEN_STAREQUAL;
                    
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = TOKEN_STARSTAR;
                    
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
                        
                        Operator = TOKEN_CARETCOLONEQUAL;
                        
                        String.put('^');
                        String.put(':');
                        String.put(c.to_char());
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        String.put('^');
                        String.put(':');
                        
                        Operator = TOKEN_ERROR_EXPECTEDEQUAL;
                    }
                }
                    break;
                case '=': {
                    Operator = TOKEN_CARETEQUAL;
                    
                    String.put('^');
                    String.put(c.to_char());
                    
                    c = nextWLCharacter();
                }
                    break;
                default: {
                    Operator = TOKEN_CARET;
                    
                    String.put('^');
                }
                    break;
            }
        }
            break;
        case '\'': {
            Operator = TOKEN_SINGLEQUOTE;
            
            String.put(c.to_char());
            
            c = nextWLCharacter();
        }
            break;
        default: {
            
            assert(c.isOperatorCharacter());
            
            //
            // Do not use String.put(c.to_char()); here because c may not be a char
            //
            auto p = c.preferredSource();
            String << p;
            
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
Token Tokenizer::handleDot(TokenizerContext Ctxt) {
    
    auto c = currentWLCharacter();

    assert(c == WLCharacter('.'));
    
    c = nextWLCharacter();
    
    if (c.isDigit()) {
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        characterQueue.push_back(std::make_pair(c, Loc));
        setCurrentWLCharacter(WLCharacter('.'), Loc);
        
        return handleNumber(Ctxt);
    }
    
    String.put('.');
    
    auto Operator = TOKEN_DOT;
    
    if (c == WLCharacter('.')) {
        
        Operator = TOKEN_DOTDOT;
        
        String.put(c.to_char());
        
        c = nextWLCharacter();
        
        if (c == WLCharacter('.')) {
            
            Operator = TOKEN_DOTDOTDOT;
            
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
    
    return Tmp;
}

Tokenizer *TheTokenizer = nullptr;
