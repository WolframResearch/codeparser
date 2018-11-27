
#include "Tokenizer.h"

#include "CharacterDecoder.h"
#include "CodePoint.h"
#include "LongNameMap.h"

#include <iomanip>
#include <cassert>

Tokenizer::Tokenizer() : _stringifyNextToken(false), cur(TOKEN_UNKNOWN), currentCached(false) {}

void Tokenizer::init(bool skipFirstLine) {

    auto c = nextWLCharacter();

    if (skipFirstLine) {
        while (true) {
            if (c == '\n') {
                c = nextWLCharacter();
                break;
            } else if (c == EOF) {
                c = nextWLCharacter();
                break;
            } else {
                c = nextWLCharacter();
            }
        }
    }
}

Token Tokenizer::nextToken() {
    
    assert(String.str().empty());
    
    TheSourceManager->setTokenStart();
    
    if (_stringifyNextToken) {
        
        cur = handleString();
        
        TheSourceManager->setTokenEnd();
        
        return cur;
    }
    
    auto c = currentWLCharacter();

    switch (c) {
        case 'a': case 'A': case 'b': case 'B': case 'c': case 'C': case 'd': case 'D': case 'e': case 'E': case 'f': case 'F':
        case 'g': case 'G': case 'h': case 'H': case 'i': case 'I': case 'j': case 'J': case 'k': case 'K': case 'l': case 'L':
        case 'm': case 'M': case 'n': case 'N': case 'o': case 'O': case 'p': case 'P': case 'q': case 'Q': case 'r': case 'R':
        case 's': case 'S': case 't': case 'T': case 'u': case 'U': case 'v': case 'V': case 'w': case 'W': case 'x': case 'X':
        case 'y': case 'Y': case 'z': case 'Z':
            
            cur = handleSymbol();
            break;
            
        case '$': case '`':
            
            cur = handleSymbol();
            break;
            
        case '"':
            
            cur = handleString();
            break;
            
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            
            cur = handleNumber();
            break;
            
        case '\n':
            
            c = nextWLCharacter();
            
            cur = TOKEN_NEWLINE;
            break;
            
        case ' ': case '\t':
            
            while (c == ' ' || c == '\t') {

                String.put(c);
                
                c = nextWLCharacter();
            }
            
            cur = TOKEN_SPACE;
            break;
            
        case '.':
            
            cur = handleDot();
            break;
            
        case '(': case ')': case '[': case ']': case ',': case '{': case '}': case ':': case '=': case '_': case '<': case '>':
        case '-': case '|': case ';': case '!': case '#': case '&': case '/': case '@': case '+': case '~': case '?': case '*':
        case '^': case '%': case '\'':
            
            cur = handleOperator();
            break;
        
        case LINEARSYNTAX_BANG: case LINEARSYNTAX_PERCENT: case LINEARSYNTAX_AMP: case LINEARSYNTAX_OPENPAREN: case LINEARSYNTAX_CLOSEPAREN:
        case LINEARSYNTAX_STAR: case LINEARSYNTAX_PLUS: case LINEARSYNTAX_SLASH: case LINEARSYNTAX_AT: case LINEARSYNTAX_CARET:
        case LINEARSYNTAX_UNDER: case LINEARSYNTAX_BACKTICK: case LINEARSYNTAX_SPACE:

            cur = handleLinearSyntax();
            break;
            
        case EOF:
            
            TheSourceManager->setEOF();
            
            cur = TOKEN_EOF;
            break;
            
        default: {
            
            // everything else involving Unicode or errors
            
            if (isLetterlikeCodePoint(c)) {
                
                cur = handleSymbol();
                break;
                
            } else if (isSpaceCodePoint(c)) {
                
                c = nextWLCharacter();
                
                cur = TOKEN_SPACE;
                break;
                
            } else if (isNewlineCodePoint(c)) {
                
                c = nextWLCharacter();
                
                cur = TOKEN_NEWLINE;
                break;
                
            } else if (isCommaCodePoint(c)) {
                
                c = nextWLCharacter();
                
                cur = OPERATOR_COMMA;
                break;
                
            } else if (isOperatorCodePoint(c)) {
                
                cur = handleOperator();
                break;
                
            } else {
                
                String << WLCharacterToString(c);
                
                c = nextWLCharacter();

                cur = ERROR_UNHANDLEDCHARACTER;
                break;
            }
        }
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
    return cur;
}

Token Tokenizer::handleLinearSyntax() {
    
    auto c = currentWLCharacter();

    String << WLCharacterToString(c);
    
    Token Operator;
    
    switch (c) {
        case LINEARSYNTAX_BANG:
            Operator = OPERATOR_LINEARSYNTAX_BANG;
            break;
        case LINEARSYNTAX_OPENPAREN:
            Operator = OPERATOR_LINEARSYNTAX_OPENPAREN;
            break;
        case LINEARSYNTAX_STAR:
            Operator = OPERATOR_LINEARSYNTAX_STAR;
            break;
        case LINEARSYNTAX_CLOSEPAREN:
            Operator = OPERATOR_LINEARSYNTAX_CLOSEPAREN;
            break;
        case LINEARSYNTAX_AT:
            Operator = OPERATOR_LINEARSYNTAX_AT;
            break;
        case LINEARSYNTAX_CARET:
            Operator = OPERATOR_LINEARSYNTAX_CARET;
            break;
        case LINEARSYNTAX_UNDER:
            Operator = OPERATOR_LINEARSYNTAX_UNDER;
            break;
        case LINEARSYNTAX_PERCENT:
            Operator = OPERATOR_LINEARSYNTAX_PERCENT;
            break;
        case LINEARSYNTAX_AMP:
            Operator = OPERATOR_LINEARSYNTAX_AMP;
            break;
        case LINEARSYNTAX_SLASH:
            Operator = OPERATOR_LINEARSYNTAX_SLASH;
            break;
        case LINEARSYNTAX_PLUS:
            Operator = OPERATOR_LINEARSYNTAX_PLUS;
            break;
        case LINEARSYNTAX_BACKTICK:
            Operator = OPERATOR_LINEARSYNTAX_BACKTICK;
            break;
        case LINEARSYNTAX_SPACE:
            Operator = OPERATOR_LINEARSYNTAX_SPACE;
            break;
        default:
            //
            // Something like \\ or \" or \space is being used outside of a string
            //
            Operator = ERROR_UNHANDLEDCHARACTER;
            break;
    }
    
    c = nextWLCharacter();

    cur = Operator;
    
    return cur;
}

//
// Do not bother decoding \:zzz into a character inside handleComment
//
// People are allowed to have weird stuff in comments
//
Token Tokenizer::handleComment() {
    // comment is already started
    
    auto c = currentWLCharacter();

    assert(c == '*');
    
    String.put('(');
    String.put('*');
    
    auto depth = 1;

    c = TheByteDecoder->nextSourceCharacter();
    
    if (c == EOF) {
        return TOKEN_EOF;
    }
    
    while (true) {
        
        if (c == '(') {
            
            String.put(c);
            
            c = TheByteDecoder->nextSourceCharacter();
            
            if (c == EOF) {
                return TOKEN_EOF;
            }
            
            if (c == '*') {
                
                String.put(c);
                
                c = TheByteDecoder->nextSourceCharacter();
                
                if (c == EOF) {
                    return TOKEN_EOF;
                }
                
                depth = depth + 1;
            }
            
        } else if (c == '*') {
            
            String.put(c);

            c = TheByteDecoder->nextSourceCharacter();
            
            if (c == EOF) {
                return TOKEN_EOF;
            }
            
            if (c == ')') {
                
                String.put(c);

                // This comment is closing
                
                depth = depth - 1;
                
                if (depth == 0) {
                    
                    // Leaving comments, make sure to grab next character
                    
                    c = nextWLCharacter();
                    
                    break;
                    
                } else {
                    
                    // Still in comments, we do not care about characters
                    
                    c = TheByteDecoder->nextSourceCharacter();
                    
                    if (c == EOF) {
                        return TOKEN_EOF;
                    }
                }
            }
            
        } else {
            
            String.put(c);

            c = TheByteDecoder->nextSourceCharacter();
            
            if (c == EOF) {
                return TOKEN_EOF;
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

    assert(c == '`' || isAlphaOrDollar(c) || isLetterlikeCodePoint(c));
    
    if (isAlphaOrDollar(c) || isLetterlikeCodePoint(c)) {
        handleSymbolSegment();
    }
    
    c = currentWLCharacter();
    
    while (c == '`') {
        
        String.put(c);
        
        c = nextWLCharacter();
        
        if (isAlphaOrDollar(c) || isLetterlikeCodePoint(c)) {
            handleSymbolSegment();
        } else {
            
            cur = ERROR_UNHANDLEDCHARACTER;
            return cur;
        }
        
        c = currentWLCharacter();
        
    } // while
    
    return TOKEN_SYMBOL;
}

void Tokenizer::handleSymbolSegment() {
    
    auto c = currentWLCharacter();

    assert(isAlphaOrDollar(c) || isLetterlikeCodePoint(c));
    
    if (isAlphaOrDollar(c)) {
        
        String.put(c);
        
        c = nextWLCharacter();
        
    } else {
        
        if (isStrangeLetterlikeCodePoint(c)) {
            
            auto Span = TheSourceManager->getWLCharacterSpan();
            
            auto Issue = SyntaxIssue("Strange character in symbol: " + WLCharacterToString(c), SEVERITY_ERROR, Span);

            Issues.push_back(Issue);
        }
        
        String << WLCharacterToString(c);
        
        c = nextWLCharacter();
    }
    
    while (true) {
        
        if (isDigitOrAlphaOrDollar(c)) {
            
            String.put(c);
            
            c = nextWLCharacter();
            
        } else if (isLetterlikeCodePoint(c)) {
            
            if (isStrangeLetterlikeCodePoint(c)) {
                
                auto Span = TheSourceManager->getWLCharacterSpan();
                
                auto Issue = SyntaxIssue("Strange character in symbol: " + WLCharacterToString(c), SEVERITY_ERROR, Span);

                Issues.push_back(Issue);
            }
            
            String << WLCharacterToString(c);
            
            c = nextWLCharacter();
            
        } else {
            break;
        }
        
    } // while
}

Token Tokenizer::handleString() {
    
    auto c = currentWLCharacter();

    assert(c == '"' || _stringifyNextToken);
    
    if (_stringifyNextToken && c != '"') {
        
        //
        // magically turn into a string
        //
        
        // first eat the spaces
        while (c == ' ') {
            
            c = nextWLCharacter();
        }
        
        auto empty = true;
        while (true) {
            
            if (isAlpha(c) || isDigit(c) || c == '`' || c == '$' || c == '.' || c == '_' || c == '/') {
                
                empty = false;

                String.put(c);
                
                c = nextWLCharacter();
                
            } else {
                
                break;
            }
            
        } // while
        
        _stringifyNextToken = false;
        
        if (empty) {

            //
            // Something like a::EOF
            //

            return ERROR_EMPTYSTRING;
        }

        return TOKEN_STRING;
        
    } else {
        
        String.put('"');

        while (true) {
            
            c = nextWLCharacter();
            
            if (c == EOF) {
                
                return TOKEN_EOF;
                
            } else if (c == '"') {
                
                break;
                
            } else if (c == '\\') {
                
                String.put(c);
                
                // Clear Issues
                auto Tmp = TheCharacterDecoder->getIssues();
                
                std::copy(Tmp.begin(), Tmp.end(), std::back_inserter(Issues));
                
                c = nextWLCharacter();
                
                String.put(c);
                
            } else {

                if (isLinearSyntax(c)) {

                    auto Span = TheSourceManager->getWLCharacterSpan();

                    auto Issue = SyntaxIssue("Linear syntax character in string", SEVERITY_WARNING, Span);

                    Issues.push_back(Issue);
                }

                String << WLCharacterToString(c);
            }
            
        } // while
        
        String.put('"');
        
        c = nextWLCharacter();
        
        if (_stringifyNextToken) {
            _stringifyNextToken = false;
        }

        return TOKEN_STRING;
    }
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
    if (c == '^') {
        
        c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
        
        if (c == '^') {
            
            base = parseInteger(String.str(), 10);
            
            if (base < 2 || base > 36) {
                
                cur = ERROR_INVALIDBASE;
                return cur;
            }
            
            String.put('^');
            String.put(c);
            
            c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            
            if (isDigitOrAlpha(c)) {
                
                if (!handleDigitsOrAlpha(base)) {
                    
                    cur = ERROR_UNHANDLEDCHARACTER;
                    return cur;
                }
                
            } else {
                
                String.str("");
                
                cur = ERROR_EXPECTEDDIGITORALPHA;
                return cur;
            }
            
        } else {
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            characterQueue.push_back(std::make_pair(c, Loc));
            setCurrentWLCharacter('^', Loc);
            
            return TOKEN_NUMBER;
        }
    }
    
    if (c == '.') {
        
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
    if (c == '`') {
        
        String.put('`');
        
        c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
        
        bool accuracy = false;
        if (c == '`') {
            
            String.put('`');
            
            c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            
            accuracy = true;
        }
        
        if (accuracy || isDigit(c) || c == '-' || c == '+' || c == '.') {
            
            if (c == '-' || c == '+') {
                
                auto s = c;
                
                auto Loc = TheSourceManager->getSourceLocation();
                
                c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
                
                if (isDigit(c)) {
                    
                    String.put(s);
                    
                } else if (c == '.') {
                    
                    String.put(s);
                    
                } else if (accuracy) {
                    
                    //
                    // Something like 1.2``->3
                    //
                    
                    cur = ERROR_EXPECTEDACCURACY;
                    return cur;
                    
                } else {

                    //
                    // Something like 1.2`->3
                    //

                    std::string msg;
                    if (s == '-') {
                        msg = "Put a space between ` and - to reduce ambiguity";
                    } else {
                        msg = "Put a space between ` and + to reduce ambiguity";
                    }
                    auto Issue = SyntaxIssue(msg, SEVERITY_REMARK, (SourceSpan{Loc, Loc}));
                
                    Issues.push_back(Issue);
                    
                    
                    Loc = TheSourceManager->getSourceLocation();
                    
                    characterQueue.push_back(std::make_pair(c, Loc));
                    setCurrentWLCharacter(s, Loc);
                    
                    
                    return TOKEN_NUMBER;
                }
            }
            
            bool handled = false;
            
            if (isDigit(c)) {
                
                handleDigits();
                
                handled = true;
            }
            
            c = currentWLCharacter();
            
            if (c == '.') {
                
                if (handleFractionalPart(10)) {
                    
                    handled = true;
                    
                } else if (!accuracy) {
                    
                    return TOKEN_NUMBER;
                }
            }
            
            if (accuracy) {
                if (!handled) {
                    cur = ERROR_EXPECTEDACCURACY;
                    return cur;
                }
            }
        }
    }
    
    c = currentWLCharacter();
    
    if (c == '*') {
        
        c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
        
        if (c == '^') {
            
            String.put('*');
            String.put(c);
            
            c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            
            if (c == '-' || c == '+') {
                
                String.put(c);
                
                c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            }
            
            if (!expectDigits()) {
                
                cur = ERROR_UNHANDLEDCHARACTER;
                return cur;
            }
            
            if (c == '.') {
                
                c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
                
                cur = ERROR_EXPONENT;
                return cur;
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
            setCurrentWLCharacter('*', Loc);
            
            return TOKEN_NUMBER;
        }
    }
    
    return TOKEN_NUMBER;
}

bool Tokenizer::handleFractionalPart(int base) {
    
    auto c = currentWLCharacter();

    assert(c == '.');
    
    auto Loc = TheSourceManager->getSourceLocation();
    
    c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
    
    if (c == '.') {
        
        //
        // Something like 0..
        //
        
        auto Issue = SyntaxIssue("Put a space between number and . to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));
        
        Issues.push_back(Issue);

        
        Loc = TheSourceManager->getSourceLocation();
        
        characterQueue.push_back(std::make_pair(c, Loc));
        setCurrentWLCharacter('.', Loc);
        
        
        return false;
        
    } else {
        
        String.put('.');
        
        if (isDigitOrAlpha(c)) {
            
            if (!handleDigitsOrAlpha(base)) {
                return false;
            }
        }
    }
    
    return true;
}

bool Tokenizer::expectDigits() {
    
    auto c = currentWLCharacter();

    if (isDigit(c)) {
        
        handleDigits();
        
        return true;
        
    } else {
        
        return false;
    }
}

void Tokenizer::handleDigits() {
    
    auto c = currentWLCharacter();

    while (true) {
        
        if (isDigit(c)) {
            
            String.put(c);
            
            c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            
        } else {
            
            break;
        }
    }
}

bool Tokenizer::handleDigitsOrAlpha(int base) {
    
    auto c = currentWLCharacter();

    while (true) {
        
        if (isDigitOrAlpha(c)) {
            
            int baseDigit = toBaseDigit(c);
            
            if (baseDigit == -1 || baseDigit >= base) {
                return false;
            }
            
            String.put(c);
            
            c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            
        } else {
            break;
        }
    }
    
    return true;
}

void Tokenizer::handleDigitsOrAlphaOrDollar() {
    
    auto c = currentWLCharacter();

    while (true) {
        
        if (isDigitOrAlphaOrDollar(c)) {
            
            String.put(c);
            
            c = nextWLCharacter(POLICY_DO_NOT_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION);
            
        } else {
            break;
        }
    }
}

Token Tokenizer::handleOperator() {
    
    auto c = currentWLCharacter();

    Token Operator;
    
    switch (c) {
        case ':': {
            Operator = OPERATOR_COLON;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case ':': {
                    Operator = OPERATOR_COLONCOLON;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    _stringifyNextToken = true;
                }
                    break;
                case '=': {
                    Operator = OPERATOR_COLONEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '>': {
                    Operator = OPERATOR_COLONGREATER;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '(': {
            Operator = OPERATOR_OPENPAREN;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            if (c == '*') {
                
                String.str("");
                
                return handleComment();
            }
        }
            break;
        case ')': {
            Operator = OPERATOR_CLOSEPAREN;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case '[': {
            
            Operator = OPERATOR_OPENSQUARE;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case ']': {
            Operator = OPERATOR_CLOSESQUARE;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case ',': {
            Operator = OPERATOR_COMMA;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case '{': {
            Operator = OPERATOR_OPENCURLY;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case '}': {
            Operator = OPERATOR_CLOSECURLY;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case '=': {
            Operator = OPERATOR_EQUAL;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '=': {
                    Operator = OPERATOR_EQUALEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    if (c == '=') {
                        
                        Operator = OPERATOR_EQUALEQUALEQUAL;
                        
                        String.put(c);
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '!': {
                    Operator = OPERATOR_UNKNOWN;
                    
                    c = nextWLCharacter();
                    
                    if (c == '=') {
                        
                        Operator = OPERATOR_EQUALBANGEQUAL;
                        
                        String.put('!');
                        String.put(c);
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter('!', Loc);
                        
                        Operator = OPERATOR_EQUAL;
                    }
                }
                    break;
            }
        }
            break;
        case '_': {
            Operator = OPERATOR_UNDER;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '_': {
                    Operator = OPERATOR_UNDERUNDER;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    if (c == '_') {
                        
                        Operator = OPERATOR_UNDERUNDERUNDER;
                        
                        String.put(c);
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '.': {
                    Operator = OPERATOR_UNKNOWN;
                    
                    auto Loc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter();
                    
                    if (c == '.') {
                        
                        //
                        // Could be _... which should parse as _ ...
                        //
                        
                        auto Issue = SyntaxIssue("Put a space between _ and . to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));
                        
                        Issues.push_back(Issue);

                        
                        Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter('.', Loc);
                        
                        
                        Operator = OPERATOR_UNDER;
                        
                    } else if (isDigit(c)) {
                        
                        String.put('.');
                        
                        //
                        // Something like _.0
                        //
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        auto Issue = SyntaxIssue("Put a space between . and number to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{Loc,Loc}));
                        
                        Issues.push_back(Issue);

                        Operator = OPERATOR_UNDERDOT;
                        
                    } else {
                        
                        String.put('.');
                        
                        Operator = OPERATOR_UNDERDOT;
                    }
                }
                    break;
            }
            
            return Operator;
        }
            break;
        case '<': {
            Operator = OPERATOR_LESS;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '|': {
                    Operator = OPERATOR_LESSBAR;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '<': {
                    Operator = OPERATOR_LESSLESS;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    _stringifyNextToken = true;
                }
                    break;
                case '>': {
                    Operator = OPERATOR_LESSGREATER;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = OPERATOR_LESSEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    Operator = OPERATOR_UNKNOWN;
                    
                    c = nextWLCharacter();
                    
                    if (c == '>') {
                        
                        Operator = OPERATOR_LESSMINUSGREATER;
                        
                        String.put('-');
                        String.put(c);
                        
                        c = nextWLCharacter();
                        
                    } else {
                        
                        auto Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter('-', Loc);
                        
                        
                        Operator = OPERATOR_LESS;
                    }
                }
                    break;
            }
        }
            break;
        case '>': {
            Operator = OPERATOR_GREATER;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '>': {
                    Operator = OPERATOR_GREATERGREATER;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    _stringifyNextToken = true;
                }
                    break;
                case '=': {
                    Operator = OPERATOR_GREATEREQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '-': {
            Operator = OPERATOR_MINUS;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            //
            // Do not lex as a number here
            // Makes it easier to handle invisible multiplication later
            //
            // Because if we lexed - as a number here, then it is
            // harder to know that b-1 is Plus[b, -1] instead of
            // b<invisiblespace>-1 which is Times[b, -1]
            //
            
            switch (c) {
                case '>': {
                    Operator = OPERATOR_MINUSGREATER;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '-': {
                    Operator = OPERATOR_MINUSMINUS;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = OPERATOR_MINUSEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '|': {
            Operator = OPERATOR_BAR;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '>': {
                    Operator = OPERATOR_BARGREATER;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '|': {
                    Operator = OPERATOR_BARBAR;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case ';': {
            Operator = OPERATOR_SEMICOLON;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            if (c == ';') {
                
                Operator = OPERATOR_SEMICOLONSEMICOLON;
                
                String.put(c);
                
                c = nextWLCharacter();
            }
        }
            break;
        case '!': {
            Operator = OPERATOR_BANG;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '=': {
                    Operator = OPERATOR_BANGEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '!': {
                    Operator = OPERATOR_BANGBANG;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '#': {
            
            // auto Loc = TheSourceManager->getNextLoc();
            
            Operator = OPERATOR_UNKNOWN;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            if (isDigitOrAlphaOrDollar(c)) {
                
                Operator = OPERATOR_HASH;
                
                handleDigitsOrAlphaOrDollar();
                
                return Operator;
                
            } else if (c == '#') {
                
                Operator = OPERATOR_HASHHASH;
                
                String.put(c);
                
                c = nextWLCharacter();
                
                if (isDigit(c)) {
                    
                    handleDigits();
                    
                    return Operator;
                }

            } else {
                
                Operator = OPERATOR_HASH;
            }
        }
            break;
        case '%': {
            Operator = OPERATOR_PERCENT;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            if (c == '%') {
                
                while (true) {
                    
                    if (c != '%') {
                        break;
                    }
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                
                return Operator;
                
            } else if (isDigit(c)) {
                
                handleDigits();
                
                return Operator;
            }
        }
            break;
        case '&': {
            Operator = OPERATOR_AMP;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            if (c == '&') {
                
                Operator = OPERATOR_AMPAMP;
                
                String.put(c);
                
                c = nextWLCharacter();
            }
        }
            break;
        case '/': {
            Operator = OPERATOR_SLASH;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '@': {
                    Operator = OPERATOR_SLASHAT;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case ';': {
                    Operator = OPERATOR_SLASHSEMICOLON;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '.': {
                    Operator = OPERATOR_UNKNOWN;
                    
                    auto Loc = TheSourceManager->getSourceLocation();
                    
                    c = nextWLCharacter();
                    
                    if (isDigit(c)) {
                        
                        //
                        // Something like Round[t/.03 + 1]
                        //
                        
                        auto SlashLoc = Loc;
                        SlashLoc.Col--;
                        
                        auto Issue = SyntaxIssue("Put a space between / and . to reduce ambiguity", SEVERITY_REMARK, (SourceSpan{SlashLoc,Loc}));
                        
                        Issues.push_back(Issue);

                        
                        Loc = TheSourceManager->getSourceLocation();
                        
                        characterQueue.push_back(std::make_pair(c, Loc));
                        setCurrentWLCharacter('.', Loc);
                        
                        
                        Operator = OPERATOR_SLASH;
                        
                    } else {
                        
                        String.put('.');
                        
                        Operator = OPERATOR_SLASHDOT;
                    }
                }
                    break;
                case '/': {
                    Operator = OPERATOR_SLASHSLASH;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    switch (c) {
                        case '.': {
                            Operator = OPERATOR_SLASHSLASHDOT;
                            
                            String.put(c);
                            
                            c = nextWLCharacter();
                        }
                            break;
                        case '@': {
                            Operator = OPERATOR_SLASHSLASHAT;
                            
                            String.put(c);
                            
                            c = nextWLCharacter();
                        }
                            break;
                    }
                }
                    break;
                case ':': {
                    Operator = OPERATOR_SLASHCOLON;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = OPERATOR_SLASHEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = OPERATOR_SLASHSTAR;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '@': {
            Operator = OPERATOR_AT;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '@': {
                    Operator = OPERATOR_ATAT;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                    
                    if (c == '@') {
                        
                        Operator = OPERATOR_ATATAT;
                        
                        String.put(c);
                        
                        c = nextWLCharacter();
                    }
                }
                    break;
                case '*': {
                    Operator = OPERATOR_ATSTAR;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '+': {

            Operator = OPERATOR_PLUS;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '+': {
                    Operator = OPERATOR_PLUSPLUS;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '=': {
                    Operator = OPERATOR_PLUSEQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '~': {
            Operator = OPERATOR_TILDE;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            if (c == '~') {
                
                Operator = OPERATOR_TILDETILDE;
                
                String.put(c);
                
                c = nextWLCharacter();
            }
        }
            break;
        case '?': {
            Operator = OPERATOR_QUESTION;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
            break;
        case '*': {
            Operator = OPERATOR_STAR;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            switch (c) {
                case '=': {
                    Operator = OPERATOR_STAREQUAL;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                case '*': {
                    Operator = OPERATOR_STARSTAR;
                    
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
            }
        }
            break;
        case '^': {
            
            Operator = OPERATOR_UNKNOWN;
            
            c = nextWLCharacter();
            
            switch (c) {
                case ':': {
                    
                    c = nextWLCharacter();
                    
                    if (c == '=') {
                        
                        Operator = OPERATOR_CARETCOLONEQUAL;
                        
                        String.put('^');
                        String.put(':');
                        String.put(c);
                        
                        c = nextWLCharacter();
                        
                    } else {

                        cur = ERROR_UNHANDLEDCHARACTER;
                        return cur;
                    }
                }
                    break;
                case '=': {
                    Operator = OPERATOR_CARETEQUAL;
                    
                    String.put('^');
                    String.put(c);
                    
                    c = nextWLCharacter();
                }
                    break;
                default: {
                    Operator = OPERATOR_CARET;
                    
                    String.put('^');
                }
                    break;
            }
        }
            break;
        case '\'': {
            Operator = OPERATOR_TICK;
            
            String.put(c);
            
            c = nextWLCharacter();
            
            while (true) {
                
                if (c != '\'') {
                    break;
                }
                
                String.put(c);
                
                c = nextWLCharacter();
            }
            
            return Operator;
        }
            break;
        default: {
            
            assert(isOperatorCodePoint(c));
            
            String << WLCharacterToString(c);
            
            Operator = LongNameCodePointToOperator(c);
            
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

    assert(c == '.');
    
    c = nextWLCharacter();
    
    if (isDigit(c)) {
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        characterQueue.push_back(std::make_pair(c, Loc));
        setCurrentWLCharacter('.', Loc);
        
        cur = handleNumber();
        return cur;
    }
    
    String.put('.');
    
    auto Operator = OPERATOR_DOT;
    
    if (c == '.') {
        
        Operator = OPERATOR_DOTDOT;
        
        String.put(c);
        
        c = nextWLCharacter();
        
        if (c == '.') {
            
            Operator = OPERATOR_DOTDOTDOT;
            
            String.put(c);
            
            c = nextWLCharacter();
        }
    }
    
    cur = Operator;
    return cur;
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
