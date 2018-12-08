
#include "CharacterDecoder.h"

#include "ByteDecoder.h"
#include "LongNameMap.h"

#include <iomanip>
#include <cassert>

CharacterDecoder::CharacterDecoder() : c(0), characterQueue(), Issues() {}

//
// Returns a useful character
//
// Keeps track of character counts
//
WLCharacter CharacterDecoder::nextWLCharacter(NextCharacterPolicy policy) {
    
    assert(Issues.empty());

    // handle the queue before anything else
    if (!characterQueue.empty()) {
        
        auto p = characterQueue[0];
        
        // erase first
        characterQueue.erase(characterQueue.begin());
        
        c = p.first;
        auto Loc = p.second;
        
        TheSourceManager->setSourceLocation(Loc);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();

        return c;
    }
    
    c = TheByteDecoder->nextSourceCharacter();
    
    if (c == EOF) {
        
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        return c;
    }
    
    if (c != '\\') {
        
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        return c;
    }
    
    //
    // Handle \
    //
    // handle escapes like line continuation and special characters
    //
    
    TheSourceManager->setWLCharacterStart();
    auto CharacterStart = TheSourceManager->getWLCharacterStart();
    c = TheByteDecoder->nextSourceCharacter();
    
    switch (c) {
        case '\r': {
            
            //
            // Ignore \r as part of line continuation
            //
            
            nextWLCharacter();
            while (c == '\r') {
                nextWLCharacter();
            }
        }
        case '\n': {
        
            //
            // Line continuation
            //
            
            nextWLCharacter();
            
            //
            // Process the white space and glue together pieces
            //
            if ((policy & PRESERVE_WS_AFTER_LC) != PRESERVE_WS_AFTER_LC) {
                while (c == ' ' || c == '\t') {
                    nextWLCharacter();
                }
            }
            
            //
            // return here
            //
            // nextCharacter() is called recursively if there is a line continuation,
            // but we want some functions to only be called once
            //
            return c;
        }
        case '[': {
            
            handleLongName(CharacterStart, policy);
        }
            break;
        case ':': {
            
            handle4Hex(CharacterStart, policy);
        }
            break;
        case '.': {
            
            handle2Hex(CharacterStart, policy);
        }
            break;
        case '|': {
            
            handle6Hex(CharacterStart, policy);
        }
            break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            handleOctal(CharacterStart, policy);
        }
            break;
        //
        // Simple escaped characters
        // \b \f \n \r \t
        //
        case 'b': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = '\b';
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case 'f': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = '\f';
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case 'n': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = '\n';
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case 'r': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = '\r';
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case 't': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = '\t';
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '"': case '\\': case '<': case '>': {
            
            //
            // Special string characters
            // Can only appear in strings so always leave alone
            //
            // What are \< and \> ?
            // https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
            // https://stackoverflow.com/q/6065887
            //
            
            leaveAlone(CharacterStart, {'\\', c});
        }
            break;
        case '!': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_BANG;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '%': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_PERCENT;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '&': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_AMP;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '(': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_OPENPAREN;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case ')': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_CLOSEPAREN;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '*': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_STAR;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '+': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_PLUS;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '/': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_SLASH;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '@': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_AT;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '^': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_CARET;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '_': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_UNDER;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case '`': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_BACKTICK;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        case ' ': {
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {
                c = WLCHARACTER_LINEARSYNTAX_SPACE;
            } else {
                leaveAlone(CharacterStart, {'\\', c});
            }
        }
            break;
        default: {
            
            //
            // Anything else
            //

            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = WLCHARACTER_ERROR_MALFORMED;

            } else {

                auto Loc = TheSourceManager->getSourceLocation();

                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character \\" + WLCharacterToString(c) + ". Did you mean " + WLCharacterToString(c) + " or \\\\" + WLCharacterToString(c) + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

                Issues.push_back(Issue);


                leaveAlone(CharacterStart, {'\\', c});
            }
        }
    }

    TheSourceManager->setWLCharacterEnd();

    return c;
}

void CharacterDecoder::leaveAlone(SourceLocation CharacterStart, std::vector<WLCharacter> chars) {

    assert(!chars.empty());

    auto first = chars[0];

    auto Loc = CharacterStart;
    for (size_t i = 1; i < chars.size(); i++) {
        auto l = chars[i];
        Loc = Loc + 1;
        characterQueue.push_back(std::make_pair(l, Loc));
    }
    
    c = first;
    
    TheSourceManager->setSourceLocation(CharacterStart);
    TheSourceManager->setWLCharacterStart();
}

WLCharacter CharacterDecoder::currentWLCharacter() {
   return c;
}

//
// c is set to the next WL character
//
void CharacterDecoder::handleLongName(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(c == '[');
    
    //
    // Do not write leading \[ or trailing ] to LongName
    //
    std::ostringstream LongName;
    
    c = TheByteDecoder->nextSourceCharacter();
    
    auto wellFormed = false;
    while (true) {
        
        if (isDigitOrAlpha(c)) {
            
            LongName.put(c);
            
            c = TheByteDecoder->nextSourceCharacter();
            
        } else if (c == ']') {
            
            wellFormed = true;
            
            break;
            
        } else {
            
            //
            // Unrecognized
            //
            // Something like \[...] which is not a long name
            //
            
            break;
        }
    }
    
    auto LongNameStr = LongName.str();
    
    if (!wellFormed) {

        if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

            c = WLCHARACTER_ERROR_MALFORMED;

        } else {

            auto Loc = TheSourceManager->getSourceLocation();
        
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\[" + LongNameStr + WLCharacterToString(c) + ". Did you mean [" + LongNameStr + WLCharacterToString(c) + " or \\\\[" + LongNameStr + WLCharacterToString(c) + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
            
            Issues.push_back(Issue);


            std::vector<WLCharacter> chars;
            chars.push_back('\\');
            chars.push_back('[');
            for (size_t i = 0; i < LongNameStr.size(); i++) {
                chars.push_back(LongNameStr[i]);
            }
            chars.push_back(c);

            leaveAlone(CharacterStart, chars);
        }

    } else {
        
        auto it = LongNameToCodePointMap.find(LongNameStr);
        if (it != LongNameToCodePointMap.end()) {
            
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = it->second;

            } else {

                std::vector<WLCharacter> chars;
                chars.push_back('\\');
                chars.push_back('[');
                for (size_t i = 0; i < LongNameStr.size(); i++) {
                    chars.push_back(LongNameStr[i]);
                }
                chars.push_back(']');

                leaveAlone(CharacterStart, chars);
            }
            
        } else {
            
            //
            // Unrecognized
            //

            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = WLCHARACTER_ERROR_UNRECOGNIZED;

            } else {

                auto Loc = TheSourceManager->getSourceLocation();
                
                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\[" + LongNameStr + WLCharacterToString(c) + "]. Did you mean [" + LongNameStr + WLCharacterToString(c) + "] or \\\\[" + LongNameStr + WLCharacterToString(c) + "]?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
                
                Issues.push_back(Issue);


                std::vector<WLCharacter> chars;
                chars.push_back('\\');
                chars.push_back('[');
                for (size_t i = 0; i < LongNameStr.size(); i++) {
                    chars.push_back(LongNameStr[i]);
                }
                chars.push_back(']');

                leaveAlone(CharacterStart, chars);
            }
        }
    }
}

void CharacterDecoder::handle4Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(c == ':');
    
    std::ostringstream Hex;
    
    for (auto i = 0; i < 4; i++) {
        
        c = TheByteDecoder->nextSourceCharacter();
        
        if (isHex(c)) {
            
            Hex.put(c);
            
        } else {
            
            //
            // Unrecognized
            //
            // Something like \:z
            //

            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = WLCHARACTER_ERROR_MALFORMED;

            } else {

                auto HexStr = Hex.str();
            
                auto Loc = TheSourceManager->getSourceLocation();
                
                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\:" + HexStr + WLCharacterToString(c) + ". Did you mean :" + HexStr + WLCharacterToString(c) + " or \\\\:" + HexStr + WLCharacterToString(c) + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

                Issues.push_back(Issue);


                std::vector<WLCharacter> chars;
                chars.push_back('\\');
                chars.push_back(':');
                for (size_t i = 0; i < HexStr.size(); i++) {
                    chars.push_back(HexStr[i]);
                }
                chars.push_back(c);

                leaveAlone(CharacterStart, chars);
            }
            
            return;
        }
    }
    
    auto HexStr = Hex.str();
    
    if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

        c = parseInteger(HexStr, 16);

    } else {

        std::vector<WLCharacter> chars;
        chars.push_back('\\');
        chars.push_back(':');
        for (size_t i = 0; i < HexStr.size(); i++) {
            chars.push_back(HexStr[i]);
        }

        leaveAlone(CharacterStart, chars);
    }
}

void CharacterDecoder::handle2Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(c == '.');
    
    std::ostringstream Hex;
    
    for (auto i = 0; i < 2; i++) {
        
        c = TheByteDecoder->nextSourceCharacter();
        
        if (isHex(c)) {
            
            Hex.put(c);
            
        } else {
            
            //
            // Unrecognized
            //
            // Something like \.z
            //
            
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = WLCHARACTER_ERROR_MALFORMED;

            } else {

                auto HexStr = Hex.str();
            
                auto Loc = TheSourceManager->getSourceLocation();
                
                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\." + HexStr + WLCharacterToString(c) + ". Did you mean ." + HexStr + WLCharacterToString(c) + " or \\\\." + HexStr + WLCharacterToString(c) + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

                Issues.push_back(Issue);


                std::vector<WLCharacter> chars;
                chars.push_back('\\');
                chars.push_back('.');
                for (size_t i = 0; i < HexStr.size(); i++) {
                    chars.push_back(HexStr[i]);
                }
                chars.push_back(c);

                leaveAlone(CharacterStart, chars);
            }

            return;
        }
    }
    
    auto HexStr = Hex.str();
    
    if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

        c = parseInteger(HexStr, 16);

    } else {

        std::vector<WLCharacter> chars;
        chars.push_back('\\');
        chars.push_back('.');
        for (size_t i = 0; i < HexStr.size(); i++) {
            chars.push_back(HexStr[i]);
        }

        leaveAlone(CharacterStart, chars);
    }
}

void CharacterDecoder::handleOctal(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(isOctal(c));
    
    std::ostringstream Octal;
    
    Octal.put(c);
    
    for (auto i = 0; i < 3-1; i++) {
        
        c = TheByteDecoder->nextSourceCharacter();
        
        if (isOctal(c)) {
            
            Octal.put(c);
            
        } else {
            
            //
            // Unrecognized
            //
            // Something like \z
            //
            
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = WLCHARACTER_ERROR_MALFORMED;

            } else {

                auto OctalStr = Octal.str();
            
                auto Loc = TheSourceManager->getSourceLocation();
                
                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\" + OctalStr + WLCharacterToString(c) + ". Did you mean " + OctalStr + WLCharacterToString(c) + " or \\\\" + OctalStr + WLCharacterToString(c) + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

                Issues.push_back(Issue);


                std::vector<WLCharacter> chars;
                chars.push_back('\\');
                for (size_t i = 0; i < OctalStr.size(); i++) {
                    chars.push_back(OctalStr[i]);
                }
                chars.push_back(c);

                leaveAlone(CharacterStart, chars);
            }
            
            return;
        }
    }
    
    auto OctalStr = Octal.str();
    
    if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

        c = parseInteger(OctalStr, 8);

    } else {

        std::vector<WLCharacter> chars;
        chars.push_back('\\');
        for (size_t i = 0; i < OctalStr.size(); i++) {
            chars.push_back(OctalStr[i]);
        }

        leaveAlone(CharacterStart, chars);
    }
}

void CharacterDecoder::handle6Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(c == '|');
    
    std::ostringstream Hex;
    
    for (auto i = 0; i < 6; i++) {
        
        c = TheByteDecoder->nextSourceCharacter();
        
        if (isHex(c)) {
            
            Hex.put(c);
            
        } else {
            
            //
            // Unrecognized
            //
            // Something like \|z
            //
            
            if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

                c = WLCHARACTER_ERROR_MALFORMED;

            } else {

                auto HexStr = Hex.str();
            
                auto Loc = TheSourceManager->getSourceLocation();

                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\|" + HexStr + WLCharacterToString(c) + ". Did you mean |" + HexStr + WLCharacterToString(c) + " or \\\\|" + HexStr + WLCharacterToString(c) + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

                Issues.push_back(Issue);


                std::vector<WLCharacter> chars;
                chars.push_back('\\');
                chars.push_back('|');
                for (size_t i = 0; i < HexStr.size(); i++) {
                    chars.push_back(HexStr[i]);
                }
                chars.push_back(c);

                leaveAlone(CharacterStart, chars);
            }
            
            return;
        }
    }
    
    auto HexStr = Hex.str();
    
    if ((policy & CONVERT_ESCAPES_TO_SINGLE) == CONVERT_ESCAPES_TO_SINGLE) {

        c = parseInteger(HexStr, 16);

    } else {

        std::vector<WLCharacter> chars;
        chars.push_back('\\');
        chars.push_back('|');
        for (size_t i = 0; i < HexStr.size(); i++) {
            chars.push_back(HexStr[i]);
        }

        leaveAlone(CharacterStart, chars);
    }
}

std::vector<SyntaxIssue> CharacterDecoder::getIssues() {
    
    auto Tmp = Issues;
    
    Issues.clear();
    
    return Tmp;
}

CharacterDecoder *TheCharacterDecoder = nullptr;








bool isLinearSyntax(WLCharacter c) {
switch (c) {
    case WLCHARACTER_LINEARSYNTAX_BANG:
    case WLCHARACTER_LINEARSYNTAX_PERCENT:
    case WLCHARACTER_LINEARSYNTAX_AMP:
    case WLCHARACTER_LINEARSYNTAX_OPENPAREN:
    case WLCHARACTER_LINEARSYNTAX_CLOSEPAREN:
    case WLCHARACTER_LINEARSYNTAX_STAR:
    case WLCHARACTER_LINEARSYNTAX_PLUS:
    case WLCHARACTER_LINEARSYNTAX_SLASH:
    case WLCHARACTER_LINEARSYNTAX_AT:
    case WLCHARACTER_LINEARSYNTAX_CARET:
    case WLCHARACTER_LINEARSYNTAX_UNDER:
    case WLCHARACTER_LINEARSYNTAX_BACKTICK:
    case WLCHARACTER_LINEARSYNTAX_SPACE:
        return true;
    default:
        return false;
}
}

std::string WLCharacterToString(WLCharacter c) {
    
    std::ostringstream String;
    
    if (c >= 0x80 || c < 0) {
        
        //
        // non-ASCII
        //
        
        if (CodePointToLongNameMap.find(c) != CodePointToLongNameMap.end()) {
            
            String.put('\\');
            String.put('[');
            String << CodePointToLongNameMap[c];
            String.put(']');
            
        } else if (isLinearSyntax(c)) {
            
            switch (c) {
                case WLCHARACTER_LINEARSYNTAX_BANG:
                    String.put('\\');
                    String.put('!');
                    break;
                case WLCHARACTER_LINEARSYNTAX_PERCENT:
                    String.put('\\');
                    String.put('%');
                    break;
                case WLCHARACTER_LINEARSYNTAX_AMP:
                    String.put('\\');
                    String.put('&');
                    break;
                case WLCHARACTER_LINEARSYNTAX_OPENPAREN:
                    String.put('\\');
                    String.put('(');
                    break;
                case WLCHARACTER_LINEARSYNTAX_CLOSEPAREN:
                    String.put('\\');
                    String.put(')');
                    break;
                case WLCHARACTER_LINEARSYNTAX_STAR:
                    String.put('\\');
                    String.put('*');
                    break;
                case WLCHARACTER_LINEARSYNTAX_PLUS:
                    String.put('\\');
                    String.put('+');
                    break;
                case WLCHARACTER_LINEARSYNTAX_SLASH:
                    String.put('\\');
                    String.put('/');
                    break;
                case WLCHARACTER_LINEARSYNTAX_AT:
                    String.put('\\');
                    String.put('@');
                    break;
                case WLCHARACTER_LINEARSYNTAX_CARET:
                    String.put('\\');
                    String.put('^');
                    break;
                case WLCHARACTER_LINEARSYNTAX_UNDER:
                    String.put('\\');
                    String.put('_');
                    break;
                case WLCHARACTER_LINEARSYNTAX_BACKTICK:
                    String.put('\\');
                    String.put('`');
                    break;
                case WLCHARACTER_LINEARSYNTAX_SPACE:
                    String.put('\\');
                    String.put(' ');
                    break;
            }
            
        } else if (c == WLCHARACTER_EOF) {
            
            //
            // Do not return a string for EOF
            //
            
        } else if (c == WLCHARACTER_ERROR_UNRECOGNIZED) {
            
            String << "WLCHARACTER_ERROR_UNRECOGNIZED";
            
        } else if (c == WLCHARACTER_ERROR_MALFORMED) {
            
            String << "WLCHARACTER_ERROR_MALFORMED";
            
        } else {
            
            assert(c > 0);

            String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec;
        }
        
    } else if (isSpace(c)) {
        
        // ASCII
        //
        // \f, \n, \r, \t, or (space) and it is ok to write directly
        //
        
        switch (c) {
            case '\f':
                String.put('\\');
                String.put('f');
                break;
            case '\n':
                String.put('\\');
                String.put('n');
                break;
            case '\r':
                String.put('\\');
                String.put('r');
                break;
            case '\t':
                String.put('\\');
                String.put('t');
                break;
            case '\v':
                //
                // \v is space, but there is no WL syntax for it
                //
                String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec;
                break;
            default:
                String.put(c);
                break;
        }
        
    } else if (isControl(c)) {
        
        // ASCII
        //
        // something nasty like '\0'
        //
        
        if (c == '\b') {
            String.put('\\');
            String.put('b');
        } else {
            String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec;
        }
        
    } else {
        
        // ASCII
        //
        
        String.put(c);
    }
    
    return String.str();
}




