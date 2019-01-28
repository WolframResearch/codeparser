
#include "CharacterDecoder.h"

#include "ByteDecoder.h"
#include "LongNameMap.h"
#include "Utils.h"

#include <iomanip>
#include <cassert>

CharacterDecoder::CharacterDecoder() : cur(0), curSource(0), characterQueue(), Issues() {}

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
        
        cur = p.first;
        auto curSourceSpan = p.second;
        
        TheSourceManager->setSourceLocation(curSourceSpan.start);
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setSourceLocation(curSourceSpan.end);
        TheSourceManager->setWLCharacterEnd();

        return cur;
    }
    
    curSource = TheByteDecoder->nextSourceCharacter();
    
    if (curSource == EOF) {
        
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        cur = WLCharacter(curSource.to_point());
        
        return cur;
    }
    
    if (curSource != '\\') {
        
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        cur = WLCharacter(curSource.to_point());
        
        return cur;
    }
    
    //
    // Handle \
    //
    // handle escapes like line continuation and special characters
    //
    
    TheSourceManager->setWLCharacterStart();
    auto CharacterStart = TheSourceManager->getWLCharacterStart();
    curSource = TheByteDecoder->nextSourceCharacter();
    
    switch (curSource.to_point()) {
        case '\r': {
            
            //
            // Ignore \r as part of line continuation
            //
            
            nextWLCharacter();
            while (curSource == '\r') {
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
                while (curSource == ' ' || curSource == '\t') {
                    nextWLCharacter();
                }
            }
            
            //
            // return here
            //
            // nextCharacter() is called recursively if there is a line continuation,
            // but we want some functions to only be called once
            //
            return cur;
        }
        case '[': {
            
            cur = handleLongName(CharacterStart, policy);
        }
            break;
        case ':': {
            
            cur = handle4Hex(CharacterStart, policy);
        }
            break;
        case '.': {
            
            cur = handle2Hex(CharacterStart, policy);
        }
            break;
        case '|': {
            
            cur = handle6Hex(CharacterStart, policy);
        }
            break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            cur = handleOctal(CharacterStart, policy);
        }
            break;
        //
        //
        // Special string characters
        // Can only appear in strings so always leave alone
        //
        // Simple escaped characters
        // \b \f \n \r \t
        //
        case 'b':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_B), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 'f':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_F), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 'n':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_N), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 'r':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_R), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 't':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_T), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        //
        // Special string characters
        // \\ \" \< \>
        //
        // What are \< and \> ?
        // https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
        // https://stackoverflow.com/q/6065887
        //
        //
        case '"':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_DOUBLEQUOTE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '\\':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_BACKSLASH), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '<':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_LESS), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '>':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_ESCAPED_GREATER), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        //
        // Linear syntax characters
        // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
        //
        case '!':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_BANG), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '%':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_PERCENT), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '&':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_AMP), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '(':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_OPENPAREN), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case ')':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_CLOSEPAREN), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '*':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_STAR), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '+':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_PLUS), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '/':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_SLASH), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '@':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_AT), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '^':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_CARET), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '_':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_UNDER), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '`':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_BACKTICK), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case ' ':
            cur = leaveAlone({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_SPACE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        default: {
            
            //
            // Anything else
            //

            auto Loc = TheSourceManager->getSourceLocation();

            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character \\" + curSource.string() + ". Did you mean " + curSource.string()+ " or \\\\" + curSource.string() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = leaveAlone(chars);
        }
    }

    TheSourceManager->setWLCharacterEnd();

    return cur;
}

WLCharacter CharacterDecoder::leaveAlone(std::vector<std::pair<WLCharacter, SourceSpan>> chars) {

    assert(!chars.empty());

    auto first = chars[0];

    for (size_t i = 1; i < chars.size(); i++) {
        characterQueue.push_back(chars[i]);
    }

    TheSourceManager->setSourceLocation(first.second.start);
    TheSourceManager->setWLCharacterStart();
    TheSourceManager->setSourceLocation(first.second.end);
    TheSourceManager->setWLCharacterEnd();

    return first.first;
}

WLCharacter CharacterDecoder::currentWLCharacter() {
   return cur;
}

//
// We parse:
// "\[RawDoubleQuote]"
// as:
// <code point for "> <special code point for RawDoubleQuote> <code point for ">
//
int CharacterDecoder::replaceRawCodePoint(int point) {
    switch (point) {
        case CODEPOINT_TAB: return CODEPOINT_RAW_TAB;
        case CODEPOINT_NEWLINE: return CODEPOINT_RAW_NEWLINE;
        case CODEPOINT_RETURN: return CODEPOINT_RAW_RETURN;
        case CODEPOINT_ESCAPE: return CODEPOINT_RAW_ESCAPE;
        case CODEPOINT_SPACE: return CODEPOINT_RAW_SPACE;
        case CODEPOINT_BANG: return CODEPOINT_RAW_BANG;
        case CODEPOINT_DOUBLEQUOTE: return CODEPOINT_RAW_DOUBLEQUOTE;
        case CODEPOINT_HASH: return CODEPOINT_RAW_HASH;
        case CODEPOINT_DOLLAR: return CODEPOINT_RAW_DOLLAR;
        case CODEPOINT_PERCENT: return CODEPOINT_RAW_PERCENT;
        case CODEPOINT_AMP: return CODEPOINT_RAW_AMP;
        case CODEPOINT_SINGLEQUOTE: return CODEPOINT_RAW_SINGLEQUOTE;
        case CODEPOINT_OPENPAREN: return CODEPOINT_RAW_OPENPAREN;
        case CODEPOINT_CLOSEPAREN: return CODEPOINT_RAW_CLOSEPAREN;
        case CODEPOINT_STAR: return CODEPOINT_RAW_STAR;
        case CODEPOINT_PLUS: return CODEPOINT_RAW_PLUS;
        case CODEPOINT_COMMA: return CODEPOINT_RAW_COMMA;
        case CODEPOINT_MINUS: return CODEPOINT_RAW_MINUS;
        case CODEPOINT_DOT: return CODEPOINT_RAW_DOT;
        case CODEPOINT_SLASH: return CODEPOINT_RAW_SLASH;
        case CODEPOINT_COLON: return CODEPOINT_RAW_COLON;
        case CODEPOINT_SEMICOLON: return CODEPOINT_RAW_SEMICOLON;
        case CODEPOINT_LESS: return CODEPOINT_RAW_LESS;
        case CODEPOINT_EQUAL: return CODEPOINT_RAW_EQUAL;
        case CODEPOINT_GREATER: return CODEPOINT_RAW_GREATER;
        case CODEPOINT_QUESTION: return CODEPOINT_RAW_QUESTION;
        case CODEPOINT_AT: return CODEPOINT_RAW_AT;
        case CODEPOINT_OPENSQUARE: return CODEPOINT_RAW_OPENSQUARE;
        case CODEPOINT_BACKSLASH: return CODEPOINT_RAW_BACKSLASH;
        case CODEPOINT_CLOSESQUARE: return CODEPOINT_RAW_CLOSESQUARE;
        case CODEPOINT_CARET: return CODEPOINT_RAW_CARET;
        case CODEPOINT_UNDER: return CODEPOINT_RAW_UNDER;
        case CODEPOINT_BACKTICK: return CODEPOINT_RAW_BACKTICK;
        case CODEPOINT_OPENCURLY: return CODEPOINT_RAW_OPENCURLY;
        case CODEPOINT_BAR: return CODEPOINT_RAW_BAR;
        case CODEPOINT_CLOSECURLY: return CODEPOINT_RAW_CLOSECURLY;
        case CODEPOINT_TILDE: return CODEPOINT_RAW_TILDE;
        default: return point;
    }
}

//
// c is set to the next WL character
//
WLCharacter CharacterDecoder::handleLongName(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == '[');
    
    //
    // Do not write leading \[ or trailing ] to LongName
    //
    std::ostringstream LongName;
    
    curSource = TheByteDecoder->nextSourceCharacter();
    
    auto wellFormed = false;
    while (true) {
        
        if (curSource.isDigitOrAlpha()) {
            
            LongName.put(curSource.to_char());
            
            curSource = TheByteDecoder->nextSourceCharacter();
            
        } else if (curSource == ']') {
            
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

        auto Loc = TheSourceManager->getSourceLocation();
    
        auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\[" + LongNameStr + curSource.string() + ". Did you mean [" + LongNameStr + curSource.string() + " or \\\\[" + LongNameStr + curSource.string() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
        
        Issues.push_back(Issue);


        std::vector<std::pair<WLCharacter, SourceSpan>> chars;
        chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
        chars.push_back(std::make_pair<>(WLCharacter('['), SourceSpan{CharacterStart+1, CharacterStart+1}));
        for (size_t i = 0; i < LongNameStr.size(); i++) {
            chars.push_back(std::make_pair<>(WLCharacter(LongNameStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
        }
        chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
        
        cur = leaveAlone(chars);

    } else {

        auto it = LongNameToCodePointMap.find(LongNameStr);
        if (it != LongNameToCodePointMap.end()) {

            auto Loc = TheSourceManager->getSourceLocation();
            
            auto point = it->second;

            point = replaceRawCodePoint(point);

            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter(point), SourceSpan{CharacterStart, Loc}));
            
            cur = leaveAlone(chars);
            
        } else {
            
            //
            // Unrecognized
            //

            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\[" + LongNameStr + curSource.string() + "]. Did you mean [" + LongNameStr + curSource.string() + "] or \\\\[" + LongNameStr + curSource.string() + "]?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
            
            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter('['), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < LongNameStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(LongNameStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(']'), SourceSpan{Loc, Loc}));
            
            cur = leaveAlone(chars);
        }
    }
    
    return cur;
}

WLCharacter CharacterDecoder::handle4Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == ':');
    
    std::ostringstream Hex;
    
    for (auto i = 0; i < 4; i++) {
        
        curSource = TheByteDecoder->nextSourceCharacter();
        
        if (curSource.isHex()) {
            
            Hex.put(curSource.to_char());
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \:z
            //

            auto HexStr = Hex.str();
        
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\:" + HexStr + curSource.string() + ". Did you mean :" + HexStr + curSource.string() + " or \\\\:" + HexStr + curSource.string() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter(':'), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < HexStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(HexStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = leaveAlone(chars);
            
            return cur;
        }
    }

    auto HexStr = Hex.str();
    
    auto point = parseInteger(HexStr, 16);
    
    point = replaceRawCodePoint(point);

    auto Loc = TheSourceManager->getSourceLocation();
    
    std::vector<std::pair<WLCharacter, SourceSpan>> chars;
    chars.push_back(std::make_pair<>(WLCharacter(point), SourceSpan{CharacterStart, Loc}));
    
    cur = leaveAlone(chars);
    
    return cur;
}

WLCharacter CharacterDecoder::handle2Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == '.');
    
    std::ostringstream Hex;
    
    for (auto i = 0; i < 2; i++) {
        
        curSource = TheByteDecoder->nextSourceCharacter();
        
        if (curSource.isHex()) {
            
            Hex.put(curSource.to_char());
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \.z
            //

            auto HexStr = Hex.str();
        
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\." + HexStr + curSource.string() + ". Did you mean ." + HexStr + curSource.string() + " or \\\\." + HexStr +curSource.string() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter('.'), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < HexStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(HexStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = leaveAlone(chars);

            return cur;
        }
    }
    
    auto HexStr = Hex.str();

    auto point = parseInteger(HexStr, 16);

    point = replaceRawCodePoint(point);

    auto Loc = TheSourceManager->getSourceLocation();
    
    std::vector<std::pair<WLCharacter, SourceSpan>> chars;
    chars.push_back(std::make_pair<>(WLCharacter(point), SourceSpan{CharacterStart, Loc}));
    
    cur = leaveAlone(chars);
    
    return cur;
}

WLCharacter CharacterDecoder::handleOctal(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource.isOctal());
    
    std::ostringstream Octal;
    
    Octal.put(curSource.to_char());
    
    for (auto i = 0; i < 3-1; i++) {
        
        curSource = TheByteDecoder->nextSourceCharacter();
        
        if (curSource.isOctal()) {
            
            Octal.put(curSource.to_char());
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \z
            //

            auto OctalStr = Octal.str();
        
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\" + OctalStr + curSource.string() + ". Did you mean " + OctalStr + curSource.string() + " or \\\\" + OctalStr + curSource.string() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            for (size_t i = 0; i < OctalStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(OctalStr[i]), SourceSpan{CharacterStart+1+i, CharacterStart+1+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = leaveAlone(chars);
            
            return cur;
        }
    }
    
    auto OctalStr = Octal.str();

    auto point = parseInteger(OctalStr, 8);

    point = replaceRawCodePoint(point);

    auto Loc = TheSourceManager->getSourceLocation();
    
    std::vector<std::pair<WLCharacter, SourceSpan>> chars;
    chars.push_back(std::make_pair<>(WLCharacter(point), SourceSpan{CharacterStart, Loc}));
    
    cur = leaveAlone(chars);
    
    return cur;
}

WLCharacter CharacterDecoder::handle6Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == '|');
    
    std::ostringstream Hex;
    
    for (auto i = 0; i < 6; i++) {
        
        curSource = TheByteDecoder->nextSourceCharacter();
        
        if (curSource.isHex()) {
            
            Hex.put(curSource.to_char());
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \|z
            //

            auto HexStr = Hex.str();
        
            auto Loc = TheSourceManager->getSourceLocation();

            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\|" + HexStr + curSource.string() + ". Did you mean |" + HexStr + curSource.string() + " or \\\\|" + HexStr + curSource.string() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter('|'), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < HexStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(HexStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = leaveAlone(chars);
            
            return cur;
        }
    }
    
    auto HexStr = Hex.str();

    auto point = parseInteger(HexStr, 16);

    point = replaceRawCodePoint(point);

    auto Loc = TheSourceManager->getSourceLocation();
    
    std::vector<std::pair<WLCharacter, SourceSpan>> chars;
    chars.push_back(std::make_pair<>(WLCharacter(point), SourceSpan{CharacterStart, Loc}));
    
    cur = leaveAlone(chars);
    
    return cur;
}

std::vector<SyntaxIssue> CharacterDecoder::getIssues() {
    
    auto Tmp = Issues;
    
    Issues.clear();
    
    return Tmp;
}

CharacterDecoder *TheCharacterDecoder = nullptr;




std::string WLCharacter::string() const {
    
    std::ostringstream String;
    
    auto i = value_;
    if (i >= 0x80) {
        
        //
        // non-ASCII
        //
        
        if (CodePointToLongNameMap.find(i) != CodePointToLongNameMap.end()) {
            
            String.put('\\');
            String.put('[');
            String << CodePointToLongNameMap[i];
            String.put(']');
            
        } else {
            
            switch (i) {
                case CODEPOINT_LINEARSYNTAX_BANG:
                    String.put('\\');
                    String.put('!');
                    break;
                case CODEPOINT_LINEARSYNTAX_PERCENT:
                    String.put('\\');
                    String.put('%');
                    break;
                case CODEPOINT_LINEARSYNTAX_AMP:
                    String.put('\\');
                    String.put('&');
                    break;
                case CODEPOINT_LINEARSYNTAX_OPENPAREN:
                    String.put('\\');
                    String.put('(');
                    break;
                case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
                    String.put('\\');
                    String.put(')');
                    break;
                case CODEPOINT_LINEARSYNTAX_STAR:
                    String.put('\\');
                    String.put('*');
                    break;
                case CODEPOINT_LINEARSYNTAX_PLUS:
                    String.put('\\');
                    String.put('+');
                    break;
                case CODEPOINT_LINEARSYNTAX_SLASH:
                    String.put('\\');
                    String.put('/');
                    break;
                case CODEPOINT_LINEARSYNTAX_AT:
                    String.put('\\');
                    String.put('@');
                    break;
                case CODEPOINT_LINEARSYNTAX_CARET:
                    String.put('\\');
                    String.put('^');
                    break;
                case CODEPOINT_LINEARSYNTAX_UNDER:
                    String.put('\\');
                    String.put('_');
                    break;
                case CODEPOINT_LINEARSYNTAX_BACKTICK:
                    String.put('\\');
                    String.put('`');
                    break;
                //
                // CODEPOINT_LINEARSYNTAX_SPACE is deliberately not here
                // There is not an actual dedicated WL code point for CODEPOINT_LINEARSYNTAX_SPACE
                // and this is handled further down.
                // 
                default:
                    String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << i << std::dec;
                    break;
            }
        }
        
    } else if (i < 0) {
        
        switch (i) {
            case CODEPOINT_EOF:
                //
                // Do not return a string for EOF
                //
                break;
            case CODEPOINT_LINEARSYNTAX_SPACE:
                String.put('\\');
                String.put(' ');
                break;
            case CODEPOINT_ERROR_INTERNAL:
                String << "ERROR";
                break;
            case CODEPOINT_ESCAPED_B:
                String.put('\\');
                String.put('b');
                break;
            case CODEPOINT_ESCAPED_F:
                String.put('\\');
                String.put('f');
                break;
            case CODEPOINT_ESCAPED_N:
                String.put('\\');
                String.put('n');
                break;
            case CODEPOINT_ESCAPED_R:
                String.put('\\');
                String.put('r');
                break;
            case CODEPOINT_ESCAPED_T:
                String.put('\\');
                String.put('t');
                break;
            case CODEPOINT_ESCAPED_DOUBLEQUOTE:
                String.put('\\');
                String.put('"');
                break;
            case CODEPOINT_ESCAPED_BACKSLASH:
                String.put('\\');
                String.put('\\');
                break;
            case CODEPOINT_ESCAPED_LESS:
                String.put('\\');
                String.put('<');
                break;
            case CODEPOINT_ESCAPED_GREATER:
                String.put('\\');
                String.put('>');
                break;
            case CODEPOINT_RAW_TAB:
                String << "\\[RawTab]";
                break;  
            case CODEPOINT_RAW_NEWLINE:
                String << "\\[NewLine]";
                break;  
            case CODEPOINT_RAW_RETURN:
                String << "\\[RawReturn]";
                break;  
            case CODEPOINT_RAW_ESCAPE:
                String << "\\[RawEscape]";
                break;  
            case CODEPOINT_RAW_SPACE:
                String << "\\[RawSpace]";
                break;  
            case CODEPOINT_RAW_BANG:
                String << "\\[RawExclamation]";
                break;  
            case CODEPOINT_RAW_DOUBLEQUOTE:
                String << "\\[RawDoubleQuote]";
                break;  
            case CODEPOINT_RAW_HASH:
                String << "\\[RawNumberSign]";
                break;  
            case CODEPOINT_RAW_DOLLAR:
                String << "\\[RawDollar]";
                break;  
            case CODEPOINT_RAW_PERCENT:
                String << "\\[RawTab]";
                break;  
            case CODEPOINT_RAW_AMP:
                String << "\\[RawAmpersand]";
                break;  
            case CODEPOINT_RAW_SINGLEQUOTE:
                String << "\\[RawQuote]";
                break;  
            case CODEPOINT_RAW_OPENPAREN:
                String << "\\[RawLeftParenthesis]";
                break;  
            case CODEPOINT_RAW_CLOSEPAREN:
                String << "\\[RawRightParenthesis]";
                break;  
            case CODEPOINT_RAW_STAR:
                String << "\\[RawStar]";
                break;  
            case CODEPOINT_RAW_PLUS:
                String << "\\[RawPlus]";
                break;  
            case CODEPOINT_RAW_COMMA:
                String << "\\[RawComma]";
                break;  
            case CODEPOINT_RAW_MINUS:
                String << "\\[RawDash]";
                break;  
            case CODEPOINT_RAW_DOT:
                String << "\\[RawDot]";
                break;  
            case CODEPOINT_RAW_SLASH:
                String << "\\[RawSlash]";
                break;  
            case CODEPOINT_RAW_COLON:
                String << "\\[RawColon]";
                break;  
            case CODEPOINT_RAW_SEMICOLON:
                String << "\\[RawSemicolon]";
                break;  
            case CODEPOINT_RAW_LESS:
                String << "\\[RawLess]";
                break;  
            case CODEPOINT_RAW_EQUAL:
                String << "\\[RawEqual]";
                break;  
            case CODEPOINT_RAW_GREATER:
                String << "\\[RawGreater]";
                break;  
            case CODEPOINT_RAW_QUESTION:
                String << "\\[RawQuestion]";
                break;  
            case CODEPOINT_RAW_AT:
                String << "\\[RawAt]";
                break;  
            case CODEPOINT_RAW_OPENSQUARE:
                String << "\\[RawLeftBracket]";
                break;  
            case CODEPOINT_RAW_BACKSLASH:
                String << "\\[RawBackslash]";
                break;  
            case CODEPOINT_RAW_CLOSESQUARE:
                String << "\\[RawRightBracket]";
                break;  
            case CODEPOINT_RAW_CARET:
                String << "\\[RawWedge]";
                break;  
            case CODEPOINT_RAW_UNDER:
                String << "\\[RawUnderscore]";
                break;  
            case CODEPOINT_RAW_BACKTICK:
                String << "\\[RawBackquote]";
                break;  
            case CODEPOINT_RAW_OPENCURLY:
                String << "\\[RawLeftBrace]";
                break;  
            case CODEPOINT_RAW_BAR:
                String << "\\[RawVerticalBar]";
                break;  
            case CODEPOINT_RAW_CLOSECURLY:
                String << "\\[RawRightBrace]";
                break;  
            case CODEPOINT_RAW_TILDE:
                String << "\\[RawTilde]";
                break;  
            default:
                assert(false);
                break;
        }
        
    } else if (isSpace()) {
        
        // ASCII space
        //
        // \f, \n, \r, \t, \v, or (space) and it is ok to write directly
        //
        
        switch (i) {
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
                String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << i << std::dec;
                break;
            default:
                String.put(i);
                break;
        }
        
    } else if (isControl()) {
        
        // ASCII control
        //
        // something nasty like '\0'
        //
        
        if (i == '\b') {
            String.put('\\');
            String.put('b');
        } else {
            String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << i << std::dec;
        }
        
    } else {
        
        // ASCII plain
        //
        
        String.put(i);
    }
    
    return String.str();
}

bool WLCharacter::isDigitOrAlpha() const {
    return std::isalnum(value_);
}

bool WLCharacter::isAlphaOrDollar() const {
    return std::isalpha(value_) || value_ == '$';
}

bool WLCharacter::isDigitOrAlphaOrDollar() const {
    return std::isalnum(value_) || value_ == '$';
}

bool WLCharacter::isHex() const {
    return std::isxdigit(value_);
}

bool WLCharacter::isOctal() const {
    return '0' <= value_ && value_ <= '7';
}

bool WLCharacter::isDigit() const {
    return std::isdigit(value_);
}

bool WLCharacter::isAlpha() const {
    return std::isalpha(value_);
}

bool WLCharacter::isSpace() const {
    return std::isspace(value_);
}

bool WLCharacter::isControl() const {
    return iscntrl(value_);
}

// Convert the character c into the digit that it represents
//
int WLCharacter::toBaseDigit() const {
    switch (value_) {
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


