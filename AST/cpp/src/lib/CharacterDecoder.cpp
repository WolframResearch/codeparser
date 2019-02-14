
#include "CharacterDecoder.h"

#include "LongNameMap.h"
#include "Utils.h"

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
    
    if (curSource == SourceCharacter(EOF)) {
        
        TheSourceManager->setWLCharacterStart();
        TheSourceManager->setWLCharacterEnd();
        
        cur = WLCharacter(curSource.to_point());
        
        return cur;
    }
    
    if (curSource != SourceCharacter('\\')) {
        
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
            while (curSource == SourceCharacter('\r')) {
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
                while (curSource == SourceCharacter(' ') || curSource == SourceCharacter('\t')) {
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
        case '[':
            cur = handleLongName(CharacterStart, policy);
            break;
        case ':':
            cur = handle4Hex(CharacterStart, policy);
            break;
        case '.':
            cur = handle2Hex(CharacterStart, policy);
            break;
        case '|':
            cur = handle6Hex(CharacterStart, policy);
            break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
            cur = handleOctal(CharacterStart, policy);
            break;
        //
        // Simple escaped characters
        // \b \f \n \r \t
        //
        case 'b':
            cur = enqueue({std::make_pair<>(WLCharacter('\b', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 'f':
            cur = enqueue({std::make_pair<>(WLCharacter('\f', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 'n':
            cur = enqueue({std::make_pair<>(WLCharacter('\n', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 'r':
            cur = enqueue({std::make_pair<>(WLCharacter('\r', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case 't':
            cur = enqueue({std::make_pair<>(WLCharacter('\t', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        //
        // \\ \" \< \>
        //
        // String meta characters
        // What are \< and \> ?
        // https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
        // https://stackoverflow.com/q/6065887
        //
        // String meta characters are not considered to be escaped
        //
        case '"':
            cur = enqueue({std::make_pair<>(WLCharacter('"', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '\\':
            cur = enqueue({std::make_pair<>(WLCharacter('\\', ESCAPE_SINGLE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '<':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_STRINGMETA_OPEN), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '>':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_STRINGMETA_CLOSE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        //
        // Linear syntax characters
        // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
        //
        // Linear syntax characters are not considered to be escaped
        //
        case '!':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_BANG), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '%':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_PERCENT), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '&':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_AMP), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '(':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_OPENPAREN), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case ')':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_CLOSEPAREN), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '*':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_STAR), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '+':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_PLUS), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '/':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_SLASH), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '@':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_AT), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '^':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_CARET), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '_':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_UNDER), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case '`':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_BACKTICK), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case ' ':
            cur = enqueue({std::make_pair<>(WLCharacter(CODEPOINT_LINEARSYNTAX_SPACE), SourceSpan{CharacterStart, CharacterStart+1})});
            break;
        case EOF: {
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Incomplete character \\"), SEVERITY_FATAL, SourceSpan{CharacterStart, Loc});
            
            Issues.push_back(Issue);
            
            
            cur = enqueue({
                std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart})
            });
            break;
        }
        default: {
            
            //
            // Anything else
            //

            auto Loc = TheSourceManager->getSourceLocation();

            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Unrecognized character \\") + curSource.to_char() + ". Did you mean " + curSource.to_char()+ " or \\\\" + curSource.to_char() + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);
            
            //
            // Keep these treated as 2 characters. This is how bad escapes are handled in WL strings.
            //
            cur = enqueue({
                std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}),
                std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc})});
            break;
        }
    }

    TheSourceManager->setWLCharacterEnd();

    return cur;
}

WLCharacter CharacterDecoder::enqueue(std::vector<std::pair<WLCharacter, SourceSpan>> chars) {

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
// c is set to the next WL character
//
WLCharacter CharacterDecoder::handleLongName(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == SourceCharacter('['));
    
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
            
        } else if (curSource == SourceCharacter(']')) {
            
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
    
        auto curSourceStr = (curSource == SourceCharacter(EOF)) ? std::string("") : std::string(1, curSource.to_char());
        
        auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Unrecognized character: \\[") + LongNameStr + curSourceStr + ". Did you mean [" + LongNameStr + curSourceStr + " or \\\\[" + LongNameStr + curSourceStr + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
        
        Issues.push_back(Issue);


        std::vector<std::pair<WLCharacter, SourceSpan>> chars;
        chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
        chars.push_back(std::make_pair<>(WLCharacter('['), SourceSpan{CharacterStart+1, CharacterStart+1}));
        for (size_t i = 0; i < LongNameStr.size(); i++) {
            chars.push_back(std::make_pair<>(WLCharacter(LongNameStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
        }
        chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
        
        cur = enqueue(chars);

    } else {

        auto it = LongNameToCodePointMap.find(LongNameStr);
        if (it != LongNameToCodePointMap.end()) {

            auto Loc = TheSourceManager->getSourceLocation();
            
            auto point = it->second;
            
            cur = enqueue({std::make_pair<>(WLCharacter(point, ESCAPE_LONGNAME), SourceSpan{CharacterStart, Loc})});
            
        } else {
            
            //
            // Unrecognized
            //

            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Unrecognized character: \\[") + LongNameStr + "]. Did you mean [" + LongNameStr + "] or \\\\[" + LongNameStr + "]?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
            
            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter('['), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < LongNameStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(LongNameStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(']'), SourceSpan{Loc, Loc}));
            
            cur = enqueue(chars);
        }
    }
    
    return cur;
}

WLCharacter CharacterDecoder::handle4Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == SourceCharacter(':'));
    
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
            
            auto curSourceStr = (curSource == SourceCharacter(EOF)) ? std::string("") : std::string(1, curSource.to_char());
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Unrecognized character: \\:") + HexStr + curSourceStr + ". Did you mean :" + HexStr + curSourceStr + " or \\\\:" + HexStr + curSourceStr + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter(':'), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < HexStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(HexStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = enqueue(chars);
            
            return cur;
        }
    }

    auto HexStr = Hex.str();
    
    auto point = parseInteger(HexStr, 16);

    auto Loc = TheSourceManager->getSourceLocation();
    
    cur = enqueue({std::make_pair<>(WLCharacter(point, ESCAPE_4HEX), SourceSpan{CharacterStart, Loc})});
    
    return cur;
}

WLCharacter CharacterDecoder::handle2Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == SourceCharacter('.'));
    
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
            
            auto curSourceStr = (curSource == SourceCharacter(EOF)) ? std::string("") : std::string(1, curSource.to_char());
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Unrecognized character: \\." + HexStr + curSourceStr + ". Did you mean ." + HexStr + curSourceStr + " or \\\\." + HexStr +curSourceStr + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter('.'), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < HexStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(HexStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = enqueue(chars);

            return cur;
        }
    }
    
    auto HexStr = Hex.str();

    auto point = parseInteger(HexStr, 16);

    auto Loc = TheSourceManager->getSourceLocation();
    
    cur = enqueue({std::make_pair<>(WLCharacter(point, ESCAPE_2HEX), SourceSpan{CharacterStart, Loc})});
    
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
            
            auto curSourceStr = (curSource == SourceCharacter(EOF)) ? std::string("") : std::string(1, curSource.to_char());
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Unrecognized character: \\") + OctalStr + curSourceStr + ". Did you mean " + OctalStr + curSourceStr + " or \\\\" + OctalStr + curSourceStr + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            for (size_t i = 0; i < OctalStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(OctalStr[i]), SourceSpan{CharacterStart+1+i, CharacterStart+1+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = enqueue(chars);
            
            return cur;
        }
    }
    
    auto OctalStr = Octal.str();

    auto point = parseInteger(OctalStr, 8);

    auto Loc = TheSourceManager->getSourceLocation();
    
    cur = enqueue({std::make_pair<>(WLCharacter(point, ESCAPE_OCTAL), SourceSpan{CharacterStart, Loc})});
    
    return cur;
}

WLCharacter CharacterDecoder::handle6Hex(SourceLocation CharacterStart, NextCharacterPolicy policy) {
    
    assert(curSource == SourceCharacter('|'));
    
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

            auto curSourceStr = (curSource == SourceCharacter(EOF)) ? std::string("") : std::string(1, curSource.to_char());
            
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, std::string("Unrecognized character: \\|") + HexStr + curSourceStr + ". Did you mean |" + HexStr + curSourceStr + " or \\\\|" + HexStr + curSourceStr + "?", SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);


            std::vector<std::pair<WLCharacter, SourceSpan>> chars;
            chars.push_back(std::make_pair<>(WLCharacter('\\'), SourceSpan{CharacterStart, CharacterStart}));
            chars.push_back(std::make_pair<>(WLCharacter('|'), SourceSpan{CharacterStart+1, CharacterStart+1}));
            for (size_t i = 0; i < HexStr.size(); i++) {
                chars.push_back(std::make_pair<>(WLCharacter(HexStr[i]), SourceSpan{CharacterStart+2+i, CharacterStart+2+i}));
            }
            chars.push_back(std::make_pair<>(WLCharacter(curSource.to_point()), SourceSpan{Loc, Loc}));
            
            cur = enqueue(chars);
            
            return cur;
        }
    }
    
    auto HexStr = Hex.str();

    auto point = parseInteger(HexStr, 16);

    auto Loc = TheSourceManager->getSourceLocation();
    
    cur = enqueue({std::make_pair<>(WLCharacter(point, ESCAPE_6HEX), SourceSpan{CharacterStart, Loc})});
    
    return cur;
}

std::vector<SyntaxIssue> CharacterDecoder::getIssues() {
    
    auto Tmp = Issues;
    
    Issues.clear();
    
    return Tmp;
}

CharacterDecoder *TheCharacterDecoder = nullptr;



//
// If linear syntax or string meta characters, must do \x
// Prefer to use the short \b \f \n \r \t \\ \" syntax first.
// Then prefer to use long names \[RawEscape] \[RawSpace] \[RawBang]
// Then prefer to use \|xxxxxx for characters >= 0x10000
// Then prefer to use use \:xxxx for characters >= 0x100
// Then prefer to use short \.xx syntax for ASCII characters
//
std::vector<SourceCharacter> WLCharacter::source() const {
    
    auto i = value_;
    
    if (escape_ == ESCAPE_NONE) {
        
        switch (i) {
            case CODEPOINT_STRINGMETA_OPEN:
                return {SourceCharacter('\\'), SourceCharacter('<')};
            case CODEPOINT_STRINGMETA_CLOSE:
                return {SourceCharacter('\\'), SourceCharacter('>')};
            case CODEPOINT_LINEARSYNTAX_BANG:
                return {SourceCharacter('\\'), SourceCharacter('!')};
            case CODEPOINT_LINEARSYNTAX_PERCENT:
                return {SourceCharacter('\\'), SourceCharacter('%')};
            case CODEPOINT_LINEARSYNTAX_AMP:
                return {SourceCharacter('\\'), SourceCharacter('&')};
            case CODEPOINT_LINEARSYNTAX_OPENPAREN:
                return {SourceCharacter('\\'), SourceCharacter('(')};
            case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
                return {SourceCharacter('\\'), SourceCharacter(')')};
            case CODEPOINT_LINEARSYNTAX_STAR:
                return {SourceCharacter('\\'), SourceCharacter('*')};
            case CODEPOINT_LINEARSYNTAX_PLUS:
                return {SourceCharacter('\\'), SourceCharacter('+')};
            case CODEPOINT_LINEARSYNTAX_SLASH:
                return {SourceCharacter('\\'), SourceCharacter('/')};
            case CODEPOINT_LINEARSYNTAX_AT:
                return {SourceCharacter('\\'), SourceCharacter('@')};
            case CODEPOINT_LINEARSYNTAX_CARET:
                return {SourceCharacter('\\'), SourceCharacter('^')};
            case CODEPOINT_LINEARSYNTAX_UNDER:
                return {SourceCharacter('\\'), SourceCharacter('_')};
            case CODEPOINT_LINEARSYNTAX_BACKTICK:
                return {SourceCharacter('\\'), SourceCharacter('`')};
            case CODEPOINT_LINEARSYNTAX_SPACE:
                return {SourceCharacter('\\'), SourceCharacter(' ')};
            default:
                return {SourceCharacter(i)};
        }
    }
    
    switch (i) {
        case '\b':
            return {SourceCharacter('\\'), SourceCharacter('b')};
        case '\f':
            return {SourceCharacter('\\'), SourceCharacter('f')};
        case '\n':
            return {SourceCharacter('\\'), SourceCharacter('n')};
        case '\r':
            return {SourceCharacter('\\'), SourceCharacter('r')};
        case '\t':
            return {SourceCharacter('\\'), SourceCharacter('t')};
        case '\\':
            return {SourceCharacter('\\'), SourceCharacter('\\')};
        case '"':
            return {SourceCharacter('\\'), SourceCharacter('"')};
        default:
            
            if (CodePointToLongNameMap.find(i) != CodePointToLongNameMap.end()) {
                
                //
                // Has a long name
                //
                
                auto LongName = CodePointToLongNameMap[i];
                
                std::vector<SourceCharacter> source;
                source.push_back(SourceCharacter('\\'));
                source.push_back(SourceCharacter('['));
                for (size_t idx = 0; idx < LongName.size(); idx++) {
                    source.push_back(SourceCharacter(LongName[idx]));
                }
                source.push_back(SourceCharacter(']'));
                
                return source;
                
            } else if (i >= 0x10000) {
                
                //
                // Some unhandled non-PrintableASCII character, just use \| syntax
                //
                
                auto ii = i;
                auto x0 = ii % 16;
                ii = ii / 16;
                auto x1 = ii % 16;
                ii = ii / 16;
                auto x2 = ii % 16;
                ii = ii / 16;
                auto x3 = ii % 16;
                ii = ii / 16;
                auto x4 = ii % 16;
                ii = ii / 16;
                auto x5 = ii % 16;
                
                return {SourceCharacter('\\'), SourceCharacter('|'), SourceCharacter(WLCharacter::fromDigit(x5)), SourceCharacter(WLCharacter::fromDigit(x4)), SourceCharacter(WLCharacter::fromDigit(x3)), SourceCharacter(WLCharacter::fromDigit(x2)), SourceCharacter(WLCharacter::fromDigit(x1)), SourceCharacter(WLCharacter::fromDigit(x0))};
                
            } else if (i >= 0x100) {
                
                //
                // Some unhandled non-PrintableASCII character, just use \: syntax
                //
                
                auto ii = i;
                auto x0 = ii % 16;
                ii = ii / 16;
                auto x1 = ii % 16;
                ii = ii / 16;
                auto x2 = ii % 16;
                ii = ii / 16;
                auto x3 = ii % 16;
                
                return {SourceCharacter('\\'), SourceCharacter(':'), SourceCharacter(WLCharacter::fromDigit(x3)), SourceCharacter(WLCharacter::fromDigit(x2)), SourceCharacter(WLCharacter::fromDigit(x1)), SourceCharacter(WLCharacter::fromDigit(x0))};
                
            } else {
                
                //
                // Some unhandled non-PrintableASCII character, just use \: syntax
                //
                
                auto ii = i;
                auto x0 = ii % 16;
                ii = ii / 16;
                auto x1 = ii % 16;
                
                return {SourceCharacter('\\'), SourceCharacter('.'), SourceCharacter(WLCharacter::fromDigit(x1)), SourceCharacter(WLCharacter::fromDigit(x0))};
            }
    }
}

std::string WLCharacter::string() const {
    
    std::ostringstream String;
    
    String << this;
    
    return String.str();
}

std::ostream& operator<<(std::ostream& s, const WLCharacter c) {

    auto src = c.source();
    for (auto S : src) {
        auto bytes = S.bytes();
        for (auto b : bytes) {
            s.put(b);
        }
    }

    return s;
}


bool WLCharacter::isDigitOrAlpha() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::isalnum(value_);
}

bool WLCharacter::isAlphaOrDollar() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::isalpha(value_) || value_ == '$';
}

bool WLCharacter::isDigitOrAlphaOrDollar() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::isalnum(value_) || value_ == '$';
}

bool WLCharacter::isHex() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::isxdigit(value_);
}

bool WLCharacter::isOctal() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return '0' <= value_ && value_ <= '7';
}

bool WLCharacter::isDigit() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::isdigit(value_);
}

bool WLCharacter::isAlpha() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::isalpha(value_);
}

bool WLCharacter::isPunctuation() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    if (isEscaped()) {
        return false;
    }
    return std::ispunct(value_);
}

bool WLCharacter::isLinearSyntax() const {
    if (isEscaped()) {
        return false;
    }
    switch (value_) {
        case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
        case CODEPOINT_LINEARSYNTAX_BANG:
        case CODEPOINT_LINEARSYNTAX_AT:
        case CODEPOINT_LINEARSYNTAX_PERCENT:
        case CODEPOINT_LINEARSYNTAX_CARET:
        case CODEPOINT_LINEARSYNTAX_AMP:
        case CODEPOINT_LINEARSYNTAX_STAR:
        case CODEPOINT_LINEARSYNTAX_OPENPAREN:
        case CODEPOINT_LINEARSYNTAX_UNDER:
        case CODEPOINT_LINEARSYNTAX_PLUS:
        case CODEPOINT_LINEARSYNTAX_SLASH:
        case CODEPOINT_LINEARSYNTAX_BACKTICK:
        case CODEPOINT_LINEARSYNTAX_SPACE:
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isLetterlikeCharacter() const {

    if (0 <= value_ && value_ <= 0x7f) {

        //
        // ASCII
        //

        if (isAlphaOrDollar()) {
            return true;
        }
        
        return false;
    }

    //
    // Non-ASCII
    //

    if (isOperatorCharacter()) {
        return false;
    }
    
    if (value_ == EOF) {
        return false;
    }
    
    if (isLinearSyntax()) {
        return false;
    }

    if (isSpaceCharacter()) {
        return false;
    }

    if (isNewlineCharacter()) {
        return false;
    }

    if (isCommaCharacter()) {
        return false;
    }

    if (isUninterpretableCharacter()) {
        return false;
    }

    return true;
}

bool WLCharacter::isStrangeLetterlikeCharacter() const {

    if (!isLetterlikeCharacter()) {
        return false;
    }

    if (escape_ == ESCAPE_NONE || escape_ == ESCAPE_LONGNAME) {
        return false;
    }

    return true;
}

// Convert value_ to the digit that it represents
//
int WLCharacter::toDigit() const {
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

//
// Given a digit, return the character
//
int WLCharacter::fromDigit(int d) {
    switch (d) {
        case 0: return '0';
        case 1: return '1';
        case 2: return '2';
        case 3: return '3';
        case 4: return '4';
        case 5: return '5';
        case 6: return '6';
        case 7: return '7';
        case 8: return '8';
        case 9: return '9';
        case 10: return 'a';
        case 11: return 'b';
        case 12: return 'c';
        case 13: return 'd';
        case 14: return 'e';
        case 15: return 'f';
        case 16: return 'g';
        case 17: return 'h';
        case 18: return 'i';
        case 19: return 'j';
        case 20: return 'k';
        case 21: return 'l';
        case 22: return 'm';
        case 23: return 'n';
        case 24: return 'o';
        case 25: return 'p';
        case 26: return 'q';
        case 27: return 'r';
        case 28: return 's';
        case 29: return 't';
        case 30: return 'u';
        case 31: return 'v';
        case 32: return 'w';
        case 33: return 'x';
        case 34: return 'y';
        case 35: return 'z';
        default:
            return -1;
    }
}
