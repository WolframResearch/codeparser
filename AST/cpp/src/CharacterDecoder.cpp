
#include "CharacterDecoder.h"

#include "ByteDecoder.h"
#include "LongNameMap.h"

#include <iomanip>
#include <cassert>


bool isLinearSyntax(WLCharacter c) {
switch (c) {
    case LINEARSYNTAX_BANG:
    case LINEARSYNTAX_PERCENT:
    case LINEARSYNTAX_AMP:
    case LINEARSYNTAX_OPENPAREN:
    case LINEARSYNTAX_CLOSEPAREN:
    case LINEARSYNTAX_STAR:
    case LINEARSYNTAX_PLUS:
    case LINEARSYNTAX_SLASH:
    case LINEARSYNTAX_AT:
    case LINEARSYNTAX_CARET:
    case LINEARSYNTAX_UNDER:
    case LINEARSYNTAX_BACKTICK:
    case LINEARSYNTAX_SPACE:
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
                case LINEARSYNTAX_BANG:
                    String.put('\\');
                    String.put('!');
                    break;
                case LINEARSYNTAX_PERCENT:
                    String.put('\\');
                    String.put('%');
                    break;
                case LINEARSYNTAX_AMP:
                    String.put('\\');
                    String.put('&');
                    break;
                case LINEARSYNTAX_OPENPAREN:
                    String.put('\\');
                    String.put('(');
                    break;
                case LINEARSYNTAX_CLOSEPAREN:
                    String.put('\\');
                    String.put(')');
                    break;
                case LINEARSYNTAX_STAR:
                    String.put('\\');
                    String.put('*');
                    break;
                case LINEARSYNTAX_PLUS:
                    String.put('\\');
                    String.put('+');
                    break;
                case LINEARSYNTAX_SLASH:
                    String.put('\\');
                    String.put('/');
                    break;
                case LINEARSYNTAX_AT:
                    String.put('\\');
                    String.put('@');
                    break;
                case LINEARSYNTAX_CARET:
                    String.put('\\');
                    String.put('^');
                    break;
                case LINEARSYNTAX_UNDER:
                    String.put('\\');
                    String.put('_');
                    break;
                case LINEARSYNTAX_BACKTICK:
                    String.put('\\');
                    String.put('`');
                    break;
                case LINEARSYNTAX_SPACE:
                    String.put('\\');
                    String.put(' ');
                    break;
            }
            
        } else if (c == EOF) {
            
            //
            // Do not return a string for EOF
            //
            
        } else {
            
            String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec;
        }
        
    } else if (isSpace(c)) {
        
        // ASCII
        //
        // \n, \r, \t, or (space) and it is ok to write directly
        //
        
        String.put(c);
        
    } else if (isControl(c)) {
        
        // ASCII
        //
        // something nasty like '\0'
        //
        
        String << "\\:" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec;
        
    } else {
        
        // ASCII
        //
        
        String.put(c);
    }
    
    return String.str();
}

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
        case '\n': {
        
            //
            // Line continuation
            //
            
            nextWLCharacter();
            
            //
            // Process the white space and glue together pieces
            //
            if (policy != POLICY_PRESERVE_WHITESPACE_AFTER_LINE_CONTINUATION) {
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
            
            handleLongName(CharacterStart);
        }
            break;
        case ':': {
            
            handle4Hex(CharacterStart);
        }
            break;
        case '.': {
            
            handle2Hex(CharacterStart);
        }
            break;
        case '|': {
            
            handle6Hex(CharacterStart);
        }
            break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            handleOctal(CharacterStart);
        }
            break;
        // Simple escaped characters
        //
        // C++ also has \a and \v, but Wolfram Language does not have \a nor \v
        //
        case 'b': {

            c = '\b';
        }
            break;
        case 'f': {

            c = '\f';
        }
            break;
        case 'n': {

            c = '\n';
        }
            break;
        case 'r': {

            c = '\r';
        }
            break;
        case 't': {

            c = '\t';
        }
            break;
        case '"': case '\\': case '<': case '>': {
            
            //
            // Special string characters: Leave alone
            //
            // What is \< and \> ?
            // https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
            // https://stackoverflow.com/q/6065887
            //
            
            auto Loc = CharacterStart;
            Loc = Loc + 1;
            
            characterQueue.push_back(std::make_pair(c, Loc));
            
            c = '\\';
            
            TheSourceManager->setSourceLocation(CharacterStart);
            TheSourceManager->setWLCharacterStart();
        }
            break;
        case '!': {
            c = LINEARSYNTAX_BANG;
        }
            break;
        case '%': {
            c = LINEARSYNTAX_PERCENT;
        }
            break;
        case '&': {
            c = LINEARSYNTAX_AMP;
        }
            break;
        case '(': {
            c = LINEARSYNTAX_OPENPAREN;
        }
            break;
        case ')': {
            c = LINEARSYNTAX_CLOSEPAREN;
        }
            break;
        case '*': {
            c = LINEARSYNTAX_STAR;
        }
            break;
        case '+': {
            c = LINEARSYNTAX_PLUS;
        }
            break;
        case '/': {
            c = LINEARSYNTAX_SLASH;
        }
            break;
        case '@': {
            c = LINEARSYNTAX_AT;
        }
            break;
        case '^': {
            c = LINEARSYNTAX_CARET;
        }
            break;
        case '_': {
            c = LINEARSYNTAX_UNDER;
        }
            break;
        case '`': {
            c = LINEARSYNTAX_BACKTICK;
        }
            break;
        case ' ': {
            c = LINEARSYNTAX_SPACE;
        }
            break;
        default: {
            
            //
            // Anything else: Leave alone and also warn
            //

            auto Loc = TheSourceManager->getSourceLocation();

            auto Issue = SyntaxIssue("Unrecognized escaped character: \\" + WLCharacterToString(c), SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});

            Issues.push_back(Issue);
            


            Loc = CharacterStart;
            Loc = Loc + 1;
            
            characterQueue.push_back(std::make_pair(c, Loc));
            
            c = '\\';
            
            TheSourceManager->setSourceLocation(CharacterStart);
            TheSourceManager->setWLCharacterStart();
        }
    }

    TheSourceManager->setWLCharacterEnd();

    return c;
}

WLCharacter CharacterDecoder::currentWLCharacter() {
   return c;
}

//
// c is set to the next WL character
//
void CharacterDecoder::handleLongName(SourceLocation CharacterStart) {
    
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
        
        auto Loc = TheSourceManager->getSourceLocation();
        
        auto Issue = SyntaxIssue("Unrecognized character: \\[" + LongNameStr + WLCharacterToString(c), SEVERITY_ERROR, SourceSpan{Loc, Loc});
        
        Issues.push_back(Issue);
        
        
        Loc = CharacterStart;
        Loc = Loc + 1;
        characterQueue.push_back(std::make_pair('[', Loc));
        for (size_t i = 0; i < LongNameStr.size(); i++) {
            auto l = LongNameStr[i];
            Loc = Loc + 1;
            characterQueue.push_back(std::make_pair(l, Loc));
        }
        Loc = Loc + 1;
        characterQueue.push_back(std::make_pair(c, Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
    } else if (LongNameStr == "RawDoubleQuote") {
        // \[RawDoubleQuote]

        //
        // insert \" instead of just "
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('"', Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
    } else if (LongNameStr == "RawBackslash") {
        // \[RawBackslash]
        
        //
        // insert \\ instead of just \
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('\\', Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
   } else {
        
        auto it = LongNameToCodePointMap.find(LongNameStr);
        if (it != LongNameToCodePointMap.end()) {
            
            c = it->second;
            
        } else {
            
            //
            // Unrecognized
            //
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue("Unrecognized character: \\[" + LongNameStr, SEVERITY_ERROR, SourceSpan{CharacterStart, Loc});
            
            Issues.push_back(Issue);
            
            Loc = CharacterStart;
            characterQueue.push_back(std::make_pair('\\', Loc));
            Loc = Loc + 1;
            characterQueue.push_back(std::make_pair('[', Loc));
            for (size_t i = 0; i < LongNameStr.size(); i++) {
                auto l = LongNameStr[i];
                Loc = Loc + 1;
                characterQueue.push_back(std::make_pair(l, Loc));
            }
            Loc = Loc + 1;
            characterQueue.push_back(std::make_pair(']', Loc));
            
            c = '\\';
            
            TheSourceManager->setSourceLocation(CharacterStart);
            TheSourceManager->setWLCharacterStart();
        }
    }
}

void CharacterDecoder::handle4Hex(SourceLocation CharacterStart) {
    
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
            
            auto HexStr = Hex.str();
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue("Unrecognized character: \\:" + HexStr + WLCharacterToString(c), SEVERITY_ERROR, SourceSpan{Loc, Loc});

            Issues.push_back(Issue);

            Loc = CharacterStart;
            characterQueue.push_back(std::make_pair('\\', Loc));
            Loc = Loc + 1;
            characterQueue.push_back(std::make_pair(':', Loc));
            for (size_t i = 0; i < HexStr.size(); i++) {
                auto l = HexStr[i];
                Loc = Loc + 1;
                characterQueue.push_back(std::make_pair(l, Loc));
            }
            
            c = '\\';

            TheSourceManager->setSourceLocation(CharacterStart);
            TheSourceManager->setWLCharacterStart();
            
            return;
        }
    }
    
    auto HexStr = Hex.str();
    
    if (HexStr == "0022") {
        // \[RawDoubleQuote]

        //
        // insert \" instead of just "
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('"', Loc));
        
        c = '\\';
        
    } else if (HexStr == "005c") {
        // \[RawBackslash]

        //
        // insert \\ instead of just \
        //

        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('\\', Loc));

        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
   } else {
        
        c = parseInteger(HexStr, 16);
    }
}

void CharacterDecoder::handle2Hex(SourceLocation CharacterStart) {
    
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
            
            auto HexStr = Hex.str();
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue("Unrecognized character: \\." + HexStr + WLCharacterToString(c), SEVERITY_ERROR, SourceSpan{Loc, Loc});

            Issues.push_back(Issue);

            characterQueue.push_back(std::make_pair('\\', Loc));
            characterQueue.push_back(std::make_pair('.', Loc));
            for (size_t i = 0; i < HexStr.size(); i++) {
                auto l = HexStr[i];
                characterQueue.push_back(std::make_pair(l, Loc));
            }
            
            c = '\\';

            return;
        }
    }
    
    auto HexStr = Hex.str();
    
    if (HexStr == "22") {
        // \[RawDoubleQuote]

        //
        // insert \" instead of just "
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('"', Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
    } else if (HexStr == "5c") {
        // \[RawBackslash]

        //
        // insert \\ instead of just \
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('\\', Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
   } else {
        
        c = parseInteger(HexStr, 16);
    }
}

void CharacterDecoder::handleOctal(SourceLocation CharacterStart) {
    
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
            
            auto OctalStr = Octal.str();
            
            auto Loc = TheSourceManager->getSourceLocation();
            
            auto Issue = SyntaxIssue("Unrecognized character: \\" + OctalStr = WLCharacterToString(c), SEVERITY_ERROR, SourceSpan{Loc, Loc});

            Issues.push_back(Issue);

            characterQueue.push_back(std::make_pair('\\', Loc));
            for (size_t i = 0; i < OctalStr.size(); i++) {
                auto l = OctalStr[i];
                characterQueue.push_back(std::make_pair(l, Loc));
            }
            
            c = '\\';
            
            TheSourceManager->setSourceLocation(CharacterStart);
            TheSourceManager->setWLCharacterStart();
            
            return;
        }
    }
    
    auto OctalStr = Octal.str();
    
    if (OctalStr == "042") {
        // \[RawDoubleQuote]

        //
        // insert \" instead of just "
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('"', Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
    } else if (OctalStr == "134") {
        // \[RawBackslash]

        //
        // insert \\ instead of just \
        //

        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('\\', Loc));

        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
   } else {
        
        c = parseInteger(OctalStr, 8);
    }
}

void CharacterDecoder::handle6Hex(SourceLocation CharacterStart) {
    
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
            
            auto HexStr = Hex.str();
            
            auto Loc = TheSourceManager->getSourceLocation();

            auto Issue = SyntaxIssue("Unrecognized character: \\|" + HexStr + WLCharacterToString(c), SEVERITY_ERROR, SourceSpan{Loc, Loc});

            Issues.push_back(Issue);

            characterQueue.push_back(std::make_pair('\\', Loc));
            characterQueue.push_back(std::make_pair('|', Loc));
            for (size_t i = 0; i < HexStr.size(); i++) {
                auto l = HexStr[i];
                characterQueue.push_back(std::make_pair(l, Loc));
            }
            
            c = '\\';
            
            TheSourceManager->setSourceLocation(CharacterStart);
            TheSourceManager->setWLCharacterStart();
            
            return;
        }
    }
    
    auto HexStr = Hex.str();
    
    if (HexStr == "000022") {
        // \[RawDoubleQuote]

        //
        // insert \" instead of just "
        //
        
        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('"', Loc));
        
        c = '\\';
        
        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
    } else if (HexStr == "00005c") {
        // \[RawBackslash]

        //
        // insert \\ instead of just \
        //

        auto Loc = CharacterStart;
        characterQueue.push_back(std::make_pair('\\', Loc));

        c = '\\';

        TheSourceManager->setSourceLocation(CharacterStart);
        TheSourceManager->setWLCharacterStart();
        
   } else {
        
        c = parseInteger(HexStr, 16);
    }
}

std::vector<SyntaxIssue> CharacterDecoder::getIssues() {
    
    auto Tmp = Issues;
    
    Issues.clear();
    
    return Tmp;
}

CharacterDecoder *TheCharacterDecoder = nullptr;
