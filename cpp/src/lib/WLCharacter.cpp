
#include "WLCharacter.h"

#include "Utils.h" // for isUnsupportedLongName
#include "Source.h" // for SourceCharacer
#include "CharacterMaps.h" // for FromSpecialMap

#include <cctype> // for isdigit, isalpha, ispunct, iscntrl with GCC and MSVC
#include <sstream> // for ostringstream
#include <ostream> // for ostream

char fromDigit(size_t d);

//
// Respect the actual escape style
//
std::ostream& operator<<(std::ostream& stream, WLCharacter c) {
    
    auto i = c.to_point();
    
    assert(i != CODEPOINT_UNKNOWN);
    
    auto escape = c.escape();
    
    switch (escape) {
        case ESCAPE_NONE:
        case ESCAPE_RAW:
            stream << SourceCharacter(i);
            break;
        case ESCAPE_SINGLE:
            stream << SourceCharacter('\\');
            switch (i) {
                case CODEPOINT_STRINGMETA_BACKSPACE:
                    stream << SourceCharacter('b');
                    break;
                case CODEPOINT_STRINGMETA_FORMFEED:
                    stream << SourceCharacter('f');
                    break;
                case CODEPOINT_STRINGMETA_LINEFEED:
                    stream << SourceCharacter('n');
                    break;
                case CODEPOINT_STRINGMETA_CARRIAGERETURN:
                    stream << SourceCharacter('r');
                    break;
                case CODEPOINT_STRINGMETA_TAB:
                    stream << SourceCharacter('t');
                    break;
                case CODEPOINT_STRINGMETA_OPEN:
                    stream << SourceCharacter('<');
                    break;
                case CODEPOINT_STRINGMETA_CLOSE:
                    stream << SourceCharacter('>');
                    break;
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE:
                    stream << SourceCharacter('"');
                    break;
                case CODEPOINT_STRINGMETA_BACKSLASH:
                    stream << SourceCharacter('\\');
                    break;
                case CODEPOINT_LINEARSYNTAX_BANG:
                    stream << SourceCharacter('!');
                    break;
                case CODEPOINT_LINEARSYNTAX_PERCENT:
                    stream << SourceCharacter('%');
                    break;
                case CODEPOINT_LINEARSYNTAX_AMP:
                    stream << SourceCharacter('&');
                    break;
                case CODEPOINT_LINEARSYNTAX_OPENPAREN:
                    stream << SourceCharacter('(');
                    break;
                case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
                    stream << SourceCharacter(')');
                    break;
                case CODEPOINT_LINEARSYNTAX_STAR:
                    stream << SourceCharacter('*');
                    break;
                case CODEPOINT_LINEARSYNTAX_PLUS:
                    stream << SourceCharacter('+');
                    break;
                case CODEPOINT_LINEARSYNTAX_SLASH:
                    stream << SourceCharacter('/');
                    break;
                case CODEPOINT_LINEARSYNTAX_AT:
                    stream << SourceCharacter('@');
                    break;
                case CODEPOINT_LINEARSYNTAX_CARET:
                    stream << SourceCharacter('^');
                    break;
                case CODEPOINT_LINEARSYNTAX_UNDER:
                    stream << SourceCharacter('_');
                    break;
                case CODEPOINT_LINEARSYNTAX_BACKTICK:
                    stream << SourceCharacter('`');
                    break;
                case CODEPOINT_LINEARSYNTAX_SPACE:
                    stream << SourceCharacter(' ');
                    break;
                case CODEPOINT_LINECONTINUATION_LF:
                    stream << SourceCharacter('\n');
                    break;
                case CODEPOINT_LINECONTINUATION_CR:
                    stream << SourceCharacter('\r');
                    break;
                case CODEPOINT_LINECONTINUATION_CRLF:
                    stream << SourceCharacter('\r');
                    stream << SourceCharacter('\n');
                    break;
                default:
                    assert(false);
                    break;
            }
            break;
        case ESCAPE_LONGNAME: {
            
            auto LongName = CodePointToLongNameMap[i];
            
            stream << SourceCharacter('\\');
            stream << SourceCharacter('[');
            for (size_t idx = 0; idx < LongName.size(); idx++) {
                stream << SourceCharacter(LongName[idx]);
            }
            stream << SourceCharacter(']');
        }
            break;
        case ESCAPE_OCTAL: {
            
            auto ii = i;
            
            auto it = FromSpecialMap.find(i);
            if (it != FromSpecialMap.end()) {
                ii = it->second;
            }
            
            auto o0 = ii % 8;
            ii /= 8;
            auto o1 = ii % 8;
            ii /= 8;
            auto o2 = ii % 8;
            
            stream << SourceCharacter('\\');
            stream << SourceCharacter(fromDigit(o2));
            stream << SourceCharacter(fromDigit(o1));
            stream << SourceCharacter(fromDigit(o0));
        }
            break;
        case ESCAPE_2HEX: {
            
            auto ii = i;
            
            auto it = FromSpecialMap.find(i);
            if (it != FromSpecialMap.end()) {
                ii = it->second;
            }
            
            auto x0 = ii % 16;
            ii /= 16;
            auto x1 = ii % 16;
            
            stream << SourceCharacter('\\');
            stream << SourceCharacter('.');
            stream << SourceCharacter(fromDigit(x1));
            stream << SourceCharacter(fromDigit(x0));
        }
            break;
        case ESCAPE_4HEX: {
            
            auto ii = i;
            
            auto it = FromSpecialMap.find(i);
            if (it != FromSpecialMap.end()) {
                ii = it->second;
            }
            
            auto x0 = ii % 16;
            ii /= 16;
            auto x1 = ii % 16;
            ii /= 16;
            auto x2 = ii % 16;
            ii /= 16;
            auto x3 = ii % 16;
            
            stream << SourceCharacter('\\');
            stream << SourceCharacter(':');
            stream << SourceCharacter(fromDigit(x3));
            stream << SourceCharacter(fromDigit(x2));
            stream << SourceCharacter(fromDigit(x1));
            stream << SourceCharacter(fromDigit(x0));
        }
            break;
        case ESCAPE_6HEX: {
            
            auto ii = i;
            
            auto it = FromSpecialMap.find(i);
            if (it != FromSpecialMap.end()) {
                ii = it->second;
            }
            
            auto x0 = ii % 16;
            ii /= 16;
            auto x1 = ii % 16;
            ii /= 16;
            auto x2 = ii % 16;
            ii /= 16;
            auto x3 = ii % 16;
            ii /= 16;
            auto x4 = ii % 16;
            ii /= 16;
            auto x5 = ii % 16;
            
            stream << SourceCharacter('\\');
            stream << SourceCharacter('|');
            stream << SourceCharacter(fromDigit(x5));
            stream << SourceCharacter(fromDigit(x4));
            stream << SourceCharacter(fromDigit(x3));
            stream << SourceCharacter(fromDigit(x2));
            stream << SourceCharacter(fromDigit(x1));
            stream << SourceCharacter(fromDigit(x0));
        }
            break;
        default: {
            assert(false);
            break;
        }
    }
    
    return stream;
}

std::string WLCharacter::graphicalString() const {
    
    std::ostringstream String;
    
    String << set_graphical << *this << clear_graphical;
    
    return String.str();
}

bool WLCharacter::isEscaped() const {
    return escape() != ESCAPE_NONE;
}

bool WLCharacter::isLetterlike() const {
    
    auto val = to_point();
    
    //
    // Most of ASCII control characters are letterlike.
    // jessef: There may be such a thing as *too* binary-safe...
    //
    // Except for LF, CR: those are newlines
    //
    // Except for TAB, VT, and FF: those are spaces
    //
    // Except for BEL and DEL: those are uninterpretable
    //
    switch (val) {
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': /*    \x07*/
        case '\x08': /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        case '$':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        /* \x7f*/
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isStrangeLetterlike() const {
    
    //
    // Dump out if not a letterlike character
    //
    if (!isLetterlike()) {
        return false;
    }
    
    if (isVeryStrangeLetterlike()) {
        return true;
    }
    
    return false;
}

bool WLCharacter::isVeryStrangeLetterlike() const {
    
    //
    // Dump out if not a letterlike character
    //
    if (!isLetterlike()) {
        return false;
    }
    
    //
    // Using control character as letterlike is very strange
    //
    // jessef: There may be such a thing as *too* binary-safe...
    //
    if (isControl()) {
        return true;
    }
    
    return false;
}

bool WLCharacter::isWhitespace() const {
    
    auto val = to_point();
    
    switch (val) {
        case ' ': case '\t': case '\v': case '\f':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isStrangeWhitespace() const {
    
    auto val = to_point();
    
    switch (val) {
        case '\v': case '\f':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isNewline() const {
    
    auto val = to_point();
    
    switch (val) {
        case '\n': case '\r':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isAlpha() const {
    
    auto val = to_point();
    
    switch (val) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isDigit() const {
    
    auto val = to_point();
    
    switch (val) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isAlphaOrDigit() const {
    
    auto val = to_point();
    
    switch (val) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isHex() const {
    
    auto val = to_point();
    
    switch (val) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isOctal() const {
    
    auto val = to_point();
    
    switch (val) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isControl() const {
    
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    return std::iscntrl(val);
}

bool WLCharacter::isSign() const {
    
    auto val = to_point();
    
    switch (val) {
        case '-': case '+':
            return true;
        default:
            return false;
    }
}


//
// Multi-byte character properties
//

bool WLCharacter::isMBLinearSyntax() const {
    
    auto val = to_point();
    
    switch (val) {
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

bool WLCharacter::isMBStringMeta() const {
    
    auto val = to_point();
    
    switch (val) {
        case CODEPOINT_STRINGMETA_OPEN:
        case CODEPOINT_STRINGMETA_CLOSE:
        case CODEPOINT_STRINGMETA_BACKSLASH:
        case CODEPOINT_STRINGMETA_DOUBLEQUOTE:
        case CODEPOINT_STRINGMETA_BACKSPACE:
        case CODEPOINT_STRINGMETA_FORMFEED:
        case CODEPOINT_STRINGMETA_LINEFEED:
        case CODEPOINT_STRINGMETA_CARRIAGERETURN:
        case CODEPOINT_STRINGMETA_TAB:
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isMBUnsupported() const {
    
    auto esc = escape();
    
    if (esc == ESCAPE_LONGNAME) {
        
        auto val = to_point();
        
        if (Utils::isUnsupportedLongName(CodePointToLongNameMap[val])) {
            return true;
        }
    }
    
    return false;
}

//
// isLetterlikeCharacter is special because it is defined in terms of other categories
//
// basically, if it's not anything else, then it's letterlike
//
bool WLCharacter::isMBLetterlike() const {
    auto val = to_point();
    
    //
    // Reject if single byte, should use isLetterlike()
    //
    if ((0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    if (isMBPunctuation()) {
        return false;
    }
    
    //
    // Must handle all of the specially defined CodePoints
    //
    
    if (isMBLineContinuation()) {
        return false;
    }
    
    if (val == CODEPOINT_ENDOFFILE) {
        return false;
    }
    
    if (val == CODEPOINT_UNKNOWN) {
        return false;
    }
    
    if (val == CODEPOINT_CRLF) {
        return false;
    }
    
    if (isMBLinearSyntax()) {
        return false;
    }
    
    if (isMBStringMeta()) {
        return false;
    }
    
    if (isMBWhitespace()) {
        return false;
    }
    
    if (isMBNewline()) {
        return false;
    }
    
    if (isMBUninterpretable()) {
        return false;
    }
    
    if (isMBUnsupported()) {
        return false;
    }
    
    return true;
}

bool WLCharacter::isMBStrangeLetterlike() const {
    
    //
    // Dump out if not a letterlike character
    //
    if (!isMBLetterlike()) {
        return false;
    }
    
    if (isMBVeryStrangeLetterlike()) {
        return true;
    }
    
    auto esc = escape();
    auto val = to_point();
    if (esc == ESCAPE_LONGNAME) {
        return Utils::isStrangeLetterlikeLongName(CodePointToLongNameMap[val]);
    }
    
    //
    // Assume that using other escapes is strange
    //
    
    return true;
}

bool WLCharacter::isMBVeryStrangeLetterlike() const {
    
    //
    // Dump out if not a letterlike character
    //
    if (!isMBLetterlike()) {
        return false;
    }
    
    auto esc = escape();
    auto val = to_point();
    if (esc == ESCAPE_LONGNAME) {
        return Utils::isVeryStrangeLetterlikeLongName(CodePointToLongNameMap[val]);
    }
    
    //
    // Using control character as letterlike is very strange
    //
    if (isMBControl()) {
        return true;
    }
    
    return false;
}

bool WLCharacter::isMBStrangeWhitespace() const {
    
    //
    // Dump out if not a space character
    //
    if (!isMBWhitespace()) {
        return false;
    }
    
    auto esc = escape();
    //
    // Assume that if some high character is directly encoded with no escaping, then it is purposeful
    //
    if (esc == ESCAPE_NONE) {
        return false;
    }
    
    //
    // Assume that all space characters are strange
    //
    
    return true;
}

bool WLCharacter::isMBStrangeNewline() const {
    
    //
    // Dump out if not a newline character
    //
    if (!isMBNewline()) {
        return false;
    }
    
    auto esc = escape();
    //
    // Assume that if some high character is directly encoded with no escaping, then it is purposeful
    //
    if (esc == ESCAPE_NONE) {
        return false;
    }
    
    //
    // Assume that all newline characters are strange
    //
    
    return true;
}

bool WLCharacter::isMBControl() const {
    
    auto val = to_point();
    
    //
    // C1 block of Unicode
    //
    if ((0x80 <= val && val <= 0x9f)) {
        return true;
    }
    
    return false;
}

bool WLCharacter::isMBLineContinuation() const {
    
    auto val = to_point();
    
    switch (val) {
        case CODEPOINT_LINECONTINUATION_LF:
        case CODEPOINT_LINECONTINUATION_CR:
        case CODEPOINT_LINECONTINUATION_CRLF:
            return true;
        default:
            return false;
    }
}

bool WLCharacter::isMBNewline() const {
    
    auto val = to_point();
    
    return Utils::isMBNewline(val);
}

bool WLCharacter::isMBWhitespace() const {
    
    auto val = to_point();
    
    return Utils::isMBWhitespace(val);
}

bool WLCharacter::isMBPunctuation() const {
    
    auto val = to_point();
    
    return Utils::isMBPunctuation(val);
}

bool WLCharacter::isMBUninterpretable() const {
    
    auto val = to_point();
    
    return Utils::isMBUninterpretable(val);
}

//
// Given a digit, return the character
//
char fromDigit(size_t d) {
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
            assert(false);
            return '!';
    }
}

//
// For googletest
//
void PrintTo(const WLCharacter& C, std::ostream* stream) {
    *stream << set_graphical << C << clear_graphical;
}
