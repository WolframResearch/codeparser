
#include "WLCharacter.h"

#include "Utils.h"
#include "Source.h"
#include "CodePoint.h"
#include "CharacterMaps.h"

#include <sstream>
#include <cctype> // for isdigit, isalpha, ispunct, iscntrl with GCC and MSVC

int fromDigit(int d);

int get_graphical_i();

void makeGraphical(std::ostream& stream, int i);

//
// Respect the actual escape style
//
std::ostream& operator<<(std::ostream& stream, WLCharacter c) {
    
    auto i = c.to_point();
    
    assert(i != CODEPOINT_ERROR_INTERNAL);
    
    auto escape = c.escape();
    
    switch (escape) {
        case ESCAPE_NONE:
            
            //
            // Determine whether graphical flag is on
            //
            
            if (stream.iword(get_graphical_i()) == 0) {
                stream << SourceCharacter(i);
                break;
            }
            
            makeGraphical(stream, i);
            
            break;
        case ESCAPE_SINGLE:
            switch (i) {
                case CODEPOINT_STRINGMETA_BACKSPACE:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('b');
                    break;
                case CODEPOINT_STRINGMETA_FORMFEED:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('f');
                    break;
                case CODEPOINT_STRINGMETA_LINEFEED:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('n');
                    break;
                case CODEPOINT_STRINGMETA_CARRIAGERETURN:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('r');
                    break;
                case CODEPOINT_STRINGMETA_TAB:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('t');
                    break;
                case CODEPOINT_STRINGMETA_OPEN:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('<');
                    break;
                case CODEPOINT_STRINGMETA_CLOSE:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('>');
                    break;
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('"');
                    break;
                case CODEPOINT_STRINGMETA_BACKSLASH:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('\\');
                    break;
                case CODEPOINT_LINEARSYNTAX_BANG:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('!');
                    break;
                case CODEPOINT_LINEARSYNTAX_PERCENT:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('%');
                    break;
                case CODEPOINT_LINEARSYNTAX_AMP:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('&');
                    break;
                case CODEPOINT_LINEARSYNTAX_OPENPAREN:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('(');
                    break;
                case CODEPOINT_LINEARSYNTAX_CLOSEPAREN:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter(')');
                    break;
                case CODEPOINT_LINEARSYNTAX_STAR:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('*');
                    break;
                case CODEPOINT_LINEARSYNTAX_PLUS:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('+');
                    break;
                case CODEPOINT_LINEARSYNTAX_SLASH:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('/');
                    break;
                case CODEPOINT_LINEARSYNTAX_AT:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('@');
                    break;
                case CODEPOINT_LINEARSYNTAX_CARET:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('^');
                    break;
                case CODEPOINT_LINEARSYNTAX_UNDER:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('_');
                    break;
                case CODEPOINT_LINEARSYNTAX_BACKTICK:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter('`');
                    break;
                case CODEPOINT_LINEARSYNTAX_SPACE:
                    stream << SourceCharacter('\\');
                    stream << SourceCharacter(' ');
                    break;
                case CODEPOINT_LINECONTINUATION:
                    //
                    // FIXME: could have come from source \\\r\n or even just \\\r
                    //
                    stream << SourceCharacter('\\');
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
            ii = ii / 8;
            auto o1 = ii % 8;
            ii = ii / 8;
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
            ii = ii / 16;
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
            ii = ii / 16;
            auto x1 = ii % 16;
            ii = ii / 16;
            auto x2 = ii % 16;
            ii = ii / 16;
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

void makeGraphical(std::ostream& stream, int i) {
    
    switch (i) {
        case CODEPOINT_ENDOFFILE:
            //
            // Invent something for EOF
            //
            stream << SourceCharacter('<');
            stream << SourceCharacter('E');
            stream << SourceCharacter('O');
            stream << SourceCharacter('F');
            stream << SourceCharacter('>');
            break;
        //
        // whitespace and newline characters
        //
        case '\t':
            stream << WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
            break;
        case '\n':
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
        case '\r':
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            break;
        case CODEPOINT_ESC:
            //
            // Use \[RawEscape]
            //
            stream << WLCharacter(CODEPOINT_ESC, ESCAPE_LONGNAME);
            break;
        //
        // C0 control characters
        //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': case '\x07': case '\x08':
        //
        // Skip TAB, LF, CR, and ESC. They are handled above
        //
        case '\x0b': case '\x0c': case '\x0e': case '\x0f': case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15':
        case '\x16': case '\x17': case '\x18': case '\x19': case '\x1a': case '\x1c': case '\x1d': case '\x1e': case '\x1f':
        //
        // Make sure to include DEL
        //
        case CODEPOINT_DEL:
        //
        // C1 control characters
        //
        case '\x80': case '\x81': case '\x82': case '\x83': case '\x84': case '\x85': case '\x86': case '\x87': case '\x88': case '\x89':
        case '\x8a': case '\x8b': case '\x8c': case '\x8d': case '\x8e': case '\x8f': case '\x90': case '\x91': case '\x92': case '\x93':
        case '\x94': case '\x95': case '\x96': case '\x97': case '\x98': case '\x99': case '\x9a': case '\x9b': case '\x9c': case '\x9d':
        case '\x9e': case '\x9f':
            stream << WLCharacter(i, ESCAPE_2HEX);
            break;
        //
        // everything else is untouched
        //
        default:
            stream << SourceCharacter(i);
            break;
    }
}

int get_graphical_i() {
    static int i = std::ios_base::xalloc();
    return i;
}

std::ostream& set_graphical(std::ostream& stream) {
    stream.iword(get_graphical_i()) = 1;
    return stream;
}

std::ostream& clear_graphical(std::ostream& stream) {
    stream.iword(get_graphical_i()) = 0;
    return stream;
}


std::string WLCharacter::string() const {
    
    std::ostringstream String;
    
    String << clear_graphical << *this;
    
    return String.str();
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
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    if (isSpace()) {
        return false;
    }
    
    if (isNewline()) {
        return false;
    }
    
    if (isUninterpretable()) {
        return false;
    }
    
    //
    // Most of ASCII control characters are letterlike.
    //
    // Except for BEL and DEL. Note that '\x07' and '\x7f' are missing.
    // They are uninterpretable.
    //
    
    return isAlpha() || val == '$' || isControl();
}

bool WLCharacter::isStrangeLetterlike() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    //
    // Dump out if not a letterlike character
    //
    if (!isLetterlike()) {
        return false;
    }
    
    //
    // Using control character as letterlike is strange
    //
    if (isControl()) {
        return true;
    }
    
    return false;
}

bool WLCharacter::isSpace() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return val == ' ' || val == '\t' || val == '\v' || val == '\f';
}

bool WLCharacter::isStrangeSpace() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return val == '\v' || val == '\f';
}

bool WLCharacter::isNewline() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return val == '\n' || val == '\r';
}

bool WLCharacter::isAlpha() const {
    auto val = to_point();
    
    if (!(0 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isalpha(val);
}

bool WLCharacter::isDigit() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isdigit(val);
}

bool WLCharacter::isAlphaOrDigit() const {
    auto val = to_point();
    
    if (!(0 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isalnum(val);
}

bool WLCharacter::isHex() const {
    auto val = to_point();
    
    if (!(0 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isxdigit(val);
}

bool WLCharacter::isOctal() const {
    auto val = to_point();
    
    if (!(0 <= val && val <= 0x7f)) {
        return false;
    }
    
    return '0' <= val && val <= '7';
}

bool WLCharacter::isPunctuation() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::ispunct(val);
}

bool WLCharacter::isUninterpretable() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    switch (val) {
        case CODEPOINT_BEL:
        case CODEPOINT_DEL:
            return true;
        default:
            return false;
    }
    
}

bool WLCharacter::isLinearSyntax() const {
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

bool WLCharacter::isStringMeta() const {
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

bool WLCharacter::isControl() const {
    auto val = to_point();
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    return std::iscntrl(val);
}

//
// isLetterlikeCharacter is special because it is defined in terms of other categories
//
// basically, if it's not anything else, then it's letterlike
//
bool WLCharacter::isLetterlikeCharacter() const {
    auto val = to_point();
    auto esc = escape();
    
    //
    // Reject if ASCII, should use isLetterlike()
    //
    if ((0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    if (isPunctuationCharacter()) {
        return false;
    }
    
    //
    // Must handle all of the specially defined CodePoints
    //
    
    if (val == CODEPOINT_LINECONTINUATION) {
        return false;
    }
    
    if (val == CODEPOINT_ENDOFFILE) {
        return false;
    }
    
    if (val == CODEPOINT_ERROR_INTERNAL) {
        return false;
    }
    
//    if (val == CODEPOINT_NAKED_BACKSLASH) {
//        return false;
//    }
    
    if (isLinearSyntax()) {
        return false;
    }
    
    if (isStringMeta()) {
        return false;
    }
    
    if (isSpaceCharacter()) {
        return false;
    }
    
    if (isNewlineCharacter()) {
        return false;
    }
    
    if (isUninterpretableCharacter()) {
        return false;
    }
    
    if (esc == ESCAPE_LONGNAME && Utils::isUnsupportedLongName(CodePointToLongNameMap[val])) {
        return false;
    }
    
    return true;
}

bool WLCharacter::isStrangeLetterlikeCharacter() const {
    auto esc = escape();
    auto val = to_point();
    
    //
    // Dump out if not a letterlike character
    //
    if (!isLetterlikeCharacter()) {
        return false;
    }
    
    //
    // Assume that if some high character like 0xf456 is directly encoded with no escaping, then it is purposeful
    //
    if (esc == ESCAPE_NONE) {
        return false;
    }
    
    //
    //
    //
    if (esc == ESCAPE_LONGNAME) {
        return Utils::isStrangeLetterlikeLongName(CodePointToLongNameMap[val]);
    }
    
    //
    // Using control character as letterlike is strange
    //
    if (isControlCharacter()) {
        return true;
    }
    
    //
    // Assume that using other escapes is strange
    //
    
    return true;
}

bool WLCharacter::isStrangeSpaceCharacter() const {
    auto esc = escape();
    
    //
    // Dump out if not a space character
    //
    if (!isSpaceCharacter()) {
        return false;
    }
    
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

bool WLCharacter::isStrangeNewlineCharacter() const {
    auto esc = escape();
    
    //
    // Dump out if not a newline character
    //
    if (!isNewlineCharacter()) {
        return false;
    }
    
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

bool WLCharacter::isControlCharacter() const {
    auto val = to_point();
    
    //
    // Reject if ASCII, should use isControl()
    //
    if ((0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    //
    // C1 block of Unicode
    //
    if ((0x80 <= val && val <= 0x9f)) {
        return true;
    }
    
    return false;
}

//
// Given a digit, return the character
//
int fromDigit(int d) {
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

//
// For googletest
//
void PrintTo(const WLCharacter& C, std::ostream* stream) {
    *stream << set_graphical << C << clear_graphical;
}
