
#include "WLCharacter.h"

#include "Utils.h" // for set_graphical, etc.
#include "Source.h" // for SourceCharacer
#include "LongNames.h"
#include "LongNamesRegistration.h" // for CodePointToLongNameMap
#include "CodePoint.h" // for CODEPOINT_ASSERTFALSE

#include <cctype> // for isdigit, isalpha, ispunct, iscntrl with GCC and MSVC
#include <sstream> // for ostringstream
#include <cstddef> // for size_t
#include <algorithm> // for lower_bound
#include <cassert>


char fromDigit(uint8_t d);

//
// Respect the actual escape style
//
std::ostream& operator<<(std::ostream& s, WLCharacter c) {
    
    auto i = c.to_point();
    
    assert(i != CODEPOINT_ASSERTFALSE);
    
    auto escape = c.escape();
    
    switch (escape) {
        case ESCAPE_NONE:
        case ESCAPE_RAW: {
            
            s << SourceCharacter(i);
            
            return s;
        }
        case ESCAPE_SINGLE: {
            
            s << SourceCharacter('\\');
            
            switch (i) {
                case CODEPOINT_STRINGMETA_BACKSPACE: {
                    
                    s << SourceCharacter('b');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_FORMFEED: {
                    
                    s << SourceCharacter('f');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_LINEFEED: {
                    
                    s << SourceCharacter('n');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_CARRIAGERETURN: {
                    
                    s << SourceCharacter('r');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_TAB: {
                    
                    s << SourceCharacter('t');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_OPEN: {
                    
                    s << SourceCharacter('<');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_CLOSE: {
                    
                    s << SourceCharacter('>');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
                    
                    s << SourceCharacter('"');
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_BACKSLASH: {
                    
                    s << SourceCharacter('\\');
                    
                    break;
                }
                case CODEPOINT_LINECONTINUATION_LINEFEED: {
                    
                    s << SourceCharacter('\n');
                    
                    break;
                }
                case CODEPOINT_LINECONTINUATION_CARRIAGERETURN: {
                    
                    s << SourceCharacter('\r');
                    
                    break;
                }
                case CODEPOINT_LINECONTINUATION_CRLF: {
                    
                    s << SourceCharacter('\r');
                    s << SourceCharacter('\n');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_BANG: {
                    
                    s << SourceCharacter('!');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_PERCENT: {
                    
                    s << SourceCharacter('%');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_AMP: {
                    
                    s << SourceCharacter('&');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_OPENPAREN: {
                    
                    s << SourceCharacter('(');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_CLOSEPAREN: {
                    
                    s << SourceCharacter(')');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_STAR: {
                    
                    s << SourceCharacter('*');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_PLUS: {
                    
                    s << SourceCharacter('+');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_SLASH: {
                    
                    s << SourceCharacter('/');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_AT: {
                    
                    s << SourceCharacter('@');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_CARET: {
                    
                    s << SourceCharacter('^');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_UNDER: {
                    
                    s << SourceCharacter('_');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_BACKTICK: {
                    
                    s << SourceCharacter('`');
                    
                    break;
                }
                case CODEPOINT_LINEARSYNTAX_SPACE: {
                    
                    s << SourceCharacter(' ');
                    
                    break;
                }
                default: {
                    
                    assert(false);
                    
                    break;
                }
            }
            
            return s;
        }
        case ESCAPE_LONGNAME: {
            
            auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), i);
            
            assert(it != CodePointToLongNameMap_points.end());
            assert(*it == i);
            
            auto idx = it - CodePointToLongNameMap_points.begin();
            
            auto LongName = CodePointToLongNameMap_names[idx];
            
            s << SourceCharacter('\\');
            s << SourceCharacter('[');
            
            for (size_t idx = 0; idx < LongName.size(); idx++) {
                s << SourceCharacter(LongName[idx]);
            }
            
            s << SourceCharacter(']');
            
            return s;
        }
        case ESCAPE_OCTAL: {
            
            switch (i) {
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
                    
                    i = CODEPOINT_ACTUAL_DOUBLEQUOTE;
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_BACKSLASH: {
                    
                    i = CODEPOINT_ACTUAL_BACKSLASH;
                    
                    break;
                }
            }
            
            auto o0 = static_cast<uint8_t>(i) % 8u;
            i /= 8;
            auto o1 = static_cast<uint8_t>(i) % 8u;
            i /= 8;
            auto o2 = static_cast<uint8_t>(i) % 8u;
            
            s << SourceCharacter('\\');
            s << SourceCharacter(fromDigit(o2));
            s << SourceCharacter(fromDigit(o1));
            s << SourceCharacter(fromDigit(o0));
            
            return s;
        }
        case ESCAPE_2HEX: {
            
            switch (i) {
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
                    
                    i = CODEPOINT_ACTUAL_DOUBLEQUOTE;
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_BACKSLASH: {
                    
                    i = CODEPOINT_ACTUAL_BACKSLASH;
                    
                    break;
                }
            }
            
            auto x0 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x1 = static_cast<uint8_t>(i) % 16u;
            
            s << SourceCharacter('\\');
            s << SourceCharacter('.');
            s << SourceCharacter(fromDigit(x1));
            s << SourceCharacter(fromDigit(x0));
            
            return s;
        }
        case ESCAPE_4HEX: {
            
            switch (i) {
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
                    
                    i = CODEPOINT_ACTUAL_DOUBLEQUOTE;
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_BACKSLASH: {
                    
                    i = CODEPOINT_ACTUAL_BACKSLASH;
                    
                    break;
                }
            }
            
            auto x0 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x1 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x2 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x3 = static_cast<uint8_t>(i) % 16u;
            
            s << SourceCharacter('\\');
            s << SourceCharacter(':');
            s << SourceCharacter(fromDigit(x3));
            s << SourceCharacter(fromDigit(x2));
            s << SourceCharacter(fromDigit(x1));
            s << SourceCharacter(fromDigit(x0));
            
            return s;
        }
        case ESCAPE_6HEX: {
            
            switch (i) {
                case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
                    
                    i = CODEPOINT_ACTUAL_DOUBLEQUOTE;
                    
                    break;
                }
                case CODEPOINT_STRINGMETA_BACKSLASH: {
                    
                    i = CODEPOINT_ACTUAL_BACKSLASH;
                    
                    break;
                }
            }
            
            auto x0 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x1 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x2 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x3 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x4 = static_cast<uint8_t>(i) % 16u;
            i /= 16;
            auto x5 = static_cast<uint8_t>(i) % 16u;
            
            s << SourceCharacter('\\');
            s << SourceCharacter('|');
            s << SourceCharacter(fromDigit(x5));
            s << SourceCharacter(fromDigit(x4));
            s << SourceCharacter(fromDigit(x3));
            s << SourceCharacter(fromDigit(x2));
            s << SourceCharacter(fromDigit(x1));
            s << SourceCharacter(fromDigit(x0));
            
            return s;
        }
    }
    
    assert(false);
    
    return s;
}

std::string WLCharacter::graphicalString() const {
    
    std::ostringstream String;
    
    String << set_graphical << *this << clear_graphical;
    
    return String.str();
}

std::string WLCharacter::safeAndGraphicalString() const {
    
    std::ostringstream String;
    
    if (escape() == ESCAPE_NONE) {
        
        String << "\"" << *this << "\" (" << set_graphical << *this << clear_graphical << ")";
        
        return String.str();
    }
        
    String << *this;
    
    return String.str();
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
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '$':
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': /*    \x07*/
        case '\x08': /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': case '\x1b': case '\x1c': case '\x1d': case '\x1e': case '\x1f': {
        /* \x7f*/
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isStrangeLetterlike() const {
    
    //
    // Dump out if not a letterlike character
    //
    if (!isLetterlike()) {
        return false;
    }
    
    //
    // Using control character as letterlike is strange
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
        case ' ': case '\t': case '\v': case '\f': {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isStrangeWhitespace() const {
    
    auto val = to_point();
    
    switch (val) {
        case '\v': case '\f': {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isUpper() const {
    
    auto val = to_point();
    
    switch (val) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isDigit() const {
    
    auto val = to_point();
    
    switch (val) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isAlphaOrDigit() const {
    
    auto val = to_point();
    
    switch (val) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isHex() const {
    
    auto val = to_point();
    
    switch (val) {
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isOctal() const {
    
    auto val = to_point();
    
    switch (val) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            return true;
        }
    }
    
    return false;
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
        case '-': case '+': {
            return true;
        }
    }
    
    return false;
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
        case CODEPOINT_LINEARSYNTAX_SPACE: {
            return true;
        }
    }
    
    return false;
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
        case CODEPOINT_STRINGMETA_TAB: {
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
    
    if (val == CODEPOINT_ENDOFFILE) {
        return false;
    }
    
    if (val == CODEPOINT_ASSERTFALSE) {
        assert(false);
        return false;
    }
    
    if (val == CODEPOINT_CRLF) {
        return false;
    }
    
    if (isMBLineContinuation()) {
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
    
    if (isMBUnsafeUTF8Sequence()) {
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
    
    auto val = to_point();
    
    return !LongNames::isMBNotStrangeLetterlike(val);
}

bool WLCharacter::isMBStrangeWhitespace() const {
    
    //
    // Dump out if not a space character
    //
    if (!isMBWhitespace()) {
        return false;
    }
    
    return true;
}

bool WLCharacter::isMBStrangeNewline() const {
    
    //
    // Dump out if not a newline character
    //
    if (!isMBNewline()) {
        return false;
    }
    
    auto val = to_point();
    
    //
    // \r\n is not strange
    //
    if (val == CODEPOINT_CRLF) {
        return false;
    }
    
    //
    // FIXME: somehow supply encodingMode to test here
    //
    // \[IndentingNewLine] is not strange if coming from boxes
    //
//    if (val == CODEPOINT_LONGNAME_INDENTINGNEWLINE) {
//        if (encodingMode == ENCODINGMODE_BOX) {
//            return false;
//        }
//    }
    
    return true;
}

bool WLCharacter::isMBNewline() const {
    
    auto val = to_point();
    
    return LongNames::isMBNewline(val);
}

bool WLCharacter::isMBWhitespace() const {
    
    auto val = to_point();
    
    //
    // Handle COMPATIBILITY characters here
    //
    // Cannot use LongNames data here, because the long name \[COMPATIBILITYNoBreak] is Unsupported and
    // LongNames::isMBWhitespace(val) returns false
    //
    // But the actual *character* U+F3A2 is whitespace
    //
    // Yes, this is a bit messy
    //
    if (val == CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK) {
        return true;
    }
    
    return LongNames::isMBWhitespace(val);
}

bool WLCharacter::isMBPunctuation() const {
    
    auto val = to_point();
    
    return LongNames::isMBPunctuation(val);
}

bool WLCharacter::isMBUninterpretable() const {
    
    auto val = to_point();
    
    return LongNames::isMBUninterpretable(val);
}

bool WLCharacter::isMBLineContinuation() const {
    
    auto val = to_point();
    
    switch (val) {
        case CODEPOINT_LINECONTINUATION_LINEFEED:
        case CODEPOINT_LINECONTINUATION_CARRIAGERETURN:
        case CODEPOINT_LINECONTINUATION_CRLF: {
            return true;
        }
    }
    
    return false;
}

bool WLCharacter::isMBUnsafeUTF8Sequence() const {
    
    auto val = to_point();
    
    switch (val) {
        case CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE:
        case CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE:
        case CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE: {
            return true;
        }
    }
    
    return false;
}

char fromDigitLookup[] = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!'};

char fromDigit(uint8_t d) {
    return fromDigitLookup[d];
}

#if BUILD_TESTS
//
// For googletest
//
void PrintTo(const WLCharacter& c, std::ostream *s) {
    *s << set_graphical << c << clear_graphical;
}
#endif // BUILD_TESTS
