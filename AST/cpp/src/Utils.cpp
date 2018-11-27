
#include "Utils.h"

#include "Symbol.h"

#include <cctype>
#include <sstream>
#include <iomanip>
#include <iostream>
#include <cassert>
#include <string>
#include <memory>
#include <vector>

bool isDigit(int c) {
    return std::isdigit(c);
}

bool isAlpha(int c) {
    return std::isalpha(c);
}

bool isDigitOrAlpha(int c) {
    return std::isalnum(c);
}

bool isDigitOrAlphaOrDollar(int c) {
    return std::isalnum(c) || c == '$';
}

bool isAlphaOrDollar(int c) {
    return std::isalpha(c) || c == '$';
}

bool isHex(int c) {
    return std::isxdigit(c);
}

bool isOctal(int c) {
    return c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' || c == '7';
}

bool isSpace(int c) {
    return std::isspace(c);
}

bool isControl(int c) {
    return iscntrl(c);
}

int parseInteger(std::string s, int base) {
    return std::stoi(s, nullptr, base);
}

std::string stringEscape(std::string s) {
    std::ostringstream escaped;
    escaped << "\"";
    for (size_t i = 0; i < s.size(); i++) {
        auto c = s[i];
        //
        // Handle " and \ specially
        //
        // and then handle \b, \f, \n, \r, \t
        //
        switch (c) {
            case '\\':
                escaped << "\\\\";
                break;
            case '"':
                escaped << "\\\"";
                break;
            case '\b':
                escaped << "\\b";
                break;
            case '\f':
                escaped << "\\f";
                break;
            case '\n':
                escaped << "\\n";
                break;
            case '\r':
                escaped << "\\r";
                break;
            case '\t':
                escaped << "\\t";
                break;
            default:
                escaped << c;
                break;
        }
    }
    escaped << "\"";
    
    return escaped.str();
}

// Convert the character c into the digit that it represents
//
int toBaseDigit(int c) {
    switch (c) {
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


std::string ASTSourceString(SourceSpan span) {
    std::ostringstream ss;
    ss << SYMBOL_SOURCE->name();
    ss << "->{";
    ss << span.start.string();
    ss << ", ";
    ss << span.end.string();
    ss << "}";
    return ss.str();
}
