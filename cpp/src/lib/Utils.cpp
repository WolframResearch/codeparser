
#include "Utils.h"

#include <sstream>
#include <unordered_set>

//
// s MUST contain an integer
//
int Utils::parseInteger(std::string s, int base) {
    return std::stoi(s, nullptr, base);
}

std::string Utils::stringEscape(std::string s) {
    std::ostringstream escaped;
    escaped << "\"";
    for (size_t i = 0; i < s.size(); i++) {
        auto c = s[i];
        switch (c) {
            case '\\':
                escaped << "\\\\";
                break;
            case '"':
                escaped << "\\\"";
                break;
            default:
                escaped << c;
                break;
        }
    }
    escaped << "\"";
    
    return escaped.str();
}

//
// Convert whitespace characters and control characters to graphical representations,
// appropriate for displaying
//
std::string Utils::makeGraphical(std::string s) {
    std::ostringstream graph;
    for (size_t i = 0; i < s.size(); i++) {
        auto c = s[i];
        switch (c) {
            //
            // whitespace characters
            //
            case '\t':
                graph << "\\t";
                break;
            case '\n':
                graph << "\\n";
                break;
            case '\v':
                graph << "\\.0b";
                break;
            case '\f':
                graph << "\\.0c";
                break;
            case '\r':
                graph << "\\r";
                break;
            //
            // control characters
            //
            case 0x00:
                graph << "\\.00";
                break;
            case 0x01:
                graph << "\\.01";
                break;
            case 0x02:
                graph << "\\.02";
                break;
            case 0x03:
                graph << "\\.03";
                break;
            case 0x04:
                graph << "\\.04";
                break;
            case 0x05:
                graph << "\\.05";
                break;
            case 0x06:
                graph << "\\.06";
                break;
            case 0x07:
                graph << "\\.07";
                break;
            case 0x08:
                graph << "\\.08";
                break;
            //
            // Skip TAB, LF, VT, FF, CR. They are handled above
            //
            case 0x0e:
                graph << "\\.0e";
                break;
            case 0x0f:
                graph << "\\.0f";
                break;
            case 0x10:
                graph << "\\.10";
                break;
            case 0x11:
                graph << "\\.11";
                break;
            case 0x12:
                graph << "\\.12";
                break;
            case 0x13:
                graph << "\\.13";
                break;
            case 0x14:
                graph << "\\.14";
                break;
            case 0x15:
                graph << "\\.15";
                break;
            case 0x16:
                graph << "\\.16";
                break;
            case 0x17:
                graph << "\\.17";
                break;
            case 0x18:
                graph << "\\.18";
                break;
            case 0x19:
                graph << "\\.19";
                break;
            case 0x1a:
                graph << "\\.1a";
                break;
            case 0x1b:
                graph << "\\.1b";
                break;
            case 0x1c:
                graph << "\\.1c";
                break;
            case 0x1d:
                graph << "\\.1d";
                break;
            case 0x1e:
                graph << "\\.1e";
                break;
            case 0x01f:
                graph << "\\.1f";
                break;
            //
            // Make sure to include DEL
            //
            case 0x7f:
                graph << "\\.7f";
                break;
            //
            // everything else is untouched
            //
            default:
                graph << c;
                break;
        }
    }
    
    return graph.str();
}

bool Utils::containsNonASCII(std::string s) {
    for (auto c : s) {
        //
        // Take care to cast to int before comparing
        //
        if ((static_cast<int>(c) & 0xff) >= 0x80) {
            return true;
        }
    }
    return false;
}


//
// Completely arbitrary list of long names that would make someone pause and reconsider the code.
//
// Example: \[Alpha] = 2  is completely fine
//
// But \[SZ] = 2  is completely legal but strange
//
std::unordered_set<std::string> strangeLetterlikeLongNames {
    "DownExclamation", "Cent", "Sterling", "Currency", "Yen", "Section","DoubleDot", "Copyright", "LeftGuillemet",
        "DiscretionaryHyphen", "RegisteredTrademark",
    "Micro", "Paragraph", "Cedilla", "RightGuillemet", "DownQuestion",
        "CapitalAGrave", "CapitalAAcute", "CapitalAHat", "CapitalATilde", "CapitalADoubleDot", "CapitalARing", "CapitalAE",
        "CapitalCCedilla", "CapitalEGrave", "CapitalEAcute", "CapitalEHat", "CapitalEDoubleDot", "CapitalIGrave", "CapitalIAcute",
        "CapitalIHat", "CapitalIDoubleDot", "CapitalEth", "CapitalNTilde", "CapitalOGrave", "CapitalOAcute", "CapitalOHat", "CapitalOTilde",
        "CapitalODoubleDot", "CapitalOSlash", "CapitalUGrave", "CapitalUAcute", "CapitalUHat", "CapitalUDoubleDot", "CapitalYAcute",
        "CapitalThorn", "SZ", "AGrave", "AAcute", "AHat", "ATilde", "ADoubleDot", "ARing", "AE", "CCedilla", "EGrave", "EAcute", "EHat",
        "EDoubleDot", "IGrave", "IAcute", "IHat", "IDoubleDot", "Eth", "NTilde", "OGrave", "OAcute", "OHat", "OTilde", "ODoubleDot",
        "OSlash", "UGrave", "UAcute", "UHat", "UDoubleDot", "YAcute", "Thorn", "YDoubleDot", "CapitalABar", "ABar", "CapitalACup", "ACup",
        "CapitalCAcute", "CAcute", "CapitalCHacek", "CHacek", "CapitalDHacek", "DHacek", "CapitalEBar", "EBar", "CapitalECup", "ECup",
        "CapitalEHacek", "EHacek", "CapitalICup", "ICup",
    "CapitalLSlash", "LSlash", "CapitalNHacek", "NHacek",
        "CapitalODoubleAcute", "ODoubleAcute", "CapitalOE", "OE", "CapitalRHacek", "RHacek", "CapitalSHacek", "SHacek", "CapitalTHacek",
        "THacek", "CapitalURing", "URing", "CapitalUDoubleAcute", "UDoubleAcute", "CapitalZHacek", "ZHacek", "Florin", "Hacek", "Breve",
        "Hyphen", "Dash", "LongDash", "Dagger", "DoubleDagger", "Bullet", "Ellipsis",
    "Prime", "DoublePrime", "ReversePrime", "ReverseDoublePrime",
    "Euro", "Rupee", "Trademark", "ReturnIndicator",
    "EmptySet", "VerticalEllipsis", "CenterEllipsis", "AscendingEllipsis",
        "DescendingEllipsis", "CloverLeaf", "WatchIcon", "OverBracket", "UnderBracket", "HorizontalLine", "VerticalLine", "FilledSquare",
        "EmptySquare", "FilledVerySmallSquare", "EmptyVerySmallSquare", "FilledRectangle", "EmptyRectangle", "FilledUpTriangle",
        "EmptyUpTriangle", "UpPointer", "FilledRightTriangle", "RightPointer", "FilledDownTriangle", "EmptyDownTriangle", "DownPointer",
        "FilledLeftTriangle", "LeftPointer", "FilledDiamond", "EmptyDiamond", "EmptyCircle", "FilledCircle", "EmptySmallCircle",
        "EmptySmallSquare", "FilledSmallSquare", "FivePointedStar", "Sun", "CheckmarkedBox", "CheckedBox", "SadSmiley", "HappySmiley",
        "Moon", "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Neptune", "Pluto", "AriesSign", "TaurusSign", "GeminiSign", "CancerSign",
        "LeoSign", "VirgoSign", "LibraSign", "ScorpioSign", "SagittariusSign", "CapricornSign", "AquariusSign", "PiscesSign", "WhiteKing",
        "WhiteQueen", "WhiteRook", "WhiteBishop", "WhiteKnight", "WhitePawn", "BlackKing", "BlackQueen", "BlackRook", "BlackBishop",
        "BlackKnight", "BlackPawn", "SpadeSuit", "HeartSuit", "DiamondSuit", "ClubSuit", "QuarterNote", "EighthNote", "BeamedEighthNote",
        "BeamedSixteenthNote", "Flat", "Natural", "Sharp", "Uranus", "Checkmark", "SixPointedStar", "Shah", "WolframLanguageLogo",
        "WolframLanguageLogoCircle", "FreeformPrompt", "WolframAlphaPrompt", "Null", "AutoPlaceholder", "AutoOperand",
        "EntityStart", "EntityEnd", "SpanFromLeft", "SpanFromAbove", "SpanFromBoth", "StepperRight", "StepperLeft", "StepperUp",
        "StepperDown", "Earth", "SelectionPlaceholder", "Placeholder",
    "Wolf", "FreakedSmiley", "NeutralSmiley", "LightBulb", "NumberSign", "WarningSign", "Villa", "Akuz",
        "Andy", "Spooky",
        "FilledSmallCircle", "DottedSquare", "GraySquare", "GrayCircle", "LetterSpace", "DownBreve", "KernelIcon", "MathematicaIcon",
        "TripleDot", "SystemEnterKey", "ControlKey", "AliasDelimiter", "ReturnKey", "AliasIndicator", "EscapeKey", "CommandKey",
        "LeftModified", "RightModified",
    "TabKey",
        "SpaceKey", "DeleteKey", "AltKey", "OptionKey", "KeyBar", "EnterKey", "ShiftKey", "Mod1Key", "Mod2Key", "ConstantC",
    "GothicZero", "GothicOne", "GothicTwo",
        "GothicThree", "GothicFour", "GothicFive", "GothicSix", "GothicSeven", "GothicEight", "GothicNine", "ScriptZero", "ScriptOne",
        "ScriptTwo", "ScriptThree", "ScriptFour", "ScriptFive", "ScriptSix", "ScriptSeven", "ScriptEight", "ScriptNine", "FirstPage",
        "LastPage",
    "FiLigature", "FlLigature", "OverParenthesis", "UnderParenthesis",
        "OverBrace", "UnderBrace", "UnknownGlyph" };

//
// Long names that are in an intermediate stage of support within WL.
//
// Perhaps they have been deprecated, perhaps the Front End understands the character, but the kernel doesn't, etc.
//
std::unordered_set<std::string> unsupportedLongNames {
    "COMPATIBILITYKanjiSpace", "COMPATIBILITYNoBreak", "NumberComma" };

//
// Defined by grabbing character notebooks from Documentation/English/System/ReferencePages/Characters and comparing with existing long names
//
std::unordered_set<std::string> undocumentedLongNames {
    "Akuz", "Andy", "CheckmarkedBox", "COMPATIBILITYKanjiSpace", "COMPATIBILITYNoBreak", "ContinuedFractionK", "Curl", "Divergence",
    "DivisionSlash", "ExpectationE", "FreeformPrompt", "Gradient", "InlinePart", "KeyBar", "Laplacian", "Minus", "Mod1Key", "Mod2Key",
    "Moon", "NumberComma", "PageBreakAbove", "PageBreakBelow", "Perpendicular", "ProbabilityPr", "Rupee", "Shah", "ShiftKey",
    "Spooky", "StepperDown", "StepperLeft", "StepperRight", "StepperUp", "Sun", "UnknownGlyph", "Villa", "WolframAlphaPrompt" };



bool Utils::isStrangeLetterlikeLongName(std::string s) {
    return strangeLetterlikeLongNames.find(s) != strangeLetterlikeLongNames.end();
}

bool Utils::isUnsupportedLongName(std::string s) {
    return unsupportedLongNames.find(s) != unsupportedLongNames.end();
}

bool Utils::isUndocumentedLongName(std::string s) {
    return undocumentedLongNames.find(s) != undocumentedLongNames.end();
}

// Convert value_ to the digit that it represents
//
int Utils::toDigit(int val) {
    switch (val) {
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
int Utils::fromDigit(int d) {
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

