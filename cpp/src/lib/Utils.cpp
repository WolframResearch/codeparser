
#include "Utils.h"

#include "Tokenizer.h" // for Tokenizer
#include "CharacterMaps.h" // for CodePointToLongNameMap

#include <cstring> // for strcmp with GCC and MSVC
#include <cassert>


//
// s MUST contain an integer
//
// Don't be weird: return an int, not size_t
//
int Utils::parseInteger(std::string s, size_t base) {
    return std::stoi(s, nullptr, static_cast<int>(base));
}


//
// These have been downgraded from very strange to just strange, because they do appear in actual code
//
std::unordered_set<std::string> strangeLetterlikeLongNames {
    "Placeholder", "Checkmark", "SpanFromLeft" };

//
// Completely arbitrary list of long names that would make someone pause and reconsider the code.
//
// Example: \[Alpha] = 2  is completely fine
//
// But \[SZ] = 2  is completely legal but strange
//
std::unordered_set<std::string> veryStrangeLetterlikeLongNames {
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
    "VerticalEllipsis", "CenterEllipsis", "AscendingEllipsis",
    "DescendingEllipsis", "CloverLeaf", "WatchIcon", "OverBracket", "UnderBracket", "HorizontalLine", "VerticalLine", "FilledSquare",
    "EmptySquare", "FilledVerySmallSquare", "EmptyVerySmallSquare", "FilledRectangle", "EmptyRectangle", "FilledUpTriangle",
    "EmptyUpTriangle", "UpPointer", "FilledRightTriangle", "RightPointer", "FilledDownTriangle", "EmptyDownTriangle", "DownPointer",
    "FilledLeftTriangle", "LeftPointer", "FilledDiamond", "EmptyDiamond", "EmptyCircle", "FilledCircle", "EmptySmallCircle",
    "EmptySmallSquare", "FilledSmallSquare", "FivePointedStar", "Sun", "CheckmarkedBox", "CheckedBox", "SadSmiley", "HappySmiley",
    "Moon", "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Neptune", "Pluto", "AriesSign", "TaurusSign", "GeminiSign", "CancerSign",
    "LeoSign", "VirgoSign", "LibraSign", "ScorpioSign", "SagittariusSign", "CapricornSign", "AquariusSign", "PiscesSign", "WhiteKing",
    "WhiteQueen", "WhiteRook", "WhiteBishop", "WhiteKnight", "WhitePawn", "BlackKing", "BlackQueen", "BlackRook", "BlackBishop",
    "BlackKnight", "BlackPawn", "SpadeSuit", "HeartSuit", "DiamondSuit", "ClubSuit", "QuarterNote", "EighthNote", "BeamedEighthNote",
    "BeamedSixteenthNote", "Flat", "Natural", "Sharp", "Uranus",
//    "Checkmark",
    "SixPointedStar", "Shah", "WolframLanguageLogo",
    "WolframLanguageLogoCircle", "FreeformPrompt", "WolframAlphaPrompt", "Null", "AutoPlaceholder", "AutoOperand",
    "EntityStart", "EntityEnd",
//    "SpanFromLeft",
    "SpanFromAbove", "SpanFromBoth", "StepperRight", "StepperLeft", "StepperUp",
    "StepperDown", "Earth", "SelectionPlaceholder",
//    "Placeholder",
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

bool Utils::isStrangeLetterlikeLongName(std::string s) {
    return strangeLetterlikeLongNames.find(s) != strangeLetterlikeLongNames.end();
}

bool Utils::isVeryStrangeLetterlikeLongName(std::string s) {
    return veryStrangeLetterlikeLongNames.find(s) != strangeLetterlikeLongNames.end();
}


std::unordered_set<std::string> undocumentedLongNames;

bool Utils::isUndocumentedLongName(std::string s) {
    return undocumentedLongNames.find(s) != undocumentedLongNames.end();
}


#if !NISSUES
void Utils::strangeLetterlikeWarning(Source Src, WLCharacter c) {
    
    assert(c.isStrangeLetterlike() || c.isMBStrangeLetterlike());
    
    if (c.isVeryStrangeLetterlike() || c.isMBVeryStrangeLetterlike()) {
        
        auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected letterlike character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        TheTokenizer->addIssue(std::move(I));
        
        return;
    }
    
    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected letterlike character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.90, {}));
    
    TheTokenizer->addIssue(std::move(I));
}

bool Utils::isStrange(int32_t point) {

    switch (point) {
            //
            // C0 control characters
            //
            // Skipping LF, CR, TAB, and ESC
            //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': case '\x07':
        case '\x08': /*    \x09*/ /*    \x0a*/ case '\x0b': case '\x0c': /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': /*    \x1b*/ case '\x1c': case '\x1d': case '\x1e': case '\x1f':
            //
            // Make sure to include DEL
            //
        case '\x7f':
            return true;
        default:
            return false;
    }
}

bool Utils::isMBStrange(int32_t point) {

    //
    // Reject if ASCII, should use isStrange()
    //
    if ((0x00 <= point && point <= 0x7f)) {
        return false;
    }

    //
    // Individual characters
    //
    switch (point) {
            //
            // ZERO WIDTH SPACE
            //
        case 0x200b:
            return true;
            //
            // ZERO WIDTH NON-JOINER
            //
        case 0x200c:
            return true;
            //
            // ZERO WIDTH JOINER
            //
        case 0x200d:
            return true;
            //
            // LINE SEPARATOR
            //
//        case 0x2028:
//            return true;
            //
            // WORD JOINER
            //
            // This is the character that is recommended to use for ZERO WIDTH NON-BREAKING SPACE
            // https://unicode.org/faq/utf_bom.html#bom6
            //
//        case 0x2060:
//            return true;
            //
            // FUNCTION APPLICATION
            //
        case 0x2061:
            return true;
            //
            // ZERO WIDTH NO-BREAK SPACE
            //
            // But most likely BOM
            //
        case 0xfeff:
            //
            // Do not assert(false)
            //
            // \:feff is completely fine
            // The problem with 0xfeff is when it is a source character
            //
            return true;
            //
            // ZERO WIDTH NO-BREAK SPACE
            //
            // also BOM
            //
        case 0xe001:
            return true;
            //
            // REPLACEMENT CHARACTER
            //
            // This can be the result of badly encoded UTF-8
            //
        case 0xfffd:
            return true;
    }

    //
    // C1
    //
    if (0x0080 <= point && point <= 0x009f) {
        return true;
    }

    //
    // High surrogates
    //
    if (0xd800 <= point && point <= 0xdbff) {
        return true;
    }

    //
    // Low surrogates
    //
    if (0xdc00 <= point && point <= 0xdfff) {
        return true;
    }

    //
    // BMP PUA
    //

    //
    // Disable checking BMP PUA for now
    //
    // There are a lot of WL-specific characters in the BMP PUA
    //

//    if (0xe000 <= val && val <= 0xf8ff) {
//        return true;
//    }

    //
    // Plane 15 PUA
    //
    if (0xf0000 <= point && point <= 0xffffd) {
        return true;
    }

    //
    // Plane 16 PUA
    //
    if (0x100000 <= point && point <= 0x10fffd) {
        return true;
    }

    if (Utils::isMBNonCharacter(point)) {
        return true;
    }

    return false;
}
#endif // NISSUES

//
// https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
//
bool Utils::isMBNonCharacter(int32_t point) {
    
    switch (point) {
        case 0xfdd0: case 0xfdd1: case 0xfdd2: case 0xfdd3: case 0xfdd4: case 0xfdd5: case 0xfdd6: case 0xfdd7:
        case 0xfdd8: case 0xfdd9: case 0xfdda: case 0xfddb: case 0xfddc: case 0xfddd: case 0xfdde: case 0xfddf:
        case 0xfde0: case 0xfde1: case 0xfde2: case 0xfde3: case 0xfde4: case 0xfde5: case 0xfde6: case 0xfde7:
        case 0xfde8: case 0xfde9: case 0xfdea: case 0xfdeb: case 0xfdec: case 0xfded: case 0xfdee: case 0xfdef:
        case 0x0fffe: case 0x0ffff:
        case 0x1fffe: case 0x1ffff:
        case 0x2fffe: case 0x2ffff:
        case 0x3fffe: case 0x3ffff:
        case 0x4fffe: case 0x4ffff:
        case 0x5fffe: case 0x5ffff:
        case 0x6fffe: case 0x6ffff:
        case 0x7fffe: case 0x7ffff:
        case 0x8fffe: case 0x8ffff:
        case 0x9fffe: case 0x9ffff:
        case 0xafffe: case 0xaffff:
        case 0xbfffe: case 0xbffff:
        case 0xcfffe: case 0xcffff:
        case 0xdfffe: case 0xdffff:
        case 0xefffe: case 0xeffff:
        case 0xffffe: case 0xfffff:
        case 0x10fffe: case 0x10ffff:
            return true;
        default:
            return false;
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


