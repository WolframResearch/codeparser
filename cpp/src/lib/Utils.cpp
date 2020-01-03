
#include "Utils.h"

#include "Tokenizer.h" // for Tokenizer

#include <unordered_set> // for unordered_set
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

//
// Long names that are in an intermediate stage of support within WL.
//
// Perhaps they have been deprecated, perhaps the Front End understands the character but the kernel doesn't, etc.
//
// FIXME: all of these should be kernel quirks?
//
std::unordered_set<std::string> unsupportedLongNames {
    "COMPATIBILITYKanjiSpace", "COMPATIBILITYNoBreak", "NumberComma", "InlinePart" };

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

bool Utils::isVeryStrangeLetterlikeLongName(std::string s) {
    return veryStrangeLetterlikeLongNames.find(s) != strangeLetterlikeLongNames.end();
}

bool Utils::isUnsupportedLongName(std::string s) {
    return unsupportedLongNames.find(s) != unsupportedLongNames.end();
}

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
#endif // !NISSUES

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
