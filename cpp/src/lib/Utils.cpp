#include "Utils.h"

#include "Parser.h"
#include "Tokenizer.h"

#include <sstream>
#include <unordered_set>
#include <cstring> // for strcmp with GCC and MSVC
#include <cassert>

//
// s MUST contain an integer
//
int Utils::parseInteger(std::string s, int base) {
    return std::stoi(s, nullptr, base);
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






void Utils::differentLineWarning(Token Tok1, Token Tok2) {
    
    if (Tok1.Tok() == TOKEN_ERROR_ABORTED) {
        return;
    }
    if (Tok2.Tok() == TOKEN_ERROR_ABORTED) {
        return;
    }
    
    //
    // Skip DifferentLine issues if ENDOFFILE
    //
    if (Tok1.Tok() == TOKEN_ENDOFFILE) {
        return;
    }
    if (Tok2.Tok() == TOKEN_ENDOFFILE) {
        return;
    }
    
    if (Tok2.Tok() == TOKEN_ERROR_EMPTYSTRING) {
        return;
    }
    
    //
    // Only check if LineCol
    //
    if (Tok1.Src.style != SOURCESTYLE_LINECOL) {
        return;
    }
    
    if (Tok1.Src.lineCol.end.Line == Tok2.Src.lineCol.start.Line) {
        return;
    }
    
    std::vector<CodeActionPtr> Actions;
    Actions.push_back(CodeActionPtr(new DeleteTriviaCodeAction("Delete newline", Source(Tok1.Src, Tok2.Src))));
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_DIFFERENTLINE, "``" + Tok1.Str + "`` and ``" + Tok2.Str + "`` are on different lines.", SYNTAXISSUESEVERITY_WARNING, Source(Tok1.Src, Tok2.Src), 0.75, std::move(Actions)));
    
    TheParser->addIssue(std::move(I));
}

void Utils::differentLineWarning(NodeSeq& Args, Token Tok2) {
    
    auto F = Args.first();
    
    auto Tok1 = F->lastToken();
    
    Utils::differentLineWarning(Tok1, Tok2);
}

void Utils::endOfLineWarning(Token Tok, Token EndTok) {
    
    if (Tok.Tok() == TOKEN_ERROR_ABORTED) {
        return;
    }
    if (EndTok.Tok() == TOKEN_ERROR_ABORTED) {
        return;
    }
    
    if (EndTok.Tok() != TOKEN_NEWLINE && EndTok.Tok() != TOKEN_ENDOFFILE) {
        return;
    }
    
    //
    // Only check if LineCol
    //
    if (Tok.Src.style != SOURCESTYLE_LINECOL) {
        return;
    }
    
    std::vector<CodeActionPtr> Actions;
    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``;;`` with ``;``.", Source(Tok.Src), ";")));
    
    auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_ENDOFLINE, "``" + Tok.Str + "`` is at the end of the line.", SYNTAXISSUESEVERITY_REMARK, Tok.Src, 0.80, std::move(Actions)));
    
    TheParser->addIssue(std::move(I));
}

void Utils::notContiguousWarning(Token Tok1, Token Tok2) {
    
    if (Tok1.Tok() == TOKEN_ERROR_ABORTED) {
        return;
    }
    if (Tok2.Tok() == TOKEN_ERROR_ABORTED) {
        return;
    }
    
    //
    // Only check if LineCol or OffsetLen
    //
    if (!(Tok1.Src.style == SOURCESTYLE_LINECOL ||
         Tok1.Src.style == SOURCESTYLE_OFFSETLEN)) {
        return;
    }
    
    if (isContiguous(Tok1.Src, Tok2.Src)) {
        return;
    }
    
    std::vector<CodeActionPtr> Actions;
    Actions.push_back(CodeActionPtr(new DeleteTriviaCodeAction("Delete trivia", Source(Tok1.Src, Tok2.Src))));
    
    auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_NOTCONTIGUOUS, std::string("Tokens are not contiguous."), FORMATISSUESEVERITY_FORMATTING, Source(Tok1.Src, Tok2.Src), 1.0, std::move(Actions)));
    
    TheParser->addIssue(std::move(I));
}

void Utils::strangeLetterlikeWarning(WLCharacter c) {
    
    assert(c.isStrangeLetterlike() || c.isStrangeLetterlikeCharacter());
    
    if (c.isVeryStrangeLetterlike() || c.isVeryStrangeLetterlikeCharacter()) {
        
        auto Src = TheCharacterDecoder->getWLCharacterSource();
        
        auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.95, {}));
        
        TheTokenizer->addIssue(std::move(I));
        
        return;
    }
    
    auto Src = TheCharacterDecoder->getWLCharacterSource();
    
    auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + c.graphicalString() + "``.", SYNTAXISSUESEVERITY_WARNING, Src, 0.90, {}));
    
    TheTokenizer->addIssue(std::move(I));
}




bool Utils::parseBooleanSymbol(const char * sym) {
    
    if (strcmp(sym, SYMBOL_TRUE->name()) == 0) {
        return true;
    }
    
    return false;
}

SourceStyle Utils::parseSourceStyle(const char *str) {
    
    if (strcmp(str, "LineCol") == 0) {
        return SOURCESTYLE_LINECOL;
    } else if (strcmp(str, "OffsetLen") == 0) {
        return SOURCESTYLE_OFFSETLEN;
    }
    
    return SOURCESTYLE_UNKNOWN;
}




