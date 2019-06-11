
#include "Utils.h"

#include "Parser.h"

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
// Convert whitespace Source characters and control Source characters to graphical representations,
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
            case '\t': graph << "\\t"; break;
            case '\n': graph << "\\n"; break;
            case '\v': graph << "\\.0b"; break;
            case '\f': graph << "\\.0c"; break;
            case '\r': graph << "\\r"; break;
            //
            // C0 control characters
            //
            case 0x00: graph << "\\.00"; break;
            case 0x01: graph << "\\.01"; break;
            case 0x02: graph << "\\.02"; break;
            case 0x03: graph << "\\.03"; break;
            case 0x04: graph << "\\.04"; break;
            case 0x05: graph << "\\.05"; break;
            case 0x06: graph << "\\.06"; break;
            case 0x07: graph << "\\.07"; break;
            case 0x08: graph << "\\.08"; break;
            //
            // Skip TAB, LF, VT, FF, CR. They are handled above
            //
            case 0x0e: graph << "\\.0e"; break;
            case 0x0f: graph << "\\.0f"; break;
            case 0x10: graph << "\\.10"; break;
            case 0x11: graph << "\\.11"; break;
            case 0x12: graph << "\\.12"; break;
            case 0x13: graph << "\\.13"; break;
            case 0x14: graph << "\\.14"; break;
            case 0x15: graph << "\\.15"; break;
            case 0x16: graph << "\\.16"; break;
            case 0x17: graph << "\\.17"; break;
            case 0x18: graph << "\\.18"; break;
            case 0x19: graph << "\\.19"; break;
            case 0x1a: graph << "\\.1a"; break;
            case 0x1b: graph << "\\.1b"; break;
            case 0x1c: graph << "\\.1c"; break;
            case 0x1d: graph << "\\.1d"; break;
            case 0x1e: graph << "\\.1e"; break;
            case 0x1f: graph << "\\.1f"; break;
            //
            // Make sure to include DEL
            //
            case 0x7f: graph << "\\.7f"; break;
            //
            // C1 control characters
            //
            case -0x80: graph << "\\.80"; break;
            case -0x7f: graph << "\\.81"; break;
            case -0x7e: graph << "\\.82"; break;
            case -0x7d: graph << "\\.83"; break;
            case -0x7c: graph << "\\.84"; break;
            case -0x7b: graph << "\\.85"; break;
            case -0x7a: graph << "\\.86"; break;
            case -0x79: graph << "\\.87"; break;
            case -0x78: graph << "\\.88"; break;
            case -0x77: graph << "\\.89"; break;
            case -0x76: graph << "\\.8a"; break;
            case -0x75: graph << "\\.8b"; break;
            case -0x74: graph << "\\.8c"; break;
            case -0x73: graph << "\\.8d"; break;
            case -0x72: graph << "\\.8e"; break;
            case -0x71: graph << "\\.8f"; break;
            case -0x70: graph << "\\.90"; break;
            case -0x6f: graph << "\\.91"; break;
            case -0x6e: graph << "\\.92"; break;
            case -0x6d: graph << "\\.93"; break;
            case -0x6c: graph << "\\.94"; break;
            case -0x6b: graph << "\\.95"; break;
            case -0x6a: graph << "\\.96"; break;
            case -0x69: graph << "\\.97"; break;
            case -0x68: graph << "\\.98"; break;
            case -0x67: graph << "\\.99"; break;
            case -0x66: graph << "\\.9a"; break;
            case -0x65: graph << "\\.9b"; break;
            case -0x64: graph << "\\.9c"; break;
            case -0x63: graph << "\\.9d"; break;
            case -0x62: graph << "\\.9e"; break;
            case -0x61: graph << "\\.9f"; break;
            
            //
            // everything else is untouched
            //
            default: graph << c; break;
        }
    }
    
    return graph.str();
}

bool Utils::containsOnlyASCII(std::string s) {
    for (auto c : s) {
        //
        // Take care to cast to int before comparing
        //
        if ((static_cast<int>(c) & 0xff) >= 0x80) {
            return false;
        }
    }
    return true;
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



Token Utils::eatAll(Token TokIn, ParserContext Ctxt, NodeSeq& Args) {
    
    auto Tok = TokIn;
    
    while (Tok.Tok == TOKEN_WHITESPACE ||
           Tok.Tok == TOKEN_NEWLINE ||
           Tok.Tok == TOKEN_COMMENT) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        Args.push_back(std::make_shared<LeafNode>(Tok));
        
        Tok = TheParser->nextToken(Ctxt);
    }
    
    return Tok;
}

Token Utils::eatAndPreserveToplevelNewlines(Token TokIn, ParserContext Ctxt, NodeSeq& Args) {
    
    auto Tok = TokIn;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        if (Tok.Tok == TOKEN_WHITESPACE ||
            Tok.Tok == TOKEN_COMMENT) {
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt);
            
        } else if (Tok.Tok == TOKEN_NEWLINE) {
            
            if (Ctxt.getGroupDepth() == 0) {
                
                break;
                
            } else {
                
                Args.push_back(std::make_shared<LeafNode>(Tok));
                
                Tok = TheParser->nextToken(Ctxt);
            }
            
        } else {
            break;
        }
    }
    
    return Tok;
}

void Utils::differentLineWarning(Token Tok1, Token Tok2, SyntaxIssueSeverity Severity) {
    
    if (Tok1.Tok == TOKEN_ERROR_ABORTED) {
        return;
    }
    if (Tok2.Tok == TOKEN_ERROR_ABORTED) {
        return;
    }
    
    //
    // Skip DifferentLine issues if ENDOFFILE
    //
    if (Tok1.Tok == TOKEN_ENDOFFILE) {
        return;
    }
    if (Tok2.Tok == TOKEN_ENDOFFILE) {
        return;
    }
    
    if (Tok1.Span.lines.end.Line == Tok2.Span.lines.start.Line) {
        return;
    }
    
    auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``" + Tok1.Str + "`` and ``" + Tok2.Str + "`` are on different lines.", Severity, Source(Tok1.Span.lines.start, Tok2.Span.lines.end));
    
    TheParser->addIssue(Issue);
}

Token Utils::lastToken(NodePtr Node) {
    
    if (auto Leaf = std::dynamic_pointer_cast<const LeafNode>(Node)) {
        
        return Leaf->getToken();
    }
    
    auto Children = Node->getChildren();
    auto LastNode = Children[Children.size()-1];
    
    return Utils::lastToken(LastNode);
}

void Utils::differentLineWarning(NodeSeq Args, Token Tok2, SyntaxIssueSeverity Severity) {
    
    auto Arg1 = Args.main();
    
    auto Tok1 = Utils::lastToken(Arg1);
    
    Utils::differentLineWarning(Tok1, Tok2, Severity);
}

void Utils::endOfLineWarning(Token Tok, Token EndTok) {
    
    if (Tok.Tok == TOKEN_ERROR_ABORTED) {
        return;
    }
    if (EndTok.Tok == TOKEN_ERROR_ABORTED) {
        return;
    }
    
    if (EndTok.Tok != TOKEN_NEWLINE && EndTok.Tok != TOKEN_ENDOFFILE) {
        return;
    }
    
    auto Issue = SyntaxIssue(SYNTAXISSUETAG_ENDOFLINE, "``;;`` is at the end of a line. Did you mean ``;``?", SYNTAXISSUESEVERITY_WARNING, Tok.Span);
    
    TheParser->addIssue(Issue);
}

void Utils::notContiguousWarning(Token Tok1, Token Tok2) {
    
    if (Tok1.Tok == TOKEN_ERROR_ABORTED) {
        return;
    }
    if (Tok2.Tok == TOKEN_ERROR_ABORTED) {
        return;
    }
    
    if (isContiguous(Tok1.Span, Tok2.Span)) {
        return;
    }
    
    auto Issue = SyntaxIssue(SYNTAXISSUETAG_NOTCONTIGUOUS, std::string("Tokens are not contiguous"), SYNTAXISSUESEVERITY_FORMATTING, Source(Tok1.Span.lines.end, Tok2.Span.lines.start));
    
    TheParser->addIssue(Issue);
}







TimeScoper::TimeScoper(std::chrono::microseconds *acc) : acc(acc) {
    t1 = std::chrono::high_resolution_clock::now();
};

TimeScoper::~TimeScoper() {
    
    auto t2 = std::chrono::high_resolution_clock::now();
    
    std::chrono::microseconds span = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1);
    
    *acc += span;
}


