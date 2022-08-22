(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`Parselet`"]

PrefixOperatorToParselet

InfixOperatorToParselet


Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeParser`Generate`TokenEnum`"]; (* for tokens *)
Needs["CodeParser`Generate`Common`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


checkBuildDir[]
checkSrcDir[]


KeyValueMap[
  Function[{k, v},
    PrefixOperatorToParselet[k] = v;
  ]
  ,
  importedPrefixParselets
]

PrefixOperatorToParselet[_] = Parselet`PrefixUnhandledParselet[]



KeyValueMap[
  Function[{k, v},
    InfixOperatorToParselet[k] = v;
  ]
  ,
  importedInfixParselets
]

InfixOperatorToParselet[_] = Parselet`InfixImplicitTimesParselet[]



tokensSansCount = DeleteCases[tokens, Token`Count]



formatPrefix[Parselet`PrefixNullPointerParselet[]] := "nullptr"

formatPrefix[Parselet`PrefixEndOfFileParselet[]] := "&prefixEndOfFileParselet"

formatPrefix[Parselet`PrefixErrorParselet[]] := "&prefixErrorParselet"

formatPrefix[Parselet`PrefixCloserParselet[]] := "&prefixCloserParselet"

formatPrefix[Parselet`PrefixUnsupportedTokenParselet[]] := "&prefixUnsupportedTokenParselet"

formatPrefix[Parselet`PrefixUnhandledParselet[]] := "&prefixUnhandledParselet"

formatPrefix[Parselet`PrefixCommaParselet[]] := "&prefixCommaParselet"

formatPrefix[Parselet`LeafParselet[]] := "&leafParselet"

formatPrefix[Parselet`SymbolParselet[]] := "symbolParselet"

formatPrefix[Parselet`UnderParselet[1]] := "under1Parselet"

formatPrefix[Parselet`UnderParselet[2]] := "under2Parselet"

formatPrefix[Parselet`UnderParselet[3]] := "under3Parselet"

formatPrefix[Parselet`UnderDotParselet[]] := "underDotParselet"

formatPrefix[Parselet`HashParselet[]] := "new HashParselet()"

formatPrefix[Parselet`HashHashParselet[]] := "new HashHashParselet()"

formatPrefix[Parselet`PercentParselet[]] := "new PercentParselet()"

formatPrefix[Parselet`LessLessParselet[]] := "new LessLessParselet()"

formatPrefix[Parselet`SemiSemiParselet[]] := "&semiSemiParselet"

formatPrefix[Parselet`IntegralParselet[]] := "new IntegralParselet()"

formatPrefix[Parselet`PrefixOperatorParselet[tok_, precedence_, op_]] := "new PrefixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatPrefix[Parselet`GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]] := "&squareGroupParselet"

formatPrefix[Parselet`GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]] := "&doubleBracketGroupParselet"

formatPrefix[Parselet`GroupParselet[tok_, op_]] := "new GroupParselet(" <> toGlobal[tok] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"


formatInfix[Parselet`InfixNullPointerParselet[]] := "nullptr"

formatInfix[Parselet`InfixAssertFalseParselet[]] := "&infixAssertFalseParselet"

formatInfix[Parselet`InfixImplicitTimesParselet[]] := "&infixImplicitTimesParselet"



formatInfix[Parselet`BinaryOperatorParselet[tok_, precedence_, op_]] := "new BinaryOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`InfixOperatorParselet[tok_, precedence_, op_]] := "new InfixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`PostfixOperatorParselet[tok_, precedence_, op_]] := "new PostfixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`ColonParselet[]] := "colonParselet"

formatInfix[Parselet`CallParselet[groupParselet_]] := "new CallParselet(" <> formatPrefix[groupParselet] <> ")"

formatInfix[Parselet`EqualParselet[]] := "equalParselet"

formatInfix[Parselet`ColonEqualParselet[]] := "colonEqualParselet"

formatInfix[Parselet`TildeParselet[]] := "new TildeParselet()"

formatInfix[Parselet`SlashColonParselet[]] := "slashColonParselet"

formatInfix[Parselet`CommaParselet[]] := "&commaParselet"

formatInfix[Parselet`SemiParselet[]] := "&semiParselet"

formatInfix[Parselet`SemiSemiParselet[]] := "&semiSemiParselet"

formatInfix[Parselet`ColonColonParselet[]] := "new ColonColonParselet()"

formatInfix[Parselet`GreaterGreaterParselet[]] := "new GreaterGreaterParselet()"

formatInfix[Parselet`GreaterGreaterGreaterParselet[]] := "new GreaterGreaterGreaterParselet()"

formatInfix[Parselet`InfixDifferentialDParselet[]] := "&infixDifferentialDParselet"

formatInfix[Parselet`InfixToplevelNewlineParselet[]] := "new InfixToplevelNewlineParselet()"

formatInfix[Parselet`TimesParselet[]] := "timesParselet"


generate[] := (

Print["Generating Parselet..."];

parseletRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"TokenEnumRegistration.h\" // for TOKEN_COUNT

#include <array>

class PrefixParselet;
class InfixParselet;
class SymbolParselet;
class UnderParselet;
class UnderDotParselet;
class ColonParselet;
class SlashColonParselet;
class EqualParselet;
class ColonEqualParselet;
class PrefixToplevelCloserParselet;
class TimesParselet;

using PrefixParseletPtr = PrefixParselet *;
using InfixParseletPtr = InfixParselet *;


extern std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets;
extern std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets;

extern SymbolParselet *symbolParselet;
extern UnderParselet *under1Parselet;
extern UnderParselet *under2Parselet;
extern UnderParselet *under3Parselet;
extern UnderDotParselet *underDotParselet;
extern ColonParselet *colonParselet;
extern SlashColonParselet *slashColonParselet;
extern EqualParselet *equalParselet;
extern ColonEqualParselet *colonEqualParselet;
extern PrefixToplevelCloserParselet *prefixToplevelCloserParselet;
extern TimesParselet *timesParselet;
"};

Print["exporting ParseletRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "ParseletRegistration.h"}], Column[parseletRegistrationCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];


parseletRegistrationCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"ParseletRegistration.h\"

#include \"Parselet.h\" // for SymbolParselet, UnderParselet, etc.
#include \"ByteDecoder.h\" // for TheByteDecoder
#include \"SymbolRegistration.h\"

#include <cassert>

SymbolParselet *symbolParselet = new SymbolParselet();

auto leafParselet = LeafParselet();

auto prefixEndOfFileParselet = PrefixEndOfFileParselet();

auto prefixErrorParselet = PrefixErrorParselet();

auto prefixCloserParselet = PrefixCloserParselet();

PrefixToplevelCloserParselet *prefixToplevelCloserParselet = new PrefixToplevelCloserParselet();

auto prefixUnsupportedTokenParselet = PrefixUnsupportedTokenParselet();

auto prefixUnhandledParselet = PrefixUnhandledParselet();

auto prefixCommaParselet = PrefixCommaParselet();

auto infixAssertFalseParselet = InfixAssertFalseParselet();

auto infixImplicitTimesParselet = InfixImplicitTimesParselet();

auto commaParselet = CommaParselet();

auto semiParselet = SemiParselet();

auto semiSemiParselet = SemiSemiParselet();

SlashColonParselet *slashColonParselet = new SlashColonParselet();

ColonParselet *colonParselet = new ColonParselet();

EqualParselet *equalParselet = new EqualParselet();

ColonEqualParselet *colonEqualParselet = new ColonEqualParselet();

auto infixDifferentialDParselet = InfixDifferentialDParselet();

UnderParselet *under1Parselet = new UnderParselet(SYMBOL_BLANK, SYMBOL_CODEPARSER_PATTERNBLANK);

UnderParselet *under2Parselet = new UnderParselet(SYMBOL_BLANKSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE);

UnderParselet *under3Parselet = new UnderParselet(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE);

UnderDotParselet *underDotParselet = new UnderDotParselet();

auto squareGroupParselet = GroupParselet(TOKEN_OPENSQUARE, SYMBOL_CODEPARSER_GROUPSQUARE);

auto doubleBracketGroupParselet = GroupParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET);

TimesParselet *timesParselet = new TimesParselet();

//
//
//
std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets {{"} ~Join~

(Row[{"  ", formatPrefix[PrefixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"}};

//
//
//
std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets {{"} ~Join~

(Row[{"  ", formatInfix[InfixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"}};
"};

Print["exporting ParseletRegistration.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "ParseletRegistration.cpp"}], Column[parseletRegistrationCPPSource], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done Parselet"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]

