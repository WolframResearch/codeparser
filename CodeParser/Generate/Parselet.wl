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



formatPrefix[Parselet`PrefixEndOfFileParselet[]] := "prefixEndOfFileParselet"

formatPrefix[Parselet`PrefixErrorParselet[]] := "prefixErrorParselet"

formatPrefix[Parselet`PrefixCloserParselet[]] := "prefixCloserParselet"

formatPrefix[Parselet`PrefixUnsupportedTokenParselet[]] := "prefixUnsupportedTokenParselet"

formatPrefix[Parselet`PrefixUnhandledParselet[]] := "prefixUnhandledParselet"

formatPrefix[Parselet`PrefixCommaParselet[]] := "prefixCommaParselet"

formatPrefix[Parselet`LeafParselet[]] := "leafParselet"

formatPrefix[Parselet`SymbolParselet[]] := "symbolParselet"

formatPrefix[Parselet`UnderParselet[1]] := "under1Parselet"

formatPrefix[Parselet`UnderParselet[2]] := "under2Parselet"

formatPrefix[Parselet`UnderParselet[3]] := "under3Parselet"

formatPrefix[Parselet`UnderDotParselet[]] := "underDotParselet"

formatPrefix[Parselet`HashParselet[]] := "new HashParselet()"

formatPrefix[Parselet`HashHashParselet[]] := "new HashHashParselet()"

formatPrefix[Parselet`PercentParselet[]] := "new PercentParselet()"

formatPrefix[Parselet`LessLessParselet[]] := "new LessLessParselet()"

formatPrefix[Parselet`SemiSemiParselet[]] := "semiSemiParselet"

formatPrefix[Parselet`IntegralParselet[op1_, op2_]] := "new IntegralParselet(" <> "SYMBOL_" <> toGlobal[op1] <> ", " <> "SYMBOL_" <> toGlobal[op2] <> ")"

formatPrefix[Parselet`PrefixOperatorParselet[precedence_, op_]] := "new PrefixOperatorParselet(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatPrefix[Parselet`GroupParselet[tok_, op_]] := "new GroupParselet(" <> toGlobal[tok] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"


formatInfix[Parselet`InfixAssertFalseParselet[]] := "infixAssertFalseParselet"

formatInfix[Parselet`InfixImplicitTimesParselet[]] := "infixImplicitTimesParselet"



formatInfix[Parselet`BinaryOperatorParselet[precedence_, op_]] := "new BinaryOperatorParselet(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`InfixOperatorParselet[precedence_, op_]] := "new InfixOperatorParselet(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`PostfixOperatorParselet[precedence_, op_]] := "new PostfixOperatorParselet(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`ColonParselet[]] := "colonParselet"

formatInfix[Parselet`CallParselet[groupParselet_]] := "new CallParselet(" <> formatPrefix[groupParselet] <> ")"

formatInfix[Parselet`EqualParselet[]] := "equalParselet"

formatInfix[Parselet`ColonEqualParselet[]] := "colonEqualParselet"

formatInfix[Parselet`TildeParselet[]] := "new TildeParselet()"

formatInfix[Parselet`SlashColonParselet[]] := "slashColonParselet"

formatInfix[Parselet`CommaParselet[]] := "commaParselet"

formatInfix[Parselet`SemiParselet[]] := "semiParselet"

formatInfix[Parselet`SemiSemiParselet[]] := "semiSemiParselet"

formatInfix[Parselet`ColonColonParselet[]] := "new ColonColonParselet()"

formatInfix[Parselet`GreaterGreaterParselet[]] := "new GreaterGreaterParselet()"

formatInfix[Parselet`GreaterGreaterGreaterParselet[]] := "new GreaterGreaterGreaterParselet()"

formatInfix[Parselet`InfixDifferentialDParselet[]] := "infixDifferentialDParselet"

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


SymbolParselet *symbolParselet = new SymbolParselet();

UnderParselet *under1Parselet = new UnderParselet(SYMBOL_BLANK, SYMBOL_CODEPARSER_PATTERNBLANK);

UnderParselet *under2Parselet = new UnderParselet(SYMBOL_BLANKSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE);

UnderParselet *under3Parselet = new UnderParselet(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE);

UnderDotParselet *underDotParselet = new UnderDotParselet();

ColonParselet *colonParselet = new ColonParselet();

SlashColonParselet *slashColonParselet = new SlashColonParselet();

EqualParselet *equalParselet = new EqualParselet();

ColonEqualParselet *colonEqualParselet = new ColonEqualParselet();

PrefixToplevelCloserParselet *prefixToplevelCloserParselet = new PrefixToplevelCloserParselet();

TimesParselet *timesParselet = new TimesParselet();


auto leafParselet = new LeafParselet();

auto prefixEndOfFileParselet = new PrefixEndOfFileParselet();

auto prefixErrorParselet = new PrefixErrorParselet();

auto prefixCloserParselet = new PrefixCloserParselet();

auto prefixUnsupportedTokenParselet = new PrefixUnsupportedTokenParselet();

auto prefixUnhandledParselet = new PrefixUnhandledParselet();

auto prefixCommaParselet = new PrefixCommaParselet();

auto infixAssertFalseParselet = new InfixAssertFalseParselet();

auto infixImplicitTimesParselet = new InfixImplicitTimesParselet();

auto commaParselet = new CommaParselet();

auto semiParselet = new SemiParselet();

auto semiSemiParselet = new SemiSemiParselet();

auto infixDifferentialDParselet = new InfixDifferentialDParselet();

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

