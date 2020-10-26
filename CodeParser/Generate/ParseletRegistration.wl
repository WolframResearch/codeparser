BeginPackage["CodeParser`Generate`ParseletRegistration`"]

PrefixOperatorToParselet

InfixOperatorToParselet


Begin["`Private`"]

Needs["CodeParser`Generate`TokenEnum`"] (* for tokens *)
Needs["CodeParser`Generate`Common`"]
Needs["CodeTools`Generate`GenerateSources`"]


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

formatPrefix[Parselet`SymbolParselet[]] := "&symbolParselet"

formatPrefix[Parselet`UnderParselet[1]] := "&under1Parselet"

formatPrefix[Parselet`UnderParselet[2]] := "&under2Parselet"

formatPrefix[Parselet`UnderParselet[3]] := "&under3Parselet"

formatPrefix[Parselet`UnderDotParselet[]] := "&underDotParselet"

formatPrefix[Parselet`HashParselet[]] := "new HashParselet()"

formatPrefix[Parselet`HashHashParselet[]] := "new HashHashParselet()"

formatPrefix[Parselet`PercentParselet[]] := "new PercentParselet()"

formatPrefix[Parselet`PercentPercentParselet[]] := "new PercentPercentParselet()"

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

formatInfix[Parselet`ColonParselet[]] := "&colonParselet"

formatInfix[Parselet`CallParselet[groupParselet_]] := "new CallParselet(" <> formatPrefix[groupParselet] <> ")"

formatInfix[Parselet`EqualParselet[]] := "new EqualParselet()"

formatInfix[Parselet`ColonEqualParselet[]] := "new ColonEqualParselet()"

formatInfix[Parselet`TildeParselet[]] := "new TildeParselet()"

formatInfix[Parselet`SlashColonParselet[]] := "new SlashColonParselet()"

formatInfix[Parselet`CommaParselet[]] := "&commaParselet"

formatInfix[Parselet`SemiParselet[]] := "&semiParselet"

formatInfix[Parselet`SemiSemiParselet[]] := "&semiSemiParselet"

formatInfix[Parselet`ColonColonParselet[]] := "new ColonColonParselet()"

formatInfix[Parselet`GreaterGreaterParselet[]] := "new GreaterGreaterParselet()"

formatInfix[Parselet`GreaterGreaterGreaterParselet[]] := "new GreaterGreaterGreaterParselet()"

formatInfix[Parselet`InfixDifferentialDParselet[]] := "&infixDifferentialDParselet"

formatInfix[Parselet`InfixToplevelNewlineParselet[]] := "new InfixToplevelNewlineParselet()"



generate[] := (

Print["Generating ParseletRegistration..."];

parseletRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"TokenEnum.h\"

#include <array>

class PrefixParselet;
class InfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
class PrefixToplevelCloserParselet;

using PrefixParseletPtr = PrefixParselet *;
using InfixParseletPtr = InfixParselet *;
using ContextSensitivePrefixParseletPtr = ContextSensitivePrefixParselet *;
using ContextSensitiveInfixParseletPtr = ContextSensitiveInfixParselet *;
using PrefixToplevelCloserParseletPtr = PrefixToplevelCloserParselet *;

extern std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets;
extern std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets;

extern ContextSensitivePrefixParseletPtr contextSensitiveSymbolParselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder1Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder2Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder3Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnderDotParselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveColonParselet;

extern PrefixToplevelCloserParseletPtr contextSensitivePrefixToplevelCloserParselet;
"};

Print["exporting ParseletRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "ParseletRegistration.h"}], Column[parseletRegistrationCPPHeader], "String"];

If[FailureQ[res],
  Print[res];
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

#include <cassert>

auto symbolParselet = SymbolParselet();

auto leafParselet = LeafParselet();

auto prefixEndOfFileParselet = PrefixEndOfFileParselet();

auto prefixErrorParselet = PrefixErrorParselet();

auto prefixCloserParselet = PrefixCloserParselet();

auto prefixToplevelCloserParselet = PrefixToplevelCloserParselet();

auto prefixUnsupportedTokenParselet = PrefixUnsupportedTokenParselet();

auto prefixUnhandledParselet = PrefixUnhandledParselet();

auto prefixCommaParselet = PrefixCommaParselet();

auto infixAssertFalseParselet = InfixAssertFalseParselet();

auto infixImplicitTimesParselet = InfixImplicitTimesParselet();

auto commaParselet = CommaParselet();

auto semiParselet = SemiParselet();

auto semiSemiParselet = SemiSemiParselet();

auto colonParselet = ColonParselet();

auto infixDifferentialDParselet = InfixDifferentialDParselet();

auto under1Parselet = UnderParselet(SYMBOL_BLANK, SYMBOL_CODEPARSER_PATTERNBLANK);

auto under2Parselet = UnderParselet(SYMBOL_BLANKSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE);

auto under3Parselet = UnderParselet(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE);

auto underDotParselet = UnderDotParselet();

auto squareGroupParselet = GroupParselet(TOKEN_OPENSQUARE, SYMBOL_CODEPARSER_GROUPSQUARE);

auto doubleBracketGroupParselet = GroupParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET);

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

ContextSensitivePrefixParseletPtr contextSensitiveSymbolParselet(&symbolParselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder1Parselet(&under1Parselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder2Parselet(&under2Parselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder3Parselet(&under3Parselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnderDotParselet(&underDotParselet);
ContextSensitiveInfixParseletPtr contextSensitiveColonParselet(&colonParselet);

PrefixToplevelCloserParseletPtr contextSensitivePrefixToplevelCloserParselet(&prefixToplevelCloserParselet);
"};

Print["exporting ParseletRegistration.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "ParseletRegistration.cpp"}], Column[parseletRegistrationCPPSource], "String"];

If[FailureQ[res],
  Print[res];
  Quit[1]
];

Print["Done ParseletRegistration"]
)

If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]

