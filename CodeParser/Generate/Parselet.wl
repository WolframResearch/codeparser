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



formatPrefix[Parselet`PrefixEndOfFileParselet[]] := "&prefixEndOfFileParselet"

formatPrefix[Parselet`PrefixNullPointerParselet[]] := "&PrefixAssertFalseParselet {}"

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

formatPrefix[Parselet`HashParselet[]] := "&HashParselet {}"

formatPrefix[Parselet`HashHashParselet[]] := "&HashHashParselet {}"

formatPrefix[Parselet`PercentParselet[]] := "&PercentParselet {}"

formatPrefix[Parselet`LessLessParselet[]] := "&(LessLessParselet {})"

formatPrefix[Parselet`SemiSemiParselet[]] := "&semiSemiParselet"

formatPrefix[Parselet`IntegralParselet[op1_, op2_]] := "&IntegralParselet::new(" <> "SYMBOL_" <> toGlobal[op1] <> ", " <> "SYMBOL_" <> toGlobal[op2] <> ")"

formatPrefix[Parselet`PrefixOperatorParselet[precedence_, op_]] := "&PrefixOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatPrefix[Parselet`GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]] := "&squareGroupParselet"

formatPrefix[Parselet`GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]] := "&doubleBracketGroupParselet"

formatPrefix[Parselet`GroupParselet[tok_, op_]] := "&GroupParselet::new(" <> toGlobal[tok] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"


formatInfix[Parselet`InfixAssertFalseParselet[]] := "&infixAssertFalseParselet"

formatInfix[Parselet`InfixNullPointerParselet[]] := "&infixAssertFalseParselet"

formatInfix[Parselet`InfixImplicitTimesParselet[]] := "&infixImplicitTimesParselet"



formatInfix[Parselet`BinaryOperatorParselet[precedence_, op_]] := "&BinaryOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`InfixOperatorParselet[precedence_, op_]] := "&InfixOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`PostfixOperatorParselet[precedence_, op_]] := "&PostfixOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[Parselet`ColonParselet[]] := "&colonParselet"

formatInfix[Parselet`CallParselet[groupParselet_]] := "&(CallParselet::new(" <> formatPrefix[groupParselet] <> "))"

formatInfix[Parselet`EqualParselet[]] := "&equalParselet"

formatInfix[Parselet`ColonEqualParselet[]] := "&colonEqualParselet"

formatInfix[Parselet`TildeParselet[]] := "(&TildeParselet {})"

formatInfix[Parselet`SlashColonParselet[]] := "&slashColonParselet"

formatInfix[Parselet`CommaParselet[]] := "&commaParselet"

formatInfix[Parselet`SemiParselet[]] := "&semiParselet"

formatInfix[Parselet`SemiSemiParselet[]] := "&semiSemiParselet"

formatInfix[Parselet`ColonColonParselet[]] := "(&ColonColonParselet {})"

formatInfix[Parselet`GreaterGreaterParselet[]] := "(&GreaterGreaterParselet {})"

formatInfix[Parselet`GreaterGreaterGreaterParselet[]] := "(&GreaterGreaterGreaterParselet {})"

formatInfix[Parselet`InfixDifferentialDParselet[]] := "&infixDifferentialDParselet"

formatInfix[Parselet`InfixToplevelNewlineParselet[]] := "&(InfixToplevelNewlineParselet {})"

formatInfix[Parselet`TimesParselet[]] := "&timesParselet"


generate[] := (

Print["Generating Parselet..."];

parseletRegistrationCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(non_upper_case_globals)]

use crate::{
	token_enum_registration::TokenEnum::*,
	symbol_registration::*,
	precedence::*,
	parselet::{*}
};

pub(crate) const symbolParselet: SymbolParselet = SymbolParselet {};

pub(crate) const leafParselet: LeafParselet = LeafParselet {};

pub(crate) const prefixEndOfFileParselet: PrefixEndOfFileParselet = PrefixEndOfFileParselet {};

pub(crate) const prefixErrorParselet: PrefixErrorParselet = PrefixErrorParselet {};

pub(crate) const prefixCloserParselet: PrefixCloserParselet = PrefixCloserParselet {};

pub(crate) const prefixUnsupportedTokenParselet: PrefixUnsupportedTokenParselet = PrefixUnsupportedTokenParselet {};

pub(crate) const prefixUnhandledParselet: PrefixUnhandledParselet = PrefixUnhandledParselet {};

pub(crate) const prefixCommaParselet: PrefixCommaParselet = PrefixCommaParselet {};

pub(crate) const infixAssertFalseParselet: InfixAssertFalseParselet = InfixAssertFalseParselet {};

pub(crate) const infixImplicitTimesParselet: InfixImplicitTimesParselet = InfixImplicitTimesParselet {};

pub(crate) const commaParselet: CommaParselet = CommaParselet {};

pub(crate) const semiParselet: SemiParselet = SemiParselet {};

pub(crate) const semiSemiParselet: SemiSemiParselet = SemiSemiParselet {};

pub(crate) const slashColonParselet: SlashColonParselet = SlashColonParselet {};

pub(crate) const colonParselet: ColonParselet = ColonParselet {};

pub(crate) const equalParselet: EqualParselet = EqualParselet::new();

pub(crate) const colonEqualParselet: ColonEqualParselet = ColonEqualParselet::new();

pub(crate) const infixDifferentialDParselet: InfixDifferentialDParselet = InfixDifferentialDParselet {};

pub(crate) const under1Parselet: UnderParselet = UnderParselet::new(SYMBOL_BLANK, SYMBOL_CODEPARSER_PATTERNBLANK);
pub(crate) const under2Parselet: UnderParselet = UnderParselet::new(SYMBOL_BLANKSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE);
pub(crate) const under3Parselet: UnderParselet = UnderParselet::new(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE);

pub(crate) const underDotParselet: UnderDotParselet = UnderDotParselet {};

pub(crate) const squareGroupParselet: GroupParselet = GroupParselet::new(TOKEN_OPENSQUARE, SYMBOL_CODEPARSER_GROUPSQUARE);

pub(crate) const doubleBracketGroupParselet: GroupParselet = GroupParselet::new(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET);

pub(crate) const timesParselet: TimesParselet = TimesParselet {};

//
//
//

pub(crate) const PREFIX_PARSELETS: [PrefixParseletPtr; TOKEN_COUNT.value() as usize] = ["} ~Join~

(Row[{"  ", formatPrefix[PrefixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"];

//
//
//
pub(crate) const INFIX_PARSELETS: [InfixParseletPtr; TOKEN_COUNT.value() as usize] = ["} ~Join~

(Row[{"  ", formatInfix[InfixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"];
"};

Print["exporting ParseletRegistration.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "parselet_registration.rs"}], Column[parseletRegistrationCPPSource], "String"];

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

