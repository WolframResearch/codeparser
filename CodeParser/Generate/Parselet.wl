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

(*------------*)
(* $Operators *)
(*------------*)

(* Variants of `enum Operator { ... }` *)
$Operators = Join[
	AssociationMap[Identity, {
		Times,
		Span,
		Pattern,
		Optional,
		Set,
		SetDelayed,
		Unset,
		TagSet,
		TagSetDelayed,
		TagUnset,
		CompoundExpression,
		MessageName,
		Put,
		PutAppend,
		Get,
		CodeParser`InternalInvalid,
		CodeParser`Comma,
		CodeParser`TernaryTilde,
		CodeParser`TernaryOptionalPattern,
		CodeParser`InfixTilde
	}],
	DeleteDuplicates @ Association @ Flatten @ Replace[
		Join[
			Values[importedPrefixParselets],
			Values[importedInfixParselets]
		],
		{
			Parselet`PrefixOperatorParselet[precedence_, op_] :> (op -> op),
			Parselet`BinaryOperatorParselet[precedence_, op_] :> (op -> op),
			Parselet`InfixOperatorParselet[precedence_, op_] :> (op -> op),
			Parselet`PostfixOperatorParselet[precedence_, op_] :> (op -> op),
			(* NOTE: Ignore op2, which is part of $PrefixBinaryOperators. *)
			Parselet`IntegralParselet[op1_, op2_] -> (op2 -> op2),
			(* These are part of $GroupOperators. *)
			Parselet`GroupParselet[tok_, op_] -> Nothing,
			_ -> Nothing
		},
		{1}
	]
]

$PrefixBinaryOperators =
	Association @ Flatten @ Replace[
		Join[
			Values[importedPrefixParselets],
			Values[importedInfixParselets]
		],
		{
			(* FIXME: Make this comment more precise once infix and prefix
			          operators are factored out of $Operators. *)
			(* NOTE: Ignore op2, which is part of $Operators. *)
			Parselet`IntegralParselet[op1_, op2_] :> (op1 -> op1),
			_ -> Nothing
		},
		{1}
	]

$GroupOperators = Join[
	AssociationMap[Identity, {
		Token`Comment
	}],
	Association @ Cases[
		Join[
			Values[importedPrefixParselets],
			Values[importedInfixParselets]
		],
		Parselet`GroupParselet[tok_, op_] :> (op -> op)
	]
]

$CompoundOperators = Join[
	AssociationMap[Identity, {
		Blank,
		BlankSequence,
		BlankNullSequence,
		Slot,
		SlotSequence,
		Out,
		CodeParser`PatternBlank,
		CodeParser`PatternBlankSequence,
		CodeParser`PatternBlankNullSequence,
		CodeParser`PatternOptionalDefault
	}]
]

If[!MatchQ[$Operators, <| (_Symbol -> _Symbol) ... |>],
	FatalError["Bad $Operators: ", $Operators];
]

If[!MatchQ[$PrefixBinaryOperators, <| (_Symbol -> _Symbol) ... |>],
	FatalError["Bad $PrefixBinaryOperators: ", InputForm @ $PrefixBinaryOperators];
]

If[!MatchQ[$GroupOperators, <| (_Symbol -> _Symbol) ... |>],
	FatalError["Bad $GroupOperators: ", InputForm @ $GroupOperators];
]

If[!MatchQ[$CompoundOperators, <| (_Symbol -> _Symbol) ... |>],
	FatalError["Bad $CompoundOperators: ", InputForm @ $CompoundOperators];
]

(*--------------*)
(* formatPrefix *)
(*--------------*)

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

formatPrefix[Parselet`IntegralParselet[op1_, op2_]] := "&IntegralParselet::new(PrefixBinaryOperator::" <> toGlobal[op1, "UpperCamelCase"] <> ", Operator::" <> toGlobal[op2, "UpperCamelCase"] <> ")"

formatPrefix[Parselet`PrefixOperatorParselet[precedence_, op_]] := "&PrefixOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "Operator::" <> toGlobal[op, "UpperCamelCase"] <> ")"

formatPrefix[Parselet`GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]] := "&squareGroupParselet"

formatPrefix[Parselet`GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]] := "&doubleBracketGroupParselet"

formatPrefix[Parselet`GroupParselet[tok_, op_]] := (
	"&GroupParselet::new(TokenKind::"
	<> toTokenEnumVariant[tok]
	<> ", "
	<> "GroupOperator::"
	<> toGlobal[op, "UpperCamelCase"]
	<> ")"
)

(*-------------*)
(* formatInfix *)
(*-------------*)

formatInfix[Parselet`InfixAssertFalseParselet[]] := "&infixAssertFalseParselet"

formatInfix[Parselet`InfixNullPointerParselet[]] := "&infixAssertFalseParselet"

formatInfix[Parselet`InfixImplicitTimesParselet[]] := "&infixImplicitTimesParselet"



formatInfix[Parselet`BinaryOperatorParselet[precedence_, op_]] := "&BinaryOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "Operator::" <> toGlobal[op, "UpperCamelCase"] <> ")"

formatInfix[Parselet`InfixOperatorParselet[precedence_, op_]] := "&InfixOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "Operator::" <> toGlobal[op, "UpperCamelCase"] <> ")"

formatInfix[Parselet`PostfixOperatorParselet[precedence_, op_]] := "&PostfixOperatorParselet::new(" <> toGlobal[precedence] <> ", " <> "Operator::" <> toGlobal[op, "UpperCamelCase"] <> ")"

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



formatOperatorEnumDef[name_?StringQ, values_?AssociationQ] :=
	StringJoin[
		"#[allow(non_camel_case_types)]\n",
		"#[derive(Debug, Copy, Clone, PartialEq)]\n",
		"pub enum " <> name <> " {\n",
		KeyValueMap[
			{k, v} |-> Replace[{k, v}, {
				{operator_Symbol, symbol_Symbol} :> "    " <> toGlobal[operator, "UpperCamelCase"] <> ",\n",
				other_ :> FatalError["Unexpected operator: ", other]
			}],
			values
		],
		"}\n\n"
	]

formatOperatorEnumImpl[name_?StringQ, values_?AssociationQ] :=
	StringJoin[
		"impl " <> name <> " {\n",
		"    #[allow(dead_code)]\n",
		"    #[doc(hidden)]\n",
		"    pub fn to_symbol(self) -> Symbol {\n",
		"        match self {\n",
		KeyValueMap[
			{k, v} |-> Replace[{k, v}, {
				{operator_Symbol, symbol_Symbol} :>
					"            " <> name <> "::" <> toGlobal[operator, "UpperCamelCase"] <> " => sym::" <> toGlobal[symbol, "UpperCamelCase"] <> ",\n",
				other_ :> FatalError["Unexpected operator: ", other]
			}],
			values
		],
		"        }\n",
		"    }\n",
		"\n",
		"    #[doc(hidden)]\n",
		"    pub fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {\n",
		"        let operator = match symbol {\n",
		KeyValueMap[
			{k, v} |-> Replace[{k, v}, {
				{operator_Symbol, symbol_Symbol} :>
					"            sym::" <> toGlobal[symbol, "UpperCamelCase"] <> " => " <> name <> "::" <> toGlobal[operator, "UpperCamelCase"] <> ",\n",
				other_ :> FatalError["Unexpected operator: ", other]
			}],
			values
		],
		"            _ => return None,\n",
		"        };\n",
		"\n",
		"        Some(operator)\n",
		"    }\n",
		"}\n"
	]



generate[] := (

Print["Generating Parselet..."];

parseletRegistrationCPPSource = {
"\
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(non_upper_case_globals)]

use wolfram_expr::symbol::SymbolRef;

use crate::{
	token::TokenKind,
	symbol::Symbol,
	symbol_registration as sym,
	precedence::*,
	parselet::*
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

pub(crate) const under1Parselet: UnderParselet = UnderParselet::new(CompoundOperator::Blank, CompoundOperator::CodeParser_PatternBlank);
pub(crate) const under2Parselet: UnderParselet = UnderParselet::new(CompoundOperator::BlankSequence, CompoundOperator::CodeParser_PatternBlankSequence);
pub(crate) const under3Parselet: UnderParselet = UnderParselet::new(CompoundOperator::BlankNullSequence, CompoundOperator::CodeParser_PatternBlankNullSequence);

pub(crate) const underDotParselet: UnderDotParselet = UnderDotParselet {};

pub(crate) const squareGroupParselet: GroupParselet = GroupParselet::new(TokenKind::OpenSquare, GroupOperator::CodeParser_GroupSquare);

pub(crate) const doubleBracketGroupParselet: GroupParselet = GroupParselet::new(TokenKind::LongName_LeftDoubleBracket, GroupOperator::CodeParser_GroupDoubleBracket);

pub(crate) const timesParselet: TimesParselet = TimesParselet {};

//
//
//

pub(crate) const PREFIX_PARSELETS: [PrefixParseletPtr; TokenKind::Count.value() as usize] = ["} ~Join~

(Row[{"    ", formatPrefix[PrefixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"];

//
//
//
pub(crate) const INFIX_PARSELETS: [InfixParseletPtr; TokenKind::Count.value() as usize] = ["} ~Join~

(Row[{"    ", formatInfix[InfixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"];\n"} ~Join~

{
	StringJoin[
		(*============================*)
		(* Define Enums               *)
		(*============================*)

		formatOperatorEnumDef["Operator", $Operators],
		formatOperatorEnumDef["PrefixBinaryOperator", $PrefixBinaryOperators],
		formatOperatorEnumDef["GroupOperator", $GroupOperators],
		formatOperatorEnumDef["CompoundOperator", $CompoundOperators],

		(*============================*)
		(* Define Impls               *)
		(*============================*)

		formatOperatorEnumImpl["Operator", $Operators],
		formatOperatorEnumImpl["PrefixBinaryOperator", $PrefixBinaryOperators],
		formatOperatorEnumImpl["GroupOperator", $GroupOperators],
		formatOperatorEnumImpl["CompoundOperator", $CompoundOperators]
	]
};

	Print["exporting ParseletRegistration.cpp"];
	res = Export[
		FileNameJoin[{generatedCPPSrcDir, "parselet_registration.rs"}],
		Column[parseletRegistrationCPPSource],
		"String"
	];

	Print[res];

	If[FailureQ[res],
			Quit[1]
	];

	Print["Done Parselet"]

) (* generate[] *)

If[!StringQ[script],
	Quit[1]
]

If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
	generate[]
]

End[]

EndPackage[]

