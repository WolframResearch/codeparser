(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`Symbol`"]

Begin["`Private`"]

(*
auto-load any symbols that require the PacletManager that may appear later
System`MapApply evaluates in CodeParser`Generate`Parselet` because InfixParselets.wl is loaded
this prevents e.g. "Get::noopen: Cannot open MapApplyCompatibility`." messages when building
*)
System`MapApply
(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeParser`Generate`Parselet`"];
Needs["CodeParser`Generate`TokenEnum`"]; (* for tokens *)
Needs["CodeParser`Generate`Common`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


checkBuildDir[]


(*
Bug 321344:

ExportString["String", "String"] returns ""

checkBug321344[] sets the flag $WorkaroundBug321344 to True if we still need to workaround bug 321344
*)
checkBug321344[] :=
Module[{res},
  res = ExportString["String", "String"];
  Switch[res,
    "",
    True
    ,
    "String",
    False
    ,
    _,
    Print["Unhandled result while checking bug 321344: ", res];
    Quit[1]
  ]
]

$WorkaroundBug321344 = checkBug321344[];
Print["Work around Bug 321344: ", $WorkaroundBug321344];


(*
We want to fully-qualify symbol names over the wire.
This allows library->kernel traffic to work when CodeParser` is not on $ContextPath.
However, it is still not possible to fully-qualify System` symbols
Related bugs: 283291, 284492
So also make library->kernel traffic match this behavior
*)
stringifyForTransmitting[sym_Symbol] :=
Module[{ctxt},
  ctxt = Context[sym];
  If[ctxt == "System`",
    SymbolName[sym]
    ,
    Context[sym] <> SymbolName[sym]
  ]
]



symbols = Union[Flatten[Join[
  {Blank, BlankSequence, BlankNullSequence, ByteArray,
    ConfidenceLevel, EndOfFile, EvaluatePacket, Integer, Missing,
    Null, Out, Optional, Pattern, Rational, Real, Rule, Slot,
    SlotSequence, String, Symbol, TagSet, TagSetDelayed, TagUnset,
    Unset, Whitespace, $Aborted},
  {CodeParser`Source},
  {CodeParser`LeafNode,
    CodeParser`ErrorNode, CodeParser`UnterminatedTokenErrorNeedsReparseNode,
    CodeParser`PrefixNode,
    CodeParser`BinaryNode, CodeParser`InfixNode,
    CodeParser`TernaryNode, CodeParser`PostfixNode, CodeParser`CallNode,
    CodeParser`GroupNode,
    CodeParser`CompoundNode,
    CodeParser`SyntaxErrorNode,
    CodeParser`GroupMissingCloserNode, CodeParser`UnterminatedGroupNeedsReparseNode,
    CodeParser`PrefixBinaryNode},
  {CodeParser`SyntaxIssue, CodeParser`FormatIssue, CodeParser`EncodingIssue,
    CodeParser`ReplaceText, CodeParser`DeleteText, CodeParser`InsertText,
    CodeParser`CodeActions, CodeParser`CodeAction},
  {CodeParser`Library`LongNameSuggestion, CodeParser`Library`SetConcreteParseProgress},
  {CodeParser`InternalInvalid, CodeParser`PatternBlank, CodeParser`PatternBlankSequence,
    CodeParser`PatternBlankNullSequence, CodeParser`PatternOptionalDefault},
  {SyntaxError`ExpectedSet, SyntaxError`ExpectedTilde, SyntaxError`ExpectedSymbol},
  {Token`Newline},
  DownValues[PrefixOperatorToParselet][[All, 2]] /. {
    Parselet`PrefixOperatorParselet[_, op_] :> op,
    Parselet`GroupParselet[_, op_] :> op,
    Parselet`LeafParselet[] :> Nothing,
    Parselet`UnderParselet[_] :> Nothing,
    Parselet`IntegralParselet[op1_, op2_] :> {op1, op2},
    Parselet`LessLessParselet[] :> Get,
    Parselet`PrefixNullPointerParselet[] :> Nothing,
    Parselet`PrefixCloserParselet[] :> Nothing,
    Parselet`PrefixEndOfFileParselet[] :> Nothing,
    Parselet`PrefixErrorParselet[] :> Nothing,
    Parselet`PrefixUnhandledParselet[] :> Nothing,
    Parselet`PrefixCommaParselet[] :> Nothing,
    Parselet`PrefixUnsupportedTokenParselet[] :> Nothing,
    Parselet`SymbolParselet[] :> Nothing,
    Parselet`UnderDotParselet[] :> Nothing,
    Parselet`HashParselet[] :> Nothing,
    Parselet`HashHashParselet[] :> Nothing,
    Parselet`PercentParselet[] :> Nothing,
    Parselet`PercentPercentParselet[] :> Nothing,
    Parselet`SemiSemiParselet[] :> Span
  },
  DownValues[InfixOperatorToParselet][[All, 2]] /. {
    Parselet`BinaryOperatorParselet[_, op_] :> op,
    Parselet`CallParselet[_] :> Nothing,
    Parselet`InfixOperatorParselet[_, op_] :> op,
    Parselet`TimesParselet[] :> Times,
    Parselet`PostfixOperatorParselet[_, op_] :> op,
    Parselet`ColonColonParselet[] :> MessageName,
    Parselet`ColonEqualParselet[] :> SetDelayed,
    Parselet`ColonParselet[] :> Nothing,
    Parselet`EqualParselet[] :> Set,
    Parselet`GreaterGreaterGreaterParselet[] :> PutAppend,
    Parselet`GreaterGreaterParselet[] :> Put,
    Parselet`InfixAssertFalseParselet[] :> Nothing,
    Parselet`InfixNullPointerParselet[] :> Nothing,
    Parselet`InfixDifferentialDParselet[] :> Nothing,
    Parselet`InfixImplicitTimesParselet[] :> Nothing,
    Parselet`InfixToplevelNewlineParselet[] :> Nothing,
    Parselet`SlashColonParselet[] :> Nothing,
    Parselet`TildeParselet[] :> CodeParser`TernaryTilde,
    Parselet`CommaParselet[] :> CodeParser`Comma,
    Parselet`SemiParselet[] :> CompoundExpression,
    Parselet`SemiSemiParselet[] :> Span
  },
  tokens
]]]



generate[] := (

Print["Generating Symbol..."];

symbolRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"Symbol.h\"


//
// All symbols that are used by CodeParser
//"} ~Join~
MapIndexed[
If[#1 === String && $WorkaroundBug321344,
  (*
  handle String specially because of bug 321344
  *)
  Row[{"constexpr Symbol", " ", "SYMBOL_STRING", "(", "\"String\"", ",", " ", ToString[#2[[1]]-1], ")", ";"}]
  ,
  Row[{"constexpr Symbol", " ", toGlobal["Symbol`"<>ToString[#1]], "(", "\"", stringifyForTransmitting[#1], "\"", ",", " ", ToString[#2[[1]]-1], ")", ";"}]]&, symbols] ~Join~
{""};

Print["exporting SymbolRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "SymbolRegistration.h"}], Column[symbolRegistrationCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done Symbol"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
