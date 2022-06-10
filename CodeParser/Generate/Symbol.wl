(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`Symbol`"]

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeParser`Generate`ParseletRegistration`"];
Needs["CodeParser`Generate`TokenEnum`"]; (* for tokens *)
Needs["CodeParser`Generate`Common`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


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



symbols = Union[Join[
    {Blank, BlankSequence, BlankNullSequence, ByteArray,
      ConfidenceLevel, EndOfFile, EvaluatePacket, Integer, Integral,
      Integrate, Missing, Null, Out, Optional, Pattern, Rational,
      Real, Rule, Slot, SlotSequence, String, Symbol, TagSet,
      TagSetDelayed, TagUnset, Unset, Whitespace},
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
      Parselet`PrefixOperatorParselet[_, _, op_] :> op,
      Parselet`GroupParselet[_, op_] :> op,
      Parselet`LeafParselet[] :> Nothing,
      Parselet`UnderParselet[_] :> Nothing,
      Parselet`IntegralParselet[] :> Nothing,
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
      Parselet`BinaryOperatorParselet[_, _, op_] :> op,
      Parselet`CallParselet[_] :> Nothing,
      Parselet`InfixOperatorParselet[_, _, op_] :> op,
      Parselet`PostfixOperatorParselet[_, _, op_] :> op,
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
]]



generate[] := (

Print["Generating Symbol..."];

symbolCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#if USE_MATHLINK
#include \"mathlink.h\"
#undef P
#endif // USE_MATHLINK

#include <memory>
#include <ostream>

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
// A kernel symbol
//
class Symbol {

  char const *Name;
  const int Id;

public:

  constexpr Symbol(char const *Name, int Id) : Name(Name), Id(Id) {}
  
  constexpr char const *name() const {
    return Name;
  }

  constexpr int getId() const {
    return Id;
  }

  void print(std::ostream& s) const;

#if USE_MATHLINK
  void put(MLINK mlp) const;
#endif // USE_MATHLINK

#if USE_EXPR_LIB
  expr toExpr() const;
#endif // USE_EXPR_LIB
};

bool operator==(Symbol a, Symbol b);

bool operator!=(Symbol a, Symbol b);


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

Print["exporting Symbol.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Symbol.h"}], Column[symbolCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

symbolCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"Symbol.h\"

#include \"Token.h\"

#if USE_EXPR_LIB
#include \"ExprLibrary.h\"
#endif // USE_EXPR_LIB

#include <cassert>

bool operator==(Symbol a, Symbol b) {
  return a.getId() == b.getId();
}

bool operator!=(Symbol a, Symbol b) {
  return a.getId() != b.getId();
}

void Symbol::print(std::ostream& s) const {
    s << Name;
}

#if USE_MATHLINK
void Symbol::put(MLINK mlp) const {
    if (!MLPutSymbol(mlp, Name)) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr Symbol::toExpr() const {
  return Expr_MEncodedStringToSymbolExpr(Name);
}
#endif // USE_EXPR_LIB

"} ~Join~

{""};

Print["exporting Symbol.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "Symbol.cpp"}], Column[symbolCPPSource], "String"];

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
