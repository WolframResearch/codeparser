BeginPackage["CodeParser`Generate`Symbol`"]

Begin["`Private`"]

Needs["CodeParser`Generate`GenerateSources`"]
Needs["CodeParser`Generate`ParseletRegistration`"]



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



GroupOpenerToCloser[Token`OpenCurly] = Closer`CloseCurly
GroupOpenerToCloser[Token`LessBar] = Closer`BarGreater
GroupOpenerToCloser[Token`OpenSquare] = Closer`CloseSquare
GroupOpenerToCloser[Token`OpenParen] = Closer`CloseParen

GroupOpenerToCloser[Token`LongName`LeftAngleBracket] = Closer`LongName`RightAngleBracket
GroupOpenerToCloser[Token`LongName`LeftCeiling] = Closer`LongName`RightCeiling
GroupOpenerToCloser[Token`LongName`LeftFloor] = Closer`LongName`RightFloor
GroupOpenerToCloser[Token`LongName`LeftDoubleBracket] = Closer`LongName`RightDoubleBracket
GroupOpenerToCloser[Token`LongName`LeftBracketingBar] = Closer`LongName`RightBracketingBar
GroupOpenerToCloser[Token`LongName`LeftDoubleBracketingBar] = Closer`LongName`RightDoubleBracketingBar
GroupOpenerToCloser[Token`LongName`LeftAssociation] = Closer`LongName`RightAssociation
GroupOpenerToCloser[Token`LongName`OpenCurlyQuote] = Closer`LongName`CloseCurlyQuote
GroupOpenerToCloser[Token`LongName`OpenCurlyDoubleQuote] = Closer`LongName`CloseCurlyDoubleQuote

GroupOpenerToCloser[Token`LinearSyntax`OpenParen] = Closer`LinearSyntax`CloseParen


TokenToCloser[Token`CloseCurly] = Closer`CloseCurly
TokenToCloser[Token`BarGreater] = Closer`BarGreater
TokenToCloser[Token`CloseSquare] = Closer`CloseSquare
TokenToCloser[Token`CloseParen] = Closer`CloseParen

TokenToCloser[Token`LongName`RightAngleBracket] = Closer`LongName`RightAngleBracket
TokenToCloser[Token`LongName`RightCeiling] = Closer`LongName`RightCeiling
TokenToCloser[Token`LongName`RightFloor] = Closer`LongName`RightFloor
TokenToCloser[Token`LongName`RightDoubleBracket] = Closer`LongName`RightDoubleBracket
TokenToCloser[Token`LongName`RightBracketingBar] = Closer`LongName`RightBracketingBar
TokenToCloser[Token`LongName`RightDoubleBracketingBar] = Closer`LongName`RightDoubleBracketingBar
TokenToCloser[Token`LongName`RightAssociation] = Closer`LongName`RightAssociation
TokenToCloser[Token`LongName`CloseCurlyQuote] = Closer`LongName`CloseCurlyQuote
TokenToCloser[Token`LongName`CloseCurlyDoubleQuote] = Closer`LongName`CloseCurlyDoubleQuote

TokenToCloser[Token`LinearSyntax`CloseParen] = Closer`LinearSyntax`CloseParen









Print["Generating Symbol..."]

$WorkaroundBug321344 = checkBug321344[]
Print["Work around Bug 321344: ", $WorkaroundBug321344];

symbols = Union[Join[
    {Blank, BlankSequence, BlankNullSequence, EndOfFile, Integer, Integral, Integrate, Null, Out, Optional, Pattern,
      Rational, Real, Slot, SlotSequence, String, Symbol, TagSet, TagSetDelayed, TagUnset, Whitespace},
    {CodeParser`Library`MakeLeafNode, CodeParser`Library`MakeErrorNode, CodeParser`Library`MakePrefixNode,
      CodeParser`Library`MakeBinaryNode, CodeParser`Library`MakeInfixNode,
            CodeParser`Library`MakeTernaryNode, CodeParser`Library`MakePostfixNode, CodeParser`Library`MakeCallNode,
            CodeParser`Library`MakeGroupNode,
            CodeParser`Library`MakeBlankNode, CodeParser`Library`MakeBlankSequenceNode,
            CodeParser`Library`MakeBlankNullSequenceNode,
            CodeParser`Library`MakePatternBlankNode, CodeParser`Library`MakePatternBlankSequenceNode,
            CodeParser`Library`MakePatternBlankNullSequenceNode, CodeParser`Library`MakePatternOptionalDefaultNode, CodeParser`Library`MakeSyntaxErrorNode,
            CodeParser`Library`MakeSlotNode, CodeParser`Library`MakeSlotSequenceNode, CodeParser`Library`MakeOutNode,
            CodeParser`Library`MakeGroupMissingCloserNode, CodeParser`Library`MakeGroupMissingCloserNeedsReparseNode, CodeParser`Library`MakePrefixBinaryNode,
            CodeParser`Library`MakeSyntaxIssue, CodeParser`Library`MakeReplaceTextCodeAction, CodeParser`Library`MakeInsertTextCodeAction,
            CodeParser`Library`MakeFormatIssue, CodeParser`Library`MakeDeleteTextCodeAction, CodeParser`Library`MakeDeleteTriviaCodeAction,
            CodeParser`Library`MakeEncodingIssue,
            CodeParser`Library`MakeInsertTextAfterCodeAction, CodeParser`Library`MakeSourceCharacterNode, CodeParser`Library`MakeSafeStringNode},
    {CodeParser`InternalInvalid, CodeParser`PatternBlank, CodeParser`PatternBlankSequence,
      CodeParser`PatternBlankNullSequence, CodeParser`PatternOptionalDefault},
    {CodeParser`SourceCharacter},
    {Token`Newline},
    DownValues[PrefixOperatorToParselet][[All, 2]] /. {
      PrefixOperatorParselet[_, _, op_] :> op,
      GroupParselet[_, op_] :> op,
      LeafParselet[_] :> Nothing,
      UnderParselet[_] :> Nothing,
      IntegralParselet[] :> Nothing,
      LessLessParselet[] :> Get,
      LinearSyntaxOpenParenParselet[] :> CodeParser`GroupLinearSyntaxParen,
      PrefixAssertFalseParselet[] :> Nothing,
      PrefixCloserParselet[] :> Nothing,
      PrefixEndOfFileParselet[] :> Nothing,
      PrefixErrorParselet[] :> Nothing,
      PrefixUnhandledParselet[] :> Nothing,
      PrefixUnsupportedTokenParselet[] :> Nothing,
      SymbolParselet[] :> Nothing,
      UnderDotParselet[] :> Nothing,
      HashParselet[] :> Nothing,
      HashHashParselet[] :> Nothing,
      PercentParselet[] :> Nothing,
      PercentPercentParselet[] :> Nothing,
      SemiSemiParselet[] :> Span
    },
    DownValues[InfixOperatorToParselet][[All, 2]] /. {
      BinaryOperatorParselet[_, _, op_] :> op,
      CallParselet[_] :> Nothing,
      InfixOperatorParselet[_, _, op_] :> op,
      InfixOperatorWithTrailingParselet[_, _, op_] :> op,
      PostfixOperatorParselet[_, _, op_] :> op,
      ColonColonParselet[] :> MessageName,
      ColonEqualParselet[] :> SetDelayed,
      ColonParselet[] :> Nothing,
      EqualDotParselet[] :> Unset,
      EqualParselet[] :> Set,
      GreaterGreaterGreaterParselet[] :> PutAppend,
      GreaterGreaterParselet[] :> Put,
      InfixAssertFalseParselet[] :> Nothing,
      InfixCloserParselet[] :> Nothing,
      InfixDifferentialDParselet[] :> Nothing,
      InfixEndOfFileParselet[] :> Nothing,
      InfixErrorParselet[] :> Nothing,
      InfixImplicitTimesParselet[] :> Nothing,
      InfixToplevelNewlineParselet[] :> Nothing,
      InfixUnsupportedTokenParselet[] :> Nothing,
      SlashColonParselet[] :> Nothing,
      TildeParselet[] :> CodeParser`TernaryTilde,
      SemiSemiParselet[] :> Span
    },
    tokens
]]

symbolCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"TokenEnum.h\"

#if USE_MATHLINK
#include \"mathlink.h\"
#undef P
#endif // USE_MATHLINK

#include <memory>

//
//
//
class Symbol {
public:
  constexpr Symbol(const char *Name) : Name(Name) {}
  const char *name() const;

#if USE_MATHLINK
  void put(MLINK mlp) const;
#endif

private:
  const char *Name;
};

using SymbolPtr = std::unique_ptr<Symbol>;

Closer GroupOpenerToCloser(TokenEnum T);
Closer TokenToCloser(TokenEnum T);

SymbolPtr& TokenToSymbol(TokenEnum T);

//
//
//"} ~Join~
(Row[{"extern", " ", "SymbolPtr", " ", toGlobal["Symbol`"<>ToString[#]], ";"}]& /@ symbols) ~Join~
{""}

Print["exporting Symbol.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Symbol.h"}], Column[symbolCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

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
    Context[sym]<>SymbolName[sym]
  ]
]

symbolCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"Symbol.h\"

#include \"Token.h\"

#include <cassert>

const char *Symbol::name() const {
   return Name;
}

#if USE_MATHLINK
void Symbol::put(MLINK mlp) const {
    if (!MLPutSymbol(mlp, Name)) {
        assert(false);
    }
}
#endif
"} ~Join~

(If[# === String && $WorkaroundBug321344,
  (*
  handle String specially because of bug 321344
  *)
  "SymbolPtr SYMBOL_STRING = SymbolPtr(new Symbol(\"String\"));"
  ,
  Row[{"SymbolPtr", " ", toGlobal["Symbol`"<>ToString[#]], " = SymbolPtr(new Symbol(\"", stringifyForTransmitting[#], "\"));"}]]& /@ symbols) ~Join~

{""} ~Join~

{"Closer GroupOpenerToCloser(TokenEnum T) {"} ~Join~
{"switch (T.value()) {"} ~Join~
Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[GroupOpenerToCloser]] ~Join~
{"default: assert(false && \"Unhandled token\"); return CLOSER_ASSERTFALSE;",
"}"} ~Join~
{"}"} ~Join~

{""} ~Join~

{"Closer TokenToCloser(TokenEnum T) {"} ~Join~
{"switch (T.value()) {"} ~Join~
Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[TokenToCloser]] ~Join~
{"default: return CLOSER_ASSERTFALSE;",
"}"} ~Join~
{"}"} ~Join~

{""}

Print["exporting Symbol.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Symbol.cpp"}], Column[symbolCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Symbol"]

End[]

EndPackage[]
