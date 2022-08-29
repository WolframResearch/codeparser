(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`TokenEnum`"]

GroupOpenerToCloser

tokens

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeParser`Generate`Common`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


checkBuildDir[]


GroupOpenerToCloser[Token`OpenCurly] = Closer`CloseCurly
GroupOpenerToCloser[Token`LessBar] = Closer`BarGreater
GroupOpenerToCloser[Token`OpenSquare] = Closer`CloseSquare
GroupOpenerToCloser[Token`OpenParen] = Closer`CloseParen
GroupOpenerToCloser[Token`ColonColonOpenSquare] = Closer`CloseSquare

GroupOpenerToCloser[Token`LongName`LeftAngleBracket] = Closer`LongName`RightAngleBracket
GroupOpenerToCloser[Token`LongName`LeftCeiling] = Closer`LongName`RightCeiling
GroupOpenerToCloser[Token`LongName`LeftFloor] = Closer`LongName`RightFloor
GroupOpenerToCloser[Token`LongName`LeftDoubleBracket] = Closer`LongName`RightDoubleBracket
GroupOpenerToCloser[Token`LongName`LeftBracketingBar] = Closer`LongName`RightBracketingBar
GroupOpenerToCloser[Token`LongName`LeftDoubleBracketingBar] = Closer`LongName`RightDoubleBracketingBar
GroupOpenerToCloser[Token`LongName`LeftAssociation] = Closer`LongName`RightAssociation
GroupOpenerToCloser[Token`LongName`OpenCurlyQuote] = Closer`LongName`CloseCurlyQuote
GroupOpenerToCloser[Token`LongName`OpenCurlyDoubleQuote] = Closer`LongName`CloseCurlyDoubleQuote


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



If[FailureQ[importedTokenEnumSource],
  Print[importedTokenEnumSource];
  Quit[1]
];


cur = 0;
enumMap = <||>;
KeyValueMap[(
  Which[
    IntegerQ[#2], cur = #2,
    #2 === Next, cur++,
    True, cur = enumMap[#2]
  ];
  AssociateTo[enumMap, #1 -> cur])&
  ,
  importedTokenEnumSource
];

(*
sanity check that all tokens are in order
*)
cur = -Infinity;
KeyValueMap[
  If[!TrueQ[#2 >= cur],
    Print["Token is out of order: ", #1 -> #2];
    Quit[1]
    ,
    cur = #2
  ]&
  ,
  enumMap
];

(*
remove values like Error`First in:
<|
Error`Unknown -> Next,
Error`First -> Error`Unknown,
|>

because C switch statements cannot have duplicate cases

*)
uniqueEnums = DeleteCases[importedTokenEnumSource, v_ /; !IntegerQ[v] && UnsameQ[v, Next]];

tokens = Keys[uniqueEnums]



(*
Use the System` context symbols for literals when we can

These are compiled as SYMBOL_FOO
*)
tokenToSymbol[Token`EndOfFile] = "Symbol`EndOfFile"
tokenToSymbol[Token`Symbol] = "Symbol`Symbol"
tokenToSymbol[Token`String] = "Symbol`String"
tokenToSymbol[Token`Integer] = "Symbol`Integer"
tokenToSymbol[Token`Real] = "Symbol`Real"
tokenToSymbol[Token`Rational] = "Symbol`Rational"

tokenToSymbol[Token`Whitespace] = "Symbol`Whitespace"


(*
For parser code, there is a distinction between toplevel and internal newlines

But once a token Expr is actually created, just treat them both the same
*)
tokenToSymbol[Token`ToplevelNewline] = "Symbol`Token`Newline"
tokenToSymbol[Token`InternalNewline] = "Symbol`Token`Newline"

(*
everything else will be Symbol`Token`Foo

which is compiled as SYMBOL_TOKEN_FOO
*)
tokenToSymbol[s_] := "Symbol`"<>ToString[s]










isPossibleBeginning[Token`Symbol] = True
isPossibleBeginning[Token`String] = True
isPossibleBeginning[Token`Integer] = True
isPossibleBeginning[Token`Real] = True
isPossibleBeginning[Token`Rational] = True
isPossibleBeginning[Token`LinearSyntaxBlob] = True

isPossibleBeginning[Token`Percent] = True
isPossibleBeginning[Token`PercentPercent] = True

isPossibleBeginning[Token`Hash] = True
isPossibleBeginning[Token`HashHash] = True

isPossibleBeginning[Token`Under] = True
isPossibleBeginning[Token`UnderUnder] = True
isPossibleBeginning[Token`UnderUnderUnder] = True
isPossibleBeginning[Token`UnderDot] = True

isPossibleBeginning[Token`SemiSemi] = True

(*
Prefix operators
*)
isPossibleBeginning[Token`Bang] = True
isPossibleBeginning[Token`Minus] = True
isPossibleBeginning[Token`Plus] = True
isPossibleBeginning[Token`LessLess] = True
isPossibleBeginning[Token`MinusMinus] = True
isPossibleBeginning[Token`PlusPlus] = True
isPossibleBeginning[Token`BangBang] = True

(*
Openers
*)
isPossibleBeginning[Token`OpenParen] = True
isPossibleBeginning[Token`OpenCurly] = True
isPossibleBeginning[Token`LessBar] = True
isPossibleBeginning[Token`LongName`LeftCeiling] = True
isPossibleBeginning[Token`LongName`LeftFloor] = True
isPossibleBeginning[Token`LongName`LeftAngleBracket] = True
isPossibleBeginning[Token`LongName`LeftBracketingBar] = True
isPossibleBeginning[Token`LongName`LeftDoubleBracketingBar] = True
isPossibleBeginning[Token`LongName`LeftAssociation] = True
isPossibleBeginning[Token`LongName`OpenCurlyQuote] = True
isPossibleBeginning[Token`LongName`OpenCurlyDoubleQuote] = True
(*
these openers are Call syntax and NOT possible beginning

isPossibleBeginning[Token`OpenSquare] = True
isPossibleBeginning[Token`ColonColonOpenSquare] = True
isPossibleBeginning[Token`LongName`LeftDoubleBracket] = True
*)

(*
Prefix linear syntax operators
*)
isPossibleBeginning[Token`LinearSyntax`Bang] = True


(*
Integration operators
*)
isPossibleBeginning[Token`LongName`Integral] = True
isPossibleBeginning[Token`LongName`ContourIntegral] = True
isPossibleBeginning[Token`LongName`DoubleContourIntegral] = True
isPossibleBeginning[Token`LongName`ClockwiseContourIntegral] = True
isPossibleBeginning[Token`LongName`CounterClockwiseContourIntegral] = True

(*
Prefix LongName operators
*)
isPossibleBeginning[Token`LongName`Not] = True
isPossibleBeginning[Token`LongName`PlusMinus] = True
isPossibleBeginning[Token`LongName`Sum] = True
isPossibleBeginning[Token`LongName`ForAll] = True
isPossibleBeginning[Token`LongName`Exists] = True
isPossibleBeginning[Token`LongName`NotExists] = True
isPossibleBeginning[Token`LongName`Del] = True
isPossibleBeginning[Token`LongName`Product] = True
isPossibleBeginning[Token`LongName`Coproduct] = True
isPossibleBeginning[Token`LongName`Minus] = True
isPossibleBeginning[Token`LongName`MinusPlus] = True
isPossibleBeginning[Token`LongName`Sqrt] = True
isPossibleBeginning[Token`LongName`CubeRoot] = True
isPossibleBeginning[Token`LongName`CircleTimes] = True
isPossibleBeginning[Token`LongName`Piecewise] = True
isPossibleBeginning[Token`LongName`InvisiblePrefixScriptBase] = True
isPossibleBeginning[Token`LongName`ContinuedFractionK] = True
isPossibleBeginning[Token`LongName`ProbabilityPr] = True
isPossibleBeginning[Token`LongName`ExpectationE] = True
isPossibleBeginning[Token`LongName`CapitalDifferentialD] = True
isPossibleBeginning[Token`LongName`DifferentialD] = True
isPossibleBeginning[Token`LongName`Square] = True

(*
Anything else
*)
isPossibleBeginning[_] = False









isCloser[Token`BarGreater] = True
isCloser[Token`CloseCurly] = True
isCloser[Token`CloseParen] = True
isCloser[Token`CloseSquare] = True
isCloser[Token`LongName`CloseCurlyDoubleQuote] = True
isCloser[Token`LongName`CloseCurlyQuote] = True
isCloser[Token`LongName`RightAngleBracket] = True
isCloser[Token`LongName`RightAssociation] = True
isCloser[Token`LongName`RightBracketingBar] = True
isCloser[Token`LongName`RightCeiling] = True
isCloser[Token`LongName`RightDoubleBracket] = True
isCloser[Token`LongName`RightDoubleBracketingBar] = True
isCloser[Token`LongName`RightFloor] = True

isCloser[_] = False





isError[Token`Error`First] = True
isError[Token`Error`Unknown] = True
isError[Token`Error`ExpectedEqual] = True
isError[Token`Error`Number] = True
isError[Token`Error`UnhandledCharacter] = True
isError[Token`Error`ExpectedLetterlike] = True
isError[Token`Error`Aborted] = True
isError[Token`Error`ExpectedOperand] = True
isError[Token`Error`ExpectedTag] = True
isError[Token`Error`ExpectedFile] = True
isError[Token`Error`UnsupportedToken] = True
isError[Token`Error`UnexpectedCloser] = True
isError[Token`Error`UnterminatedComment] = True
isError[Token`Error`UnterminatedString] = True
isError[Token`Error`UnterminatedFileString] = True
isError[Token`Error`UnterminatedLinearSyntaxBlob] = True
isError[Token`Error`PrefixImplicitNull] = True
isError[Token`Error`InfixImplicitNull] = True
isError[Token`Error`UnsafeCharacterEncoding] = True
isError[Token`Error`UnexpectedCommentCloser] = True
isError[Token`Error`End] = True

isError[_] = False















isTrivia[Token`Comment] = True
isTrivia[Token`ToplevelNewline] = True
isTrivia[Token`InternalNewline] = True
isTrivia[Token`Whitespace] = True

isTrivia[_] = False







$isEmptyTokens = {
  (*
  EndOfFile is not empty
  It is a single byte 0xff

  Token`EndOfFile,
  *)
  Token`Fake`ImplicitTimes,
  Token`Error`Aborted,
  Token`Fake`ImplicitNull,
  Token`Fake`ImplicitOne,
  Token`Fake`ImplicitAll,
  Token`Error`ExpectedOperand,
  Token`Error`ExpectedTag,
  Token`Error`ExpectedFile,
  Token`Error`PrefixImplicitNull,
  Token`Error`InfixImplicitNull
  (*
  Newlines are not empty

  Token`ToplevelNewline
  Token`InternalNewline
  *)
}

isEmpty[tok_ /; MemberQ[$isEmptyTokens, tok]] := True

isEmpty[_] = False


group1Bits[tok_] := group1Bits[tok] =
Which[
  isPossibleBeginning[tok], BitShiftLeft[2^^01, 9],
  isCloser[tok],            BitShiftLeft[2^^10, 9],
  isError[tok],             BitShiftLeft[2^^11, 9],
  True,                     BitShiftLeft[2^^00, 9]
]


group2Bits[tok_] := group2Bits[tok] =
Which[
  isEmpty[tok],         BitShiftLeft[2^^01, 9 + 2],
(*unused,               BitShiftLeft[2^^10, 9 + 2],*)
(*unused,               BitShiftLeft[2^^11, 9 + 2],*)
  True,                 BitShiftLeft[2^^00, 9 + 2]
]


tokenToSymbolCases = Row[{"case ", toGlobal[#], ".value(): return ", toGlobal[tokenToSymbol[#]], ";"}]& /@ tokens


tokenIsEmptyCases = Row[{"tokenIsEmpty", "[", ToString[#], "]", " ", "=", " ", "True"}]& /@ $isEmptyTokens


generate[] := (

Print["Generating TokenEnum..."];

tokenEnumRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"TokenEnum.h\"

#include \"Symbol.h\"


Symbol TokenToSymbol(TokenEnum T);

//
// All token enums
//"} ~Join~
KeyValueMap[(
  Row[{"constexpr TokenEnum ", toGlobal[#1], "(",
    BitOr[
      group2Bits[#1],
      group1Bits[#1],
      #2
    ], "); // { group2Bits:", group2Bits[#1], ", group1Bits:", group1Bits[#1], ", enum:", #2, ", ", StringJoin["0b", {StringTake[#, {1, 1}], "_", StringTake[#, {2, 5}], "_", StringTake[#, {6, 9}]}&[IntegerString[#2, 2, 9]]], " }"
  }])&
  ,
  enumMap
] ~Join~ {
};

Print["exporting TokenEnumRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "TokenEnumRegistration.h"}], Column[tokenEnumRegistrationCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];


tokenEnumRegistrationCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"TokenEnumRegistration.h\"

#include \"SymbolRegistration.h\"
#include \"TokenEnum.h\" // for Closer

#include <cassert>

//
// TOKEN_INTEGER must be 0x4 to allow setting the 0b1 bit to convert to TOKEN_REAL, and 0b10 bit to convert to TOKEN_RATIONAL
//
static_assert(TOKEN_INTEGER.value() == 0x4, \"Check your assumptions\");
static_assert(TOKEN_REAL.value() == 0x5, \"Check your assumptions\");
static_assert(TOKEN_RATIONAL.value() == 0x6, \"Check your assumptions\");

//
// TOKEN_INTERNALNEWLINE must be 0x8 to allow setting the 0b100 bit to convert to TOKEN_TOPLEVELNEWLINE
//
static_assert(TOKEN_INTERNALNEWLINE.value() == 0x8, \"Check your assumptions\");
static_assert(TOKEN_TOPLEVELNEWLINE.value() == 0xc, \"Check your assumptions\");
static_assert(TOKEN_ERROR_FIRST.value() == 0x10, \"Check your assumptions\");

//
// TOKEN_ERROR_UNTERMINATED_FIRST must be 0x1c to allow checking 0b0_0001_11xx for isUnterminated
//
static_assert(TOKEN_ERROR_UNTERMINATED_FIRST.value() == 0x1c, \"Check your assumptions\");
static_assert(TOKEN_ERROR_UNTERMINATED_END.value() == 0x20, \"Check your assumptions\");
"} ~Join~

{"Symbol TokenToSymbol(TokenEnum T) {"} ~Join~
{"switch (T.value()) {"} ~Join~
tokenToSymbolCases ~Join~
{"}",
"assert(false && \"Unhandled token type\");",
"return SYMBOL_TOKEN_UNKNOWN;",
"}",
""};


Print["exporting TokenEnumRegistration.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "TokenEnumRegistration.cpp"}], Column[tokenEnumRegistrationCPPSource], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

tokenEnumWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

BeginPackage[\"CodeParser`TokenEnum`\"]

tokenIsEmpty

Begin[\"`Private`\"]

Needs[\"CodeParser`\"]
Needs[\"CodeParser`Utils`\"]
"} ~Join~

tokenIsEmptyCases ~Join~

{"
tokenIsEmpty[_] = False

End[]

EndPackage[]
"};

Print["exporting TokenEnum.wl"];
res = Export[FileNameJoin[{buildDir, "paclet", "CodeParser", "Kernel", "TokenEnum.wl"}], Column[tokenEnumWL], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done TokenEnum"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
