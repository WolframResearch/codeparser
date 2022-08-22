(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`LongNames`"]

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


(*
Map into string meta characters
*)
longNameToHexDigits["RawDoubleQuote"] := "CODEPOINT_STRINGMETA_DOUBLEQUOTE"
longNameToHexDigits["RawBackslash"] := "CODEPOINT_STRINGMETA_BACKSLASH"


(*
longNameToHexDigits["Alpha"] is "0x03b1"
*)
longNameToHexDigits[longName_String] :=
  ("0x"<>IntegerString[#, 16, If[# > 16^^FFFF, 6, 4]])&[longNameToCharacterCode[longName]]


longNameToCharacterCode[name_] := importedLongNames[name][[2]]


validateLongNameMap[m_] := (
  Print["validating LongName map"];

  If[FailureQ[m],
    Print[m];
    Quit[1]
  ];

  If[!AssociationQ[m],
    Print["LongName map is not an Association"];
    Quit[1]
  ];

  If[!DuplicateFreeQ[Keys[m]],
    Print["LongName map has duplicates"];
    Quit[1]
  ];

  If[!OrderedQ[longNameToCharacterCode /@ Keys[m]],
    Print["LongName map is not ordered"];
    Quit[1]
  ];
)


validateLongNameMap[importedLongNames]



escapeString[s_] :=
  ToString[s, InputForm, CharacterEncoding -> "ASCII"]



(*
Define lexical ordering to use for generated C++ strings

The default Wolfram Language ordering is not the same as C++ lexical ordering

For example, given this list: {"Or", "OSlash", "OTilde", "OverBrace"}

The Wolfram Language ordering is:
{"Or", "OSlash", "OTilde", "OverBrace"}

while the C++ lexical ordering is
{"OSlash", "OTilde", "Or", "OverBrace"}

The differences are:
Wolfram Language ordering DOES consider length of string
C++ lexical ordering does NOT consider length of string

Wolfram Language ordering for letters is {a, A, b, B, c, C, ...} (NOT ASCII ordering)
C++ lexical ordering for letters is {A, B, C, ..., a, b, c, ...} (ASCII ordering)

Punctuation and digits are also ordered differently.

So make sure to call ToCharacterCode to get ASCII codes and
also compare respective characters BEFORE considering string length
*)
lexOrdering["", ""] := 0
lexOrdering["", b_] := 1
lexOrdering[a_, ""] := -1
lexOrdering[a_, b_] :=
  Order[ToCharacterCode[StringTake[a, 1]], ToCharacterCode[StringTake[b, 1]]] /. 
    0 :> lexOrdering[StringDrop[a, 1], StringDrop[b, 1]]

(*

Unreported bug in v11.0:

In[77]:= Sort[{{1, 1}, {1, 3}, {1, 2}}, lexOrdering]

Out[77]= {{1, 1}, {1, 3}, {1, 2}}

Fixed in 11.1

checkUnreportedSortBug1[] sets the flag $WorkaroundUnreportedSortBug1 to True if we still need to workaround unreported Sort bug 1
*)
checkUnreportedSortBug1[] :=
Module[{res},
  res = Sort[{"aa", "ac", "ab"}, lexOrdering];
  Switch[res,
    {"aa", "ac", "ab"},
      True
    ,
    {"aa", "ab", "ac"},
      False
    ,
    _,
    Print["Unhandled result while checking unreported Sort Bug 1: ", res];
    Quit[1]
  ]
]


(*
Yes, this is slower than it needs to be
But it is simple and reliable
*)
bubbleLexSort[listIn_] :=
Module[{list, len, tmp},
  Print["bubbleLexSort... \[WatchIcon]"];
  list = listIn;
  len = Length[list];
  Do[
    If[lexOrdering[list[[i]], list[[j]]] == -1,
      tmp = list[[i]]; 
      list[[i]] = list[[j]];
      list[[j]] = tmp
    ];
    ,
    {i, 1, len}
    ,
    {j, i, len}
  ];
  list
]



Check[
longNames = ("\"" <> # <> "\", ")& /@ Keys[importedLongNames]
,
Print["Message while generating LongNames"];
Quit[1]
];


importedNotStrangeLetterlikeLongNames = Keys[Select[importedLongNames, #[[1]] === LetterlikeCharacter && MemberQ[Lookup[#[[3]], "Extra", {}], "NotStrangeLetterlike"]&]];

importedASCIIReplacements = KeyValueMap[Function[{k, v}, k -> v[[3, Key["ASCIIReplacements"]]]], Select[importedLongNames, KeyExistsQ[#[[3]], "ASCIIReplacements"]&]];

importedPunctuationLongNames = Keys[Select[importedLongNames, (#[[1]] === PunctuationCharacter)&]];

importedWhitespaceLongNames = Keys[Select[importedLongNames, (#[[1]] === WhitespaceCharacter)&]];

importedNewlineLongNames = Keys[Select[importedLongNames, (#[[1]] === NewlineCharacter)&]];

importedUninterpretableLongNames = Keys[Select[importedLongNames, (#[[1]] === UninterpretableCharacter)&]];

importedRawLongNames = Keys[Select[importedLongNames, (#[[1]] === RawCharacter)&]];



Check[
longNameDefines = ("constexpr codepoint " <> toGlobal["CodePoint`LongName`" <> #] <> "(" <> longNameToHexDigits[#] <> ");")& /@ Keys[importedLongNames]
,
Print["Message while generating LongNames"];
Quit[1]
];

$WorkaroundUnreportedSortBug1 = checkUnreportedSortBug1[];
Print["Work around unreported Sort bug1: ", $WorkaroundUnreportedSortBug1];


If[$WorkaroundUnreportedSortBug1,
  lexSort = bubbleLexSort
  ,
  (*
  TODO: v12.0 introduced SortBy[list, f, p]
  when targeting v12.0 as a minimum, then can use SortBy[list, ToCharacterCode, lexOrderingForLists]
  *)
  lexSort = Sort[#, lexOrdering]&
];

$lexSortedImportedLongNames = lexSort[Keys[importedLongNames]];


(*
\r\n is technically multi-byte...

Put CodePoint`CRLF before actual code points
*)
mbNewlines = toGlobal /@ ( { CodePoint`CRLF } ~Join~ ( ("CodePoint`LongName`"<>#)& /@ SortBy[importedNewlineLongNames, longNameToCharacterCode]));



(*
strings is a flat list of Strings
*)
insertNewlines[strings_] :=
  FoldPairList[If[#1 + StringLength[#2] > 120, {{"\n", #2}, StringLength[#2]}, {#2, #1 + StringLength[#2]}] &, 0, strings]



longNameToCodePointMapNames = {
  "//",
  "//",
  "//",
  "std::array<std::string, LONGNAMES_COUNT> LongNameToCodePointMap_names {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{escapeString[#], ",", " "}& /@ $lexSortedImportedLongNames]]], "\n"]) ~Join~
  {"}};",
  ""}

longNameToCodePointMapPoints = {
  "//",
  "//",
  "//",
  "std::array<codepoint, LONGNAMES_COUNT> LongNameToCodePointMap_points {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ $lexSortedImportedLongNames]]], "\n"]) ~Join~
  {"}};",
  ""}

codePointToLongNameMapPoints = {
  "//",
  "//",
  "//",
  "std::array<codepoint, LONGNAMES_COUNT> CodePointToLongNameMap_points {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[Keys[importedLongNames], longNameToCharacterCode]]]], "\n"]) ~Join~
  {"}};",
  ""}

codePointToLongNameMapNames = {
  "//",
  "//",
  "//",
  "std::array<std::string, LONGNAMES_COUNT> CodePointToLongNameMap_names {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{escapeString[#], ",", " "}& /@ SortBy[Keys[importedLongNames], longNameToCharacterCode]]]], "\n"]) ~Join~
  {"}};",
  ""}

rawSet = {
  "//",
  "//",
  "//",
  "std::array<std::string, RAWLONGNAMES_COUNT> RawSet {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{"{", "\""<>#<>"\"", "}", ",", " "}& /@ lexSort[importedRawLongNames]]]], "\n"]) ~Join~
  {"}};",
  ""}

notStrangeLetterlikeSource = {
  "//",
  "//",
  "//",
  "std::array<codepoint, MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT> mbNotStrangeLetterlikeCodePoints {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedNotStrangeLetterlikeLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"}};",
  ""}

asciiReplacementsSource = {
  "//",
  "//",
  "//",
  "std::map<codepoint, std::vector<std::string>> asciiReplacementsMap {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{"{", toGlobal["CodePoint`LongName`"<>#[[1]]], ", ", escapeString[#[[2]]], "}", ",", " "}& /@ SortBy[importedASCIIReplacements, longNameToCharacterCode[#[[1]]]&]]]], "\n"]) ~Join~
  {"}};",
  ""}

punctuationSource = {
  "//",
  "//",
  "//",
  "std::array<codepoint, MBPUNCTUATIONCODEPOINTS_COUNT> mbPunctuationCodePoints {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedPunctuationLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"}};",
  ""};

whitespaceSource = {
  "//",
  "//",
  "//",
  "std::array<codepoint, MBWHITESPACECODEPOINTS_COUNT> mbWhitespaceCodePoints {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedWhitespaceLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"}};",
  ""};

newlineSource = {
  "//",
  "//",
  "//",
  "std::array<codepoint, MBNEWLINECODEPOINTS_COUNT> mbNewlineCodePoints {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{#, ",", " "}& /@ mbNewlines]]], "\n"]) ~Join~
  {"}};",
  ""};

uninterpretableSource = {
  "//",
  "//",
  "//",
  "std::array<codepoint, MBUNINTERPRETABLECODEPOINTS_COUNT> mbUninterpretableCodePoints {{"} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedUninterpretableLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"}};",
  ""};

LongNameCodePointToOperatorSource = {
  "//",
  "//",
  "//",
  "TokenEnum LongNameCodePointToOperator(codepoint c) {",
  "switch (c) {"} ~Join~
  (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
  {"}",
  "assert(false && \"Need to add operator\");",
  "return TOKEN_UNKNOWN;",
  "}",
  ""};


generate[] := (

Print["Generating LongNames..."];


longNamesRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"CodePoint.h\" // for codepoint, CODEPOINT_STRINGMETA_DOUBLEQUOTE, etc.

#include <string>
#include <array>
#include <map>
#include <vector>
#include <cstddef> // for size_t


constexpr size_t LONGNAMES_COUNT = " <> ToString[Length[importedLongNames]] <> ";
constexpr size_t RAWLONGNAMES_COUNT = " <> ToString[Length[importedRawLongNames]] <> ";

constexpr size_t MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT = " <> ToString[Length[importedNotStrangeLetterlikeLongNames]] <> ";
constexpr size_t MBPUNCTUATIONCODEPOINTS_COUNT = " <> ToString[Length[importedPunctuationLongNames]] <> ";
constexpr size_t MBWHITESPACECODEPOINTS_COUNT = " <> ToString[Length[importedWhitespaceLongNames]] <> ";
constexpr size_t MBNEWLINECODEPOINTS_COUNT = " <> ToString[Length[mbNewlines]] <> ";
constexpr size_t MBUNINTERPRETABLECODEPOINTS_COUNT = " <> ToString[Length[importedUninterpretableLongNames]] <> ";

//
//
//
extern std::array<std::string, LONGNAMES_COUNT> LongNameToCodePointMap_names;

//
//
//
extern std::array<codepoint, LONGNAMES_COUNT> LongNameToCodePointMap_points;

//
//
//
extern std::array<codepoint, LONGNAMES_COUNT> CodePointToLongNameMap_points;

//
//
//
extern std::array<std::string, LONGNAMES_COUNT> CodePointToLongNameMap_names;

//
//
//
extern std::array<std::string, RAWLONGNAMES_COUNT> RawSet;

//
//
//
extern std::array<codepoint, MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT> mbNotStrangeLetterlikeCodePoints;

//
//
//
extern std::map<codepoint, std::vector<std::string>> asciiReplacementsMap;

//
//
//
extern std::array<codepoint, MBPUNCTUATIONCODEPOINTS_COUNT> mbPunctuationCodePoints;

//
//
//
extern std::array<codepoint, MBWHITESPACECODEPOINTS_COUNT> mbWhitespaceCodePoints;

//
//
//
extern std::array<codepoint, MBNEWLINECODEPOINTS_COUNT> mbNewlineCodePoints;

//
//
//
extern std::array<codepoint, MBUNINTERPRETABLECODEPOINTS_COUNT> mbUninterpretableCodePoints;

//
//
//
TokenEnum LongNameCodePointToOperator(codepoint c);

//
// All long name code points
//"} ~Join~
longNameDefines ~Join~
{""};

Print["exporting LongNamesRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNamesRegistration.h"}], Column[longNamesRegistrationCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];


longNamesRegistrationCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"LongNamesRegistration.h\"

#include \"TokenEnum.h\"
#include \"TokenEnumRegistration.h\"

#include <algorithm> // for lower_bound
#include <cassert>
"} ~Join~
longNameToCodePointMapNames ~Join~
longNameToCodePointMapPoints ~Join~
codePointToLongNameMapPoints ~Join~
codePointToLongNameMapNames ~Join~
rawSet ~Join~
notStrangeLetterlikeSource ~Join~
asciiReplacementsSource ~Join~
punctuationSource ~Join~
whitespaceSource ~Join~
newlineSource ~Join~
uninterpretableSource ~Join~
LongNameCodePointToOperatorSource;

Print["exporting LongNamesRegistration.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "LongNamesRegistration.cpp"}], Column[longNamesRegistrationCPPSource], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

longNamesWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

{"} ~Join~ longNames ~Join~ {
"Nothing
}
"
};

Print["exporting LongNames.wl"];
res = Export[FileNameJoin[{generatedWLDir, "Resources", "Generated", "LongNames.wl"}], Column[longNamesWL], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done LongNames"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
