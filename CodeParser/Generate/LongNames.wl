BeginPackage["CodeParser`Generate`LongNames`"]

Begin["`Private`"]

Needs["CodeParser`Generate`GenerateSources`"]



escapeString[s_] :=
  ToString[s, InputForm, CharacterEncoding -> "ASCII"]




Print["Generating LongNames..."]

Check[
longNameDefines = ("constexpr codepoint " <> toGlobal["CodePoint`LongName`" <> #] <> "(" <> longNameToHexDigits[#] <> ");")& /@ Keys[importedLongNames]
,
Print["Message while generating LongNames"];
Quit[1]
]





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



$WorkaroundUnreportedSortBug1 = checkUnreportedSortBug1[]
Print["Work around unreported Sort bug1: ", $WorkaroundUnreportedSortBug1];


(*
Yes, this slower than it needs to be
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


If[$WorkaroundUnreportedSortBug1,
  lexSort = bubbleLexSort
  ,
  (*
  TODO: v12.0 introduced SortBy[list, f, p]
  when targeting v12.0 as a minimum, then can use SortBy[list, ToCharacterCode, lexOrderingForLists]
  *)
  lexSort = Sort[#, lexOrdering]&
]



(*
\r\n is technically multi-byte...

Put CodePoint`CRLF before actual code points
*)
mbNewlines = toGlobal /@ ( { CodePoint`CRLF } ~Join~ ( ("CodePoint`LongName`"<>#)& /@ SortBy[importedNewlineLongNames, longNameToCharacterCode]))


notStrangeLetterlikeSource = 
  {
    "//",
    "//",
    "//",
    "std::array<codepoint, MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT> mbNotStrangeLetterlikeCodePoints {{"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ SortBy[importedNotStrangeLetterlikeLongNames, longNameToCharacterCode]) ~Join~
    {"}};",
    "",
    "//",
    "//",
    "//",
    "bool LongNames::isMBNotStrangeLetterlike(codepoint point) { ",
    "auto it = std::lower_bound(mbNotStrangeLetterlikeCodePoints.begin(), mbNotStrangeLetterlikeCodePoints.end(), point);",
    "return it != mbNotStrangeLetterlikeCodePoints.end() && *it == point;",
    "}",
    ""
  }

asciiReplacementsSource = 
  {
    "//",
    "//",
    "//",
    "std::map<codepoint, std::vector<std::string>> asciiReplacementsMap {{"} ~Join~
    (Row[{"{", toGlobal["CodePoint`LongName`"<>#[[1]]], ", ", escapeString[#[[2]]], "}", ","}]& /@ SortBy[importedASCIIReplacements, longNameToCharacterCode[#[[1]]]&]) ~Join~
    {"}};",
    "",
    "//",
    "//",
    "//",
    "std::vector<std::string> LongNames::asciiReplacements(codepoint point) { ",
    "auto it = asciiReplacementsMap.find(point);",
    "return (it != asciiReplacementsMap.end()) ? it->second : std::vector<std::string>{};",
    "}", ""}

replacementGraphicalSource =
  {
    "//",
    "//",
    "//",
    "std::string LongNames::replacementGraphical(std::string replacement) {",
    "  if (replacement == \" \") {",
    "    //",
    "    // \\[SpaceIndicator]",
    "    //",
    "",
    "    // this was:",
    "    // return \"\\u2423\";",
    "    //",
    "    // But MSVC gave:",
    "    // warning C4566: character represented by universal-character-name '\\u2423' cannot be represented in the current code page (1252)",
    "    //",
    "",
    "    //",
    "    // UTF-8 bytes for U+2423",
    "    //",
    "    return \"\\xe2\\x90\\xa3\";",
    "  } else if (replacement == \"\\n\") {",
    "    return \"\\\\n\";",
    "  } else {",
    "    return replacement;",
    "  }",
    "}",
    ""
  }

punctuationSource = 
  {
    "//",
    "//",
    "//",
    "std::array<codepoint, MBPUNCTUATIONCODEPOINTS_COUNT> mbPunctuationCodePoints {{"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ SortBy[importedPunctuationLongNames, longNameToCharacterCode]) ~Join~
    {"}};", "",
    "//",
    "//",
    "//",
    "bool LongNames::isMBPunctuation(codepoint point) { ",
    "auto it = std::lower_bound(mbPunctuationCodePoints.begin(), mbPunctuationCodePoints.end(), point);",
    "return it != mbPunctuationCodePoints.end() && *it == point;",
    "}",
    ""
  }

whitespaceSource = 
  {
    "//",
    "//",
    "//",
    "std::array<codepoint, MBWHITESPACECODEPOINTS_COUNT> mbWhitespaceCodePoints {{"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ SortBy[importedWhitespaceLongNames, longNameToCharacterCode]) ~Join~
    {"}};", "",
    "//",
    "//",
    "//",
    "bool LongNames::isMBWhitespace(codepoint point) {",
    "auto it = std::lower_bound(mbWhitespaceCodePoints.begin(), mbWhitespaceCodePoints.end(), point);",
    "return it != mbWhitespaceCodePoints.end() && *it == point;",
    "}",
    ""
  }

newlineSource = 
  {
    "//",
    "//",
    "//",
    "std::array<codepoint, MBNEWLINECODEPOINTS_COUNT> mbNewlineCodePoints {{"} ~Join~
    (Row[{#, ","}]& /@ mbNewlines) ~Join~
    {"}};", "",
    "//",
    "//",
    "//",
    "bool LongNames::isMBNewline(codepoint point) {",
    "auto it = std::lower_bound(mbNewlineCodePoints.begin(), mbNewlineCodePoints.end(), point);",
    "return it != mbNewlineCodePoints.end() && *it == point;",
    "}",
    ""
  }

uninterpretableSource = 
  {
    "//",
    "//",
    "//",
    "std::array<codepoint, MBUNINTERPRETABLECODEPOINTS_COUNT> mbUninterpretableCodePoints {{"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ SortBy[importedUninterpretableLongNames, longNameToCharacterCode]) ~Join~
    {"}};", "",
    "//",
    "//",
    "//",
    "bool LongNames::isMBUninterpretable(codepoint point) {",
    "auto it = std::lower_bound(mbUninterpretableCodePoints.begin(), mbUninterpretableCodePoints.end(), point);",
    "return it != mbUninterpretableCodePoints.end() && *it == point;",
    "}",
    ""
  }

unsupportedSource = 
  {
    "//",
    "//",
    "//",
    "std::array<codepoint, UNSUPPORTEDLONGNAMESCODEPOINTS_COUNT> unsupportedLongNameCodePoints {{"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ SortBy[importedUnsupportedLongNames, Identity]) ~Join~
    {"}};", "",
    "//",
    "//",
    "//",
    "bool LongNames::isUnsupportedLongNameCodePoint(codepoint point) {",
    "auto it =  std::lower_bound(unsupportedLongNameCodePoints.begin(), unsupportedLongNameCodePoints.end(), point);",
    "return it != unsupportedLongNameCodePoints.end() && *it == point;",
    "}",
    ""
  }

LongNameCodePointToOperatorSource = 
  {
    "//",
    "//",
    "//",
    "TokenEnum LongNameCodePointToOperator(codepoint c) {",
    "switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
    {
      "default:",
      "assert(false && \"Need to add operator\");",
      "return TOKEN_UNKNOWN;",
      "}",
      "}"
  }









longNamesCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"CodePoint.h\" // for codepoint

#include <string>
#include <array>
#include <map>
#include <vector>

constexpr size_t LONGNAMES_COUNT = " <> ToString[Length[importedLongNames]] <> ";
constexpr size_t RAWLONGNAMES_COUNT = " <> ToString[Length[importedRawLongNames]] <> ";

constexpr size_t MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT = " <> ToString[Length[importedNotStrangeLetterlikeLongNames]] <> ";
constexpr size_t MBPUNCTUATIONCODEPOINTS_COUNT = " <> ToString[Length[importedPunctuationLongNames]] <> ";
constexpr size_t MBWHITESPACECODEPOINTS_COUNT = " <> ToString[Length[importedWhitespaceLongNames]] <> ";
constexpr size_t MBNEWLINECODEPOINTS_COUNT = " <> ToString[Length[mbNewlines]] <> ";
constexpr size_t MBUNINTERPRETABLECODEPOINTS_COUNT = " <> ToString[Length[importedUninterpretableLongNames]] <> ";
constexpr size_t UNSUPPORTEDLONGNAMESCODEPOINTS_COUNT = " <> ToString[Length[importedUnsupportedLongNames]] <> ";

extern std::array<std::string, LONGNAMES_COUNT> LongNameToCodePointMap_names;
extern std::array<codepoint, LONGNAMES_COUNT> LongNameToCodePointMap_points;
extern std::array<codepoint, LONGNAMES_COUNT> CodePointToLongNameMap_points;
extern std::array<std::string, LONGNAMES_COUNT> CodePointToLongNameMap_names;

//
// Collection of utility functions for codepoints and long names
//
class LongNames {
public:
    
    static bool isMBNotStrangeLetterlike(codepoint point);

    static bool isMBPunctuation(codepoint point);

    static bool isMBWhitespace(codepoint point);

    static bool isMBNewline(codepoint point);

    static bool isMBUninterpretable(codepoint point);

    static bool isUnsupportedLongNameCodePoint(codepoint point);

    //
    // Is this \\[Raw] something?
    //
    static bool isRaw(std::string LongNameStr);

    static std::vector<std::string> asciiReplacements(codepoint point);

    static std::string replacementGraphical(std::string replacement);
};

//
// All long name code points
//"} ~Join~
longNameDefines ~Join~
{""}

Print["exporting LongNames.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNames.h"}], Column[longNamesCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]








$lexSortedImportedLongNames = lexSort[Keys[importedLongNames]]




longNameToCodePointMapNames = {
"//",
"//",
"//",
"std::array<std::string, LONGNAMES_COUNT> LongNameToCodePointMap_names {{"} ~Join~
  (Row[{escapeString[#], ","}]& /@ $lexSortedImportedLongNames) ~Join~
  {"}};", ""}

longNameToCodePointMapPoints = {
"//",
"//",
"//",
"std::array<codepoint, LONGNAMES_COUNT> LongNameToCodePointMap_points {{"} ~Join~
  (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ $lexSortedImportedLongNames) ~Join~
  {"}};", ""}

codePointToLongNameMapPoints = {
"//",
"//",
"//",
"std::array<codepoint, LONGNAMES_COUNT> CodePointToLongNameMap_points {{"} ~Join~
  (Row[{toGlobal["CodePoint`LongName`"<>#], ","}] & /@ SortBy[Keys[importedLongNames], longNameToCharacterCode]) ~Join~
  {"}};", ""}

codePointToLongNameMapNames = {
"//",
"//",
"//",
"std::array<std::string, LONGNAMES_COUNT> CodePointToLongNameMap_names {{"} ~Join~
  (Row[{escapeString[#], ","}] & /@ SortBy[Keys[importedLongNames], longNameToCharacterCode]) ~Join~
  {"}};", ""}

rawSet = {
"//",
"//",
"//",
"std::array<std::string, RAWLONGNAMES_COUNT> RawSet {{"} ~Join~
(Row[{"{", "\""<>#<>"\"", "}", ","}]& /@ lexSort[importedRawLongNames]) ~Join~
{"}};",
"
//
//
//
bool LongNames::isRaw(std::string LongNameStr) {
  auto it =  std::lower_bound(RawSet.begin(), RawSet.end(), LongNameStr);
  return it != RawSet.end() && *it == LongNameStr;
}
"}


longNamesCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"LongNames.h\"

#include <algorithm> // for std::lower_bound
#include <cassert>
"} ~Join~
longNameToCodePointMapNames ~Join~
longNameToCodePointMapPoints ~Join~
codePointToLongNameMapPoints ~Join~
codePointToLongNameMapNames ~Join~
rawSet ~Join~
notStrangeLetterlikeSource ~Join~
asciiReplacementsSource ~Join~
replacementGraphicalSource ~Join~
punctuationSource ~Join~
whitespaceSource ~Join~
newlineSource ~Join~
uninterpretableSource ~Join~
unsupportedSource ~Join~
LongNameCodePointToOperatorSource

Print["exporting LongNames.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "LongNames.cpp"}], Column[longNamesCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]









Check[
longNames = ("\"" <> # <> "\", ")& /@ Keys[importedLongNames]
,
Print["Message while generating LongNames"];
Quit[1]
]

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
}

Print["exporting LongNames.wl"]
res = Export[FileNameJoin[{generatedWLDir, "LongNames.wl"}], Column[longNamesWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]




Print["Done LongNames"]

End[]

EndPackage[]
