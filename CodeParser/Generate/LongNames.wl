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
longNameToHexDigits["RawDoubleQuote"] := "CodePoint::StringMeta_DoubleQuote"
longNameToHexDigits["RawBackslash"] := "CodePoint::StringMeta_Backslash"


(*
longNameToHexDigits["Alpha"] is "0x03b1"
*)
longNameToHexDigits[longName_String] :=
	With[{code = longNameToCharacterCode[longName]},
		"'\\u{" <> IntegerString[code, 16, If[code > 16^^FFFF, 6, 4]] <> "}'"
	]


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

importedASCIIReplacements = KeyValueMap[
	Function[{k, v}, k -> v[[3, Key["ASCIIReplacements"]]]],
	Select[importedLongNames, KeyExistsQ[#[[3]], "ASCIIReplacements"]&]
];

importedPunctuationLongNames = Keys[Select[importedLongNames, (#[[1]] === PunctuationCharacter)&]];

importedWhitespaceLongNames = Keys[Select[importedLongNames, (#[[1]] === WhitespaceCharacter)&]];

importedNewlineLongNames = Keys[Select[importedLongNames, (#[[1]] === NewlineCharacter)&]];

importedUninterpretableLongNames = Keys[Select[importedLongNames, (#[[1]] === UninterpretableCharacter)&]];

importedRawLongNames = Keys[Select[importedLongNames, (#[[1]] === RawCharacter)&]];



Check[
	longNameDefines = Map[
		longName |-> Module[{name, value, type},
			name = toGlobal["CodePoint`LongName`" <> longName];
			value = longNameToHexDigits[longName];
			type = If[StringStartsQ[value, "CodePoint::StringMeta"],
				"CodePoint",
				"char"
			];
			"pub const " <> name <> ": " <> type <> " = " <> value <> ";"
		],
		Keys[importedLongNames]
	]
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
mbNewlines = Map[
	point |-> toGlobal[point, "CodePoint"],
	(
		{ CodePoint`CRLF } ~Join~ (
			("CodePoint`LongName`"<>#)& /@ SortBy[importedNewlineLongNames, longNameToCharacterCode]
		)
	)
];



(*
strings is a flat list of Strings
*)
insertNewlines[strings_] :=
  FoldPairList[If[#1 + StringLength[#2] > 120, {{"\n", #2}, StringLength[#2]}, {#2, #1 + StringLength[#2]}] &, 0, strings]



longNameToCodePointMapNames = {
  "//",
  "//",
  "//",
  "pub const LONGNAME_TO_CODE_POINT_MAP__NAMES: [&str; LONGNAMES_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{escapeString[#], ",", " "}& /@ $lexSortedImportedLongNames]]], "\n"]) ~Join~
  {"];",
  ""}

longNameToCodePointMapPoints = {
  "//",
  "//",
  "//",
  "pub const LONGNAME_TO_CODE_POINT_MAP__POINTS: [CodePoint; LONGNAMES_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#, "CodePoint"], ",", " "}& /@ $lexSortedImportedLongNames]]], "\n"]) ~Join~
  {"];",
  ""}

codePointToLongNameMapPoints = {
  "//",
  "//",
  "//",
  "pub const CODE_POINT_TO_LONGNAME_MAP__POINTS: [CodePoint; LONGNAMES_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#, "CodePoint"], ",", " "}& /@ SortBy[Keys[importedLongNames], longNameToCharacterCode]]]], "\n"]) ~Join~
  {"];",
  ""}

codePointToLongNameMapNames = {
  "//",
  "//",
  "//",
  "pub const CODE_POINT_TO_LONGNAME_MAP__NAMES: [&str; LONGNAMES_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{escapeString[#], ",", " "}& /@ SortBy[Keys[importedLongNames], longNameToCharacterCode]]]], "\n"]) ~Join~
  {"];",
  ""}

rawSet = {
  "//",
  "//",
  "//",
  "pub const RAW_SET: [&str; RAWLONGNAMES_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{"\""<>#<>"\"", ",", " "}& /@ lexSort[importedRawLongNames]]]], "\n"]) ~Join~
  {"];",
  ""}

notStrangeLetterlikeSource = {
  "//",
  "//",
  "//",
  "pub const MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS: [char; MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedNotStrangeLetterlikeLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"];",
  ""}

asciiReplacementsSource = {
  "//",
  "//",
  "//",
  "pub static ASCII_REPLACEMENTS_MAP: Lazy<HashMap<char, &[&str]>> = Lazy::new(|| HashMap::from_iter(["} ~Join~
  (
	Map[
		Row[{#}]&,
		StringSplit[
			StringJoin[
				insertNewlines[Flatten[
					{
						"(",
						toGlobal["CodePoint`LongName`"<>#[[1]]],
						", ",
						(* escapeString[#[[2]]], *)
						Replace[#[[2]], {
							values:{___?StringQ} :> {"[", {escapeString[#], ", "}& /@ values, "].as_slice()"},
							other_ :> Quit[3]
						}],
						")",
						", "}& /@ SortBy[importedASCIIReplacements, longNameToCharacterCode[#[[1]]]&]
				]]
			],
			"\n"
		]
	]
  ) ~Join~
  {"]));",
  ""}

punctuationSource = {
  "//",
  "//",
  "//",
  "pub const MB_PUNCTUATION_CODE_POINTS: [char; MBPUNCTUATIONCODEPOINTS_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedPunctuationLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"];",
  ""};

whitespaceSource = {
  "//",
  "//",
  "//",
  "pub const MB_WHITESPACE_CODE_POINTS: [char; MBWHITESPACECODEPOINTS_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedWhitespaceLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"];",
  ""};

newlineSource = {
  "//",
  "//",
  "//",
  "pub const MB_NEWLINE_CODE_POINTS: [CodePoint; MBNEWLINECODEPOINTS_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{#, ",", " "}& /@ mbNewlines]]], "\n"]) ~Join~
  {"];",
  ""};

uninterpretableSource = {
  "//",
  "//",
  "//",
  "pub const MB_UNINTERPRETABLE_CODE_POINTS: [char; MBUNINTERPRETABLECODEPOINTS_COUNT] = ["} ~Join~
  (Row[{#}]& /@ StringSplit[StringJoin[insertNewlines[Flatten[{toGlobal["CodePoint`LongName`"<>#], ",", " "}& /@ SortBy[importedUninterpretableLongNames, longNameToCharacterCode]]]], "\n"]) ~Join~
  {"];",
  ""};

LongNameCodePointToOperatorSource = {
  "//",
  "//",
  "//",
  "pub(crate) fn LongNameCodePointToOperator(c: char) -> TokenKind {",
  "    match c {"} ~Join~
  (Row[{"        ", toGlobal["CodePoint`LongName`"<>#], " => ", "return", " ", toTokenEnumVariant["Token`LongName`"<>#], ","}]& /@ importedPunctuationLongNames) ~Join~
  {"        _ => panic!(\"Need to add operator\"),"} ~Join~
  {"    }",
  "}",
  ""};


generate[] := (

Print["Generating LongNames..."];


longNamesRegistrationCPPHeader = {
"\
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

const LONGNAMES_COUNT: usize = " <> ToString[Length[importedLongNames]] <> ";
const RAWLONGNAMES_COUNT: usize = " <> ToString[Length[importedRawLongNames]] <> ";

const MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT: usize = " <> ToString[Length[importedNotStrangeLetterlikeLongNames]] <> ";
const MBPUNCTUATIONCODEPOINTS_COUNT: usize = " <> ToString[Length[importedPunctuationLongNames]] <> ";
const MBWHITESPACECODEPOINTS_COUNT: usize = " <> ToString[Length[importedWhitespaceLongNames]] <> ";
const MBNEWLINECODEPOINTS_COUNT: usize = " <> ToString[Length[mbNewlines]] <> ";
const MBUNINTERPRETABLECODEPOINTS_COUNT: usize = " <> ToString[Length[importedUninterpretableLongNames]] <> ";

//
// All long name code points
//"} ~Join~
longNameDefines ~Join~
{""};

(* Print["exporting LongNamesRegistration.h"]; *)
(* res = Export[FileNameJoin[{generatedCPPIncludeDir, "long_names_registration_data.rs"}], Column[longNamesRegistrationCPPHeader], "String"]; *)

(* Print[res]; *)

(* If[FailureQ[res],
  Quit[1]
]; *)


longNamesRegistrationCPPSource = longNamesRegistrationCPPHeader ~Join~ {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::{
	read::code_point::*,
	tokenize::TokenKind::{self, *},
};

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
res = Export[FileNameJoin[{generatedCPPSrcDir, "long_names_registration.rs"}], Column[longNamesRegistrationCPPSource], "String"];

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
res = Export[FileNameJoin[{buildDir, "paclet", "CodeParser", "Resources", "Generated", "LongNames.wl"}], Column[longNamesWL], "String"];

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
