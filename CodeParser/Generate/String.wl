(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`String`"]

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


strings = Union[Join[
  {"AdditionalDescriptions"},
  {"InsertionText", "ReplacementText"},
  {"UnsafeCharacterEncoding_IncompleteUTF8Sequence",
    "UnsafeCharacterEncoding_StraySurrogate",
    "UnsafeCharacterEncoding_BOM",
    "UnsafeCharacterEncoding_Unknown"},
  (*
  SyntaxIssue Tags
  *)
  {"UnhandledCharacter", "UnsupportedCharacter",
    "UndocumentedCharacter", "UnexpectedEscapeSequence",
    "UnexpectedCharacter", "UnexpectedNewlineCharacter",
    "UnexpectedSpaceCharacter", "UnexpectedLetterlikeCharacter",
    "UndocumentedSlotSyntax", "UnexpectedImplicitTimes",
    "UnexpectedDot", "Comma", "UnexpectedSign", "Ambiguous"},
  (*
  FormatIssue Tags
  *)
  {"Ambiguous"},
  (*
  EncodingIssue Tags
  *)
  {"IncompleteUTF8Sequence", "StraySurrogate", "BOM",
    "UnexpectedCarriageReturn", "UnexpectedCharacter",
    "NonASCIICharacter"},
  (*
  Severities
  *)
  {"Remark", "Warning", "Error", "Fatal", "Formatting"}
]]



generate[] := (

Print["Generating String..."];

myStringRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"MyString.h\"


//
// All strings that are used by CodeParser
//"} ~Join~
MapIndexed[Row[{"constexpr", " ", "MyString", " ", toGlobal["String`"<>#1], "(", "\"", #, "\"", ",", " ", StringLength[#1], ",", " ", ToString[#2[[1]]-1], ")", ";"}]&, strings] ~Join~
{""};

Print["exporting MyStringRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "MyStringRegistration.h"}], Column[myStringRegistrationCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done String"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
