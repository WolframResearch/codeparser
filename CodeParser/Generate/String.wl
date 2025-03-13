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
    "UnsafeCharacterEncoding_BOM"}
]]



generate[] := (

Print["Generating String..."];

myStringRegistrationCPPHeader = {
"\
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(dead_code)]

use crate::my_string::MyString;

//
// All strings that are used by CodeParser
//"} ~Join~
	MapIndexed[
		Row[{
			"pub const ", toGlobal["String`"<>#1], ": MyString = MyString::new(\"", #, "\");"
		}]&,
		strings
	] ~Join~
{""};

Print["exporting MyStringRegistration.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "my_string_registration.rs"}], Column[myStringRegistrationCPPHeader], "String"];

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
