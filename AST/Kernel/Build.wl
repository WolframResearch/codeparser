BeginPackage["AST`Build`"]

toGlobal

longNameToCharacterCode


longNameToHexDigits


validateLongNameMap



checkBug321344



Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]



(*
uppercases and replaces ` with _
*)
toGlobal[n_] := 
 StringReplace[ToUpperCase[ToString[n]], {"`" -> "_", "$" -> "_"}]




(*
specify the long names that are not in earlier, supported versions

e.g., Generate.wl may be run in a 11.0 kernel, but the target may be a 12.1 kernel
So we want to recognize characters that are not in 11.0 while building with 11.0

*)
(*
added in 11.1:
TwoWayRule
*)
longNameToCharacterCode["TwoWayRule"] = 16^^f120
(*
added in 11.2:
Limit
MaxLimit
MinLimit
*)
longNameToCharacterCode["Limit"] = 16^^f438
longNameToCharacterCode["MaxLimit"] = 16^^f439
longNameToCharacterCode["MinLimit"] = 16^^f43a
(*
added in 12.0:
VectorGreater
VectorGreaterEqual
VectorLess
VectorLessEqual
*)
longNameToCharacterCode["VectorGreater"] = 16^^f434
longNameToCharacterCode["VectorGreaterEqual"] = 16^^f435
longNameToCharacterCode["VectorLess"] = 16^^f436
longNameToCharacterCode["VectorLessEqual"] = 16^^f437


(*
specify the long names that are not supported characters
*)
longNameToCharacterCode["COMPATIBILITYKanjiSpace"] = 16^^3000
longNameToCharacterCode["COMPATIBILITYNoBreak"] = 16^^f3a2
longNameToCharacterCode["NumberComma"] = 16^^f7fc


longNameToCharacterCode[longName_String] :=
  ToCharacterCode[ToExpression["\"\\[" <> longName <> "]\""]][[1]]


(*
longNameToHexDigits["Alpha"] is "03b1"
*)
longNameToHexDigits[longName_String] :=
  IntegerString[longNameToCharacterCode[longName], 16, 4]

integerToHexDigits[int_Integer] :=
  IntegerString[int, 16, 4]




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






End[]

EndPackage[]
