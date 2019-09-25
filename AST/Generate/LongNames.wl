BeginPackage["AST`Generate`LongNames`"]

Begin["`Private`"]

Needs["AST`Generate`"]

Print["Generating LongNames..."]

Check[
longNameDefines = ("constexpr int " <> toGlobal["CodePoint`LongName`" <> #] <> "(" <> longNameToHexDigits[#] <> ");")& /@ Keys[importedLongNames]
,
Print["Message while generating LongNameDefines"];
Quit[1]
]


longNameDefinesCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"CodePoint.h\"
"} ~Join~ longNameDefines ~Join~ {""}

Print["exporting LongNameDefines.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNameDefines.h"}], Column[longNameDefinesCPPHeader], "String"]

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
