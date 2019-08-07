BeginPackage["AST`Generate`LongNameDefines`"]

Begin["`Private`"]

Needs["AST`Generate`"]

Print["Generating LongNameDefines..."]

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

Print["Done LongNameDefines"]

End[]

EndPackage[]
