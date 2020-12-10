
If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`Precedence`"]

Begin["`Private`"]

Needs["CodeParser`Generate`Common`"]
Needs["CodeTools`Generate`GenerateSources`"]


associativityToValue[Associativity`NonRight] = 0
associativityToValue[Associativity`Right] = 1


generate[] := (

Print["Generating Precedence..."];

If[FailureQ[importedPrecedenceSource],
  Print[importedPrecedenceSource];
  Quit[1]
];

(*
resolve the symbolic values in the Precedence table to integer values
*)
cur = {0, Associativity`NonRight};
enumMap = <||>;
KeyValueMap[(
  Which[
    Head[#2] === Symbol, cur = enumMap[#2],
    Head[#2[[1]]] === Integer, cur = #2,
    #2[[1]] === Next, cur[[1]]++;cur[[2]] = #2[[2]],
    True, Print["Unhandled precedence"]; Quit[1]
  ];
  AssociateTo[enumMap, #1 -> cur])&
  ,
  importedPrecedenceSource
];


(*
sanity check that all precedences are in order
*)
cur = -Infinity;
KeyValueMap[
  If[!TrueQ[#2[[1]] >= cur],
    Print["Precedence is out of order: ", #1->#2];
    Quit[1]
    ,
    cur = #2[[1]]
  ]&
  ,
  enumMap
];


precedenceCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include <cstdint> // for uint8_t

//
// All levels of precedence
//
enum Precedence : uint8_t {"} ~Join~
   KeyValueMap[(Row[{toGlobal[#1], " = ", BitShiftLeft[#2[[1]], 1] + associativityToValue[#2[[2]]], ",", "// prec: ", #2[[1]], ", assoc: ", #2[[2]]}])&, enumMap] ~Join~
   {"};", ""};

Print["exporting Precedence.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Precedence.h"}], Column[precedenceCPPHeader], "String"];

If[FailureQ[res],
  Print[res];
  Quit[1]
];

Print["Done Precedence"]
)

If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
