BeginPackage["AST`Generate`ExpressionLibrary`"]

Begin["`Private`"]


Needs["Compile`"]
Needs["TypeFramework`"]

Print["Generating ExpressionLibrary..."]


packageDir = Directory[]

If[FileNameSplit[packageDir][[-1]] =!= "ast",
  Print["Cannot proceed; Not inside ast directory: ", packageDir];
  Quit[1]
]

buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[FileType[buildDir] =!= Directory,
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]



ExpressionLibraryProgram[] :=
Module[{},
  Program[{
    MetaData[<|"Exported" -> True, "Name" -> "Main"|>
    ]@Function[{},
      True
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`BuildExpression|>
    ]@Function[{Typed[head, "Expression"], Typed[length, "MachineInteger"]},
      Module[{ef},
        ef = Native`PrimitiveFunction["CreateHeaded_IE_E"][length, head];
        ef
      ]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`Insert|>
    ]@Function[{Typed[expr, "Expression"], Typed[index, "MachineInteger"], Typed[part, "Expression"]},
      Module[{},
        Native`PrimitiveFunction["SetElement_EIE_Void"][expr, index, part];
      ]
    ]
  }]
]

buildExpressionLibrary[] /; $VersionNumber < 12.1 := (
  Print["Skipping ExpressionLibrary"]
)

buildExpressionLibrary[] /; $VersionNumber >= 12.1 :=
Module[{targetDir, prog, compLib},

  targetDir = FileNameJoin[{ buildDir, "paclet", "AST", "LibraryResources", $SystemID }];

  prog = ExpressionLibraryProgram[];

  Print["Exporting expr shared library"];

  compLib = CompileToLibrary[prog, "LibraryName" -> "expr", "EntryFunctionName" -> "Main", "TargetDirectory" -> targetDir];

  If[FailureQ[compLib],
    Print[compLib];
    Quit[1]
  ]
]

buildExpressionLibrary[]

Print["Done ExpressionLibrary"]

End[]

EndPackage[]
