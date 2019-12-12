BeginPackage["AST`Generate`ExpressionLibrary`"]

Begin["`Private`"]

Needs["AST`Generate`"]
Needs["Compile`"]
Needs["TypeFramework`"]

Print["Generating ExpressionLibrary..."]

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

buildExpressionLibrary[] /; $VersionNumber < 12.2 := (
  Print["Skipping Expression Library"]
)

buildExpressionLibrary[] /; $VersionNumber >= 12.2 :=
Module[{targetDir, prog, compLib},

  targetDir = FileNameJoin[{ buildDir, "paclet", "AST", "LibraryResources", $SystemID }];

  prog = ExpressionLibraryProgram[];

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
