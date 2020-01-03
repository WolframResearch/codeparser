BeginPackage["AST`Generate`ExpressionLibrary`"]

Begin["`Private`"]


Needs["Compile`"] (* for Program *)
Needs["TypeFramework`"] (* for MetaData *)

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
    MetaData[<|"Exported" -> True, "Name" -> Expr`Length|>
    ]@Function[{Typed[arg1, "Expression"]},
      Length[arg1]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`ToInteger|>
    ]@Function[{Typed[arg1, "Expression"]},
      Module[{hand = Native`Handle[]},
        Native`PrimitiveFunction["ExprToInteger64"][hand,  arg1];
        Native`Load[hand]
      ]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`FromInteger|>
    ]@Function[{Typed[arg1, "Integer64"]},
      Native`PrimitiveFunction["Integer64ToExpr"][arg1]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`LookupSymbol|>
    ]@Function[{Typed[arg1, "MachineInteger"]},
      Module[ {cstr = Native`BitCast[arg1, "CString"], str, sym},
        str = String`CloneNew[cstr];
        sym = Native`LookupSymbol[str];
        sym
      ]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`BuildExpression0|>
    ]@Function[{Typed[arg1, "Expression"]},
      Native`BuildExpression[arg1]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`BuildExpression1|>
    ]@Function[{Typed[arg1, "Expression"], Typed[arg2, "Expression"]},
      Native`BuildExpression[arg1, arg2]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`BuildExpression2|>
    ]@Function[{Typed[arg1, "Expression"], Typed[arg2, "Expression"], Typed[arg3, "Expression"]},
      Native`BuildExpression[arg1, arg2, arg3]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`Evaluate|>
    ]@Function[{Typed[arg1, "Expression"]},
      Native`Evaluate[arg1]
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
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`Pointer|>
    ]@Function[{Typed[arg1, "Expression"]},
      Native`BitCast[arg1, "MachineInteger"]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`FromPointer|>
    ]@Function[{Typed[arg1, "MachineInteger"]},
      Native`BitCast[arg1, "Expression"]
    ]
    (*
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`DecrementRefCount|>
    ]@Function[{Typed[expr, "Expression"]},
      Native`PrimitiveFunction["DecrementReferenceCount"][expr];
    ]
    *)
  }]
]


buildExpressionLibrary[] :=
Catch[
Module[{targetDir, prog, compLib},

  If[$VersionNumber < 12.1,
    Print["Skipping ExpressionLibrary"];
    Throw[Null]
  ];

  targetDir = FileNameJoin[{ buildDir, "paclet", "AST", "LibraryResources", $SystemID }];

  prog = ExpressionLibraryProgram[];

  Print["Exporting expr shared library (this might take a while...)"];

  compLib = CompileToLibrary[prog, "LibraryName" -> "expr", "EntryFunctionName" -> "Main", "TargetDirectory" -> targetDir];

  If[FailureQ[compLib],
    Print[compLib];
    Quit[1]
  ]
]]

buildExpressionLibrary[]

Print["Done ExpressionLibrary"]

End[]

EndPackage[]
