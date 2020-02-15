BeginPackage["CodeParser`Generate`GenerateExpressionLibrary`"]

Begin["`Private`"]


Needs["Compile`"] (* for Program *)
Needs["TypeFramework`"] (* for MetaData *)

Print[OutputForm["Generating ExpressionLibrary..."]]


packageDir = Directory[]

If[FileNameSplit[packageDir][[-1]] =!= "codeparser",
  Print[OutputForm["Cannot proceed; Not inside codeparser directory: "], packageDir];
  Quit[1]
]

buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print[OutputForm["Cannot proceed; Unsupported build directory"]];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[FileType[buildDir] =!= Directory,
  Print[OutputForm["Cannot proceed; Unsupported build directory"]];
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
      Module[{cstr, str, sym},
        cstr = Native`BitCast[arg1, "CString"];
        str = String`CloneNew[cstr];
        sym = Native`LookupSymbol[str];
        sym
      ]
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
      Native`PrimitiveFunction["SetElement_EIE_Void"][expr, index, part];
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`Pointer|>
    ]@Function[{Typed[arg1, "Expression"]},
      Native`BitCast[Native`BitCast[arg1, "VoidHandle"], "MachineInteger"]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`FromPointer|>
    ]@Function[{Typed[arg1, "MachineInteger"]},
      Native`BitCast[Native`BitCast[arg1, "VoidHandle"], "Expression"]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`Release, "MemoryManagement" -> False|>
    ]@Function[{Typed[arg1, "Expression"]},
      Memory`Release[arg1];
    ]
  }]
]


buildExpressionLibrary[] :=
Catch[
Module[{targetDir, prog, compLib},

  If[$VersionNumber < 12.1,
    Print[OutputForm["Skipping ExpressionLibrary"]];
    Throw[Null]
  ];

  targetDir = FileNameJoin[{ buildDir, "paclet", "CodeParser", "LibraryResources", $SystemID }];

  prog = ExpressionLibraryProgram[];

  Print[OutputForm["Exporting expr shared library... (this might take a while)"]];

  compLib =
    CompileToLibrary[prog,
      "LibraryName" -> "libExpr",
      "EntryFunctionName" -> "Main",
      "TargetDirectory" -> targetDir,
      "TraceFunction" -> Print
    ];

  Print[compLib];

  If[FailureQ[compLib],
    Quit[1]
  ]
]]

buildExpressionLibrary[]

Print[OutputForm["Done ExpressionLibrary"]]

End[]

EndPackage[]
