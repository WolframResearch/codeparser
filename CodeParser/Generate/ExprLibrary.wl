BeginPackage["CodeParser`Generate`ExprLibrary`"]

Begin["`Private`"]

Needs["CodeParser`Generate`GenerateSources`"]
Needs["Compile`"] (* for Program *)
Needs["TypeFramework`"] (* for MetaData *)


ExprLibraryProgram[] :=
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
    MetaData[<|"Exported" -> True, "Name" -> Expr`ToInteger64|>
    ]@Function[{Typed[arg1, "Expression"]},
      Module[{hand = Native`Handle[]},
        Native`PrimitiveFunction["ExprToInteger64"][hand, arg1];
        Native`Load[hand]
      ]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`FromInteger64|>
    ]@Function[{Typed[arg1, "Integer64"]},
      Native`PrimitiveFunction["Integer64ToExpr"][arg1]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`FromReal64|>
    ]@Function[{Typed[arg1, "Real64"]},
      Native`PrimitiveFunction["Real64ToExpr"][arg1]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`MEncodedStringToSymbolExpr|>
    ]@Function[{Typed[arg1, "MachineInteger"]},
      Module[{cstr, str, sym},
        cstr = Native`BitCast[arg1, "CArray"["UnsignedInteger8"]];
        str = String`CloneNew[cstr];
        sym = Native`LookupSymbol[str];
        sym
      ]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`BuildExpr|>
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
      Null
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
      Null
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`UTF8BytesToStringExpr|>
    ]@Function[{Typed[arg1, "MachineInteger"], Typed[arg2, "MachineInteger"]},
      Module[{cast1},
        cast1 = Native`BitCast[arg1, "CArray"["UnsignedInteger8"]];
        Native`PrimitiveFunction["UTF8BytesToStringExpression"][cast1, arg2]
      ]
    ]
    ,
    MetaData[<|"Exported" -> True, "Name" -> Expr`CStringToIntegerExpr|>
    (*
    used to be:
    {Typed[arg1, "MachineInteger"], Typed[arg2, "MachineInteger"], Typed[arg3, "MBool"]}

    but MBool was changed to be 32-bits and started getting these errors:

    Compile::err: TypeError. Cannot find a definition for the function Native`PrimitiveFunction[CStringToIntegerExpr] that takes arguments with the types CArray[UnsignedInteger8], Integer64 and Integer32.
    *)
    ]@Function[{Typed[arg1, "MachineInteger"], Typed[arg2, "MachineInteger"], Typed[arg3, "Boolean"]},
      Module[{e, cast1},
        cast1 = Native`BitCast[arg1, "CString"];
        e = Native`PrimitiveFunction["CStringToIntegerExpr"][cast1, arg2, arg3];
        e
      ]
    ]
  }]
]


buildExprLibrary[] :=
Catch[
Module[{targetDir, prog, compLib},

  Print["Generating ExprLibrary..."];

  If[$VersionNumber < 12.2,
    Print["Skipping ExprLibrary"];
    Throw[Null]
  ];

  targetDir = FileNameJoin[{ buildDir, "paclet", "CodeParser", "LibraryResources", $SystemID }];

  prog = ExprLibraryProgram[];

  Print["Exporting expr shared library... \[WatchIcon]"];

  Print[];

  compLib =
    CompileToLibrary[prog,
      "LibraryName" -> "expr",
      "EntryFunctionName" -> "Main",
      "TargetDirectory" -> targetDir,
      "TraceFunction" -> Print
    ];

  Print["compiled library: ", compLib];

  If[FailureQ[compLib],
    Quit[1]
  ];

  Print["Done ExprLibrary"]
]]

If[script === $InputFileName,
buildExprLibrary[]
]


End[]

EndPackage[]
