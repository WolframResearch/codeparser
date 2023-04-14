(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`ExprLibrary`"]

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
Print["Needs[\"Compile`\"]... \[WatchIcon]"];
Needs["Compile`"] (* for Program *)
Needs["TypeFramework`"] (* for MetaData *)


checkBuildDir[]


$sharedExt = 
  Switch[$OperatingSystem, 
    "MacOSX", "dylib",
    "Windows", "dll",
    _, "so"
  ]



(*
bug 424474: CreateFile is broken
*)
checkBug424474[] :=
Module[{file},
  
  file = CreateFile[];

  If[FailureQ[file],
    Print["CreateFile[] failed"];
    Quit[1]
  ];

  DeleteFile[file]
]

checkBug424474[]



ExprLibraryProgram[] :=
Module[{},
  Program[{
    MetaData[<| "Name" -> Expr`Length |>] @
    Function[{Typed[arg1, "Expression"]},
      Length[arg1]
    ]
    ,
    MetaData[<| "Name" -> Expr`ToInteger64 |>] @
    Function[{Typed[arg1, "Expression"]},
      Module[{hand = Native`Handle[]},
        Native`PrimitiveFunction["ExprToInteger64"][hand, arg1];
        Native`Load[hand]
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`FromInteger64 |>] @
    Function[{Typed[arg1, "Integer64"]},
      (*
      used to be Compile`Cast
      *)
      System`Cast[arg1, "Expression"]
    ]
    ,
    MetaData[<| "Name" -> Expr`FromReal64 |>] @
    Function[{Typed[arg1, "Real64"]},
      (*
      used to be Compile`Cast
      *)
      System`Cast[arg1, "Expression"]
    ]
    ,
    MetaData[<| "Name" -> Expr`MEncodedStringToSymbolExpr |>] @
    Function[{Typed[arg1, "MachineInteger"]},
      Module[{cstr, str, sym},
        cstr = Native`BitCast[arg1, "CArray"["UnsignedInteger8"]];
        (*
        used to be:
        str = String`CloneNew[cstr];

        but String`CloneNew was removed
        *)
        str = String`New[CString`Clone[cstr]];
        sym = Native`LookupSymbol[str];
        sym
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`BuildExpr |>] @
    Function[{Typed[head, "Expression"], Typed[length, "MachineInteger"]},
      Module[{ef},
        ef = Native`PrimitiveFunction["CreateHeaded_IE_E"][length, head];
        ef
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`BuildExprA |>] @
    Function[{Typed[head, "Expression"], Typed[length, "MachineInteger"]},
      Module[{ef},
        ef = Native`PrimitiveFunction["CreateHeaded_IE_E"][length, head];
        Memory`Release[head];
        ef
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`Insert |>] @
    Function[{Typed[expr, "Expression"], Typed[index, "MachineInteger"], Typed[part, "Expression"]},
      Native`PrimitiveFunction["SetElement_EIE_Void"][expr, index, part];
      Null
    ]
    ,
    MetaData[<| "Name" -> Expr`InsertA |>] @
    Function[{Typed[expr, "Expression"], Typed[index, "MachineInteger"], Typed[part, "Expression"]},
      Native`PrimitiveFunction["SetElement_EIE_Void"][expr, index, part];
      Memory`Release[part];
      Null
    ]
    ,
    MetaData[<| "Name" -> Expr`Pointer |>] @
    Function[{Typed[arg1, "Expression"]},
      Native`BitCast[Native`BitCast[arg1, "VoidHandle"], "MachineInteger"]
    ]
    ,
    MetaData[<| "Name" -> Expr`FromPointer |>] @
    Function[{Typed[arg1, "MachineInteger"]},
      Native`BitCast[Native`BitCast[arg1, "VoidHandle"], "Expression"]
    ]
    ,
    MetaData[<| "Name" -> Expr`FromPointerA |>] @
    Function[{Typed[arg1, "MachineInteger"]},
      Module[{e},
        e = Native`BitCast[Native`BitCast[arg1, "VoidHandle"], "Expression"];
        Memory`Release[e];
        e
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`Release, "MemoryManagement" -> False |>] @
    Function[{Typed[arg1, "Expression"]},
      Memory`Release[arg1];
      Null
    ]
    ,
    MetaData[<| "Name" -> Expr`UTF8BytesToStringExpr |>] @
    Function[{Typed[arg1, "MachineInteger"], Typed[arg2, "MachineInteger"]},
      Module[{cast1},
        cast1 = Native`BitCast[arg1, "CArray"["UnsignedInteger8"]];
        Native`PrimitiveFunction["UTF8BytesToStringExpression"][cast1, arg2]
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`CStringToIntegerExpr |>] @
    (*
    used to be:
    {Typed[arg1, "MachineInteger"], Typed[arg2, "MachineInteger"], Typed[arg3, "MBool"]}

    but MBool was changed to be 32-bits and started getting these errors:

    Compile::err: TypeError. Cannot find a definition for the function Native`PrimitiveFunction[CStringToIntegerExpr] that takes arguments with the types CArray[UnsignedInteger8], Integer64 and Integer32.
    *)
    Function[{Typed[arg1, "MachineInteger"], Typed[arg2, "MachineInteger"], Typed[arg3, "Boolean"]},
      Module[{e, cast1},
        cast1 = Native`BitCast[arg1, "CString"];
        e = Native`PrimitiveFunction["CStringToIntegerExpr"][cast1, arg2, arg3];
        e
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`LongNameSuggestion |>] @
    Function[{Typed[arg1, "Expression"]},
      Module[{h},
        h = InertExpression[CodeParser`Library`LongNameSuggestion];
        InertEvaluate[Construct[h, arg1]]
      ]
    ]
    ,
    MetaData[<| "Name" -> Expr`StringExprToUTF8Bytes |>] @
    Function[{Typed[arg1, "Expression"], Typed[arg2, "MachineInteger"], Typed[arg3, "MachineInteger"]},
      Module[{cast2, cast3},
        cast2 = Native`BitCast[arg2, "Pointer"["CArray"["UnsignedInteger8"]]];
        cast3 = Native`BitCast[arg3, "Pointer"["MachineInteger"]];
        Native`PrimitiveFunction["StringExpressionToUTF8Bytes"][arg1, cast2, cast3]
      ]
    ]
  }]
]


generate[] :=
Catch[
Catch[
Module[{workingDir, targetDir, prog, compLib, compStart, compEnd,
  env, res},

  Print["Generating ExprLibrary..."];

  workingDir = FileNameJoin[{ buildDir, "ccompilerdriver-working" }];

  targetDir = FileNameJoin[{ buildDir, "paclet", "CodeParser", "LibraryResources", $SystemID }];


  (*
  work-around bug 411375
  *)
  Unprotect[Failure];

  FormatValues[Failure] =.;


  prog = ExprLibraryProgram[];

  Print["Calling CreateCompilerEnvironment[]... \[WatchIcon]"];

  compStart = Now;

  (*
  Create a compiler environment that does not load dependent libraries.

  make sure to avoid dependent libraries which may have abort handling
  *)
  env = CreateCompilerEnvironment["TypeEnvironmentOptions" -> {"AddLibraries" -> None}];

  compEnd = Now;

  Print["CreateCompilerEnvironment[] time: ", ToString[compEnd - compStart]];

  Print["Calling CompileToLibrary[]... \[WatchIcon]"];

  compStart = Now;

  Print[];

  Off[CCompilerDriver`CreateLibrary::wddirty];
  
  compLib =
    CompileToLibrary[prog,
      "LibraryName" -> "libexpr",
      "EntryFunctionName" -> "Main",
      "TargetDirectory" -> targetDir,
      "TraceFunction" -> Print,
      (*
      Turn off abort handling in the generated library
      *)
      "AbortHandling" -> False,
      CompilerEnvironment -> env,
      "CreateLibraryOptions" -> {"SystemLibraries" -> {}, "WorkingDirectory" -> workingDir, "CleanIntermediate" -> False}
    ];

  compEnd = Now;

  Print[];

  Print[compLib];

  Print[];
  
  Print["CompileToLibrary[] time: ", ToString[compEnd - compStart]];

  If[FailureQ[compLib],
    Quit[1]
  ];

  If[!MatchQ[compLib, _CompiledLibrary`CompiledLibrary],
    Quit[1]
  ];

  If[!FileExistsQ[FileNameJoin[{targetDir, "libexpr."<>$sharedExt}]],
    Print["libexpr."<>$sharedExt <> " does not exist"];
    Quit[1]
  ];

  If[$OperatingSystem == "Windows",
    
    Print["copying ", FileNameJoin[{workingDir, "libexpr.lib"}], " to ", FileNameJoin[{targetDir, "libexpr.lib"}]];

    res = CopyFile[FileNameJoin[{workingDir, "libexpr.lib"}], FileNameJoin[{targetDir, "libexpr.lib"}], OverwriteTarget -> True];
    
    If[FailureQ[res],
      Quit[1]
    ];
    
    If[!StringQ[res],
      Quit[1]
    ];

    If[!FileExistsQ[FileNameJoin[{targetDir, "libexpr.lib"}]],
      Print["libexpr.lib does not exist"];
      Quit[1]
    ];

    Print["copying ", FileNameJoin[{workingDir, "libexpr.exp"}], " to ", FileNameJoin[{targetDir, "libexpr.exp"}]];

    res = CopyFile[FileNameJoin[{workingDir, "libexpr.exp"}], FileNameJoin[{targetDir, "libexpr.exp"}], OverwriteTarget -> True];
    
    If[FailureQ[res],
      Quit[1]
    ];
    
    If[!StringQ[res],
      Quit[1]
    ];

    If[!FileExistsQ[FileNameJoin[{targetDir, "libexpr.exp"}]],
      Print["libexpr.exp does not exist"];
      Quit[1]
    ];
  ];

  Print["Done ExprLibrary"]
]]
,
_
,
Function[{value, tag},
  Print["Uncaught Throw"];
  Print["value: ", value];
  Print["tag: ", tag];
  Quit[1]
]
]

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]


End[]

EndPackage[]
