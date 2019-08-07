BeginPackage["AST`Generate`"]

toGlobal

packageDir

buildDirFlagPosition

buildDir

generatedCPPDir
generatedCPPIncludeDir
generatedCPPSrcDir

tablesDir

buildSrcDir


importedLongNames

importedPunctuationLongNames

importedSpaceLongNames

importedNewlineLongNames

importedUninterpretableLongNames

importedTokenEnumSource

importedPrecedenceSource

tokens

longNameToCharacterCode

longNameToHexDigits

Begin["`Private`"]

(*
uppercases and replaces ` with _
*)
toGlobal[n_] := 
 StringReplace[ToUpperCase[ToString[n]], {"`" -> "_", "$" -> "_"}]

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
added in 12.1:
CubeRoot
*)
longNameToCharacterCode["CubeRoot"] = 16^^221b

(*
specify the long names that are not supported
*)
longNameToCharacterCode["COMPATIBILITYKanjiSpace"] = 16^^3000
longNameToCharacterCode["COMPATIBILITYNoBreak"] = 16^^f3a2
longNameToCharacterCode["NumberComma"] = 16^^f7fc

(*
everything else
*)
longNameToCharacterCode[longName_String] :=
  ToCharacterCode[ToExpression["\"\\[" <> longName <> "]\""]][[1]]




(*
Map into string meta characters
*)
longNameToHexDigits["RawDoubleQuote"] := "CODEPOINT_STRINGMETA_DOUBLEQUOTE"
longNameToHexDigits["RawBackslash"] := "CODEPOINT_STRINGMETA_BACKSLASH"


(*
longNameToHexDigits["Alpha"] is "03b1"
*)
longNameToHexDigits[longName_String] :=
  "0x"<>IntegerString[longNameToCharacterCode[longName], 16, 4]






Print["Generating additional required C++ files..."]


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


generatedCPPDir = FileNameJoin[{buildDir, "generated", "cpp"}]
generatedCPPIncludeDir = FileNameJoin[{generatedCPPDir, "include"}]
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir, "src", "lib"}]
(*
pacletASTDir = FileNameJoin[{buildDir, "paclet", "AST"}]
*)
tablesDir = FileNameJoin[{packageDir, "tables"}]

generateSrcDir = FileNameJoin[{packageDir, "AST", "Generate"}]

PrependTo[$Path, generateSrcDir]

If[FailureQ[FindFile["AST`Generate`"]],
  Print["AST`Generate` could not be found."];
  Quit[1]
]

(* setup *)
Print["Setup"]
(*
If[!($VersionNumber >= 12.0),
PrependTo[$Path, pacletASTDir];
]
*)
(*
res = PacletDirectoryAdd[pacletASTDir];
Print["PacletDirectoryAdd returned: ", res]
*)
(*
If[FailureQ[FindFile["AST`"]],
  Print["AST could not be found."];
  Quit[1]
]
*)
(*
If[FindFile["AST`"] =!= FileNameJoin[{pacletASTDir, "Kernel", "AST.wl"}],
  Print["Conflicting location for AST was found."];
  Print["Expected to find AST here: ", FileNameJoin[{pacletASTDir, "Kernel", "AST.wl"}]];
  Print["Actually found AST here: ", FindFile["AST`"]];
  If[FindFile["AST`"] === FileNameJoin[{packageDir, "AST", "AST.wl"}],
    Print["It looks like the AST source is being used. This is not supported during build time."];
    Print["There may be a problem with the version of Wolfram Engine that is being used."];
    ,
    Print["Consider running:\nPacletUninstall[\"AST\"]\nto ensure that no other installations of AST interfere with the build."];
  ];
  Quit[1]
]
*)
(*
Catch[
res = Needs[#];

If[FailureQ[res],
  Print["Needs[" <> # <> "] failed: ", res];
  Quit[1];
]
,
_
,
(Print[#];Quit[1])&
]& /@ {"AST`", "AST`Utils`", "AST`Build`"}
*)

(* clean *)
Print["Clean..."]

Quiet[DeleteDirectory[generatedCPPDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedCPPDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPIncludeDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPSrcDir], CreateDirectory::filex]

Print["Done Clean"]



importedLongNames = Get[FileNameJoin[{tablesDir, "LongNames.wl"}]]

validateLongNameMap[importedLongNames]

importedPunctuationLongNames = Keys[Select[importedLongNames, # === Punctuation &]]

importedSpaceLongNames = Keys[Select[importedLongNames, # === Space &]]

importedNewlineLongNames = Keys[Select[importedLongNames, # === Newline &]]

importedUninterpretableLongNames = Keys[Select[importedLongNames, # === Uninterpretable &]]

importedPrecedenceSource = Get[FileNameJoin[{tablesDir, "Precedence.wl"}]]

If[FailureQ[importedPrecedenceSource],
  Print[importedPrecedenceSource];
  Quit[1]
]

importedTokenEnumSource = Get[FileNameJoin[{tablesDir, "TokenEnum.wl"}]]

If[FailureQ[importedTokenEnumSource],
  Print[importedTokenEnumSource];
  Quit[1]
]






Get["AST`Generate`LongNameDefines`"]

Get["AST`Generate`CharacterMaps`"]

Get["AST`Generate`CodePoint`"]

Get["AST`Generate`Token`"]

Get["AST`Generate`Precedence`"]

Get["AST`Generate`Symbol`"]


Print["Done generating additional required C++ files"]

End[]

EndPackage[]
