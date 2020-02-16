BeginPackage["CodeParser`Generate`GenerateSources`"]

toGlobal

packageDir

buildDirFlagPosition

buildDir

generatedCPPDir
generatedCPPIncludeDir
generatedCPPSrcDir

generatedWLDir

tablesDir

buildSrcDir


importedLongNames

importedPunctuationLongNames

importedWhitespaceLongNames

importedNewlineLongNames

importedUninterpretableLongNames

importedUnsupportedLongNames

importedRawLongNames



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
specify the long names that are unsupported
*)
longNameToCharacterCode["COMPATIBILITYKanjiSpace"] = 16^^3000
longNameToCharacterCode["COMPATIBILITYNoBreak"] = 16^^f3a2
longNameToCharacterCode["NumberComma"] = 16^^f7fc
longNameToCharacterCode["InlinePart"] = 16^^f51e

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
longNameToHexDigits["Alpha"] is "0x03b1"
*)
longNameToHexDigits[longName_String] :=
  "0x"<>IntegerString[longNameToCharacterCode[longName], 16, 4]






Print["Generating additional required source files..."]


packageDir = Directory[]

If[FileNameSplit[packageDir][[-1]] =!= "codeparser",
  Print["Cannot proceed; Not inside codeparser directory: ", packageDir];
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

generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]


tablesDir = FileNameJoin[{packageDir, "tables"}]

generateSrcDir = FileNameJoin[{packageDir, "CodeParser", "Generate"}]

PrependTo[$Path, generateSrcDir]

If[FailureQ[FindFile["CodeParser`Generate`GenerateSources`"]],
  Print["CodeParser`Generate`GenerateSources` could not be found."];
  Quit[1]
]

Print["Clean..."]

Quiet[DeleteDirectory[generatedCPPDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedCPPDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPIncludeDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPSrcDir], CreateDirectory::filex]

Quiet[DeleteDirectory[generatedWLDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedWLDir], CreateDirectory::filex]

Print["Done Clean"]



importedLongNames = Get[FileNameJoin[{tablesDir, "LongNames.wl"}]]

validateLongNameMap[importedLongNames]

importedPunctuationLongNames = Keys[Select[importedLongNames, # === PunctuationCharacter &]]

importedWhitespaceLongNames = Keys[Select[importedLongNames, # === WhitespaceCharacter &]]

importedNewlineLongNames = Keys[Select[importedLongNames, # === NewlineCharacter &]]

importedUninterpretableLongNames = Keys[Select[importedLongNames, # === UninterpretableCharacter &]]

importedUnsupportedLongNames = Keys[Select[importedLongNames, # === UnsupportedCharacter &]]

importedRawLongNames = Keys[Select[importedLongNames, # === RawCharacter &]]

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






Get["CodeParser`Generate`LongNames`"]

Get["CodeParser`Generate`TokenEnum`"]

Get["CodeParser`Generate`Precedence`"]

Get["CodeParser`Generate`Symbol`"]

Print["Done generating additional required source files"]

End[]

EndPackage[]
