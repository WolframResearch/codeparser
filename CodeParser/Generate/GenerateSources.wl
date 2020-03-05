BeginPackage["CodeParser`Generate`GenerateSources`"]

toGlobal

buildDirFlagPosition

buildDir

srcDirFlagPosition

srcDir

generatedCPPDir
generatedCPPIncludeDir
generatedCPPSrcDir

generatedWLDir

dataDir


importedLongNames

importedPunctuationLongNames

importedWhitespaceLongNames

importedNewlineLongNames

importedUninterpretableLongNames

importedUnsupportedLongNames
importedUnsupportedLongNameCodePoints

importedRawLongNames



importedTokenEnumSource

importedPrecedenceSource

tokens

longNameToCharacterCode

longNameToHexDigits

codePointToHexDigits


Begin["`Private`"]

(*
uppercases and replaces ` with _
*)
toGlobal[n_] := 
 StringReplace[ToUpperCase[ToString[n]], {"`" -> "_", "$" -> "_"}]

longNameToCharacterCode[name_] := importedLongNames[name][[2]]

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
Map into string meta characters
*)
longNameToHexDigits["RawDoubleQuote"] := "CODEPOINT_STRINGMETA_DOUBLEQUOTE"
longNameToHexDigits["RawBackslash"] := "CODEPOINT_STRINGMETA_BACKSLASH"


(*
longNameToHexDigits["Alpha"] is "0x03b1"
*)
longNameToHexDigits[longName_String] :=
  "0x"<>IntegerString[longNameToCharacterCode[longName], 16, 4]

codePointToHexDigits[point_Integer] :=
Which[
  point <= 16^^ff,
    "0x"<>IntegerString[point, 16, 2]
  ,
  point <= 16^^ffff,
    "0x"<>IntegerString[point, 16, 4]
  ,
  True,
    "0x"<>IntegerString[point, 16, 6]
]





Print["Generating additional required source files..."]


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

srcDirFlagPosition = FirstPosition[$CommandLine, "-srcDir"]

If[MissingQ[srcDirFlagPosition],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]

srcDir = $CommandLine[[srcDirFlagPosition[[1]] + 1]]

If[FileType[srcDir] =!= Directory,
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]




generatedCPPDir = FileNameJoin[{buildDir, "generated", "cpp"}]
generatedCPPIncludeDir = FileNameJoin[{generatedCPPDir, "include"}]
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir, "src", "lib"}]

generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]


dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]

generateDir = FileNameJoin[{srcDir, "CodeParser", "Generate"}]

PrependTo[$Path, generateDir]

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



importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]

validateLongNameMap[importedLongNames]

importedPunctuationLongNames = Keys[Select[importedLongNames, #[[1]] === PunctuationCharacter &]]

importedWhitespaceLongNames = Keys[Select[importedLongNames, #[[1]] === WhitespaceCharacter &]]

importedNewlineLongNames = Keys[Select[importedLongNames, #[[1]] === NewlineCharacter &]]

importedUninterpretableLongNames = Keys[Select[importedLongNames, #[[1]] === UninterpretableCharacter &]]

importedUnsupportedLongNames = Keys[Select[importedLongNames, #[[1]] === UnsupportedCharacter &]]
importedUnsupportedLongNameCodePoints = importedLongNames[[Key[#], 2]]& /@ importedUnsupportedLongNames

importedRawLongNames = Keys[Select[importedLongNames, #[[1]] === RawCharacter &]]

importedPrecedenceSource = Get[FileNameJoin[{dataDir, "Precedence.wl"}]]

If[FailureQ[importedPrecedenceSource],
  Print[importedPrecedenceSource];
  Quit[1]
]

importedTokenEnumSource = Get[FileNameJoin[{dataDir, "TokenEnum.wl"}]]

If[FailureQ[importedTokenEnumSource],
  Print[importedTokenEnumSource];
  Quit[1]
]

(*
remove values like Error`First in:
<|
Error`Unknown -> Next,
Error`First -> Error`Unknown,
|>

because C switch statements cannot have duplicate cases

*)
uniqueEnums = DeleteCases[importedTokenEnumSource, v_ /; !IntegerQ[v] && UnsameQ[v, Next]]

tokens = Keys[uniqueEnums]






Get["CodeParser`Generate`LongNames`"]

Get["CodeParser`Generate`TokenEnum`"]

(*
ParseletRegistration depends on TokenEnum
*)
Get["CodeParser`Generate`ParseletRegistration`"]

Get["CodeParser`Generate`Precedence`"]

Get["CodeParser`Generate`Symbol`"]

Print["Done generating additional required source files"]

End[]

EndPackage[]
