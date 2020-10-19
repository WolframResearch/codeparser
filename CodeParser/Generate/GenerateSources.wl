BeginPackage["CodeParser`Generate`GenerateSources`"]

toGlobal

buildDirFlagPosition

buildDir

srcDirFlagPosition

srcDir

script

generatedCPPDir
generatedCPPIncludeDir
generatedCPPSrcDir

generatedWLDir

dataDir


importedLongNames

importedPrefixParselets

importedInfixParselets

importedNotStrangeLetterlikeLongNames

importedASCIIReplacements

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



buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[!DirectoryQ[buildDir],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

srcDirFlagPosition = FirstPosition[$CommandLine, "-srcDir"]

If[MissingQ[srcDirFlagPosition],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]

srcDir = $CommandLine[[srcDirFlagPosition[[1]] + 1]]

If[!DirectoryQ[srcDir],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]

scriptPosition = FirstPosition[$CommandLine, "-script"]

If[MissingQ[scriptPosition],
  Print["Cannot proceed; Unsupported script"];
  Quit[1]
]

script = $CommandLine[[scriptPosition[[1]] + 1]]



generatedCPPDir = FileNameJoin[{buildDir, "generated", "cpp"}]
generatedCPPIncludeDir = FileNameJoin[{generatedCPPDir, "include"}]
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir, "src", "lib"}]

generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]


dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]



importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]

importedPrefixParselets = Get[FileNameJoin[{dataDir, "PrefixParselets.wl"}]]

importedInfixParselets = Get[FileNameJoin[{dataDir, "InfixParselets.wl"}]]





importedNotStrangeLetterlikeLongNames = Keys[Select[importedLongNames, #[[1]] === LetterlikeCharacter && MemberQ[Lookup[#[[3]], "Extra", {}], "NotStrange"]&]]

importedASCIIReplacements = KeyValueMap[Function[{k, v}, k -> v[[3, Key["ASCIIReplacements"]]]], Select[importedLongNames, KeyExistsQ[#[[3]], "ASCIIReplacements"]&]]

importedPunctuationLongNames = Keys[Select[importedLongNames, #[[1]] === PunctuationCharacter &]]

importedWhitespaceLongNames = Keys[Select[importedLongNames, #[[1]] === WhitespaceCharacter &]]

importedNewlineLongNames = Keys[Select[importedLongNames, #[[1]] === NewlineCharacter &]]

importedUninterpretableLongNames = Keys[Select[importedLongNames, #[[1]] === UninterpretableCharacter &]]

importedUnsupportedLongNames = Keys[Select[importedLongNames, #[[1]] === UnsupportedCharacter &]]

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





End[]

EndPackage[]
