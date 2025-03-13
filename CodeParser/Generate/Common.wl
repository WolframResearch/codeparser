BeginPackage["CodeParser`Generate`Common`"]

toGlobal
toTokenEnumVariant

generatedCPPDir
generatedCPPIncludeDir
generatedCPPSrcDir

dataDir

importedPrefixParselets

importedInfixParselets

importedLongNames

importedPrecedenceSource

importedTokenEnumSource

FatalError::usage = "FatalError[expr, ...] prints an error message an exists with a fatal error code."

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeTools`Generate`GenerateSources`"];
]


(*
uppercases and replaces ` with _
*)
toGlobal[n_] :=
  StringReplace[ToUpperCase[ToString[n]], {"`" -> "_", "$" -> "_"}]

toGlobal[n_, "UpperCamelCase"] :=
  StringReplace[ToString[n], {"`" -> "_", "$" -> "_"}]

toTokenEnumVariant[name_] :=
	StringReplace[
		toGlobal[name, "UpperCamelCase"],
		StartOfString ~~ "Token_" -> ""
	]

(* generatedCPPDir = FileNameJoin[{buildDir, "generated", "rust"}] *)
generatedCPPDir = FileNameJoin[{srcDir, "crate", "src", "generated"}]
generatedCPPIncludeDir = FileNameJoin[{generatedCPPDir}]
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir}]

dataDir := dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]

importedPrefixParselets := importedPrefixParselets = Get[FileNameJoin[{dataDir, "PrefixParselets.wl"}]]

importedInfixParselets := importedInfixParselets = Get[FileNameJoin[{dataDir, "InfixParselets.wl"}]]

importedLongNames := importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]

importedPrecedenceSource := importedPrecedenceSource = Get[FileNameJoin[{dataDir, "Precedence.wl"}]]

importedTokenEnumSource := importedTokenEnumSource = Get[FileNameJoin[{dataDir, "TokenEnum.wl"}]]

FatalError[args___] := (
	Print["\n\nFATAL ERROR: ", args, "\n\n"];

	Exit[-1]
)

End[]

EndPackage[]
