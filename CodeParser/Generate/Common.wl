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
toGlobal[n0_String] := Module[{n = n0},
	(* TODO(cleanup): This is a workaround *)
	If[StringStartsQ[n, "CodePoint`LongName`"],
		n = ToUpperCase[n]
	];

	StringReplace[n, {"`" -> "_", "$" -> "_"}]
]

toGlobal[n_Symbol] := (
	If[StringStartsQ[Context[n], "Precedence`"],
		StringReplace[
			toGlobal[ToUpperCase[ToString[n]]],
			"PRECEDENCE_" -> "Precedence::"
		]
		,
		toGlobal[ToUpperCase[ToString[n]]]
	]
)

toGlobal[n_, "CodePoint"] :=
	Replace[n, {
		CodePoint`CRLF -> "CodePoint::CRLF",
		"CodePoint`LongName`RawDoubleQuote" -> toGlobal[n],
		"CodePoint`LongName`RawBackslash" -> toGlobal[n],
		other_String :> StringJoin["CodePoint::from_char(", toGlobal[other], ")"]
	}]

toGlobal[n_, "UpperCamelCase"] :=
  StringReplace[ToString[n], {"`" -> "_", "$" -> "_"}]


toGlobal[sym_Symbol, "DefinePrecedence"] :=
	StringTrim[toGlobal[sym], "Precedence::"]

toGlobal[args___] := FatalError[{"BAD ARGS: ", args}]

toTokenEnumVariant[name_] :=
	StringReplace[
		toGlobal[name, "UpperCamelCase"],
		StartOfString ~~ "Token_" -> ""
	]

(* generatedCPPDir = FileNameJoin[{buildDir, "generated", "rust"}] *)
generatedCPPDir = FileNameJoin[{srcDir, "crates", "wolfram-parser", "src", "generated"}]
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
