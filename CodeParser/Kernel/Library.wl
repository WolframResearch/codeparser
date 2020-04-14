BeginPackage["CodeParser`Library`"]

setupLibraries

loadAllFuncs

libraryFunctionWrapper


(*
library functions calling INTO lib
*)
concreteParseBytesListableFunc
tokenizeBytesListableFunc
concreteParseLeafFunc
safeStringFunc
exprTestFunc

setupLongNamesFunc

SetupLongNames



(*
library functions coming FROM lib
*)
MakeLeafNode
MakeErrorNode

MakePrefixNode
MakeBinaryNode
MakeTernaryNode
MakeInfixNode
MakePostfixNode
MakeGroupNode
MakeCallNode
MakePrefixBinaryNode

MakeBlankNode
MakeBlankSequenceNode
MakeBlankNullSequenceNode
MakePatternBlankNode
MakePatternBlankSequenceNode
MakePatternBlankNullSequenceNode
MakePatternOptionalDefaultNode

MakeSlotNode
MakeSlotSequenceNode

MakeOutNode

MakeSyntaxErrorNode
MakeGroupMissingCloserNode
MakeGroupMissingCloserNeedsReparseNode
MakeAbstractSyntaxErrorNode

MakeSourceCharacterNode
MakeSafeStringNode

MakeSyntaxIssue
MakeFormatIssue
MakeEncodingIssue

MakeReplaceTextCodeAction
MakeInsertTextCodeAction
MakeInsertTextAfterCodeAction
MakeDeleteTextCodeAction
MakeDeleteTriviaCodeAction

(*
SetConcreteParseProgress
*)



(*
Blocked when returning from library
*)
$StructureSrcArgs
parseConvention




$ConcreteParseProgress
$ConcreteParseTime
$ConcreteParseStart


LongNameSuggestion


Begin["`Private`"]

Needs["CodeParser`"]
If[$VersionNumber >= 12.1,
	Needs["CompiledLibrary`"] (* for CompiledLibrary, etc. *)
]
Needs["PacletManager`"] (* for PacletInformation *)


$CodeParserLib := $CodeParserLib =
Catch[
Module[{res},
	
	(*
	If CodeParser was built with an earlier version, then expr lib may not exist
	*)
	If[FileExistsQ[$exprLib],

		If[$VersionNumber < 12.1,
			(*
			Built with 12.1+, and being used in an earlier version

			NOT SUPPORTED!
			*)
			Throw[Failure["CodeParserVersion", <| "Message" -> "CodeParser was built with a newer version and is not backwards-compatible." |>]]
		];

		LibraryLoad[$exprLib];
	];

	res = FindLibrary["CodeParser"];
	If[FailureQ[res],
		Throw[Failure["CodeParserLibraryNotFound", <||>]]
	];
	res
]]

(*
$exprLib := $exprLib =
Catch[
Module[{res},
	res = FindLibrary["expr"];
	If[FailureQ[res],
		Throw[Failure["ExpressionLibraryNotFound", <||>]]
	];
	res
]]
*)

$exprLib

$exprCompiledLib

$exprCompiledLibFuns




$sharedExt = 
	Switch[$OperatingSystem, 
		"MacOSX", "dylib",
		"Windows", "dll",
		_, "so"]

setupLibraries[] :=
Module[{location, libraryResources},
	
	(*
	TODO: when targeting 12.1 as a minimum, then look into doing paclet["AssetLocation", "LibraryResources"] or similar
	*)
	location = "Location" /. PacletInformation["CodeParser"];

	libraryResources = FileNameJoin[{location, "LibraryResources", $SystemID}];

	(*
	This allows expr lib to be found
	*)
	(*
	PrependTo[$LibraryPath, libraryResources];
	*)

	$exprLib = FileNameJoin[{libraryResources, "expr."<>$sharedExt}];

	(*
	If CodeParser was built with an earlier version, then expr lib may not exist
	*)
	If[FileExistsQ[$exprLib],

		If[$VersionNumber < 12.1,
			(*
			Built with 12.1+, and being used in an earlier version

			NOT SUPPORTED!
			*)
			Throw[Failure["CodeParserVersion", <| "Message" -> "CodeParser was built with a newer version and is not backwards-compatible." |>]]
		];

		$exprCompiledLib = CompiledLibrary`CompiledLibrary[$exprLib];

		$exprCompiledLibFuns = CompiledLibrary`CompiledLibraryLoadFunctions[$exprCompiledLib]
	]
]





loadFunc[name_String, params_, ret_] :=
Catch[
Module[{res, loaded, linkObject},

	If[FailureQ[$CodeParserLib],
		Throw[$CodeParserLib]
	];

	If[{params, ret} =!= {LinkObject, LinkObject},

		(*
		"regular" LibraryLink with no MathLink silliness
		*)
		loaded = LibraryFunctionLoad[$CodeParserLib, name, params, ret];

		If[Head[loaded] =!= LibraryFunction,
			Throw[Failure["LibraryFunctionLoad", <|"Result"->loaded|>]]
		];

		Throw[loaded]
	];

	(*
	LibraryLink creates a separate loopback link for each library function
	*)
	res = newestLinkObject[LibraryFunctionLoad[$CodeParserLib, name, params, ret]];

	If[FailureQ[res],
		Throw[res]
	];

	{loaded, linkObject} = res;

	If[FailureQ[loaded],
		Throw[loaded]
	];

	If[Head[loaded] =!= LibraryFunction,
		Throw[Failure["LibraryFunctionLoad", <|"Result"->loaded|>]]
	];

	(*
	send fully-qualified symbol names over the wire
	library->kernel traffic has fully-qualified symbols.
	This allows LibraryLink traffic to work when CodeParser` is not on $ContextPath.
	And we want kernel->library traffic to match this behavior, to minimize surprises.
	Note: this still does not enable sending fully-qualified System` symbols
	bug 283291
	bug 284492
	*)
	MathLink`LinkSetPrintFullSymbols[linkObject, True];

	loaded
]]

loadAllFuncs[] := (

concreteParseBytesListableFunc := (setupLibraries[]; concreteParseBytesListableFunc = loadFunc["ConcreteParseBytes_Listable_LibraryLink", LinkObject, LinkObject]);

tokenizeBytesListableFunc := (setupLibraries[]; tokenizeBytesListableFunc = loadFunc["TokenizeBytes_Listable_LibraryLink", LinkObject, LinkObject]);

concreteParseLeafFunc := (setupLibraries[]; concreteParseLeafFunc = loadFunc["ConcreteParseLeaf_LibraryLink", LinkObject, LinkObject]);

safeStringFunc := (setupLibraries[]; safeStringFunc = loadFunc["SafeString_LibraryLink", LinkObject, LinkObject]);

exprTestFunc := (setupLibraries[]; exprTestFunc = loadFunc["ExprTest_LibraryLink", {}, Integer]);

setupLongNamesFunc := (setupLibraries[]; setupLongNamesFunc = loadFunc["SetupLongNames_LibraryLink", LinkObject, LinkObject]);
)




Attributes[newestLinkObject] = {HoldFirst}

(*
Return the LinkObject that is created when evaluating expr along with the result of evaluating expr

this is all just to find the LinkObject associated with this LibraryFunction

TODO: If there is ever a nicer way to find the LinkObject, then use that
*)
newestLinkObject[expr_] :=
Catch[
Module[{before, after, res, set, first},
	before = Links[];
	(*evaluate*)
	res = expr;
	If[FailureQ[res],
		Throw[res]
	];
	after = Links[];
	If[before == after,
		Throw[Failure["LinksDidNotChange", <||>]]
	];
	set = Complement[after, before];
	If[Length[set] != 1,
		Throw[Failure["InternalLinksError", <|"Before"->before, "After"->after|>]]
	];
	first = set[[1]];
	{res, first}
]]


(*
Handle the errors that may occur when calling LibraryLink functions
*)
libraryFunctionWrapper[libFunc_, args___] :=
Catch[
Module[{res},

	If[FailureQ[libFunc],
		Throw[libFunc]
	];

	(*
	in the event of an abort, force reload of functions
	This will fix the transient error that can happen when an abort occurs
	and the next use throws LIBRARY_FUNCTION_ERROR
	*)
	CheckAbort[
	res = libFunc[args];
	,
	loadAllFuncs[];
	Abort[]
	];

	(*
	There may still be a hiccup when there is a LIBRARY_FUNCTION_ERROR and the next
	use of the function returns unevaluated
	*)
	If[MatchQ[res, _LibraryFunctionError | Verbatim[LibraryFunction][___][___]],
		(*
		Need to specify PageWidth, or else ToString does not do anything with Short
		Related bugs: ?
		*)
		Throw[Failure["LibraryFunctionError",
			<|
				"ShortResult" -> ToString[Short[res], OutputForm, PageWidth -> 100],
				(*
				"ShortArguments" and "Arguments" is really just taking up space to force "FullResult" to be hidden by default
				*)
				"ShortArguments" -> ToString[Short[{args}], OutputForm, PageWidth -> 100],
				"Arguments" -> {args},
				"FullResult" -> res
			|>]]
	];

	res
]]





(*
SetConcreteParseProgress[prog_] := (
	$ConcreteParseProgress = prog;
	If[prog == 100,
		$ConcreteParseTime = Now - $ConcreteParseStart;
	];
)
*)


parseConvention["LineColumn"] = structureSrcArgsLineColumn
parseConvention["SourceCharacterIndex"] = structureSrcArgsSourceCharacterIndex


structureSrcArgsLineColumn[startLine_, startCol_, endLine_, endCol_] := {{startLine, startCol}, {endLine, endCol}}

structureSrcArgsSourceCharacterIndex[startLineIgnored_, startCol_, endLineIgnored_, endCol_] := {startCol, endCol-1}




MakeLeafNode[tag_, payload_, srcArgs___] :=
	LeafNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeErrorNode[tag_, payload_, srcArgs___] :=
	ErrorNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePrefixNode[tag_, payload_, srcArgs___] :=
	PrefixNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeBinaryNode[tag_, payload_, srcArgs___] :=
	BinaryNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeTernaryNode[tag_, payload_, srcArgs___] :=
	TernaryNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeInfixNode[tag_, payload_, srcArgs___] :=
	InfixNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePostfixNode[tag_, payload_, srcArgs___] :=
	PostfixNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeGroupNode[tag_, payload_, srcArgs___] :=
	GroupNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeCallNode[tag_, payload_, srcArgs___] :=
	CallNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePrefixBinaryNode[tag_, payload_, srcArgs___] :=
	PrefixBinaryNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeBlankNode[tag_, payload_, srcArgs___] :=
	BlankNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeBlankSequenceNode[tag_, payload_, srcArgs___] :=
	BlankSequenceNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeBlankNullSequenceNode[tag_, payload_, srcArgs___] :=
	BlankNullSequenceNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePatternBlankNode[tag_, payload_, srcArgs___] :=
	PatternBlankNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePatternBlankSequenceNode[tag_, payload_, srcArgs___] :=
	PatternBlankSequenceNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePatternBlankNullSequenceNode[tag_, payload_, srcArgs___] :=
	PatternBlankNullSequenceNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakePatternOptionalDefaultNode[tag_, payload_, srcArgs___] :=
	PatternOptionalDefaultNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeSlotNode[tag_, payload_, srcArgs___] :=
	SlotNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeSlotSequenceNode[tag_, payload_, srcArgs___] :=
	SlotSequenceNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeOutNode[tag_, payload_, srcArgs___] :=
	OutNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeSyntaxErrorNode[tag_, payload_, srcArgs___] :=
	SyntaxErrorNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeGroupMissingCloserNode[tag_, payload_, srcArgs___] :=
	GroupMissingCloserNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeGroupMissingCloserNeedsReparseNode[tag_, payload_, srcArgs___] :=
	GroupMissingCloserNeedsReparseNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeAbstractSyntaxErrorNode[tag_, payload_, srcArgs___] :=
	AbstractSyntaxErrorNode[tag, payload, <| Source -> $StructureSrcArgs[srcArgs] |>]



MakeSyntaxIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real] :=
	SyntaxIssue[tag, msg, severity, <| Source -> $StructureSrcArgs[srcArgs], ConfidenceLevel -> confidence |>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer, airyness_Real] :=
	FormatIssue[tag, msg, severity, <| Source -> $StructureSrcArgs[srcArgs], CodeFormatter`AirynessLevel -> airyness |>]

(*
Only add CodeActions if there is at least 1
*)
MakeSyntaxIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real, actions:CodeAction[_, _, _]..] :=
	SyntaxIssue[tag, msg, severity, <| Source -> $StructureSrcArgs[srcArgs], ConfidenceLevel -> confidence, CodeActions -> {actions} |>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer, airyness_Real, actions:CodeAction[_, _, _]..] :=
	FormatIssue[tag, msg, severity, <| Source -> $StructureSrcArgs[srcArgs], CodeFormatter`AirynessLevel -> airyness, CodeActions -> {actions} |>]

MakeEncodingIssue[tag_String, msg_String, severity_String, srcArgs___Integer] :=
	EncodingIssue[tag, msg, severity, <| Source -> $StructureSrcArgs[srcArgs], CodeFormatter`AirynessLevel -> 0.0 |>]


MakeReplaceTextCodeAction[label_String, srcArgs___Integer, replacementText_String] :=
	CodeAction[label, ReplaceText, <| Source -> $StructureSrcArgs[srcArgs], "ReplacementText" -> replacementText |>]

MakeInsertTextCodeAction[label_String, srcArgs___Integer, insertionText_String] :=
	CodeAction[label, InsertText, <| Source -> $StructureSrcArgs[srcArgs], "InsertionText" -> insertionText |>]

MakeInsertTextAfterCodeAction[label_String, srcArgs___Integer, insertionText_String] :=
	CodeAction[label, InsertTextAfter, <| Source -> $StructureSrcArgs[srcArgs], "InsertionText" -> insertionText |>]

MakeDeleteTextCodeAction[label_String, srcArgs___Integer] :=
	CodeAction[label, DeleteText, <| Source -> $StructureSrcArgs[srcArgs] |>]

MakeDeleteTriviaCodeAction[label_String, srcArgs___Integer] :=
	CodeAction[label, DeleteTrivia, <| Source -> $StructureSrcArgs[srcArgs] |>]




MakeSafeStringNode[str_] :=
	str



$longNames

LongNameSuggestion[input_String] :=
Catch[
Module[{nearest, location, longNamesFile},
	If[$Debug,
		Print["input: ", input];
	];

	(*
	lazy initialization of $longNames]
	*)
	If[!ListQ[$longNames],

		(*
		TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "LongNames"]
		*)
		location = "Location" /. PacletInformation["CodeParser"];

		longNamesFile = FileNameJoin[{location, "Resources", "Generated", "LongNames.wl"}];

		$longNames = Quiet[Get[longNamesFile], {Get::noopen}];
		If[FailureQ[$longNames],
			Throw["LongNameSuggestionFailure"]
		];
	];
	
	nearest = Nearest[$longNames, input, {1, 2}];
	If[nearest == {},
		Throw[""]
	];

	If[$Debug,
		Print["nearest: ", nearest[[1]] ];
	];
	nearest[[1]]
]]



SetupLongNames[] :=
Catch[
Module[{names, documentedNames, undocumentedNames, location, longNamesFile},

	(*
	lazy initialization of $longNames]
	*)
	If[!ListQ[$longNames],

		(*
		TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "LongNames"]
		*)
		location = "Location" /. PacletInformation["CodeParser"];

		longNamesFile = FileNameJoin[{location, "Resources", "Generated", "LongNames.wl"}];

		$longNames = Quiet[Get[longNamesFile], {Get::noopen}];
		If[FailureQ[$longNames],
			Throw[{"UndocumentedLongNameFailure"}]
		];
	];

	SetDirectory[FileNameJoin[{$InstallationDirectory, "Documentation/English/System/ReferencePages/Characters"}]];

	names = FileNames["*.nb", "", Infinity];

	documentedNames = StringDrop[#, -3] & /@ names;

	undocumentedNames = Complement[$longNames, documentedNames];

	ResetDirectory[];

	libraryFunctionWrapper[setupLongNamesFunc, undocumentedNames]
]]




End[]

EndPackage[]
