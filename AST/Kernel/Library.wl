BeginPackage["AST`Library`"]

setupLibraries

loadFunc

loadAllFuncs

newestLinkObject

libraryFunctionWrapper


(*
library functions calling INTO lib
*)
concreteParseBytesFunc
tokenizeBytesFunc
tokenizeBytesListableFunc
parseLeafFunc
safeStringFunc
exprTestFunc


(*
library functions coming FROM lib
*)
MakeLeafNode

MakePrefixNode
MakeBinaryNode
MakeTernaryNode
MakeInfixNode
MakePostfixNode
MakeGroupNode
MakeCallNode
MakePrefixBinaryNode

(*
MakeStartOfLineNode
MakeStartOfFileNode
*)
MakeBlankNode
MakeBlankSequenceNode
MakeBlankNullSequenceNode
MakePatternBlankNode
MakePatternBlankSequenceNode
MakePatternBlankNullSequenceNode
MakeOptionalDefaultPatternNode

MakeSyntaxErrorNode
MakeGroupMissingCloserNode
MakeAbstractSyntaxErrorNode

MakeSourceCharacterNode
MakeSafeStringNode

MakeSyntaxIssue
MakeFormatIssue

MakeReplaceTextCodeAction
MakeInsertTextCodeAction
MakeInsertTextAfterCodeAction
MakeDeleteTextCodeAction
MakeDeleteTriviaCodeAction

(*
SetConcreteParseProgress
*)




$ConcreteParseProgress
$ConcreteParseTime
$ConcreteParseStart


LongNameSuggestion
UndocumentedLongNames


Begin["`Private`"]

Needs["AST`"]
Needs["CompiledLibrary`"] (* for CompiledLibrary, etc. *)


$ASTLib := $ASTLib =
Catch[
Module[{res},
	
	LibraryLoad["expr"];

	res = FindLibrary["AST"];
	If[FailureQ[res],
		Throw[Failure["ASTLibraryNotFound", <||>]]
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

$exprCompiledLib

$exprCompiledLibFuns




$sharedExt = 
	Switch[$OperatingSystem, 
		"MacOSX", "dylib",
		"Windows", "dll",
		_, "so"]

setupLibraries[] :=
Module[{location, libraryResources},
	
	location = "Location" /. PacletInformation["AST"];

	libraryResources = FileNameJoin[{location, "LibraryResources", $SystemID}];

	(*
	This allows expr lib to be found
	*)
	(*
	PrependTo[$LibraryPath, libraryResources];
	*)

	$exprCompiledLib = CompiledLibrary[ FileNameJoin[{libraryResources, "expr."<>$sharedExt}] ];

	$exprCompiledLibFuns = CompiledLibraryLoadFunctions[$exprCompiledLib]
]





loadFunc[name_String, params_, ret_] :=
Catch[
Module[{res, loaded, linkObject},

	If[FailureQ[$ASTLib],
		Throw[$ASTLib]
	];

	If[{params, ret} =!= {LinkObject, LinkObject},

		(*
		"regular" LibraryLink with no MathLink silliness
		*)
		loaded = LibraryFunctionLoad[$ASTLib, name, params, ret];

		If[Head[loaded] =!= LibraryFunction,
			Throw[Failure["LibraryFunctionLoad", <|"Result"->loaded|>]]
		];

		Throw[loaded]
	];

	(*
	LibraryLink creates a separate loopback link for each library function
	*)
	res = newestLinkObject[LibraryFunctionLoad[$ASTLib, name, params, ret]];

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
	This allows LibraryLink traffic to work when AST` is not on $ContextPath.
	And we want kernel->library traffic to match this behavior, to minimize surprises.
	Note: this still does not enable sending fully-qualified System` symbols
	bug 283291
	bug 284492
	*)
	MathLink`LinkSetPrintFullSymbols[linkObject, True];

	loaded
]]

loadAllFuncs[] := (

concreteParseBytesFunc := concreteParseBytesFunc = loadFunc["ConcreteParseBytes_LibraryLink", LinkObject, LinkObject];

tokenizeBytesFunc := tokenizeBytesFunc = loadFunc["TokenizeBytes_LibraryLink", LinkObject, LinkObject];

tokenizeBytesListableFunc := tokenizeBytesListableFunc = loadFunc["TokenizeBytes_Listable_LibraryLink", LinkObject, LinkObject];

parseLeafFunc := parseLeafFunc = loadFunc["ParseLeaf_LibraryLink", LinkObject, LinkObject];

safeStringFunc := safeStringFunc = loadFunc["SafeString_LibraryLink", LinkObject, LinkObject];

exprTestFunc := exprTestFunc = loadFunc["ExprTest_LibraryLink", {}, Integer];
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
		Throw[Failure["LibraryFunctionError", <|"Result"->ToString[Short[res], OutputForm, PageWidth -> 100]|>]]
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



structureSrcArgs[startLine_, startCol_, endLine_, endCol_] := {{startLine,startCol},{endLine,endCol}}

structureSrcArgs[offset_, len_] := {offset, len}

structureSrcArgs[] := Null



MakeLeafNode[tag_, payload_, srcArgs___] :=
	LeafNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakePrefixNode[tag_, payload_, srcArgs___] :=
	PrefixNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeBinaryNode[tag_, payload_, srcArgs___] :=
	BinaryNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeTernaryNode[tag_, payload_, srcArgs___] :=
	TernaryNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeInfixNode[tag_, payload_, srcArgs___] :=
	InfixNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakePostfixNode[tag_, payload_, srcArgs___] :=
	PostfixNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeGroupNode[tag_, payload_, srcArgs___] :=
	GroupNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeCallNode[tag_, payload_, srcArgs___] :=
	CallNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakePrefixBinaryNode[tag_, payload_, srcArgs___] :=
	PrefixBinaryNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

(*
MakeStartOfLineNode[tag_, payload_, srcArgs___] :=
	StartOfLineNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeStartOfFileNode[tag_, payload_, srcArgs___] :=
	StartOfFileNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]
*)

MakeBlankNode[tag_, payload_, srcArgs___] :=
	BlankNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeBlankSequenceNode[tag_, payload_, srcArgs___] :=
	BlankSequenceNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeBlankNullSequenceNode[tag_, payload_, srcArgs___] :=
	BlankNullSequenceNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakePatternBlankNode[tag_, payload_, srcArgs___] :=
	PatternBlankNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakePatternBlankSequenceNode[tag_, payload_, srcArgs___] :=
	PatternBlankSequenceNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakePatternBlankNullSequenceNode[tag_, payload_, srcArgs___] :=
	PatternBlankNullSequenceNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeOptionalDefaultPatternNode[tag_, payload_, srcArgs___] :=
	OptionalDefaultPatternNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeSyntaxErrorNode[tag_, payload_, srcArgs___] :=
	SyntaxErrorNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeGroupMissingCloserNode[tag_, payload_, srcArgs___] :=
	GroupMissingCloserNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeAbstractSyntaxErrorNode[tag_, payload_, srcArgs___] :=
	AbstractSyntaxErrorNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]



MakeSyntaxIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real] :=
	SyntaxIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], ConfidenceLevel->confidence|>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer, airyness_Real] :=
	FormatIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], Format`AirynessLevel->airyness|>]

(*
Only add CodeActions if there is at least 1
*)
MakeSyntaxIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real, actions:CodeAction[_, _, _]..] :=
	SyntaxIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], ConfidenceLevel->confidence, CodeActions->{actions}|>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer, airyness_Real, actions:CodeAction[_, _, _]..] :=
	FormatIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], Format`AirynessLevel->airyness, CodeActions->{actions}|>]


MakeReplaceTextCodeAction[label_String, srcArgs___Integer, replacementText_String] :=
	CodeAction[label, ReplaceText, <|Source->structureSrcArgs[srcArgs], "ReplacementText"->replacementText|>]

MakeInsertTextCodeAction[label_String, srcArgs___Integer, insertionText_String] :=
	CodeAction[label, InsertText, <|Source->structureSrcArgs[srcArgs], "InsertionText"->insertionText|>]

MakeInsertTextAfterCodeAction[label_String, srcArgs___Integer, insertionText_String] :=
	CodeAction[label, InsertTextAfter, <|Source->structureSrcArgs[srcArgs], "InsertionText"->insertionText|>]

MakeDeleteTextCodeAction[label_String, srcArgs___Integer] :=
	CodeAction[label, DeleteText, <|Source->structureSrcArgs[srcArgs]|>]

MakeDeleteTriviaCodeAction[label_String, srcArgs___Integer] :=
	CodeAction[label, DeleteTrivia, <|Source->structureSrcArgs[srcArgs]|>]




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

		location = "Location" /. PacletInformation["AST"];

		longNamesFile = FileNameJoin[{location, "Generated", "LongNames.wl"}];

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
		Print["nearest: ", nearest[[1]]];
	];
	nearest[[1]]
]]



UndocumentedLongNames[] :=
Catch[
Module[{names, documentedNames, undocumentedNames, location, longNamesFile},

	(*
	lazy initialization of $longNames]
	*)
	If[!ListQ[$longNames],

		location = "Location" /. PacletInformation["AST"];

		longNamesFile = FileNameJoin[{location, "Generated", "LongNames.wl"}];

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

	undocumentedNames
]]




End[]

EndPackage[]
