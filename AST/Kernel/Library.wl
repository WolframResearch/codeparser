BeginPackage["AST`Library`"]

$lib

loadFunc

loadAllFuncs

newestLinkObject


concreteParseFileFunc
concreteParseBytesFunc
tokenizeFileFunc
tokenizeBytesFunc
parseLeafFunc




MakeLeafNode

MakePrefixNode
MakeBinaryNode
MakeTernaryNode
MakeInfixNode
MakePostfixNode
MakeGroupNode
MakeCallNode
MakePrefixBinaryNode

MakeStartOfLineNode
MakeStartOfFileNode
MakeBlankNode
MakeBlankSequenceNode
MakeBlankNullSequenceNode
MakePatternBlankNode
MakePatternBlankSequenceNode
MakePatternBlankNullSequenceNode
MakeOptionalDefaultPatternNode

MakeSyntaxErrorNode
MakeGroupMissingCloserNode
MakeGroupMissingOpenerNode
MakeAbstractSyntaxErrorNode


MakeSyntaxIssue
MakeFormatIssue

MakeReplaceTextCodeAction
MakeInsertTextCodeAction
MakeInsertTextAfterCodeAction
MakeDeleteTextCodeAction
MakeDeleteTriviaCodeAction


SetConcreteParseProgress


$ConcreteParseProgress
$ConcreteParseTime
$ConcreteParseStart

$MathLinkTime


LongNameSuggestion


Begin["`Private`"]

Needs["AST`"]



$lib := $lib =
Catch[
Module[{res},
	res = FindLibrary["AST"];
	If[FailureQ[res],
		Throw[Failure["ASTLibraryNotFound", <||>]]
	];
	res
]]

(*
LibraryLink creates a separate loopback link for each library function
*)

loadFunc[name_String] :=
Catch[
Module[{res, loaded, linkObject},

	If[FailureQ[$lib],
		Throw[$lib]
	];

	res = newestLinkObject[LibraryFunctionLoad[$lib, name, LinkObject, LinkObject]];

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

concreteParseFileFunc := concreteParseFileFunc = loadFunc["ConcreteParseFile_LibraryLink"];

concreteParseBytesFunc := concreteParseBytesFunc = loadFunc["ConcreteParseBytes_LibraryLink"];

tokenizeFileFunc := tokenizeFileFunc = loadFunc["TokenizeFile_LibraryLink"];

tokenizeBytesFunc := tokenizeBytesFunc = loadFunc["TokenizeBytes_LibraryLink"];

parseLeafFunc := parseLeafFunc = loadFunc["ParseLeaf_LibraryLink"];
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







SetConcreteParseProgress[prog_] := (
	$ConcreteParseProgress = prog;
	If[prog == 100,
		$ConcreteParseTime = Now - $ConcreteParseStart;
	];
)


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


MakeStartOfLineNode[tag_, payload_, srcArgs___] :=
	StartOfLineNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]

MakeStartOfFileNode[tag_, payload_, srcArgs___] :=
	StartOfFileNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


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


MakeGroupMissingOpenerNode[tag_, payload_, srcArgs___] :=
	GroupMissingOpenerNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]


MakeAbstractSyntaxErrorNode[tag_, payload_, srcArgs___] :=
	AbstractSyntaxErrorNode[tag, payload, <|Source->structureSrcArgs[srcArgs]|>]



MakeSyntaxIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real] :=
	SyntaxIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], ConfidenceLevel->confidence|>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real] :=
	FormatIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], ConfidenceLevel->confidence|>]

(*
Only add CodeActions if there is at least 1
*)
MakeSyntaxIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real, actions:CodeAction[_, _, _]..] :=
	SyntaxIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], ConfidenceLevel->confidence, CodeActions->{actions}|>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer, confidence_Real, actions:CodeAction[_, _, _]..] :=
	FormatIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs], ConfidenceLevel->confidence, CodeActions->{actions}|>]


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



(*
MakeFormatIssue["Space", msg_String, severity_String, srcArgs___Integer] :=
	FormatIssue["Space", msg, severity, <|Source->structureSrcArgs[srcArgs], CodeActions->{CodeAction["Insert", InsertText, <|Source -> structureSrcArgs[srcArgs], "InsertionText"->" "|>]}|>]

MakeFormatIssue["SpaceAfter", msg_String, severity_String, srcArgs___Integer] :=
	FormatIssue["SpaceAfter", msg, severity, <|Source->structureSrcArgs[srcArgs], CodeActions->{CodeAction["InsertAfter", InsertTextAfter, <|Source -> structureSrcArgs[srcArgs], "InsertionText"->" "|>]}|>]

MakeFormatIssue["UnexpectedLineContinuation", msg_String, severity_String, srcArgs___Integer] :=
	FormatIssue["UnexpectedLineContinuation", msg, severity, <|Source->structureSrcArgs[srcArgs], CodeActions->{CodeAction["Delete", DeleteText, <|Source -> structureSrcArgs[srcArgs]|>]}|>]

MakeFormatIssue["NotContiguous", msg_String, severity_String, srcArgs___Integer] :=
	FormatIssue["NotContiguous", msg, severity, <|Source->structureSrcArgs[srcArgs], CodeActions->{CodeAction["Delete", DeleteTrivia, <|Source -> structureSrcArgs[srcArgs]|>]}|>]

MakeFormatIssue[tag_String, msg_String, severity_String, srcArgs___Integer] :=
	FormatIssue[tag, msg, severity, <|Source->structureSrcArgs[srcArgs]|>]
*)





$longNames

LongNameSuggestion[input_String] :=
Catch[
Module[{nearest},
	If[$Debug,
		Print["input: ", input];
	];
	If[!ListQ[$longNames],
		$longNames = Quiet[Get["AST`LongNames`"], {Get::noopen}];
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





End[]

EndPackage[]
