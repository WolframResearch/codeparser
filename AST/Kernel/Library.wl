BeginPackage["AST`Library`"]

$lib

loadFunc

loadAllFuncs

newestLinkObject



concreteParseStringFunc
concreteParseFileFunc
tokenizeStringFunc
tokenizeFileFunc
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


SetConcreteParseProgress


$ConcreteParseProgress
$ConcreteParseTime
$ConcreteParseStart

$MathLinkTime

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

concreteParseStringFunc := concreteParseStringFunc = loadFunc["ConcreteParseString_LibraryLink"];

concreteParseFileFunc := concreteParseFileFunc = loadFunc["ConcreteParseFile_LibraryLink"];

tokenizeStringFunc := tokenizeStringFunc = loadFunc["TokenizeString_LibraryLink"];

tokenizeFileFunc := tokenizeFileFunc = loadFunc["TokenizeFile_LibraryLink"];

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




MakeLeafNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	LeafNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakePrefixNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	PrefixNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakeBinaryNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	BinaryNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakeTernaryNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	TernaryNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakeInfixNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	InfixNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakePostfixNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	PostfixNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakeGroupNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	GroupNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakeCallNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	CallNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]

MakePrefixBinaryNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	PrefixBinaryNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeStartOfLineNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	StartOfLineNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeBlankNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	BlankNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeBlankSequenceNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	BlankSequenceNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeBlankNullSequenceNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	BlankNullSequenceNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakePatternBlankNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	PatternBlankNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakePatternBlankSequenceNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	PatternBlankSequenceNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakePatternBlankNullSequenceNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	PatternBlankNullSequenceNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeOptionalDefaultPatternNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	OptionalDefaultPatternNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeSyntaxErrorNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	SyntaxErrorNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeGroupMissingCloserNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	GroupMissingCloserNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeGroupMissingOpenerNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	GroupMissingOpenerNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


MakeAbstractSyntaxErrorNode[tag_, payload_, startLine_, startCol_, endLine_, endCol_] :=
	AbstractSyntaxErrorNode[tag, payload, <|Source->{{startLine,startCol},{endLine,endCol}}|>]



MakeSyntaxIssue[tag_, msg_, severity_, startLine_, startCol_, endLine_, endCol_] :=
	SyntaxIssue[tag, msg, severity, <|Source->{{startLine,startCol},{endLine,endCol}}|>]


End[]

EndPackage[]
