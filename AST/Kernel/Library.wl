BeginPackage["AST`Library`"]

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
