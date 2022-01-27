BeginPackage["CodeParser`Folds`"]

aggregate

deparen


aggregateButNotToplevelNewlines


linearize


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
Remove comments, whitespace, and newlines

Collapse CallNode[{op}, {}] to CallNode[op, {}]


*)

aggregate[Null] := Null

aggregate[LeafNode[Whitespace | Token`Comment | Token`Newline | Token`Boxes`MultiWhitespace, _, _]] := Nothing

aggregateButNotToplevelNewlines[LeafNode[Whitespace | Token`Comment | Token`Boxes`MultiWhitespace, _, _]] := Nothing

aggregate[l_LeafNode] := l


(*
Multiple implicit Times tokens may have been inserted when parsing boxes, so remove them here
*)
aggregate[InfixNode[Times, children_, data_]] :=
Module[{aggregatedChildren},

	aggregatedChildren = aggregate /@ children;

	aggregatedChildren = First /@ Split[aggregatedChildren, (MatchQ[#1, LeafNode[Token`Fake`ImplicitTimes, _, _]] && MatchQ[#2, LeafNode[Token`Fake`ImplicitTimes, _, _]])&];

	InfixNode[Times, aggregatedChildren, data]
]

aggregateButNotToplevelNewlines[InfixNode[Times, children_, data_]] :=
Module[{aggregatedChildren},

	aggregatedChildren = aggregate /@ children;

	aggregatedChildren = First /@ Split[aggregatedChildren, (MatchQ[#1, LeafNode[Token`Fake`ImplicitTimes, _, _]] && MatchQ[#2, LeafNode[Token`Fake`ImplicitTimes, _, _]])&];

	InfixNode[Times, aggregatedChildren, data]
]


(*
from boxes
*)
aggregate[GroupNode[Comment, _, _]] := Nothing

aggregateButNotToplevelNewlines[GroupNode[Comment, _, _]] := Nothing



aggregate[CallNode[head_List, children_, data_]] :=
	CallNode[aggregate[head[[1]]], aggregate /@ children, data]

aggregateButNotToplevelNewlines[CallNode[head_List, children_, data_]] :=
	CallNode[aggregate[head[[1]]], aggregate /@ children, data]



aggregate[node:CallNode[headIn_, childrenIn_, dataIn_]] :=
	Failure["InvalidHead", <|
			"Message" -> "Head is not a list (Possibly calling Aggregate on abstract syntax)",
			"Function" -> aggregate,
			"Arguments" -> {node}
		|>
	]


aggregate[ContainerNode[File, childrenIn_, dataIn_]] :=
Catch[
Module[{children, aggChildren, data},

	children = childrenIn;
	data = dataIn;

	aggChildren = aggregate /@ children;

	ContainerNode[File, aggChildren, data]
]]

aggregate[ContainerNode[Box, childrenIn_, dataIn_]] :=
Catch[
Module[{children, aggChildren, data},

	children = childrenIn;
	data = dataIn;

	aggChildren = aggregate /@ children;

	ContainerNode[Box, aggChildren, data]
]]

aggregateButNotToplevelNewlines[ContainerNode[Box, childrenIn_, dataIn_]] :=
Catch[
Module[{children, aggChildren, data},

	children = childrenIn;
	data = dataIn;

	aggChildren = aggregateButNotToplevelNewlines /@ children;

	ContainerNode[Box, aggChildren, data]
]]

aggregate[ContainerNode[Hold, childrenIn_, dataIn_]] :=
Catch[
Module[{children, aggChildren, data},

	children = childrenIn;
	data = dataIn;

	aggChildren = aggregate /@ children;

	ContainerNode[Hold, aggChildren, data]
]]


(*
BoxNode[RowBox] and BoxNode[GridBox] have lists as children
*)
aggregate[l_List] := aggregate /@ l

aggregateButNotToplevelNewlines[l_List] := aggregateButNotToplevelNewlines /@ l


aggregateButNotToplevelNewlines[BoxNode[RowBox, childrenIn_, dataIn_]] :=
Catch[
Module[{children, aggChildren, data},

	children = childrenIn;
	data = dataIn;

	aggChildren = aggregateButNotToplevelNewlines /@ children;

	If[MatchQ[aggChildren, {{_}}],
		(*
		children is now a single node, so collapse the RowBox
		*)
		Throw[aggChildren[[1, 1]]]
	];

	BoxNode[RowBox, aggChildren, data]
]]

aggregate[BoxNode[RowBox, childrenIn_, dataIn_]] :=
Catch[
Module[{children, aggChildren, data},

	children = childrenIn;
	data = dataIn;

	aggChildren = aggregate /@ children;

	If[MatchQ[aggChildren, {{_}}],
		(*
		children is now a single node, so collapse the RowBox
		*)
		Throw[aggChildren[[1, 1]]]
	];

	BoxNode[RowBox, aggChildren, data]
]]

(*
Do not descend into CodeNode
*)
aggregate[n:CodeNode[_, _, _]] := n

aggregate[m_?MissingQ] := m

aggregate[node_[tag_, children_, data_]] :=
	node[tag, aggregate /@ children, data]

aggregateButNotToplevelNewlines[node_[tag_, children_, data_]] :=
	node[tag, aggregate /@ children, data]



aggregateButNotToplevelNewlines[arg_] := Failure["bad", <| "function" -> "aggregateButNotNewlines", "arguments" -> {arg} |>]



(*

Remove parens

*)

deparen[Null] := Null

deparen[l_LeafNode] := l


deparen[GroupNode[GroupParen, { _, child:InfixNode[Comma, _, _], _ }, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, child, data]

deparen[GroupNode[GroupParen, { _, child_, _}, data_]] :=
	deparen[child]

deparen[CallNode[head_, children_, dataIn_]] :=
Catch[
Module[{deHead, deChildren, data},

	data = dataIn;

	deHead = deparen[head];

	deChildren = deparen /@ children;

	CallNode[deHead, deChildren, data]
]]

deparen[node_[tag_, children_, dataIn_]] :=
Module[{deChildren, data},

	data = dataIn;
	
	deChildren = deparen /@ children;

	node[tag, deChildren, data]
]



linearize[node_] := Flatten[linearize0[node]]

linearize0[ContainerNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[GroupNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[BinaryNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[InfixNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[TernaryNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[QuaternaryNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[ClauseNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[PrefixNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[PostfixNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[PrefixBinaryNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[CompoundNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[BoxNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[CallNode[head_List, fs_List, _]] :=
  {linearize0 /@ head, linearize0 /@ fs}

linearize0[SyntaxErrorNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[GroupMissingCloserNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[UnterminatedGroupNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[AbstractSyntaxErrorNode[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[CallMissingCloserNode[head_List, fs_List, _]] :=
  {linearize0 /@ head, linearize0 /@ fs}

linearize0[UnterminatedCallNode[head_List, fs_List, _]] :=
  {linearize0 /@ head, linearize0 /@ fs}


(*
Distribute StartOfLine and EndOfLine data over the fragments of a comment
*)
linearize0[LeafNode[Token`Comment, children_List, data_]] :=
  linearize0 /@ (insertData[#, data, {StartOfLine, EndOfLine}]& /@ children)


linearize0[LeafNode[tag_, fs_List, data_]] :=
  linearize0 /@ fs

linearize0[ErrorNode[tag_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[n:LeafNode[_, _String, _]] :=
  n

linearize0[n:ErrorNode[_, _String, _]] :=
  n

linearize0[n:FragmentNode[_, _String, _]] :=
  n

linearize0[m_?MissingQ] :=
  m

linearize0[args___] :=
	Failure["InternalUnhandled", <| "Function" -> linearize0, "Arguments" -> {args} |>]



insertData[FragmentNode[tag_, str_, data1_], data_, keys_] :=
	FragmentNode[tag, str, <| data1, KeyTake[data, keys] |>]

insertData[args___] :=
	Failure["InternalUnhandled", <| "Function" -> insertData, "Arguments"->{args} |>]



End[]

EndPackage[]
