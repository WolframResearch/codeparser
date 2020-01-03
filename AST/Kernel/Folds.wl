BeginPackage["AST`Folds`"]

aggregate

deparen


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]


(*
Remove comments, whitespace, and newlines

Collapse CallNode[{op}, {}] to CallNode[op, {}]


*)

aggregate[Null] := Null

aggregate[LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _]] := Nothing

aggregate[l_LeafNode] := l

(*
do not touch linear syntax
*)
aggregate[node:GroupNode[GroupLinearSyntaxParen, _, _]] := node

(*
from boxes
*)
aggregate[node:GroupNode[Comment, _, _]] := Nothing

aggregate[CallNode[headIn_, childrenIn_, dataIn_]] :=
Catch[
Module[{head, children, aggHead, aggChildren, data},

	head = headIn;
	children = childrenIn;
	data = dataIn;

	aggHead = aggregate /@ head;

	If[Length[aggHead] != 1,
		Throw[Failure["InternalUnhandled", <|"Function"->aggregate, "Arguments"->HoldForm[{CallNode[head, children, data]}]|>]]
	];

	aggHead = aggHead[[1]];

	aggChildren = aggregate /@ children;

	CallNode[aggHead, aggChildren, data]
]]

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

aggregate[node_[tag_, childrenIn_, dataIn_]] :=
Module[{children, aggChildren, data},
	
	children = childrenIn;
	data = dataIn;

	aggChildren = aggregate /@ children;

	node[tag, aggChildren, data]
]





(*

Remove parens

*)

deparen[Null] := Null

deparen[l_LeafNode] := l


deparen[GroupNode[GroupParen, { _, child:InfixNode[Comma, _, _], _ }, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, child, KeyTake[data, keysToTake]]
deparen[GroupNode[GroupParen, { _, child_, _}, data_]] := deparen[child]

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



End[]

EndPackage[]
