BeginPackage["AST`Folds`"]

aggregate

deparen

recalculateSources


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]


(*
Remove comments, whitespace, and newlines

Collapse CallNode[{op}, {}] to CallNode[op, {}]


*)

aggregate[Null] := Null

aggregate[LeafNode[Token`Comment | Token`WhiteSpace | Token`Newline, _, _]] := Nothing

aggregate[l_LeafNode] := l

(*
do not touch linear syntax
*)
aggregate[node:GroupNode[GroupLinearSyntaxParen, _, _]] := node

aggregate[CallNode[head_, children_, dataIn_]] :=
Catch[
Module[{newSrc, aggHead, aggChildren, data},

	data = dataIn;

	aggHead = aggregate /@ head;

	If[Length[aggHead] != 1,
		Throw[Failure["InternalUnhandled", <|"Function"->aggregate, "Arguments"->HoldForm[{CallNode[head, children, dataIn]}]|>]]
	];

	aggHead = aggHead[[1]];

	aggChildren = aggregate /@ children;
	newSrc = {aggHead[[3]][Source][[1]], aggChildren[[-1]][[3]][Source][[2]]};

	If[newSrc =!= data[Source],
		data[Source] = newSrc
	];

	CallNode[aggHead, aggChildren, data]
]]

aggregate[node_[tag_, children_, dataIn_]] :=
Module[{newSrc, aggChildren, data},

	data = dataIn;
	
	aggChildren = aggregate /@ children;

	If[empty[aggChildren],
		data[Source] =.
		,
		newSrc = {aggChildren[[1]][[3]][Source][[1]], aggChildren[[-1]][[3]][Source][[2]]};

		If[newSrc =!= data[Source],
			data[Source] = newSrc
		];
	];

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









recalculateSources[Null] := Null

recalculateSources[l_LeafNode] := l

recalculateSources[a_AbstractSyntaxErrorNode] := a

recalculateSources[CallNode[head_, childrenIn_, dataIn_]] :=
Catch[
Module[{children, data},

	children = childrenIn;
	data = dataIn;

	If[empty[children],

		(*
		no children, so only bother with head
		*)

		(*
		do not remove any Sources, they may have been present from before

		something like  f[]  should remember about the []

		data[Source] =.;
		*)

		newSrc = head[[3]][Source][[1]];
		If[newSrc =!= data[Source][[1]],
			data[[Key[Source], 1]] = newSrc
		];

		Throw[CallNode[head, children, data]]
	];

	children = recalculateSources /@ children;

	newSrc = {head[[3]][Source][[1]], children[[-1]][[3]][Source][[2]]};

	If[newSrc =!= data[Source],
		data[Source] = newSrc
	];

	CallNode[head, children, data]
]]

recalculateSources[node_[tag_, childrenIn_, dataIn_]] :=
Catch[
Module[{children, data},

	children = childrenIn;
	data = dataIn;

	If[empty[children],
		data[Source] =.;
		Throw[node[tag, children, data]]
	];

	children = recalculateSources /@ children;

	newSrc = {children[[1]][[3]][Source][[1]], children[[-1]][[3]][Source][[2]]};

	If[newSrc =!= data[Source],
		data[Source] = newSrc
	];

	node[tag, children, data]
]]




End[]

EndPackage[]
