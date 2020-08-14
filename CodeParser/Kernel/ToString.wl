BeginPackage["CodeParser`ToString`"]

toInputFormStringButNotToplevelNewlines

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



$systemNewline =
Switch[$OperatingSystem,
	"Windows", "\r\n",
	_, "\n"
]




(*
ToInputFormString is intended for aggregate syntax trees
*)

ToInputFormString::usage = "ToInputFormString[aggregate] returns a string representation of aggregate."

ToInputFormString[agg_] :=
Block[{$RecursionLimit = Infinity},
	toInputFormString[agg]
]




(*
special case ImplicitTimes to fill in " " for the operators
*)
toInputFormString[LeafNode[Token`Fake`ImplicitTimes, _,  _]] :=
	" "

(*
special case Plus to fix stringifying  1.2` + 3  as  1.2`+3
*)
toInputFormString[LeafNode[Token`Plus, _, _]] :=
	" + "

(*
special case Minus to fix stringifying  1.2` - 3  as  1.2`-3
*)
toInputFormString[LeafNode[Token`Minus, _, _]] :=
	" - "

(*
special case Dot to fix stringifying  c_ . _LinearSolve  as  c_._LinearSolve
*)
toInputFormString[LeafNode[Token`Dot, _, _]] :=
	" . "

(*
special case DotDot to fix stringifying  0. .. as 0...
*)
toInputFormString[LeafNode[Token`DotDot, _, _]] :=
	" .."

(*
special case DotDot to fix stringifying  0. ... as 0....
*)
toInputFormString[LeafNode[Token`DotDotDot, _, _]] :=
	" ..."

(*
special case SlashDot to fix stringifying  x /. 0 as x/.0
*)
toInputFormString[LeafNode[Token`SlashDot, _, _]] :=
	" /. "

(*
special case Dot to fix stringifying  x //. 0 as x//.0
*)
toInputFormString[LeafNode[Token`SlashSlashDot, _, _]] :=
	" //. "

(*
special case for a; ;, which is   a Semi InternalNullNode Semi
*)
toInputFormString[LeafNode[Token`Fake`ImplicitNull, _, _]] :=
	" "

toInputFormString[LeafNode[_, str_, _]] :=
	str


toInputFormString[ErrorNode[_, str_, _]] :=
	str


toInputFormString[BoxNode[RowBox, children_, _]] :=
	Catch[
	Module[{nodes, nodeStrs},
		nodes = children[[1]];
		nodeStrs = toInputFormString[#]& /@ nodes;
		If[AnyTrue[nodeStrs, FailureQ],
			Throw[SelectFirst[nodeStrs, FailureQ]]
		];
		StringJoin[Riffle[nodeStrs, " "]]
	]]

toInputFormString[args:BoxNode[box_, children_, _]] :=
	Failure["CannotConvertBoxesToInputFormString", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]


toInputFormString[PrefixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[" ", nodeStrs, " "]
]]

toInputFormString[BinaryNode[op_, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[" ", nodeStrs, " "]
]]





toInputFormString[InfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[" ", nodeStrs, " "]
]]



toInputFormString[TernaryNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[" ", nodeStrs, " "]
]]


toInputFormString[PostfixNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[" ", nodeStrs, " "]
]]

(*
toInputFormString is intended for aggregate syntax, and aggregate syntax
only ever has 1 arg for a Call: i.e. CallNode[head, {GroupNode[GroupSquare, {args}]}]

If you see an unevaluated toInputFormString[CallNode[head, {arg1, arg2}]], then
that is abstract syntax
*)
toInputFormString[CallNode[op_, nodes_, data_]] :=
Catch[
Module[{opStr, nodeStrs},
	opStr = toInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[opStr, nodeStrs]
]]

toInputFormString[GroupNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[PrefixBinaryNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]



toInputFormString[CompoundNode[_, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]




toInputFormString[SyntaxErrorNode[tag_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[" ", nodeStrs, " "]
]]

toInputFormString[GroupMissingCloserNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[UnterminatedGroupNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]


toInputFormString[ContainerNode[Hold, nodes_, data_]] :=
Module[{processed},
	
	processed = Riffle[nodes, LeafNode[Token`Comma, ",", <||>]];

	toInputFormString[CallNode[LeafNode[Symbol, "Hold", <||>], {
								GroupNode[GroupSquare, {
									LeafNode[Token`OpenSquare, "[", <||>] } ~Join~
									{ InfixNode[Comma, processed, <||>] } ~Join~
									{ LeafNode[Token`CloseSquare, "]", <||>] }, <||>] }, <||> ]]
]

toInputFormStringButNotToplevelNewlines[ContainerNode[Box, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[ContainerNode[_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, $systemNewline]]
]]




toInputFormString[f_Failure] := f

toInputFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]







(*
ToFullFormString is intended for abstract syntax trees
*)

ToFullFormString::usage = "ToFullFormString[abstract] returns a string representation of abstract."

ToFullFormString[ast_] :=
Catch[
Module[{str},
Block[{$RecursionLimit = Infinity},
	str = toFullFormString[ast];
	If[FailureQ[str],
		Throw[str]
	];
	StringJoin[str]
]]]


toFullFormString[LeafNode[Symbol, str_, _]] :=
	str

(*
strings may not originally be quoted, a::b

But they become quoted when they are abstracted
*)
toFullFormString[node:LeafNode[String, str_, _]] :=
Catch[
	Switch[StringPart[str, 1],
		"\"",
			str
		,
		"\\",
			(*
			assume this is a line continuation without checking any further
			*)
			str
		,
		_,
			Throw[Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{node}]|>]]
	]
]

toFullFormString[LeafNode[_, str_, _]] :=
	str


(*
used to be:
Failure["ErrorNode", <|"Tag"->tag, "String"->str, "Data"->data|>]

but Failure object usurps the Tag key, so use Token instead
*)
toFullFormString[ErrorNode[tok_, str_, data_]] :=
	Failure["ErrorNode", <|"Token"->tok, "String"->str, "Data"->data|>]


(*
The interesting case of  a // -1  not being the same as  -1[a]

No need for Rational here, because FullForm of Rational is... Rational[]

Related bug reports: 391443
*)
toFullFormString[CallNode[head:LeafNode[Integer | Real, str_ /; StringStartsQ[str, "-"], _], nodes_, _]] :=
Catch[
Module[{headStr, nodeStrs},
	headStr = toFullFormString[head];
	If[FailureQ[headStr],
		Throw[headStr]
	];
	nodeStrs = toFullFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{"(", headStr, ")", "[", Riffle[nodeStrs, ", "], "]"}]
]]

toFullFormString[CallNode[head_, nodes_, _]] :=
Catch[
Module[{headStr, nodeStrs},
	headStr = toFullFormString[head];
	If[FailureQ[headStr],
		Throw[headStr]
	];
	nodeStrs = toFullFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{headStr, "[", Riffle[nodeStrs, ", "], "]"}]
]]



(*
FIXME: linear syntax is skipped right now
*)
toFullFormString[p:PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
	ToInputFormString[p]

toFullFormString[args:BoxNode[box_, children_, _]] :=
	Failure["CannotConvertBoxesToFullForm", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{args}]|>]




toFullFormString[ContainerNode[Hold, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ nodes;
	nodeStrs = Flatten[nodeStrs];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{"Hold", "[", Riffle[nodeStrs, ", "], "]"}]
]]

toFullFormString[ContainerNode[_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ nodes;
	nodeStrs = Flatten[nodeStrs];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, $systemNewline]]
]]




(*
returns a list
*)
toFullFormString[PackageNode[args_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ ({CallNode[ToNode[BeginPackage], args, <||>]} ~Join~ nodes ~Join~ {CallNode[ToNode[EndPackage], {}, <||>]});
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]

(*
returns a list
*)
toFullFormString[ContextNode[args_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ ({CallNode[ToNode[Begin], args, <||>]} ~Join~ nodes ~Join~ {CallNode[ToNode[End], {}, <||>]});
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]

toFullFormString[NewContextPathNode[args_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ (
		{CallNode[ToNode[System`Private`NewContextPath], args, <||>]} ~Join~
		nodes ~Join~
		{CallNode[ToNode[System`Private`RestoreContextPath], {}, <||>]});

	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]





toFullFormString[n_SyntaxErrorNode] := Failure["SyntaxError", <|"Error"->n|>]

toFullFormString[f_?FailureQ] := f

toFullFormString[args___] :=
	(*
	Need to specify PageWidth, or else ToString does not do anything with Short
	Related bugs: ?
	*)
	Failure["InternalUnhandled", <|
		(*
		"Function" and "ShortArguments" is really just taking up space to force "Arguments" to be hidden by default
		*)
		"Function"->ToFullFormString,
		"ShortArguments" -> ToString[Short[{args}], OutputForm, PageWidth -> 100],
		"Arguments"->{args}|>]







(*
ToSourceCharacterString is intended for concrete syntax trees
*)

ToSourceCharacterString::usage = "ToSourceCharacterString[concrete] returns a string representation of concrete."

ToSourceCharacterString[cst_] :=
Catch[
Module[{str},
Block[{$RecursionLimit = Infinity},
	str = toSourceCharacterString[cst, False];
	If[FailureQ[str],
		Throw[str]
	];
	StringJoin[str]
]]]


toSourceCharacterString[LeafNode[_, str_, _], insideBoxes_] :=
	str

toSourceCharacterString[ErrorNode[_, str_, _], insideBoxes_] :=
	str



(*
toSourceCharacterString is intended for concrete syntax, and concrete syntax
has a List for the head of a Call: i.e. CallNode[{head}, {GroupNode[GroupSquare, {args}]}]

But!

Put in a hack to handle non-List for head of CallNode, because it is convenient

*)
toSourceCharacterString[CallNode[op_, nodes_, data_], insideBoxes_] :=
Catch[
Module[{opStrs, nodeStrs},
	If[ListQ[op],
		opStrs = toSourceCharacterString[#, insideBoxes]& /@ op;
		,
		opStrs = toSourceCharacterString[#, insideBoxes]& /@ { op };
	];
	If[AnyTrue[opStrs, FailureQ],
		Throw[SelectFirst[opStrs, FailureQ]]
	];
	nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[opStrs, nodeStrs]
]]

toSourceCharacterString[ContainerNode[Hold, nodesIn_, opts_], insideBoxes_] :=
Catch[
Module[{nodes, nodeStrs},
	nodes = nodesIn;
	(*
	remove top-level trivia
	*)
	nodes = DeleteCases[nodes, LeafNode[Whitespace | Token`Newline | Token`Comment, _, _]];
	nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
	nodeStrs = Flatten[nodeStrs];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{"Hold", "[", Riffle[nodeStrs, ", "], "]"}]
]]

toSourceCharacterString[ContainerNode[File, nodes_, opts_], insideBoxes_] :=
Catch[
Module[{nodeStrs},
	
	nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
	nodeStrs = Flatten[nodeStrs];

	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toSourceCharacterString[ContainerNode[_, nodes_, opts_], insideBoxes_] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
	nodeStrs = Flatten[nodeStrs];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]


toSourceCharacterString[BoxNode[RowBox, children_, _], insideBoxes_] :=
	Catch[
	Module[{nodes, nodeStrs},
		nodes = children[[1]];
		nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
		If[AnyTrue[nodeStrs, FailureQ],
			Throw[SelectFirst[nodeStrs, FailureQ]]
		];
		StringJoin[nodeStrs]
	]]


toSourceCharacterString[args:BoxNode[box_, children_, _], insideBoxes_] :=
	Failure["CannotConvertBoxesToSourceCharacterString", <|"Function"->ToSourceCharacterString, "Arguments"->{args}|>]


toSourceCharacterString[_[op_, nodes_, data_], insideBoxes_] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toSourceCharacterString[f_?FailureQ, _] := f

toSourceCharacterString[args___] := Failure["InternalUnhandled", <|"Function"->toSourceCharacterString, "Arguments"->{args}|>]


End[]

EndPackage[]
