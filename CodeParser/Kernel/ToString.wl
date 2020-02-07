BeginPackage["CodeParser`ToString`"]

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
ToInputFormString is intended for aggregate syntax trees
*)

ToInputFormString::usage = "ToInputFormString[aggregate] returns a string representation of aggregate."

ToInputFormString[agg_] :=
Block[{$RecursionLimit = Infinity},
	toInputFormString[agg]
]


toInputFormString[LeafNode[Symbol, str_, _]] :=
	str

toInputFormString[LeafNode[String, str_, _]] :=
	str

toInputFormString[LeafNode[Integer, str_, _]] :=
	str

toInputFormString[LeafNode[Real, str_, _]] :=
	str

toInputFormString[LeafNode[Slot, str_, _]] :=
	str

toInputFormString[LeafNode[SlotSequence, str_, _]] :=
	str

toInputFormString[LeafNode[Out, str_, _]] :=
	str

toInputFormString[LeafNode[OptionalDefault, str_, _]] :=
	str

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



(*
toInputFormString[StartOfLineNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[StartOfFileNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]
*)



toInputFormString[BlankNode[Blank, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[BlankSequenceNode[BlankSequence, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[BlankNullSequenceNode[BlankNullSequence, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[PatternBlankNode[PatternBlank, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[PatternBlankSequenceNode[PatternBlankSequence, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toInputFormString[OptionalDefaultPatternNode[OptionalDefaultPattern, nodes_, _]] :=
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



toInputFormString[ContainerNode[Hold, nodes_, data_]] :=
Module[{processed},
	
	processed = Riffle[nodes, LeafNode[Token`Comma, ",", <||>]];

	toInputFormString[CallNode[LeafNode[Symbol, "Hold", <||>], {
								GroupNode[GroupSquare, {
									LeafNode[Token`OpenSquare, "[", <||>] } ~Join~
									{ InfixNode[Comma, processed, <||>] } ~Join~
									{ LeafNode[Token`CloseSquare, "]", <||>] }, <||>] }, <||> ]]
]

toInputFormString[ContainerNode[_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
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
	If[!StringStartsQ[str, "\""],
		Throw[Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{node}]|>]]
	];
	str
]

toFullFormString[LeafNode[Integer, str_, _]] :=
	str

toFullFormString[LeafNode[Real, str_, _]] :=
	str


toFullFormString[ErrorNode[tag_, str_, data_]] :=
	Failure["ErrorNode", <|"Tag"->tag, "String"->str, "Data"->data|>]


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
toFullFormString[f:StartOfFileNode[Shebang, _, _]] :=
	ToInputFormString[f]
*)


(*
FIXME: linear syntax is skipped right now
*)
toFullFormString[p:PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
	ToInputFormString[p]

toFullFormString[g:GroupNode[GroupLinearSyntaxParen, _, _]] :=
	ToInputFormString[g]

toFullFormString[args:BoxNode[box_, children_, _]] :=
	Failure["CannotConvertBoxesToFullForm", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{args}]|>]




toFullFormString[ContainerNode[Hold, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	If[empty[nodes],
		nodeStrs = {"Null"}
		,
		nodeStrs = toFullFormString /@ nodes;
		nodeStrs = Flatten[nodeStrs];
	];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{"Hold", "[", Riffle[nodeStrs, ", "], "]"}]
]]

toFullFormString[ContainerNode[_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	If[empty[nodes],
		nodeStrs = {"Null"}
		,
		nodeStrs = toFullFormString /@ nodes;
		nodeStrs = Flatten[nodeStrs];
	];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
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

	(*
	System`Private`NewContextPath[] with ; after is a lot more common
	*)

	nodeStrs = toFullFormString /@ (
		{CallNode[ToNode[CompoundExpression], { CallNode[ToNode[System`Private`NewContextPath], args, <||>], ToNode[Null] }, <||>] } ~Join~
		nodes ~Join~
		{CallNode[ToNode[CompoundExpression], { CallNode[ToNode[System`Private`RestoreContextPath], {}, <||>], ToNode[Null] }, <||>]});

	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]





toFullFormString[n_SyntaxErrorNode] := Failure["SyntaxError", <|"Error"->n|>]

toFullFormString[f_?FailureQ] := f

toFullFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{args}]|>]







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
	If[empty[nodes],
		nodeStrs = {"Null"}
		,
		(*
		remove top-level trivia
		*)
		nodes = DeleteCases[nodes, LeafNode[Whitespace | Token`Newline | Token`Comment | Token`LineContinuation, _, _]];
		nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
		nodeStrs = Flatten[nodeStrs];
	];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{"Hold", "[", Riffle[nodeStrs, ", "], "]"}]
]]

toSourceCharacterString[ContainerNode[_, nodes_, opts_], insideBoxes_] :=
Catch[
Module[{nodeStrs},
	If[empty[nodes],
		nodeStrs = {"Null"}
		,
		nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
		nodeStrs = Flatten[nodeStrs];
	];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]


toSourceCharacterString[args:BoxNode[box_, children_, _], insideBoxes_] :=
	Failure["CannotConvertBoxesToSourceCharacterString", <|"Function"->ToSourceCharacterString, "Arguments"->HoldForm[{args}]|>]


toSourceCharacterString[_[op_, nodes_, data_], insideBoxes_] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toSourceCharacterString[#, insideBoxes]& /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]

toSourceCharacterString[args___] := Failure["InternalUnhandled", <|"Function"->toSourceCharacterString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
