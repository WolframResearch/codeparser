BeginPackage["AST`ToInputFormString`"]

Begin["`Private`"]

Needs["AST`"]


(*
ToInputFormString is intended for aggregate syntax trees
*)



ToInputFormString[cst_] :=
Block[{$RecursionLimit = Infinity},
	StringJoin[toInputFormString[cst]]
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



toInputFormString[LeafNode[_, str_, _]] :=
	str

(*
special case for a; ;, which is   a Semi InternalNullNode Semi
*)
toInputFormString[LeafNode[Token`Fake`Null, str_, _]] :=
	" "

toInputFormString[LeafNode[Token`Fake`All, str_, _]] :=
	str

toInputFormString[LeafNode[Token`Fake`One, str_, _]] :=
	str










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
Module[{opStr},
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




toInputFormString[StartOfLineNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]





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

toInputFormString[GroupMissingOpenerNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]




toInputFormString[FileNode[File, nodes_, data_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
]]





toInputFormString[HoldNode[Hold, nodes_, data_]] :=
Module[{processed},
	
	processed = Riffle[nodes, LeafNode[Token`Comma, ",", <||>]];

	toInputFormString[CallNode[LeafNode[Symbol, "Hold", <||>], {
								GroupNode[GroupSquare, {
									LeafNode[Token`OpenSquare, "[", <||>] } ~Join~
									{ InfixNode[Comma, processed, <||>] } ~Join~
									{ LeafNode[Token`CloseSquare, "]", <||>] }, <||>] }, <||> ]]
]


(*
ConcreteParseString[""] returns Null, so handle that
*)
toInputFormString[Null] := ""




toInputFormString[f_Failure] := f

toInputFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
