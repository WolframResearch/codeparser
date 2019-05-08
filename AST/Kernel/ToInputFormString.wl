BeginPackage["AST`ToInputFormString`"]

Begin["`Private`"]

Needs["AST`"]


(*
ToInputFormString is intended for concrete syntax trees
*)

ToInputFormString[cst_] :=
Block[{$RecursionLimit = Infinity},
	StringJoin[toInputFormString[cst]]
]


toInputFormString[SymbolNode[Symbol, str_, _]] :=
	str

toInputFormString[StringNode[String, str_, _]] :=
	str

toInputFormString[IntegerNode[Integer, str_, _]] :=
	str

toInputFormString[RealNode[Real, str_, _]] :=
	str

toInputFormString[SlotNode[Slot, str_, _]] :=
	str

toInputFormString[SlotSequenceNode[SlotSequence, str_, _]] :=
	str

toInputFormString[OutNode[Out, str_, _]] :=
	str

toInputFormString[OptionalDefaultNode[OptionalDefault, str_, _]] :=
	str

(*
special case ImplicitTimes to fill in " " for the operators
*)
toInputFormString[TokenNode[Token`Fake`ImplicitTimes, _,  _]] :=
	" "

(*
special case Plus to fix stringifying  1.2` + 3  as  1.2`+3
*)
toInputFormString[TokenNode[Token`Plus, _, _]] :=
	" + "

(*
special case Minus to fix stringifying  1.2` - 3  as  1.2`-3
*)
toInputFormString[TokenNode[Token`Minus, _, _]] :=
	" - "

(*
special case Dot to fix stringifying  c_ . _LinearSolve  as  c_._LinearSolve
*)
toInputFormString[TokenNode[Token`Dot, _, _]] :=
	" . "

(*
special case DotDot to fix stringifying  0. .. as 0...
*)
toInputFormString[TokenNode[Token`DotDot, _, _]] :=
	" .."

(*
special case DotDot to fix stringifying  0. ... as 0....
*)
toInputFormString[TokenNode[Token`DotDotDot, _, _]] :=
	" ..."

(*
special case SlashDot to fix stringifying  x /. 0 as x/.0
*)
toInputFormString[TokenNode[Token`SlashDot, _, _]] :=
	" /. "

(*
special case Dot to fix stringifying  x //. 0 as x//.0
*)
toInputFormString[TokenNode[Token`SlashSlashDot, _, _]] :=
	" //. "



toInputFormString[TokenNode[_, str_, _]] :=
	str

(*
special case for a; ;, which is   a Semi InternalNullNode Semi
*)
toInputFormString[InternalNullNode[Null, str_, _]] :=
	" "

toInputFormString[InternalAllNode[All, str_, _]] :=
	str

toInputFormString[InternalOneNode[1, str_, _]] :=
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
toInputFormString is intended for concrete syntax, and concrete syntax
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
	processed = Riffle[nodes, TokenNode[Token`Comma, ",", <||>]];
	toInputFormString[CallNode[SymbolNode[Symbol, "Hold", <||>], {
								GroupNode[GroupSquare, {
									TokenNode[Token`OpenSquare, "[", <||>] } ~Join~
									processed ~Join~
									{ TokenNode[Token`CloseSquare, "]", <||>] }, <||>] }, <||> ]]
]


(*
ConcreteParseString[""] returns Null, so handle that
*)
toInputFormString[Null] := ""




toInputFormString[f_Failure] := f

toInputFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
