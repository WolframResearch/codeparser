BeginPackage["AST`ToInputFormString`"]

Begin["`Private`"]

Needs["AST`"]

(*
ToInputFormString is intended for concrete syntax trees
*)

ToInputFormString[SymbolNode[str_, _, _]] :=
	str

ToInputFormString[StringNode[str_, _, _]] :=
	str

ToInputFormString[NumberNode[str_, _, _]] :=
	str

ToInputFormString[SlotNode[str_, _, _]] :=
	str

ToInputFormString[SlotSequenceNode[str_, _, _]] :=
	str

ToInputFormString[OutNode[str_, _, _]] :=
	str




ToInputFormString[PrefixNode[op_, {operand_}, _]] :=
Module[{os},
	os = ToInputFormString[operand];
	SymbolToPrefixOperatorString[op] <> os /; StringQ[os]
]

ToInputFormString[BinaryNode[op_, {left_, right_}, _]] :=
Module[{ls, rs},
	ls = ToInputFormString[left];
	rs = ToInputFormString[right];
	ls <> SymbolToBinaryOperatorString[op] <> rs /; (StringQ[ls] && StringQ[rs])
]


ToInputFormString[InfixNode[op_, nodes_, _]] :=
Module[{},
	StringJoin[Riffle[ToInputFormString /@ nodes, SymbolToInfixOperatorString[op]]]
]

(*
ToInputFormString[InfixNode[Plus, nodes_, opts_]] :=
	StringJoin[{ToInputFormString[First[nodes]],
		(* extra space *)
		Switch[#,
			InternalMinusNode[_, _, _],
				{" - ", ToInputFormString[#]}
			,
			_,
				{" + ", ToInputFormString[#]}
		]& /@ Rest[nodes]}]

ToInputFormString[InternalMinusNode[Minus, {operand_}, _]] :=
	ToInputFormString[operand]
*)

(* -1/a *)
(*
ToInputFormString[InfixNode[Times, {NumberNode["-1", {}, _], InternalSlashNode[Divide, {op_}, _]}, _]] :=
	"-1/"<>ToInputFormString[op]
*)

(* -a *)
ToInputFormString[InfixNode[Times, {NumberNode["-1", {}, _], op_}, opts_]] :=
Module[{},
	"-"<>ToInputFormString[op]
]

(*
ToInputFormString[InfixNode[Times, nodes_, opts_]] :=
	StringJoin[{ToInputFormString[First[nodes]],
		(* extra space *)
		Switch[#,
			InternalSlashNode[_, _, _],
				{" / ", ToInputFormString[#]}
			,
			InternalImplicitTimesNode[_, _, _],
				{" ", ToInputFormString[#]}
			,
			_,
				{" * ", ToInputFormString[#]}
		]& /@ Rest[nodes]}]

ToInputFormString[InternalSlashNode[Divide, {operand_}, _]] :=
	ToInputFormString[operand]

ToInputFormString[InternalImplicitTimesNode[ImplicitTimes, {operand_}, _]] :=
	ToInputFormString[operand]
*)

(*
ToInputFormString[InfixNode[Inequality, children_, _]] :=
Module[{rators, rands},
	rators = children[[2;;-2;;2]];
	rands = children[[1;;-1;;2]];
	StringJoin[Riffle[ToInputFormString /@ rands, SymbolToInfixOperatorString[FromNode[#]]& /@ rators]]
]
*)



ToInputFormString[TernaryNode[op_, {left_, middle_, right_}, opts_]] :=
Module[{pair = SymbolToTernaryPair[op]},
	ToInputFormString[left] <> SymbolToTernaryOperatorString[pair[[1]]] <> ToInputFormString[middle] <>
		SymbolToTernaryOperatorString[pair[[2]]] <> ToInputFormString[right]
]


ToInputFormString[PostfixNode[op_, {operand_}, opts_]] :=
Module[{},
	ToInputFormString[operand] <> SymbolToPostfixOperatorString[op]
]

(*
ToInputFormString is intended for concrete syntax, and concrete syntax
only ever has 1 arg for a Call: i.e. CallNode[head, {GroupNode[GroupSquare, {}]}]

If you see an unevaluated ToInputFormString[CallNode[head, {arg1, arg2}]], then
that is abstract syntax
*)
ToInputFormString[CallNode[op_, {node_}, opts_]] :=
Module[{ns},
	ns = ToInputFormString[node];
	ToInputFormString[op] <> ns /; StringQ[ns]
]

ToInputFormString[CallMissingCloserNode[op_, {node_}, opts_]] :=
Module[{},
	ToInputFormString[op] <> ToInputFormString[node]
]

ToInputFormString[GroupNode[op_, nodes_, opts_]] :=
Module[{pair = SymbolToGroupPair[op], ns},
	ns = ToInputFormString /@ nodes;
	StringJoin[{pair[[1]], ns, pair[[2]]}] /; AllTrue[ns, StringQ]
]





ToInputFormString[BlankNode[Blank, {}, _]] :=
	"_"

ToInputFormString[BlankNode[Blank, {sym2_}, _]] :=
Module[{},
	"_" <> ToInputFormString[sym2]
]

ToInputFormString[BlankSequenceNode[BlankSequence, {}, _]] :=
	"__"

ToInputFormString[BlankSequenceNode[BlankSequence, {sym2_}, _]] :=
Module[{},
	"__" <> ToInputFormString[sym2]
]

ToInputFormString[BlankNullSequenceNode[BlankNullSequence, {}, _]] :=
	"___"

ToInputFormString[BlankNullSequenceNode[BlankNullSequence, {sym2_}, _]] :=
Module[{},
	"___" <> ToInputFormString[sym2]
]

ToInputFormString[OptionalDefaultNode[OptionalDefault, {}, _]] :=
	"_."

ToInputFormString[PatternBlankNode[PatternBlank, {sym1_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "_"
]

ToInputFormString[PatternBlankNode[PatternBlank, {sym1_, sym2_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "_" <> ToInputFormString[sym2]
]

ToInputFormString[PatternBlankSequenceNode[PatternBlankSequence, {sym1_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "__"
]

ToInputFormString[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, sym2_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "__" <> ToInputFormString[sym2]
]

ToInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "___"
]

ToInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, sym2_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "___" <> ToInputFormString[sym2]
]

ToInputFormString[OptionalDefaultPatternNode[OptionalDefaultPattern, {sym1_}, _]] :=
Module[{},
	ToInputFormString[sym1] <> "_."
]


	

ToInputFormString[InternalTokenNode[str_, _, _]] :=
	str



(*
InternalAllNode
InternalDotNode
InternalNullNode
InternalOneNode

all represent input that was skipped
*)

ToInputFormString[InternalAllNode[All, _, _]] :=
	""

ToInputFormString[InternalDotNode[Dot, _, _]] :=
	""

ToInputFormString[InternalNullNode[Null, _, _]] :=
	""

ToInputFormString[InternalOneNode[1, _, _]] :=
	""




ToInputFormString[SyntaxErrorNode[tok_, nodes_, opts_]] :=
Module[{},
	StringJoin[ToInputFormString /@ nodes]
]






ToInputFormString[FileNode[File, nodes_, opts_]] :=
Module[{},
	StringJoin[Riffle[ToInputFormString /@ nodes, "\n"]]
]

ToInputFormString[HoldNode[Hold, nodes_, opts_]] :=
	ToInputFormString[CallNode[ToNode[Hold], {GroupNode[GroupSquare, Riffle[nodes, InternalTokenNode[",", {}, <||>]], <||>]}, <||>]]


(*
ParseString[""] returns Null, so handle that
*)
ToInputFormString[Null] := ""




ToInputFormString[args___] := Failure["Unhandled", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]



End[]

EndPackage[]
