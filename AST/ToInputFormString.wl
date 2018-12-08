BeginPackage["AST`ToInputFormString`"]


Begin["`Private`"]

Needs["AST`"]


ToInputFormString[SymbolNode[str_, _, _]] :=
	str

ToInputFormString[StringNode[str_, _, _]] :=
	str

ToInputFormString[NumberNode[str_, _, _]] :=
	str

ToInputFormString[SyntaxErrorNode[tok_, nodes_, opts_]] :=
	StringJoin[ToInputFormString /@ nodes]



ToInputFormString[BlankNode[_, {sym2_}, _]] :=
	"_" <> ToInputFormString[sym2]

ToInputFormString[BlankSequenceNode[_, {sym2_}, _]] :=
	"__" <> ToInputFormString[sym2]

ToInputFormString[BlankNullSequenceNode[_, {sym2_}, _]] :=
	"___" <> ToInputFormString[sym2]

ToInputFormString[PatternBlankNode[_, {sym1_, sym2_}, _]] :=
	ToInputFormString[sym1] <> "_" <> ToInputFormString[sym2]

ToInputFormString[PatternBlankSequenceNode[_, {sym1_, sym2_}, _]] :=
	ToInputFormString[sym1] <> "__" <> ToInputFormString[sym2]

ToInputFormString[PatternBlankNullSequenceNode[_, {sym1_, sym2_}, _]] :=
	ToInputFormString[sym1] <> "___" <> ToInputFormString[sym2]

ToInputFormString[OptionalDefaultNode[_, {sym1_}, _]] :=
	ToInputFormString[sym1] <> "_."

ToInputFormString[SlotNode[str_, _, _]] :=
	str

ToInputFormString[SlotSequenceNode[str_, _, _]] :=
	str

ToInputFormString[OutNode[str_, _, _]] :=
	str

ToInputFormString[InternalEmptyNode[_, _, _]] :=
	""



ToInputFormString[PrefixNode[op_, {operand_}, _]] :=
	SymbolToPrefixOperatorString[op] <> ToInputFormString[operand]


ToInputFormString[BinaryNode[op_, {left_, right_}, _]] :=
	ToInputFormString[left] <> SymbolToBinaryOperatorString[op] <> ToInputFormString[right]



ToInputFormString[InfixNode[op_, nodes_, opts_]] :=
	StringJoin[Riffle[ToInputFormString /@ nodes, SymbolToInfixOperatorString[op]]]

ToInputFormString[InfixNode[Plus, nodes_, opts_]] :=
Module[{},
	StringJoin[{ToInputFormString[First[nodes]],
		(* extra space *)
		(If[MatchQ[#, InternalMinusNode[___]],
			{" - ", ToInputFormString[#]},
			{" + ", ToInputFormString[#]}])& /@ Rest[nodes]}]
]

ToInputFormString[InternalMinusNode[_, {operand_}, _]] :=
	ToInputFormString[operand]




ToInputFormString[TernaryNode[op_, {left_, middle_, right_}, opts_]] :=
Module[{pair = SymbolToTernaryPair[op]},
	ToInputFormString[left] <> SymbolToTernaryOperatorString[pair[[1]]] <> ToInputFormString[middle] <>
		SymbolToTernaryOperatorString[pair[[2]]] <> ToInputFormString[right]
]


ToInputFormString[PostfixNode[op_, {operand_}, opts_]] :=
	ToInputFormString[operand] <> SymbolToPostfixOperatorString[op]

ToInputFormString[PostfixNode[Derivative, {operand_}, opts_]] :=
	ToInputFormString[operand] <> Table["'", opts[DerivativeOrder]]



ToInputFormString[CallNode[op_, nodes_, opts_]] :=
	ToInputFormString[op] <> ToInputFormString[GroupNode[GroupSquare, nodes, opts]]

ToInputFormString[CallMissingCloserNode[op_, nodes_, opts_]] :=
	ToInputFormString[op] <> ToInputFormString[GroupNode[GroupMissingCloserSquare, nodes, opts]]

ToInputFormString[PartNode[op_, nodes_, opts_]] :=
	ToInputFormString[op] <> ToInputFormString[GroupNode[GroupDoubleBracket, nodes, opts]]

ToInputFormString[GroupNode[op_, nodes_, opts_]] :=
Module[{pair = SymbolToGroupPair[op]},
	StringJoin[{pair[[1]], Riffle[ToInputFormString /@ nodes, ", "], pair[[2]]}]
]

ToInputFormString[GroupNode[op:GroupLinearSyntaxParen, nodes_, opts_]] :=
Module[{pair = SymbolToGroupPair[op]},
	StringJoin[{pair[[1]], ToInputFormString /@ nodes, pair[[2]]}]
]

ToInputFormString[GroupNode[op:GroupMissingCloserLinearSyntaxParen, nodes_, opts_]] :=
Module[{pair = SymbolToGroupPair[op]},
	StringJoin[{pair[[1]], ToInputFormString /@ nodes, pair[[2]]}]
]




ToInputFormString[InternalTokenNode[str_, _, _]] :=
	str

ToInputFormString[FileNode[_, nodes_, opts_]] :=
Module[{},
	StringJoin[Riffle[ToInputFormString /@ nodes, "\n\n"]]
]

End[]

EndPackage[]

