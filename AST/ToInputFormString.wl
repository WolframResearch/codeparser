BeginPackage["AST`ToInputFormString`"]

Begin["`Private`"]

Needs["AST`"]


(*
ToInputFormString is intended for concrete syntax trees
*)

ToInputFormString[cst_] :=
Block[{$RecursionLimit = Infinity},
	toInputFormString[cst]
]


toInputFormString[SymbolNode[str_, _, _]] :=
	str

toInputFormString[StringNode[str_, _, _]] :=
	str

toInputFormString[IntegerNode[str_, _, _]] :=
	str

toInputFormString[RealNode[str_, _, _]] :=
	str

toInputFormString[SlotNode[str_, _, _]] :=
	str

toInputFormString[SlotSequenceNode[str_, _, _]] :=
	str

toInputFormString[OutNode[str_, _, _]] :=
	str




toInputFormString[PrefixNode[op_, {operand_}, _]] :=
Catch[
Module[{os},
	os = toInputFormString[operand];
	If[FailureQ[op],
		Throw[op]
	];
	SymbolToPrefixOperatorString[op] <> os
]]

toInputFormString[BinaryNode[op_, {left_, right_}, _]] :=
Catch[
Module[{ls, rs},
	ls = toInputFormString[left];
	If[FailureQ[ls],
		Throw[ls]
	];
	rs = toInputFormString[right];
	If[FailureQ[rs],
		Throw[rs]
	];
	ls <> SymbolToBinaryOperatorString[op] <> rs
]]


toInputFormString[InfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, SymbolToInfixOperatorString[op]]]
]]


(* -a *)
toInputFormString[InfixNode[Times, {IntegerNode["-1", {}, _], op_}, opts_]] :=
Catch[
Module[{opStr},
	opStr = toInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	"-" <> opStr
]]

toInputFormString[InfixNode[Times, {RealNode["-1", {}, _], op_}, opts_]] :=
Catch[
Module[{opStr},
	opStr = toInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	"-" <> opStr
]]




toInputFormString[TernaryNode[op_, nodes_, opts_]] :=
Catch[
Module[{pair = SymbolToTernaryPair[op], nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, {SymbolToTernaryOperatorString[pair[[1]]], SymbolToTernaryOperatorString[pair[[2]]]}]]
]]


toInputFormString[PostfixNode[op_, {operand_}, opts_]] :=
Catch[
Module[{opStr},
	opStr = toInputFormString[operand];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	opStr <> SymbolToPostfixOperatorString[op]
]]

(*
toInputFormString is intended for concrete syntax, and concrete syntax
only ever has 1 arg for a Call: i.e. CallNode[head, {GroupNode[GroupSquare, {args}]}]

If you see an unevaluated toInputFormString[CallNode[head, {arg1, arg2}]], then
that is abstract syntax
*)
toInputFormString[CallNode[op_, {node_}, opts_]] :=
Catch[
Module[{opStr, ns},
	opStr = toInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	ns = toInputFormString[node];
	If[FailureQ[ns],
		Throw[ns]
	];
	opStr <> ns
]]

toInputFormString[CallMissingCloserNode[op_, {node_}, opts_]] :=
Catch[
Module[{opStr, ns},
	opStr = toInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	ns = toInputFormString[node];
	If[FailureQ[ns],
		Throw[ns]
	];
	opStr <> ns
]]

toInputFormString[GroupNode[op_, nodes_, opts_]] :=
Catch[
Module[{pair = SymbolToGroupPair[op], ns},
	ns = toInputFormString /@ nodes;
	If[AnyTrue[ns, FailureQ],
		Throw[SelectFirst[ns, FailureQ]]
	];
	StringJoin[{pair[[1]], ns, pair[[2]]}]
]]





toInputFormString[BlankNode[Blank, {}, _]] :=
	"_"

toInputFormString[BlankNode[Blank, {sym2_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym2];
	If[FailureQ[str],
		Throw[str]
	];
	"_" <> str
]]

toInputFormString[BlankSequenceNode[BlankSequence, {}, _]] :=
	"__"

toInputFormString[BlankSequenceNode[BlankSequence, {sym2_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym2];
	If[FailureQ[str],
		Throw[str]
	];
	"__" <> str
]]

toInputFormString[BlankNullSequenceNode[BlankNullSequence, {}, _]] :=
	"___"

toInputFormString[BlankNullSequenceNode[BlankNullSequence, {sym2_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym2];
	If[FailureQ[str],
		Throw[str]
	];
	"___" <> str
]]

toInputFormString[OptionalDefaultNode[OptionalDefault, {}, _]] :=
	"_."

toInputFormString[PatternBlankNode[PatternBlank, {sym1_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "_"
]]

toInputFormString[PatternBlankNode[PatternBlank, {sym1_, sym2_}, _]] :=
Catch[
Module[{str1, str2},
	str1 = toInputFormString[sym1];
	If[FailureQ[str1],
		Throw[str1]
	];
	str2 = toInputFormString[sym2];
	If[FailureQ[str2],
		Throw[str2]
	];
	str1 <> "_" <> str2
]]

toInputFormString[PatternBlankSequenceNode[PatternBlankSequence, {sym1_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "__"
]]

toInputFormString[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, sym2_}, _]] :=
Catch[
Module[{str1, str2},
	str1 = toInputFormString[sym1];
	If[FailureQ[str1],
		Throw[str1]
	];
	str2 = toInputFormString[sym2];
	If[FailureQ[str2],
		Throw[str2]
	];
	str1 <> "__" <> str2
]]

toInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "___"
]]

toInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, sym2_}, _]] :=
Catch[
Module[{str1, str2},
	str1 = toInputFormString[sym1];
	If[FailureQ[str1],
		Throw[str1]
	];
	str2 = toInputFormString[sym2];
	If[FailureQ[str2],
		Throw[str2]
	];
	str1 <> "___" <> str2
]]

toInputFormString[OptionalDefaultPatternNode[OptionalDefaultPattern, {sym1_}, _]] :=
Catch[
Module[{str},
	str = toInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "_."
]]


	

toInputFormString[TokenNode[_, str_, _]] :=
	str



(*
InternalAllNode
InternalDotNode
InternalNullNode
InternalOneNode

all represent input that was skipped, so InputForm is ""
*)

toInputFormString[InternalAllNode[All, _, _]] :=
	""

toInputFormString[InternalDotNode[Dot, _, _]] :=
	""

toInputFormString[InternalNullNode[Null, _, _]] :=
	""

toInputFormString[InternalOneNode[1, _, _]] :=
	""


toInputFormString[CommentNode[str_, _, _]] :=
	str




toInputFormString[SyntaxErrorNode[tok_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]






toInputFormString[FileNode[File, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
]]





toInputFormString[HoldNode[Hold, nodes_, opts_]] :=
Module[{x, lastNonCommentIndex, processed},
	x = 1;
	lastNonCommentIndex = 0;
	processed = Flatten[(If[Head[#] === CommentNode,
        x++;
        #
        ,
        lastNonCommentIndex = x;
        x += 2;
        {#, TokenNode[Token`Operator`Comma, ",", <||>]}
        ])& /@ nodes];
	If[lastNonCommentIndex > 0,
		processed = Delete[processed, lastNonCommentIndex + 1];
	];
	toInputFormString[CallNode[ToNode[Hold], {
		GroupNode[GroupSquare, processed, <||>] }, <||>]]
]


(*
ConcreteParseString[""] returns Null, so handle that
*)
toInputFormString[Null] := ""




toInputFormString[f_Failure] := f

toInputFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
