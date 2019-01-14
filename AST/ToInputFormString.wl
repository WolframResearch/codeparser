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
Catch[
Module[{os},
	os = ToInputFormString[operand];
	If[FailureQ[op],
		Throw[op]
	];
	SymbolToPrefixOperatorString[op] <> os
]]

ToInputFormString[BinaryNode[op_, {left_, right_}, _]] :=
Catch[
Module[{ls, rs},
	ls = ToInputFormString[left];
	If[FailureQ[ls],
		Throw[ls]
	];
	rs = ToInputFormString[right];
	If[FailureQ[rs],
		Throw[rs]
	];
	ls <> SymbolToBinaryOperatorString[op] <> rs
]]


ToInputFormString[InfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = ToInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, SymbolToInfixOperatorString[op]]]
]]


(* InternalMinusNode stop-gap *)
ToInputFormString[InfixNode[Plus, nodes_, opts_]] :=
Catch[
Module[{first, rest},

	first = ToInputFormString[First[nodes]];
	If[FailureQ[first],
		Throw[first]
	];

	StringJoin[{first,
		(* extra space *)
		Switch[#,
			InternalMinusNode[_, _, _],
				{" - ", ToInputFormString[#[[2]][[1]]] /. {f_FailureQ :> Throw[f]}}
			,
			_,
				{" + ", ToInputFormString[#] /. {f_FailureQ :> Throw[f]}}
		]& /@ Rest[nodes]}]
]]


(* -a *)
ToInputFormString[InfixNode[Times, {NumberNode["-1", {}, _], op_}, opts_]] :=
Catch[
Module[{opStr},
	opStr = ToInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	"-" <> opStr
]]



ToInputFormString[TernaryNode[op_, nodes_, opts_]] :=
Catch[
Module[{pair = SymbolToTernaryPair[op], nodeStrs},
	nodeStrs = ToInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, {SymbolToTernaryOperatorString[pair[[1]]], SymbolToTernaryOperatorString[pair[[2]]]}]]
]]


ToInputFormString[PostfixNode[op_, {operand_}, opts_]] :=
Catch[
Module[{opStr},
	opStr = ToInputFormString[operand];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	opStr <> SymbolToPostfixOperatorString[op]
]]

(*
ToInputFormString is intended for concrete syntax, and concrete syntax
only ever has 1 arg for a Call: i.e. CallNode[head, {GroupNode[GroupSquare, {args}]}]

If you see an unevaluated ToInputFormString[CallNode[head, {arg1, arg2}]], then
that is abstract syntax
*)
ToInputFormString[CallNode[op_, {node_}, opts_]] :=
Catch[
Module[{opStr, ns},
	opStr = ToInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	ns = ToInputFormString[node];
	If[FailureQ[ns],
		Throw[ns]
	];
	opStr <> ns
]]

ToInputFormString[CallMissingCloserNode[op_, {node_}, opts_]] :=
Catch[
Module[{opStr, ns},
	opStr = ToInputFormString[op];
	If[FailureQ[opStr],
		Throw[opStr]
	];
	ns = ToInputFormString[node];
	If[FailureQ[ns],
		Throw[ns]
	];
	opStr <> ns
]]

ToInputFormString[GroupNode[op_, nodes_, opts_]] :=
Catch[
Module[{pair = SymbolToGroupPair[op], ns},
	ns = ToInputFormString /@ nodes;
	If[AnyTrue[ns, FailureQ],
		Throw[SelectFirst[ns, FailureQ]]
	];
	StringJoin[{pair[[1]], ns, pair[[2]]}]
]]





ToInputFormString[BlankNode[Blank, {}, _]] :=
	"_"

ToInputFormString[BlankNode[Blank, {sym2_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym2];
	If[FailureQ[str],
		Throw[str]
	];
	"_" <> str
]]

ToInputFormString[BlankSequenceNode[BlankSequence, {}, _]] :=
	"__"

ToInputFormString[BlankSequenceNode[BlankSequence, {sym2_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym2];
	If[FailureQ[str],
		Throw[str]
	];
	"__" <> str
]]

ToInputFormString[BlankNullSequenceNode[BlankNullSequence, {}, _]] :=
	"___"

ToInputFormString[BlankNullSequenceNode[BlankNullSequence, {sym2_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym2];
	If[FailureQ[str],
		Throw[str]
	];
	"___" <> str
]]

ToInputFormString[OptionalDefaultNode[OptionalDefault, {}, _]] :=
	"_."

ToInputFormString[PatternBlankNode[PatternBlank, {sym1_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "_"
]]

ToInputFormString[PatternBlankNode[PatternBlank, {sym1_, sym2_}, _]] :=
Catch[
Module[{str1, str2},
	str1 = ToInputFormString[sym1];
	If[FailureQ[str1],
		Throw[str1]
	];
	str2 = ToInputFormString[sym2];
	If[FailureQ[str2],
		Throw[str2]
	];
	str1 <> "_" <> str2
]]

ToInputFormString[PatternBlankSequenceNode[PatternBlankSequence, {sym1_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "__"
]]

ToInputFormString[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, sym2_}, _]] :=
Catch[
Module[{str1, str2},
	str1 = ToInputFormString[sym1];
	If[FailureQ[str1],
		Throw[str1]
	];
	str2 = ToInputFormString[sym2];
	If[FailureQ[str2],
		Throw[str2]
	];
	str1 <> "__" <> str2
]]

ToInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "___"
]]

ToInputFormString[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, sym2_}, _]] :=
Catch[
Module[{str1, str2},
	str1 = ToInputFormString[sym1];
	If[FailureQ[str1],
		Throw[str1]
	];
	str2 = ToInputFormString[sym2];
	If[FailureQ[str2],
		Throw[str2]
	];
	str1 <> "___" <> str2
]]

ToInputFormString[OptionalDefaultPatternNode[OptionalDefaultPattern, {sym1_}, _]] :=
Catch[
Module[{str},
	str = ToInputFormString[sym1];
	If[FailureQ[str],
		Throw[str]
	];
	str <> "_."
]]


	

ToInputFormString[InternalTokenNode[str_, _, _]] :=
	str



(*
InternalAllNode
InternalDotNode
InternalNullNode
InternalOneNode

all represent input that was skipped, so InputForm is ""
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
Catch[
Module[{nodeStrs},
	nodeStrs = ToInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[nodeStrs]
]]






ToInputFormString[FileNode[File, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = ToInputFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
]]



ToInputFormString[HoldNode[Hold, nodes_, opts_]] :=
	ToInputFormString[CallNode[ToNode[Hold], {GroupNode[GroupSquare, Riffle[nodes, InternalTokenNode[",", {}, <||>]], <||>]}, <||>]]


(*
ConcreteParseString[""] returns Null, so handle that
*)
ToInputFormString[Null] := ""




ToInputFormString[f_FailureQ] := f


ToInputFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToInputFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
