BeginPackage["AST`Abstract`"]

Abstract::usage = "Abstract[concrete] returns an abstract syntax tree from a concrete syntax tree."

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]



(*
what keys do we want to keep when abstracting?
*)
keysToTake = {Source, AbstractSyntaxIssues}


Abstract[cst_] :=
Block[{$RecursionLimit = Infinity},
	abstract[cst]
]



abstract[s_SymbolNode] := s
abstract[s_StringNode] := s
abstract[n_NumberNode] := n



abstract[SlotNode["#", {}, data_]] := CallNode[ToNode[Slot], {ToNode[1]}, KeyTake[data, keysToTake]]
abstract[SlotNode[str_ /; StringMatchQ[str, "#"~~DigitCharacter..], {}, data_]] := CallNode[ToNode[Slot], {ToNode[FromDigits[StringDrop[str, 1]]]}, KeyTake[data, keysToTake]]
abstract[SlotNode[str_, {}, data_]] := CallNode[ToNode[Slot], {ToNode[StringDrop[str, 1]]}, KeyTake[data, keysToTake]]
abstract[SlotSequenceNode["##", {}, data_]] := CallNode[ToNode[SlotSequence], {ToNode[1]}, KeyTake[data, keysToTake]]
abstract[SlotSequenceNode[str_ /; StringMatchQ[str, "##"~~DigitCharacter..], {}, data_]] := CallNode[ToNode[SlotSequence], {ToNode[FromDigits[StringDrop[str, 2]]]}, KeyTake[data, keysToTake]]
abstract[OutNode["%", {}, data_]] := CallNode[ToNode[Out], {}, KeyTake[data, keysToTake]]




abstract[PrefixNode[Minus, {op_}, data_]] := abstract[negate[op, KeyTake[data, keysToTake]]]
abstract[PrefixNode[PrefixLinearSyntaxBang, children:{Except[GroupNode[GroupLinearSyntaxParen, _, _]]}, data_]] := SyntaxErrorNode[Token`Operator`LinearSyntax`Bang, children, KeyTake[data, keysToTake]]
(*
FIXME: keep linear syntax for now
*)
abstract[PrefixNode[PrefixLinearSyntaxBang, children_, data_]] := PrefixNode[PrefixLinearSyntaxBang, children, KeyTake[data, keysToTake]]
abstract[PrefixNode[op_, {operand_}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]


abstract[PostfixNode[Derivative, {operand_}, data_]] := abstractDerivative[PostfixNode[Derivative, {operand}, KeyTake[data, keysToTake]]]
abstract[PostfixNode[op_, {operand_}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]




abstract[minus:BinaryNode[Minus, _, _]] := abstractPlus[minus]
abstract[times:BinaryNode[Divide, _, _]] := abstractTimes[times]

abstract[BinaryNode[Equal, children_, data_]] := abstractInequality[BinaryNode[Equal, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Unequal, children_, data_]] := abstractInequality[BinaryNode[Unequal, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Less, children_, data_]] := abstractInequality[BinaryNode[Less, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Greater, children_, data_]] := abstractInequality[BinaryNode[Greater, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[LessEqual, children_, data_]] := abstractInequality[BinaryNode[LessEqual, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[GreaterEqual, children_, data_]] := abstractInequality[BinaryNode[GreaterEqual, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[BinaryAt, {left_, right_}, data_]] := CallNode[abstract[left], {abstract[right]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinaryAtAtAt, {left_, right_}, data_]] := CallNode[ToNode[Apply], abstract /@ {left, right, GroupNode[List, {ToNode[1]}, <||>]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinarySlashSlash, {left_, right_}, data_]] := CallNode[abstract[right], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[SameQ, children_, data_]] := abstractSameQ[BinaryNode[SameQ, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[UnsameQ, children_, data_]] := abstractUnsameQ[BinaryNode[UnsameQ, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Composition, children_, data_]] := abstractComposition[BinaryNode[Composition, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[RightComposition, children_, data_]] := abstractRightComposition[BinaryNode[RightComposition, children, KeyTake[data, keysToTake]]]

abstract[BinaryNode[Divide, children_, data_]] := abstractTimes[BinaryNode[Divide, children, KeyTake[data, keysToTake]]]

(* NonAssociative errors *)
abstract[BinaryNode[PatternTest, children:{BinaryNode[PatternTest, _, _], _}, data_]] := SyntaxErrorNode[Token`Operator`Question, children, KeyTake[data, keysToTake]]
abstract[BinaryNode[DirectedEdge, children:{BinaryNode[DirectedEdge, _, _], _}, data_]] := SyntaxErrorNode[Token`Operator`LongName`DirectedEdge, children, KeyTake[data, keysToTake]]
abstract[BinaryNode[UndirectedEdge, children:{BinaryNode[UndirectedEdge, _, _], _}, data_]] := SyntaxErrorNode[Token`Operator`LongName`UndirectedEdge, children, KeyTake[data, keysToTake]]

abstract[BinaryNode[Span, {InternalOneNode[1, {}, _], InternalAllNode[All, {}, _]}, data_]] := CallNode[ToNode[Span], {ToNode[1], ToNode[All]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[Span, {left_, InternalAllNode[All, {}, _]}, data_]] := CallNode[ToNode[Span], {abstract[left], ToNode[All]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[Span, {InternalOneNode[1, {}, _], right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], abstract[right]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[Unset, {left_, InternalDotNode[Dot, {}, _]}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[op_, {left_, right_}, data_]] := CallNode[ToNode[op], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]




abstract[InfixNode[Plus, children_, data_]] := abstractPlus[InfixNode[Plus, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[InfixImplicitPlus, children_, data_]] := abstractPlus[InfixNode[InfixImplicitPlus, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[Times, children_, data_]] := abstractTimes[InfixNode[Times, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[ImplicitTimes, children_, data_]] := abstractTimes[InfixNode[ImplicitTimes, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[InfixInvisibleTimes, children_, data_]] := abstractTimes[InfixNode[InfixInvisibleTimes, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[InfixTimes, children_, data_]] := abstractTimes[InfixNode[InfixTimes, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[And, children_, data_]] := abstractAnd[InfixNode[And, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[Or, children_, data_]] := abstractOr[InfixNode[Or, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[Divisible, children_, data_]] := abstractDivisible[InfixNode[Divisible, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[CompoundExpression, children_, data_]] := abstractCompoundExpression[InfixNode[CompoundExpression, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[StringJoin, children_, data_]] := abstractStringJoin[InfixNode[StringJoin, children, KeyTake[data, keysToTake]]]
abstract[InfixNode[op_, nodes_, data_]] := CallNode[ToNode[op], abstract /@ nodes, KeyTake[data, keysToTake]]






abstract[TernaryNode[TernaryTilde, {left_, middle_, right_}, data_]] := CallNode[abstract[middle], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[TagSet, {left_, middle_, right_}, data_]] := CallNode[ToNode[TagSet], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[TagSetDelayed, {left_, middle_, right_}, data_]] := CallNode[ToNode[TagSetDelayed], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[TagUnset, {left_, middle_, InternalDotNode[Dot, {}, _]}, data_]] := CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[Span, {InternalOneNode[1, {}, _], InternalAllNode[All, {}, _], right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], ToNode[All], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {InternalOneNode[1, {}, _], middle_, right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {left_, InternalAllNode[All, {}, _], right_}, data_]] := CallNode[ToNode[Span], {abstract[left], ToNode[All], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {left_, middle_, right_}, data_]] := CallNode[ToNode[Span], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[MessageName, {left_, middle_, right_}, data_]] := CallNode[ToNode[MessageName], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]




(* handle CallNode before possible GroupNode errors *)
abstract[CallNode[op_, children_, data_]] := abstractCallNode[CallNode[op, children, KeyTake[data, keysToTake]]]
abstract[CallMissingCloserNode[p[_, children_, data_]]] := abstractCallNode[CallMissingCloserNode[op, children, KeyTake[data, keysToTake]]]


(*
take care of specific GroupNodes before calling abstractGroupNode
*)
abstract[GroupNode[GroupParen, {child_}, data_]] := abstract[child]

(* GroupNode errors *)
abstract[GroupNode[GroupSquare, children_, data_]] := SyntaxErrorNode[Token`Operator`OpenSquare, children, KeyTake[data, keysToTake]]
abstract[GroupNode[GroupParen, children_, data_]] := SyntaxErrorNode[Token`Operator`OpenParen, children, KeyTake[data, keysToTake]]

(*
FIXME: skip abstracting linear syntax for now
GroupLinearSyntaxParen retains its commas, so handle before abstractGroupNode
*)
abstract[GroupNode[GroupLinearSyntaxParen, children_, data_]] := GroupNode[GroupLinearSyntaxParen, children, KeyTake[data, keysToTake]]
abstract[GroupNode[op_, children_, data_]] := abstractGroupNode[GroupNode[op, children, KeyTake[data, keysToTake]]]




abstract[BlankNode[Blank, {}, data_]] := CallNode[ToNode[Blank], {}, KeyTake[data, keysToTake]]
abstract[BlankNode[Blank, {sym2_}, data_]] := CallNode[ToNode[Blank], {sym2}, KeyTake[data, keysToTake]]
abstract[BlankSequenceNode[BlankSequence, {}, data_]] := CallNode[ToNode[BlankSequence], {}, KeyTake[data, keysToTake]]
abstract[BlankSequenceNode[BlankSequence, {sym2_}, data_]] := CallNode[ToNode[BlankSequence], {sym2}, KeyTake[data, keysToTake]]
abstract[BlankNullSequenceNode[BlankNullSequence, {}, data_]] := CallNode[ToNode[BlankNullSequence], {}, KeyTake[data, keysToTake]]
abstract[BlankNullSequenceNode[BlankNullSequence, {sym2_}, data_]] := CallNode[ToNode[BlankNullSequence], {sym2}, KeyTake[data, keysToTake]]
abstract[OptionalDefaultNode[OptionalDefault, {}, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]

abstract[PatternBlankNode[PatternBlank, {sym1_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNode[PatternBlank, {sym1_, sym2_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[Blank], {sym2}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankSequenceNode[PatternBlankSequence, {sym1_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankSequence], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, sym2_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankSequence], {sym2}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankNullSequence], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, sym2_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankNullSequence], {sym2}, <||>]}, KeyTake[data, keysToTake]]
abstract[OptionalDefaultPatternNode[OptionalDefaultPattern, {sym1_}, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[Blank], {}, <||>]}, <||>]}, KeyTake[data, keysToTake]]



abstract[FileNode[File, children_, data_]] := FileNode[File, abstract /@ children, KeyTake[data, keysToTake]]
abstract[HoldNode[Hold, children_, data_]] := HoldNode[Hold, abstract /@ children, KeyTake[data, keysToTake]]



abstract[Null] := Null

abstract[CommentNode[_, _, _]] := Nothing



(*
Just pass SyntaxErrorNode pass
*)
abstract[SyntaxErrorNode[op_, children_, data_]] := SyntaxErrorNode[op, children, KeyTake[data, keysToTake]]



abstract[f_Failure] := f









(*
concrete syntax does not have negated numbers
abstract syntax is allowed to have negated numbers
*)
negate[NumberNode[str_, {}, data1_], data_] :=
	NumberNode["-"<>str, {}, data]


(*
Important to use InfixNode[Times and not just CallNode[Times,

This allows these nodes to be merged later e.g., 1-a/b
*)

negate[InfixNode[Times, children_, data1_], data_] :=
	InfixNode[Times, {ToNode[-1]}~Join~children, data]

negate[InfixNode[ImplicitTimes, children_, data1_], data_] :=
	InfixNode[Times, {ToNode[-1]}~Join~children, data]

negate[InfixNode[InfixInvisibleTimes, children_, data1_], data_] :=
	InfixNode[Times, {ToNode[-1]}~Join~children, data]

negate[InfixNode[InfixTimes, children_, data1_], data_] :=
	InfixNode[Times, {ToNode[-1]}~Join~children, data]

negate[node_, data_] :=
	InfixNode[Times, {ToNode[-1], node}, data]


reciprocate[node_, data_] :=
	CallNode[ToNode[Power], {node, ToNode[-1]}, data]




(*
abstract syntax of  +a + b - c \[ImplicitPlus] d  is a single Plus expression
except when it's not, bug 365287
*)
flattenPlus[nodes_List, data_] :=
	Module[{},
		(
			Switch[#,
				PrefixNode[Plus, _, _],
					flattenPlus[{#[[2]][[1]]}, data]
				,
				InfixNode[Plus, _, _],
					flattenPlus[#[[2]], data]
				,
				BinaryNode[Minus, _, _],
					flattenPlus[{First[#[[2]]], negate[#, data]& /@ Rest[#[[2]]]}, data]
				,
				InfixNode[InfixImplicitPlus, _, _],
					flattenPlus[#[[2]], data]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractPlus[InfixNode[Plus, children_, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[children, data]], data]

abstractPlus[BinaryNode[Minus, children_, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[{First[children], negate[#, data]& /@ Rest[children]}, data]], data]

abstractPlus[InfixNode[InfixImplicitPlus, children_, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[children, data]], data]

(*
abstract syntax of  -a * b / c d \[InvisibleTimes] e \[Times] f  is a single Times expression
*)
flattenTimes[nodes_List, data_] :=
	Module[{},
		(
			Switch[#,
				PrefixNode[Minus, {NumberNode[_, _, _]}, _],
					{negate[#[[2,1]], data]}
				,
				PrefixNode[Minus, _, _],
					{ToNode[-1], #[[2]]}
				,
				InfixNode[Times, _, _],
					flattenTimes[#[[2]], data]
				,
				BinaryNode[Divide, _, _],
					flattenTimes[{#[[2]][[1]], reciprocate[#[[2]][[2]], data]}, data]
				,
				InfixNode[ImplicitTimes, _, _],
					flattenTimes[#[[2]], data]
				,
				InfixNode[InfixInvisibleTimes, _, _],
					flattenTimes[#[[2]], data]
				,
				InfixNode[InfixTimes, _, _],
					flattenTimes[#[[2]], data]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractTimes[InfixNode[Times, children_, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[children, data]], data]

abstractTimes[BinaryNode[Divide, children_, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[{children[[1]], reciprocate[children[[2]], data]}, data]], data]

abstractTimes[InfixNode[ImplicitTimes, children_, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[children, data]], data]

abstractTimes[InfixNode[InfixInvisibleTimes, children_, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[children, data]], data]

abstractTimes[InfixNode[InfixTimes, children_, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[children, data]], data]





(*
abstract syntax of  a == b != c < d > e <= f >= g  is a single Inequality expression
*)
flattenInequality[nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[Equal, _, _],
					flattenInequality[{#[[2]][[1]], ToNode[Equal], #[[2]][[2]]}]
				,
				BinaryNode[Unequal, _, _],
					flattenInequality[{#[[2]][[1]], ToNode[Unequal], #[[2]][[2]]}]
				,
				BinaryNode[Less, _, _],
					flattenInequality[{#[[2]][[1]], ToNode[Less], #[[2]][[2]]}]
				,
				BinaryNode[Greater, _, _],
					flattenInequality[{#[[2]][[1]], ToNode[Greater], #[[2]][[2]]}]
				,
				BinaryNode[LessEqual, _, _],
					flattenInequality[{#[[2]][[1]], ToNode[LessEqual], #[[2]][[2]]}]
				,
				BinaryNode[GreaterEqual, _, _],
					flattenInequality[{#[[2]][[1]], ToNode[GreaterEqual], #[[2]][[2]]}]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractInequality[BinaryNode[Equal, {left_, right_}, data_]] :=
	simplifyInequality[InfixNode[Inequality, Flatten[flattenInequality[{left, ToNode[Equal], right}]], data]]

abstractInequality[BinaryNode[Unequal, {left_, right_}, data_]] :=
	simplifyInequality[InfixNode[Inequality, Flatten[flattenInequality[{left, ToNode[Unequal], right}]], data]]

abstractInequality[BinaryNode[Less, {left_, right_}, data_]] :=
	simplifyInequality[InfixNode[Inequality, Flatten[flattenInequality[{left, ToNode[Less], right}]], data]]

abstractInequality[BinaryNode[Greater, {left_, right_}, data_]] :=
	simplifyInequality[InfixNode[Inequality, Flatten[flattenInequality[{left, ToNode[Greater], right}]], data]]

abstractInequality[BinaryNode[LessEqual, {left_, right_}, data_]] :=
	simplifyInequality[InfixNode[Inequality, Flatten[flattenInequality[{left, ToNode[LessEqual], right}]], data]]

abstractInequality[BinaryNode[GreaterEqual, {left_, right_}, data_]] :=
	simplifyInequality[InfixNode[Inequality, Flatten[flattenInequality[{left, ToNode[GreaterEqual], right}]], data]]

(*
attempt to simplify e.g. Inequality[a, Less, b, Less, c] to Less[a, b, c]
*)
simplifyInequality[InfixNode[Inequality, children_, data_]] :=
Module[{rators, rands},
	rators = children[[2;;-2;;2]];
	rands = children[[1;;-1;;2]];

	Switch[rators,
		{ToNode[Equal]..},
			CallNode[ToNode[Equal], abstract /@ rands, data]
		,
		{ToNode[Unequal]..},
			CallNode[ToNode[Unequal], abstract /@ rands, data]
		,
		{ToNode[Less]..},
			CallNode[ToNode[Less], abstract /@ rands, data]
		,
		{ToNode[Greater]..},
			CallNode[ToNode[Greater], abstract /@ rands, data]
		,
		{ToNode[LessEqual]..},
			CallNode[ToNode[LessEqual], abstract /@ rands, data]
		,
		{ToNode[GreaterEqual]..},
			CallNode[ToNode[GreaterEqual], abstract /@ rands, data]
		,
		_,
			CallNode[ToNode[Inequality], abstract /@ children, data]
	]
]





(*
abstract syntax of  a === b === c  is a single SameQ expression
*)
flattenSameQ[nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[SameQ, _, _],
					flattenSameQ[#[[2]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractSameQ[BinaryNode[SameQ, children_, data_]] :=
	CallNode[ToNode[SameQ], abstract /@ Flatten[flattenSameQ[children]], data]



(*
abstract syntax of  a =!= b =!= c  is a single UnsameQ expression
*)
flattenUnsameQ[nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[UnsameQ, _, _],
					flattenUnsameQ[#[[2]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractUnsameQ[BinaryNode[UnsameQ, children_, data_]] :=
	CallNode[ToNode[UnsameQ], abstract /@ Flatten[flattenUnsameQ[children]], data]



(*
abstract syntax of  a @* b @* c  is a single Composition expression
*)
flattenComposition[nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[Composition, _, _],
					flattenComposition[#[[2]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractComposition[BinaryNode[Composition, children_, data_]] :=
	CallNode[ToNode[Composition], abstract /@ Flatten[flattenComposition[children]], data]




(*
abstract syntax of  a /* b /* c  is a single RightComposition expression
*)
flattenRightComposition[nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[RightComposition, _, _],
					flattenRightComposition[#[[2]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractRightComposition[BinaryNode[RightComposition, children_, data_]] :=
	CallNode[ToNode[RightComposition], abstract /@ Flatten[flattenRightComposition[children]], data]





abstractCompoundExpressionChild[InternalNullNode[Null, {}, data_]] :=
	SymbolNode["Null", {}, data]

abstractCompoundExpressionChild[c_] :=
	abstract[c]

abstractCompoundExpression[InfixNode[CompoundExpression, children_, data_]] :=
	CallNode[ToNode[CompoundExpression], abstractCompoundExpressionChild /@ children, data]






(*
handle "featuroid" bug 365013 where a<>StringJoin@b parses as a<>b
*)

abstractStringJoinChild[BinaryNode[BinaryAt, {SymbolNode["StringJoin", {}, _], c_}, _]] :=
	abstract[c]

abstractStringJoinChild[c_] :=
	abstract[c]

abstractStringJoin[InfixNode[StringJoin, children_, data_]] :=
	CallNode[ToNode[StringJoin], abstractStringJoinChild /@ children, data]












(*
abstract syntax of  a && b \[And] c  is a single And expression
*)
flattenAnd[nodes_List] :=
	Module[{},
		(
			Switch[#,
				InfixNode[And, _, _],
					flattenAnd[#[[2]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractAnd[InfixNode[And, children_, data_]] :=
	CallNode[ToNode[And], abstract /@ Flatten[flattenAnd[children]], data]



(*
abstract syntax of  a || b \[Or] c  is a single Or expression
*)
flattenOr[nodes_List] :=
	Module[{},
		(
			Switch[#,
				InfixNode[Or, _, _],
					flattenOr[#[[2]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractOr[InfixNode[Or, children_, data_]] :=
	CallNode[ToNode[Or], abstract /@ Flatten[flattenOr[children]], data]



(*
make sure to reverse children of Divisible
*)
abstractDivisible[InfixNode[Divisible, children_, data_]] :=
	CallNode[ToNode[Divisible], abstract /@ Reverse[children], data]






(*
Collect all of the ' in f'''[x]
*)

derivativeOrderAndBody[PostfixNode[Derivative, {rand_}, data_]] :=
Module[{order, body},
	{order, body} = derivativeOrderAndBody[rand];
	{order+1, body}
]

derivativeOrderAndBody[node_] :=
	{0, abstract[node]}

abstractDerivative[deriv:PostfixNode[Derivative, {rand_}, data_]] :=
Module[{order, body},
	{order, body} = derivativeOrderAndBody[deriv];
	CallNode[CallNode[ToNode[Derivative], {ToNode[order]}, <||>], {body}, <||>]
]









(*

Removes all commas

Fills in Nulls and gives SyntaxIssues for e.g. {1,,2}
*)
abstractGroupNode[GroupNode[tag_, children_, dataIn_]] :=
Module[{lastWasComma, abstractedChildren, issues, data},
	data = dataIn;
	issues = {};
	lastWasComma = !empty[children] && MatchQ[children[[1]], TokenNode[Token`Operator`Comma, _, _]];
	abstractedChildren = (Switch[#,
		TokenNode[Token`Operator`Comma, _, _],
			If[lastWasComma,
    			AppendTo[issues, SyntaxIssue["SyntaxError", "Comma encountered with no adjacent expression. The expression will be treated as Null", "Error", #[[3]]]];
    			SymbolNode["Null", {}, <||>]
    			,
    			lastWasComma = True;
    			Nothing
    		]
    	,
    	_,
    		lastWasComma = False;
    		abstract[#]
   ])& /@ children;
   If[lastWasComma,
   	AppendTo[issues, SyntaxIssue["SyntaxError", "Comma encountered with no adjacent expression. The expression will be treated as Null", "Error", children[[-1]][[3]]]];
   	AppendTo[abstractedChildren, SymbolNode["Null", {}, <||>]];
   ];

   If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];
   
   CallNode[ToNode[tag], abstractedChildren, data]
]



(*

Concrete parse of a[[2]] returns CallNode[a, GroupNode[Square, {GroupNode[Square, {2}]}]]
abstract parse of a[[2]] returns CallNode[Part, {a, 2}]

So convert from concrete [[ syntax to abstract Part syntax

*)
abstractCallNode[CallNode[headIn_, {outer:GroupNode[GroupSquare, {inner:GroupNode[GroupSquare, _, _]}, _]}, dataIn_]] :=
Module[{head, data, part, innerData, outerData, issues, partData},
	head = headIn;
	data = dataIn;
	part = inner;
	innerData = inner[[3]];
	outerData = outer[[3]];
	issues = {};

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[outerData[Source][[1,2]]+1 != innerData[Source][[1,2]],
		AppendTo[issues, SyntaxIssue["NotContiguous", "Part brackets [[ are not contiguous", "Warning", <|Source->{outerData[Source][[1]], innerData[Source][[1]]}|>]];
	];

	If[innerData[Source][[2,2]]+1 != outerData[Source][[2,2]],
		AppendTo[issues, SyntaxIssue["NotContiguous", "Part brackets ]] are not contiguous", "Warning", <|Source->{innerData[Source][[2]], outerData[Source][[2]]}|>]];
	];

	If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	CallNode[ToNode[Part], {head}~Join~part[[2]], data]
]

(*

Concrete parse of a[2] returns CallNode[a, GroupNode[Square, {2}]]
abstract parse of a[2] returns CallNode[a, {2}]

So convert from concrete [[ syntax to abstract Part syntax

*)
abstractCallNode[(c:CallNode|CallMissingCloserNode)[headIn_, {partIn:GroupNode[GroupSquare|GroupMissingCloserSquare, _, _]}, dataIn_]] :=
Module[{head, part, partData, issues, data},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	c[head, part[[2]], data]
]

(*

Concrete parse of a\[LeftDoubleBracket]2\[RightDoubleBracket] returns CallNode[a, GroupNode[DoubleBracket, {2}]]
abstract parse of a\[LeftDoubleBracket]2\[RightDoubleBracket] returns CallNode[Part, {a, 2}]

*)
abstractCallNode[CallNode[headIn_, {partIn:GroupNode[GroupDoubleBracket, _, _]}, dataIn_]] :=
Module[{head, part, partData, data, issues},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	CallNode[ToNode[Part], {head}~Join~(part[[2]]), data]
]


abstractCallNode[CallNode[head_, children_, data_]] :=
	CallNode[abstract[head], abstract /@ children, data]


End[]

EndPackage[]
