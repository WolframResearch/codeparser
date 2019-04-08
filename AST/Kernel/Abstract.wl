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



abstract[SymbolNode[s_, {}, data_]] := SymbolNode[s, {}, KeyTake[data, keysToTake]]
abstract[StringNode[s_, {}, data_]] := StringNode[s, {}, KeyTake[data, keysToTake]]
abstract[IntegerNode[s_, {}, data_]] := IntegerNode[s, {}, KeyTake[data, keysToTake]]
abstract[RealNode[s_, {}, data_]] := RealNode[s, {}, KeyTake[data, keysToTake]]



abstract[SlotNode["#", {}, data_]] :=
	CallNode[ToNode[Slot], {ToNode[1]}, KeyTake[data, keysToTake]]
abstract[SlotNode[str_ /; StringMatchQ[str, "#"~~DigitCharacter..], {}, data_]] :=
	CallNode[ToNode[Slot], {ToNode[FromDigits[StringDrop[str, 1]]]}, KeyTake[data, keysToTake]]
abstract[SlotNode[str_, {}, data_]] :=
	CallNode[ToNode[Slot], {ToNode[abstractString[StringDrop[str, 1]]]}, KeyTake[data, keysToTake]]


abstract[SlotSequenceNode["##", {}, data_]] :=
	CallNode[ToNode[SlotSequence], {ToNode[1]}, KeyTake[data, keysToTake]]
abstract[SlotSequenceNode[str_ /; StringMatchQ[str, "##"~~DigitCharacter..], {}, data_]] :=
	CallNode[ToNode[SlotSequence], {ToNode[FromDigits[StringDrop[str, 2]]]}, KeyTake[data, keysToTake]]


abstract[OutNode["%", {}, data_]] := CallNode[ToNode[Out], {}, KeyTake[data, keysToTake]]
abstract[OutNode[str_, {}, data_]] := CallNode[ToNode[Out], { ToNode[-StringLength[str]] }, KeyTake[data, keysToTake]]



abstract[PrefixNode[Minus, {op_}, data_]] := abstract[negate[op, KeyTake[data, keysToTake]]]
abstract[PrefixNode[PrefixLinearSyntaxBang, children:{Except[GroupNode[GroupLinearSyntaxParen, _, _]]}, data_]] := AbstractSyntaxErrorNode[Token`LinearSyntax`Bang, children, KeyTake[data, keysToTake]]
(*
FIXME: keep linear syntax for now
*)
abstract[PrefixNode[PrefixLinearSyntaxBang, children_, data_]] := PrefixNode[PrefixLinearSyntaxBang, children, KeyTake[data, keysToTake]]

(*
strings may be quoted

concrete syntax: <<a
abstract syntax Get["a"]

concrete syntax: <<"a"
abstract syntax Get["a"]
*)
abstract[PrefixNode[Get, {StringNode[str_, _, _]}, data_]] := CallNode[ToNode[Get], {ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]

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

abstract[BinaryNode[Put, {left_, StringNode[str_, _, _]}, data_]] := CallNode[ToNode[Put], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[PutAppend, {left_, StringNode[str_, _, _]}, data_]] := CallNode[ToNode[PutAppend], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]



(* NonAssociative errors *)
abstract[BinaryNode[PatternTest, children:{BinaryNode[PatternTest, _, _], _}, data_]] := AbstractSyntaxErrorNode[Token`Question, children, KeyTake[data, keysToTake]]
abstract[BinaryNode[DirectedEdge, children:{BinaryNode[DirectedEdge, _, _], _}, data_]] := AbstractSyntaxErrorNode[Token`LongName`DirectedEdge, children, KeyTake[data, keysToTake]]
abstract[BinaryNode[UndirectedEdge, children:{BinaryNode[UndirectedEdge, _, _], _}, data_]] := AbstractSyntaxErrorNode[Token`LongName`UndirectedEdge, children, KeyTake[data, keysToTake]]

abstract[BinaryNode[Span, {InternalOneNode[1, {}, _], InternalAllNode[All, {}, _]}, data_]] := CallNode[ToNode[Span], {ToNode[1], ToNode[All]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[Span, {left_, InternalAllNode[All, {}, _]}, data_]] := CallNode[ToNode[Span], {abstract[left], ToNode[All]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[Span, {InternalOneNode[1, {}, _], right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], abstract[right]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[Unset, {left_, InternalDotNode[Dot, {}, _]}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[System`VectorLess, children_, data_]] := abstractVectorInequality[BinaryNode[System`VectorLess, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[System`VectorGreater, children_, data_]] := abstractVectorInequality[BinaryNode[System`VectorGreater, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[System`VectorLessEqual, children_, data_]] := abstractVectorInequality[BinaryNode[System`VectorLessEqual, children, KeyTake[data, keysToTake]]]
abstract[BinaryNode[System`VectorGreaterEqual, children_, data_]] := abstractVectorInequality[BinaryNode[System`VectorGreaterEqual, children, KeyTake[data, keysToTake]]]

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

abstract[InfixNode[MessageName, children_, data_]] := abstractMessageName[InfixNode[MessageName, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[op_, nodes_, data_]] := CallNode[ToNode[op], abstract /@ nodes, KeyTake[data, keysToTake]]





(*
all TernaryNodes must be handled separately
*)

abstract[TernaryNode[TernaryTilde, {left_, middle_, right_}, data_]] := CallNode[abstract[middle], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[TagSet, {left_, middle_, right_}, data_]] := CallNode[ToNode[TagSet], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[TagSetDelayed, {left_, middle_, right_}, data_]] := CallNode[ToNode[TagSetDelayed], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[TagUnset, {left_, middle_, InternalDotNode[Dot, {}, _]}, data_]] := CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[Span, {InternalOneNode[1, {}, _], InternalAllNode[All, {}, _], right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], ToNode[All], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {InternalOneNode[1, {}, _], middle_, right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {left_, InternalAllNode[All, {}, _], right_}, data_]] := CallNode[ToNode[Span], {abstract[left], ToNode[All], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {left_, middle_, right_}, data_]] := CallNode[ToNode[Span], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]







(* handle CallNode before possible GroupNode errors *)
abstract[CallNode[op_, children_, data_]] := abstractCallNode[CallNode[op, children, KeyTake[data, keysToTake]]]


(*
take care of specific GroupNodes before calling abstractGroupNode
*)

(*
GroupParen
*)
abstract[GroupNode[GroupParen, {child_}, data_]] := abstract[child]

(* GroupNode errors *)
abstract[GroupNode[GroupSquare, children_, data_]] := AbstractSyntaxErrorNode[Token`OpenSquare, children, KeyTake[data, keysToTake]]
abstract[GroupNode[GroupParen, children_, data_]] := AbstractSyntaxErrorNode[Token`OpenParen, children, KeyTake[data, keysToTake]]

(*
FIXME: skip abstracting linear syntax for now
GroupLinearSyntaxParen retains its commas, so handle before abstractGroupNode
*)
abstract[GroupNode[GroupLinearSyntaxParen, children_, data_]] :=
	GroupNode[GroupLinearSyntaxParen, children, KeyTake[data, keysToTake]]



(*
All of the missing closers
*)

abstract[GroupNode[GroupMissingCloserList, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserList, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserAssociation, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserAssociation, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserAngleBracket, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserAngleBracket, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserCeiling, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserCeiling, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserFloor, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserFloor, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserDoubleBracket, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserDoubleBracket, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserBracketingBar, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserBracketingBar, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserDoubleBracketingBar, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserDoubleBracketingBar, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserParen, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserParen, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserSquare, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserSquare, children, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupMissingCloserLinearSyntaxParen, children_, data_]] :=
	AbstractSyntaxErrorNode[GroupMissingCloserLinearSyntaxParen, children, KeyTake[data, keysToTake]]





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



abstract[FileNode[File, children_, dataIn_]] :=
Module[{abstracted, issues, data},

	data = dataIn;

	{abstracted, issues} = abstractTopLevel[abstract /@ children];

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	FileNode[File, abstracted, KeyTake[data, keysToTake]]
]

abstract[HoldNode[Hold, children_, dataIn_]] :=
Module[{abstracted, issues, data},

	data = dataIn;

	{abstracted, issues} = abstractTopLevel[abstract /@ children];

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	HoldNode[Hold, abstracted, KeyTake[data, keysToTake]]
]



matchingOperatorPatterns[CallNode[SymbolNode["EndPackage", {}, _], {}, _]] = _PackageNode
matchingOperatorPatterns[CallNode[SymbolNode["End", {}, _], {}, _]] = _ContextNode
matchingOperatorPatterns[CallNode[SymbolNode["EndStaticAnalysisIgnore", {}, _], {}, _]] = _StaticAnalysisIgnoreNode

(*

input: a list of top-level nodes

returns: {abstracted top-level nodes, any AbstractSyntaxErrors that occurred}

*)
abstractTopLevel[listIn_] :=
Catch[
Module[{list, nodeListStack , currentList, operatorStack, currentOperator, x, issues},
	
	list = listIn;
	nodeListStack = {{}};
	operatorStack = {None};

	issues = {};

	Do[
		x = list[[i]];
		Switch[x,
		(*
		BeginPackage["Foo`"]
		*)
		CallNode[SymbolNode["BeginPackage", {}, _], {_StringNode, _:{}}, _],
			AppendTo[operatorStack, PackageNode[x[[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
			(*
			If[i != 1,
				AppendTo[issues, SyntaxIssue["Package", "BeginPackage is not the first expression.", "Warning", x[[3]]]];
			]*)
		,
		(*
		BeginPackage["Foo`"] ;
		*)
		CallNode[SymbolNode["CompoundExpression", {}, _], {CallNode[SymbolNode["BeginPackage", {}, _], {_StringNode, _:{}}, _], SymbolNode["Null", {}, _]}, _],
   		AppendTo[operatorStack, PackageNode[x[[2]][[1]][[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
			(*
			If[i != 1,
				AppendTo[issues, SyntaxIssue["Package", "BeginPackage is not the first expression.", "Warning", x[[2]][[1]][[3]]]];
			]*)
   	,
   	(*
		Begin["`Private`"]
		*)
		CallNode[SymbolNode["Begin", {}, _], {_StringNode}, _],
			AppendTo[operatorStack, ContextNode[x[[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		Begin["`Private`"] ;
		*)
		CallNode[SymbolNode["CompoundExpression", {}, _], {CallNode[SymbolNode["Begin", {}, _], {_StringNode}, _], SymbolNode["Null", {}, _]}, _],
   		AppendTo[operatorStack, ContextNode[x[[2]][[1]][[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		BeginStaticAnalysisIgnore[]
		*)
		CallNode[SymbolNode["BeginStaticAnalysisIgnore", {}, _], {}, _],
			AppendTo[operatorStack, StaticAnalysisIgnoreNode[x[[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		BeginStaticAnalysisIgnore[] ;
		*)
		CallNode[SymbolNode["CompoundExpression", {}, _], {CallNode[SymbolNode["BeginStaticAnalysisIgnore", {}, _], {}, _], SymbolNode["Null", {}, _]}, _],
   		AppendTo[operatorStack, StaticAnalysisIgnoreNode[x[[2]][[1]][[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		EndPackage[]
		End[]
		EndStaticAnalysisIgnore[]
		*)
		CallNode[SymbolNode["EndPackage" | "End" | "EndStaticAnalysisIgnore", {}, _], {}, _],
			currentOperator = operatorStack[[-1]];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", x[[3]]]];
				Throw[{list, issues}];
			];
			operatorStack = Drop[operatorStack, -1];
			currentList = nodeListStack[[-1]];
			nodeListStack = Drop[nodeListStack, -1];
			currentOperator[[2]] = currentList;
			AppendTo[nodeListStack[[-1]], currentOperator];
			(*
			Switch[x,
				CallNode[SymbolNode["EndPackage", {}, _], {}, _],
					If[i != Length[list],
						AppendTo[issues, SyntaxIssue["Package", "EndPackage is not the last expression.", "Warning", x[[3]]]];
					]
			]*)
		,
		(*
		EndPackage[] ;
		End[] ;
		EndStaticAnalysisIgnore[] ;
		*)
		CallNode[SymbolNode["CompoundExpression", {}, _], {CallNode[SymbolNode["EndPackage" | "End" | "EndStaticAnalysisIgnore", {}, _], {}, _], SymbolNode["Null", {}, _]}, _],
   		currentOperator = operatorStack[[-1]];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x[[2]][[1]]]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", x[[2]][[1]][[3]]]];
				Throw[{list, issues}];
			];
			operatorStack = Drop[operatorStack, -1];
			currentList = nodeListStack[[-1]];
			nodeListStack = Drop[nodeListStack, -1];
			currentOperator[[2]] = currentList;
			AppendTo[nodeListStack[[-1]], currentOperator];
			(*
			Switch[x,
				CallNode[SymbolNode["CompoundExpression", {}, _], {CallNode[SymbolNode["EndPackage", {}, _], {}, _], SymbolNode["Null", {}, _]}, _],
					If[i != Length[list],
						AppendTo[issues, SyntaxIssue["Package", "EndPackage is not the last expression.", "Warning", x[[2]][[1]][[3]]]];
					];
			]*)
   	,
   	(*
		All other calls to recognized directives
   	*)
		CallNode[SymbolNode["BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" |
										"EndPackage" | "End" | "EndStaticAnalysisIgnore", {}, _], _, _],
			AppendTo[issues, SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", x[[3]]]];
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;
   	*)
		CallNode[SymbolNode["CompoundExpression", {}, _], {CallNode[SymbolNode["BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" |
																											"EndPackage" | "End" | "EndStaticAnalysisIgnore", {}, _], {}, _], SymbolNode["Null", {}, _]}, _],
			AppendTo[issues, SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", x[[2]][[1]][[3]]]];
			Throw[{list, issues}];
		,
		(*
		All other expressions
		*)
		_,
			AppendTo[nodeListStack[[-1]], x]
		]
   ,
   {i, 1, Length[list]}
	];
	If[operatorStack =!= {None},
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", list[[1]][[3]]]];
		Throw[{list, issues}];
	];
	If[Length[nodeListStack] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", list[[1]][[3]]]];
		Throw[{list, issues}];
	];
	{nodeListStack[[1]], issues}
]]






(*
ConcreteParseString[""] returns Null
*)
abstract[Null] := Null

(*
a top-level comment gets abstracted to Null
*)
abstract[CommentNode[_, _, _]] := Null





(*
Just pass SyntaxErrorNode pass
*)
abstract[SyntaxErrorNode[op_, children_, data_]] := SyntaxErrorNode[op, children, KeyTake[data, keysToTake]]



abstract[f_Failure] := f




(*

String "a" -> a
String a -> a

ToNode is normally called after abstractString

for handling the various stringification operators
#a
#"a"
a::b
a::"b"
a>>b
a>>"b"
*)
abstractString[str_String /; StringStartsQ[str, "\""]] := ToExpression[str]
abstractString[str_String] := str






(*
concrete syntax does not have negated numbers
abstract syntax is allowed to have negated numbers
*)
negate[IntegerNode[str_, {}, data1_], data_] :=
	IntegerNode["-"<>str, {}, data]

negate[RealNode[str_, {}, data1_], data_] :=
	RealNode["-"<>str, {}, data]

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
				PrefixNode[Minus, {IntegerNode[_, _, _]}, _],
					{negate[#[[2,1]], data]}
				,
				PrefixNode[Minus, {RealNode[_, _, _]}, _],
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
abstract syntax of  a \[VectorLess] b \[VectorLessEqual] c \[VectorGreater] d \[VectorGreaterEqual] e  is a single Developer`VectorInequality expression
*)
flattenVectorInequality[nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[System`VectorLess, _, _],
					flattenVectorInequality[{#[[2]][[1]], ToNode[System`VectorLess], #[[2]][[2]]}]
				,
				BinaryNode[System`VectorGreater, _, _],
					flattenVectorInequality[{#[[2]][[1]], ToNode[System`VectorGreater], #[[2]][[2]]}]
				,
				BinaryNode[System`VectorLessEqual, _, _],
					flattenVectorInequality[{#[[2]][[1]], ToNode[System`VectorLessEqual], #[[2]][[2]]}]
				,
				BinaryNode[System`VectorGreaterEqual, _, _],
					flattenVectorInequality[{#[[2]][[1]], ToNode[System`VectorGreaterEqual], #[[2]][[2]]}]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractVectorInequality[BinaryNode[System`VectorLess, {left_, right_}, data_]] :=
	simplifyVectorInequality[InfixNode[Developer`VectorInequality, Flatten[flattenVectorInequality[{left, ToNode[System`VectorLess], right}]], data]]

abstractVectorInequality[BinaryNode[System`VectorGreater, {left_, right_}, data_]] :=
	simplifyVectorInequality[InfixNode[Developer`VectorInequality, Flatten[flattenVectorInequality[{left, ToNode[System`VectorGreater], right}]], data]]

abstractVectorInequality[BinaryNode[System`VectorLessEqual, {left_, right_}, data_]] :=
	simplifyVectorInequality[InfixNode[Developer`VectorInequality, Flatten[flattenVectorInequality[{left, ToNode[System`VectorLessEqual], right}]], data]]

abstractVectorInequality[BinaryNode[System`VectorGreaterEqual, {left_, right_}, data_]] :=
	simplifyVectorInequality[InfixNode[Developer`VectorInequality, Flatten[flattenVectorInequality[{left, ToNode[System`VectorGreaterEqual], right}]], data]]

(*
attempt to simplify e.g. Developer`VectorInequality[a, VectorLess, b, VectorLess, c] to VectorLess[{a, b, c}]

Yes, make sure that it is VectorLess[{a, b, c}] and not VectorLess[a, b, c]

*)
simplifyVectorInequality[InfixNode[Developer`VectorInequality, children_, data_]] :=
Module[{rators, rands},
	rators = children[[2;;-2;;2]];
	rands = children[[1;;-1;;2]];

	Switch[rators,
		{ToNode[System`VectorLess]..},
			CallNode[ToNode[System`VectorLess], { CallNode[ToNode[List], abstract /@ rands, <||>] }, data]
		,
		{ToNode[System`VectorGreater]..},
			CallNode[ToNode[System`VectorGreater], { CallNode[ToNode[List], abstract /@ rands, <||>] }, data]
		,
		{ToNode[System`VectorLessEqual]..},
			CallNode[ToNode[System`VectorLessEqual], { CallNode[ToNode[List], abstract /@ rands, <||>] }, data]
		,
		{ToNode[System`VectorGreaterEqual]..},
			CallNode[ToNode[System`VectorGreaterEqual], { CallNode[ToNode[List], abstract /@ rands, <||>] }, data]
		,
		_,
			CallNode[ToNode[Developer`VectorInequality], abstract /@ children, data]
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







(*
properly abstract and warn about a;b;[]
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupSquare, _, _] }, dataIn_]] :=
Module[{head, data, groupData, issues},

	data = dataIn;

	groupData = groupIn[[3]];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->groupData[Source]|>]];

	AssociateTo[data, AbstractSyntaxIssues -> issues];

    head = abstractCompoundExpression[InfixNode[CompoundExpression, {headIn, InternalNullNode[Null, {}, <||>]}, <||>]];

	abstract[CallNode[head, { groupIn }, data]]
]

(*
properly abstract and warn about a;b;[];c
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupSquare, _, _], rest__ }, dataIn_]] :=
Module[{head, data, groupData, issues},

	data = dataIn;

	groupData = groupIn[[3]];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->groupData[Source]|>]];

	AssociateTo[data, AbstractSyntaxIssues -> issues];

    head = abstractCompoundExpression[InfixNode[CompoundExpression, {headIn, InternalNullNode[Null, {}, <||>]}, <||>]];

	abstract[InfixNode[CompoundExpression, { CallNode[head, { groupIn }, data], rest }, <||>]]
]


(*
properly abstract and warn about a;b;\[LeftDoubleBracket]\[RightDoubleBracket]
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupDoubleBracket, _, _] }, dataIn_]] :=
Module[{head, data, groupData, issues},

    data = dataIn;

    groupData = groupIn[[3]];

    issues = Lookup[data, AbstractSyntaxIssues, {}];

    AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->groupData[Source]|>]];

    AssociateTo[data, AbstractSyntaxIssues -> issues];

    head = abstractCompoundExpression[InfixNode[CompoundExpression, {headIn, InternalNullNode[Null, {}, <||>]}, <||>]];

    abstract[CallNode[head, { groupIn }, data]]
]

(*
properly abstract and warn about a;b;\[LeftDoubleBracket]\[RightDoubleBracket];c
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupDoubleBracket, _, _], rest__ }, dataIn_]] :=
Module[{head, data, groupData, issues},

    data = dataIn;

    groupData = groupIn[[3]];

    issues = Lookup[data, AbstractSyntaxIssues, {}];

    AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->groupData[Source]|>]];

    AssociateTo[data, AbstractSyntaxIssues -> issues];

    head = abstractCompoundExpression[InfixNode[CompoundExpression, {headIn, InternalNullNode[Null, {}, <||>]}, <||>]];

    abstract[InfixNode[CompoundExpression, { CallNode[head, { groupIn }, data], rest }, <||>]]
]


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
strings may be quoted

concrete syntax: a::b
abstract syntax MessageName[a, "b"]

concrete syntax: a::"b"
abstract syntax MessageName[a, "b"]
*)
abstractMessageName[InfixNode[MessageName, {left_, rest:_StringNode..}, dataIn_]] :=
Module[{data, issues},
	
	data = dataIn;

	issues = {};

	If[Length[{rest}] > 2,
		AppendTo[issues, SyntaxIssue["SyntaxUndocumentedMessageName", "This syntax is not documented.", "Error", <|Source->data[Source]|>]];
	];
	
	If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	CallNode[ToNode[MessageName], {abstract[left]} ~Join~ (ToNode[abstractString[#[[1]]]]& /@ {rest}), data]
]








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
	lastWasComma = !empty[children] && MatchQ[children[[1]], TokenNode[Token`Comma, _, _]];
	abstractedChildren = (Switch[#,
		TokenNode[Token`Comma, _, _],
			If[lastWasComma,
    			AppendTo[issues, SyntaxIssue["SyntaxError", "Comma encountered with no adjacent expression. The expression will be treated as Null.", "Error", #[[3]]]];
    			SymbolNode["Null", {}, #[[3]]]
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
   	AppendTo[issues, SyntaxIssue["SyntaxError", "Comma encountered with no adjacent expression. The expression will be treated as Null.", "Error", children[[-1]][[3]]]];
   	AppendTo[abstractedChildren, SymbolNode["Null", {}, children[[-1]][[3]]]];
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

    Switch[head,
    		(*
			feel strongly about ##2[arg]
			##2 represents a sequence of arguments
    		*)
    		_SlotSequenceNode,
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        _SymbolNode (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode | _SlotNode (*| _SlotSequenceNode *),
        (* these are fine *)
        Null
        ,
        (*
        BinaryNode[PatternTest, _, _],
        (* these are fine *)
        Null
        ,*)
        GroupNode[GroupParen | List | Association, _, _],
        (* these are fine *)
        Null
        ,
        (*
        PostfixNode[Function | Derivative, _, _],
        (* these are fine *)
        Null
        ,*)
        _,
        (*
        warn about anything else
        *)
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->data[Source]|>]];
    ];

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[outerData[Source][[1,2]]+1 != innerData[Source][[1,2]],
		AppendTo[issues, SyntaxIssue["NotContiguous", "Part brackets [[ are not contiguous.", "Warning", <|Source->{outerData[Source][[1]], innerData[Source][[1]]}|>]];
	];

	If[innerData[Source][[2,2]]+1 != outerData[Source][[2,2]],
		AppendTo[issues, SyntaxIssue["NotContiguous", "Part brackets ]] are not contiguous.", "Warning", <|Source->{innerData[Source][[2]], outerData[Source][[2]]}|>]];
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

So convert from concrete [ syntax to abstract Call syntax

*)
abstractCallNode[CallNode[headIn_, {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, partData, issues, data},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

    Switch[head,
    		(*
			feel strongly about ##2[arg]
			##2 represents a sequence of arguments
    		*)
    		_SlotSequenceNode,
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        _SymbolNode | _StringNode | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*)|
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode | _SlotNode (*| _SlotSequenceNode*),
        (* these are fine *)
        Null
        ,
        BinaryNode[PatternTest, _, _],
        (* these are fine *)
        Null
        ,
        GroupNode[GroupParen | List | Association, _, _],
        (*
        these are fine
        List is allowed because this is popular to do:
        Through[{a, b, c}[1]]
        *)
        Null
        ,
        PostfixNode[Function | Derivative, _, _],
        (* these are fine *)
        Null
        ,
        _,
        (*
        warn about anything else
        *)
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->data[Source]|>]];
    ];

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	CallNode[head, part[[2]], data]
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

    Switch[head,
    		(*
			feel strongly about ##2[arg]
			##2 represents a sequence of arguments
    		*)
    		_SlotSequenceNode,
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        _SymbolNode (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode | _SlotNode (*| _SlotSequenceNode *),
        (* these are fine *)
        Null
        ,
        (*
        BinaryNode[PatternTest, _, _],
        (* these are fine *)
        Null
        ,*)
        GroupNode[GroupParen | List | Association, _, _],
        (* these are fine *)
        Null
        ,
        (*
        PostfixNode[Function | Derivative, _, _],
        (* these are fine *)
        Null
        ,*)
        _,
        (*
        warn about anything else
        *)
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Error", <|Source->data[Source]|>]];
    ];

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
