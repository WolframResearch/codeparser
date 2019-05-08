BeginPackage["AST`Abstract`"]

Abstract::usage = "Abstract[cst] returns an abstract syntax tree from a concrete syntax tree."

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



abstract[SymbolNode[Symbol, s_, data_]] := SymbolNode[Symbol, s, KeyTake[data, keysToTake]]
abstract[StringNode[String, s_, data_]] := StringNode[String, s, KeyTake[data, keysToTake]]
abstract[IntegerNode[Integer, s_, data_]] := IntegerNode[Integer, s, KeyTake[data, keysToTake]]
abstract[RealNode[Real, s_, data_]] := RealNode[Real, s, KeyTake[data, keysToTake]]



abstract[SlotNode[Slot, "#", data_]] :=
	CallNode[ToNode[Slot], {ToNode[1]}, KeyTake[data, keysToTake]]
abstract[SlotNode[Slot, str_ /; StringMatchQ[str, "#"~~DigitCharacter..], data_]] :=
	CallNode[ToNode[Slot], {ToNode[FromDigits[StringDrop[str, 1]]]}, KeyTake[data, keysToTake]]
abstract[SlotNode[Slot, str_, data_]] :=
	CallNode[ToNode[Slot], {ToNode[abstractString[StringDrop[str, 1]]]}, KeyTake[data, keysToTake]]


abstract[SlotSequenceNode[SlotSequence, "##", data_]] :=
	CallNode[ToNode[SlotSequence], {ToNode[1]}, KeyTake[data, keysToTake]]
abstract[SlotSequenceNode[SlotSequence, str_ /; StringMatchQ[str, "##"~~DigitCharacter..], data_]] :=
	CallNode[ToNode[SlotSequence], {ToNode[FromDigits[StringDrop[str, 2]]]}, KeyTake[data, keysToTake]]


abstract[OutNode[Out, "%", data_]] := CallNode[ToNode[Out], {}, KeyTake[data, keysToTake]]
abstract[OutNode[Out, str_, data_]] := CallNode[ToNode[Out], { ToNode[-StringLength[str]] }, KeyTake[data, keysToTake]]

abstract[OptionalDefaultNode[OptionalDefault, _, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]





abstract[PrefixNode[Minus, {_, rand_}, data_]] := abstract[negate[rand, KeyTake[data, keysToTake]]]
abstract[PrefixNode[PrefixLinearSyntaxBang, children:{_, Except[GroupNode[GroupLinearSyntaxParen, _, _]]}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`LinearSyntaxBang, children, KeyTake[data, keysToTake]]
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
abstract[PrefixNode[Get, {_, StringNode[String, str_, _]}, data_]] := CallNode[ToNode[Get], {ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]

abstract[PrefixNode[Information, {TokenNode[Token`Question, _, _], StringNode[String, str_, _]}, data_]] := CallNode[ToNode[Information], {ToNode[str], CallNode[ToNode[Rule], { ToNode[LongForm], ToNode[False] }, <||>]}, KeyTake[data, keysToTake]]
abstract[PrefixNode[Information, {TokenNode[Token`QuestionQuestion, _, _], StringNode[String, str_, _]}, data_]] := CallNode[ToNode[Information], {ToNode[str], CallNode[ToNode[Rule], { ToNode[LongForm], ToNode[True] }, <||>]}, KeyTake[data, keysToTake]]

abstract[PrefixNode[op_, {_, operand_}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]


abstract[PostfixNode[Derivative, {operand_, _}, data_]] := abstractDerivative[PostfixNode[Derivative, {operand}, KeyTake[data, keysToTake]]]
abstract[PostfixNode[op_, {operand_, _}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]




abstract[BinaryNode[Minus, { left_, _, right_ }, data_]] := abstractPlus[BinaryNode[Minus, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Divide, { left_, _, right_ }, data_]] := abstractTimes[BinaryNode[Divide, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[Equal, { left_, _, right_ }, data_]] := abstractInequality[BinaryNode[Equal, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Unequal, { left_, _, right_ }, data_]] := abstractInequality[BinaryNode[Unequal, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Less, { left_, _, right_ }, data_]] := abstractInequality[BinaryNode[Less, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Greater, { left_, _, right_ }, data_]] := abstractInequality[BinaryNode[Greater, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[LessEqual, { left_, _, right_ }, data_]] := abstractInequality[BinaryNode[LessEqual, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[GreaterEqual, { left_, _, right_ }, data_]] := abstractInequality[BinaryNode[GreaterEqual, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[BinaryAt, {left_, _, right_}, data_]] := CallNode[abstract[left], {abstract[right]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinaryAtAtAt, {left_, _, right_}, data_]] := CallNode[ToNode[Apply], abstract /@ {left, right, GroupNode[List, { TokenNode[Token`OpenCurly, "{", <||>], ToNode[1], TokenNode[Token`CloseCurly, "}", <||>] }, <||>]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinarySlashSlash, {left_, _, right_}, data_]] := CallNode[abstract[right], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[SameQ, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[SameQ, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[UnsameQ, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[UnsameQ, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Composition, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[Composition, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[RightComposition, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[RightComposition, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[Element, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[Element, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Subset, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[Subset, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Superset, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[Superset, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[SubsetEqual, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[SubsetEqual, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[SupersetEqual, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[SupersetEqual, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[NotElement, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[NotElement, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[NotSubset, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[NotSubset, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[NotSuperset, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[NotSuperset, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[NotSubsetEqual, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[NotSubsetEqual, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[NotSupersetEqual, { left_, _, right_ }, data_]] := abstractBinaryToInfix[BinaryNode[NotSupersetEqual, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[Divide, { left_, _, right_ }, data_]] := abstractTimes[BinaryNode[Divide, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[Put, {left_, _, StringNode[String, str_, _]}, data_]] := CallNode[ToNode[Put], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[PutAppend, {left_, _, StringNode[String, str_, _]}, data_]] := CallNode[ToNode[PutAppend], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]



(* Abstract NonAssociative errors *)

(*
DirectedEdge and UndirectedEdge do not associate with each other
*)
abstract[BinaryNode[DirectedEdge, children:{BinaryNode[UndirectedEdge, _, _], _, _}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociative, children, KeyTake[data, keysToTake]]
abstract[BinaryNode[UndirectedEdge, children:{BinaryNode[DirectedEdge, _, _], _, _}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociative, children, KeyTake[data, keysToTake]]


abstract[BinaryNode[Span, {InternalOneNode[1, _, _], _, InternalAllNode[All, _, _]}, data_]] := CallNode[ToNode[Span], {ToNode[1], ToNode[All]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[Span, {left_, _, InternalAllNode[All, _, _]}, data_]] := CallNode[ToNode[Span], {abstract[left], ToNode[All]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[Span, {InternalOneNode[1, _, _], _, right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], abstract[right]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[Unset, {left_, _, _}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[System`VectorLess, {left_, _, right_}, data_]] := abstractVectorInequality[BinaryNode[System`VectorLess, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[System`VectorGreater, {left_, _, right_}, data_]] := abstractVectorInequality[BinaryNode[System`VectorGreater, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[System`VectorLessEqual, {left_, _, right_}, data_]] := abstractVectorInequality[BinaryNode[System`VectorLessEqual, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[System`VectorGreaterEqual, {left_, _, right_}, data_]] := abstractVectorInequality[BinaryNode[System`VectorGreaterEqual, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[op_, {left_, _, right_}, data_]] := CallNode[ToNode[op], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]




abstract[InfixNode[Plus, children_, data_]] := abstractPlus[InfixNode[Plus, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[InfixImplicitPlus, children_, data_]] := abstractPlus[InfixNode[InfixImplicitPlus, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[Times, children_, data_]] := abstractTimes[InfixNode[Times, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[ImplicitTimes, children_, data_]] := abstractTimes[InfixNode[ImplicitTimes, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[InfixInvisibleTimes, children_, data_]] := abstractTimes[InfixNode[InfixInvisibleTimes, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[InfixTimes, children_, data_]] := abstractTimes[InfixNode[InfixTimes, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[And, children_, data_]] := abstractAnd[InfixNode[And, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[Or, children_, data_]] := abstractOr[InfixNode[Or, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[Divisible, children_, data_]] := abstractDivisible[InfixNode[Divisible, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[CompoundExpression, children_, data_]] := abstractCompoundExpression[InfixNode[CompoundExpression, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[StringJoin, children_, data_]] := abstractStringJoin[InfixNode[StringJoin, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[MessageName, children_, data_]] := abstractMessageName[InfixNode[MessageName, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[op_, children_, data_]] := CallNode[ToNode[op], abstract /@ children[[;;;;2]], KeyTake[data, keysToTake]]





(*
all TernaryNodes must be handled separately
*)

abstract[TernaryNode[TernaryTilde, {left_, _, middle_, _, right_}, data_]] := CallNode[abstract[middle], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[TagSet, {left_, _, middle_, _, right_}, data_]] := CallNode[ToNode[TagSet], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[TagSetDelayed, {left_, _, middle_, _, right_}, data_]] := CallNode[ToNode[TagSetDelayed], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[TagUnset, {left_, _, middle_, _, _}, data_]] := CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[Span, {InternalOneNode[1, _, _], _, InternalAllNode[All, _, _], _, right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], ToNode[All], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {InternalOneNode[1, _, _], _, middle_, _, right_}, data_]] := CallNode[ToNode[Span], {ToNode[1], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {left_, _, InternalAllNode[All, _, _], _, right_}, data_]] := CallNode[ToNode[Span], {abstract[left], ToNode[All], abstract[right]}, KeyTake[data, keysToTake]]
abstract[TernaryNode[Span, {left_, _, middle_, _, right_}, data_]] := CallNode[ToNode[Span], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]







(*
handle CallNode before possible GroupNode errors

what are the different shapes that calls can have?

They are:

f [ ]

f [ [ ] ]

f \[LeftDoubleBracket] \[RightDoubleBracket]

strip off the concrete syntax while we are here
*)
abstract[CallNode[op_, { GroupNode[GroupSquare, { _, GroupNode[GroupSquare, {_, inner___, _}, data3_], _ }, data2_] }, data1_]] := abstractCallNode[CallNode[op, { GroupNode[GroupSquare, { GroupNode[GroupSquare, { inner }, KeyTake[data3, keysToTake]] }, KeyTake[data2, keysToTake]] }, KeyTake[data1, keysToTake]]]

abstract[CallNode[op_, { GroupNode[GroupSquare, { _, inner___, _ }, data2_] }, data1_]] := abstractCallNode[CallNode[op, { GroupNode[GroupSquare, { inner }, KeyTake[data2, keysToTake]] }, KeyTake[data1, keysToTake]]]

abstract[CallNode[op_, { GroupNode[GroupDoubleBracket, { _, inner___, _ }, data2_] }, data1_]] := abstractCallNode[CallNode[op, { GroupNode[GroupDoubleBracket, { inner }, KeyTake[data2, keysToTake]] }, KeyTake[data1, keysToTake]]]







(*
take care of specific GroupNodes before calling abstractGroupNode
*)

(*
GroupParen
*)
abstract[GroupNode[GroupParen, {_, child_, _}, data_]] := abstract[child]

(* GroupNode errors *)
abstract[GroupNode[GroupSquare, children_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, children, KeyTake[data, keysToTake]]
abstract[GroupNode[GroupParen, children_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, children, KeyTake[data, keysToTake]]

(*
FIXME: skip abstracting linear syntax for now
GroupLinearSyntaxParen retains its commas, so handle before abstractGroupNode
*)
abstract[GroupNode[GroupLinearSyntaxParen, children_, data_]] :=
	GroupNode[GroupLinearSyntaxParen, children, KeyTake[data, keysToTake]]



(*
Missing closers and openers
*)

abstract[GroupMissingCloserNode[_, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, children, KeyTake[data, keysToTake]]

abstract[GroupMissingOpenerNode[_, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingOpener, children, KeyTake[data, keysToTake]]


abstract[GroupNode[op_, children_, data_]] := abstractGroupNode[GroupNode[op, children[[2;;-2]], KeyTake[data, keysToTake]]]





abstract[PrefixBinaryNode[Integrate, {_, operand1_, PrefixNode[DifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[Integrate], {abstract[operand1], abstract[var]}, KeyTake[data, keysToTake]]

abstract[PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]] := CallNode[ToNode[op], {abstract[operand1], abstract[operand2]}, KeyTake[data, keysToTake]]






abstract[BlankNode[Blank, {_}, data_]] := CallNode[ToNode[Blank], {}, KeyTake[data, keysToTake]]
abstract[BlankNode[Blank, {_, sym2_}, data_]] := CallNode[ToNode[Blank], {sym2}, KeyTake[data, keysToTake]]
abstract[BlankSequenceNode[BlankSequence, {_}, data_]] := CallNode[ToNode[BlankSequence], {}, KeyTake[data, keysToTake]]
abstract[BlankSequenceNode[BlankSequence, {_, sym2_}, data_]] := CallNode[ToNode[BlankSequence], {sym2}, KeyTake[data, keysToTake]]
abstract[BlankNullSequenceNode[BlankNullSequence, {_}, data_]] := CallNode[ToNode[BlankNullSequence], {}, KeyTake[data, keysToTake]]
abstract[BlankNullSequenceNode[BlankNullSequence, {_, sym2_}, data_]] := CallNode[ToNode[BlankNullSequence], {sym2}, KeyTake[data, keysToTake]]

abstract[PatternBlankNode[PatternBlank, {sym1_, _}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNode[PatternBlank, {sym1_, _, sym2_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[Blank], {sym2}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, _}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankSequence], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, _, sym2_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankSequence], {sym2}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, _}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankNullSequence], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, _, sym2_}, data_]] := CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[BlankNullSequence], {sym2}, <||>]}, KeyTake[data, keysToTake]]
abstract[OptionalDefaultPatternNode[OptionalDefaultPattern, {sym1_, _}, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Pattern], {sym1, CallNode[ToNode[Blank], {}, <||>]}, <||>]}, KeyTake[data, keysToTake]]



abstract[FileNode[File, children_, dataIn_]] :=
Module[{abstracted, issues, issues1, issues2, data, abstractedChildren},

	data = dataIn;

	issues = {};

	{abstractedChildren, issues1} = abstractTopLevelChildren[children];

	{abstracted, issues2} = abstractTopLevel[abstractedChildren];

	issues = issues1 ~Join~ issues2;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	FileNode[File, abstracted, KeyTake[data, keysToTake]]
]

abstract[HoldNode[Hold, children_, dataIn_]] :=
Module[{abstracted, issues, issues1, issues2, data, abstractedChildren},

	data = dataIn;

	issues = {};

	{abstractedChildren, issues1} = abstractTopLevelChildren[children];

	{abstracted, issues2} = abstractTopLevel[abstractedChildren];

	issues = issues1 ~Join~ issues2;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	HoldNode[Hold, abstracted, KeyTake[data, keysToTake]]
]



matchingOperatorPatterns[CallNode[SymbolNode[Symbol, "EndPackage", _], {}, _]] = _PackageNode
matchingOperatorPatterns[CallNode[SymbolNode[Symbol, "End", _], {}, _]] = _ContextNode
matchingOperatorPatterns[CallNode[SymbolNode[Symbol, "EndStaticAnalysisIgnore", _], {}, _]] = _StaticAnalysisIgnoreNode



(*

Call abstract on children

But also warn if something strange is at top-level

*)
abstractTopLevelChildren[children_] :=
Module[{abstractedChildren, issues, single},

	issues = {};

	(*
	if there is only 1 expression in the file, then assume it could be a data file or an expression from ParseString[]

	TODO: handle as a Data file
	*)
	single = (Length[children] == 1);

	abstractedChildren = (
		If[!single, issues = issues ~Join~ topLevelChildIssues[#, True]];
		abstract[#]
		)& /@ children;

	{abstractedChildren, issues}
]


(*

topLevelChildIssues[node_, active_] will return a list of issues by treating node as a top-level child

Some nodes would be strange at the top-level in a package. For example, 1+1 would be strange at the top-level.

*)

(*
if not active, return no issues
*)
topLevelChildIssues[_, False] := {}

(*
Call could be anything
*)
topLevelChildIssues[CallNode[_,_,_], True] := {}

(*
probably a declaration
*)
topLevelChildIssues[SymbolNode[_,_,_], True] := {}

(*
Side-effecting or calling binary operators
*)
topLevelChildIssues[BinaryNode[AddTo | Apply | BinaryAt | BinaryAtAtAt |
												BinarySlashSlash | Map | Set | SetDelayed |
												SubtractFrom | Unset | UpSet | UpSetDelayed, _, _], True] := {}

(*
Side-effecting ternary operators
*)
topLevelChildIssues[TernaryNode[TagSet | TagSetDelayed | TernaryTilde, _, _], True] := {}

(*
Side-effecting prefix operators
*)
topLevelChildIssues[PrefixNode[Get | PreDecrement | PreIncrement, _, _], True] := {}

(*
Side-effecting postfix operators
*)
topLevelChildIssues[PostfixNode[Decrement | Increment, _, _], True] := {}

(*
e.g., list of declarations in StartUp code

sym | "str" | Symbol["str"]

kind of a hack

TODO: handle StartUp files as a format

*)
topLevelChildIssues[GroupNode[List, {
	TokenNode[Token`OpenCurly, _, _],
	PatternSequence[_SymbolNode | _StringNode | CallNode[SymbolNode[Symbol, "Symbol", _], _, _], TokenNode[Token`Comma, _, _]]...,
	_SymbolNode | _StringNode | CallNode[SymbolNode[Symbol, "Symbol", _], _, _],
	TokenNode[Token`CloseCurly, _, _]}, _], True] := {}

(*
just assume parens connote intention
*)
topLevelChildIssues[GroupNode[GroupParen, _, _], True] := {}


(*
Define CompoundExpression versions of everything above

But do not simply make a recursive call back to previous definitions

We want to be able to catch e.g.,

foo[] := Message[foo::bad]; $Failed

where the parsing is ( foo[] := Message[foo::bad]) ; $Failed

A recursive call would see the SetDelayed, and say fine

And then the recursive call would see the $Failed and say fine (because it's a symbol and might be a declaration)

So hard-code the CompoundExpression versions to be able to catch these cases

*)
topLevelChildIssues[InfixNode[CompoundExpression, {
												CallNode[_, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												SymbolNode[_, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												BinaryNode[AddTo | Apply | BinaryAt | BinaryAtAtAt |
													BinarySlashSlash | Map | Set | SetDelayed |
													SubtractFrom | Unset | UpSet | UpSetDelayed, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												TernaryNode[TagSet | TagSetDelayed | TernaryTilde, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PrefixNode[Get | PreDecrement | PreIncrement, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PostfixNode[Decrement | Increment, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[List, {
												TokenNode[Token`OpenCurly, _, _],
												PatternSequence[_SymbolNode | _StringNode | CallNode[SymbolNode[Symbol, "Symbol", _], _, _], TokenNode[Token`Comma, _, _]]...,
												_SymbolNode | _StringNode | CallNode[SymbolNode[Symbol, "Symbol", _], _, _],
												TokenNode[Token`CloseCurly, _, _]}, _], _TokenNode, _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[GroupParen, _, _], _TokenNode, _InternalNullNode }, _], True] := {}

(*
more specific stuff inside CompoundExpression
*)

(*
allow a=1;b=2;c=3;

FIXME: maybe this is too niche

*)
topLevelChildIssues[InfixNode[CompoundExpression, {
												PatternSequence[BinaryNode[Set | SetDelayed | Unset, _, _], _TokenNode].., _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PatternSequence[_SymbolNode, _TokenNode].., _InternalNullNode }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PatternSequence[_SymbolNode, _TokenNode].., _SymbolNode }, _], True] := {}


topLevelChildIssues[InfixNode[CompoundExpression, _, data_], True] := { SyntaxIssue["TopLevel", "Strange expression is at top-level.\n\
Consider breaking up expression on separate lines.", "Warning", data] }

(*
Anything else, then warn
*)
topLevelChildIssues[node_, True] := { SyntaxIssue["TopLevel", "Strange expression is at top-level.", "Warning", node[[3]]] }








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
		CallNode[SymbolNode[Symbol, "BeginPackage", _], {StringNode[String, _?contextQ, _], StringNode[String, _?contextQ, _] | CallNode[SymbolNode[Symbol, "List", <||>], { StringNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _],
			AppendTo[operatorStack, PackageNode[x[[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		BeginPackage["Foo`"] ;
		*)
		CallNode[SymbolNode[Symbol, "CompoundExpression", _], {CallNode[SymbolNode[Symbol, "BeginPackage", _], {StringNode[String, _?contextQ, _], StringNode[String, _?contextQ, _] | CallNode[SymbolNode[Symbol, "List", <||>], { StringNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _], SymbolNode[Symbol, "Null", _]}, _],
   		AppendTo[operatorStack, PackageNode[x[[2]][[1]][[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		Begin["`Private`"]
		*)
		CallNode[SymbolNode[Symbol, "Begin", _], {StringNode[String, _?contextQ, _]}, _],
			AppendTo[operatorStack, ContextNode[x[[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		Begin["`Private`"] ;
		*)
		CallNode[SymbolNode[Symbol, "CompoundExpression", _], {CallNode[SymbolNode[Symbol, "Begin", _], {StringNode[String, _?contextQ, _]}, _], SymbolNode[Symbol, "Null", _]}, _],
   		AppendTo[operatorStack, ContextNode[x[[2]][[1]][[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		BeginStaticAnalysisIgnore[]
		*)
		CallNode[SymbolNode[Symbol, "BeginStaticAnalysisIgnore", _], {}, _],
			AppendTo[operatorStack, StaticAnalysisIgnoreNode[x[[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		BeginStaticAnalysisIgnore[] ;
		*)
		CallNode[SymbolNode[Symbol, "CompoundExpression", _], {CallNode[SymbolNode[Symbol, "BeginStaticAnalysisIgnore", _], {}, _], SymbolNode[Symbol, "Null", _]}, _],
   		AppendTo[operatorStack, StaticAnalysisIgnoreNode[x[[2]][[1]][[2]], {}, <||>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		EndPackage[]
		End[]
		EndStaticAnalysisIgnore[]
		*)
		CallNode[SymbolNode[Symbol, "EndPackage" | "End" | "EndStaticAnalysisIgnore", _], {}, _],
			currentOperator = operatorStack[[-1]];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced package directives.", "Error", x[[3]]]];
				Throw[{list, issues}];
			];
			operatorStack = Drop[operatorStack, -1];
			currentList = nodeListStack[[-1]];
			nodeListStack = Drop[nodeListStack, -1];
			currentOperator[[2]] = currentList;
			AppendTo[nodeListStack[[-1]], currentOperator];
		,
		(*
		EndPackage[] ;
		End[] ;
		EndStaticAnalysisIgnore[] ;
		*)
		CallNode[SymbolNode[Symbol, "CompoundExpression", _], {CallNode[SymbolNode[Symbol, "EndPackage" | "End" | "EndStaticAnalysisIgnore", _], {}, _], SymbolNode[Symbol, "Null", _]}, _],
   		currentOperator = operatorStack[[-1]];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x[[2]][[1]]]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced package directives.", "Error", x[[2]][[1]][[3]]]];
				Throw[{list, issues}];
			];
			operatorStack = Drop[operatorStack, -1];
			currentList = nodeListStack[[-1]];
			nodeListStack = Drop[nodeListStack, -1];
			currentOperator[[2]] = currentList;
			AppendTo[nodeListStack[[-1]], currentOperator];
   	,
   	(*
		All other calls to recognized directives
   	*)
		CallNode[SymbolNode[Symbol, "BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" | "EndPackage" | "End" | "EndStaticAnalysisIgnore", _], _, _],
			AppendTo[issues, SyntaxIssue["Package", "Package directive does not have correct syntax.", "Error", x[[3]]]];
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;
   	*)
		CallNode[SymbolNode[Symbol, "CompoundExpression", _], {CallNode[SymbolNode[Symbol, "BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" | "EndPackage" | "End" | "EndStaticAnalysisIgnore", _], _, _], SymbolNode[Symbol, "Null", _]}, _],
			AppendTo[issues, SyntaxIssue["Package", "Package directive does not have correct syntax.", "Error", x[[2]][[1]][[3]]]];
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
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced package directives.", "Error", list[[1]][[3]]]];
		Throw[{list, issues}];
	];
	If[Length[nodeListStack] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced package directives.", "Error", list[[1]][[3]]]];
		Throw[{list, issues}];
	];
	{nodeListStack[[1]], issues}
]]



(*

Match a string that is a context

tutorial/InputSyntax#6562

Symbol Names and Contexts

*)
contextQ[s_String] := StringMatchQ[s, RegularExpression["\"`?([a-zA-Z][a-zA-Z0-9]*`)+\""]]






(*
ConcreteParseString[""] returns Null
*)
abstract[Null] := Null





(*
Just pass SyntaxErrorNode pass
*)
abstract[SyntaxErrorNode[op_, children_, data_]] := SyntaxErrorNode[op, children, KeyTake[data, keysToTake]]



abstract[f_Failure] := f

abstract[args___] := Failure["InternalUnhandled", <|"Function"->abstract, "Arguments"->HoldForm[{args}]|>]





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
negate[IntegerNode[Integer, str_, data1_], data_] :=
	IntegerNode[Integer, "-"<>str, data]

negate[RealNode[Real, str_, data1_], data_] :=
	RealNode[Real, "-"<>str, data]

(*

NOT ABSTRACTED YET!

Important to use InfixNode[Times and not just CallNode[Times,

This allows these nodes to be merged later e.g., 1-a/b
*)

negate[InfixNode[Times, children_, data1_], data_] :=
	InfixNode[Times, { ToNode[-1], TokenNode[Token`Star, "*", <||>] } ~Join~ children, data]

negate[InfixNode[ImplicitTimes, children_, data1_], data_] :=
	InfixNode[Times, { ToNode[-1], TokenNode[Token`Fake`ImplicitTimes, "", <||>] } ~Join~ children, data]

negate[InfixNode[InfixInvisibleTimes, children_, data1_], data_] :=
	InfixNode[Times, { ToNode[-1], TokenNode[Token`LongName`InvisibleTimes, "\\[InvisibleTimes]", <||>] } ~Join~ children, data]

negate[InfixNode[InfixTimes, children_, data1_], data_] :=
	InfixNode[Times, { ToNode[-1], TokenNode[Token`LongName`Times, "\\[Times]", <||>] } ~Join~ children, data]

negate[node_, data_] :=
	InfixNode[Times, { ToNode[-1], TokenNode[Token`Star, "*", <||>], node }, data]


(*
NOT ABSTRACTED YET!

so must still supply GroupNode[ { OpenSquare, CloseSquare } ]
*)
reciprocate[node_, data_] :=
	CallNode[ToNode[Power], { GroupNode[GroupSquare, { TokenNode[Token`OpenSquare, "[", <||>], node, ToNode[-1], TokenNode[Token`CloseSquare, "]", <||>] }, <||> ] }, data]




(*
abstract syntax of  +a + b - c \[ImplicitPlus] d  is a single Plus expression
except when it's not, bug 365287
TODO: add 365287 to kernel quirks mode
*)
flattenPlus[nodes_List, data_] :=
	Module[{},
		(
			Switch[#,
				PrefixNode[Plus, {_, _}, _],
					flattenPlus[{#[[2,2]]}, data]
				,
				InfixNode[Plus, _, _],
					flattenPlus[#[[2]][[;;;;2]], data]
				,
				BinaryNode[Minus, {_, _, _}, _],
					flattenPlus[{#[[2,1]], negate[#[[2,3]], data]}, data]
				,
				InfixNode[InfixImplicitPlus, _, _],
					flattenPlus[#[[2]][[;;;;2]], data]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractPlus[InfixNode[Plus, children_, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[children, data]], data]

abstractPlus[BinaryNode[Minus, {left_, right_}, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[{left, negate[right, data]}, data]], data]

abstractPlus[InfixNode[InfixImplicitPlus, children_, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[children, data]], data]

(*
abstract syntax of  -a * b / c d \[InvisibleTimes] e \[Times] f  is a single Times expression
*)
flattenTimes[nodes_List, data_] :=
	Module[{},
		(
			Switch[#,
				(*
				These rules for PrefixNode illustrate the difference between the FE and kernel
				bug 139531

				TODO: add to kernel quirks mode
				TODO: add to frontend quirks mode

				*)
				PrefixNode[Minus, {_, IntegerNode[_, _, _]}, _],
					{negate[#[[2,2]], data]}
				,
				PrefixNode[Minus, {_, RealNode[_, _, _]}, _],
					{negate[#[[2,2]], data]}
				,
				PrefixNode[Minus, {_, _}, _],
					{ToNode[-1], #[[2,2]]}
				,
				InfixNode[Times, _, _],
					flattenTimes[#[[2]][[;;;;2]], data]
				,
				(*
				This rule for BinaryNode[Divide] illustrate the difference between the FE and kernel
				*)
				BinaryNode[Divide, {_, _, _}, _],
					flattenTimes[{#[[2,1]], reciprocate[#[[2,3]], data]}, data]
				,
				InfixNode[ImplicitTimes, _, _],
					flattenTimes[#[[2]][[;;;;2]], data]
				,
				InfixNode[InfixInvisibleTimes, _, _],
					flattenTimes[#[[2]][[;;;;2]], data]
				,
				InfixNode[InfixTimes, _, _],
					flattenTimes[#[[2]][[;;;;2]], data]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractTimes[InfixNode[Times, children_, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[children, data]], data]

abstractTimes[BinaryNode[Divide, {left_, right_}, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[{left, reciprocate[right, data]}, data]], data]

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
					flattenInequality[{#[[2,1]], ToNode[Equal], #[[2,3]]}]
				,
				BinaryNode[Unequal, _, _],
					flattenInequality[{#[[2,1]], ToNode[Unequal], #[[2,3]]}]
				,
				BinaryNode[Less, _, _],
					flattenInequality[{#[[2,1]], ToNode[Less], #[[2,3]]}]
				,
				BinaryNode[Greater, _, _],
					flattenInequality[{#[[2,1]], ToNode[Greater], #[[2,3]]}]
				,
				BinaryNode[LessEqual, _, _],
					flattenInequality[{#[[2,1]], ToNode[LessEqual], #[[2,3]]}]
				,
				BinaryNode[GreaterEqual, _, _],
					flattenInequality[{#[[2,1]], ToNode[GreaterEqual], #[[2,3]]}]
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






flattenBinaryToInfix[op_, nodes_List] :=
	Module[{},
		(
			Switch[#,
				BinaryNode[op, _, _],
					flattenBinaryToInfix[op, #[[2, {1, 3}]]]
				,
				_,
					#
			]
		)& /@ nodes
	]

abstractBinaryToInfix[BinaryNode[op_, children_, data_]] :=
	CallNode[ToNode[op], abstract /@ Flatten[flattenBinaryToInfix[op, children]], data]










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

    head = InfixNode[CompoundExpression, Riffle[{headIn}, TokenNode[Token`Semi, ";", <||>]] ~Join~ {TokenNode[Token`Semi, ";", <||>], InternalNullNode[Null, "", <||>]}, <||>];

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

    head = InfixNode[CompoundExpression, Riffle[{headIn}, TokenNode[Token`Semi, ";", <||>]] ~Join~ {TokenNode[Token`Semi, ";", <||>], InternalNullNode[Null, "", <||>]}, <||>];

	abstract[InfixNode[CompoundExpression, { CallNode[head, { groupIn }, data], TokenNode[Token`Semi, ";", <||>] } ~Join~ Riffle[{rest}, TokenNode[Token`Semi, ";", <||>]], <||>]]
]


(*
properly abstract and warn about a;b;\[LeftDoubleBracket]\[RightDoubleBracket]
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupDoubleBracket, _, _] }, dataIn_]] :=
Module[{head, data, groupData, issues},

    data = dataIn;

    groupData = groupIn[[3]];

    issues = Lookup[data, AbstractSyntaxIssues, {}];

    AppendTo[issues, SyntaxIssue["StrangeCall", "Strange Part call.", "Error", <|Source->groupData[Source]|>]];

    AssociateTo[data, AbstractSyntaxIssues -> issues];

    head = InfixNode[CompoundExpression, Riffle[{headIn}, TokenNode[Token`Semi, ";", <||>]] ~Join~ {TokenNode[Token`Semi, ";", <||>], InternalNullNode[Null, "", <||>]}, <||>];

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

    AppendTo[issues, SyntaxIssue["StrangeCall", "Strange Part call.", "Error", <|Source->groupData[Source]|>]];

    AssociateTo[data, AbstractSyntaxIssues -> issues];

    head = InfixNode[CompoundExpression, Riffle[{headIn}, TokenNode[Token`Semi, ";", <||>]] ~Join~ {TokenNode[Token`Semi, ";", <||>], InternalNullNode[Null, "", <||>]}, <||>];

    abstract[InfixNode[CompoundExpression, { CallNode[head, { groupIn }, data], TokenNode[Token`Semi, ";", <||>] } ~Join~ Riffle[{rest}, TokenNode[Token`Semi, ";", <||>]], <||>]]
]


abstractCompoundExpressionChild[InternalNullNode[Null, _, data_]] :=
    SymbolNode[Symbol, "Null", data]

abstractCompoundExpressionChild[c_] :=
	abstract[c]

abstractCompoundExpression[InfixNode[CompoundExpression, children_, data_]] :=
	CallNode[ToNode[CompoundExpression], abstractCompoundExpressionChild /@ children, data]






(*
handle "featuroid"
bug 365013
where a<>StringJoin@b parses as a<>b

Difference between FE and kernel

TODO: add to kernel quirks mode
*)

abstractStringJoinChild[BinaryNode[BinaryAt, {SymbolNode[Symbol, "StringJoin", _], _, c_}, _]] :=
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

	(*
	a::b::c::d
	*)
	If[Length[{rest}] > 2,
		AppendTo[issues, SyntaxIssue["SyntaxUndocumentedMessageName", "This syntax is not documented.", "Error", <|Source->data[Source]|>]];
	];
	
	If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	CallNode[ToNode[MessageName], {abstract[left]} ~Join~ (ToNode[abstractString[#["String"]]]& /@ {rest}), data]
]








(*
abstract syntax of  a && b \[And] c  is a single And expression
*)
flattenAnd[nodes_List] :=
	Module[{},
		(
			Switch[#,
				InfixNode[And, _, _],
					flattenAnd[#[[2, ;;;;2]]]
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
					flattenOr[#[[2, ;;;;2]]]
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

derivativeOrderAndBody[PostfixNode[Derivative, {rand_, _}, data_]] :=
Module[{order, body},
	{order, body} = derivativeOrderAndBody[rand];
	{order+1, body}
]

derivativeOrderAndBody[node_] :=
	{0, abstract[node]}

abstractDerivative[PostfixNode[Derivative, {rand_}, data_]] :=
Module[{order, body},
	{order, body} = derivativeOrderAndBody[rand];
	CallNode[CallNode[ToNode[Derivative], {ToNode[order+1]}, <||>], {body}, <||>]
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
    			AppendTo[issues, SyntaxIssue["Comma", "Comma encountered with no adjacent expression.\n\
The expression will be treated as Null.", "Error", #[[3]]]];
    			SymbolNode[Symbol, "Null", #[[3]]]
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
   	AppendTo[issues, SyntaxIssue["Comma", "Comma encountered with no adjacent expression.\n\
The expression will be treated as Null.", "Error", children[[-1]][[3]]]];
   	AppendTo[abstractedChildren, SymbolNode[Symbol, "Null", children[[-1]][[3]]]];
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
			##2 represents a sequence of arguments, so it is wrong to call
    		*)
    		_SlotSequenceNode,
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange ``Part`` call.", "Error", <|Source->data[Source]|>]];
    		,
        _SymbolNode (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode | _SlotNode (*| _SlotSequenceNode *),
        (* these are fine *)
        Null
        ,
        _OutNode,
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange ``Part`` call.", "Warning", <|Source->data[Source]|>]];
        ,
        PrefixNode[PrefixLinearSyntaxBang, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange ``Part`` call.", "Remark", <|Source->data[Source]|>]];
        ,
        InfixNode[CompoundExpression, _, _],
        (* CompoundExpression was already handled *)
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
        GroupNode[GroupLinearSyntaxParen, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange ``Part`` call.", "Remark", <|Source->data[Source]|>]];
        ,
        GroupNode[_, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange ``Part`` call.", "Warning", <|Source->data[Source]|>]];
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
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange ``Part`` call.", "Error", <|Source->data[Source]|>]];
    ];

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[outerData[Source][[1,2]]+1 != innerData[Source][[1,2]],
		AppendTo[issues, SyntaxIssue["NotContiguous", "``Part`` brackets ``[[`` are not contiguous.", "Formatting", <|Source->{outerData[Source][[1]], innerData[Source][[1]]}|>]];
	];

	If[innerData[Source][[2,2]]+1 != outerData[Source][[2,2]],
		AppendTo[issues, SyntaxIssue["NotContiguous", "``Part`` brackets ``]]`` are not contiguous.", "Formatting", <|Source->{innerData[Source][[2]], outerData[Source][[2]]}|>]];
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
			##2 represents a sequence of arguments, so it is wrong to call
    		*)
    		_SlotSequenceNode,
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        _SymbolNode | _StringNode | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*)|
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode | _SlotNode (*| _SlotSequenceNode*),
        (* these are fine *)
        Null
        ,
        _OutNode,
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Warning", <|Source->data[Source]|>]];
        ,
        BinaryNode[PatternTest, _, _],
        (* these are fine *)
        Null
        ,
        InfixNode[CompoundExpression, _, _],
        (* CompoundExpression was already handled *)
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
        GroupNode[_, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Warning", <|Source->data[Source]|>]];
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
			##2 represents a sequence of arguments, so it is wrong to call
    		*)
    		_SlotSequenceNode,
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        _SymbolNode (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode | _SlotNode (*| _SlotSequenceNode *),
        (* these are fine *)
        Null
        ,
        _OutNode,
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Warning", <|Source->data[Source]|>]];
        ,
        PrefixNode[PrefixLinearSyntaxBang, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Remark", <|Source->data[Source]|>]];
        ,
        (*
        BinaryNode[PatternTest, _, _],
        (* these are fine *)
        Null
        ,*)
        InfixNode[CompoundExpression, _, _],
        (* CompoundExpression was already handled *)
        Null
        ,
        GroupNode[GroupParen | List | Association, _, _],
        (* these are fine *)
        Null
        ,
        GroupNode[GroupLinearSyntaxParen, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Remark", <|Source->data[Source]|>]];
        ,
        GroupNode[_, _, _],
        AppendTo[issues, SyntaxIssue["StrangeCall", "Strange call.", "Warning", <|Source->data[Source]|>]];
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
