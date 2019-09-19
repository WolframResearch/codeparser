BeginPackage["AST`Abstract`"]

Aggregate

Abstract



$AggregateParseProgress

$AbstractParseProgress


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["AST`Folds`"]

(*

concrete syntax is everything

concrete syntax has CallNode[{head, comment}, {child1}]

concrete syntax has InfixNode[Plus, {1, +, comment, 1}]




aggregate syntax

aggregate removes comments, whitespace, and newlines

aggregate syntax has CallNode[head, {child1}]

aggregate syntax has InfixNode[Plus, {1, +, 1}]






abstract syntax

abstract syntax has CallNode[head, {child1}]

abstract syntax has CallNode[Plus, {1, 1}]
*)

Aggregate::usage = "Aggregate[cst] returns an aggregate syntax tree from a concrete syntax tree."

Aggregate[cst_] :=
Block[{$RecursionLimit = Infinity},
Module[{agg},

	AST`Abstract`$AggregateParseStart = Now;
	AST`Abstract`$AggregateParseTime = Quantity[0, "Seconds"];

	AST`Abstract`$AggregateParseProgress = 5;

	agg = aggregate[cst];

	AST`Abstract`$AggregateParseProgress = 100;
	AST`Abstract`$AggregateParseTime = Now - AST`Abstract`$AggregateParseStart;

	agg
]]





(*
what keys do we want to keep when abstracting?
*)
keysToTake = {Source, AbstractSyntaxIssues}


Abstract::usage = "Abstract[agg] returns an abstract syntax tree from an aggregate syntax tree."

Abstract[agg_] :=
Block[{$RecursionLimit = Infinity},
Module[{ast},

	AST`Abstract`$AbstractParseStart = Now;
	AST`Abstract`$AbstractParseTime = Quantity[0, "Seconds"];

	AST`Abstract`$AbstractParseProgress = 5;

	ast = abstract[agg];

	AST`Abstract`$AbstractParseProgress = 100;

	AST`Abstract`$AbstractParseTime = Now - AST`Abstract`$AbstractParseStart;

	ast
]]



abstract[LeafNode[Symbol, s_, data_]] := LeafNode[Symbol, abstractLineContinuation[s], KeyTake[data, keysToTake]]
abstract[LeafNode[String, s_, data_]] := LeafNode[String, abstractLineContinuation[s], KeyTake[data, keysToTake]]
abstract[LeafNode[Integer, s_, data_]] := LeafNode[Integer, abstractLineContinuation[s], KeyTake[data, keysToTake]]
abstract[LeafNode[Real, s_, data_]] := LeafNode[Real, abstractLineContinuation[s], KeyTake[data, keysToTake]]

abstract[LeafNode[Slot, str_, data_]] :=
Module[{a},
	a = abstractLineContinuation[str];
	Switch[a,
		"#",
		    CallNode[ToNode[Slot], {ToNode[1]}, KeyTake[data, keysToTake]]
		,
		test_ /; StringMatchQ[test, "#"~~DigitCharacter..],
		    CallNode[ToNode[Slot], {ToNode[FromDigits[StringDrop[a, 1]]]}, KeyTake[data, keysToTake]]
		,
		_,
		    CallNode[ToNode[Slot], {ToNode[abstractString[StringDrop[a, 1]]]}, KeyTake[data, keysToTake]]
	]
]

abstract[LeafNode[SlotSequence, str_, data_]] :=
Module[{a},
	a = abstractLineContinuation[str];
	Switch[a,
		"##",
		    CallNode[ToNode[SlotSequence], {ToNode[1]}, KeyTake[data, keysToTake]]
		,
		_,
		    CallNode[ToNode[SlotSequence], {ToNode[FromDigits[StringDrop[a, 2]]]}, KeyTake[data, keysToTake]]
	]
]

abstract[LeafNode[Out, str_, data_]] :=
Module[{a},
   a = abstractLineContinuation[str];
	Switch[a,
		"%",
		    CallNode[ToNode[Out], {}, KeyTake[data, keysToTake]]
		,
		_,
		    CallNode[ToNode[Out], { ToNode[-StringLength[a]] }, KeyTake[data, keysToTake]]
	]
]

abstract[LeafNode[Blank, _, data_]] := CallNode[ToNode[Blank], {}, KeyTake[data, keysToTake]]
abstract[LeafNode[BlankSequence, _, data_]] := CallNode[ToNode[BlankSequence], {}, KeyTake[data, keysToTake]]
abstract[LeafNode[BlankNullSequence, _, data_]] := CallNode[ToNode[BlankNullSequence], {}, KeyTake[data, keysToTake]]
abstract[LeafNode[OptionalDefault, _, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]

abstract[LeafNode[Token`Error`UnhandledCharacter, str_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`Unhandled, str, KeyTake[data, keysToTake]]

abstract[LeafNode[Token`Fake`ImplicitNull, _, data_]] := LeafNode[Symbol, "Null", KeyTake[data, keysToTake] ~Join~ <|AbstractSyntaxIssues->{SyntaxIssue["Comma", "Comma encountered with no adjacent expression.\n\
The expression will be treated as ``Null``.", "Error", <|data, CodeActions->{CodeAction["Delete Comma", DeleteNode, <||>]}|>]}|>]

abstract[LeafNode[Token`Fake`ImplicitOne, _, data_]] := LeafNode[Integer, "1", KeyTake[data, keysToTake]]

abstract[LeafNode[Token`Fake`ImplicitAll, _, data_]] := LeafNode[Symbol, "All", KeyTake[data, keysToTake]]

abstract[BlankNode[Blank, {_, sym2_}, data_]] := CallNode[ToNode[Blank], {abstract[sym2]}, KeyTake[data, keysToTake]]
abstract[BlankSequenceNode[BlankSequence, {_, sym2_}, data_]] := CallNode[ToNode[BlankSequence], {abstract[sym2]}, KeyTake[data, keysToTake]]
abstract[BlankNullSequenceNode[BlankNullSequence, {_, sym2_}, data_]] := CallNode[ToNode[BlankNullSequence], {abstract[sym2]}, KeyTake[data, keysToTake]]

abstract[PatternBlankNode[PatternBlank, {sym1_, _}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNode[PatternBlank, {sym1_, _, sym2_}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[Blank], {abstract[sym2]}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, _}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[BlankSequence], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, _, sym2_}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[BlankSequence], {abstract[sym2]}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, _}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[BlankNullSequence], {}, <||>]}, KeyTake[data, keysToTake]]
abstract[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, _, sym2_}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[BlankNullSequence], {abstract[sym2]}, <||>]}, KeyTake[data, keysToTake]]
abstract[OptionalDefaultPatternNode[OptionalDefaultPattern, {sym1_, _}, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[Blank], {}, <||>]}, <||>]}, KeyTake[data, keysToTake]]


(*
Remove line continuations

Line continuations might be inside of strings and we want to remove them

I don't feel like constructing a regex to do the matching for true line continuations
So use an auxiliary function. This is probably easier to understand.
*)
abstractLineContinuation[s_String /; StringContainsQ[s, "\\\n"]] :=
Module[{candidatePoss, actualLCs, specs},
	candidatePoss = StringPosition[s, "\\\n"];
	actualLCs = Select[candidatePoss, isLineContinuation[s, #]&];

	(*
	Used to be:
	StringReplacePart[s, "", actualLCs]

	but this is VERY slow
	*)

	(*
	Convert the LC poss into Take specs in-between the line continuations
	*)
	specs = {#[[1]] + 1, #[[2]] - 1}& /@ Partition[{0} ~Join~ Flatten[actualLCs] ~Join~ {StringLength[s] + 1}, 2];

	(*
	And then StringJoin all of the in-between parts
	*)
	StringJoin[StringTake[s, specs]]
]

abstractLineContinuation[s_String] := s



(*

We need to do a test for line continuation because \\\n does not NECESSARILY mean a line continuation

There could be preceding \\, thus changing the semantics of \\\n


s: input string
candidatePos: position of \\\n that looks like a line continuation

go backwards and count an odd number of \\

isLineContinuation["abc\\\ndef", {4, 5}] => True

isLineContinuation["ab\\\\\ndef", {4, 5}] => False

*)
isLineContinuation[s_String, candidatePos_List] :=
Catch[
Module[{acc, nToTest},
	(*
	acc: how many \ have we seen so far?
	*)
	acc = 1;
	nToTest = candidatePos[[1]] - 1;
	While[True,
		If[nToTest == 0,
			Throw[OddQ[acc]]
		];
		If[StringPart[s, nToTest] != "\\",
			Throw[OddQ[acc]]
		];
		acc++;
		nToTest--;
	]
]]







abstract[PrefixNode[Minus, {_, rand_}, data_]] := abstract[negate[rand, KeyTake[data, keysToTake]]]
abstract[PrefixNode[PrefixLinearSyntaxBang, children:{_, Except[GroupNode[GroupLinearSyntaxParen, _, _]]}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`LinearSyntaxBang, children, KeyTake[data, keysToTake]]
(*
FIXME: keep linear syntax for now
*)
abstract[PrefixNode[PrefixLinearSyntaxBang, {rator_, rand_}, data_]] := PrefixNode[PrefixLinearSyntaxBang, {rator, abstract[rand]}, KeyTake[data, keysToTake]]

(*
strings may be quoted

concrete syntax: <<a
abstract syntax Get["a"]

concrete syntax: <<"a"
abstract syntax Get["a"]
*)
abstract[PrefixNode[Get, {_, LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Get], {ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]

abstract[PrefixNode[op_, {_, operand_}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]


abstract[PostfixNode[Derivative, {operand_, rator_}, data_]] := abstractDerivative[PostfixNode[Derivative, {operand, rator}, KeyTake[data, keysToTake]]]
abstract[PostfixNode[op_, {operand_, _}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]








abstract[BinaryNode[Minus, { left_, _, right_ }, data_]] := abstractPlus[BinaryNode[Minus, {left, right}, KeyTake[data, keysToTake]]]
abstract[BinaryNode[Divide, { left_, _, right_ }, data_]] := abstractTimes[BinaryNode[Divide, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[BinaryAt, {left_, _, right_}, data_]] := CallNode[abstract[left], {abstract[right]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinaryAtAtAt, {left_, _, right_}, data_]] := CallNode[ToNode[Apply], abstract /@ {left, right, GroupNode[List, { LeafNode[Token`OpenCurly, "{", <||>], ToNode[1], LeafNode[Token`CloseCurly, "}", <||>] }, <||>]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinarySlashSlash, {left_, _, right_}, data_]] := CallNode[abstract[right], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[Put, {left_, _, LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Put], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[PutAppend, {left_, _, LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[PutAppend], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]



(* Abstract NonAssociative errors *)

abstract[BinaryNode[PatternTest, children:{BinaryNode[PatternTest, _, _], _, _}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociative, children, KeyTake[data, keysToTake]]

(*
DirectedEdge and UndirectedEdge do not associate with themselves or each other
*)
abstract[BinaryNode[DirectedEdge, children:{BinaryNode[DirectedEdge | UndirectedEdge, _, _], _, _}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociative, children, KeyTake[data, keysToTake]]
abstract[BinaryNode[UndirectedEdge, children:{BinaryNode[DirectedEdge | UndirectedEdge, _, _], _, _}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociative, children, KeyTake[data, keysToTake]]

(* could be  a =. *)
abstract[BinaryNode[Unset, {left_, _}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]
(* or it could be  a = . *)
abstract[BinaryNode[Unset, {left_, _, _}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[op_, {left_, _, right_}, data_]] := CallNode[ToNode[op], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]







abstract[InfixNode[Inequality, children_, data_]] := abstractInequality[InfixNode[Inequality, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[Developer`VectorInequality, children_, data_]] := abstractVectorInequality[InfixNode[Developer`VectorInequality, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[Plus, children_, data_]] := abstractPlus[InfixNode[Plus, children[[;;;;2]], KeyTake[data, keysToTake]]]
abstract[InfixNode[Times, children_, data_]] := abstractTimes[InfixNode[Times, children[[;;;;2]], KeyTake[data, keysToTake]]]

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

(* could be  a /: b =. *)
abstract[TernaryNode[TagUnset, {left_, _, middle_, _}, data_]] := CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, KeyTake[data, keysToTake]]
(* or it could be  a /: b = . *)
abstract[TernaryNode[TagUnset, {left_, _, middle_, _, _}, data_]] := CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, KeyTake[data, keysToTake]]

abstract[TernaryNode[Span, {left_, _, middle_, _, right_}, data_]] := CallNode[ToNode[Span], {abstract[left], abstract[middle], abstract[right]}, KeyTake[data, keysToTake]]




abstract[StartOfLineNode[Information, {LeafNode[Token`Question, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Information], {ToNode[str], CallNode[ToNode[Rule], { ToNode[LongForm], ToNode[False] }, <||>]}, KeyTake[data, keysToTake]]
abstract[StartOfLineNode[Information, {LeafNode[Token`QuestionQuestion, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Information], {ToNode[str], CallNode[ToNode[Rule], { ToNode[LongForm], ToNode[True] }, <||>]}, KeyTake[data, keysToTake]]
abstract[StartOfLineNode[Run, {LeafNode[Token`Bang, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Run], {ToNode[str]}, KeyTake[data, keysToTake]]
abstract[StartOfLineNode[FilePrint, {LeafNode[Token`BangBang, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[FilePrint], {ToNode[str]}, KeyTake[data, keysToTake]]



(*
handle CallNode before possible GroupNode errors

what are the different shapes that calls can have?

They are:

f [ ]

f [ [ ] ]

f \[LeftDoubleBracket] \[RightDoubleBracket]

strip off the concrete syntax while we are here
*)

abstract[CallNode[op_, { GroupNode[GroupSquare, { _, GroupNode[GroupSquare, { _, inner___, _ }, data3_], _ }, data2_] }, data1_]] :=
	abstractCallNode[CallNode[op, { GroupNode[GroupSquare, { GroupNode[GroupSquare, { inner }, KeyTake[data3, keysToTake]] }, KeyTake[data2, keysToTake]] }, KeyTake[data1, keysToTake]]]

abstract[CallNode[op_, { GroupNode[GroupSquare, { _, inner___, _ }, data2_] }, data1_]] :=
	abstractCallNode[CallNode[op, { GroupNode[GroupSquare, { inner }, KeyTake[data2, keysToTake]] }, KeyTake[data1, keysToTake]]]

abstract[CallNode[op_, { GroupNode[GroupDoubleBracket, { _, inner___, _ }, data2_] }, data1_]] :=
	abstractCallNode[CallNode[op, { GroupNode[GroupDoubleBracket, { inner }, KeyTake[data2, keysToTake]] }, KeyTake[data1, keysToTake]]]


(*
must handle this so that AbstractSyntaxErrorNode is created later
*)

abstract[CallNode[op_, { missing:GroupMissingOpenerNode[_, _, _] }, data1_]] :=
	abstractCallNode[CallNode[op, { missing }, KeyTake[data1, keysToTake]]]

abstract[CallNode[op_, { missing:GroupMissingCloserNode[_, _, _] }, data1_]] :=
	abstractCallNode[CallNode[op, { missing }, KeyTake[data1, keysToTake]]]





(*
take care of specific GroupNodes before calling abstractGroupNode
*)

(*
GroupParen
*)
abstract[GroupNode[GroupParen, { _, child:InfixNode[Comma, _, _], _ }, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, child, KeyTake[data, keysToTake]]
abstract[GroupNode[GroupParen, { _, child_, _}, data_]] := abstract[child]

(* GroupNode errors *)
abstract[GroupNode[GroupSquare, children_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, children, KeyTake[data, keysToTake]]

(*
FIXME: skip abstracting linear syntax for now
GroupLinearSyntaxParen retains its commas, so handle before abstractGroupNode
*)
abstract[GroupNode[GroupLinearSyntaxParen, children_, data_]] := GroupNode[GroupLinearSyntaxParen, children, KeyTake[data, keysToTake]]



(*
Missing closers and openers
*)

abstract[GroupMissingCloserNode[_, children_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, children, KeyTake[data, keysToTake]]

abstract[GroupMissingOpenerNode[_, children_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingOpener, children, KeyTake[data, keysToTake]]

abstract[GroupNode[op_, {_, inner___, _}, data_]] := abstractGroupNode[GroupNode[op, { inner }, KeyTake[data, keysToTake]]]




abstract[PrefixBinaryNode[Integrate, {_, operand1_, PrefixNode[DifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[Integrate], {abstract[operand1], abstract[var]}, KeyTake[data, keysToTake]]

abstract[PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]] := CallNode[ToNode[op], {abstract[operand1], abstract[operand2]}, KeyTake[data, keysToTake]]



abstract[FileNode[File, children_, dataIn_]] :=
Module[{abstracted, issues, issues1, issues2, data, abstractedChildren, node},

	data = dataIn;

	issues = {};

	{abstractedChildren, issues1} = abstractTopLevelChildren[children];

	{abstracted, issues2} = abstractTopLevel[abstractedChildren];

	issues = issues1 ~Join~ issues2;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	node = FileNode[File, abstracted, KeyTake[data, keysToTake]];

	node
]

abstract[HoldNode[Hold, children_, dataIn_]] :=
Module[{abstracted, issues, issues1, issues2, data, abstractedChildren, node},

	data = dataIn;

	issues = {};

	{abstractedChildren, issues1} = abstractTopLevelChildren[children];

	{abstracted, issues2} = abstractTopLevel[abstractedChildren];

	issues = issues1 ~Join~ issues2;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];

	node = HoldNode[Hold, abstracted, KeyTake[data, keysToTake]];

	node
]



matchingOperatorPatterns[CallNode[LeafNode[Symbol, "EndPackage", _], {}, _]] = _PackageNode
matchingOperatorPatterns[CallNode[LeafNode[Symbol, "End", _], {}, _]] = _ContextNode
matchingOperatorPatterns[CallNode[LeafNode[Symbol, "EndStaticAnalysisIgnore", _], {}, _]] = _StaticAnalysisIgnoreNode



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
topLevelChildIssues[LeafNode[Symbol,_,_], True] := {}

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
	LeafNode[Token`OpenCurly, _, _],
	LeafNode[Symbol | String, _, _],
	LeafNode[Token`CloseCurly, _, _] }, _], True] := {}

topLevelChildIssues[GroupNode[List, {
	LeafNode[Token`OpenCurly, _, _],
	InfixNode[Comma, { PatternSequence[LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _], _]...,
		LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _] }, _],
	LeafNode[Token`CloseCurly, _, _] }, _], True] := {}

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
												CallNode[_, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												LeafNode[Symbol, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												BinaryNode[AddTo | Apply | BinaryAt | BinaryAtAtAt |
													BinarySlashSlash | Map | Set | SetDelayed |
													SubtractFrom | Unset | UpSet | UpSetDelayed, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												TernaryNode[TagSet | TagSetDelayed | TernaryTilde, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PrefixNode[Get | PreDecrement | PreIncrement, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PostfixNode[Decrement | Increment, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[List, {
												LeafNode[Token`OpenCurly, _, _],
												InfixNode[Comma, { PatternSequence[LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _], LeafNode[Token`Comma, _, _]]...,
													LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _] }, _],
												LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[List, {
												LeafNode[Token`OpenCurly, _, _],
												LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _],
												LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[GroupParen, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

(*
more specific stuff inside CompoundExpression
*)

(*
allow a=1;b=2;c=3;

FIXME: maybe this is too niche

*)
topLevelChildIssues[InfixNode[CompoundExpression, {
												PatternSequence[BinaryNode[Set | SetDelayed | Unset, _, _], _LeafNode].., LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PatternSequence[LeafNode[Symbol, _, _], _LeafNode].., LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PatternSequence[LeafNode[Symbol, _, _], _LeafNode].., LeafNode[Symbol, _, _] }, _], True] := {}


topLevelChildIssues[InfixNode[CompoundExpression, _, data_], True] := { SyntaxIssue["TopLevel", "Strange expression is at top-level.\n\
Consider breaking up expression on separate lines or removing the ``;``.", "Warning", data] }




(*
Anything else, then warn

Specifically add a DidYouMean for /
*)
topLevelChildIssues[BinaryNode[Divide, _, data_], True] := { SyntaxIssue["TopLevel", "Strange expression is at top-level.\n\
Did you mean ``/@``?", "Warning", data] }

topLevelChildIssues[node_, True] := { SyntaxIssue["TopLevel", "Strange expression is at top-level.", "Warning", node[[3]]] }








(*

input: a list of top-level nodes

returns: {abstracted top-level nodes, any AbstractSyntaxErrors that occurred}

*)
abstractTopLevel[listIn_] :=
Catch[
Module[{list, nodeListStack , currentList, operatorStack, currentOperator, x, issues, nodeList},
	
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
		CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _],
			AppendTo[operatorStack, PackageNode[x[[2]], {}, <|Source->{x[[3]][Source][[1]], (*partially constructed Source*)Indeterminate}|>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		BeginPackage["Foo`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _], LeafNode[Symbol, "Null", _]}, _],
   		AppendTo[operatorStack, PackageNode[x[[2]][[1]][[2]], {}, <|Source->{x[[2]][[1]][[3]][Source][[1]], (*partially constructed Source*)Indeterminate}|>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		Begin["`Private`"]
		*)
		CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _],
			AppendTo[operatorStack, ContextNode[x[[2]], {}, <|Source->{x[[3]][Source][[1]], (*partially constructed Source*)Indeterminate}|>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		Begin["`Private`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _], LeafNode[Symbol, "Null", _]}, _],
   		AppendTo[operatorStack, ContextNode[x[[2]][[1]][[2]], {}, <|Source->{x[[2]][[1]][[3]][Source][[1]], (*partially constructed Source*)Indeterminate}|>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		BeginStaticAnalysisIgnore[]
		*)
		CallNode[LeafNode[Symbol, "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore", _], {}, _],
			AppendTo[operatorStack, StaticAnalysisIgnoreNode[x[[2]], {}, <|Source->{x[[3]][Source][[1]], (*partially constructed Source*)Indeterminate}|>]];
			AppendTo[nodeListStack, {}];
		,
		(*
		BeginStaticAnalysisIgnore[] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore", _], {}, _], LeafNode[Symbol, "Null", _]}, _],
   		AppendTo[operatorStack, StaticAnalysisIgnoreNode[x[[2]][[1]][[2]], {}, <|Source->{x[[2]][[1]][[3]][Source][[1]], (*partially constructed Source*)Indeterminate}|>]];
			AppendTo[nodeListStack, {}];
   	,
   	(*
		EndPackage[]
		End[]
		EndStaticAnalysisIgnore[]
		*)
		CallNode[LeafNode[Symbol, "EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], {}, _],
			currentOperator = operatorStack[[-1]];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced package directives.", "Error", x[[3]]]];
				Throw[{list, issues}];
			];
			operatorStack = Drop[operatorStack, -1];
			currentList = nodeListStack[[-1]];
			nodeListStack = Drop[nodeListStack, -1];
			currentOperator[[2]] = currentList;
			(* finish constructing Source *)
			currentOperator[[3, Key[Source], 2]] = x[[3]][Source][[2]];
			AppendTo[nodeListStack[[-1]], currentOperator];
		,
		(*
		EndPackage[] ;
		End[] ;
		EndStaticAnalysisIgnore[] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], {}, _], LeafNode[Symbol, "Null", _]}, _],
   		currentOperator = operatorStack[[-1]];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x[[2]][[1]]]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced package directives.", "Error", x[[2]][[1]][[3]]]];
				Throw[{list, issues}];
			];
			operatorStack = Drop[operatorStack, -1];
			currentList = nodeListStack[[-1]];
			nodeListStack = Drop[nodeListStack, -1];
			currentOperator[[2]] = currentList;
			(* finish constructing Source *)
			currentOperator[[3, Key[Source], 2]] = x[[2]][[1]][[3]][Source][[2]];
			AppendTo[nodeListStack[[-1]], currentOperator];
   	,
   	(*
		All other calls to recognized directives
   	*)
		CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore" |
											"EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], _, _],
			AppendTo[issues, SyntaxIssue["Package", "Package directive does not have correct syntax.", "Error", x[[3]]]];
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;
   	*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore" |
																													"EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], _, _], LeafNode[Symbol, "Null", _]}, _],
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

	nodeList = nodeListStack[[1]];

	{nodeList, issues}
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







parenthesizedIntegerOrRealQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
	parenthesizedIntegerOrRealQ[child]

parenthesizedIntegerOrRealQ[LeafNode[Integer, _, _]] := True
parenthesizedIntegerOrRealQ[LeafNode[Real, _, _]] := True

parenthesizedIntegerOrRealQ[_] := False





possiblyNegatedZeroQ[LeafNode[Integer, "0", _]] := True

possiblyNegatedZeroQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
	possiblyNegatedZeroQ[child]

possiblyNegatedZeroQ[PrefixNode[Minus, { _, child_}, _]] :=
	possiblyNegatedZeroQ[child]

possiblyNegatedZeroQ[_] := False



(*
concrete syntax does not have negated numbers
abstract syntax is allowed to have negated numbers
*)

negate[LeafNode[Integer, "0", data1_], data_] :=
	LeafNode[Integer, "0", data]

negate[LeafNode[Integer, str_, data1_], data_] :=
	LeafNode[Integer, "-"<>str, data]

negate[LeafNode[Real, str_, data1_], data_] :=
	LeafNode[Real, "-"<>str, data]

(*
dig down into parens

something like  -(1.2)  is still parsed as  -1.2

TODO: maybe this is a kernel quirk?
*)
negate[GroupNode[GroupParen, {_, child_?possiblyNegatedZeroQ, _}, data1_], data_] :=
	negate[child, data]

negate[PrefixNode[Minus, {_, child_?possiblyNegatedZeroQ}, data1_], data_] :=
	negate[child, data]

negate[node_?parenthesizedIntegerOrRealQ, data_] :=
	negate[node[[2]][[2]], data]



(*

NOT ABSTRACTED YET!

Important to use InfixNode[Times and not just CallNode[Times,

This allows these nodes to be merged later e.g., 1-a/b
*)

negate[InfixNode[Times, children_, data1_], data_] :=
	InfixNode[Times, { ToNode[-1], LeafNode[Token`Star, "*", <||>] } ~Join~ children, data]

negate[node_, data_] :=
	InfixNode[Times, { ToNode[-1], LeafNode[Token`Star, "*", <||>], node }, data]


(*
NOT ABSTRACTED YET!

so must still supply GroupNode[ { OpenSquare, CloseSquare } ]
*)
reciprocate[node_, data_] :=
	CallNode[ToNode[Power], {
		GroupNode[GroupSquare, {
			LeafNode[Token`OpenSquare, "[", <||>],
			InfixNode[Comma, { node, LeafNode[Token`Comma, ",", <||>], ToNode[-1] }, <||>],
			LeafNode[Token`CloseSquare, "]", <||>] }, <||> ] }, data]




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
				_,
					#
			]
		)& /@ nodes
	]

abstractPlus[InfixNode[Plus, children_, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[children, data]], data]

abstractPlus[BinaryNode[Minus, {left_, right_}, data_]] :=
	CallNode[ToNode[Plus], abstract /@ Flatten[flattenPlus[{left, negate[right, data]}, data]], data]

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
				PrefixNode[Minus, { _, LeafNode[Integer | Real, _, _] }, _],
					{negate[#[[2,2]], data]}
				,
				PrefixNode[Minus, { _, _?parenthesizedIntegerOrRealQ }, _],
					{negate[#[[2,2]], data]}
				,
				PrefixNode[Minus, {_, _}, _],
					{ToNode[-1], #[[2,2]]}
				,
				InfixNode[Times, _, _],
					flattenTimes[#[[2]][[;;;;2]], data]
				,
				(*
				This rule for BinaryNode[Divide] illustrates the difference between the FE and kernel

				TODO: add to kernel quirks mode
				TODO: add to frontend quirks mode

				*)
				BinaryNode[Divide, {_, _, _}, _],
					flattenTimes[{#[[2,1]], reciprocate[#[[2,3]], data]}, data]
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




abstractInequality[InfixNode[Inequality, children_, data_]] :=
	simplifyInequality[InfixNode[Inequality, children, data]]

(*
attempt to simplify e.g. Inequality[a, Less, b, Less, c] to Less[a, b, c]
*)
simplifyInequality[InfixNode[Inequality, children_, data_]] :=
Module[{rators, rands},
	rators = inequalityOperatorToSymbol /@ children[[2;;-2;;2]];
	rands = abstract /@ children[[1;;-1;;2]];

	Switch[rators,
		{ToNode[Equal]..},
			CallNode[ToNode[Equal], rands, data]
		,
		{ToNode[Unequal]..},
			CallNode[ToNode[Unequal], rands, data]
		,
		{ToNode[Less]..},
			CallNode[ToNode[Less], rands, data]
		,
		{ToNode[Greater]..},
			CallNode[ToNode[Greater], rands, data]
		,
		{ToNode[LessEqual]..},
			CallNode[ToNode[LessEqual], rands, data]
		,
		{ToNode[GreaterEqual]..},
			CallNode[ToNode[GreaterEqual], rands, data]
		,
		_,
			CallNode[ToNode[Inequality], Riffle[rands, rators], data]
	]
]


inequalityOperatorToSymbol[LeafNode[Token`EqualEqual | Token`LongName`Equal, _, _]] := ToNode[Equal]
inequalityOperatorToSymbol[LeafNode[Token`BangEqual | Token`LongName`NotEqual, _, _]] := ToNode[Unequal]
inequalityOperatorToSymbol[LeafNode[Token`Less, _, _]] := ToNode[Less]
inequalityOperatorToSymbol[LeafNode[Token`Greater, _, _]] := ToNode[Greater]
inequalityOperatorToSymbol[LeafNode[Token`LessEqual | Token`LongName`LessEqual, _, _]] := ToNode[LessEqual]
inequalityOperatorToSymbol[LeafNode[Token`GreaterEqual | Token`LongName`GreaterEqual, _, _]] := ToNode[GreaterEqual]






abstractVectorInequality[InfixNode[Developer`VectorInequality, children_, data_]] :=
	simplifyVectorInequality[InfixNode[Developer`VectorInequality, children, data]]

(*
attempt to simplify e.g. Developer`VectorInequality[a, VectorLess, b, VectorLess, c] to VectorLess[{a, b, c}]

Yes, make sure that it is VectorLess[{a, b, c}] and not VectorLess[a, b, c]
*)
simplifyVectorInequality[InfixNode[Developer`VectorInequality, children_, data_]] :=
Module[{rators, rands},
	rators = vectorInequalityOperatorToSymbol /@ children[[2;;-2;;2]];
	rands = abstract /@ children[[1;;-1;;2]];

	Switch[rators,
		{ToNode[System`VectorLess]..},
			CallNode[ToNode[System`VectorLess], { CallNode[ToNode[List], rands, <||>] }, data]
		,
		{ToNode[System`VectorGreater]..},
			CallNode[ToNode[System`VectorGreater], { CallNode[ToNode[List], rands, <||>] }, data]
		,
		{ToNode[System`VectorLessEqual]..},
			CallNode[ToNode[System`VectorLessEqual], { CallNode[ToNode[List], rands, <||>] }, data]
		,
		{ToNode[System`VectorGreaterEqual]..},
			CallNode[ToNode[System`VectorGreaterEqual], { CallNode[ToNode[List], rands, <||>] }, data]
		,
		_,
			CallNode[ToNode[Developer`VectorInequality], Riffle[rands, rators], data]
	]
]


vectorInequalityOperatorToSymbol[LeafNode[Token`LongName`VectorLess, _, _]] := ToNode[System`VectorLess]
vectorInequalityOperatorToSymbol[LeafNode[Token`LongName`VectorGreater, _, _]] := ToNode[System`VectorGreater]
vectorInequalityOperatorToSymbol[LeafNode[Token`LongName`VectorLessEqual, _, _]] := ToNode[System`VectorLessEqual]
vectorInequalityOperatorToSymbol[LeafNode[Token`LongName`VectorGreaterEqual, _, _]] := ToNode[System`VectorGreaterEqual]







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

   head = InfixNode[CompoundExpression, Riffle[{headIn}, LeafNode[Token`Semi, ";", <||>]] ~Join~ {LeafNode[Token`Semi, ";", <||>], LeafNode[Token`Fake`ImplicitNull, "", <||>]}, <||>];

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

    head = InfixNode[CompoundExpression, Riffle[{headIn}, LeafNode[Token`Semi, ";", <||>]] ~Join~ {LeafNode[Token`Semi, ";", <||>], LeafNode[Token`Fake`ImplicitNull, "", <||>]}, <||>];

	abstract[InfixNode[CompoundExpression, { CallNode[head, { groupIn }, data], LeafNode[Token`Semi, ";", <||>] } ~Join~ Riffle[{rest}, LeafNode[Token`Semi, ";", <||>]], <||>]]
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

    head = InfixNode[CompoundExpression, Riffle[{headIn}, LeafNode[Token`Semi, ";", <||>]] ~Join~ {LeafNode[Token`Semi, ";", <||>], LeafNode[Token`Fake`ImplicitNull, "", <||>]}, <||>];

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

    head = InfixNode[CompoundExpression, Riffle[{headIn}, LeafNode[Token`Semi, ";", <||>]] ~Join~ {LeafNode[Token`Semi, ";", <||>], LeafNode[Token`Fake`ImplicitNull, "", <||>]}, <||>];

    abstract[InfixNode[CompoundExpression, { CallNode[head, { groupIn }, data], LeafNode[Token`Semi, ";", <||>] } ~Join~ Riffle[{rest}, LeafNode[Token`Semi, ";", <||>]], <||>]]
]


abstractCompoundExpressionChild[LeafNode[Token`Fake`ImplicitNull, _, data_]] :=
    LeafNode[Symbol, "Null", data]

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

abstractStringJoinChild[BinaryNode[BinaryAt, {LeafNode[Symbol, "StringJoin", _], _, c_}, _]] :=
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
abstractMessageName[InfixNode[MessageName, {left_, rest:LeafNode[String, _, _]..}, dataIn_]] :=
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

abstractDerivative[PostfixNode[Derivative, {rand_, LeafNode[Token`SingleQuote, _, _]}, data_]] :=
Module[{order, body},
	{order, body} = derivativeOrderAndBody[rand];
	CallNode[CallNode[ToNode[Derivative], {ToNode[order+1]}, <||>], {body}, <||>]
]

abstractDerivative[PostfixNode[Derivative, {rand_, LeafNode[Token`Boxes`MultiSingleQuote, quoteStr_, _]}, data_]] :=
Module[{order},
	order = StringLength[quoteStr];
	CallNode[CallNode[ToNode[Derivative], {ToNode[order]}, <||>], {rand}, <||>]
]







(*
Precondition: Opener and Closer have been removed
Precondition: Commas are still present

Removes all commas

Fills in Nulls and gives SyntaxIssues for e.g. {1,,2}
*)
abstractGroupNode[GroupNode[tag_, { errs:___SyntaxErrorNode, InfixNode[Comma, children_, _] }, dataIn_]] :=
Module[{abstractedChildren, issues, data, abstractedErrs},
	data = dataIn;

	abstractedChildren = {};

	issues = {};

	abstractedErrs = abstract /@ {errs};

	abstractedChildren = abstractedErrs ~Join~ (abstract /@ children[[;;;;2]]);

   If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];
   
   CallNode[ToNode[tag], abstractedChildren, data]
]

abstractGroupNode[GroupNode[tag_, { errs:___SyntaxErrorNode, child_ }, dataIn_]] :=
Module[{abstractedChildren, issues, data, abstractedErrs},
	data = dataIn;

	abstractedChildren = {};

	issues = {};

	abstractedErrs = abstract /@ {errs};

	(*
	Handle leading commas like  f[,2]
	*)
	abstractedErrs = abstractedErrs /. {
		SyntaxErrorNode[SyntaxError`ExpectedPossibleExpression, {LeafNode[Token`Comma, _, _]}, data_] :>
			(* make sure to abstract in order to get the syntax warning *)
			abstract[LeafNode[Token`Fake`ImplicitNull, _, data]] };

	abstractedChildren = abstractedErrs ~Join~ { abstract[child] };

   If[issues != {},
   	issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
   	AssociateTo[data, AbstractSyntaxIssues -> issues];
   ];
   
   CallNode[ToNode[tag], abstractedChildren, data]
]

abstractGroupNode[GroupNode[tag_, { }, dataIn_]] :=
Module[{data},
	data = dataIn;
   
   CallNode[ToNode[tag], {}, data]
]



(*
FIXME: would be good to remember tag somehow
*)

abstractGroupNode[GroupMissingOpenerNode[tag_, children_, dataIn_]] :=
Module[{data},

	data = dataIn;

   AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingOpener, children, data]
]

abstractGroupNode[GroupMissingCloserNode[tag_, children_, dataIn_]] :=
Module[{data},

	data = dataIn;

   AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, children, data]
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
    		LeafNode[SlotSequence, _, _],
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange ``Part`` call.", "Error", <|Source->data[Source]|>]];
    		,
        LeafNode[Symbol | Slot | Blank | BlankSequence | BlankNullSequence, _, _] (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode (*| _SlotSequenceNode *),
        (* these are fine *)
        Null
        ,
        LeafNode[Out, _, _],
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
    		LeafNode[SlotSequence, _, _],
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        LeafNode[Symbol | String | Slot | Blank | BlankSequence | BlankNullSequence, _, _] | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*)|
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode (*| _SlotSequenceNode*),
        (* these are fine *)
        Null
        ,
        LeafNode[Out, _, _],
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
    		LeafNode[SlotSequence, _, _],
    		AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Strange call.", "Error", <|Source->data[Source]|>]];
    		,
        LeafNode[Symbol | Slot | Blank | BlankSequence | BlankNullSequence, _, _] (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
            _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode (*| _SlotSequenceNode *),
        (* these are fine *)
        Null
        ,
        LeafNode[Out, _, _],
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

	CallNode[ToNode[Part], {head} ~Join~ (part[[2]]), data]
]


abstractCallNode[CallNode[headIn_, {partIn:GroupMissingOpenerNode[_, _, _]}, dataIn_]] :=
Module[{head, part, data},
	head = headIn;
	part = partIn;
	data = dataIn;

	head = abstract[head];
	part = abstractGroupNode[part];

	CallNode[head, { part }, data]
]

abstractCallNode[CallNode[headIn_, {partIn:GroupMissingCloserNode[_, _, _]}, dataIn_]] :=
Module[{head, part, data},
	head = headIn;
	part = partIn;
	data = dataIn;

	head = abstract[head];
	part = abstractGroupNode[part];

	CallNode[head, { part }, data]
]







(*
FIXME: keep boxes for now

Abstract any child boxes

Do not touch CodeNodes

*)
abstract[BoxNode[SqrtBox, children_, data_]] := BoxNode[SqrtBox, abstract /@ children, KeyTake[data, keysToTake]]

abstract[BoxNode[FractionBox, children_, data_]] := BoxNode[FractionBox, abstract /@ children, KeyTake[data, keysToTake]]


abstract[BoxNode[RasterBox, children_, data_]] := BoxNode[RasterBox, children, KeyTake[data, keysToTake]]




End[]

EndPackage[]
