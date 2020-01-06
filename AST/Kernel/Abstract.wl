BeginPackage["AST`Abstract`"]

Aggregate

Abstract



$AggregateParseProgress

$AbstractParseProgress


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["AST`Folds`"]
Needs["AST`Quirks`"]



(*
How many top-level expressions are allowed?

Until completely switched over to using new DataStructure stack, we are using O(n^2) AppendTo to create the stack
of top-level expressions.

Beyond this limit, parsing is infeasible
*)
$TopLevelExpressionLimit = 5000




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
keysToTake = {Source, AbstractSyntaxIssues, AbstractFormatIssues}


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



abstract[LeafNode[Symbol, sIn_, dataIn_]] :=
	LeafNode[Symbol, sIn, KeyTake[dataIn, keysToTake]]

abstract[LeafNode[String, sIn_, dataIn_]] :=
	LeafNode[String, sIn, KeyTake[dataIn, keysToTake]]

abstract[LeafNode[Integer, sIn_, dataIn_]] :=
	LeafNode[Integer, sIn, KeyTake[dataIn, keysToTake]]

abstract[LeafNode[Real, sIn_, dataIn_]] :=
	LeafNode[Real, sIn, KeyTake[dataIn, keysToTake]]

abstract[LeafNode[Slot, sIn_, dataIn_]] :=
Module[{s, data, rest, lastPos},

	s = sIn;

	data = dataIn;

	data = KeyTake[data, keysToTake];

	lastPos = StringPosition[s, "#" | "\\.23" | "\\:0023" | "\\|000023" | "\\043" | "\\[RawNumberSign]"][[-1, 2]];

	rest = StringDrop[s, lastPos];

	(*
	Remove line continuations before analyzing
	*)
	rest = StringReplace[rest, {"\\\r\n" -> "", "\\\n" -> "", "\\\r" -> ""}];
	
	Switch[rest,
		"",
		    CallNode[ToNode[Slot], {ToNode[1]}, data]
		,
		test_ /; StringMatchQ[test, DigitCharacter..],
		    CallNode[ToNode[Slot], {ToNode[FromDigits[rest]]}, data]
		,
		_,
		    CallNode[ToNode[Slot], {ToNode[abstractString[rest]]}, data]
	]
]

abstract[LeafNode[SlotSequence, sIn_, dataIn_]] :=
Module[{s, data, rest, lastPos},

	s = sIn;

	data = dataIn;

	data = KeyTake[data, keysToTake];

	lastPos = StringPosition[s, "#" | "\\.23" | "\\:0023" | "\\|000023" | "\\043" | "\\[RawNumberSign]"][[-1, 2]];

	rest = StringDrop[s, lastPos];

	(*
	Remove line continuations before analyzing
	*)
	rest = StringReplace[rest, {"\\\r\n" -> "", "\\\n" -> "", "\\\r" -> ""}];

	Switch[rest,
		"",
			CallNode[ToNode[SlotSequence], {ToNode[1]}, data]
		,
		_,
			CallNode[ToNode[SlotSequence], {ToNode[FromDigits[rest]]}, data]
	]
]

abstract[LeafNode[Out, sIn_, dataIn_]] :=
Module[{s, data, count, lastPos, rest},

	s = sIn;

	data = dataIn;

	data = KeyTake[data, keysToTake];

	count = StringCount[s, "%" | "\\.25" | "\\:0025" | "\\|000025" | "\\045" | "\\[RawPercent]"];

	Switch[count,
		1,
			lastPos = StringPosition[s, "%" | "\\.25" | "\\:0025" | "\\|000025" | "\\045" | "\\[RawPercent]"][[-1, 2]];

			rest = StringDrop[s, lastPos];

			(*
			Remove line continuations before analyzing
			*)
			rest = StringReplace[rest, {"\\\r\n" -> "", "\\\n" -> "", "\\\r" -> ""}];

			Switch[rest,
				"",
					CallNode[ToNode[Out], {}, data]
				,
				_,
					CallNode[ToNode[Out], {ToNode[FromDigits[rest]]}, data]
			]
		,
		_,
			CallNode[ToNode[Out], { ToNode[-count] }, data]
	]
]

abstract[LeafNode[Blank, _, data_]] := CallNode[ToNode[Blank], {}, KeyTake[data, keysToTake]]
abstract[LeafNode[BlankSequence, _, data_]] := CallNode[ToNode[BlankSequence], {}, KeyTake[data, keysToTake]]
abstract[LeafNode[BlankNullSequence, _, data_]] := CallNode[ToNode[BlankNullSequence], {}, KeyTake[data, keysToTake]]
abstract[LeafNode[OptionalDefault, _, data_]] := CallNode[ToNode[Optional], {CallNode[ToNode[Blank], {}, <||>]}, KeyTake[data, keysToTake]]

abstract[LeafNode[Token`Error`UnhandledCharacter, str_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`UnhandledCharacter, { LeafNode[Token`Error`UnhandledCharacter, str, data] }, KeyTake[data, keysToTake]]

abstract[LeafNode[Token`Fake`ImplicitNull, _, data_]] := LeafNode[Symbol, "Null", KeyTake[data, keysToTake] ~Join~ <|AbstractSyntaxIssues->{SyntaxIssue["Comma", "Extra ``,``.", "Error", <| data, CodeActions->{CodeAction["Delete ``,``", DeleteNode, <| Source->data[Source] |>]}, ConfidenceLevel -> 1.0 |>]}|>]

abstract[LeafNode[Token`Error`ExpectedOperand, str_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedOperand, { LeafNode[Token`Error`ExpectedOperand, str, data] }, data]

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


abstract[LeafNode[tok_, str_, data_]] :=
	Switch[tok,
		Token`Error`UnterminatedString,
			AbstractSyntaxErrorNode[AbstractSyntaxError`UnterminatedString, { LeafNode[tok, str, data] }, KeyTake[data, keysToTake]]
		,
		Token`Boxes`OpenParenStar,
			AbstractSyntaxErrorNode[AbstractSyntaxError`UnterminatedComment, { LeafNode[tok, str, data] }, KeyTake[data, keysToTake]]
		,
		Token`Question,
			AbstractSyntaxErrorNode[AbstractSyntaxError`EmptyString, { LeafNode[tok, str, data] }, KeyTake[data, keysToTake]]
		,
		_,
			AbstractSyntaxErrorNode[AbstractSyntaxError`UnhandledToken, { LeafNode[tok, str, data] }, KeyTake[data, keysToTake]]
	]




abstract[PrefixNode[Minus, {_, rand_}, data_]] := abstract[negate[rand, KeyTake[data, keysToTake]]]

abstract[PrefixNode[PrefixNot2, {notNotTok_, rand_}, data_]] := abstractNot2[rand, notNotTok, KeyTake[data, keysToTake]]

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


abstract[PostfixNode[System`HermitianConjugate, {rand_, _}, data_]] := CallNode[ToNode[ConjugateTranspose], {abstract[rand]}, KeyTake[data, keysToTake]]

abstract[PostfixNode[Derivative, {operand_, rator_}, data_]] := abstractDerivative[PostfixNode[Derivative, {operand, rator}, KeyTake[data, keysToTake]]]

abstract[PostfixNode[op_, {operand_, _}, data_]] := CallNode[ToNode[op], {abstract[operand]}, KeyTake[data, keysToTake]]







abstract[BinaryNode[Divide, { left_, _, right_ }, data_]] := abstractTimes[BinaryNode[Divide, {left, right}, KeyTake[data, keysToTake]]]

abstract[BinaryNode[BinaryAt, {left_, _, right_}, data_]] := CallNode[abstract[left], {abstract[right]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinaryAtAtAt, {left_, _, right_}, data_]] := CallNode[ToNode[Apply], abstract /@ {left, right, GroupNode[List, { LeafNode[Token`OpenCurly, "{", <||>], ToNode[1], LeafNode[Token`CloseCurly, "}", <||>] }, <||>]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[BinarySlashSlash, {left_, _, right_}, data_]] := CallNode[abstract[right], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[Put, {left_, _, LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Put], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]
abstract[BinaryNode[PutAppend, {left_, _, LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[PutAppend], {abstract[left], ToNode[abstractString[str]]}, KeyTake[data, keysToTake]]



(*
Abstract NonAssociative errors

a ? b ? c being NonAssociative is alluded to being a bug in bug report 206938
*)
abstract[BinaryNode[PatternTest, children:{BinaryNode[PatternTest, _, _], _, _}, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociativePatternTest, children, KeyTake[data, keysToTake]]

(* could be  a =. *)
abstract[BinaryNode[Unset, {left_, _}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]
(* or it could be  a = . *)
abstract[BinaryNode[Unset, {left_, _, _}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[op_, {left_, _, right_}, data_]] := CallNode[ToNode[op], {abstract[left], abstract[right]}, KeyTake[data, keysToTake]]

abstract[BinaryNode[op_, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedOperand, children, KeyTake[data, keysToTake]]





(*
Convert f[,1] into f[Null,1]
*)
abstract[InfixNode[Comma, { LeafNode[Token`Error`ExpectedOperand, _, data1_], rest___ }, data_]] := abstract[InfixNode[Comma, { LeafNode[Token`Fake`ImplicitNull, "", data1], rest }, KeyTake[data, keysToTake]]]

abstract[InfixNode[Inequality, children_, data_]] := abstractInequality[InfixNode[Inequality, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[Developer`VectorInequality, children_, data_]] := abstractVectorInequality[InfixNode[Developer`VectorInequality, children, KeyTake[data, keysToTake]]]

(*
Do not do children[[;;;;2]]
need to remember whether Token`Plus or Token`Minus
*)
abstract[InfixNode[Plus, children_, data_]] := abstractPlus[InfixNode[Plus, children, KeyTake[data, keysToTake]]]

abstract[InfixNode[Times, children_, data_]] := abstractTimes[InfixNode[Times, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[Divisible, children_, data_]] := abstractDivisible[InfixNode[Divisible, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[CompoundExpression, children_, data_]] := abstractCompoundExpression[InfixNode[CompoundExpression, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[StringJoin, children_, data_]] := abstractStringJoin[InfixNode[StringJoin, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[MessageName, children_, data_]] := abstractMessageName[InfixNode[MessageName, children[[;;;;2]], KeyTake[data, keysToTake]]]

abstract[InfixNode[op_, children_ /; OddQ[Length[children]], data_]] := CallNode[ToNode[op], abstract /@ children[[;;;;2]], KeyTake[data, keysToTake]]

abstract[InfixNode[op_, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedOperand, children, KeyTake[data, keysToTake]]



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



(*
abstract[StartOfLineNode[Information, {LeafNode[Token`Question, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Information], {ToNode[str], CallNode[ToNode[Rule], { ToNode[LongForm], ToNode[False] }, <||>]}, KeyTake[data, keysToTake]]
abstract[StartOfLineNode[Information, {LeafNode[Token`QuestionQuestion, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Information], {ToNode[str], CallNode[ToNode[Rule], { ToNode[LongForm], ToNode[True] }, <||>]}, KeyTake[data, keysToTake]]
abstract[StartOfLineNode[Run, {LeafNode[Token`Bang, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[Run], {ToNode[str]}, KeyTake[data, keysToTake]]
abstract[StartOfLineNode[FilePrint, {LeafNode[Token`BangBang, _, _], LeafNode[String, str_, _]}, data_]] := CallNode[ToNode[FilePrint], {ToNode[str]}, KeyTake[data, keysToTake]]
*)

(*
Do not abstract #! nodes
*)
(*
abstract[StartOfFileNode[Shebang, children_, data_]] := StartOfFileNode[Shebang, children, KeyTake[data, keysToTake]]
*)




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

abstract[CallNode[op_, { missing:GroupMissingCloserNode[_, _, _] }, data1_]] :=
	abstractCallNode[CallNode[op, { missing }, KeyTake[data1, keysToTake]]]





(*
take care of specific GroupNodes before calling abstractGroupNode
*)

(*
GroupParen
*)

abstract[GroupNode[GroupParen, { _, _ }, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`EmptyParens, {}, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupParen, { _, child:InfixNode[Comma, _, _], _ }, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, { child }, KeyTake[data, keysToTake]]

abstract[GroupNode[GroupParen, { _, child_, _}, data_]] := abstract[child]

(* GroupNode errors *)
abstract[GroupNode[GroupSquare, children_, data_]] := AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, children, KeyTake[data, keysToTake]]

(*
FIXME: skip abstracting linear syntax for now
GroupLinearSyntaxParen retains its commas, so handle before abstractGroupNode
*)
abstract[GroupNode[GroupLinearSyntaxParen, children_, data_]] := GroupNode[GroupLinearSyntaxParen, children, KeyTake[data, keysToTake]]



(*
Missing closers
*)

abstract[GroupMissingCloserNode[_, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, children, KeyTake[data, keysToTake]]

abstract[GroupNode[op_, {_, inner___, _}, data_]] :=
	abstractGroupNode[GroupNode[op, { inner }, KeyTake[data, keysToTake]]]




abstract[PrefixBinaryNode[Integrate, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[Integrate], {abstract[operand1], abstract[var]}, KeyTake[data, keysToTake]]

abstract[PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]] := CallNode[ToNode[op], {abstract[operand1], abstract[operand2]}, KeyTake[data, keysToTake]]




abstract[ContainerNode[tag_, childrenIn_, dataIn_]] :=
Catch[
Module[{abstracted, issues, issues1, issues2, data, abstractedChildren, node, reportIssues, children},

	children = childrenIn;

	If[Length[children] > $TopLevelExpressionLimit,
		Throw[Failure["TooManyTopLevelExpressions", <||>]]
	];

	data = dataIn;

	reportIssues = (tag === File);

	issues = {};

	{abstractedChildren, issues1} = abstractTopLevelChildren[children, reportIssues];

	{abstracted, issues2} = abstractTopLevel[abstractedChildren];

	issues = issues1 ~Join~ issues2;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];

	node = ContainerNode[tag, abstracted, KeyTake[data, keysToTake]];

	node
]]



matchingOperatorPatterns[CallNode[LeafNode[Symbol, "EndPackage", _], {}, _]] = _PackageNode
matchingOperatorPatterns[CallNode[LeafNode[Symbol, "End", _], {}, _]] = _ContextNode
matchingOperatorPatterns[CallNode[LeafNode[Symbol, "EndStaticAnalysisIgnore", _], {}, _]] = _StaticAnalysisIgnoreNode



(*

Call abstract on children

But also warn if something strange is at top-level

*)
abstractTopLevelChildren[children_, reportIssues_] :=
Module[{abstractedChildren, issues, issuesMaybe},

	{abstractedChildren, issuesMaybe} =
		Reap[(
			Sow[topLevelChildIssues[#, reportIssues]];
			abstract[#])& /@ children
			,
			_
			,
			Flatten[#2]&
		];

	If[issuesMaybe == {},
		issues = {}
		,
		issues = issuesMaybe[[1]]
	];

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

where the parsing is ( foo[] := Message[foo::bad] ) ; $Failed

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


topLevelChildIssues[InfixNode[CompoundExpression, {_, LeafNode[Token`Semi, _, _], _[Except[Token`Fake`ImplicitNull], _, _], ___}, data_], True] := {
	SyntaxIssue["TopLevel", "``CompoundExpression`` at top-level. Consider breaking up.", "Warning",
		<| Source -> data[Source],
			ConfidenceLevel -> 0.75,
			CodeActions -> { CodeAction["Insert newline", InsertNode,
									<|	Source->nextData[Source],
										"InsertionNode"->LeafNode[Token`Newline, "\n", <||>]|>] } |>] }




(*
Anything else, then warn

Specifically add a DidYouMean for / -> /@
*)
topLevelChildIssues[BinaryNode[Divide, {_, LeafNode[Token`Slash, _, slashData_], _}, data_], True] := {
	SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning",
		<| Source -> slashData[Source],
			ConfidenceLevel -> 0.75,
			CodeActions -> { CodeAction["Replace ``/`` with ``/@``", ReplaceNode,
									<|	Source->slashData[Source],
										"ReplacementNode"->LeafNode[Token`SlashAt, "/@", <||>] |>] } |>] }

topLevelChildIssues[BinaryNode[Span, {___, LeafNode[Token`Fake`ImplicitAll, _, _]}, data_], True] := {
	SyntaxIssue["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
		<| Source -> data[Source],
			ConfidenceLevel -> 0.95 |>] }

topLevelChildIssues[InfixNode[Times, {___, BinaryNode[Span, {___, LeafNode[Token`Fake`ImplicitAll, _, _]}, data_]}, _], True] := {
	SyntaxIssue["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
		<| Source -> data[Source],
			ConfidenceLevel -> 0.95 |>] }

(*
No need to issue warning for errors being strange
*)
topLevelChildIssues[SyntaxErrorNode[_, _, _], True] := {}

topLevelChildIssues[AbstractSyntaxErrorNode[_, _, _], True] := {}

topLevelChildIssues[node:_[_, _, data_], True] :=
Module[{src},
	
	src = data[Source];

	{ SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning",
		<| Source -> TakeFirstLine[src],
			ConfidenceLevel -> 0.75 |>] }
]







(*

input: a list of top-level nodes

returns: {abstracted top-level nodes, any AbstractSyntaxErrors that occurred}

*)
abstractTopLevel[listIn_] :=
Catch[
Module[{list, nodeListStack , currentList, operatorStack, currentOperator, x, issues, nodeList, peek, error},
	
	list = listIn;

	nodeListStack = System`CreateDataStructure["ExpressionStack"];
	operatorStack = System`CreateDataStructure["ExpressionStack"];

	nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
	operatorStack["Push", None];

	issues = {};

	Do[
		x = list[[i]];
		Switch[x,
		(*
		BeginPackage["Foo`"]
		*)
		CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _],
			operatorStack["Push", PackageNode[x[[2]], {}, <|Source->{x[[3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
		,
		(*
		BeginPackage["Foo`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", PackageNode[x[[2, 1, 2]], {}, <|Source->{x[[2, 1, 3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
		,
		(*
		Begin["`Private`"]
		*)
		CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _],
			operatorStack["Push", ContextNode[x[[2]], {}, <|Source->{x[[3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
		,
		(*
		Begin["`Private`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", ContextNode[x[[2, 1, 2]], {}, <|Source->{x[[2, 1, 3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
		,
		(*
		BeginStaticAnalysisIgnore[]
		*)
		CallNode[LeafNode[Symbol, "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore", _], {}, _],
			operatorStack["Push", StaticAnalysisIgnoreNode[x[[2]], {}, <|Source->{x[[3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
		,
		(*
		BeginStaticAnalysisIgnore[] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore", _], {}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", StaticAnalysisIgnoreNode[x[[2, 1, 2]], {}, <|Source->{x[[2, 1, 3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["ExpressionStack"]];
		,
		(*
		EndPackage[]
		End[]
		EndStaticAnalysisIgnore[]
		*)
		CallNode[LeafNode[Symbol, "EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], {}, _],
			currentOperator = operatorStack["Pop"];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced Package directives.", "Error", <| x[[3]], ConfidenceLevel -> 1.0 |> ]];
				Throw[{list, issues}];
			];
			currentList = nodeListStack["Pop"];
			currentOperator[[2]] = Reverse[Normal[currentList]];
			(* finish constructing Source *)
			currentOperator[[3, Key[Source], 2]] = x[[3, Key[Source], 2]];
			peek = nodeListStack["Peek"];
			peek["Push", currentOperator];
		,
		(*
		EndPackage[] ;
		End[] ;
		EndStaticAnalysisIgnore[] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], {}, _], LeafNode[Symbol, "Null", _]}, _],
			currentOperator = operatorStack["Pop"];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x[[2, 1]] ]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced Package directives.", "Error", <| x[[2, 1, 3]], ConfidenceLevel -> 1.0 |>]];
				Throw[{list, issues}];
			];
			currentList = nodeListStack["Pop"];
			currentOperator[[2]] = Reverse[Normal[currentList]];
			(* finish constructing Source *)
			currentOperator[[3, Key[Source], 2]] = x[[2, 1, 3, Key[Source], 2]];
			peek = nodeListStack["Peek"];
			peek["Push", currentOperator];
		,
		(*
		All other calls to recognized directives
		*)
		CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore" |
											"EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], _, _],
			AppendTo[issues, SyntaxIssue["Package", "Package directive does not have correct syntax.", "Error", <| x[[3]], ConfidenceLevel -> 1.0 |> ]];
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "BeginStaticAnalysisIgnore" | "AST`BeginStaticAnalysisIgnore" |
																													"EndPackage" | "End" | "EndStaticAnalysisIgnore" | "AST`EndStaticAnalysisIgnore", _], _, _], LeafNode[Symbol, "Null", _]}, _],
			AppendTo[issues, SyntaxIssue["Package", "Package directive does not have correct syntax.", "Error", <| x[[2, 1, 3]], ConfidenceLevel -> 1.0 |>]];
			Throw[{list, issues}];
		,
		(*
		a,b  at top-level is an error
		*)
		CallNode[LeafNode[Symbol, "AST`Comma", _], _, _],
			peek = nodeListStack["Peek"];
			error = AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, x[[2]], x[[3]]];
			peek["Push", error];
		,
		(*
		All other expressions
		*)
		_,
			peek = nodeListStack["Peek"];
			peek["Push", x];
		]
	,
	{i, 1, Length[list]}
	];
	If[operatorStack["Length"] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced Package directives.", "Error", <| list[[1, 3]], ConfidenceLevel -> 1.0 |>]];
		Throw[{list, issues}];
	];
	If[operatorStack["Peek"] =!= None,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced Package directives.", "Error", <| list[[1, 3]], ConfidenceLevel -> 1.0 |>]];
		Throw[{list, issues}];
	];
	If[nodeListStack["Length"] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced Package directives.", "Error", <| list[[1, 3]], ConfidenceLevel -> 1.0 |>]];
		Throw[{list, issues}];
	];
	peek = nodeListStack["Peek"];
	nodeList = Reverse[Normal[peek]];

	(*
	Hack to prevent memory leak with shims
	*)
	Quiet[Remove["AST`Shims`Private`stack*"];, {Remove::rmnsm}];
	Quiet[Remove["AST`Shims`Private`stackVal*"];, {Remove::rmnsm}];

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

(*
This can happen with:  a-EOF
*)
negate[node:LeafNode[Token`Error`ExpectedOperand, _, _], _] :=
	node

negate[LeafNode[Integer, "0", _], data_] :=
	LeafNode[Integer, "0", data]

negate[LeafNode[Integer, str_, _], data_] :=
	LeafNode[Integer, "-"<>str, data]

negate[LeafNode[Real, str_, _], data_] :=
	LeafNode[Real, "-"<>str, data]

(*
dig down into parens

something like  -(1.2)  is still parsed as  -1.2

TODO: maybe this is a kernel quirk?
*)
negate[GroupNode[GroupParen, {_, child_?possiblyNegatedZeroQ, _}, _], data_] :=
	negate[child, data]

negate[PrefixNode[Minus, {_, child_?possiblyNegatedZeroQ}, _], data_] :=
	negate[child, data]

negate[node_?parenthesizedIntegerOrRealQ, data_] :=
	negate[node[[2, 2]], data]



(*

NOT ABSTRACTED YET!

Important to use InfixNode[Times and not just CallNode[Times,

This allows these nodes to be merged later e.g., 1-a/b
*)

negate[InfixNode[Times, children_, _], data_] :=
	InfixNode[Times, { ToNode[-1], LeafNode[Token`Star, "*", <||>] } ~Join~ children, data]

negate[node_, data_] :=
	InfixNode[Times, { ToNode[-1], LeafNode[Token`Star, "*", <||>], node }, data]


(*
NOT ABSTRACTED YET!

so must still supply GroupNode[ { OpenSquare, CloseSquare } ]
*)

(*
This can happen with:  a/EOF
*)
reciprocate[node:LeafNode[Token`Error`ExpectedOperand, _, _], _] :=
	node

reciprocate[node_, data_] :=
	CallNode[ToNode[Power], {
		GroupNode[GroupSquare, {
			LeafNode[Token`OpenSquare, "[", <||>],
			InfixNode[Comma, { node, LeafNode[Token`Comma, ",", <||>], ToNode[-1] }, <||>],
			LeafNode[Token`CloseSquare, "]", <||>] }, <||> ] }, data]



processPlusPair[{LeafNode[Token`Plus, _, _], rand_}] := rand

processPlusPair[{LeafNode[Token`Minus, _, opData_], rand_}] :=
Module[{synthesizedData},
	(*
	When parsing a - b + c, make sure to give the abstracted Times expression the correct Source.
	That is, the source of  - b
	*)
	synthesizedData = <| Source -> { opData[[Key[Source], 1]], rand[[3, Key[Source], 2]] } |>;
	negate[rand, synthesizedData]
]

(*
is it a quirk that  a + + b  is parsed as  a + b  ?
The prefix + is eaten
TODO: add to kernel quirks mode
*)
flattenPrefixPlus[PrefixNode[Plus, {_, rand_}, _]] := rand

flattenPrefixPlus[rand_] := rand



(*
abstract syntax of  +a + b - c \[ImplicitPlus] d  is a single Plus expression
except when it's not
Related bugs: 365287
TODO: add 365287 to kernel quirks mode
*)
abstractPlus[InfixNode[Plus, children_, data_]] :=
Module[{pairs, processedPairs, flattened},

	pairs = Partition[children[[2;;]], 2];

	processedPairs = processPlusPair /@ pairs;

	flattened = flattenPrefixPlus /@ ( { children[[1]] } ~Join~ processedPairs);

	CallNode[ToNode[Plus], abstract /@ flattened, data]
]




(*
abstract syntax of  -a * b / c d \[InvisibleTimes] e \[Times] f  is a single Times expression
*)
flattenTimes[nodes_List, data_] :=
	Module[{flattenTimesQuirk},

		flattenTimesQuirk = Lookup[$Quirks, "FlattenTimes", False];

		(
			Switch[#,
				(*
				These rules for PrefixNode illustrate the difference between the FE and kernel
				Related bugs: 139531

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
					If[flattenTimesQuirk,
						{ToNode[-1], #[[2,2]]}
						,
						#
					]
				,
				InfixNode[Times, _, _],
					flattenTimes[#[[2, ;;;;2]], data]
				,
				(*
				This rule for BinaryNode[Divide] illustrates the difference between the FE and kernel

				TODO: add to kernel quirks mode
				TODO: add to frontend quirks mode
				*)
				BinaryNode[Divide, {_, _, _}, _],
					If[flattenTimesQuirk,
						flattenTimes[{#[[2, 1]], reciprocate[#[[2, 3]], data]}, data]
						,
						#
					]
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
		{ToNode[Greater]..},
			CallNode[ToNode[Greater], rands, data]
		,
		{ToNode[Less]..},
			CallNode[ToNode[Less], rands, data]
		,
		{ToNode[GreaterEqual]..},
			CallNode[ToNode[GreaterEqual], rands, data]
		,
		{ToNode[GreaterEqualLess]..},
			CallNode[ToNode[GreaterEqualLess], rands, data]
		,
		{ToNode[GreaterFullEqual]..},
			CallNode[ToNode[GreaterFullEqual], rands, data]
		,
		{ToNode[GreaterGreater]..},
			CallNode[ToNode[GreaterGreater], rands, data]
		,
		{ToNode[GreaterLess]..},
			CallNode[ToNode[GreaterLess], rands, data]
		,
		{ToNode[GreaterTilde]..},
			CallNode[ToNode[GreaterTilde], rands, data]
		,
		{ToNode[LessEqual]..},
			CallNode[ToNode[LessEqual], rands, data]
		,
		{ToNode[LessEqualGreater]..},
			CallNode[ToNode[LessEqualGreater], rands, data]
		,
		{ToNode[LessFullEqual]..},
			CallNode[ToNode[LessFullEqual], rands, data]
		,
		{ToNode[LessGreater]..},
			CallNode[ToNode[LessGreater], rands, data]
		,
		{ToNode[LessLess]..},
			CallNode[ToNode[LessLess], rands, data]
		,
		{ToNode[LessTilde]..},
			CallNode[ToNode[LessTilde], rands, data]
		,
		{ToNode[NestedGreaterGreater]..},
			CallNode[ToNode[NestedGreaterGreater], rands, data]
		,
		{ToNode[NestedLessLess]..},
			CallNode[ToNode[NestedLessLess], rands, data]
		,
		{ToNode[NotGreater]..},
			CallNode[ToNode[NotGreater], rands, data]
		,
		{ToNode[NotGreaterEqual]..},
			CallNode[ToNode[NotGreaterEqual], rands, data]
		,
		{ToNode[NotGreaterFullEqual]..},
			CallNode[ToNode[NotGreaterFullEqual], rands, data]
		,
		{ToNode[NotGreaterGreater]..},
			CallNode[ToNode[NotGreaterGreater], rands, data]
		,
		{ToNode[NotGreaterLess]..},
			CallNode[ToNode[NotGreaterLess], rands, data]
		,
		{ToNode[NotGreaterSlantEqual]..},
			CallNode[ToNode[NotGreaterSlantEqual], rands, data]
		,
		{ToNode[NotGreaterTilde]..},
			CallNode[ToNode[NotGreaterTilde], rands, data]
		,
		{ToNode[NotLess]..},
			CallNode[ToNode[NotLess], rands, data]
		,
		{ToNode[NotLessEqual]..},
			CallNode[ToNode[NotLessEqual], rands, data]
		,
		{ToNode[NotLessFullEqual]..},
			CallNode[ToNode[NotLessFullEqual], rands, data]
		,
		{ToNode[NotLessGreater]..},
			CallNode[ToNode[NotLessGreater], rands, data]
		,
		{ToNode[NotLessLess]..},
			CallNode[ToNode[NotLessLess], rands, data]
		,
		{ToNode[NotLessSlantEqual]..},
			CallNode[ToNode[NotLessSlantEqual], rands, data]
		,
		{ToNode[NotLessTilde]..},
			CallNode[ToNode[NotLessTilde], rands, data]
		,
		{ToNode[NotNestedGreaterGreater]..},
			CallNode[ToNode[NotNestedGreaterGreater], rands, data]
		,
		{ToNode[NotNestedLessLess]..},
			CallNode[ToNode[NotNestedLessLess], rands, data]
		,
		_,
			CallNode[ToNode[Inequality], Riffle[rands, rators], data]
	]
]


inequalityOperatorToSymbol[LeafNode[Token`EqualEqual | Token`LongName`Equal | Token`LongName`LongEqual, _, _]] := ToNode[Equal]
inequalityOperatorToSymbol[LeafNode[Token`BangEqual | Token`LongName`NotEqual, _, _]] := ToNode[Unequal]
inequalityOperatorToSymbol[LeafNode[Token`Less, _, _]] := ToNode[Less]
inequalityOperatorToSymbol[LeafNode[Token`Greater, _, _]] := ToNode[Greater]
inequalityOperatorToSymbol[LeafNode[Token`LessEqual | Token`LongName`LessEqual, _, _]] := ToNode[LessEqual]
inequalityOperatorToSymbol[LeafNode[Token`GreaterEqual | Token`LongName`GreaterEqual, _, _]] := ToNode[GreaterEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterEqualLess, _, _]] := ToNode[GreaterEqualLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterFullEqual, _, _]] := ToNode[GreaterFullEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterGreater, _, _]] := ToNode[GreaterGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterLess, _, _]] := ToNode[GreaterLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterSlantEqual, _, _]] := ToNode[GreaterEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterTilde, _, _]] := ToNode[GreaterTilde]
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessEqualGreater, _, _]] := ToNode[LessEqualGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessFullEqual, _, _]] := ToNode[LessFullEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessGreater, _, _]] := ToNode[LessGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessLess, _, _]] := ToNode[LessLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessSlantEqual, _, _]] := ToNode[LessEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessTilde, _, _]] := ToNode[LessTilde]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NestedGreaterGreater, _, _]] := ToNode[NestedGreaterGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NestedLessLess, _, _]] := ToNode[NestedLessLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreater, _, _]] := ToNode[NotGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterEqual, _, _]] := ToNode[NotGreaterEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterFullEqual, _, _]] := ToNode[NotGreaterFullEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterGreater, _, _]] := ToNode[NotGreaterGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterLess, _, _]] := ToNode[NotGreaterLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterSlantEqual, _, _]] := ToNode[NotGreaterSlantEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterTilde, _, _]] := ToNode[NotGreaterTilde]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLess, _, _]] := ToNode[NotLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessEqual, _, _]] := ToNode[NotLessEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessFullEqual, _, _]] := ToNode[NotLessFullEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessGreater, _, _]] := ToNode[NotLessGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessLess, _, _]] := ToNode[NotLessLess]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessSlantEqual, _, _]] := ToNode[NotLessSlantEqual]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessTilde, _, _]] := ToNode[NotLessTilde]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotNestedGreaterGreater, _, _]] := ToNode[NotNestedGreaterGreater]
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotNestedLessLess, _, _]] := ToNode[NotNestedLessLess]


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

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->groupData[Source], ConfidenceLevel -> 1.0|>]];

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

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->groupData[Source], ConfidenceLevel -> 1.0|>]];

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

    AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Error", <|Source->groupData[Source], ConfidenceLevel -> 1.0|>]];

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

    AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Error", <|Source->groupData[Source], ConfidenceLevel -> 1.0|>]];

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
abstractMessageName[InfixNode[MessageName, {left_, rest__}, dataIn_]] :=
Module[{data, issues},
	
	data = dataIn;

	issues = {};

	(*
	a::b::c::d
	*)
	If[Length[{rest}] > 2,
		AppendTo[issues, SyntaxIssue["SyntaxUndocumentedMessageName", "This syntax is not documented.", "Error", <|Source->data[Source], ConfidenceLevel -> 1.0|>]];
	];
	
	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];

	CallNode[ToNode[MessageName], {abstract[left]} ~Join~ (abstractMessageNameChild /@ {rest}), data]
]


abstractMessageNameChild[LeafNode[String, str_, _]] := ToNode[abstractString[str]]

abstractMessageNameChild[LeafNode[Token`Error`EmptyString, str_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`EmptyString, { LeafNode[Token`Error`EmptyString, str, data] }, data]

abstractMessageNameChild[LeafNode[Token`Error`ExpectedLetterlike, str_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedLetterlike, { LeafNode[Token`Error`ExpectedOperand, str, data] }, data]







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
abstractGroupNode[GroupNode[tag_, children_, dataIn_]] :=
Module[{abstractedChildren, issues, data},
	
	data = dataIn;

	issues = {};

	abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];

	CallNode[ToNode[tag], abstractedChildren, data]
]

selectChildren[CallNode[ToNode[Comma], children_, _]] := children

selectChildren[n_] := n







(*
FIXME: would be good to remember tag somehow
*)

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
Module[{head, data, part, innerData, outerData, issues, partData, src},

	head = headIn;
	data = dataIn;
	part = inner;
	innerData = inner[[3]];
	outerData = outer[[3]];
	issues = {};

	Switch[head,
			(*
			feel strongly about ##2[[arg]]
			##2 represents a sequence of arguments, so it is wrong to call
			*)
			LeafNode[SlotSequence, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Unexpected ``Part`` call.", "Error", <|Source->data[Source], ConfidenceLevel -> 1.0|>]];
			,
			LeafNode[Symbol | Slot | Blank | BlankSequence | BlankNullSequence, _, _] (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
				_PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode (*| _SlotSequenceNode *),
			(* these are fine *)
			Null
			,
			LeafNode[Out, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			PrefixNode[PrefixLinearSyntaxBang, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			GroupNode[_, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Error", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
	];

	head = abstract[head];
	part = abstractGroupNode[part];
	partData = part[[3]];

	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	(*
	Only warn if LineCol style
	*)
	If[MatchQ[outerData[[ Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],

		If[outerData[[ Key[Source], 1, 2]]+1 != innerData[[ Key[Source], 1, 2]],
			src = {outerData[[ Key[Source], 1]], innerData[[ Key[Source], 1]]};
			AppendTo[issues, FormatIssue["NotContiguous", "``Part`` brackets ``[[`` are not contiguous.", "Formatting",
										<|	Source->src,
											CodeActions->{CodeAction["DeleteTrivia", DeleteTrivia,
																<|Source->src|>]},
											Format`AirynessLevel -> 0.0|>]];
		];

		If[innerData[[ Key[Source], 2, 2]]+1 != outerData[[ Key[Source], 2, 2]],
			src = {innerData[[ Key[Source], 2]], outerData[[ Key[Source], 2]]};
			AppendTo[issues, FormatIssue["NotContiguous", "``Part`` brackets ``]]`` are not contiguous.", "Formatting",
										<|	Source->src,
											CodeActions->{CodeAction["DeleteTrivia", DeleteTrivia, 
																<|Source->src|>]},
											Format`AirynessLevel -> 0.0|>]];
		];
	];

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];

	CallNode[ToNode[Part], {head} ~Join~ part[[2]], data]
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
			AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Unexpected call.", "Error", <|Source->data[Source], ConfidenceLevel -> 1.0|>]];
			,
			LeafNode[Symbol | String | Slot | Blank | BlankSequence | BlankNullSequence, _, _] | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*)|
			   _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode (*| _SlotSequenceNode*),
			(* these are fine *)
			Null
			,
			LeafNode[Out, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			PostfixNode[Function | Derivative, _, _],
			(* these are fine *)
			Null
			,
			_,
			(*
			warn about anything else
			*)
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Unexpected call.", "Error", <|Source->data[Source], ConfidenceLevel -> 1.0|>]];
			,
			LeafNode[Symbol | Slot | Blank | BlankSequence | BlankNullSequence, _, _] (* |_StringNode*) | _CallNode | _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode (*| _OptionalDefaultNode*) |
				_PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode (*| _SlotSequenceNode *),
			(* these are fine *)
			Null
			,
			LeafNode[Out, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			PrefixNode[PrefixLinearSyntaxBang, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Remark", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Remark", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			GroupNode[_, _, _],
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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

abstract[n:CodeNode[_, _, _]] := n

(*
a is a List of boxes
*)
abstract[BoxNode[RowBox, {a_}, data_]] := BoxNode[RowBox, {abstract /@ a}, KeyTake[data, keysToTake]]

abstract[BoxNode[b_, children_, data_]] := BoxNode[b, abstract /@ children, KeyTake[data, keysToTake]]





abstractNot2[rand_, notNotTok_, dataIn_] :=
Module[{notNotData},
	
	notNotData = notNotTok[[3]];

	data = dataIn;

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	AppendTo[issues, SyntaxIssue["PrefixNotNot", "Unexpected parse.", "Warning", <|Source->notNotData[Source], ConfidenceLevel -> 1.0|>]];

	AssociateTo[data, AbstractSyntaxIssues -> issues];

	CallNode[LeafNode[Symbol, "Not", <||>], {
		CallNode[LeafNode[Symbol, "Not", <||>], {
			abstract[rand]}, <||>]}, data]
]



End[]

EndPackage[]
