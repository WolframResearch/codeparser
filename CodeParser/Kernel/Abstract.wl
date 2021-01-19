BeginPackage["CodeParser`Abstract`"]

Aggregate

Abstract



$AggregateParseProgress

$AbstractParseProgress


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeParser`Folds`"]
Needs["CodeParser`Quirks`"]
Needs["CodeParser`Shims`"]








Aggregate::usage = "Aggregate[cst] returns an aggregate syntax tree from a concrete syntax tree."

Aggregate[cst_] :=
Block[{$RecursionLimit = Infinity},
Module[{agg},

	CodeParser`Abstract`$AggregateParseStart = Now;
	CodeParser`Abstract`$AggregateParseTime = Quantity[0, "Seconds"];

	CodeParser`Abstract`$AggregateParseProgress = 5;

	agg = aggregate[cst];

	CodeParser`Abstract`$AggregateParseProgress = 100;
	CodeParser`Abstract`$AggregateParseTime = Now - CodeParser`Abstract`$AggregateParseStart;

	agg
]]





Abstract::usage = "Abstract[agg] returns an abstract syntax tree from an aggregate syntax tree."

Abstract[agg_] :=
Block[{$RecursionLimit = Infinity},
Module[{ast},

	CodeParser`Abstract`$AbstractParseStart = Now;
	CodeParser`Abstract`$AbstractParseTime = Quantity[0, "Seconds"];

	CodeParser`Abstract`$AbstractParseProgress = 5;

	ast = agg;

	(*
	There is some surgery involved with gluing "-" onto numbers and this can screw up removing of line continuations.

	So make sure to normalize tokens BEFORE doing the abstract fold
	*)
	ast = normalizeTokens[ast];

	ast = abstract[ast];

	CodeParser`Abstract`$AbstractParseProgress = 100;

	CodeParser`Abstract`$AbstractParseTime = Now - CodeParser`Abstract`$AbstractParseStart;

	ast
]]


abstract[LeafNode[Token`Under, _, data_]] := CallNode[ToNode[Blank], {}, data]
abstract[LeafNode[Token`UnderUnder, _, data_]] := CallNode[ToNode[BlankSequence], {}, data]
abstract[LeafNode[Token`UnderUnderUnder, _, data_]] := CallNode[ToNode[BlankNullSequence], {}, data]
abstract[LeafNode[Token`UnderDot, _, data_]] := CallNode[ToNode[Optional], { CallNode[ToNode[Blank], {}, data] }, data]


abstract[LeafNode[Token`Hash, _, data_]] := CallNode[ToNode[Slot], { ToNode[1] }, data]
abstract[LeafNode[Token`HashHash, _, data_]] := CallNode[ToNode[SlotSequence], { ToNode[1] }, data]


abstract[LeafNode[Token`Percent, _, data_]] := CallNode[ToNode[Out], {}, data]

abstract[LeafNode[Token`PercentPercent, s_, data_]] :=
Module[{count},

	count = StringCount[s, "%" | "\\.25" | "\\:0025" | "\\|000025" | "\\045" | "\\[RawPercent]"];

	CallNode[ToNode[Out], { ToNode[-count] }, data]
]

(*
Token`Fake`ImplicitNull does NOT get abstracted because it is handled at its possible parents: Comma and CompoundExpression
*)

abstract[LeafNode[Token`Fake`ImplicitOne, _, data_]] := LeafNode[Integer, "1", data]

abstract[LeafNode[Token`Fake`ImplicitAll, _, data_]] := LeafNode[Symbol, "All", data]

(*
Symbols, Strings, Integers, Reals, and Rationals just get passed through

Also, LinearSyntaxBlob just gets passed through
*)
abstract[leaf_LeafNode] :=
	leaf

abstract[n_ErrorNode] :=
	n



abstract[CompoundNode[Blank, {_, sym2_}, data_]] := CallNode[ToNode[Blank], {abstract[sym2]}, data]
abstract[CompoundNode[BlankSequence, {_, sym2_}, data_]] := CallNode[ToNode[BlankSequence], {abstract[sym2]}, data]
abstract[CompoundNode[BlankNullSequence, {_, sym2_}, data_]] := CallNode[ToNode[BlankNullSequence], {abstract[sym2]}, data]


abstract[CompoundNode[PatternBlank, {sym1_, blank_}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], abstract[blank]}, data]
abstract[CompoundNode[PatternBlankSequence, {sym1_, blankSeq_}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], abstract[blankSeq]}, data]
abstract[CompoundNode[PatternBlankNullSequence, {sym1_, blankNullSeq_}, data_]] := CallNode[ToNode[Pattern], {abstract[sym1], abstract[blankNullSeq]}, data]
abstract[CompoundNode[PatternOptionalDefault, {sym1_, LeafNode[Token`UnderDot, _, optionalDefaultData_]}, data_]] := CallNode[ToNode[Optional], { CallNode[ToNode[Pattern], {abstract[sym1], CallNode[ToNode[Blank], {}, optionalDefaultData]}, data] }, data]


abstract[CompoundNode[Slot, {_, arg:LeafNode[Integer, _, data1_]}, data_]] := CallNode[ToNode[Slot], {abstract[arg]}, data]
abstract[CompoundNode[Slot, {_, arg:LeafNode[Symbol, s_, data1_]}, data_]] := CallNode[ToNode[Slot], {LeafNode[String, escapeString[abstractSymbolString[s]], data1]}, data]
abstract[CompoundNode[Slot, {_, arg:LeafNode[String, s_, data1_]}, data_]] := CallNode[ToNode[Slot], {LeafNode[String, escapeString[abstractSymbolString[s]], data1]}, data]


abstract[CompoundNode[SlotSequence, {_, arg:LeafNode[Integer, _, _]}, data_]] := CallNode[ToNode[SlotSequence], {abstract[arg]}, data]


abstract[CompoundNode[Out, {_, arg:LeafNode[Integer, _, _]}, data_]] := CallNode[ToNode[Out], {abstract[arg]}, data]


abstract[PrefixNode[Minus, {_, rand_}, data_]] := abstract[negate[rand, data]]

abstract[PrefixNode[Plus, {_, rand_}, data_]] := abstractPrefixPlus[rand, data]

abstract[PrefixNode[PrefixNot2, {notNotTok_, rand_}, data_]] := abstractNot2[rand, notNotTok, data]

(*
FIXME: keep linear syntax for now
*)
abstract[PrefixNode[PrefixLinearSyntaxBang, {rator_, rand:LeafNode[Token`LinearSyntaxBlob, _, _]}, data_]] :=
	PrefixNode[PrefixLinearSyntaxBang, {rator, abstract[rand]}, data]

abstract[PrefixNode[PrefixLinearSyntaxBang, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`LinearSyntaxBang, children, data]

(*
strings may be quoted

concrete syntax: <<a
abstract syntax Get["a"]

concrete syntax: <<"a"
abstract syntax Get["a"]
*)
abstract[PrefixNode[Get, {_, LeafNode[String, str_, data1_]}, data_]] := CallNode[ToNode[Get], {LeafNode[String, escapeString[abstractFileString[str]], data1]}, data]

abstract[PrefixNode[op_, {_, operand_}, data_]] := CallNode[ToNode[op], {abstract[operand]}, data]


abstract[PostfixNode[System`HermitianConjugate, {rand_, _}, data_]] := CallNode[ToNode[ConjugateTranspose], {abstract[rand]}, data]

abstract[PostfixNode[Derivative, {operand_, rator_}, data_]] := abstractDerivative[PostfixNode[Derivative, {operand, rator}, data]]

abstract[PostfixNode[op_, {operand_, _}, data_]] := CallNode[ToNode[op], {abstract[operand]}, data]







abstract[BinaryNode[Divide, { left_, _, right_ }, data_]] := abstractTimes[BinaryNode[Divide, {left, right}, data]]

abstract[BinaryNode[BinaryAt, {left_, _, right_}, data_]] := CallNode[abstract[left], {abstract[right]}, data]

abstract[BinaryNode[BinaryAtAtAt, {left_, _, right_}, data_]] := CallNode[ToNode[Apply], abstract /@ {left, right, GroupNode[List, { LeafNode[Token`OpenCurly, "{", <||>], ToNode[1], LeafNode[Token`CloseCurly, "}", <||>] }, <||>]}, data]

(*
Make sure to reverse the arguments
*)
abstract[BinaryNode[BinarySlashSlash, {left_, _, right_}, data_]] := CallNode[abstract[right], {abstract[left]}, data]

abstract[BinaryNode[Put, {left_, _, LeafNode[String, str_, data1_]}, data_]] := CallNode[ToNode[Put], {abstract[left], LeafNode[String, escapeString[abstractFileString[str]], data1]}, data]
abstract[BinaryNode[PutAppend, {left_, _, LeafNode[String, str_, data1_]}, data_]] := CallNode[ToNode[PutAppend], {abstract[left], LeafNode[String, escapeString[abstractFileString[str]], data1]}, data]


(*
First arg must be a symbol
*)
abstract[BinaryNode[Pattern, {left:LeafNode[Symbol, _, _], _, right_}, data_]] :=
	CallNode[ToNode[Pattern], {abstract[left], abstract[right]}, data]

abstract[BinaryNode[Pattern, {left_, _, right_}, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`PatternColonError, {abstract[left], abstract[right]}, data]

(*
Abstract NonAssociative errors

a ? b ? c being NonAssociative is alluded to being a bug in bug report 206938
Related bugs: 206938
*)
abstract[BinaryNode[PatternTest, {left:BinaryNode[PatternTest, _, _], _, right_}, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociativePatternTest, {abstract[left], abstract[right]}, data]

abstract[BinaryNode[Unset, {left_, _, _}, data_]] := CallNode[ToNode[Unset], {abstract[left]}, data]

abstract[BinaryNode[op_, {left_, _, right_}, data_]] := CallNode[ToNode[op], {abstract[left], abstract[right]}, data]




abstract[node:InfixNode[InfixInequality, children_, data_]] := abstractInfixInequality[node]

(*
Handle SameQ and UnsameQ specially because they do not participate in the InfixBinaryAt quirk
*)
abstract[InfixNode[op:SameQ | UnsameQ, children_ /; OddQ[Length[children]], data_]] := CallNode[ToNode[op], abstract /@ children[[;;;;2]], data]

(*
Do not do children[[;;;;2]]
need to remember whether Token`Plus or Token`Minus
*)
abstract[InfixNode[Plus, children_, data_]] := abstractPlus[InfixNode[Plus, children, data]]

abstract[InfixNode[Times, children_, data_]] := abstractTimes[InfixNode[Times, children[[;;;;2]], data]]

abstract[InfixNode[Divisible, children_, data_]] := abstractDivisible[InfixNode[Divisible, children[[;;;;2]], data]]

abstract[InfixNode[Comma, children_, data_]] := abstractComma[InfixNode[Comma, children[[;;;;2]], data]]

abstract[InfixNode[CompoundExpression, children_, data_]] := abstractCompoundExpression[InfixNode[CompoundExpression, children[[;;;;2]], data]]

abstract[InfixNode[MessageName, children_, data_]] := abstractMessageName[InfixNode[MessageName, children[[;;;;2]], data]]

abstract[InfixNode[op_, children_ /; OddQ[Length[children]], data_]] :=
	CallNode[ToNode[op], abstract /@ (processInfixBinaryAtQuirk[#, ToString[op]]& /@ children[[;;;;2]]), data]



(*
all TernaryNodes must be handled separately
*)

(*
handle  a ~f,~ b

Cannot have  (f,)[a, b]
*)
abstract[TernaryNode[TernaryTilde, {left_, _, middle:InfixNode[Comma, _, _], _, right_}, data_]] :=
	With[{abstractedMiddle = abstract[middle]},
		CallNode[AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, abstractedMiddle[[2]], abstractedMiddle[[3]]], {
			abstract[left], abstract[right]}, data]
	]
abstract[TernaryNode[TernaryTilde, {left_, _, middle_, _, right_}, data_]] :=
	CallNode[abstract[middle], {abstract[left], abstract[right]}, data]

abstract[TernaryNode[TagSet, {left_, _, middle_, _, right_}, data_]] :=
	CallNode[ToNode[TagSet], {abstract[left], abstract[middle], abstract[right]}, data]
abstract[TernaryNode[TagSetDelayed, {left_, _, middle_, _, right_}, data_]] :=
	CallNode[ToNode[TagSetDelayed], {abstract[left], abstract[middle], abstract[right]}, data]

abstract[TernaryNode[TagUnset, {left_, _, middle_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]] :=
	CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, data]

abstract[TernaryNode[Span, {left_, _, middle_, _, right_}, data_]] :=
	CallNode[ToNode[Span], {abstract[left], abstract[middle], abstract[right]}, data]






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
	abstractCallNode[CallNode[op, { GroupNode[GroupSquare, { GroupNode[GroupSquare, { inner }, data3] }, data2] }, data1]]

abstract[CallNode[op_, { GroupNode[GroupSquare, { _, inner___, _ }, data2_] }, data1_]] :=
	abstractCallNode[CallNode[op, { GroupNode[GroupSquare, { inner }, data2] }, data1]]

abstract[CallNode[op_, { GroupNode[GroupDoubleBracket, { _, inner___, _ }, data2_] }, data1_]] :=
	abstractCallNode[CallNode[op, { GroupNode[GroupDoubleBracket, { inner }, data2] }, data1]]


(*

We need special node CallMissingCloserNode because it used to be the case that both

{[a}

and

List[a

had the same AST:

CallNode[LeafNode[Symbol, "List", <||>], {
	GroupMissingCloserNode[GroupSquare, {
		LeafNode[Token`OpenSquare, "[", <||>],
		LeafNode[Symbol, "a", <||>]}, <||>]}, <||>]

we need to distinguish these cases, so it makes sense to have special node to say

"this is a CallNode, but with the closer missing"


GroupMissingCloserNode gets abstracted

strip off opener
*)
abstract[CallNode[head_, {GroupMissingCloserNode[GroupSquare, {_, inner___}, _]}, data_]] :=
	abstractCallNode[CallMissingCloserNode[head, { GroupMissingCloserNode[GroupSquare, { inner }, data] }, data]]

(*
UnterminatedGroupNode does NOT get abstracted

leave all concrete syntax in tact
*)
abstract[CallNode[head_, {UnterminatedGroupNode[GroupSquare, children_, _]}, data_]] :=
	abstractCallNode[UnterminatedCallNode[head, { UnterminatedGroupNode[GroupSquare, children, data] }, data]]




(*
take care of specific GroupNodes before calling abstractGroupNode
*)

(*
GroupParen
*)

abstract[GroupNode[GroupParen, { _, child:InfixNode[Comma, _, _], _ }, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, { child }, data]

abstract[GroupNode[GroupParen, { _, child_, _}, data_]] :=
	abstract[child]

abstract[GroupNode[GroupParen, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, children[[2;;-2]], data]

(* GroupNode errors *)
abstract[GroupNode[GroupSquare, children_, data_]] :=
	AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, children, data]



(*
Missing closers
*)

abstract[GroupMissingCloserNode[tag_, {_, inner___}, data_]] :=
	abstractGroupNode[GroupMissingCloserNode[tag, { inner }, data]]



abstract[n:UnterminatedGroupNode[_, _, _]] :=
	n



abstract[GroupNode[tag_, {_, inner___, _}, data_]] :=
	abstractGroupNode[GroupNode[tag, { inner }, data]]




abstract[PrefixBinaryNode[Integrate, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[Integrate], {abstract[operand1], abstract[var]}, data]

abstract[PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]] := CallNode[ToNode[op], {abstract[operand1], abstract[operand2]}, data]




abstract[ContainerNode[tag_, childrenIn_, dataIn_]] :=
Catch[
Module[{abstracted, issues, issues1, issues2, data, abstractedChildren, node, reportIssues, children},

	children = childrenIn;

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

	node = ContainerNode[tag, abstracted, data];

	node
]]



matchingOperatorPatterns[CallNode[LeafNode[Symbol, "EndPackage", _], {}, _]] = _PackageNode
matchingOperatorPatterns[CallNode[LeafNode[Symbol, "End", _], {}, _]] = _ContextNode
matchingOperatorPatterns[CallNode[LeafNode[Symbol, "System`Private`RestoreContextPath", _], {}, _]] = _NewContextPathNode



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
												SubtractFrom | Unset | UpSet | UpSetDelayed |
												Put | PutAppend, _, _], True] := {}

(*
Side-effecting ternary operators
*)
topLevelChildIssues[TernaryNode[TagSet | TagSetDelayed | TagUnset, _, _], True] := {}

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

symbolDeclPat = LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _]

topLevelChildIssues[GroupNode[List, {
	LeafNode[Token`OpenCurly, _, _],
	symbolDeclPat,
	LeafNode[Token`CloseCurly, _, _] }, _], True] := {}

topLevelChildIssues[GroupNode[List, {
	LeafNode[Token`OpenCurly, _, _],
	InfixNode[Comma | CompoundExpression, {
		PatternSequence[symbolDeclPat, _]..., LeafNode[Token`Fake`ImplicitNull, _, _]}, _],
	LeafNode[Token`CloseCurly, _, _] }, _], True] := {}

topLevelChildIssues[GroupNode[List, {
	LeafNode[Token`OpenCurly, _, _],
	InfixNode[Comma | CompoundExpression, {
		PatternSequence[symbolDeclPat, _]..., symbolDeclPat }, _],
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
													SubtractFrom | Unset | UpSet | UpSetDelayed |
													Put | PutAppend, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												TernaryNode[TagSet | TagSetDelayed | TernaryTilde, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PrefixNode[Get | PreDecrement | PreIncrement, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												PostfixNode[Decrement | Increment, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[List, {
												LeafNode[Token`OpenCurly, _, _], symbolDeclPat,
												LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[List, {
												LeafNode[Token`OpenCurly, _, _],
												InfixNode[Comma | CompoundExpression, { PatternSequence[symbolDeclPat, _]..., LeafNode[Token`Fake`ImplicitNull, _, _] }, _],
												LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }, _], True] := {}

topLevelChildIssues[InfixNode[CompoundExpression, {
												GroupNode[List, {
												LeafNode[Token`OpenCurly, _, _],
												InfixNode[Comma | CompoundExpression, { PatternSequence[symbolDeclPat, _]..., symbolDeclPat }, _],
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


topLevelChildIssues[InfixNode[CompoundExpression, {BinaryNode[Set | SetDelayed, _, _], LeafNode[Token`Semi, _, _], end:_[Except[Token`Fake`ImplicitNull], _, _], ___}, data_], ignored_] := {
	SyntaxIssue["TopLevel", "Definition does not contain the rest of the ``CompoundExpression``.", "Error",
		<| Source -> firstExplicitToken[end][[3, Key[Source]]],
			ConfidenceLevel -> 0.95
			(*FIXME: wrap parentheses CodeAction*) |>] }

topLevelChildIssues[InfixNode[CompoundExpression, {_, LeafNode[Token`Semi, _, data1_], LeafNode[Token`Fake`ImplicitNull, _, _]}, data_], ignored_] := {
	SyntaxIssue["TopLevel", "``CompoundExpression`` at top-level. ``;`` may not be needed at top-level.", "Warning",
		<| Source -> data1[Source],
			ConfidenceLevel -> 0.75
			(*FIXME: insert newline CodeAction*) |>] }

topLevelChildIssues[InfixNode[CompoundExpression, {_, LeafNode[Token`Semi, _, data1_], _, ___}, data_], ignored_] := {
	SyntaxIssue["TopLevel", "``CompoundExpression`` at top-level. Consider breaking up onto separate lines.", "Warning",
		<| Source -> data1[Source],
			ConfidenceLevel -> 0.75
			(*FIXME: insert newline CodeAction*) |>] }

(*
Anything else, then warn

Specifically add a DidYouMean for / -> /@
*)
topLevelChildIssues[BinaryNode[Divide, {_, LeafNode[Token`Slash, _, slashData_], _}, data_], True] := {
	SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning",
		<| Source -> slashData[Source],
			ConfidenceLevel -> 0.95,
			CodeActions -> { CodeAction["Replace ``/`` with ``/@``", ReplaceNode,
									<|	Source->slashData[Source],
										"ReplacementNode"->LeafNode[Token`SlashAt, "/@", <||>] |>] } |>] }

(*
No need to issue warning for errors being strange
*)
topLevelChildIssues[ErrorNode[_, _, _], True] := {}

topLevelChildIssues[SyntaxErrorNode[_, _, _], True] := {}

topLevelChildIssues[AbstractSyntaxErrorNode[_, _, _], True] := {}

topLevelChildIssues[GroupMissingCloserNode[_, _, _], True] := {}


topLevelChildIssues[node:_[_, _, _], True] :=
Module[{first, firstSrc, issues},

	issues = {};

	(*
	Just grab the first token to use
	*)
	first = firstExplicitToken[node];
	firstSrc = first[[3, Key[Source]]];

	Switch[first[[1]],
		Token`OpenCurly,
			AppendTo[issues, SyntaxIssue["TopLevelList", "Unexpected list at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.75 |>]]
		,
		Token`LessBar,
			AppendTo[issues, SyntaxIssue["TopLevelAssociation", "Unexpected association at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.75 |>]]
		,
		String,
			AppendTo[issues, SyntaxIssue["TopLevelString", "Unexpected string at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.75 |>]]
		,
		_,
			AppendTo[issues, SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.95 |>]]
	];

	issues
]



firstExplicitToken[node:_[_, _String, _]] := node
firstExplicitToken[CallNode[first_, ___, _, _]] := firstExplicitToken[first]
firstExplicitToken[_[_, {}, _]] := Failure["CannotFindFirstExplicitToken", <||>]
firstExplicitToken[_[_, ts_List, _]] :=
	Catch[
	Module[{explicit},
		explicit = DeleteCases[ts, LeafNode[Token`Fake`ImplicitOne, _, _]];
		If[explicit == {},
			Throw[Failure["CannotFindFirstExplicitToken", <||>]]
		];

		firstExplicitToken[explicit[[1]]]
	]]



(*

input: a list of top-level nodes

returns: {abstracted top-level nodes, any AbstractSyntaxErrors that occurred}

*)
abstractTopLevel[listIn_] :=
Catch[
Module[{list, nodeListStack , currentList, operatorStack, currentOperator, x, issues, nodeList, peek, error, def},
	
	list = listIn;

	nodeListStack = System`CreateDataStructure["Stack"];
	operatorStack = System`CreateDataStructure["Stack"];

	nodeListStack["Push", System`CreateDataStructure["Stack"]];
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
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		BeginPackage["Foo`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", PackageNode[x[[2, 1, 2]], {}, <|Source->{x[[2, 1, 3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		Begin["`Private`"]
		*)
		CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _],
			operatorStack["Push", ContextNode[x[[2]], {}, <|Source->{x[[3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		Begin["`Private`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", ContextNode[x[[2, 1, 2]], {}, <|Source->{x[[2, 1, 3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		System`Private`NewContextPath[{"Foo`"}]
		*)
		CallNode[LeafNode[Symbol, "System`Private`NewContextPath", _], { CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] }, _],
			operatorStack["Push", NewContextPathNode[x[[2]], {}, <|Source->{x[[3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		System`Private`NewContextPath[{"Foo`"}] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "System`Private`NewContextPath", _], { CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] }, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", NewContextPathNode[x[[2, 1, 2]], {}, <|Source->{x[[2, 1, 3, Key[Source], 1]], (*partially constructed Source*)Indeterminate}|>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		EndPackage[]
		End[]
		*)
		CallNode[LeafNode[Symbol, "EndPackage" | "End" | "System`Private`RestoreContextPath", _], {}, _],
			currentOperator = operatorStack["Pop"];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> x[[3, Key[Source]]], ConfidenceLevel -> 1.0 |> ]];
				Throw[{list, issues}];
			];
			currentList = nodeListStack["Pop"];
			currentOperator[[2]] = Normal[currentList];
			(* finish constructing Source *)
			currentOperator[[3, Key[Source], 2]] = x[[3, Key[Source], 2]];
			peek = nodeListStack["Peek"];
			peek["Push", currentOperator];
		,
		(*
		EndPackage[] ;
		End[] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "EndPackage" | "End" | "System`Private`RestoreContextPath", _], {}, _], LeafNode[Symbol, "Null", _]}, _],
			currentOperator = operatorStack["Pop"];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x[[2, 1]]]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> x[[2, 1, 3, Key[Source]]], ConfidenceLevel -> 1.0 |>]];
				Throw[{list, issues}];
			];
			currentList = nodeListStack["Pop"];
			currentOperator[[2]] = Normal[currentList];
			(* finish constructing Source *)
			currentOperator[[3, Key[Source], 2]] = x[[2, 1, 3, Key[Source], 2]];
			peek = nodeListStack["Peek"];
			peek["Push", currentOperator];
		,
		(*
		All other calls to recognized directives

		GroupMissingCloserNode
		*)
		CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], { GroupMissingCloserNode[_, _, _] }, _],
			(*
			if GroupMissingCloserNode, then do not complain
			*)
			Throw[{list, issues}];
		,
		CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], _, _],
			AppendTo[issues, SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <| Source -> x[[3, Key[Source]]], ConfidenceLevel -> 1.0 |> ]];
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;

		GroupMissingCloserNode
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], { GroupMissingCloserNode[_, _, _] }, _], LeafNode[Symbol, "Null", _]}, _],
			(*
			if GroupMissingCloserNode, then do not complain
			*)
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], _, _], LeafNode[Symbol, "Null", _]}, _],
			AppendTo[issues, SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <| Source -> x[[2, 1, 3, Key[Source]]], ConfidenceLevel -> 1.0 |>]];
			Throw[{list, issues}];
		,
		(*
		a,b  at top-level is an error
		*)
		CallNode[LeafNode[Symbol, "CodeParser`Comma", _], _, _],
			peek = nodeListStack["Peek"];
			error = AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, x[[2]], x[[3]]];
			peek["Push", error];
		,
		(*
		foo[] := 1+1  at top-level

		insert "Definition" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs_ /; !FailureQ[DeclarationName[lhs]], _}, _],
			peek = nodeListStack["Peek"];
			def = CallNode[LeafNode[Symbol, x[[1, 2]], x[[1, 3]]], x[[2]], <| x[[3]], "Definition" -> DeclarationName[x[[2, 1]]] |> ];
			peek["Push", def];
		,
		(*
		foo[] := 1+1  at top-level ;

		insert "Definition" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], { CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs_ /; !FailureQ[DeclarationName[lhs]], _}, _], LeafNode[Symbol, "Null", _] }, _],
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], { CallNode[LeafNode[Symbol, x[[2, 1, 1, 2]], x[[2, 1, 1, 3]]], x[[2, 1, 2]], <| x[[2, 1, 3]], "Definition" -> DeclarationName[x[[2, 1, 2, 1]]] |> ], x[[2, 2]] }, x[[3]]];
			peek["Push", def];
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
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 1.0 |>]];
		Throw[{list, issues}];
	];
	If[operatorStack["Peek"] =!= None,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 1.0 |>]];
		Throw[{list, issues}];
	];
	If[nodeListStack["Length"] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 1.0 |>]];
		Throw[{list, issues}];
	];
	peek = nodeListStack["Peek"];
	nodeList = Normal[peek];

	cleanupStackShimMemoryLeak[];

	{nodeList, issues}
]]




(*

Match a string that is a context

tutorial/InputSyntax#6562

Symbol Names and Contexts

*)
contextQ[s_String] := StringMatchQ[s, RegularExpression["\"`?([a-zA-Z][a-zA-Z0-9]*`)+\""]]




abstract[SyntaxErrorNode[SyntaxError`ExpectedTilde, {left_, _, middle_}, data_]] :=
	SyntaxErrorNode[SyntaxError`ExpectedTilde, {abstract[left], abstract[middle]}, data]

abstract[SyntaxErrorNode[SyntaxError`ExpectedSet, {left_, _, middle_}, data_]] :=
	SyntaxErrorNode[SyntaxError`ExpectedSet, {abstract[left], abstract[middle]}, data]





abstract[f_Failure] := f

abstract[args___] := Failure["InternalUnhandled", <|"Function"->abstract, "Arguments"->HoldForm[{args}]|>]





(*

String "a" -> a
String a -> a

for handling the various stringification operators
#a
#"a"
a::b
a::"b"
*)
abstractSymbolString[str_String /; StringStartsQ[str, "\""]] :=
	Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
	
abstractSymbolString[str_String] :=
	Quiet[ToExpression["\""<>str<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

(*
a>>b
a>>"b"

The strings might be something like:
b\c => b\\c
b\f => b\\f

FIXME: once the semantics are completely understood, move this to library
*)
abstractFileString[str_String /; StringStartsQ[str, "\""]] :=
Module[{},
	Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
]

abstractFileString[str_String] :=
Module[{replaced},

	(*
	convert to the language that is understood by quoted strings, to be given to ToExpression
	*)
	replaced = StringReplace[str, {
			(*
			single character escapes
			*)
			"\\b" -> "\\\\b",
			"\\f" -> "\\\\f",
			"\\n" -> "\\\\n",
			"\\r" -> "\\\\r",
			"\\t" -> "\\\\t",
			(*
			and double quote
			*)
			"\"" -> "\\\"",
			(*
			and backslash
			*)
			"\\" -> "\\\\"
		}];

	Quiet[ToExpression["\""<>replaced<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
]









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
(*
negate[node:ErrorNode[Token`Error`ExpectedOperand, _, _], _] :=
	node
*)

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
(*
reciprocate[node:ErrorNode[Token`Error`ExpectedOperand, _, _], _] :=
	node
*)

reciprocate[node_, data_] :=
	CallNode[ToNode[Power], {
		GroupNode[GroupSquare, {
			LeafNode[Token`OpenSquare, "[", <||>],
			InfixNode[Comma, { node, LeafNode[Token`Comma, ",", <||>], ToNode[-1] }, <||>],
			LeafNode[Token`CloseSquare, "]", <||>] }, <||> ] }, data]



processPlusPair[{LeafNode[Token`Plus | Token`LongName`ImplicitPlus, _, _], rand_}] := rand

processPlusPair[{LeafNode[Token`Minus | Token`LongName`Minus, _, opData_], rand_}] :=
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
flattenPrefixPlus[PrefixNode[Plus, {_, rand_}, _]] := flattenPrefixPlus[rand]

flattenPrefixPlus[rand_] := rand



(*
abstract syntax of  +a + b - c \[ImplicitPlus] d  is a single Plus expression
except when it's not
Related bugs: 365287
TODO: add 365287 to kernel quirks mode
*)
abstractPlus[InfixNode[Plus, children_, data_]] :=
Module[{pairs, processedPairs, flattened, processed},

	pairs = Partition[children[[2;;]], 2];

	processedPairs = processPlusPair /@ pairs;

	flattened = flattenPrefixPlus /@ ( { children[[1]] } ~Join~ processedPairs);

	processed = processInfixBinaryAtQuirk[#, "Plus"]& /@ flattened;

	CallNode[ToNode[Plus], abstract /@ processed, data]
]


(*
+ +a  parses the same as  +a
is it a quirk that  + +a  is parsed as  +a  ?
The first + is eaten
TODO: add to kernel quirks mode
*)
abstractPrefixPlus[PrefixNode[Plus, {_, rand_}, _], data_] := abstractPrefixPlus[rand, data]

abstractPrefixPlus[rand_, data_] := CallNode[ToNode[Plus], {abstract[rand]}, data]



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
					{negate[#[[2, 2]], data]}
				,
				PrefixNode[Minus, { _, _?parenthesizedIntegerOrRealQ }, _],
					{negate[#[[2, 2]], data]}
				,
				PrefixNode[Minus, {_, _}, _],
					If[flattenTimesQuirk,
						(*
						it is possible to have nested prefix Minus, e.g., - - a
						so must call recursively into flattenTimes
						*)
						{ToNode[-1], flattenTimes[{#[[2, 2]]}, data]}
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
Module[{flattened, processed},

	flattened = Flatten[flattenTimes[children, data]];

	processed = processInfixBinaryAtQuirk[#, "Times"]& /@ flattened;

	CallNode[ToNode[Times], abstract /@ processed, data]
]

abstractTimes[BinaryNode[Divide, {left_, right_}, data_]] :=
	CallNode[ToNode[Times], abstract /@ Flatten[flattenTimes[{left, reciprocate[right, data]}, data]], data]




(*
attempt to simplify e.g. Inequality[a, Less, b, Less, c] to Less[a, b, c]

Also integrate the newer VectorInequality functionality
*)
abstractInfixInequality[InfixNode[InfixInequality, children_, data_]] :=
Module[{processed, first, rator, rand, affinity},

	first = children[[1]];
	first = abstract[first];

	processed = { first };

	(*
	affinity is purposely not True nor False when starting
	*)
	Do[
		rator = children[[i]];
		rand = children[[i + 1]];

		rator = inequalityOperatorToSymbol[rator];
		rand = abstract[rand];

		Which[
			vectorInequalityAffinity[rator] === False,
				If[affinity === True,
					(*
					affinity is True, so all operators up to now are 1 sub node
					*)
					processed = { simplifyInfixInequality[processed, affinity, data], ToNode[rator], rand };
					,
					processed = processed ~Join~ {ToNode[rator], rand}
				];
				(*
				affinity is definitely False now
				*)
				affinity = False;
			,
			vectorInequalityAffinity[rator] === True,
				If[affinity === False,
					(*
					affinity is True, so all operators up to now are 1 sub node
					*)
					processed = { simplifyInfixInequality[processed, affinity, data], ToNode[rator], rand }
					,
					processed = processed ~Join~ {ToNode[rator], rand}
				];
				(*
				affinity is definitely True now
				*)
				affinity = True;
			,
			True,
				processed = processed ~Join~ {ToNode[rator], rand}
		];

		,
		{i, 2, Length[children], 2}
	];

	simplifyInfixInequality[processed, affinity, data]
]


simplifyInfixInequality[processed_, affinity_, data_] :=
Module[{rators, rands},

	rators = processed[[2;;-2;;2]];
	rands = processed[[1;;-1;;2]];

	(*
	Try simple cases of all the same operator first
	*)
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
		{ToNode[System`VectorLess]..},
			(*
			Yes, make sure that it is VectorLess[{a, b, c}] and not VectorLess[a, b, c]
			*)
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
			Which[
				affinity === True,
					(*
					Anything containing a combination inequality and Vector inequality operators is abstracted to VectorInequality
					Related bugs: 385771
					*)
					CallNode[ToNode[Developer`VectorInequality], Riffle[rands, rators], data]
				,
				affinity === False,
					CallNode[ToNode[Inequality], Riffle[rands, rators], data]
				,
				True,
					CallNode[ToNode[Inequality], Riffle[rands, rators], data]
			]
	]
]





inequalityOperatorToSymbol[LeafNode[Token`EqualEqual | Token`LongName`Equal | Token`LongName`LongEqual, _, _]] := Equal
inequalityOperatorToSymbol[LeafNode[Token`BangEqual | Token`LongName`NotEqual, _, _]] := Unequal
inequalityOperatorToSymbol[LeafNode[Token`Less, _, _]] := Less
inequalityOperatorToSymbol[LeafNode[Token`Greater, _, _]] := Greater
inequalityOperatorToSymbol[LeafNode[Token`LessEqual | Token`LongName`LessEqual, _, _]] := LessEqual
inequalityOperatorToSymbol[LeafNode[Token`GreaterEqual | Token`LongName`GreaterEqual, _, _]] := GreaterEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterEqualLess, _, _]] := GreaterEqualLess
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterFullEqual, _, _]] := GreaterFullEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterGreater, _, _]] := GreaterGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterLess, _, _]] := GreaterLess
(*
GreaterSlantEqual parses to GreaterEqual
Related bugs: 78439
*)
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterSlantEqual, _, _]] := GreaterEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`GreaterTilde, _, _]] := GreaterTilde
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessEqualGreater, _, _]] := LessEqualGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessFullEqual, _, _]] := LessFullEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessGreater, _, _]] := LessGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessLess, _, _]] := LessLess
(*
LessSlantEqual parses to LessEqual
Related bugs: 78439
*)
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessSlantEqual, _, _]] := LessEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`LessTilde, _, _]] := LessTilde
inequalityOperatorToSymbol[LeafNode[Token`LongName`NestedGreaterGreater, _, _]] := NestedGreaterGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`NestedLessLess, _, _]] := NestedLessLess
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreater, _, _]] := NotGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterEqual, _, _]] := NotGreaterEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterFullEqual, _, _]] := NotGreaterFullEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterGreater, _, _]] := NotGreaterGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterLess, _, _]] := NotGreaterLess
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterSlantEqual, _, _]] := NotGreaterSlantEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotGreaterTilde, _, _]] := NotGreaterTilde
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLess, _, _]] := NotLess
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessEqual, _, _]] := NotLessEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessFullEqual, _, _]] := NotLessFullEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessGreater, _, _]] := NotLessGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessLess, _, _]] := NotLessLess
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessSlantEqual, _, _]] := NotLessSlantEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotLessTilde, _, _]] := NotLessTilde
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotNestedGreaterGreater, _, _]] := NotNestedGreaterGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`NotNestedLessLess, _, _]] := NotNestedLessLess

inequalityOperatorToSymbol[LeafNode[Token`LongName`VectorLess, _, _]] := System`VectorLess
inequalityOperatorToSymbol[LeafNode[Token`LongName`VectorGreater, _, _]] := System`VectorGreater
inequalityOperatorToSymbol[LeafNode[Token`LongName`VectorLessEqual, _, _]] := System`VectorLessEqual
inequalityOperatorToSymbol[LeafNode[Token`LongName`VectorGreaterEqual, _, _]] := System`VectorGreaterEqual


(*

Just these operators do not have an affinity, neither True nor False

vectorInequalityAffinity[Equal] := xxx
vectorInequalityAffinity[Unequal] := xxx
vectorInequalityAffinity[Less] := xxx
vectorInequalityAffinity[Greater] := xxx
vectorInequalityAffinity[LessEqual] := xxx
vectorInequalityAffinity[GreaterEqual] := xxx
*)

(*
Definitely NOT a VectorInequality
*)
vectorInequalityAffinity[GreaterEqualLess] := False
vectorInequalityAffinity[GreaterFullEqual] := False
vectorInequalityAffinity[GreaterGreater] := False
vectorInequalityAffinity[GreaterLess] := False
vectorInequalityAffinity[GreaterSlantEqual] := False
vectorInequalityAffinity[GreaterTilde] := False
vectorInequalityAffinity[LessEqualGreater] := False
vectorInequalityAffinity[LessFullEqual] := False
vectorInequalityAffinity[LessGreater] := False
vectorInequalityAffinity[LessLess] := False
vectorInequalityAffinity[LessSlantEqual] := False
vectorInequalityAffinity[LessTilde] := False
vectorInequalityAffinity[NestedGreaterGreater] := False
vectorInequalityAffinity[NestedLessLess] := False
vectorInequalityAffinity[NotGreater] := False
vectorInequalityAffinity[NotGreaterEqual] := False
vectorInequalityAffinity[NotGreaterFullEqual] := False
vectorInequalityAffinity[NotGreaterGreater] := False
vectorInequalityAffinity[NotGreaterLess] := False
vectorInequalityAffinity[NotGreaterSlantEqual] := False
vectorInequalityAffinity[NotGreaterTilde] := False
vectorInequalityAffinity[NotLess] := False
vectorInequalityAffinity[NotLessEqual] := False
vectorInequalityAffinity[NotLessFullEqual] := False
vectorInequalityAffinity[NotLessGreater] := False
vectorInequalityAffinity[NotLessLess] := False
vectorInequalityAffinity[NotLessSlantEqual] := False
vectorInequalityAffinity[NotLessTilde] := False
vectorInequalityAffinity[NotNestedGreaterGreater] := False
vectorInequalityAffinity[NotNestedLessLess] := False

(*
Definitely a VectorInequality
*)
vectorInequalityAffinity[System`VectorLess] := True
vectorInequalityAffinity[System`VectorGreater] := True
vectorInequalityAffinity[System`VectorLessEqual] := True
vectorInequalityAffinity[System`VectorGreaterEqual] := True



abstractComma[InfixNode[Comma, children_, data_]] :=
	CallNode[ToNode[Comma], abstract /@ (children /. LeafNode[Token`Fake`ImplicitNull, _, data1_] :> LeafNode[Symbol, "Null", data1]), data]


(*
properly abstract and warn about a;b;[]
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupSquare, _, _] }, dataIn_]] :=
Module[{head, data, issues, first},

	data = dataIn;

	first = firstExplicitToken[groupIn];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 1.0|>]];

	AssociateTo[data, AbstractSyntaxIssues -> issues];

	head = InfixNode[CompoundExpression, Riffle[{headIn}, LeafNode[Token`Semi, ";", <||>]] ~Join~ {LeafNode[Token`Semi, ";", <||>], LeafNode[Token`Fake`ImplicitNull, "", <||>]}, <||>];

	abstract[CallNode[head, { groupIn }, data]]
]

(*
properly abstract and warn about a;b;[];c
*)
abstractCompoundExpression[InfixNode[CompoundExpression, { headIn__, groupIn:GroupNode[GroupSquare, _, _], rest__ }, dataIn_]] :=
Module[{head, data, issues, first},

	data = dataIn;

	first = firstExplicitToken[groupIn];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 1.0|>]];

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


abstractMessageNameChild[LeafNode[String, str_, data_]] := LeafNode[String, escapeString[abstractSymbolString[str]], data]

abstractMessageNameChild[n_] := n







(*
make sure to reverse children of Divisible
*)
abstractDivisible[InfixNode[Divisible, children_, data_]] :=
Module[{processed},

	processed = processInfixBinaryAtQuirk[#, "Divisible"]& /@ children;

	CallNode[ToNode[Divisible], abstract /@ Reverse[processed], data]
]






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

	abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	If[issues != {},
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];

	CallNode[ToNode[tag], abstractedChildren, data]
]


selectChildren[CallNode[ToNode[Comma], children_, _]] := children

selectChildren[n_] := n


abstractGroupNode[GroupMissingCloserNode[tag_, children_, dataIn_]] :=
Module[{abstractedChildren, issues, data},

	data = dataIn;

	abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	If[issues != {},
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];

	GroupMissingCloserNode[tag, abstractedChildren, data]
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
			LeafNode[Token`HashHash, _, _] | CompoundNode[SlotSequence, _, _],
				AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Unexpected ``Part`` call.", "Error", <|Source->data[Source], ConfidenceLevel -> 1.0|>]];
			,
			LeafNode[Symbol (* | String *) | Token`Hash | Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _] | _CallNode |
				CompoundNode[Blank | BlankSequence | BlankNullSequence | PatternBlank | PatternBlankSequence | PatternBlankNullSequence | Slot (* | SlotSequence *), _, _],
				(* these are fine *)
				Null
			,
			LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _],
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			LeafNode[Token`LinearSyntaxBlob, _, _],
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
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
			GroupNode[_, _, _],
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|Source->data[Source], ConfidenceLevel -> 0.95|>]];
			,
			(*
			PostfixNode[Function | Derivative, _, _],
			(* these are fine *)
			Null
			,*)
			(*
			Now handle boxes
			*)
			BoxNode[TemplateBox | InterpretationBox, _, _],
				(* this is fine *)
				Null
			,
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
	If[MatchQ[outerData[[Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],

		If[outerData[[Key[Source], 1, 2]]+1 != innerData[[Key[Source], 1, 2]],
			src = {outerData[[Key[Source], 1]], innerData[[Key[Source], 1]]};
			AppendTo[issues, FormatIssue["NotContiguous", "``Part`` brackets ``[[`` are not contiguous.", "Formatting",
										<|	Source->src,
											CodeActions->{CodeAction["DeleteTrivia", DeleteTrivia,
																<|Source->src|>]},
											ConfidenceLevel -> 1.0|>]];
		];

		If[innerData[[Key[Source], 2, 2]]+1 != outerData[[Key[Source], 2, 2]],
			src = {innerData[[Key[Source], 2]], outerData[[Key[Source], 2]]};
			AppendTo[issues, FormatIssue["NotContiguous", "``Part`` brackets ``]]`` are not contiguous.", "Formatting",
										<|	Source->src,
											CodeActions->{CodeAction["DeleteTrivia", DeleteTrivia, 
																<|Source->src|>]},
											ConfidenceLevel -> 1.0|>]];
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

(*
feel strongly about ##2[arg]
##2 represents a sequence of arguments, so it is wrong to call
*)
abstractCallNode[CallNode[headIn:LeafNode[Token`HashHash, _, _] | CompoundNode[SlotSequence, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, partData, issues, data, first},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	first = firstExplicitToken[head];

	AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Unexpected call.", "Error", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 1.0|>]];

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

abstractCallNode[CallNode[headIn:LeafNode[Symbol | String | Token`Hash | Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _] | _CallNode |
			 CompoundNode[Blank | BlankSequence | BlankNullSequence | PatternBlank | PatternBlankSequence | PatternBlankNullSequence | Slot (*| SlotSequence*), _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
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

	CallNode[head, part[[2]], data]
]

abstractCallNode[CallNode[headIn:LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, partData, issues, data, first},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	first = firstExplicitToken[head];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];

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

abstractCallNode[CallNode[headIn:BinaryNode[PatternTest, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
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

	CallNode[head, part[[2]], data]
]

abstractCallNode[CallNode[headIn:InfixNode[CompoundExpression, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
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

	CallNode[head, part[[2]], data]
]

(*
these are fine
List is allowed because this is popular to do:
Through[{a, b, c}[1]]
*)
abstractCallNode[CallNode[headIn:GroupNode[GroupParen | List | Association, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
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

	CallNode[head, part[[2]], data]
]

abstractCallNode[CallNode[headIn:GroupNode[_, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, partData, issues, data, first},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	first = firstExplicitToken[head];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];

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

(* these are fine *)
abstractCallNode[CallNode[headIn:PostfixNode[Function | Derivative, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
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

	CallNode[head, part[[2]], data]
]

(* this is fine *)
abstractCallNode[CallNode[headIn:BoxNode[TemplateBox | InterpretationBox, _, _], {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
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

	CallNode[head, part[[2]], data]
]

(*
warn about anything else
*)
abstractCallNode[CallNode[headIn_, {partIn:GroupNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, partData, issues, data, first},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	first = firstExplicitToken[head];

	AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];

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
Module[{head, part, partData, data, issues, first},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	Switch[head,
			(*
			feel strongly about ##2[arg]
			##2 represents a sequence of arguments, so it is wrong to call
			*)
			LeafNode[Token`HashHash, _, _] | CompoundNode[SlotSequence, _, _],
				first = firstExplicitToken[head];
				AppendTo[issues, SyntaxIssue["StrangeCallSlotSequence", "Unexpected call.", "Error", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 1.0|>]];
			,
			LeafNode[Symbol (* | String *) | Token`Hash | Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _] | _CallNode |
				CompoundNode[Blank | BlankSequence | BlankNullSequence | PatternBlank | PatternBlankSequence | PatternBlankNullSequence | Slot (* | SlotSequence *), _, _],
				(* these are fine *)
				Null
			,
			LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _],
				first = firstExplicitToken[head];
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];
			,
			LeafNode[Token`LinearSyntaxBlob, _, _],
				first = firstExplicitToken[head];
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Remark", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];
			,
			PrefixNode[PrefixLinearSyntaxBang, _, _],
				first = firstExplicitToken[head];
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Remark", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];
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
			GroupNode[_, _, _],
				first = firstExplicitToken[head];
				AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];
			,
			(*
			PostfixNode[Function | Derivative, _, _],
				(* these are fine *)
				Null
			,*)
			(*
			Now handle boxes
			*)
			BoxNode[TemplateBox | InterpretationBox, _, _],
				(* this is fine *)
				Null
			,
			_,
			(*
			warn about anything else
			*)
			first = firstExplicitToken[head];
			AppendTo[issues, SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source->first[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]];
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



abstractCallNode[CallMissingCloserNode[headIn_, {partIn:GroupMissingCloserNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, data, issues},
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

	CallMissingCloserNode[head, part[[2]], data]
]

abstractCallNode[UnterminatedCallNode[headIn_, {partIn:UnterminatedGroupNode[GroupSquare, _, _]}, dataIn_]] :=
Module[{head, part, data, issues},
	head = headIn;
	part = partIn;
	data = dataIn;

	issues = {};

	head = abstract[head];
	(*
	part = abstractGroupNode[part];
	partData = part[[3]];
	*)

	(*
	issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

	If[issues != {},
		issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
		AssociateTo[data, AbstractSyntaxIssues -> issues];
	];
	*)

	UnterminatedCallNode[head, part[[2]], data]
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
abstract[BoxNode[RowBox, {a_}, data_]] := BoxNode[RowBox, {abstract /@ a}, data]

abstract[BoxNode[b_, children_, data_]] := BoxNode[b, abstract /@ children, data]





abstractNot2[rand_, notNotTok_, dataIn_] :=
Module[{notNotData, data},
	
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
