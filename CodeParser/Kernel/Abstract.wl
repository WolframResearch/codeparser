(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`Abstract`"]

Aggregate

Abstract


abstract

abstractGroupNode

$AggregateParseProgress

$AbstractParseProgress


$CurrentBatchMode


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`AbstractCallNode`"]
Needs["CodeParser`Folds`"]
Needs["CodeParser`Quirks`"]
Needs["CodeParser`TopLevel`"] (* for abstractTopLevelChildren *)
Needs["CodeParser`Utils`"]



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

(*
BatchMode -> True where Begin[] and End[] nodes will be at top-level (e.g., .wl files)
PackageNodes[] and ContextNodes[] WILL be created
Issues about unbalanced directives WILL be created

BatchMode -> False otherwise, i.e., where Begin[] and End[] nodes are separate or not easily scanned together (e.g., cells in notebooks)
PackageNodes[] and ContextNodes[] will NOT be created
Issues about unbalanced directives will NOT be created
*)
Options[Abstract] = {
  "BatchMode" -> True
}

Abstract[agg_, opts:OptionsPattern[]] :=
Block[{$RecursionLimit = Infinity},
Module[{ast, batchMode},

  batchMode = OptionValue["BatchMode"];

  CodeParser`Abstract`$AbstractParseStart = Now;
  CodeParser`Abstract`$AbstractParseTime = Quantity[0, "Seconds"];

  CodeParser`Abstract`$AbstractParseProgress = 5;

  ast = agg;

  (*
  There is some surgery involved with gluing "-" onto numbers and this can screw up removing of line continuations.

  So make sure to normalize tokens BEFORE doing the abstract fold
  *)
  ast = normalizeTokens[ast];

  Block[{$CurrentBatchMode},

    $CurrentBatchMode = batchMode;

    ast = abstract[ast];
  ];

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
Token`Fake`ImplicitNull, Token`Error`PrefixImplicitNull, and Token`Error`InfixImplicitNull do NOT get abstracted because they are handled at their possible parents: Comma and CompoundExpression
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

abstract[BinaryNode[System`MapApply, {left_, _, right_}, data_]] :=
Module[{oldAtAtAtQuirk},
  oldAtAtAtQuirk = Lookup[$Quirks, "OldAtAtAt", False];
  If[oldAtAtAtQuirk,
    CallNode[ToNode[Apply], abstract /@ {left, right, GroupNode[List, { LeafNode[Token`OpenCurly, "{", <||>], ToNode[1], LeafNode[Token`CloseCurly, "}", <||>] }, <||>]}, data]
    ,
    CallNode[ToNode[System`MapApply], abstract /@ {left, right}, data]
  ]
]

(*
Make sure to reverse the arguments
*)
abstract[BinaryNode[BinarySlashSlash, {left_, _, right_}, data_]] := CallNode[abstract[right], {abstract[left]}, data]

abstract[BinaryNode[Put, {left_, _, LeafNode[String, str_, data1_]}, data_]] := CallNode[ToNode[Put], {abstract[left], LeafNode[String, escapeString[abstractFileString[str]], data1]}, data]
abstract[BinaryNode[PutAppend, {left_, _, LeafNode[String, str_, data1_]}, data_]] := CallNode[ToNode[PutAppend], {abstract[left], LeafNode[String, escapeString[abstractFileString[str]], data1]}, data]

abstract[BinaryNode[Pattern, {left_, _, right_}, data_]] :=
  CallNode[ToNode[Pattern], {abstract[left], abstract[right]}, data]

(*
Abstract NonAssociative errors

a ? b ? c being NonAssociative is alluded to being a bug in bug report 206938
Related bugs: 206938
*)
abstract[BinaryNode[PatternTest, {left:BinaryNode[PatternTest, _, _], _, right_}, data_]] :=
  AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociativePatternTest, {abstract[left], abstract[right]}, data]

abstract[BinaryNode[Unset, {left_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]] :=
  CallNode[ToNode[Unset], {abstract[left]}, data]

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

abstract[InfixNode[InfixTilde, children_, data_]] := abstractInfixTilde[InfixNode[InfixTilde, children[[;;;;2]], data]]

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

(*
Allow non-Symbols for left; not a syntax error
*)
abstract[TernaryNode[TagSet, {left_, _, middle_, _, right_}, data_]] :=
  CallNode[ToNode[TagSet], {abstract[left], abstract[middle], abstract[right]}, data]

(*
Allow non-Symbols for left; not a syntax error
*)
abstract[TernaryNode[TagSetDelayed, {left_, _, middle_, _, right_}, data_]] :=
  CallNode[ToNode[TagSetDelayed], {abstract[left], abstract[middle], abstract[right]}, data]

(*
Allow non-Symbols for left; not a syntax error
*)
abstract[TernaryNode[TagUnset, {left_, _, middle_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]] :=
  CallNode[ToNode[TagUnset], {abstract[left], abstract[middle]}, data]

abstract[TernaryNode[Span, {left_, _, middle_, _, right_}, data_]] :=
  CallNode[ToNode[Span], {abstract[left], abstract[middle], abstract[right]}, data]


(*
TernaryOptionalPattern comes from boxes
*)
abstract[TernaryNode[TernaryOptionalPattern, {left_, _, middle_, _, right_}, data_]] :=
  CallNode[ToNode[Optional], {CallNode[ToNode[Pattern], {abstract[left], abstract[middle]}, <||>], abstract[right]}, data]






(*
handle CallNode before possible GroupNode errors

what are the different shapes that calls can have?

They are:

f [ ]

f [ [ ] ]

f ::[ ]

f \[LeftDoubleBracket] \[RightDoubleBracket]
*)

abstract[CallNode[op_, child:GroupNode[GroupSquare, { _, GroupNode[GroupSquare, _, _], _ }, _], data1_]] :=
  abstractCallNode[CallNode[op, child, data1]]

abstract[CallNode[op_, child:GroupNode[GroupSquare, _, _], data1_]] :=
  abstractCallNode[CallNode[op, child, data1]]

abstract[CallNode[op_, child:GroupNode[GroupTypeSpecifier, _, _], data1_]] :=
  abstractCallNode[CallNode[op, child, data1]]

abstract[CallNode[op_, child:GroupNode[GroupDoubleBracket, _, _], data1_]] :=
  abstractCallNode[CallNode[op, child, data1]]


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
*)
abstract[CallNode[head_, child:GroupMissingCloserNode[GroupSquare, _, _], data_]] :=
  abstractCallNode[CallMissingCloserNode[head, child, data]]

abstract[CallNode[head_, child:GroupMissingCloserNode[GroupTypeSpecifier, _, _], data_]] :=
  abstractCallNode[CallMissingCloserNode[head, child, data]]

abstract[CallNode[head_, child:GroupMissingCloserNode[GroupDoubleBracket, _, _], data_]] :=
  abstractCallNode[CallMissingCloserNode[head, child, data]]

(*
UnterminatedGroupNode does NOT get abstracted
*)
abstract[CallNode[head_, child:UnterminatedGroupNode[GroupSquare, _, _], data_]] :=
  abstractCallNode[UnterminatedCallNode[head, child, data]]

abstract[CallNode[head_, child:UnterminatedGroupNode[GroupTypeSpecifier, _, _], data_]] :=
  abstractCallNode[UnterminatedCallNode[head, child, data]]

abstract[CallNode[head_, child:UnterminatedGroupNode[GroupDoubleBracket, _, _], data_]] :=
  abstractCallNode[UnterminatedCallNode[head, child, data]]



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

(*
GroupNode errors

naked []

naked ::[]

naked \[LeftDoubleBracket]\[RightDoubleBracket]
*)
abstract[GroupNode[GroupSquare, children_, data_]] :=
  AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, children, data]

abstract[GroupNode[GroupTypeSpecifier, children_, data_]] :=
  AbstractSyntaxErrorNode[AbstractSyntaxError`ColonColonOpenSquare, children, data]

abstract[GroupNode[GroupDoubleBracket, children_, data_]] :=
  AbstractSyntaxErrorNode[AbstractSyntaxError`LeftDoubleBracket, children, data]



(*
Missing closers
*)

abstract[GroupMissingCloserNode[tag_, children_, data_]] :=
  abstractGroupNode[GroupMissingCloserNode[tag, children, data]]



abstract[n:UnterminatedGroupNode[_, _, _]] :=
  n



(*
Missing openers

Only possible with boxes
*)

abstract[GroupMissingOpenerNode[tag_, children_, data_]] :=
  abstractGroupNode[GroupMissingOpenerNode[tag, children, data]]




abstract[GroupNode[tag_, children_, data_]] :=
  abstractGroupNode[GroupNode[tag, children, data]]




abstract[PrefixBinaryNode[Integrate, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[Integrate], {abstract[operand1], abstract[var]}, data]

abstract[PrefixBinaryNode[ContourIntegral, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[ContourIntegral], {abstract[operand1], abstract[var]}, data]

abstract[PrefixBinaryNode[DoubleContourIntegral, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[DoubleContourIntegral], {abstract[operand1], abstract[var]}, data]

abstract[PrefixBinaryNode[ClockwiseContourIntegral, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[ClockwiseContourIntegral], {abstract[operand1], abstract[var]}, data]

abstract[PrefixBinaryNode[CounterClockwiseContourIntegral, {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]}, data_]] := CallNode[ToNode[CounterClockwiseContourIntegral], {abstract[operand1], abstract[var]}, data]

abstract[PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]] := CallNode[ToNode[op], {abstract[operand1], abstract[operand2]}, data]



abstract[ContainerNode[tag_, childrenIn_, dataIn_]] :=
Catch[
Module[{abstracted, issues, issues1, issues2, data,
  abstractedChildren, node, willReportToplevelIssues,
  reportIssuesBehavior, children, res},

  children = childrenIn;

  data = dataIn;

  If[FailureQ[children],
    Throw[ContainerNode[tag, children, data]]
  ];

  willReportToplevelIssues = (tag === File);

  reportIssuesBehavior = <| "WillReportToplevelIssues" -> willReportToplevelIssues, "ToplevelChildrenLength" -> Length[children] |>;

  issues = {};

  {abstractedChildren, issues1} = abstractTopLevelChildren[children, reportIssuesBehavior];
  
  (*
  was:
  If[TrueQ[$CurrentBatchMode],
    res = abstractTopLevel[abstractedChildren];

    If[FailureQ[res],
      Throw[res]
    ];

    {abstracted, issues2} = res;
    ,
    {abstracted, issues2} = {abstractedChildren, {}}
  ];
  but it is important to always run abstractTopLevel, because it supplies information like "Definitions"

  related bugs: 410408

  *)
  res = abstractTopLevel[abstractedChildren];

  If[FailureQ[res],
    Throw[res]
  ];

  {abstracted, issues2} = res;

  issues = issues1 ~Join~ issues2;

  If[issues != {},
    issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
    AssociateTo[data, AbstractSyntaxIssues -> issues];
  ];

  node = ContainerNode[tag, abstracted, data];

  node
]]



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
  CallNode[ToNode[Power],
    GroupNode[GroupSquare, {
      LeafNode[Token`OpenSquare, "[", <||>],
      InfixNode[Comma, { node, LeafNode[Token`Comma, ",", <||>], ToNode[-1] }, <||>],
      LeafNode[Token`CloseSquare, "]", <||>] }, <||> ], data]



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
  CallNode[ToNode[Comma], abstract /@ (children /. ErrorNode[Token`Error`PrefixImplicitNull | Token`Error`InfixImplicitNull, _, data1_] :> LeafNode[Symbol, "Null", data1]), data]



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
    AppendTo[issues, SyntaxIssue["SyntaxUndocumentedMessageName", "This syntax is not documented.", "Error", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]];
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

derivativeOrderAndAbstractedBody[PostfixNode[Derivative, {rand_, _}, data_]] :=
Module[{order, body},
  {order, body} = derivativeOrderAndAbstractedBody[rand];
  {order+1, body}
]

derivativeOrderAndAbstractedBody[node_] :=
  {0, abstract[node]}

abstractDerivative[PostfixNode[Derivative, {rand_, LeafNode[Token`SingleQuote, _, _]}, data_]] :=
Module[{order, abstractedBody},
  {order, abstractedBody} = derivativeOrderAndAbstractedBody[rand];
  CallNode[CallNode[ToNode[Derivative], {ToNode[order+1]}, <||>], {abstractedBody}, <||>]
]

abstractDerivative[PostfixNode[Derivative, {rand_, LeafNode[Token`Boxes`MultiSingleQuote, quoteStr_, _]}, data_]] :=
Module[{order},
  order = StringLength[quoteStr];
  CallNode[CallNode[ToNode[Derivative], {ToNode[order]}, <||>], {abstract[rand]}, <||>]
]



(*
only from boxes
*)

abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {left_, middle_}, dataIn_]] :=
  AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {left, middle}, dataIn]

abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {left_, middle_, right_}, dataIn_]] :=
  CallNode[abstract[middle], {left, abstract[right]}, dataIn]

abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {left_, middle_, right_, rest___}, dataIn_]] :=
  abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {left, middle, right}, <||>]], rest}, dataIn]]

abstractInfixTilde[InfixNode[InfixTilde, {left_, middle_}, dataIn_]] :=
  AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {left, middle}, dataIn]

abstractInfixTilde[InfixNode[InfixTilde, {left_, middle_, right_}, dataIn_]] :=
  abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {abstract[left], middle, right}, dataIn]]

abstractInfixTilde[InfixNode[InfixTilde, {left_, middle_, right_, rest___}, dataIn_]] :=
  abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {abstractInfixTilde[InfixNode[InfixTilde, {left, middle, right}, <||>]], rest}, dataIn]]




(*
Precondition: Opener and Closer are still present
Precondition: Commas are still present

Removes all commas

Fills in Nulls and gives SyntaxIssues for e.g. {1,,2}
*)
abstractGroupNode[GroupNode[tag_, childrenIn_, dataIn_]] :=
Module[{children, abstractedChildren, issues, data},
  
  children = childrenIn;
  data = dataIn;

  children = children[[2;;-2]];

  abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];

  issues = Lookup[data, AbstractSyntaxIssues, {}];

  If[issues != {},
    AssociateTo[data, AbstractSyntaxIssues -> issues];
  ];

  CallNode[ToNode[tag], abstractedChildren, data]
]


selectChildren[CallNode[ToNode[Comma], children_, _]] := children

selectChildren[n_] := n


abstractGroupNode[GroupMissingCloserNode[tag_, childrenIn_, dataIn_]] :=
Module[{children, abstractedChildren, issues, data},

  children = childrenIn;
  data = dataIn;

  children = children[[2;;]];

  abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];

  issues = Lookup[data, AbstractSyntaxIssues, {}];

  If[issues != {},
    AssociateTo[data, AbstractSyntaxIssues -> issues];
  ];

  GroupMissingCloserNode[tag, abstractedChildren, data]
]


abstractGroupNode[GroupMissingOpenerNode[tag_, childrenIn_, dataIn_]] :=
Module[{children, abstractedChildren, issues, data},

  children = childrenIn;
  data = dataIn;

  children = children[[;;-2]];

  abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];

  issues = Lookup[data, AbstractSyntaxIssues, {}];

  If[issues != {},
    AssociateTo[data, AbstractSyntaxIssues -> issues];
  ];

  GroupMissingOpenerNode[tag, abstractedChildren, data]
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

(*
a is a List of Lists
*)
abstract[BoxNode[GridBox, {a_, rest___}, data_]] := BoxNode[GridBox, {Map[abstract, a, {2}]} ~Join~ (abstract /@ {rest}), data]

(*
Handle special form of [[x]] in subscript

Keep the [[]] structure un-abstracted

FIXME: when things like SuperscriptBox[] -> Power[] and FractionBox[] -> Divide, then also do SubscriptBox[..., [[]] ] -> Part
*)
abstract[
  BoxNode[
    SubscriptBox
    ,
    {
      a_
      ,
      GroupNode[GroupSquare, {
        o1:LeafNode[Token`OpenSquare, _, _],
        GroupNode[GroupSquare, {
          o2:LeafNode[Token`OpenSquare, _, _],
          b_,
          c2:LeafNode[Token`CloseSquare, _, _]}
          ,
          data2_
        ],
        c1:LeafNode[Token`CloseSquare, _, _]}
        ,
        data1_
      ]
      ,
      ___
    }
    ,
    data_
  ]
] := BoxNode[SubscriptBox, {abstract[a], GroupNode[GroupSquare, {o1, GroupNode[GroupSquare, {o2, abstract[b], c2}, data2], c1}, data1]}, data]

abstract[
  BoxNode[
    SubscriptBox
    ,
    {
      a_
      ,
      GroupNode[GroupDoubleBracket, {
        o:LeafNode[Token`LongName`LeftDoubleBracket, _, _],
        b_,
        c:LeafNode[Token`LongName`RightDoubleBracket, _, _]}
        ,
        data1_
      ]
      ,
      ___
    }
    ,
    data_
  ]
] := BoxNode[SubscriptBox, {abstract[a], GroupNode[GroupDoubleBracket, {o, abstract[b], c}, data1]}, data]


(*
Handle special form of TagBox[(), Derivative] in superscript

Keep the TagBox[(), Derivative] structure un-abstracted

TagBox is considered "easier" than say, FormBox

Contents of TagBox are largely valid boxes

FIXME: when things like SuperscriptBox[] -> Power[] and FractionBox[] -> Divide, then also do SuperscriptBox[..., TagBox[(), Derivative] ] -> Derivative

FIXME: maybe first arg of TagBox should be treated as a CodeNode and not parsed at all
*)
abstract[
  BoxNode[
    SuperscriptBox
    ,
    {
      a_
      ,
      BoxNode[TagBox, {
        GroupNode[GroupParen, {
          o:LeafNode[Token`OpenParen, _, _],
          b_,
          c:LeafNode[Token`CloseParen, _, _]}
          ,
          data2_
        ]
        ,
        t:CodeNode[Null, Derivative, _]}
        ,
        data1_
      ]
      ,
      ___
    }
    ,
    data_
  ]
] := BoxNode[SuperscriptBox, {abstract[a], BoxNode[TagBox, {GroupNode[GroupParen, {o, abstract[b], c}, data2], t}, data1]}, data]


abstract[BoxNode[b_, children_, data_]] := BoxNode[b, abstract /@ children, data]


abstract[CellNode[c_, children_, data_]] := CellNode[c, abstract /@ children, data]




abstractNot2[rand_, notNotTok_, dataIn_] :=
Module[{notNotData, data, issues},
  
  notNotData = notNotTok[[3]];

  data = dataIn;

  issues = Lookup[data, AbstractSyntaxIssues, {}];

  AppendTo[issues, SyntaxIssue["PrefixNotNot", "Unexpected parse.", "Warning", <| Source -> notNotData[Source], ConfidenceLevel -> 1.0 |>]];

  AssociateTo[data, AbstractSyntaxIssues -> issues];

  CallNode[LeafNode[Symbol, "Not", <||>], {
    CallNode[LeafNode[Symbol, "Not", <||>], {
      abstract[rand]}, <||>]}, data]
]



abstract[SyntaxErrorNode[SyntaxError`ExpectedTilde, {left_, _, middle_}, data_]] :=
  SyntaxErrorNode[SyntaxError`ExpectedTilde, {abstract[left], abstract[middle]}, data]

abstract[SyntaxErrorNode[SyntaxError`ExpectedSet, {left_, _, middle_}, data_]] :=
  SyntaxErrorNode[SyntaxError`ExpectedSet, {abstract[left], abstract[middle]}, data]

abstract[SyntaxErrorNode[SyntaxError`OldFESyntax, children_, data_]] :=
  SyntaxErrorNode[SyntaxError`OldFESyntax, abstract /@ children, data]

abstract[SyntaxErrorNode[SyntaxError`BuggyFESyntax, children_, data_]] :=
  SyntaxErrorNode[SyntaxError`BuggyFESyntax, abstract /@ children, data]

abstract[SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, {left_, _, _, right_}, data_]] :=
  SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, {abstract[left], abstract[right]}, data]

abstract[SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, {left_, _, middle_, _}, data_]] :=
  SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, {abstract[left], abstract[middle]}, data]

abstract[SyntaxErrorNode[SyntaxError`ExpectedSymbol, {left_, _, right_}, data_]] :=
  SyntaxErrorNode[SyntaxError`ExpectedSymbol, {abstract[left], abstract[right]}, data]




abstract[f_?FailureQ] := f

abstract[m_?MissingQ] := m

abstract[args___] :=
  Failure["Unhandled", <| "Function" -> abstract, "Arguments" -> HoldForm[{args}] |>]

  

End[]

EndPackage[]
