(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`Abstract`"]

Aggregate

Abstract


abstract

$AggregateParseProgress

$AbstractParseProgress


$CurrentBatchMode


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Quirks`"]
Needs["CodeParser`TopLevel`"] (* for abstractTopLevelChildren *)
Needs["CodeParser`Utils`"]
Needs["CodeParser`Library`"] (* For aggregateFunc, abstractFunc *)


$containerKind = None

Aggregate::usage = "Aggregate[cst] returns an aggregate syntax tree from a concrete syntax tree."

Aggregate[cst_] :=
Block[{$RecursionLimit = Infinity},
Module[{agg},

  CodeParser`Abstract`$AggregateParseStart = Now;
  CodeParser`Abstract`$AggregateParseTime = Quantity[0, "Seconds"];

  CodeParser`Abstract`$AggregateParseProgress = 5;

  agg = aggregateFunc[cst];

	(* NOTE:
		This ReplaceAll forces evaluation of the 3rd argument of CodeNode.

		This is necessary because:
			* Hold[Association[]] is NOT equal to Hold[<||>].
			  - The former is a TNORMAL expression, while the later is a
			    TASSOCIATION, which means they are not considered SameQ.
		    * aggregateFunc[..] is implemented as a LibraryLink WSTP function, and
			  Association expressions deserialized from WSTP are intially
			  constructed as TNORMAL expressions. They only get turned into
			  TASSOCIATION expressions if they are evaluated.
			* The 3rd argument of CodeNode is an Association, typically
			  containing Source info.
		    * Since Association[] and <||> are not the same, tests involving
			  CodeNode will otherwise fail if the test is written using <||>.
			* Forcing evaluating of the 3rd parameter turns the TNORMAL into
			  a TASSOCIATION, causes the tests to succeed.
	*)
	agg = ReplaceAll[agg, {
		CodeNode[a_, b_, c0_] :> With[{c = c0}, CodeNode[a, b, c]]
	}];

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

	(* NOTE:
		This ReplaceAll forces evaluation of the 3rd argument of CodeNode.

		This is necessary because:
			* Hold[Association[]] is NOT equal to Hold[<||>].
				- The former is a TNORMAL expression, while the later is a
				TASSOCIATION, which means they are not considered SameQ.
			* aggregateFunc[..] is implemented as a LibraryLink WSTP function, and
				Association expressions deserialized from WSTP are intially
				constructed as TNORMAL expressions. They only get turned into
				TASSOCIATION expressions if they are evaluated.
			* The 3rd argument of CodeNode is an Association, typically
				containing Source info.
			* Since Association[] and <||> are not the same, tests involving
				CodeNode will otherwise fail if the test is written using <||>.
			* Forcing evaluating of the 3rd parameter turns the TNORMAL into
				a TASSOCIATION, causes the tests to succeed.
	*)
	ast = ReplaceAll[ast, {
		CodeNode[a_, b_, c0_] :> With[{c = c0}, CodeNode[a, b, c]]
	}];

  CodeParser`Abstract`$AbstractParseProgress = 100;

  CodeParser`Abstract`$AbstractParseTime = Now - CodeParser`Abstract`$AbstractParseStart;

  ast
]]




abstract[n_ErrorNode] :=
  n


abstract[
	expr:(
		_LeafNode
		| _CompoundNode
		| _PrefixNode
		| _PostfixNode
		| _BinaryNode
		| _InfixNode
		| _TernaryNode
		| _CallNode
		| _GroupNode
		| _PrefixBinaryNode
		| _CodeNode
		| _BoxNode
		| _SyntaxErrorNode
		| _GroupMissingCloserNode
		| _GroupMissingOpenerNode
	)
] :=
	libraryFunctionWrapper[abstractFunc, expr, $Quirks, $containerKind]


abstract[ContainerNode[tag_, childrenIn_, dataIn_]] := Block[{
	(* Set this value so that Source::from_expr() knows how to parse the Source
	   values of the nodes, to preserve the source information as this node
	   round-trips from WL => Rust => WL. *)
	$containerKind = tag
},
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
]]]






selectChildren[CallNode[ToNode[Comma], children_, _]] := children

selectChildren[n_] := n


abstract[CellNode[c_, children_, data_]] := CellNode[c, abstract /@ children, data]









abstract[f_?FailureQ] := f

abstract[m_?MissingQ] := m

abstract[args___] :=
  Failure["Unhandled", <| "Function" -> abstract, "Arguments" -> HoldForm[{args}] |>]

  

End[]

EndPackage[]
