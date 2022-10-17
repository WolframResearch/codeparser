(* ::Package::"Tags"-><|"Control" -> <|Enabled -> False|>, "NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`TopLevel`"]

abstractTopLevelChildren

abstractTopLevel

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Abstract`"] (* for abstract *)
Needs["CodeParser`Definitions`"] (* for DefinitionSymbols *)
Needs["CodeParser`Shims`"] (* cleanupStackShimMemoryLeak *)
Needs["CodeParser`Utils`"]


(*

Call abstract on children

But also warn if something strange is at top-level

*)
abstractTopLevelChildren[{missing_?MissingQ}, _] :=
  {{missing}, {}}
  
abstractTopLevelChildren[children_, reportIssuesBehavior_] :=
Module[{abstractedChildren, issues, issuesMaybe},

  {abstractedChildren, issuesMaybe} =
    Reap[
      MapIndexed[
        Function[{child, idx},
          Sow[topLevelChildIssues[child, <| reportIssuesBehavior, "ToplevelChildIndex" -> idx[[1]] |>]];
          abstract[child]
        ]
        ,
        children
      ]
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

Some nodes would be strange at top-level in a package. For example, 1+1 would be strange at top-level.

*)


topLevelChildIssues[n:CallNode[_, GroupNode[GroupSquare, {_, GroupNode[GroupSquare, _, _], _}, _], data_], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
Catch[
Module[{first},

  first = firstExplicitToken[n];

  If[FailureQ[first],
    Throw[{}]
  ];

  {SyntaxIssue["TopLevelPart", "Unexpected ``Part`` expression at top-level.", "Warning", <|
    Source -> first[[3, Key[Source]]],
    ConfidenceLevel -> 0.95
  |>]}
]]

(*
Call could be anything
*)
topLevelChildIssues[CallNode[_, GroupNode[GroupSquare, _, _], _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

(*
probably a declaration
*)
topLevelChildIssues[LeafNode[Symbol, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

topLevelChildIssues[LeafNode[String, _, data_], reportIssuesBehavior:KeyValuePattern["WillReportToplevelIssues" -> True]] :=
Catch[
Module[{},

  (*
  If a list or whatever is the only expression in a file, or if it is the last expression in a file,
  then assume it is "Data" or something and do not complain
  *)
  If[reportIssuesBehavior["ToplevelChildIndex"] == reportIssuesBehavior["ToplevelChildrenLength"],
    Throw[{}]
  ];

  {SyntaxIssue["TopLevelString", "Unexpected string at top-level.", "Warning", <|
    Source -> data[Source],
    ConfidenceLevel -> 0.75
  |>]}
]]

(*
Side-effecting or calling binary operators
*)
topLevelChildIssues[
  BinaryNode[AddTo | Apply | BinaryAt | BinarySlashSlash |
    Map | System`MapApply | Set | SetDelayed |
    SubtractFrom | Unset | UpSet | UpSetDelayed | Put |
    PutAppend
    ,
    _
    ,
    _
  ]
  ,
  KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
  {}

(*
Side-effecting ternary operators
*)
topLevelChildIssues[TernaryNode[TagSet | TagSetDelayed | TagUnset | TernaryTilde, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

(*
Side-effecting prefix operators
*)
topLevelChildIssues[PrefixNode[Get | PreDecrement | PreIncrement, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

(*
Side-effecting postfix operators
*)
topLevelChildIssues[PostfixNode[Decrement | Increment, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

(*
just assume parens connote intention
*)
topLevelChildIssues[GroupNode[GroupParen, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

(*

assume to be declaration syntax:

{a}

*)
topLevelChildIssues[GroupNode[List, {
  LeafNode[Token`OpenCurly, _, _],
  LeafNode[Symbol, _, _],
  LeafNode[Token`CloseCurly, _, _]}, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

(*

assume to be declaration syntax:

{a, b, c}

The strange data_ /; MatchQ is to work-around bug 419646
*)
topLevelChildIssues[GroupNode[List, {
  LeafNode[Token`OpenCurly, _, _],
  InfixNode[Comma, {
    PatternSequence[LeafNode[Symbol, _, _], LeafNode[Token`Comma, _, _]].., LeafNode[Symbol, _, _]}, _],
  LeafNode[Token`CloseCurly, _, _]}, _], data_ /; MatchQ[data, KeyValuePattern["WillReportToplevelIssues" -> True]]] :=
  {}

topLevelChildIssues[n:GroupNode[List, _, data_], reportIssuesBehavior:KeyValuePattern["WillReportToplevelIssues" -> True]] :=
Catch[
Module[{first},

  (*
  If a list or whatever is the only expression in a file, or if it is the last expression in a file,
  then assume it is "Data" or something and do not complain
  *)
  If[reportIssuesBehavior["ToplevelChildIndex"] == reportIssuesBehavior["ToplevelChildrenLength"],
    Throw[{}]
  ];

  first = firstExplicitToken[n];

  If[FailureQ[first],
    Throw[{}]
  ];

  {SyntaxIssue["TopLevelList", "Unexpected list at top-level.", "Warning", <|
    Source -> first[[3, Key[Source]]],
    ConfidenceLevel -> 0.75
  |>]}
]]

topLevelChildIssues[n:GroupNode[Association, _, data_], reportIssuesBehavior:KeyValuePattern["WillReportToplevelIssues" -> True]] :=
Catch[
Module[{first},

  (*
  If a list or whatever is the only expression in a file, or if it is the last expression in a file,
  then assume it is "Data" or something and do not complain
  *)
  If[reportIssuesBehavior["ToplevelChildIndex"] == reportIssuesBehavior["ToplevelChildrenLength"],
    Throw[{}]
  ];

  first = firstExplicitToken[n];

  If[FailureQ[first],
    Throw[{}]
  ];

  {SyntaxIssue["TopLevelAssociation", "Unexpected association at top-level.", "Warning", <|
    Source -> first[[3, Key[Source]]],
    ConfidenceLevel -> 0.75
  |>]}
]]

(*
descend
*)
topLevelChildIssues[
  InfixNode[CompoundExpression, {first_, LeafNode[Token`Semi, _, _], LeafNode[Token`Fake`ImplicitNull, _, _]}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  topLevelChildIssues[first, ignored]

(*

assume to be declaration syntax:

a; b; c;

*)
topLevelChildIssues[
  InfixNode[CompoundExpression, {
    PatternSequence[LeafNode[Symbol, _, _], LeafNode[Token`Semi, _, _]].., LeafNode[Token`Fake`ImplicitNull, _, _]}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  {}

(*

assume to be declaration syntax:

a; b; c

*)
topLevelChildIssues[
  InfixNode[CompoundExpression, {
    PatternSequence[LeafNode[Symbol, _, _], LeafNode[Token`Semi, _, _]].., LeafNode[Symbol, _, _]}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  {}

(*

assume to be definition syntax:

a = 1; b = 2; c = 3;

*)
topLevelChildIssues[
  InfixNode[CompoundExpression, {
    PatternSequence[
      BinaryNode[Set | SetDelayed | UpSet | UpSetDelayed | Unset, _, _] |
      TernaryNode[TagSet | TagSetDelayed | TagUnset, _, _], LeafNode[Token`Semi, _, _]].., LeafNode[Token`Fake`ImplicitNull, _, _]}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  {}

(*

assume to be definition syntax:

a = 1; b = 2; c = 3

*)
topLevelChildIssues[
  InfixNode[CompoundExpression, {
    PatternSequence[
      BinaryNode[Set | SetDelayed | UpSet | UpSetDelayed | Unset, _, _] |
      TernaryNode[TagSet | TagSetDelayed | TagUnset, _, _], LeafNode[Token`Semi, _, _]]..,
    BinaryNode[Set | SetDelayed | UpSet | UpSetDelayed | Unset, _, _] |
    TernaryNode[TagSet | TagSetDelayed | TagUnset, _, _]}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  {}

(*
We want to be able to catch e.g.,

foo[] := Message[foo::bad]; $Failed

where the parsing is ( foo[] := Message[foo::bad] ) ; $Failed

A recursive call would see the SetDelayed, and say fine

And then the recursive call would see the $Failed and say fine (because it's a symbol and might be a declaration)

So hard-code the CompoundExpression versions to be able to catch these cases
*)
topLevelChildIssues[
  InfixNode[CompoundExpression, {
    BinaryNode[Set | SetDelayed | UpSet | UpSetDelayed, _, _] |
    TernaryNode[TagSet | TagSetDelayed, _, _], LeafNode[Token`Semi, _, _], LeafNode[Token`Fake`ImplicitNull, _, _]}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  {}

topLevelChildIssues[
  InfixNode[CompoundExpression, {
    BinaryNode[Set | SetDelayed | UpSet | UpSetDelayed, _, _] |
    TernaryNode[TagSet | TagSetDelayed, _, _], LeafNode[Token`Semi, _, _], end_(*something not implicit Null*)}, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
Catch[
Module[{first},

  first = firstExplicitToken[end];

  If[FailureQ[first],
    Throw[{}]
  ];

  {
    SyntaxIssue["TopLevelDefinitionCompoundExpression", "Definition does not contain the end of the ``CompoundExpression``.", "Error",
      <|
        Source -> first[[3, Key[Source]]],
        ConfidenceLevel -> 0.75,
        "AdditionalDescriptions" -> {"Consider breaking up onto separate lines."},
        "CompoundExpressionSource" -> data[Source]
        (*FIXME: wrap parentheses CodeAction*)
      |>
    ]
  }
]]

topLevelChildIssues[InfixNode[CompoundExpression, _, data_]
  ,
  (*
  always report
  *)
  ignored_
] :=
  {SyntaxIssue["TopLevelCompoundExpression", "Unexpected ``CompoundExpression`` at top-level.", "Remark", <|
    Source -> data[Source],
    ConfidenceLevel -> 0.95,
    "AdditionalDescriptions" -> {"Consider breaking up onto separate lines."}
  |>]}

(*
Anything else, then warn

Specifically add a DidYouMean for / -> /@
*)
topLevelChildIssues[
  BinaryNode[Divide,
    {_, LeafNode[_, _, slashData_], _}
    ,
    data_
  ]
  ,
  KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
  {
    SyntaxIssue["TopLevelExpression", "Unexpected expression at top-level.", "Warning",
      <|
        Source -> slashData[Source],
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Replace ``/`` with ``/@``", ReplaceNode,
          <|
            Source -> slashData[Source],
            "ReplacementNode" -> LeafNode[Token`SlashAt, "/@", <||>]
          |>]
        }
      |>
    ]
  }

topLevelChildIssues[
  BinaryNode[Rule,
    {_, LeafNode[_, ruleStr_, ruleData_], _}
    ,
    data_
  ]
  ,
  KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
  {
    SyntaxIssue["TopLevelExpression", "Unexpected expression at top-level.", "Warning",
      <|
        Source -> ruleData[Source],
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Replace ``" <> ruleStr <> "`` with ``=``", ReplaceNode,
          <|
            Source -> ruleData[Source],
            "ReplacementNode" -> LeafNode[Token`Equal, "=", <||>]
          |>]
        }
      |>
    ]
  }

topLevelChildIssues[
  BinaryNode[RuleDelayed,
    {_, LeafNode[_, ruleDelayedStr_, ruleDelayedData_], _}
    ,
    data_
  ]
  ,
  KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
  {
    SyntaxIssue["TopLevelExpression", "Unexpected expression at top-level.", "Warning",
      <|
        Source -> ruleDelayedData[Source],
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Replace ``" <> ruleDelayedStr <> "`` with ``:=``", ReplaceNode,
          <|
            Source -> ruleDelayedData[Source],
            "ReplacementNode" -> LeafNode[Token`ColonEqual, ":=", <||>]
          |>]
        }
      |>
    ]
  }

(*
No need to issue warning for errors being strange
*)
topLevelChildIssues[ErrorNode[_, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

topLevelChildIssues[SyntaxErrorNode[_, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

topLevelChildIssues[AbstractSyntaxErrorNode[_, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

topLevelChildIssues[GroupMissingCloserNode[_, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
  {}

topLevelChildIssues[node:_[_, _, data_], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
Catch[
Module[{first},

  first = firstExplicitToken[node];

  If[FailureQ[first],
    Throw[{}]
  ];

  {SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning", <|
    Source -> first[[3, Key[Source]]],
    ConfidenceLevel -> 0.95
  |>]}
]]

(*
if not active, return no issues
*)
topLevelChildIssues[_, KeyValuePattern["WillReportToplevelIssues" -> False]] :=
  {}


(*
input: aggregate
*)
firstExplicitToken[node:_[_, _String, _]] := node
firstExplicitToken[CallNode[first_, _, _]] := firstExplicitToken[first]
(*
Do not descend into CodeNode
*)
firstExplicitToken[CodeNode[_, _, _]] := Failure["CannotFindFirstExplicitToken", <||>]
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
Module[{list, nodeListStack, operatorStack, x, issues, nodeList,
  peek, newIssues, flow, exprToReturn, res},
  
  list = listIn;

  nodeListStack = System`CreateDataStructure["Stack"];
  operatorStack = System`CreateDataStructure["Stack"];

  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  operatorStack["Push", None];

  issues = {};

  Do[ (* Do i *)
    
    x = list[[i]];
    
    res = recurse[x, operatorStack, nodeListStack];

    If[FailureQ[res],
      Throw[res]
    ];

    {exprToReturn, newIssues, flow} = res;

    If[newIssues != {},
      issues = issues ~Join~ newIssues
    ];

    Switch[flow,
      Continue,
        (*
        everything is fine
        *)
        peek = nodeListStack["Peek"];
        peek["Push", exprToReturn];
      ,
      Throw,
        cleanupStackShimMemoryLeak[];
        Throw[{list, issues}];
      ,
      Null,
        (*
        a directive
        already handled
        *)
        Null
      ,
      _,
        Throw[Failure["UnhandledFlow", <| "Flow" -> flow |>]]
    ]
    ,
    {i, 1, Length[list]}
  ]; (* Do i *)

  If[operatorStack["Length"] != 1,
    AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 0.70 |>]];
    cleanupStackShimMemoryLeak[];
    Throw[{list, issues}];
  ];
  If[operatorStack["Peek"] =!= None,
    AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 0.70 |>]];
    cleanupStackShimMemoryLeak[];
    Throw[{list, issues}];
  ];
  If[nodeListStack["Length"] != 1,
    AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 0.70 |>]];
    cleanupStackShimMemoryLeak[];
    Throw[{list, issues}];
  ];
  peek = nodeListStack["Peek"];
  nodeList = Normal[peek];

  cleanupStackShimMemoryLeak[];

  {nodeList, issues}
]]


recurse[CellNode[Cell, children_, data_], operatorStack_, nodeListStack_] :=
Catch[
Module[{exprsToReturn, issues, flows, res},

  res = recurse[#, operatorStack, nodeListStack]& /@ children;

  If[AnyTrue[res, FailureQ],
    Throw[SelectFirst[res, FailureQ]]
  ];

  exprsToReturn = res[[All, 1]];

  issues = Join @@ res[[All, 2]];

  flows = combinedFlows[res[[All, 3]]];

  {CellNode[Cell, exprsToReturn, data], issues, flows}
]]

(*
handle special because people love writing:

BeginPackage["Foo`"] ;

even though the ; is not doing anything
*)
recurse[CallNode[head:LeafNode[Symbol, "CompoundExpression", _], {child_, null:LeafNode[Symbol, "Null" ,_]}, data_], operatorStack_, nodeListStack_] :=
Catch[
Module[{exprToReturn, issues, flow, res},

  res = recurse[child, operatorStack, nodeListStack];

  If[FailureQ[res],
    Throw[res]
  ];

  {exprToReturn, issues, flow} = res;

  {CallNode[head, { exprToReturn, null }, data], issues, flow}
]]

(*
something solid
*)
recurse[x_, operatorStack_, nodeListStack_] :=
Catch[
Module[{exprToReturn, newIssues, flow},

  {exprToReturn, newIssues, flow} = process[x, operatorStack, nodeListStack];

  {exprToReturn, newIssues, flow}
]]



(*
BeginPackage["Foo`"]
*)
process[
  CallNode[LeafNode[Symbol, "BeginPackage", _], {
    context:LeafNode[String, _?contextQ, _] }, data_],
  operatorStack_,
  nodeListStack_
] :=
Catch[
Module[{},
  If[!$CurrentBatchMode,
    Throw[{Null, {}, Null}]
  ];
  operatorStack["Push", PackageNode[{context}, {}, <| Source -> sourceSpan[sourceStart[data[Source]], (*partially constructed Source*)Indeterminate] |>]];
  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  {Null, {}, Null}
]]

(*
BeginPackage["Foo`", need]
*)
process[
  CallNode[LeafNode[Symbol, "BeginPackage", _], {
    context:LeafNode[String, _?contextQ, _],
    need:LeafNode[String, _?contextQ, _] }, data_],
  operatorStack_,
  nodeListStack_
] :=
Catch[
Module[{},
  If[!$CurrentBatchMode,
    Throw[{Null, {}, Null}]
  ];
  operatorStack["Push", PackageNode[{context, CallNode[LeafNode[Symbol, "List", <||>], { need }, <||>]}, {}, <| Source -> sourceSpan[sourceStart[data[Source]], (*partially constructed Source*)Indeterminate] |>]];
  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  {Null, {}, Null}
]]

(*
BeginPackage["Foo`", {}]
*)
process[
  CallNode[LeafNode[Symbol, "BeginPackage", _], {
    LeafNode[String, _?contextQ, _],
    CallNode[LeafNode[Symbol, "List", _], {}, _] }, data_],
  operatorStack_,
  nodeListStack_
] :=
{Null, {SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <| Source -> data[Source], ConfidenceLevel -> 0.95 |> ]}, Throw}

(*
BeginPackage["Foo`", {need1, need2}]
*)
process[
  CallNode[LeafNode[Symbol, "BeginPackage", _], children:{
    LeafNode[String, _?contextQ, _],
    CallNode[LeafNode[Symbol, "List", _], { LeafNode[String, _?contextQ, _]... }, _] }, data_],
  operatorStack_,
  nodeListStack_
] :=
Catch[
Module[{},
  If[!$CurrentBatchMode,
    Throw[{Null, {}, Null}]
  ];
  operatorStack["Push", PackageNode[children, {}, <| Source -> sourceSpan[sourceStart[data[Source]], (*partially constructed Source*)Indeterminate] |>]];
  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  {Null, {}, Null}
]]

(*
Begin["`Private`"]
*)
process[
  CallNode[LeafNode[Symbol, "Begin", _], children:{
    LeafNode[String, _?contextQ, _]}, data_],
  operatorStack_,
  nodeListStack_
] :=
Catch[
Module[{},
  If[!$CurrentBatchMode,
    Throw[{Null, {}, Null}]
  ];
  operatorStack["Push", ContextNode[children, {}, <| Source -> sourceSpan[sourceStart[data[Source]], (*partially constructed Source*)Indeterminate] |>]];
  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  {Null, {}, Null}
]]

(*
System`Private`NewContextPath[{"Foo`"}]
*)
process[
  CallNode[LeafNode[Symbol, "System`Private`NewContextPath", _], children:{
    CallNode[LeafNode[Symbol, "List", _], {
      LeafNode[String, _?contextQ, _]... }, _] }, data_],
  operatorStack_,
  nodeListStack_
] :=
Catch[
Module[{},
  If[!$CurrentBatchMode,
    Throw[{Null, {}, Null}]
  ];
  operatorStack["Push", NewContextPathNode[children, {}, <| Source -> sourceSpan[sourceStart[data[Source]], (*partially constructed Source*)Indeterminate] |>]];
  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  {Null, {}, Null}
]]

(*
EndPackage[]
End[]
*)
process[
  x:CallNode[LeafNode[Symbol, "EndPackage" | "End" | "System`Private`RestoreContextPath", _], {}, data_],
  operatorStack_,
  nodeListStack_
] :=
Catch[
Module[{currentOperator, currentList, peek},
  If[!$CurrentBatchMode,
    Throw[{Null, {}, Null}]
  ];
  currentOperator = operatorStack["Pop"];
  If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
    Throw[{Null, {SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> data[Source], ConfidenceLevel -> 0.95 |> ]}, Throw}];
  ];
  currentList = nodeListStack["Pop"];
  currentOperator[[2]] = Normal[currentList];
  (* finish constructing Source *)
  currentOperator[[3, Key[Source]]] = sourceSpan[sourceStart[currentOperator[[3, Key[Source]]]], sourceEnd[data[Source]]];
  peek = nodeListStack["Peek"];
  peek["Push", currentOperator];
  {Null, {}, Null}
]]

(*
All other calls to recognized directives

GroupMissingCloserNode
*)
process[
  CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], {
    GroupMissingCloserNode[_, _, _] }, _],
  operatorStack_,
  nodeListStack_
] :=
Module[{},
  (*
  if GroupMissingCloserNode, then do not complain
  *)
  {Null, {}, Throw}
]

process[
  CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], _, data_],
  operatorStack_,
  nodeListStack_
] :=
Module[{},
  {Null, {SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <| Source -> data[Source], ConfidenceLevel -> 0.70 |> ]}, Throw}
]

(*
a,b  at top-level is an error
*)
process[
  CallNode[LeafNode[Symbol, "CodeParser`Comma", _], children_, data_],
  operatorStack_,
  nodeListStack_
] :=
Module[{error},
  error = AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, children, data];
  {error, {}, Continue}
]

(*
Format[a] := b  at top-level

insert "AdditionalDefinitions" metadata for a

*)
process[
  CallNode[head:LeafNode[Symbol, "Set" | "SetDelayed", _], children:{
    CallNode[LeafNode[Symbol, "Attributes" | "Format" | "Options", _], {child1_}, _], _}, data_] /; DefinitionSymbols[child1] != {},
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "AdditionalDefinitions" -> DefinitionSymbols[child1] |> ];
  {def, {}, Continue}
]

(*
MessageName[a, b] := c  at top-level

insert "AdditionalDefinitions" metadata for a

*)
process[
  CallNode[head:LeafNode[Symbol, "Set" | "SetDelayed", _], children:{
    CallNode[LeafNode[Symbol, "MessageName", _], {child1_, ___}, _], _}, data_] /; DefinitionSymbols[child1] != {},
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "AdditionalDefinitions" -> DefinitionSymbols[child1] |> ];
  {def, {}, Continue}
]

(*
foo[] := 1+1  at top-level

insert "Definitions" metadata for foo

*)
process[
  CallNode[head:LeafNode[Symbol, "Set" | "SetDelayed", _], children:{child1_, _}, data_] /; DefinitionSymbols[child1] != {},
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "Definitions" -> DefinitionSymbols[child1] |> ];
  {def, {}, Continue}
]

(*
foo /: bar[foo] := 1+1  at top-level

insert:
"Definitions" metadata for foo
"AdditionalDefinitions" metadata for bar


*)
process[
  CallNode[head:LeafNode[Symbol, "TagSet" | "TagSetDelayed", _], children:{child1_, child2_, _}, data_] /; DefinitionSymbols[child1] != {} || DefinitionSymbols[child2] != {},
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "Definitions" -> DefinitionSymbols[child1], "AdditionalDefinitions" -> DefinitionSymbols[child2] |> ];
  {def, {}, Continue}
]

(*
foo[bar[]] ^:= 1+1  at top-level

insert:
"Definitions" metadata for bar
"AdditionalDefinitions" metadata for foo

*)
process[
  CallNode[head:LeafNode[Symbol, "UpSet" | "UpSetDelayed", _], children:{
    CallNode[child1_, children1_, _], _}, data_] /; DefinitionSymbols[child1] != {} || AnyTrue[children1, (DefinitionSymbols[#] != {})&],
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "AdditionalDefinitions" -> DefinitionSymbols[child1], "Definitions" -> Flatten[DefinitionSymbols /@ children1] |> ];
  {def, {}, Continue}
]

(*
foo =.  at top-level

insert "AdditionalDefinitions" metadata for foo

*)
process[
  CallNode[head:LeafNode[Symbol, "Unset", _], children:{child1_}, data_] /; DefinitionSymbols[child1] != {},
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "AdditionalDefinitions" -> DefinitionSymbols[child1] |> ];
  {def, {}, Continue}
]

(*
foo /: h[foo] =.  at top-level

insert "AdditionalDefinitions" metadata for foo

*)
process[
  CallNode[head:LeafNode[Symbol, "TagUnset", _], children:{child1_, child2_}, data_] /; DefinitionSymbols[child1] != {} || DefinitionSymbols[child2] != {},
  operatorStack_,
  nodeListStack_
] :=
Module[{def},
  def = CallNode[head, children, <| data, "AdditionalDefinitions" -> Flatten[DefinitionSymbols /@ {child1, child2}] |> ];
  {def, {}, Continue}
]

(*
All other expressions
*)
process[
  x_,
  operatorStack_,
  nodeListStack_
] :=
Module[{},
  {x, {}, Continue}
]



(*
For LineColumn convention, return the start or end
*)
sourceStart[{start:{_Integer, _Integer}, {_Integer, _Integer} | Indeterminate}] :=
  start

sourceEnd[{{_Integer, _Integer}, end:{_Integer, _Integer}}] :=
  end

sourceSpan[start:{_Integer, _Integer}, end:{_Integer, _Integer} | Indeterminate] :=
  {start, end}


(*
For SourceCharacterIndex convention, return the start or end
*)
sourceStart[{start:_Integer, _Integer}] :=
  start

sourceEnd[{_Integer, end:_Integer}] :=
  end

sourceSpan[start:_Integer, end:_Integer] :=
  {start, end}


sourceStart[Span[start_, _]] :=
  start

sourceEnd[Span[_, end_]] :=
  end


(*
For other conventions, just return the src

We do not know what to do
*)
sourceStart[src_] :=
  src

sourceEnd[src_] :=
  src

sourceSpan[start_, end_] :=
  Span[start, end]



(*

Match a string that is a context

tutorial/InputSyntax#6562

Symbol Names and Contexts

*)
contextQ[s_String] := StringMatchQ[s, RegularExpression["\"`?([a-zA-Z][a-zA-Z0-9]*`)+\""]]



matchingOperatorPatterns[CallNode[LeafNode[Symbol, "EndPackage", _], {}, _]] =
  _PackageNode

matchingOperatorPatterns[CallNode[LeafNode[Symbol, "End", _], {}, _]] =
  _ContextNode

matchingOperatorPatterns[CallNode[LeafNode[Symbol, "System`Private`RestoreContextPath", _], {}, _]] =
  _NewContextPathNode


End[]

EndPackage[]
