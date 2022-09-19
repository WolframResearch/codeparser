(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`AbstractCallNode`"]

abstractCallNode


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]
Needs["CodeParser`Utils`"]


(*
These boxes are ok to have as head of calls

What is the process for adding boxes to this list?

It's on-demand as case-by-case basis
*)
$okCallBoxPat = TemplateBox | InterpretationBox | SubscriptBox | SuperscriptBox | StyleBox | NamespaceBox | OverscriptBox | SubsuperscriptBox



(*

concrete parse of a[[2]] returns CallNode[a, GroupNode[Square, {GroupNode[Square, {2}]}]]
abstract parse of a[[2]] returns CallNode[Part, {a, 2}]

So convert from concrete [[ syntax to abstract Part syntax

*)
abstractCallNode[CallNode[headIn_, c:GroupNode[GroupSquare, {first_, inner:GroupNode[GroupSquare, _, _], last_}, _], dataIn_]] :=
Module[{head, data, part, issues},

  head = headIn;
  data = dataIn;
  part = inner;
  issues = {};

  Switch[head,
    (*
    feel strongly about ##2[[arg]]
    ##2 represents a sequence of arguments, so it is wrong to call
    *)
    LeafNode[Token`HashHash, _, _] | CompoundNode[SlotSequence, _, _],
      AppendTo[issues,

        SyntaxIssue["StrangeCallSlotSequence", "Unexpected ``Part`` call.", "Error", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 1.0,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    LeafNode[Symbol (* | String *) | Token`Hash | Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _] | _CallNode |
      CompoundNode[Blank | BlankSequence | BlankNullSequence | PatternBlank | PatternBlankSequence | PatternBlankNullSequence | Slot (* | SlotSequence *), _, _],
      (* these are fine *)
      Null
    ,
    LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _],
      (*
      was:

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];

      but % is already scanned in CodeInspector TokenRules, and this just adds more noise

      *)

      (* these are fine *)
      Null
    ,
    LeafNode[Token`LinearSyntaxBlob, _, _],

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    PrefixNode[PrefixLinearSyntaxBang, _, _],

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    InfixNode[CompoundExpression, _, _],
      
      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
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

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    PostfixNode[Transpose, _, _],
    (*
    a\[Transpose][[2]] is fine
    *)
    Null
    ,
    (*
    Now handle boxes
    *)
    BoxNode[$okCallBoxPat, _, _],
      (* this is fine *)
      Null
    ,
    BoxNode[_, _, _],
      
      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call: ``" <> ToString[head[[1]]] <> "``.", "Error", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    _,
      (*
      warn about anything else
      *)

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Error", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
  ];

  head = abstract[head];
  part = abstractGroupNode[part];

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
abstractCallNode[CallNode[headIn:LeafNode[Token`HashHash, _, _] | CompoundNode[SlotSequence, _, _], partIn:GroupNode[GroupSquare, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  AppendTo[issues,
    SyntaxIssue["StrangeCallSlotSequence", "Unexpected call.", "Error", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 1.0,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
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

abstractCallNode[CallNode[headIn:LeafNode[Symbol | String | Token`Hash | Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _] | _CallNode |
       CompoundNode[Blank | BlankSequence | BlankNullSequence | PatternBlank | PatternBlankSequence | PatternBlankNullSequence | Slot (*| SlotSequence*), _, _], partIn:GroupNode[GroupSquare, _, _], dataIn_]] :=
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

abstractCallNode[CallNode[headIn:LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _], partIn:GroupNode[GroupSquare, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  (*
  was:
  AppendTo[issues,
    SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
  ];

  but % is already scanned in CodeInspector TokenRules, and this just adds more noise
  *)

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

abstractCallNode[CallNode[headIn:BinaryNode[PatternTest, _, _], partIn:GroupNode[GroupSquare, _, _], dataIn_]] :=
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

abstractCallNode[CallNode[headIn:InfixNode[CompoundExpression, _, _], partIn:GroupNode[GroupSquare, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  AppendTo[issues,
    SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
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
these are fine
List is allowed because this is popular to do:
Through[{a, b, c}[1]]
*)
abstractCallNode[CallNode[headIn:GroupNode[GroupParen | List | Association, _, _], partIn:GroupNode[GroupSquare, _, _], dataIn_]] :=
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

abstractCallNode[CallNode[headIn:GroupNode[_, _, _], partIn:GroupNode[GroupSquare, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  AppendTo[issues,
    SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
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

(* these are fine *)
abstractCallNode[CallNode[headIn:PostfixNode[Function | Derivative, _, _], partIn:GroupNode[GroupSquare, _, _], dataIn_]] :=
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
abstractCallNode[CallNode[headIn:BoxNode[$okCallBoxPat, _, _], partIn:GroupNode[GroupSquare, _, _], dataIn_]] :=
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

abstractCallNode[CallNode[headIn:BoxNode[tag_, _, _], partIn:GroupNode[GroupSquare, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  AppendTo[issues,
    SyntaxIssue["StrangeCall", "Unexpected call: ``" <> ToString[tag] <> "``.", "Error", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
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
warn about anything else
*)
abstractCallNode[CallNode[headIn_, partIn:GroupNode[GroupSquare, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  AppendTo[issues,
    SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
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
this is fine
*)
abstractCallNode[CallNode[headIn:LeafNode[String, _, _], partIn:GroupNode[GroupTypeSpecifier, {first_, ___, last_}, _], dataIn_]] :=
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

  CallNode[CallNode[ToNode[System`TypeSpecifier], {head}, <||>], part[[2]], data]
]

(*
warn about anything else
*)
abstractCallNode[CallNode[headIn_, partIn:GroupNode[GroupTypeSpecifier, {first_, ___, last_}, _], dataIn_]] :=
Module[{head, part, partData, issues, data},
  head = headIn;
  part = partIn;
  data = dataIn;

  issues = {};

  AppendTo[issues,
    SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|
      Source -> first[[3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      "AdditionalSources" -> {last[[3, Key[Source]]]}
    |>]
  ];

  head = abstract[head];
  part = abstractGroupNode[part];
  partData = part[[3]];

  issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

  If[issues != {},
    issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
    AssociateTo[data, AbstractSyntaxIssues -> issues];
  ];

  CallNode[CallNode[ToNode[System`TypeSpecifier], {head}, <||>], part[[2]], data]
]


(*

Concrete parse of a\[LeftDoubleBracket]2\[RightDoubleBracket] returns CallNode[a, GroupNode[DoubleBracket, {2}]]
abstract parse of a\[LeftDoubleBracket]2\[RightDoubleBracket] returns CallNode[Part, {a, 2}]

*)
abstractCallNode[CallNode[headIn_, partIn:GroupNode[GroupDoubleBracket, {first_, ___, last_}, _], dataIn_]] :=
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
    LeafNode[Token`HashHash, _, _] | CompoundNode[SlotSequence, _, _],

      AppendTo[issues,
        SyntaxIssue["StrangeCallSlotSequence", "Unexpected call.", "Error", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 1.0,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    LeafNode[Symbol (* | String *) | Token`Hash | Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _] | _CallNode |
      CompoundNode[Blank | BlankSequence | BlankNullSequence | PatternBlank | PatternBlankSequence | PatternBlankNullSequence | Slot (* | SlotSequence *), _, _],
      (* these are fine *)
      Null
    ,
    LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _],
      (*
      was:

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];

      but % is already scanned in CodeInspector TokenRules, and this just adds more noise
      *)

      (* these are fine *)
      Null
    ,
    LeafNode[Token`LinearSyntaxBlob, _, _],

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected call.", "Remark", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    PrefixNode[PrefixLinearSyntaxBang, _, _],

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected call.", "Remark", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    (*
    BinaryNode[PatternTest, _, _],
      (* these are fine *)
      Null
    ,*)
    InfixNode[CompoundExpression, _, _],
      
      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    GroupNode[GroupParen | List | Association, _, _],
      (* these are fine *)
      Null
    ,
    GroupNode[_, _, _],

      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>]
      ];
    ,
    PostfixNode[Transpose, _, _],
      (*
      a\[Transpose]\[LeftDoubleBracket]2\[RightDoubleBracket] is fine
      *)
      Null
    ,
    (*
    Now handle boxes
    *)
    BoxNode[$okCallBoxPat, _, _],
      (* this is fine *)
      Null
    ,
    BoxNode[_, _, _],
      AppendTo[issues,
        SyntaxIssue["StrangeCall", "Unexpected call: ``" <> ToString[head[[1]]] <> "``.", "Error", <|
          Source -> first[[3, Key[Source]]],
          ConfidenceLevel -> 0.95,
          "AdditionalSources" -> {last[[3, Key[Source]]]}
        |>
      ]
    ];
    ,
    _,
    (*
    warn about anything else
    *)

    AppendTo[issues,
      SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|
        Source -> first[[3, Key[Source]]],
        ConfidenceLevel -> 0.95,
        "AdditionalSources" -> {last[[3, Key[Source]]]}
      |>]
    ];
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



abstractCallNode[CallMissingCloserNode[headIn_, partIn:GroupMissingCloserNode[GroupSquare, _, _], dataIn_]] :=
Module[{head, part, data, issues, partData},
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

abstractCallNode[CallMissingCloserNode[headIn_, partIn:GroupMissingCloserNode[GroupTypeSpecifier, _, _], dataIn_]] :=
Module[{head, part, data, issues, partData},
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

  CallMissingCloserNode[CallNode[ToNode[System`TypeSpecifier], {head}, <||>], part[[2]], data]
]

abstractCallNode[CallMissingCloserNode[headIn_, partIn:GroupMissingCloserNode[GroupDoubleBracket, _, _], dataIn_]] :=
Module[{head, part, data, issues, partData},
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

  CallMissingCloserNode[ToNode[Part], {head} ~Join~ part[[2]], data]
]


abstractCallNode[args___] :=
  Failure["Unhandled", <| "Function" -> abstractCallNode, "Arguments" -> HoldForm[{args}] |>]


End[]

EndPackage[]
