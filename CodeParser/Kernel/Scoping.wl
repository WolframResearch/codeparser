BeginPackage["CodeParser`Scoping`"]

ScopingData


scopingDataObject


freePatterns


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


ScopingData[astIn_] :=
  ScopingData[astIn, True&]

ScopingData[f_Failure, _] := f

ScopingData[astIn_, pred_] :=
Module[{ast, definitions},

  ast = astIn;

  Block[{$LexicalScope, $Data, $ExcludePatternNames, $ConditionPatternNames, $Definitions},
  
    (*
    $LexicalScope is an assoc of name -> scope
    *)
    $LexicalScope = <||>;

    (*
    $Data is an assoc of name -> {scopingDataObject[]}
    *)
    $Data = <||>;

    (*
    $ExcludePatternNames is a list of names
    *)
    $ExcludePatternNames = {};
    
    (*
    $ConditionPatternNames is a list of names
    *)
    $ConditionPatternNames = {};


    definitions = Flatten[Cases[ast, _[_, _, KeyValuePattern["Definitions" -> defs_]] :> defs, Infinity]];

    (*
    $Definitions is an assoc of name -> symbols
    *)
    $Definitions = GroupBy[definitions, #[[2]]&];

    $LexicalScope = <| (# -> {"Defined"})& /@ Keys[$Definitions] |>;


    walk[ast, pred];

    Flatten[Values[$Data]]
  ]
]


walk[ContainerNode[_, f_?FailureQ, _], pred_] :=
  {}

freePatterns[ContainerNode[_, f_?FailureQ, _]] :=
  {}


walk[ContainerNode[_, children_, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[children, pred]]

freePatterns[ContainerNode[_, children_, _]] := 
  Flatten[freePatterns /@ children]


walk[PackageNode[_, children_, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[children, pred]]

freePatterns[PackageNode[_, children_, _]] :=
  Flatten[freePatterns /@ children]


walk[ContextNode[_, children_, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[children, pred]]

freePatterns[ContextNode[_, children_, _]] :=
  Flatten[freePatterns /@ children]


walk[NewContextPathNode[_, children_, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[children, pred]]

freePatterns[NewContextPathNode[_, children_, _]] :=
  Flatten[freePatterns /@ children]



walk[CallNode[LeafNode[Symbol, "Module", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _], pred_] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      LeafNode[Symbol, name_, data1_] :> {{{name, data1[Source]}}, {}},
      (*
      This could also be made to use the Source of the Set[] itself but that may be strange
      *)
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {LeafNode[Symbol, name_, data2_], rhs_}, data1_] :> {{{name, data2[Source]}}, walk[rhs, pred]},
      _ :> {{}, {}}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]], 1];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]], 1];

    variableNames = #[[1]]& /@ variableSymbols;

    newScope = <| (# -> {"Module"})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, variableSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, variableNames]
  ]
]


optPat = CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, _}, _]

(*
DynamicModule can have options

The options have the same scope as the body, they understand the local variables
*)
walk[CallNode[LeafNode[Symbol, "DynamicModule", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_, optSeq:optPat...}, _], pred_] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring, optOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      LeafNode[Symbol, name_, data1_] :> {{{name, data1[Source]}}, {}},
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {LeafNode[Symbol, name_, data2_], rhs_}, data1_] :> {{{name, data2[Source]}}, walk[rhs, pred]},
      _ :> {{}, {}}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]], 1];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]], 1];

    variableNames = #[[1]]& /@ variableSymbols;

    newScope = <| (# -> {"DynamicModule"})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    optOccurring = Flatten[walk[#, pred]& /@ Select[{optSeq}, pred]];

    Scan[add[#[[1]], #[[2]], bodyOccurring ~Join~ optOccurring]&, variableSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring ~Join~ optOccurring, variableNames]
  ]
]

walk[CallNode[LeafNode[Symbol, tag: "Block" | "Internal`InheritedBlock", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _], pred_] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring, usedHeuristics,
  variableNamesLocalized, variableNamesNotLocalized},

  (*

  Now we will use heuristics to pare down the list of unused variables in Block

  if you have Block[{x = 1}, b]  then it is probably on purpose
  i.e., setting x to a value shows intention

  Blocking fully-qualified symbol is probably on purpose

  after removing fully-qualified symbols, now scan for lowercase symbols and only let those through

  on the assumption that lowercase symbols will be treated as "local" variables
  *)
  usedHeuristics = <||>;

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      LeafNode[Symbol, name_, data1_] :> (If[fullyQualifiedSymbolNameQ[name] || uppercaseOrDollarSymbolNameQ[name], usedHeuristics[{name, data1[Source]}] = True];{{{name, data1[Source]}}, {}}),
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {LeafNode[Symbol, name_, data2_], rhs_}, data1_] :> (usedHeuristics[{name, data2[Source]}] = True;{{{name, data2[Source]}}, walk[rhs, pred]}),
      _ :> {{}, {}}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]], 1];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]], 1];

    variableNames = #[[1]]& /@ variableSymbols;

    newScope = <| (# -> {tag})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring || Lookup[usedHeuristics, Key[#], False]]&, variableSymbols];

    (*
    Actually only filter out occurrences of variable names that are not localized

    In the form:
    Module[{x}, Block[{x = 2}, x]]

    the x in the Block has actually been localized to x$1234

    I'm not sure if testing for {___, "Module", "Block"} is fully general

    Related bugs: 414554
    *)
    variableNamesLocalized = Select[variableNames, MatchQ[Lookup[$LexicalScope, #], {___, "Module", "Block"}]&];
    variableNamesNotLocalized = Complement[variableNames, variableNamesLocalized];

    rhsOccurring ~Join~ Complement[bodyOccurring, variableNamesNotLocalized]
  ]
]

(*
if there is a ` anywhere in the symbol, then assume it is fully-qualified
*)
fullyQualifiedSymbolNameQ[s_String] :=
  StringContainsQ[s, "`"]

uppercaseOrDollarSymbolNameQ[s_String] :=
  StringMatchQ[s, RegularExpression["[A-Z\\$].*"]]


(*
With base case
*)
walk[CallNode[LeafNode[Symbol, "With", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _], pred_] :=
Module[{paramSymbolsAndRHSOccurring, paramSymbols, rhsOccurring, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndRHSOccurring = Replace[vars, {
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {LeafNode[Symbol, name_, data1_], rhs_}, _] :> {{{name, data1[Source]}}, walk[rhs, pred]},
      _ :> {{}, {}}
    }, 1];

    paramSymbols = Flatten[paramSymbolsAndRHSOccurring[[All, 1]], 1];
    rhsOccurring = Flatten[paramSymbolsAndRHSOccurring[[All, 2]], 1];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {"With"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "With", _], {
  vars:CallNode[LeafNode[Symbol, "List", _], _, _],
  varsRestSeq:PatternSequence[
    CallNode[LeafNode[Symbol, "List", _], _, _],
    CallNode[LeafNode[Symbol, "List", _], _, _]...],
  body_}, data_]
  ,
  pred_
] :=
Module[{newBody, paramSymbolsAndRHSOccurring, paramSymbols, rhsOccurring, paramNames, newScope, bodyOccurring},

  newBody = CallNode[LeafNode[Symbol, "With", <||>], {varsRestSeq} ~Join~ {body}, data];

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndRHSOccurring = Replace[vars[[2]], {
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {LeafNode[Symbol, name_, data2_], rhs_}, data1_] :> {{{name, data2[Source]}}, walk[rhs, pred]},
      _ :> {{}, {}}
    }, 1];

    paramSymbols = Flatten[paramSymbolsAndRHSOccurring[[All, 1]], 1];
    rhsOccurring = Flatten[paramSymbolsAndRHSOccurring[[All, 2]], 1];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {"With"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[newBody, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[LeafNode[Symbol, tag : "SetDelayed" | "RuleDelayed" | "UpSetDelayed", _], {lhs_, rhs_}, _], pred_] :=
Module[{patterns, lhsOccurring, patternSymbols, patternAssoc, patternNames, newScope, rhsOccurring, conditionOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs, pred];

      conditionOccurring = walkCondition[lhs];
    ];

    patternSymbols = Replace[patterns, {
      CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, name_, data1_], _}, _] :> {name, data1[Source]}
    }, 1];

    patternAssoc = GroupBy[patternSymbols, #[[1]]&];

    patternNames = Keys[patternAssoc];

    newScope = <| (# -> {tag})& /@ patternNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs, pred];

    KeyValueMap[
      Function[{name, syms},
        Which[
          Length[syms] > 1,
            (*
            non-linear pattern, so mark all as used
            *)
            Scan[add[#[[1]], #[[2]], True]&, syms]
          ,
          !MemberQ[$ExcludePatternNames, name],
            add[syms[[1, 1]], syms[[1, 2]], rhsOccurring ~Join~ conditionOccurring]
        ]
      ]
      ,
      patternAssoc
    ];

    lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]

freePatterns[CallNode[LeafNode[Symbol, "SetDelayed" | "RuleDelayed" | "UpSetDelayed", _], {lhs_, rhs_}, _]] := 
  freePatterns[rhs]


walk[CallNode[LeafNode[Symbol, "TagSetDelayed", _], {tag:LeafNode[Symbol, _, _], lhs_, rhs_}, _], pred_] :=
Module[{tagOccurring, patterns, lhsOccurring, patternSymbols, patternAssoc, patternNames, newScope, rhsOccurring, conditionOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    tagOccurring = walk[tag, pred];

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs, pred];

      conditionOccurring = walkCondition[lhs];
    ];

    patternSymbols = Replace[patterns, {
      CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, name_, data1_], _}, _] :> {name, data1[Source]}
    }, 1];

    patternAssoc = GroupBy[patternSymbols, #[[1]]&];

    patternNames = Keys[patternAssoc];

    newScope = <| (# -> {"TagSetDelayed"})& /@ patternNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs, pred];

    KeyValueMap[
      Function[{name, syms},
        Which[
          Length[syms] > 1,
            (*
            non-linear pattern, so mark all as used
            *)
            Scan[add[#[[1]], #[[2]], True]&, syms]
          ,
          !MemberQ[$ExcludePatternNames, name],
            add[syms[[1, 1]], syms[[1, 2]], rhsOccurring ~Join~ conditionOccurring]
        ]
      ]
      ,
      patternAssoc
    ];

    tagOccurring ~Join~ lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]

freePatterns[CallNode[LeafNode[Symbol, "TagSetDelayed", _], {LeafNode[Symbol, _, _], lhs_, rhs_}, _]] := 
  freePatterns[rhs]


walk[CallNode[LeafNode[Symbol, "Function", _], {body_}, _], pred_] :=
Module[{newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    newScope = <| $slotName -> {"SlotFunction"} |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Complement[bodyOccurring, {$slotName}]
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], {LeafNode[Symbol, "Null", _], body_, attrs:(PatternSequence[] | _)}, _], pred_] :=
Module[{newScope, bodyOccurring, attrsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    attrsOccurring = Flatten[walk[#, pred]& /@ Select[{attrs}, pred]];

    newScope = <| $slotName -> {"SlotFunction"} |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Complement[bodyOccurring, {$slotName}] ~Join~ attrsOccurring
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], {LeafNode[Symbol, name_, data_], body_, attrs:(PatternSequence[] | _)}, _], pred_] :=
Module[{newScope, bodyOccurring, attrsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    attrsOccurring = Flatten[walk[#, pred]& /@ Select[{attrs}, pred]];

    newScope = <| (name -> {"Function"}) |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    add[name, data[Source], bodyOccurring];

    Complement[bodyOccurring, {name}] ~Join~ attrsOccurring
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], {CallNode[LeafNode[Symbol, "List", _], params:{LeafNode[Symbol, _, _]...}, _], body_, attrs:(PatternSequence[] | _)}, _], pred_] :=
Module[{paramSymbols, paramNames, newScope, bodyOccurring, attrsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    attrsOccurring = Flatten[walk[#, pred]& /@ Select[{attrs}, pred]];

    paramSymbols = Replace[params, {
      LeafNode[Symbol, name_, data1_] :> {name, data1[Source]}
    }, 1];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {"Function"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    Complement[bodyOccurring, paramNames] ~Join~ attrsOccurring
  ]
]


compiledFunctionTypePat = CallNode[LeafNode[Symbol, "Typed", _], {LeafNode[Symbol, _, _], _}, _]

walk[CallNode[LeafNode[Symbol, "Function", _], {CallNode[LeafNode[Symbol, "List", _], params:{compiledFunctionTypePat, compiledFunctionTypePat...}, _], body_, attrs:(PatternSequence[] | _)}, _], pred_] :=
Module[{paramSymbolsAndTypeOccurring, paramSymbols, typeOccurring, paramNames, newScope, bodyOccurring, attrsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndTypeOccurring = Replace[params, {
      CallNode[LeafNode[Symbol, "Typed", _], {LeafNode[Symbol, name_, data1_], type_}, _] :> {{{name, data1[Source]}}, walk[type, pred]}
    }, 1];

    paramSymbols = Flatten[paramSymbolsAndTypeOccurring[[All, 1]], 1];
    typeOccurring = Flatten[paramSymbolsAndTypeOccurring[[All, 2]], 1];

    attrsOccurring = Flatten[walk[#, pred]& /@ Select[{attrs}, pred]];
    
    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {"Function"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    typeOccurring ~Join~ Complement[bodyOccurring, paramNames] ~Join~ attrsOccurring
  ]
]


iterPat = CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _, _ | PatternSequence[], _ | PatternSequence[]} | {_}, _]

(*
Do | Table base case
*)
walk[CallNode[head:LeafNode[Symbol, tag : "Do" | "Table", _], {body_, iter:iterPat}, _], pred_] :=
Module[{paramSymbolsAndIterOccurring, paramSymbols, iterOccurring, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndIterOccurring =
      Replace[iter[[2]], {
        {LeafNode[Symbol, name_, data1_], max_} :> {{{name, data1[Source]}}, walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_, d_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred] ~Join~ walk[d, pred]},
        {max_} :> {{}, walk[max, pred]},
        _ :> {{}, {}}
      }, {0}];

    paramSymbols = paramSymbolsAndIterOccurring[[1]];
    iterOccurring = paramSymbolsAndIterOccurring[[2]];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {tag})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    iterOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[head:LeafNode[Symbol, tag : "Do" | "Table", _], {body_, iter:iterPat, iterRestSeq:PatternSequence[iterPat, iterPat...]}, data_], pred_] :=
Module[{newBody, paramSymbolsAndIterOccurring, paramSymbols, iterOccurring, paramNames, newScope, bodyOccurring},

  newBody = CallNode[LeafNode[Symbol, tag, <||>], {body} ~Join~ {iterRestSeq}, data];

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndIterOccurring =
      Replace[iter[[2]], {
        {LeafNode[Symbol, name_, data1_], max_} :> {{{name, data1[Source]}}, walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_, d_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred] ~Join~ walk[d, pred]},
        {max_} :> {{}, walk[max, pred]},
        _ :> {{}, {}}
      }, {0}];

    paramSymbols = paramSymbolsAndIterOccurring[[1]];
    iterOccurring = paramSymbolsAndIterOccurring[[2]];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {tag})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[newBody, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    iterOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

(*
Sum | Product | ParallelTable base case

Sum, Product, and ParallelTable can have options
*)
walk[CallNode[head:LeafNode[Symbol, tag : "Sum" | "Product" | "ParallelTable", _], {body_, iter:iterPat, optSeq:optPat...}, _], pred_] :=
Module[{paramSymbolsAndIterOccurring, paramSymbols, iterOccurring, paramNames, newScope, bodyOccurring, optOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndIterOccurring =
      Replace[iter[[2]], {
        {LeafNode[Symbol, name_, data1_], max_} :> {{{name, data1[Source]}}, walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_, d_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred] ~Join~ walk[d, pred]},
        {max_} :> {{}, walk[max, pred]},
        _ :> {{}, {}}
      }, {0}];

    paramSymbols = paramSymbolsAndIterOccurring[[1]];
    iterOccurring = paramSymbolsAndIterOccurring[[2]];

    optOccurring = Flatten[walk[#, pred]& /@ Select[{optSeq}, pred]];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {tag})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    iterOccurring ~Join~ Complement[bodyOccurring, paramNames] ~Join~ optOccurring
  ]
]

walk[CallNode[head:LeafNode[Symbol, tag : "Sum" | "Product" | "ParallelTable", _], {body_, iter:iterPat, iterRestSeq:PatternSequence[iterPat, iterPat...], optSeq:optPat...}, data_], pred_] :=
Module[{newBody, paramSymbolsAndIterOccurring, paramSymbols, iterOccurring, paramNames, newScope, bodyOccurring, optOccurring},

  newBody = CallNode[LeafNode[Symbol, head[[2]], <||>], {body} ~Join~ {iterRestSeq}, data];

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndIterOccurring =
      Replace[iter[[2]], {
        {LeafNode[Symbol, name_, data1_], max_} :> {{{name, data1[Source]}}, walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred]},
        {LeafNode[Symbol, name_, data1_], min_, max_, d_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred] ~Join~ walk[d, pred]},
        {max_} :> {{}, walk[max, pred]},
        _ :> {{}, {}}
      }, {0}];

    paramSymbols = paramSymbolsAndIterOccurring[[1]];
    iterOccurring = paramSymbolsAndIterOccurring[[2]];

    optOccurring = Flatten[walk[#, pred]& /@ Select[{optSeq}, pred]];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {tag})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[newBody, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    iterOccurring ~Join~ Complement[bodyOccurring, paramNames] ~Join~ optOccurring
  ]
]


rangePat = CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _, _}, _]

walk[CallNode[head:LeafNode[Symbol, tag : "Play" | "Plot", _], {body_, range:rangePat, optSeq:optPat...}, _], pred_] :=
Module[{paramSymbolsAndRangeOccurring, paramSymbols, rangeOccurring, paramNames, newScope, bodyOccurring, optOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndRangeOccurring =
      Replace[range[[2]], {
        {LeafNode[Symbol, name_, data1_], min_, max_} :> {{{name, data1[Source]}}, walk[min, pred] ~Join~ walk[max, pred]}
      }, {0}];

    paramSymbols = paramSymbolsAndRangeOccurring[[1]];
    rangeOccurring = paramSymbolsAndRangeOccurring[[2]];

    optOccurring = Flatten[walk[#, pred]& /@ Select[{optSeq}, pred]];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {tag})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    rangeOccurring ~Join~ Complement[bodyOccurring, paramNames] ~Join~ optOccurring
  ]
]


compileTypePat = LeafNode[Symbol, _, _] | CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], Repeated[_, {0, 2}]}, _]

walk[CallNode[LeafNode[Symbol, "Compile", _], {CallNode[LeafNode[Symbol, "List", _], types:{compileTypePat, compileTypePat...}, _], body_, optionsAndPatterns___}, _], pred_] :=
Module[{paramSymbolsAndTypeOccurring, paramSymbols, typeOccurring, paramNames, newScope, bodyOccurring, optionsAndPatternsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndTypeOccurring =
      Function[{type},
        Replace[type, {
          LeafNode[Symbol, name_, data1_] :> {{{name, data1[Source]}}, {}},
          CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, name_, data1_]}, _] :> {{{name, data1[Source]}}, {}},
          CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, name_, data1_], pattern_}, _] :> {{{name, data1[Source]}}, walk[pattern, pred]},
          CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, name_, data1_], pattern_, rank_}, _] :> {{{name, data1[Source]}}, walk[pattern, pred] ~Join~ walk[rank, pred]},
          _ :> {{}, {}}
        }, {0}]
      ] /@ types;

    paramSymbols = Flatten[paramSymbolsAndTypeOccurring[[All, 1]], 1];
    typeOccurring = Flatten[paramSymbolsAndTypeOccurring[[All, 2]], 1];

    optionsAndPatternsOccurring = Flatten[walk[#, pred]& /@ Select[{optionsAndPatterns}, pred]];

    paramNames = #[[1]]& /@ paramSymbols;

    newScope = <| (# -> {"Compile"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body, pred];

    Scan[add[#[[1]], #[[2]], bodyOccurring]&, paramSymbols];

    typeOccurring ~Join~ Complement[bodyOccurring, paramNames] ~Join~ optionsAndPatternsOccurring
  ]
]



freePatterns[CallNode[LeafNode[Symbol, "Condition", _], {lhs_, rhs_}, _]] := 
  freePatterns[lhs]

walkCondition[CallNode[head:LeafNode[Symbol, "Condition", _], {lhs_, rhs_}, _]] :=
Module[{lhsOccurring, rhsOccurring},

  lhsOccurring = walkCondition[lhs];

  Internal`InheritedBlock[{$ConditionPatternNames},

    $ConditionPatternNames = $ConditionPatternNames ~Join~ $ExcludePatternNames;

    rhsOccurring = walkCondition[rhs];
  ];

  lhsOccurring ~Join~ rhsOccurring
]


walk[CallNode[LeafNode[Symbol, "Pattern", _], {lhs:LeafNode[Symbol, name_, _], rhs_}, _], pred_] :=
Module[{decls},

  Internal`InheritedBlock[{$LexicalScope},

    decls = Lookup[$LexicalScope, name, {}];

    If[MatchQ[decls, {___, "SetDelayed" | "RuleDelayed" | "TagSetDelayed" | "UpSetDelayed"}],
      (*
      If the pattern name is already bound to another pattern, then treat this as an error

      e.g.:
      f[x_] := g[x_]

      treat the 2nd x as an error
      *)
      decls = decls ~Join~ {"Error"};
      $LexicalScope[name] = decls
    ];

    If[!MemberQ[$ExcludePatternNames, name],
      walk[lhs, pred] ~Join~ walk[rhs, pred]
      ,
      walk[rhs, pred]
    ]
  ]
]

freePatterns[pat:CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, _, _], rhs_}, _]] := 
  Flatten[{pat} ~Join~ freePatterns[rhs]]


walk[n:CallNode[LeafNode[Symbol, "Slot" | "SlotSequence", _], _, data_], pred_] :=
Catch[
Module[{decls, entry},

  decls = Lookup[$LexicalScope, $slotName, {}];

  If[!empty[decls],

    (*
    Source may have been abstracted away
    *)
    If[KeyExistsQ[data, Source],

      entry = Lookup[$Data, $slotName, {}];

      AppendTo[entry,
        scopingDataObject[
          data[[Key[Source]]],
          decls,
          modifiersSet[decls, True],
          ToFullFormString[n]
        ]
      ];

      $Data[$slotName] = entry;
    ];

    Throw[{$slotName}]
  ];


  (*
  naked Slot
  *)
  (*
  Source may have been abstracted away
  *)
  If[KeyExistsQ[data, Source],

    entry = Lookup[$Data, $slotName, {}];

    AppendTo[entry,
      scopingDataObject[
        data[[Key[Source]]],
        decls,
        {"error"},
        ToFullFormString[n]
      ]
    ];

    $Data[$slotName] = entry;
  ];

  {$slotName}
]]


walk[CallNode[head_, children_, _], pred_] :=
  Flatten[Join[walk[head, pred], walk[#, pred]& /@ Select[children, pred]]]

freePatterns[CallNode[head_, children_, _]] := 
  Flatten[freePatterns[head] ~Join~ (freePatterns /@ children)]

walkCondition[CallNode[head_, children_, _]] :=
  Flatten[Join[walkCondition[head], walkCondition /@ children]]



walk[CellNode[Cell, children_, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[children, pred]]

freePatterns[CellNode[Cell, children_, _]] := 
  Flatten[freePatterns /@ children]

walkCondition[CellNode[Cell, children_, _]] :=
  Flatten[walkCondition /@ children]



walk[PrefixNode[PrefixLinearSyntaxBang, _, _], pred_] :=
  {}

freePatterns[PrefixNode[PrefixLinearSyntaxBang, _, _]] := 
  {}

walkCondition[PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
  {}



walk[sym:LeafNode[Symbol, name_, data_], pred_] :=
Catch[
Module[{decls, entry, defs},

  decls = Lookup[$LexicalScope, name, {}];

  defs = Lookup[$Definitions, name, {}];

  (*
  a definition
  *)
  If[MemberQ[defs, sym],

    (*
    Source may have been abstracted away
    *)
    If[KeyExistsQ[data, Source],

      entry = Lookup[$Data, name, {}];

      AppendTo[entry,
        scopingDataObject[
          data[[Key[Source]]],
          decls,
          {"definition"},
          name
        ]
      ];

      $Data[name] = entry;
    ];

    Throw[{name}]
  ];

  If[!empty[decls],

    (*
    Source may have been abstracted away
    *)
    If[KeyExistsQ[data, Source],

      entry = Lookup[$Data, name, {}];

      AppendTo[entry,
        scopingDataObject[
          data[[Key[Source]]],
          decls,
          modifiersSet[decls, True],
          name
        ]
      ];

      $Data[name] = entry;
    ];

    Throw[{name}]
  ];

  {}
]]

walkCondition[sym:LeafNode[Symbol, name_, data_]] :=
Module[{decls, entry},
  
  decls = Lookup[$LexicalScope, name, {}];

  decls = decls ~Join~ {"ThingThatUnderstandsCondition"};

  If[MemberQ[$ConditionPatternNames, name],

    entry = Lookup[$Data, name, {}];

    (*
    Remove any previous data

    We may have more knowledge now about the symbol that is in the Condition

    We may now know that it is shadowed

    So remove previous entries for the same symbol
    *)
    entry = DeleteCases[entry, scopingDataObject[data[[Key[Source]]], _, _, _]];

    AppendTo[entry,
      scopingDataObject[
        data[[Key[Source]]],
        decls,
        modifiersSet[decls, True],
        name
      ]
    ];

    $Data[name] = entry;

    {name}
    ,
    {}
  ]
]

walkCondition[LeafNode[_, _, _]] :=
  {}


add[name_, src_, True] :=
Module[{decls, entry},

  decls = Lookup[$LexicalScope, name, {}];

  entry = Lookup[$Data, name, {}];

  AppendTo[entry,
    scopingDataObject[
      src,
      decls,
      modifiersSet[decls, True],
      name
    ]
  ];

  $Data[name] = entry;
]

add[name_, src_, occurringScopedNames_] :=
Module[{decls, entry},

  decls = Lookup[$LexicalScope, name, {}];

  entry = Lookup[$Data, name, {}];

  AppendTo[entry,
    scopingDataObject[
      src,
      decls,
      modifiersSet[decls, MemberQ[occurringScopedNames, name]],
      name
    ]
  ];

  $Data[name] = entry;
]


walk[LeafNode[_, _, _], pred_] :=
  {}

freePatterns[LeafNode[_, _, _]] := 
  {}


walk[ErrorNode[_, _, _], pred_] :=
  {}

freePatterns[ErrorNode[_, _, _]] := 
  {}


walk[SyntaxErrorNode[_, _, _], pred_] :=
  {}

freePatterns[SyntaxErrorNode[_, _, _]] := 
  {}


walk[AbstractSyntaxErrorNode[_, _, _], pred_] :=
  {}

freePatterns[AbstractSyntaxErrorNode[_, _, _]] := 
  {}


walk[GroupMissingCloserNode[_, _, _], pred_] :=
  {}

freePatterns[GroupMissingCloserNode[_, _, _]] := 
  {}


walk[GroupMissingOpenerNode[_, _, _], pred_] :=
  {}

freePatterns[GroupMissingOpenerNode[_, _, _]] := 
  {}


walk[CallMissingCloserNode[_, _, _], pred_] :=
  {}

freePatterns[CallMissingCloserNode[_, _, _]] := 
  {}


(*
a is a List of boxes
*)
walk[BoxNode[RowBox, {a_}, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[a, pred]]

freePatterns[BoxNode[RowBox, {a_}, _]] :=
  Flatten[freePatterns /@ a]

walkCondition[BoxNode[RowBox, {a_}, _]] :=
  Flatten[walkCondition /@ a]

(*
a is a List of Lists
*)
walk[BoxNode[GridBox, {a_, ___}, _], pred_] := (
  (*
  FIXME:
  Select[a, pred, {2}] ??
  *)
  Flatten[Map[walk[#, pred]&, a, {2}]]
)

freePatterns[BoxNode[GridBox, {a_, ___}, _]] :=
  Flatten[Map[freePatterns, a, {2}]]

walkCondition[BoxNode[GridBox, {a_, ___}, _]] :=
  Flatten[Map[walkCondition, a, {2}]]



(*
Handle special un-abstracted form of [[x]] in subscript
*)
walk[
  BoxNode[SubscriptBox, {
    a_,
    GroupNode[GroupSquare, {
      _,
      GroupNode[GroupSquare, {
        _,
        b_,
        _}, _],
      _}, _],
    ___}, _]
  ,
  pred_
] := Flatten[Join[walk[a, pred], walk[b, pred]]]

freePatterns[
  BoxNode[SubscriptBox, {
    a_,
    GroupNode[GroupSquare, {
      _,
      GroupNode[GroupSquare, {
        _,
        b_,
        _}, _],
      _}, _],
    ___}, _]
] := Flatten[Join[freePatterns[a], freePatterns[b]]]

walkCondition[
  BoxNode[SubscriptBox, {
    a_,
    GroupNode[GroupSquare, {
      _,
      GroupNode[GroupSquare, {
        _,
        b_,
        _}, _],
      _}, _],
    ___}, _]
] := Flatten[Join[walkCondition[a], walkCondition[b]]]

walk[
  BoxNode[SubscriptBox, {
    a_,
    GroupNode[GroupDoubleBracket, {
      _,
      b_,
      _}, _],
    ___}, _]
  ,
  pred_
] := Flatten[Join[walk[a, pred], walk[b, pred]]]

freePatterns[
  BoxNode[SubscriptBox, {
    a_,
    GroupNode[GroupDoubleBracket, {
      _,
      b_,
      _}, _],
    ___}, _]
] := Flatten[Join[freePatterns[a], freePatterns[b]]]

walkCondition[
  BoxNode[SubscriptBox, {
    a_,
    GroupNode[GroupDoubleBracket, {
      _,
      b_,
      _}, _],
    ___}, _]
] := Flatten[Join[walkCondition[a], walkCondition[b]]]

(*
Handle special un-abstracted form of TagBox[(), Derivative] in superscript
*)
walk[
  BoxNode[SuperscriptBox, {
    a_,
    BoxNode[TagBox, {
      GroupNode[GroupParen, {
        _,
        b_,
        _}, _],
      CodeNode[Evaluated, Derivative, _]}, _],
    ___}, _]
  ,
  pred_
] := Flatten[Join[walk[a, pred], walk[b, pred]]]

freePatterns[
  BoxNode[SuperscriptBox, {
    a_,
    BoxNode[TagBox, {
      GroupNode[GroupParen, {
        _,
        b_,
        _}, _],
      CodeNode[Evaluated, Derivative, _]}, _],
    ___}, _]
] := Flatten[Join[freePatterns[a], freePatterns[b]]]

walkCondition[
  BoxNode[SuperscriptBox, {
    a_,
    BoxNode[TagBox, {
      GroupNode[GroupParen, {
        _,
        b_,
        _}, _],
      CodeNode[Evaluated, Derivative, _]}, _],
    ___}, _]
] := Flatten[Join[walkCondition[a], walkCondition[b]]]


walk[BoxNode[_, children_, _], pred_] :=
  Flatten[walk[#, pred]& /@ Select[children, pred]]

freePatterns[BoxNode[_, body_, _]] := 
  Flatten[freePatterns /@ body]

walkCondition[BoxNode[_, children_, _]] :=
  Flatten[walkCondition /@ children]


(*
Do not touch CodeNode
*)
walk[CodeNode[_, _, _], pred_] :=
  {}

freePatterns[CodeNode[_, _, _]] :=
  {}

walkCondition[CodeNode[_, _, _]] :=
  {}



(*
modifiersSet[decls_, used_]
*)
modifiersSet[{___, "Error"}, _] :=
  {"error"}

(*
Handle the common case of entering:

foo[a_] :=
Module[{a},
  xxx
]

The pattern and the Module variable have the same name

FIXME: I should probably do more to handle more of these errors: errors of Module variables shadowing patterns

*)
modifiersSet[{___, "SetDelayed" | "RuleDelayed" | "TagSetDelayed" | "UpSetDelayed", "Module" | "Block" | "With" | "DynamicModule" | "Internal`InheritedBlock"}, _] :=
  {"error"}


modifiersSet[{_}, False] :=
  {"unused"}
modifiersSet[{_}, True] :=
  {}
modifiersSet[_, False] :=
  {"unused", "shadowed"}
modifiersSet[_, True] :=
  {"shadowed"}



End[]

EndPackage[]
