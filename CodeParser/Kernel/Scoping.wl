BeginPackage["CodeParser`Scoping`"]

ScopingData

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


ScopingData[ast_] :=
Module[{},

  Block[{$LexicalScope, $Data, $ExcludePatternNames},
  
    (*
    $LexicalScope is an assoc of names -> decls
    *)
    $LexicalScope = <||>;

    (*
    $Data is a bag of {line, startChar, len, tokenType, tokenModifiers}
    *)
    $Data = Internal`Bag[];

    (*
    $ExcludePatternNames is a list of names
    *)
    $ExcludePatternNames = {};
    
    walk[ast];

    Internal`BagPart[$Data, All]
  ]
]


walk[ContainerNode[_, children_, _]] :=
  Flatten[walk /@ children]

freePatterns[ContainerNode[_, children_, _]] := 
  Flatten[freePatterns /@ children]


walk[PackageNode[_, children_, _]] :=
  Flatten[walk /@ children]

freePatterns[PackageNode[_, children_, _]] :=
  Flatten[freePatterns /@ children]


walk[ContextNode[_, children_, _]] :=
  Flatten[walk /@ children]

freePatterns[ContextNode[_, children_, _]] :=
  Flatten[freePatterns /@ children]


walk[NewContextPathNode[_, children_, _]] :=
  Flatten[walk /@ children]

freePatterns[NewContextPathNode[_, children_, _]] :=
  Flatten[freePatterns /@ children]



walk[CallNode[LeafNode[Symbol, "Module", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _]] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      sym:LeafNode[Symbol, _, _] :> {{sym}, {}},
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], rhs_}, _] :> {{lhs}, walk[rhs]},
      _ :> {}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]]];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]]];

    variableNames = #[[2]]& /@ variableSymbols;

    newScope = <| (# -> {"Module"})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, variableSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, variableNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "DynamicModule", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _]] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      sym:LeafNode[Symbol, _, _] :> {{sym}, {}},
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], rhs_}, _] :> {{lhs}, walk[rhs]},
      _ :> {}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]]];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]]];

    variableNames = #[[2]]& /@ variableSymbols;

    newScope = <| (# -> {"Module"})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, variableSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, variableNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "Block", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _]] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      sym:LeafNode[Symbol, _, _] :> {{sym}, {}},
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], rhs_}, _] :> {{lhs}, walk[rhs]},
      _ :> {}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]]];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]]];

    variableNames = #[[2]]& /@ variableSymbols;

    newScope = <| (# -> {"Block"})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, variableSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, variableNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "Internal`InheritedBlock", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _]] :=
Module[{variableSymbolsAndRHSOccurring, variableSymbols, rhsOccurring, variableNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    variableSymbolsAndRHSOccurring = Replace[vars, {
      sym:LeafNode[Symbol, _, _] :> {{sym}, {}},
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], rhs_}, _] :> {{lhs}, walk[rhs]},
      _ :> {}
    }, 1];

    variableSymbols = Flatten[variableSymbolsAndRHSOccurring[[All, 1]]];
    rhsOccurring = Flatten[variableSymbolsAndRHSOccurring[[All, 2]]];

    variableNames = #[[2]]& /@ variableSymbols;

    newScope = <| (# -> {"Block"})& /@ variableNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, variableSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, variableNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "With", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], body_}, _]] :=
Module[{paramSymbolsAndRHSOccurring, paramSymbols, rhsOccurring, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndRHSOccurring = Replace[vars, {
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], rhs_}, _] :> {{lhs}, walk[rhs]},
      _ :> {}
    }, 1];

    paramSymbols = Flatten[paramSymbolsAndRHSOccurring[[All, 1]]];
    rhsOccurring = Flatten[paramSymbolsAndRHSOccurring[[All, 2]]];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (# -> {"parameter"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "With", _], children:{
  CallNode[LeafNode[Symbol, "List", _], vars_, _],
  CallNode[LeafNode[Symbol, "List", _], _, _],
  CallNode[LeafNode[Symbol, "List", _], _, _]..., body_}, data_]] :=
Module[{newBody, paramSymbolsAndRHSOccurring, paramSymbols, rhsOccurring, paramNames, newScope, bodyOccurring},

  newBody = CallNode[LeafNode[Symbol, "With", <||>], Rest[children] ~Join~ {body} , data];

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndRHSOccurring = Replace[vars, {
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], rhs_}, _] :> {{lhs}, walk[rhs]},
      _ :> {}
    }, 1];

    paramSymbols = Flatten[paramSymbolsAndRHSOccurring[[All, 1]]];
    rhsOccurring = Flatten[paramSymbolsAndRHSOccurring[[All, 2]]];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (# -> {"parameter"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[newBody];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    rhsOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "SetDelayed", _], {lhs_, rhs_}, _]] :=
Module[{patterns, lhsOccurring, patternSymbols, patternNames, newScope, rhsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs];
    ];

    patternSymbols = Replace[patterns, {
      pat:CallNode[LeafNode[Symbol, "Pattern", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym
    }, 1];

    patternNames = #[[2]]& /@ patternSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs];

    Scan[If[!MemberQ[$ExcludePatternNames, #[[2]]], add[#, rhsOccurring]]&, patternSymbols];

    lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]

freePatterns[CallNode[LeafNode[Symbol, "SetDelayed", _], {lhs_, rhs_}, _]] := 
  freePatterns[rhs]


walk[CallNode[LeafNode[Symbol, "TagSetDelayed", _], {tag:LeafNode[Symbol, _, _], lhs_, rhs_}, _]] :=
Module[{tagOccurring, patterns, lhsOccurring, patternSymbols, patternNames, newScope, rhsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    tagOccurring = walk[tag];

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs];
    ];

    patternSymbols = Replace[patterns, {
      pat:CallNode[LeafNode[Symbol, "Pattern", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym
    }, 1];

    patternNames = #[[2]]& /@ patternSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs];

    Scan[If[!MemberQ[$ExcludePatternNames, #[[2]]], add[#, rhsOccurring]]&, patternSymbols];

    tagOccurring ~Join~ lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]

freePatterns[CallNode[LeafNode[Symbol, "TagSetDelayed", _], {LeafNode[Symbol, _, _], lhs_, rhs_}, _]] := 
  freePatterns[rhs]


walk[CallNode[LeafNode[Symbol, "UpSetDelayed", _], {lhs_, rhs_}, _]] :=
Module[{patterns, lhsOccurring, patternSymbols, patternNames, newScope, rhsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs];
    ];

    patternSymbols = Replace[patterns, {
      pat:CallNode[LeafNode[Symbol, "Pattern", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym
    }, 1];

    patternNames = #[[2]]& /@ patternSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs];

    Scan[If[!MemberQ[$ExcludePatternNames, #[[2]]], add[#, rhsOccurring]]&, patternSymbols];

    lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]

freePatterns[CallNode[LeafNode[Symbol, "UpSetDelayed", _], {LeafNode[Symbol, _, _], lhs_, rhs_}, _]] := 
  freePatterns[rhs]


walk[CallNode[LeafNode[Symbol, "RuleDelayed", _], {lhs_, rhs_}, _]] :=
Module[{patterns, lhsOccurring, patternSymbols, patternNames, newScope, rhsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs];
    ];

    patternSymbols = Replace[patterns, {
      pat:CallNode[LeafNode[Symbol, "Pattern", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym
    }, 1];

    patternNames = #[[2]]& /@ patternSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs];

    Scan[If[!MemberQ[$ExcludePatternNames, #[[2]]], add[#, rhsOccurring]]&, patternSymbols];

    lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]

freePatterns[CallNode[LeafNode[Symbol, "RuleDelayed", _], {lhs_, rhs_}, _]] :=
  freePatterns[rhs]


(* walk[CallNode[LeafNode[Symbol, "Condition", _], {lhs_, rhs_}, _]] :=
Module[{patterns, lhsOccurring, patternSymbols, patternNames, newScope, rhsOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    patterns = freePatterns[lhs];

    Internal`InheritedBlock[{$ExcludePatternNames},

      $ExcludePatternNames = $ExcludePatternNames ~Join~ (#[[2, 1, 2]]& /@ patterns);

      lhsOccurring = walk[lhs];
    ];

    patternSymbols = Replace[patterns, {
      pat:CallNode[LeafNode[Symbol, "Pattern", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym
    }, 1];

    patternNames = #[[2]]& /@ patternSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    rhsOccurring = walk[rhs];

    (* Scan[If[!MemberQ[$ExcludePatternNames, #[[2]]], add[#, rhsOccurring]]&, patternSymbols]; *)

    Scan[add[#, rhsOccurring]&, patternSymbols];

    lhsOccurring ~Join~ Complement[rhsOccurring, patternNames]
  ]
]
*)

(* freePatterns[CallNode[LeafNode[Symbol, "Condition", _], {lhs_, rhs_}, _]] :=
  freePatterns[lhs] *)


walk[CallNode[LeafNode[Symbol, "Function", _], {paramSymbol:LeafNode[Symbol, "Null", _], body_, PatternSequence[] | _}, _]] :=
  walk[body]

walk[CallNode[LeafNode[Symbol, "Function", _], {paramSymbol:LeafNode[Symbol, _, _], body_, PatternSequence[] | _}, _]] :=
Module[{paramName, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramName = paramSymbol[[2]];

    newScope = <| (paramName -> {"parameter"}) |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    add[paramSymbol, bodyOccurring];

    Complement[bodyOccurring, {paramName}]
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], {CallNode[LeafNode[Symbol, "List", _], params:{LeafNode[Symbol, _, _]...}, _], body_, PatternSequence[] | _}, _]] :=
Module[{paramSymbols, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbols = Replace[params, {
      sym:LeafNode[Symbol, _, _] :> sym
    }, 1];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (# -> {"parameter"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    Complement[bodyOccurring, paramNames]
  ]
]


compiledFunctionTypePat = CallNode[LeafNode[Symbol, "Typed", _], {LeafNode[Symbol, _, _], _}, _]

walk[CallNode[LeafNode[Symbol, "Function", _], {CallNode[LeafNode[Symbol, "List", _], params:{compiledFunctionTypePat, compiledFunctionTypePat...}, _], body_, PatternSequence[] | _}, _]] :=
Module[{paramSymbolsAndTypeOccurring, paramSymbols, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndTypeOccurring = Replace[params, {
      CallNode[LeafNode[Symbol, "Typed", _], {sym:LeafNode[Symbol, _, _], type_}, _] :> {{sym}, walk[type]}
    }, 1];

    paramSymbols = Flatten[paramSymbolsAndTypeOccurring[[All, 1]]];
    typeOccurring = Flatten[paramSymbolsAndTypeOccurring[[All, 2]]];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (# -> {"parameter"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    typeOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]



iterPat = CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _, _ | PatternSequence[], _ | PatternSequence[]} | {_}, _]


walk[CallNode[LeafNode[Symbol, "Do", _], children:{body_, iterPat, iterPat...}, _]] :=
Module[{paramSymbolsAndIterOccurring, paramSymbols, iterOccurring, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndIterOccurring =
      Function[{child},
        paramSymbols = Replace[child[[2]], {
          {sym:LeafNode[Symbol, _, _], max_} :> {{sym}, walk[max]},
          {sym:LeafNode[Symbol, _, _], min_, max_} :> {{sym}, walk[min] ~Join~ walk[max]},
          {sym:LeafNode[Symbol, _, _], min_, max_, d_} :> {{sym}, walk[min] ~Join~ walk[max] ~Join~ walk[d]},
          {max_} :> {{}, walk[max]},
          _ :> {{}, {}}
        }, {0}]
      ] /@ Rest[children];

    paramSymbols = Flatten[paramSymbolsAndIterOccurring[[All, 1]]];
    iterOccurring = Flatten[paramSymbolsAndIterOccurring[[All, 2]]];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ paramSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    iterOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "Do", _], children:{_, _}, _]] :=
Module[{},

  Internal`InheritedBlock[{$LexicalScope},

    Flatten[walk /@ children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Table", _], children:{body_, iterPat, iterPat...}, _]] :=
Module[{paramSymbolsAndIterOccurring, paramSymbols, iterOccurring, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndIterOccurring =
      Function[{child},
        paramSymbols = Replace[child[[2]], {
          {sym:LeafNode[Symbol, _, _], max_} :> {{sym}, walk[max]},
          {sym:LeafNode[Symbol, _, _], min_, max_} :> {{sym}, walk[min] ~Join~ walk[max]},
          {sym:LeafNode[Symbol, _, _], min_, max_, d_} :> {{sym}, walk[min] ~Join~ walk[max] ~Join~ walk[d]},
          {max_} :> {{}, walk[max]},
          _ :> {{}, {}}
        }, {0}]
      ] /@ Rest[children];

    paramSymbols = Flatten[paramSymbolsAndIterOccurring[[All, 1]]];
    iterOccurring = Flatten[paramSymbolsAndIterOccurring[[All, 2]]];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (#[[2]] -> {"parameter"})& /@ paramSymbols |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    iterOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]

walk[CallNode[LeafNode[Symbol, "Table", _], children:{_, _}, _]] :=
Module[{},

  Internal`InheritedBlock[{$LexicalScope},

    Flatten[walk /@ children]
  ]
]


compileTypePat = CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _, PatternSequence[] | _}, _]


walk[CallNode[LeafNode[Symbol, "Compile", _], {CallNode[LeafNode[Symbol, "List", _], types:{compileTypePat, compileTypePat...}, _], body_, optionsAndPatterns___}, _]] :=
Module[{paramSymbolsAndTypeOccurring, paramSymbols, typeOccurring, paramNames, newScope, bodyOccurring},

  Internal`InheritedBlock[{$LexicalScope},

    paramSymbolsAndTypeOccurring =
      Function[{type},
        Replace[type[[2]], {
          {sym:LeafNode[Symbol, _, _], pattern_} :> {{sym}, walk[pattern]},
          {sym:LeafNode[Symbol, _, _], pattern_, rank_} :> {{sym}, walk[pattern] ~Join~ walk[rank]},
          _ :> {{}, {}}
        }, {0}]
      ] /@ types;

    paramSymbols = Flatten[paramSymbolsAndTypeOccurring[[All, 1]]];
    typeOccurring = Flatten[paramSymbolsAndTypeOccurring[[All, 2]]];

    paramNames = #[[2]]& /@ paramSymbols;

    newScope = <| (# -> {"parameter"})& /@ paramNames |>;

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    bodyOccurring = walk[body];

    Scan[add[#, bodyOccurring]&, paramSymbols];

    typeOccurring ~Join~ Complement[bodyOccurring, paramNames]
  ]
]


walk[CallNode[LeafNode[Symbol, "Pattern", _], {lhs:LeafNode[Symbol, name_, _], rhs_}, _]] := (
  If[!MemberQ[$ExcludePatternNames, name],
    walk[lhs] ~Join~ walk[rhs]
    ,
    walk[rhs]
  ]
)

freePatterns[pat:CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, _, _], rhs_}, _]] := 
  Flatten[{pat} ~Join~ freePatterns[rhs]]


walk[CallNode[head_, children_, _]] :=
  Flatten[Join[walk[head], walk /@ children]]

freePatterns[CallNode[head_, body_, _]] := 
  Flatten[freePatterns[head] ~Join~ (freePatterns /@ body)]


walk[sym:LeafNode[Symbol, name_, data_]] :=
Module[{decls},

  decls = Lookup[$LexicalScope, name, {}];

  If[!empty[decls],
    Internal`StuffBag[$Data, {
        data[[Key[Source]]],
        tokenType[decls],
        modifiersSet[decls, True]
      }
    ];
    {name}
    ,
    {}
  ]
]

add[sym:LeafNode[Symbol, name_, data_], occurringScopedNames_] :=
Module[{decls},

  decls = Lookup[$LexicalScope, name, {}];

  Internal`StuffBag[$Data, {
      data[[Key[Source]]],
      tokenType[decls],
      modifiersSet[decls, MemberQ[occurringScopedNames, name]]
    }
  ]
]


walk[LeafNode[_, _, _]] :=
  {}

freePatterns[LeafNode[_, _, _]] := 
  {}


walk[ErrorNode[_, _, _]] :=
  {}

freePatterns[ErrorNode[_, _, _]] := 
  {}


walk[UnterminatedCallNode[_, _, _]] :=
  {}

freePatterns[UnterminatedCallNode[_, _, _]] := 
  {}


walk[UnterminatedGroupNode[_, _, _]] :=
  {}

freePatterns[UnterminatedGroupNode[_, _, _]] := 
  {}



tokenType[{"parameter"..}] := "parameter"
tokenType[_] := "variable"

(*
modifiersSet[decls_, used_]
*)
modifiersSet[{"parameter"}, False] =
  {"unused"}
modifiersSet[{"parameter"}, True] =
  {}
modifiersSet[{decl_}, False] :=
  {decl, "unused"}
modifiersSet[{decl_}, True] :=
  {decl}
modifiersSet[_, False] :=
  {"unused", "shadowed"}
modifiersSet[_, True] :=
  {"shadowed"}



End[]

EndPackage[]
