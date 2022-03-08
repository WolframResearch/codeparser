BeginPackage["CodeParser`CodeAction`"]

ApplyCodeAction



Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



(*
provide some selectors for CodeAction objects
*)

CodeAction[label_, command_, actionData_]["Label"] := label



ApplyCodeAction[action:CodeAction[label_, command_, actionData_]][cst_] :=
	ApplyCodeAction[action, cst]

(*
input: a cst

srcPosMapIn: Association of src -> pos of src in cst, or Null


output: a cst

*)

ApplyCodeAction[action:CodeAction[label_, DeleteNode, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, srcPosMap, parentPos, parent, commaChildren, commaChildrenLength,
  deletedWasLastNode, leadingCommaPos, trailingCommaPos, child},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, src], srcPosMap[src]];

  If[originalNodePos == {},
  Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  cst = Delete[cst, originalNodePos];

  (*
  The DeleteNode command does an extra bit of work, 
  by cleaning up any surrounding commas
  *)
     
  If[Length[originalNodePos] > 2,
    parentPos = Drop[originalNodePos, -2];
    parent = Extract[cst, {parentPos}][[1]];
    commaChildren = parent[[2]];
    commaChildrenLength = Length[commaChildren];
    deletedWasLastNode = (originalNodePos[[-1]] > commaChildrenLength);
    If[$Debug,
      Print["commaChildrenLength: ", commaChildrenLength];
      Print["deletedWasLastNode: ", deletedWasLastNode];
    ];
    If[deletedWasLastNode,
      (* deletedWasLastNode *)
      leadingCommaPos = originalNodePos[[;; -2]] ~Join~ {originalNodePos[[-1]] - 1};
      If[MatchQ[Extract[cst, {leadingCommaPos}][[1]], LeafNode[Token`Comma, _, _]],
        cst = Delete[cst, leadingCommaPos]
        ,
        Null
      ]
      ,
      (* NOT deletedWasLastNode *)
      trailingCommaPos = originalNodePos[[;; -2]] ~Join~ {originalNodePos[[-1]] + 0};
      If[$Debug,
        Print["trailingCommaPos: ", trailingCommaPos];
      ];
      (* FIXME: advance past Implicit Null of the Comma *)
      Switch[Extract[cst, {trailingCommaPos}][[1]],
        (*
        LeafNode[Token`Fake`ImplicitNull, _, _],
        cst = Delete[cst, trailingCommaPos];
        If[t

        ]
        ,
        *)
        ErrorNode[Token`Error`InfixImplicitNull, _, _],
        cst = Delete[cst, trailingCommaPos];
      ]
    ];

    (*
    now also handle the removal of penultimate child of Comma node, where we need to remove the Comma node itself

    If we started with InfixNode[Comma, {a, comma, b}]

    and then we removed b and now have InfixNode[Comma, {a}]

    we need to remove Comma and just have a
    *)
    parent = Extract[cst, {parentPos}][[1]];
    commaChildren = parent[[2]];
    commaChildrenLength = Length[commaChildren];
    If[commaChildrenLength == 1,
      child = commaChildren[[1]];
      cst = ReplacePart[cst, parentPos -> child];
    ]
  ];
      
  cst
]]

ApplyCodeAction[action:CodeAction[label_, DeleteTriviaNode, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, src], srcPosMap[src]];

  If[originalNodePos == {},
  Throw[Failure["CannotFindNode", <| actionData, "CST" -> cst, "Source" -> src |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  cst = Delete[cst, originalNodePos];

  cst
]]

ApplyCodeAction[action:CodeAction[label_, DeleteText, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, srcInter, srcIntra, node, leafText, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  src = expandSourceThatMayHaveOriginatedInLibrary[src, cst];

  If[$Debug,
    Print["src: ", src]
  ];

  If[!MatchQ[Last[src], Intra[___]],
    (*
    There is no Intra in the position, so we can just use DeleteNode
    *)
    cst = ApplyCodeAction[CodeAction[label, DeleteNode, <|Source->src|>], cst];
    cst = cleanupIssue[cst, action];
    Throw[cst]
  ];

  (*
  There is Intra in the last position
  *)
  srcInter = Most[src];
  srcIntra = Last[src];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, srcInter], srcPosMap[srcInter]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  node = Extract[cst, {originalNodePos}][[1]];
  leafText = node[[2]];
  
  leafText = StringReplacePart[leafText, "", List @@ srcIntra];
  node[[2]] = leafText;

  If[originalNodePos == {},
    cst = node
    ,
    cst = ReplacePart[cst, originalNodePos -> node];
  ];

  cst = cleanupIssue[cst, action];

  cst
]]

(*

DeleteTrivia is buggy and poorly understood

"Sources" is a span, but we do not have a way of representing spans of Position specs

ApplyCodeAction[action:CodeAction[label_, DeleteTrivia, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{srcs, cst, func, trivia, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  srcs = Lookup[actionData, "Sources"];

  If[$Debug,
    Print["srcs: ", srcs];
  ];

  func = SourceMemberQ[srcs];
  trivia = Cases[cst, LeafNode[Whitespace | Token`Newline | Token`Comment, _, KeyValuePattern[Source -> src_?func]], Infinity];

  Scan[(cst = ApplyCodeAction[CodeAction["delete", DeleteNode, <| Source -> #[[3, Key[Source]]]|>], cst];)&, trivia];
  
  cst
]]
*)

ApplyCodeAction[action:CodeAction[label_, Identity, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, cst, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  cst
]]

ApplyCodeAction[action:CodeAction[label_, InsertNode, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, insertionNode, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  insertionNode = actionData["InsertionNode"];
  If[$Debug,
    Print["insertionNode: ", insertionNode];
  ];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, src], srcPosMap[src]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  cst = Insert[cst, insertionNode, originalNodePos];
  cst
]]

ApplyCodeAction[action:CodeAction[label_, InsertNodeAfter, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, insertionNode, insertionNodePos, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  insertionNode = actionData["InsertionNode"];
  If[$Debug,
    Print["insertionNode: ", insertionNode];
  ];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, src], srcPosMap[src]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  insertionNodePos = Append[Most[originalNodePos], Last[originalNodePos] + 1];

  cst = Insert[cst, insertionNode, insertionNodePos];
  cst
]]

ApplyCodeAction[action:CodeAction[label_, InsertText, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, insertionText, insertionNode, srcInter, srcIntra, node, leafText, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src]
  ];

  src = expandSourceThatMayHaveOriginatedInLibrary[src, cst];

  If[$Debug,
    Print["src: ", src]
  ];

  insertionText = actionData["InsertionText"];

  If[!MatchQ[Last[src], Intra[___]],
    (*
    There is no Intra in the position, so we can just use InsertNode

    This used to be CodeConcreteParseLeaf[insertionText]
    but there is no guarantee that insertionText is a leaf
    *)
    insertionNode = CodeConcreteParse[insertionText];
    If[FailureQ[insertionNode],
      Throw[insertionNode]
    ];
    (*
    Look inside the generated node and assume there is only 1 child
    *)
    If[Length[insertionNode[[2]]] != 1,
      Throw[Failure["CannotApplyCodeAction", <|"CodeAction" -> action, "CST" -> cst|>]]
    ];
    (*
    strip off ContainerNode
    *)
    insertionNode = insertionNode[[2, 1]];
    (*
    FIXME: insertionNode still has LineColumn source from CodeConcreteParse and should be stripped
    *)
    cst = ApplyCodeAction[CodeAction[label, InsertNode, <|Source->src, "InsertionNode"->insertionNode|>], cst];
    cst = cleanupIssue[cst, action];
    Throw[cst]
  ];

  (*
  There is Intra in the last position
  *)
  srcInter = Most[src];
  srcIntra = Last[src];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, srcInter], srcPosMap[srcInter]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  node = Extract[cst, {originalNodePos}][[1]];
  leafText = node[[2]];
    
  leafText = StringInsert[leafText, insertionText, srcIntra[[1]]];
  node[[2]] = leafText;

  If[originalNodePos == {},
    cst = node
    ,
    cst = ReplacePart[cst, originalNodePos -> node];
  ];

  cst = cleanupIssue[cst, action];

  cst
]]

ApplyCodeAction[action:CodeAction[label_, InsertText, actionData_], cstIn_, srcPosMap_] :=
Catch[
Module[{src, originalNodePos, cst, insertionText, insertionNode, srcInter, srcIntra, node, leafText},

  cst = cstIn;

  src = Lookup[actionData, Source];

  src = expandSourceThatMayHaveOriginatedInLibrary[src, cst];

  insertionText = actionData["InsertionText"];

  If[!MatchQ[Last[src], Intra[___]],
    (*
    There is no Intra in the position, so we can just use InsertNode

    This used to be CodeConcreteParseLeaf[insertionText]
    but there is no guarantee that insertionText is a leaf
    *)
    insertionNode = CodeConcreteParse[insertionText];
    If[FailureQ[insertionNode],
      Throw[insertionNode]
    ];
    (*
    Look inside the generated node and assume there is only 1 child
    *)
    If[Length[insertionNode[[2]]] != 1,
      Throw[Failure["CannotApplyCodeAction", <|"CodeAction" -> action, "CST" -> cst|>]]
    ];
    (*
    strip off ContainerNode
    *)
    insertionNode = insertionNode[[2, 1]];
    (*
    FIXME: insertionNode still has LineColumn source from CodeConcreteParse and should be stripped
    *)
    cst = ApplyCodeAction[CodeAction[label, InsertNode, <|Source->src, "InsertionNode"->insertionNode|>], cst];
    cst = cleanupIssue[cst, action];
    Throw[cst]
  ];

  (*
  There is Intra in the last position
  *)
  srcInter = Most[src];
  srcIntra = Last[src];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];

  but this is super slow
  *)
  originalNodePos = srcPosMap[srcInter];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  node = Extract[cst, {originalNodePos}][[1]];
  leafText = node[[2]];
  
  leafText = StringInsert[leafText, insertionText, srcIntra[[1]]];
  node[[2]] = leafText;

  If[originalNodePos == {},
    cst = node
    ,
    cst = ReplacePart[cst, originalNodePos -> node];
  ];

  cst = cleanupIssue[cst, action];

  cst
]]

ApplyCodeAction[action:CodeAction[label_, InsertTextAfter, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, insertionText, insertionNode, srcInter, srcIntra, node, leafText, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  src = expandSourceThatMayHaveOriginatedInLibrary[src, cst];

  insertionText = actionData["InsertionText"];
  If[$Debug,
    Print["insertionText: ", insertionText];
  ];

  If[!MatchQ[Last[src], Intra[___]],
    (*
    There is no Intra in the position, so we can just use InsertNode

    This used to be CodeConcreteParseLeaf[insertionText]
    but there is no guarantee that insertionText is a leaf
    *)
    insertionNode = CodeConcreteParse[insertionText];
    If[FailureQ[insertionNode],
      Throw[insertionNode]
    ];
    (*
    Look inside the generated node and assume there is only 1 child
    *)
    If[Length[insertionNode[[2]]] != 1,
      Throw[Failure["CannotApplyCodeAction", <|"CodeAction" -> action, "CST" -> cst|>]]
    ];
    (*
    strip off ContainerNode
    *)
    insertionNode = insertionNode[[2, 1]];
    (*
    FIXME: insertionNode still has LineColumn source from CodeConcreteParse and should be stripped
    *)
    cst = ApplyCodeAction[CodeAction[label, InsertNodeAfter, <|Source->src, "InsertionNode"->insertionNode|>], cst];
    cst = cleanupIssue[cst, action];
    Throw[cst]
  ];

  (*
  There is Intra in the last position
  *)
  srcInter = Most[src];
  srcIntra = Last[src];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, srcInter], srcPosMap[srcInter]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  node = Extract[cst, {originalNodePos}][[1]];
  leafText = node[[2]];
  
  leafText = StringInsert[leafText, insertionText, srcIntra[[1]] + 1];
  node[[2]] = leafText;

  If[originalNodePos == {},
    cst = node
    ,
    cst = ReplacePart[cst, originalNodePos -> node];
  ];

  cst = cleanupIssue[cst, action];

  cst
]]

ApplyCodeAction[action:CodeAction[label_, ReplaceNode, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, replacementNode, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  replacementNode = actionData["ReplacementNode"];
  If[$Debug,
    Print["replacementNode: ", replacementNode];
  ];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, src], srcPosMap[src]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];

  If[originalNodePos == {},
    cst = replacementNode
    ,
    cst = ReplacePart[cst, originalNodePos -> replacementNode];
  ];
  cst
]]

ApplyCodeAction[action:CodeAction[label_, ReplaceText, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, replacementNode, srcInter, srcIntra, replacementText, node, leafText, srcPosMap},

  cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

  If[$Debug,
    Print["src: ", src];
  ];

  src = expandSourceThatMayHaveOriginatedInLibrary[src, cst];

  replacementText = actionData["ReplacementText"];
  If[$Debug,
    Print["replacementText: ", replacementText];
  ];

  If[!MatchQ[Last[src], Intra[___]],
    (*
    There is no Intra in the position, so we can just use ReplaceNode

    This used to be CodeConcreteParseLeaf[replacementText]
    but there is no guarantee that replacementText is a leaf
    *)
    replacementNode = CodeConcreteParse[replacementText];
    If[FailureQ[replacementNode],
      Throw[replacementNode]
    ];
    (*
    Look inside the generated node and assume there is only 1 child
    *)
    If[Length[replacementNode[[2]]] != 1,
      Throw[Failure["CannotApplyCodeAction", <|"CodeAction" -> action, "CST" -> cst|>]]
    ];
    (*
    strip off ContainerNode
    *)
    replacementNode = replacementNode[[2, 1]];
    (*
    FIXME: replacementNode still has LineColumn source from CodeConcreteParse and should be stripped
    *)
    cst = ApplyCodeAction[CodeAction[label, ReplaceNode, <|Source->src, "ReplacementNode"->replacementNode|>], cst];
    cst = cleanupIssue[cst, action];
    Throw[cst]
  ];

  (*
  There is Intra in the last position
  *)
  srcInter = Most[src];
  srcIntra = Last[src];

  (*
  was originally:
  originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];

  but this is super slow
  *)
  originalNodePos = If[srcPosMap === Null, Position[cst, srcInter], srcPosMap[srcInter]];

  If[originalNodePos == {},
    Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
  ];
  originalNodePos = Drop[originalNodePos[[1]], -2];
  If[$Debug,
    Print["originalNodePos: ", originalNodePos];
  ];

  node = Extract[cst, {originalNodePos}][[1]];
  leafText = node[[2]];
  
  leafText = StringReplacePart[leafText, replacementText, List @@ srcIntra];
  node[[2]] = leafText;

  If[originalNodePos == {},
    cst = node
    ,
    cst = ReplacePart[cst, originalNodePos -> node];
  ];

  cst = cleanupIssue[cst, action];

  cst
]]




(*
Remove the corresponding issue from the CST
*)
cleanupIssue[cstIn_, action_CodeAction] :=
Module[{issue, cst},

  cst = cstIn;

  issue = FirstCase[cst, (SyntaxIssue|FormatIssue|EncodingIssue)[_, _, _, KeyValuePattern[CodeActions -> actions_ /; MemberQ[actions, action]]], $Failed, Infinity];
  
  If[FailureQ[issue],
    (*
    It is ok that issue could not be found
    Maybe it is someone playing around with actions that are not actually in the CST
    No need to freak out
    *)
    Throw[cst]
  ];

  cst = DeleteCases[cst, issue, Infinity];

  cst
]



(*
Find the token that contains the Source
*)
containingToken[src_, cst_] :=
  FirstCase[cst, (LeafNode|ErrorNode)[_, _, KeyValuePattern[Source -> tokSrc_ /; SourceMemberQ[tokSrc, src]]], $Failed, Infinity]

(*
convert {{1,23},{1,23}} into {{1,3},{2,14},Intra[21,21]}

the parser library does not have a way of supplying {{1,3},{2,14},Intra[21,21]}
so it just returns {{1,23},{1,23}} and this needs to be handled here

this is only needed for Sources that come from the native library

Sources that come from WL code should look like {{1,3},{2,14},Intra[21,21]} from the start
*)
expandSourceThatMayHaveOriginatedInLibrary[textSrc:{{_Integer, _Integer}, {_Integer, _Integer}}, cst_] :=
Catch[
Module[{tok, tokSrc, tokStartCol},

  tok = containingToken[textSrc, cst];
  tokSrc = tok[[3, Key[Source]]];

  If[textSrc == tokSrc,
    Throw[textSrc]
  ];

  If[textSrc[[1, 1]] != textSrc[[2, 1]],
    Throw[$Failed, "Unimplemented"]
  ];
  If[textSrc[[1, 1]] != tokSrc[[1, 1]],
    Throw[$Failed, "Unimplemented"]
  ];

  tokStartCol = tokSrc[[1, 2]];

  Append[tokSrc, Intra[textSrc[[1, 2]]-tokStartCol+1, textSrc[[2, 2]]-tokStartCol+1-1]]
  (*                                               ^ 1-based *)
  (*                                                                              ^ 1-based *)
  (*                                                                                ^ inclusive *)
]
]

(*
all other conventions are ignored
*)
expandSourceThatMayHaveOriginatedInLibrary[textSrc_, cst_] :=
  textSrc


End[]

EndPackage[]
