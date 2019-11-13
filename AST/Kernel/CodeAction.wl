BeginPackage["AST`CodeAction`"]

ApplyCodeAction



Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]


ApplyCodeAction[action:CodeAction[label_, command_, actionData_]][cst_] :=
	ApplyCodeAction[action, cst]

(*
input: a cst

srcPosMapIn: Association of src -> pos of src in cst, or Null


output: a cst

*)

ApplyCodeAction[action:CodeAction[label_, command_, actionData_], cstIn_, srcPosMapIn_:Null] :=
Catch[
Module[{src, originalNodePos, cst, replacementNode, func, trivia, insertionText,
	insertionNode, srcInter, srcIntra, replacementText, node, leafText, insertionNodePos,
  srcPosMap},

	cst = cstIn;
  srcPosMap = srcPosMapIn;

  src = Lookup[actionData, Source];

	If[$Debug,
		Print["src: ", src];
    Print["command: ", command];
   ];

   (*
	Deal with the text-based actions first

	DeleteText
	InsertText
	DeleteTrivia
   *)
   Switch[command,

    DeleteNode, (

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
          LeafNode[Token`Comma, _, _],
          cst = Delete[cst, trailingCommaPos];
        ]
       ]
      ];
      
      cst
    )
    ,

   	DeleteText, (

    src = expandSource[src, cst];

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
    There is Intra in the position
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
    )

   	,

   	DeleteTrivia, (

   	func = SourceMemberQ[src];
   	trivia = Cases[cst, LeafNode[Token`WhiteSpace | Token`Newline | Token`Comment | Token`LineContinuation, _, KeyValuePattern[Source -> src_?func]], Infinity];

   	Scan[(cst = ApplyCodeAction[CodeAction["delete", DeleteNode, <|Source->#[[3, Key[Source] ]]|>], cst];)&, trivia];
   	cst
    )
   	,

    Identity, (
    
    cst
    )

    ,

    InsertNode, (

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
    )

    ,

    InsertNodeAfter, (

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
    )

    ,

   	InsertText, (

    src = expandSource[src, cst];

   	insertionText = actionData["InsertionText"];
   	If[$Debug,
   		Print["insertionText: ", insertionText];
   	];

    If[!MatchQ[Last[src], Intra[___]],
      (*
      There is no Intra in the position, so we can just use InsertNode
      *)
      insertionNode = ParseLeaf[insertionText];
      If[FailureQ[insertionNode],
        Throw[insertionNode]
      ];
      cst = ApplyCodeAction[CodeAction[label, InsertNode, <|Source->src, "InsertionNode"->insertionNode|>], cst];
      cst = cleanupIssue[cst, action];
      Throw[cst]
    ];

    (*
    There is Intra in the position
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
    )

    ,

    InsertTextAfter, (

    src = expandSource[src, cst];

    insertionText = actionData["InsertionText"];
    If[$Debug,
      Print["insertionText: ", insertionText];
    ];

    If[!MatchQ[Last[src], Intra[___]],
      (*
      There is no Intra in the position, so we can just use InsertNode
      *)
      insertionNode = ParseLeaf[insertionText];
      If[FailureQ[insertionNode],
        Throw[insertionNode]
      ];
      cst = ApplyCodeAction[CodeAction[label, InsertNodeAfter, <|Source->src, "InsertionNode"->insertionNode|>], cst];
      cst = cleanupIssue[cst, action];
      Throw[cst]
    ];

    (*
    There is Intra in the position
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
    )

    ,

    ReplaceNode, (

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
     )
    ,

    ReplaceText, (

    src = expandSource[src, cst];

    replacementText = actionData["ReplacementText"];
     If[$Debug,
      Print["replacementText: ", replacementText];
     ];

    If[!MatchQ[Last[src], Intra[___]],
      (*
      There is no Intra in the position, so we can just use ReplaceNode
      *)
      replacementNode = ParseLeaf[replacementText];
      If[FailureQ[replacementNode],
        Throw[replacementNode]
      ];
      cst = ApplyCodeAction[CodeAction[label, ReplaceNode, <|Source->src, "ReplacementNode"->replacementNode|>], cst];
      cst = cleanupIssue[cst, action];
      Throw[cst]
    ];

    (*
    There is Intra in the position
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
     )
     
     ,

     _, (

     If[$Debug,
      Print["unknownCommand: ", command]
      ];
      Failure["unknownCommand", <|"Command"->command|>]
     )
   ]
]]



(*
Remove the corresponding issue from the CST
*)
cleanupIssue[cstIn_, action_CodeAction] :=
Module[{issue, cst},

  cst = cstIn;

  issue = FirstCase[cst, SyntaxIssue[_, _, _, KeyValuePattern[CodeActions -> actions_ /; MemberQ[actions, action]]],
            $Failed,
            Infinity];
  
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
  FirstCase[cst, LeafNode[_, _, KeyValuePattern[Source -> tokSrc_ /; SourceMemberQ[tokSrc, src]]], $Failed, Infinity]

(*
convert {{1,23},{1,23}} into {{1,3},{2,14},Intra[21,21]}
*)
expandSource[textSrc_, cst_] :=
Catch[
  Module[{tok, tokSrc, tokStartCol},

    tok = containingToken[textSrc, cst];
    tokSrc = tok[[3, Key[Source] ]];

    If[textSrc == tokSrc,
      Throw[textSrc]
    ];

    If[textSrc[[1,1]] != textSrc[[2,1]],
      Throw[$Failed, "Unimplemented"]
    ];
    If[textSrc[[1,1]] != tokSrc[[1,1]],
      Throw[$Failed, "Unimplemented"]
    ];

    tokStartCol = tokSrc[[1,2]];

    Append[tokSrc, Intra[textSrc[[1,2]]-tokStartCol+1, textSrc[[2,2]]-tokStartCol+1]]
  ]
]



End[]

EndPackage[]
