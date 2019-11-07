BeginPackage["AST`CodeAction`"]

ApplyCodeAction



Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]



(*
input: a string
output: a string
*)
(*
ApplyCodeAction[strIn_String, CodeAction[label_, command_, actionData_]] :=
Catch[
Module[{str, src, replacementText, lines, lineNumber, line, col1, col2, insertionText},

	str = strIn;

	If[$Debug,
		Print["command: ", command];
   ];

   src = Lookup[actionData, Source];
   If[MissingQ[src],
   	Throw[Failure["MissingSource", actionData]]
   ];

   If[!MatchQ[src, {{line_Integer, _Integer}, {line_Integer, _Integer}}],
   	Throw[Failure["BadSource", actionData]]
   ];

   lineNumber = src[[1, 1]];
   col1 = src[[1, 2]];
   col2 = src[[2, 2]];

   lines = ImportString[str, "Lines"];

   line = lines[[lineNumber]];

   Switch[command,
     
   	InsertText,
   	If[col1 != col2,
   		Throw[Failure["BadColumn", actionData]]
   	];
   	insertionText = actionData["InsertionText"];
   	line = StringInsert[line, insertionText, {col1}]
   	,

     	ReplaceText,
     	replacementText = actionData["ReplacementText"];
     	line = StringReplacePart[line, replacementText, {col1, col2}];

     ,
     _, Failure["UnknownCommand", <|"Command"->command|>]
   ];

   lines[[lineNumber]] = line;
   str = StringJoin[StringRiffle[lines, "\n"]];
   str

]]
*)

ApplyCodeAction[action:CodeAction[label_, command_, actionData_]][cst_] :=
	ApplyCodeAction[action, cst]

(*
input: a cst
output: a cst
*)

ApplyCodeAction[action:CodeAction[label_, command_, actionData_], cstIn_] :=
Catch[
Module[{src, originalNodePos, cst, replacementNode, func, trivia, insertionText,
	insertionNode, srcInter, srcIntra, replacementText, node, leafText, insertionNodePos},

	cst = cstIn;

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

      originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
     ];
     originalNodePos = originalNodePos[[1]];

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

   	originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
     ];
     originalNodePos = originalNodePos[[1]];

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

    originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
     ];
     originalNodePos = originalNodePos[[1]];

    cst = Insert[cst, insertionNode, originalNodePos];
    cst
    )

    ,

    InsertNodeAfter, (

    insertionNode = actionData["InsertionNode"];
    If[$Debug,
      Print["insertionNode: ", insertionNode];
    ];

    originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
     ];
     originalNodePos = originalNodePos[[1]];

     insertionNodePos = Append[Most[originalNodePos], Last[originalNodePos] + 1];

    cst = Insert[cst, insertionNode, insertionNodePos];
    cst
    )

    ,

   	InsertText, (

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

    originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
     ];
     originalNodePos = originalNodePos[[1]];

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

    ReplaceNode, (

      replacementNode = actionData["ReplacementNode"];
     If[$Debug,
      Print["replacementNode: ", replacementNode];
     ];

     originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
     ];
     originalNodePos = originalNodePos[[1]];

     If[originalNodePos == {},
      cst = replacementNode
      ,
      cst = ReplacePart[cst, originalNodePos -> replacementNode];
     ];
     cst
     )
    ,

    ReplaceText, (

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

     originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> srcInter]]];
     If[originalNodePos == {},
      Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->srcInter |>]]
     ];
     originalNodePos = originalNodePos[[1]];
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











End[]

EndPackage[]
