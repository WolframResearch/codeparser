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

ApplyCodeAction[CodeAction[label_, command_, actionData_], cstIn_] :=
Catch[
Module[{src, originalNodePos, cst, replacementNode, func, trivia, lineNumber, col1, col2, case, pos, insertionText, insertionLeaf,
	insertionNode, leaves},

	cst = cstIn;

	If[$Debug,
		Print["command: ", command];
   ];



   (*
	Deal with the text-based actions first

	DeleteText
	InsertText
	DeleteTrivia
   *)
   Switch[command,

   	DeleteText,

   	src = Lookup[actionData, Source];
   	func = SourceMemberQ[src];
   	leaves = Cases[cst, LeafNode[_, _, KeyValuePattern[Source -> src_?func]], Infinity];
   	If[$Debug,
   		Print["leaves: ", leaves];
   	];
   	Scan[(cst = ApplyCodeAction[CodeAction["delete", DeleteNode, <|Source->#[[3, Key[Source] ]]|>], cst])&, leaves];
   	Throw[cst]

   	,

   	DeleteTrivia,

   	src = Lookup[actionData, Source];
   	func = SourceMemberQ[src];
   	trivia = Cases[cst, LeafNode[Token`WhiteSpace | Token`Newline | Token`Comment | Token`LineContinuation, _, KeyValuePattern[Source -> src_?func]], Infinity];

   	Scan[(cst = ApplyCodeAction[CodeAction["delete", DeleteNode, <|Source->#[[3, Key[Source] ]]|>], cst];)&, trivia];
   	Throw[cst]

   	,

   	InsertText,

   	src = Lookup[actionData, Source];
   	insertionText = actionData["InsertionText"];
   	insertionLeaf = ParseLeaf[insertionText];
   	If[$Debug,
   		Print["insertionLeaf: ", insertionLeaf];
   	];
   	If[FailureQ[insertionLeaf],
   		Throw[insertionLeaf]
   	];
   	cst = ApplyCodeAction[CodeAction["insert", InsertNode, <|Source->src, "InsertionNode"->insertionLeaf|>], cst];
   	Throw[cst]

   ];

   (*
   resolve ReplaceText and InsertText and DeleteText actions into nodes
	*)

   Switch[command,
     (*
     The DeleteNode command does any extra bit of work, 
     by cleaning up any surrounding commas
     *)
     DeleteNode,

     src = Lookup[actionData, Source];
     leaves = Cases[cst, _[_, _, KeyValuePattern[Source -> src]], Infinity];
     If[$Debug,
     	Print["leaves: ", leaves];
     ];
     Scan[(cst = DeleteCases[cst, #, Infinity])&, leaves];

     
     If[Length[originalNodePos] > 2,
      parentPos = Drop[originalNodePos, -2];
      parent = Extract[cst, parentPos];
      commaChildren = parent[[2]];
      commaChildrenLength = Length[commaChildren];
      deletedWasLastNode = (originalNodePos[[-1]] > commaChildrenLength);
      If[$Debug,
   		Print["deletedWasLastNode: ", deletedWasLastNode];
   	];
      If[deletedWasLastNode,
      	(* deletedWasLastNode *)
       leadingCommaPos = originalNodePos[[;; -2]] ~Join~ {originalNodePos[[-1]] - 1};
       If[MatchQ[Extract[cst, leadingCommaPos], LeafNode[Token`Comma, _, _]],
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
   	Switch[Extract[cst, trailingCommaPos],
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
     
     ,
     ReplaceNode,

     src = Lookup[actionData, Source];
     originalNodePos = Position[cst, _[_, _, KeyValuePattern[Source -> src]]];
     If[originalNodePos == {},
     	Throw[Failure["CannotFindNode", <| actionData, "CST"->cst, "Source"->src |>]]
     ];
     originalNodePos = originalNodePos[[1]];

     replacementNode = actionData["ReplacementNode"];
     If[$Debug,
     	Print["replacementNode: ", replacementNode];
     ];
     If[originalNodePos == {},
     	cst = replacementNode
     	,
     	cst = ReplacePart[cst, originalNodePos -> replacementNode];
     ];
     cst

     ,

     InsertNode,

     src = Lookup[actionData, Source];
   	lineNumber = src[[1, 1]];
   	col1 = src[[1, 2]];
   	col2 = src[[2, 2]];
   	If[col1 != col2,
   		Throw[Failure["BadColumn", actionData]]
   	];

   	case = FirstCase[cst, LeafNode[_, _, KeyValuePattern[Source -> {{lineNumber, col1}, _}]], $Failed, Infinity];
   	If[FailureQ[case],
   		Throw[Failure["BadCase", actionData]]
   	];

   	insertionNode = actionData["InsertionNode"];

   	pos = Position[cst, case][[1]];

   	cst = Insert[cst, insertionNode, pos];
   	cst

   	,

   	Identity,
   	cst

     ,
     _, If[$Debug, Print["unknownCommand: ", command]];Failure["unknownCommand", <|"Command"->command|>]
   ]
]]



End[]

EndPackage[]
