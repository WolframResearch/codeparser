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

Some nodes would be strange at the top-level in a package. For example, 1+1 would be strange at the top-level.

*)

(*
if not active, return no issues
*)
topLevelChildIssues[_, KeyValuePattern["WillReportToplevelIssues" -> False]] :=
	{}

(*
Call could be anything
*)
topLevelChildIssues[CallNode[_,_,_], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
	{}

(*
probably a declaration
*)
topLevelChildIssues[LeafNode[Symbol,_,_], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
	{}

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
e.g., list of declarations in StartUp code

sym | "str" | Symbol["str"]

kind of a hack

TODO: handle StartUp files as a format

*)

symbolDeclPat = LeafNode[Symbol | String, _, _] | CallNode[LeafNode[Symbol, "Symbol", _], _, _]

topLevelChildIssues[
	GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _],
		symbolDeclPat,
		LeafNode[Token`CloseCurly, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _],
		InfixNode[Comma, {
			PatternSequence[symbolDeclPat, _]..., ErrorNode[Token`Error`InfixImplicitNull, _, _]}, _],
		LeafNode[Token`CloseCurly, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _],
		InfixNode[CompoundExpression, {
			PatternSequence[symbolDeclPat, _]..., LeafNode[Token`Fake`ImplicitNull, _, _]}, _],
		LeafNode[Token`CloseCurly, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _],
		InfixNode[Comma | CompoundExpression, {
			PatternSequence[symbolDeclPat, _]..., symbolDeclPat }, _],
		LeafNode[Token`CloseCurly, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

(*
just assume parens connote intention
*)
topLevelChildIssues[GroupNode[GroupParen, _, _], KeyValuePattern["WillReportToplevelIssues" -> True]] :=
	{}


(*
Define CompoundExpression versions of everything above

But do not simply make a recursive call back to previous definitions

We want to be able to catch e.g.,

foo[] := Message[foo::bad]; $Failed

where the parsing is ( foo[] := Message[foo::bad] ) ; $Failed

A recursive call would see the SetDelayed, and say fine

And then the recursive call would see the $Failed and say fine (because it's a symbol and might be a declaration)

So hard-code the CompoundExpression versions to be able to catch these cases

*)
topLevelChildIssues[
	InfixNode[CompoundExpression, {
		CallNode[_, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		LeafNode[Symbol, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		BinaryNode[AddTo | Apply | BinaryAt | BinarySlashSlash |
			Map | System`MapApply | Set | SetDelayed |
			SubtractFrom | Unset | UpSet | UpSetDelayed | Put |
			PutAppend, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		TernaryNode[TagSet | TagSetDelayed | TernaryTilde, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		PrefixNode[Get | PreDecrement | PreIncrement, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		PostfixNode[Decrement | Increment, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _], symbolDeclPat,
		LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _],
		InfixNode[Comma | CompoundExpression, { PatternSequence[symbolDeclPat, _]..., LeafNode[Token`Fake`ImplicitNull, _, _] }, _],
		LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		GroupNode[List, {
		LeafNode[Token`OpenCurly, _, _],
		InfixNode[Comma | CompoundExpression, { PatternSequence[symbolDeclPat, _]..., symbolDeclPat }, _],
		LeafNode[Token`CloseCurly, _, _]}, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		GroupNode[GroupParen, _, _], _LeafNode, LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

(*
more specific stuff inside CompoundExpression
*)

(*
allow a=1;b=2;c=3;

FIXME: maybe this is too niche

*)
topLevelChildIssues[
	InfixNode[CompoundExpression, {
		PatternSequence[BinaryNode[Set | SetDelayed | Unset, _, _], _LeafNode].., LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		PatternSequence[LeafNode[Symbol, _, _], _LeafNode].., LeafNode[Token`Fake`ImplicitNull, _, _] }
		,
		_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}

topLevelChildIssues[
	InfixNode[CompoundExpression, {PatternSequence[LeafNode[Symbol, _, _], _LeafNode].., LeafNode[Symbol, _, _] }, _]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{}


topLevelChildIssues[
	InfixNode[CompoundExpression, {BinaryNode[Set | SetDelayed, _, _], LeafNode[Token`Semi, _, _], end:_[Except[Token`Fake`ImplicitNull], _, _], ___}, data_]
	,
	ignored_
] :=
Catch[
Module[{first},

	first = firstExplicitToken[end];

	If[FailureQ[first],
		Throw[{}]
	];

	{
		SyntaxIssue["TopLevel", "Definition does not contain the rest of the ``CompoundExpression``.", "Error",
			<|
				Source -> first[[3, Key[Source]]],
				ConfidenceLevel -> 0.95
				(*FIXME: wrap parentheses CodeAction*)
			|>
		]
	}
]]

topLevelChildIssues[
	InfixNode[CompoundExpression, {_, LeafNode[Token`Semi, _, data1_], LeafNode[Token`Fake`ImplicitNull, _, _]}, data_]
	,
	ignored_
] :=
	{
		SyntaxIssue["TopLevel", "``CompoundExpression`` at top-level.", "Warning",
			<|
				Source -> data1[Source],
				ConfidenceLevel -> 0.75,
				"AdditionalDescriptions" -> {"``;`` may not be needed at top-level."}
				(*FIXME: insert newline CodeAction*)
			|>
		]
	}

topLevelChildIssues[
	InfixNode[CompoundExpression, {
		_, LeafNode[Token`Semi, _, data1_], _, ___}
		,
		data_
	]
	,
	ignored_
] :=
	{
		SyntaxIssue["TopLevel", "``CompoundExpression`` at top-level.", "Warning",
			<|
				Source -> data1[Source],
				ConfidenceLevel -> 0.75,
				"AdditionalDescriptions" -> {"Consider breaking up onto separate lines."}
				(*FIXME: insert newline CodeAction*)
			|>
		]
	}

(*
Anything else, then warn

Specifically add a DidYouMean for / -> /@
*)
topLevelChildIssues[
	BinaryNode[Divide,
		{_, LeafNode[Token`Slash, _, slashData_], _}
		,
		data_
	]
	,
	KeyValuePattern["WillReportToplevelIssues" -> True]
] :=
	{
		SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning",
			<|
				Source -> slashData[Source],
				ConfidenceLevel -> 0.95,
				CodeActions -> {
					CodeAction["Replace ``/`` with ``/@``", ReplaceNode,
					<|
						Source->slashData[Source],
						"ReplacementNode"->LeafNode[Token`SlashAt, "/@", <||>]
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


topLevelChildIssues[node:_[_, _, _], reportIssuesBehavior:KeyValuePattern["WillReportToplevelIssues" -> True]] :=
Catch[
Module[{first, firstSrc, issues},

	issues = {};

	(*
	If a list or whatever is the only expression in a file, or if it is the last expression in a file,
	then assume it is "Data" or something and do not complain
	*)
	If[reportIssuesBehavior["ToplevelChildIndex"] == reportIssuesBehavior["ToplevelChildrenLength"],
		Throw[issues]
	];

	(*
	Just grab the first token to use
	*)
	first = firstExplicitToken[node];

	If[FailureQ[first],
		Throw[issues]
	];

	firstSrc = first[[3, Key[Source]]];

	Switch[first[[1]],
		Token`OpenCurly,
			AppendTo[issues, SyntaxIssue["TopLevelList", "Unexpected list at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.75 |>]
			]
		,
		Token`LessBar,
			AppendTo[issues, SyntaxIssue["TopLevelAssociation", "Unexpected association at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.75 |>]
			]
		,
		String,
			AppendTo[issues, SyntaxIssue["TopLevelString", "Unexpected string at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.75 |>]
			]
		,
		_,
			AppendTo[issues, SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning",
				<| Source -> firstSrc,
				ConfidenceLevel -> 0.95 |>]
			]
	];

	issues
]]



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
Module[{list, nodeListStack , currentList, operatorStack, currentOperator, x, issues, nodeList, peek, error, def},
	
	list = listIn;

	nodeListStack = System`CreateDataStructure["Stack"];
	operatorStack = System`CreateDataStructure["Stack"];

	nodeListStack["Push", System`CreateDataStructure["Stack"]];
	operatorStack["Push", None];

	issues = {};

	Do[
		x = list[[i]];
		Switch[x,
		(*
		BeginPackage["Foo`"]
		*)
		CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _],
			operatorStack["Push", PackageNode[x[[2]], {}, <| Source -> sourceSpan[sourceStart[x[[3, Key[Source]]]], (*partially constructed Source*)Indeterminate] |>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		BeginPackage["Foo`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage", _], {LeafNode[String, _?contextQ, _], LeafNode[String, _?contextQ, _] | CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] | PatternSequence[]}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", PackageNode[x[[2, 1, 2]], {}, <| Source -> sourceSpan[sourceStart[x[[2, 1, 3, Key[Source]]]], (*partially constructed Source*)Indeterminate] |>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		Begin["`Private`"]
		*)
		CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _],
			operatorStack["Push", ContextNode[x[[2]], {}, <| Source -> sourceSpan[sourceStart[x[[3, Key[Source]]]], (*partially constructed Source*)Indeterminate] |>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		Begin["`Private`"] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "Begin", _], {LeafNode[String, _?contextQ, _]}, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", ContextNode[x[[2, 1, 2]], {}, <| Source -> sourceSpan[sourceStart[x[[2, 1, 3, Key[Source]]]], (*partially constructed Source*)Indeterminate] |>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		System`Private`NewContextPath[{"Foo`"}]
		*)
		CallNode[LeafNode[Symbol, "System`Private`NewContextPath", _], { CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] }, _],
			operatorStack["Push", NewContextPathNode[x[[2, 1, 2]], {}, <| Source -> sourceSpan[sourceStart[x[[3, Key[Source]]]], (*partially constructed Source*)Indeterminate] |>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		System`Private`NewContextPath[{"Foo`"}] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "System`Private`NewContextPath", _], { CallNode[LeafNode[Symbol, "List", <||>], { LeafNode[String, _?contextQ, _]... }, _] }, _], LeafNode[Symbol, "Null", _]}, _],
			operatorStack["Push", NewContextPathNode[x[[2, 1, 2, 1, 2]], {}, <| Source -> sourceSpan[sourceStart[x[[2, 1, 3, Key[Source]]]], (*partially constructed Source*)Indeterminate] |>]];
			nodeListStack["Push", System`CreateDataStructure["Stack"]];
		,
		(*
		EndPackage[]
		End[]
		*)
		CallNode[LeafNode[Symbol, "EndPackage" | "End" | "System`Private`RestoreContextPath", _], {}, _],
			currentOperator = operatorStack["Pop"];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> x[[3, Key[Source]]], ConfidenceLevel -> 0.95 |> ]];
				Throw[{list, issues}];
			];
			currentList = nodeListStack["Pop"];
			currentOperator[[2]] = Normal[currentList];
			(* finish constructing Source *)
			currentOperator[[3, Key[Source]]] = sourceSpan[sourceStart[currentOperator[[3, Key[Source]]]], sourceEnd[x[[3, Key[Source]]]]];
			peek = nodeListStack["Peek"];
			peek["Push", currentOperator];
		,
		(*
		EndPackage[] ;
		End[] ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "EndPackage" | "End" | "System`Private`RestoreContextPath", _], {}, _], LeafNode[Symbol, "Null", _]}, _],
			currentOperator = operatorStack["Pop"];
			If[!MatchQ[currentOperator, matchingOperatorPatterns[x[[2, 1]]]],
				AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> x[[2, 1, 3, Key[Source]]], ConfidenceLevel -> 0.95 |>]];
				Throw[{list, issues}];
			];
			currentList = nodeListStack["Pop"];
			currentOperator[[2]] = Normal[currentList];
			(* finish constructing Source *)
			currentOperator[[3, Key[Source]]] = sourceSpan[sourceStart[currentOperator[[3, Key[Source]]]], sourceEnd[x[[2, 1, 3, Key[Source]]]]];
			peek = nodeListStack["Peek"];
			peek["Push", currentOperator];
		,
		(*
		All other calls to recognized directives

		GroupMissingCloserNode
		*)
		CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], { GroupMissingCloserNode[_, _, _] }, _],
			(*
			if GroupMissingCloserNode, then do not complain
			*)
			Throw[{list, issues}];
		,
		CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], _, _],
			AppendTo[issues, SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <| Source -> x[[3, Key[Source]]], ConfidenceLevel -> 0.7 |> ]];
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;

		GroupMissingCloserNode
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], { GroupMissingCloserNode[_, _, _] }, _], LeafNode[Symbol, "Null", _]}, _],
			(*
			if GroupMissingCloserNode, then do not complain
			*)
			Throw[{list, issues}];
		,
		(*
		All other calls to recognized directives, with ;
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "BeginPackage" | "Begin" | "System`Private`NewContextPath" | "EndPackage" | "End" | "System`Private`RestoreContextPath", _], _, _], LeafNode[Symbol, "Null", _]}, _],
			AppendTo[issues, SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <| Source -> x[[2, 1, 3, Key[Source]]], ConfidenceLevel -> 0.7 |>]];
			Throw[{list, issues}];
		,
		(*
		a,b  at top-level is an error
		*)
		CallNode[LeafNode[Symbol, "CodeParser`Comma", _], _, _],
			peek = nodeListStack["Peek"];
			error = AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, x[[2]], x[[3]]];
			peek["Push", error];
		,
		(*
		Format[a] := b  at top-level

		insert "Definitions" metadata for a
		
		*)
		CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {CallNode[LeafNode[Symbol, "Attributes" | "Format" | "Options", _], {_, ___}, _], _}, _] /; DefinitionSymbols[x[[2, 1, 2, 1]]] != {},
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], x[[2]], <| x[[3]], "Definitions" -> DefinitionSymbols[x[[2, 1, 2, 1]]] |> ];
			peek["Push", def];
		,
		(*
		Format[a] := b  at top-level ;

		insert "Definitions" metadata for a
		
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], { CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {CallNode[LeafNode[Symbol, "Attributes" | "Format" | "Options", _], {_, ___}, _], _}, _] /; DefinitionSymbols[x[[2, 1, 2, 1, 2, 1]]] != {}, LeafNode[Symbol, "Null", _] }, _],
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], { CallNode[x[[2, 1, 1]], x[[2, 1, 2]], <| x[[2, 1, 3]], "Definitions" -> DefinitionSymbols[x[[2, 1, 2, 1, 2, 1]]] |> ], x[[2, 2]] }, x[[3]]];
			peek["Push", def];
		,
		(*
		foo[] := 1+1  at top-level

		insert "Definitions" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {_, _}, _] /; DefinitionSymbols[x[[2, 1]]] != {},
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], x[[2]], <| x[[3]], "Definitions" -> DefinitionSymbols[x[[2, 1]]] |> ];
			peek["Push", def];
		,
		(*
		foo[] := 1+1  at top-level ;

		insert "Definitions" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], { CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {_, _}, _] /; DefinitionSymbols[x[[2, 1, 2, 1]]] != {}, LeafNode[Symbol, "Null", _] }, _],
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], { CallNode[x[[2, 1, 1]], x[[2, 1, 2]], <| x[[2, 1, 3]], "Definitions" -> DefinitionSymbols[x[[2, 1, 2, 1]]] |> ], x[[2, 2]] }, x[[3]]];
			peek["Push", def];
		,
		(*
		foo /: foo[] := 1+1  at top-level

		insert "Definitions" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "TagSet" | "TagSetDelayed", _], {_, _, _}, _] /; DefinitionSymbols[x[[2, 1]]] != {} || DefinitionSymbols[x[[2, 2]]] != {},
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], x[[2]], <| x[[3]], "Definitions" -> DefinitionSymbols[x[[2, 1]]] ~Join~ DefinitionSymbols[x[[2, 2]]] |> ];
			peek["Push", def];
		,
		(*
		foo /: foo[] := 1+1  at top-level ;

		insert "Definitions" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], { CallNode[LeafNode[Symbol, "TagSet" | "TagSetDelayed", _], {_, _, _}, _] /; DefinitionSymbols[x[[2, 1, 2, 1]]] != {} || DefinitionSymbols[x[[2, 1, 2, 2]]] != {}, LeafNode[Symbol, "Null", _] }, _],
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], { CallNode[x[[2, 1, 1]], x[[2, 1, 2]], <| x[[2, 1, 3]], "Definitions" -> DefinitionSymbols[x[[2, 1, 2, 1]]] ~Join~ DefinitionSymbols[x[[2, 1, 2, 2]]] |> ], x[[2, 2]] }, x[[3]]];
			peek["Push", def];
		,
		(*
		foo[bar[]] ^:= 1+1  at top-level

		insert "Definitions" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "UpSet" | "UpSetDelayed", _], {CallNode[_, _, _], _}, _] /; DefinitionSymbols[x[[2, 1, 1]]] != {} || AnyTrue[x[[2, 1, 2]], (DefinitionSymbols[#] != {})&],
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], x[[2]], <| x[[3]], "Definitions" -> DefinitionSymbols[x[[2, 1, 1]]] ~Join~ Flatten[DefinitionSymbols /@ x[[2, 1, 2]]] |> ];
			peek["Push", def];
		,
		(*
		foo[bar[]] ^:= 1+1  at top-level ;

		insert "Definitions" metadata for foo
		
		*)
		CallNode[LeafNode[Symbol, "CompoundExpression", _], { CallNode[LeafNode[Symbol, "UpSet" | "UpSetDelayed", _], {CallNode[_, _, _], _}, _] /; DefinitionSymbols[x[[2, 1, 2, 1, 1]]] != {} || AnyTrue[x[[2, 1, 2, 1, 2]], (DefinitionSymbols[#] != {})&], LeafNode[Symbol, "Null", _] }, _],
			peek = nodeListStack["Peek"];
			def = CallNode[x[[1]], { CallNode[x[[2, 1, 1]], x[[2, 1, 2]], <| x[[2, 1, 3]], "Definitions" -> DefinitionSymbols[x[[2, 1, 2, 1, 1]]] ~Join~ Flatten[DefinitionSymbols /@ x[[2, 1, 2, 1, 2]]] |> ], x[[2, 2]] }, x[[3]]];
			peek["Push", def];
		,
		(*
		All other expressions
		*)
		_,
			peek = nodeListStack["Peek"];
			peek["Push", x];
		]
		,
		{i, 1, Length[list]}
	]; (* Do *)

	If[operatorStack["Length"] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 0.7 |>]];
		Throw[{list, issues}];
	];
	If[operatorStack["Peek"] =!= None,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 0.7 |>]];
		Throw[{list, issues}];
	];
	If[nodeListStack["Length"] != 1,
		AppendTo[issues, SyntaxIssue["Package", "There are unbalanced directives.", "Error", <| Source -> list[[1, 3, Key[Source]]], ConfidenceLevel -> 0.7 |>]];
		Throw[{list, issues}];
	];
	peek = nodeListStack["Peek"];
	nodeList = Normal[peek];

	cleanupStackShimMemoryLeak[];

	{nodeList, issues}
]]



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
