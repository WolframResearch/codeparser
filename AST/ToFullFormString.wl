BeginPackage["AST`ToFullFormString`"]

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]

(*
ToFullFormString is intended for abstract syntax trees
*)

ToFullFormString[cst_] :=
Block[{$RecursionLimit = Infinity},
	toFullFormString[cst]
]


toFullFormString[SymbolNode[str_, _, _]] :=
	str

(*
strings may not be quoted, a::b
*)
toFullFormString[StringNode[str_, _, _]] :=
	If[StringStartsQ[str, "\""],
		str,
		escapeString[str]
	]

toFullFormString[NumberNode[str_, _, _]] :=
	str

toFullFormString[CallNode[head_, nodes_, _]] :=
Catch[
Module[{headStr, nodeStrs},
	headStr = toFullFormString[head];
	If[FailureQ[headStr],
		Throw[headStr]
	];
	nodeStrs = toFullFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{headStr, "[", Riffle[nodeStrs, ", "], "]"}]
]]



(*
linear syntax is skipped right now
*)
toFullFormString[p:PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
	ToInputFormString[p]

toFullFormString[g:GroupNode[GroupLinearSyntaxParen, _, _]] :=
	ToInputFormString[g]





toFullFormString[FileNode[File, nodes_, opts_]] :=
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
]

toFullFormString[HoldNode[Hold, {}, opts_]] :=
	toFullFormString[CallNode[ToNode[Hold], {ToNode[Null]}, <||>]]

toFullFormString[HoldNode[Hold, nodes_, opts_]] :=
	toFullFormString[CallNode[ToNode[Hold], nodes, <||>]]



toFullFormString[n_SyntaxErrorNode] := Failure["SyntaxError", <|"Error"->n|>]

toFullFormString[f_Failure] := f

toFullFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
