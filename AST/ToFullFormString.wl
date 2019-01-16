BeginPackage["AST`ToFullFormString`"]

Begin["`Private`"]

Needs["AST`"]

(*
ToFullFormString is intended for abstract syntax trees
*)


ToFullFormString[SymbolNode[str_, _, _]] :=
	str

(*
strings may not be quoted, a::b
*)
ToFullFormString[StringNode[str_, _, _]] :=
	If[StringStartsQ[str, "\""],
		str,
		"\""<>str<>"\""
	]

ToFullFormString[NumberNode[str_, _, _]] :=
	str

ToFullFormString[CallNode[head_, nodes_, _]] :=
Catch[
Module[{headStr, nodeStrs},
	headStr = ToFullFormString[head];
	If[FailureQ[headStr],
		Throw[headStr]
	];
	nodeStrs = ToFullFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{headStr, "[", Riffle[nodeStrs, ", "], "]"}]
]]



(*
linear syntax is skipped right now
*)
ToFullFormString[p:PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
	ToInputFormString[p]

ToFullFormString[g:GroupNode[GroupLinearSyntaxParen, _, _]] :=
	ToInputFormString[g]





ToFullFormString[FileNode[File, nodes_, opts_]] :=
Module[{nodeStrs},
	nodeStrs = ToFullFormString /@ nodes;
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
]

ToFullFormString[HoldNode[Hold, nodes_, opts_]] :=
	ToFullFormString[CallNode[ToNode[Hold], nodes, <||>]]



ToFullFormString[f_Failure] := f

ToFullFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
