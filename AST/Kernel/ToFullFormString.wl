BeginPackage["AST`ToFullFormString`"]

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]

(*
ToFullFormString is intended for abstract syntax trees
*)

ToFullFormString[ast_] :=
Catch[
Module[{str},
Block[{$RecursionLimit = Infinity},
	str = toFullFormString[ast];
	If[FailureQ[str],
		Throw[str]
	];
	StringJoin[str]
]]]


toFullFormString[SymbolNode[Symbol, str_, _]] :=
	str

(*
strings may not originally be quoted, a::b

But they become quoted when they are abstracted
*)
toFullFormString[node:StringNode[String, str_, _]] :=
Catch[
	If[!StringStartsQ[str, "\""],
		Throw[Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{node}]|>]]
	];
	str
]

toFullFormString[IntegerNode[Integer, str_, _]] :=
	str

toFullFormString[RealNode[Real, str_, _]] :=
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
FIXME: linear syntax is skipped right now
*)
toFullFormString[p:PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
	ToInputFormString[p]

toFullFormString[g:GroupNode[GroupLinearSyntaxParen, _, _]] :=
	ToInputFormString[g]





toFullFormString[FileNode[File, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	If[empty[nodes],
		nodeStrs = {"Null"}
		,
		nodeStrs = toFullFormString /@ nodes;
		nodeStrs = Flatten[nodeStrs];
	];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[Riffle[nodeStrs, "\n"]]
]]

toFullFormString[HoldNode[Hold, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	If[empty[nodes],
		nodeStrs = {"Null"}
		,
		nodeStrs = toFullFormString /@ nodes;
		nodeStrs = Flatten[nodeStrs];
	];
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	StringJoin[{"Hold", "[", Riffle[nodeStrs, ", "], "]"}]
]]




(*
returns a list
*)
toFullFormString[PackageNode[args_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ ({CallNode[ToNode[BeginPackage], args, <||>]} ~Join~ nodes ~Join~ {CallNode[ToNode[EndPackage], {}, <||>]});
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]

(*
returns a list
*)
toFullFormString[ContextNode[args_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ ({CallNode[ToNode[Begin], args, <||>]} ~Join~ nodes ~Join~ {CallNode[ToNode[End], {}, <||>]});
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]

(*
returns a list
*)
toFullFormString[StaticAnalysisIgnoreNode[args_, nodes_, opts_]] :=
Catch[
Module[{nodeStrs},
	nodeStrs = toFullFormString /@ ({CallNode[ToNode[BeginStaticAnalysisIgnore], args, <||>]} ~Join~ nodes ~Join~ {CallNode[ToNode[EndStaticAnalysisIgnore], {}, <||>]});
	If[AnyTrue[nodeStrs, FailureQ],
		Throw[SelectFirst[nodeStrs, FailureQ]]
	];
	nodeStrs
]]



(*
ParseString[""] returns Null, so handle that
*)
toFullFormString[Null] := "Null"





toFullFormString[n_SyntaxErrorNode] := Failure["SyntaxError", <|"Error"->n|>]

toFullFormString[f_?FailureQ] := f

toFullFormString[args___] := Failure["InternalUnhandled", <|"Function"->ToFullFormString, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
