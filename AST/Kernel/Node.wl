BeginPackage["AST`Node`"]

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]


(*
Some selectors
*)


LeafNode[_, str_, _]["String"] := str





(*
ToNode[sym] returns a SymbolNode
*)
ToNode[s_Symbol] :=
	If[Context[s] == "System`",
		LeafNode[Symbol, SymbolName[s], <||>]
		,
		(*
		Play it safe for now and fully qualify any non-System` symbol
		*)
		LeafNode[Symbol, Context[s]<>SymbolName[s], <||>]
	]

(*
ToNode[string] returns a StringNode
*)
ToNode[s_String] := LeafNode[String, escapeString[s], <||>]

(*
ToNode[integer] returns an IntegerNode
ToNode[real] returns a RealNode
*)
ToNode[s_Integer] := LeafNode[Integer, ToString[s, InputForm], <||>]
ToNode[s_Real] := LeafNode[Real, ToString[s, InputForm], <||>]


ToNode[args___] := Failure["Unhandled", <|"Function"->ToNode, "Arguments"->HoldForm[{args}]|>]



FromNode[LeafNode[Symbol, s_, _]] :=
	Symbol[s]

(*
No simple way to convert "\"123\"" to "123"
*)
FromNode[LeafNode[String, s_, _]] :=
	ToExpression[s]

(*
No simple way to convert "123.456``7" to 123.456``7
*)
FromNode[LeafNode[Integer, s_, _]] :=
	ToExpression[s]

FromNode[LeafNode[Real, s_, _]] :=
	ToExpression[s]


FromNode[args___] := Failure["InternalUnhandled", <|"Function"->FromNode, "Arguments"->HoldForm[{args}]|>]



End[]

EndPackage[]
