BeginPackage["AST`Node`"]

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]

(*
ToNode[sym] returns a SymbolNode
*)
ToNode[s_Symbol] :=
	If[Context[s] == "System`",
		SymbolNode[SymbolName[s], {}, <||>]
		,
		(*
		Play it safe for now and fully qualify any non-System` symbol
		*)
		SymbolNode[Context[s]<>SymbolName[s], {}, <||>]
	]

(*
ToNode[string] returns a StringNode
*)
ToNode[s_String] := StringNode[escapeString[s], {}, <||>]

(*
ToNode[number] returns a NumberNode
*)
ToNode[s_Integer] := NumberNode[ToString[s, InputForm], {}, <||>]
ToNode[s_Real] := NumberNode[ToString[s, InputForm], {}, <||>]


ToNode[args___] := Failure["Unhandled", <|"Function"->ToNode, "Arguments"->HoldForm[{args}]|>]



FromNode[SymbolNode[s_, {}, _]] :=
	Symbol[s]

(*
No simple way to convert "\"123\"" to "123"
*)
FromNode[StringNode[s_, {}, _]] :=
	ToExpression[s]

(*
No simple way to convert "123.456``7" to 123.456``7
*)
FromNode[NumberNode[s_, {}, _]] :=
	ToExpression[s]


FromNode[args___] := Failure["InternalUnhandled", <|"Function"->FromNode, "Arguments"->HoldForm[{args}]|>]



End[]

EndPackage[]
