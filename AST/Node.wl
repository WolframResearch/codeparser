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


End[]

EndPackage[]
