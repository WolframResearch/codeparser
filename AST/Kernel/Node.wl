BeginPackage["AST`Node`"]

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]



(*
Catch incorrect uses of node ctors
*)
(*
SymbolNode[a1_, a2_] := Throw[Failure["Unhandled", <|"Function"->SymbolNode, "Arguments"->HoldForm[{a1, a2}]|>], "Unhandled"]

SymbolNode[a1_String, a2_, a3_] := Throw[Failure["Unhandled", <|"Function"->SymbolNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

SymbolNode[a1_, a2_List, a3_] := Throw[Failure["Unhandled", <|"Function"->SymbolNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

SymbolNode[a1_, a2_, a3_, a4__] := Throw[Failure["Unhandled", <|"Function"->SymbolNode, "Arguments"->HoldForm[{a1, a2, a3, a4}]|>], "Unhandled"]

SymbolNode /: SymbolNode[args___][[1]] := Throw[Failure["UnhandledPart", <|"Function"->SymbolNode, "Arguments"->HoldForm[{args}]|>], "Unhandled"]
*)

(*
StringNode[a1_, a2_] := Throw[Failure["Unhandled", <|"Function"->StringNode, "Arguments"->HoldForm[{a1, a2}]|>], "Unhandled"]

StringNode[a1_String, a2_, a3_] := Throw[Failure["Unhandled", <|"Function"->StringNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

StringNode[a1_, a2_List, a3_] := Throw[Failure["Unhandled", <|"Function"->StringNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

StringNode[a1_, a2_, a3_, a4__] := Throw[Failure["Unhandled", <|"Function"->StringNode, "Arguments"->HoldForm[{a1, a2, a3, a4}]|>], "Unhandled"]

StringNode /: StringNode[args___][[1]] := Throw[Failure["UnhandledPart", <|"Function"->StringNode, "Arguments"->HoldForm[{args}]|>], "Unhandled"]
*)

(*
IntegerNode[a1_, a2_] := Throw[Failure["Unhandled", <|"Function"->IntegerNode, "Arguments"->HoldForm[{a1, a2}]|>], "Unhandled"]

IntegerNode[a1_String, a2_, a3_] := Throw[Failure["Unhandled", <|"Function"->IntegerNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

IntegerNode[a1_, a2_List, a3_] := Throw[Failure["Unhandled", <|"Function"->IntegerNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

IntegerNode[a1_, a2_, a3_, a4__] := Throw[Failure["Unhandled", <|"Function"->IntegerNode, "Arguments"->HoldForm[{a1, a2, a3, a4}]|>], "Unhandled"]

IntegerNode /: IntegerNode[args___][[1]] := Throw[Failure["UnhandledPart", <|"Function"->IntegerNode, "Arguments"->HoldForm[{args}]|>], "Unhandled"]
*)

(*
RealNode[a1_, a2_] := Throw[Failure["Unhandled", <|"Function"->RealNode, "Arguments"->HoldForm[{a1, a2}]|>], "Unhandled"]

RealNode[a1_String, a2_, a3_] := Throw[Failure["Unhandled", <|"Function"->RealNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

RealNode[a1_, a2_List, a3_] := Throw[Failure["Unhandled", <|"Function"->RealNode, "Arguments"->HoldForm[{a1, a2, a3}]|>], "Unhandled"]

RealNode[a1_, a2_, a3_, a4__] := Throw[Failure["Unhandled", <|"Function"->RealNode, "Arguments"->HoldForm[{a1, a2, a3, a4}]|>], "Unhandled"]

RealNode /: RealNode[args___][[1]] := Throw[Failure["UnhandledPart", <|"Function"->RealNode, "Arguments"->HoldForm[{args}]|>], "Unhandled"]
*)



(*
CallNode[a1_, a2_] := Throw[Failure["Unhandled", <|"Function"->CallNode, "Arguments"->HoldForm[{a1, a2}]|>], "Unhandled"]

CallNode[a1_, a2_, a3_, a4__] := Throw[Failure["Unhandled", <|"Function"->CallNode, "Arguments"->HoldForm[{a1, a2, a3, a4}]|>], "Unhandled"]
*)





(*
Some selectors
*)


SymbolNode[_, str_, _]["String"] := str

StringNode[_, str_, _]["String"] := str

OutNode[_, str_, _]["String"] := str

TokenNode[_, str_, _]["String"] := str





(*
ToNode[sym] returns a SymbolNode
*)
ToNode[s_Symbol] :=
	If[Context[s] == "System`",
		SymbolNode[Symbol, SymbolName[s], <||>]
		,
		(*
		Play it safe for now and fully qualify any non-System` symbol
		*)
		SymbolNode[Symbol, Context[s]<>SymbolName[s], <||>]
	]

(*
ToNode[string] returns a StringNode
*)
ToNode[s_String] := StringNode[String, escapeString[s], <||>]

(*
ToNode[integer] returns an IntegerNode
ToNode[real] returns a RealNode
*)
ToNode[s_Integer] := IntegerNode[Integer, ToString[s, InputForm], <||>]
ToNode[s_Real] := RealNode[Real, ToString[s, InputForm], <||>]


ToNode[args___] := Failure["Unhandled", <|"Function"->ToNode, "Arguments"->HoldForm[{args}]|>]



FromNode[SymbolNode[Symbol, s_, _]] :=
	Symbol[s]

(*
No simple way to convert "\"123\"" to "123"
*)
FromNode[StringNode[String, s_, _]] :=
	ToExpression[s]

(*
No simple way to convert "123.456``7" to 123.456``7
*)
FromNode[IntegerNode[Integer, s_, _]] :=
	ToExpression[s]

FromNode[RealNode[Real, s_, _]] :=
	ToExpression[s]


FromNode[args___] := Failure["InternalUnhandled", <|"Function"->FromNode, "Arguments"->HoldForm[{args}]|>]



End[]

EndPackage[]
