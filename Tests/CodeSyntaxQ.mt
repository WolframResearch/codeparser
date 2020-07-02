(* Wolfram Language Test file *)

Needs["CodeParser`"]

(*
There should be no messages from CodeSyntaxQ
*)
Test[
	CodeSyntaxQ["#\"\\A\""]
	,
	True
	,
	{}
	,
	TestID->"CodeSyntaxQ-20200702-D6P6W9"
]
