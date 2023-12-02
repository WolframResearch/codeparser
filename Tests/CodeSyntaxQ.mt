Print["\n===== Start CodeSyntaxQ.mt =====\n"]

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

Test[
	CodeSyntaxQ["a>>b\\1c"]
	,
	True
	,
	{}
	,
	TestID->"CodeSyntaxQ-20200703-Q2R5G9"
]



Test[
	CodeSyntaxQ[File["doesntexist"]]
	,
	False
	,
	TestID->"CodeSyntaxQ-20230426-X7Z7D3"
]
