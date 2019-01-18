(* Wolfram Language Test file *)

Needs["AST`"]

(*
obscure syntax
*)

Test[
	"\\[Integral] a \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-W8E4P4"
]




