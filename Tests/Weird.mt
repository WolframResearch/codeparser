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
	TestID->"Weird-20181202-W8E4P4"
]


Test[
	"\\[Integral]x/(a^2+x^2)^(3/2) \\[DifferentialD]x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20190601-G4E6Y0"
]





(*
Error
*)

Test[
	ParseString["\\[Integral] f \\[DifferentialD]"]
	,
	CallNode[SymbolNode[Symbol, "Integrate", <||>], {SymbolNode[Symbol, "f", <|Source -> {{1, 13}, {1, 13}}|>],
		SyntaxErrorNode[SyntaxError`ExpectedPossibleExpression, {TokenNode[Token`EndOfFile, "", <|Source -> {{2, 0}, {2, 0}}|>]},
			<|Source -> {{2, 0}, {2, 0}}|>]}, <|Source -> {{1, 1}, {2, 0}}|>]
	,
	TestID->"Weird-20190601-M2O1W0"
]

