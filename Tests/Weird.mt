
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "ASTTestUtils"}]
PrependTo[$Path, path]

Needs["ASTTestUtils`"]

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
	"\\[Integral] x+y \\[DifferentialD]x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20190623-G9O2K9"
]

Test[
	"\\[Integral] x^y \\[DifferentialD]x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20190623-H8D1Y1"

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

Test[
	"\\[Integral]!b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20191016-C1Z5S5"
]





(*
Error
*)

TestMatch[
	ParseString["\\[Integral] f \\[DifferentialD]"]
	,
	StringNode[String, {
		CallNode[LeafNode[Symbol, "Integrate", <||>], {
			LeafNode[Symbol, "f", <|Source -> {{1, 13}, {1, 14}}|>],
			AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedOperand, {
				LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 31}, {1, 31}}|>]},
				<|Source -> {{1, 31}, {1, 31}}|>]},
			<|Source -> {{1, 1}, {1, 31}}|>] },
		<||>]
	,
	TestID->"Weird-20190601-M2O1W0"
]

Test[
	"\\[Integral] 9 \\[CapitalDifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20191223-S8T6T5"
]
















