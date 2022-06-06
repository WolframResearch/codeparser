
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]



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





(*
Error
*)

Test[
	CodeParse["\\[Integral] f \\[DifferentialD]"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Integrate", <||>], {
			LeafNode[Symbol, "f", <|Source -> {{1, 13}, {1, 14}}|>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 31}, {1, 31}}|>]}, <|Source -> {{1, 1}, {1, 31}}|>] }, <|Source -> {{1, 1}, {1, 31}}|>]
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





(*
bug 410404
*)

Test[
	"\\[Integral] a + 2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20210521-W7U9U1"
]










