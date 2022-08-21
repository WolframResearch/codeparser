
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



Test[
	"\\[Integral] \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20220808-C0V6P1"
]










Test[
	CodeParse["a ~{}~ b"]
	,
	ContainerNode[String, {
		CallNode[CallNode[LeafNode[Symbol, "List", <||>], {}, <|Source -> {{1, 4}, {1, 6}}|>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 8}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Weird-20220629-I9X4S4"
]

Test[
	CodeParse["a ~f;~ b"]
	,
	ContainerNode[String, {
		CallNode[CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 5}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 4}, {1, 6}}|>], {
			
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 8}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Weird-20220629-F5L9D1"
]

Test[
	CodeParse["a ~f;;~ b"]
	,
	ContainerNode[String, {
		CallNode[CallNode[LeafNode[Symbol, "Span", <||>], {
			LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 5}}|>],
			LeafNode[Symbol, "All", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 4}, {1, 7}}|>], {
			
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 9}, {1, 10}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>]
	,
	TestID->"Weird-20220629-Q2W2O5"
]


Test[
	CodeParse["a;[]"]
	,
	ContainerNode[String, {
		CallNode[CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>], {}, <|Source -> {{1, 1}, {1, 5}}, AbstractSyntaxIssues -> {
		
		SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|Source -> {{1, 3}, {1, 4}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 4}, {1, 5}}}|>]}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"Weird-20220629-T9B5G3"
]

Test[
	CodeParse["a;;[]"]
	,
	ContainerNode[String, {
		CallNode[CallNode[LeafNode[Symbol, "Span", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "All", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>], {}, <|Source -> {{1, 1}, {1, 6}}, AbstractSyntaxIssues -> {
		
		SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 4}, {1, 5}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 5}, {1, 6}}}|>]}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"Weird-20220629-L9O4J4"
]



(*
kernel bugs
*)

Test[
	CodeParse["x . -y"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Dot", <||>], {
			LeafNode[Symbol, "x", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Times", <||>], {
				LeafNode[Integer, "-1", <||>],
				LeafNode[Symbol, "y", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 5}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Weird-20220629-J5E3Z0"
]

Test[
	CodeParse["x \\[DifferentialD] !y"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Times", <||>], {
			LeafNode[Symbol, "x", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "DifferentialD", <||>], {
				CallNode[LeafNode[Symbol, "Not", <||>], {
					LeafNode[Symbol, "y", <|Source -> {{1, 21}, {1, 22}}|>]}, <|Source -> {{1, 20}, {1, 22}}|>]}, <|Source -> {{1, 3}, {1, 22}}|>]}, <|Source -> {{1, 1}, {1, 22}}|>]}, <|Source -> {{1, 1}, {1, 22}}|>]
	,
	TestID->"Weird-20220629-V6B4O2"
]

Test[
	CodeParse["x \\[CapitalDifferentialD] !y"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Times", <||>], {
			LeafNode[Symbol, "x", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "CapitalDifferentialD", <||>], {
				CallNode[LeafNode[Symbol, "Not", <||>], {
					LeafNode[Symbol, "y", <|Source -> {{1, 28}, {1, 29}}|>]}, <|Source -> {{1, 27}, {1, 29}}|>]}, <|Source -> {{1, 3}, {1, 29}}|>]}, <|Source -> {{1, 1}, {1, 29}}|>]}, <|Source -> {{1, 1}, {1, 29}}|>]
	,
	TestID->"Weird-20220629-T3T9P8"
]

Test[
	CodeParse["x \\[CircleTimes] \\[CircleTimes] y"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CircleTimes", <||>], {
			LeafNode[Symbol, "x", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "CircleTimes", <||>], {
				LeafNode[Symbol, "y", <|Source -> {{1, 33}, {1, 34}}|>]}, <|Source -> {{1, 18}, {1, 34}}|>]}, <|Source -> {{1, 1}, {1, 34}}|>]}, <|Source -> {{1, 1}, {1, 34}}|>]
	,
	TestID->"Weird-20220629-L0Y6R5"
]


Test[
	CodeParse["\\[Sqrt] \\[CircleTimes] x"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Sqrt", <||>], {
			CallNode[LeafNode[Symbol, "CircleTimes", <||>], {
				LeafNode[Symbol, "x", <|Source -> {{1, 24}, {1, 25}}|>]}, <|Source -> {{1, 9}, {1, 25}}|>]}, <|Source -> {{1, 1}, {1, 25}}|>]}, <|Source -> {{1, 1}, {1, 25}}|>]
	,
	TestID->"Weird-20220629-J1S9O6"
]

Test[
	CodeParse["x' '"]
	,
	ContainerNode[String, {
		CallNode[CallNode[LeafNode[Symbol, "Derivative", <||>], {
			LeafNode[Integer, "2", <||>]}, <||>], {LeafNode[Symbol, "x", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"Weird-20220629-C5X4D3"
]

Test[
	"\\[CountourIntegral] a \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20220821-D0P3C2"
]

Test[
	"\\[DoubleCountourIntegral] a \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20220821-H4S8N6"
]

Test[
	"\\[ClockwiseCountourIntegral] a \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20220821-C2P4Y0"
]

Test[
	"\\[CounterClockwiseCountourIntegral] a \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Weird-20220821-X2S1Q0"
]