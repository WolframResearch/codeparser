
Needs["AST`"]


Clear[a]

Test[
	ToNode[a]
	,
	SymbolNode["Global`a", {}, <||>]
	,
	TestID->"ToNode-20181230-L1R6Q9"
]


Test[
	ToNode["abc"]
	,
	StringNode["\"abc\"", {}, <||>]
	,
	TestID->"ToNode-20181230-S1R5V6"
]


Test[
	ToNode[123]
	,
	IntegerNode["123", {}, <||>]
	,
	TestID->"ToNode-20181230-O2A4T0"
]


Test[
	ToNode[1.23]
	,
	RealNode["1.23", {}, <||>]
	,
	TestID->"ToNode-20181230-E5S9U5"
]




