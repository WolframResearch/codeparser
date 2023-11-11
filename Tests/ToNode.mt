Print["\n===== Start ToNode.mt =====\n"]

Needs["CodeParser`"]


Clear[a]

Test[
	ToNode[a]
	,
	LeafNode[Symbol, "Global`a", <||>]
	,
	TestID->"ToNode-20181230-L1R6Q9"
]


Test[
	ToNode["abc"]
	,
	LeafNode[String, "\"abc\"", <||>]
	,
	TestID->"ToNode-20181230-S1R5V6"
]


Test[
	ToNode[123]
	,
	LeafNode[Integer, "123", <||>]
	,
	TestID->"ToNode-20181230-O2A4T0"
]


Test[
	ToNode[1.23]
	,
	LeafNode[Real, "1.23", <||>]
	,
	TestID->"ToNode-20181230-E5S9U5"
]



rat = 1/16

Test[
	ToNode[rat]
	,
	LeafNode[Rational, "16^^1*^-1", <||>]
	,
	TestID->"ToNode-20200413-V3L1T8"
]


rat = 1/37

Test[
	ToNode[rat]
	,
	CallNode[LeafNode[Symbol, "Rational", <||>], {LeafNode[Integer, "1", <||>], LeafNode[Integer, "37", <||>]}, <||>]
	,
	TestID->"ToNode-20200413-V2I2X0"
]







