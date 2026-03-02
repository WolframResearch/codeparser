

path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]




Test[
	"a \\[LeftArrow] b \\[LeftArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-N8B8X3"
]

Test[
	"a \\[UpArrow] b \\[UpArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-O5V5V9"
]

Test[
	"a \\[RightArrow] b \\[RightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-O2N1Y0"
]

Test[
	"a \\[DownArrow] b \\[DownArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-N5I5G6"
]

Test[
	"a \\[LeftRightArrow] b \\[LeftRightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-F9O7N7"
]

Test[
	"a \\[UpDownArrow] b \\[UpDownArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-M1C1I9"
]

Test[
	"a \\[UpperLeftArrow] b \\[UpperLeftArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-O3X7M5"
]

Test[
	"a \\[UpperRightArrow] b \\[UpperRightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-Q8W1C9"
]

Test[
	"a \\[LowerRightArrow] b \\[LowerRightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-Q1E8M4"
]

Test[
	"a \\[LowerLeftArrow] b \\[LowerLeftArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-A2O1T8"
]

Test[
	"a \\[LeftTeeArrow] b \\[LeftTeeArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-P2X7V9"
]

Test[
	"a \\[UpTeeArrow] b \\[UpTeeArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-J3P4S2"
]

Test[
	"a \\[RightTeeArrow] b \\[RightTeeArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-M3P7L2"
]

Test[
	"a \\[DownTeeArrow] b \\[DownTeeArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-U8I5N7"
]


Test[
	"a \\[LeftVector] b \\[LeftVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-U9B1R3"
]

Test[
	"a \\[DownLeftVector] b \\[DownLeftVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-V6L1I6"
]

Test[
	"a \\[RightUpVector] b \\[RightUpVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-C6B2E3"
]

Test[
	"a \\[LeftUpVector] b \\[LeftUpVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-T9F1U9"
]

Test[
	"a \\[RightVector] b \\[RightVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-G6H9L1"
]

Test[
	"a \\[DownRightVector] b \\[DownRightVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-H9K2I2"
]

Test[
	"a \\[RightDownVector] b \\[RightDownVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-L9P8C2"
]

Test[
	"a \\[LeftDownVector] b \\[LeftDownVector] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-J9F4O6"
]

Test[
	"a \\[RightArrowLeftArrow] b \\[RightArrowLeftArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-W7O3E7"
]

Test[
	"a \\[UpArrowDownArrow] b \\[UpArrowDownArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-C2Q9H3"
]

Test[
	"a \\[LeftArrowRightArrow] b \\[LeftArrowRightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-G1S8C6"
]

Test[
	"a \\[DoubleLeftArrow] b \\[DoubleLeftArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-Q8F5B9"
]

Test[
	"a \\[DoubleUpArrow] b \\[DoubleUpArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-F1T2R4"
]

Test[
	"a \\[DoubleRightArrow] b \\[DoubleRightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-T2H5G5"
]

Test[
	"a \\[DoubleDownArrow] b \\[DoubleDownArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-Q8M5K8"
]

Test[
	"a \\[DoubleLeftRightArrow] b \\[DoubleLeftRightArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-T8W9Z5"
]

Test[
	"a \\[DoubleUpDownArrow] b \\[DoubleUpDownArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-N8P5V5"
]

Test[
	"a \\[LeftArrowBar] b \\[LeftArrowBar] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-H9A2T0"
]

Test[
	"a \\[RightArrowBar] b \\[RightArrowBar] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-T1I2I8"
]

Test[
	"a \\[DownArrowUpArrow] b \\[DownArrowUpArrow] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-P0X4R9"
]

Test[
	"a \\[ReverseEquilibrium] b \\[ReverseEquilibrium] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-I5E6H3"
]

Test[
	"a \\[Equilibrium] b \\[Equilibrium] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Arrows-20190629-O7V1H2"
]


