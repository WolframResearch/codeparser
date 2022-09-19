
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



Test[
	"%[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-J8L7N8"
]

Test[
	"%%%[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-D3S9K4"
]

Test[
	"%45[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-A9R7S9"
]


Test[
	"a;b[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-O2G0U6"
]

Test[
	"(a;b)[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-G3Y5B8"
]

Test[
	"{}[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-K9T8A2"
]

Test[
	"<||>[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-L8D0T8"
]

Test[
	"a\\[Transpose][[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-D6E7X6"
]

Test[
	"\\[LeftCeiling]\\[RightCeiling][[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-O9M6L6"
]








Test[
	"%[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-V4F3I2"
]

Test[
	"%%%[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Z7Y1N6"
]

Test[
	"%45[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-T6L5S2"
]


Test[
	"a;b[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-J0D7S6"
]

Test[
	"(a;b)[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Q8T9P6"
]

Test[
	"{}[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-F3I0G6"
]

Test[
	"<||>[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-W3G8E5"
]

Test[
	"a\\[Transpose][]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-D3T8P7"
]

Test[
	"\\[LeftCeiling]\\[RightCeiling][]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-W2E0S1"
]





Test[
	"%::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Q8Q5S8"
]

Test[
	"%%%::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-W8D8J0"
]

Test[
	"%45::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-E3P1I4"
]


Test[
	"a;b::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-F3S4Q9"
]

Test[
	"(a;b)::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-C1H7W1"
]

Test[
	"{}::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Q0V6H1"
]

Test[
	"<||>::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-F1N9W4"
]

Test[
	"a\\[Transpose]::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Y1B8L5"
]

Test[
	"\\[LeftCeiling]\\[RightCeiling]::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-D4X6I3"
]







Test[
	"%\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-L5T6W2"
]

Test[
	"%%%\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-S4I7X6"
]

Test[
	"%45\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-N0M4X4"
]


Test[
	"a;b\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-N6P0J4"
]

Test[
	"(a;b)\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-K9P3E3"
]

Test[
	"{}\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-T6K0B4"
]

Test[
	"<||>\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Q6X9G5"
]

Test[
	"a\\[Transpose]\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-F0V8S8"
]

Test[
	"\\[LeftCeiling]\\[RightCeiling]\\[LeftDoubleBracket\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-O5K5L1"
]






Test[
	"\(\)[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-Y8S2H4"
]

Test[
	"\(\)[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-H3I8O4"
]

Test[
	"\(\)::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-A0R9T7"
]

Test[
	"\(\)\\[LeftDoubleBracket]\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-B7F8I0"
]

Test[
	"\!\(\)[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-R4G4P7"
]

Test[
	"\!\(\)[[]]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-J5L3T1"
]

Test[
	"\!\(\)::[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-M7V1S8"
]

Test[
	"\!\(\)\\[LeftDoubleBracket]\\[RightDoubleBracket]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"AbstractCallNode-20220919-O7W8Y4"
]
















