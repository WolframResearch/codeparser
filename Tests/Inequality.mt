path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



Test[
	"a < b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181207-M8H7A4"
]


Test[
	"a == b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-L0T2T5"
]

Test[
	"a != b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-V9W5X1"
]

Test[
	"a \[Equal] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-F3V9J5"
]

Test[
	"a \[LongEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-C9F6J3"
]

Test[
	"a < b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-M2V4O9"
]

Test[
	"a > b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-L6A4B3"
]

Test[
	"a <= b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-N1U0N1"
]

Test[
	"a \[LessEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-A5Y7E1"
]

Test[
	"a <= b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-W1L6L5"
]

Test[
	"a \[GreaterEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-T3A9L8"
]

Test[
	"a \[GreaterEqualLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-T3S8E9"
]

Test[
	"a \[GreaterFullEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-E7E5E2"
]

Test[
	"a \[GreaterGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-D4L0M7"
]

Test[
	"a \[GreaterLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-X7E2I5"
]

Test[
	"a \[GreaterSlantEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-X2V7C1"
]

Test[
	"a \[GreaterTilde] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-J2Z2R8"
]

Test[
	"a \[LessEqualGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-X0W6O8"
]

Test[
	"a \[LessFullEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-L6B5Q2"
]

Test[
	"a \[LessGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-Y7P7Q1"
]

Test[
	"a \[LessLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-L3T7H3"
]

Test[
	"a \[LessSlantEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-K1E3O0"
]

Test[
	"a \[LessTilde] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-C7C5Y6"
]

Test[
	"a \[NestedGreaterGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-V6Q7I1"
]

Test[
	"a \[NestedLessLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-U4B4I6"
]

Test[
	"a \[NotGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-L0N0F2"
]

Test[
	"a \[NotGreaterEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-A6L7G0"
]

Test[
	"a \[NotGreaterFullEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-V3V6Y0"
]

Test[
	"a \[NotGreaterGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-B5P2Q2"
]

Test[
	"a \[NotGreaterLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-A4V4C9"
]

Test[
	"a \[NotGreaterSlantEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-N6A7S7"
]

Test[
	"a \[NotGreaterTilde] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-I2N1J8"
]

Test[
	"a \[NotLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-G0K9R0"
]

Test[
	"a \[NotLessEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-P8F0W0"
]

Test[
	"a \[NotLessFullEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-N5Y5C3"
]

Test[
	"a \[NotLessGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-J7Y6M9"
]

Test[
	"a \[NotLessLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-M4T1S8"
]

Test[
	"a \[NotLessSlantEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-J6B9U6"
]

Test[
	"a \[NotLessTilde] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-A3Y2C1"
]

Test[
	"a \[NotNestedGreaterGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-C1Q9I7"
]

Test[
	"a \[NotNestedLessLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-K3S8H8"
]

Test[
	"a \[VectorLess] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-O9P7V5"
]

Test[
	"a \[VectorGreater] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-Q6K9S2"
]

Test[
	"a \[VectorLessEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-M6A0B5"
]

Test[
	"a \[VectorGreaterEqual] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-K7S9O2"
]













Test[
	"a == b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-O0U1Q1"
]

Test[
	"a != b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-G3S2S5"
]

Test[
	"a \[Equal] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-P1U5N5"
]

Test[
	"a \[LongEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-H6V8H3"
]

Test[
	"a < b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-M9K3Z1"
]

Test[
	"a > b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-N6L8E3"
]

Test[
	"a <= b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-U4F8W2"
]

Test[
	"a \[LessEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-R6R0P3"
]

Test[
	"a <= b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-L7V4V7"
]

Test[
	"a \[GreaterEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-F7M6T9"
]

Test[
	"a \[GreaterEqualLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-X7L3Q2"
]

Test[
	"a \[GreaterFullEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-E1O9U8"
]

Test[
	"a \[GreaterGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-E7F5D1"
]

Test[
	"a \[GreaterLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-H0B3S9"
]

Test[
	"a \[GreaterSlantEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-I5Q5F1"
]

Test[
	"a \[GreaterTilde] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-D4M6W1"
]

Test[
	"a \[LessEqualGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-C1M0H6"
]

Test[
	"a \[LessFullEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-U0S3U5"
]

Test[
	"a \[LessGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-F8Z0J1"
]

Test[
	"a \[LessLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-D8Q3Y4"
]

Test[
	"a \[LessSlantEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-Q0S1H4"
]

Test[
	"a \[LessTilde] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-G3I4P8"
]

Test[
	"a \[NestedGreaterGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-B9Y3C7"
]

Test[
	"a \[NestedLessLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-E1A4I3"
]

Test[
	"a \[NotGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-X8K9N1"
]

Test[
	"a \[NotGreaterEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-M4B1Z6"
]

Test[
	"a \[NotGreaterFullEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-R1F8S9"
]

Test[
	"a \[NotGreaterGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-R5S9A2"
]

Test[
	"a \[NotGreaterLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-U6D6P8"
]

Test[
	"a \[NotGreaterSlantEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-N3M3B7"
]

Test[
	"a \[NotGreaterTilde] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-E2O0U5"
]

Test[
	"a \[NotLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-A5S4Y3"
]

Test[
	"a \[NotLessEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-J7Y3B4"
]

Test[
	"a \[NotLessFullEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-M2X4H6"
]

Test[
	"a \[NotLessGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-R5B9Q2"
]

Test[
	"a \[NotLessLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-Y2P1X7"
]

Test[
	"a \[NotLessSlantEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-I6T9B3"
]

Test[
	"a \[NotLessTilde] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-V3R5L8"
]

Test[
	"a \[NotNestedGreaterGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-Y2Y9B0"
]

Test[
	"a \[NotNestedLessLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-K4Y6U9"
]

Test[
	"a \[VectorLess] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-B8N1U9"
]

Test[
	"a \[VectorGreater] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-F3B1H9"
]

Test[
	"a \[VectorLessEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-E1Q8K1"
]

Test[
	"a \[VectorGreaterEqual] b < c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Inequality-20220919-P2D0P9"
]



















