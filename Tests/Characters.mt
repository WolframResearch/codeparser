
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "ASTTestUtils"}]
PrependTo[$Path, path]

Needs["ASTTestUtils`"]


Needs["AST`"]
Needs["AST`Utils`"]



(*
guarantee that "\:f3a2" does not get returned as StringNode[String, "\[COMPATIBILITYNoBreak]", <||>]
*)

TestMatch[
	ConcreteParseString["\"\\:f3a2\""]
	,
	ContainerNode[String, {
		LeafNode[String, "\"\\:f3a2\"", <|Source -> {{1, 1}, {1, 9}}|>] }, _]
	,
	TestID->"Characters-20190601-E6Q0I8"
]


(*
\r and \[RawReturn]
*)
Test[
	"\"\\r\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20181115-M4K2F9"
]

Test[
	"\"\\[RawReturn]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20181115-A3F2Z1"
]

Test[
	"\"\\:000d\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190126-A6E4K4"
]



(*
\[RawDoubleQuote]
*)
Test[
	"\"\\[RawDoubleQuote]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190126-S9D1H2"
]

Test[
	"\"\\:0022\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190126-O0I4X0"
]



(*
\[RawBackslash]
*)
Test[
	"\"\\[RawBackslash]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190126-T0Y0O1"
]

Test[
	"\"\\:005c\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190126-F7Z5P8"
]





Test[
	"\"\\.00\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190128-I9O3D9"
]


Test[
	"\"\\|010023\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Characters-20190129-O8S8M2"
]






