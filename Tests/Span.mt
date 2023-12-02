Print["\n===== Start Span.mt =====\n"]

path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*

Implicit times and span

*)

Test[
	";; ;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20181231-N2J2Z3"
]

Test[
	";;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190109-B0V5P7"
]

Test[
	"a;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-E5E6B1"
]

Test[
	"a;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-Y6L0F5"
]

Test[
	";;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-M8I9Z2"
]

Test[
	"a;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-J5A3O4"
]

Test[
	";;a;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190109-M8Y0B6"
]

Test[
	";;;;a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-L0I6I3"
]

Test[
	";;a;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-U3F5F7"
]

Test[
	"a;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-C7J3X7"
]

Test[
	"a;;b;;c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-B6M3X4"
]

Test[
	";;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-T0Y0L1"
]

Test[
	";;b;;c;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-V7M9T8"
]

Test[
	"a;;b;;c;;d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-G4T7T1"
]

Test[
	"a;;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-L4W4G9"
]

Test[
	";;a;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-Y5S8V9"
]

Test[
	";;;;a;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-T8R1B0"
]

Test[
	";;;;;;a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-S0D8M5"
]

Test[
	";;;;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-W9H4P5"
]

Test[
	"a;;;;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-T4T2R7"
]

Test[
	";;a;;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-Y9A7L2"
]

Test[
	";;;;a;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-K8Q1S1"
]

Test[
	";;;;;;a;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-N7X0T0"
]

Test[
	";;;;;;;;a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-T3H6N4"
]

Test[
	"a;;;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-B1F7N5"
]

Test[
	";;a;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-H7G3T9"
]

Test[
	";;;;a;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-S1L1E1"
]

Test[
	";;;;;;a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-D8U0E6"
]

Test[
	";;;;;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-V4H4O6"
]

Test[
	"a;;;;;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-T8T4G5"
]

Test[
	";;a;;;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-G6W0G8"
]

Test[
	";;;;a;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-I5M5M6"
]

Test[
	";;;;;;a;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-B2S1L0"
]

Test[
	";;;;;;;;a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-C1M5E0"
]

Test[
	";;;;;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-J7I0N0"
]

Test[
	"a;;c;;;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-V5R5K9"
]

Test[
	";;a;;c;;;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-A1K3Y2"
]

Test[
	";;;;a;;c;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-H1W2W2"
]

Test[
	"c;;;;;;a;;b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-C1K3V9"
]

Test[
	"{ ;;\n;; }"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-S9O5S7"
]

Test[
	"{ ;;\n;;a }"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-E2I3R8"
]

Test[
	";;;;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-C1V9W3"
]

Test[
	"a;;b;;c;;d;;e"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-E0A2L9"
]

Test[
	"a;;b;;c;;d;;e;;f"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-Y0F1O2"
]

Test[
	"a;;b;;c;;d;;e;;f;;g"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-D8G2B8"
]

Test[
	"a;;b;;c;;d;;e;;f;;g;;h"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-V3Y3U4"
]

Test[
	"a;;;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-K1V8P0"
]

Test[
	"b;;a;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190620-T6E4Z3"
]

Test[
	";;a;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20190621-I4J4Y6"
]

Test[
	CodeParse["a;;!"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Span", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Not", <||>], {
				ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"Span-20191016-M3L9D1"
]





(*
verify that nested ImplicitTimes are not created
*)
TestMatch[
	CodeConcreteParse[";; ;; ;;"]
	,
	ContainerNode[String, {
		InfixNode[Times, {
			BinaryNode[Span, _, _],
			LeafNode[Token`Fake`ImplicitTimes, _, _],
			BinaryNode[Span, _, _],
			LeafNode[Token`Fake`ImplicitTimes, _, _],
			BinaryNode[Span, _, _] }, _]}, _]
	,
	TestID->"Span-20190622-C2T8X7"
]





Test[
	CodeConcreteParse["a ;; &"]
	,
	ContainerNode[String, {
		PostfixNode[Function, {
			BinaryNode[Span, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>],
				LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 3}, {1, 5}}|>], 
				LeafNode[Whitespace, " ", <|Source -> {{1, 5}, {1, 6}}|>],
				LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>],
			LeafNode[Token`Amp, "&", <|Source -> {{1, 6}, {1, 7}}|>] }, <|Source -> {{1, 1}, {1, 7}}|>] }, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Span-20191016-J5K9B8"
]

Test[
	CodeConcreteParse["a ;; \\t", ContainerNode -> f]
	,
	f[{{BinaryNode[Span, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>],
			LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 3}, {1, 5}}|>],
			LeafNode[Whitespace, " ", <|Source -> {{1, 5}, {1, 6}}|>],
			LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>],
		ErrorNode[Token`Error`UnhandledCharacter, "\\t", <|Source -> {{1, 6}, {1, 8}}|>]},
		{}, {}, {}, {}, {}}]
	,
	TestID->"Span-20191016-T6M0R1"
]


Test[
	"a ;; b ;; c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Span-20220616-S1F8C3"
]
