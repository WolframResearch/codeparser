
Needs["AST`"]

(*
Package:
*)

TestMatch[
	FirstCase[ParseString["BeginPackage[\"Foo`\"]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-P2N0D7"
]

TestMatch[
	FirstCase[ParseString["EndPackage[]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-M6K6Y5"
]

TestMatch[
	FirstCase[ParseString["Begin[\"Foo`\"]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-F7B2Y5"
]

TestMatch[
	FirstCase[ParseString["End[]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-T0U9L8"
]


TestMatch[
	FirstCase[ParseString["BeginStaticAnalysisIgnore[]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-G8U7Y8"
]

TestMatch[
	FirstCase[ParseString["EndStaticAnalysisIgnore[]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-A9T9G9"
]






(*
StrangeCall:
*)

TestMatch[
	FirstCase[ParseString["a;b;[]", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-T2E6L3"
]

TestMatch[
	FirstCase[ParseString["a;b;[];c", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-Z2Y3I1"
]

TestMatch[
	FirstCase[ParseString[" a;b;\[LeftDoubleBracket]\[RightDoubleBracket] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-P6P7U2"
]

TestMatch[
	FirstCase[ParseString[" a;b;\[LeftDoubleBracket]\[RightDoubleBracket];c ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-S6O1O1"
]

TestMatch[
	FirstCase[ParseString[" %[] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-X5H0W9"
]

TestMatch[
	FirstCase[ParseString[" \\!\\(x\\)[] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-V9T6S1"
]

TestMatch[
	FirstCase[ParseString[" \\(x\\)[] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-I7T4W0"
]


TestMatch[
	FirstCase[ParseString[" x--[] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-I3X6I7"
]







(*
SyntaxUndocumentedMessageName:
*)

TestMatch[
	FirstCase[ParseString[" a::b::c::d ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["SyntaxUndocumentedMessageName", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-F4W6X1"
]








(*
Comma:
*)


TestMatch[
	FirstCase[ParseString[" f[1,2,] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Comma", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-V4Q1U5"
]

TestMatch[
	ParseString[" f[,1] "]
	,
	StringNode[String, {
		CallNode[LeafNode[Symbol, "f", _], {
			LeafNode[Symbol, "Null", KeyValuePattern[AbstractSyntaxIssues -> _]],
			LeafNode[Integer, "1", _]}, _] }, _]
	,
	TestID->"AbstractSyntaxIssues-20190520-V9J1I3"
]








(*
StrangeCallSlotSequence:
*)

TestMatch[
	FirstCase[ParseString[" ##2[] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCallSlotSequence", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-O7G6C1"
]





(*
NotContiguous:
*)


TestMatch[
	FirstCase[ParseString[" a[[] ] ", HoldNode[Hold, #[[1]], <||>] &],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {FormatIssue["NotContiguous", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-U1R2G5"
]







