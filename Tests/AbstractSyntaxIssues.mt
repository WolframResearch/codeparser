
Needs["CodeParser`"]

(*
Package:
*)

TestMatch[
	FirstCase[CodeParse["BeginPackage[\"Foo`\"]", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-P2N0D7"
]

TestMatch[
	FirstCase[CodeParse["EndPackage[]", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-M6K6Y5"
]

TestMatch[
	FirstCase[CodeParse["Begin[\"Foo`\"]", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-F7B2Y5"
]

TestMatch[
	FirstCase[CodeParse["End[]", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["Package", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-T0U9L8"
]








(*
StrangeCall:
*)

TestMatch[
	FirstCase[CodeParse["a;b;[]", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-T2E6L3"
]

TestMatch[
	FirstCase[CodeParse["a;b;[];c", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-Z2Y3I1"
]

TestMatch[
	FirstCase[CodeParse[" a;b;\[LeftDoubleBracket]\[RightDoubleBracket] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-P6P7U2"
]

TestMatch[
	FirstCase[CodeParse[" a;b;\[LeftDoubleBracket]\[RightDoubleBracket];c ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-S6O1O1"
]

TestMatch[
	FirstCase[CodeParse[" %[] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-X5H0W9"
]

TestMatch[
	FirstCase[CodeParse[" \\!\\(x\\)[] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-V9T6S1"
]

TestMatch[
	FirstCase[CodeParse[" \\(x\\)[] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-I7T4W0"
]


TestMatch[
	FirstCase[CodeParse[" x--[] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
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
	FirstCase[CodeParse[" a::b::c::d ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["SyntaxUndocumentedMessageName", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-F4W6X1"
]







(*
StrangeCallSlotSequence:
*)

TestMatch[
	FirstCase[CodeParse[" ##2[] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {SyntaxIssue["StrangeCallSlotSequence", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-O7G6C1"
]





(*
NotContiguous:
*)
(*
Not handled by parser any more

handled by syntax highlighting
*)
(*TestMatch[
	FirstCase[CodeParse[" a[[] ] ", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)],
		KeyValuePattern[AbstractSyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[AbstractSyntaxIssues -> {FormatIssue["NotContiguous", _, _, _]}]
	,
	TestID->"AbstractSyntaxIssues-20190520-U1R2G5"
]

*)





