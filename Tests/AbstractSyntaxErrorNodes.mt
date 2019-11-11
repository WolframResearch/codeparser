
Needs["AST`"]


(*
OpenSquare:
*)

TestMatch[
	ParseString[" [x] "]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-U4J1C1"
]



(*
OpenParen:
*)

TestMatch[
	ParseString[" (1,2,3) "]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-E0X9G7"
]



(*
GroupMissingCloser:
*)

TestMatch[
	ParseString["{"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-M0B3Z5"
]


TestMatch[
	ParseString["<|"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-U0L5P6"
]

TestMatch[
	ParseString["\[LeftAngleBracket]"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-X7G1G5"
]


TestMatch[
	ParseString["\[LeftCeiling]"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-Q4A4B9"
]


TestMatch[
	ParseString["\[LeftFloor]"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-C4T4D9"
]

TestMatch[
	ParseString["\[LeftDoubleBracket]"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-S1C3U4"
]

TestMatch[
	ParseString["\[LeftBracketingBar]"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-H0B3W9"
]

TestMatch[
	ParseString["\[LeftDoubleBracketingBar]"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-R4A5I7"
]

TestMatch[
	ParseString["("]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-K6C7J1"
]

TestMatch[
	ParseString["["]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-Y0H1P1"
]


TestMatch[
	ParseString["\\("]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-B2V0A0"
]








(*
LinearSyntaxBang:
*)


TestMatch[
	ParseString["\\!123"]
	,
	StringNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`LinearSyntaxBang, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-N8K8K4"
]





