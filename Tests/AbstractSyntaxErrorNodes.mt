
Needs["AST`"]


(*
OpenSquare:
*)

TestMatch[
	ParseString[" [x] "]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-U4J1C1"
]



(*
OpenParen:
*)

TestMatch[
	ParseString[" (1,2,3) "]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-E0X9G7"
]



(*
GroupMissingCloser:
*)

TestMatch[
	ParseString["{"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-M0B3Z5"
]


TestMatch[
	ParseString["<|"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-U0L5P6"
]

TestMatch[
	ParseString["\[LeftAngleBracket]"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-X7G1G5"
]


TestMatch[
	ParseString["\[LeftCeiling]"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-Q4A4B9"
]


TestMatch[
	ParseString["\[LeftFloor]"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-C4T4D9"
]

TestMatch[
	ParseString["\[LeftDoubleBracket]"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-S1C3U4"
]

TestMatch[
	ParseString["\[LeftBracketingBar]"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-H0B3W9"
]

TestMatch[
	ParseString["\[LeftDoubleBracketingBar]"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-R4A5I7"
]

TestMatch[
	ParseString["("]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-K6C7J1"
]

TestMatch[
	ParseString["["]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-Y0H1P1"
]


TestMatch[
	ParseString["\\("]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`GroupMissingCloser, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-B2V0A0"
]








(*
LinearSyntaxBang:
*)


TestMatch[
	ParseString["\\!123"]
	,
	AbstractSyntaxErrorNode[AbstractSyntaxError`LinearSyntaxBang, _, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-N8K8K4"
]





