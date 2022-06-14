
Needs["CodeParser`"]


(*
OpenSquare:
*)

TestMatch[
	CodeParse[" [x] "]
	,
	ContainerNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-U4J1C1"
]



(*
OpenParen:
*)

TestMatch[
	CodeParse[" (1,2,3) "]
	,
	ContainerNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-E0X9G7"
]



(*
GroupMissingCloser:
*)

TestMatch[
	CodeParse["{"]
	,
	ContainerNode[String, {UnterminatedGroupNode[List, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-M0B3Z5"
]


TestMatch[
	CodeParse["<|"]
	,
	ContainerNode[String, {UnterminatedGroupNode[Association, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-U0L5P6"
]

TestMatch[
	CodeParse["\[LeftAngleBracket]"]
	,
	ContainerNode[String, {UnterminatedGroupNode[AngleBracket, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-X7G1G5"
]


TestMatch[
	CodeParse["\[LeftCeiling]"]
	,
	ContainerNode[String, {UnterminatedGroupNode[Ceiling, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-Q4A4B9"
]


TestMatch[
	CodeParse["\[LeftFloor]"]
	,
	ContainerNode[String, {UnterminatedGroupNode[Floor, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-C4T4D9"
]

TestMatch[
	CodeParse["\[LeftDoubleBracket]"]
	,
	ContainerNode[String, {UnterminatedGroupNode[GroupDoubleBracket, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-S1C3U4"
]

TestMatch[
	CodeParse["\[LeftBracketingBar]"]
	,
	ContainerNode[String, {UnterminatedGroupNode[BracketingBar, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-H0B3W9"
]

TestMatch[
	CodeParse["\[LeftDoubleBracketingBar]"]
	,
	ContainerNode[String, {UnterminatedGroupNode[DoubleBracketingBar, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-R4A5I7"
]

TestMatch[
	CodeParse["("]
	,
	ContainerNode[String, {UnterminatedGroupNode[GroupParen, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-K6C7J1"
]

TestMatch[
	CodeParse["["]
	,
	ContainerNode[String, {UnterminatedGroupNode[GroupSquare, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-Y0H1P1"
]


TestMatch[
	CodeParse["\\("]
	,
	ContainerNode[String, {ErrorNode[Token`Error`UnterminatedLinearSyntaxBlob, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-B2V0A0"
]








(*
LinearSyntaxBang:
*)


TestMatch[
	CodeParse["\\!123"]
	,
	ContainerNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`LinearSyntaxBang, _, _]}, _]
	,
	TestID->"AbstractSyntaxErrorNodes-20190520-N8K8K4"
]


(*
NonAssociative:

TODO: is this a quirk?

*)

Test[
	CodeParse["a ? b ? c"]
	,
	ContainerNode[String, {
		AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociativePatternTest, {
			CallNode[LeafNode[Symbol, "PatternTest", <||>], {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>],
			LeafNode[Symbol, "c", <|Source -> {{1, 9}, {1, 10}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>] }, <|Source -> {{1, 1}, {1, 10}}|>]
	,
	TestID->"AbstractSyntaxErrorNodes-20190521-A6K4H1"
]


(*
ExpectedSymbol:
*)

Test[
	CodeParse["1:2"]
	,
	ContainerNode[String, {
		SyntaxErrorNode[SyntaxError`ExpectedSymbol, {
			LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Integer, "2", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]
	,
	TestID->"AbstractSyntaxErrorNodes-20190521-Z6D6T1"
]

