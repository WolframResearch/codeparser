
Needs["CodeParser`"]

(*
ExpectedTilde:
*)

Test[
	CodeParse["a ~f"]
	,
	ContainerNode[String, {
		SyntaxErrorNode[SyntaxError`ExpectedTilde, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
	  		LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>] }, <||>]
	,
	TestID->"SyntaxErrorNodes-20190521-T2R4L9"
]

Test[
	CodeConcreteParse["~"]
	,
	ContainerNode[String, {
		BinaryNode[TernaryTilde, {
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {{1, 1}, {1, 2}}|>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]
	,
	TestID->"SyntaxErrorNodes-20200628-O0J0J1"
]



(*
ExpectedSet:
*)

Test[
	CodeParse["a /: b * c"]
	,
	ContainerNode[String, {
		SyntaxErrorNode[SyntaxError`ExpectedSet, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Times", <||>], {
				LeafNode[Symbol, "b", <|Source -> {{1, 6}, {1, 7}}|>],
		    	LeafNode[Symbol, "c", <|Source -> {{1, 10}, {1, 11}}|>]}, <|Source -> {{1, 6}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>] }, <||>]
	,
	TestID->"SyntaxErrorNodes-20190521-D9G5L2"
]

