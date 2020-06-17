
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
	  		LeafNode[Token`Tilde, "~", <|Source -> {{1, 3}, {1, 4}}|>], 
	  		LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>] }, <||>]
	,
	TestID->"SyntaxErrorNodes-20190521-T2R4L9"
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
			LeafNode[Token`SlashColon, "/:", <|Source -> {{1, 3}, {1, 5}}|>],
			InfixNode[Times, {
				LeafNode[Symbol, "b", <|Source -> {{1, 6}, {1, 7}}|>], 
		    	LeafNode[Token`Star, "*", <|Source -> {{1, 8}, {1, 9}}|>], 
		    	LeafNode[Symbol, "c", <|Source -> {{1, 10}, {1, 11}}|>]}, <|Source -> {{1, 6}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>] }, <||>]
	,
	TestID->"SyntaxErrorNodes-20190521-D9G5L2"
]

