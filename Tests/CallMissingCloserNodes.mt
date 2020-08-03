(* Wolfram Language Test file *)

Needs["CodeParser`"]


Test[
	CodeParse["f["]
	,
	ContainerNode[String, {
		UnterminatedCallNode[
			LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>], {
				LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 2}, {1, 3}}|>]
				}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"CallMissingCloserNodes-20190701-H7G3R7"
]

Test[
	CodeParse["(a[b[])"]
	,
	ContainerNode[String, {
		CallMissingCloserNode[LeafNode[Symbol, "a", <|Source -> {{1, 2}, {1, 3}}|>], {
			CallNode[LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 5}}|>], {}, <|Source -> {{1, 4}, {1, 7}}|>]}, <|Source -> {{1, 2}, {1, 7}}|>]}, <||>]
	,
	TestID->"CallMissingCloserNodes-20190803-C7O2S5"
]

Test[
	CodeParse["List[a"]
	,
	ContainerNode[String, {
		UnterminatedCallNode[LeafNode[Symbol, "List", <|Source -> {{1, 1}, {1, 5}}|>], {
			LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 5}, {1, 6}}|>],
			LeafNode[Symbol, "a", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <||>]
	,
	TestID->"CallMissingCloserNodes-20200708-Y2V4V2"

]






