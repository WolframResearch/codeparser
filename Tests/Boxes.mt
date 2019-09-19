
Needs["AST`"]

Test[
	ConcreteParseBox[RowBox[{"<<", "ExampleData`FunctionWithAssert`"}]]
	,
	PrefixNode[Get, {
		LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
		LeafNode[Token`Other, "ExampleData`FunctionWithAssert`", <|Source -> {1, 2}|>]}, <|Source -> {1}|>]
	,
	TestID->"Boxes-20190918-D2P4H5"
]




