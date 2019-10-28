
Needs["AST`"]

Test[
	ConcreteParseBox[RowBox[{"<<", "ExampleData`FunctionWithAssert`"}]]
	,
	PrefixNode[Get, {
		LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
		LeafNode[String, "ExampleData`FunctionWithAssert`", <|Source -> {1, 2}|>]}, <|Source -> {1}|>]
	,
	TestID->"Boxes-20190918-D2P4H5"
]

Test[
	ConcreteParseBox[RowBox[{"a", " ", "b"}]]
	,
	InfixNode[Times, {
		LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {1, 2}|>],
		LeafNode[Token`Fake`ImplicitTimes, "", <||>],
		LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <|Source -> {1}|>]
	,
	TestID->"Boxes-20191015-Q5H2Y6"
]
