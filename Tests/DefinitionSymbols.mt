
Needs["CodeParser`Definitions`"]
Needs["CodeParser`"]


ast = CodeParse["a"]

lhs = ast[[2, 1]]

Test[
	DefinitionSymbols[lhs]
	,
	{LeafNode[Symbol, "a", <| Source -> {{1, 1}, {1, 2}} |>]}
	,
	TestID->"DefinitionSymbols-20181230-C3E7Y2"
]


ast = CodeParse["a[]"]

lhs = ast[[2, 1]]

Test[
	DefinitionSymbols[lhs]
	,
	{LeafNode[Symbol, "a", <| Source -> {{1, 1}, {1, 2}} |> ]}
	,
	TestID->"DefinitionSymbols-20181230-V6Q8O6"
]


ast = CodeParse["a /; q"]

lhs = ast[[2, 1]]

Test[
	DefinitionSymbols[lhs]
	,
	{LeafNode[Symbol, "a", <| Source -> {{1, 1}, {1, 2}} |>]}
	,
	TestID->"DefinitionSymbols-20181230-Z9C5M8"
]




ast = CodeParse["123"]

lhs = ast[[2, 1]]

Test[
	DefinitionSymbols[lhs]
	,
	{}
	,
	TestID->"DefinitionSymbols-20181230-C8D4W9"
]



ast = CodeParse["e:InitializationValue[Except[_String | _Symbol],___]"]

lhs = ast[[2, 1]]

Test[
	DefinitionSymbols[lhs]
	,
	{ LeafNode[Symbol, "InitializationValue", <| Source -> {{1, 3}, {1, 22}} |>] }
	,
	TestID->"DefinitionSymbols-20200218-I1H4I7"
]








(*
410337
*)
Test[
	CodeParse["/: a ="]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "TagSet", <||>], {
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Symbol, "a", <|Source -> {{1, 4}, {1, 5}}|>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 7}, {1, 7}}|>]}
			,
			<|Source -> {{1, 1}, {1, 7}},
			"Definitions" -> {LeafNode[Symbol, "a", <|Source -> {{1, 4}, {1, 5}}|>]}|>]}, <||>]
	,
	TestID->"DefinitionSymbols-20210520-S5U4M6"
]


Test[
	CodeParse["a /: : b ="]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "TagSet", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			AbstractSyntaxErrorNode[AbstractSyntaxError`PatternColonError, {
				ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 6}, {1, 6}}|>],
				LeafNode[Symbol, "b", <|Source -> {{1, 8}, {1, 9}}|>]}, <|Source -> {{1, 6}, {1, 9}}|>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 11}, {1, 11}}|>]}
		,
		<|Source -> {{1, 1}, {1, 11}},
		"Definitions" -> {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>]}|>]}, <||>]
	,
	TestID->"DefinitionSymbols-20210520-E0Y0T1"
] 








