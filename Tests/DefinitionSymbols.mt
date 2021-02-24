
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










