
Needs["CodeParser`"]

(*
Test the listability of functions
*)

Test[
	CodeConcreteParse["1"]
	,
	ContainerNode[String, {LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]
	,
	TestID->"CodeParser-20200311-Y8A1B4"
]


Test[
	CodeConcreteParse[{"1", "2", "3"}]
	,
	{
		ContainerNode[String, {LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>],
		ContainerNode[String, {LeafNode[Integer, "2", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>],
		ContainerNode[String, {LeafNode[Integer, "3", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]}
	,
	TestID->"CodeParser-20200311-Y0F4F3"
]


Test[
	CodeParse["1"]
	,
	ContainerNode[String, {LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]
	,
	TestID->"CodeParser-20200311-I4F2P8"
]


Test[
	CodeParse[{"1", "2", "3"}]
	,
	{
		ContainerNode[String, {LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>],
		ContainerNode[String, {LeafNode[Integer, "2", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>],
		ContainerNode[String, {LeafNode[Integer, "3", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]}
	,
	TestID->"CodeParser-20200311-G2W9M2"
]


Test[
	CodeTokenize["1"]
	,
	{LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]}
	,
	TestID->"CodeParser-20200311-G9U8X5"
]


Test[
	CodeTokenize[{"1", "2", "3"}]
	,
	{
		{LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]},
		{LeafNode[Integer, "2", <|Source -> {{1, 1}, {1, 2}}|>]},
		{LeafNode[Integer, "3", <|Source -> {{1, 1}, {1, 2}}|>]}}
	,
	TestID->"CodeParser-20200311-B7G5P8"
]

