(* Wolfram Language Test file *)

Needs["CodeParser`"]


Test[
	CodeParse["f["]
	,
	ContainerNode[String, {
		CallMissingCloserNode[
			LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>], {}, <|Source -> {{1, 1}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]
	,
	TestID->"CallMissingCloserNodes-20190701-H7G3R7"
]

Test[
	CodeParse["f[1"]
	,
	ContainerNode[String, {
		CallMissingCloserNode[
			LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>], {
				LeafNode[Integer, "1", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-B7K4Z8"
]

Test[
	CodeParse["f::["]
	,
	ContainerNode[String, {CallMissingCloserNode[CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>], {}, <|Source -> {{1, 1}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-W2D0J4"
]

Test[
	CodeParse["f::[1"]
	,
	ContainerNode[String, {CallMissingCloserNode[CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>]}, <||>], {LeafNode[Integer, "1", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-L2H2N5"
]

Test[
	CodeParse["f\\[LeftDoubleBracket]"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "Part", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 22}}|>]}, <|Source -> {{1, 1}, {1, 22}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-C2J4K7"
]

Test[
	CodeParse["f\\[LeftDoubleBracket]1"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "Part", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>], LeafNode[Integer, "1", <|Source -> {{1, 22}, {1, 23}}|>]}, <|Source -> {{1, 1}, {1, 23}}|>]}, <|Source -> {{1, 1}, {1, 23}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-J0V0J3"
]

Test[
	CodeParse["( f[ )"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 4}}|>], {}, <|Source -> {{1, 3}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-L0B1H1"
]

Test[
	CodeParse["( f[1 )"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 4}}|>], {LeafNode[Integer, "1", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 3}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-J2C1D4"
]

Test[
	CodeParse["( f::[ )"]
	,
	ContainerNode[String, {CallMissingCloserNode[CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 4}}|>]}, <||>], {}, <|Source -> {{1, 3}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-S2K7R7"
]

Test[
	CodeParse["( f::[1 )"]
	,
	ContainerNode[String, {CallMissingCloserNode[CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 4}}|>]}, <||>], {LeafNode[Integer, "1", <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 3}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-K9J6S4"
]

Test[
	CodeParse["( f\\[LeftDoubleBracket] )"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "Part", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 3}, {1, 24}}|>]}, <|Source -> {{1, 1}, {1, 26}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-D4Q8T3"
]

Test[
	CodeParse["( f\\[LeftDoubleBracket]1 )"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "Part", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 4}}|>], LeafNode[Integer, "1", <|Source -> {{1, 24}, {1, 25}}|>]}, <|Source -> {{1, 3}, {1, 25}}|>]}, <|Source -> {{1, 1}, {1, 27}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-V9R1M3"
]

Test[
	CodeParse["( [ )"]
	,
	ContainerNode[String, {GroupMissingCloserNode[GroupSquare, {}, <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-B5I2K2"
]

Test[
	CodeParse["( ::[ )"]
	,
	ContainerNode[String, {GroupMissingCloserNode[GroupTypeSpecifier, {}, <|Source -> {{1, 3}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-X9W0H8"
]

Test[
	CodeParse["( \\[LeftDoubleBracket] )"]
	,
	ContainerNode[String, {GroupMissingCloserNode[GroupDoubleBracket, {}, <|Source -> {{1, 3}, {1, 23}}|>]}, <|Source -> {{1, 1}, {1, 25}}|>]
	,
	TestID->"CallMissingCloserNodes-20220917-G3Y7X9"
]

Test[
	CodeParse["(a[b[])"]
	,
	ContainerNode[String, {
		CallMissingCloserNode[LeafNode[Symbol, "a", <|Source -> {{1, 2}, {1, 3}}|>], {
			CallNode[LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 5}}|>], {}, <|Source -> {{1, 4}, {1, 7}}|>]}, <|Source -> {{1, 2}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"CallMissingCloserNodes-20190803-C7O2S5"
]

Test[
	CodeParse["List[a"]
	,
	ContainerNode[String, {CallMissingCloserNode[LeafNode[Symbol, "List", <|Source -> {{1, 1}, {1, 5}}|>], {LeafNode[Symbol, "a", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"CallMissingCloserNodes-20200708-Y2V4V2"

]






