
Needs["CodeParser`"]


Test[
	Internal`InheritedBlock[{CodeParser`Quirks`$Quirks},
		
		CodeParser`Quirks`$Quirks["OldAtAtAt"] = True;
		
		CodeParse["a @@@ b"]
	]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Apply", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 8}}|>],
			CallNode[LeafNode[Symbol, "List", <||>], {LeafNode[Integer, "1", <||>]}, <||>]}, <|Source -> {{1, 1}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"Quirks-20220919-O2S9R6"
]