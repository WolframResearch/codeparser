Needs["CodeParser`"]
Needs["CodeParser`Folds`"] (* For aggregate *)

Test[Context[aggregate], "CodeParser`Folds`"]

Test[
	aggregate @ ContainerNode[String, {
		InfixNode[Plus, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>],
			LeafNode[Token`Plus, "+", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Whitespace, " ", <|Source -> {{1, 4}, {1, 5}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 6}}|>]
		}, <|Source -> {{1, 1}, {1, 6}}|>]
	}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	ContainerNode[String, {
		InfixNode[Plus, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Plus, "+", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 6}}|>]
		}, <|Source -> {{1, 1}, {1, 6}}|>]
	}, <|Source -> {{1, 1}, {1, 6}}|>]
]

(* Test aggregate[..] of a non-ContainerNode *)
Test[
	aggregate @ InfixNode[Plus, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>],
		LeafNode[Token`Plus, "+", <|Source -> {{1, 3}, {1, 4}}|>],
		LeafNode[Whitespace, " ", <|Source -> {{1, 4}, {1, 5}}|>],
		LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 6}}|>]
	}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	InfixNode[Plus, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`Plus, "+", <|Source -> {{1, 3}, {1, 4}}|>],
		LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 6}}|>]
	}, <|Source -> {{1, 1}, {1, 6}}|>]
]

(*----------------------------------------*)
(* Test aggregate[..] of individual tokes *)
(*----------------------------------------*)

With[{
	symbolTok = LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>]
},
	Test[aggregate[symbolTok], symbolTok]
]

Test[
	aggregate @ LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>],
	Nothing
]
