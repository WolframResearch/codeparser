
Needs["AST`"]

(*
make sure that both InternalNullNodes have correct Sources (and not the same ones)
*)
Test[
	ConcreteParseString["a; ;"]
	,
	InfixNode[CompoundExpression, {SymbolNode["a", {}, <|Source -> {{1, 1}, {1, 1}}|>], 
  InternalNullNode[Null, {}, <|Source -> {{1, 2}, {1, 2}}|>], 
  InternalNullNode[Null, {}, <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]
	,
	TestID->"Concrete-20190117-C3R5P5"
]




Test[
	ConcreteParseString[""]
	,
	Null
	,
	TestID->"Concrete-20190227-H6C1K7"
]

