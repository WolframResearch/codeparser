
Needs["AST`"]

Test[
	ConcreteParseBox[RowBox[{"<<", "ExampleData`FunctionWithAssert`"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Get, {
			LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
			LeafNode[String, "ExampleData`FunctionWithAssert`", <|Source -> {1, 2}|>]}, <|Source -> {1}|>]}, <||>]
	,
	TestID->"Boxes-20190918-D2P4H5"
]

Test[
	ConcreteParseBox[RowBox[{"a", " ", "b"}]]
	,
	ContainerNode[Box, {
		InfixNode[Times, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {1, 3}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <|Source -> {1}|>]}, <||>]
	,
	TestID->"Boxes-20191015-Q5H2Y6"
]





box = TagBox["a", Function[BoxForm`e$, BoxForm`e$]]

cst = ConcreteParseBox[box]

Test[
	cst
	,
	With[{evaledData = <||>},
		ContainerNode[Box, {
			BoxNode[TagBox, {
				LeafNode[Symbol, "a", <|Source -> {1}|>],
				CodeNode[Null, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20191119-A5T4V2"
]

agg = AST`Abstract`Aggregate[cst]

Test[
	agg
	,
	With[{evaledData = <||>},
		ContainerNode[Box, {
			BoxNode[TagBox, {
				LeafNode[Symbol, "a", <|Source -> {1}|>], 
	  			CodeNode[Null, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20191119-F6L1J2"
]

(*
Test[
	ToInputFormString[agg]
	,
	"\\!\\(\\*TagBox[a, Function[BoxForm`e$, BoxForm`e$]]\\)"
	,
	TestID->"Boxes-20191119-R1Q6C7"
]
*)

ast = AST`Abstract`Abstract[agg]

Test[
	ast
	,
	With[{evaledData = <||>},
		ContainerNode[Box, {
			BoxNode[TagBox, {
				LeafNode[Symbol, "a", <|Source -> {1}|>],
				CodeNode[Null, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20191119-R5E0G4"
]

TestMatch[
	ToFullFormString[ast]
	,
	Failure["CannotConvertBoxesToFullForm", _]
	,
	TestID->"Boxes-20191119-Z6J3D5"
]
 

box = RowBox[{"<<", "ExampleData`FunctionWithAssert`", " "}]
 
Test[
	ConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixNode[Get, {
			LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
			LeafNode[String, "ExampleData`FunctionWithAssert`", <|Source -> {1, 2}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 3}|>]}, <|Source -> {1}|>]}, <||>]
	,
	TestID->"Boxes-20191230-I9W2G5"
]




box = RowBox[{"a", "::", "b", "::", "c", "::", "d"}]
 
Test[
	ConcreteParseBox[box]
	,
	ContainerNode[Box, {
		InfixNode[MessageName, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 2}|>],
			LeafNode[String, "b", <|Source -> {1, 3}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 4}|>],
			LeafNode[String, "c", <|Source -> {1, 5}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 6}|>],
			LeafNode[String, "d", <|Source -> {1, 7}|>]}, <|Source -> {1}|>]}, <||>]
	,
	TestID->"Boxes-20191230-K0E3D0"
]






 
 
 
 
 
 
 
 