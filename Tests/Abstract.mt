
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]


agg =
	ContainerNode[Box, {
		TernaryNode[TernaryOptionalPattern, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Colon, ":", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>],
			LeafNode[Token`Colon, ":", <|Source -> {1, 4}|>],
			LeafNode[Symbol, "c", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]


(*
bug 409304
*)
Test[
	CodeParser`Abstract`Abstract[agg]
	,
	ContainerNode[Box, {
		CallNode[
			LeafNode[Symbol, "Optional", <||>], {
			CallNode[LeafNode[Symbol, "Pattern", <||>], {
				LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
				LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <||>],
			LeafNode[Symbol, "c", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Abstract-20210430-D1U9S1"
]






(*
Bug 409472
*)
cst = CodeConcreteParseBox[{RowBox[{"Begin", "[", "\"\<FindMinimumTrek`\>\"", "]"}], "\n", RowBox[{"End", "[", "]"}]}]

agg = CodeParser`Abstract`Aggregate[cst]

Test[
	CodeParser`Abstract`Abstract[agg]
	,
	ContainerNode[Box, {
		ContextNode[{LeafNode[String, "\"FindMinimumTrek`\"", <|Source -> {1, 1, 3}|>]}, {}, <|Source -> {1} ;; {3}|>]}, <||>]
	,
	TestID->"Abstract-20210504-G2K4C0"
]








(*
bug 414131
*)

cst = 
  CodeConcreteParseBox[
   RowBox[{"Timing", "@", 
     RowBox[{"Do", "[", 
       RowBox[{RowBox[{RowBox[{"AnnotationValue", "[", 
             RowBox[{RowBox[{"{", RowBox[{"g", ",", "v"}], "}"}], ",",
                "VertexWeight"}], "]"}], "=."}], ",", 
         RowBox[{"{", 
           RowBox[{"v", ",", RowBox[{"VertexList", "[", "g", "]"}]}], 
           "}"}]}], "]"}]}]];

agg = CodeParser`Abstract`Aggregate[cst];

ast = CodeParser`Abstract`Abstract[agg];

Test[
	ScopingData[ast]
	,
	{scopingDataObject[{1, 3, 1, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 2, 1, 3}, {"Do"}, {}, "v"], scopingDataObject[{1, 3, 1, 3, 1, 3, 1, 2, 1, 1}, {"Do"}, {}, "v"]}
	,
	TestID->"Abstract-20210908-W3G8I3"
]


(*
bug 414131
*)

cst = CodeConcreteParseBox[RowBox[{"a", "=."}]];

agg = CodeParser`Abstract`Aggregate[cst];

TestMatch[
	CodeParser`Abstract`Abstract[agg]
	,
	ContainerNode[Box, {
		CallNode[LeafNode[Symbol, "Unset", _], {
			LeafNode[Symbol, "a", _]}, _]}, _]
	,
	TestID->"Abstract-20210908-D4I9W4"
]


