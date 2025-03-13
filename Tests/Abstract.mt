
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
cst = CodeConcreteParseBox[{
	RowBox[{"Begin", "[", "\"\<FindMinimumTrek`\>\"", "]"}],
	"\n",
	RowBox[{"End", "[", "]"}]
}]

Test[
	cst,
	ContainerNode[Box, {
		CallNode[
			{
				LeafNode[Symbol, "Begin", <| Source -> {1, 1, 1} |>]
			},
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <| Source -> {1, 1, 2} |>],
				LeafNode[String, "\"FindMinimumTrek`\"", <| Source -> {1, 1, 3}|>],
				LeafNode[Token`CloseSquare, "]", <| Source -> {1, 1, 4} |>]
			}, <||>],
			<|Source -> {1} |>
		],
		LeafNode[Token`Newline, "\n", <| Source -> {2} |>],
		CallNode[
			{LeafNode[Symbol, "End", <| Source -> {3, 1, 1} |>]},
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <| Source -> {3, 1, 2} |>],
				LeafNode[Token`CloseSquare,	"]", <| Source -> {3, 1, 3} |>]
			}, <||>],
			<| Source -> {3} |>
		]
	}, <||>]
]

agg = CodeParser`Abstract`Aggregate[cst]

Test[
	agg,
	ContainerNode[Box, {
		CallNode[
			LeafNode[Symbol, "Begin", <| Source -> {1, 1, 1} |>],
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <| Source -> {1, 1, 2} |>],
				LeafNode[String, "\"FindMinimumTrek`\"", <| Source -> {1, 1, 3}|>],
				LeafNode[Token`CloseSquare, "]", <| Source -> {1, 1, 4} |>]
			}, <||>],
			<|Source -> {1} |>
		],
		CallNode[
			LeafNode[Symbol, "End", <| Source -> {3, 1, 1} |>],
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <| Source -> {3, 1, 2} |>],
				LeafNode[Token`CloseSquare,	"]", <| Source -> {3, 1, 3} |>]
			}, <||>],
			<| Source -> {3} |>
		]
	}, <||>]
]

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









(*
bug 414139
*)

cst = CodeConcreteParseBox[RowBox[{RowBox[{"Function", "[", RowBox[{"x", ",", RowBox[{"x", "^", "2"}]}], "]"}], "'"}]]

agg = CodeParser`Abstract`Aggregate[cst]

TestMatch[
	CodeParser`Abstract`Abstract[agg]
	,
	ContainerNode[Box, {
		CallNode[CallNode[LeafNode[Symbol, "Derivative", _], {LeafNode[Integer, "1", _]}, _], {
			CallNode[LeafNode[Symbol, "Function", _], {
				LeafNode[Symbol, "x", _],
				CallNode[LeafNode[Symbol, "Power", _], {
					LeafNode[Symbol, "x", _],
					LeafNode[Integer, "2", _]}, _]}, _]}, _]}, _]
	,
	TestID->"Abstract-20210908-F9H3D4"
]




Test[
	CodeParse["___f"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "BlankNullSequence", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"Abstract-20220917-E3Y3D8"
]

Test[
	CodeParse["a__f"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "Pattern", <||>], {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], CallNode[LeafNode[Symbol, "BlankSequence", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 2}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"Abstract-20220917-B2E7W6"
]

Test[
	CodeParse["a___f"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "Pattern", <||>], {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], CallNode[LeafNode[Symbol, "BlankNullSequence", <||>], {LeafNode[Symbol, "f", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 2}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"Abstract-20220917-Z5G2R2"
]


Test[
	CodeParse["a @@@ b"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "MapApply", <||>], {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"Abstract-20220917-Y0I3N5"
]


Test[
	CodeParse["a\\[LeftDoubleBracket]b, c\\[RightDoubleBracket]"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "Part", <||>], {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], LeafNode[Symbol, "b", <|Source -> {{1, 22}, {1, 23}}|>], LeafNode[Symbol, "c", <|Source -> {{1, 25}, {1, 26}}|>]}, <|Source -> {{1, 1}, {1, 47}}|>]}, <|Source -> {{1, 1}, {1, 47}}|>]
	,
	TestID->"Abstract-20220917-Z0U5W8"
]




Test[
	CodeParse["##2[[a]]"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "Part", <||>], {CallNode[LeafNode[Symbol, "SlotSequence", <||>], {LeafNode[Integer, "2", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>], LeafNode[Symbol, "a", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 9}}, AbstractSyntaxIssues -> {SyntaxIssue["StrangeCallSlotSequence", "Unexpected ``Part`` call.", "Error", <|Source -> {{1, 4}, {1, 5}}, ConfidenceLevel -> 1., "AdditionalSources" -> {{{1, 8}, {1, 9}}}|>]}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Abstract-20220917-L9Y5R2"
]

Test[
	CodeParse["\(x\)[[a]]"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "Part", <||>], {LeafNode[Token`LinearSyntaxBlob, "\(x\)", <|Source -> {{1, 1}, {1, 4}}|>], LeafNode[Symbol, "a", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 9}}, AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|Source -> {{1, 4}, {1, 5}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 8}, {1, 9}}}|>]}|>]}, <|SyntaxIssues -> {EncodingIssue["NonASCIICharacter", "Non-ASCII character: ``\"\(\" (\\()``.", "Remark", <|Source -> {{1, 1}, {1, 2}}, ConfidenceLevel -> 1., CodeActions -> {CodeAction["Replace with ``\\(``", ReplaceText, <|Source -> {{1, 1}, {1, 2}}, "ReplacementText" -> "\\("|>]}|>], EncodingIssue["NonASCIICharacter", "Non-ASCII character: ``\"\)\" (\\))``.", "Remark", <|Source -> {{1, 3}, {1, 4}}, ConfidenceLevel -> 1., CodeActions -> {CodeAction["Replace with ``\\)``", ReplaceText, <|Source -> {{1, 3}, {1, 4}}, "ReplacementText" -> "\\)"|>]}|>]}, Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Abstract-20220917-C5B5H8"
]

Test[
	CodeParse["\!\(x\)[[a]]"]
	,
	ContainerNode[String, {CallNode[LeafNode[Symbol, "Part", <||>], {PrefixNode[PrefixLinearSyntaxBang, {LeafNode[Token`LinearSyntax`Bang, "\!", <|Source -> {{1, 1}, {1, 2}}|>], LeafNode[Token`LinearSyntaxBlob, "\(x\)", <|Source -> {{1, 2}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>], LeafNode[Symbol, "a", <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 10}}, AbstractSyntaxIssues -> {SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Remark", <|Source -> {{1, 5}, {1, 6}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 9}, {1, 10}}}|>]}|>]}, <|SyntaxIssues -> {EncodingIssue["NonASCIICharacter", "Non-ASCII character: ``\"\!\" (\\!)``.", "Remark", <|Source -> {{1, 1}, {1, 2}}, ConfidenceLevel -> 1., CodeActions -> {CodeAction["Replace with ``\\!``", ReplaceText, <|Source -> {{1, 1}, {1, 2}}, "ReplacementText" -> "\\!"|>]}|>], EncodingIssue["NonASCIICharacter", "Non-ASCII character: ``\"\(\" (\\()``.", "Remark", <|Source -> {{1, 2}, {1, 3}}, ConfidenceLevel -> 1., CodeActions -> {CodeAction["Replace with ``\\(``", ReplaceText, <|Source -> {{1, 2}, {1, 3}}, "ReplacementText" -> "\\("|>]}|>], EncodingIssue["NonASCIICharacter", "Non-ASCII character: ``\"\)\" (\\))``.", "Remark", <|Source -> {{1, 4}, {1, 5}}, ConfidenceLevel -> 1., CodeActions -> {CodeAction["Replace with ``\\)``", ReplaceText, <|Source -> {{1, 4}, {1, 5}}, "ReplacementText" -> "\\)"|>]}|>]}, Source -> {{1, 1}, {1, 10}}|>]
	,
	TestID->"Abstract-20220917-H5X2C9"
]

Test[
	CodeParse["[[a]]"]
	,
	ContainerNode[String, {AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, {
		AbstractSyntaxErrorNode[AbstractSyntaxError`OpenSquare, {
			LeafNode[Symbol, "a", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 2}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"Abstract-20220917-J7P6H3"
]









