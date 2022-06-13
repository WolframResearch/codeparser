
Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]


Test[
    CodeParse["\"PackedArray\"::[]"]
    ,
    ContainerNode[String, {
    	CallNode[
    		CallNode[
    			LeafNode[Symbol, "TypeSpecifier", <||>], {
    			LeafNode[String, "\"PackedArray\"", <|Source -> {{1, 1}, {1, 14}}|>]}, <||>], {}, <|Source -> {{1, 1}, {1, 18}}|>]}, <|Source -> {{1, 1}, {1, 18}}|>]
	,
	TestID->"TypeSpecifier-20220421-V2D3X7"
]

Test[
    CodeParse["\"PackedArray\"::[\"Real64\"]"]
    ,
    ContainerNode[String, {
    	CallNode[
    		CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[String, "\"PackedArray\"", <|Source -> {{1, 1}, {1, 14}}|>]}, <||>], {
    		LeafNode[String, "\"Real64\"", <|Source -> {{1, 17}, {1, 25}}|>]}, <|Source -> {{1, 1}, {1, 26}}|>]}, <|Source -> {{1, 1}, {1, 26}}|>]
	,
	TestID->"TypeSpecifier-20220421-W2F5P2"
]


Test[
    CodeParse["\"PackedArray\"::[\"Real64\", 1]"]
    ,
    ContainerNode[String, {
    	CallNode[
    		CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[String, "\"PackedArray\"", <|Source -> {{1, 1}, {1, 14}}|>]}, <||>], {
    		LeafNode[String, "\"Real64\"", <|Source -> {{1, 17}, {1, 25}}|>],
    		LeafNode[Integer, "1", <|Source -> {{1, 27}, {1, 28}}|>]}, <|Source -> {{1, 1}, {1, 29}}|>]}, <|Source -> {{1, 1}, {1, 29}}|>]
	,
	TestID->"TypeSpecifier-20220421-P2R7E3"
]

Test[
	CodeParse["foo::[bar, baz]"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>]}, <||>], {
			LeafNode[Symbol, "bar", <|Source -> {{1, 7}, {1, 10}}|>],
			LeafNode[Symbol, "baz", <|Source -> {{1, 12}, {1, 15}}|>]}, <|Source -> {{1, 1}, {1, 16}},
			
			AbstractSyntaxIssues -> {
				SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 4}, {1, 7}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 15}, {1, 16}}}|>]}|>]}, <|Source -> {{1, 1}, {1, 16}}|>]
	,
	TestID->"TypeSpecifier-20220506-X9H6A6"
]

Test[
	CodeParse["foo::[bar, baz, dram]"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>]}, <||>], {
			LeafNode[Symbol, "bar", <|Source -> {{1, 7}, {1, 10}}|>],
			LeafNode[Symbol, "baz", <|Source -> {{1, 12}, {1, 15}}|>],
			LeafNode[Symbol, "dram", <|Source -> {{1, 17}, {1, 21}}|>]}, <|Source -> {{1, 1}, {1, 22}},
			
			AbstractSyntaxIssues -> {
				SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 4}, {1, 7}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 21}, {1, 22}}}|>]}|>]}
		,
		<|Source -> {{1, 1}, {1, 22}}|>]
	,
	TestID->"TypeSpecifier-20220506-R7I5O8"
]

Test[
	CodeParse["foo::[bar, baz]::[bar, baz]"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {
			CallNode[
				CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>]}, <||>], {
				LeafNode[Symbol, "bar", <|Source -> {{1, 7}, {1, 10}}|>],
				LeafNode[Symbol, "baz", <|Source -> {{1, 12}, {1, 15}}|>]}, <|Source -> {{1, 1}, {1, 16}},
				
				AbstractSyntaxIssues -> {
					SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 4}, {1, 7}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 15}, {1, 16}}}|>]}|>]}, <||>], {
			
			LeafNode[Symbol, "bar", <|Source -> {{1, 19}, {1, 22}}|>],
			LeafNode[Symbol, "baz", <|Source -> {{1, 24}, {1, 27}}|>]}, <|Source -> {{1, 1}, {1, 28}},
			
			AbstractSyntaxIssues -> {
				SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 16}, {1, 19}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 27}, {1, 28}}}|>]}|>]}
		,
		<|Source -> {{1, 1}, {1, 28}}|>]
	,
	TestID->"TypeSpecifier-20220506-H2T4X0"
]

Test[
	CodeParse["foo::[foo::[bar, baz], foo::[bar, baz]]"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {
				LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>]}, <||>], {
			
			CallNode[
				CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "foo", <|Source -> {{1, 7}, {1, 10}}|>]}, <||>], {
				LeafNode[Symbol, "bar", <|Source -> {{1, 13}, {1, 16}}|>],
				LeafNode[Symbol, "baz", <|Source -> {{1, 18}, {1, 21}}|>]}, <|Source -> {{1, 7}, {1, 22}},
			
				AbstractSyntaxIssues -> {
					SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 10}, {1, 13}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 21}, {1, 22}}}|>]}|>],
			
			CallNode[
				CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "foo", <|Source -> {{1, 24}, {1, 27}}|>]}, <||>], {
				LeafNode[Symbol, "bar", <|Source -> {{1, 30}, {1, 33}}|>],
				LeafNode[Symbol, "baz", <|Source -> {{1, 35}, {1, 38}}|>]}, <|Source -> {{1, 24}, {1, 39}},
			
			AbstractSyntaxIssues -> {
				SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 27}, {1, 30}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 38}, {1, 39}}}|>]}|>]}, <|Source -> {{1, 1}, {1, 40}},
				
		AbstractSyntaxIssues -> {
			SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 4}, {1, 7}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 39}, {1, 40}}}|>]}|>]}, <|Source -> {{1, 1}, {1, 40}}|>]
	,
	TestID->"TypeSpecifier-20220506-N2X6U7"
]

Test[
	CodeParse["Power::infy::[bar, baz]"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {
				CallNode[LeafNode[Symbol, "MessageName", <||>], {
					LeafNode[Symbol, "Power", <|Source -> {{1, 1}, {1, 6}}|>],
					LeafNode[String, "\"infy\"", <|Source -> {{1, 8}, {1, 12}}|>]}, <|Source -> {{1, 1}, {1, 12}}|>]}, <||>], {
				LeafNode[Symbol, "bar", <|Source -> {{1, 15}, {1, 18}}|>],
				LeafNode[Symbol, "baz", <|Source -> {{1, 20}, {1, 23}}|>]}, <|Source -> {{1, 1}, {1, 24}},
				
			AbstractSyntaxIssues -> {
				SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 12}, {1, 15}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 23}, {1, 24}}}|>]}|>]}
		,
		<|Source -> {{1, 1}, {1, 24}}|>
	]
	,
	TestID->"TypeSpecifier-20220506-Q0M5O1"
]

Test[
	CodeParse["foo::[bar, baz][1, 2, 3]"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[
				CallNode[LeafNode[Symbol, "TypeSpecifier", <||>], {LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>]}, <||>], {
				LeafNode[Symbol, "bar", <|Source -> {{1, 7}, {1, 10}}|>],
				LeafNode[Symbol, "baz", <|Source -> {{1, 12}, {1, 15}}|>]}, <|Source -> {{1, 1}, {1, 16}},
				
				AbstractSyntaxIssues -> {
					SyntaxIssue["StrangeCall", "Unexpected call.", "Error", <|Source -> {{1, 4}, {1, 7}}, ConfidenceLevel -> 0.95, "AdditionalSources" -> {{{1, 15}, {1, 16}}}|>]}|>], {
			
			LeafNode[Integer, "1", <|Source -> {{1, 17}, {1, 18}}|>],
			LeafNode[Integer, "2", <|Source -> {{1, 20}, {1, 21}}|>],
			LeafNode[Integer, "3", <|Source -> {{1, 23}, {1, 24}}|>]}, <|Source -> {{1, 1}, {1, 25}}|>]}
		,
		<|Source -> {{1, 1}, {1, 25}}|>
	]
	,
	TestID->"TypeSpecifier-20220506-E2V1M1"
]

Test[
	CodeConcreteParse["::[]"]
	,
	ContainerNode[String, {
		GroupNode[GroupTypeSpecifier, {
			LeafNode[Token`ColonColonOpenSquare, "::[", <|Source -> {{1, 1}, {1, 4}}|>],
			LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}
		,
		<|Source -> {{1, 1}, {1, 5}}|>
	]
	,
	TestID->"TypeSpecifier-20220506-Q5T6J2"
]

Test[
	CodeConcreteParse["f::[]"]
	,
	ContainerNode[String, {
		CallNode[
			{LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>]},
			GroupNode[GroupTypeSpecifier, {
				LeafNode[Token`ColonColonOpenSquare, "::[", <|Source -> {{1, 2}, {1, 5}}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 2}, {1, 6}}|>], <|Source -> {{1, 1}, {1, 6}}|>]}
		,
		<|Source -> {{1, 1}, {1, 6}}|>
	]
	,
	TestID->"TypeSpecifier-20220506-T4G6M1"
]

Test[
	CodeConcreteParseBox[RowBox[{"::[", "]"}]]
	,
	ContainerNode[Box, {
		GroupNode[GroupTypeSpecifier, {
			LeafNode[Token`ColonColonOpenSquare, "::[", <|Source -> {1, 1}|>],
			LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"TypeSpecifier-20220506-T4W0G2"
]

Test[
	CodeConcreteParseBox[RowBox[{"f", "::[", RowBox[{"1", ",", "2", ",", "3"}], "]"}]]
	,
	ContainerNode[Box, {
		CallNode[
			{LeafNode[Symbol, "f", <|Source -> {1, 1}|>]},
			GroupNode[GroupTypeSpecifier, {
				LeafNode[Token`ColonColonOpenSquare, "::[", <|Source -> {1, 2}|>],
				InfixNode[Comma, {
					LeafNode[Integer, "1", <|Source -> {1, 3, 1, 1}|>],
					LeafNode[Token`Comma, ",", <|Source -> {1, 3, 1, 2}|>],
					LeafNode[Integer, "2", <|Source -> {1, 3, 1, 3}|>],
					LeafNode[Token`Comma, ",", <|Source -> {1, 3, 1, 4}|>],
					LeafNode[Integer, "3", <|Source -> {1, 3, 1, 5}|>]}, <|Source -> {1, 3}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 4}|>]}, <||>], <|Source -> {}|>]}, <||>]
	,
	TestID->"TypeSpecifier-20220506-G6R6D4"
]

