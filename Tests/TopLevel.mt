
Needs["CodeParser`"]


Test[
	CodeParse["foo[] := Message[foo::bad]; $Failed"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			CallNode[LeafNode[Symbol, "SetDelayed", <||>], {
				CallNode[LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>], {}, <|Source -> {{1, 1}, {1, 6}}|>],
				CallNode[LeafNode[Symbol, "Message", <|Source -> {{1, 10}, {1, 17}}|>], {
					CallNode[LeafNode[Symbol, "MessageName", <||>], {
						LeafNode[Symbol, "foo", <|Source -> {{1, 18}, {1, 21}}|>],
						LeafNode[String, "\"bad\"", <|Source -> {{1, 23}, {1, 26}}|>]}, <|Source -> {{1, 18}, {1, 26}}|>]}, <|Source -> {{1, 10}, {1, 27}}|>]}, <|Source -> {{1, 1}, {1, 27}}|>],
			LeafNode[Symbol, "$Failed", <|Source -> {{1, 29}, {1, 36}}|>]}, <|Source -> {{1, 1}, {1, 36}}|>]}
		,
		<|Source -> {{1, 1}, {1, 36}},
			AbstractSyntaxIssues -> {
			SyntaxIssue["TopLevelDefinitionCompoundExpression", "Definition does not contain the end of the ``CompoundExpression``.", "Error", <|
				Source -> {{1, 29}, {1, 36}},
				ConfidenceLevel -> 0.75,
				"AdditionalDescriptions" -> {"Consider breaking up onto separate lines."},
				"CompoundExpressionSource" -> {{1, 1}, {1, 36}}
			|>]}
		|>]
	,
	TestID->"TopLevel-20220418-E9F3E1"
]


(*
treat as declarations, so no warnings
*)
Test[
	CodeParse["a; b; c"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 5}}|>],
			LeafNode[Symbol, "c", <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"TopLevel-20220418-L3L8A8"
]


(*
no Package issue here

the Begin is too "buried" and it's not worth digging
*)
Test[
	CodeParse["a; Begin[\"Foo`\"]; c"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Begin", <|Source -> {{1, 4}, {1, 9}}|>], {LeafNode[String, "\"Foo`\"", <|Source -> {{1, 10}, {1, 16}}|>]}, <|Source -> {{1, 4}, {1, 17}}|>],
			LeafNode[Symbol, "c", <|Source -> {{1, 19}, {1, 20}}|>]}, <|Source -> {{1, 1}, {1, 20}}|>]}
		,
		<|Source -> {{1, 1}, {1, 20}},
			AbstractSyntaxIssues -> {
			SyntaxIssue["TopLevelCompoundExpression", "Unexpected ``CompoundExpression`` at top-level.", "Remark", <|
				Source -> {{1, 1}, {1, 20}},
				ConfidenceLevel -> 0.95,
				"AdditionalDescriptions" -> {"Consider breaking up onto separate lines."}
			|>]}
		|>]
	,
	TestID->"TopLevel-20220418-N7B1M0"
]


(*
the Begin here is easy to scan, so give a Package issue
*)
Test[
	CodeParse["Begin[\"Foo`\"];"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			CallNode[LeafNode[Symbol, "Begin", <|Source -> {{1, 1}, {1, 6}}|>], {LeafNode[String, "\"Foo`\"", <|Source -> {{1, 7}, {1, 13}}|>]}, <|Source -> {{1, 1}, {1, 14}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 15}, {1, 15}}|>]}, <|Source -> {{1, 1}, {1, 15}}|>]}
		,
		<|Source -> {{1, 1}, {1, 15}},
			AbstractSyntaxIssues -> {
			SyntaxIssue["Package", "There are unbalanced directives.", "Error", <|
				Source -> {{1, 1}, {1, 15}},
				ConfidenceLevel -> 0.7|>]}
		|>]
	,
	TestID->"TopLevel-20220418-N9M3C5"
]


Test[
	CodeParse["\
Begin[\"Foo`\"];
1+1;
End[];"]
	,
	ContainerNode[String, {
		ContextNode[{LeafNode[String, "\"Foo`\"", <|Source -> {{1, 7}, {1, 13}}|>]}, {
			CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
				CallNode[LeafNode[Symbol, "Plus", <||>], {
					LeafNode[Integer, "1", <|Source -> {{2, 1}, {2, 2}}|>],
					LeafNode[Integer, "1", <|Source -> {{2, 3}, {2, 4}}|>]}, <|Source -> {{2, 1}, {2, 4}}|>],
				LeafNode[Symbol, "Null", <|Source -> {{2, 5}, {2, 5}}|>]}, <|Source -> {{2, 1}, {2, 5}}|>]}, <|Source -> {{1, 1}, {3, 6}}|>]}, <|Source -> {{1, 1}, {3, 7}}|>]
	,
	TestID->"TopLevel-20220418-C9P1G1"
]


Test[
	CodeParse["foo[] := 1+1;"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			CallNode[LeafNode[Symbol, "SetDelayed", <||>], {
				CallNode[LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>], {}, <|Source -> {{1, 1}, {1, 6}}|>],
				CallNode[LeafNode[Symbol, "Plus", <||>], {
					LeafNode[Integer, "1", <|Source -> {{1, 10}, {1, 11}}|>],
					LeafNode[Integer, "1", <|Source -> {{1, 12}, {1, 13}}|>]}, <|Source -> {{1, 10}, {1, 13}}|>]}, <|Source -> {{1, 1}, {1, 13}}, "Definitions" -> {LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>]}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 14}, {1, 14}}|>]}, <|Source -> {{1, 1}, {1, 14}}|>]}, <|Source -> {{1, 1}, {1, 14}}|>]
	,
	TestID->"TopLevel-20220122-L2U7V1"
]


Test[
	CodeParse["foo[] := 1+1; ;"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "CompoundExpression", <||>], {
			CallNode[LeafNode[Symbol, "SetDelayed", <||>], {
				CallNode[LeafNode[Symbol, "foo", <|Source -> {{1, 1}, {1, 4}}|>], {}, <|Source -> {{1, 1}, {1, 6}}|>],
				CallNode[LeafNode[Symbol, "Plus", <||>], {
					LeafNode[Integer, "1", <|Source -> {{1, 10}, {1, 11}}|>],
					LeafNode[Integer, "1", <|Source -> {{1, 12}, {1, 13}}|>]}, <|Source -> {{1, 10}, {1, 13}}|>]}, <|Source -> {{1, 1}, {1, 13}}|>],
				LeafNode[Symbol, "Null", <|Source -> {{1, 15}, {1, 15}}|>],
				LeafNode[Symbol, "Null", <|Source -> {{1, 16}, {1, 16}}|>]}, <|Source -> {{1, 1}, {1, 16}}|>]}
		,
		<|Source -> {{1, 1}, {1, 16}},
			AbstractSyntaxIssues -> {
			SyntaxIssue["TopLevelCompoundExpression", "Unexpected ``CompoundExpression`` at top-level.", "Remark", <|
				Source -> {{1, 1}, {1, 16}},
				ConfidenceLevel -> 0.95,
				"AdditionalDescriptions" -> {"Consider breaking up onto separate lines."}|>]}
		|>]
	,
	TestID->"TopLevel-20220122-G7R4U6"
]


Test[
	CodeParse["a::b=1"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Set", <||>], {
			CallNode[LeafNode[Symbol, "MessageName", <||>], {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[String, "\"b\"", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>],
			LeafNode[Integer, "1", <|Source -> {{1, 6}, {1, 7}}|>]},
		
			<|Source -> {{1, 1}, {1, 7}}, "AdditionalDefinitions" -> {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>]}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"TopLevel-20220808-I5K1K1"
]



Test[
	CodeParse["

BeginPackage[\"Foo`\", {}]

EndPackage[]

"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "BeginPackage", <|Source -> {{3, 1}, {3, 13}}|>], {
			LeafNode[String, "\"Foo`\"", <|Source -> {{3, 14}, {3, 20}}|>],
			CallNode[LeafNode[Symbol, "List", <||>], {}, <|Source -> {{3, 22}, {3, 24}}|>]}, <|Source -> {{3, 1}, {3, 25}}|>],
		CallNode[LeafNode[Symbol, "EndPackage", <|Source -> {{5, 1}, {5, 11}}|>], {}, <|Source -> {{5, 1}, {5, 13}}|>]}, <|Source -> {{1, 1}, {7, 1}},
	
		AbstractSyntaxIssues -> {
			SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", <|Source -> {{3, 1}, {3, 25}}, ConfidenceLevel -> 0.95|>]}|>]
	,
	TestID->"TopLevel-20221017-B8C2H1"
]







