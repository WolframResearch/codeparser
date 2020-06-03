
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



TestMatch[
	CodeParse["f["]
	,
	ContainerNode[String, {
		CallNode[_, { _GroupMissingCloserNode }, _] }, <||>]
	,
	TestID->"Errors-20190701-H7G3R7"
]



(*
Malformed \[] characters
Unrecognized \[] characters
*)

ast = CodeParse["\"\\[.*\\]\""]

child = ast[[2]][[1]]

type = child[[1]]
s = child[[2]]

data = ast[[3]]
issues = data[SyntaxIssues]

Test[
	Head[child]
	,
	LeafNode
	,
	TestID->"Errors-20181207-O9W0O1"
]

Test[
	type
	,
	String
	,
	TestID->"Errors-20190619-K2V0N7"
]

Test[
	s
	,
	"\"\\[.*\\]\""
	,
	TestID->"Errors-20181207-X9G8E1"
]

Test[
	Length[issues]
	,
	2
	,
	TestID->"Errors-20181202-E8N4Z4"
]






(*
Parsing <newline>23 should be fine, but parsing \n23 should fail
*)

(*
Use "1\n23" here because "\n23" is parsed as a single expression by CodeParser and as two expressions by the kernel
*)
Test[
	"1\n23"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Errors-20190126-W2J2Q1"
]

TestMatch[
	CodeParse["\\n23", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)]
	,
	ContainerNode[Hold, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\n", _],
		LeafNode[Integer, "23", _] }, _]
	,
	TestID->"Errors-20190126-Q9U0H8"
]

TestMatch[
	CodeParse["\\t23", ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)]
	,
	ContainerNode[Hold, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\t", _],
		LeafNode[Integer, "23", _] }, _]
	,
	TestID->"Errors-20190203-F5C9L1"
]

(*
important that space after - is not in SyntaxErrorNode

The  a - \t  and  b  are 2 separate expressions
*)
Test[
	CodeConcreteParse["a - \\tb"]
	,
	ContainerNode[String, {
		InfixNode[Times, {
			InfixNode[Plus, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
	    		LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>], 
	    		LeafNode[Token`Minus, "-", <|Source -> {{1, 3}, {1, 4}}|>], 
	    		LeafNode[Whitespace, " ", <|Source -> {{1, 4}, {1, 5}}|>], 
	   			ErrorNode[Token`Error`UnhandledCharacter, "\\t", <|Source -> {{1, 5}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>],
	       	LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 7}, {1, 7}}|>],
	       	LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 8}}|>] }, <|Source -> {{1, 1}, {1, 8}}|>] }, <||>]
	,
	TestID->"Errors-20190203-G0U2N7"
]

TestMatch[
	CodeParse["\\"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, _, _] }, <||> ]
	,
	TestID->"Errors-20190203-M3A0S4"
]







Test[
	CodeParse["(a[b[])"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "a", <|Source -> {{1, 2}, {1, 3}}|>], {
			GroupMissingCloserNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 3}, {1, 4}}|>],
				CallNode[LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 5}}|>], {
					GroupNode[GroupSquare, {
						LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 5}, {1, 6}}|>],
						LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 5}, {1, 7}}|>]}, <|Source -> {{1, 4}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 7}}|>]}, <|Source -> {{1, 2}, {1, 7}}|>] }, <||>]
	,
	TestID->"Errors-20190803-C7O2S5"
]




Test[
	CodeConcreteParse["\\[]"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\[]", <|Source -> {{1, 1}, {1, 4}}|>]},
		<|SyntaxIssues -> {
			SyntaxIssue["UnrecognizedCharacter", "Unrecognized character: ``\\[]``.", "Error", <|Source -> {{1, 1}, {1, 4}}, ConfidenceLevel -> 1., 
      			CodeActions -> {CodeAction["Replace with \\\\[]", ReplaceText, <|Source -> {{1, 1}, {1, 4}}, "ReplacementText" -> "\\\\[]"|>]}|>]}|>]
	,
	TestID->"Errors-20200602-U2U6P1"
]








