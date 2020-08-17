
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]






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


Test[
	CodeConcreteParse["{b+)c}"]
	,
	ContainerNode[String, {
		GroupMissingCloserNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 2}}|>],
			InfixNode[Plus, {
				LeafNode[Symbol, "b", <|Source -> {{1, 2}, {1, 3}}|>],
				LeafNode[Token`Plus, "+", <|Source -> {{1, 3}, {1, 4}}|>],
				ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 2}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>], 
  		ErrorNode[Token`Error`UnexpectedCloser, ")", <|Source -> {{1, 4}, {1, 5}}|>],
  		LeafNode[Symbol, "c", <|Source -> {{1, 5}, {1, 6}}|>],
  		ErrorNode[Token`Error`UnexpectedCloser, "}", <|Source -> {{1, 6}, {1, 7}}|>]}, <||>]
	,
	TestID->"Errors-20200516-T4E0K2"
]


(*
ExpectedOperand:
*)

TestMatch[
	CodeParse["{ + }"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "List", <||>], {
			CallNode[LeafNode[Symbol, "Plus", <||>], {
	    		ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 4}, {1, 4}} |>]},
	    		<|Source -> {{1, 3}, {1, 4}}|>]},
	    	<|Source -> {{1, 1}, {1, 6}}|>] },
	    <||>]
	,
	TestID->"Errors-20190521-C1B3O0"
]


(*
ExpectedPossibleExpression:
*)

TestMatch[
	CodeParse["&"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Function", <||>], {
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>]},
			<|Source -> {{1, 1}, {1, 2}}|>] },
		<||>]
	,
	TestID->"Errors-20190521-O5D4A9"
]


(*
SyntaxError:
*)

TestMatch[
	CodeConcreteParse["\\"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\", <|Source -> {{1, 1}, {1, 2}}|>] }, <||>]
	,
	TestID->"Errors-20190521-P7R3O7"
]


TestMatch[
	CodeConcreteParse["A`;"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`ExpectedLetterlike, "A`", <|Source -> {{1, 1}, {1, 3}}|>],
		InfixNode[CompoundExpression, {
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 3}, {1, 4}}|>]}, <||>]
	,
	TestID->"Errors-20200621-A3S2X7"
]


TestMatch[
	CodeConcreteParse["\"123"]
	,
	ContainerNode[String, {
		(*
		deliberately empty content
		*)
		ErrorNode[Token`Error`UnterminatedString, "\"123", <|Source -> {{1, 1}, {1, 5}}|>]}, <||>]
	,
	TestID->"Errors-20200623-O0B7Z2"
]


TestMatch[
	CodeConcreteParse["f[a@,2]"]
	,
	ContainerNode[String, {
		CallNode[{LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>]}, {
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 2}, {1, 3}}|>], 
				InfixNode[Comma, {
					BinaryNode[BinaryAt, {
						LeafNode[Symbol, "a", <|Source -> {{1, 3}, {1, 4}}|>], 
				        LeafNode[Token`At, "@", <|Source -> {{1, 4}, {1, 5}}|>], 
				        ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 3}, {1, 5}}|>],
				    LeafNode[Token`Comma, ",", <|Source -> {{1, 5}, {1, 6}}|>], 
				    LeafNode[Integer, "2", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 7}}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 2}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]}, <||>]
	,
	TestID->"Errors-20200627-I9S5C6"
]


Test[
	CodeConcreteParse["a~b~"]
	,
	ContainerNode[String, {
		TernaryNode[TernaryTilde, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {{1, 2}, {1, 3}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {{1, 4}, {1, 5}}|>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"Errors-20200628-R6O2J3"
]



Test[
	CodeParse["-"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Times", <||>], {
			LeafNode[Integer, "-1", <||>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]
	,
	TestID->"Errors-20200629-Y2S2R8"
]
