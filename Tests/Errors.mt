
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "ASTTestUtils"}]
PrependTo[$Path, path]

Needs["ASTTestUtils`"]

Needs["AST`"]
Needs["AST`Utils`"]



TestMatch[
	ParseString["f["]
	,
	CallNode[_, { _AbstractSyntaxErrorNode }, _]
	,
	TestID->"Errors-20190701-H7G3R7"
]



(*
Malformed \[] characters
Unrecognized \[] characters
*)

ast = ParseString["\"\\[.*\\]\""]

type = ast[[1]]
s = ast[[2]]
data = ast[[3]]
issues = data[SyntaxIssues]

Test[
	Head[ast]
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
Use "1\n23" here because "\n23" is parsed as a single expression by AST and as two expressions by the kernel
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
	ParseString["\\n23", HoldNode[Hold, #[[1]], <||>]&]
	,
	HoldNode[Hold, {
		CallNode[LeafNode[Symbol, "Times", _], {
			AbstractSyntaxErrorNode[AbstractSyntaxError`Unhandled, _, _],
			LeafNode[Integer, "23", _] }, _] }, _]
	,
	TestID->"Errors-20190126-Q9U0H8"
]

TestMatch[
	ParseString["\\t23", HoldNode[Hold, #[[1]], <||>]&]
	,
	HoldNode[Hold, {
		CallNode[LeafNode[Symbol, "Times", _], {
			AbstractSyntaxErrorNode[AbstractSyntaxError`Unhandled, _, _],
			LeafNode[Integer, "23", _] }, _] }, _]
	,
	TestID->"Errors-20190203-F5C9L1"
]

(*
important that space after - is not in SyntaxErrorNode
*)
Test[
	ConcreteParseString["a - \\tb"]
	,
	BinaryNode[Minus, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Minus, "-", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 4}, {1, 4}}|>],
			InfixNode[Times, {
				LeafNode[Token`Unhandled, "\\t", <|Source -> {{1, 5}, {1, 6}}|>], 
		  		LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 7}, {1, 7}}|>],
		  		LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 7}}|>] }, <|Source -> {{1, 5}, {1, 7}}|>] }, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Errors-20190203-G0U2N7"
]

TestMatch[
	ParseString["\\"]
	,
	_SyntaxErrorNode
	,
	TestID->"Errors-20190203-M3A0S4"
]







Test[
	ParseString["(a[b[])"]
	,
	CallNode[LeafNode[Symbol, 
  "a", <|Source -> {{1, 2}, {1, 2}}|>], {AbstractSyntaxErrorNode[
   AbstractSyntaxError`GroupMissingOpener, {LeafNode[Token`OpenSquare,
      "[", <|Source -> {{1, 3}, {1, 3}}|>], 
    CallNode[
     LeafNode[Symbol, 
      "b", <|Source -> {{1, 4}, {1, 4}}|>], {GroupNode[
       GroupSquare, {LeafNode[Token`OpenSquare, 
         "[", <|Source -> {{1, 5}, {1, 5}}|>], 
        LeafNode[Token`CloseSquare, 
         "]", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 
           5}, {1, 6}}|>]}, <|Source -> {{1, 4}, {1, 6}}|>], 
    LeafNode[Token`CloseParen, 
     ")", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 
       7}}|>]}, <|Source -> {{1, 2}, {1, 7}}|>]
	,
	TestID->"Errors-20190803-C7O2S5"
]





