
Needs["AST`"]


(*
PatternColon and OptionalColon
*)
Test[
	ConcreteParseString["a:b"]
	,
	BinaryNode[Pattern, {LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  LeafNode[Token`Colon, ":", <|Source -> {{1, 2}, {1, 2}}|>], 
  LeafNode[Symbol, 
   "b", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 
     3}}|>]
	,
	TestID->"Concrete-20190521-U1K7U2"
]

Test[
	ConcreteParseString["a_:b"]
	,
	BinaryNode[Optional, {PatternBlankNode[
   PatternBlank, {LeafNode[Symbol, 
     "a", <|Source -> {{1, 1}, {1, 1}}|>], 
    LeafNode[Token`Under, 
     "_", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 
       2}}|>], LeafNode[Token`Colon, 
   ":", <|Source -> {{1, 3}, {1, 3}}|>], 
  LeafNode[Symbol, 
   "b", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 
     4}}|>]
	,
	TestID->"Concrete-20190521-V6P9B0"
]









(*
make sure that both InternalNullNodes have correct Sources (and not the same ones)
*)
Test[
	ConcreteParseString["a; ;"]
	,
	InfixNode[CompoundExpression, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 3}, {1, 3}}|>],
		LeafNode[Token`Fake`Null, "", <|Source -> {{1, 4}, {1, 4}}|>],
		LeafNode[Token`Semi, ";", <|Source -> {{1, 4}, {1, 4}}|>],
		LeafNode[Token`Fake`Null, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]
	,
	TestID->"Concrete-20190117-C3R5P5"
]












Test[
	ConcreteParseString["?a"]
	,
	StartOfLineNode[Information, {LeafNode[Token`Question, 
   "?", <|Source -> {{1, 1}, {1, 1}}|>], 
  LeafNode[String, 
   "a", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 
     2}}|>]
	,
	TestID->"Concrete-20190601-B0Y1X6"
]



Test[
	ConcreteParseString["{\n?a}"]
	,
	GroupNode[List, {
		LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`Newline, "\n", <|Source -> {{2, 0}, {2, 0}}|>],
		SyntaxErrorNode[SyntaxError`ExpectedPossibleExpression, {
			LeafNode[Token`Question, "?", <|Source -> {{2, 1}, {2, 1}}|>]
			}, <|Source -> {{2, 1}, {2, 1}}|>],
		LeafNode[Symbol, "a", <|Source -> {{2, 2}, {2, 2}}|>],
		LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 3}, {2, 3}}|>]
	}, <|Source -> {{1, 1}, {2, 3}}|>]
	,
	TestID->"Concrete-20190601-H6L1N7"
]






(*
line continuations and newlines
*)
Test[
	ConcreteParseString["\"abc\\\r\ndef\""]
	,
	LeafNode[String, "\"abc\ndef\"", <|Source -> {{1, 1}, {2, 4}}|>]
	,
	TestID->"Concrete-20190606-U7J9I3"
]







Test[
	ConcreteParseString[""]
	,
	Null
	,
	TestID->"Concrete-20190227-H6C1K7"
]






(* Syntax Errors *)

Test[
	ConcreteParseString["a ~f x"]
	,
	SyntaxErrorNode[SyntaxError`ExpectedTilde, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
		LeafNode[Token`Tilde, "~", <|Source -> {{1, 3}, {1, 3}}|>],
		LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 4}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 5}, {1, 5}}|>],
		LeafNode[Symbol, "x", <|Source -> {{1, 6}, {1, 6}}|>] }, <|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"Concrete-20190521-L2C2Y8"
]










Test[
	ConcreteParseString["f[1,\\[InvisibleComma]2]"]
	,
	CallNode[{ LeafNode[Symbol, "f", <|Source->{{1,1}, {1,1}}|>] }, {
		GroupNode[GroupSquare, {
			LeafNode[Token`OpenSquare, "[", <|Source->{{1,2}, {1,2}}|>],
			InfixNode[Comma, {
				LeafNode[Integer, "1", <|Source->{{1,3}, {1,3}}|>],
				LeafNode[Token`Comma, ",", <|Source->{{1,4}, {1,4}}|>],
				LeafNode[Token`Fake`Null, "", <|Source->{{1,5}, {1,5}}|>],
				LeafNode[Token`LongName`InvisibleComma, "\\[InvisibleComma]", <|Source->{{1,5}, {1,21}}|>],
				LeafNode[Integer, "2", <|Source->{{1,22}, {1,22}}|>]}, <|Source->{{1,3}, {1,22}}|>],
			LeafNode[Token`CloseSquare, "]", <|Source->{{1,23}, {1,23}}|>] }, <|Source->{{1,2}, {1,23}}|>] }, <|Source->{{1,1}, {1,23}}|>]
	,
	TestID->"Concrete-20190704-F8G9M5"
]





Test[
	ConcreteParseString["f[b : c]"]
	,
	CallNode[{LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 1}}|>]}, {
		GroupNode[GroupSquare, {
			LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 2}, {1, 2}}|>],
			BinaryNode[Pattern, {
				LeafNode[Symbol, "b", <|Source -> {{1, 3}, {1, 3}}|>],
				LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 4}, {1, 4}}|>],
				LeafNode[Token`Colon, ":", <|Source -> {{1, 5}, {1, 5}}|>],
				LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 6}, {1, 6}}|>],
				LeafNode[Symbol, "c", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 7}}|>],
			LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 8}, {1, 8}}|>]}, <|Source -> {{1, 2}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"Concrete-20190709-P9A0T8"
]







Test[
	ConcreteParseString["{ ( a }"]
	,
	GroupNode[List, {
		LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
		GroupMissingOpenerNode[List, {
			LeafNode[Token`OpenParen, "(", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 4}, {1, 4}}|>],
			LeafNode[Symbol, "a", <|Source -> {{1, 5}, {1, 5}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 6}, {1, 6}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 7}}|>],
		LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Concrete-20190717-L0P2V0"
]

Test[
	ConcreteParseString["{ a ) }"]
	,
	SyntaxErrorNode[SyntaxError`ExpectedPossibleExpression, {
		LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 7}, {1, 7}}|>]
	,
	TestID->"Concrete-20190717-N8D4U4"
]

















