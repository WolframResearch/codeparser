
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
    LeafNode[Blank, "_", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 
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
		LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 2}, {1, 2}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 3}, {1, 3}}|>],
		LeafNode[Token`Semi, ";", <|Source -> {{1, 4}, {1, 4}}|>],
		LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]
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
	LeafNode[String, "\"abc\\\n\ndef\"", <|Source -> {{1, 1}, {2, 4}}|>]
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
				LeafNode[Token`Fake`ImplicitNull, "", <|Source->{{1,4}, {1,4}}|>],
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






Test[
	ConcreteParseString[";;(**);;"]
	,
	InfixNode[Times, {
		BinaryNode[Span, {
			LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
		LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 7}, {1, 7}}|>],
		BinaryNode[Span, {
			LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 7}, {1, 7}}|>],
			LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 7}, {1, 8}}|>],
			LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 8}, {1, 8}}|>]}, <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"Concrete-20190914-Z5W3U0"
]

Test[
	ConcreteParseString["a;;b;;(**)&"]
	,
	PostfixNode[Function, {
		InfixNode[Times, {
			BinaryNode[Span, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], 
		     	LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 2}, {1, 3}}|>], 
		     	LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>],
      		LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 5}, {1, 5}}|>], 
      		BinaryNode[Span, {LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 5}, {1, 5}}|>], 
      			LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 5}, {1, 6}}|>], 
      			LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>], 
  		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 7}, {1, 10}}|>],
  		LeafNode[Token`Amp, "&", <|Source -> {{1, 11}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>]
	,
	TestID->"Concrete-20190914-U5A2R0"
]

Test[
	ConcreteParseString["a;(**)&"]
	,
	PostfixNode[Function, {
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
		LeafNode[Token`Amp, "&", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Concrete-20190914-L1P0K2"
]

Test[
	ConcreteParseString["a;(**);"]
	,
	InfixNode[CompoundExpression, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
		LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 2}, {1, 2}}|>],
		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
		LeafNode[Token`Semi, ";", <|Source -> {{1, 7}, {1, 7}}|>],
		LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Concrete-20190914-E0H4C5"
]

Test[
	ConcreteParseString["{a;\n}"]
	,
	GroupNode[List, {
		LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "a", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 2}, {1, 3}}|>],
		LeafNode[Token`Newline, "\n", <|Source -> {{2, 0}, {2, 0}}|>],
		LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 1}, {2, 1}}|>]}, <|Source -> {{1, 1}, {2, 1}}|>]
	,
	TestID->"Concrete-20190914-X0P1Q6"
]



Test[
	ConcreteParseString["a;;;;(**);;"]
	,
	InfixNode[Times, {
		BinaryNode[Span, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
    		LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 2}, {1, 3}}|>], 
    		LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>],
    	LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 4}, {1, 4}}|>], 
    	BinaryNode[Span, {
    		LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 4}, {1, 4}}|>],
    		LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 4}, {1, 5}}|>],
    		LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 4}, {1, 5}}|>],
    	LeafNode[Token`Comment, "(**)", <|Source -> {{1, 6}, {1, 9}}|>],
    	LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 10}, {1, 10}}|>],
    	BinaryNode[Span, {
    		LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 10}, {1, 10}}|>],
    		LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 10}, {1, 11}}|>],
    		LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 11}, {1, 11}}|>]}, <|Source -> {{1, 10}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>]
	,
	TestID->"Concrete-20190914-K2Z7E2"
]


Test[
	ConcreteParseString["a(**):b"]
	,
	BinaryNode[Pattern, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 2}, {1, 5}}|>],
		LeafNode[Token`Colon, ":", <|Source -> {{1, 6}, {1, 6}}|>],
		LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Concrete-20190915-S7T5K8"
]

Test[
	ConcreteParseString["a:(**)b"]
	,
	BinaryNode[Pattern, {
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`Colon, ":", <|Source -> {{1, 2}, {1, 2}}|>],
		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
		LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Concrete-20190915-E7Q5J3"
]

Test[
	ConcreteParseString[";;(**)*2"]
	,
	InfixNode[Times, {
		BinaryNode[Span, {
			LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
		LeafNode[Token`Star, "*", <|Source -> {{1, 7}, {1, 7}}|>],
		LeafNode[Integer, "2", <|Source -> {{1, 8}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]
	,
	TestID->"Concrete-20190915-Y4E3E1"
]





