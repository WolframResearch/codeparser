
Needs["AST`"]


(*
PatternColon and OptionalColon
*)
Test[
	ConcreteParseString["a:b"]
	,
	StringNode[String, {
		BinaryNode[Pattern, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  			LeafNode[Token`Colon, ":", <|Source -> {{1, 2}, {1, 2}}|>], 
  			LeafNode[Symbol, "b", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>] }, <||>]
	,
	TestID->"Concrete-20190521-U1K7U2"
]

Test[
	ConcreteParseString["a_:b"]
	,
	StringNode[String, {
		BinaryNode[Optional, {
			PatternBlankNode[PatternBlank, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
				LeafNode[Blank, "_", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Colon, ":", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>] }, <||>]
	,
	TestID->"Concrete-20190521-V6P9B0"
]









(*
make sure that both InternalNullNodes have correct Sources (and not the same ones)
*)
Test[
	ConcreteParseString["a; ;"]
	,
	StringNode[String, {
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 4}, {1, 4}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>] }, <||>]
	,
	TestID->"Concrete-20190117-C3R5P5"
]












Test[
	ConcreteParseString["?a"]
	,
	(*
	StringNode[String, {
		StartOfLineNode[Information, {
			LeafNode[Token`Question, "?", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[String, "a", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>] }, <||>]
	*)
	StringNode[String, {
		BinaryNode[PatternTest, {
			LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Question, "?", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Symbol, "a", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>]}, <||>]
	,
	TestID->"Concrete-20190601-B0Y1X6"
]



Test[
	ConcreteParseString["{\n?a}"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Newline, "\n", <|Source -> {{2, 0}, {2, 0}}|>],
			BinaryNode[PatternTest, {
				LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{2, 1}, {2, 1}}|>],
				LeafNode[Token`Question, "?", <|Source -> {{2, 1}, {2, 1}}|>],
				LeafNode[Symbol, "a", <|Source -> {{2, 2}, {2, 2}}|>]}, <|Source -> {{2, 1}, {2, 2}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 3}, {2, 3}}|>]}, <|Source -> {{1, 1}, {2, 3}}|>] }, <||>]
	,
	TestID->"Concrete-20190601-H6L1N7"
]






(*
line continuations and newlines
*)
Test[
	ConcreteParseString["\"abc\\\r\ndef\""]
	,
	StringNode[String, {
		LeafNode[String, "\"abc\ndef\"", <|Source -> {{1, 1}, {2, 4}}|>] }, <||>]
	,
	TestID->"Concrete-20190606-U7J9I3"
]







Test[
	ConcreteParseString[""]
	,
	StringNode[String, {}, <||>]
	,
	TestID->"Concrete-20190227-H6C1K7"
]






(* Syntax Errors *)

Test[
	ConcreteParseString["a ~f x", f]
	,
	f[{{
		InfixNode[Times, {
			SyntaxErrorNode[SyntaxError`ExpectedTilde, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
				LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
				LeafNode[Token`Tilde, "~", <|Source -> {{1, 3}, {1, 3}}|>],
				LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 4}}|>] }, <|Source -> {{1, 1}, {1, 4}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 5}, {1, 5}}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 6}, {1, 6}}|>],
			LeafNode[Symbol, "x", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]}, {}}]
	,
	TestID->"Concrete-20190521-L2C2Y8"
]










TestMatch[
	ConcreteParseString["f[1,\\[InvisibleComma]2]"]
	,
	StringNode[String, {
		CallNode[{ LeafNode[Symbol, "f", <|Source->{{1,1}, {1,1}}|>] }, {
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <|Source->{{1,2}, {1,2}}|>],
				InfixNode[Comma, {
					LeafNode[Integer, "1", <|Source->{{1,3}, {1,3}}|>],
					LeafNode[Token`Comma, ",", <|Source->{{1,4}, {1,4}}|>],
					LeafNode[Token`Fake`ImplicitNull, "", <|Source->{{1,4}, {1,4}}|>],
					LeafNode[Token`LongName`InvisibleComma, "\\[InvisibleComma]", <|Source->{{1,5}, {1,21}}|>],
					LeafNode[Integer, "2", <|Source->{{1,22}, {1,22}}|>]}, <|Source->{{1,3}, {1,22}}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 23}, {1, 23}}|>] }, <|Source -> {{1, 2}, {1, 23}}|>] }, <|Source -> {{1, 1}, {1, 23}}|>] }, _]
	,
	TestID->"Concrete-20190704-F8G9M5"
]





Test[
	ConcreteParseString["f[b : c]"]
	,
	StringNode[String, {
			CallNode[{LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 1}}|>]}, {
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 2}, {1, 2}}|>],
				BinaryNode[Pattern, {
					LeafNode[Symbol, "b", <|Source -> {{1, 3}, {1, 3}}|>],
					LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 4}, {1, 4}}|>],
					LeafNode[Token`Colon, ":", <|Source -> {{1, 5}, {1, 5}}|>],
					LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 6}, {1, 6}}|>],
					LeafNode[Symbol, "c", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 7}}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 8}, {1, 8}}|>]}, <|Source -> {{1, 2}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>] }, <||>]
	,
	TestID->"Concrete-20190709-P9A0T8"
]







Test[
	ConcreteParseString["{ ( a }"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
			GroupMissingCloserNode[GroupParen, {
				LeafNode[Token`OpenParen, "(", <|Source -> {{1, 3}, {1, 3}}|>],
				LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 4}, {1, 4}}|>],
				LeafNode[Symbol, "a", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 3}, {1, 5}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 6}, {1, 6}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>] }, <||>]
	,
	TestID->"Concrete-20190717-L0P2V0"
]

Test[
	ConcreteParseString["{ a ) }"]
	,
	StringNode[String, {
		GroupMissingCloserNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Symbol, "a", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 4}, {1, 4}}|>],
		SyntaxErrorNode[SyntaxError`UnexpectedCloser, {
			LeafNode[Token`CloseParen, ")", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 5}, {1, 5}}|>],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 6}, {1, 6}}|>],
		SyntaxErrorNode[SyntaxError`UnexpectedCloser, {
			LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 7}, {1, 7}}|>]}, <||>]
	,
	TestID->"Concrete-20190717-N8D4U4"
]






TestMatch[
	ConcreteParseString[";;(**);;"]
	,
	StringNode[String, {
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
				LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 8}, {1, 8}}|>]}, <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>] }, _]
	,
	TestID->"Concrete-20190914-Z5W3U0"
]

TestMatch[
	ConcreteParseString["a;;b;;(**)&"]
	,
	StringNode[String, {
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
	  		LeafNode[Token`Amp, "&", <|Source -> {{1, 11}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>] }, _]
	,
	TestID->"Concrete-20190914-U5A2R0"
]

Test[
	ConcreteParseString["a;(**)&"]
	,
	StringNode[String, {
		PostfixNode[Function, {
			InfixNode[CompoundExpression, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
				LeafNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
				LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
			LeafNode[Token`Amp, "&", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>] }, <||>]
	,
	TestID->"Concrete-20190914-L1P0K2"
]

Test[
	ConcreteParseString["a;(**);"]
	,
	StringNode[String, {
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 7}, {1, 7}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>] }, <||>]
	,
	TestID->"Concrete-20190914-E0H4C5"
]

Test[
	ConcreteParseString["{a;\n}"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			InfixNode[CompoundExpression, {
				LeafNode[Symbol, "a", <|Source -> {{1, 2}, {1, 2}}|>],
				LeafNode[Token`Semi, ";", <|Source -> {{1, 3}, {1, 3}}|>],
				LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 2}, {1, 3}}|>],
			LeafNode[Token`Newline, "\n", <|Source -> {{2, 0}, {2, 0}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 1}, {2, 1}}|>]}, <|Source -> {{1, 1}, {2, 1}}|>] }, <||>]
	,
	TestID->"Concrete-20190914-X0P1Q6"
]



TestMatch[
	ConcreteParseString["a;;;;;;"]
	,
	StringNode[String, {
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
	    	LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 6}, {1, 6}}|>],
	    	BinaryNode[Span, {
	    		LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 6}, {1, 6}}|>],
	    		LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 6}, {1, 7}}|>],
	    		LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>] }, _]
	,
	TestID->"Concrete-20190928-L8N1U0"
]

TestMatch[
	ConcreteParseString["a;;;;(**);;"]
	,
	StringNode[String, {
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
	    		LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 11}, {1, 11}}|>]}, <|Source -> {{1, 10}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>] }, _]
	,
	TestID->"Concrete-20190914-K2Z7E2"
]

Test[
	ConcreteParseString["a(**):b"]
	,
	StringNode[String, {
		BinaryNode[Pattern, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Comment, "(**)", <|Source -> {{1, 2}, {1, 5}}|>],
			LeafNode[Token`Colon, ":", <|Source -> {{1, 6}, {1, 6}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>] }, <||>]
	,
	TestID->"Concrete-20190915-S7T5K8"
]

Test[
	ConcreteParseString["a:(**)b"]
	,
	StringNode[String, {
		BinaryNode[Pattern, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Colon, ":", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 7}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>] }, <||>]
	,
	TestID->"Concrete-20190915-E7Q5J3"
]

Test[
	ConcreteParseString[";;(**)*2"]
	,
	StringNode[String, {
		InfixNode[Times, {
			BinaryNode[Span, {
				LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 1}, {1, 1}}|>],
				LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Comment, "(**)", <|Source -> {{1, 3}, {1, 6}}|>],
			LeafNode[Token`Star, "*", <|Source -> {{1, 7}, {1, 7}}|>],
			LeafNode[Integer, "2", <|Source -> {{1, 8}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>] }, <||>]
	,
	TestID->"Concrete-20190915-Y4E3E1"
]

Test[
	ConcreteParseString["{ headIn__ }"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
			PatternBlankSequenceNode[PatternBlankSequence, {
				LeafNode[Symbol, "headIn", <|Source -> {{1, 3}, {1, 8}}|>],
				LeafNode[BlankSequence, "__", <|Source -> {{1, 9}, {1, 10}}|>]}, <|Source -> {{1, 3}, {1, 10}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 11}, {1, 11}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 12}, {1, 12}}|>]}, <|Source -> {{1, 1}, {1, 12}}|>] }, <||>]
	,
	TestID->"Concrete-20190916-X5M4H9"
]

Test[
	ConcreteParseString["a(*1*)+(*2*)b(*3*);"]
	,
	StringNode[String, {
		InfixNode[CompoundExpression, {
			InfixNode[Plus, {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
				LeafNode[Token`Comment, "(*1*)", <|Source -> {{1, 2}, {1, 6}}|>],
				LeafNode[Token`Plus, "+", <|Source -> {{1, 7}, {1, 7}}|>],
				LeafNode[Token`Comment, "(*2*)", <|Source -> {{1, 8}, {1, 12}}|>],
				LeafNode[Symbol, "b", <|Source -> {{1, 13}, {1, 13}}|>]}, <|Source -> {{1, 1}, {1, 13}}|>],
			LeafNode[Token`Comment, "(*3*)", <|Source -> {{1, 14}, {1, 18}}|>],
			LeafNode[Token`Semi, ";", <|Source -> {{1, 19}, {1, 19}}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {{1, 19}, {1, 19}}|>]}, <|Source -> {{1, 1}, {1, 19}}|>] }, <||>]
	,
	TestID->"Concrete-20190916-L9G1L8"
]

Test[
	ConcreteParseString["{(*1*)[(*2*)](*3*)}"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Comment, "(*1*)", <|Source -> {{1, 2}, {1, 6}}|>],
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 7}, {1, 7}}|>],
				LeafNode[Token`Comment, "(*2*)", <|Source -> {{1, 8}, {1, 12}}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 13}, {1, 13}}|>]}, <|Source -> {{1, 7}, {1, 13}}|>],
			LeafNode[Token`Comment, "(*3*)", <|Source -> {{1, 14}, {1, 18}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 19}, {1, 19}}|>]}, <|Source -> {{1, 1}, {1, 19}}|>] }, <||>]
	,
	TestID->"Concrete-20190916-U0Q1K6"
]

TestMatch[
	ConcreteParseString["{(*0*)a(*1*),(*2*)b(*3*)}"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Comment, "(*0*)", <|Source -> {{1, 2}, {1, 6}}|>],
			InfixNode[Comma, {
				LeafNode[Symbol, "a", <|Source -> {{1, 7}, {1, 7}}|>],
				LeafNode[Token`Comment, "(*1*)", <|Source -> {{1, 8}, {1, 12}}|>],
				LeafNode[Token`Comma, ",", <|Source -> {{1, 13}, {1, 13}}|>],
				LeafNode[Token`Comment, "(*2*)", <|Source -> {{1, 14}, {1, 18}}|>],
				LeafNode[Symbol, "b", <|Source -> {{1, 19}, {1, 19}}|>]}, <|Source -> {{1, 7}, {1, 19}}|>],
			LeafNode[Token`Comment, "(*3*)", <|Source -> {{1, 20}, {1, 24}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 25}, {1, 25}}|>]}, <|Source -> {{1, 1}, {1, 25}}|>] }, _]
	,
	TestID->"Concrete-20190916-C5C7M2"
]

Test[
	ConcreteParseString["a=..", f]
	,
	f[{{LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		LeafNode[Token`Error`UnhandledDot, "=..", <|Source -> {{1, 2}, {1, 4}}|>]}, {}}]
	,
	TestID->"Concrete-20190916-N2A8O1"
]

Test[
	ConcreteParseString["a~f~b"]
	,
	StringNode[String, {
		TernaryNode[TernaryTilde, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Symbol, "f", <|Source -> {{1, 3}, {1, 3}}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {{1, 4}, {1, 4}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>] }, <||>]
	,
	TestID->"Concrete-20190928-H3B3V1"
]

Test[
	ConcreteParseString["a /: b = (c = d)"]
	,
	StringNode[String, {
		TernaryNode[TagSet, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Token`SlashColon, "/:", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 5}, {1, 5}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 6}, {1, 6}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 7}, {1, 7}}|>],
			LeafNode[Token`Equal, "=", <|Source -> {{1, 8}, {1, 8}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 9}, {1, 9}}|>],
			GroupNode[GroupParen, {
				LeafNode[Token`OpenParen, "(", <|Source -> {{1, 10}, {1, 10}}|>],
				BinaryNode[Set, {
					LeafNode[Symbol, "c", <|Source -> {{1, 11}, {1, 11}}|>],
					LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 12}, {1, 12}}|>],
					LeafNode[Token`Equal, "=", <|Source -> {{1, 13}, {1, 13}}|>],
					LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 14}, {1, 14}}|>],
					LeafNode[Symbol, "d", <|Source -> {{1, 15}, {1, 15}}|>]}, <|Source -> {{1, 11}, {1, 15}}|>],
				LeafNode[Token`CloseParen, ")", <|Source -> {{1, 16}, {1, 16}}|>]}, <|Source -> {{1, 10}, {1, 16}}|>]}, <|Source -> {{1, 1}, {1, 16}}|>] }, <||>]
	,
	TestID->"Concrete-20190929-V7J8D5"
]
			
TestMatch[
	ConcreteParseString["{12,\\\n3}"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			InfixNode[Comma, {
				LeafNode[Integer, "12", <|Source -> {{1, 2}, {1, 3}}|>],
				LeafNode[Token`Comma, ",", <|Source -> {{1, 4}, {1, 4}}|>],
				LeafNode[Token`LineContinuation, "\\\n", <|Source -> {{1, 5}, {2, 0}}|>],
				LeafNode[Integer, "3", <|Source -> {{2, 1}, {2, 1}}|>]}, <|Source -> {{1, 2}, {2, 1}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 2}, {2, 2}}|>]}, <|Source -> {{1, 1}, {2, 2}}|>] }, _]
	,
	TestID->"Concrete-20190930-B8P9Y9"
]

Test[
	ConcreteParseString["{ @@ }"]
	,
	StringNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>],
			BinaryNode[Apply, {
				LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 3}, {1, 3}}|>], 
	    		LeafNode[Token`AtAt, "@@", <|Source -> {{1, 3}, {1, 4}}|>], 
	    		LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 3}, {1, 4}}|>],
	    	LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 5}, {1, 5}}|>], 
	    	LeafNode[Token`CloseCurly, "}", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>] }, <||>]
	,
	TestID->"Concrete-20191012-S7X3B0"
]

Test[
	ConcreteParseString["\"a\\\\\r\nb\""]
	,
	StringNode[String, {
		LeafNode[String, "\"a\\\\\r\nb\"", <|Source -> {{1, 1}, {2, 2}}|>] }, <||>]
	,
	TestID->"Concrete-20191024-X1D5H3"
]

Test[
	ConcreteParseString["^ "]
	,
	StringNode[String, {
		BinaryNode[Power, {
			LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Caret, "^", <|Source -> {{1, 1}, {1, 1}}|>],
			LeafNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>]}, <||>]}],
		LeafNode[Token`WhiteSpace, " ", <|Source -> {{1, 2}, {1, 2}}|>]
	,
	TestID->"Concrete-20191117-M2R5P9"
]

TestMatch[
	ConcreteParseString["0.."]
	,
	StringNode[String, {
		PostfixNode[Repeated, {
			LeafNode[Integer, "0", <|Source -> {{2, 1}, {2, 1}}|>],
			LeafNode[Token`DotDot, "..", <|Source -> {{2, 2}, {2, 3}}|>]}, <|Source -> {{2, 1}, {2, 3}}|>]}, _]
	,
	TestID -> "Concrete-20191119-J0T1L1"
]

