
Needs["AST`"]


(*
PatternColon and OptionalColon
*)
Test[
	ConcreteParseString["a:b"]
	,
	BinaryNode[Pattern, {SymbolNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  TokenNode[Token`Fake`PatternColon, ":", <|Source -> {{1, 2}, {1, 2}}|>], 
  SymbolNode[Symbol, 
   "b", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 
     3}}|>]
	,
	TestID->"Concrete-20190521-U1K7U2"
]

Test[
	ConcreteParseString["a_:b"]
	,
	BinaryNode[Optional, {PatternBlankNode[
   PatternBlank, {SymbolNode[Symbol, 
     "a", <|Source -> {{1, 1}, {1, 1}}|>], 
    TokenNode[Token`Under, 
     "_", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 
       2}}|>], TokenNode[Token`Fake`OptionalColon, 
   ":", <|Source -> {{1, 3}, {1, 3}}|>], 
  SymbolNode[Symbol, 
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
		SymbolNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		TokenNode[Token`Semi, ";", <|Source -> {{1, 2}, {1, 2}}|>],
		InternalNullNode[Null, "", <|Source -> {{1, 2}, {1, 2}}|>],
		TokenNode[Token`Semi, ";", <|Source -> {{1, 4}, {1, 4}}|>],
		InternalNullNode[Null, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]
	,
	TestID->"Concrete-20190117-C3R5P5"
]












Test[
	ConcreteParseString["?a"]
	,
	PrefixNode[Information, {TokenNode[Token`Question, 
   "?", <|Source -> {{1, 1}, {1, 1}}|>], 
  StringNode[String, 
   "a", <|Source -> {{1, 2}, {1, 2}}|>]}, <|Source -> {{1, 1}, {1, 
     2}}|>]
	,
	TestID->"Concrete-20190601-B0Y1X6"
]



Test[
	ConcreteParseString["{\n?a}"]
	,
	GroupNode[List, {TokenNode[Token`OpenCurly, 
   "{", <|Source -> {{1, 1}, {1, 1}}|>], 
  SyntaxErrorNode[
   SyntaxError`ExpectedPossibleExpression, {TokenNode[Token`Question, 
     "?", <|Source -> {{2, 1}, {2, 1}}|>]}, <|Source -> {{2, 1}, {2, 
       1}}|>], SymbolNode[Symbol, 
   "a", <|Source -> {{2, 2}, {2, 2}}|>], 
  TokenNode[Token`CloseCurly, 
   "}", <|Source -> {{2, 3}, {2, 3}}|>]}, <|Source -> {{1, 1}, {2, 
     3}}|>]
	,
	TestID->"Concrete-20190601-H6L1N7"
]






(*
line continuations and newlines
*)
Test[
	ConcreteParseString["\"abc\\\r\ndef\""]
	,
	StringNode[String, "\"abc\ndef\"", <|Source -> {{1, 1}, {2, 4}}|>]
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
		SymbolNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		TokenNode[Token`Tilde, "~", <|Source -> {{1, 3}, {1, 3}}|>],
		SymbolNode[Symbol, "f", <|Source -> {{1, 4}, {1, 4}}|>],
		TokenNode[Token`Symbol, "x", <|Source -> {{1, 6}, {1, 6}}|>] },

		<|Source -> {{1, 1}, {1, 6}}|>]
	,
	TestID->"Concrete-20190521-L2C2Y8"
]













