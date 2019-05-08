
(*
ExpectedOperand:
*)

Test[
	ParseString["{ + }"]
	,
CallNode[SymbolNode[Symbol, 
  "List", <||>], {CallNode[
   SymbolNode[Symbol, 
    "Plus", <||>], {SyntaxErrorNode[
     SyntaxError`ExpectedOperand, {TokenNode[Token`CloseCurly, 
       "}", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 5}, {1,
          5}}|>]}, <|Source -> {{1, 3}, {1, 5}}|>]}, <|Source -> {{1, 
     1}, {1, 5}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-C1B3O0"
]


(*
NonAssociative:
*)

Test[
	ParseString["a ? b ? c"]
	,
SyntaxErrorNode[SyntaxError`NonAssociative, {BinaryNode[
   PatternTest, {SymbolNode[Symbol, 
     "a", <|Source -> {{1, 1}, {1, 1}}|>], 
    TokenNode[Token`Question, "?", <|Source -> {{1, 3}, {1, 3}}|>], 
    SymbolNode[Symbol, 
     "b", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 
       5}}|>], TokenNode[Token`Question, 
   "?", <|Source -> {{1, 7}, {1, 7}}|>], 
  SymbolNode[Symbol, 
   "c", <|Source -> {{1, 9}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 
     9}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-A6K4H1"
]




(*
ExpectedTilde:
*)

Test[
	ParseString["a ~f"]
	,
SyntaxErrorNode[SyntaxError`ExpectedTilde, {SymbolNode[Symbol, 
   "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  TokenNode[Token`Tilde, "~", <|Source -> {{1, 3}, {1, 3}}|>], 
  SymbolNode[Symbol, "f", <|Source -> {{1, 4}, {1, 4}}|>], 
  TokenNode[Token`EndOfFile, 
   "", <|Source -> {{2, 0}, {2, 0}}|>]}, <|Source -> {{1, 1}, {2, 
     0}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-T2R4L9"
]





(*
ExpectedSymbol:
*)

Test[
	ParseString["1:2"]
	,
SyntaxErrorNode[SyntaxError`ExpectedSymbol, {IntegerNode[Integer, 
   "1", <|Source -> {{1, 1}, {1, 1}}|>], 
  TokenNode[Token`Colon, ":", <|Source -> {{1, 2}, {1, 2}}|>], 
  IntegerNode[Integer, 
   "2", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 
     3}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-Z6D6T1"
]







(*
ExpectedSet:
*)

Test[
	ParseString["a /: b * c"]
	,
SyntaxErrorNode[SyntaxError`ExpectedSet, {SymbolNode[Symbol, 
   "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  TokenNode[Token`SlashColon, "/:", <|Source -> {{1, 3}, {1, 4}}|>], 
  InfixNode[
   Times, {SymbolNode[Symbol, "b", <|Source -> {{1, 6}, {1, 6}}|>], 
    TokenNode[Token`Star, "*", <|Source -> {{1, 8}, {1, 8}}|>], 
    SymbolNode[Symbol, 
     "c", <|Source -> {{1, 10}, {1, 10}}|>]}, <|Source -> {{1, 6}, {1,
        10}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-D9G5L2"
]





(*
ExpectedPossibleExpression:
*)

Test[
	ParseString["&"]
	,
	SyntaxErrorNode[SyntaxError`ExpectedPossibleExpression, {
		TokenNode[Token`Amp, "&", <|Source -> {{1, 1}, {1, 1}}|>]}, <|Source -> {{1, 1}, {1, 1}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-O5D4A9"
]





