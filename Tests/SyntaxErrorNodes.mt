
(*
ExpectedOperand:
*)

Test[
	ParseString["{ + }"]
	,
	CallNode[LeafNode[Symbol, "List", <||>], {
		CallNode[LeafNode[Symbol, "Plus", <||>], {
    		AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedOperand, "", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 3}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-C1B3O0"
]


(*
NonAssociative:
*)

Test[
	ParseString["a ? b ? c"]
	,
AbstractSyntaxErrorNode[AbstractSyntaxError`NonAssociative, {BinaryNode[
   PatternTest, {LeafNode[Symbol, 
     "a", <|Source -> {{1, 1}, {1, 1}}|>], 
    LeafNode[Token`Question, "?", <|Source -> {{1, 3}, {1, 3}}|>], 
    LeafNode[Symbol, 
     "b", <|Source -> {{1, 5}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 
       5}}|>], LeafNode[Token`Question, 
   "?", <|Source -> {{1, 7}, {1, 7}}|>], 
  LeafNode[Symbol, 
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
SyntaxErrorNode[SyntaxError`ExpectedTilde, {LeafNode[Symbol, 
   "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  LeafNode[Token`Tilde, "~", <|Source -> {{1, 3}, {1, 3}}|>], 
  LeafNode[Symbol, "f", <|Source -> {{1, 4}, {1, 4}}|>], 
  LeafNode[EndOfFile, 
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
SyntaxErrorNode[SyntaxError`ColonError, {LeafNode[Integer, 
   "1", <|Source -> {{1, 1}, {1, 1}}|>], 
  LeafNode[Token`Colon, ":", <|Source -> {{1, 2}, {1, 2}}|>], 
  LeafNode[Integer, 
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
SyntaxErrorNode[SyntaxError`ExpectedSet, {LeafNode[Symbol, 
   "a", <|Source -> {{1, 1}, {1, 1}}|>], 
  LeafNode[Token`SlashColon, "/:", <|Source -> {{1, 3}, {1, 4}}|>], 
  InfixNode[
   Times, {LeafNode[Symbol, "b", <|Source -> {{1, 6}, {1, 6}}|>], 
    LeafNode[Token`Star, "*", <|Source -> {{1, 8}, {1, 8}}|>], 
    LeafNode[Symbol, 
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
	CallNode[LeafNode[Symbol, "Function", <||>], {
		AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedOperand, "", <|Source -> {{1, 1}, {1, 1}}|>]}, <|Source -> {{1, 1}, {1, 1}}|>]
	,
	TestID->"SyntaxErrorNodes-20190521-O5D4A9"
]






(*
SyntaxError:
*)

TestMatch[
	ConcreteParseString["\\"]
	,
	SyntaxErrorNode[SyntaxError`UnhandledCharacter, {LeafNode[
   Token`Error`UnhandledCharacter, 
   "\\", <|Source -> {{1, 1}, {2, 0}}|>]}, <|Source -> {{1, 1}, {2, 
     0}}|>]
	,
	TestID->"SyntaxIssues-20190521-P7R3O7"
]


