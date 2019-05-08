
(*
UnhandledCharacter:
*)

Test[
	TokenizeString["\\[SkeletonIndicator]"]
	,
	{TokenNode[Token`Error`UnhandledCharacter, 
  "\\[SkeletonIndicator]", <|Source -> {{1, 1}, {1, 20}}|>]}
	,
	TestID->"TokenErrors-20190520-B1H0A6"
]

Test[
	TokenizeString["a::\\\""]
	,
	{TokenNode[Token`Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], 
 TokenNode[Token`ColonColon, "::", <|Source -> {{1, 2}, {1, 3}}|>], 
 TokenNode[Token`Error`UnhandledCharacter, 
  "\\\"", <|Source -> {{1, 4}, {1, 5}}|>], 
 TokenNode[Token`Error`EmptyString, 
  "", <|Source -> {{2, 0}, {2, 0}}|>]}
	,
	TestID->"TokenErrors-20190520-L5N7B0"
]



(*
UnterminatedComment:
*)

Test[
	TokenizeString["(*"]
	,
	{TokenNode[Token`Error`UnterminatedComment, 
  "(*", <|Source -> {{1, 1}, {1, 2}}|>]}
	,
	TestID->"TokenErrors-20190520-C8W1P2"
]




(*
ExpectedAlphaOrDollar:
*)


Test[
	TokenizeString["aaa`1"]
	,
	{TokenNode[Token`Error`ExpectedLetterlike, 
  "aaa`", <|Source -> {{1, 1}, {1, 4}}|>], 
 TokenNode[Token`Integer, "1", <|Source -> {{1, 5}, {1, 5}}|>]}
	,
	TestID->"TokenErrors-20190520-H9P0H9"
]




(*
EmptyString:
*)

Test[
	TokenizeString["a::"]
	,
	{TokenNode[Token`Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], TokenNode[Token`ColonColon, "::",
		<|Source -> {{1, 2}, {1, 3}}|>], TokenNode[Token`Error`EmptyString, "", <|Source -> {{2, 0}, {2, 0}}|>]}
	,
	TestID->"TokenErrors-20190520-R2P3A3"
]

Test[
	TokenizeString["a>>"]
	,
	{TokenNode[Token`Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
		TokenNode[Token`GreaterGreater, ">>", <|Source -> {{1, 2}, {1, 3}}|>],
		TokenNode[Token`Error`EmptyString, "", <|Source -> {{2, 0}, {2, 0}}|>]}
	,
	TestID->"TokenErrors-20190520-M3H7E9"
]


(*
UnterminatedString:
*)

Test[
	TokenizeString["\""]
	,
	{TokenNode[Token`Error`UnterminatedString, "\"", <|Source -> {{1, 1}, {1, 1}}|>]}
	,
	TestID->"TokenErrors-20190520-L6N6S8"
]









(*
InvalidBase:
*)

Test[
	TokenizeString["37^^2"]
	,
	{TokenNode[Token`Error`InvalidBase, "37", <|Source -> {{1, 1}, {1, 2}}|>],
		TokenNode[Token`Caret, "^", <|Source -> {{1, 3}, {1, 3}}|>],
		TokenNode[Token`Caret, "^", <|Source -> {{1, 4}, {1, 4}}|>],
		TokenNode[Token`Integer, "2", <|Source -> {{1, 5}, {1, 5}}|>]}
	,
	TestID->"TokenErrors-20190520-Q9B9R6"
]








(*
ExpectedDigitOrAlpha:
*)

Test[
	TokenizeString["2^^3"]
	,
	{TokenNode[Token`Error`ExpectedDigitOrAlpha, "2^^", <|Source -> {{1, 1}, {1, 3}}|>],
		TokenNode[Token`Integer, "3", <|Source -> {{1, 4}, {1, 4}}|>]}
	,
	TestID->"TokenErrors-20190520-B7G4V4"
]


Test[
	TokenizeString["2^^@"]
	,
	{TokenNode[Token`Error`ExpectedDigitOrAlpha, "2^^", <|Source -> {{1, 1}, {1, 3}}|>],
		TokenNode[Token`At, "@", <|Source -> {{1, 4}, {1, 4}}|>]}
	,
	TestID->"TokenErrors-20190520-J3Q2S7"
]




(*
ExpectedAccuracy:
*)

Test[
	TokenizeString["1.2``->3"]
	,
	{TokenNode[Token`Error`ExpectedAccuracy, "1.2``-", <|Source -> {{1, 1}, {1, 6}}|>], TokenNode[Token`Greater, ">",
		<|Source -> {{1, 7}, {1, 7}}|>], TokenNode[Token`Integer, "3", <|Source -> {{1, 8}, {1, 8}}|>]}
	,
	TestID->"TokenErrors-20190520-B2J9I4"
]








(*
ExpectedExponent:
*)

Test[
	TokenizeString["123*^"]
	,
	{TokenNode[Token`Error`ExpectedExponent, "123*^", <|Source -> {{1, 1}, {1, 5}}|>]}
	,
	TestID->"TokenErrors-20190520-L1J8C1"
]





(*
ExpectedEqual:
*)

Test[
	TokenizeString["a ^: f"]
	,
	{TokenNode[Token`Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>], TokenNode[Token`WhiteSpace, " ",
		<|Source -> {{1, 2}, {1, 2}}|>], TokenNode[Token`Error`ExpectedEqual, "^:", <|Source -> {{1, 3}, {1, 4}}|>],
		TokenNode[Token`WhiteSpace, " ", <|Source -> {{1, 5}, {1, 5}}|>], TokenNode[Token`Symbol, "f", <|Source -> {{1, 6}, {1, 6}}|>]}
	,
	TestID->"TokenErrors-20190520-M3N7T5"
]







