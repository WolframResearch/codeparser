Print["\n===== Start TokenErrors.mt =====\n"]

Needs["CodeParser`"]

(*
UnhandledCharacter:
*)

Test[
	CodeTokenize["\\[SkeletonIndicator]"]
	,
	{ErrorNode[Token`Error`UnhandledCharacter, "\\[SkeletonIndicator]", <|Source -> {{1, 1}, {1, 21}}|>]}
	,
	TestID->"TokenErrors-20190520-B1H0A6"
]

Test[
	CodeTokenize["\\\""]
	,
	{ErrorNode[Token`Error`UnhandledCharacter, "\\\"", <|Source -> {{1, 1}, {1, 3}}|>]}
	,
	TestID->"TokenErrors-20190816-G5Q8B5"
]

Test[
	CodeTokenize["a::\\\""]
	,
	{
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`ColonColon, "::", <|Source -> {{1, 2}, {1, 4}}|>],
		ErrorNode[Token`Error`UnhandledCharacter, "\\\"", <|Source -> {{1, 4}, {1, 6}}|>] }
	,
	TestID->"TokenErrors-20190520-L5N7B0"
]



(*
UnterminatedComment:
*)

Test[
	CodeTokenize["(*"]
	,
	{ErrorNode[Token`Error`UnterminatedComment, "(*", <|Source -> {{1, 1}, {1, 3}}|>]}
	,
	TestID->"TokenErrors-20190520-C8W1P2"
]




(*
ExpectedAlphaOrDollar:
*)


Test[
	CodeTokenize["aaa`1"]
	,
	{
		ErrorNode[Token`Error`ExpectedLetterlike, "aaa`", <|Source -> {{1, 1}, {1, 5}}|>],
		LeafNode[Integer, "1", <|Source -> {{1, 5}, {1, 6}}|>]
	}
	,
	TestID->"TokenErrors-20190520-H9P0H9"
]




(*
EmptyString:
*)

Test[
	CodeTokenize["a::"]
	,
	{
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`ColonColon, "::", <|Source -> {{1, 2}, {1, 4}}|>] }
	,
	TestID->"TokenErrors-20190520-R2P3A3"
]

Test[
	CodeTokenize["a>>"]
	,
	{
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Token`GreaterGreater, ">>", <|Source -> {{1, 2}, {1, 4}}|>] }
	,
	TestID->"TokenErrors-20190520-M3H7E9"
]


(*
UnterminatedString:
*)

Test[
	CodeTokenize["\""]
	,
	{ErrorNode[Token`Error`UnterminatedString, "\"", <|Source -> {{1, 1}, {1, 2}}|>]}
	,
	TestID->"TokenErrors-20190520-L6N6S8"
]









(*
InvalidBase:
*)

Test[
	CodeTokenize["37^^2"]
	,
	{ErrorNode[Token`Error`Number, "37^^2", <|Source -> {{1, 1}, {1, 6}}|>]}
	,
	TestID->"TokenErrors-20190520-Q9B9R6"
]








(*
ExpectedDigitOrAlpha:
*)

Test[
	CodeTokenize["2^^3"]
	,
	{ErrorNode[Token`Error`Number, "2^^3", <|Source -> {{1, 1}, {1, 5}}|>]}
	,
	TestID->"TokenErrors-20190520-B7G4V4"
]


Test[
	CodeTokenize["2^^@"]
	,
	{
		ErrorNode[Token`Error`Number, "2^^", <|Source -> {{1, 1}, {1, 4}}|>],
		LeafNode[Token`At, "@", <|Source -> {{1, 4}, {1, 5}}|>]}
	,
	TestID->"TokenErrors-20190520-J3Q2S7"
]




(*
ExpectedAccuracy:
*)

Test[
	CodeTokenize["1.2``->3"]
	,
	{
		ErrorNode[Token`Error`Number, "1.2``-", <|Source -> {{1, 1}, {1, 7}}|>],
		LeafNode[Token`Greater, ">", <|Source -> {{1, 7}, {1, 8}}|>],
		LeafNode[Integer, "3", <|Source -> {{1, 8}, {1, 9}}|>]}
	,
	TestID->"TokenErrors-20190520-B2J9I4"
]








(*
ExpectedExponent:
*)

Test[
	CodeTokenize["123*^"]
	,
	{ErrorNode[Token`Error`Number, "123*^", <|Source -> {{1, 1}, {1, 6}}|>]}
	,
	TestID->"TokenErrors-20190520-L1J8C1"
]





(*
ExpectedEqual:
*)

Test[
	CodeTokenize["a ^: f"]
	,
	{
		LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
		LeafNode[Whitespace, " ", <|Source -> {{1, 2}, {1, 3}}|>],
		ErrorNode[Token`Error`ExpectedEqual, "^:", <|Source -> {{1, 3}, {1, 5}}|>],
		LeafNode[Whitespace, " ", <|Source -> {{1, 5}, {1, 6}}|>],
		LeafNode[Symbol, "f", <|Source -> {{1, 6}, {1, 7}}|>] }
	,
	TestID->"TokenErrors-20190520-M3N7T5"
]







