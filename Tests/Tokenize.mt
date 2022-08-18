
Needs["CodeParser`"]


(*
Comments
*)
Test[
	CodeTokenize["(* \\.28\\.2a *)"]
	,
	{LeafNode[Token`Comment, "(* \\.28\\.2a *)", <|Source -> {{1, 1}, {1, 15}}|>]}
	,
	TestID->"Tokenize-20181208-O3D5M5"
]


(*
Number Errors
*)
Test[
	CodeTokenize["1.2``->3"]
	,
	{
		ErrorNode[Token`Error`Number, "1.2``-", <|Source -> {{1, 1}, {1, 7}}|>],
		LeafNode[Token`Greater, ">", <|Source -> {{1, 7}, {1, 8}}|>],
		LeafNode[Integer, "3", <|Source -> {{1, 8}, {1, 9}}|>]}
	,
	TestID->"Tokenize-20181215-Z0H7Y5"
]


(*
String Errors
*)
Test[
	CodeTokenize["\"123\\\""]
	,
	{ErrorNode[Token`Error`UnterminatedString, "\"123\\\"", <|Source -> {{1, 1}, {1, 7}}|>]}
	,
	TestID->"Tokenize-20190406-A1G3U8"
]


Test[
	CodeTokenize["*)"]
	,
	{ErrorNode[Token`Error`UnexpectedCommentCloser, "*)", <|Source -> {{1, 1}, {1, 3}}|>]}
	,
	TestID->"Tokenize-20220709-J1V7W8"
]





