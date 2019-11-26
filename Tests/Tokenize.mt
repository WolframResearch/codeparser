
Needs["AST`"]


(*
Comments
*)
Test[
	TokenizeString["(* \\.28\\.2a *)"]
	,
	{LeafNode[Token`Comment, "(* \\.28\\.2a *)", <|Source -> {{1, 1}, {1, 15}}|>]}
	,
	TestID->"Tokenize-20181208-O3D5M5"
]


(*
Number Errors
*)
Test[
	TokenizeString["1.2``->3"]
	,
	{LeafNode[Token`Error`ExpectedAccuracy, "1.2``->", <|Source -> {{1, 1}, {1, 8}}|>], 
	LeafNode[Integer, "3", <|Source -> {{1, 8}, {1, 9}}|>]}
	,
	TestID->"Tokenize-20181215-Z0H7Y5"
]


(*
String Errors
*)
Test[
	TokenizeString["\"123\\\""]
	,
	{LeafNode[Token`Error`UnterminatedString, "\"123\\\"", <|Source -> {{1, 1}, {1, 7}}|>]}
	,
	TestID->"Tokenize-20190406-A1G3U8"
]





