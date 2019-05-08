
Needs["AST`"]


(*
Comments
*)
Test[
	TokenizeString["(* \\.28\\.2a *)"]
	,
	{TokenNode[Token`Comment, "(* \\.28\\.2a *)", <|Source -> {{1, 1}, {1, 14}}|>]}
	,
	TestID->"Tokenize-20181208-O3D5M5"
]


(*
Number Errors
*)
Test[
	TokenizeString["1.2``->3"]
	,
	{TokenNode[Token`Error`ExpectedAccuracy, "1.2``-", <|Source -> {{1, 1}, {1, 6}}|>], 
	TokenNode[Token`Greater, ">", <|Source -> {{1, 7}, {1, 7}}|>], 
	TokenNode[Token`Integer, "3", <|Source -> {{1, 8}, {1, 8}}|>]}
	,
	TestID->"Tokenize-20181215-Z0H7Y5"
]


(*
String Errors
*)
Test[
	TokenizeString["\"123\\\""]
	,
	{TokenNode[Token`Error`UnterminatedString, "\"123\\\"", <|Source -> {{1, 1}, {1, 6}}|>]}
	,
	TestID->"Tokenize-20190406-A1G3U8"
]





