
Needs["AST`"]

Test[
	TokenizeString["(* \\.28\\.2a *)"]
	,
	{Token[Token`Comment, "(* \\.28\\.2a *)", <|Source -> {{1, 1}, {1, 14}}|>], 
	Token[Token`EOF, "", <|Source -> {{2, 0}, {2, 0}}|>]}
	,
	TestID->"Tokenize-20181208-O3D5M5"
]


Test[
	TokenizeString["1.2``->3"]
	,
	{Token[Token`Error`ExpectedAccuracy, "1.2``-", <|Source -> {{1, 1}, {1, 6}}|>], 
	Token[Token`Operator`Greater, ">", <|Source -> {{1, 7}, {1, 7}}|>], 
	Token[Token`Number, "3", <|Source -> {{1, 8}, {1, 8}}|>], 
	Token[Token`EOF, "", <|Source -> {{2, 0}, {2, 0}}|>]}
	,
	TestID->"Tokenize-20181215-Z0H7Y5"
]




(*

Tokenize File

*)

sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "sample.wl"}]

Test[
	TokenizeFile[sample]
	,
	{Token[Token`Newline, "", <|Source -> {{2, 0}, {2, 0}}|>], 
 Token[Token`Number, "1", <|Source -> {{2, 1}, {2, 1}}|>], 
 Token[Token`Operator`Plus, "+", <|Source -> {{2, 2}, {2, 2}}|>], 
 Token[Token`Number, "1", <|Source -> {{2, 3}, {2, 3}}|>], 
 Token[Token`Newline, "", <|Source -> {{3, 0}, {3, 0}}|>], 
 Token[Token`EOF, "", <|Source -> {{4, 0}, {4, 0}}|>]}
	,
	TestID->"Tokenize-20181230-Q3C4N0"
]

