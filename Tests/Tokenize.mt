
Needs["AST`"]

Test[
	TokenizeString["(* \\.28\\.2a *)"]
	,
	{Token[Token`Comment, 
  "(* \\.28\\.2a *)", <|Source -> {{1, 1}, {1, 14}}|>], 
 Token[Token`EOF, "", <|Source -> {{2, 0}, {2, 0}}|>]}
	,
	TestID->"Tokenize-20181208-O3D5M5"
]



