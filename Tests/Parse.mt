
Needs["AST`"]

Test[
	ParseString["1+1"]
	,
	InfixNode[Plus, {NumberNode["1", {}, <|Source -> {{1, 1}, {1, 1}}|>],
		NumberNode["1", {}, <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]
	,
	TestID->"Parse-20181207-M8H7A4"
]



(*
<-> and \[TwoWayRule]
*)

BeginTestSection["TwoWayRule", $VersionNumber >= 11.1]

Test[
	ParseString["a <-> b"]
	,
	BinaryNode[TwoWayRule, {
		SymbolNode["a", {}, <|Source -> {{1, 1}, {1, 1}}|>],
		SymbolNode["b", {}, <|Source -> {{1, 7}, {1, 7}}|>]},
		<|Source -> {{1, 1}, {1,7}}|>]
	,
	TestID->"Parse-20181110-N5N6R3"
]

Test[
	ParseString["a \\[TwoWayRule] b"]
	,
	BinaryNode[TwoWayRule, {
		SymbolNode["a", {}, <|Source -> {{1, 1}, {1, 1}}|>],
		SymbolNode["b", {}, <|Source -> {{1, 17}, {1, 17}}|>]},
		<|Source -> {{1, 1}, {1,17}}|>]
	,
	TestID->"Parse-20181110-I6S2W8"
]

EndTestSection[]





Test[
	ParseString["\\[Integral] a \\[DifferentialD] x"]
	,
	PrefixNode[Integral, {InfixNode[InfixImplicitTimes, {
		SymbolNode["a", {}, <|Source->{{1, 13}, {1, 13}}|>], 
		PrefixNode[DifferentialD, {SymbolNode["x", {}, <|Source->{{1, 32}, {1, 32}}|>]}, 
			<|Source->{{1, 15}, {1, 32}}|>]}, <|Source->{{1, 13}, {1, 32}}|>]}, <|Source->{{1, 1}, {1, 32}}|>]
	,
	TestID->"Parse-20181202-W8E4P4"
]









(*
\r and \[RawReturn]
*)
Test[
	ToInputFormString[ParseString["\"\\r\""]]
	,
	"\"\\r\""
	,
	TestID->"Parse-20181115-M4K2F9"
]

Test[
	ToInputFormString[ParseString["\"\\[RawReturn]\""]]
	,
	"\"\\[RawReturn]\""
	,
	TestID->"Parse-20181115-A3F2Z1"
]


(*
Testing \space
*)
Test[
	ParseString["\\(\\ \\)"]
	,
	GroupNode[GroupLinearSyntaxParen, {InternalTokenNode["\\ ", {}, <|Source->{{1, 3}, {1, 4}}|>]}, <|Source->{{1, 1}, {1, 6}}|>]
	,
	TestID->"Parse-20181117-G1Q5J5"
]

Test[
	ParseString["\\(x\\ y\\)"]
	,
	GroupNode[GroupLinearSyntaxParen, {InternalTokenNode["x", {}, <|Source->{{1, 3}, {1, 3}}|>], 
		InternalTokenNode["\\ ", {}, <|Source->{{1, 4}, {1, 5}}|>], InternalTokenNode["y", {}, 
			<|Source->{{1, 6}, {1, 6}}|>]}, <|Source->{{1, 1}, {1, 8}}|>]
	,
	TestID->"Parse-20181202-N2P9N0"
]

Test[
	ParseString["\\(2\\ 3\\)"]
	,
	GroupNode[GroupLinearSyntaxParen, {InternalTokenNode["2", {}, <|Source->{{1, 3}, {1, 3}}|>], 
		InternalTokenNode["\\ ", {}, <|Source->{{1, 4}, {1, 5}}|>], InternalTokenNode["3", {}, 
			<|Source->{{1, 6}, {1, 6}}|>]}, <|Source->{{1, 1}, {1, 8}}|>]
	,
	TestID->"Parse-20181202-L5J7A2"
]








(*
Multi-byte characters
*)

(*
check if bug 360669 is fixed
*)

res = RunProcess[{"echo", "\[Alpha]"}]
bug360669Fixed = (res["StandardOutput"] === "\[Alpha]")

BeginTestSection["Multi-byte Characters", bug360669Fixed]

Test[
	ParseString["\"‐\""]
	,
	StringNode["\"\\[Hyphen]\"", {}, <|Source->{{1, 1}, {1, 3}}|>]
	,
	TestID->"Parse-20181202-G1K6S8"
]

EndTestSection[]



(*
Malformed \[] characters
Unrecognized \[] characters
*)

ast = ParseString["\"\\[.*\\]\""]

s = ast[[1]]
children = ast[[2]]
data = ast[[3]]
issues = data[SyntaxIssues]

Test[
	Head[ast]
	,
	StringNode
	,
	TestID->"Parse-20181207-O9W0O1"
]

Test[
	s
	,
	"\"\\[.*\\]\""
	,
	TestID->"Parse-20181207-X9G8E1"
]

Test[
	children
	,
	{}
	,
	TestID->"Parse-20181207-Y7P1V8"
]

Test[
	Length[issues]
	,
	2
	,
	TestID->"Parse-20181202-E8N4Z4"
]



(*
Error handling
*)
Test[
	ParseString["A B:C:.Ne"]
	,
	InfixNode[Dot, {SyntaxErrorNode[
   Token`Error`ExpectedSymbolOrPattern, {InfixNode[
     InfixImplicitTimes, {SymbolNode[
       "A", {}, <|Source -> {{1, 1}, {1, 1}}|>], 
      BinaryNode[
       Pattern, {SymbolNode["B", {}, <|Source -> {{1, 3}, {1, 3}}|>], 
        SymbolNode["C", {}, <|Source -> {{1, 5}, {1, 5}}|>]}, <|
        Source -> {{1, 3}, {1, 5}}|>]}, <|
      Source -> {{1, 1}, {1, 5}}|>], InternalTokenNode[":", {}, <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>],
   SymbolNode["Ne", {}, <|Source -> {{1, 8}, {1, 9}}|>]}, <|
  Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Parse-20181117-K1W0K0"
]


Test[
	ParseString["a:"]
	,
	BinaryNode[Pattern, {SymbolNode["a", {}, <|Source->{{1, 1}, {1, 1}}|>],
		SyntaxErrorNode[Token`EOF, {
			InternalTokenNode["", {}, <|Source->{{2, 0}, {2, 0}}|>]
			}, <|Source->{{2, 0}, {2, 0}}|>]},
		<|Source->{{1, 1}, {2, 0}}|>]
	,
	TestID->"Parse-20181118-V5G8O1"
]
