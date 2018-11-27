
Needs["AST`"]


(*
<-> and \[TwoWayRule]
*)

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
	"\"\\r\""
	,
	TestID->"Parse-20181115-A3F2Z1"
]


(*
Testing \space

\space is parsed as space, akin to \t being parsed as tab
*)
Test[
	ParseString["\\(\\ \\)"]
	,
	GroupNode[GroupLinearSyntaxParen, {InternalTokenNode["\\ ", {}, <|Source->{{1, 3}, {1, 4}}|>]}, <|Source->{{1, 1}, {1, 6}}|>]
	,
	TestID->"Parse-20181117-G1Q5J5"
]




(*
Multi-byte characters
*)
Test[
	ParseString["\"‐\""]
	,
	StringNode["\"\\[Hyphen]\"", {}, <|Source->{{1, 1}, {1, 3}}|>]
]





(*
Error handling
*)
Test[
	ParseString["A B:C:.Ne"]
	,
	InfixNode[Dot, {SyntaxErrorNode[
   Error`EXPECTEDSYMBOLORPATTERN, {InfixNode[
     BinarySpaceTimes, {SymbolNode[
       "A", {}, <|Source -> {{1, 1}, {1, 1}}|>], 
      BinaryNode[
       Pattern, {SymbolNode["B", {}, <|Source -> {{1, 3}, {1, 3}}|>], 
        SymbolNode["C", {}, <|Source -> {{1, 5}, {1, 5}}|>]}, <|
        Source -> {{1, 3}, {1, 5}}|>]}, <|
      Source -> {{1, 1}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>],
   SymbolNode["Ne", {}, <|Source -> {{1, 8}, {1, 9}}|>]}, <|
  Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Parse-20181117-K1W0K0"
]


Test[
	ParseString["a:"]
	,
	BinaryNode[Pattern, {SymbolNode["a", {}, <|Source->{{1, 1}, {1, 1}}|>],
		SyntaxErrorNode[Error`UNEXPECTEDEOF, {
			InternalTokenNode["", {}, <|Source->{{2, 0}, {2, 0}}|>]
			}, <|Source->{{2, 0}, {2, 0}}|>]},
		<|Source->{{1, 1}, {2, 0}}|>]
	,
	TestID->"Parse-20181118-V5G8O1"
]
