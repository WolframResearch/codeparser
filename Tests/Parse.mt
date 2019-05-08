
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "ASTTestUtils"}]
PrependTo[$Path, path]

Needs["ASTTestUtils`"]

Needs["AST`"]
Needs["AST`Utils`"]



Test[
	"1+1"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181207-M8H7A4"
]

Test[
	"{f[];}"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181219-H6P4J2"
]

Test[
	"{1\\[InvisibleComma]2}"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181219-I1H5B6"
]

Test[
	ParseString[""]
	,
	Null
	,
	TestID->"Parse-20190227-B2B5G4"
]








(*

Numbers

*)

Test[
	"12^^a.a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181231-H5K6L1"
]

Test[
	"9.8` + 3"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-O4R3J1"
]

Test[
	"9.8` - 3.0`*^-6"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-D3H9G3"
]

Test[
	"0.006687037864392394` - 0.004767194980375145`"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190524-E0W7U3"
]











(*
Slot
*)

Test[
	"#1Mod[a,b]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-Y2Q4X3"
]

Test[
	"#\"a\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190406-P2Q9C9"
]

Test[
	"#a`b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190406-C1E0T4"
]







(*
Strings
*)
Test[
	"\"a\rb\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190605-Q2J1C4"
]

Test[
	"\"a\\rb\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190605-R5J1A5"
]

Test[
	"\"a\r\nb\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190605-Y3N4N7"
]

Test[
	"\"a\\r\\nb\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190605-J8F5L9"
]

Test[
	"\"\r\n123\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190606-P6G5G5"
]










(*
Colon : parsing
*)

Test[
	"a:b_?(test)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-Y3U5P4"
]

Test[
	"a:b:c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-H8O7U7"
]

Test[
	"a~~b:c:d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-V9L9R6"
]

Test[
	"a|b:c:d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-U4P7W8"
]

Test[
	"a:b~~c:d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-M7V9H5"
]

Test[
	"a:b|c:d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-K6B8G5"
]

Test[
	"a:b:c~~d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-H1U7D1"
]

Test[
	"a:b:c|d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-W4F7O2"
]

Test[
	"a:{a1:b1:c1}:c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-S0T6G4"
]

Test[
	"id:_?validuuidQ:Automatic"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-E7Z5X8"
]

Test[
	"_:False"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-V5O5V8"
]

Test[
	"a:_:\"\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-S3A6T0"
]

(*
bug 79997
*)
Test[
	"f[n : _Integer?Positive : 1] := n"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-R6F7B2"
]

Test[
	"_a:b|c|d:e"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-L1E8H9"
]

Test[
	"a:b|c|d:e"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-S5O0X2"
]

Test[
	"a:b_c:d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190203-X5M4A5"
]












(*
Arithmetic
*)
Test[
	"a^-n"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-S2Q5B0"
]

Test[
	"a * b c \\[InvisibleTimes] d \\[Times] e / f"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190102-X7D3R8"
]







res = (ToExpression["a + b \\[ImplicitPlus] c", InputForm, Hold] === Hold[Plus[a, Plus[b, c]]])
bug365287Fixed = (res)

BeginTestSection["ImplicitPlus", bug365287Fixed]


Test[
	"a + b - c \\[ImplicitPlus] d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190102-J8R7W0"
]

EndTestSection[]




Test[
	"1-n!/n^n b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-R7G0F3"
]


Test[
	"+p1-p2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-N0K3T3"
]







(*
Derivative
*)

Test[
	"x[]'[z]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190117-X8Q1C1"
]






(*
Comments
*)

Test[
	"foo(**)[] := bar"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190223-C9F0S5"
]

Test[
	"a(**)_"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190304-I3H5Q1"
]

Test[
	"_(**)a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190304-H6E5I5"
]

Test[
	"1 (**) 2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190306-F5V4D4"
]

Test[
	"(**)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190306-P2S6H7"
]

Test[
	"(*\\a*)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190306-K8W8O1"
]












(*

Implicit times and span

*)

Test[
	";; ;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181231-N2J2Z3"
]

Test[
	";;;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-B0V5P7"
]

Test[
	";;a;;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-M8Y0B6"
]




(*

Implicit Times and symbols

*)

Test[
	"a_ b_"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190228-E0E7X8"
]




(*

LongName operators

*)
Test[
	"a \\[PermutationProduct] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190607-C3R7V3"
]







(*
Operators with both ASCII and LongNames
*)

Test[
	"a \\[And] b && c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-M6M5V0"
]

Test[
	"a || b \\[Or] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-I6C4F9"
]

(*
<-> and \[TwoWayRule]
*)

BeginTestSection["TwoWayRule", $VersionNumber >= 11.1]

Test[
	"a <-> b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181110-N5N6R3"
]

Test[
	"a \\[TwoWayRule] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181110-I6S2W8"
]



(*
bug 364202
<-> and \[TwoWayRule] had different precedences
found by AST
*)
Test[
	"a > b <-> c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-Y2S0T5"
]

Test[
	"a > b \\[TwoWayRule] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-C6X8T4"
]

EndTestSection[]












(* calls and ? *)

Test[
	"a[]?b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-G3G7J2"
]

Test[
	"a?b[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-Q0T4J6"
]

Test[
	"a_?b[c_]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190109-V1T3T3"
]





















(*

File operations

*)

Test[
	"Block[{ReinstallJava}, <<CalculateLoader`]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190301-V8I3O6"
]

Test[
	"a>>>c:\\p"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190301-R7R5J7"
]

Test[
	"a>>>c:\\n"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190301-N2X2B6"
]

Test[
	"a>>>c:\\b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190301-U8J6A5"
]

Test[
	"<< abc"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-T4J0U5"
]


Test[
	"a >>\n   b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190601-N7G5R3"
]





















(*
Linear Syntax
*)

(*
Testing \space
*)
Test[
	"\\(\\ \\)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181117-G1Q5J5"
]

Test[
	"\\(x\\ y\\)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-N2P9N0"
]

Test[
	"\\(2\\ 3\\)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-L5J7A2"
]

Test[
	"\\(x\\ny\\)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-K1B5U2"
]

Test[
	"\\(f := \n\\ng\\)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-A4A9T4"
]











(*
Stringification
*)
Test[
	"a>>C:\\progs\\hello"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190228-B1G1N2"
]





(*
non-ASCII characters in symbol
*)
Test[
	"System`\\[FormalK]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190605-R7J0W5"
]










(*
Strange characters in symbols
*)
Test[
	"a\:0000"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190305-E0A3A2"
]













(*
Multi-byte characters
*)

Test[
	(* the hyphen character below is multiple bytes *)
	"\"‐\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-G1K6S8"
]

Test[
	(* the copyright character below is multiple bytes *)
	"(* :Copyright: © 2016 by Wolfram Research, Inc. *)a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-G6W5C4"
]

Test[
	(* the alpha characters below are multiple bytes *)
	"αα"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-G7Y1W9"
]










(*
Malformed \[] characters
Unrecognized \[] characters
*)

ast = ParseString["\"\\[.*\\]\""]

s = ast[[2]]
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
	Length[issues]
	,
	2
	,
	TestID->"Parse-20181202-E8N4Z4"
]



(*
Parsing <newline>23 should be fine, but parsing \n23 should fail
*)

(*
Use "1\n23" here because "\n23" is parsed as a single expression by AST and as two expressions by the kernel
*)
Test[
	"1\n23"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-W2J2Q1"
]

TestMatch[
	ParseString["\\n23", HoldNode[Hold, #[[1]], <||>]&]
	,
	HoldNode[Hold, {_SyntaxErrorNode, _}, _]
	,
	TestID->"Parse-20190126-Q9U0H8"
]

TestMatch[
	ParseString["\\t23", HoldNode[Hold, #[[1]], <||>]&]
	,
	HoldNode[Hold, {_SyntaxErrorNode, _}, _]
	,
	TestID->"Parse-20190203-F5C9L1"
]

(*
important that space after - is not in SyntaxErrorNode
*)
Test[
	ConcreteParseString["a - \\tb"]
	,
	InfixNode[ImplicitTimes, {
		BinaryNode[Minus, {
			SymbolNode[Symbol, "a", <|Source -> {{1, 1}, {1, 1}}|>],
			TokenNode[Token`Minus, "-", <|Source -> {{1, 3}, {1, 3}}|>],
			SyntaxErrorNode[SyntaxError`UnhandledCharacter, {TokenNode[Token`Error`UnhandledCharacter, "\\t", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 5}, {1, 6}}|>] }, <|Source -> {{1, 1}, {1, 6}}|>], 
  		TokenNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 7}, {1, 7}}|>],
  		SymbolNode[Symbol, "b", <|Source -> {{1, 7}, {1, 7}}|>] }, <|Source -> {{1, 1}, {1, 7}}|>]
	,
	TestID->"Parse-20190203-G0U2N7"
]

TestMatch[
	ParseString["\\"]
	,
	_SyntaxErrorNode
	,
	TestID->"Parse-20190203-M3A0S4"
]




























(*

implementation details of parser

*)

Test[
	"1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-\
1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+\
2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-\
1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2-1+2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190117-S9J8V7"
]









(*
Proper abstracting
*)

Test[
	"a \\[CenterDot] b \\[CenterDot] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190117-A5S4V2"
]

Test[
	"a \\[Divides] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190117-R7B1K5"
]

Test[
	"a \\[Divides] b \\[Divides] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190117-G2E2V3"
]


Test[
	"a \\[SubsetEqual] b \\[SubsetEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190601-B8N2U4"
]

Test[
	"a \\[Equal] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190602-H3P8Z9"
]

Test[
	"#2 \\[DifferentialD]x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190603-H4B0W1"
]







(*
Backtracking in the parser
*)

Test[
	"2^\\[Pi]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-C9B6B8"
]

(*

Cannot test because ToExpression["1.2`-\\[Pi]"] can hang the kernel

bug 374238

Test[
	"1.2`-\\[Pi]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-Q7U4X7"
]
*)

Test[
	"2*\\[Pi]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-Z3Y8W3"
]

Test[
	"x=!\\[Pi]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-Q3O3B5"
]

Test[
	"a<-\\[Pi]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-W8F4E7"
]









(*
Ambiguities
*)


Test[
	"c_ . _LinearSolve"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-A3A3L5"
]

Test[
	"0. .."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-Q9D3W4"
]

Test[
	"0. ..."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-Z7J2D6"
]

Test[
	"- - 12.34"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-N6K0P5"
]

Test[
	"a& & b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-J5O3R9"
]

Test[
	"x ! ! y"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-O0C6C2"
]

Test[
	"x /. 0"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-I3F9G4"
]

Test[
	"x //. 0"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-B3Y1M2"
]

Test[
	"x =. 0"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-J9F1C4"
]


Test[
	"x ; ;"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-N9N1D6"
]





















