
Needs["AST`"]
Needs["AST`Utils`"]


parseEquivalenceFunction[actualIn_, expectedIgnored_] :=
Catch[
Module[{parsed, good, expected, actual},
	parsed = ParseString[actualIn, HoldNode[Hold, #, <||>]&];
	If[FailureQ[parsed],
		Throw[parsed]
	];
	expected = ToExpression[ToFullFormString[parsed], InputForm];
	actual = ToExpression[actualIn, InputForm, Hold];
	good = SameQ[expected, actual];
	If[good,
		True
		,
		unhandled[{actual, expected}]
	]
]]


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

















(*
\r and \[RawReturn]
*)
Test[
	"\"\\r\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181115-M4K2F9"
]

Test[
	"\"\\[RawReturn]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181115-A3F2Z1"
]

Test[
	"\"\\:000d\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-A6E4K4"
]



(*
\[RawDoubleQuote]
*)
Test[
	"\"\\[RawDoubleQuote]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-S9D1H2"
]

Test[
	"\"\\:0022\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-O0I4X0"
]



(*
\[RawBackslash]
*)
Test[
	"\"\\[RawBackslash]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-T0Y0O1"
]

Test[
	"\"\\:005c\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190126-F7Z5P8"
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
	(* the hyphen character below is multiple bytes *)
	"\"‐\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-G1K6S8"
]

EndTestSection[]






Test[
	"\"\\.00\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190128-I9O3D9"
]


Test[
	"\"\\|010023\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190129-O8S8M2"
]









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
	ParseString["\\n23"]
	,
	_SyntaxErrorNode
	,
	TestID->"Parse-20190126-Q9U0H8"
]

TestMatch[
	ParseString["\\t23"]
	,
	_SyntaxErrorNode
	,
	TestID->"Parse-20190203-F5C9L1"
]

(*
important that space after - is not in SyntaxErrorNode
*)
Test[
	ConcreteParseString["a - \\tb"]
	,
	InfixNode[Plus, {SymbolNode["a", {}, <|Source->{{1, 1}, {1, 1}}|>],
		InternalMinusNode[Minus, {SyntaxErrorNode[Token`Error`Rest, {SyntaxErrorNode[Token`Error`UnhandledCharacter, {
			InternalTokenNode["\\t", {}, <|Source->{{1, 5}, {1, 6}}|>]}, <|Source->{{1, 5}, {1, 6}}|>],
			InternalTokenNode["b", {}, <|Source->{{1, 7}, {1, 7}}|>]},
			<|Source->{{1, 5}, {1, 7}}|>]}, <|Source->{{1, 5}, {1, 7}}|>]}, <|Source->{{1, 1}, {1, 7}}|>]
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

Parse File

*)

sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "sample.wl"}]

cst = ConcreteParseFile[sample]

Test[
	cst
	,
	FileNode[File, {InfixNode[
   Plus, {NumberNode["1", {}, <|Source -> {{2, 1}, {2, 1}}|>], 
    NumberNode[
     "1", {}, <|Source -> {{2, 3}, {2, 3}}|>]}, <|Source -> {{2, 
       1}, {2, 3}}|>]}, <|Source -> {{2, 1}, {2, 3}}|>]
	,
	TestID->"Parse-20181230-J0G3I8"
]


shebangwarning = FileNameJoin[{DirectoryName[$CurrentTestSource], "shebangwarning.wl"}]

cst = ConcreteParseFile[shebangwarning]

TestMatch[
	cst
	,
	FileNode[File, {SlotNode["#something", {}, <|Source -> {{1, 1}, {1, 10}}|>]},
		KeyValuePattern[{
			Source -> {{1, 1}, {1, 10}},
			SyntaxIssues -> {SyntaxIssue["Shebang", "# on first line looks like #! shebang", "Remark", <|Source -> {{1, 1}, {1, 1}}|>]} }] ]
	,
	TestID->"Parse-20181230-M7H7Q7"
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








