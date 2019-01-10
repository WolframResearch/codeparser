
Needs["AST`"]


parseEquivalenceFunction[actual_, expectedIgnored_] :=
Module[{good},
	good = SameQ[
		ToExpression[ToFullFormString[ParseString[actual, HoldNode[Hold, {##}, <||>]&]], InputForm]
		,
		ToExpression[actual, InputForm, Hold]
	];
	If[good,
		True
		,
		False
	]
]


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

Implicit times

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







Test[
	"\\[Integral] a \\[DifferentialD] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-W8E4P4"
]









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
	ToInputFormString[ConcreteParseString["A B:C:.Ne"]]
	,
	"A B:C:.Ne"
	,
	TestID->"Parse-20181117-K1W0K0"
]


Test[
	ToInputFormString[ConcreteParseString["a:"]]
	,
	"a:"
	,
	TestID->"Parse-20181118-V5G8O1"
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

Test[
	cst
	,
	FileNode[File, {SlotNode[
   "#something", {}, <|Source -> {{1, 1}, {1, 
       10}}|>]}, <|Source -> {{1, 1}, {1, 10}}, 
  SyntaxIssues -> {SyntaxIssue["Shebang", 
     "# on first line looks like #! shebang", 
     "Remark", <|Source -> {{1, 1}, {1, 1}}|>]}|>]
	,
	TestID->"Parse-20181230-M7H7Q7"
]










