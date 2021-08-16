
path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



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
	CodeParse[""]
	,
	ContainerNode[String, {}, <||>]
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

Test[
	"-9.5`15.9*^3"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190710-V0J1V4"
]

Test[
	"-1.2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190725-I1J8V9"
]

Test[
	"-(1.2)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190725-W8R5A7"
]

Test[
	"x * 2/3"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190725-C8B9K0"
]

Test[
	"x * (2/3)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190725-H4A5P4"
]

Test[
	"2/3 * x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190725-O1X7U5"
]

Test[
	"(2/3) * x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190725-T1G2R1"
]

Test[
	"-0"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190730-N8U2U7"
]

Test[
	"-(-0)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190730-J8H8W7"
]

Test[
	"-(-(-0))"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190730-A3Z6W9"
]

Test[
	"-(-(-(-0)))"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190730-M9S3U2"
]

Test[
	"16^^.FFFFFF*^32"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190820-O6O9O1"
]

Test[
	"16^^.FFFFFFFFFFFFF8*^256"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190820-S7J2H3"
]

Test[
	"-16^^.FFFFFF*^32"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190820-A2P3N2"
]

Test[
	"-16^^.FFFFFFFFFFFFF8*^256"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190820-G5K3X8"
]

Test[
	"2.Pi"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190821-G1U3U4"
]

Test[
	CodeParse["2.Pi"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Times", <||>], {
			LeafNode[Real, "2.", <|Source -> {{1, 1}, {1, 3}}|>],
			LeafNode[Symbol, "Pi", <|Source -> {{1, 3}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}, <||>]
	,
	TestID->"Parse-20200112-B6C6J0"
]

Test[
	"10^^2.Pi"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190821-P0C8P3"
]

Test[
	CodeParse["10^^2.Pi"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`Number, "10^^2.Pi", <|Source -> {{1, 1}, {1, 9}}|>]}, <||>]
	,
	TestID->"Parse-20200112-G6X1N0"
]

Test[
	"1.2`3.*^4"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190821-Y5A6B1"
]

Test[
	"123`.xxx"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190821-N5B9V0"
]

Test[
	"123``.xxx"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190821-N2R2A4"
]



Test[
	"1`.+2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191116-N2A8P4"
]


Test[
	"8`."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191116-T7X2G0"
]

Test[
	"8`+."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191116-O7H4T2"
]




res = Quiet[ToExpression["a . -b", InputForm, Hold] =!= $Failed, {ToExpression::sntx}]
bug382766Fixed = (res)

BeginTestSection["DotNegative", bug382766Fixed]

Test[
	"a . -b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191116-Q8V9U8"
]

EndTestSection[]






Test[
	"002^^111"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191116-T5D2J8"
]

Test[
	"1`+.a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191116-D2C6O4"
]

Test[
	"1`.a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID -> "Parse-20191119-Q8Y9U4"
]

Test[
	"2^^."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200112-K5W6R9"
]



Test[
  "3`"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-X5V6V9"
]

Test[
  "1.25`"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-O9F0P4"
]

Test[
  "1.1`"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-C2Y2F2"
]

Test[
  "1`*^12"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-W6J4A1"
]

Test[
  "3`7.7124"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-S7W5O1"
]

Test[
  "1`5.6*^-5"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-A5E4J4"
]

Test[
  "0``23"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-L0H6H7"
]

Test[
  "3``7.7124"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-V0L6X7"
]

Test[
  "1``5.6*^-5"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-X1E9G1"
]

Test[
  "2^^101.010111`*^5"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-B5O2C4"
]

Test[
  "2^^101.010111`17*^5"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-D7D2L2"
]

Test[
  "2^^101.010111``17*^5"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200304-S0E5B8"
]

Test[
	"-1*^-2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-E3B7A4"
]

Test[
	"-(-1*^-2)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-X4F5X3"
]

Test[
	"-(-(-1*^-2))"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-M6K5B2"
]

Test[
	"-(-(-(-1*^-2)))"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-T1U9Z2"
]

Test[
	"1*^2.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-O3A7Q1"
]

Test[
	"1*^2..."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-E5L2K7"
]

Test[
	"2^^.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200404-S6W5R8"
]

Test[
	"6`5.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200531-W0B6N6"
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

Test[
	"-a/2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190610-L2I5I1"
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
found by CodeParser
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

Test[
	"\\(x \\[VeryThinSpace]\\)"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190730-Z1U3H0"
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

Make sure to escape the NUL character

"a\:0000" will cause... problems
*)
Test[
	"a\\:0000"
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
	"\"\[Hyphen]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20181202-G1K6S8"
]

Test[
	(* the copyright character below is multiple bytes *)
	"(* :Copyright: \[Copyright] 2016 by Wolfram Research, Inc. *)a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-G6W5C4"
]

Test[
	(* the alpha characters below are multiple bytes *)
	"\[Alpha]\[Alpha]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190529-G7Y1W9"
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









Test[
	"a;[]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190621-X8J2Q1"
]


Test[
	"a; &"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190623-X6G3Z0"
]


Test[
	"a * b^2  c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200603-O9O2V2"
]


Test[
	"-1/2a^2 b^2"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190626-U7U1D7"
]


Test[
	"a \\[Divide] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-C1U2O5"
]

Test[
	"\\[OpenCurlyQuote] a \\[CloseCurlyQuote]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-A9S6V8"
]

Test[
	"\\[OpenCurlyDoubleQuote] a \\[CloseCurlyDoubleQuote]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-M6T3M5"
]





Test[
	"\\[ForAll] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-O9F8D9"
]

Test[
	"\\[Exists] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-W2S5W4"
]

Test[
	"\\[NotExists] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-X5D6D4"
]

Test[
	"\\[Del] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-Z1G6N5"
]

Test[
	"a \\[ReverseElement] b \\[ReverseElement] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-I1S8K4"
]

Test[
	"a \\[NotReverseElement] b \\[NotReverseElement] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-W8G5M5"
]

Test[
	"a \\[SuchThat] b \\[SuchThat] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-R8X2V0"
]

Test[
	"\\[Product] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-V7N7L6"
]

Test[
	"\\[Coproduct] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-Q5S8M5"
]

Test[
	"\\[Sum] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-O8L6N1"
]

Test[
	"\\[Minus] x"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-F0N6F2"
]

Test[
	"a \\[MinusPlus] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-M6J6N7"
]

Test[
	"a \\[DivisionSlash] b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190629-K1S2D3"
]

Test[
	"a \\[Because] b \\[Because] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-O8X8R0"
]

Test[
	"a \\[LeftTee] b \\[LeftTee] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-P3M1M6"
]

Test[
	"a \\[RightTee] b \\[RightTee] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-L3H8M8"
]

Test[
	"a \\[LessFullEqual] b \\[LessFullEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-C6K9G3"
]

Test[
	"a \\[NestedLessLess] b \\[NestedLessLess] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-K7P3H4"
]

Test[
	"a \\[NotLess] b \\[NotLess] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-Z5X8X2"
]

Test[
	"a \\[NotLessLess] b \\[NotLessLess] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-L4T5U5"
]

Test[
	"\\[ContinuedFractionK] a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-I2H4Z7"
]

Test[
	"a \\[TensorProduct] b \\[TensorProduct] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-O8O6I6"
]

Test[
	"\\[Coproduct] a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-N5B6V7"
]

Test[
	"a \\[Coproduct] b \\[Coproduct] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-A9X2W7"
]

Test[
	"a \\[Therefore] b \\[Therefore] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-C3S4J2"
]

Test[
	"a \\[SuchThat] b \\[SuchThat] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-A0O1Q4"
]

Test[
	"a \\[Implies] b \\[Implies] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190703-E8L9K8"
]






(*

Uncomment when ?a is handled

Test[
	"?LogicalExpand"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-T7P2K7"
]
*)

(*

Uncomment when ?a is handled

Test[
	"??LogicalExpand"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-F0Y3D1"
]
*)






Test[
	"a \\[PrecedesSlantEqual] b \\[PrecedesSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-A7N7G5"
]

Test[
	"a \\[SucceedsSlantEqual] b \\[SucceedsSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-U1T9F6"
]

Test[
	"a \\[LessSlantEqual] b \\[LessSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-A1K9Z6"
]

Test[
	"a \\[GreaterSlantEqual] b \\[GreaterSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-M0M7G1"
]

Test[
	"a \\[NotPrecedesSlantEqual] b \\[NotPrecedesSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-F0X1T3"
]

Test[
	"a \\[NotSucceedsSlantEqual] b \\[NotSucceedsSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-G1S6Z6"
]

Test[
	"a \\[NotLessSlantEqual] b \\[NotLessSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-Y5S0M5"
]

Test[
	"a \\[NotGreaterSlantEqual] b \\[NotGreaterSlantEqual] c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190717-K7E0K0"
]







Test[
	"\[Piecewise]{{ChebyshevT[30,x] Sin[100 x],x<0},{ChebyshevT[10,x],True}}"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190730-K3Q6I4"
]





Test[
	"{a;,b;}"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190820-O6Q6C4"
]



Test[
	"f[,1]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190920-R5Q7N8"
]

Test[
	"f[,1,2]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190920-T8N2C7"
]

Test[
	"\\[RawPercent]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191117-S1V0Q8"
]

Test[
	"\\[RawPercent]\\[RawPercent]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191117-V6B3E9"
]

Test[
	"\\[RawPercent]\\[RawPercent]\\[RawPercent]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191117-B3A0J1"
]

Test[
	"\\[RawNumberSign]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191117-T2N7X9"
]

Test[
	"\\[RawNumberSign]123"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191117-J6D4A4"
]

Test[
	"{a\\\nb}"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191209-U5S6D3"
]


Test[
	CodeParse["a::"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "MessageName", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			ErrorNode[Token`Error`ExpectedTag, "", <|Source -> {{1, 4}, {1, 4}}|>]},
			<|Source -> {{1, 1}, {1, 4}}|>]},
		<||>]
	,
	TestID->"Parse-20191213-P6S5K2"
]



Test[
	"106`.7"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191218-G5I8D3"
]

Test[
	"#\\\n1"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191220-Z8C3P1"
]

Test[
	"x + + y"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191221-T5A0M1"
]

Test[
	"x - - y"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191221-X1H5J0"
]

Test[
	"f''''[x]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191222-A5N8K6"
]




Test[
	"!!a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191223-D0H4G9"
]


Test[
	"a \\[DownTee] c \\[DoubleRightTee] d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191223-D4U1W6"
]

Test[
	"a \\[DoubleRightTee] c \\[DownTee] d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20191223-V0J6F1"
]








Test[
	CodeParse["a/"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Times", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Power", <||>], {
				ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 3}, {1, 3}}|>],
				LeafNode[Integer, "-1", <||>]}, <|Source -> {{1, 1}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"Parse-20191224-I3Q3E6"
]

Test[
	CodeParse["a-"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Plus", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Times", <||>], {
				LeafNode[Integer, "-1", <||>],
				ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 2}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"Parse-20191224-Q4A6D4"
]

Test[
	CodeParse["a,b"]
	,
	ContainerNode[String, {
		AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <||>]
	,
	TestID->"Parse-20191224-O2I9C7"
]

Test[
	CodeParse["()"]
	,
	ContainerNode[String, {
		AbstractSyntaxErrorNode[AbstractSyntaxError`OpenParen, {}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"Parse-20191224-W2M0H4"
]

Test[
	"a - +b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200102-W8Y0U5"
]

Test[
	"++a++"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200112-R8X6M4"
]

Test[
	"--a--"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200112-Q6N9J0"
]

Test[
	"a*b\\[Conjugate]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200113-H7J9Z1"
]

Test[
	"a::\\[Beta]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200113-P3P7D5"	
]

Test[
	"a::\"\\[Beta]\""
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200113-T5L0G9"	
]

Test[
	"a>>b\\c"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200122-L8M2M1"	
]

Test[
	"a>>b\\f"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200122-S3W6N2"	
]




(*
I believe these to be the correct interpretations, not the kernel
*)

Test[
	CodeParse["a>>b\\[Beta]"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Put", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[String, "\"b\\\\[Beta]\"", <|Source -> {{1, 4}, {1, 12}}|>]}, <|Source -> {{1, 1}, {1, 12}}|>]}, <||>]
	,
	TestID->"Parse-20200123-O1V2G7"	
]

Test[
	CodeParse["a>>b\\:03b1"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Put", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[String, "\"b\\\\:03b1\"", <|Source -> {{1, 4}, {1, 11}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>]}, <||>]
	,
	TestID->"Parse-20200123-X3N6W0"	
]











Test[
	" a ~f!~ b "
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200121-D9Y3X6"
]

Test[
	" a ~f;~ b "
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200121-V7I9V3"
]

Test[
	" a ~f,~ b "
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200121-C5U1S2"
]

Test[
	" a ~f~ b + c "
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200121-K5D8I7"
]

Test[
	" a ~f~ b @ c "
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200121-C2B6F9"
]






(*
Proper Source
*)
Test[
	CodeParse["a_"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Pattern", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
			CallNode[LeafNode[Symbol, "Blank", <||>], {}, <|Source -> {{1, 2}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"Parse-20200308-S7F8E6"
]




Test[
	"_.1"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200308-L2L2F0"
]

Test[
	"+ +a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200309-J3E7R3"
]

Test[
	"+ + +a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200309-W6S7W9"
]

Test[
	"+ + + +a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200309-P4B8L1"
]




Test[
	"+ + a - 1"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200309-C9V5G7"
]

Test[
	"- - a - 1"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200314-L0R3N4"
]

Test[
	"- - a b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200314-N1L5Y5"
]

Test[
	"a * - -b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200314-X2G3W4"
]




Test[
	"a;\[SquareUnion]b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200309-N3C5E5"
]


Test[
	"a;\[Xnor]b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200309-A7U4G1"
]


Test[
	"a/:b=c|d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-R1I6Y4"
]

Test[
	"a/:b=c|d:.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-E1G3T9"
]

Test[
	"a/:b:=c|d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-A9D9Y9"
]

Test[
	"a/:b:=c|d:.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-C0G3S6"
]

Test[
	"a/:b=.c|d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-I7B8T5"
]

Test[
	"a/:b= .c|d"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200322-O6F0I5"
]

Test[
	"a/:b=.c|d:.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-J0R5D7"
]

Test[
	"a/:b= .c|d:.."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200322-I2K4Z7"
]




Test[
	"<<_  <a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200313-D6J6S0"
]




Test[
	"+;; * a"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200315-G9M6I1"
]



Test[
	"a /: b =."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200321-W1B5X6"
]

Test[
	"a /: b = ."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200321-L8M7K6"
]




Test[
	CodeParse["a /: b =. @ c"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TagUnset", <||>], {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[Symbol, "b", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>], {
			LeafNode[Symbol, "c", <|Source -> {{1, 13}, {1, 14}}|>]}, <|Source -> {{1, 1}, {1, 14}}|>]}, <||>]
	,
	TestID->"Parse-20200320-D5E2I4"
]

Test[
	CodeParse["a /: b = . @ c"]
	,
	ContainerNode[String, {
		CallNode[
			CallNode[LeafNode[Symbol, "TagUnset", <||>], {
				LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[Symbol, "b", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 11}}|>], {
			LeafNode[Symbol, "c", <|Source -> {{1, 14}, {1, 15}}|>]}, <|Source -> {{1, 1}, {1, 15}}|>]}, <||>]
	,
	TestID->"Parse-20200322-D5O4E5"
]



Test[
	CodeParse["a_..b"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Dot", <||>], {
			CallNode[LeafNode[Symbol, "Optional", <||>], {
				CallNode[LeafNode[Symbol, "Pattern", <||>], {
					LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
					CallNode[LeafNode[Symbol, "Blank", <||>], {}, <|Source -> {{1, 2}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 5}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]},
		<|SyntaxIssues -> {
			SyntaxIssue["UnexpectedCharacter", "Suspicious syntax.", "Error", <|Source -> {{1, 4}, {1, 4}}, ConfidenceLevel -> 0.95, CodeActions -> {
				CodeAction["Insert space", InsertText, <|Source -> {{1, 4}, {1, 4}}, "InsertionText" -> " "|>]}|>]}|>]
	,
	TestID->"Parse-20200821-Z5I1N7"
]

Test[
	CodeParse["a_..."]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Repeated", <||>], {
			CallNode[LeafNode[Symbol, "Optional", <||>], {
				CallNode[LeafNode[Symbol, "Pattern", <||>], {
					LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
					CallNode[LeafNode[Symbol, "Blank", <||>], {}, <|Source -> {{1, 2}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]},
		<|SyntaxIssues -> {
			SyntaxIssue["UnexpectedCharacter", "Suspicious syntax.", "Error", <|Source -> {{1, 4}, {1, 4}}, ConfidenceLevel -> 0.95, CodeActions -> {
				CodeAction["Insert space", InsertText, <|Source -> {{1, 4}, {1, 4}}, "InsertionText" -> " "|>]}|>]}|>]
	,
	TestID->"Parse-20200821-F1R5P5"
]

res = ToExpression["a_..b", InputForm, Hold] === ToExpression["(a_.).b", InputForm, Hold]
bug390755Fixed = (res)

BeginTestSection["UnderDotDot", bug390755Fixed]

(*
Prior to 12.2,  a_..b  was parsed as Times[(a_).., b]

12.2 and onward,  a_..b  is parsed as Dot[a_., b]

Related bugs: 390755
*)
Test[
	"a_..b"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200325-F8P4L2"
]

Test[
	"a_..."
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200819-W9T2S6"
]

EndTestSection[]



(*
Verify that Source is preserved

Related GitHub issues: https://github.com/WolframResearch/codeparser/issues/5
*)

Test[
	CodeParse["#1"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Slot", <||>], {
			LeafNode[Integer, "1", <|Source -> {{1, 2}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]}, <||>]
	,
	TestID->"Parse-20200413-B4O4W8"
]

Test[
	CodeParse["#abc"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Slot", <||>], {
			LeafNode[String, "\"abc\"", <|Source -> {{1, 2}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}, <||>]
	,
	TestID->"Parse-20200413-T5O4Q3"
]

Test[
	CodeParse["##2"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "SlotSequence", <||>], {
			LeafNode[Integer, "2", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <||>]
	,
	TestID->"Parse-20200413-Q5X3J6"
]

Test[
	CodeParse["%45"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Out", <||>], {
			LeafNode[Integer, "45", <|Source -> {{1, 2}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <||>]
	,
	TestID->"Parse-20200413-L1I2E4"
]

Test[
	CodeParse["a::bcd"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "MessageName", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
    		LeafNode[String, "\"bcd\"", <|Source -> {{1, 4}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <||>]
	,
	TestID->"Parse-20200413-G5B7Z1"
]

Test[
	CodeParse["<<a`"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Get", <||>], { 
    		LeafNode[String, "\"a`\"", <|Source -> {{1, 3}, {1, 5}}|>]}, <|Source -> {{1, 1}, {1, 5}}|>]}, <||>]
	,
	TestID->"Parse-20200415-P2Y8P6"
]

Test[
	CodeParse["<<\"a`\""]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Get", <||>], { 
    		LeafNode[String, "\"a`\"", <|Source -> {{1, 3}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <||>]
	,
	TestID->"Parse-20200415-I2M5B8"
]

Test[
	CodeParse["a >> b"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Put", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
    		LeafNode[String, "\"b\"", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <||>]
	,
	TestID->"Parse-20200415-W4H4K5"
]

Test[
	CodeParse["a >> \"b\""]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Put", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
    		LeafNode[String, "\"b\"", <|Source -> {{1, 6}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]}, <||>]
	,
	TestID->"Parse-20200415-X7O6E0"
]

Test[
	CodeParse["a >>> b"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "PutAppend", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
    		LeafNode[String, "\"b\"", <|Source -> {{1, 7}, {1, 8}}|>]}, <|Source -> {{1, 1}, {1, 8}}|>]}, <||>]
	,
	TestID->"Parse-20200415-V0C2F3"
]

Test[
	CodeParse["a >>> \"b\""]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "PutAppend", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>], 
    		LeafNode[String, "\"b\"", <|Source -> {{1, 7}, {1, 10}}|>]}, <|Source -> {{1, 1}, {1, 10}}|>]}, <||>]
	,
	TestID->"Parse-20200415-N1P8Z5"
]









(*
https://github.com/WolframResearch/codeparser/issues/11
*)
Test[
	CodeParse["<<[\"]"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Get", <||>], {
			LeafNode[String, "\"[\\\"]\"", <|Source -> {{1, 3}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 6}}|>]}, <||>]
	,
	TestID->"Parse-20200416-L5V7S7"
]




Test[
  "#2.c"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
  ,
  TestID->"Parse-20200419-X9B9F9"
]

Test[
  "##2.c"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
  ,
  TestID->"Parse-20200419-Q3Q6W9"
]

Test[
  "%2.c"
  ,
  Null
  ,
  EquivalenceFunction -> parseEquivalenceFunction
  ,
  TestID->"Parse-20200419-E2I4C6"
]








Test[
	"{ a, %% }"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200430-G3Y5R8"
]



Test[
	"{,}"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20200703-O0D8F0"
]




(*
Test that the embedded \t is converted to \\t
*)
Test[
	CodeParse["\"a\tb\""]
	,
	ContainerNode[String, {
		LeafNode[String, "\"a\\tb\"", <|Source -> {{1, 1}, {1, 6}}|>]}, <||>]
	,
	TestID->"Parse-20200803-G3C1P6"
]





(*
testing embedded newlines, but also we do not reformat linear syntax...
*)
Test[
	CodeParse["\\((*\n*)\\)"]
	,
	ContainerNode[String, {
		LeafNode[Token`LinearSyntaxBlob, "\\((*\n*)\\)", <|Source -> {{1, 1}, {2, 5}}|>]}, <||>]
	,
	TestID->"Parse-20200803-R4W5C6"
]







(*
make sure Comma is at top-level
*)

Test[
	CodeParse["a,?b c"]
	,
	ContainerNode[String, {
		AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			CallNode[LeafNode[Symbol, "Times", <||>], {
				CallNode[LeafNode[Symbol, "PatternTest", <||>], {
					ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> {{1, 3}, {1, 3}}|>],
					LeafNode[Symbol, "b", <|Source -> {{1, 4}, {1, 5}}|>]}, <|Source -> {{1, 3}, {1, 5}}|>],
				LeafNode[Symbol, "c", <|Source -> {{1, 6}, {1, 7}}|>]}, <|Source -> {{1, 3}, {1, 7}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]}, <||>]
	,
	TestID->"Parse-20200803-X4E2N5"
]



Test[
	CodeParse["f[1,,]"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>], {
			LeafNode[Integer, "1", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 5}, {1, 5}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 6}, {1, 6}}|>]}, <|Source -> {{1, 1}, {1, 7}}|>]},
		<| SyntaxIssues -> {
			SyntaxIssue["Comma", "Extra ``,``.", "Error", <|
				Source -> {{1, 5}, {1, 6}},
				ConfidenceLevel -> 1.,
				CodeActions -> {CodeAction["Delete ``,``", DeleteText, <|Source -> {{1, 5}, {1, 6}}|>]}|>]} |>]
	,
	TestID->"Parse-20200810-R4E3W9"
]

Test[
	CodeParse["f[1\\[InvisibleComma]\\[InvisibleComma]]"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "f", <|Source -> {{1, 1}, {1, 2}}|>], {
			LeafNode[Integer, "1", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 21}, {1, 21}}|>],
			LeafNode[Symbol, "Null", <|Source -> {{1, 38}, {1, 38}}|>]}, <|Source -> {{1, 1}, {1, 39}}|>]},
		<| SyntaxIssues -> {
			SyntaxIssue["Comma", "Extra ``,``.", "Error", <|
				Source -> {{1, 21}, {1, 38}},
				ConfidenceLevel -> 1.,
				CodeActions -> {CodeAction["Delete ``,``", DeleteText, <|Source -> {{1, 21}, {1, 38}}|>]}|>]} |>]
	,
	TestID->"Parse-20200810-F0V6V5"
]







