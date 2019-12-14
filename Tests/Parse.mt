
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
	StringNode[String, {}, <||>]
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


(*
should succeed
*)
Test[
	"2.Pi"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190821-G1U3U4"
]

(*
should fail
*)
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
	"N[-2.338107410459767038489197252446735440638540145672387852483854437213668002700\\
              283647782164041731329320284760093853265952775225466858359866744868898716819727\\
              540973152674991112748065999645628353491550367242154602253040142644994178463934\\
              453444457600947385805599328400354197885486873437032794768373623126914436368456\\
              216321695224896886771887967253644542964146651161756655217909281106701616013124\\
              010872087510680153635409304355401733365182436841536888461538337816732637447723\\
              5216626917898162900770617327677840, {pp,aa}]"
    ,
    Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"Parse-20190826-H8C5S0"
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




res = (ToExpression["a . -b", InputForm, Hold] =!= $Failed)
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
	ParseString["a::"]
	,
	StringNode[String, {
		CallNode[LeafNode[Symbol, "MessageName", <||>], {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			AbstractSyntaxErrorNode[AbstractSyntaxError`EmptyString, {
				LeafNode[Token`Error`EmptyString, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 4}, {1, 4}}|>]},
			<|Source -> {{1, 1}, {1, 4}}|>]},
		<|AbstractSyntaxIssues -> {
			SyntaxIssue["TopLevel", "Unexpected expression at top-level.", "Warning", <|
				Source -> {{1, 1}, {1, 4}}, 
				ConfidenceLevel -> 0.75|>]}|>]
	,
	TestID->"Parse-20191213-P6S5K2"
]




