Print["\n===== Start LineContinuations.mt =====\n"]

path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]


(*
line continuations and newlines
*)
Test[
	CodeConcreteParse["\"abc\\\r\ndef\""]
	,
	ContainerNode[String, {
		LeafNode[String, "\"abc\\\r\ndef\"", <|Source -> {{1, 1}, {2, 5}}|>] }, <|"ComplexLineContinuations" -> {{1, 1}}, Source -> {{1, 1}, {2, 5}}|>]
	,
	TestID->"LineContinuations-20190606-U7J9I3"
]


TestMatch[
	CodeConcreteParse["{12,\\\n3}"]
	,
	ContainerNode[String, {
		GroupNode[List, {
			LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 2}}|>],
			InfixNode[Comma, {
				LeafNode[Integer, "12", <|Source -> {{1, 2}, {1, 4}}|>],
				LeafNode[Token`Comma, ",", <|Source -> {{1, 4}, {1, 5}}|>],
				LeafNode[Integer, "\\\n3", <|Source -> {{1, 5}, {2, 2}}|>]}, <|Source -> {{1, 2}, {2, 2}}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 2}, {2, 3}}|>]}, <|Source -> {{1, 1}, {2, 3}}|>] }, _]
	,
	TestID->"LineContinuations-20190930-B8P9Y9"
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
	TestID->"LineContinuations-20190826-H8C5S0"
]


Test[
	"f''\\\n''[x]"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"LineContinuations-20191222-X8S9A3"
]





(*
Test line continuations between tokens where other whitespace matters

Whitespace matters here:
a_
_b
a_b
#1
#abc
##2
%45

These are single nodes, and spaces would break them up.
But line continuations are fine to have between tokens
*)

Test[
	"a\\\n_\\\nb"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"LineContinuations-20200415-X5M3W6"
]

TestMatch[
	CodeParse["#\\\n1"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Slot", <||>], {
			LeafNode[Integer, "1", <|Source -> {{1, 2}, {2, 2}}|>]}, <|Source -> {{1, 1}, {2, 2}}|>]}, _]
	,
	TestID->"LineContinuations-20200415-E1Q6O7"
]

TestMatch[
	CodeParse["#\\\nabc"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Slot", <||>], {
			LeafNode[String, "\"abc\"", <|Source -> {{1, 2}, {2, 4}}|>]}, <|Source -> {{1, 1}, {2, 4}}|>]}, _]
	,
	TestID->"LineContinuations-20200415-Q8V1L8"
]

TestMatch[
	CodeParse["##\\\n2"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "SlotSequence", <||>], {
			LeafNode[Integer, "2", <|Source -> {{1, 3}, {2, 2}}|>]}, <|Source -> {{1, 1}, {2, 2}}|>]}, _]
	,
	TestID->"LineContinuations-20200415-M9Y6M1"
]

TestMatch[
	CodeParse["%\\\n45"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "Out", <||>], {
			LeafNode[Integer, "45", <|Source -> {{1, 2}, {2, 3}}|>]}, <|Source -> {{1, 1}, {2, 3}}|>]}, _]
	,
	TestID->"LineContinuations-20200415-K6V1V1"
]







BeginTestSection["LineContinuationsInFiles", False]

Test[
	"<< a\\\n~"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"LineContinuations-20200425-S9S2S0"
]


Test[
	" << a\\\n//"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"LineContinuations-20200425-H9P8O1"
]

Test[
	" << a\\\n //"
	,
	Null
	,
	EquivalenceFunction -> parseEquivalenceFunction
	,
	TestID->"LineContinuations-20200425-C1U7J3"
]

EndTestSection[]




(*
Test that the embedded \t is converted to \\t
*)

Test[
	CodeParse["\"a\\\n\tb\""]
	,
	ContainerNode[String, {
		LeafNode[String, "\"a\\tb\"", <|Source -> {{1, 1}, {2, 4}}|>]}, <|Source -> {{1, 1}, {2, 4}}|>]
	,
	TestID->"LineContinuations-20200803-L8I2C9"
]
 


(*
There are simple continuations

But there are no complex continuations

Verify that simple continuation is removed and the embedded newline is escaped
*)
Test[
	CodeParse["\\\n\"ab\\\\\ncd\""]
	,
	ContainerNode[String, {LeafNode[String, "\"ab\\\\\\ncd\"", <|Source -> {{2, 1}, {3, 4}}|>]}, <|Source -> {{1, 1}, {3, 4}}|>]
	,
	TestID->"LineContinuations-20200804-C5K6X8"
]


(*
May believe it is ok to:

Try to be clever

reassign CODEPOINT_CRLF to -243

test curSource.to_point() & 0xff against only '\r' because -243 == 13 (mod 2^8)

But there are completely valid multi-byte characters that then get treated as '\r' also

Such as \[CHacek]
*)
Test[
	CodeConcreteParse["\\\[CHacek]"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\\[CHacek]", <|Source -> {{1, 1}, {1, 3}}|>]},
		
		<|SyntaxIssues -> {
			SyntaxIssue["UnhandledCharacter", "Unhandled character ``\\\\\\[CHacek]``.", "Fatal", <|Source -> {{1, 1}, {1, 3}}, ConfidenceLevel -> 1.|>]}, Source -> {{1, 1}, {1, 3}}|>]
	,
	TestID->"LineContinuations-20220618-N7N2P7"
]


(*
illustrates interesting phenomenon:

if a file ends with a line continuation, then that line continuation is considered the start of the EOF token, and is not included in the Source!

*)
Test[
	CodeConcreteParse["1\\\n"]
	,
	ContainerNode[String, {
		LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>]}, <|"SimpleLineContinuations" -> {{1, 2}}, Source -> {{1, 1}, {1, 2}}|>]
	,
	TestID->"LineContinuations-20220713-O3P5D6"
]


