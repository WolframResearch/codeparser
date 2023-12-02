Print["\n===== Start File.mt =====\n"]


path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]



Needs["CodeParser`"]
Needs["CodeParser`ToString`"] (* ToInputFormString *)
Needs["CodeParser`Utils`"]



(*

Parse File

*)

sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "sample.wl"}]

cst = CodeConcreteParse[File[sample]]

Test[
	cst
	,
	ContainerNode[File, {
		LeafNode[Token`Newline, $systemNewline, <|Source -> {{1, 1}, {2, 1}}|>],
		InfixNode[Plus, {
			LeafNode[Integer, "1", <|Source -> {{2, 1}, {2, 2}}|>],
			LeafNode[Token`Plus, "+", <|Source -> {{2, 2}, {2, 3}}|>],
    		LeafNode[Integer, "1", <|Source -> {{2, 3}, {2, 4}}|>] }, <|Source -> {{2, 1}, {2, 4}}|>],
    	LeafNode[Token`Newline, $systemNewline, <|Source -> {{2, 4}, {3, 1}}|>] }, <|Source -> {{1, 1}, {3, 1}}, "FileName" -> sample|>]
	,
	TestID->"File-20181230-J0G3I8"
]



Test[
	ToInputFormString[cst]
	,
	$systemNewline <> $systemNewline <> " 1 + 1 " <> $systemNewline <> $systemNewline
	,
	TestID->"File-20181230-T2D2W6"
]








(*
shebangwarning = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "shebangwarning.wl"}]

cst = ConcreteParseFile[shebangwarning]

TestMatch[
	cst
	,
	FileNode[File, {
		LeafNode[Slot, "#something", <|Source -> {{1, 1}, {1, 10}}|>],
		LeafNode[Token`ToplevelNewline, "\n", <|Source -> {{2, 0}, {2, 0}}|>] },
		KeyValuePattern[{
			Source -> {{1, 1}, {2, 0}},
			SyntaxIssues -> {SyntaxIssue["Shebang", "# on first line looks like #! shebang", "Remark", <|Source -> {{1, 1}, {1, 1}}|>]} }] ]
	,
	TestID->"File-20181230-M7H7Q7"
]
*)


carriagereturn = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "carriagereturn.wl"}]

cst = CodeConcreteParse[File[carriagereturn]]

TestMatch[
	cst
	,
	ContainerNode[File, {LeafNode[Token`Newline, "\r", <|Source -> {{1, 1}, {2, 1}}|>],
					LeafNode[Token`Newline, "\r", <|Source -> {{2, 1}, {3, 1}}|>],
					LeafNode[Symbol, "A", <|Source -> {{3, 1}, {3, 2}}|>]},
										<| SyntaxIssues->{
											EncodingIssue["UnexpectedCarriageReturn", _, _, _],
											EncodingIssue["UnexpectedCarriageReturn", _, _, _]}, Source -> {{1, 1}, {3, 2}}, "FileName" -> carriagereturn|>]
	,
	TestID->"File-20190422-C6U5B6"
]

carriagereturn2 = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "carriagereturn2.wl"}]

cst = CodeConcreteParse[File[carriagereturn2]]

TestMatch[
	cst
	,
	ContainerNode[File, {LeafNode[String, "\"\r\n123\"", <|Source -> {{1, 1}, {2, 5}}|>]}, <|"EmbeddedNewlines" -> {{1, 1}}, Source -> {{1, 1}, {2, 5}}, "FileName" -> carriagereturn2|>]
	,
	TestID->"File-20190606-O8I6M9"
]










(*
warnings
*)

package = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "package.wl"}]

ast = CodeParse[File[package]]

TestMatch[
	ast
	,
	ContainerNode[File, {
			CallNode[LeafNode[Symbol, "BeginPackage", <|Source -> {{2, 1}, {2, 13}}|>], {
				LeafNode[String, "\"Foo.m`\"", <|Source -> {{2, 14}, {2, 22}}|>]}, <|Source -> {{2, 1}, {2, 23}}|>], 
			CallNode[LeafNode[Symbol, "EndPackage", <|Source -> {{4, 1}, {4, 11}}|>], {}, <|Source -> {{4, 1}, {4, 13}}|>]
		},
		<|
			Source -> {{1, 1}, {6, 1}},
			"FileName" -> package,
			AbstractSyntaxIssues -> {SyntaxIssue["Package", "Directive does not have correct syntax.", "Error", _]}
		|>]
	,
	TestID->"File-20190601-E8O7Y2"
]






(*

Tokenize File

*)

sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "sample.wl"}]

Test[
	CodeTokenize[File[sample]]
	,
	{
		LeafNode[Token`Newline, $systemNewline, <|Source -> {{1, 1}, {2, 1}}|>], 
		LeafNode[Integer, "1", <|Source -> {{2, 1}, {2, 2}}|>], 
		LeafNode[Token`Plus, "+", <|Source -> {{2, 2}, {2, 3}}|>], 
		LeafNode[Integer, "1", <|Source -> {{2, 3}, {2, 4}}|>], 
		LeafNode[Token`Newline, $systemNewline, <|Source -> {{2, 4}, {3, 1}}|>]}
	,
	TestID->"File-20181230-Q3C4N0"
]









(*

test large number of comments

*)

comments = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "comments.wl"}]

cst = CodeConcreteParse[File[comments]]

TestMatch[
	cst
	,
	ContainerNode[File, _, _]
	,
	TestID->"File-20190601-G6E3K9"
]







strange = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "strange.wl"}]

cst = CodeConcreteParse[File[strange]]

TestMatch[
	cst
	,
	ContainerNode[File, {
			BinaryNode[Set, {
				LeafNode[Symbol, "\.01x", <|Source -> {{1, 1}, {1, 3}}|>],
				LeafNode[Whitespace, " ", <|Source -> {{1, 3}, {1, 4}}|>],
				LeafNode[Token`Equal, "=", <|Source -> {{1, 4}, {1, 5}}|>],
				LeafNode[Whitespace, " ", <|Source -> {{1, 5}, {1, 6}}|>],
				LeafNode[Integer, "1", <|Source -> {{1, 6}, {1, 7}}|>] }, <|Source -> {{1, 1}, {1, 7}}|>]
		},
		<|
			SyntaxIssues -> {
				(* from CharacterDecoder, strange character in general *)
				EncodingIssue["UnexpectedCharacter", "Unexpected character: ``\"\.01\" (\\.01)``.", "Warning", _],
				(* from Tokenizer, strange letterlike *)
				SyntaxIssue["UnexpectedLetterlikeCharacter", "Unexpected letterlike character: ``\"\.01\" (\\.01)``.", "Warning", _]},
			Source -> {{1, 1}, {1, 7}},
			"FileName" -> strange
		|>
	]
	,
	TestID->"File-20190602-N5D1B8"
]



strange = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "string1.wl"}]

cst = CodeConcreteParse[File[strange]]

TestMatch[
	cst
	,
	ContainerNode[File, {
			LeafNode[Token`Newline, $systemNewline, <|Source -> {{1, 1}, {2, 1}}|>],
			LeafNode[String, "\"data\\\\" <> $systemNewline <> "\"", <|Source -> {{2, 1}, {3, 2}}|>],
			LeafNode[Token`Newline, $systemNewline, <|Source -> {{3, 2}, {4, 1}}|>],
			LeafNode[Token`Newline, $systemNewline, <|Source -> {{4, 1}, {5, 1}}|>],
			LeafNode[Symbol, "x", <|Source -> {{5, 1}, {5, 2}}|>],
			LeafNode[Token`Newline, $systemNewline, <|Source -> {{5, 2}, {6, 1}}|>]
		},
		<|
			"EmbeddedNewlines" -> {{2, 1}},
			Source -> {{1, 1}, {6, 1}},
			"FileName" -> strange
		|>
	]
	,
	TestID->"File-20190804-K7V2D8"
]







continuation = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "continuation.wl"}]

cst = CodeConcreteParse[File[continuation]]

TestMatch[
	cst
	,
	ContainerNode[File, {
			GroupNode[List, {
				LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 2}}|>],
				LeafNode[Token`Newline, $systemNewline, <|Source -> {{1, 2}, {2, 1}}|>],
				LeafNode[Whitespace, "\t", <|Source -> {{2, 1}, {2, 2}}|>],
				LeafNode[Integer, "1", <|Source -> {{2, 2}, {2, 3}}|>],
				LeafNode[Token`CloseCurly, "\\" <> $systemNewline <> "}", <|Source -> {{2, 3}, {3, 2}}|>]}, <|Source -> {{1, 1}, {3, 2}}|>]
		},
		<|
			"SimpleLineContinuations" -> {{2, 3}},
			Source -> {{1, 1}, {3, 2}},
			"FileName" -> continuation
		|>
	]
	,
	TestID->"File-20191025-I3T9F3"
]









(*

parseTest

*)


Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "carriagereturn2.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-S9D2H1"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "carriagereturn3.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-M7F7D1"
]



(*
Bug 363889:

Mathematica 12.0.0 Kernel for Microsoft Windows (64-bit)
Copyright 1988-2018 Wolfram Research, Inc.

In[1]:= ToExpression["123\n", InputForm, Hold]

Out[1]= Hold[123]

In[2]:= ToExpression["123\r\n", InputForm, Hold]

Out[2]= Hold[123, Null]

In[3]:=


In other places, on all platforms, \r\n can be used as a single newline

e.g., inside of line continuations:
In[27]:= ToExpression["123\\\r\n456"] // InputForm

Out[27]//InputForm=
123456

But bug 363889 prevents \r\n from being used as a single newline at toplevel

*)
res = (ToExpression["123\r\n", InputForm, Hold] === Hold[123])
bug363889Fixed = res

BeginTestSection["ToplevelNewlineSyntaxError", bug363889Fixed]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "carriagereturn4.wl"}], 1]
	,
	(*
	when bug 363889 is fixed, the expectation is that carriagereturn4.wl will parse with no problems
	*)
	Null
	,
	TestID->"File-20190607-H0M3H1"
]

EndTestSection[]





Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "carriagereturn.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-N0A3V6"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "comments.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-C8M7N2"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash.txt"}], 1]
	,
	Null
	,
	TestID->"File-20190606-K3J8I5"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-0001.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-0001.txt"}]]|>]
	,
	TestID->"File-20190606-P8X5C7"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-0002.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-0002.txt"}]]|>]
	,
	TestID->"File-20190606-T7B2I1"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-0003.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-0003.txt"}]]|>]
	,
	TestID->"File-20190606-G9V4A7"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-characternameoperations.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-characternameoperations.txt"}]]|>]
	,
	TestID->"File-20190606-K6L3G0"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-characternamestrings.txt"}], 1]
	,
	(*
	Random inputs, but strings, so expect good syntax
	*)
	Null
	,
	TestID->"File-20190606-K3I5E7"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-characternames.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-characternames.txt"}]]|>]
	,
	TestID->"File-20190606-E9U5Q2"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-contexts.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-contexts.txt"}]]|>]
	,
	TestID->"File-20190606-B4F3R6"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-integers.txt"}], 1]
	,
	(*
	Random inputs, but integers, so expect good syntax
	*)
	Null
	,
	TestID->"File-20190606-I5H3E5"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-nestedsymbolicarithmetic.txt"}], 1]
	,
	(*
	Random inputs, bad exprs, so expect good syntax
	*)
	Null
	,
	TestID->"File-20190606-I4D8M0"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-random.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-random.txt"}]]|>]
	,
	TestID->"File-20190606-K8J6I8"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-reals.txt"}], 1]
	,
	(*
	Random inputs, but reals, so expect good syntax
	*)
	Null
	,
	TestID->"File-20190606-L1U3P6"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-specialchararacters.txt"}], 1]
	,
	(*
	Random inputs, so expect bad syntax
	*)
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-specialchararacters.txt"}]]|>]
	,
	TestID->"File-20190606-K8S9K0"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-symbolicarithmetic2.txt"}], 1]
	,
	Null
	,
	TestID->"File-20190606-G1A4I3"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-symbolicarithmetic3.txt"}], 1]
	,
	Null
	,
	TestID->"File-20190606-U5V2M1"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-symbolicarithmetic4.txt"}], 1]
	,
	Null
	,
	TestID->"File-20190606-T5M9O3"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "inputs-symbolicarithmetic.txt"}], 1]
	,
	Null
	,
	TestID->"File-20190606-U7S8O5"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "linearsyntax.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-W9Q9W8"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "package.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-G3M7M0"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "sample.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-A0H2X0"
]

(*
Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "script.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190610-D7P4F8"
]
*)

(*
Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "shebangwarning.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-F1E7Z2"
]
*)

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "strange.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190606-G6Q1C7"
]


Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "span1.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190626-L4G9A9"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "string1.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190804-T9F2J6"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "comment.wl"}], 1]
	,
	Null
	,
	TestID->"File-20190904-Q7T9Y5"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "continuation.wl"}], 1]
	,
	Null
	,
	TestID->"File-20191025-Z3P6E3"
]

(*
ToExpression["a >>\n   b", InputForm, Hold] works,

but ToExpression["Hold[a >>\n   b]"] does not

Due to the parsing of >>

But really we are just testing for a crash here, so just ignore the output
*)
Test[
	Catch[
		parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash2.txt"}], 1]
		,
		"Uncaught"
	];
	ignored
	,
	ignored
	,
	TestID->"File-20191102-Z8M8J2"
]

(*

Bad UTF-8 is converted to extended ASCII characters by the kernel

But the new parser makes sure to return \[UnknownGlyph] for bad characters


In[14]:= FromCharacterCode[{34, 241, 34}, "UTF8"]

During evaluation of In[14]:= $CharacterEncoding::utf8:
	The byte sequence {241,34} could not be interpreted as a character in the UTF-8 character encoding.

Out[14]= "\"\[NTilde]\""

I argue that Out[14] should be "\"\[UnknownGlyph]\""

*)

Test[
	Catch[
		parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash3.txt"}], 1]
		,
		"Uncaught"
	];
	ignoredBecauseofBadCharacters
	,
	ignoredBecauseofBadCharacters
	,
	{}
	,
	TestID->"File-20191102-W1Z2A1"
]


(*

?a

Uncomment when ?a is handled

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash4.txt"}], 1]
	,
	Null
	,
	TestID->"File-20191103-I6Y6P4"
]
*)

(*
ToExpression["1*\\\n"] returns 1, but should fail

The new parser does the correct thing and gives an error

bug 382006

really, we are just testing if this crashed, so just ignore the output
*)
Test[
	Catch[
		parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash5.txt"}], 1]
		,
		"Uncaught"
	];
	ignored
	,
	ignored
	,
	TestID->"File-20191103-V9V1F9"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash6.txt"}], 1]
	,
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash6.txt"}]]|>]
	,
	TestID->"File-20191103-B9T6B0"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash7.txt"}], 1]
	,
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash7.txt"}]]|>]
	,
	TestID->"File-20191103-A0V5S0"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash8.txt"}], 1]
	,
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash8.txt"}]]|>]
	,
	TestID->"File-20191103-D1L3U1"
]

Test[
	Catch[
		parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash9.txt"}], 1]
		,
		"Uncaught"
	];
	ignoredBecauseofBadCharacters
	,
	ignoredBecauseofBadCharacters
	,
	TestID->"File-20191103-F1U3Y6"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash10.txt"}], 1]
	,
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash10.txt"}]]|>]
	,
	TestID->"File-20191103-H2O6Z5"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash11.txt"}], 1]
	,
	Failure["SyntaxError", <|"File" -> File[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "crash11.txt"}]]|>]
	,
	TestID->"File-20191103-U8W2U9"
]

Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "small", "empty.wl"}], 1]
	,
	Null
	,
	TestID->"File-20200619-Y6H2W6"
]


Test[
	parseTest[FileNameJoin[{DirectoryName[$CurrentTestSource], "files", ToExpression["\"\\:9ce5\\:7269\""]<>".wl"}], 1]
	,
	Null
	,
	TestID->"File-20221116-O0T8W2"
]




















