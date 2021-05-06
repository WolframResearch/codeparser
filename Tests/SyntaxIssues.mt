
Needs["CodeParser`"]

(*
CharacterEncoding
*)

(*
TODO: invalid UTF-8 sequence
*)







(*
UnrecognizedCharacter:
*)

TestMatch[
	FirstCase[CodeConcreteParse["\\A", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-I4Y1N1"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\G", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-W6Z9W6"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\a", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-E4B2P8"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\-", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-K6Z9U4"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\[A!]", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-C2N2N9"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\[!", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-O9X2M6"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\[Alpa]", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-O4J2Z6"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\:lcdm", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B6E6G2"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\.lc", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-N9Z2X4"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\009", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Z2J8V4"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\|lhvbnr", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Z6W6P2"
]





(*
UnsupportedCharacter:
*)

(*TestMatch[
	FirstCase[CodeConcreteParse["\\[NumberComma]", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnsupportedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-W1X7L2"
]*)



(*
SyntaxUndocumentedSlot:
*)


TestMatch[
	FirstCase[CodeConcreteParse["#$aaa", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UndocumentedSlotSyntax", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-P7B1J0"
]

(*

Letterlike characters are not currently warned about

TestMatch[
	FirstCase[ConcreteParseString["#\[Alpha]", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxUndocumentedSlot", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-L3M6H1"
]

*)

TestMatch[
	FirstCase[CodeConcreteParse["#aaa`bbb", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UndocumentedSlotSyntax", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B1O4Q3"
]

TestMatch[
	FirstCase[CodeConcreteParse["#\"aaa\"", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UndocumentedSlotSyntax", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-T8A1U7"
]




(*
StrangeCharacter:
*)


TestMatch[
	FirstCase[CodeConcreteParse["a\\.00", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {
		(* from Tokenizer *)
		SyntaxIssue["UnexpectedCharacter", _, _, _],
		(* from CharacterDecoder *)
		SyntaxIssue["UnexpectedLetterlikeCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B2V1Z3"
]

TestMatch[
	FirstCase[CodeConcreteParse["a\\:f456", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedLetterlikeCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-K6H8E0"
]





(*
SyntaxAmbiguity:
*)

TestMatch[
	FirstCase[CodeConcreteParse["0..", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["InsertSpace", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-H5C3T5"
]

TestMatch[
	FirstCase[CodeConcreteParse["1.2.3", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedImplicitTimes", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-T2M0V0"
]

TestMatch[
	FirstCase[CodeConcreteParse["_...", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-U2Z6B3"
]

(*

Format issues related to implicit Times are handled by the formatter

TestMatch[
	FirstCase[ConcreteParseString["_.0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-V2H2D5"
]
*)



(*
Make sure that Source is correct
*)
TestMatch[
	FirstCase[CodeConcreteParse["a\\[DoublePrime] + 2", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedLetterlikeCharacter", _, _, KeyValuePattern[Source -> {{1, 2}, {1, 10}}]]}]
	,
	TestID->"SyntaxIssues-20200405-H2D3M0"
]

(*
Make sure that Source is correct
*)
TestMatch[
	FirstCase[CodeConcreteParse["\\[DoublePrime] + 2", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedLetterlikeCharacter", _, _, KeyValuePattern[Source -> {{1, 1}, {1, 9}}]]}]
	,
	TestID->"SyntaxIssues-20200504-H4U1I4"
]

(*
Make sure that Source is correct
*)
TestMatch[
	FirstCase[CodeConcreteParse["abc\\[DoublePrime] + 2", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedLetterlikeCharacter", _, _, KeyValuePattern[Source -> {{1, 4}, {1, 12}}]]}]
	,
	TestID->"SyntaxIssues-20200504-P6Z1C2"
]









(*

EncodingIssues

*)

(*
In[14]:= ToCharacterCode["\[Alpha]", "UTF-8"]

Out[14]= {206, 177}
*)

TestMatch[
	FirstCase[CodeConcreteParse[{206}, ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	<|SyntaxIssues -> {
		EncodingIssue["InvalidCharacterEncoding", "Invalid UTF-8 sequence.", "Fatal", <|Source -> {{1, 1}, {1, 2}}, ConfidenceLevel -> 1.|>],
		SyntaxIssue["UnexpectedLetterlikeCharacter", "Unexpected letterlike character: ``\"\[UnknownGlyph]\" (\\[UnknownGlyph])``.", "Warning", <|Source -> {{1, 1}, {1, 2}}, ConfidenceLevel -> 0.8|>]
		}|>
	,
	TestID->"SyntaxIssues-20200413-T5W0H7"
]

TestMatch[
	FirstCase[CodeConcreteParse["{1,\r2}", ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>]&)],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {
		EncodingIssue["UnexpectedCarriageReturn", _, "Error", KeyValuePattern[Source -> {{1, 4}, {1, 4}}]]}]
	,
	TestID->"SyntaxIssues-20200413-K2N2U0"
]






TestMatch[
	CodeConcreteParse["a\[InvisibleSpace]b"]
	,
	ContainerNode[String, {
		InfixNode[Times, {
			LeafNode[Symbol, "a", <|Source -> {{1, 1}, {1, 2}}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 2}, {1, 2}}|>],
			LeafNode[Whitespace, "\[InvisibleSpace]", <|Source -> {{1, 2}, {1, 3}}|>],
			LeafNode[Symbol, "b", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]},
		<| SyntaxIssues -> {
			SyntaxIssue["UnexpectedCharacter", "Unexpected character: ``\"\[InvisibleSpace]\" (\\[InvisibleSpace])``.", "Warning", <|Source -> {{1, 2}, {1, 3}}, ConfidenceLevel -> 0.75, CodeActions -> _|>],
			SyntaxIssue["UnexpectedSpaceCharacter", "Unexpected space character: ``\"\[InvisibleSpace]\" (\\[InvisibleSpace])``.", "Warning", <|Source -> {{1, 2}, {1, 3}}, ConfidenceLevel -> 0.85|>] } |>]
	,
	TestID->"SyntaxIssues-20200621-C5B3J2"
]





(*
Comma:
*)


TestMatch[
	FirstCase[CodeParse[" f[1,2,] "],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["Comma", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190520-V4Q1U5"
]

TestMatch[
	CodeParse[" f[,1] "]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "f", _], {
			LeafNode[Symbol, "Null", _],
			LeafNode[Integer, "1", _]}, _] }, KeyValuePattern[SyntaxIssues -> {SyntaxIssue["Comma", _, _, _]}]]
	,
	TestID->"SyntaxIssues-20190520-V9J1I3"
]

TestMatch[
	CodeParse["f[1,,2]"]
	,
	ContainerNode[String, {
		CallNode[LeafNode[Symbol, "f", _], {
			LeafNode[Integer, "1", _],
			LeafNode[Symbol, "Null", _],
			LeafNode[Integer, "2", _]}, _] }, KeyValuePattern[SyntaxIssues -> {SyntaxIssue["Comma", _, _, _]}]]
	,
	TestID->"SyntaxIssues-20200627-G1O2F9"
]




