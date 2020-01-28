
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
	FirstCase[CodeConcreteParse["\\A", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-I4Y1N1"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\G", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-W6Z9W6"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\a", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-E4B2P8"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\-", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-K6Z9U4"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\[A!]", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-C2N2N9"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\[!", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-O9X2M6"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\[Alpa]", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-O4J2Z6"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\:lcdm", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B6E6G2"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\.lc", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-N9Z2X4"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\009", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Z2J8V4"
]

TestMatch[
	FirstCase[CodeConcreteParse["\\|lhvbnr", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Z6W6P2"
]





(*
UnsupportedCharacter:
*)

TestMatch[
	FirstCase[CodeConcreteParse["\\[NumberComma]", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnsupportedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-W1X7L2"
]




























(*
SyntaxUndocumentedSlot:
*)


TestMatch[
	FirstCase[CodeConcreteParse["#$aaa", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
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
	FirstCase[CodeConcreteParse["#aaa`bbb", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UndocumentedSlotSyntax", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B1O4Q3"
]

TestMatch[
	FirstCase[CodeConcreteParse["#\"aaa\"", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
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
	FirstCase[CodeConcreteParse["a\\.00", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {
		(* from CharacterDecoder *)
		SyntaxIssue["UnexpectedCharacter", _, _, _],
		(* from Tokenizer *)
		SyntaxIssue["UnexpectedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B2V1Z3"
]

TestMatch[
	FirstCase[CodeConcreteParse["a\\:f456", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-K6H8E0"
]












(*
SyntaxAmbiguity:
*)

TestMatch[
	FirstCase[CodeConcreteParse["0..", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-H5C3T5"
]

TestMatch[
	FirstCase[CodeConcreteParse["1.2.3", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnexpectedImplicitTimes", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-T2M0V0"
]

TestMatch[
	FirstCase[CodeConcreteParse["_...", ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
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





