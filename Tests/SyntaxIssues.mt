

(*
CharacterEncoding
*)

(*
TODO: invalid UTF-8 sequence
*)


(*
StrayCarriageReturn
*)
TestMatch[
	FirstCase[ConcreteParseString["{ \r }", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["StrayCarriageReturn", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-I5S5I8"
]




(*
SyntaxError:
*)

TestMatch[
	FirstCase[ConcreteParseString["\\", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxError", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-P7R3O7"
]







(*
UnrecognizedCharacter:
*)

TestMatch[
	FirstCase[ConcreteParseString["\\A", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-I4Y1N1"
]

TestMatch[
	FirstCase[ConcreteParseString["\\G", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-W6Z9W6"
]

TestMatch[
	FirstCase[ConcreteParseString["\\a", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-E4B2P8"
]

TestMatch[
	FirstCase[ConcreteParseString["\\-", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-K6Z9U4"
]

TestMatch[
	FirstCase[ConcreteParseString["\\[A!]", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-C2N2N9"
]

TestMatch[
	FirstCase[ConcreteParseString["\\[!", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-O9X2M6"
]

TestMatch[
	FirstCase[ConcreteParseString["\\[Alpa]", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-O4J2Z6"
]

TestMatch[
	FirstCase[ConcreteParseString["\\:lcdm", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B6E6G2"
]

TestMatch[
	FirstCase[ConcreteParseString["\\.lc", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-N9Z2X4"
]

TestMatch[
	FirstCase[ConcreteParseString["\\009", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnrecognizedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Z2J8V4"
]

TestMatch[
	FirstCase[ConcreteParseString["\\|lhvbnr", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
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
	FirstCase[ConcreteParseString["\\[NumberComma]", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["UnsupportedCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-W1X7L2"
]






(*
DifferentLine:
*)

TestMatch[
	FirstCase[ConcreteParseString["-\na", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["DifferentLine", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Y8O9L2"
]

TestMatch[
	FirstCase[ConcreteParseString["{ a\n! }", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["DifferentLine", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-R2X2T0"
]

(*

TODO: should we have "DifferentLine" warnings about ; ?

TestMatch[
	FirstCase[ConcreteParseString["{ a\n; }", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["DifferentLine", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-M5K3M2"
]
*)

TestMatch[
	FirstCase[ConcreteParseString["{ a\n;; }", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["DifferentLine", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-E0L3O1"
]

TestMatch[
	FirstCase[ConcreteParseString["{ a~\nf~b } ", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["DifferentLine", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-S5U4W8"
]















(*
NotContiguous:
*)


TestMatch[
	FirstCase[ConcreteParseString["If []", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["NotContiguous", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-F6H2O9"
]


TestMatch[
	FirstCase[ConcreteParseString["a =    .", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["NotContiguous", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Q9Y0M6"
]










(*
SyntaxUndocumentedSlot:
*)


TestMatch[
	FirstCase[ConcreteParseString["#$aaa", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxUndocumentedSlot", _, _, _]}]
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
	FirstCase[ConcreteParseString["#aaa`bbb", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxUndocumentedSlot", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B1O4Q3"
]

TestMatch[
	FirstCase[ConcreteParseString["#\"aaa\"", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxUndocumentedSlot", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-T8A1U7"
]













(*
StrangeCharacter:
*)


TestMatch[
	FirstCase[ConcreteParseString["a\\.00", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["StrangeCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B2V1Z3"
]

TestMatch[
	FirstCase[ConcreteParseString["a\\:f456", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["StrangeCharacter", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-K6H8E0"
]












(*
SyntaxAmbiguity:
*)


TestMatch[
	FirstCase[ConcreteParseString["1.2`a", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-M4P4L8"
]

TestMatch[
	FirstCase[ConcreteParseString["1.2`->3", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-N6V1Y7"
]

TestMatch[
	FirstCase[ConcreteParseString["0..", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-H5C3T5"
]

TestMatch[
	FirstCase[ConcreteParseString["1.2.3", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-T2M0V0"
]

TestMatch[
	FirstCase[ConcreteParseString["_...", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-U2Z6B3"
]

TestMatch[
	FirstCase[ConcreteParseString["_.0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-V2H2D5"
]

TestMatch[
	FirstCase[ConcreteParseString["a-->0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-L4Q6B3"
]

TestMatch[
	FirstCase[ConcreteParseString["a--=0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-B6D1O6"
]

TestMatch[
	FirstCase[ConcreteParseString["<||>=0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-M5D3W1"
]

TestMatch[
	FirstCase[ConcreteParseString["t/.03", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-R4X1R6"
]

TestMatch[
	FirstCase[ConcreteParseString["a++=0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["SyntaxAmbiguity", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-Z7F1E3"
]



(*
LineContinuation
*)

TestMatch[
	FirstCase[ConcreteParseString[" { a, \\\n b } ", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["StrayLineContinuation", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190523-B8K3A5"
]





(*
ImplicitTimesSpan
*)

TestMatch[
	FirstCase[ConcreteParseString[";;b;;", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {OrderlessPatternSequence[SyntaxIssue["ImplicitTimesSpan", _, _, _], SyntaxIssue["EndOfLine", _, _, _]]}]
	,
	TestID->"SyntaxIssues-20190523-I1D9N0"
]


TestMatch[
	FirstCase[ConcreteParseString["a;;b;;", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {OrderlessPatternSequence[SyntaxIssue["ImplicitTimesSpan", _, _, _], SyntaxIssue["EndOfLine", _, _, _]]}]
	,
	TestID->"SyntaxIssues-20190523-L7M6K3"
]





