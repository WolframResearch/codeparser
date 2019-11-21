
Needs["AST`"]

(*
StrayCarriageReturn
*)
TestMatch[
	FirstCase[ConcreteParseString["{ \r }", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["UnexpectedCarriageReturn", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-I5S5I8"
]

(*
NotContiguous:
*)
TestMatch[
	FirstCase[ConcreteParseString["a =    .", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["NotContiguous", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-Q9Y0M6"
]

(*

FormatIssues related to implicit Times are handled in the formatter

TestMatch[
	FirstCase[ConcreteParseString["1.2`a", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-M4P4L8"
]
*)

TestMatch[
	FirstCase[ConcreteParseString["1.2`->3", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-N6V1Y7"
]

TestMatch[
	FirstCase[ConcreteParseString["a-->0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-L4Q6B3"
]

TestMatch[
	FirstCase[ConcreteParseString["a--=0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-B6D1O6"
]

TestMatch[
	FirstCase[ConcreteParseString["<||>=0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-M5D3W1"
]

TestMatch[
	FirstCase[ConcreteParseString["t/.03", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-R4X1R6"
]

TestMatch[
	FirstCase[ConcreteParseString["a++=0", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["Space", _, _, _]}]
	,
	TestID->"FormatIssues-20190521-Z7F1E3"
]



(*
LineContinuation
*)

TestMatch[
	FirstCase[ConcreteParseString[" { a, \\\n b } ", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {FormatIssue["UnexpectedLineContinuation", _, _, _]}]
	,
	TestID->"FormatIssues-20190523-B8K3A5"
]










