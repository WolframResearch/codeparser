Print["\n===== Start Error.mt =====\n"]

path = FileNameJoin[{DirectoryName[$CurrentTestSource], "CodeParserTestUtils"}]
PrependTo[$Path, path]

Needs["CodeParserTestUtils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]




(*

Tests related to Error.wl:

Handling unterminated groups
Handling unterminated tokens

Chunks

etc.

*)


TestMatch[
	CodeConcreteParse["\"\n", SourceConvention -> "LineColumn"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"", <|Source -> {{1, 1}, {1, 2}}|>]}, _]
	,
	TestID->"Error-20210118-C0F5T5"
]

TestMatch[
	CodeConcreteParse["\"\r", SourceConvention -> "LineColumn"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"", <|Source -> {{1, 1}, {1, 2}}|>]}, _]
	,
	TestID->"Error-20210118-R8T2P0"
]

TestMatch[
	CodeConcreteParse["\"\r\n", SourceConvention -> "LineColumn"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"", <|Source -> {{1, 1}, {1, 2}}|>]}, _]
	,
	TestID->"Error-20210118-T0V9F9"
]

TestMatch[
	CodeConcreteParse["\"\n", SourceConvention -> "SourceCharacterIndex"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"\n", <|Source -> {1, 2}|>]}, _]
	,
	TestID->"Error-20210118-O5Q6Y0"
]

TestMatch[
	CodeConcreteParse["\"\r", SourceConvention -> "SourceCharacterIndex"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"\r", <|Source -> {1, 2}|>]}, _]
	,
	TestID->"Error-20210118-W7L4K8"
]

TestMatch[
	CodeConcreteParse["\"\r\n", SourceConvention -> "SourceCharacterIndex"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"\r\n", <|Source -> {1, 3}|>]}, _]
	,
	TestID->"Error-20210118-R9E3S6"
]


Test[
	CodeConcreteParse["\\|110000"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\|110000", <|Source -> {{1, 1}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Error-20211104-P0L8Y0"
]

Test[
	CodeConcreteParse["\\|FFFFFF"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\|FFFFFF", <|Source -> {{1, 1}, {1, 9}}|>]}, <|Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Error-20211104-Q2O1J4"
]


Test[
	CodeConcreteParse["\\\\[Alpa]"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\\\", <|Source -> {{1, 1}, {1, 3}}|>],
		GroupNode[GroupSquare, {
			LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 3}, {1, 4}}|>],
			LeafNode[Symbol, "Alpa", <|Source -> {{1, 4}, {1, 8}}|>],
			LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 8}, {1, 9}}|>]}, <|Source -> {{1, 3}, {1, 9}}|>]}, <|SyntaxIssues -> {
		
		SyntaxIssue["UnrecognizedLongName", "Unrecognized longname: ``\\\\[Alpa]``.", "Error", <|Source -> {{1, 1}, {1, 9}}, ConfidenceLevel -> 0.75, CodeActions -> {CodeAction["Replace with ``\\\\[Alpha]``", ReplaceText, <|Source -> {{1, 1}, {1, 9}}, "ReplacementText" -> "\\\\[Alpha]"|>]}, "AdditionalDescriptions" -> {"``Alpa`` is not a valid long name."}|>]}, Source -> {{1, 1}, {1, 9}}|>]
	,
	TestID->"Error-20220709-M4Y7Z3"
]

(*
no warning
*)
Test[
	CodeConcreteParse["RegularExpression[\"\\\\[a-zA-Z0-9]+\\\\]\"]"]
	,
	ContainerNode[String, {
		CallNode[{LeafNode[Symbol, "RegularExpression", <|Source -> {{1, 1}, {1, 18}}|>]},
			GroupNode[GroupSquare, {
				LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 18}, {1, 19}}|>],
				LeafNode[String, "\"\\\\[a-zA-Z0-9]+\\\\]\"", <|Source -> {{1, 19}, {1, 38}}|>],
				LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 38}, {1, 39}}|>]}, <|Source -> {{1, 18}, {1, 39}}|>], <|Source -> {{1, 1}, {1, 39}}|>]}, <|Source -> {{1, 1}, {1, 39}}|>]
	,
	TestID->"Error-20220711-I6O1H4"
]










