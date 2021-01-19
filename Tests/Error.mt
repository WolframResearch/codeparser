
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
		ErrorNode[Token`Error`UnterminatedString, "\"\n", <|Source -> {{1, 1}, {1, 2}}|>]}, _]
	,
	TestID->"Error-20210118-C0F5T5"
]

TestMatch[
	CodeConcreteParse["\"\r", SourceConvention -> "LineColumn"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"\r", <|Source -> {{1, 1}, {1, 2}}|>]}, _]
	,
	TestID->"Error-20210118-R8T2P0"
]

TestMatch[
	CodeConcreteParse["\"\r\n", SourceConvention -> "LineColumn"]
	,
	ContainerNode[String, {
		ErrorNode[Token`Error`UnterminatedString, "\"\r\n", <|Source -> {{1, 1}, {1, 2}}|>]}, _]
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















