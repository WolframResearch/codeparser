Needs["CodeParser`"]

(*------------------------------------*)
(* Bug 439902                         *)
(*------------------------------------*)

TestMatch[
	CodeTokenize @ ExportString[1, "JPEG"],
	{__, ErrorNode[Token`Error`UnterminatedString, _, _]}
]

TestMatch[
	CodeConcreteParse @ ExportString[1, "JPEG"],
	ContainerNode[String, {__}, _]
]
