
Needs["AST`"]

Test[
	DeclarationName[ParseString["a"]]
	,
	"a"
	,
	TestID->"DeclarationName-20181230-C3E7Y2"
]

Test[
	DeclarationName[ParseString["a[]"]]
	,
	"a"
	,
	TestID->"DeclarationName-20181230-V6Q8O6"
]

Test[
	DeclarationName[ParseString["a /; q"]]
	,
	"a"
	,
	TestID->"DeclarationName-20181230-Z9C5M8"
]





TestMatch[
	DeclarationName[ParseString["123"]]
	,
	_Failure
	,
	TestID->"DeclarationName-20181230-C8D4W9"
]





