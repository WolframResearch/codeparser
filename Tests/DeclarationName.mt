
Needs["CodeParser`"]

Test[
	DeclarationName[CodeParse["a"]]
	,
	"a"
	,
	TestID->"DeclarationName-20181230-C3E7Y2"
]

Test[
	DeclarationName[CodeParse["a[]"]]
	,
	"a"
	,
	TestID->"DeclarationName-20181230-V6Q8O6"
]

Test[
	DeclarationName[CodeParse["a /; q"]]
	,
	"a"
	,
	TestID->"DeclarationName-20181230-Z9C5M8"
]





TestMatch[
	DeclarationName[CodeParse["123"]]
	,
	_Failure
	,
	TestID->"DeclarationName-20181230-C8D4W9"
]



TestMatch[
	DeclarationName[CodeParse["e:InitializationValue[Except[_String | _Symbol],___]"]]
	,
	"InitializationValue"
	,
	TestID->"DeclarationName-20200218-I1H4I7"
]










