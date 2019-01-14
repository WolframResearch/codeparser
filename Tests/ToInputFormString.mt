
Needs["AST`"]



Test[
	ToInputFormString[ConcreteParseString["1+1"]]
	,
	"1 + 1"
	,
	TestID->"ToInputFormString-20181230-P1F9Q9"
]

Test[
	ToInputFormString[ConcreteParseString["_ + __ * ___"]]
	,
	"_ + __*___"
	,
	TestID->"ToInputFormString-20181230-S7R9U8"
]

Test[
	ToInputFormString[ConcreteParseString["% ^ # ^ ## ^ f''[x]"]]
	,
	"%^#^##^f''[x]"
	,
	TestID->"ToInputFormString-20181230-E6E4O1"
]


Test[
	ToInputFormString[ConcreteParseString["@"]]
	,
	"@"
	,
	TestID->"ToInputFormString-20181230-V8O8B1"
]

Test[
	ToInputFormString[ConcreteParseString["{a_b, c__d, e___f, _., g_.}"]]
	,
	"{a_b,c__d,e___f,_.,g_.}"
	,
	TestID->"ToInputFormString-20181230-U1H3E1"
]


Test[
	ToInputFormString[ConcreteParseString["aaa - bbb + ccc - !ddd"]]
	,
	"aaa - bbb + ccc - !ddd"
	,
	TestID->"ToInputFormString-20181230-Z9F3L8"
]



Test[
	ToInputFormString[ConcreteParseString["a::b::c"]]
	,
	"a::b::c"
	,
	TestID->"ToInputFormString-20181230-P0K1Y7"
]

Test[
	ToInputFormString[ConcreteParseString["a /: b := c"]]
	,
	"a/:b:=c"
	,
	TestID->"ToInputFormString-20181230-H9T6O8"
]


Test[
	ToInputFormString[ConcreteParseString["##&"]]
	,
	"##& "
	,
	TestID->"ToInputFormString-20181230-A2F7W1"
]


Test[
	ToInputFormString[ConcreteParseString["f[]"]]
	,
	"f[]"
	,
	TestID->"ToInputFormString-20181230-R5Q3J4"
]

Test[
	ToInputFormString[ConcreteParseString["f["]]
	,
	"f["
	,
	TestID->"ToInputFormString-20181230-T4A0R3"
]


Test[
	ToInputFormString[ConcreteParseString["f[[4]]"]]
	,
	"f[[4]]"
	,
	TestID->"ToInputFormString-20181230-C6W4M5"
]










Test[
	ToInputFormString[ConcreteParseString["\\(x\\)"]]
	,
	"\\(x\\)"
	,
	TestID->"ToInputFormString-20181230-U6K9Q7"
]

Test[
	ToInputFormString[ConcreteParseString["\\(x"]]
	,
	"\\(x"
	,
	TestID->"ToInputFormString-20181230-R6R9E5"
]

Test[
	ToInputFormString[ConcreteParseString["\\(x,y\\)"]]
	,
	"\\(x,y\\)"
	,
	TestID->"ToInputFormString-20181231-U3W4B3"
]








sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "sample.wl"}]

ast = ConcreteParseFile[sample]

Test[
	ToInputFormString[ast]
	,
	"1 + 1"
	,
	TestID->"ToInputFormString-20181230-T2D2W6"
]








Test[
	ToInputFormString[Null]
	,
	""
	,
	TestID->"ToInputFormString-20181231-R6I0L4"
]



Test[
	ToInputFormString[ConcreteParseString["a& & + b"]]
	,
	"a& &  + b"
	,
	TestID->"ToInputFormString-20181231-F0J3L4"
]






(*
Error handling
*)
Test[
	ToInputFormString[ConcreteParseString["A B:C:.Ne"]]
	,
	"A B:C:.Ne"
	,
	TestID->"Parse-20181117-K1W0K0"
]


Test[
	ToInputFormString[ConcreteParseString["a:"]]
	,
	"a:"
	,
	TestID->"Parse-20181118-V5G8O1"
]
















