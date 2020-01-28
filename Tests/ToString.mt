
Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["1+1"]]]
	,
	" 1 + 1 "
	,
	TestID->"ToString-20181230-P1F9Q9"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["_ + __ * ___"]]]
	,
	" _ +  __*___  "
	,
	TestID->"ToString-20181230-S7R9U8"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["% ^ # ^ ## ^ f''[x]"]]]
	,
	" %^ #^ ##^  f' ' [x]   "
	,
	TestID->"ToString-20181230-E6E4O1"
]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["@"]]]
	,
	" @ "
	,
	TestID->"ToString-20181230-V8O8B1"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["{a_b, c__d, e___f, _., g_.}"]]]
	,
	"{ a_b,c__d,e___f,_.,g_. }"
	,
	TestID->"ToString-20181230-U1H3E1"
]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["aaa - bbb + ccc - !ddd"]]]
	,
	" aaa - bbb + ccc -  !ddd  "
	,
	TestID->"ToString-20181230-Z9F3L8"
]



Test[
	ToInputFormString[Aggregate[CodeConcreteParse["a::b::c"]]]
	,
	" a::b::c "
	,
	TestID->"ToString-20181230-P0K1Y7"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["a /: b := c"]]]
	,
	" a/:b:=c "
	,
	TestID->"ToString-20181230-H9T6O8"
]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["##&"]]]
	,
	" ##& "
	,
	TestID->"ToString-20181230-A2F7W1"
]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["f[]"]]]
	,
	"f[]"
	,
	TestID->"ToString-20181230-R5Q3J4"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["f["]]]
	,
	"f["
	,
	TestID->"ToString-20181230-T4A0R3"
]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["f[[4]]"]]]
	,
	"f[[4]]"
	,
	TestID->"ToString-20181230-C6W4M5"
]










Test[
	ToInputFormString[Aggregate[CodeConcreteParse["\\(x\\)"]]]
	,
	"\\(x\\)"
	,
	TestID->"ToString-20181230-U6K9Q7"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["\\(x"]]]
	,
	"\\(x"
	,
	TestID->"ToString-20181230-R6R9E5"
]

Test[
	ToInputFormString[Aggregate[CodeConcreteParse["\\(x,y\\)"]]]
	,
	"\\(x,y\\)"
	,
	TestID->"ToString-20181231-U3W4B3"
]















Test[
	ToInputFormString[Aggregate[CodeConcreteParse["a& & + b"]]]
	,
	"   a& &  + b "
	,
	TestID->"ToString-20181231-F0J3L4"
]






(*
Error handling
*)
Test[
	ToInputFormString[Aggregate[CodeConcreteParse["A B:C:.Ne"]]]
	,
	" A   B:C :  . Ne   "
	,
	TestID->"ToString-20190523-V1I4S4"
]


Test[
	ToInputFormString[Aggregate[CodeConcreteParse["a:"]]]
	,
	" a: "
	,
	TestID->"ToString-20190523-H5C9J2"
]






Test[
	StringJoin[ToSourceCharacterString /@ CodeConcreteParse["{]", #[[1]] &]]
	,
	"{]"
	,
	TestID->"ToString-20190926-T4I8S1"
]










