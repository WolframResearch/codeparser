
Needs["CodeParser`"]

Test[
	CodeConcreteParseBox[RowBox[{"<<", "ExampleData`FunctionWithAssert`"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Get, {
			LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
			LeafNode[String, "ExampleData`FunctionWithAssert`", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20190918-D2P4H5"
]

Test[
	CodeConcreteParseBox[RowBox[{"a", " ", "b"}]]
	,
	ContainerNode[Box, {
		InfixNode[Times, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 1}]|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20191015-Q5H2Y6"
]





box = TagBox["a", Function[BoxForm`e$, BoxForm`e$]]

cst = CodeConcreteParseBox[box]

Test[
	cst
	,
	With[{evaledData = <||>},
		ContainerNode[Box, {
			BoxNode[TagBox, {
				LeafNode[Symbol, "a", <|Source -> {1}|>],
				CodeNode[Null, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20191119-A5T4V2"
]

agg = CodeParser`Abstract`Aggregate[cst]

Test[
	agg
	,
	With[{evaledData = <||>},
		ContainerNode[Box, {
			BoxNode[TagBox, {
				LeafNode[Symbol, "a", <|Source -> {1}|>], 
	  			CodeNode[Null, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20191119-F6L1J2"
]

(*
Test[
	ToInputFormString[agg]
	,
	"\\!\\(\\*TagBox[a, Function[BoxForm`e$, BoxForm`e$]]\\)"
	,
	TestID->"Boxes-20191119-R1Q6C7"
]
*)

ast = CodeParser`Abstract`Abstract[agg]

Test[
	ast
	,
	With[{evaledData = <||>},
		ContainerNode[Box, {
			BoxNode[TagBox, {
				LeafNode[Symbol, "a", <|Source -> {1}|>],
				CodeNode[Null, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20191119-R5E0G4"
]

TestMatch[
	ToFullFormString[ast]
	,
	Failure["CannotConvertBoxesToFullForm", _]
	,
	TestID->"Boxes-20191119-Z6J3D5"
]
 

box = RowBox[{"<<", "ExampleData`FunctionWithAssert`", " "}]
 
Test[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixNode[Get, {
			LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
			LeafNode[String, "ExampleData`FunctionWithAssert`", <|Source -> {1, 2}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20191230-I9W2G5"
]




box = RowBox[{"a", "::", "b", "::", "c", "::", "d"}]
 
Test[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		InfixNode[MessageName, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 2}|>],
			LeafNode[String, "b", <|Source -> {1, 3}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 4}|>],
			LeafNode[String, "c", <|Source -> {1, 5}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 6}|>],
			LeafNode[String, "d", <|Source -> {1, 7}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20191230-K0E3D0"
]






 
box = RowBox[{"\[Integral]", RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[Integrate, {
			LeafNode[Token`LongName`Integral, "\[Integral]", KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]}, {
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>]}, <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20200126-N4V0N6"
]



(*
This is not a concrete error

When abstracting, then it becomes an error
*)
Test[
	CodeConcreteParseBox[RowBox[{"1", ":", "2"}]]
 	,
 	ContainerNode[Box, {
 		BinaryNode[Pattern, {
 			LeafNode[Integer, "1", <|Source -> {1, 1}|>],
 			LeafNode[Token`Colon, ":", <|Source -> {1, 2}|>],
 			LeafNode[Integer, "2", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200616-Y7G7J2"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"\\[", "Alpa", "]"}]]
	,
	ContainerNode[Box, {
		ErrorNode[Token`Error`UnhandledCharacter, "\\[Alpa]", _]}, <||>]
	,
	TestID->"Boxes-20200616-H4V3I2"
]


Test[
	CodeConcreteParseBox[RowBox[{"!!", "a"}]]
	,
	ContainerNode[Box, {
		PrefixNode[PrefixNot2, {
			LeafNode[Token`BangBang, "!!", <|Source -> {1, 1}|>],
			LeafNode[Symbol, "a", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200616-F1N0C4"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"\[Minus]", "34"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Minus, {
			LeafNode[Token`LongName`Minus, "\[Minus]", KeyValuePattern[Source -> {1, 1}]],
			LeafNode[Integer, "34", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20200617-G4L7A2"
]


Test[
	CodeConcreteParseBox[RowBox[{"123", ">>", "tmp"}]]
	,
	ContainerNode[Box, {
		BinaryNode[Put, {
			LeafNode[Integer, "123", <|Source -> {1, 1}|>],
			LeafNode[Token`GreaterGreater, ">>", <|Source -> {1, 2}|>],
			LeafNode[String, "tmp", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200617-P0V6A8"
]


Test[
	CodeConcreteParseBox[RowBox[{"a", "+", RowBox[{"(*", "**)"}], "b"}]]
	,
	ContainerNode[Box, {
		InfixNode[Plus, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Plus, "+", <|Source -> {1, 2}|>],
			GroupNode[Comment, {
				LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 3, 1, 1}|>],
				LeafNode[Token`Boxes`StarCloseParen, "**)", <|Source -> {1, 3, 1, 2}|>]}, <|Source -> {1, 3}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 4}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200617-K6Q4J5"
]


Test[
	CodeConcreteParseBox[RowBox[{"(*", " ", "15`.", " ", "*)"}]]
	,
	ContainerNode[Box, {
		GroupNode[Comment, {
			LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 1}|>],
			LeafNode[String, " ", <|Source -> {1, 2}|>],
			LeafNode[String, "15`.", <|Source -> {1, 3}|>],
			LeafNode[String, " ", <|Source -> {1, 4}|>],
			LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200618-H6K1F2"
]


Test[
	CodeConcreteParseBox[InterpretationBox["a", Sequence[Appearance -> "Horizontal"]]]
	,
	With[{assoc = <||>},
		ContainerNode[Box, {
			BoxNode[InterpretationBox, {
				CodeNode[Null, "a", assoc],
				CodeNode[Null, Sequence[Appearance -> "Horizontal"], assoc]}, <|Source -> {}|>]}, <||>]
	]
	,
	TestID->"Boxes-20200618-U5E6C1"
]


Test[
	CodeConcreteParseBox[RowBox[{"_", ":", "0"}]]
	,
	ContainerNode[Box, {
		BinaryNode[Optional, {
			LeafNode[Token`Under, "_", <|Source -> {1, 1}|>],
			LeafNode[Token`Colon, ":", <|Source -> {1, 2}|>],
			LeafNode[Integer, "0", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200619-I7R5A5"
]


Test[
	CodeConcreteParseBox[RowBox[{"x", "\[NonBreakingSpace]", "+", "x"}]]
	,
	ContainerNode[Box, {
		InfixNode[Plus, {
			LeafNode[Symbol, "x", <|Source -> {1, 1}|>], 
     		LeafNode[Token`Boxes`MultiWhitespace, "\[NonBreakingSpace]", <|Source -> {1, 2}|>], 
     		LeafNode[Token`Plus, "+", <|Source -> {1, 3}|>], 
     		LeafNode[Symbol, "x", <|Source -> {1, 4}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200620-L1I1H4"
]


Test[
	CodeConcreteParseBox[RowBox[{"x", "\[InvisibleSpace]", "+", "x"}]]
	,
	ContainerNode[Box, {
		InfixNode[Plus, {
			LeafNode[Symbol, "x", <|Source -> {1, 1}|>], 
     		LeafNode[Token`Boxes`MultiWhitespace, "\[InvisibleSpace]", <|Source -> {1, 2}|>], 
     		LeafNode[Token`Plus, "+", <|Source -> {1, 3}|>], 
     		LeafNode[Symbol, "x", <|Source -> {1, 4}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200621-O0X2J8"
]


Test[
	CodeConcreteParseBox[RowBox[{"x", "\[VeryThinSpace]", "+", "x"}]]
	,
	ContainerNode[Box, {
		InfixNode[Plus, {
			LeafNode[Symbol, "x", <|Source -> {1, 1}|>], 
     		LeafNode[Token`Boxes`MultiWhitespace, "\[VeryThinSpace]", <|Source -> {1, 2}|>], 
     		LeafNode[Token`Plus, "+", <|Source -> {1, 3}|>], 
     		LeafNode[Symbol, "x", <|Source -> {1, 4}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200622-S3L3F3"
]


Test[
	CodeConcreteParseBox[RowBox[{"x", "\[NegativeVeryThinSpace]", "+", "x"}]]
	,
	ContainerNode[Box, {
		InfixNode[Plus, {
			LeafNode[Symbol, "x", <|Source -> {1, 1}|>], 
     		LeafNode[Token`Boxes`MultiWhitespace, "\[NegativeVeryThinSpace]", <|Source -> {1, 2}|>], 
     		LeafNode[Token`Plus, "+", <|Source -> {1, 3}|>], 
     		LeafNode[Symbol, "x", <|Source -> {1, 4}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-C7Y4I5"
]


Test[
	CodeConcreteParseBox[RowBox[{"<<", " ", "EquationTrekker`"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Get, {
			LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[String, "EquationTrekker`", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200621-Y6O8Y8"
]


Test[
	CodeConcreteParseBox[RowBox[{"<<", "EquationTrekker`", " "}]]
	,
	ContainerNode[Box, {
		PrefixNode[Get, {
			LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>],
			LeafNode[String, "EquationTrekker`", <|Source -> {1, 2}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200624-U4H7A2"
]


Test[
	CodeConcreteParseBox[RowBox[{"?", "Join*"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Information, {
			LeafNode[Token`Question, "?", <|Source -> {1, 1}|>],
			LeafNode[String, "Join*", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200621-A3S2R0"
]


Test[
	CodeConcreteParseBox["101101^^2"]
	,
	ContainerNode[Box, {
		ErrorNode[Token`Error`InvalidBase, "101101^^2", <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200622-R5K1V9"
]


Test[
	CodeConcreteParseBox[RowBox[{"?", "Cos[x]"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Information, {
			LeafNode[Token`Question, "?", <|Source -> {1, 1}|>],
			LeafNode[String, "Cos[x]", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200622-D2N0F3"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"\[Integral]", RowBox[{"x", " ", RowBox[{"Cos", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[Integrate, {
			LeafNode[Token`LongName`Integral, "\[Integral]", KeyValuePattern[Source -> {1, 1}]],
			LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2, 1, 2}|>],
			CallNode[{LeafNode[Symbol, "Cos", <|Source -> {1, 2, 1, 3, 1, 1}|>]}, {
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 3, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 3, 1, 3}|>],
					LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 3, 1, 4}|>]}, <||>]}, <|Source -> {1, 2, 1, 3}|>],
			PrefixNode[DifferentialD, {
				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 4, 1, 1}]],
				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 4, 1, 2}|>]}, <|Source -> {1, 2, 1, 4}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20200622-H3W5G3"
]


Test[
	CodeConcreteParseBox[RowBox[{"f", "]"}]]
	,
	ContainerNode[Box, {
		GroupMissingOpenerNode[GroupSquare, {
			LeafNode[Symbol, "f", <|Source -> {1, 1}|>],
			LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200622-T6Q0P2"
]


Test[
	CodeConcreteParseBox[RowBox[{"f", "}"}]]
	,
	ContainerNode[Box, {
		GroupMissingOpenerNode[List, {
			LeafNode[Symbol, "f", <|Source -> {1, 1}|>],
			LeafNode[Token`CloseCurly, "}", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200622-X5F8A2"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"a", "\[TwoWayRule]", "b"}]]
	,
	ContainerNode[Box, {
		BinaryNode[TwoWayRule, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`LongName`TwoWayRule, "\[TwoWayRule]", KeyValuePattern[Source -> {1, 2}]],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200622-U6D8D8"
]


Test[
	CodeConcreteParseBox[RowBox[{"?", "`*"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Information, {
			LeafNode[Token`Question, "?", <|Source -> {1, 1}|>],
			LeafNode[String, "`*", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-K0R8R8"
]


Test[
	CodeConcreteParseBox["#\"user-agent\""]
	,
	ContainerNode[Box, {
		CompoundNode[Slot, {
			LeafNode[Token`Hash, "#", <|Source -> {}|>],
			LeafNode[String, "\"user-agent\"", <|Source -> {}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-N7J4G8"
]


Test[
	CodeConcreteParseBox[RowBox[{"a", "::", "\"bbb\""}]]
	,
	ContainerNode[Box, {
		InfixNode[MessageName, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 2}|>],
			LeafNode[String, "\"bbb\"", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200624-U3V8K6"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"x", "\[VectorGreaterEqual]", "y"}]]
	,
	ContainerNode[Box, {
		InfixNode[InfixInequality, {
			LeafNode[Symbol, "x", <|Source -> {1, 1}|>],
			LeafNode[Token`LongName`VectorGreaterEqual, "\[VectorGreaterEqual]", KeyValuePattern[Source -> {1, 2}]],
			LeafNode[Symbol, "y", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-Q4M7M9"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"\[CubeRoot]", RowBox[{"-", "1000"}]}]]
	,
	ContainerNode[Box, {
		PrefixNode[CubeRoot, {
			LeafNode[Token`LongName`CubeRoot, "\[CubeRoot]", KeyValuePattern[Source -> {1, 1}]],
			PrefixNode[Minus, {
				LeafNode[Token`Minus, "-", <|Source -> {1, 2, 1, 1}|>],
				LeafNode[Integer, "1000", <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-D6J5W2"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"\[Sqrt]", "3"}]]
	,
	ContainerNode[Box, {
		PrefixNode[Sqrt, {
			LeafNode[Token`LongName`Sqrt, "\[Sqrt]", KeyValuePattern[Source -> {1, 1}]],
			LeafNode[Integer, "3", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-J7F4G4"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"g", "\[PermutationProduct]", "gg"}]]
	,
	ContainerNode[Box, {
		InfixNode[PermutationProduct, {
			LeafNode[Symbol, "g", <|Source -> {1, 1}|>],
			LeafNode[Token`LongName`PermutationProduct, "\[PermutationProduct]", KeyValuePattern[Source -> {1, 2}]],
			LeafNode[Symbol, "gg", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-I5L6T6"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"n", "\[Divides]", "m"}]]
	,
	ContainerNode[Box, {
		InfixNode[Divisible, {
			LeafNode[Symbol, "n", <|Source -> {1, 1}|>],
			LeafNode[Token`LongName`Divides, "\[Divides]", KeyValuePattern[Source -> {1, 2}]],
			LeafNode[Symbol, "m", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-T3I2Z1"
]


TestMatch[
	CodeConcreteParseBox[RowBox[{"3", "\[GreaterSlantEqual]", "4"}]]
	,
	ContainerNode[Box, {
		InfixNode[InfixInequality, {
			LeafNode[Integer, "3", <|Source -> {1, 1}|>],
			LeafNode[Token`LongName`GreaterSlantEqual, "\[GreaterSlantEqual]", KeyValuePattern[Source -> {1, 2}]],
			LeafNode[Integer, "4", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20200623-S9N6Q5"
]











Test[
	CodeConcreteParseBox[RowBox[{"\n", "a"}]]
	,
	ContainerNode[Box, {
		LeafNode[Token`Newline, "\n", <|Source -> {1, 1}|>],
		LeafNode[Symbol, "a", <|Source -> {1, 2}|>]}, <||>]
	,
	TestID->"Boxes-20201023-R6Y5N5"
]



Test[
	CodeConcreteParseBox[RowBox[{"a", "\n", "b"}]]
	,
	ContainerNode[Box, {
		LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
		LeafNode[Token`Newline, "\n", <|Source -> {1, 2}|>],
		LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <||>]
	,
	TestID->"Boxes-20201023-O0K3H5"
]



Test[
	CodeConcreteParseBox[RowBox[{"a", " ", ":=", "\n", " ", "b"}]]
	,
	ContainerNode[Box, {
		BinaryNode[SetDelayed, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Token`ColonEqual, ":=", <|Source -> {1, 3}|>],
			LeafNode[Token`Newline, "\n", <|Source -> {1, 4}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 5}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 6}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20201023-H1C3C8"
]





(*

this test is out-of-date

Test[
	CodeConcreteParseBox[RowBox[{"f", ";", RowBox[{"(*", "*)"}], "\n", "g"}]]
	,
	ContainerNode[Box, {
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "f", <|Source -> {1, 1}|>],
			LeafNode[Token`Semi, ";", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <||>],
			GroupNode[Comment, {
				LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 3, 1, 1}|>],
				LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> {1, 3, 1, 2}|>]}, <|Source -> {1, 3, 1}|>]}, <||>],
		LeafNode[Token`Newline, "\n", <|Source -> {1, 4}|>],
		LeafNode[Symbol, "g", <|Source -> {1, 5}|>]}, <||>]
	,
	TestID->"Boxes-20201023-V9T4E2"
]
*)

(*

this test is out-of-date

Test[
	CodeConcreteParseBox[RowBox[{"f", ";", RowBox[{"(*", "*)"}], "\n", "g", ";"}]]
	,
	ContainerNode[Box, {
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "f", <|Source -> {1, 1}|>],
			LeafNode[Token`Semi, ";", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <||>],
			GroupNode[Comment, {
				LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 3, 1, 1}|>],
				LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> {1, 3, 1, 2}|>]}, <|Source -> {1, 3, 1}|>]}, <||>],
		LeafNode[Token`Newline, "\n", <|Source -> {1, 4}|>],
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "g", <|Source -> {1, 5}|>],
			LeafNode[Token`Semi, ";", <|Source -> {1, 6}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> {1, 6}|>]}, <||>]}, <||>]
	,
	TestID->"Boxes-20201023-P9O9Q2"
]
*)


Test[
	CodeConcreteParseBox[RowBox[{"a", " ", "b", "*", "c"}]]
	,
	ContainerNode[Box, {
		InfixNode[Times, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 1}]|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>],
			LeafNode[Token`Star, "*", <|Source -> {1, 4}|>],
			LeafNode[Symbol, "c", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20201023-I1T9S0"
]


Test[
	CodeConcreteParseBox[RowBox[{"a", "*", "b", " ", "c"}]]
	,
	ContainerNode[Box, {
		InfixNode[Times, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Star, "*", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 3}]|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 4}|>],
			LeafNode[Symbol, "c", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20201023-W2R0R5"
]


Test[
	CodeConcreteParseBox[RowBox[{"a", " ", "*", " ", "b"}]]
	,
	ContainerNode[Box, {
		InfixNode[Times, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Token`Star, "*", <|Source -> {1, 3}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 4}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20201023-Q9B1K2"
]




Test[
	CodeConcreteParseBox[RowBox[{"p", " ", "q", "*", "\n", " ", "r"}]]
	,
	ContainerNode[Box, {
		InfixNode[Times, {
			LeafNode[Symbol, "p", <|Source -> {1, 1}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 1}]|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "q", <|Source -> {1, 3}|>],
			LeafNode[Token`Star, "*", <|Source -> {1, 4}|>],
			LeafNode[Token`Newline, "\n", <|Source -> {1, 5}|>],
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 6}|>],
			LeafNode[Symbol, "r", <|Source -> {1, 7}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20201023-D5E9D4"
]



Test[
	CodeConcreteParseBox[RowBox[{"a", ";", RowBox[{"(*", "*)"}], ";"}]]
	,
	ContainerNode[Box, {
		InfixNode[CompoundExpression, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Semi, ";", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> After[{1, 2}]|>],
			GroupNode[Comment, {
				LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 3, 1, 1}|>],
				LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> {1, 3, 1, 2}|>]}, <|Source -> {1, 3}|>],
			LeafNode[Token`Semi, ";", <|Source -> {1, 4}|>],
			LeafNode[Token`Fake`ImplicitNull, "", <|Source -> After[{1, 4}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20201023-S6K5A5"
]





(*
bug 406933

Used to hang
*)
Test[
	TimeConstrained[
		CodeConcreteParseBox[
	 		RowBox[{
	 			RowBox[{"(*",
	 				RowBox[{":", "History", ":", " ",
	 					RowBox[{"PolynomialPowerMod", " ", "code", " ", "is", " ", "based", " ",
	 						"on", " ", "the", " ", "standard", " ", "package", "\n", "   ",
	 						"Algebra`PolynomialPowerMod`", " ", "originally", " ", "written", " ",
	 						"in", " ", "1988", " ", "by", " ", "Ilan", " ",
	 						RowBox[{"Vardi", "."}]
	 					}]
	 				}], "\n", "*)"
	 			}], "\n", "\n"
	 		}]]
		,
		3.0
	]
	,
	ContainerNode[Box, {
		GroupNode[Comment, {
			LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 1, 1, 1}|>],
			BoxNode[RowBox, {{
				LeafNode[String, ":", <|Source -> {1, 1, 1, 2, 1, 1}|>],
				LeafNode[String, "History", <|Source -> {1, 1, 1, 2, 1, 2}|>],
				LeafNode[String, ":", <|Source -> {1, 1, 1, 2, 1, 3}|>],
				LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 4}|>],
				BoxNode[RowBox, {{
					LeafNode[String, "PolynomialPowerMod", <|Source -> {1, 1, 1, 2, 1, 5, 1, 1}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 2}|>],
					LeafNode[String, "code", <|Source -> {1, 1, 1, 2, 1, 5, 1, 3}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 4}|>],
					LeafNode[String, "is", <|Source -> {1, 1, 1, 2, 1, 5, 1, 5}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 6}|>],
					LeafNode[String, "based", <|Source -> {1, 1, 1, 2, 1, 5, 1, 7}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 8}|>],
					LeafNode[String, "on", <|Source -> {1, 1, 1, 2, 1, 5, 1, 9}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 10}|>],
					LeafNode[String, "the", <|Source -> {1, 1, 1, 2, 1, 5, 1, 11}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 12}|>],
					LeafNode[String, "standard", <|Source -> {1, 1, 1, 2, 1, 5, 1, 13}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 14}|>],
					LeafNode[String, "package", <|Source -> {1, 1, 1, 2, 1, 5, 1, 15}|>],
					LeafNode[String, "\n", <|Source -> {1, 1, 1, 2, 1, 5, 1, 16}|>],
					LeafNode[String, "   ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 17}|>],
					LeafNode[String, "Algebra`PolynomialPowerMod`", <|Source -> {1, 1, 1, 2, 1, 5, 1, 18}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 19}|>],
					LeafNode[String, "originally", <|Source -> {1, 1, 1, 2, 1, 5, 1, 20}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 21}|>],
					LeafNode[String, "written", <|Source -> {1, 1, 1, 2, 1, 5, 1, 22}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 23}|>],
					LeafNode[String, "in", <|Source -> {1, 1, 1, 2, 1, 5, 1, 24}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 25}|>],
					LeafNode[String, "1988", <|Source -> {1, 1, 1, 2, 1, 5, 1, 26}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 27}|>],
					LeafNode[String, "by", <|Source -> {1, 1, 1, 2, 1, 5, 1, 28}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 29}|>],
					LeafNode[String, "Ilan", <|Source -> {1, 1, 1, 2, 1, 5, 1, 30}|>],
					LeafNode[String, " ", <|Source -> {1, 1, 1, 2, 1, 5, 1, 31}|>],
					BoxNode[RowBox, {{
						LeafNode[String, "Vardi", <|Source -> {1, 1, 1, 2, 1, 5, 1, 32, 1, 1}|>],
						LeafNode[String, ".", <|Source -> {1, 1, 1, 2, 1, 5, 1, 32, 1, 2}|>]}}, <|Source -> {1, 1, 1, 2, 1, 5, 1, 32}|>]
				}}, <|Source -> {1, 1, 1, 2, 1, 5}|>]}}, <|Source -> {1, 1, 1, 2}|>],
				LeafNode[String, "\n", <|Source -> {1, 1, 1, 3}|>],
				LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> {1, 1, 1, 4}|>]
			}, <|Source -> {1, 1}|>],
		LeafNode[Token`Newline, "\n", <|Source -> {1, 2}|>],
		LeafNode[Token`Newline, "\n", <|Source -> {1, 3}|>]}, <||>
	]
	,
	TestID->"Boxes-20210318-D7H2V0"
]

Test[
	TimeConstrained[
		CodeConcreteParseBox[RowBox[{"(*", RowBox[{"a", ":", RowBox[{"b", "\n", "c"}], ":"}], "*)"}]]
		,
		3.0
	]
	,
	ContainerNode[Box, {
		GroupNode[Comment, {
			LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> {1, 1}|>],
			BoxNode[RowBox, {{
				LeafNode[String, "a", <|Source -> {1, 2, 1, 1}|>],
				LeafNode[String, ":", <|Source -> {1, 2, 1, 2}|>],
				BoxNode[RowBox, {{
						LeafNode[String, "b", <|Source -> {1, 2, 1, 3, 1, 1}|>],
						LeafNode[String, "\n", <|Source -> {1, 2, 1, 3, 1, 2}|>],
						LeafNode[String, "c", <|Source -> {1, 2, 1, 3, 1, 3}|>]
					}}, <|Source -> {1, 2, 1, 3}|>],
				LeafNode[String, ":", <|Source -> {1, 2, 1, 4}|>]}}, <|Source -> {1, 2}|>],
			LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>
	]
	,
	TestID->"Boxes-20210318-J9V3J3"
]





(*
Used to give:

Failure["InternalUnhandled", <|"Function" -> "parseBox", "Arguments" -> HoldForm[{StringJoin["\\[", CodeParser`RowBox`Private`children[[2]]], {1, 2}}]|>]
*)
Test[
	CodeConcreteParseBox[RowBox[{"xx", RowBox[{RowBox[{"\\[", " ", "EntityEnd", " ", "]"}], "."}]}]]
	,
	ContainerNode[Box, {
			BoxNode[RowBox, {{
				LeafNode[Symbol, "xx", <|Source -> {1, 1}|>],
				ErrorNode[Token`Error`UnhandledCharacter, "\\[", <|
					Source -> {1, 2},
					SyntaxIssues -> {
						SyntaxIssue["UnrecognizedCharacter", "Unrecognized character: ``\\[``.", "Error", <|
							Source -> {1, 2},
							ConfidenceLevel -> 1.,
							CodeActions -> {
								CodeAction["Replace with ``\\\\[``", ReplaceText, <|Source -> {1, 2}, "ReplacementText" -> "\\\\["|>]
							}|>
						]
					}
				|>]
			}}
			,
			<|Source -> {}|>]
		}
		,
		<||>
	]
	,
	TestID->"Boxes-20210319-W8T8G4"
]


