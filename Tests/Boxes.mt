Print["\n===== Start Boxes.mt =====\n"]

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
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 2}]|>],
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
				CodeNode[Evaluated, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
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
	  			CodeNode[Evaluated, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
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
				CodeNode[Evaluated, Function[BoxForm`e$, BoxForm`e$], evaledData]}, <|Source -> {}|>]}, <||>]
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
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20200126-N4V0N6"
]

box = RowBox[{SubsuperscriptBox["\[Integral]", "a", "b"], RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[Integrate, {
			BoxNode[SubsuperscriptBox, {LeafNode[Token`LongName`Integral, "\[Integral]", KeyValuePattern[Source -> {1, 1, 1}]], LeafNode[Symbol, "a", KeyValuePattern[Source -> {1, 1, 2}]], LeafNode[Symbol, "b", KeyValuePattern[Source -> {1, 1, 3}]]}, KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-J0G2S9"
]

box = RowBox[{"\[ContourIntegral]", RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[ContourIntegral, {
			LeafNode[Token`LongName`ContourIntegral, "\[ContourIntegral]", KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-D7J7O3"
]

box = RowBox[{SubsuperscriptBox["\[ContourIntegral]", "a", "b"], RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[ContourIntegral, {
			BoxNode[SubsuperscriptBox, {LeafNode[Token`LongName`ContourIntegral, "\[ContourIntegral]", KeyValuePattern[Source -> {1, 1, 1}]], LeafNode[Symbol, "a", KeyValuePattern[Source -> {1, 1, 2}]], LeafNode[Symbol, "b", KeyValuePattern[Source -> {1, 1, 3}]]}, KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-D8T7D2"
]

box = RowBox[{"\[DoubleContourIntegral]", RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[DoubleContourIntegral, {
			LeafNode[Token`LongName`DoubleContourIntegral, "\[DoubleContourIntegral]", KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-S7W7W7"
]

box = RowBox[{SubsuperscriptBox["\[DoubleContourIntegral]", "a", "b"], RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[DoubleContourIntegral, {
			BoxNode[SubsuperscriptBox, {LeafNode[Token`LongName`DoubleContourIntegral, "\[DoubleContourIntegral]", KeyValuePattern[Source -> {1, 1, 1}]], LeafNode[Symbol, "a", KeyValuePattern[Source -> {1, 1, 2}]], LeafNode[Symbol, "b", KeyValuePattern[Source -> {1, 1, 3}]]}, KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-D0G9F0"
]

box = RowBox[{"\[ClockwiseContourIntegral]", RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[ClockwiseContourIntegral, {
			LeafNode[Token`LongName`ClockwiseContourIntegral, "\[ClockwiseContourIntegral]", KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-R4T2S8"
]

box = RowBox[{SubsuperscriptBox["\[ClockwiseContourIntegral]", "a", "b"], RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[ClockwiseContourIntegral, {
			BoxNode[SubsuperscriptBox, {LeafNode[Token`LongName`ClockwiseContourIntegral, "\[ClockwiseContourIntegral]", KeyValuePattern[Source -> {1, 1, 1}]], LeafNode[Symbol, "a", KeyValuePattern[Source -> {1, 1, 2}]], LeafNode[Symbol, "b", KeyValuePattern[Source -> {1, 1, 3}]]}, KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-R8L7G0"
]

box = RowBox[{"\[CounterClockwiseContourIntegral]", RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[CounterClockwiseContourIntegral, {
			LeafNode[Token`LongName`CounterClockwiseContourIntegral, "\[CounterClockwiseContourIntegral]", KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-V6L1R0"
]

box = RowBox[{SubsuperscriptBox["\[CounterClockwiseContourIntegral]", "a", "b"], RowBox[{RowBox[{"Sin", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]
 
TestMatch[
	CodeConcreteParseBox[box]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[CounterClockwiseContourIntegral, {
			BoxNode[SubsuperscriptBox, {LeafNode[Token`LongName`CounterClockwiseContourIntegral, "\[CounterClockwiseContourIntegral]", KeyValuePattern[Source -> {1, 1, 1}]], LeafNode[Symbol, "a", KeyValuePattern[Source -> {1, 1, 2}]], LeafNode[Symbol, "b", KeyValuePattern[Source -> {1, 1, 3}]]}, KeyValuePattern[Source -> {1, 1}]],
			CallNode[{LeafNode[Symbol, "Sin", <|Source -> {1, 2, 1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 1, 1, 2}|>],
					LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1, 1, 3}|>], 
      				LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 1, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 1}|>], 
  			PrefixNode[DifferentialD, {
  				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", KeyValuePattern[Source -> {1, 2, 1, 2, 1, 1}]],
  				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 2, 1, 2}|>]}, <|Source -> {1, 2, 1, 2}|>]}, <|Source -> {}|>]}, _]
	,
	TestID->"Boxes-20230201-J8F5B2"
]




(*
This is not a concrete error

When abstracting, then it becomes an error
*)
Test[
	CodeConcreteParseBox[RowBox[{"1", ":", "2"}]]
 	,
 	ContainerNode[Box, {
 		SyntaxErrorNode[SyntaxError`ExpectedSymbol, {
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
		SyntaxErrorNode[SyntaxError`UnhandledCharacter, {"\\[", "Alpa", "]"}, _]}, <||>]
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
				CodeNode[Unevaluated, "a", assoc],
				CodeNode[Unevaluated, Sequence[Appearance -> "Horizontal"], assoc]}, <|Source -> {}|>]}, <||>]
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
		ErrorNode[Token`Error`Number, "101101^^2", <|Source -> {}|>]}, <||>]
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


Test[
	CodeConcreteParseBox[RowBox[{"\[Integral]", RowBox[{"x", " ", RowBox[{"Cos", "[", "x", "]"}], RowBox[{"\[DifferentialD]", "x"}]}]}]]
	,
	ContainerNode[Box, {
		PrefixBinaryNode[Integrate, {
			LeafNode[Token`LongName`Integral, "\[Integral]", <|Source -> {1, 1}|>],
			InfixNode[Times, {
				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 1}|>],
				LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2, 1, 2}|>],
				LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 2, 1, 2}]|>],
				CallNode[{LeafNode[Symbol, "Cos", <|Source -> {1, 2, 1, 3, 1, 1}|>]},
					GroupNode[GroupSquare, {
						LeafNode[Token`OpenSquare, "[", <|Source -> {1, 2, 1, 3, 1, 2}|>],
						LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 3, 1, 3}|>],
						LeafNode[Token`CloseSquare, "]", <|Source -> {1, 2, 1, 3, 1, 4}|>]}, <||>], <|Source -> {1, 2, 1, 3}|>]}, <|Source -> {1, 2}|>],
			PrefixNode[DifferentialD, {
				LeafNode[Token`LongName`DifferentialD, "\[DifferentialD]", <|Source -> {1, 2, 1, 4, 1, 1}|>],
				LeafNode[Symbol, "x", <|Source -> {1, 2, 1, 4, 1, 2}|>]}, <|Source -> {1, 2, 1, 4}|>]}, <|Source -> {}|>]}, <||>]
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
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 2}]|>],
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
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 4}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 4}]|>],
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
			LeafNode[Token`Boxes`MultiWhitespace, " ", <|Source -> {1, 2}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 2}]|>],
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
		InfixNode[Times, {
			LeafNode[Symbol, "xx", <|Source -> {1, 1}|>],
			LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> After[{1, 1}]|>],
			InfixNode[Dot, {
				SyntaxErrorNode[SyntaxError`UnhandledCharacter, {"\\[", " ", "EntityEnd", " ", "]"}, <|Source -> {1, 2, 1, 1}|>],
				LeafNode[Token`Dot, ".", <|Source -> {1, 2, 1, 2}|>],
				ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 2, 1, 2}]|>]}, <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20210319-W8T8G4"
]




box = 
  RowBox[{"<<", 
    InterpretationBox[
     DynamicModuleBox[{Typeset`open = False}, 
      TemplateBox[{"String", 
        StyleBox["\"RegisterMachine`\"", "IconizedCustomName", 
         StripOnInput -> False], 
        GridBox[{{RowBox[{TagBox["\"Head: \"", "IconizedLabel"], 
             "\[InvisibleSpace]", 
             TagBox["String", "IconizedItem"]}]}, {RowBox[{TagBox[
              "\"String length: \"", "IconizedLabel"], 
             "\[InvisibleSpace]", 
             TagBox["49", "IconizedItem"]}]}, {RowBox[{TagBox[
              "\"Byte count: \"", "IconizedLabel"], 
             "\[InvisibleSpace]", TagBox["80", "IconizedItem"]}]}}, 
         GridBoxAlignment -> {"Columns" -> {{Left}}}, 
         DefaultBaseStyle -> "Column", 
         GridBoxItemSize -> {"Columns" -> {{Automatic}}, 
           "Rows" -> {{Automatic}}}], Dynamic[Typeset`open]}, 
       "IconizedObject"]], 
     "VirtualMachine`Machines`Abstract`RegisterMachine`", 
     SelectWithContents -> True, Selectable -> False]}];


(*
Test doing << foo where foo is a complicated box structure
*)
Test[
	CodeConcreteParseBox[box]
	,
With[{evaledData = <||>},
	ContainerNode[Box, {PrefixNode[
   Get, {LeafNode[Token`LessLess, "<<", <|Source -> {1, 1}|>], 
    BoxNode[
     InterpretationBox, {CodeNode[Unevaluated, 
       DynamicModuleBox[{Typeset`open = False}, 
        TemplateBox[{"String", 
          StyleBox["\"RegisterMachine`\"", "IconizedCustomName", 
           StripOnInput -> False], 
          GridBox[{{RowBox[{TagBox["\"Head: \"", "IconizedLabel"], 
               "\[InvisibleSpace]", 
               TagBox["String", "IconizedItem"]}]}, {RowBox[{TagBox[
                "\"String length: \"", "IconizedLabel"], 
               "\[InvisibleSpace]", 
               TagBox["49", "IconizedItem"]}]}, {RowBox[{TagBox[
                "\"Byte count: \"", "IconizedLabel"], 
               "\[InvisibleSpace]", TagBox["80", "IconizedItem"]}]}}, 
           GridBoxAlignment -> {"Columns" -> {{Left}}}, 
           DefaultBaseStyle -> "Column", 
           GridBoxItemSize -> {"Columns" -> {{Automatic}}, 
             "Rows" -> {{Automatic}}}], Dynamic[Typeset`open]}, 
         "IconizedObject"]], evaledData], 
      CodeNode[Unevaluated, 
       "VirtualMachine`Machines`Abstract`RegisterMachine`", evaledData], 
      CodeNode[Unevaluated, SelectWithContents -> True, evaledData], 
      CodeNode[Unevaluated, 
       Selectable -> False, evaledData]}, <|Source -> {1, 
        2}|>]}, <|Source -> {}|>]}, <||>]
]
	,
	TestID->"Boxes-20210909-O7R0T0"
]







box = RowBox[{"a", "~", "Join", "~", "b", "~", "Join", "~", "c"}]

cst = CodeConcreteParseBox[box]

Test[
	cst
	,
	ContainerNode[Box, {
		InfixNode[InfixTilde, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "Join", <|Source -> {1, 3}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {1, 4}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 5}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {1, 6}|>],
			LeafNode[Symbol, "Join", <|Source -> {1, 7}|>],
			LeafNode[Token`Tilde, "~", <|Source -> {1, 8}|>],
			LeafNode[Symbol, "c", <|Source -> {1, 9}|>]}, <|Source -> {}|>]}
		,
		<||>
	]
	,
	TestID->"Boxes-20210916-E0P0D1"
]

agg = CodeParser`Abstract`Aggregate[cst]

ast = CodeParser`Abstract`Abstract[agg]

Test[
	ast
	,
	ContainerNode[Box, {
		CallNode[LeafNode[Symbol, "Join", <|Source -> {1, 7}|>], {
			CallNode[LeafNode[Symbol, "Join", <|Source -> {1, 3}|>], {
				LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
				LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>],
			LeafNode[Symbol, "c", <|Source -> {1, 9}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20210916-O9X3M0"
]










Test[
	ToStandardFormBoxes[
 ContainerNode[
  Box, {BinaryNode[
    Optional, {BinaryNode[
      Pattern, {LeafNode[Symbol, "a", <||>], 
       LeafNode[Token`Colon, ":", <||>], 
       LeafNode[Symbol, "b", <||>]}, <||>], 
     LeafNode[Token`Colon, ":", <||>], 
     LeafNode[Symbol, "c", <||>]}, <||>]}, <||>]]
	,
	RowBox[{"a", ":", "b", ":", "c"}]
	,
	TestID->"Boxes-20211019-X1T9N3"
]


Test[
	ToStandardFormBoxes[
 ContainerNode[
  Box, {TernaryNode[
    TernaryTilde, {TernaryNode[
      TernaryTilde, {LeafNode[Symbol, "a", <||>], 
       LeafNode[Token`Tilde, "~", <||>], LeafNode[Symbol, "b", <||>], 
       LeafNode[Token`Tilde, "~", <||>], 
       LeafNode[Symbol, "c", <||>]}, <||>], 
     LeafNode[Token`Tilde, "~", <||>], LeafNode[Symbol, "d", <||>], 
     LeafNode[Token`Tilde, "~", <||>], 
     LeafNode[Symbol, "e", <||>]}, <||>]}, <||>]]
	,
	RowBox[{"a", "~", "b", "~", "c", "~", "d", "~", "e"}]
	,
	TestID->"Boxes-20211019-Y5H1T8"
]


Test[
	CodeConcreteParseBox[RowBox[{RowBox[{"g", "[", "]"}], ":="}]]
	,
	ContainerNode[Box, {
		BinaryNode[SetDelayed, {
			CallNode[{LeafNode[Symbol, "g", <|Source -> {1, 1, 1, 1}|>]},
				GroupNode[GroupSquare, {
					LeafNode[Token`OpenSquare, "[", <|Source -> {1, 1, 1, 2}|>],
					LeafNode[Token`CloseSquare, "]", <|Source -> {1, 1, 1, 3}|>]}, <||>], <|Source -> {1, 1}|>],
			LeafNode[Token`ColonEqual, ":=", <|Source -> {1, 2}|>],
			ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 2}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220618-S1C8B4"
]


(*
bug 426013
*)
Test[
	CodeConcreteParseBox[""]
	,
	ContainerNode[Box, {Missing["EmptyInput"]}, <||>]
	,
	TestID->"Boxes-20220711-Q7R8Q8"
]


(*
GIGO
*)
Test[
	CodeConcreteParseBox[RowBox[{"a", ":::", "b"}]]
	,
	ContainerNode[Box, {
		InfixNode[MessageName, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>],
			LeafNode[Token`ColonColon, "::", <|Source -> {1, 2}|>],
			LeafNode[Symbol, "b", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220829-S5T6B6"
]



(*
This was a real case of TagBox[""] causing problems

"" is not a valid box, so technically this is not valid syntax

But this is testing that there are no messages spews
*)

box = RowBox[{"CUDADilation", "[", 
      RowBox[{
         RowBox[{
            GraphicsBox[
              TagBox[RasterBox[CompressedData["
1:eJzt2O1twyAQBmDTTbpSR0gHyDadt82fRob7hPfFWOKkSgkcd09c2zH5fDy/
Hh/HcXz//f0cO3bs2DEryivmdIk2Ku+YYQr1KSWVjjD5nVLJOJPdKZUMRVmd
cp8BitI7JY8sFqW2Ok2TWDrKUYnvJqjkVtUcRWWhFlWJvSaobNQaqmbEWDNN
pQ6Ja87ZNJU1Jq6pXl+r4n/j3EYV6EZGdZ3tLQuMau8MwXZm1jhVOEdiB8Ex
jcF6UbF6FNZ4tSVUkMPtqCC14CxnGVWV3k28l2iFTu+wrNACcdikD7Gi6cK4
ggS44rm6CvFAGDUZ34EEVThuoSrNTJ24pIrKiKtg+wyjQtNdV8k3vkGSWKOd
MpqyUHUVacrIh6nsMtKU1ZSFEk6U/1fCGqceDNVcVc3L2NXRbXIfFzRVpGov
yX+06lQNRsVYQyUrRNXYidKlakaajIkoXaV8vSQKUVWBC71UMUOl7Rw0E8AV
UdknuoQaZen/r2QBKitZVTONss5VskUNFeU3tWEUgwVA4Vn9K4ksEAp0xoNR
UFbXIoIqf0sQVaidRNXFaOyr9EFoOPXl/myV97HlWbKqRpl76lnPri3K3rpP
UUko83eOC1XGb0IzVArKOFgelqlyD5b1Ma5QvebM/ItUdvokVYS1VWEWFnVr
lccCoyL3dp8FR0VVtG1qhhXnc1BhVfio0liDfg4LkAlnxVOZpnMzdqdcrGja
sWMHLn4B6FERVg==
       "], {{0, 99}, {150, 0}}, {0, 1},
                  ColorFunction -> GrayLevel],
                
          BoxForm`ImageTag["Bit", ColorSpace -> Automatic, 
           ImageSize -> Automatic, Interleaving -> None],
                Selectable -> False],
              BaseStyle -> "ImageGraphics",
              ImageSize -> Magnification[1],
              ImageSizeRaw -> {150, 99},
              PlotRange -> {{0, 150}, {0, 99}}], 
            TagBox[
              TagBox["",
                {"Bit", ColorSpace -> Automatic, 
           ImageResolution -> Automatic, ImageSize -> Automatic, 
           Interleaving -> 
                   None, Magnification -> Automatic}],
              "Image"]}], ",", "3"}], "]"}];

Test[
	CodeConcreteParseBox[box]
	,
With[{evaledAssoc = <||>},
	ContainerNode[Box, {CallNode[{LeafNode[Symbol, 
     "CUDADilation", <|Source -> {1, 1}|>]}, 
   GroupNode[
    GroupSquare, {LeafNode[Token`OpenSquare, 
      "[", <|Source -> {1, 2}|>], 
     InfixNode[
      Comma, {InfixNode[
        Times, {BoxNode[
          GraphicsBox, {CodeNode[Unevaluated, 
            TagBox[RasterBox[CompressedData["
1:eJzt2O1twyAQBmDTTbpSR0gHyDadt82fRob7hPfFWOKkSgkcd09c2zH5fDy/
Hh/HcXz//f0cO3bs2DEryivmdIk2Ku+YYQr1KSWVjjD5nVLJOJPdKZUMRVmd
cp8BitI7JY8sFqW2Ok2TWDrKUYnvJqjkVtUcRWWhFlWJvSaobNQaqmbEWDNN
pQ6Ja87ZNJU1Jq6pXl+r4n/j3EYV6EZGdZ3tLQuMau8MwXZm1jhVOEdiB8Ex
jcF6UbF6FNZ4tSVUkMPtqCC14CxnGVWV3k28l2iFTu+wrNACcdikD7Gi6cK4
ggS44rm6CvFAGDUZ34EEVThuoSrNTJ24pIrKiKtg+wyjQtNdV8k3vkGSWKOd
MpqyUHUVacrIh6nsMtKU1ZSFEk6U/1fCGqceDNVcVc3L2NXRbXIfFzRVpGov
yX+06lQNRsVYQyUrRNXYidKlakaajIkoXaV8vSQKUVWBC71UMUOl7Rw0E8AV
UdknuoQaZen/r2QBKitZVTONss5VskUNFeU3tWEUgwVA4Vn9K4ksEAp0xoNR
UFbXIoIqf0sQVaidRNXFaOyr9EFoOPXl/myV97HlWbKqRpl76lnPri3K3rpP
UUko83eOC1XGb0IzVArKOFgelqlyD5b1Ma5QvebM/ItUdvokVYS1VWEWFnVr
lccCoyL3dp8FR0VVtG1qhhXnc1BhVfio0liDfg4LkAlnxVOZpnMzdqdcrGja
sWMHLn4B6FERVg==
       "], {{0, 99}, {150, 0}}, {0, 1}, ColorFunction -> GrayLevel], 
             BoxForm`ImageTag["Bit", ColorSpace -> Automatic, 
              ImageSize -> Automatic, Interleaving -> None], 
             Selectable -> False], evaledAssoc], 
           CodeNode[Unevaluated, BaseStyle -> "ImageGraphics", evaledAssoc], 
           CodeNode[Unevaluated, ImageSize -> Magnification[1], evaledAssoc], 
           CodeNode[Unevaluated, ImageSizeRaw -> {150, 99}, evaledAssoc], 
           CodeNode[Unevaluated, 
            PlotRange -> {{0, 150}, {0, 99}}, evaledAssoc]}, <|Source -> {1, 
             3, 1, 1, 1, 1}|>], 
         LeafNode[Token`Fake`ImplicitTimes, 
          "", <|Source -> After[{1, 3, 1, 1, 1, 1}]|>], 
         BoxNode[TagBox, {BoxNode[
            TagBox, {Missing["EmptyInput"], 
             CodeNode[
              Evaluated, {"Bit", ColorSpace -> Automatic, 
               ImageResolution -> Automatic, ImageSize -> Automatic, 
               Interleaving -> None, 
               Magnification -> Automatic}, evaledAssoc]}, <|Source -> {1, 3,
                1, 1, 1, 2, 1}|>], 
           CodeNode[Evaluated, 
            "Image", evaledAssoc]}, <|Source -> {1, 3, 1, 1, 1, 
             2}|>]}, <|Source -> {1, 3, 1, 1}|>], 
       LeafNode[Token`Comma, ",", <|Source -> {1, 3, 1, 2}|>], 
       LeafNode[Integer, 
        "3", <|Source -> {1, 3, 1, 3}|>]}, <|Source -> {1, 3}|>], 
     LeafNode[Token`CloseSquare, 
      "]", <|Source -> {1, 4}|>]}, <||>], <|Source -> {}|>]}, <||>]
]
	,
	TestID->"Boxes-20220916-H7M0J2"
]





cst = CodeConcreteParseBox[RowBox[{"a", "}"}]]

agg = CodeParser`Abstract`Aggregate[cst]

ast = CodeParser`Abstract`Abstract[agg]

Test[
	ast
	,
	ContainerNode[Box, {
		GroupMissingOpenerNode[List, {
			LeafNode[Symbol, "a", <|Source -> {1, 1}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-S2F0I7"
]


Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 2}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-K5B6F0"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "f", <|Source -> {1, 3}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-M5Q4S4"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 4}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-C3X9B3"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-N6B4D4"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"~", "f"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> Before[{1, 1}]|>], LeafNode[Symbol, "f", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-K5Q4Y8"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"~", "f", "~"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 2}|>], {ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> Before[{1, 1}]|>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 3}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-O5U0L5"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"~", "f", "~", "b"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 2}|>], {ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> Before[{1, 1}]|>], LeafNode[Symbol, "b", <|Source -> {1, 4}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-B0W6B8"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"~", "b"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> Before[{1, 1}]|>], LeafNode[Symbol, "b", <|Source -> {1, 2}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-G4G5Q5"
]



Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 6}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-U2T0V7"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], LeafNode[Symbol, "c", <|Source -> {1, 7}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-E6P8D8"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c", "~"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "c", <|Source -> {1, 7}|>], {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 8}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-X9S9P2"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c", "~", "d"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "c", <|Source -> {1, 7}|>], {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], LeafNode[Symbol, "d", <|Source -> {1, 9}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-U4F5R7"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c", "~", "d", "~"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {CallNode[LeafNode[Symbol, "c", <|Source -> {1, 7}|>], {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], LeafNode[Symbol, "d", <|Source -> {1, 9}|>]}, <||>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 10}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-K9S2V4"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c", "~", "d", "~", "e"}]]]]
	,
	ContainerNode[Box, {AbstractSyntaxErrorNode[AbstractSyntaxError`ExpectedTilde, {CallNode[LeafNode[Symbol, "c", <|Source -> {1, 7}|>], {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], LeafNode[Symbol, "d", <|Source -> {1, 9}|>]}, <||>], LeafNode[Symbol, "e", <|Source -> {1, 11}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-X3Y1H3"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c", "~", "d", "~", "e", "~"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "e", <|Source -> {1, 11}|>], {CallNode[LeafNode[Symbol, "c", <|Source -> {1, 7}|>], {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], LeafNode[Symbol, "d", <|Source -> {1, 9}|>]}, <||>], ErrorNode[Token`Error`ExpectedOperand, "", <|Source -> After[{1, 12}]|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-W1A7X3"
]

Test[
	CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[CodeConcreteParseBox[RowBox[{"a", "~", "f", "~", "b", "~", "c", "~", "d", "~", "e", "~", "f"}]]]]
	,
	ContainerNode[Box, {CallNode[LeafNode[Symbol, "e", <|Source -> {1, 11}|>], {CallNode[LeafNode[Symbol, "c", <|Source -> {1, 7}|>], {CallNode[LeafNode[Symbol, "f", <|Source -> {1, 3}|>], {LeafNode[Symbol, "a", <|Source -> {1, 1}|>], LeafNode[Symbol, "b", <|Source -> {1, 5}|>]}, <||>], LeafNode[Symbol, "d", <|Source -> {1, 9}|>]}, <||>], LeafNode[Symbol, "f", <|Source -> {1, 13}|>]}, <|Source -> {}|>]}, <||>]
	,
	TestID->"Boxes-20220919-G2G8R3"
]



Module[{
	cst,
	agg,
	ast
},
	cst = CodeConcreteParseBox[
		RowBox[{"{", RowBox[{"E", "-", "1"}], "}"}]
	];

	Test[
		cst,
		ContainerNode[Box, {
			GroupNode[
				List,
				{
					LeafNode[Token`OpenCurly, "{", <|Source -> {1, 1}|>],
					InfixNode[
						Plus,
						{
							LeafNode[Symbol, "E", <|Source -> {1, 2, 1, 1}|>],
							LeafNode[Token`Minus, "-", <|Source -> {1, 2, 1, 2}|>],
							LeafNode[Integer, "1", <|Source -> {1, 2, 1, 3}|>]
						},
						<|Source -> {1, 2}|>
					],
					LeafNode[Token`CloseCurly, "}", <|Source -> {1, 3}|>]
				},
				<|Source -> {}|>
			]
		}, <||>]
	];

	agg = CodeParser`Abstract`Aggregate[cst];

	Test[agg, cst];

	ast = CodeParser`Abstract`Abstract[cst];

	(* TID:20231031/1: Synthetic box source for process plus pair *)
	Test[
		ast,
		ContainerNode[Box, {
			CallNode[
				LeafNode[Symbol, "List", <||>],
				{
					CallNode[
						LeafNode[Symbol, "Plus", <||>],
						{
							LeafNode[Symbol, "E", <|Source -> {1, 2, 1, 1}|>],
							LeafNode[Integer, "-1", <|Source -> {1, 2}|>]
						},
						<|Source -> {1, 2}|>
					]
				},
				<|Source -> {}|>
			]
		}, <||>],
		TestID -> "Boxes-20231031-1"
	]
]









Module[{
	cst
},
	(* Test CodeConcreteParse of Cell. *)
	Test[
		cst = CodeConcreteParse @ Cell[BoxData[FractionBox["1", "0"]], "Input"]
		,
		ContainerNode[Cell, {
			BoxNode[
				FractionBox,
				{
					LeafNode[Integer,"1",<|Source -> {1}|>],
					LeafNode[Integer, "0", <| Source -> {2}|>]
				},
				<|Source -> {}|>
			]
		},
			<||>
		]

	];

	(* Test Aggregate of ContainerNode[Cell, ...] *)
	Test[
		CodeParser`Abstract`Aggregate[cst]
		,
		ContainerNode[Cell, {
			BoxNode[FractionBox, {
				LeafNode[Integer, "1", <|Source -> {1}|>],
				LeafNode[Integer, "0", <|Source -> {2}|>]
			}, <|Source -> {}|>]
		}, <||>]
	]
]