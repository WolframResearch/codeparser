BeginPackage["AST`Generate`Symbol`"]

Begin["`Private`"]

Needs["AST`Generate`"]




(*
Bug 321344:

ExportString["String", "String"] returns ""

checkBug321344[] sets the flag $WorkaroundBug321344 to True if we still need to workaround bug 321344
*)
checkBug321344[] :=
Module[{res},
  res = ExportString["String", "String"];
  Switch[res,
    "",
    True
    ,
    "String",
    False
    ,
    _,
    Print["Unhandled result while checking bug 321344: ", res];
    Quit[1]
  ]
]






PrefixOperatorToSymbol[Token`Bang] = Not
PrefixOperatorToSymbol[Token`PlusPlus] = PreIncrement
PrefixOperatorToSymbol[Token`LessLess] = Get
PrefixOperatorToSymbol[Token`MinusMinus] = PreDecrement
PrefixOperatorToSymbol[Token`Plus] = Plus
PrefixOperatorToSymbol[Token`Minus] = Minus
PrefixOperatorToSymbol[Token`Question] = Information
PrefixOperatorToSymbol[Token`QuestionQuestion] = Information
PrefixOperatorToSymbol[Token`BangBang] = AST`PrefixNot2

PrefixOperatorToSymbol[Token`LongName`Sqrt] = Sqrt
PrefixOperatorToSymbol[Token`LongName`Not] = Not
PrefixOperatorToSymbol[Token`LongName`PlusMinus] = PlusMinus
PrefixOperatorToSymbol[Token`LongName`Sum] = Sum
PrefixOperatorToSymbol[Token`LongName`MinusPlus] = MinusPlus
PrefixOperatorToSymbol[Token`LongName`DifferentialD] = DifferentialD
PrefixOperatorToSymbol[Token`LongName`CapitalDifferentialD] = CapitalDifferentialD
PrefixOperatorToSymbol[Token`LongName`Minus] = Minus
PrefixOperatorToSymbol[Token`LongName`Del] = Del
PrefixOperatorToSymbol[Token`LongName`Square] = Square
PrefixOperatorToSymbol[Token`LongName`Integral] = Integral
PrefixOperatorToSymbol[Token`LongName`ContourIntegral] = ContourIntegral
PrefixOperatorToSymbol[Token`LongName`DoubleContourIntegral] = DoubleContourIntegral
PrefixOperatorToSymbol[Token`LongName`ClockwiseContourIntegral] = ClockwiseContourIntegral
PrefixOperatorToSymbol[Token`LongName`CounterClockwiseContourIntegral] = CounterClockwiseContourIntegral
PrefixOperatorToSymbol[Token`LongName`Product] = Product
PrefixOperatorToSymbol[Token`LongName`ContinuedFractionK] = ContinuedFractionK
PrefixOperatorToSymbol[Token`LongName`CircleTimes] = CircleTimes
PrefixOperatorToSymbol[Token`LongName`ForAll] = ForAll
PrefixOperatorToSymbol[Token`LongName`Exists] = Exists
PrefixOperatorToSymbol[Token`LongName`NotExists] = NotExists
PrefixOperatorToSymbol[Token`LongName`Coproduct] = Coproduct
PrefixOperatorToSymbol[Token`LongName`Piecewise] = Piecewise
PrefixOperatorToSymbol[Token`LongName`InvisiblePrefixScriptBase] = System`InvisiblePrefixScriptBase
PrefixOperatorToSymbol[Token`LongName`ExpectationE] = ExpectationE
PrefixOperatorToSymbol[Token`LongName`CubeRoot] = CubeRoot
PrefixOperatorToSymbol[Token`LongName`ProbabilityPr] = ProbabilityPr

PrefixOperatorToSymbol[Token`LinearSyntax`Bang] = AST`PrefixLinearSyntaxBang



PostfixOperatorToSymbol[Token`DotDot] = Repeated
PostfixOperatorToSymbol[Token`Bang] = Factorial
PostfixOperatorToSymbol[Token`MinusMinus] = Decrement
PostfixOperatorToSymbol[Token`PlusPlus] = Increment
PostfixOperatorToSymbol[Token`DotDotDot] = RepeatedNull
PostfixOperatorToSymbol[Token`Amp] = Function
PostfixOperatorToSymbol[Token`BangBang] = Factorial2
PostfixOperatorToSymbol[Token`SingleQuote] = Derivative
   
PostfixOperatorToSymbol[Token`LongName`Transpose] = Transpose
PostfixOperatorToSymbol[Token`LongName`Conjugate] = Conjugate
PostfixOperatorToSymbol[Token`LongName`ConjugateTranspose] = ConjugateTranspose
PostfixOperatorToSymbol[Token`LongName`HermitianConjugate] = System`HermitianConjugate
PostfixOperatorToSymbol[Token`LongName`InvisiblePostfixScriptBase] = System`InvisiblePostfixScriptBase


(*
Binary
*)

BinaryOperatorToSymbol[Token`LongName`Because] = Because
BinaryOperatorToSymbol[Token`LongName`Therefore] = Therefore

BinaryOperatorToSymbol[Token`LongName`RightTee] = RightTee
BinaryOperatorToSymbol[Token`LongName`LeftTee] = LeftTee
BinaryOperatorToSymbol[Token`LongName`DoubleRightTee] = DoubleRightTee
BinaryOperatorToSymbol[Token`LongName`DoubleLeftTee] = DoubleLeftTee
BinaryOperatorToSymbol[Token`LongName`UpTee] = UpTee
BinaryOperatorToSymbol[Token`LongName`DownTee] = DownTee

BinaryOperatorToSymbol[Token`Minus] = Minus
BinaryOperatorToSymbol[Token`Slash] = Divide
BinaryOperatorToSymbol[Token`SlashAt] = Map
BinaryOperatorToSymbol[Token`Equal] = Set
BinaryOperatorToSymbol[Token`Caret] = Power
BinaryOperatorToSymbol[Token`ColonEqual] = SetDelayed
BinaryOperatorToSymbol[Token`MinusGreater] = Rule
BinaryOperatorToSymbol[Token`Question] = PatternTest
BinaryOperatorToSymbol[Token`AtAt] = Apply
BinaryOperatorToSymbol[Token`SlashSemi] = Condition
BinaryOperatorToSymbol[Token`SlashDot] = ReplaceAll
BinaryOperatorToSymbol[Token`ColonGreater] = RuleDelayed
BinaryOperatorToSymbol[Token`SlashSlashDot] = ReplaceRepeated
BinaryOperatorToSymbol[Token`PlusEqual] = AddTo
BinaryOperatorToSymbol[Token`StarEqual] = TimesBy
BinaryOperatorToSymbol[Token`MinusEqual] = SubtractFrom
BinaryOperatorToSymbol[Token`SlashEqual] = DivideBy
BinaryOperatorToSymbol[Token`CaretEqual] = UpSet
BinaryOperatorToSymbol[Token`CaretColonEqual] = UpSetDelayed
(*
TwoWayRule was added in 11.1, but we support building with 11.0
force symbol to be in System`
*)
BinaryOperatorToSymbol[Token`LessMinusGreater] = System`TwoWayRule
BinaryOperatorToSymbol[Token`SlashSlashAt] = MapAll
BinaryOperatorToSymbol[Token`GreaterGreater] = Put
BinaryOperatorToSymbol[Token`GreaterGreaterGreater] = PutAppend
BinaryOperatorToSymbol[Token`SlashSlash] = AST`BinarySlashSlash
BinaryOperatorToSymbol[Token`SemiSemi] = Span
BinaryOperatorToSymbol[Token`At] = AST`BinaryAt
BinaryOperatorToSymbol[Token`AtAtAt] = AST`BinaryAtAtAt
BinaryOperatorToSymbol[Token`EqualDot] = Unset

BinaryOperatorToSymbol[Token`LongName`Divide] = Divide
BinaryOperatorToSymbol[Token`LongName`DivisionSlash] = Divide

BinaryOperatorToSymbol[Token`LongName`Implies] = Implies
BinaryOperatorToSymbol[Token`LongName`RoundImplies] = RoundImplies
BinaryOperatorToSymbol[Token`LongName`PlusMinus] = PlusMinus
BinaryOperatorToSymbol[Token`LongName`DirectedEdge] = DirectedEdge
BinaryOperatorToSymbol[Token`LongName`Rule] = Rule
BinaryOperatorToSymbol[Token`LongName`RuleDelayed] = RuleDelayed
BinaryOperatorToSymbol[Token`LongName`UndirectedEdge] = UndirectedEdge
BinaryOperatorToSymbol[Token`LongName`Function] = Function
BinaryOperatorToSymbol[Token`LongName`MinusPlus] = MinusPlus
(*
force TwoWayRule to be in System`
*)
BinaryOperatorToSymbol[Token`LongName`TwoWayRule] = System`TwoWayRule
(*
Use BinaryAt here
*)
BinaryOperatorToSymbol[Token`LongName`InvisibleApplication] = AST`BinaryAt
BinaryOperatorToSymbol[Token`LongName`CircleMinus] = CircleMinus

BinaryOperatorToSymbol[Token`LongName`SuchThat] = SuchThat
BinaryOperatorToSymbol[Token`LongName`Perpendicular] = Perpendicular

BinaryOperatorToSymbol[Token`LongName`Minus] = Minus




(*
Infix
*)
InfixOperatorToSymbol[Token`Semi] = CompoundExpression
InfixOperatorToSymbol[Token`Comma] = AST`Comma

InfixOperatorToSymbol[Token`LongName`InvisibleComma] = AST`Comma

InfixOperatorToSymbol[Token`EqualEqual] = Equal
InfixOperatorToSymbol[Token`BangEqual] = Unequal
InfixOperatorToSymbol[Token`Less] = Less
InfixOperatorToSymbol[Token`Greater] = Greater
InfixOperatorToSymbol[Token`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`GreaterEqual] = GreaterEqual
InfixOperatorToSymbol[Token`LongName`LessFullEqual] = LessFullEqual
InfixOperatorToSymbol[Token`LongName`NotLessFullEqual] = NotLessFullEqual
InfixOperatorToSymbol[Token`LongName`LessEqualGreater] = LessEqualGreater
InfixOperatorToSymbol[Token`LongName`GreaterEqualLess] = GreaterEqualLess

InfixOperatorToSymbol[Token`LongName`NestedLessLess] = NestedLessLess
InfixOperatorToSymbol[Token`LongName`NotNestedLessLess] = NotNestedLessLess

InfixOperatorToSymbol[Token`LongName`NotLess] = NotLess
InfixOperatorToSymbol[Token`LongName`NotGreater] = NotGreater

InfixOperatorToSymbol[Token`LongName`NotLessLess] = NotLessLess
InfixOperatorToSymbol[Token`LongName`LongEqual] = Equal
InfixOperatorToSymbol[Token`LongName`Equal] = Equal

InfixOperatorToSymbol[Token`LongName`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`LongName`GreaterEqual] = GreaterEqual
InfixOperatorToSymbol[Token`LongName`NotLessEqual] = NotLessEqual
InfixOperatorToSymbol[Token`LongName`NotGreaterEqual] = NotGreaterEqual

InfixOperatorToSymbol[Token`LongName`NotEqual] = Unequal
(*
force to be in System`:
VectorGreater
VectorGreaterEqual
VectorLess
VectorLessEqual
*)
InfixOperatorToSymbol[Token`LongName`VectorGreater] = System`VectorGreater
InfixOperatorToSymbol[Token`LongName`VectorGreaterEqual] = System`VectorGreaterEqual
InfixOperatorToSymbol[Token`LongName`VectorLess] = System`VectorLess
InfixOperatorToSymbol[Token`LongName`VectorLessEqual] = System`VectorLessEqual

InfixOperatorToSymbol[Token`LongName`NotEqualTilde] = NotEqualTilde
InfixOperatorToSymbol[Token`LongName`NotHumpEqual] = NotHumpEqual
InfixOperatorToSymbol[Token`LongName`NotHumpDownHump] = NotHumpDownHump

InfixOperatorToSymbol[Token`LongName`LeftTriangleBar] = LeftTriangleBar
InfixOperatorToSymbol[Token`LongName`RightTriangleBar] = RightTriangleBar

InfixOperatorToSymbol[Token`LongName`NotLeftTriangleBar] = NotLeftTriangleBar
InfixOperatorToSymbol[Token`LongName`NotRightTriangleBar] = NotRightTriangleBar
InfixOperatorToSymbol[Token`LongName`NotNestedLessLess] = NotNestedLessLess
InfixOperatorToSymbol[Token`LongName`NotLessSlantEqual] = NotLessSlantEqual
InfixOperatorToSymbol[Token`LongName`NotGreaterGreater] = NotGreaterGreater
InfixOperatorToSymbol[Token`LongName`NotNestedGreaterGreater] = NotNestedGreaterGreater
InfixOperatorToSymbol[Token`LongName`NotGreaterSlantEqual] = NotGreaterSlantEqual

(* other flattening operators *)
InfixOperatorToSymbol[Token`EqualEqualEqual] = SameQ
InfixOperatorToSymbol[Token`EqualBangEqual] = UnsameQ

InfixOperatorToSymbol[Token`AtStar] = Composition
InfixOperatorToSymbol[Token`SlashStar] = RightComposition

(* set relation operators *)
InfixOperatorToSymbol[Token`LongName`Element] = Element
InfixOperatorToSymbol[Token`LongName`Subset] = Subset
InfixOperatorToSymbol[Token`LongName`Superset] = Superset
InfixOperatorToSymbol[Token`LongName`SubsetEqual] = SubsetEqual
InfixOperatorToSymbol[Token`LongName`SupersetEqual] = SupersetEqual
InfixOperatorToSymbol[Token`LongName`NotElement] = NotElement
InfixOperatorToSymbol[Token`LongName`NotSubset] = NotSubset
InfixOperatorToSymbol[Token`LongName`NotSuperset] = NotSuperset
InfixOperatorToSymbol[Token`LongName`NotSubsetEqual] = NotSubsetEqual
InfixOperatorToSymbol[Token`LongName`NotSupersetEqual] = NotSupersetEqual

InfixOperatorToSymbol[Token`LongName`SquareSubset] = SquareSubset
InfixOperatorToSymbol[Token`LongName`SquareSuperset] = SquareSuperset
InfixOperatorToSymbol[Token`LongName`NotSquareSubset] = NotSquareSubset
InfixOperatorToSymbol[Token`LongName`NotSquareSuperset] = NotSquareSuperset
InfixOperatorToSymbol[Token`LongName`SquareSubsetEqual] = SquareSubsetEqual
InfixOperatorToSymbol[Token`LongName`SquareSupersetEqual] = SquareSupersetEqual
InfixOperatorToSymbol[Token`LongName`NotSquareSubsetEqual] = NotSquareSubsetEqual
InfixOperatorToSymbol[Token`LongName`NotSquareSupersetEqual] = NotSquareSupersetEqual

(* Plus and Times *)
InfixOperatorToSymbol[Token`Plus] = Plus

InfixOperatorToSymbol[Token`LongName`ImplicitPlus] = Plus

InfixOperatorToSymbol[Token`Star] = Times

InfixOperatorToSymbol[Token`Fake`ImplicitTimes] = Times

InfixOperatorToSymbol[Token`LongName`Times] = Times
InfixOperatorToSymbol[Token`LongName`InvisibleTimes] = Times

InfixOperatorToSymbol[Token`StarStar] = NonCommutativeMultiply
InfixOperatorToSymbol[Token`Dot] = Dot
InfixOperatorToSymbol[Token`AmpAmp] = And
InfixOperatorToSymbol[Token`Bar] = Alternatives
InfixOperatorToSymbol[Token`BarBar] = Or
InfixOperatorToSymbol[Token`LessGreater] = StringJoin
InfixOperatorToSymbol[Token`TildeTilde] = StringExpression
InfixOperatorToSymbol[Token`ColonColon] = MessageName

InfixOperatorToSymbol[Token`LongName`And] = And
InfixOperatorToSymbol[Token`LongName`Or] = Or
InfixOperatorToSymbol[Token`LongName`Xor] = Xor
InfixOperatorToSymbol[Token`LongName`Nand] = Nand
InfixOperatorToSymbol[Token`LongName`Nor] = Nor
InfixOperatorToSymbol[Token`LongName`Xnor] = Xnor

InfixOperatorToSymbol[Token`LongName`LeftArrow] = LeftArrow
InfixOperatorToSymbol[Token`LongName`RightArrow] = RightArrow
InfixOperatorToSymbol[Token`LongName`LeftRightArrow] = LeftRightArrow
InfixOperatorToSymbol[Token`LongName`UpperLeftArrow] = UpperLeftArrow
InfixOperatorToSymbol[Token`LongName`UpperRightArrow] = UpperRightArrow
InfixOperatorToSymbol[Token`LongName`LowerRightArrow] = LowerRightArrow
InfixOperatorToSymbol[Token`LongName`LowerLeftArrow] = LowerLeftArrow
InfixOperatorToSymbol[Token`LongName`LeftTeeArrow] = LeftTeeArrow
InfixOperatorToSymbol[Token`LongName`RightTeeArrow] = RightTeeArrow
InfixOperatorToSymbol[Token`LongName`LeftVector] = LeftVector
InfixOperatorToSymbol[Token`LongName`DownLeftVector] = DownLeftVector
InfixOperatorToSymbol[Token`LongName`RightVector] = RightVector
InfixOperatorToSymbol[Token`LongName`DownRightVector] = DownRightVector
InfixOperatorToSymbol[Token`LongName`RightArrowLeftArrow] = RightArrowLeftArrow
InfixOperatorToSymbol[Token`LongName`LeftArrowRightArrow] = LeftArrowRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLeftArrow] = DoubleLeftArrow
InfixOperatorToSymbol[Token`LongName`DoubleRightArrow] = DoubleRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLeftRightArrow] = DoubleLeftRightArrow
InfixOperatorToSymbol[Token`LongName`LeftArrowBar] = LeftArrowBar
InfixOperatorToSymbol[Token`LongName`RightArrowBar] = RightArrowBar
InfixOperatorToSymbol[Token`LongName`LeftRightVector] = LeftRightVector
InfixOperatorToSymbol[Token`LongName`DownLeftRightVector] = DownLeftRightVector
InfixOperatorToSymbol[Token`LongName`LeftVectorBar] = LeftVectorBar
InfixOperatorToSymbol[Token`LongName`RightVectorBar] = RightVectorBar
InfixOperatorToSymbol[Token`LongName`DownLeftVectorBar] = DownLeftVectorBar
InfixOperatorToSymbol[Token`LongName`DownRightVectorBar] = DownRightVectorBar
InfixOperatorToSymbol[Token`LongName`LeftTeeVector] = LeftTeeVector
InfixOperatorToSymbol[Token`LongName`RightTeeVector] = RightTeeVector
InfixOperatorToSymbol[Token`LongName`DownLeftTeeVector] = DownLeftTeeVector
InfixOperatorToSymbol[Token`LongName`DownRightTeeVector] = DownRightTeeVector
InfixOperatorToSymbol[Token`LongName`ShortRightArrow] = ShortRightArrow
InfixOperatorToSymbol[Token`LongName`ShortLeftArrow] = ShortLeftArrow

InfixOperatorToSymbol[Token`LongName`UpArrow] = UpArrow
InfixOperatorToSymbol[Token`LongName`DownArrow] = DownArrow
InfixOperatorToSymbol[Token`LongName`UpDownArrow] = UpDownArrow
InfixOperatorToSymbol[Token`LongName`UpTeeArrow] = UpTeeArrow
InfixOperatorToSymbol[Token`LongName`DownTeeArrow] = DownTeeArrow
InfixOperatorToSymbol[Token`LongName`RightUpVector] = RightUpVector
InfixOperatorToSymbol[Token`LongName`LeftUpVector] = LeftUpVector
InfixOperatorToSymbol[Token`LongName`RightDownVector] = RightDownVector
InfixOperatorToSymbol[Token`LongName`LeftDownVector] = LeftDownVector
InfixOperatorToSymbol[Token`LongName`UpArrowDownArrow] = UpArrowDownArrow
InfixOperatorToSymbol[Token`LongName`DoubleUpArrow] = DoubleUpArrow
InfixOperatorToSymbol[Token`LongName`DoubleDownArrow] = DoubleDownArrow
InfixOperatorToSymbol[Token`LongName`DoubleUpDownArrow] = DoubleUpDownArrow
InfixOperatorToSymbol[Token`LongName`DownArrowUpArrow] = DownArrowUpArrow
InfixOperatorToSymbol[Token`LongName`LongLeftArrow] = LongLeftArrow
InfixOperatorToSymbol[Token`LongName`LongRightArrow] = LongRightArrow
InfixOperatorToSymbol[Token`LongName`LongLeftRightArrow] = LongLeftRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLongLeftArrow] = DoubleLongLeftArrow
InfixOperatorToSymbol[Token`LongName`DoubleLongRightArrow] = DoubleLongRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLongLeftRightArrow] = DoubleLongLeftRightArrow
InfixOperatorToSymbol[Token`LongName`UpArrowBar] = UpArrowBar
InfixOperatorToSymbol[Token`LongName`DownArrowBar] = DownArrowBar
InfixOperatorToSymbol[Token`LongName`RightUpDownVector] = RightUpDownVector
InfixOperatorToSymbol[Token`LongName`LeftUpDownVector] = LeftUpDownVector
InfixOperatorToSymbol[Token`LongName`RightUpVectorBar] = RightUpVectorBar
InfixOperatorToSymbol[Token`LongName`RightDownVectorBar] = RightDownVectorBar
InfixOperatorToSymbol[Token`LongName`LeftUpVectorBar] = LeftUpVectorBar
InfixOperatorToSymbol[Token`LongName`LeftDownVectorBar] = LeftDownVectorBar
InfixOperatorToSymbol[Token`LongName`RightUpTeeVector] = RightUpTeeVector
InfixOperatorToSymbol[Token`LongName`RightDownTeeVector] = RightDownTeeVector
InfixOperatorToSymbol[Token`LongName`LeftUpTeeVector] = LeftUpTeeVector
InfixOperatorToSymbol[Token`LongName`LeftDownTeeVector] = LeftDownTeeVector
InfixOperatorToSymbol[Token`LongName`UpEquilibrium] = UpEquilibrium
InfixOperatorToSymbol[Token`LongName`ReverseUpEquilibrium] = ReverseUpEquilibrium
InfixOperatorToSymbol[Token`LongName`ShortUpArrow] = ShortUpArrow
InfixOperatorToSymbol[Token`LongName`ShortDownArrow] = ShortDownArrow

InfixOperatorToSymbol[Token`LongName`CenterDot] = CenterDot
InfixOperatorToSymbol[Token`LongName`TildeTilde] = TildeTilde
InfixOperatorToSymbol[Token`LongName`NotTildeTilde] = NotTildeTilde
InfixOperatorToSymbol[Token`LongName`Equivalent] = Equivalent

InfixOperatorToSymbol[Token`LongName`LeftTriangleEqual] = LeftTriangleEqual
InfixOperatorToSymbol[Token`LongName`RightTriangleEqual] = RightTriangleEqual
InfixOperatorToSymbol[Token`LongName`NotLeftTriangleEqual] = NotLeftTriangleEqual
InfixOperatorToSymbol[Token`LongName`NotRightTriangleEqual] = NotRightTriangleEqual

InfixOperatorToSymbol[Token`LongName`TildeEqual] = TildeEqual
InfixOperatorToSymbol[Token`LongName`NotTildeEqual] = NotTildeEqual
InfixOperatorToSymbol[Token`LongName`TildeFullEqual] = TildeFullEqual
InfixOperatorToSymbol[Token`LongName`NotTildeFullEqual] = NotTildeFullEqual
InfixOperatorToSymbol[Token`LongName`CircleDot] = CircleDot
InfixOperatorToSymbol[Token`LongName`Distributed] = Distributed
InfixOperatorToSymbol[Token`LongName`Conditioned] = Conditioned
InfixOperatorToSymbol[Token`LongName`Union] = Union
InfixOperatorToSymbol[Token`LongName`Intersection] = Intersection
InfixOperatorToSymbol[Token`LongName`TensorWedge] = TensorWedge
InfixOperatorToSymbol[Token`LongName`TensorProduct] = TensorProduct
InfixOperatorToSymbol[Token`LongName`Cross] = Cross
InfixOperatorToSymbol[Token`LongName`LessTilde] = LessTilde
InfixOperatorToSymbol[Token`LongName`GreaterTilde] = GreaterTilde
InfixOperatorToSymbol[Token`LongName`NotLessTilde] = NotLessTilde
InfixOperatorToSymbol[Token`LongName`NotGreaterTilde] = NotGreaterTilde
InfixOperatorToSymbol[Token`LongName`Proportional] = Proportional
InfixOperatorToSymbol[Token`LongName`Proportion] = Proportion
InfixOperatorToSymbol[Token`LongName`LessLess] = LessLess
InfixOperatorToSymbol[Token`LongName`Congruent] = Congruent
InfixOperatorToSymbol[Token`LongName`Tilde] = Tilde
InfixOperatorToSymbol[Token`LongName`SmallCircle] = SmallCircle
InfixOperatorToSymbol[Token`LongName`Divides] = Divisible
InfixOperatorToSymbol[Token`LongName`VerticalSeparator] = VerticalSeparator
InfixOperatorToSymbol[Token`LongName`Backslash] = Backslash
InfixOperatorToSymbol[Token`LongName`Diamond] = Diamond
InfixOperatorToSymbol[Token`LongName`Wedge] = Wedge
InfixOperatorToSymbol[Token`LongName`Vee] = Vee
InfixOperatorToSymbol[Token`LongName`CircleTimes] = CircleTimes
InfixOperatorToSymbol[Token`LongName`Star] = Star
InfixOperatorToSymbol[Token`LongName`VerticalTilde] = VerticalTilde
InfixOperatorToSymbol[Token`LongName`Coproduct] = Coproduct
InfixOperatorToSymbol[Token`LongName`Cap] = Cap
InfixOperatorToSymbol[Token`LongName`Cup] = Cup
InfixOperatorToSymbol[Token`LongName`CirclePlus] = CirclePlus

InfixOperatorToSymbol[Token`LongName`VerticalBar] = VerticalBar
InfixOperatorToSymbol[Token`LongName`DoubleVerticalBar] = DoubleVerticalBar
InfixOperatorToSymbol[Token`LongName`NotVerticalBar] = NotVerticalBar
InfixOperatorToSymbol[Token`LongName`NotDoubleVerticalBar] = NotDoubleVerticalBar

InfixOperatorToSymbol[Token`LongName`LeftTriangle] = LeftTriangle
InfixOperatorToSymbol[Token`LongName`RightTriangle] = RightTriangle
InfixOperatorToSymbol[Token`LongName`NotLeftTriangle] = NotLeftTriangle
InfixOperatorToSymbol[Token`LongName`NotRightTriangle] = NotRightTriangle

InfixOperatorToSymbol[Token`LongName`PermutationProduct] = PermutationProduct
InfixOperatorToSymbol[Token`LongName`Equilibrium] = Equilibrium
InfixOperatorToSymbol[Token`LongName`ReverseEquilibrium] = ReverseEquilibrium
InfixOperatorToSymbol[Token`LongName`ReverseElement] = ReverseElement
InfixOperatorToSymbol[Token`LongName`NotReverseElement] = NotReverseElement
InfixOperatorToSymbol[Token`LongName`NotTilde] = NotTilde
InfixOperatorToSymbol[Token`LongName`EqualTilde] = EqualTilde

InfixOperatorToSymbol[Token`LongName`Precedes] = Precedes
InfixOperatorToSymbol[Token`LongName`Succeeds] = Succeeds
InfixOperatorToSymbol[Token`LongName`PrecedesEqual] = PrecedesEqual
InfixOperatorToSymbol[Token`LongName`SucceedsEqual] = SucceedsEqual
InfixOperatorToSymbol[Token`LongName`PrecedesTilde] = PrecedesTilde
InfixOperatorToSymbol[Token`LongName`SucceedsTilde] = SucceedsTilde
InfixOperatorToSymbol[Token`LongName`PrecedesSlantEqual] = PrecedesSlantEqual
InfixOperatorToSymbol[Token`LongName`SucceedsSlantEqual] = SucceedsSlantEqual
InfixOperatorToSymbol[Token`LongName`NotPrecedes] = NotPrecedes
InfixOperatorToSymbol[Token`LongName`NotSucceeds] = NotSucceeds
InfixOperatorToSymbol[Token`LongName`NotPrecedesEqual] = NotPrecedesEqual
InfixOperatorToSymbol[Token`LongName`NotSucceedsEqual] = NotSucceedsEqual
InfixOperatorToSymbol[Token`LongName`NotPrecedesTilde] = NotPrecedesTilde
InfixOperatorToSymbol[Token`LongName`NotSucceedsTilde] = NotSucceedsTilde
InfixOperatorToSymbol[Token`LongName`NotPrecedesSlantEqual] = NotPrecedesSlantEqual
InfixOperatorToSymbol[Token`LongName`NotSucceedsSlantEqual] = NotSucceedsSlantEqual

(*
LessSlantEqual and GreaterSlandEqual parse to LessEqual and GreaterEqual
*)
InfixOperatorToSymbol[Token`LongName`LessSlantEqual] = LessEqual
InfixOperatorToSymbol[Token`LongName`GreaterSlantEqual] = GreaterEqual
InfixOperatorToSymbol[Token`LongName`Colon] = Colon

InfixOperatorToSymbol[Token`LongName`CupCap] = CupCap
InfixOperatorToSymbol[Token`LongName`NotCupCap] = NotCupCap

InfixOperatorToSymbol[Token`LongName`DotEqual] = DotEqual
InfixOperatorToSymbol[Token`LongName`GreaterEqualLess] = GreaterEqualLess

InfixOperatorToSymbol[Token`LongName`GreaterFullEqual] = GreaterFullEqual
InfixOperatorToSymbol[Token`LongName`NotGreaterFullEqual] = NotGreaterFullEqual

InfixOperatorToSymbol[Token`LongName`GreaterGreater] = GreaterGreater

InfixOperatorToSymbol[Token`LongName`GreaterLess] = GreaterLess
InfixOperatorToSymbol[Token`LongName`LessGreater] = LessGreater
InfixOperatorToSymbol[Token`LongName`NotGreaterLess] = NotGreaterLess
InfixOperatorToSymbol[Token`LongName`NotLessGreater] = NotLessGreater

InfixOperatorToSymbol[Token`LongName`HumpEqual] = HumpEqual
InfixOperatorToSymbol[Token`LongName`HumpDownHump] = HumpDownHump
InfixOperatorToSymbol[Token`LongName`NestedGreaterGreater] = NestedGreaterGreater
InfixOperatorToSymbol[Token`LongName`NestedLessLess] = NestedLessLess
InfixOperatorToSymbol[Token`LongName`NotCongruent] = NotCongruent

InfixOperatorToSymbol[Token`LongName`SquareUnion] = SquareUnion
InfixOperatorToSymbol[Token`LongName`SquareIntersection] = SquareIntersection

InfixOperatorToSymbol[Token`LongName`UnionPlus] = UnionPlus

(*
StartOfLineOperatorToSymbol[Token`Question] = Information
StartOfLineOperatorToSymbol[Token`QuestionQuestion] = Information
StartOfLineOperatorToSymbol[Token`Bang] = Run
StartOfLineOperatorToSymbol[Token`BangBang] = FilePrint
*)

(*
StartOfFileOperatorToSymbol[Token`HashBang] = AST`Shebang
*)



GroupOpenerToSymbol[Token`OpenCurly] = List
GroupOpenerToSymbol[Token`LessBar] = Association
GroupOpenerToSymbol[Token`OpenSquare] = AST`GroupSquare
GroupOpenerToSymbol[Token`OpenParen] = AST`GroupParen

GroupOpenerToSymbol[Token`LongName`LeftAngleBracket] = AngleBracket
GroupOpenerToSymbol[Token`LongName`LeftCeiling] = Ceiling
GroupOpenerToSymbol[Token`LongName`LeftFloor] = Floor
GroupOpenerToSymbol[Token`LongName`LeftDoubleBracket] = AST`GroupDoubleBracket
GroupOpenerToSymbol[Token`LongName`LeftBracketingBar] = BracketingBar
GroupOpenerToSymbol[Token`LongName`LeftDoubleBracketingBar] = DoubleBracketingBar
GroupOpenerToSymbol[Token`LongName`LeftAssociation] = Association
GroupOpenerToSymbol[Token`LongName`OpenCurlyQuote] = CurlyQuote
GroupOpenerToSymbol[Token`LongName`OpenCurlyDoubleQuote] = CurlyDoubleQuote

GroupOpenerToSymbol[Token`LinearSyntax`OpenParen] = AST`GroupLinearSyntaxParen



GroupOpenerToCloser[Token`OpenCurly] = Token`CloseCurly
GroupOpenerToCloser[Token`LessBar] = Token`BarGreater
GroupOpenerToCloser[Token`OpenSquare] = Token`CloseSquare
GroupOpenerToCloser[Token`OpenParen] = Token`CloseParen

GroupOpenerToCloser[Token`LongName`LeftAngleBracket] = Token`LongName`RightAngleBracket
GroupOpenerToCloser[Token`LongName`LeftCeiling] = Token`LongName`RightCeiling
GroupOpenerToCloser[Token`LongName`LeftFloor] = Token`LongName`RightFloor
GroupOpenerToCloser[Token`LongName`LeftDoubleBracket] = Token`LongName`RightDoubleBracket
GroupOpenerToCloser[Token`LongName`LeftBracketingBar] = Token`LongName`RightBracketingBar
GroupOpenerToCloser[Token`LongName`LeftDoubleBracketingBar] = Token`LongName`RightDoubleBracketingBar
GroupOpenerToCloser[Token`LongName`LeftAssociation] = Token`LongName`RightAssociation
GroupOpenerToCloser[Token`LongName`OpenCurlyQuote] = Token`LongName`CloseCurlyQuote
GroupOpenerToCloser[Token`LongName`OpenCurlyDoubleQuote] = Token`LongName`CloseCurlyDoubleQuote

GroupOpenerToCloser[Token`LinearSyntax`OpenParen] = Token`LinearSyntax`CloseParen



GroupCloserToOpener[Token`CloseCurly] = Token`OpenCurly
GroupCloserToOpener[Token`BarGreater] = Token`LessBar
GroupCloserToOpener[Token`CloseSquare] = Token`OpenSquare
GroupCloserToOpener[Token`CloseParen] = Token`OpenParen

GroupCloserToOpener[Token`LongName`RightAngleBracket] = Token`LongName`LeftAngleBracket
GroupCloserToOpener[Token`LongName`RightCeiling] = Token`LongName`LeftCeiling
GroupCloserToOpener[Token`LongName`RightFloor] = Token`LongName`LeftFloor
GroupCloserToOpener[Token`LongName`RightDoubleBracket] = Token`LongName`LeftDoubleBracket
GroupCloserToOpener[Token`LongName`RightBracketingBar] = Token`LongName`LeftBracketingBar
GroupCloserToOpener[Token`LongName`RightDoubleBracketingBar] = Token`LongName`LeftDoubleBracketingBar
GroupCloserToOpener[Token`LongName`RightAssociation] = Token`LongName`LeftAssociation
GroupCloserToOpener[Token`LongName`CloseCurlyQuote] = Token`LongName`OpenCurlyQuote
GroupCloserToOpener[Token`LongName`CloseCurlyDoubleQuote] = Token`LongName`OpenCurlyDoubleQuote

GroupCloserToOpener[Token`LinearSyntax`CloseParen] = Token`LinearSyntax`OpenParen




PrefixBinaryOperatorToSymbol[Token`LongName`Integral] = Integrate



InequalityOperatorToSymbol[Token`EqualEqual] = Inequality
InequalityOperatorToSymbol[Token`LessEqual] = Inequality
InequalityOperatorToSymbol[Token`BangEqual] = Inequality
InequalityOperatorToSymbol[Token`Less] = Inequality
InequalityOperatorToSymbol[Token`Greater] = Inequality
InequalityOperatorToSymbol[Token`GreaterEqual] = Inequality
InequalityOperatorToSymbol[Token`LongName`Equal] = Inequality
InequalityOperatorToSymbol[Token`LongName`LessEqual] = Inequality
InequalityOperatorToSymbol[Token`LongName`GreaterEqual] = Inequality
InequalityOperatorToSymbol[Token`LongName`NotEqual] = Inequality
InequalityOperatorToSymbol[Token`LongName`LessEqualGreater] = Inequality
InequalityOperatorToSymbol[Token`LongName`GreaterEqualLess] = Inequality
InequalityOperatorToSymbol[Token`LongName`NotLessLess] = Inequality
InequalityOperatorToSymbol[Token`LongName`LongEqual] = Inequality



VectorInequalityOperatorToSymbol[Token`LongName`VectorGreater] = Developer`VectorInequality
VectorInequalityOperatorToSymbol[Token`LongName`VectorGreaterEqual] = Developer`VectorInequality
VectorInequalityOperatorToSymbol[Token`LongName`VectorLess] = Developer`VectorInequality
VectorInequalityOperatorToSymbol[Token`LongName`VectorLessEqual] = Developer`VectorInequality







Print["Generating Symbol..."]

$WorkaroundBug321344 = checkBug321344[]
Print["Work around Bug 321344: ", $WorkaroundBug321344];

symbols = Union[Join[
    {Blank, BlankSequence, BlankNullSequence, EndOfFile, Inequality, Integer, List, Optional, Out, Pattern, Real, Slot, SlotSequence,
      String, Symbol, TagSet, TagSetDelayed, TagUnset, True, Whitespace},
    {Developer`VectorInequality},
    {AST`Library`MakeLeafNode, AST`Library`MakePrefixNode, AST`Library`MakeBinaryNode, AST`Library`MakeInfixNode,
            AST`Library`MakeTernaryNode, AST`Library`MakePostfixNode, AST`Library`MakeCallNode, AST`Library`MakeGroupNode,
            (*
            AST`Library`MakeStartOfLineNode, AST`Library`MakeStartOfFileNode,
            *)
            AST`Library`MakeBlankNode, AST`Library`MakeBlankSequenceNode,
            AST`Library`MakeBlankNullSequenceNode, AST`Library`MakePatternBlankNode, AST`Library`MakePatternBlankSequenceNode,
            AST`Library`MakePatternBlankNullSequenceNode, AST`Library`MakeOptionalDefaultPatternNode, AST`Library`MakeSyntaxErrorNode,
            AST`Library`MakeGroupMissingCloserNode, AST`Library`MakePrefixBinaryNode,
            AST`Library`MakeSyntaxIssue, AST`Library`MakeReplaceTextCodeAction, AST`Library`MakeInsertTextCodeAction,
            AST`Library`MakeFormatIssue, AST`Library`MakeDeleteTextCodeAction, AST`Library`MakeDeleteTriviaCodeAction,
            AST`Library`MakeInsertTextAfterCodeAction, AST`Library`MakeSourceCharacterNode, AST`Library`MakeSafeStringNode},
    {AST`InternalInvalid, AST`Metadata, AST`PatternBlank, AST`PatternBlankSequence, AST`PatternBlankNullSequence,
      AST`OptionalDefault, AST`OptionalDefaultPattern, AST`TernaryTilde},
    {AST`SourceCharacter},
    DownValues[PrefixOperatorToSymbol][[All, 2]],
    DownValues[PostfixOperatorToSymbol][[All, 2]],
    DownValues[BinaryOperatorToSymbol][[All, 2]],
    DownValues[InfixOperatorToSymbol][[All, 2]],
    DownValues[TernaryOperatorToSymbol][[All, 2]],
    DownValues[GroupOpenerToSymbol][[All, 2]],
    DownValues[PrefixBinaryOperatorToSymbol][[All, 2]],
    (*
    DownValues[StartOfLineOperatorToSymbol][[All, 2]],
    DownValues[StartOfFileOperatorToSymbol][[All, 2]],
    *)
    tokens
]]


symbolCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"TokenEnum.h\"

#if USE_MATHLINK
#include \"mathlink.h\"
#undef P
#endif // USE_MATHLINK

//#include <string>
#include <memory>

class Symbol {
public:
  constexpr Symbol(const char *Name) : Name(Name) {}
  const char *name() const;

#if USE_MATHLINK
  void put(MLINK mlp) const;
#endif

private:
  const char *Name;
};

using SymbolPtr = std::unique_ptr<Symbol>;

SymbolPtr& PrefixOperatorToSymbol(TokenEnum T);
SymbolPtr& PostfixOperatorToSymbol(TokenEnum T);
SymbolPtr& BinaryOperatorToSymbol(TokenEnum T);
SymbolPtr& InfixOperatorToSymbol(TokenEnum T);

SymbolPtr& GroupOpenerToSymbol(TokenEnum T);
SymbolPtr& PrefixBinaryOperatorToSymbol(TokenEnum T);

TokenEnum GroupOpenerToCloser(TokenEnum T);
TokenEnum GroupCloserToOpener(TokenEnum T);

SymbolPtr& TokenToSymbol(TokenEnum T);

"} ~Join~
(Row[{"extern", " ", "SymbolPtr", " ", toGlobal["Symbol`"<>ToString[#]], ";"}]& /@ symbols) ~Join~
{""}

Print["exporting Symbol.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Symbol.h"}], Column[symbolCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

(*
We want to fully-qualify symbol names over the wire.
This allows library->kernel traffic to work when AST` is not on $ContextPath.
However, it is still not possible to fully-qualify System` symbols
Related bugs: 283291, 284492
So also make library->kernel traffic match this behavior
*)
stringifyForTransmitting[sym_Symbol] :=
Module[{ctxt},
  ctxt = Context[sym];
  If[ctxt == "System`",
    SymbolName[sym]
    ,
    Context[sym]<>SymbolName[sym]
  ]
]

symbolCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"Symbol.h\"

#include \"Token.h\"

#include <cassert>

const char *Symbol::name() const {
   return Name;
}

#if USE_MATHLINK
void Symbol::put(MLINK mlp) const {
    if (!MLPutSymbol(mlp, Name)) {
        assert(false);
    }
}
#endif
"} ~Join~

  (If[# === String && $WorkaroundBug321344,
      (*
      handle String specially because of bug 321344
      *)
      "SymbolPtr SYMBOL_STRING = SymbolPtr(new Symbol(\"String\"));"
      ,
      Row[{"SymbolPtr", " ", toGlobal["Symbol`"<>ToString[#]], " = SymbolPtr(new Symbol(\"", stringifyForTransmitting[#], "\"));"}]]& /@ symbols) ~Join~

      {""} ~Join~

      {"SymbolPtr& PrefixOperatorToSymbol(TokenEnum T) {\nswitch (T.value()) {"} ~Join~
     
     Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PrefixOperatorToSymbol]] ~Join~ 
      {"default: return " <> toGlobal["Symbol`"<>ToString[AST`InternalInvalid]] <> ";",
      "}\n}"} ~Join~

     {""} ~Join~

     {"SymbolPtr& PostfixOperatorToSymbol(TokenEnum T) {\nswitch (T.value()) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PostfixOperatorToSymbol]] ~Join~
      {"default: return " <> toGlobal["Symbol`"<>ToString[AST`InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"SymbolPtr& BinaryOperatorToSymbol(TokenEnum T) {\nswitch (T.value()) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[BinaryOperatorToSymbol]] ~Join~
      {"default: return " <> toGlobal["Symbol`"<>ToString[AST`InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"SymbolPtr& InfixOperatorToSymbol(TokenEnum T) {\nswitch (T.value()) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[InfixOperatorToSymbol]] ~Join~
      {"default: return " <> toGlobal["Symbol`"<>ToString[AST`InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"SymbolPtr& GroupOpenerToSymbol(TokenEnum T) {"} ~Join~
     {"switch (T.value()) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[GroupOpenerToSymbol]] ~Join~
      {"default: return " <> toGlobal["Symbol`"<>ToString[AST`InternalInvalid]] <> ";",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"TokenEnum GroupOpenerToCloser(TokenEnum T) {"} ~Join~
     {"switch (T.value()) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[GroupOpenerToCloser]] ~Join~
      {"default: assert(false && \"Unhandled token\"); return TOKEN_UNKNOWN;",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"TokenEnum GroupCloserToOpener(TokenEnum T) {"} ~Join~
     {"switch (T.value()) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[GroupCloserToOpener]] ~Join~
      {"default: assert(false && \"Unhandled token\"); return TOKEN_UNKNOWN;",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"SymbolPtr& PrefixBinaryOperatorToSymbol(TokenEnum T) {\nswitch (T.value()) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ".value():", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PrefixBinaryOperatorToSymbol]] ~Join~
      {"default: return " <> toGlobal["Symbol`"<>ToString[AST`InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""}

Print["exporting Symbol.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Symbol.cpp"}], Column[symbolCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Symbol"]

End[]

EndPackage[]
