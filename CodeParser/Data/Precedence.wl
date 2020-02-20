
(*

Largely derived from documentation:

tutorial/OperatorInputForms
tutorial/Operators
and the System function Precedence

Precedence[] only reports the precedence values associated with operators, used for OUTPUT

With modifications based on empirical observations

*)

<|
Precedence`Lowest -> 0,

Precedence`Comma -> Next,
Precedence`LongName`InvisibleComma -> Precedence`Comma,

Precedence`Semi -> Next, (* Precedence[CompoundExpression] == 10 *)

Precedence`GreaterGreater -> Next, (* Precedence[Put] == 30 *)
Precedence`GreaterGreaterGreater -> Precedence`GreaterGreater, (* Precedence[PutAppend] == 30 *)

Precedence`Equal -> Next, (* Precedence[Set] == 40 *)
Precedence`ColonEqual -> Precedence`Equal, (* Precedence[SetDelayed] == 40 *)
Precedence`CaretEqual -> Precedence`Equal, (* Precedence[UpSet] == 40 *)
Precedence`CaretColonEqual -> Precedence`Equal, (* Precedence[UpSetDelayed] == 40 *)
Precedence`LongName`Function -> Precedence`Equal,

(*
Make /: higher than = and :=
*)
Precedence`SlashColon -> Next,

Precedence`LongName`Because -> Next, (* Precedence[Because] == 50 *)
Precedence`LongName`Therefore -> Precedence`LongName`Because, (* Precedence[Therefore] == 50 *)

Precedence`LongName`VerticalSeparator -> Next, (* Precedence[VerticalSeparator] == 60 *)

Precedence`SlashSlash -> Next, (* Precedence[Postfix] == 70 *)

Precedence`LongName`Colon -> Next, (* Precedence[Colon] == 80 *)

Precedence`Amp -> Next, (* Precedence[Function] == 90 *)

Precedence`PlusEqual -> Next, (* Precedence[AddTo] == 100 *)
Precedence`StarEqual -> Precedence`PlusEqual, (* Precedence[TimesBy] == 100 *)
Precedence`MinusEqual -> Precedence`PlusEqual, (* Precedence[SubtractFrom] == 100 *)
Precedence`SlashEqual -> Precedence`PlusEqual, (* Precedence[DivideBy] == 100 *)

Precedence`SlashDot -> Next, (* Precedence[ReplaceAll] == 110 *)
Precedence`SlashSlashDot -> Precedence`SlashDot, (* Precedence[ReplaceRepeated] == 110 *)

Precedence`MinusGreater -> Next, (* Precedence[Rule] == 120 *)
Precedence`ColonGreater -> Precedence`MinusGreater, (* Precedence[RuleDelayed] == 120 *)
Precedence`LongName`Rule -> Precedence`MinusGreater,
Precedence`LongName`RuleDelayed -> Precedence`MinusGreater,

(*
added in 11.1:
TwoWayRule

TwoWayRule did not have correct precedence until 12.0

<-> was UndirectedEdge before 11.2.
From 11.2 forward, <-> is TwoWayRule, but it kept the precedence of UndirectedEdge

From 12.0 forward, <-> has correct precedence.
*)
Precedence`LessMinusGreater -> Next, (* Precedence[System`TwoWayRule] == 125 *)
Precedence`LongName`TwoWayRule -> Precedence`LessMinusGreater,

Precedence`SlashSemi -> Next, (* Precedence[Condition] == 130 *)

Precedence`TildeTilde -> Next, (* Precedence[StringExpression] == 135 *)

Precedence`Fake`OptionalColon -> Next, (* Precedence[Optional] == 140 *)

Precedence`Fake`PatternColon -> Next, (* Precedence[Pattern] == 150 *)

Precedence`Bar -> Next, (* Precedence[Alternatives] == 160 *)

Precedence`DotDot -> Next, (* Precedence[Repeated] == 170 *)
Precedence`DotDotDot -> Precedence`DotDot, (* Precedence[RepeatedNull] == 170 *)

Precedence`LongName`SuchThat -> Next, (* Precedence[SuchThat] == 180 *)

Precedence`LongName`UpTee -> Next, (* Precedence[UpTee] == 190 *)
Precedence`LongName`DownTee -> Precedence`LongName`UpTee, (* Precedence[DownTee] == 190 *)
Precedence`LongName`RightTee -> Precedence`LongName`UpTee, (* Precedence[RightTee] == 190 *)
Precedence`LongName`LeftTee -> Precedence`LongName`UpTee, (* Precedence[LeftTee] == 190 *)
Precedence`LongName`DoubleRightTee -> Precedence`LongName`UpTee, (* Precedence[DoubleRightTee] == 190 *)
Precedence`LongName`DoubleLeftTee -> Precedence`LongName`UpTee, (* Precedence[DoubleLeftTee] == 190 *)
Precedence`LongName`Perpendicular -> Precedence`LongName`UpTee, (* Precedence[Perpendicular] == 190 *)

Precedence`LongName`Conditioned -> Next, (* Precedence[Conditioned] == 195 *)

Precedence`LongName`Implies -> Next, (* Precedence[Implies] == 200 *)
(*
DISCREPANCY
Precedence[RoundImplies] is 240
But this is wrong, RoundImplies has same precedence as Implies
*)
Precedence`LongName`RoundImplies -> Precedence`LongName`Implies,

Precedence`LongName`Equivalent -> Next, (* Precedence[Equivalent] == 205 *)

Precedence`BarBar -> Next, (* Precedence[Or] == 215 *)
Precedence`LongName`Or -> Precedence`BarBar,
Precedence`LongName`Nor -> Precedence`BarBar,

(*
DISCREPANCY
Precedence[Xor] is 215, which is equal to Or.
But this is wrong, a \[Xor] b is between || and &&
*)
Precedence`LongName`Xor -> Next,
Precedence`LongName`Xnor -> Precedence`LongName`Xor,

(*
DISCREPANCY
Precedence[And] is 215, which is equal to Or.
But this is wrong, a && b has higher precedence than a || b
*)
Precedence`AmpAmp -> Next,
Precedence`LongName`And -> Precedence`AmpAmp,
Precedence`LongName`Nand -> Precedence`AmpAmp,

Precedence`Prefix`Bang -> Next, (* Precedence[Not] == 230 *)
Precedence`LongName`Not -> Precedence`Prefix`Bang,
Precedence`Fake`Prefix`BangBang -> Precedence`Prefix`Bang,

Precedence`LongName`ForAll -> Next, (* Precedence[ForAll] == 240 *)
Precedence`LongName`Exists -> Precedence`LongName`ForAll, (* Precedence[Exists] == 240 *)
Precedence`LongName`NotExists -> Precedence`LongName`ForAll, (* Precedence[NotExists] == 240 *)

(*
Set relations
*)
(*
Precedence`LongName`Element -> Precedence[Element], (* 250 *)
Precedence`LongName`Subset -> Precedence[Subset], (* 250 *)
Precedence`LongName`Superset -> Precedence[Superset], (* 250 *)
Precedence`LongName`SubsetEqual -> Precedence[SubsetEqual], (* 250 *)
Precedence`LongName`SupersetEqual -> Precedence[SupersetEqual], (* 250 *)
Precedence`LongName`NotElement -> Precedence[NotElement], (* 250 *)
Precedence`LongName`NotSubset -> Precedence[NotSubset], (* 250 *)
Precedence`LongName`NotSuperset -> Precedence[NotSuperset], (* 250 *)
Precedence`LongName`NotSubsetEqual -> Precedence[NotSubsetEqual], (* 250 *)
Precedence`LongName`NotSupersetEqual -> Precedence[NotSupersetEqual], (* 250 *)
Precedence`LongName`ReverseElement -> Precedence[ReverseElement], (* 250 *)
Precedence`LongName`NotReverseElement -> Precedence[NotReverseElement], (* 250 *)
Precedence`LongName`SquareSubset -> Precedence[SquareSubset], (* 250 *)
Precedence`LongName`SquareSuperset -> Precedence[SquareSuperset], (* 250 *)
Precedence`LongName`NotSquareSubset -> Precedence[NotSquareSubset], (* 250 *)
Precedence`LongName`NotSquareSuperset -> Precedence[NotSquareSuperset], (* 250 *)
Precedence`LongName`SquareSubsetEqual -> Precedence[SquareSubsetEqual], (* 250 *)
Precedence`LongName`SquareSupersetEqual -> Precedence[SquareSupersetEqual], (* 250 *)
Precedence`LongName`NotSquareSubsetEqual -> Precedence[NotSquareSubsetEqual], (* 250 *)
Precedence`LongName`NotSquareSupersetEqual -> Precedence[NotSquareSupersetEqual], (* 250 *)
Precedence`LongName`Distributed -> Precedence[Distributed], (* 250 *)
*)
Precedence`Class`SetRelations -> Next, (* Precedence[Element] == 250 *)

(*
DISCREPANCY
Precedence[SameQ] is 290
But empirically it is between \[Element] and \[RightTeeArrow]
*)
Precedence`EqualEqualEqual -> Next,
Precedence`EqualBangEqual -> Precedence`EqualEqualEqual,

(*
Horizontal arrows
*)
(*
Precedence`LongName`LeftArrow -> Precedence[LeftArrow], (* 270 *)
Precedence`LongName`RightArrow -> Precedence[RightArrow], (* 270 *)
Precedence`LongName`LeftRightArrow -> Precedence[LeftRightArrow], (* 270 *)
Precedence`LongName`LeftTeeArrow -> Precedence[LeftTeeArrow], (* 270 *)
Precedence`LongName`RightTeeArrow -> Precedence[RightTeeArrow], (* 270 *)
Precedence`LongName`RightArrowLeftArrow -> Precedence[RightArrowLeftArrow], (* 270 *)
Precedence`LongName`LeftArrowRightArrow -> Precedence[LeftArrowRightArrow], (* 270 *)
Precedence`LongName`DoubleLeftArrow -> Precedence[DoubleLeftArrow], (* 270 *)
Precedence`LongName`DoubleRightArrow -> Precedence[DoubleRightArrow], (* 270 *)
Precedence`LongName`DoubleLeftRightArrow -> Precedence[DoubleLeftRightArrow], (* 270 *)
Precedence`LongName`LeftArrowBar -> Precedence[LeftArrowBar], (* 270 *)
Precedence`LongName`RightArrowBar -> Precedence[RightArrowBar], (* 270 *)
Precedence`LongName`ShortRightArrow -> Precedence[ShortRightArrow], (* 270 *)
Precedence`LongName`ShortLeftArrow -> Precedence[ShortLeftArrow], (* 270 *)
*)
Precedence`Class`HorizontalArrows -> Next, (* Precedence[LeftArrow] == 270 *)

(*
Vector operators
*)
(*
Precedence`LongName`LeftVector -> Precedence[LeftVector], (* 270 *)
Precedence`LongName`RightVector -> Precedence[RightVector], (* 270 *)
Precedence`LongName`LeftRightVector -> Precedence[LeftRightVector], (* 270 *)
Precedence`LongName`LeftVectorBar -> Precedence[LeftVectorBar], (* 270 *)
Precedence`LongName`RightVectorBar -> Precedence[RightVectorBar], (* 270 *)
Precedence`LongName`LeftTeeVector -> Precedence[LeftTeeVector], (* 270 *)
Precedence`LongName`RightTeeVector -> Precedence[RightTeeVector], (* 270 *)
Precedence`LongName`DownLeftVector -> Precedence[DownLeftVector], (* 270 *)
Precedence`LongName`DownRightVector -> Precedence[DownRightVector], (* 270 *)
Precedence`LongName`DownLeftRightVector -> Precedence[DownLeftRightVector], (* 270 *)
Precedence`LongName`DownLeftVectorBar -> Precedence[DownLeftVectorBar], (* 270 *)
Precedence`LongName`DownRightVectorBar -> Precedence[DownRightVectorBar], (* 270 *)
Precedence`LongName`DownLeftTeeVector -> Precedence[DownLeftTeeVector], (* 270 *)
Precedence`LongName`DownRightTeeVector -> Precedence[DownRightTeeVector], (* 270 *)
*)
Precedence`Class`VectorOperators -> Precedence`Class`HorizontalArrows, (* Precedence[LeftVector] == 270 *)

(*
Diagonal arrow operators
*)
(*
Precedence`LongName`UpperLeftArrow -> Precedence[UpperLeftArrow], (* 270 *)
Precedence`LongName`UpperRightArrow -> Precedence[UpperRightArrow], (* 270 *)
Precedence`LongName`LowerRightArrow -> Precedence[LowerRightArrow], (* 270 *)
Precedence`LongName`LowerLeftArrow -> Precedence[LowerLeftArrow], (* 270 *)
*)
Precedence`Class`DiagonalArrowOperators -> Precedence`Class`HorizontalArrows, (* Precedence[UpperLeftArrow] == 270 *)

Precedence`LongName`VerticalBar -> Next, (* Precedence[VerticalBar] == 280 *)
Precedence`LongName`NotVerticalBar -> Precedence`LongName`VerticalBar, (* Precedence[NotVerticalBar] == 280 *)
Precedence`LongName`DoubleVerticalBar -> Precedence`LongName`VerticalBar, (* Precedence[DoubleVerticalBar] == 280 *)
Precedence`LongName`NotDoubleVerticalBar -> Precedence`LongName`VerticalBar, (* Precedence[NotDoubleVerticalBar] == 280 *)

(*
Ordering operators
*)
(*
Precedence`LongName`LeftTriangle -> Precedence[LeftTriangle], (* 290 *)
Precedence`LongName`RightTriangle -> Precedence[RightTriangle], (* 290 *)
Precedence`LongName`NotLeftTriangle -> Precedence[NotLeftTriangle], (* 290 *)
Precedence`LongName`NotRightTriangle -> Precedence[NotRightTriangle], (* 290 *)
Precedence`LongName`LeftTriangleEqual -> Precedence[LeftTriangleEqual], (* 290 *)
Precedence`LongName`RightTriangleEqual -> Precedence[RightTriangleEqual], (* 290 *)
Precedence`LongName`NotLeftTriangleEqual -> Precedence[NotLeftTriangleEqual], (* 290 *)
Precedence`LongName`NotRightTriangleEqual -> Precedence[NotRightTriangleEqual], (* 290 *)
Precedence`LongName`LeftTriangleBar -> Precedence[LeftTriangleBar], (* 290 *)
Precedence`LongName`RightTriangleBar -> Precedence[RightTriangleBar], (* 290 *)
Precedence`LongName`NotLeftTriangleBar -> Precedence[NotLeftTriangleBar], (* 290 *)
Precedence`LongName`NotRightTriangleBar -> Precedence[NotRightTriangleBar], (* 290 *)
Precedence`LongName`TildeEqual -> Precedence[TildeEqual], (* 290 *)
Precedence`LongName`NotTildeEqual -> Precedence[NotTildeEqual], (* 290 *)
Precedence`LongName`TildeFullEqual -> Precedence[TildeFullEqual], (* 290 *)
Precedence`LongName`NotTildeFullEqual -> Precedence[NotTildeFullEqual], (* 290 *)
Precedence`LongName`Tilde -> Precedence[Tilde], (* 290 *)
Precedence`LongName`NotTilde -> Precedence[NotTilde], (* 290 *)
Precedence`LongName`EqualTilde -> Precedence[EqualTilde], (* 290 *)
Precedence`LongName`NotEqualTilde -> Precedence[NotEqualTilde], (* 290 *)
Precedence`LongName`TildeTilde -> Precedence[TildeTilde], (* 290 *)
Precedence`LongName`NotTildeTilde -> Precedence[NotTildeTilde], (* 290 *)
Precedence`LongName`Proportional -> Precedence[Proportional], (* 290 *)
Precedence`LongName`Proportion -> Precedence[Proportion], (* 290 *)
Precedence`LongName`Congruent -> Precedence[Congruent], (* 290 *)
Precedence`LongName`NotCongruent -> Precedence[NotCongruent], (* 290 *)
Precedence`LongName`Equilibrium -> Precedence[Equilibrium], (* 290 *)
Precedence`LongName`ReverseEquilibrium -> Precedence[ReverseEquilibrium], (* 290 *)
Precedence`LongName`DotEqual -> Precedence[DotEqual], (* 290 *)
Precedence`LongName`Precedes -> Precedence[Precedes], (* 290 *)
Precedence`LongName`Succeeds -> Precedence[Succeeds], (* 290 *)
Precedence`LongName`PrecedesEqual -> Precedence[PrecedesEqual], (* 290 *)
Precedence`LongName`SucceedsEqual -> Precedence[SucceedsEqual], (* 290 *)
Precedence`LongName`PrecedesTilde -> Precedence[PrecedesTilde], (* 290 *)
Precedence`LongName`SucceedsTilde -> Precedence[SucceedsTilde], (* 290 *)
Precedence`LongName`PrecedesSlantEqual -> Precedence[PrecedesSlantEqual], (* 290 *)
Precedence`LongName`SucceedsSlantEqual -> Precedence[SucceedsSlantEqual], (* 290 *)
Precedence`LongName`NotPrecedes -> Precedence[NotPrecedes], (* 290 *)
Precedence`LongName`NotSucceeds -> Precedence[NotSucceeds], (* 290 *)
Precedence`LongName`NotPrecedesTilde -> Precedence[NotPrecedesTilde], (* 290 *)
Precedence`LongName`NotSucceedsTilde -> Precedence[NotSucceedsTilde], (* 290 *)
Precedence`LongName`NotPrecedesEqual -> Precedence[NotPrecedesEqual], (* 290 *)
Precedence`LongName`NotSucceedsEqual -> Precedence[NotSucceedsEqual], (* 290 *)
Precedence`LongName`NotPrecedesSlantEqual -> Precedence[PrecedesSlantEqual], (* 290 *)
Precedence`LongName`NotSucceedsSlantEqual -> Precedence[SucceedsSlantEqual], (* 290 *)
Precedence`LongName`CupCap -> Precedence[CupCap], (* 290 *)
Precedence`LongName`NotCupCap -> Precedence[NotCupCap], (* 290 *)
Precedence`LongName`HumpEqual -> Precedence[HumpEqual], (* 290 *)
Precedence`LongName`HumpDownHump -> Precedence[HumpDownHump], (* 290 *)
Precedence`LongName`NotHumpEqual -> Precedence[NotHumpEqual], (* 290 *)
Precedence`LongName`NotHumpDownHump -> Precedence[NotHumpDownHump], (* 290 *)
*)
Precedence`Class`OrderingOperators -> Next, (* Precedence[LeftTriangle] == 290 *)


Precedence`Class`Inequality -> Precedence`Class`OrderingOperators, (* Precedence[Equal] == 290 *)

Precedence`LongName`DirectedEdge -> Next, (* Precedence[DirectedEdge] == 295 *)
Precedence`LongName`UndirectedEdge -> Precedence`LongName`DirectedEdge, (* Precedence[UndirectedEdge] == 295 *)

(*
DISCREPANCY
Precedence[Span] is 305
But empirically it is lower than \[Union]
*)
Precedence`SemiSemi -> Next,

(*
Union operators
*)
(*
Precedence`LongName`Union -> Precedence[Union], (* 300 *)
Precedence`LongName`SquareUnion -> Precedence[SquareUnion], (* 300 *)
Precedence`LongName`UnionPlus -> Precedence[UnionPlus], (* 300 *)
*)
Precedence`Class`UnionOperators -> Next, (* Precedence[Union] == 300 *)

(*
Intersection operators
*)
(*
Precedence`LongName`Intersection -> Precedence[Intersection], (* 305 *)
Precedence`LongName`SquareIntersection -> Precedence[SquareIntersection], (* 305 *)
*)
Precedence`Class`IntersectionOperators -> Next, (* Precedence[Intersection] == 305 *)

Precedence`Infix`Plus -> Next, (* Precedence[Plus] == 310 *)
Precedence`Infix`Minus -> Precedence`Infix`Plus,
Precedence`Infix`LongName`PlusMinus -> Precedence`Infix`Plus, (* Precedence[PlusMinus] == 310 *)
Precedence`Infix`LongName`MinusPlus -> Precedence`Infix`Plus, (* Precedence[MinusPlus] == 310 *)
Precedence`Infix`LongName`Minus -> Precedence`Infix`Plus, (* 310 *)

Precedence`LongName`ImplicitPlus -> Next,

Precedence`LongName`Sum -> Next, (* Precedence[Sum] == 320 *)

(*
Precedence`LongName`Integral -> Precedence[Integrate], (* 325 *)
Precedence`LongName`ContourIntegral -> Precedence`LongName`Integral,
Precedence`LongName`DoubleContourIntegral -> Precedence`LongName`Integral,
Precedence`LongName`ClockwiseContourIntegral -> Precedence`LongName`Integral,
Precedence`LongName`CounterClockwiseContourIntegral -> Precedence`LongName`Integral,
*)
Precedence`Class`IntegrationOperators -> Next, (* Precedence[Integrate] == 325 *)
Precedence`LongName`ExpectationE -> Precedence`Class`IntegrationOperators, (* Precedence[ExpectationE] == 325 *)
Precedence`LongName`ProbabilityPr -> Precedence`Class`IntegrationOperators, (* Precedence[ProbabilityPr] == 325 *)

Precedence`LongName`CirclePlus -> Next, (* Precedence[CirclePlus] == 330 *)
Precedence`LongName`CircleMinus -> Precedence`LongName`CirclePlus, (* Precedence[CircleMinus] == 330 *)

Precedence`LongName`Cup -> Next, (* Precedence[Cup] == 340 *)

Precedence`LongName`Cap -> Next, (* Precedence[Cap] == 350 *)

Precedence`Infix`LongName`Coproduct -> Next, (* Precedence[Coproduct] == 360 *)

Precedence`LongName`VerticalTilde -> Next, (* Precedence[VerticalTilde] == 370 *)

Precedence`LongName`Product -> Next, (* Precedence[Product] == 380 *)
Precedence`LongName`ContinuedFractionK -> Precedence`LongName`Product, (* Precedence[ContinuedFractionK] == 380 *)

Precedence`LongName`Star -> Next, (* Precedence[Star] == 390 *)

Precedence`Star -> Next, (* Precedence[Times] == 400 *)
Precedence`LongName`Times -> Precedence`Star,
Precedence`LongName`InvisibleTimes -> Precedence`Star,
Precedence`Fake`ImplicitTimes -> Precedence`Star,

Precedence`LongName`CenterDot -> Next, (* Precedence[CenterDot] == 410 *)

Precedence`Infix`LongName`CircleTimes -> Next, (* Precedence[CircleTimes] == 420 *)

(*
DISCREPANCY
Precedence[PermutationProduct] is 520
But empirically it is between \[CircleTimes] and \[TensorWedge]
*)
Precedence`LongName`PermutationProduct -> Next,

(*
DISCREPANCY
Precedence[TensorProduct] is 495
But empirically it is between \[PermutationProduct] and \[TensorWedge]
*)
Precedence`LongName`TensorProduct -> Next,

(*
DISCREPANCY
Precedence[TensorWedge] is 500
But empirically it is between \[TensorProduct] and \[Vee]
*)
Precedence`LongName`TensorWedge -> Next,

Precedence`LongName`Vee -> Next, (* Precedence[Vee] == 430 *)

Precedence`LongName`Wedge -> Next, (* Precedence[Wedge] == 440 *)

Precedence`LongName`Diamond -> Next, (* Precedence[Diamond] == 450 *)

Precedence`LongName`Backslash -> Next, (* Precedence[Backslash] == 460 *)

Precedence`Slash -> Next, (* Precedence[Divide] == 470 *)
Precedence`LongName`Divide -> Precedence`Slash,
Precedence`LongName`Divides -> Precedence`Slash,
Precedence`LongName`DivisionSlash -> Precedence`Slash,

Precedence`Prefix`Minus -> Next, (* Precedence[Minus] == 480 *)
Precedence`Prefix`Plus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`PlusMinus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`MinusPlus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`Minus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`CircleTimes -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`Coproduct -> Precedence`Prefix`Minus,

Precedence`Dot -> Next, (* Precedence[Dot] == 490 *)

Precedence`LongName`Cross -> Next, (* Precedence[Cross] == 500 *)

Precedence`StarStar -> Next, (* Precedence[NonCommutativeMultiply] == 510 *)

Precedence`LongName`CircleDot -> Next, (* Precedence[CircleDot] == 520 *)

Precedence`LongName`SmallCircle -> Next, (* Precedence[SmallCircle] == 530 *)

Precedence`LongName`Square -> Next, (* Precedence[Square] == 540 *)

Precedence`LongName`Del -> Next, (* Precedence[Del] == 550 *)

(*
DISCREPANCY
Precedence[Piecewise] is 480
But empirically, it is between Del and DifferentialD

Also, Piecewise is documented to have higher precedence than ::

*)
Precedence`LongName`Piecewise -> Next,

(*
DISCREPANCY
Precedence[DifferentialD] is 550
But empirically, it is between Piecewise and Sqrt
*)
Precedence`LongName`DifferentialD -> Next,
Precedence`LongName`CapitalDifferentialD -> Precedence`LongName`DifferentialD,

Precedence`LongName`Sqrt -> Next,
(*
added in 12.1:
CubeRoot
*)
Precedence`LongName`CubeRoot -> Precedence`LongName`Sqrt,

(*
Vertical arrow operators
*)
(*
Precedence`LongName`UpArrow -> Precedence[UpArrow], (* 580 *)
Precedence`LongName`DownArrow -> Precedence[DownArrow], (* 580 *)
Precedence`LongName`UpDownArrow -> Precedence[UpDownArrow], (* 580 *)
Precedence`LongName`UpTeeArrow -> Precedence[UpTeeArrow], (* 580 *)
Precedence`LongName`DownTeeArrow -> Precedence[DownTeeArrow], (* 580 *)
Precedence`LongName`UpArrowDownArrow -> Precedence[UpArrowDownArrow], (* 580 *)
Precedence`LongName`DoubleUpArrow -> Precedence[DoubleUpArrow], (* 580 *)
Precedence`LongName`DoubleDownArrow -> Precedence[DoubleDownArrow], (* 580 *)
Precedence`LongName`DoubleUpDownArrow -> Precedence[DoubleUpDownArrow], (* 580 *)
Precedence`LongName`DownArrowUpArrow -> Precedence[DownArrowUpArrow], (* 580 *)
Precedence`LongName`LongLeftArrow -> Precedence[LongLeftArrow], (* 580 *)
Precedence`LongName`LongRightArrow -> Precedence[LongRightArrow], (* 580 *)
Precedence`LongName`LongLeftRightArrow -> Precedence[LongLeftRightArrow], (* 580 *)
Precedence`LongName`DoubleLongLeftArrow -> Precedence[DoubleLongLeftArrow], (* 580 *)
Precedence`LongName`DoubleLongRightArrow -> Precedence[DoubleLongRightArrow], (* 580 *)
Precedence`LongName`DoubleLongLeftRightArrow -> Precedence[DoubleLongLeftRightArrow], (* 580 *)
Precedence`LongName`UpArrowBar -> Precedence[UpArrowBar], (* 580 *)
Precedence`LongName`DownArrowBar -> Precedence[DownArrowBar], (* 580 *)
Precedence`LongName`ShortUpArrow -> Precedence[ShortUpArrow], (* 580 *)
Precedence`LongName`ShortDownArrow -> Precedence[ShortDownArrow], (* 580 *)
*)
Precedence`Class`VerticalArrowOperators -> Next, (* Precedence[UpArrow] == 580 *)

(*
Vertical vector operators
*)
(*
Precedence`LongName`RightUpVector -> Precedence[RightUpVector], (* 580 *)
Precedence`LongName`LeftUpVector -> Precedence[LeftUpVector], (* 580 *)
Precedence`LongName`RightDownVector -> Precedence[RightDownVector], (* 580 *)
Precedence`LongName`LeftDownVector -> Precedence[LeftDownVector], (* 580 *)
Precedence`LongName`RightUpDownVector -> Precedence[RightUpDownVector], (* 580 *)
Precedence`LongName`LeftUpDownVector -> Precedence[LeftUpDownVector], (* 580 *)
Precedence`LongName`RightUpVectorBar -> Precedence[RightUpVectorBar], (* 580 *)
Precedence`LongName`RightDownVectorBar -> Precedence[RightDownVectorBar], (* 580 *)
Precedence`LongName`LeftUpVectorBar -> Precedence[LeftUpVectorBar], (* 580 *)
Precedence`LongName`LeftDownVectorBar -> Precedence[LeftDownVectorBar], (* 580 *)
Precedence`LongName`RightUpTeeVector -> Precedence[RightUpTeeVector], (* 580 *)
Precedence`LongName`RightDownTeeVector -> Precedence[RightDownTeeVector], (* 580 *)
Precedence`LongName`LeftUpTeeVector -> Precedence[LeftUpTeeVector], (* 580 *)
Precedence`LongName`LeftDownTeeVector -> Precedence[LeftDownTeeVector], (* 580 *)
Precedence`LongName`UpEquilibrium -> Precedence[UpEquilibrium], (* 580 *)
Precedence`LongName`ReverseUpEquilibrium -> Precedence[ReverseUpEquilibrium], (* 580 *)
*)
Precedence`Class`VerticalVectorOperators -> Precedence`Class`VerticalArrowOperators, (* Precedence[RightUpVector] == 580 *)

Precedence`Caret -> Next, (* Precedence[Power] == 590 *)

Precedence`LessGreater -> Next, (* Precedence[StringJoin] == 600 *)

Precedence`SingleQuote -> Next,

Precedence`LongName`Transpose -> Next,
Precedence`LongName`Conjugate -> Precedence`LongName`Transpose,
Precedence`LongName`ConjugateTranspose -> Precedence`LongName`Transpose,
Precedence`LongName`HermitianConjugate -> Precedence`LongName`Transpose,

Precedence`Postfix`Bang -> Next, (* Precedence[Factorial] == 610 *)
Precedence`Postfix`BangBang -> Precedence`Postfix`Bang, (* Precedence[Factorial2] == 610 *)

Precedence`AtAt -> Next, (* Precedence[Apply] == 620 *)
Precedence`SlashAt -> Precedence`AtAt, (* Precedence[Map] == 620 *)
Precedence`AtAtAt -> Precedence`AtAt,
Precedence`SlashSlashAt -> Precedence`AtAt, (* Precedence[MapAll] == 620 *)

Precedence`Tilde -> Next, (* Precedence[Infix] == 630 *)

Precedence`At -> Next, (* Precedence[Prefix] == 640 *)
Precedence`LongName`InvisibleApplication -> Precedence`At, (* Precedence[InvisibleApplication] == 640 *)

(*
DISCREPANCY
Precedence[RightComposition] is 624
Precedence[Composition] is 625
But empirically they are higher than @
*)
Precedence`SlashStar -> Next,
Precedence`AtStar -> Next,

Precedence`Prefix`PlusPlus -> Next, (* Precedence[PreIncrement] == 660 *)
Precedence`Prefix`MinusMinus -> Precedence`Prefix`PlusPlus, (* Precedence[PreDecrement] == 660 *)

(*
DISCREPANCY
Precedence[Increment] is 660
Precedence[PreIncrement] is 660
But empirically Increment is higher than PreIncrement
*)
Precedence`Postfix`PlusPlus -> Next, (* Precedence[Increment] == 660 *)
Precedence`Postfix`MinusMinus -> Precedence`Postfix`PlusPlus, (* Precedence[Decrement] == 660 *)

Precedence`Call -> Next, (* Precedence[Do] == 670 just an example of any System symbol that is a function to call *)

Precedence`Infix`Question -> Next, (* Precedence[PatternTest] == 680 *)

Precedence`LinearSyntax`Bang -> Next,

Precedence`LessLess -> Next, (* Precedence[Get] == 720 *)

Precedence`ColonColon -> Next, (* Precedence[MessageName] == 750 *)

Precedence`LongName`InvisiblePrefixScriptBase -> Next,
Precedence`LongName`InvisiblePostfixScriptBase -> Precedence`LongName`InvisiblePrefixScriptBase,

Precedence`Highest -> Next, (* Precedence["foo"] == 1000 just an example of any atom *)
(* Symbol and Under are needed because they have separate Parselets that have to know about precedence *)
Precedence`Symbol -> Precedence`Highest,
Precedence`Under -> Precedence`Highest
|>
