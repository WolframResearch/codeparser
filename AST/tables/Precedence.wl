
(*

Largely derived from documentation:

tutorial/OperatorInputForms
tutorial/Operators
and the System function Precedence

Precedence[] only reports the precedence values associated with operators, used for OUTPUT

With modifications based on empirical observations

*)

<|
Precedence`Unused -> Indeterminate,
Precedence`Colon -> Precedence`Unused,

Precedence`Lowest -> 0,

Precedence`Semi -> Precedence[CompoundExpression], (* 10 *)

Precedence`GreaterGreater -> Precedence[Put], (* 30 *)
Precedence`GreaterGreaterGreater -> Precedence[PutAppend], (* 30 *)

Precedence`Equal -> Precedence[Set], (* 40 *)
Precedence`ColonEqual -> Precedence[SetDelayed], (* 40 *)
Precedence`CaretEqual -> Precedence[UpSet], (* 40 *)
Precedence`CaretColonEqual -> Precedence[UpSetDelayed], (* 40 *)
Precedence`SlashColon -> Precedence`Equal,
Precedence`LongName`Function -> Precedence`Equal,

Precedence`LongName`Therefore -> Precedence[Therefore], (* 50 *)

Precedence`LongName`VerticalSeparator -> Precedence[VerticalSeparator], (* 60 *)

Precedence`SlashSlash -> Precedence[Postfix], (* 70 *)

Precedence`LongName`Colon -> Precedence[Colon], (* 80 *)

Precedence`Amp -> Precedence[Function], (* 90 *)

Precedence`PlusEqual -> Precedence[AddTo], (* 100 *)
Precedence`StarEqual -> Precedence[TimesBy], (* 100 *)
Precedence`MinusEqual -> Precedence[SubtractFrom], (* 100 *)
Precedence`SlashEqual -> Precedence[DivideBy], (* 100 *)

Precedence`SlashDot -> Precedence[ReplaceAll], (* 110 *)
Precedence`SlashSlashDot -> Precedence[ReplaceRepeated], (* 110 *)

Precedence`MinusGreater -> Precedence[Rule], (* 120 *)
Precedence`ColonGreater -> Precedence[RuleDelayed], (* 120 *)
Precedence`LongName`Rule -> Precedence`MinusGreater,
Precedence`LongName`RuleDelayed -> Precedence`MinusGreater,

Precedence`LessMinusGreater -> Precedence[TwoWayRule], (* 125 *)
Precedence`LongName`TwoWayRule -> Precedence`LessMinusGreater,

Precedence`SlashSemi -> Precedence[Condition], (* 130 *)

Precedence`TildeTilde -> Precedence[StringExpression], (* 135 *)

Precedence`Fake`OptionalColon -> Precedence[Optional], (* 140 *)

Precedence`Fake`PatternColon -> Precedence[Pattern], (* 150 *)

Precedence`Bar -> Precedence[Alternatives], (* 160 *)

Precedence`DotDot -> Precedence[Repeated], (* 170 *)
Precedence`DotDotDot -> Precedence[RepeatedNull], (* 170 *)

Precedence`LongName`SuchThat -> Precedence[SuchThat], (* 180 *)

Precedence`LongName`RightTee -> Precedence[RightTee], (* 190 *)

Precedence`LongName`Conditioned -> Precedence[Conditioned], (* 195 *)

Precedence`LongName`Implies -> Precedence[Implies], (* 200 *)

Precedence`LongName`Equivalent -> Precedence[Equivalent], (* 205 *)

Precedence`BarBar -> Precedence[Or], (* 215 *)
Precedence`LongName`Or -> Precedence`BarBar,
Precedence`LongName`Nor -> Precedence`BarBar,

(*
Precedence[Xor] is 215, which is equal to Or.
But this is wrong, a \[Xor] b is between || and &&
*)
Precedence`LongName`Xor -> Next,
Precedence`LongName`Xnor -> Precedence`LongName`Xor,

(*
Precedence[And] is 215, which is equal to Or.
But this is wrong, a && b has higher precedence than a || b
*)
Precedence`AmpAmp -> Next,
Precedence`LongName`And -> Precedence`AmpAmp,
Precedence`LongName`Nand -> Precedence`AmpAmp,

Precedence`Prefix`Bang -> Precedence[Not], (* 230 *)
Precedence`LongName`Not -> Precedence`Prefix`Bang,

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

Precedence`LongName`Distributed -> Precedence[Distributed], (* 250 *)

(*
Precedence[SameQ] is 290
But empirically it is between \[Element] and \[RightTeeArrow]
*)
Precedence`EqualEqualEqual -> Next,
Precedence`EqualBangEqual -> Precedence`EqualEqualEqual,

Precedence`LongName`RightTeeArrow -> Precedence[RightTeeArrow], (* 270 *)
Precedence`LongName`RightArrow -> Precedence[RightArrow], (* 270 *)
Precedence`LongName`LeftRightArrow -> Precedence[LeftRightArrow], (* 270 *)

Precedence`LongName`VerticalBar -> Precedence[VerticalBar], (* 280 *)
Precedence`LongName`NotVerticalBar -> Precedence[NotVerticalBar], (* 280 *)
Precedence`LongName`DoubleVerticalBar -> Precedence[DoubleVerticalBar], (* 280 *)
Precedence`LongName`NotDoubleVerticalBar -> Precedence[NotDoubleVerticalBar], (* 280 *)

Precedence`EqualEqual -> Precedence[Equal], (* 290 *)
Precedence`Greater -> Precedence[Greater], (* 290 *)
Precedence`LessEqual -> Precedence[LessEqual], (* 290 *)
Precedence`GreaterEqual -> Precedence[GreaterEqual], (* 290 *)
Precedence`Less -> Precedence[Less], (* 290 *)
Precedence`BangEqual -> Precedence[Unequal], (* 290 *)
Precedence`LongName`Equal -> Precedence`EqualEqual,
Precedence`LongName`LessEqual -> Precedence`LessEqual,
Precedence`LongName`NotEqual -> Precedence`BangEqual,
Precedence`LongName`GreaterEqual -> Precedence`GreaterEqual,
Precedence`LongName`TildeTilde -> Precedence[TildeTilde], (* 290 *)
Precedence`LongName`NotTildeTilde -> Precedence[NotTildeTilde], (* 290 *)
Precedence`LongName`LeftTriangleEqual -> Precedence[LeftTriangleEqual], (* 290 *)
Precedence`LongName`TildeEqual -> Precedence[TildeEqual], (* 290 *)
Precedence`LongName`TildeFullEqual -> Precedence[TildeFullEqual], (* 290 *)
Precedence`LongName`NotTildeFullEqual -> Precedence[NotTildeFullEqual], (* 290 *)
Precedence`LongName`GreaterTilde -> Precedence[GreaterTilde], (* 290 *)
Precedence`LongName`Tilde -> Precedence[Tilde], (* 290 *)
Precedence`LongName`Proportional -> Precedence[Proportional], (* 290 *)
Precedence`LongName`LessLess -> Precedence[LessLess], (* 290 *)
Precedence`LongName`Congruent -> Precedence[Congruent], (* 290 *)
Precedence`LongName`RightTriangle -> Precedence[RightTriangle], (* 290 *)
Precedence`LongName`LeftTriangle -> Precedence[LeftTriangle], (* 290 *)

Precedence`LongName`DirectedEdge -> Precedence[DirectedEdge], (* 295 *)
Precedence`LongName`UndirectedEdge -> Precedence[UndirectedEdge], (* 295 *)

(*
Precedence[Span] is 305
But empirically it is lower than \[Union]
*)
Precedence`SemiSemi -> Next,

Precedence`LongName`Union -> Precedence[Union], (* 300 *)
Precedence`LongName`Intersection -> Precedence[Intersection], (* 300 *)

Precedence`Infix`Plus -> Precedence[Plus], (* 310 *)
Precedence`Infix`Minus -> Precedence`Infix`Plus,
Precedence`Infix`LongName`PlusMinus -> Precedence[PlusMinus], (* 310 *)
Precedence`Infix`LongName`MinusPlus -> Precedence[MinusPlus], (* 310 *)

Precedence`LongName`ImplicitPlus -> Next,

Precedence`LongName`Sum -> Precedence[Sum], (* 320 *)

Precedence`LongName`Integral -> Next,
Precedence`LongName`ContourIntegral -> Precedence`LongName`Integral,
Precedence`LongName`DoubleContourIntegral -> Precedence`LongName`Integral,
Precedence`LongName`ClockwiseContourIntegral -> Precedence`LongName`Integral,
Precedence`LongName`CounterClockwiseContourIntegral -> Precedence`LongName`Integral,

Precedence`LongName`CirclePlus -> Precedence[CirclePlus], (* 330 *)
Precedence`LongName`CircleMinus -> Precedence[CircleMinus], (* 330 *)

Precedence`LongName`Cup -> Precedence[Cup], (* 340 *)

Precedence`LongName`Cap -> Precedence[Cap], (* 350 *)

Precedence`LongName`Coproduct -> Precedence[Coproduct], (* 360 *)

Precedence`LongName`VerticalTilde -> Precedence[VerticalTilde], (* 370 *)

Precedence`LongName`Product -> Precedence[Product], (* 380 *)

Precedence`LongName`Star -> Precedence[Star], (* 390 *)

Precedence`Star -> Precedence[Times], (* 400 *)
Precedence`LongName`Times -> Precedence`Star,
Precedence`Fake`ImplicitTimes -> Precedence`Star,
Precedence`LongName`InvisibleTimes -> Precedence`Star,

Precedence`LongName`CenterDot -> Precedence[CenterDot], (* 410 *)

Precedence`LongName`CircleTimes -> Precedence[CircleTimes], (* 420 *)

(*
Precedence[TensorWedge] is 500
But empirically it is between \[CircleTimes] and \[Vee]
*)
Precedence`LongName`TensorWedge -> Next,

Precedence`LongName`Vee -> Precedence[Vee], (* 430 *)

Precedence`LongName`Wedge -> Precedence[Wedge], (* 440 *)

Precedence`LongName`Diamond -> Precedence[Backslash], (* 450 *)

Precedence`LongName`Backslash -> Precedence[Backslash], (* 460 *)

Precedence`Slash -> Precedence[Divide], (* 470 *)
Precedence`LongName`Divides -> Precedence`Slash,

Precedence`Prefix`Minus -> Precedence[Minus], (* 480 *)
Precedence`Prefix`Plus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`PlusMinus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`MinusPlus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`Minus -> Precedence`Prefix`Minus,

Precedence`Dot -> Precedence[Dot], (* 490 *)

Precedence`LongName`Cross -> Precedence[Cross], (* 500 *)

Precedence`StarStar -> Precedence[NonCommutativeMultiply], (* 510 *)

Precedence`LongName`CircleDot -> Precedence[CircleDot], (* 520 *)

Precedence`LongName`SmallCircle -> Precedence[SmallCircle], (* 530 *)

Precedence`LongName`Square -> Precedence[Square], (* 540 *)

Precedence`LongName`DifferentialD -> Precedence[DifferentialD], (* 550 *)
Precedence`LongName`Del -> Precedence[Del], (* 550 *)

Precedence`LongName`Sqrt -> Next,

Precedence`LongName`DoubleLongLeftRightArrow -> Precedence[DoubleLongLeftRightArrow], (* 580 *)
Precedence`LongName`DoubleLongRightArrow -> Precedence[DoubleLongRightArrow], (* 580 *)
Precedence`LongName`LongRightArrow -> Precedence[LongRightArrow], (* 580 *)

Precedence`Caret -> Precedence[Power], (* 590 *)

Precedence`LessGreater -> Precedence[StringJoin], (* 600 *)

Precedence`SingleQuote -> Next,

Precedence`Postfix`Bang -> Precedence[Factorial], (* 610 *)

Precedence`BangBang -> Precedence[Factorial2], (* 610 *)

Precedence`LongName`Transpose -> Next,
Precedence`LongName`Conjugate -> Precedence`LongName`Transpose,
Precedence`LongName`ConjugateTranspose -> Precedence`LongName`Transpose,
Precedence`LongName`HermitianConjugate -> Precedence`LongName`Transpose,

Precedence`AtAt -> Precedence[Apply], (* 620 *)
Precedence`SlashAt -> Precedence[Map], (* 620 *)
Precedence`AtAtAt -> Precedence`AtAt,
Precedence`SlashSlashAt -> Precedence[MapAll], (* 620 *)

Precedence`Tilde -> Precedence[Infix], (* 630 *)

Precedence`At -> Precedence[Prefix], (* 640 *)
Precedence`LongName`InvisibleApplication -> Precedence[InvisibleApplication], (* 640 *)

(*
Precedence[RightComposition] is 624
Precedence[Composition] is 625
But empirically they are higher than @
*)
Precedence`SlashStar -> Next,
Precedence`AtStar -> Next,

Precedence`Prefix`PlusPlus -> Precedence[PreIncrement], (* 660 *)
Precedence`Prefix`MinusMinus -> Precedence[PreDecrement], (* 660 *)
Precedence`Postfix`PlusPlus -> Precedence[Increment], (* 660 *)
Precedence`Postfix`MinusMinus -> Precedence[Decrement], (* 660 *)

Precedence`Call -> Precedence[Do], (* 670 just an example of any System symbol that is a function to call *)

Precedence`Infix`Question -> Precedence[PatternTest], (* 680 *)

Precedence`LessLess -> Precedence[Get], (* 720 *)

Precedence`ColonColon -> Precedence[MessageName], (* 750 *)

Precedence`Highest -> Precedence["foo"], (* 1000 just an example of any atom *)
Precedence`LinearSyntax`Bang -> Precedence`Highest,
Precedence`LongName`InvisiblePrefixScriptBase -> Precedence`Highest,
Precedence`LongName`InvisiblePostfixScriptBase -> Precedence`Highest,

Nothing
|>
