
(*
CodeParser Data file

Do not modify this file directly
*)

(*

Largely derived from documentation:

tutorial/OperatorInputForms
tutorial/Operators
and the System function Precedence

Precedence[] only reports the precedence values associated with operators, used for OUTPUT

With modifications based on empirical observations

*)

<|
Precedence`Lowest -> {0, Associativity`NonRight},

Precedence`Comma -> {Next, Associativity`NonRight},
Precedence`LongName`InvisibleComma -> Precedence`Comma,

Precedence`Semi -> {Next, Associativity`NonRight}, (* Precedence[CompoundExpression] == 10 *)

Precedence`GreaterGreater -> {Next, Associativity`NonRight}, (* Precedence[Put] == 30 *)
Precedence`GreaterGreaterGreater -> Precedence`GreaterGreater, (* Precedence[PutAppend] == 30 *)

Precedence`Equal -> {Next, Associativity`Right}, (* Precedence[Set] == 40 *)
Precedence`ColonEqual -> Precedence`Equal, (* Precedence[SetDelayed] == 40 *)
Precedence`CaretEqual -> Precedence`Equal, (* Precedence[UpSet] == 40 *)
Precedence`CaretColonEqual -> Precedence`Equal, (* Precedence[UpSetDelayed] == 40 *)
Precedence`LongName`Function -> Precedence`Equal,
(*
Precedence`Fake`EqualDot is used by minifier
*)
Precedence`Fake`EqualDot -> Precedence`Equal,
(*
added in 12.2:
|->
*)
Precedence`BarMinusGreater -> Precedence`Equal,

(*
Make /: higher than = and :=
*)
Precedence`SlashColon -> {Next, Associativity`Right},

Precedence`LongName`Because -> {Next, Associativity`NonRight}, (* Precedence[Because] == 50 *)
(*
DISCREPANCY
Precedence[Therefore] is 50
But it really should be higher than Because, which is also 50
*)
Precedence`LongName`Therefore -> {Next, Associativity`Right}, (* Precedence[Therefore] == 50 *)

Precedence`LongName`VerticalSeparator -> {Next, Associativity`NonRight}, (* Precedence[VerticalSeparator] == 60 *)

Precedence`SlashSlash -> {Next, Associativity`NonRight}, (* Precedence[Postfix] == 70 *)

(*
added in 12.2:
ApplyTo
*)
Precedence`SlashSlashEqual -> {Next, Associativity`Right}, (* Precedence[ApplyTo] == 75 *)

Precedence`LongName`Colon -> {Next, Associativity`NonRight}, (* Precedence[Colon] == 80 *)

Precedence`Amp -> {Next, Associativity`NonRight}, (* Precedence[Function] == 90 *)

Precedence`PlusEqual -> {Next, Associativity`Right}, (* Precedence[AddTo] == 100 *)
Precedence`StarEqual -> Precedence`PlusEqual, (* Precedence[TimesBy] == 100 *)
Precedence`MinusEqual -> Precedence`PlusEqual, (* Precedence[SubtractFrom] == 100 *)
Precedence`SlashEqual -> Precedence`PlusEqual, (* Precedence[DivideBy] == 100 *)

Precedence`SlashDot -> {Next, Associativity`NonRight}, (* Precedence[ReplaceAll] == 110 *)
Precedence`SlashSlashDot -> Precedence`SlashDot, (* Precedence[ReplaceRepeated] == 110 *)

Precedence`MinusGreater -> {Next, Associativity`Right}, (* Precedence[Rule] == 120 *)
Precedence`ColonGreater -> Precedence`MinusGreater, (* Precedence[RuleDelayed] == 120 *)
Precedence`LongName`Rule -> Precedence`MinusGreater,
Precedence`LongName`RuleDelayed -> Precedence`MinusGreater,

(*
added in 11.1:
TwoWayRule

TwoWayRule did not have correct precedence until 12.0

<-> was UndirectedEdge before 11.2.
From 11.2 forward, <-> is TwoWayRule, but it kept the precedence of UndirectedEdge (meaning <-> and \[TwoWayRule] had different precedences)

From 12.0 forward, <-> has correct precedence.
*)
Precedence`LessMinusGreater -> {Next, Associativity`Right}, (* Precedence[System`TwoWayRule] == 125 *)
Precedence`LongName`TwoWayRule -> Precedence`LessMinusGreater,

Precedence`SlashSemi -> {Next, Associativity`NonRight}, (* Precedence[Condition] == 130 *)

Precedence`TildeTilde -> {Next, Associativity`NonRight}, (* Precedence[StringExpression] == 135 *)

Precedence`Fake`OptionalColon -> {Next, Associativity`NonRight}, (* Precedence[Optional] == 140 *)

Precedence`Fake`PatternColon -> {Next, Associativity`NonRight}, (* Precedence[Pattern] == 150 *)

Precedence`Bar -> {Next, Associativity`NonRight}, (* Precedence[Alternatives] == 160 *)

Precedence`DotDot -> {Next, Associativity`NonRight}, (* Precedence[Repeated] == 170 *)
Precedence`DotDotDot -> Precedence`DotDot, (* Precedence[RepeatedNull] == 170 *)

Precedence`LongName`SuchThat -> {Next, Associativity`Right}, (* Precedence[SuchThat] == 180 *)

Precedence`LongName`UpTee -> {Next, Associativity`NonRight}, (* Precedence[UpTee] == 190 *)
Precedence`LongName`DownTee -> Precedence`LongName`UpTee, (* Precedence[DownTee] == 190 *)
Precedence`LongName`LeftTee -> Precedence`LongName`UpTee, (* Precedence[LeftTee] == 190 *)
Precedence`LongName`DoubleLeftTee -> Precedence`LongName`UpTee, (* Precedence[DoubleLeftTee] == 190 *)
Precedence`LongName`Perpendicular -> Precedence`LongName`UpTee, (* Precedence[Perpendicular] == 190 *)
(*
DISCREPANCY
Precedence[RightTee] is 190
But it really should be higher than UpTee, which is also 190
*)
Precedence`LongName`RightTee -> {Next, Associativity`Right}, (* Precedence[RightTee] == 190 *)
Precedence`LongName`DoubleRightTee -> Precedence`LongName`RightTee, (* Precedence[DoubleRightTee] == 190 *)

Precedence`LongName`Conditioned -> {Next, Associativity`NonRight}, (* Precedence[Conditioned] == 195 *)

Precedence`LongName`Implies -> {Next, Associativity`Right}, (* Precedence[Implies] == 200 *)
(*
DISCREPANCY
Precedence[RoundImplies] is 240
But this is wrong, RoundImplies has same precedence as Implies
*)
Precedence`LongName`RoundImplies -> Precedence`LongName`Implies,

Precedence`LongName`Equivalent -> {Next, Associativity`NonRight}, (* Precedence[Equivalent] == 205 *)

Precedence`BarBar -> {Next, Associativity`NonRight}, (* Precedence[Or] == 215 *)
Precedence`LongName`Or -> Precedence`BarBar,
Precedence`LongName`Nor -> Precedence`BarBar,

(*
DISCREPANCY
Precedence[Xor] is 215, which is equal to Or.
But this is wrong, a \[Xor] b is between || and &&
*)
Precedence`LongName`Xor -> {Next, Associativity`NonRight},
Precedence`LongName`Xnor -> Precedence`LongName`Xor,

(*
DISCREPANCY
Precedence[And] is 215, which is equal to Or.
But this is wrong, a && b has higher precedence than a || b
*)
Precedence`AmpAmp -> {Next, Associativity`NonRight},
Precedence`LongName`And -> Precedence`AmpAmp,
Precedence`LongName`Nand -> Precedence`AmpAmp,

Precedence`Prefix`Bang -> {Next, Associativity`NonRight}, (* Precedence[Not] == 230 *)
Precedence`LongName`Not -> Precedence`Prefix`Bang,
Precedence`Fake`Prefix`BangBang -> Precedence`Prefix`Bang,

Precedence`LongName`ForAll -> {Next, Associativity`NonRight}, (* Precedence[ForAll] == 240 *)
Precedence`LongName`Exists -> Precedence`LongName`ForAll, (* Precedence[Exists] == 240 *)
Precedence`LongName`NotExists -> Precedence`LongName`ForAll, (* Precedence[NotExists] == 240 *)

Precedence`Class`SetRelations -> {Next, Associativity`NonRight}, (* Precedence[Element] == 250 *)

(*
DISCREPANCY
Precedence[SameQ] is 290
But empirically it is between \[Element] and \[RightTeeArrow]
*)
Precedence`EqualEqualEqual -> {Next, Associativity`NonRight},
Precedence`EqualBangEqual -> Precedence`EqualEqualEqual,

Precedence`Class`HorizontalArrows -> {Next, Associativity`NonRight}, (* Precedence[LeftArrow] == 270 *)

Precedence`Class`VectorOperators -> Precedence`Class`HorizontalArrows, (* Precedence[LeftVector] == 270 *)

Precedence`Class`DiagonalArrowOperators -> Precedence`Class`HorizontalArrows, (* Precedence[UpperLeftArrow] == 270 *)

Precedence`LongName`VerticalBar -> {Next, Associativity`NonRight}, (* Precedence[VerticalBar] == 280 *)
Precedence`LongName`NotVerticalBar -> Precedence`LongName`VerticalBar, (* Precedence[NotVerticalBar] == 280 *)
Precedence`LongName`DoubleVerticalBar -> Precedence`LongName`VerticalBar, (* Precedence[DoubleVerticalBar] == 280 *)
Precedence`LongName`NotDoubleVerticalBar -> Precedence`LongName`VerticalBar, (* Precedence[NotDoubleVerticalBar] == 280 *)

Precedence`Class`OrderingOperators -> {Next, Associativity`NonRight}, (* Precedence[LeftTriangle] == 290 *)


Precedence`Class`Inequality -> Precedence`Class`OrderingOperators, (* Precedence[Equal] == 290 *)

Precedence`LongName`DirectedEdge -> {Next, Associativity`Right}, (* Precedence[DirectedEdge] == 295 *)
Precedence`LongName`UndirectedEdge -> Precedence`LongName`DirectedEdge, (* Precedence[UndirectedEdge] == 295 *)

(*
DISCREPANCY
Precedence[Span] is 305
But empirically it is lower than \[Union]
*)
Precedence`SemiSemi -> {Next, Associativity`NonRight},

Precedence`Class`UnionOperators -> {Next, Associativity`NonRight}, (* Precedence[Union] == 300 *)

Precedence`Class`IntersectionOperators -> {Next, Associativity`NonRight}, (* Precedence[Intersection] == 305 *)

Precedence`Infix`Plus -> {Next, Associativity`NonRight}, (* Precedence[Plus] == 310 *)
Precedence`Infix`Minus -> Precedence`Infix`Plus,
Precedence`Infix`LongName`PlusMinus -> Precedence`Infix`Plus, (* Precedence[PlusMinus] == 310 *)
Precedence`Infix`LongName`MinusPlus -> Precedence`Infix`Plus, (* Precedence[MinusPlus] == 310 *)
Precedence`Infix`LongName`Minus -> Precedence`Infix`Plus, (* 310 *)

Precedence`LongName`ImplicitPlus -> {Next, Associativity`NonRight},

Precedence`LongName`Sum -> {Next, Associativity`NonRight}, (* Precedence[Sum] == 320 *)

Precedence`Class`IntegrationOperators -> {Next, Associativity`NonRight}, (* Precedence[Integrate] == 325 *)
Precedence`LongName`ExpectationE -> Precedence`Class`IntegrationOperators, (* Precedence[ExpectationE] == 325 *)
Precedence`LongName`ProbabilityPr -> Precedence`Class`IntegrationOperators, (* Precedence[ProbabilityPr] == 325 *)

Precedence`LongName`CirclePlus -> {Next, Associativity`NonRight}, (* Precedence[CirclePlus] == 330 *)
Precedence`LongName`CircleMinus -> Precedence`LongName`CirclePlus, (* Precedence[CircleMinus] == 330 *)

Precedence`LongName`Cup -> {Next, Associativity`NonRight}, (* Precedence[Cup] == 340 *)

Precedence`LongName`Cap -> {Next, Associativity`NonRight}, (* Precedence[Cap] == 350 *)

Precedence`Infix`LongName`Coproduct -> {Next, Associativity`NonRight}, (* Precedence[Coproduct] == 360 *)

Precedence`LongName`VerticalTilde -> {Next, Associativity`NonRight}, (* Precedence[VerticalTilde] == 370 *)

Precedence`LongName`Product -> {Next, Associativity`NonRight}, (* Precedence[Product] == 380 *)
Precedence`LongName`ContinuedFractionK -> Precedence`LongName`Product, (* Precedence[ContinuedFractionK] == 380 *)

Precedence`LongName`Star -> {Next, Associativity`NonRight}, (* Precedence[Star] == 390 *)

Precedence`Star -> {Next, Associativity`NonRight}, (* Precedence[Times] == 400 *)
Precedence`LongName`Times -> Precedence`Star,
Precedence`LongName`InvisibleTimes -> Precedence`Star,
Precedence`Fake`ImplicitTimes -> Precedence`Star,

Precedence`LongName`CenterDot -> {Next, Associativity`NonRight}, (* Precedence[CenterDot] == 410 *)

Precedence`Infix`LongName`CircleTimes -> {Next, Associativity`NonRight}, (* Precedence[CircleTimes] == 420 *)

Precedence`LongName`Vee -> {Next, Associativity`NonRight}, (* Precedence[Vee] == 430 *)

Precedence`LongName`Wedge -> {Next, Associativity`NonRight}, (* Precedence[Wedge] == 440 *)

Precedence`LongName`Diamond -> {Next, Associativity`NonRight}, (* Precedence[Diamond] == 450 *)

Precedence`LongName`Backslash -> {Next, Associativity`NonRight}, (* Precedence[Backslash] == 460 *)

Precedence`Slash -> {Next, Associativity`NonRight}, (* Precedence[Divide] == 470 *)
Precedence`LongName`Divide -> Precedence`Slash,
Precedence`LongName`Divides -> Precedence`Slash,
Precedence`LongName`DivisionSlash -> Precedence`Slash,

Precedence`Prefix`Minus -> {Next, Associativity`NonRight}, (* Precedence[Minus] == 480 *)
Precedence`Prefix`Plus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`PlusMinus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`MinusPlus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`Minus -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`CircleTimes -> Precedence`Prefix`Minus,
Precedence`Prefix`LongName`Coproduct -> Precedence`Prefix`Minus,

Precedence`Dot -> {Next, Associativity`NonRight}, (* Precedence[Dot] == 490 *)

Precedence`LongName`TensorProduct -> {Next, Associativity`NonRight}, (* Precedence[TensorProduct] == 495 *)

Precedence`LongName`Cross -> {Next, Associativity`NonRight}, (* Precedence[Cross] == 500 *)
Precedence`LongName`TensorWedge -> Precedence`LongName`Cross, (* Precedence[TensorWedge] == 500 *)

Precedence`StarStar -> {Next, Associativity`NonRight}, (* Precedence[NonCommutativeMultiply] == 510 *)

Precedence`LongName`CircleDot -> {Next, Associativity`NonRight}, (* Precedence[CircleDot] == 520 *)
Precedence`LongName`PermutationProduct -> Precedence`LongName`CircleDot, (* Precedence[PermutationProduct] == 520 *)

Precedence`LongName`SmallCircle -> {Next, Associativity`NonRight}, (* Precedence[SmallCircle] == 530 *)

Precedence`LongName`Square -> {Next, Associativity`NonRight}, (* Precedence[Square] == 540 *)

Precedence`LongName`Del -> {Next, Associativity`NonRight}, (* Precedence[Del] == 550 *)

(*
DISCREPANCY
Precedence[Piecewise] is 480
But empirically, it is between Del and DifferentialD

Also, Piecewise is documented to have higher precedence than ::

*)
Precedence`LongName`Piecewise -> {Next, Associativity`NonRight},

(*
DISCREPANCY
Precedence[DifferentialD] is 550
But empirically, it is between Piecewise and Sqrt
*)
Precedence`LongName`DifferentialD -> {Next, Associativity`NonRight},
Precedence`LongName`CapitalDifferentialD -> Precedence`LongName`DifferentialD,

Precedence`LongName`Sqrt -> {Next, Associativity`NonRight},
(*
added in 12.1:
CubeRoot
*)
Precedence`LongName`CubeRoot -> Precedence`LongName`Sqrt,

Precedence`Class`VerticalArrowOperators -> {Next, Associativity`NonRight}, (* Precedence[UpArrow] == 580 *)

Precedence`Class`VerticalVectorOperators -> Precedence`Class`VerticalArrowOperators, (* Precedence[RightUpVector] == 580 *)

Precedence`Caret -> {Next, Associativity`Right}, (* Precedence[Power] == 590 *)

Precedence`LessGreater -> {Next, Associativity`NonRight}, (* Precedence[StringJoin] == 600 *)

Precedence`SingleQuote -> {Next, Associativity`NonRight},

Precedence`LongName`Transpose -> {Next, Associativity`NonRight},
Precedence`LongName`Conjugate -> Precedence`LongName`Transpose,
Precedence`LongName`ConjugateTranspose -> Precedence`LongName`Transpose,
Precedence`LongName`HermitianConjugate -> Precedence`LongName`Transpose,

Precedence`Postfix`Bang -> {Next, Associativity`NonRight}, (* Precedence[Factorial] == 610 *)
Precedence`Postfix`BangBang -> Precedence`Postfix`Bang, (* Precedence[Factorial2] == 610 *)

Precedence`AtAt -> {Next, Associativity`Right}, (* Precedence[Apply] == 620 *)
Precedence`SlashAt -> Precedence`AtAt, (* Precedence[Map] == 620 *)
(*
changed in 13.1:
@@@

before 13.1:
a @@@ b parsed as Apply[a, b, {1}]

13.1 onward:
a @@@ b parses as MapApply[a, b]
*)
Precedence`AtAtAt -> Precedence`AtAt, (* Precedence[System`MapApply] == 620 *)
Precedence`SlashSlashAt -> Precedence`AtAt, (* Precedence[MapAll] == 620 *)

Precedence`Tilde -> {Next, Associativity`NonRight}, (* Precedence[Infix] == 630 *)

Precedence`At -> {Next, Associativity`Right}, (* Precedence[Prefix] == 640 *)
Precedence`LongName`InvisibleApplication -> Precedence`At,

(*
added in 12.2:
Application
*)
Precedence`LongName`Application -> {Next, Associativity`NonRight}, (* Precedence[??] == 645 *)

Precedence`SlashStar -> {Next, Associativity`NonRight}, (* Precedence[RightComposition] == 648 *)
Precedence`AtStar -> {Next, Associativity`NonRight}, (* Precedence[Composition] == 650 *)

Precedence`Prefix`PlusPlus -> {Next, Associativity`NonRight}, (* Precedence[PreIncrement] == 660 *)
Precedence`Prefix`MinusMinus -> Precedence`Prefix`PlusPlus, (* Precedence[PreDecrement] == 660 *)

(*
DISCREPANCY
Precedence[Increment] is 660
Precedence[PreIncrement] is 660
But empirically Increment is higher than PreIncrement
*)
Precedence`Postfix`PlusPlus -> {Next, Associativity`NonRight}, (* Precedence[Increment] == 660 *)
Precedence`Postfix`MinusMinus -> Precedence`Postfix`PlusPlus, (* Precedence[Decrement] == 660 *)

Precedence`Call -> {Next, Associativity`NonRight}, (* Precedence[Do] == 670 just an example of any System symbol that is a function to call *)

Precedence`Infix`Question -> {Next, Associativity`NonRight}, (* Precedence[PatternTest] == 680 *)

Precedence`LinearSyntax`Bang -> {Next, Associativity`NonRight},

Precedence`LessLess -> {Next, Associativity`NonRight}, (* Precedence[Get] == 720 *)

Precedence`ColonColon -> {Next, Associativity`NonRight}, (* Precedence[MessageName] == 750 *)

Precedence`LongName`InvisiblePrefixScriptBase -> {Next, Associativity`NonRight},
Precedence`LongName`InvisiblePostfixScriptBase -> Precedence`LongName`InvisiblePrefixScriptBase,

Precedence`Highest -> {Next, Associativity`NonRight}, (* Precedence["foo"] == 1000 just an example of any atom *)
(* Symbol and Under are needed because they have separate Parselets that have to know about precedence *)
Precedence`Symbol -> Precedence`Highest,
Precedence`Under -> Precedence`Highest
|>
