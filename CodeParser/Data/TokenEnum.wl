
(*
CodeParser Data file

Do not modify this file directly
*)

(*

The token enum

This is a complete enumeration of all tokens in Wolfram Language

*)

<|
Token`Unknown -> 0,
Token`EndOfFile -> Next,
Token`Symbol -> Next,
Token`String -> Next,
Token`Integer -> Next,
Token`Real -> Next,
Token`Rational -> Next,
Token`LinearSyntaxBlob -> Next,

(*
trivia

Any Buffers before trivia and any Buffers after trivia serve the purpose of
allowing fast testing of trivia (just a bit mask)
*)
Token`InternalNewline -> Next, (*8*)
Token`Comment -> Next,
Token`Whitespace -> Next,
Token`Buffer1 -> Next,
Token`ToplevelNewline -> Next,

Token`Buffer2 -> Next,
Token`Buffer3 -> Next,
Token`Buffer4 -> Next,

(* errors *)
Token`Error`ExpectedEqual -> Next, (*16*)
(*
Order of First appearing here is important.
During generation, values that are not Next are removed
*)
Token`Error`First -> Token`Error`ExpectedEqual,
Token`Error`Number -> Next,
Token`Error`UnhandledCharacter -> Next,
Token`Error`ExpectedLetterlike -> Next,
Token`Error`Aborted -> Next,
Token`Error`ExpectedOperand -> Next,
Token`Error`ExpectedTag -> Next,
Token`Error`ExpectedFile -> Next,
Token`Error`UnexpectedCloser -> Next,
(* implicit  Null  in  f[,2]  *)
Token`Error`PrefixImplicitNull -> Next,
(* implicit  Null  in  f[1,]  *)
Token`Error`InfixImplicitNull -> Next,
Token`Error`UnsafeCharacterEncoding -> Next,
(*
Unterminated errors

Any buffers before unterminateds and after unterminateds serve the purpose of giving the correct
value to unterminateds to allow fast checking with a bit mask.
*)
Token`Error`UnterminatedComment -> Next, (*28*)
Token`Error`Unterminated`First -> Token`Error`UnterminatedComment,
Token`Error`UnterminatedString -> Next,
Token`Error`UnterminatedFileString -> Next,
Token`Error`UnterminatedLinearSyntaxBlob -> Next,
Token`Error`UnsupportedToken -> Next, (*32*)
Token`Error`Unterminated`End -> Token`Error`UnsupportedToken,
Token`Error`UnexpectedCommentCloser -> Next,
Token`Error`End -> Next,

(* 1 character tokens *)
Token`Dot -> Next, (* . *)
Token`Colon -> Next, (* : *)
Token`OpenParen -> Next, (* ( *)
Token`CloseParen -> Next, (* ) *)
Token`OpenSquare -> Next, (* [ *)
Token`CloseSquare -> Next, (* ] *)
Token`Comma -> Next, (* , *)
Token`OpenCurly -> Next, (* { *)
Token`CloseCurly -> Next, (* } *)
Token`Equal -> Next, (* = *)
Token`Bang -> Next, (* ! *)
Token`Under -> Next, (* _ *)
Token`Less -> Next, (* < *)
Token`Greater -> Next, (* > *)
Token`Minus -> Next, (* - *)
Token`Bar -> Next, (* | *)
Token`Semi -> Next, (* ; *)
Token`Hash -> Next, (* # *)
Token`Amp -> Next, (* & *)
Token`Slash -> Next, (* / *)
Token`At -> Next, (* @ *)
Token`Plus -> Next, (* + *)
Token`Tilde -> Next, (* ~ *)
Token`Star -> Next, (* * *)
Token`Caret -> Next, (* ^ *)
Token`SingleQuote -> Next, (* ' *)
Token`Percent -> Next, (* % *)
Token`Question -> Next, (* ? *)

(* 2 character tokens *)
Token`DotDot -> Next, (* .. *)
Token`ColonColon -> Next, (* :: *)
Token`ColonEqual -> Next, (* := *)
Token`ColonGreater -> Next, (* :> *)
Token`EqualEqual -> Next, (* == *)
Token`UnderUnder -> Next, (* __ *)
Token`UnderDot -> Next, (* _. *)
Token`LessBar -> Next, (* <| *)
Token`LessLess -> Next, (* << *)
Token`LessGreater -> Next, (* <> *)
Token`LessEqual -> Next, (* <= *)
Token`GreaterGreater -> Next, (* >> *)
Token`GreaterEqual -> Next, (* >= *)
Token`MinusGreater -> Next, (* -> *)
Token`MinusMinus -> Next, (* -- *)
Token`MinusEqual -> Next, (* -= *)
Token`BarBar -> Next, (* || *)
Token`BarGreater -> Next, (* |> *)
Token`SemiSemi -> Next, (* ;; *)
Token`AmpAmp -> Next, (* && *)
Token`SlashAt -> Next, (* /@ *)
Token`SlashSemi -> Next, (* /; *)
Token`SlashDot -> Next, (* /. *)
Token`SlashSlash -> Next, (* // *)
Token`SlashColon -> Next, (* /: *)
Token`SlashEqual -> Next, (* /= *)
Token`SlashStar -> Next, (* /* *)
Token`AtAt -> Next, (* @@ *)
Token`AtStar -> Next, (* @* *)
Token`PlusPlus -> Next, (* ++ *)
Token`PlusEqual -> Next, (* += *)
Token`TildeTilde -> Next, (* ~~ *)
Token`StarEqual -> Next, (* *= *)
Token`StarStar -> Next, (* ** *)
Token`CaretEqual -> Next, (* ^= *)
Token`HashHash -> Next, (* ## *)
Token`BangEqual -> Next, (* != *)
(*
!! is a real token: postfix for Factorial2,
so when prefix !! is encountered, it is convenient to also treat it as a single token
!!a is Not[Not[a]]
*)
Token`BangBang -> Next, (* !! *)
Token`QuestionQuestion -> Next, (* ?? *)

(* 3 character tokens *)
Token`DotDotDot -> Next, (* ... *)
Token`EqualEqualEqual -> Next, (* === *)
Token`EqualBangEqual -> Next, (* =!= *)
Token`UnderUnderUnder -> Next, (* ___ *)
Token`SlashSlashDot -> Next, (* //. *)
Token`AtAtAt -> Next, (* @@@ *)
Token`LessMinusGreater -> Next, (* <-> *)
Token`SlashSlashAt -> Next, (* //@ *)
Token`CaretColonEqual -> Next, (* ^:= *)
Token`GreaterGreaterGreater -> Next, (* >>> *)
(*
added in 12.2:
|->
//=
*)
Token`BarMinusGreater -> Next, (* |-> *)
Token`SlashSlashEqual -> Next, (* //= *)
(*
added in 13.1:
::[
*)
Token`ColonColonOpenSquare -> Next, (* ::[ *)

(* variable length character tokens *)
Token`PercentPercent -> Next, (* %% *)

(* Linear syntax tokens *)
Token`LinearSyntax`Bang -> Next, (* \! *)
Token`LinearSyntax`CloseParen -> Next, (* \) *)
Token`LinearSyntax`At -> Next, (* \@ *)
Token`LinearSyntax`Amp -> Next, (* \& *)
Token`LinearSyntax`Star -> Next, (* \* *)
Token`LinearSyntax`Under -> Next, (* \_ *)
Token`LinearSyntax`Caret -> Next, (* \^ *)
Token`LinearSyntax`Space -> Next, (* \  *)
Token`LinearSyntax`Percent -> Next, (* \% *)
Token`LinearSyntax`Plus -> Next, (* \+ *)
Token`LinearSyntax`Slash -> Next, (* \/ *)
Token`LinearSyntax`BackTick -> Next, (* \` *)

(* Fake tokens *)

(* implicit Times operator in  a b  *)
Token`Fake`ImplicitTimes -> Next,

(* implicit  Null  in  a; *)
Token`Fake`ImplicitNull -> Next,
(* implicit  1  in  ;;b  *)
Token`Fake`ImplicitOne -> Next,
(* implicit  All  in  a;;  *)
Token`Fake`ImplicitAll -> Next,

(*
Used when parsing boxes

The FE treats  ( *  and  * )  as tokens
(broken up here so as to not mess up the comment)
*)
Token`Boxes`OpenParenStar -> Next,

(* variable length character tokens *)
(*
The FE treats  ***** )  as a single token
*)
Token`Boxes`StarCloseParen -> Next,
(*
The FE treats  ''''  as a single token
*)
Token`Boxes`MultiSingleQuote -> Next,

(*
The FE treats  <space><space><space>  as a single token
*)
Token`Boxes`MultiWhitespace -> Next,

(* Token`Boxes`LongName`LeftSkeleton -> Next,

Token`Boxes`LongName`RightSkeleton -> Next, *)

(*
Parsing  f.m  as a leaf from the front end (from example input such as <<f.m)
*)
(*Token`Other -> Next,*)

(*
All multi-byte character tokens

Luckily, they all have long names to use for identification
*)
Token`LongName`Not -> Next,
Token`LongName`PlusMinus -> Next,
Token`LongName`CenterDot -> Next,
Token`LongName`Times -> Next,
Token`LongName`Divide -> Next,
Token`LongName`OpenCurlyQuote -> Next,
Token`LongName`CloseCurlyQuote -> Next,
Token`LongName`OpenCurlyDoubleQuote -> Next,
Token`LongName`CloseCurlyDoubleQuote -> Next,
Token`LongName`InvisibleTimes -> Next,
Token`LongName`LeftArrow -> Next,
Token`LongName`UpArrow -> Next,
Token`LongName`RightArrow -> Next,
Token`LongName`DownArrow -> Next,
Token`LongName`LeftRightArrow -> Next,
Token`LongName`UpDownArrow -> Next,
Token`LongName`UpperLeftArrow -> Next,
Token`LongName`UpperRightArrow -> Next,
Token`LongName`LowerRightArrow -> Next,
Token`LongName`LowerLeftArrow -> Next,
Token`LongName`LeftTeeArrow -> Next,
Token`LongName`UpTeeArrow -> Next,
Token`LongName`RightTeeArrow -> Next,
Token`LongName`DownTeeArrow -> Next,
Token`LongName`LeftVector -> Next,
Token`LongName`DownLeftVector -> Next,
Token`LongName`RightUpVector -> Next,
Token`LongName`LeftUpVector -> Next,
Token`LongName`RightVector -> Next,
Token`LongName`DownRightVector -> Next,
Token`LongName`RightDownVector -> Next,
Token`LongName`LeftDownVector -> Next,
Token`LongName`RightArrowLeftArrow -> Next,
Token`LongName`UpArrowDownArrow -> Next,
Token`LongName`LeftArrowRightArrow -> Next,
Token`LongName`ReverseEquilibrium -> Next,
Token`LongName`Equilibrium -> Next,
Token`LongName`DoubleLeftArrow -> Next,
Token`LongName`DoubleUpArrow -> Next,
Token`LongName`DoubleRightArrow -> Next,
Token`LongName`DoubleDownArrow -> Next,
Token`LongName`DoubleLeftRightArrow -> Next,
Token`LongName`DoubleUpDownArrow -> Next,
Token`LongName`LeftArrowBar -> Next,
Token`LongName`RightArrowBar -> Next,
Token`LongName`DownArrowUpArrow -> Next,
Token`LongName`ForAll -> Next,
Token`LongName`PartialD -> Next,
Token`LongName`Exists -> Next,
Token`LongName`NotExists -> Next,
Token`LongName`Laplacian -> Next,
Token`LongName`Del -> Next,
Token`LongName`Element -> Next,
Token`LongName`NotElement -> Next,
Token`LongName`ReverseElement -> Next,
Token`LongName`NotReverseElement -> Next,
Token`LongName`SuchThat -> Next,
Token`LongName`Product -> Next,
Token`LongName`Coproduct -> Next,
Token`LongName`Sum -> Next,
Token`LongName`Minus -> Next,
Token`LongName`MinusPlus -> Next,
Token`LongName`DivisionSlash -> Next,
Token`LongName`Backslash -> Next,
Token`LongName`SmallCircle -> Next,
Token`LongName`Sqrt -> Next,
Token`LongName`CubeRoot -> Next,
Token`LongName`Proportional -> Next,
Token`LongName`Divides -> Next,
Token`LongName`DoubleVerticalBar -> Next,
Token`LongName`NotDoubleVerticalBar -> Next,
Token`LongName`And -> Next,
Token`LongName`Or -> Next,
Token`LongName`Integral -> Next,
Token`LongName`ContourIntegral -> Next,
Token`LongName`DoubleContourIntegral -> Next,
Token`LongName`ClockwiseContourIntegral -> Next,
Token`LongName`CounterClockwiseContourIntegral -> Next,
Token`LongName`Therefore -> Next,
Token`LongName`Because -> Next,
Token`LongName`Colon -> Next,
Token`LongName`Proportion -> Next,
Token`LongName`Tilde -> Next,
Token`LongName`VerticalTilde -> Next,
Token`LongName`NotTilde -> Next,
Token`LongName`EqualTilde -> Next,
Token`LongName`TildeEqual -> Next,
Token`LongName`NotTildeEqual -> Next,
Token`LongName`TildeFullEqual -> Next,
Token`LongName`NotTildeFullEqual -> Next,
Token`LongName`TildeTilde -> Next,
Token`LongName`NotTildeTilde -> Next,
Token`LongName`CupCap -> Next,
Token`LongName`HumpDownHump -> Next,
Token`LongName`HumpEqual -> Next,
Token`LongName`DotEqual -> Next,
Token`LongName`NotEqual -> Next,
Token`LongName`Congruent -> Next,
Token`LongName`NotCongruent -> Next,
Token`LongName`LessEqual -> Next,
Token`LongName`GreaterEqual -> Next,
Token`LongName`LessFullEqual -> Next,
Token`LongName`GreaterFullEqual -> Next,
Token`LongName`NotLessFullEqual -> Next,
Token`LongName`NotGreaterFullEqual -> Next,
Token`LongName`LessLess -> Next,
Token`LongName`GreaterGreater -> Next,
Token`LongName`NotCupCap -> Next,
Token`LongName`NotLess -> Next,
Token`LongName`NotGreater -> Next,
Token`LongName`NotLessEqual -> Next,
Token`LongName`NotGreaterEqual -> Next,
Token`LongName`LessTilde -> Next,
Token`LongName`GreaterTilde -> Next,
Token`LongName`NotLessTilde -> Next,
Token`LongName`NotGreaterTilde -> Next,
Token`LongName`LessGreater -> Next,
Token`LongName`GreaterLess -> Next,
Token`LongName`NotLessGreater -> Next,
Token`LongName`NotGreaterLess -> Next,
Token`LongName`Precedes -> Next,
Token`LongName`Succeeds -> Next,
Token`LongName`PrecedesSlantEqual -> Next,
Token`LongName`SucceedsSlantEqual -> Next,
Token`LongName`PrecedesTilde -> Next,
Token`LongName`SucceedsTilde -> Next,
Token`LongName`NotPrecedes -> Next,
Token`LongName`NotSucceeds -> Next,
Token`LongName`Subset -> Next,
Token`LongName`Superset -> Next,
Token`LongName`NotSubset -> Next,
Token`LongName`NotSuperset -> Next,
Token`LongName`SubsetEqual -> Next,
Token`LongName`SupersetEqual -> Next,
Token`LongName`NotSubsetEqual -> Next,
Token`LongName`NotSupersetEqual -> Next,
Token`LongName`UnionPlus -> Next,
Token`LongName`SquareSubset -> Next,
Token`LongName`SquareSuperset -> Next,
Token`LongName`SquareSubsetEqual -> Next,
Token`LongName`SquareSupersetEqual -> Next,
Token`LongName`SquareIntersection -> Next,
Token`LongName`SquareUnion -> Next,
Token`LongName`CirclePlus -> Next,
Token`LongName`CircleMinus -> Next,
Token`LongName`CircleTimes -> Next,
Token`LongName`CircleDot -> Next,
Token`LongName`RightTee -> Next,
Token`LongName`LeftTee -> Next,
Token`LongName`DownTee -> Next,
Token`LongName`UpTee -> Next,
Token`LongName`DoubleRightTee -> Next,
Token`LongName`LeftTriangle -> Next,
Token`LongName`RightTriangle -> Next,
Token`LongName`LeftTriangleEqual -> Next,
Token`LongName`RightTriangleEqual -> Next,
Token`LongName`Xor -> Next,
Token`LongName`Nand -> Next,
Token`LongName`Nor -> Next,
Token`LongName`Wedge -> Next,
Token`LongName`Vee -> Next,
Token`LongName`Intersection -> Next,
Token`LongName`Union -> Next,
Token`LongName`Diamond -> Next,
Token`LongName`Star -> Next,
Token`LongName`LessEqualGreater -> Next,
Token`LongName`GreaterEqualLess -> Next,
Token`LongName`NotPrecedesSlantEqual -> Next,
Token`LongName`NotSucceedsSlantEqual -> Next,
Token`LongName`NotSquareSubsetEqual -> Next,
Token`LongName`NotSquareSupersetEqual -> Next,
Token`LongName`NotPrecedesTilde -> Next,
Token`LongName`NotSucceedsTilde -> Next,
Token`LongName`NotLeftTriangle -> Next,
Token`LongName`NotRightTriangle -> Next,
Token`LongName`NotLeftTriangleEqual -> Next,
Token`LongName`NotRightTriangleEqual -> Next,
Token`LongName`LeftCeiling -> Next,
Token`LongName`RightCeiling -> Next,
Token`LongName`LeftFloor -> Next,
Token`LongName`RightFloor -> Next,
Token`LongName`Cap -> Next,
Token`LongName`Cup -> Next,
Token`LongName`LeftAngleBracket -> Next,
Token`LongName`RightAngleBracket -> Next,
Token`LongName`Perpendicular -> Next,
Token`LongName`LongLeftArrow -> Next,
Token`LongName`LongRightArrow -> Next,
Token`LongName`LongLeftRightArrow -> Next,
Token`LongName`DoubleLongLeftArrow -> Next,
Token`LongName`DoubleLongRightArrow -> Next,
Token`LongName`DoubleLongLeftRightArrow -> Next,
Token`LongName`UpArrowBar -> Next,
Token`LongName`DownArrowBar -> Next,
Token`LongName`LeftRightVector -> Next,
Token`LongName`RightUpDownVector -> Next,
Token`LongName`DownLeftRightVector -> Next,
Token`LongName`LeftUpDownVector -> Next,
Token`LongName`LeftVectorBar -> Next,
Token`LongName`RightVectorBar -> Next,
Token`LongName`RightUpVectorBar -> Next,
Token`LongName`RightDownVectorBar -> Next,
Token`LongName`DownLeftVectorBar -> Next,
Token`LongName`DownRightVectorBar -> Next,
Token`LongName`LeftUpVectorBar -> Next,
Token`LongName`LeftDownVectorBar -> Next,
Token`LongName`LeftTeeVector -> Next,
Token`LongName`RightTeeVector -> Next,
Token`LongName`RightUpTeeVector -> Next,
Token`LongName`RightDownTeeVector -> Next,
Token`LongName`DownLeftTeeVector -> Next,
Token`LongName`DownRightTeeVector -> Next,
Token`LongName`LeftUpTeeVector -> Next,
Token`LongName`LeftDownTeeVector -> Next,
Token`LongName`UpEquilibrium -> Next,
Token`LongName`ReverseUpEquilibrium -> Next,
Token`LongName`RoundImplies -> Next,
Token`LongName`LeftTriangleBar -> Next,
Token`LongName`RightTriangleBar -> Next,
Token`LongName`Equivalent -> Next,
Token`LongName`LessSlantEqual -> Next,
Token`LongName`GreaterSlantEqual -> Next,
Token`LongName`NestedLessLess -> Next,
Token`LongName`NestedGreaterGreater -> Next,
Token`LongName`PrecedesEqual -> Next,
Token`LongName`SucceedsEqual -> Next,
Token`LongName`DoubleLeftTee -> Next,
Token`LongName`LeftDoubleBracket -> Next,
Token`LongName`RightDoubleBracket -> Next,
Token`LongName`LeftAssociation -> Next,
Token`LongName`RightAssociation -> Next,
Token`LongName`TwoWayRule -> Next,
Token`LongName`Piecewise -> Next,
Token`LongName`ImplicitPlus -> Next,
Token`LongName`AutoLeftMatch -> Next,
Token`LongName`AutoRightMatch -> Next,
Token`LongName`InvisiblePrefixScriptBase -> Next,
Token`LongName`InvisiblePostfixScriptBase -> Next,
Token`LongName`Transpose -> Next,
Token`LongName`Conjugate -> Next,
Token`LongName`ConjugateTranspose -> Next,
Token`LongName`HermitianConjugate -> Next,
Token`LongName`VerticalBar -> Next,
Token`LongName`NotVerticalBar -> Next,
Token`LongName`Distributed -> Next,
Token`LongName`Conditioned -> Next,
Token`LongName`UndirectedEdge -> Next,
Token`LongName`DirectedEdge -> Next,
Token`LongName`Gradient -> Next,
Token`LongName`Divergence -> Next,
Token`LongName`Curl -> Next,
Token`LongName`ContinuedFractionK -> Next,
Token`LongName`TensorProduct -> Next,
Token`LongName`TensorWedge -> Next,
Token`LongName`ProbabilityPr -> Next,
Token`LongName`ExpectationE -> Next,
Token`LongName`PermutationProduct -> Next,
Token`LongName`NotEqualTilde -> Next,
Token`LongName`NotHumpEqual -> Next,
Token`LongName`NotHumpDownHump -> Next,
Token`LongName`NotLeftTriangleBar -> Next,
Token`LongName`NotRightTriangleBar -> Next,
Token`LongName`NotLessLess -> Next,
Token`LongName`NotNestedLessLess -> Next,
Token`LongName`NotLessSlantEqual -> Next,
Token`LongName`NotGreaterGreater -> Next,
Token`LongName`NotNestedGreaterGreater -> Next,
Token`LongName`NotGreaterSlantEqual -> Next,
Token`LongName`NotPrecedesEqual -> Next,
Token`LongName`NotSucceedsEqual -> Next,
Token`LongName`NotSquareSubset -> Next,
Token`LongName`NotSquareSuperset -> Next,
Token`LongName`Equal -> Next,
Token`LongName`VerticalSeparator -> Next,
Token`LongName`VectorGreater -> Next,
Token`LongName`VectorGreaterEqual -> Next,
Token`LongName`VectorLess -> Next,
Token`LongName`VectorLessEqual -> Next,
Token`LongName`Limit -> Next,
Token`LongName`MaxLimit -> Next,
Token`LongName`MinLimit -> Next,
Token`LongName`Cross -> Next,
Token`LongName`Function -> Next,
Token`LongName`Xnor -> Next,
Token`LongName`DiscreteShift -> Next,
Token`LongName`DifferenceDelta -> Next,
Token`LongName`DiscreteRatio -> Next,
Token`LongName`RuleDelayed -> Next,
Token`LongName`Square -> Next,
Token`LongName`Rule -> Next,
Token`LongName`Implies -> Next,
Token`LongName`ShortRightArrow -> Next,
Token`LongName`ShortLeftArrow -> Next,
Token`LongName`ShortUpArrow -> Next,
Token`LongName`ShortDownArrow -> Next,
Token`LongName`Application -> Next,
Token`LongName`LeftBracketingBar -> Next,
Token`LongName`RightBracketingBar -> Next,
Token`LongName`LeftDoubleBracketingBar -> Next,
Token`LongName`RightDoubleBracketingBar -> Next,
Token`LongName`CapitalDifferentialD -> Next,
Token`LongName`DifferentialD -> Next,
Token`LongName`InvisibleComma -> Next,
Token`LongName`InvisibleApplication -> Next,
Token`LongName`LongEqual -> Next,

Token`Count -> Next
|>
