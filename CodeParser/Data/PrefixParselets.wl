
(*
CodeParser Data file

Do not modify this file directly
*)

<|

Token`String -> Parselet`LeafParselet[],
Token`Integer -> Parselet`LeafParselet[],
Token`Real -> Parselet`LeafParselet[],
Token`Rational -> Parselet`LeafParselet[],
Token`LinearSyntaxBlob -> Parselet`LeafParselet[],

Token`Unknown -> Parselet`PrefixNullPointerParselet[],
Token`Whitespace -> Parselet`PrefixNullPointerParselet[],
Token`InternalNewline -> Parselet`PrefixNullPointerParselet[],
Token`Comment -> Parselet`PrefixNullPointerParselet[],

Token`EndOfFile -> Parselet`PrefixEndOfFileParselet[],

Token`Error`ExpectedEqual -> Parselet`PrefixErrorParselet[],
Token`Error`Number -> Parselet`PrefixErrorParselet[],
Token`Error`UnhandledCharacter -> Parselet`PrefixErrorParselet[],
Token`Error`ExpectedLetterlike -> Parselet`PrefixErrorParselet[],
Token`Error`Aborted -> Parselet`PrefixErrorParselet[],
Token`Error`ExpectedOperand -> Parselet`PrefixErrorParselet[],
Token`Error`ExpectedTag -> Parselet`PrefixErrorParselet[],
Token`Error`ExpectedFile -> Parselet`PrefixErrorParselet[],
Token`Error`UnterminatedComment -> Parselet`PrefixErrorParselet[],
Token`Error`UnterminatedString -> Parselet`PrefixErrorParselet[],
Token`Error`UnterminatedFileString -> Parselet`PrefixErrorParselet[],
Token`Error`UnterminatedLinearSyntaxBlob -> Parselet`PrefixErrorParselet[],
Token`Error`UnsupportedToken -> Parselet`PrefixErrorParselet[],
Token`Error`UnexpectedCloser -> Parselet`PrefixErrorParselet[],


Token`BarGreater -> Parselet`PrefixCloserParselet[],
Token`CloseCurly -> Parselet`PrefixCloserParselet[],
Token`CloseParen -> Parselet`PrefixCloserParselet[],
Token`CloseSquare -> Parselet`PrefixCloserParselet[],
Token`LongName`CloseCurlyDoubleQuote -> Parselet`PrefixCloserParselet[],
Token`LongName`CloseCurlyQuote -> Parselet`PrefixCloserParselet[],
Token`LongName`RightAngleBracket -> Parselet`PrefixCloserParselet[],
Token`LongName`RightAssociation -> Parselet`PrefixCloserParselet[],
Token`LongName`RightBracketingBar -> Parselet`PrefixCloserParselet[],
Token`LongName`RightCeiling -> Parselet`PrefixCloserParselet[],
Token`LongName`RightDoubleBracket -> Parselet`PrefixCloserParselet[],
Token`LongName`RightDoubleBracketingBar -> Parselet`PrefixCloserParselet[],
Token`LongName`RightFloor -> Parselet`PrefixCloserParselet[],


Token`Minus -> Parselet`PrefixOperatorParselet[Token`Minus, Precedence`Prefix`Minus, Minus],
Token`Plus -> Parselet`PrefixOperatorParselet[Token`Plus, Precedence`Prefix`Plus, Plus],
Token`Bang -> Parselet`PrefixOperatorParselet[Token`Bang, Precedence`Prefix`Bang, Not],
Token`PlusPlus -> Parselet`PrefixOperatorParselet[Token`PlusPlus, Precedence`Prefix`PlusPlus, PreIncrement],
Token`MinusMinus -> Parselet`PrefixOperatorParselet[Token`MinusMinus, Precedence`Prefix`MinusMinus, PreDecrement],

Token`BangBang -> Parselet`PrefixOperatorParselet[Token`BangBang, Precedence`Fake`Prefix`BangBang, CodeParser`PrefixNot2],

Token`LongName`PlusMinus -> Parselet`PrefixOperatorParselet[Token`LongName`PlusMinus, Precedence`Prefix`LongName`PlusMinus, PlusMinus],
Token`LongName`Sum -> Parselet`PrefixOperatorParselet[Token`LongName`Sum, Precedence`LongName`Sum, Sum],
Token`LongName`Not -> Parselet`PrefixOperatorParselet[Token`LongName`Not, Precedence`LongName`Not, Not],
Token`LongName`Sqrt -> Parselet`PrefixOperatorParselet[Token`LongName`Sqrt, Precedence`LongName`Sqrt, Sqrt],
Token`LongName`MinusPlus -> Parselet`PrefixOperatorParselet[Token`LongName`MinusPlus, Precedence`Prefix`LongName`MinusPlus, MinusPlus],
Token`LongName`DifferentialD -> Parselet`PrefixOperatorParselet[Token`LongName`DifferentialD, Precedence`LongName`DifferentialD, DifferentialD],
Token`LongName`CapitalDifferentialD -> Parselet`PrefixOperatorParselet[Token`LongName`CapitalDifferentialD, Precedence`LongName`CapitalDifferentialD, CapitalDifferentialD],
Token`LongName`Minus -> Parselet`PrefixOperatorParselet[Token`LongName`Minus, Precedence`Prefix`LongName`Minus, Minus],
Token`LongName`Del -> Parselet`PrefixOperatorParselet[Token`LongName`Del, Precedence`LongName`Del, Del],
Token`LongName`Square -> Parselet`PrefixOperatorParselet[Token`LongName`Square, Precedence`LongName`Square, Square],


Token`Comma -> Parselet`PrefixCommaParselet[],


(*
Integration operators
*)
Token`LongName`ContourIntegral -> Parselet`PrefixOperatorParselet[Token`LongName`ContourIntegral, Precedence`Class`IntegrationOperators, ContourIntegral],
Token`LongName`DoubleContourIntegral -> Parselet`PrefixOperatorParselet[Token`LongName`DoubleContourIntegral, Precedence`Class`IntegrationOperators, DoubleContourIntegral],
Token`LongName`ClockwiseContourIntegral -> Parselet`PrefixOperatorParselet[Token`LongName`ClockwiseContourIntegral, Precedence`Class`IntegrationOperators, ClockwiseContourIntegral],
Token`LongName`CounterClockwiseContourIntegral -> Parselet`PrefixOperatorParselet[Token`LongName`CounterClockwiseContourIntegral, Precedence`Class`IntegrationOperators, CounterClockwiseContourIntegral],


Token`LongName`Product -> Parselet`PrefixOperatorParselet[Token`LongName`Product, Precedence`LongName`Product, Product],
Token`LongName`ContinuedFractionK -> Parselet`PrefixOperatorParselet[Token`LongName`ContinuedFractionK, Precedence`LongName`ContinuedFractionK, ContinuedFractionK],
Token`LongName`CircleTimes -> Parselet`PrefixOperatorParselet[Token`LongName`CircleTimes, Precedence`Prefix`LongName`CircleTimes, CircleTimes],
Token`LongName`ForAll -> Parselet`PrefixOperatorParselet[Token`LongName`ForAll, Precedence`LongName`ForAll, ForAll],
Token`LongName`Exists -> Parselet`PrefixOperatorParselet[Token`LongName`Exists, Precedence`LongName`Exists, Exists],
Token`LongName`NotExists -> Parselet`PrefixOperatorParselet[Token`LongName`NotExists, Precedence`LongName`NotExists, NotExists],
Token`LongName`Coproduct -> Parselet`PrefixOperatorParselet[Token`LongName`Coproduct, Precedence`Prefix`LongName`Coproduct, Coproduct],
Token`LongName`Piecewise -> Parselet`PrefixOperatorParselet[Token`LongName`Piecewise, Precedence`LongName`Piecewise, Piecewise],
Token`LongName`InvisiblePrefixScriptBase -> Parselet`PrefixOperatorParselet[Token`LongName`InvisiblePrefixScriptBase, Precedence`LongName`InvisiblePrefixScriptBase, System`InvisiblePrefixScriptBase],
Token`LongName`ExpectationE -> Parselet`PrefixOperatorParselet[Token`LongName`ExpectationE, Precedence`LongName`ExpectationE, ExpectationE],
Token`LongName`CubeRoot -> Parselet`PrefixOperatorParselet[Token`LongName`CubeRoot, Precedence`LongName`CubeRoot, CubeRoot],
Token`LongName`ProbabilityPr -> Parselet`PrefixOperatorParselet[Token`LongName`ProbabilityPr, Precedence`LongName`ProbabilityPr, ProbabilityPr],

Token`LinearSyntax`Bang -> Parselet`PrefixOperatorParselet[Token`LinearSyntax`Bang, Precedence`LinearSyntax`Bang, CodeParser`PrefixLinearSyntaxBang],
Token`LinearSyntax`At -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Amp -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Star -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Under -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Caret -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Space -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Percent -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Plus -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`Slash -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LinearSyntax`BackTick -> Parselet`PrefixUnsupportedTokenParselet[],

Token`LinearSyntax`CloseParen -> Parselet`PrefixUnsupportedTokenParselet[],


(*
Groups
*)
Token`OpenParen -> Parselet`GroupParselet[Token`OpenParen, CodeParser`GroupParen],
Token`OpenSquare -> Parselet`GroupParselet[Token`OpenSquare, CodeParser`GroupSquare],
Token`OpenCurly -> Parselet`GroupParselet[Token`OpenCurly, List],
Token`LessBar -> Parselet`GroupParselet[Token`LessBar, Association],
Token`LongName`LeftAngleBracket -> Parselet`GroupParselet[Token`LongName`LeftAngleBracket, AngleBracket],
Token`LongName`LeftCeiling -> Parselet`GroupParselet[Token`LongName`LeftCeiling, Ceiling],
Token`LongName`LeftFloor -> Parselet`GroupParselet[Token`LongName`LeftFloor, Floor],
Token`LongName`LeftDoubleBracket -> Parselet`GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket],
Token`LongName`LeftBracketingBar -> Parselet`GroupParselet[Token`LongName`LeftBracketingBar, BracketingBar],
Token`LongName`LeftDoubleBracketingBar -> Parselet`GroupParselet[Token`LongName`LeftDoubleBracketingBar, DoubleBracketingBar],
Token`LongName`LeftAssociation -> Parselet`GroupParselet[Token`LongName`LeftAssociation, Association],
Token`LongName`OpenCurlyQuote -> Parselet`GroupParselet[Token`LongName`OpenCurlyQuote, CurlyQuote],
Token`LongName`OpenCurlyDoubleQuote -> Parselet`GroupParselet[Token`LongName`OpenCurlyDoubleQuote, CurlyDoubleQuote],

(*
Special
*)

(*
context sensitive parsing of  x_
*)
Token`Symbol -> Parselet`SymbolParselet[],

(*
context sensitive parsing of _x
*)
Token`Under -> Parselet`UnderParselet[1],
Token`UnderUnder -> Parselet`UnderParselet[2],
Token`UnderUnderUnder -> Parselet`UnderParselet[3],

Token`UnderDot -> Parselet`UnderDotParselet[],


Token`Hash -> Parselet`HashParselet[],
Token`HashHash -> Parselet`HashHashParselet[],

Token`Percent -> Parselet`PercentParselet[],
Token`PercentPercent -> Parselet`PercentPercentParselet[],

(*
prefix, infix, postfix
*)
Token`SemiSemi -> Parselet`SemiSemiParselet[],

(*
Has to handle \[Integral] f \[DifferentialD] x
*)
Token`LongName`Integral -> Parselet`IntegralParselet[],

(*
stringify next token (as a file]
*)
Token`LessLess -> Parselet`LessLessParselet[],


Token`QuestionQuestion -> Parselet`PrefixUnsupportedTokenParselet[],

(*
Also use for operators that are only valid in StandardForm.
e.g., \[Gradient] does not have an interpretation in InputForm

\[Gradient] is not letterlike, so it needs some kind of categorization,
but it also needs to be prevented from making any valid parses.
*)
Token`LongName`Gradient -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`Divergence -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`Curl -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`Limit -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`MaxLimit -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`MinLimit -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`AutoLeftMatch -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`AutoRightMatch -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`DiscreteShift -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`DifferenceDelta -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`DiscreteRatio -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`Laplacian -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`PartialD -> Parselet`PrefixUnsupportedTokenParselet[]

|>
