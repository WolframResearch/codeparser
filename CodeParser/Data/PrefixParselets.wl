
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

Token`Unknown -> Parselet`PrefixErrorParselet[],
Token`Whitespace -> Parselet`PrefixErrorParselet[],
Token`InternalNewline -> Parselet`PrefixErrorParselet[],
Token`Comment -> Parselet`PrefixErrorParselet[],

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
Token`Error`UnsafeCharacterEncoding -> Parselet`PrefixErrorParselet[],
Token`Error`UnexpectedCommentCloser -> Parselet`PrefixErrorParselet[],


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


Token`Minus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`Minus, Minus],
Token`Plus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`Plus, Plus],
Token`Bang -> Parselet`PrefixOperatorParselet[Precedence`Prefix`Bang, Not],
Token`PlusPlus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`PlusPlus, PreIncrement],
Token`MinusMinus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`MinusMinus, PreDecrement],

Token`BangBang -> Parselet`PrefixOperatorParselet[Precedence`Fake`Prefix`BangBang, CodeParser`PrefixNot2],

Token`LongName`PlusMinus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`LongName`PlusMinus, PlusMinus],
Token`LongName`Sum -> Parselet`PrefixOperatorParselet[Precedence`LongName`Sum, Sum],
Token`LongName`Not -> Parselet`PrefixOperatorParselet[Precedence`LongName`Not, Not],
Token`LongName`Sqrt -> Parselet`PrefixOperatorParselet[Precedence`LongName`Sqrt, Sqrt],
Token`LongName`MinusPlus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`LongName`MinusPlus, MinusPlus],
Token`LongName`DifferentialD -> Parselet`PrefixOperatorParselet[Precedence`LongName`DifferentialD, DifferentialD],
Token`LongName`CapitalDifferentialD -> Parselet`PrefixOperatorParselet[Precedence`LongName`CapitalDifferentialD, CapitalDifferentialD],
Token`LongName`Minus -> Parselet`PrefixOperatorParselet[Precedence`Prefix`LongName`Minus, Minus],
Token`LongName`Del -> Parselet`PrefixOperatorParselet[Precedence`LongName`Del, Del],
Token`LongName`Square -> Parselet`PrefixOperatorParselet[Precedence`LongName`Square, Square],


Token`Comma -> Parselet`PrefixCommaParselet[],
Token`LongName`InvisibleComma -> Parselet`PrefixCommaParselet[],


Token`LongName`Product -> Parselet`PrefixOperatorParselet[Precedence`LongName`Product, Product],
Token`LongName`ContinuedFractionK -> Parselet`PrefixOperatorParselet[Precedence`LongName`ContinuedFractionK, ContinuedFractionK],
Token`LongName`CircleTimes -> Parselet`PrefixOperatorParselet[Precedence`Prefix`LongName`CircleTimes, CircleTimes],
Token`LongName`ForAll -> Parselet`PrefixOperatorParselet[Precedence`LongName`ForAll, ForAll],
Token`LongName`Exists -> Parselet`PrefixOperatorParselet[Precedence`LongName`Exists, Exists],
Token`LongName`NotExists -> Parselet`PrefixOperatorParselet[Precedence`LongName`NotExists, NotExists],
Token`LongName`Coproduct -> Parselet`PrefixOperatorParselet[Precedence`Prefix`LongName`Coproduct, Coproduct],
Token`LongName`Piecewise -> Parselet`PrefixOperatorParselet[Precedence`LongName`Piecewise, Piecewise],
Token`LongName`InvisiblePrefixScriptBase -> Parselet`PrefixOperatorParselet[Precedence`LongName`InvisiblePrefixScriptBase, System`InvisiblePrefixScriptBase],
Token`LongName`ExpectationE -> Parselet`PrefixOperatorParselet[Precedence`LongName`ExpectationE, ExpectationE],
Token`LongName`CubeRoot -> Parselet`PrefixOperatorParselet[Precedence`LongName`CubeRoot, CubeRoot],
Token`LongName`ProbabilityPr -> Parselet`PrefixOperatorParselet[Precedence`LongName`ProbabilityPr, ProbabilityPr],

Token`LinearSyntax`Bang -> Parselet`PrefixOperatorParselet[Precedence`LinearSyntax`Bang, CodeParser`PrefixLinearSyntaxBang],
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
Token`ColonColonOpenSquare -> Parselet`GroupParselet[Token`ColonColonOpenSquare, CodeParser`GroupTypeSpecifier],
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
Token`PercentPercent -> Parselet`LeafParselet[],

(*
prefix, infix, postfix
*)
Token`SemiSemi -> Parselet`SemiSemiParselet[],

(*
Has to handle \[Integral] f \[DifferentialD] x
*)
Token`LongName`Integral -> Parselet`IntegralParselet[Integrate, Integral],
Token`LongName`ContourIntegral -> Parselet`IntegralParselet[ContourIntegral, ContourIntegral],
Token`LongName`DoubleContourIntegral -> Parselet`IntegralParselet[DoubleContourIntegral, DoubleContourIntegral],
Token`LongName`ClockwiseContourIntegral -> Parselet`IntegralParselet[ClockwiseContourIntegral, ClockwiseContourIntegral],
Token`LongName`CounterClockwiseContourIntegral -> Parselet`IntegralParselet[CounterClockwiseContourIntegral, CounterClockwiseContourIntegral],

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
(*
technically, \[AutoLeftMatch] foo \[AutoRightMatch] does parse as
AutoMatch[foo] in InputForm but this is not documented,
and I'm not going to support it
*)
Token`LongName`AutoLeftMatch -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`AutoRightMatch -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`DiscreteShift -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`DifferenceDelta -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`DiscreteRatio -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`Laplacian -> Parselet`PrefixUnsupportedTokenParselet[],
Token`LongName`PartialD -> Parselet`PrefixUnsupportedTokenParselet[]

|>
