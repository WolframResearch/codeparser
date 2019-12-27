BeginPackage["AST`Generate`TokenEnum`"]

Begin["`Private`"]

Needs["AST`Generate`"]




(*
Use the System` context symbols for literals when we can

These are compiled as SYMBOL_FOO
*)
tokenToSymbol[Token`EndOfFile] = "Symbol`EndOfFile"
tokenToSymbol[Token`Symbol] = "Symbol`Symbol"
tokenToSymbol[Token`String] = "Symbol`String"
tokenToSymbol[Token`Integer] = "Symbol`Integer"
tokenToSymbol[Token`Real] = "Symbol`Real"

tokenToSymbol[Token`Whitespace] = "Symbol`Whitespace"

tokenToSymbol[Token`Hash] = "Symbol`Slot"
tokenToSymbol[Token`HashHash] = "Symbol`SlotSequence"

tokenToSymbol[Token`Percent] = "Symbol`Out"

tokenToSymbol[Token`Under] = "Symbol`Blank"
tokenToSymbol[Token`UnderUnder] = "Symbol`BlankSequence"
tokenToSymbol[Token`UnderUnderUnder] = "Symbol`BlankNullSequence"

tokenToSymbol[Token`UnderDot] = "Symbol`AST`OptionalDefault"

(*
everything else will be Symbol`Token`Foo

which is compiled as SYMBOL_TOKEN_FOO
*)
tokenToSymbol[s_] := "Symbol`"<>ToString[s]






(*
Errors
*)
isPossibleBeginningOfExpression[Token`Error`Unknown] = False
isPossibleBeginningOfExpression[Token`Error`ExpectedEqual] = False
isPossibleBeginningOfExpression[Token`Error`UnhandledDot] = False
isPossibleBeginningOfExpression[Token`Error`UnhandledCharacter] = False
isPossibleBeginningOfExpression[Token`Error`ExpectedLetterlike] = False
isPossibleBeginningOfExpression[Token`Error`UnterminatedComment] = False
isPossibleBeginningOfExpression[Token`Error`UnterminatedString] = False
isPossibleBeginningOfExpression[Token`Error`InvalidBase] = False
isPossibleBeginningOfExpression[Token`Error`ExpectedAccuracy] = False
isPossibleBeginningOfExpression[Token`Error`ExpectedExponent] = False
isPossibleBeginningOfExpression[Token`Error`EmptyString] = False
isPossibleBeginningOfExpression[Token`Error`Aborted] = False
isPossibleBeginningOfExpression[Token`Error`ExpectedOperand] = False
isPossibleBeginningOfExpression[Token`Error`UnrecognizedDigit] = False

(*
EndOfFile
*)
isPossibleBeginningOfExpression[Token`EndOfFile] = False

(*
Trivia
*)
isPossibleBeginningOfExpression[Token`Comment] = False
isPossibleBeginningOfExpression[Token`Newline] = False
isPossibleBeginningOfExpression[Token`Whitespace] = False
isPossibleBeginningOfExpression[Token`LineContinuation] = False


(*
These binary/infix operators are also prefix
*)
(*isPossibleBeginningOfExpression[Token`Minus] = False*)
(*isPossibleBeginningOfExpression[Token`LongName`PlusMinus] = False*)
(*isPossibleBeginningOfExpression[Token`LongName`MinusPlus] = False*)
(*isPossibleBeginningOfExpression[Token`Plus] = False*)
(*isPossibleBeginningOfExpression[Token`LongName`CircleTimes] = False*)
(*isPossibleBeginningOfExpression[Token`LongName`Coproduct] = False*)
(*isPossibleBeginningOfExpression[Token`Bang] = False*)
(*isPossibleBeginningOfExpression[Token`MinusMinus] = False*)
(*isPossibleBeginningOfExpression[Token`PlusPlus] = False*)
(*isPossibleBeginningOfExpression[Token`SemiSemi] = False*)
(*isPossibleBeginningOfExpression[Token`BangBang] = False*)
(*
Calls
*)
(*
isPossibleBeginningOfExpression[Token`OpenSquare] = False
isPossibleBeginningOfExpression[Token`LongName`LeftDoubleBracket] = False
*)

(*
Binary ops
*)
isPossibleBeginningOfExpression[Token`Slash] = False
isPossibleBeginningOfExpression[Token`Caret] = False
isPossibleBeginningOfExpression[Token`CaretEqual] = False
isPossibleBeginningOfExpression[Token`CaretColonEqual] = False
isPossibleBeginningOfExpression[Token`SlashAt] = False
isPossibleBeginningOfExpression[Token`MinusGreater] = False
isPossibleBeginningOfExpression[Token`AtAt] = False
isPossibleBeginningOfExpression[Token`SlashSemi] = False
isPossibleBeginningOfExpression[Token`SlashDot] = False
isPossibleBeginningOfExpression[Token`ColonGreater] = False
isPossibleBeginningOfExpression[Token`SlashSlashDot] = False
isPossibleBeginningOfExpression[Token`PlusEqual] = False
isPossibleBeginningOfExpression[Token`StarEqual] = False
isPossibleBeginningOfExpression[Token`MinusEqual] = False
isPossibleBeginningOfExpression[Token`SlashEqual] = False
isPossibleBeginningOfExpression[Token`LessMinusGreater] = False
isPossibleBeginningOfExpression[Token`SlashSlashAt] = False
isPossibleBeginningOfExpression[Token`At] = False
isPossibleBeginningOfExpression[Token`AtAtAt] = False
isPossibleBeginningOfExpression[Token`SlashSlash] = False
isPossibleBeginningOfExpression[Token`ColonEqual] = False
isPossibleBeginningOfExpression[Token`Question] = False
isPossibleBeginningOfExpression[Token`LongName`Because] = False
isPossibleBeginningOfExpression[Token`LongName`Therefore] = False
isPossibleBeginningOfExpression[Token`LongName`RightTee] = False
isPossibleBeginningOfExpression[Token`LongName`LeftTee] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleRightTee] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLeftTee] = False
isPossibleBeginningOfExpression[Token`LongName`UpTee] = False
isPossibleBeginningOfExpression[Token`LongName`DownTee] = False
isPossibleBeginningOfExpression[Token`LongName`Divide] = False
isPossibleBeginningOfExpression[Token`LongName`DivisionSlash] = False
isPossibleBeginningOfExpression[Token`LongName`Implies] = False
isPossibleBeginningOfExpression[Token`LongName`RoundImplies] = False
isPossibleBeginningOfExpression[Token`LongName`DirectedEdge] = False
isPossibleBeginningOfExpression[Token`LongName`Rule] = False
isPossibleBeginningOfExpression[Token`LongName`RuleDelayed] = False
isPossibleBeginningOfExpression[Token`LongName`UndirectedEdge] = False
isPossibleBeginningOfExpression[Token`LongName`Function] = False
isPossibleBeginningOfExpression[Token`LongName`TwoWayRule] = False
isPossibleBeginningOfExpression[Token`LongName`InvisibleApplication] = False
isPossibleBeginningOfExpression[Token`LongName`CircleMinus] = False
isPossibleBeginningOfExpression[Token`LongName`SuchThat] = False
isPossibleBeginningOfExpression[Token`LongName`Perpendicular] = False

(*
Infix ops
*)
isPossibleBeginningOfExpression[Token`AmpAmp] = False
isPossibleBeginningOfExpression[Token`AtStar] = False
isPossibleBeginningOfExpression[Token`BangEqual] = False
isPossibleBeginningOfExpression[Token`Bar] = False
isPossibleBeginningOfExpression[Token`BarBar] = False
isPossibleBeginningOfExpression[Token`ColonColon] = False
isPossibleBeginningOfExpression[Token`Comma] = False
isPossibleBeginningOfExpression[Token`Dot] = False
isPossibleBeginningOfExpression[Token`EqualBangEqual] = False
isPossibleBeginningOfExpression[Token`EqualEqual] = False
isPossibleBeginningOfExpression[Token`EqualEqualEqual] = False
isPossibleBeginningOfExpression[Token`Greater] = False
isPossibleBeginningOfExpression[Token`GreaterEqual] = False
isPossibleBeginningOfExpression[Token`Less] = False
isPossibleBeginningOfExpression[Token`LessEqual] = False
isPossibleBeginningOfExpression[Token`LessGreater] = False
isPossibleBeginningOfExpression[Token`Semi] = False
isPossibleBeginningOfExpression[Token`SlashStar] = False
isPossibleBeginningOfExpression[Token`Star] = False
isPossibleBeginningOfExpression[Token`StarStar] = False
isPossibleBeginningOfExpression[Token`TildeTilde] = False

isPossibleBeginningOfExpression[Token`LongName`And] = False
isPossibleBeginningOfExpression[Token`LongName`Backslash] = False
isPossibleBeginningOfExpression[Token`LongName`Cap] = False
isPossibleBeginningOfExpression[Token`LongName`CenterDot] = False
isPossibleBeginningOfExpression[Token`LongName`CircleDot] = False
isPossibleBeginningOfExpression[Token`LongName`CirclePlus] = False
isPossibleBeginningOfExpression[Token`LongName`Colon] = False
isPossibleBeginningOfExpression[Token`LongName`Conditioned] = False
isPossibleBeginningOfExpression[Token`LongName`Congruent] = False
isPossibleBeginningOfExpression[Token`LongName`Cross] = False
isPossibleBeginningOfExpression[Token`LongName`Cup] = False

isPossibleBeginningOfExpression[Token`LongName`CupCap] = False
isPossibleBeginningOfExpression[Token`LongName`NotCupCap] = False

isPossibleBeginningOfExpression[Token`LongName`RightTee] = False
isPossibleBeginningOfExpression[Token`LongName`LeftTee] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleRightTee] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLeftTee] = False
isPossibleBeginningOfExpression[Token`LongName`UpTee] = False
isPossibleBeginningOfExpression[Token`LongName`DownTee] = False

isPossibleBeginningOfExpression[Token`LongName`Diamond] = False
isPossibleBeginningOfExpression[Token`LongName`Distributed] = False
isPossibleBeginningOfExpression[Token`LongName`Divides] = False
isPossibleBeginningOfExpression[Token`LongName`DotEqual] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleDownArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLeftRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLongLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLongLeftRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleLongRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleUpArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleUpDownArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DownArrowBar] = False
isPossibleBeginningOfExpression[Token`LongName`DownArrowUpArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DownLeftRightVector] = False
isPossibleBeginningOfExpression[Token`LongName`DownLeftTeeVector] = False
isPossibleBeginningOfExpression[Token`LongName`DownLeftVector] = False
isPossibleBeginningOfExpression[Token`LongName`DownLeftVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`DownRightTeeVector] = False
isPossibleBeginningOfExpression[Token`LongName`DownRightVector] = False
isPossibleBeginningOfExpression[Token`LongName`DownRightVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`DownTeeArrow] = False
isPossibleBeginningOfExpression[Token`LongName`Element] = False
isPossibleBeginningOfExpression[Token`LongName`Equal] = False
isPossibleBeginningOfExpression[Token`LongName`EqualTilde] = False
isPossibleBeginningOfExpression[Token`LongName`Equilibrium] = False
isPossibleBeginningOfExpression[Token`LongName`Equivalent] = False

isPossibleBeginningOfExpression[Token`LongName`LessEqualGreater] = False
isPossibleBeginningOfExpression[Token`LongName`GreaterEqualLess] = False

isPossibleBeginningOfExpression[Token`LongName`GreaterFullEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreaterFullEqual] = False

isPossibleBeginningOfExpression[Token`LongName`GreaterGreater] = False

isPossibleBeginningOfExpression[Token`LongName`GreaterLess] = False
isPossibleBeginningOfExpression[Token`LongName`LessGreater] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreaterLess] = False
isPossibleBeginningOfExpression[Token`LongName`NotLessGreater] = False

isPossibleBeginningOfExpression[Token`LongName`GreaterSlantEqual] = False

isPossibleBeginningOfExpression[Token`LongName`LessTilde] = False
isPossibleBeginningOfExpression[Token`LongName`GreaterTilde] = False
isPossibleBeginningOfExpression[Token`LongName`NotLessTilde] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreaterTilde] = False

isPossibleBeginningOfExpression[Token`LongName`HumpDownHump] = False
isPossibleBeginningOfExpression[Token`LongName`HumpEqual] = False
isPossibleBeginningOfExpression[Token`LongName`ImplicitPlus] = False
isPossibleBeginningOfExpression[Token`LongName`Intersection] = False
isPossibleBeginningOfExpression[Token`LongName`InvisibleComma] = False
isPossibleBeginningOfExpression[Token`LongName`InvisibleTimes] = False
isPossibleBeginningOfExpression[Token`LongName`LeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LeftArrowBar] = False
isPossibleBeginningOfExpression[Token`LongName`LeftArrowRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LeftDownTeeVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftDownVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftDownVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`LeftRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LeftRightVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftTeeArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LeftTeeVector] = False

isPossibleBeginningOfExpression[Token`LongName`LeftTriangle] = False
isPossibleBeginningOfExpression[Token`LongName`RightTriangle] = False
isPossibleBeginningOfExpression[Token`LongName`NotLeftTriangle] = False
isPossibleBeginningOfExpression[Token`LongName`NotRightTriangle] = False

isPossibleBeginningOfExpression[Token`LongName`LeftTriangleEqual] = False
isPossibleBeginningOfExpression[Token`LongName`RightTriangleEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotLeftTriangleEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotRightTriangleEqual] = False

isPossibleBeginningOfExpression[Token`LongName`LeftUpDownVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftUpTeeVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftUpVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftUpVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`LeftVector] = False
isPossibleBeginningOfExpression[Token`LongName`LeftVectorBar] = False

isPossibleBeginningOfExpression[Token`LongName`LessEqual] = False
isPossibleBeginningOfExpression[Token`LongName`GreaterEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotLessEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreaterEqual] = False

isPossibleBeginningOfExpression[Token`LongName`LessFullEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotLessFullEqual] = False

isPossibleBeginningOfExpression[Token`LongName`LessLess] = False
isPossibleBeginningOfExpression[Token`LongName`LessSlantEqual] = False
isPossibleBeginningOfExpression[Token`LongName`LongEqual] = False
isPossibleBeginningOfExpression[Token`LongName`LongLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LongLeftRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LongRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LowerLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`LowerRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`Nand] = False
isPossibleBeginningOfExpression[Token`LongName`NestedGreaterGreater] = False

isPossibleBeginningOfExpression[Token`LongName`NestedLessLess] = False
isPossibleBeginningOfExpression[Token`LongName`NotNestedLessLess] = False

isPossibleBeginningOfExpression[Token`LongName`Nor] = False
isPossibleBeginningOfExpression[Token`LongName`NotCongruent] = False
isPossibleBeginningOfExpression[Token`LongName`NotElement] = False
isPossibleBeginningOfExpression[Token`LongName`NotEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotEqualTilde] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreaterGreater] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreaterSlantEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotHumpDownHump] = False
isPossibleBeginningOfExpression[Token`LongName`NotHumpEqual] = False

isPossibleBeginningOfExpression[Token`LongName`LeftTriangleBar] = False
isPossibleBeginningOfExpression[Token`LongName`RightTriangleBar] = False

isPossibleBeginningOfExpression[Token`LongName`NotLeftTriangleBar] = False

isPossibleBeginningOfExpression[Token`LongName`NotLess] = False
isPossibleBeginningOfExpression[Token`LongName`NotGreater] = False

isPossibleBeginningOfExpression[Token`LongName`NotLessLess] = False
isPossibleBeginningOfExpression[Token`LongName`NotLessSlantEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotNestedGreaterGreater] = False
isPossibleBeginningOfExpression[Token`LongName`NotNestedLessLess] = False

isPossibleBeginningOfExpression[Token`LongName`NotReverseElement] = False
isPossibleBeginningOfExpression[Token`LongName`NotRightTriangleBar] = False

isPossibleBeginningOfExpression[Token`LongName`SquareSubset] = False
isPossibleBeginningOfExpression[Token`LongName`SquareSuperset] = False
isPossibleBeginningOfExpression[Token`LongName`NotSquareSubset] = False
isPossibleBeginningOfExpression[Token`LongName`NotSquareSuperset] = False
isPossibleBeginningOfExpression[Token`LongName`SquareSubsetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`SquareSupersetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotSquareSubsetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotSquareSupersetEqual] = False

isPossibleBeginningOfExpression[Token`LongName`NotSubset] = False
isPossibleBeginningOfExpression[Token`LongName`NotSubsetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotSuperset] = False
isPossibleBeginningOfExpression[Token`LongName`NotSupersetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotTilde] = False
isPossibleBeginningOfExpression[Token`LongName`NotTildeEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotTildeFullEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotTildeTilde] = False
isPossibleBeginningOfExpression[Token`LongName`Or] = False
isPossibleBeginningOfExpression[Token`LongName`PermutationProduct] = False

isPossibleBeginningOfExpression[Token`LongName`Precedes] = False
isPossibleBeginningOfExpression[Token`LongName`Succeeds] = False
isPossibleBeginningOfExpression[Token`LongName`PrecedesEqual] = False
isPossibleBeginningOfExpression[Token`LongName`SucceedsEqual] = False
isPossibleBeginningOfExpression[Token`LongName`PrecedesTilde] = False
isPossibleBeginningOfExpression[Token`LongName`SucceedsTilde] = False
isPossibleBeginningOfExpression[Token`LongName`PrecedesSlantEqual] = False
isPossibleBeginningOfExpression[Token`LongName`SucceedsSlantEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotPrecedes] = False
isPossibleBeginningOfExpression[Token`LongName`NotSucceeds] = False
isPossibleBeginningOfExpression[Token`LongName`NotPrecedesEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotSucceedsEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotPrecedesTilde] = False
isPossibleBeginningOfExpression[Token`LongName`NotSucceedsTilde] = False
isPossibleBeginningOfExpression[Token`LongName`NotPrecedesSlantEqual] = False
isPossibleBeginningOfExpression[Token`LongName`NotSucceedsSlantEqual] = False

isPossibleBeginningOfExpression[Token`LongName`Perpendicular] = False
isPossibleBeginningOfExpression[Token`LongName`Proportion] = False
isPossibleBeginningOfExpression[Token`LongName`Proportional] = False
isPossibleBeginningOfExpression[Token`LongName`ReverseElement] = False
isPossibleBeginningOfExpression[Token`LongName`ReverseEquilibrium] = False
isPossibleBeginningOfExpression[Token`LongName`ReverseUpEquilibrium] = False
isPossibleBeginningOfExpression[Token`LongName`RightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`RightArrowBar] = False
isPossibleBeginningOfExpression[Token`LongName`RightArrowLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`RightDownTeeVector] = False
isPossibleBeginningOfExpression[Token`LongName`RightDownVector] = False
isPossibleBeginningOfExpression[Token`LongName`RightDownVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`RightTeeArrow] = False
isPossibleBeginningOfExpression[Token`LongName`RightTeeVector] = False

isPossibleBeginningOfExpression[Token`LongName`RightUpDownVector] = False
isPossibleBeginningOfExpression[Token`LongName`RightUpTeeVector] = False
isPossibleBeginningOfExpression[Token`LongName`RightUpVector] = False
isPossibleBeginningOfExpression[Token`LongName`RightUpVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`RightVector] = False
isPossibleBeginningOfExpression[Token`LongName`RightVectorBar] = False
isPossibleBeginningOfExpression[Token`LongName`ShortDownArrow] = False
isPossibleBeginningOfExpression[Token`LongName`ShortLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`ShortRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`ShortUpArrow] = False
isPossibleBeginningOfExpression[Token`LongName`SmallCircle] = False
isPossibleBeginningOfExpression[Token`LongName`Star] = False
isPossibleBeginningOfExpression[Token`LongName`Subset] = False
isPossibleBeginningOfExpression[Token`LongName`SubsetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`Superset] = False
isPossibleBeginningOfExpression[Token`LongName`SupersetEqual] = False
isPossibleBeginningOfExpression[Token`LongName`TensorProduct] = False
isPossibleBeginningOfExpression[Token`LongName`TensorWedge] = False
isPossibleBeginningOfExpression[Token`LongName`Tilde] = False
isPossibleBeginningOfExpression[Token`LongName`TildeEqual] = False
isPossibleBeginningOfExpression[Token`LongName`TildeFullEqual] = False
isPossibleBeginningOfExpression[Token`LongName`TildeTilde] = False
isPossibleBeginningOfExpression[Token`LongName`Times] = False
isPossibleBeginningOfExpression[Token`LongName`Union] = False
isPossibleBeginningOfExpression[Token`LongName`UnionPlus] = False

isPossibleBeginningOfExpression[Token`LongName`UpArrow] = False
isPossibleBeginningOfExpression[Token`LongName`DownArrow] = False

isPossibleBeginningOfExpression[Token`LongName`UpArrowBar] = False
isPossibleBeginningOfExpression[Token`LongName`UpArrowDownArrow] = False
isPossibleBeginningOfExpression[Token`LongName`UpDownArrow] = False
isPossibleBeginningOfExpression[Token`LongName`UpEquilibrium] = False
isPossibleBeginningOfExpression[Token`LongName`UpperLeftArrow] = False
isPossibleBeginningOfExpression[Token`LongName`UpperRightArrow] = False
isPossibleBeginningOfExpression[Token`LongName`UpTeeArrow] = False
isPossibleBeginningOfExpression[Token`LongName`VectorGreater] = False
isPossibleBeginningOfExpression[Token`LongName`VectorGreaterEqual] = False
isPossibleBeginningOfExpression[Token`LongName`VectorLess] = False
isPossibleBeginningOfExpression[Token`LongName`VectorLessEqual] = False
isPossibleBeginningOfExpression[Token`LongName`Vee] = False
isPossibleBeginningOfExpression[Token`LongName`VerticalSeparator] = False
isPossibleBeginningOfExpression[Token`LongName`VerticalTilde] = False
isPossibleBeginningOfExpression[Token`LongName`Wedge] = False
isPossibleBeginningOfExpression[Token`LongName`Xor] = False

isPossibleBeginningOfExpression[Token`LongName`VerticalBar] = False
isPossibleBeginningOfExpression[Token`LongName`DoubleVerticalBar] = False
isPossibleBeginningOfExpression[Token`LongName`NotVerticalBar] = False
isPossibleBeginningOfExpression[Token`LongName`NotDoubleVerticalBar] = False

isPossibleBeginningOfExpression[Token`Fake`ImplicitTimes] = False

(*
Closers
*)
isPossibleBeginningOfExpression[Token`BarGreater] = False
isPossibleBeginningOfExpression[Token`CloseCurly] = False
isPossibleBeginningOfExpression[Token`CloseParen] = False
isPossibleBeginningOfExpression[Token`CloseSquare] = False
isPossibleBeginningOfExpression[Token`LongName`CloseCurlyDoubleQuote] = False
isPossibleBeginningOfExpression[Token`LongName`CloseCurlyQuote] = False
isPossibleBeginningOfExpression[Token`LongName`RightAngleBracket] = False
isPossibleBeginningOfExpression[Token`LongName`RightAssociation] = False
isPossibleBeginningOfExpression[Token`LongName`RightBracketingBar] = False
isPossibleBeginningOfExpression[Token`LongName`RightCeiling] = False
isPossibleBeginningOfExpression[Token`LongName`RightDoubleBracket] = False
isPossibleBeginningOfExpression[Token`LongName`RightDoubleBracketingBar] = False
isPossibleBeginningOfExpression[Token`LongName`RightFloor] = False
isPossibleBeginningOfExpression[Token`LinearSyntax`CloseParen] = False

(*
Postfix ops
*)
isPossibleBeginningOfExpression[Token`Amp] = False
isPossibleBeginningOfExpression[Token`DotDot] = False
isPossibleBeginningOfExpression[Token`DotDotDot] = False
isPossibleBeginningOfExpression[Token`SingleQuote] = False
isPossibleBeginningOfExpression[Token`LongName`Transpose] = False
isPossibleBeginningOfExpression[Token`LongName`Conjugate] = False
isPossibleBeginningOfExpression[Token`LongName`ConjugateTranspose] = False
isPossibleBeginningOfExpression[Token`LongName`HermitianConjugate] = False
isPossibleBeginningOfExpression[Token`LongName`InvisiblePostfixScriptBase] = False

(*
Infix ops with trailing
*)
isPossibleBeginningOfExpression[Token`Semi] = False
isPossibleBeginningOfExpression[Token`Comma] = False
isPossibleBeginningOfExpression[Token`LongName`InvisibleComma] = False

(*
Ternary
*)
isPossibleBeginningOfExpression[Token`Tilde] = False

(*
Context sensitive binary
*)
isPossibleBeginningOfExpression[Token`Colon] = False

(*
Ternary
*)
isPossibleBeginningOfExpression[Token`SlashColon] = False

(*
Special binary ops
*)
isPossibleBeginningOfExpression[Token`Equal] = False
isPossibleBeginningOfExpression[Token`EqualDot] = False

isPossibleBeginningOfExpression[Token`GreaterGreater] = False
isPossibleBeginningOfExpression[Token`GreaterGreaterGreater] = False



isPossibleBeginningOfExpression[Token`Count] = False

(*
Anything else
*)
isPossibleBeginningOfExpression[_] = True









isCloser[Token`BarGreater] = True
isCloser[Token`CloseCurly] = True
isCloser[Token`CloseParen] = True
isCloser[Token`CloseSquare] = True
isCloser[Token`LongName`CloseCurlyDoubleQuote] = True
isCloser[Token`LongName`CloseCurlyQuote] = True
isCloser[Token`LongName`RightAngleBracket] = True
isCloser[Token`LongName`RightAssociation] = True
isCloser[Token`LongName`RightBracketingBar] = True
isCloser[Token`LongName`RightCeiling] = True
isCloser[Token`LongName`RightDoubleBracket] = True
isCloser[Token`LongName`RightDoubleBracketingBar] = True
isCloser[Token`LongName`RightFloor] = True
isCloser[Token`LinearSyntax`CloseParen] = True

isCloser[_] = False





isError[Token`Error`Unknown] = True
isError[Token`Error`ExpectedEqual] = True
isError[Token`Error`UnhandledDot] = True
isError[Token`Error`UnhandledCharacter] = True
isError[Token`Error`ExpectedLetterlike] = True
isError[Token`Error`UnterminatedComment] = True
isError[Token`Error`UnterminatedString] = True
isError[Token`Error`InvalidBase] = True
isError[Token`Error`ExpectedAccuracy] = True
isError[Token`Error`ExpectedExponent] = True
isError[Token`Error`EmptyString] = True
isError[Token`Error`Aborted] = True
isError[Token`Error`ExpectedOperand] = True
isError[Token`Error`UnrecognizedDigit] = True

isError[_] = False





isTrivia[Token`Comment] = True
isTrivia[Token`Newline] = True
isTrivia[Token`Whitespace] = True
isTrivia[Token`LineContinuation] = True

isTrivia[_] = False





(*
Actual infix operators, not binary operators
*)

isInfixOperator[Token`AmpAmp] = True
isInfixOperator[Token`AtStar] = True
isInfixOperator[Token`BangEqual] = True
isInfixOperator[Token`Bar] = True
isInfixOperator[Token`BarBar] = True
isInfixOperator[Token`ColonColon] = True
isInfixOperator[Token`Comma] = True
isInfixOperator[Token`Dot] = True
isInfixOperator[Token`EqualBangEqual] = True
isInfixOperator[Token`EqualEqual] = True
isInfixOperator[Token`EqualEqualEqual] = True
isInfixOperator[Token`Greater] = True
isInfixOperator[Token`GreaterEqual] = True
isInfixOperator[Token`Less] = True
isInfixOperator[Token`LessEqual] = True
isInfixOperator[Token`LessGreater] = True
isInfixOperator[Token`Plus] = True
isInfixOperator[Token`Semi] = True
isInfixOperator[Token`SlashStar] = True
isInfixOperator[Token`Star] = True
isInfixOperator[Token`StarStar] = True
isInfixOperator[Token`TildeTilde] = True

isInfixOperator[Token`LongName`And] = True
isInfixOperator[Token`LongName`Backslash] = True
isInfixOperator[Token`LongName`Cap] = True
isInfixOperator[Token`LongName`CenterDot] = True
isInfixOperator[Token`LongName`CircleDot] = True
isInfixOperator[Token`LongName`CirclePlus] = True
isInfixOperator[Token`LongName`CircleTimes] = True
isInfixOperator[Token`LongName`Colon] = True
isInfixOperator[Token`LongName`Conditioned] = True
isInfixOperator[Token`LongName`Congruent] = True
isInfixOperator[Token`LongName`Coproduct] = True
isInfixOperator[Token`LongName`Cross] = True
isInfixOperator[Token`LongName`Cup] = True

isInfixOperator[Token`LongName`CupCap] = True
isInfixOperator[Token`LongName`NotCupCap] = True

isInfixOperator[Token`LongName`Diamond] = True
isInfixOperator[Token`LongName`Distributed] = True
isInfixOperator[Token`LongName`Divides] = True
isInfixOperator[Token`LongName`DotEqual] = True
isInfixOperator[Token`LongName`DoubleDownArrow] = True
isInfixOperator[Token`LongName`DoubleLeftArrow] = True
isInfixOperator[Token`LongName`DoubleLeftRightArrow] = True
isInfixOperator[Token`LongName`DoubleLongLeftArrow] = True
isInfixOperator[Token`LongName`DoubleLongLeftRightArrow] = True
isInfixOperator[Token`LongName`DoubleLongRightArrow] = True
isInfixOperator[Token`LongName`DoubleRightArrow] = True
isInfixOperator[Token`LongName`DoubleUpArrow] = True
isInfixOperator[Token`LongName`DoubleUpDownArrow] = True
isInfixOperator[Token`LongName`DownArrowBar] = True
isInfixOperator[Token`LongName`DownArrowUpArrow] = True
isInfixOperator[Token`LongName`DownLeftRightVector] = True
isInfixOperator[Token`LongName`DownLeftTeeVector] = True
isInfixOperator[Token`LongName`DownLeftVector] = True
isInfixOperator[Token`LongName`DownLeftVectorBar] = True
isInfixOperator[Token`LongName`DownRightTeeVector] = True
isInfixOperator[Token`LongName`DownRightVector] = True
isInfixOperator[Token`LongName`DownRightVectorBar] = True
isInfixOperator[Token`LongName`DownTeeArrow] = True
isInfixOperator[Token`LongName`Element] = True
isInfixOperator[Token`LongName`Equal] = True
isInfixOperator[Token`LongName`EqualTilde] = True
isInfixOperator[Token`LongName`Equilibrium] = True
isInfixOperator[Token`LongName`Equivalent] = True

isInfixOperator[Token`LongName`LessEqualGreater] = True
isInfixOperator[Token`LongName`GreaterEqualLess] = True

isInfixOperator[Token`LongName`GreaterFullEqual] = True
isInfixOperator[Token`LongName`NotGreaterFullEqual] = True

isInfixOperator[Token`LongName`GreaterGreater] = True

isInfixOperator[Token`LongName`GreaterLess] = True
isInfixOperator[Token`LongName`LessGreater] = True
isInfixOperator[Token`LongName`NotGreaterLess] = True
isInfixOperator[Token`LongName`NotLessGreater] = True

isInfixOperator[Token`LongName`GreaterSlantEqual] = True

isInfixOperator[Token`LongName`LessTilde] = True
isInfixOperator[Token`LongName`GreaterTilde] = True
isInfixOperator[Token`LongName`NotLessTilde] = True
isInfixOperator[Token`LongName`NotGreaterTilde] = True

isInfixOperator[Token`LongName`HumpDownHump] = True
isInfixOperator[Token`LongName`HumpEqual] = True
isInfixOperator[Token`LongName`ImplicitPlus] = True
isInfixOperator[Token`LongName`Intersection] = True
isInfixOperator[Token`LongName`InvisibleComma] = True
isInfixOperator[Token`LongName`InvisibleTimes] = True
isInfixOperator[Token`LongName`LeftArrow] = True
isInfixOperator[Token`LongName`LeftArrowBar] = True
isInfixOperator[Token`LongName`LeftArrowRightArrow] = True
isInfixOperator[Token`LongName`LeftDownTeeVector] = True
isInfixOperator[Token`LongName`LeftDownVector] = True
isInfixOperator[Token`LongName`LeftDownVectorBar] = True
isInfixOperator[Token`LongName`LeftRightArrow] = True
isInfixOperator[Token`LongName`LeftRightVector] = True
isInfixOperator[Token`LongName`LeftTeeArrow] = True
isInfixOperator[Token`LongName`LeftTeeVector] = True

isInfixOperator[Token`LongName`LeftTriangle] = True
isInfixOperator[Token`LongName`RightTriangle] = True
isInfixOperator[Token`LongName`NotLeftTriangle] = True
isInfixOperator[Token`LongName`NotRightTriangle] = True

isInfixOperator[Token`LongName`LeftTriangleEqual] = True
isInfixOperator[Token`LongName`RightTriangleEqual] = True
isInfixOperator[Token`LongName`NotLeftTriangleEqual] = True
isInfixOperator[Token`LongName`NotRightTriangleEqual] = True

isInfixOperator[Token`LongName`LeftUpDownVector] = True
isInfixOperator[Token`LongName`LeftUpTeeVector] = True
isInfixOperator[Token`LongName`LeftUpVector] = True
isInfixOperator[Token`LongName`LeftUpVectorBar] = True
isInfixOperator[Token`LongName`LeftVector] = True
isInfixOperator[Token`LongName`LeftVectorBar] = True

isInfixOperator[Token`LongName`LessEqual] = True
isInfixOperator[Token`LongName`GreaterEqual] = True
isInfixOperator[Token`LongName`NotLessEqual] = True
isInfixOperator[Token`LongName`NotGreaterEqual] = True

isInfixOperator[Token`LongName`LessFullEqual] = True
isInfixOperator[Token`LongName`NotLessFullEqual] = True

isInfixOperator[Token`LongName`LessLess] = True
isInfixOperator[Token`LongName`LessSlantEqual] = True
isInfixOperator[Token`LongName`LongEqual] = True
isInfixOperator[Token`LongName`LongLeftArrow] = True
isInfixOperator[Token`LongName`LongLeftRightArrow] = True
isInfixOperator[Token`LongName`LongRightArrow] = True
isInfixOperator[Token`LongName`LowerLeftArrow] = True
isInfixOperator[Token`LongName`LowerRightArrow] = True
isInfixOperator[Token`LongName`Nand] = True
isInfixOperator[Token`LongName`NestedGreaterGreater] = True

isInfixOperator[Token`LongName`NestedLessLess] = True
isInfixOperator[Token`LongName`NotNestedLessLess] = True

isInfixOperator[Token`LongName`Nor] = True
isInfixOperator[Token`LongName`NotCongruent] = True
isInfixOperator[Token`LongName`NotElement] = True
isInfixOperator[Token`LongName`NotEqual] = True
isInfixOperator[Token`LongName`NotEqualTilde] = True
isInfixOperator[Token`LongName`NotGreaterGreater] = True
isInfixOperator[Token`LongName`NotGreaterSlantEqual] = True
isInfixOperator[Token`LongName`NotHumpDownHump] = True
isInfixOperator[Token`LongName`NotHumpEqual] = True

isInfixOperator[Token`LongName`LeftTriangleBar] = True
isInfixOperator[Token`LongName`RightTriangleBar] = True

isInfixOperator[Token`LongName`NotLeftTriangleBar] = True

isInfixOperator[Token`LongName`NotLess] = True
isInfixOperator[Token`LongName`NotGreater] = True

isInfixOperator[Token`LongName`NotLessLess] = True
isInfixOperator[Token`LongName`NotLessSlantEqual] = True
isInfixOperator[Token`LongName`NotNestedGreaterGreater] = True
isInfixOperator[Token`LongName`NotNestedLessLess] = True

isInfixOperator[Token`LongName`NotReverseElement] = True
isInfixOperator[Token`LongName`NotRightTriangleBar] = True

isInfixOperator[Token`LongName`SquareSubset] = True
isInfixOperator[Token`LongName`SquareSuperset] = True
isInfixOperator[Token`LongName`NotSquareSubset] = True
isInfixOperator[Token`LongName`NotSquareSuperset] = True
isInfixOperator[Token`LongName`SquareSubsetEqual] = True
isInfixOperator[Token`LongName`SquareSupersetEqual] = True
isInfixOperator[Token`LongName`NotSquareSubsetEqual] = True
isInfixOperator[Token`LongName`NotSquareSupersetEqual] = True

isInfixOperator[Token`LongName`NotSubset] = True
isInfixOperator[Token`LongName`NotSubsetEqual] = True
isInfixOperator[Token`LongName`NotSuperset] = True
isInfixOperator[Token`LongName`NotSupersetEqual] = True
isInfixOperator[Token`LongName`NotTilde] = True
isInfixOperator[Token`LongName`NotTildeEqual] = True
isInfixOperator[Token`LongName`NotTildeFullEqual] = True
isInfixOperator[Token`LongName`NotTildeTilde] = True
isInfixOperator[Token`LongName`Or] = True
isInfixOperator[Token`LongName`PermutationProduct] = True

isInfixOperator[Token`LongName`Precedes] = True
isInfixOperator[Token`LongName`Succeeds] = True
isInfixOperator[Token`LongName`PrecedesEqual] = True
isInfixOperator[Token`LongName`SucceedsEqual] = True
isInfixOperator[Token`LongName`PrecedesTilde] = True
isInfixOperator[Token`LongName`SucceedsTilde] = True
isInfixOperator[Token`LongName`PrecedesSlantEqual] = True
isInfixOperator[Token`LongName`SucceedsSlantEqual] = True
isInfixOperator[Token`LongName`NotPrecedes] = True
isInfixOperator[Token`LongName`NotSucceeds] = True
isInfixOperator[Token`LongName`NotPrecedesEqual] = True
isInfixOperator[Token`LongName`NotSucceedsEqual] = True
isInfixOperator[Token`LongName`NotPrecedesTilde] = True
isInfixOperator[Token`LongName`NotSucceedsTilde] = True
isInfixOperator[Token`LongName`NotPrecedesSlantEqual] = True
isInfixOperator[Token`LongName`NotSucceedsSlantEqual] = True

isInfixOperator[Token`LongName`Proportion] = True
isInfixOperator[Token`LongName`Proportional] = True
isInfixOperator[Token`LongName`ReverseElement] = True
isInfixOperator[Token`LongName`ReverseEquilibrium] = True
isInfixOperator[Token`LongName`ReverseUpEquilibrium] = True
isInfixOperator[Token`LongName`RightArrow] = True
isInfixOperator[Token`LongName`RightArrowBar] = True
isInfixOperator[Token`LongName`RightArrowLeftArrow] = True
isInfixOperator[Token`LongName`RightDownTeeVector] = True
isInfixOperator[Token`LongName`RightDownVector] = True
isInfixOperator[Token`LongName`RightDownVectorBar] = True
isInfixOperator[Token`LongName`RightTeeArrow] = True
isInfixOperator[Token`LongName`RightTeeVector] = True

isInfixOperator[Token`LongName`RightUpDownVector] = True
isInfixOperator[Token`LongName`RightUpTeeVector] = True
isInfixOperator[Token`LongName`RightUpVector] = True
isInfixOperator[Token`LongName`RightUpVectorBar] = True
isInfixOperator[Token`LongName`RightVector] = True
isInfixOperator[Token`LongName`RightVectorBar] = True
isInfixOperator[Token`LongName`ShortDownArrow] = True
isInfixOperator[Token`LongName`ShortLeftArrow] = True
isInfixOperator[Token`LongName`ShortRightArrow] = True
isInfixOperator[Token`LongName`ShortUpArrow] = True
isInfixOperator[Token`LongName`SmallCircle] = True
isInfixOperator[Token`LongName`Star] = True
isInfixOperator[Token`LongName`Subset] = True
isInfixOperator[Token`LongName`SubsetEqual] = True
isInfixOperator[Token`LongName`Superset] = True
isInfixOperator[Token`LongName`SupersetEqual] = True
isInfixOperator[Token`LongName`TensorProduct] = True
isInfixOperator[Token`LongName`TensorWedge] = True
isInfixOperator[Token`LongName`Tilde] = True
isInfixOperator[Token`LongName`TildeEqual] = True
isInfixOperator[Token`LongName`TildeFullEqual] = True
isInfixOperator[Token`LongName`TildeTilde] = True
isInfixOperator[Token`LongName`Times] = True
isInfixOperator[Token`LongName`Union] = True
isInfixOperator[Token`LongName`UnionPlus] = True

isInfixOperator[Token`LongName`UpArrow] = True
isInfixOperator[Token`LongName`DownArrow] = True

isInfixOperator[Token`LongName`UpArrowBar] = True
isInfixOperator[Token`LongName`UpArrowDownArrow] = True
isInfixOperator[Token`LongName`UpDownArrow] = True
isInfixOperator[Token`LongName`UpEquilibrium] = True
isInfixOperator[Token`LongName`UpperLeftArrow] = True
isInfixOperator[Token`LongName`UpperRightArrow] = True
isInfixOperator[Token`LongName`UpTeeArrow] = True
isInfixOperator[Token`LongName`VectorGreater] = True
isInfixOperator[Token`LongName`VectorGreaterEqual] = True
isInfixOperator[Token`LongName`VectorLess] = True
isInfixOperator[Token`LongName`VectorLessEqual] = True
isInfixOperator[Token`LongName`Vee] = True
isInfixOperator[Token`LongName`VerticalSeparator] = True
isInfixOperator[Token`LongName`VerticalTilde] = True
isInfixOperator[Token`LongName`Wedge] = True
isInfixOperator[Token`LongName`Xor] = True

isInfixOperator[Token`LongName`VerticalBar] = True
isInfixOperator[Token`LongName`DoubleVerticalBar] = True
isInfixOperator[Token`LongName`NotVerticalBar] = True
isInfixOperator[Token`LongName`NotDoubleVerticalBar] = True

isInfixOperator[Token`Fake`ImplicitTimes] = True

isInfixOperator[_] = False







isInequalityOperator[Token`BangEqual] = True
isInequalityOperator[Token`EqualEqual] = True
isInequalityOperator[Token`Greater] = True
isInequalityOperator[Token`GreaterEqual] = True
isInequalityOperator[Token`Less] = True
isInequalityOperator[Token`LessEqual] = True
isInequalityOperator[Token`LongName`Equal] = True
isInequalityOperator[Token`LongName`GreaterEqual] = True
isInequalityOperator[Token`LongName`GreaterEqualLess] = True
isInequalityOperator[Token`LongName`GreaterFullEqual] = True
isInequalityOperator[Token`LongName`GreaterGreater] = True
isInequalityOperator[Token`LongName`GreaterLess] = True
isInequalityOperator[Token`LongName`GreaterSlantEqual] = True
isInequalityOperator[Token`LongName`GreaterTilde] = True
isInequalityOperator[Token`LongName`LessEqual] = True
isInequalityOperator[Token`LongName`LessEqualGreater] = True
isInequalityOperator[Token`LongName`LessFullEqual] = True
isInequalityOperator[Token`LongName`LessGreater] = True
isInequalityOperator[Token`LongName`LessLess] = True
isInequalityOperator[Token`LongName`LessSlantEqual] = True
isInequalityOperator[Token`LongName`LessTilde] = True
isInequalityOperator[Token`LongName`LongEqual] = True
isInequalityOperator[Token`LongName`NestedGreaterGreater] = True
isInequalityOperator[Token`LongName`NestedLessLess] = True
isInequalityOperator[Token`LongName`NotEqual] = True
isInequalityOperator[Token`LongName`NotGreater] = True
isInequalityOperator[Token`LongName`NotGreaterEqual] = True
isInequalityOperator[Token`LongName`NotGreaterFullEqual] = True
isInequalityOperator[Token`LongName`NotGreaterGreater] = True
isInequalityOperator[Token`LongName`NotGreaterLess] = True
isInequalityOperator[Token`LongName`NotGreaterSlantEqual] = True
isInequalityOperator[Token`LongName`NotGreaterTilde] = True
isInequalityOperator[Token`LongName`NotLess] = True
isInequalityOperator[Token`LongName`NotLessEqual] = True
isInequalityOperator[Token`LongName`NotLessFullEqual] = True
isInequalityOperator[Token`LongName`NotLessGreater] = True
isInequalityOperator[Token`LongName`NotLessLess] = True
isInequalityOperator[Token`LongName`NotLessSlantEqual] = True
isInequalityOperator[Token`LongName`NotLessTilde] = True
isInequalityOperator[Token`LongName`NotNestedGreaterGreater] = True
isInequalityOperator[Token`LongName`NotNestedLessLess] = True

isInequalityOperator[_] = False





isVectorInequalityOperator[Token`LongName`VectorGreater] = True
isVectorInequalityOperator[Token`LongName`VectorGreaterEqual] = True
isVectorInequalityOperator[Token`LongName`VectorLess] = True
isVectorInequalityOperator[Token`LongName`VectorLessEqual] = True

isVectorInequalityOperator[_] = False





isEmpty[Token`EndOfFile] = True
isEmpty[Token`Fake`ImplicitTimes] = True
isEmpty[Token`Error`EmptyString] = True
isEmpty[Token`Error`Aborted] = True
isEmpty[Token`Fake`ImplicitNull] = True
isEmpty[Token`Fake`ImplicitOne] = True
isEmpty[Token`Fake`ImplicitAll] = True
isEmpty[Token`Error`ExpectedOperand] = True
(*
isEmpty[Token`Newline] = True
*)

isEmpty[_] = False




isDifferentialD[Token`LongName`DifferentialD] = True
isDifferentialD[Token`LongName`CapitalDifferentialD] = True

isDifferentialD[_] = False






group1Bits[tok_] := group1Bits[tok] =
Which[
	isPossibleBeginningOfExpression[tok], BitShiftLeft[2^^001, 9],
	isCloser[tok],                        BitShiftLeft[2^^010, 9],
	isError[tok],                         BitShiftLeft[2^^011, 9],
	isTrivia[tok],                        BitShiftLeft[2^^100, 9],
	isInequalityOperator[tok],            BitShiftLeft[2^^101, 9],
	isVectorInequalityOperator[tok],      BitShiftLeft[2^^110, 9],
	(* unused                             BitShiftLeft[2^^111, 9],*)

	True,                                 BitShiftLeft[2^^000, 9]
]






Print["Generating TokenEnum..."]

operatorMacros = 
  Association[
   ToExpression["Token`LongName`" <> #] -> Next& /@ importedPunctuationLongNames]

joined = importedTokenEnumSource ~Join~ operatorMacros ~Join~ <| Token`Count -> Next |>

oldTokens = ToString /@ Keys[joined]


cur = 0
enumMap = <||>
KeyValueMap[(
    Which[
    	IntegerQ[#2],
    		cur = #2,
    	#2 === Next,
    		cur = cur + 1,
    	True,
    		cur = enumMap[#2]];
    AssociateTo[enumMap, #1 -> cur]) &, joined]


tokenCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include <cstdint> // for uint16_t

struct TokenEnum {

  uint16_t T;

  constexpr TokenEnum() : T(0) {}

  constexpr TokenEnum(uint16_t T) : T(T) {}

  constexpr uint16_t value() const {
    return (T & 0x1ff);
  }

  constexpr bool isPossibleBeginningOfExpression() const {
      return static_cast<bool>((T & 0xe00) == 0x200);
  }
  
  constexpr bool isCloser() const {
      return static_cast<bool>((T & 0xe00) == 0x400);
  }
  
  constexpr bool isError() const {
      return static_cast<bool>((T & 0xe00) == 0x600);
  }
  
  constexpr bool isTrivia() const {
      return static_cast<bool>((T & 0xe00) == 0x800);
  }
  
  constexpr bool isInequalityOperator() const {
      return static_cast<bool>((T & 0xe00) == 0xa00);
  }
  
  constexpr bool isVectorInequalityOperator() const {
      return static_cast<bool>((T & 0xe00) == 0xc00);
  }

  constexpr bool isInfixOperator() const {
      return static_cast<bool>((T & 0x1000) == 0x1000);
  }

  constexpr bool isEmpty() const {
      return static_cast<bool>((T & 0x2000) == 0x2000);
  }

  constexpr bool isDifferentialD() const {
      return static_cast<bool>((T & 0x4000) == 0x4000);
  }

};

bool operator==(TokenEnum a, TokenEnum b);

bool operator!=(TokenEnum a, TokenEnum b);
"} ~Join~
   KeyValueMap[(Row[{"constexpr TokenEnum ", toGlobal[#1], "(",
   	BitOr[
   		If[isDifferentialD[#1], 16^^4000, 0],
   		If[isEmpty[#1], 16^^2000, 0],
   		If[isInfixOperator[#1], 16^^1000, 0],
   		group1Bits[#1],
   		#2
   	], "); // { isDifferentialD:", isDifferentialD[#1], ", isEmpty:", isEmpty[#1], ", isInfixOperator:", isInfixOperator[#1], ", group1Bits:", group1Bits[#1], ", enum:", #2, " }"}])&, enumMap] ~Join~
{
"
"
}

Print["exporting TokenEnum.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "TokenEnum.h"}], Column[tokenCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]




(*
remove values like Error`First in:
<|
Error`Unknown -> Next,
Error`First -> Error`Unknown,
|>

because C switch statements cannot have duplicate cases

*)
uniqueEnums = DeleteCases[importedTokenEnumSource, v_ /; !IntegerQ[v] && UnsameQ[v, Next]]

tokens = Keys[uniqueEnums]

operatorMacros = ToExpression["Token`LongName`" <> #]& /@ importedPunctuationLongNames



tokens = tokens ~Join~ operatorMacros

tokenToSymbolCases = Row[{"case ", toGlobal[#], ".value(): return ", toGlobal[tokenToSymbol[#]], ";"}]& /@ tokens


tokenCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"TokenEnum.h\"

#include \"Symbol.h\"
#include \"Token.h\"

#include <cassert>
"} ~Join~
    {"SymbolPtr& TokenToSymbol(TokenEnum T) {"} ~Join~
    {"switch (T.value()) {"} ~Join~
    tokenToSymbolCases ~Join~
    {"default:"} ~Join~
    {"assert(false && \"Unhandled token type\"); return SYMBOL_TOKEN_UNKNOWN;"} ~Join~
    {"}"} ~Join~
    {"}"} ~Join~
    {""} ~Join~
    {
"bool operator==(TokenEnum a, TokenEnum b) {
  return a.value() == b.value();
}

bool operator!=(TokenEnum a, TokenEnum b) {
  return a.value() != b.value();
}
"}

Print["exporting TokenEnum.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "TokenEnum.cpp"}], Column[tokenCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Token"]

End[]

EndPackage[]
