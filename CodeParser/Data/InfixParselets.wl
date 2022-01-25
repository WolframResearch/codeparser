
(*
CodeParser Data file

Do not modify this file directly
*)

<|

Token`Unknown -> Parselet`InfixNullPointerParselet[],
Token`Whitespace -> Parselet`InfixNullPointerParselet[],
Token`InternalNewline -> Parselet`InfixNullPointerParselet[],
Token`Comment -> Parselet`InfixNullPointerParselet[],

Token`EndOfFile -> Parselet`InfixAssertFalseParselet[],


Token`ToplevelNewline -> Parselet`InfixToplevelNewlineParselet[],


Token`Error`ExpectedEqual -> Parselet`InfixAssertFalseParselet[],
Token`Error`Number -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnhandledCharacter -> Parselet`InfixAssertFalseParselet[],
Token`Error`ExpectedLetterlike -> Parselet`InfixAssertFalseParselet[],
Token`Error`Aborted -> Parselet`InfixAssertFalseParselet[],
Token`Error`ExpectedOperand -> Parselet`InfixAssertFalseParselet[],
Token`Error`ExpectedTag -> Parselet`InfixAssertFalseParselet[],
Token`Error`ExpectedFile -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnterminatedComment -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnterminatedString -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnterminatedFileString -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnterminatedLinearSyntaxBlob -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnsupportedToken -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnexpectedCloser -> Parselet`InfixAssertFalseParselet[],
Token`Error`UnsafeCharacterEncoding -> Parselet`InfixAssertFalseParselet[],

Token`BarGreater -> Parselet`InfixAssertFalseParselet[],
Token`CloseCurly -> Parselet`InfixAssertFalseParselet[],
Token`CloseParen -> Parselet`InfixAssertFalseParselet[],
Token`CloseSquare -> Parselet`InfixAssertFalseParselet[],
Token`LongName`CloseCurlyDoubleQuote -> Parselet`InfixAssertFalseParselet[],
Token`LongName`CloseCurlyQuote -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightAngleBracket -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightAssociation -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightBracketingBar -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightCeiling -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightDoubleBracket -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightDoubleBracketingBar -> Parselet`InfixAssertFalseParselet[],
Token`LongName`RightFloor -> Parselet`InfixAssertFalseParselet[],

Token`LongName`DifferentialD -> Parselet`InfixDifferentialDParselet[],
Token`LongName`CapitalDifferentialD -> Parselet`InfixDifferentialDParselet[],


(*
Binary
*)

Token`Slash -> Parselet`BinaryOperatorParselet[Token`Slash, Precedence`Slash, Divide],
Token`Caret -> Parselet`BinaryOperatorParselet[Token`Caret, Precedence`Caret, Power],
Token`CaretEqual -> Parselet`BinaryOperatorParselet[Token`CaretEqual, Precedence`CaretEqual, UpSet],
Token`CaretColonEqual -> Parselet`BinaryOperatorParselet[Token`CaretColonEqual, Precedence`CaretColonEqual, UpSetDelayed],
Token`SlashAt -> Parselet`BinaryOperatorParselet[Token`SlashAt, Precedence`SlashAt, Map],
Token`MinusGreater -> Parselet`BinaryOperatorParselet[Token`MinusGreater, Precedence`MinusGreater, Rule],
Token`AtAt -> Parselet`BinaryOperatorParselet[Token`AtAt, Precedence`AtAt, Apply],
Token`SlashSemi -> Parselet`BinaryOperatorParselet[Token`SlashSemi, Precedence`SlashSemi, Condition],
Token`SlashDot -> Parselet`BinaryOperatorParselet[Token`SlashDot, Precedence`SlashDot, ReplaceAll],
Token`ColonGreater -> Parselet`BinaryOperatorParselet[Token`ColonGreater, Precedence`ColonGreater, RuleDelayed],
Token`SlashSlashDot -> Parselet`BinaryOperatorParselet[Token`SlashSlashDot, Precedence`SlashSlashDot, ReplaceRepeated],
Token`PlusEqual -> Parselet`BinaryOperatorParselet[Token`PlusEqual, Precedence`PlusEqual, AddTo],
Token`StarEqual -> Parselet`BinaryOperatorParselet[Token`StarEqual, Precedence`StarEqual, TimesBy],
Token`MinusEqual -> Parselet`BinaryOperatorParselet[Token`MinusEqual, Precedence`MinusEqual, SubtractFrom],
Token`SlashEqual -> Parselet`BinaryOperatorParselet[Token`SlashEqual, Precedence`SlashEqual, DivideBy],
Token`LessMinusGreater -> Parselet`BinaryOperatorParselet[Token`LessMinusGreater, Precedence`LessMinusGreater, System`TwoWayRule],
Token`SlashSlashAt -> Parselet`BinaryOperatorParselet[Token`SlashSlashAt, Precedence`SlashSlashAt, MapAll],
Token`At -> Parselet`BinaryOperatorParselet[Token`At, Precedence`At, CodeParser`BinaryAt],
Token`AtAtAt -> Parselet`BinaryOperatorParselet[Token`AtAtAt, Precedence`AtAtAt, System`MapApply],
Token`SlashSlash -> Parselet`BinaryOperatorParselet[Token`SlashSlash, Precedence`SlashSlash, CodeParser`BinarySlashSlash],
Token`Question -> Parselet`BinaryOperatorParselet[Token`Question, Precedence`Infix`Question, PatternTest],
Token`BarMinusGreater -> Parselet`BinaryOperatorParselet[Token`BarMinusGreater, Precedence`BarMinusGreater, Function],
Token`SlashSlashEqual -> Parselet`BinaryOperatorParselet[Token`SlashSlashEqual, Precedence`SlashSlashEqual, System`ApplyTo],

Token`LongName`Divide -> Parselet`BinaryOperatorParselet[Token`LongName`Divide, Precedence`LongName`Divide, Divide],
Token`LongName`DivisionSlash -> Parselet`BinaryOperatorParselet[Token`LongName`DivisionSlash, Precedence`LongName`DivisionSlash, Divide],
Token`LongName`Implies -> Parselet`BinaryOperatorParselet[Token`LongName`Implies, Precedence`LongName`Implies, Implies],
Token`LongName`RoundImplies -> Parselet`BinaryOperatorParselet[Token`LongName`RoundImplies, Precedence`LongName`RoundImplies, RoundImplies],
Token`LongName`PlusMinus -> Parselet`BinaryOperatorParselet[Token`LongName`PlusMinus, Precedence`Infix`LongName`PlusMinus, PlusMinus],
Token`LongName`DirectedEdge -> Parselet`BinaryOperatorParselet[Token`LongName`DirectedEdge, Precedence`LongName`DirectedEdge, DirectedEdge],
Token`LongName`Rule -> Parselet`BinaryOperatorParselet[Token`LongName`Rule, Precedence`LongName`Rule, Rule],
Token`LongName`RuleDelayed -> Parselet`BinaryOperatorParselet[Token`LongName`RuleDelayed, Precedence`LongName`RuleDelayed, RuleDelayed],
Token`LongName`UndirectedEdge -> Parselet`BinaryOperatorParselet[Token`LongName`UndirectedEdge, Precedence`LongName`UndirectedEdge, UndirectedEdge],
Token`LongName`Function -> Parselet`BinaryOperatorParselet[Token`LongName`Function, Precedence`LongName`Function, Function],
Token`LongName`MinusPlus -> Parselet`BinaryOperatorParselet[Token`LongName`MinusPlus, Precedence`Infix`LongName`MinusPlus, MinusPlus],
Token`LongName`TwoWayRule -> Parselet`BinaryOperatorParselet[Token`LongName`TwoWayRule, Precedence`LongName`TwoWayRule, System`TwoWayRule],
Token`LongName`InvisibleApplication -> Parselet`BinaryOperatorParselet[Token`LongName`InvisibleApplication, Precedence`LongName`InvisibleApplication, CodeParser`BinaryAt],
Token`LongName`CircleMinus -> Parselet`BinaryOperatorParselet[Token`LongName`CircleMinus, Precedence`LongName`CircleMinus, CircleMinus],
Token`LongName`SuchThat -> Parselet`BinaryOperatorParselet[Token`LongName`SuchThat, Precedence`LongName`SuchThat, SuchThat],
Token`LongName`Perpendicular -> Parselet`BinaryOperatorParselet[Token`LongName`Perpendicular, Precedence`LongName`Perpendicular, Perpendicular],
Token`LongName`Because -> Parselet`BinaryOperatorParselet[Token`LongName`Because, Precedence`LongName`Because, Because],
Token`LongName`Therefore -> Parselet`BinaryOperatorParselet[Token`LongName`Therefore, Precedence`LongName`Therefore, Therefore],
Token`LongName`RightTee -> Parselet`BinaryOperatorParselet[Token`LongName`RightTee, Precedence`LongName`RightTee, RightTee],
Token`LongName`LeftTee -> Parselet`BinaryOperatorParselet[Token`LongName`LeftTee, Precedence`LongName`LeftTee, LeftTee],
Token`LongName`DoubleRightTee -> Parselet`BinaryOperatorParselet[Token`LongName`DoubleRightTee, Precedence`LongName`DoubleRightTee, DoubleRightTee],
Token`LongName`DoubleLeftTee -> Parselet`BinaryOperatorParselet[Token`LongName`DoubleLeftTee, Precedence`LongName`DoubleLeftTee, DoubleLeftTee],
Token`LongName`UpTee -> Parselet`BinaryOperatorParselet[Token`LongName`UpTee, Precedence`LongName`UpTee, UpTee],
Token`LongName`DownTee -> Parselet`BinaryOperatorParselet[Token`LongName`DownTee, Precedence`LongName`DownTee, DownTee],
Token`LongName`Application -> Parselet`BinaryOperatorParselet[Token`LongName`Application, Precedence`LongName`Application, System`Application],


(*
Infix

Note that these are the operators that make sense to be infix in WL source code.

These may not necessarily correspond to Flat functions in WL.
*)
Token`Minus -> Parselet`InfixOperatorParselet[Token`Minus, Precedence`Infix`Minus, Plus],
Token`EqualEqualEqual -> Parselet`InfixOperatorParselet[Token`EqualEqualEqual, Precedence`EqualEqualEqual, SameQ],
Token`EqualBangEqual -> Parselet`InfixOperatorParselet[Token`EqualBangEqual, Precedence`EqualBangEqual, UnsameQ],
Token`Plus -> Parselet`InfixOperatorParselet[Token`Plus, Precedence`Infix`Plus, Plus],
Token`Star -> Parselet`InfixOperatorParselet[Token`Star, Precedence`Star, Times],
Token`Dot -> Parselet`InfixOperatorParselet[Token`Dot, Precedence`Dot, Dot],
Token`StarStar -> Parselet`InfixOperatorParselet[Token`StarStar, Precedence`StarStar, NonCommutativeMultiply],
Token`AmpAmp -> Parselet`InfixOperatorParselet[Token`AmpAmp, Precedence`AmpAmp, And],
Token`BarBar -> Parselet`InfixOperatorParselet[Token`BarBar, Precedence`BarBar, Or],
Token`Bar -> Parselet`InfixOperatorParselet[Token`Bar, Precedence`Bar, Alternatives],
Token`LessGreater -> Parselet`InfixOperatorParselet[Token`LessGreater, Precedence`LessGreater, StringJoin],
Token`TildeTilde -> Parselet`InfixOperatorParselet[Token`TildeTilde, Precedence`TildeTilde, StringExpression],
Token`AtStar -> Parselet`InfixOperatorParselet[Token`AtStar, Precedence`AtStar, Composition],
Token`SlashStar -> Parselet`InfixOperatorParselet[Token`SlashStar, Precedence`SlashStar, RightComposition],

(*
Set relations
*)
Token`LongName`Element -> Parselet`InfixOperatorParselet[Token`LongName`Element, Precedence`Class`SetRelations, Element],
Token`LongName`Subset -> Parselet`InfixOperatorParselet[Token`LongName`Subset, Precedence`Class`SetRelations, Subset],
Token`LongName`Superset -> Parselet`InfixOperatorParselet[Token`LongName`Superset, Precedence`Class`SetRelations, Superset],
Token`LongName`SubsetEqual -> Parselet`InfixOperatorParselet[Token`LongName`SubsetEqual, Precedence`Class`SetRelations, SubsetEqual],
Token`LongName`SupersetEqual -> Parselet`InfixOperatorParselet[Token`LongName`SupersetEqual, Precedence`Class`SetRelations, SupersetEqual],
Token`LongName`NotElement -> Parselet`InfixOperatorParselet[Token`LongName`NotElement, Precedence`Class`SetRelations, NotElement],
Token`LongName`NotSubset -> Parselet`InfixOperatorParselet[Token`LongName`NotSubset, Precedence`Class`SetRelations, NotSubset],
Token`LongName`NotSuperset -> Parselet`InfixOperatorParselet[Token`LongName`NotSuperset, Precedence`Class`SetRelations, NotSuperset],
Token`LongName`NotSubsetEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotSubsetEqual, Precedence`Class`SetRelations, NotSubsetEqual],
Token`LongName`NotSupersetEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotSupersetEqual, Precedence`Class`SetRelations, NotSupersetEqual],
Token`LongName`SquareSubset -> Parselet`InfixOperatorParselet[Token`LongName`SquareSubset, Precedence`Class`SetRelations, SquareSubset],
Token`LongName`SquareSuperset -> Parselet`InfixOperatorParselet[Token`LongName`SquareSuperset, Precedence`Class`SetRelations, SquareSuperset],
Token`LongName`NotSquareSubset -> Parselet`InfixOperatorParselet[Token`LongName`NotSquareSubset, Precedence`Class`SetRelations, NotSquareSubset],
Token`LongName`NotSquareSuperset -> Parselet`InfixOperatorParselet[Token`LongName`NotSquareSuperset, Precedence`Class`SetRelations, NotSquareSuperset],
Token`LongName`SquareSubsetEqual -> Parselet`InfixOperatorParselet[Token`LongName`SquareSubsetEqual, Precedence`Class`SetRelations, SquareSubsetEqual],
Token`LongName`SquareSupersetEqual -> Parselet`InfixOperatorParselet[Token`LongName`SquareSupersetEqual, Precedence`Class`SetRelations, SquareSupersetEqual],
Token`LongName`NotSquareSubsetEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotSquareSubsetEqual, Precedence`Class`SetRelations, NotSquareSubsetEqual],
Token`LongName`NotSquareSupersetEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotSquareSupersetEqual, Precedence`Class`SetRelations, NotSquareSupersetEqual],
Token`LongName`ReverseElement -> Parselet`InfixOperatorParselet[Token`LongName`ReverseElement, Precedence`Class`SetRelations, ReverseElement],
Token`LongName`NotReverseElement -> Parselet`InfixOperatorParselet[Token`LongName`NotReverseElement, Precedence`Class`SetRelations, NotReverseElement],
Token`LongName`Distributed -> Parselet`InfixOperatorParselet[Token`LongName`Distributed, Precedence`Class`SetRelations, Distributed],

Token`LongName`ImplicitPlus -> Parselet`InfixOperatorParselet[Token`LongName`ImplicitPlus, Precedence`LongName`ImplicitPlus, Plus],
Token`LongName`Times -> Parselet`InfixOperatorParselet[Token`LongName`Times, Precedence`LongName`Times, Times],
Token`LongName`InvisibleTimes -> Parselet`InfixOperatorParselet[Token`LongName`InvisibleTimes, Precedence`LongName`InvisibleTimes, Times],
Token`LongName`And -> Parselet`InfixOperatorParselet[Token`LongName`And, Precedence`LongName`And, And],
Token`LongName`Or -> Parselet`InfixOperatorParselet[Token`LongName`Or, Precedence`LongName`Or, Or],
Token`LongName`Xor -> Parselet`InfixOperatorParselet[Token`LongName`Xor, Precedence`LongName`Xor, Xor],
Token`LongName`Nand -> Parselet`InfixOperatorParselet[Token`LongName`Nand, Precedence`LongName`Nand, Nand],
Token`LongName`Nor -> Parselet`InfixOperatorParselet[Token`LongName`Nor, Precedence`LongName`Nor, Nor],

(*
Horizontal arrows
*)
Token`LongName`LeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`LeftArrow, Precedence`Class`HorizontalArrows, LeftArrow],
Token`LongName`RightArrow -> Parselet`InfixOperatorParselet[Token`LongName`RightArrow, Precedence`Class`HorizontalArrows, RightArrow],
Token`LongName`LeftRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`LeftRightArrow, Precedence`Class`HorizontalArrows, LeftRightArrow],
Token`LongName`LeftTeeArrow -> Parselet`InfixOperatorParselet[Token`LongName`LeftTeeArrow, Precedence`Class`HorizontalArrows, LeftTeeArrow],
Token`LongName`RightTeeArrow -> Parselet`InfixOperatorParselet[Token`LongName`RightTeeArrow, Precedence`Class`HorizontalArrows, RightTeeArrow],
Token`LongName`RightArrowLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`RightArrowLeftArrow, Precedence`Class`HorizontalArrows, RightArrowLeftArrow],
Token`LongName`LeftArrowRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`LeftArrowRightArrow, Precedence`Class`HorizontalArrows, LeftArrowRightArrow],
Token`LongName`DoubleLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleLeftArrow, Precedence`Class`HorizontalArrows, DoubleLeftArrow],
Token`LongName`DoubleRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleRightArrow, Precedence`Class`HorizontalArrows, DoubleRightArrow],
Token`LongName`DoubleLeftRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleLeftRightArrow, Precedence`Class`HorizontalArrows, DoubleLeftRightArrow],
Token`LongName`LeftArrowBar -> Parselet`InfixOperatorParselet[Token`LongName`LeftArrowBar, Precedence`Class`HorizontalArrows, LeftArrowBar],
Token`LongName`RightArrowBar -> Parselet`InfixOperatorParselet[Token`LongName`RightArrowBar, Precedence`Class`HorizontalArrows, RightArrowBar],
Token`LongName`ShortRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`ShortRightArrow, Precedence`Class`HorizontalArrows, ShortRightArrow],
Token`LongName`ShortLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`ShortLeftArrow, Precedence`Class`HorizontalArrows, ShortLeftArrow],

(*
Diagonal arrow operators
*)
Token`LongName`UpperLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`UpperLeftArrow, Precedence`Class`DiagonalArrowOperators, UpperLeftArrow],
Token`LongName`UpperRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`UpperRightArrow, Precedence`Class`DiagonalArrowOperators, UpperRightArrow],
Token`LongName`LowerRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`LowerRightArrow, Precedence`Class`DiagonalArrowOperators, LowerRightArrow],
Token`LongName`LowerLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`LowerLeftArrow, Precedence`Class`DiagonalArrowOperators, LowerLeftArrow],

(*
Vector operators
*)
Token`LongName`LeftVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftVector, Precedence`Class`VectorOperators, LeftVector],
Token`LongName`RightVector -> Parselet`InfixOperatorParselet[Token`LongName`RightVector, Precedence`Class`VectorOperators, RightVector],
Token`LongName`LeftRightVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftRightVector, Precedence`Class`VectorOperators, LeftRightVector],
Token`LongName`LeftVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`LeftVectorBar, Precedence`Class`VectorOperators, LeftVectorBar],
Token`LongName`RightVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`RightVectorBar, Precedence`Class`VectorOperators, RightVectorBar],
Token`LongName`LeftTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftTeeVector, Precedence`Class`VectorOperators, LeftTeeVector],
Token`LongName`RightTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`RightTeeVector, Precedence`Class`VectorOperators, RightTeeVector],
Token`LongName`DownLeftVector -> Parselet`InfixOperatorParselet[Token`LongName`DownLeftVector, Precedence`Class`VectorOperators, DownLeftVector],
Token`LongName`DownRightVector -> Parselet`InfixOperatorParselet[Token`LongName`DownRightVector, Precedence`Class`VectorOperators, DownRightVector],
Token`LongName`DownLeftRightVector -> Parselet`InfixOperatorParselet[Token`LongName`DownLeftRightVector, Precedence`Class`VectorOperators, DownLeftRightVector],
Token`LongName`DownLeftVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`DownLeftVectorBar, Precedence`Class`VectorOperators, DownLeftVectorBar],
Token`LongName`DownRightVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`DownRightVectorBar, Precedence`Class`VectorOperators, DownRightVectorBar],
Token`LongName`DownLeftTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`DownLeftTeeVector, Precedence`Class`VectorOperators, DownLeftTeeVector],
Token`LongName`DownRightTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`DownRightTeeVector, Precedence`Class`VectorOperators, DownRightTeeVector],

(*
Vertical arrow operators
*)
Token`LongName`UpArrow -> Parselet`InfixOperatorParselet[Token`LongName`UpArrow, Precedence`Class`VerticalArrowOperators, UpArrow],
Token`LongName`DownArrow -> Parselet`InfixOperatorParselet[Token`LongName`DownArrow, Precedence`Class`VerticalArrowOperators, DownArrow],
Token`LongName`UpDownArrow -> Parselet`InfixOperatorParselet[Token`LongName`UpDownArrow, Precedence`Class`VerticalArrowOperators, UpDownArrow],
Token`LongName`UpTeeArrow -> Parselet`InfixOperatorParselet[Token`LongName`UpTeeArrow, Precedence`Class`VerticalArrowOperators, UpTeeArrow],
Token`LongName`DownTeeArrow -> Parselet`InfixOperatorParselet[Token`LongName`DownTeeArrow, Precedence`Class`VerticalArrowOperators, DownTeeArrow],
Token`LongName`UpArrowDownArrow -> Parselet`InfixOperatorParselet[Token`LongName`UpArrowDownArrow, Precedence`Class`VerticalArrowOperators, UpArrowDownArrow],
Token`LongName`DoubleUpArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleUpArrow, Precedence`Class`VerticalArrowOperators, DoubleUpArrow],
Token`LongName`DoubleDownArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleDownArrow, Precedence`Class`VerticalArrowOperators, DoubleDownArrow],
Token`LongName`DoubleUpDownArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleUpDownArrow, Precedence`Class`VerticalArrowOperators, DoubleUpDownArrow],
Token`LongName`DownArrowUpArrow -> Parselet`InfixOperatorParselet[Token`LongName`DownArrowUpArrow, Precedence`Class`VerticalArrowOperators, DownArrowUpArrow],
(*
itai asking about precedence of "long" arrows:
https://mail-archive.wolfram.com/archive/l-typeset/2021/Jul00/0000.html
*)
Token`LongName`LongLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`LongLeftArrow, Precedence`Class`VerticalArrowOperators, LongLeftArrow],
Token`LongName`LongRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`LongRightArrow, Precedence`Class`VerticalArrowOperators, LongRightArrow],
Token`LongName`LongLeftRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`LongLeftRightArrow, Precedence`Class`VerticalArrowOperators, LongLeftRightArrow],
Token`LongName`DoubleLongLeftArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleLongLeftArrow, Precedence`Class`VerticalArrowOperators, DoubleLongLeftArrow],
Token`LongName`DoubleLongRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleLongRightArrow, Precedence`Class`VerticalArrowOperators, DoubleLongRightArrow],
Token`LongName`DoubleLongLeftRightArrow -> Parselet`InfixOperatorParselet[Token`LongName`DoubleLongLeftRightArrow, Precedence`Class`VerticalArrowOperators, DoubleLongLeftRightArrow],
Token`LongName`UpArrowBar -> Parselet`InfixOperatorParselet[Token`LongName`UpArrowBar, Precedence`Class`VerticalArrowOperators, UpArrowBar],
Token`LongName`DownArrowBar -> Parselet`InfixOperatorParselet[Token`LongName`DownArrowBar, Precedence`Class`VerticalArrowOperators, DownArrowBar],
Token`LongName`ShortUpArrow -> Parselet`InfixOperatorParselet[Token`LongName`ShortUpArrow, Precedence`Class`VerticalArrowOperators, ShortUpArrow],
Token`LongName`ShortDownArrow -> Parselet`InfixOperatorParselet[Token`LongName`ShortDownArrow, Precedence`Class`VerticalArrowOperators, ShortDownArrow],


(*
Vertical vector operators
*)
Token`LongName`RightUpVector -> Parselet`InfixOperatorParselet[Token`LongName`RightUpVector, Precedence`Class`VerticalVectorOperators, RightUpVector],
Token`LongName`LeftUpVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftUpVector, Precedence`Class`VerticalVectorOperators, LeftUpVector],
Token`LongName`RightDownVector -> Parselet`InfixOperatorParselet[Token`LongName`RightDownVector, Precedence`Class`VerticalVectorOperators, RightDownVector],
Token`LongName`LeftDownVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftDownVector, Precedence`Class`VerticalVectorOperators, LeftDownVector],
Token`LongName`RightUpDownVector -> Parselet`InfixOperatorParselet[Token`LongName`RightUpDownVector, Precedence`Class`VerticalVectorOperators, RightUpDownVector],
Token`LongName`LeftUpDownVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftUpDownVector, Precedence`Class`VerticalVectorOperators, LeftUpDownVector],
Token`LongName`RightUpVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`RightUpVectorBar, Precedence`Class`VerticalVectorOperators, RightUpVectorBar],
Token`LongName`RightDownVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`RightDownVectorBar, Precedence`Class`VerticalVectorOperators, RightDownVectorBar],
Token`LongName`LeftUpVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`LeftUpVectorBar, Precedence`Class`VerticalVectorOperators, LeftUpVectorBar],
Token`LongName`LeftDownVectorBar -> Parselet`InfixOperatorParselet[Token`LongName`LeftDownVectorBar, Precedence`Class`VerticalVectorOperators, LeftDownVectorBar],
Token`LongName`RightUpTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`RightUpTeeVector, Precedence`Class`VerticalVectorOperators, RightUpTeeVector],
Token`LongName`RightDownTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`RightDownTeeVector, Precedence`Class`VerticalVectorOperators, RightDownTeeVector],
Token`LongName`LeftUpTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftUpTeeVector, Precedence`Class`VerticalVectorOperators, LeftUpTeeVector],
Token`LongName`LeftDownTeeVector -> Parselet`InfixOperatorParselet[Token`LongName`LeftDownTeeVector, Precedence`Class`VerticalVectorOperators, LeftDownTeeVector],
Token`LongName`UpEquilibrium -> Parselet`InfixOperatorParselet[Token`LongName`UpEquilibrium, Precedence`Class`VerticalVectorOperators, UpEquilibrium],
Token`LongName`ReverseUpEquilibrium -> Parselet`InfixOperatorParselet[Token`LongName`ReverseUpEquilibrium, Precedence`Class`VerticalVectorOperators, ReverseUpEquilibrium],


Token`LongName`CenterDot -> Parselet`InfixOperatorParselet[Token`LongName`CenterDot, Precedence`LongName`CenterDot, CenterDot],
Token`LongName`Equivalent -> Parselet`InfixOperatorParselet[Token`LongName`Equivalent, Precedence`LongName`Equivalent, Equivalent],
Token`LongName`CircleDot -> Parselet`InfixOperatorParselet[Token`LongName`CircleDot, Precedence`LongName`CircleDot, CircleDot],
Token`LongName`Conditioned -> Parselet`InfixOperatorParselet[Token`LongName`Conditioned, Precedence`LongName`Conditioned, Conditioned],

(*
Union operators
*)
Token`LongName`Union -> Parselet`InfixOperatorParselet[Token`LongName`Union, Precedence`Class`UnionOperators, Union],
Token`LongName`SquareUnion -> Parselet`InfixOperatorParselet[Token`LongName`SquareUnion, Precedence`Class`UnionOperators, SquareUnion],
Token`LongName`UnionPlus -> Parselet`InfixOperatorParselet[Token`LongName`UnionPlus, Precedence`Class`UnionOperators, UnionPlus],

(*
Intersection operators
*)
Token`LongName`Intersection -> Parselet`InfixOperatorParselet[Token`LongName`Intersection, Precedence`Class`IntersectionOperators, Intersection],
Token`LongName`SquareIntersection -> Parselet`InfixOperatorParselet[Token`LongName`SquareIntersection, Precedence`Class`IntersectionOperators, SquareIntersection],


Token`LongName`TensorWedge -> Parselet`InfixOperatorParselet[Token`LongName`TensorWedge, Precedence`LongName`TensorWedge, TensorWedge],
Token`LongName`TensorProduct -> Parselet`InfixOperatorParselet[Token`LongName`TensorProduct, Precedence`LongName`TensorProduct, TensorProduct],
Token`LongName`Cross -> Parselet`InfixOperatorParselet[Token`LongName`Cross, Precedence`LongName`Cross, Cross],
Token`LongName`SmallCircle -> Parselet`InfixOperatorParselet[Token`LongName`SmallCircle, Precedence`LongName`SmallCircle, SmallCircle],
Token`LongName`Divides -> Parselet`InfixOperatorParselet[Token`LongName`Divides, Precedence`LongName`Divides, Divisible],
Token`LongName`VerticalSeparator -> Parselet`InfixOperatorParselet[Token`LongName`VerticalSeparator, Precedence`LongName`VerticalSeparator, VerticalSeparator],
Token`LongName`Backslash -> Parselet`InfixOperatorParselet[Token`LongName`Backslash, Precedence`LongName`Backslash, Backslash],
Token`LongName`Diamond -> Parselet`InfixOperatorParselet[Token`LongName`Diamond, Precedence`LongName`Diamond, Diamond],
Token`LongName`Wedge -> Parselet`InfixOperatorParselet[Token`LongName`Wedge, Precedence`LongName`Wedge, Wedge],
Token`LongName`Vee -> Parselet`InfixOperatorParselet[Token`LongName`Vee, Precedence`LongName`Vee, Vee],
Token`LongName`CircleTimes -> Parselet`InfixOperatorParselet[Token`LongName`CircleTimes, Precedence`Infix`LongName`CircleTimes, CircleTimes],
Token`LongName`Star -> Parselet`InfixOperatorParselet[Token`LongName`Star, Precedence`LongName`Star, Star],
Token`LongName`VerticalTilde -> Parselet`InfixOperatorParselet[Token`LongName`VerticalTilde, Precedence`LongName`VerticalTilde, VerticalTilde],
Token`LongName`Coproduct -> Parselet`InfixOperatorParselet[Token`LongName`Coproduct, Precedence`Infix`LongName`Coproduct, Coproduct],
Token`LongName`Cap -> Parselet`InfixOperatorParselet[Token`LongName`Cap, Precedence`LongName`Cap, Cap],
Token`LongName`Cup -> Parselet`InfixOperatorParselet[Token`LongName`Cup, Precedence`LongName`Cup, Cup],
Token`LongName`CirclePlus -> Parselet`InfixOperatorParselet[Token`LongName`CirclePlus, Precedence`LongName`CirclePlus, CirclePlus],
Token`LongName`VerticalBar -> Parselet`InfixOperatorParselet[Token`LongName`VerticalBar, Precedence`LongName`VerticalBar, VerticalBar],
Token`LongName`DoubleVerticalBar -> Parselet`InfixOperatorParselet[Token`LongName`DoubleVerticalBar, Precedence`LongName`DoubleVerticalBar, DoubleVerticalBar],
Token`LongName`NotVerticalBar -> Parselet`InfixOperatorParselet[Token`LongName`NotVerticalBar, Precedence`LongName`NotVerticalBar, NotVerticalBar],
Token`LongName`NotDoubleVerticalBar -> Parselet`InfixOperatorParselet[Token`LongName`NotDoubleVerticalBar, Precedence`LongName`NotDoubleVerticalBar, NotDoubleVerticalBar],

(*
Ordering operators
*)
Token`LongName`LeftTriangle -> Parselet`InfixOperatorParselet[Token`LongName`LeftTriangle, Precedence`Class`OrderingOperators, LeftTriangle],
Token`LongName`RightTriangle -> Parselet`InfixOperatorParselet[Token`LongName`RightTriangle, Precedence`Class`OrderingOperators, RightTriangle],
Token`LongName`NotLeftTriangle -> Parselet`InfixOperatorParselet[Token`LongName`NotLeftTriangle, Precedence`Class`OrderingOperators, NotLeftTriangle],
Token`LongName`NotRightTriangle -> Parselet`InfixOperatorParselet[Token`LongName`NotRightTriangle, Precedence`Class`OrderingOperators, NotRightTriangle],
Token`LongName`LeftTriangleEqual -> Parselet`InfixOperatorParselet[Token`LongName`LeftTriangleEqual, Precedence`Class`OrderingOperators, LeftTriangleEqual],
Token`LongName`RightTriangleEqual -> Parselet`InfixOperatorParselet[Token`LongName`RightTriangleEqual, Precedence`Class`OrderingOperators, RightTriangleEqual],
Token`LongName`NotLeftTriangleEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotLeftTriangleEqual, Precedence`Class`OrderingOperators, NotLeftTriangleEqual],
Token`LongName`NotRightTriangleEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotRightTriangleEqual, Precedence`Class`OrderingOperators, NotRightTriangleEqual],
Token`LongName`LeftTriangleBar -> Parselet`InfixOperatorParselet[Token`LongName`LeftTriangleBar, Precedence`Class`OrderingOperators, LeftTriangleBar],
Token`LongName`RightTriangleBar -> Parselet`InfixOperatorParselet[Token`LongName`RightTriangleBar, Precedence`Class`OrderingOperators, RightTriangleBar],
Token`LongName`NotLeftTriangleBar -> Parselet`InfixOperatorParselet[Token`LongName`NotLeftTriangleBar, Precedence`Class`OrderingOperators, NotLeftTriangleBar],
Token`LongName`NotRightTriangleBar -> Parselet`InfixOperatorParselet[Token`LongName`NotRightTriangleBar, Precedence`Class`OrderingOperators, NotRightTriangleBar],
Token`LongName`TildeEqual -> Parselet`InfixOperatorParselet[Token`LongName`TildeEqual, Precedence`Class`OrderingOperators, TildeEqual],
Token`LongName`NotTildeEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotTildeEqual, Precedence`Class`OrderingOperators, NotTildeEqual],
Token`LongName`TildeFullEqual -> Parselet`InfixOperatorParselet[Token`LongName`TildeFullEqual, Precedence`Class`OrderingOperators, TildeFullEqual],
Token`LongName`NotTildeFullEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotTildeFullEqual, Precedence`Class`OrderingOperators, NotTildeFullEqual],
Token`LongName`Tilde -> Parselet`InfixOperatorParselet[Token`LongName`Tilde, Precedence`Class`OrderingOperators, Tilde],
Token`LongName`NotTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotTilde, Precedence`Class`OrderingOperators, NotTilde],
Token`LongName`EqualTilde -> Parselet`InfixOperatorParselet[Token`LongName`EqualTilde, Precedence`Class`OrderingOperators, EqualTilde],
Token`LongName`NotEqualTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotEqualTilde, Precedence`Class`OrderingOperators, NotEqualTilde],
Token`LongName`TildeTilde -> Parselet`InfixOperatorParselet[Token`LongName`TildeTilde, Precedence`Class`OrderingOperators, TildeTilde],
Token`LongName`NotTildeTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotTildeTilde, Precedence`Class`OrderingOperators, NotTildeTilde],
Token`LongName`Proportional -> Parselet`InfixOperatorParselet[Token`LongName`Proportional, Precedence`Class`OrderingOperators, Proportional],
Token`LongName`Proportion -> Parselet`InfixOperatorParselet[Token`LongName`Proportion, Precedence`Class`OrderingOperators, Proportion],
Token`LongName`Congruent -> Parselet`InfixOperatorParselet[Token`LongName`Congruent, Precedence`Class`OrderingOperators, Congruent],
Token`LongName`NotCongruent -> Parselet`InfixOperatorParselet[Token`LongName`NotCongruent, Precedence`Class`OrderingOperators, NotCongruent],
Token`LongName`Equilibrium -> Parselet`InfixOperatorParselet[Token`LongName`Equilibrium, Precedence`Class`OrderingOperators, Equilibrium],
Token`LongName`ReverseEquilibrium -> Parselet`InfixOperatorParselet[Token`LongName`ReverseEquilibrium, Precedence`Class`OrderingOperators, ReverseEquilibrium],
Token`LongName`DotEqual -> Parselet`InfixOperatorParselet[Token`LongName`DotEqual, Precedence`Class`OrderingOperators, DotEqual],
Token`LongName`Precedes -> Parselet`InfixOperatorParselet[Token`LongName`Precedes, Precedence`Class`OrderingOperators, Precedes],
Token`LongName`Succeeds -> Parselet`InfixOperatorParselet[Token`LongName`Succeeds, Precedence`Class`OrderingOperators, Succeeds],
Token`LongName`PrecedesEqual -> Parselet`InfixOperatorParselet[Token`LongName`PrecedesEqual, Precedence`Class`OrderingOperators, PrecedesEqual],
Token`LongName`SucceedsEqual -> Parselet`InfixOperatorParselet[Token`LongName`SucceedsEqual, Precedence`Class`OrderingOperators, SucceedsEqual],
Token`LongName`PrecedesTilde -> Parselet`InfixOperatorParselet[Token`LongName`PrecedesTilde, Precedence`Class`OrderingOperators, PrecedesTilde],
Token`LongName`SucceedsTilde -> Parselet`InfixOperatorParselet[Token`LongName`SucceedsTilde, Precedence`Class`OrderingOperators, SucceedsTilde],
Token`LongName`PrecedesSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`PrecedesSlantEqual, Precedence`Class`OrderingOperators, PrecedesSlantEqual],
Token`LongName`SucceedsSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`SucceedsSlantEqual, Precedence`Class`OrderingOperators, SucceedsSlantEqual],
Token`LongName`NotPrecedes -> Parselet`InfixOperatorParselet[Token`LongName`NotPrecedes, Precedence`Class`OrderingOperators, NotPrecedes],
Token`LongName`NotSucceeds -> Parselet`InfixOperatorParselet[Token`LongName`NotSucceeds, Precedence`Class`OrderingOperators, NotSucceeds],
Token`LongName`NotPrecedesEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotPrecedesEqual, Precedence`Class`OrderingOperators, NotPrecedesEqual],
Token`LongName`NotSucceedsEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotSucceedsEqual, Precedence`Class`OrderingOperators, NotSucceedsEqual],
Token`LongName`NotPrecedesTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotPrecedesTilde, Precedence`Class`OrderingOperators, NotPrecedesTilde],
Token`LongName`NotSucceedsTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotSucceedsTilde, Precedence`Class`OrderingOperators, NotSucceedsTilde],
Token`LongName`NotPrecedesSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotPrecedesSlantEqual, Precedence`Class`OrderingOperators, NotPrecedesSlantEqual],
Token`LongName`NotSucceedsSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotSucceedsSlantEqual, Precedence`Class`OrderingOperators, NotSucceedsSlantEqual],
Token`LongName`CupCap -> Parselet`InfixOperatorParselet[Token`LongName`CupCap, Precedence`Class`OrderingOperators, CupCap],
Token`LongName`NotCupCap -> Parselet`InfixOperatorParselet[Token`LongName`NotCupCap, Precedence`Class`OrderingOperators, NotCupCap],
Token`LongName`HumpEqual -> Parselet`InfixOperatorParselet[Token`LongName`HumpEqual, Precedence`Class`OrderingOperators, HumpEqual],
Token`LongName`HumpDownHump -> Parselet`InfixOperatorParselet[Token`LongName`HumpDownHump, Precedence`Class`OrderingOperators, HumpDownHump],
Token`LongName`NotHumpEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotHumpEqual, Precedence`Class`OrderingOperators, NotHumpEqual],
Token`LongName`NotHumpDownHump -> Parselet`InfixOperatorParselet[Token`LongName`NotHumpDownHump, Precedence`Class`OrderingOperators, NotHumpDownHump],

(*
special Inequality
*)
Token`BangEqual -> Parselet`InfixOperatorParselet[Token`BangEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`EqualEqual -> Parselet`InfixOperatorParselet[Token`EqualEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`Greater -> Parselet`InfixOperatorParselet[Token`Greater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`GreaterEqual -> Parselet`InfixOperatorParselet[Token`GreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LessEqual -> Parselet`InfixOperatorParselet[Token`LessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`Less -> Parselet`InfixOperatorParselet[Token`Less, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`Equal -> Parselet`InfixOperatorParselet[Token`LongName`Equal, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterEqual -> Parselet`InfixOperatorParselet[Token`LongName`GreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterEqualLess -> Parselet`InfixOperatorParselet[Token`LongName`GreaterEqualLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterFullEqual -> Parselet`InfixOperatorParselet[Token`LongName`GreaterFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterGreater -> Parselet`InfixOperatorParselet[Token`LongName`GreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterLess -> Parselet`InfixOperatorParselet[Token`LongName`GreaterLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`GreaterSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterTilde -> Parselet`InfixOperatorParselet[Token`LongName`GreaterTilde, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessEqual -> Parselet`InfixOperatorParselet[Token`LongName`LessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessEqualGreater -> Parselet`InfixOperatorParselet[Token`LongName`LessEqualGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessFullEqual -> Parselet`InfixOperatorParselet[Token`LongName`LessFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessGreater -> Parselet`InfixOperatorParselet[Token`LongName`LessGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessLess -> Parselet`InfixOperatorParselet[Token`LongName`LessLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`LessSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessTilde -> Parselet`InfixOperatorParselet[Token`LongName`LessTilde, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LongEqual -> Parselet`InfixOperatorParselet[Token`LongName`LongEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NestedGreaterGreater -> Parselet`InfixOperatorParselet[Token`LongName`NestedGreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NestedLessLess -> Parselet`InfixOperatorParselet[Token`LongName`NestedLessLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreater -> Parselet`InfixOperatorParselet[Token`LongName`NotGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotGreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterFullEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotGreaterFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterGreater -> Parselet`InfixOperatorParselet[Token`LongName`NotGreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterLess -> Parselet`InfixOperatorParselet[Token`LongName`NotGreaterLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotGreaterSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotGreaterTilde, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLess -> Parselet`InfixOperatorParselet[Token`LongName`NotLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotLessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessFullEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotLessFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessGreater -> Parselet`InfixOperatorParselet[Token`LongName`NotLessGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessLess -> Parselet`InfixOperatorParselet[Token`LongName`NotLessLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessSlantEqual -> Parselet`InfixOperatorParselet[Token`LongName`NotLessSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessTilde -> Parselet`InfixOperatorParselet[Token`LongName`NotLessTilde, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotNestedGreaterGreater -> Parselet`InfixOperatorParselet[Token`LongName`NotNestedGreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotNestedLessLess -> Parselet`InfixOperatorParselet[Token`LongName`NotNestedLessLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
(*
special VectorInequality
*)
Token`LongName`VectorGreater -> Parselet`InfixOperatorParselet[Token`LongName`VectorGreater, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`VectorGreaterEqual -> Parselet`InfixOperatorParselet[Token`LongName`VectorGreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`VectorLess -> Parselet`InfixOperatorParselet[Token`LongName`VectorLess, Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`VectorLessEqual -> Parselet`InfixOperatorParselet[Token`LongName`VectorLessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality],


Token`LongName`PermutationProduct -> Parselet`InfixOperatorParselet[Token`LongName`PermutationProduct, Precedence`LongName`PermutationProduct, PermutationProduct],
Token`LongName`Colon -> Parselet`InfixOperatorParselet[Token`LongName`Colon, Precedence`LongName`Colon, Colon],
Token`LongName`Xnor -> Parselet`InfixOperatorParselet[Token`LongName`Xnor, Precedence`LongName`Xnor, Xnor],
Token`LongName`Minus -> Parselet`InfixOperatorParselet[Token`LongName`Minus, Precedence`Infix`LongName`Minus, Plus],

Token`Fake`ImplicitTimes -> Parselet`InfixOperatorParselet[Token`Fake`ImplicitTimes, Precedence`Star, Times],


(*
Postfix
*)
Token`Amp -> Parselet`PostfixOperatorParselet[Token`Amp, Precedence`Amp, Function],
Token`DotDot -> Parselet`PostfixOperatorParselet[Token`DotDot, Precedence`DotDot, Repeated],
Token`Bang -> Parselet`PostfixOperatorParselet[Token`Bang, Precedence`Postfix`Bang, Factorial],
Token`MinusMinus -> Parselet`PostfixOperatorParselet[Token`MinusMinus, Precedence`Postfix`MinusMinus, Decrement],
Token`PlusPlus -> Parselet`PostfixOperatorParselet[Token`PlusPlus, Precedence`Postfix`PlusPlus, Increment],
Token`DotDotDot -> Parselet`PostfixOperatorParselet[Token`DotDotDot, Precedence`DotDotDot, RepeatedNull],
Token`BangBang -> Parselet`PostfixOperatorParselet[Token`BangBang, Precedence`Postfix`BangBang, Factorial2],
Token`SingleQuote -> Parselet`PostfixOperatorParselet[Token`SingleQuote, Precedence`SingleQuote, Derivative],
Token`LongName`Transpose -> Parselet`PostfixOperatorParselet[Token`LongName`Transpose, Precedence`LongName`Transpose, Transpose],
Token`LongName`Conjugate -> Parselet`PostfixOperatorParselet[Token`LongName`Conjugate, Precedence`LongName`Conjugate, Conjugate],
Token`LongName`ConjugateTranspose -> Parselet`PostfixOperatorParselet[Token`LongName`ConjugateTranspose, Precedence`LongName`ConjugateTranspose, ConjugateTranspose],
Token`LongName`HermitianConjugate -> Parselet`PostfixOperatorParselet[Token`LongName`HermitianConjugate, Precedence`LongName`HermitianConjugate, System`HermitianConjugate],
Token`LongName`InvisiblePostfixScriptBase -> Parselet`PostfixOperatorParselet[Token`LongName`InvisiblePostfixScriptBase, Precedence`LongName`InvisiblePostfixScriptBase, System`InvisiblePostfixScriptBase],


(*
Calls
*)
Token`OpenSquare -> Parselet`CallParselet[Parselet`GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]],
Token`LongName`LeftDoubleBracket -> Parselet`CallParselet[Parselet`GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]],




(*
trailing ; and , is allowed
*)
Token`Semi -> Parselet`SemiParselet[],

Token`Comma -> Parselet`CommaParselet[],
Token`LongName`InvisibleComma -> Parselet`CommaParselet[],

(*
prefix, infix, postfix
*)
Token`SemiSemi -> Parselet`SemiSemiParselet[],

(*
ternary
*)
Token`Tilde -> Parselet`TildeParselet[],

(*
context sensitive parsing of sym:obj and pat:v
*)
Token`Colon -> Parselet`ColonParselet[],

(*
ternary, with different possibilities for second operator
*)
Token`SlashColon -> Parselet`SlashColonParselet[],

(*
Has to handle  a =.  and  a = .
*)
Token`Equal -> Parselet`EqualParselet[],
Token`ColonEqual -> Parselet`ColonEqualParselet[],

(*
stringify next token (as a symbol)
*)
Token`ColonColon -> Parselet`ColonColonParselet[],

(*
stringify next token (as a file)
*)
Token`GreaterGreater -> Parselet`GreaterGreaterParselet[],
Token`GreaterGreaterGreater -> Parselet`GreaterGreaterGreaterParselet[],


Token`QuestionQuestion -> Parselet`InfixAssertFalseParselet[],

(*
Also use for operators that are only valid in StandardForm.
e.g., \[Gradient] does not have an interpretation in InputForm

\[Gradient] is not letterlike, so it needs some kind of categorization,
but it also needs to be prevented from making any valid parses.
*)
Token`LongName`Gradient -> Parselet`InfixAssertFalseParselet[],
Token`LongName`Divergence -> Parselet`InfixAssertFalseParselet[],
Token`LongName`Curl -> Parselet`InfixAssertFalseParselet[],
Token`LongName`Limit -> Parselet`InfixAssertFalseParselet[],
Token`LongName`MaxLimit -> Parselet`InfixAssertFalseParselet[],
Token`LongName`MinLimit -> Parselet`InfixAssertFalseParselet[],
Token`LongName`AutoLeftMatch -> Parselet`InfixAssertFalseParselet[],
Token`LongName`AutoRightMatch -> Parselet`InfixAssertFalseParselet[],
Token`LongName`DiscreteShift -> Parselet`InfixAssertFalseParselet[],
Token`LongName`DifferenceDelta -> Parselet`InfixAssertFalseParselet[],
Token`LongName`DiscreteRatio -> Parselet`InfixAssertFalseParselet[],
Token`LongName`Laplacian -> Parselet`InfixAssertFalseParselet[],
Token`LongName`PartialD -> Parselet`InfixAssertFalseParselet[]

|>
