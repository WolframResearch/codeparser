
(*
CodeParser Data file

Do not modify this file directly
*)

<|

Token`Unknown -> Parselet`InfixAssertFalseParselet[],
Token`Whitespace -> Parselet`InfixAssertFalseParselet[],
Token`InternalNewline -> Parselet`InfixAssertFalseParselet[],
Token`Comment -> Parselet`InfixAssertFalseParselet[],

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
Token`Error`UnexpectedCommentCloser -> Parselet`InfixAssertFalseParselet[],

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

Token`Slash -> Parselet`BinaryOperatorParselet[Precedence`Slash, Divide],
Token`Caret -> Parselet`BinaryOperatorParselet[Precedence`Caret, Power],
Token`CaretEqual -> Parselet`BinaryOperatorParselet[Precedence`CaretEqual, UpSet],
Token`CaretColonEqual -> Parselet`BinaryOperatorParselet[Precedence`CaretColonEqual, UpSetDelayed],
Token`SlashAt -> Parselet`BinaryOperatorParselet[Precedence`SlashAt, Map],
Token`MinusGreater -> Parselet`BinaryOperatorParselet[Precedence`MinusGreater, Rule],
Token`AtAt -> Parselet`BinaryOperatorParselet[Precedence`AtAt, Apply],
Token`SlashSemi -> Parselet`BinaryOperatorParselet[Precedence`SlashSemi, Condition],
Token`SlashDot -> Parselet`BinaryOperatorParselet[Precedence`SlashDot, ReplaceAll],
Token`ColonGreater -> Parselet`BinaryOperatorParselet[Precedence`ColonGreater, RuleDelayed],
Token`SlashSlashDot -> Parselet`BinaryOperatorParselet[Precedence`SlashSlashDot, ReplaceRepeated],
Token`PlusEqual -> Parselet`BinaryOperatorParselet[Precedence`PlusEqual, AddTo],
Token`StarEqual -> Parselet`BinaryOperatorParselet[Precedence`StarEqual, TimesBy],
Token`MinusEqual -> Parselet`BinaryOperatorParselet[Precedence`MinusEqual, SubtractFrom],
Token`SlashEqual -> Parselet`BinaryOperatorParselet[Precedence`SlashEqual, DivideBy],
Token`LessMinusGreater -> Parselet`BinaryOperatorParselet[Precedence`LessMinusGreater, System`TwoWayRule],
Token`SlashSlashAt -> Parselet`BinaryOperatorParselet[Precedence`SlashSlashAt, MapAll],
Token`At -> Parselet`BinaryOperatorParselet[Precedence`At, CodeParser`BinaryAt],
Token`AtAtAt -> Parselet`BinaryOperatorParselet[Precedence`AtAtAt, System`MapApply],
Token`SlashSlash -> Parselet`BinaryOperatorParselet[Precedence`SlashSlash, CodeParser`BinarySlashSlash],
Token`Question -> Parselet`BinaryOperatorParselet[Precedence`Infix`Question, PatternTest],
Token`BarMinusGreater -> Parselet`BinaryOperatorParselet[Precedence`BarMinusGreater, Function],
Token`SlashSlashEqual -> Parselet`BinaryOperatorParselet[Precedence`SlashSlashEqual, System`ApplyTo],

Token`LongName`Divide -> Parselet`BinaryOperatorParselet[Precedence`LongName`Divide, Divide],
Token`LongName`DivisionSlash -> Parselet`BinaryOperatorParselet[Precedence`LongName`DivisionSlash, Divide],
Token`LongName`Implies -> Parselet`BinaryOperatorParselet[Precedence`LongName`Implies, Implies],
Token`LongName`RoundImplies -> Parselet`BinaryOperatorParselet[Precedence`LongName`RoundImplies, RoundImplies],
Token`LongName`PlusMinus -> Parselet`BinaryOperatorParselet[Precedence`Infix`LongName`PlusMinus, PlusMinus],
Token`LongName`DirectedEdge -> Parselet`BinaryOperatorParselet[Precedence`LongName`DirectedEdge, DirectedEdge],
Token`LongName`Rule -> Parselet`BinaryOperatorParselet[Precedence`LongName`Rule, Rule],
Token`LongName`RuleDelayed -> Parselet`BinaryOperatorParselet[Precedence`LongName`RuleDelayed, RuleDelayed],
Token`LongName`UndirectedEdge -> Parselet`BinaryOperatorParselet[Precedence`LongName`UndirectedEdge, UndirectedEdge],
Token`LongName`Function -> Parselet`BinaryOperatorParselet[Precedence`LongName`Function, Function],
Token`LongName`MinusPlus -> Parselet`BinaryOperatorParselet[Precedence`Infix`LongName`MinusPlus, MinusPlus],
Token`LongName`TwoWayRule -> Parselet`BinaryOperatorParselet[Precedence`LongName`TwoWayRule, System`TwoWayRule],
Token`LongName`InvisibleApplication -> Parselet`BinaryOperatorParselet[Precedence`LongName`InvisibleApplication, CodeParser`BinaryAt],
Token`LongName`CircleMinus -> Parselet`BinaryOperatorParselet[Precedence`LongName`CircleMinus, CircleMinus],
Token`LongName`SuchThat -> Parselet`BinaryOperatorParselet[Precedence`LongName`SuchThat, SuchThat],
Token`LongName`Perpendicular -> Parselet`BinaryOperatorParselet[Precedence`LongName`Perpendicular, Perpendicular],
Token`LongName`Because -> Parselet`BinaryOperatorParselet[Precedence`LongName`Because, Because],
Token`LongName`Therefore -> Parselet`BinaryOperatorParselet[Precedence`LongName`Therefore, Therefore],
Token`LongName`RightTee -> Parselet`BinaryOperatorParselet[Precedence`LongName`RightTee, RightTee],
Token`LongName`LeftTee -> Parselet`BinaryOperatorParselet[Precedence`LongName`LeftTee, LeftTee],
Token`LongName`DoubleRightTee -> Parselet`BinaryOperatorParselet[Precedence`LongName`DoubleRightTee, DoubleRightTee],
Token`LongName`DoubleLeftTee -> Parselet`BinaryOperatorParselet[Precedence`LongName`DoubleLeftTee, DoubleLeftTee],
Token`LongName`UpTee -> Parselet`BinaryOperatorParselet[Precedence`LongName`UpTee, UpTee],
Token`LongName`DownTee -> Parselet`BinaryOperatorParselet[Precedence`LongName`DownTee, DownTee],
Token`LongName`Application -> Parselet`BinaryOperatorParselet[Precedence`LongName`Application, System`Application],


(*
Infix

Note that these are the operators that make sense to be infix in WL source code.

These may not necessarily correspond to Flat functions in WL.
*)
Token`Minus -> Parselet`InfixOperatorParselet[Precedence`Infix`Minus, Plus],
Token`EqualEqualEqual -> Parselet`InfixOperatorParselet[Precedence`EqualEqualEqual, SameQ],
Token`EqualBangEqual -> Parselet`InfixOperatorParselet[Precedence`EqualBangEqual, UnsameQ],
Token`Plus -> Parselet`InfixOperatorParselet[Precedence`Infix`Plus, Plus],
Token`Dot -> Parselet`InfixOperatorParselet[Precedence`Dot, Dot],
Token`StarStar -> Parselet`InfixOperatorParselet[Precedence`StarStar, NonCommutativeMultiply],
Token`AmpAmp -> Parselet`InfixOperatorParselet[Precedence`AmpAmp, And],
Token`BarBar -> Parselet`InfixOperatorParselet[Precedence`BarBar, Or],
Token`Bar -> Parselet`InfixOperatorParselet[Precedence`Bar, Alternatives],
Token`LessGreater -> Parselet`InfixOperatorParselet[Precedence`LessGreater, StringJoin],
Token`TildeTilde -> Parselet`InfixOperatorParselet[Precedence`TildeTilde, StringExpression],
Token`AtStar -> Parselet`InfixOperatorParselet[Precedence`AtStar, Composition],
Token`SlashStar -> Parselet`InfixOperatorParselet[Precedence`SlashStar, RightComposition],

(*
Times
*)
Token`Star -> Parselet`TimesParselet[],
Token`LongName`Times -> Parselet`TimesParselet[],
Token`LongName`InvisibleTimes -> Parselet`TimesParselet[],
Token`Fake`ImplicitTimes -> Parselet`TimesParselet[],


(*
Set relations
*)
Token`LongName`Element -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, Element],
Token`LongName`Subset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, Subset],
Token`LongName`Superset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, Superset],
Token`LongName`SubsetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, SubsetEqual],
Token`LongName`SupersetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, SupersetEqual],
Token`LongName`NotElement -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotElement],
Token`LongName`NotSubset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSubset],
Token`LongName`NotSuperset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSuperset],
Token`LongName`NotSubsetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSubsetEqual],
Token`LongName`NotSupersetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSupersetEqual],
Token`LongName`SquareSubset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, SquareSubset],
Token`LongName`SquareSuperset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, SquareSuperset],
Token`LongName`NotSquareSubset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSquareSubset],
Token`LongName`NotSquareSuperset -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSquareSuperset],
Token`LongName`SquareSubsetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, SquareSubsetEqual],
Token`LongName`SquareSupersetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, SquareSupersetEqual],
Token`LongName`NotSquareSubsetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSquareSubsetEqual],
Token`LongName`NotSquareSupersetEqual -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotSquareSupersetEqual],
Token`LongName`ReverseElement -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, ReverseElement],
Token`LongName`NotReverseElement -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, NotReverseElement],
Token`LongName`Distributed -> Parselet`InfixOperatorParselet[Precedence`Class`SetRelations, Distributed],

Token`LongName`ImplicitPlus -> Parselet`InfixOperatorParselet[Precedence`LongName`ImplicitPlus, Plus],
Token`LongName`And -> Parselet`InfixOperatorParselet[Precedence`LongName`And, And],
Token`LongName`Or -> Parselet`InfixOperatorParselet[Precedence`LongName`Or, Or],
Token`LongName`Xor -> Parselet`InfixOperatorParselet[Precedence`LongName`Xor, Xor],
Token`LongName`Nand -> Parselet`InfixOperatorParselet[Precedence`LongName`Nand, Nand],
Token`LongName`Nor -> Parselet`InfixOperatorParselet[Precedence`LongName`Nor, Nor],

(*
Horizontal arrows
*)
Token`LongName`LeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, LeftArrow],
Token`LongName`RightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, RightArrow],
Token`LongName`LeftRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, LeftRightArrow],
Token`LongName`LeftTeeArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, LeftTeeArrow],
Token`LongName`RightTeeArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, RightTeeArrow],
Token`LongName`RightArrowLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, RightArrowLeftArrow],
Token`LongName`LeftArrowRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, LeftArrowRightArrow],
Token`LongName`DoubleLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, DoubleLeftArrow],
Token`LongName`DoubleRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, DoubleRightArrow],
Token`LongName`DoubleLeftRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, DoubleLeftRightArrow],
Token`LongName`LeftArrowBar -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, LeftArrowBar],
Token`LongName`RightArrowBar -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, RightArrowBar],
Token`LongName`ShortRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, ShortRightArrow],
Token`LongName`ShortLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`HorizontalArrows, ShortLeftArrow],

(*
Diagonal arrow operators
*)
Token`LongName`UpperLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`DiagonalArrowOperators, UpperLeftArrow],
Token`LongName`UpperRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`DiagonalArrowOperators, UpperRightArrow],
Token`LongName`LowerRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`DiagonalArrowOperators, LowerRightArrow],
Token`LongName`LowerLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`DiagonalArrowOperators, LowerLeftArrow],

(*
Vector operators
*)
Token`LongName`LeftVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, LeftVector],
Token`LongName`RightVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, RightVector],
Token`LongName`LeftRightVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, LeftRightVector],
Token`LongName`LeftVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, LeftVectorBar],
Token`LongName`RightVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, RightVectorBar],
Token`LongName`LeftTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, LeftTeeVector],
Token`LongName`RightTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, RightTeeVector],
Token`LongName`DownLeftVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownLeftVector],
Token`LongName`DownRightVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownRightVector],
Token`LongName`DownLeftRightVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownLeftRightVector],
Token`LongName`DownLeftVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownLeftVectorBar],
Token`LongName`DownRightVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownRightVectorBar],
Token`LongName`DownLeftTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownLeftTeeVector],
Token`LongName`DownRightTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VectorOperators, DownRightTeeVector],

(*
Vertical arrow operators
*)
Token`LongName`UpArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, UpArrow],
Token`LongName`DownArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DownArrow],
Token`LongName`UpDownArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, UpDownArrow],
Token`LongName`UpTeeArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, UpTeeArrow],
Token`LongName`DownTeeArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DownTeeArrow],
Token`LongName`UpArrowDownArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, UpArrowDownArrow],
Token`LongName`DoubleUpArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DoubleUpArrow],
Token`LongName`DoubleDownArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DoubleDownArrow],
Token`LongName`DoubleUpDownArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DoubleUpDownArrow],
Token`LongName`DownArrowUpArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DownArrowUpArrow],
(*
itai asking about precedence of "long" arrows:
https://mail-archive.wolfram.com/archive/l-typeset/2021/Jul00/0000.html
*)
Token`LongName`LongLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, LongLeftArrow],
Token`LongName`LongRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, LongRightArrow],
Token`LongName`LongLeftRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, LongLeftRightArrow],
Token`LongName`DoubleLongLeftArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DoubleLongLeftArrow],
Token`LongName`DoubleLongRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DoubleLongRightArrow],
Token`LongName`DoubleLongLeftRightArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DoubleLongLeftRightArrow],
Token`LongName`UpArrowBar -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, UpArrowBar],
Token`LongName`DownArrowBar -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, DownArrowBar],
Token`LongName`ShortUpArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, ShortUpArrow],
Token`LongName`ShortDownArrow -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalArrowOperators, ShortDownArrow],


(*
Vertical vector operators
*)
Token`LongName`RightUpVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightUpVector],
Token`LongName`LeftUpVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftUpVector],
Token`LongName`RightDownVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightDownVector],
Token`LongName`LeftDownVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftDownVector],
Token`LongName`RightUpDownVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightUpDownVector],
Token`LongName`LeftUpDownVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftUpDownVector],
Token`LongName`RightUpVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightUpVectorBar],
Token`LongName`RightDownVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightDownVectorBar],
Token`LongName`LeftUpVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftUpVectorBar],
Token`LongName`LeftDownVectorBar -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftDownVectorBar],
Token`LongName`RightUpTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightUpTeeVector],
Token`LongName`RightDownTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, RightDownTeeVector],
Token`LongName`LeftUpTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftUpTeeVector],
Token`LongName`LeftDownTeeVector -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, LeftDownTeeVector],
Token`LongName`UpEquilibrium -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, UpEquilibrium],
Token`LongName`ReverseUpEquilibrium -> Parselet`InfixOperatorParselet[Precedence`Class`VerticalVectorOperators, ReverseUpEquilibrium],


Token`LongName`CenterDot -> Parselet`InfixOperatorParselet[Precedence`LongName`CenterDot, CenterDot],
Token`LongName`Equivalent -> Parselet`InfixOperatorParselet[Precedence`LongName`Equivalent, Equivalent],
Token`LongName`CircleDot -> Parselet`InfixOperatorParselet[Precedence`LongName`CircleDot, CircleDot],
Token`LongName`Conditioned -> Parselet`InfixOperatorParselet[Precedence`LongName`Conditioned, Conditioned],

(*
Union operators
*)
Token`LongName`Union -> Parselet`InfixOperatorParselet[Precedence`Class`UnionOperators, Union],
Token`LongName`SquareUnion -> Parselet`InfixOperatorParselet[Precedence`Class`UnionOperators, SquareUnion],
Token`LongName`UnionPlus -> Parselet`InfixOperatorParselet[Precedence`Class`UnionOperators, UnionPlus],

(*
Intersection operators
*)
Token`LongName`Intersection -> Parselet`InfixOperatorParselet[Precedence`Class`IntersectionOperators, Intersection],
Token`LongName`SquareIntersection -> Parselet`InfixOperatorParselet[Precedence`Class`IntersectionOperators, SquareIntersection],


Token`LongName`TensorWedge -> Parselet`InfixOperatorParselet[Precedence`LongName`TensorWedge, TensorWedge],
Token`LongName`TensorProduct -> Parselet`InfixOperatorParselet[Precedence`LongName`TensorProduct, TensorProduct],
Token`LongName`Cross -> Parselet`InfixOperatorParselet[Precedence`LongName`Cross, Cross],
Token`LongName`SmallCircle -> Parselet`InfixOperatorParselet[Precedence`LongName`SmallCircle, SmallCircle],
Token`LongName`Divides -> Parselet`InfixOperatorParselet[Precedence`LongName`Divides, Divisible],
Token`LongName`VerticalSeparator -> Parselet`InfixOperatorParselet[Precedence`LongName`VerticalSeparator, VerticalSeparator],
Token`LongName`Backslash -> Parselet`InfixOperatorParselet[Precedence`LongName`Backslash, Backslash],
Token`LongName`Diamond -> Parselet`InfixOperatorParselet[Precedence`LongName`Diamond, Diamond],
Token`LongName`Wedge -> Parselet`InfixOperatorParselet[Precedence`LongName`Wedge, Wedge],
Token`LongName`Vee -> Parselet`InfixOperatorParselet[Precedence`LongName`Vee, Vee],
Token`LongName`CircleTimes -> Parselet`InfixOperatorParselet[Precedence`Infix`LongName`CircleTimes, CircleTimes],
Token`LongName`Star -> Parselet`InfixOperatorParselet[Precedence`LongName`Star, Star],
Token`LongName`VerticalTilde -> Parselet`InfixOperatorParselet[Precedence`LongName`VerticalTilde, VerticalTilde],
Token`LongName`Coproduct -> Parselet`InfixOperatorParselet[Precedence`Infix`LongName`Coproduct, Coproduct],
Token`LongName`Cap -> Parselet`InfixOperatorParselet[Precedence`LongName`Cap, Cap],
Token`LongName`Cup -> Parselet`InfixOperatorParselet[Precedence`LongName`Cup, Cup],
Token`LongName`CirclePlus -> Parselet`InfixOperatorParselet[Precedence`LongName`CirclePlus, CirclePlus],
Token`LongName`VerticalBar -> Parselet`InfixOperatorParselet[Precedence`LongName`VerticalBar, VerticalBar],
Token`LongName`DoubleVerticalBar -> Parselet`InfixOperatorParselet[Precedence`LongName`DoubleVerticalBar, DoubleVerticalBar],
Token`LongName`NotVerticalBar -> Parselet`InfixOperatorParselet[Precedence`LongName`NotVerticalBar, NotVerticalBar],
Token`LongName`NotDoubleVerticalBar -> Parselet`InfixOperatorParselet[Precedence`LongName`NotDoubleVerticalBar, NotDoubleVerticalBar],

(*
Ordering operators
*)
Token`LongName`LeftTriangle -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, LeftTriangle],
Token`LongName`RightTriangle -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, RightTriangle],
Token`LongName`NotLeftTriangle -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotLeftTriangle],
Token`LongName`NotRightTriangle -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotRightTriangle],
Token`LongName`LeftTriangleEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, LeftTriangleEqual],
Token`LongName`RightTriangleEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, RightTriangleEqual],
Token`LongName`NotLeftTriangleEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotLeftTriangleEqual],
Token`LongName`NotRightTriangleEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotRightTriangleEqual],
Token`LongName`LeftTriangleBar -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, LeftTriangleBar],
Token`LongName`RightTriangleBar -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, RightTriangleBar],
Token`LongName`NotLeftTriangleBar -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotLeftTriangleBar],
Token`LongName`NotRightTriangleBar -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotRightTriangleBar],
Token`LongName`TildeEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, TildeEqual],
Token`LongName`NotTildeEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotTildeEqual],
Token`LongName`TildeFullEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, TildeFullEqual],
Token`LongName`NotTildeFullEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotTildeFullEqual],
Token`LongName`Tilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Tilde],
Token`LongName`NotTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotTilde],
Token`LongName`EqualTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, EqualTilde],
Token`LongName`NotEqualTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotEqualTilde],
Token`LongName`TildeTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, TildeTilde],
Token`LongName`NotTildeTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotTildeTilde],
Token`LongName`Proportional -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Proportional],
Token`LongName`Proportion -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Proportion],
Token`LongName`Congruent -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Congruent],
Token`LongName`NotCongruent -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotCongruent],
Token`LongName`Equilibrium -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Equilibrium],
Token`LongName`ReverseEquilibrium -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, ReverseEquilibrium],
Token`LongName`DotEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, DotEqual],
Token`LongName`Precedes -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Precedes],
Token`LongName`Succeeds -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, Succeeds],
Token`LongName`PrecedesEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, PrecedesEqual],
Token`LongName`SucceedsEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, SucceedsEqual],
Token`LongName`PrecedesTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, PrecedesTilde],
Token`LongName`SucceedsTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, SucceedsTilde],
Token`LongName`PrecedesSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, PrecedesSlantEqual],
Token`LongName`SucceedsSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, SucceedsSlantEqual],
Token`LongName`NotPrecedes -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotPrecedes],
Token`LongName`NotSucceeds -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotSucceeds],
Token`LongName`NotPrecedesEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotPrecedesEqual],
Token`LongName`NotSucceedsEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotSucceedsEqual],
Token`LongName`NotPrecedesTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotPrecedesTilde],
Token`LongName`NotSucceedsTilde -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotSucceedsTilde],
Token`LongName`NotPrecedesSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotPrecedesSlantEqual],
Token`LongName`NotSucceedsSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotSucceedsSlantEqual],
Token`LongName`CupCap -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, CupCap],
Token`LongName`NotCupCap -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotCupCap],
Token`LongName`HumpEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, HumpEqual],
Token`LongName`HumpDownHump -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, HumpDownHump],
Token`LongName`NotHumpEqual -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotHumpEqual],
Token`LongName`NotHumpDownHump -> Parselet`InfixOperatorParselet[Precedence`Class`OrderingOperators, NotHumpDownHump],

(*
special Inequality
*)
Token`BangEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`EqualEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`Greater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`GreaterEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LessEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`Less -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`Equal -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterEqualLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterFullEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`GreaterTilde -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessEqualGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessFullEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LessTilde -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`LongEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NestedGreaterGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NestedLessLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterFullEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotGreaterTilde -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessFullEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessSlantEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotLessTilde -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotNestedGreaterGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`NotNestedLessLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
(*
special VectorInequality
*)
Token`LongName`VectorGreater -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`VectorGreaterEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`VectorLess -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],
Token`LongName`VectorLessEqual -> Parselet`InfixOperatorParselet[Precedence`Class`Inequality, CodeParser`InfixInequality],


Token`LongName`PermutationProduct -> Parselet`InfixOperatorParselet[Precedence`LongName`PermutationProduct, PermutationProduct],
Token`LongName`Colon -> Parselet`InfixOperatorParselet[Precedence`LongName`Colon, Colon],
Token`LongName`Xnor -> Parselet`InfixOperatorParselet[Precedence`LongName`Xnor, Xnor],
Token`LongName`Minus -> Parselet`InfixOperatorParselet[Precedence`Infix`LongName`Minus, Plus],


(*
Postfix
*)
Token`Amp -> Parselet`PostfixOperatorParselet[Precedence`Amp, Function],
Token`DotDot -> Parselet`PostfixOperatorParselet[Precedence`DotDot, Repeated],
Token`Bang -> Parselet`PostfixOperatorParselet[Precedence`Postfix`Bang, Factorial],
Token`MinusMinus -> Parselet`PostfixOperatorParselet[Precedence`Postfix`MinusMinus, Decrement],
Token`PlusPlus -> Parselet`PostfixOperatorParselet[Precedence`Postfix`PlusPlus, Increment],
Token`DotDotDot -> Parselet`PostfixOperatorParselet[Precedence`DotDotDot, RepeatedNull],
Token`BangBang -> Parselet`PostfixOperatorParselet[Precedence`Postfix`BangBang, Factorial2],
Token`SingleQuote -> Parselet`PostfixOperatorParselet[Precedence`SingleQuote, Derivative],
Token`LongName`Transpose -> Parselet`PostfixOperatorParselet[Precedence`LongName`Transpose, Transpose],
Token`LongName`Conjugate -> Parselet`PostfixOperatorParselet[Precedence`LongName`Conjugate, Conjugate],
Token`LongName`ConjugateTranspose -> Parselet`PostfixOperatorParselet[Precedence`LongName`ConjugateTranspose, ConjugateTranspose],
Token`LongName`HermitianConjugate -> Parselet`PostfixOperatorParselet[Precedence`LongName`HermitianConjugate, System`HermitianConjugate],
Token`LongName`InvisiblePostfixScriptBase -> Parselet`PostfixOperatorParselet[Precedence`LongName`InvisiblePostfixScriptBase, System`InvisiblePostfixScriptBase],


(*
Calls
*)
Token`OpenSquare -> Parselet`CallParselet[Parselet`GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]],
Token`LongName`LeftDoubleBracket -> Parselet`CallParselet[Parselet`GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]],
Token`ColonColonOpenSquare -> Parselet`CallParselet[Parselet`GroupParselet[Token`ColonColonOpenSquare, CodeParser`GroupTypeSpecifier]],




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
