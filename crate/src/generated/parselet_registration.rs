
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(non_upper_case_globals)]

use crate::{
	token_enum_registration::TokenEnum::*,
	symbol_registration::*,
	precedence::*,
	parselet::{*}
};

pub(crate) const symbolParselet: SymbolParselet = SymbolParselet {};

pub(crate) const leafParselet: LeafParselet = LeafParselet {};

pub(crate) const prefixEndOfFileParselet: PrefixEndOfFileParselet = PrefixEndOfFileParselet {};

pub(crate) const prefixErrorParselet: PrefixErrorParselet = PrefixErrorParselet {};

pub(crate) const prefixCloserParselet: PrefixCloserParselet = PrefixCloserParselet {};

pub(crate) const prefixUnsupportedTokenParselet: PrefixUnsupportedTokenParselet = PrefixUnsupportedTokenParselet {};

pub(crate) const prefixUnhandledParselet: PrefixUnhandledParselet = PrefixUnhandledParselet {};

pub(crate) const prefixCommaParselet: PrefixCommaParselet = PrefixCommaParselet {};

pub(crate) const infixAssertFalseParselet: InfixAssertFalseParselet = InfixAssertFalseParselet {};

pub(crate) const infixImplicitTimesParselet: InfixImplicitTimesParselet = InfixImplicitTimesParselet {};

pub(crate) const commaParselet: CommaParselet = CommaParselet {};

pub(crate) const semiParselet: SemiParselet = SemiParselet {};

pub(crate) const semiSemiParselet: SemiSemiParselet = SemiSemiParselet {};

pub(crate) const slashColonParselet: SlashColonParselet = SlashColonParselet {};

pub(crate) const colonParselet: ColonParselet = ColonParselet {};

pub(crate) const equalParselet: EqualParselet = EqualParselet::new();

pub(crate) const colonEqualParselet: ColonEqualParselet = ColonEqualParselet::new();

pub(crate) const infixDifferentialDParselet: InfixDifferentialDParselet = InfixDifferentialDParselet {};

pub(crate) const under1Parselet: UnderParselet = UnderParselet::new(SYMBOL_BLANK, SYMBOL_CODEPARSER_PATTERNBLANK);
pub(crate) const under2Parselet: UnderParselet = UnderParselet::new(SYMBOL_BLANKSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE);
pub(crate) const under3Parselet: UnderParselet = UnderParselet::new(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE);

pub(crate) const underDotParselet: UnderDotParselet = UnderDotParselet {};

pub(crate) const squareGroupParselet: GroupParselet = GroupParselet::new(TOKEN_OPENSQUARE, SYMBOL_CODEPARSER_GROUPSQUARE);

pub(crate) const doubleBracketGroupParselet: GroupParselet = GroupParselet::new(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET);

pub(crate) const timesParselet: TimesParselet = TimesParselet {};

//
//
//

pub(crate) const PREFIX_PARSELETS: [PrefixParseletPtr; TOKEN_COUNT.value() as usize] = [
  &prefixErrorParselet, // Token`Unknown
  &prefixEndOfFileParselet, // Token`EndOfFile
  &symbolParselet, // Token`Symbol
  &leafParselet, // Token`String
  &leafParselet, // Token`Integer
  &leafParselet, // Token`Real
  &leafParselet, // Token`Rational
  &leafParselet, // Token`LinearSyntaxBlob
  &prefixErrorParselet, // Token`InternalNewline
  &prefixErrorParselet, // Token`Comment
  &prefixErrorParselet, // Token`Whitespace
  &prefixUnhandledParselet, // Token`Buffer1
  &prefixUnhandledParselet, // Token`ToplevelNewline
  &prefixUnhandledParselet, // Token`Buffer2
  &prefixUnhandledParselet, // Token`Buffer3
  &prefixUnhandledParselet, // Token`Buffer4
  &prefixErrorParselet, // Token`Error`ExpectedEqual
  &prefixErrorParselet, // Token`Error`Number
  &prefixErrorParselet, // Token`Error`UnhandledCharacter
  &prefixErrorParselet, // Token`Error`ExpectedLetterlike
  &prefixErrorParselet, // Token`Error`Aborted
  &prefixErrorParselet, // Token`Error`ExpectedOperand
  &prefixErrorParselet, // Token`Error`ExpectedTag
  &prefixErrorParselet, // Token`Error`ExpectedFile
  &prefixErrorParselet, // Token`Error`UnexpectedCloser
  &prefixUnhandledParselet, // Token`Error`PrefixImplicitNull
  &prefixUnhandledParselet, // Token`Error`InfixImplicitNull
  &prefixErrorParselet, // Token`Error`UnsafeCharacterEncoding
  &prefixErrorParselet, // Token`Error`UnterminatedComment
  &prefixErrorParselet, // Token`Error`UnterminatedString
  &prefixErrorParselet, // Token`Error`UnterminatedFileString
  &prefixErrorParselet, // Token`Error`UnterminatedLinearSyntaxBlob
  &prefixErrorParselet, // Token`Error`UnsupportedToken
  &prefixErrorParselet, // Token`Error`UnexpectedCommentCloser
  &prefixUnhandledParselet, // Token`Error`End
  &prefixUnhandledParselet, // Token`Dot
  &prefixUnhandledParselet, // Token`Colon
  &GroupParselet::new(TOKEN_OPENPAREN, SYMBOL_CODEPARSER_GROUPPAREN), // Token`OpenParen
  &prefixCloserParselet, // Token`CloseParen
  &squareGroupParselet, // Token`OpenSquare
  &prefixCloserParselet, // Token`CloseSquare
  &prefixCommaParselet, // Token`Comma
  &GroupParselet::new(TOKEN_OPENCURLY, SYMBOL_LIST), // Token`OpenCurly
  &prefixCloserParselet, // Token`CloseCurly
  &prefixUnhandledParselet, // Token`Equal
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_BANG, SYMBOL_NOT), // Token`Bang
  &under1Parselet, // Token`Under
  &prefixUnhandledParselet, // Token`Less
  &prefixUnhandledParselet, // Token`Greater
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_MINUS, SYMBOL_MINUS), // Token`Minus
  &prefixUnhandledParselet, // Token`Bar
  &prefixUnhandledParselet, // Token`Semi
  &HashParselet {}, // Token`Hash
  &prefixUnhandledParselet, // Token`Amp
  &prefixUnhandledParselet, // Token`Slash
  &prefixUnhandledParselet, // Token`At
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_PLUS, SYMBOL_PLUS), // Token`Plus
  &prefixUnhandledParselet, // Token`Tilde
  &prefixUnhandledParselet, // Token`Star
  &prefixUnhandledParselet, // Token`Caret
  &prefixUnhandledParselet, // Token`SingleQuote
  &PercentParselet {}, // Token`Percent
  &prefixUnhandledParselet, // Token`Question
  &prefixUnhandledParselet, // Token`DotDot
  &prefixUnhandledParselet, // Token`ColonColon
  &prefixUnhandledParselet, // Token`ColonEqual
  &prefixUnhandledParselet, // Token`ColonGreater
  &prefixUnhandledParselet, // Token`EqualEqual
  &under2Parselet, // Token`UnderUnder
  &underDotParselet, // Token`UnderDot
  &GroupParselet::new(TOKEN_LESSBAR, SYMBOL_ASSOCIATION), // Token`LessBar
  &(LessLessParselet {}), // Token`LessLess
  &prefixUnhandledParselet, // Token`LessGreater
  &prefixUnhandledParselet, // Token`LessEqual
  &prefixUnhandledParselet, // Token`GreaterGreater
  &prefixUnhandledParselet, // Token`GreaterEqual
  &prefixUnhandledParselet, // Token`MinusGreater
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_MINUSMINUS, SYMBOL_PREDECREMENT), // Token`MinusMinus
  &prefixUnhandledParselet, // Token`MinusEqual
  &prefixUnhandledParselet, // Token`BarBar
  &prefixCloserParselet, // Token`BarGreater
  &semiSemiParselet, // Token`SemiSemi
  &prefixUnhandledParselet, // Token`AmpAmp
  &prefixUnhandledParselet, // Token`SlashAt
  &prefixUnhandledParselet, // Token`SlashSemi
  &prefixUnhandledParselet, // Token`SlashDot
  &prefixUnhandledParselet, // Token`SlashSlash
  &prefixUnhandledParselet, // Token`SlashColon
  &prefixUnhandledParselet, // Token`SlashEqual
  &prefixUnhandledParselet, // Token`SlashStar
  &prefixUnhandledParselet, // Token`AtAt
  &prefixUnhandledParselet, // Token`AtStar
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_PLUSPLUS, SYMBOL_PREINCREMENT), // Token`PlusPlus
  &prefixUnhandledParselet, // Token`PlusEqual
  &prefixUnhandledParselet, // Token`TildeTilde
  &prefixUnhandledParselet, // Token`StarEqual
  &prefixUnhandledParselet, // Token`StarStar
  &prefixUnhandledParselet, // Token`CaretEqual
  &HashHashParselet {}, // Token`HashHash
  &prefixUnhandledParselet, // Token`BangEqual
  &PrefixOperatorParselet::new(PRECEDENCE_FAKE_PREFIX_BANGBANG, SYMBOL_CODEPARSER_PREFIXNOT2), // Token`BangBang
  &prefixUnsupportedTokenParselet, // Token`QuestionQuestion
  &prefixUnhandledParselet, // Token`DotDotDot
  &prefixUnhandledParselet, // Token`EqualEqualEqual
  &prefixUnhandledParselet, // Token`EqualBangEqual
  &under3Parselet, // Token`UnderUnderUnder
  &prefixUnhandledParselet, // Token`SlashSlashDot
  &prefixUnhandledParselet, // Token`AtAtAt
  &prefixUnhandledParselet, // Token`LessMinusGreater
  &prefixUnhandledParselet, // Token`SlashSlashAt
  &prefixUnhandledParselet, // Token`CaretColonEqual
  &prefixUnhandledParselet, // Token`GreaterGreaterGreater
  &prefixUnhandledParselet, // Token`BarMinusGreater
  &prefixUnhandledParselet, // Token`SlashSlashEqual
  &GroupParselet::new(TOKEN_COLONCOLONOPENSQUARE, SYMBOL_CODEPARSER_GROUPTYPESPECIFIER), // Token`ColonColonOpenSquare
  &leafParselet, // Token`PercentPercent
  &PrefixOperatorParselet::new(PRECEDENCE_LINEARSYNTAX_BANG, SYMBOL_CODEPARSER_PREFIXLINEARSYNTAXBANG), // Token`LinearSyntax`Bang
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`CloseParen
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`At
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Amp
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Star
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Under
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Caret
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Space
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Percent
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Plus
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`Slash
  &prefixUnsupportedTokenParselet, // Token`LinearSyntax`BackTick
  &prefixUnhandledParselet, // Token`Fake`ImplicitTimes
  &prefixUnhandledParselet, // Token`Fake`ImplicitNull
  &prefixUnhandledParselet, // Token`Fake`ImplicitOne
  &prefixUnhandledParselet, // Token`Fake`ImplicitAll
  &prefixUnhandledParselet, // Token`Boxes`OpenParenStar
  &prefixUnhandledParselet, // Token`Boxes`StarCloseParen
  &prefixUnhandledParselet, // Token`Boxes`MultiSingleQuote
  &prefixUnhandledParselet, // Token`Boxes`MultiWhitespace
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_NOT, SYMBOL_NOT), // Token`LongName`Not
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS, SYMBOL_PLUSMINUS), // Token`LongName`PlusMinus
  &prefixUnhandledParselet, // Token`LongName`CenterDot
  &prefixUnhandledParselet, // Token`LongName`Times
  &prefixUnhandledParselet, // Token`LongName`Divide
  &GroupParselet::new(TOKEN_LONGNAME_OPENCURLYQUOTE, SYMBOL_CURLYQUOTE), // Token`LongName`OpenCurlyQuote
  &prefixCloserParselet, // Token`LongName`CloseCurlyQuote
  &GroupParselet::new(TOKEN_LONGNAME_OPENCURLYDOUBLEQUOTE, SYMBOL_CURLYDOUBLEQUOTE), // Token`LongName`OpenCurlyDoubleQuote
  &prefixCloserParselet, // Token`LongName`CloseCurlyDoubleQuote
  &prefixUnhandledParselet, // Token`LongName`InvisibleTimes
  &prefixUnhandledParselet, // Token`LongName`LeftArrow
  &prefixUnhandledParselet, // Token`LongName`UpArrow
  &prefixUnhandledParselet, // Token`LongName`RightArrow
  &prefixUnhandledParselet, // Token`LongName`DownArrow
  &prefixUnhandledParselet, // Token`LongName`LeftRightArrow
  &prefixUnhandledParselet, // Token`LongName`UpDownArrow
  &prefixUnhandledParselet, // Token`LongName`UpperLeftArrow
  &prefixUnhandledParselet, // Token`LongName`UpperRightArrow
  &prefixUnhandledParselet, // Token`LongName`LowerRightArrow
  &prefixUnhandledParselet, // Token`LongName`LowerLeftArrow
  &prefixUnhandledParselet, // Token`LongName`LeftTeeArrow
  &prefixUnhandledParselet, // Token`LongName`UpTeeArrow
  &prefixUnhandledParselet, // Token`LongName`RightTeeArrow
  &prefixUnhandledParselet, // Token`LongName`DownTeeArrow
  &prefixUnhandledParselet, // Token`LongName`LeftVector
  &prefixUnhandledParselet, // Token`LongName`DownLeftVector
  &prefixUnhandledParselet, // Token`LongName`RightUpVector
  &prefixUnhandledParselet, // Token`LongName`LeftUpVector
  &prefixUnhandledParselet, // Token`LongName`RightVector
  &prefixUnhandledParselet, // Token`LongName`DownRightVector
  &prefixUnhandledParselet, // Token`LongName`RightDownVector
  &prefixUnhandledParselet, // Token`LongName`LeftDownVector
  &prefixUnhandledParselet, // Token`LongName`RightArrowLeftArrow
  &prefixUnhandledParselet, // Token`LongName`UpArrowDownArrow
  &prefixUnhandledParselet, // Token`LongName`LeftArrowRightArrow
  &prefixUnhandledParselet, // Token`LongName`ReverseEquilibrium
  &prefixUnhandledParselet, // Token`LongName`Equilibrium
  &prefixUnhandledParselet, // Token`LongName`DoubleLeftArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleUpArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleRightArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleDownArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleLeftRightArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleUpDownArrow
  &prefixUnhandledParselet, // Token`LongName`LeftArrowBar
  &prefixUnhandledParselet, // Token`LongName`RightArrowBar
  &prefixUnhandledParselet, // Token`LongName`DownArrowUpArrow
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_FORALL, SYMBOL_FORALL), // Token`LongName`ForAll
  &prefixUnsupportedTokenParselet, // Token`LongName`PartialD
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_EXISTS, SYMBOL_EXISTS), // Token`LongName`Exists
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_NOTEXISTS, SYMBOL_NOTEXISTS), // Token`LongName`NotExists
  &prefixUnsupportedTokenParselet, // Token`LongName`Laplacian
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_DEL, SYMBOL_DEL), // Token`LongName`Del
  &prefixUnhandledParselet, // Token`LongName`Element
  &prefixUnhandledParselet, // Token`LongName`NotElement
  &prefixUnhandledParselet, // Token`LongName`ReverseElement
  &prefixUnhandledParselet, // Token`LongName`NotReverseElement
  &prefixUnhandledParselet, // Token`LongName`SuchThat
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_PRODUCT, SYMBOL_PRODUCT), // Token`LongName`Product
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_COPRODUCT, SYMBOL_COPRODUCT), // Token`LongName`Coproduct
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_SUM, SYMBOL_SUM), // Token`LongName`Sum
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_MINUS, SYMBOL_MINUS), // Token`LongName`Minus
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS, SYMBOL_MINUSPLUS), // Token`LongName`MinusPlus
  &prefixUnhandledParselet, // Token`LongName`DivisionSlash
  &prefixUnhandledParselet, // Token`LongName`Backslash
  &prefixUnhandledParselet, // Token`LongName`SmallCircle
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_SQRT, SYMBOL_SQRT), // Token`LongName`Sqrt
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_CUBEROOT, SYMBOL_CUBEROOT), // Token`LongName`CubeRoot
  &prefixUnhandledParselet, // Token`LongName`Proportional
  &prefixUnhandledParselet, // Token`LongName`Divides
  &prefixUnhandledParselet, // Token`LongName`DoubleVerticalBar
  &prefixUnhandledParselet, // Token`LongName`NotDoubleVerticalBar
  &prefixUnhandledParselet, // Token`LongName`And
  &prefixUnhandledParselet, // Token`LongName`Or
  &IntegralParselet::new(SYMBOL_INTEGRATE, SYMBOL_INTEGRAL), // Token`LongName`Integral
  &IntegralParselet::new(SYMBOL_CONTOURINTEGRAL, SYMBOL_CONTOURINTEGRAL), // Token`LongName`ContourIntegral
  &IntegralParselet::new(SYMBOL_DOUBLECONTOURINTEGRAL, SYMBOL_DOUBLECONTOURINTEGRAL), // Token`LongName`DoubleContourIntegral
  &IntegralParselet::new(SYMBOL_CLOCKWISECONTOURINTEGRAL, SYMBOL_CLOCKWISECONTOURINTEGRAL), // Token`LongName`ClockwiseContourIntegral
  &IntegralParselet::new(SYMBOL_COUNTERCLOCKWISECONTOURINTEGRAL, SYMBOL_COUNTERCLOCKWISECONTOURINTEGRAL), // Token`LongName`CounterClockwiseContourIntegral
  &prefixUnhandledParselet, // Token`LongName`Therefore
  &prefixUnhandledParselet, // Token`LongName`Because
  &prefixUnhandledParselet, // Token`LongName`Colon
  &prefixUnhandledParselet, // Token`LongName`Proportion
  &prefixUnhandledParselet, // Token`LongName`Tilde
  &prefixUnhandledParselet, // Token`LongName`VerticalTilde
  &prefixUnhandledParselet, // Token`LongName`NotTilde
  &prefixUnhandledParselet, // Token`LongName`EqualTilde
  &prefixUnhandledParselet, // Token`LongName`TildeEqual
  &prefixUnhandledParselet, // Token`LongName`NotTildeEqual
  &prefixUnhandledParselet, // Token`LongName`TildeFullEqual
  &prefixUnhandledParselet, // Token`LongName`NotTildeFullEqual
  &prefixUnhandledParselet, // Token`LongName`TildeTilde
  &prefixUnhandledParselet, // Token`LongName`NotTildeTilde
  &prefixUnhandledParselet, // Token`LongName`CupCap
  &prefixUnhandledParselet, // Token`LongName`HumpDownHump
  &prefixUnhandledParselet, // Token`LongName`HumpEqual
  &prefixUnhandledParselet, // Token`LongName`DotEqual
  &prefixUnhandledParselet, // Token`LongName`NotEqual
  &prefixUnhandledParselet, // Token`LongName`Congruent
  &prefixUnhandledParselet, // Token`LongName`NotCongruent
  &prefixUnhandledParselet, // Token`LongName`LessEqual
  &prefixUnhandledParselet, // Token`LongName`GreaterEqual
  &prefixUnhandledParselet, // Token`LongName`LessFullEqual
  &prefixUnhandledParselet, // Token`LongName`GreaterFullEqual
  &prefixUnhandledParselet, // Token`LongName`NotLessFullEqual
  &prefixUnhandledParselet, // Token`LongName`NotGreaterFullEqual
  &prefixUnhandledParselet, // Token`LongName`LessLess
  &prefixUnhandledParselet, // Token`LongName`GreaterGreater
  &prefixUnhandledParselet, // Token`LongName`NotCupCap
  &prefixUnhandledParselet, // Token`LongName`NotLess
  &prefixUnhandledParselet, // Token`LongName`NotGreater
  &prefixUnhandledParselet, // Token`LongName`NotLessEqual
  &prefixUnhandledParselet, // Token`LongName`NotGreaterEqual
  &prefixUnhandledParselet, // Token`LongName`LessTilde
  &prefixUnhandledParselet, // Token`LongName`GreaterTilde
  &prefixUnhandledParselet, // Token`LongName`NotLessTilde
  &prefixUnhandledParselet, // Token`LongName`NotGreaterTilde
  &prefixUnhandledParselet, // Token`LongName`LessGreater
  &prefixUnhandledParselet, // Token`LongName`GreaterLess
  &prefixUnhandledParselet, // Token`LongName`NotLessGreater
  &prefixUnhandledParselet, // Token`LongName`NotGreaterLess
  &prefixUnhandledParselet, // Token`LongName`Precedes
  &prefixUnhandledParselet, // Token`LongName`Succeeds
  &prefixUnhandledParselet, // Token`LongName`PrecedesSlantEqual
  &prefixUnhandledParselet, // Token`LongName`SucceedsSlantEqual
  &prefixUnhandledParselet, // Token`LongName`PrecedesTilde
  &prefixUnhandledParselet, // Token`LongName`SucceedsTilde
  &prefixUnhandledParselet, // Token`LongName`NotPrecedes
  &prefixUnhandledParselet, // Token`LongName`NotSucceeds
  &prefixUnhandledParselet, // Token`LongName`Subset
  &prefixUnhandledParselet, // Token`LongName`Superset
  &prefixUnhandledParselet, // Token`LongName`NotSubset
  &prefixUnhandledParselet, // Token`LongName`NotSuperset
  &prefixUnhandledParselet, // Token`LongName`SubsetEqual
  &prefixUnhandledParselet, // Token`LongName`SupersetEqual
  &prefixUnhandledParselet, // Token`LongName`NotSubsetEqual
  &prefixUnhandledParselet, // Token`LongName`NotSupersetEqual
  &prefixUnhandledParselet, // Token`LongName`UnionPlus
  &prefixUnhandledParselet, // Token`LongName`SquareSubset
  &prefixUnhandledParselet, // Token`LongName`SquareSuperset
  &prefixUnhandledParselet, // Token`LongName`SquareSubsetEqual
  &prefixUnhandledParselet, // Token`LongName`SquareSupersetEqual
  &prefixUnhandledParselet, // Token`LongName`SquareIntersection
  &prefixUnhandledParselet, // Token`LongName`SquareUnion
  &prefixUnhandledParselet, // Token`LongName`CirclePlus
  &prefixUnhandledParselet, // Token`LongName`CircleMinus
  &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_CIRCLETIMES, SYMBOL_CIRCLETIMES), // Token`LongName`CircleTimes
  &prefixUnhandledParselet, // Token`LongName`CircleDot
  &prefixUnhandledParselet, // Token`LongName`RightTee
  &prefixUnhandledParselet, // Token`LongName`LeftTee
  &prefixUnhandledParselet, // Token`LongName`DownTee
  &prefixUnhandledParselet, // Token`LongName`UpTee
  &prefixUnhandledParselet, // Token`LongName`DoubleRightTee
  &prefixUnhandledParselet, // Token`LongName`LeftTriangle
  &prefixUnhandledParselet, // Token`LongName`RightTriangle
  &prefixUnhandledParselet, // Token`LongName`LeftTriangleEqual
  &prefixUnhandledParselet, // Token`LongName`RightTriangleEqual
  &prefixUnhandledParselet, // Token`LongName`Xor
  &prefixUnhandledParselet, // Token`LongName`Nand
  &prefixUnhandledParselet, // Token`LongName`Nor
  &prefixUnhandledParselet, // Token`LongName`Wedge
  &prefixUnhandledParselet, // Token`LongName`Vee
  &prefixUnhandledParselet, // Token`LongName`Intersection
  &prefixUnhandledParselet, // Token`LongName`Union
  &prefixUnhandledParselet, // Token`LongName`Diamond
  &prefixUnhandledParselet, // Token`LongName`Star
  &prefixUnhandledParselet, // Token`LongName`LessEqualGreater
  &prefixUnhandledParselet, // Token`LongName`GreaterEqualLess
  &prefixUnhandledParselet, // Token`LongName`NotPrecedesSlantEqual
  &prefixUnhandledParselet, // Token`LongName`NotSucceedsSlantEqual
  &prefixUnhandledParselet, // Token`LongName`NotSquareSubsetEqual
  &prefixUnhandledParselet, // Token`LongName`NotSquareSupersetEqual
  &prefixUnhandledParselet, // Token`LongName`NotPrecedesTilde
  &prefixUnhandledParselet, // Token`LongName`NotSucceedsTilde
  &prefixUnhandledParselet, // Token`LongName`NotLeftTriangle
  &prefixUnhandledParselet, // Token`LongName`NotRightTriangle
  &prefixUnhandledParselet, // Token`LongName`NotLeftTriangleEqual
  &prefixUnhandledParselet, // Token`LongName`NotRightTriangleEqual
  &GroupParselet::new(TOKEN_LONGNAME_LEFTCEILING, SYMBOL_CEILING), // Token`LongName`LeftCeiling
  &prefixCloserParselet, // Token`LongName`RightCeiling
  &GroupParselet::new(TOKEN_LONGNAME_LEFTFLOOR, SYMBOL_FLOOR), // Token`LongName`LeftFloor
  &prefixCloserParselet, // Token`LongName`RightFloor
  &prefixUnhandledParselet, // Token`LongName`Cap
  &prefixUnhandledParselet, // Token`LongName`Cup
  &GroupParselet::new(TOKEN_LONGNAME_LEFTANGLEBRACKET, SYMBOL_ANGLEBRACKET), // Token`LongName`LeftAngleBracket
  &prefixCloserParselet, // Token`LongName`RightAngleBracket
  &prefixUnhandledParselet, // Token`LongName`Perpendicular
  &prefixUnhandledParselet, // Token`LongName`LongLeftArrow
  &prefixUnhandledParselet, // Token`LongName`LongRightArrow
  &prefixUnhandledParselet, // Token`LongName`LongLeftRightArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleLongLeftArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleLongRightArrow
  &prefixUnhandledParselet, // Token`LongName`DoubleLongLeftRightArrow
  &prefixUnhandledParselet, // Token`LongName`UpArrowBar
  &prefixUnhandledParselet, // Token`LongName`DownArrowBar
  &prefixUnhandledParselet, // Token`LongName`LeftRightVector
  &prefixUnhandledParselet, // Token`LongName`RightUpDownVector
  &prefixUnhandledParselet, // Token`LongName`DownLeftRightVector
  &prefixUnhandledParselet, // Token`LongName`LeftUpDownVector
  &prefixUnhandledParselet, // Token`LongName`LeftVectorBar
  &prefixUnhandledParselet, // Token`LongName`RightVectorBar
  &prefixUnhandledParselet, // Token`LongName`RightUpVectorBar
  &prefixUnhandledParselet, // Token`LongName`RightDownVectorBar
  &prefixUnhandledParselet, // Token`LongName`DownLeftVectorBar
  &prefixUnhandledParselet, // Token`LongName`DownRightVectorBar
  &prefixUnhandledParselet, // Token`LongName`LeftUpVectorBar
  &prefixUnhandledParselet, // Token`LongName`LeftDownVectorBar
  &prefixUnhandledParselet, // Token`LongName`LeftTeeVector
  &prefixUnhandledParselet, // Token`LongName`RightTeeVector
  &prefixUnhandledParselet, // Token`LongName`RightUpTeeVector
  &prefixUnhandledParselet, // Token`LongName`RightDownTeeVector
  &prefixUnhandledParselet, // Token`LongName`DownLeftTeeVector
  &prefixUnhandledParselet, // Token`LongName`DownRightTeeVector
  &prefixUnhandledParselet, // Token`LongName`LeftUpTeeVector
  &prefixUnhandledParselet, // Token`LongName`LeftDownTeeVector
  &prefixUnhandledParselet, // Token`LongName`UpEquilibrium
  &prefixUnhandledParselet, // Token`LongName`ReverseUpEquilibrium
  &prefixUnhandledParselet, // Token`LongName`RoundImplies
  &prefixUnhandledParselet, // Token`LongName`LeftTriangleBar
  &prefixUnhandledParselet, // Token`LongName`RightTriangleBar
  &prefixUnhandledParselet, // Token`LongName`Equivalent
  &prefixUnhandledParselet, // Token`LongName`LessSlantEqual
  &prefixUnhandledParselet, // Token`LongName`GreaterSlantEqual
  &prefixUnhandledParselet, // Token`LongName`NestedLessLess
  &prefixUnhandledParselet, // Token`LongName`NestedGreaterGreater
  &prefixUnhandledParselet, // Token`LongName`PrecedesEqual
  &prefixUnhandledParselet, // Token`LongName`SucceedsEqual
  &prefixUnhandledParselet, // Token`LongName`DoubleLeftTee
  &doubleBracketGroupParselet, // Token`LongName`LeftDoubleBracket
  &prefixCloserParselet, // Token`LongName`RightDoubleBracket
  &GroupParselet::new(TOKEN_LONGNAME_LEFTASSOCIATION, SYMBOL_ASSOCIATION), // Token`LongName`LeftAssociation
  &prefixCloserParselet, // Token`LongName`RightAssociation
  &prefixUnhandledParselet, // Token`LongName`TwoWayRule
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_PIECEWISE, SYMBOL_PIECEWISE), // Token`LongName`Piecewise
  &prefixUnhandledParselet, // Token`LongName`ImplicitPlus
  &prefixUnsupportedTokenParselet, // Token`LongName`AutoLeftMatch
  &prefixUnsupportedTokenParselet, // Token`LongName`AutoRightMatch
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, SYMBOL_INVISIBLEPREFIXSCRIPTBASE), // Token`LongName`InvisiblePrefixScriptBase
  &prefixUnhandledParselet, // Token`LongName`InvisiblePostfixScriptBase
  &prefixUnhandledParselet, // Token`LongName`Transpose
  &prefixUnhandledParselet, // Token`LongName`Conjugate
  &prefixUnhandledParselet, // Token`LongName`ConjugateTranspose
  &prefixUnhandledParselet, // Token`LongName`HermitianConjugate
  &prefixUnhandledParselet, // Token`LongName`VerticalBar
  &prefixUnhandledParselet, // Token`LongName`NotVerticalBar
  &prefixUnhandledParselet, // Token`LongName`Distributed
  &prefixUnhandledParselet, // Token`LongName`Conditioned
  &prefixUnhandledParselet, // Token`LongName`UndirectedEdge
  &prefixUnhandledParselet, // Token`LongName`DirectedEdge
  &prefixUnsupportedTokenParselet, // Token`LongName`Gradient
  &prefixUnsupportedTokenParselet, // Token`LongName`Divergence
  &prefixUnsupportedTokenParselet, // Token`LongName`Curl
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_CONTINUEDFRACTIONK, SYMBOL_CONTINUEDFRACTIONK), // Token`LongName`ContinuedFractionK
  &prefixUnhandledParselet, // Token`LongName`TensorProduct
  &prefixUnhandledParselet, // Token`LongName`TensorWedge
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_PROBABILITYPR, SYMBOL_PROBABILITYPR), // Token`LongName`ProbabilityPr
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_EXPECTATIONE, SYMBOL_EXPECTATIONE), // Token`LongName`ExpectationE
  &prefixUnhandledParselet, // Token`LongName`PermutationProduct
  &prefixUnhandledParselet, // Token`LongName`NotEqualTilde
  &prefixUnhandledParselet, // Token`LongName`NotHumpEqual
  &prefixUnhandledParselet, // Token`LongName`NotHumpDownHump
  &prefixUnhandledParselet, // Token`LongName`NotLeftTriangleBar
  &prefixUnhandledParselet, // Token`LongName`NotRightTriangleBar
  &prefixUnhandledParselet, // Token`LongName`NotLessLess
  &prefixUnhandledParselet, // Token`LongName`NotNestedLessLess
  &prefixUnhandledParselet, // Token`LongName`NotLessSlantEqual
  &prefixUnhandledParselet, // Token`LongName`NotGreaterGreater
  &prefixUnhandledParselet, // Token`LongName`NotNestedGreaterGreater
  &prefixUnhandledParselet, // Token`LongName`NotGreaterSlantEqual
  &prefixUnhandledParselet, // Token`LongName`NotPrecedesEqual
  &prefixUnhandledParselet, // Token`LongName`NotSucceedsEqual
  &prefixUnhandledParselet, // Token`LongName`NotSquareSubset
  &prefixUnhandledParselet, // Token`LongName`NotSquareSuperset
  &prefixUnhandledParselet, // Token`LongName`Equal
  &prefixUnhandledParselet, // Token`LongName`VerticalSeparator
  &prefixUnhandledParselet, // Token`LongName`VectorGreater
  &prefixUnhandledParselet, // Token`LongName`VectorGreaterEqual
  &prefixUnhandledParselet, // Token`LongName`VectorLess
  &prefixUnhandledParselet, // Token`LongName`VectorLessEqual
  &prefixUnsupportedTokenParselet, // Token`LongName`Limit
  &prefixUnsupportedTokenParselet, // Token`LongName`MaxLimit
  &prefixUnsupportedTokenParselet, // Token`LongName`MinLimit
  &prefixUnhandledParselet, // Token`LongName`Cross
  &prefixUnhandledParselet, // Token`LongName`Function
  &prefixUnhandledParselet, // Token`LongName`Xnor
  &prefixUnsupportedTokenParselet, // Token`LongName`DiscreteShift
  &prefixUnsupportedTokenParselet, // Token`LongName`DifferenceDelta
  &prefixUnsupportedTokenParselet, // Token`LongName`DiscreteRatio
  &prefixUnhandledParselet, // Token`LongName`RuleDelayed
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_SQUARE, SYMBOL_SQUARE), // Token`LongName`Square
  &prefixUnhandledParselet, // Token`LongName`Rule
  &prefixUnhandledParselet, // Token`LongName`Implies
  &prefixUnhandledParselet, // Token`LongName`ShortRightArrow
  &prefixUnhandledParselet, // Token`LongName`ShortLeftArrow
  &prefixUnhandledParselet, // Token`LongName`ShortUpArrow
  &prefixUnhandledParselet, // Token`LongName`ShortDownArrow
  &prefixUnhandledParselet, // Token`LongName`Application
  &GroupParselet::new(TOKEN_LONGNAME_LEFTBRACKETINGBAR, SYMBOL_BRACKETINGBAR), // Token`LongName`LeftBracketingBar
  &prefixCloserParselet, // Token`LongName`RightBracketingBar
  &GroupParselet::new(TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR, SYMBOL_DOUBLEBRACKETINGBAR), // Token`LongName`LeftDoubleBracketingBar
  &prefixCloserParselet, // Token`LongName`RightDoubleBracketingBar
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_CAPITALDIFFERENTIALD, SYMBOL_CAPITALDIFFERENTIALD), // Token`LongName`CapitalDifferentialD
  &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_DIFFERENTIALD, SYMBOL_DIFFERENTIALD), // Token`LongName`DifferentialD
  &prefixCommaParselet, // Token`LongName`InvisibleComma
  &prefixUnhandledParselet, // Token`LongName`InvisibleApplication
  &prefixUnhandledParselet, // Token`LongName`LongEqual
];

//
//
//
pub(crate) const INFIX_PARSELETS: [InfixParseletPtr; TOKEN_COUNT.value() as usize] = [
  &infixAssertFalseParselet, // Token`Unknown
  &infixAssertFalseParselet, // Token`EndOfFile
  &infixImplicitTimesParselet, // Token`Symbol
  &infixImplicitTimesParselet, // Token`String
  &infixImplicitTimesParselet, // Token`Integer
  &infixImplicitTimesParselet, // Token`Real
  &infixImplicitTimesParselet, // Token`Rational
  &infixImplicitTimesParselet, // Token`LinearSyntaxBlob
  &infixAssertFalseParselet, // Token`InternalNewline
  &infixAssertFalseParselet, // Token`Comment
  &infixAssertFalseParselet, // Token`Whitespace
  &infixImplicitTimesParselet, // Token`Buffer1
  &(InfixToplevelNewlineParselet {}), // Token`ToplevelNewline
  &infixImplicitTimesParselet, // Token`Buffer2
  &infixImplicitTimesParselet, // Token`Buffer3
  &infixImplicitTimesParselet, // Token`Buffer4
  &infixAssertFalseParselet, // Token`Error`ExpectedEqual
  &infixAssertFalseParselet, // Token`Error`Number
  &infixAssertFalseParselet, // Token`Error`UnhandledCharacter
  &infixAssertFalseParselet, // Token`Error`ExpectedLetterlike
  &infixAssertFalseParselet, // Token`Error`Aborted
  &infixAssertFalseParselet, // Token`Error`ExpectedOperand
  &infixAssertFalseParselet, // Token`Error`ExpectedTag
  &infixAssertFalseParselet, // Token`Error`ExpectedFile
  &infixAssertFalseParselet, // Token`Error`UnexpectedCloser
  &infixImplicitTimesParselet, // Token`Error`PrefixImplicitNull
  &infixImplicitTimesParselet, // Token`Error`InfixImplicitNull
  &infixAssertFalseParselet, // Token`Error`UnsafeCharacterEncoding
  &infixAssertFalseParselet, // Token`Error`UnterminatedComment
  &infixAssertFalseParselet, // Token`Error`UnterminatedString
  &infixAssertFalseParselet, // Token`Error`UnterminatedFileString
  &infixAssertFalseParselet, // Token`Error`UnterminatedLinearSyntaxBlob
  &infixAssertFalseParselet, // Token`Error`UnsupportedToken
  &infixAssertFalseParselet, // Token`Error`UnexpectedCommentCloser
  &infixImplicitTimesParselet, // Token`Error`End
  &InfixOperatorParselet::new(PRECEDENCE_DOT, SYMBOL_DOT), // Token`Dot
  &colonParselet, // Token`Colon
  &infixImplicitTimesParselet, // Token`OpenParen
  &infixAssertFalseParselet, // Token`CloseParen
  &(CallParselet::new(&squareGroupParselet)), // Token`OpenSquare
  &infixAssertFalseParselet, // Token`CloseSquare
  &commaParselet, // Token`Comma
  &infixImplicitTimesParselet, // Token`OpenCurly
  &infixAssertFalseParselet, // Token`CloseCurly
  &equalParselet, // Token`Equal
  &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_BANG, SYMBOL_FACTORIAL), // Token`Bang
  &infixImplicitTimesParselet, // Token`Under
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`Less
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`Greater
  &InfixOperatorParselet::new(PRECEDENCE_INFIX_MINUS, SYMBOL_PLUS), // Token`Minus
  &InfixOperatorParselet::new(PRECEDENCE_BAR, SYMBOL_ALTERNATIVES), // Token`Bar
  &semiParselet, // Token`Semi
  &infixImplicitTimesParselet, // Token`Hash
  &PostfixOperatorParselet::new(PRECEDENCE_AMP, SYMBOL_FUNCTION), // Token`Amp
  &BinaryOperatorParselet::new(PRECEDENCE_SLASH, SYMBOL_DIVIDE), // Token`Slash
  &BinaryOperatorParselet::new(PRECEDENCE_AT, SYMBOL_CODEPARSER_BINARYAT), // Token`At
  &InfixOperatorParselet::new(PRECEDENCE_INFIX_PLUS, SYMBOL_PLUS), // Token`Plus
  (&TildeParselet {}), // Token`Tilde
  &timesParselet, // Token`Star
  &BinaryOperatorParselet::new(PRECEDENCE_CARET, SYMBOL_POWER), // Token`Caret
  &PostfixOperatorParselet::new(PRECEDENCE_SINGLEQUOTE, SYMBOL_DERIVATIVE), // Token`SingleQuote
  &infixImplicitTimesParselet, // Token`Percent
  &BinaryOperatorParselet::new(PRECEDENCE_INFIX_QUESTION, SYMBOL_PATTERNTEST), // Token`Question
  &PostfixOperatorParselet::new(PRECEDENCE_DOTDOT, SYMBOL_REPEATED), // Token`DotDot
  (&ColonColonParselet {}), // Token`ColonColon
  &colonEqualParselet, // Token`ColonEqual
  &BinaryOperatorParselet::new(PRECEDENCE_COLONGREATER, SYMBOL_RULEDELAYED), // Token`ColonGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`EqualEqual
  &infixImplicitTimesParselet, // Token`UnderUnder
  &infixImplicitTimesParselet, // Token`UnderDot
  &infixImplicitTimesParselet, // Token`LessBar
  &infixImplicitTimesParselet, // Token`LessLess
  &InfixOperatorParselet::new(PRECEDENCE_LESSGREATER, SYMBOL_STRINGJOIN), // Token`LessGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LessEqual
  (&GreaterGreaterParselet {}), // Token`GreaterGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`GreaterEqual
  &BinaryOperatorParselet::new(PRECEDENCE_MINUSGREATER, SYMBOL_RULE), // Token`MinusGreater
  &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_MINUSMINUS, SYMBOL_DECREMENT), // Token`MinusMinus
  &BinaryOperatorParselet::new(PRECEDENCE_MINUSEQUAL, SYMBOL_SUBTRACTFROM), // Token`MinusEqual
  &InfixOperatorParselet::new(PRECEDENCE_BARBAR, SYMBOL_OR), // Token`BarBar
  &infixAssertFalseParselet, // Token`BarGreater
  &semiSemiParselet, // Token`SemiSemi
  &InfixOperatorParselet::new(PRECEDENCE_AMPAMP, SYMBOL_AND), // Token`AmpAmp
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHAT, SYMBOL_MAP), // Token`SlashAt
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHSEMI, SYMBOL_CONDITION), // Token`SlashSemi
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHDOT, SYMBOL_REPLACEALL), // Token`SlashDot
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASH, SYMBOL_CODEPARSER_BINARYSLASHSLASH), // Token`SlashSlash
  &slashColonParselet, // Token`SlashColon
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHEQUAL, SYMBOL_DIVIDEBY), // Token`SlashEqual
  &InfixOperatorParselet::new(PRECEDENCE_SLASHSTAR, SYMBOL_RIGHTCOMPOSITION), // Token`SlashStar
  &BinaryOperatorParselet::new(PRECEDENCE_ATAT, SYMBOL_APPLY), // Token`AtAt
  &InfixOperatorParselet::new(PRECEDENCE_ATSTAR, SYMBOL_COMPOSITION), // Token`AtStar
  &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_PLUSPLUS, SYMBOL_INCREMENT), // Token`PlusPlus
  &BinaryOperatorParselet::new(PRECEDENCE_PLUSEQUAL, SYMBOL_ADDTO), // Token`PlusEqual
  &InfixOperatorParselet::new(PRECEDENCE_TILDETILDE, SYMBOL_STRINGEXPRESSION), // Token`TildeTilde
  &BinaryOperatorParselet::new(PRECEDENCE_STAREQUAL, SYMBOL_TIMESBY), // Token`StarEqual
  &InfixOperatorParselet::new(PRECEDENCE_STARSTAR, SYMBOL_NONCOMMUTATIVEMULTIPLY), // Token`StarStar
  &BinaryOperatorParselet::new(PRECEDENCE_CARETEQUAL, SYMBOL_UPSET), // Token`CaretEqual
  &infixImplicitTimesParselet, // Token`HashHash
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`BangEqual
  &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_BANGBANG, SYMBOL_FACTORIAL2), // Token`BangBang
  &infixAssertFalseParselet, // Token`QuestionQuestion
  &PostfixOperatorParselet::new(PRECEDENCE_DOTDOTDOT, SYMBOL_REPEATEDNULL), // Token`DotDotDot
  &InfixOperatorParselet::new(PRECEDENCE_EQUALEQUALEQUAL, SYMBOL_SAMEQ), // Token`EqualEqualEqual
  &InfixOperatorParselet::new(PRECEDENCE_EQUALBANGEQUAL, SYMBOL_UNSAMEQ), // Token`EqualBangEqual
  &infixImplicitTimesParselet, // Token`UnderUnderUnder
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASHDOT, SYMBOL_REPLACEREPEATED), // Token`SlashSlashDot
  &BinaryOperatorParselet::new(PRECEDENCE_ATATAT, SYMBOL_MAPAPPLY), // Token`AtAtAt
  &BinaryOperatorParselet::new(PRECEDENCE_LESSMINUSGREATER, SYMBOL_TWOWAYRULE), // Token`LessMinusGreater
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASHAT, SYMBOL_MAPALL), // Token`SlashSlashAt
  &BinaryOperatorParselet::new(PRECEDENCE_CARETCOLONEQUAL, SYMBOL_UPSETDELAYED), // Token`CaretColonEqual
  (&GreaterGreaterGreaterParselet {}), // Token`GreaterGreaterGreater
  &BinaryOperatorParselet::new(PRECEDENCE_BARMINUSGREATER, SYMBOL_FUNCTION), // Token`BarMinusGreater
  &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASHEQUAL, SYMBOL_APPLYTO), // Token`SlashSlashEqual
  &(CallParselet::new(&GroupParselet::new(TOKEN_COLONCOLONOPENSQUARE, SYMBOL_CODEPARSER_GROUPTYPESPECIFIER))), // Token`ColonColonOpenSquare
  &infixImplicitTimesParselet, // Token`PercentPercent
  &infixImplicitTimesParselet, // Token`LinearSyntax`Bang
  &infixImplicitTimesParselet, // Token`LinearSyntax`CloseParen
  &infixImplicitTimesParselet, // Token`LinearSyntax`At
  &infixImplicitTimesParselet, // Token`LinearSyntax`Amp
  &infixImplicitTimesParselet, // Token`LinearSyntax`Star
  &infixImplicitTimesParselet, // Token`LinearSyntax`Under
  &infixImplicitTimesParselet, // Token`LinearSyntax`Caret
  &infixImplicitTimesParselet, // Token`LinearSyntax`Space
  &infixImplicitTimesParselet, // Token`LinearSyntax`Percent
  &infixImplicitTimesParselet, // Token`LinearSyntax`Plus
  &infixImplicitTimesParselet, // Token`LinearSyntax`Slash
  &infixImplicitTimesParselet, // Token`LinearSyntax`BackTick
  &timesParselet, // Token`Fake`ImplicitTimes
  &infixImplicitTimesParselet, // Token`Fake`ImplicitNull
  &infixImplicitTimesParselet, // Token`Fake`ImplicitOne
  &infixImplicitTimesParselet, // Token`Fake`ImplicitAll
  &infixImplicitTimesParselet, // Token`Boxes`OpenParenStar
  &infixImplicitTimesParselet, // Token`Boxes`StarCloseParen
  &infixImplicitTimesParselet, // Token`Boxes`MultiSingleQuote
  &infixImplicitTimesParselet, // Token`Boxes`MultiWhitespace
  &infixImplicitTimesParselet, // Token`LongName`Not
  &BinaryOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_PLUSMINUS, SYMBOL_PLUSMINUS), // Token`LongName`PlusMinus
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CENTERDOT, SYMBOL_CENTERDOT), // Token`LongName`CenterDot
  &timesParselet, // Token`LongName`Times
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DIVIDE, SYMBOL_DIVIDE), // Token`LongName`Divide
  &infixImplicitTimesParselet, // Token`LongName`OpenCurlyQuote
  &infixAssertFalseParselet, // Token`LongName`CloseCurlyQuote
  &infixImplicitTimesParselet, // Token`LongName`OpenCurlyDoubleQuote
  &infixAssertFalseParselet, // Token`LongName`CloseCurlyDoubleQuote
  &timesParselet, // Token`LongName`InvisibleTimes
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_LEFTARROW), // Token`LongName`LeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_UPARROW), // Token`LongName`UpArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_RIGHTARROW), // Token`LongName`RightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOWNARROW), // Token`LongName`DownArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_LEFTRIGHTARROW), // Token`LongName`LeftRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_UPDOWNARROW), // Token`LongName`UpDownArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, SYMBOL_UPPERLEFTARROW), // Token`LongName`UpperLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, SYMBOL_UPPERRIGHTARROW), // Token`LongName`UpperRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, SYMBOL_LOWERRIGHTARROW), // Token`LongName`LowerRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, SYMBOL_LOWERLEFTARROW), // Token`LongName`LowerLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_LEFTTEEARROW), // Token`LongName`LeftTeeArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_UPTEEARROW), // Token`LongName`UpTeeArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_RIGHTTEEARROW), // Token`LongName`RightTeeArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOWNTEEARROW), // Token`LongName`DownTeeArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_LEFTVECTOR), // Token`LongName`LeftVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNLEFTVECTOR), // Token`LongName`DownLeftVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTUPVECTOR), // Token`LongName`RightUpVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTUPVECTOR), // Token`LongName`LeftUpVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_RIGHTVECTOR), // Token`LongName`RightVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNRIGHTVECTOR), // Token`LongName`DownRightVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTDOWNVECTOR), // Token`LongName`RightDownVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTDOWNVECTOR), // Token`LongName`LeftDownVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_RIGHTARROWLEFTARROW), // Token`LongName`RightArrowLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_UPARROWDOWNARROW), // Token`LongName`UpArrowDownArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_LEFTARROWRIGHTARROW), // Token`LongName`LeftArrowRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_REVERSEEQUILIBRIUM), // Token`LongName`ReverseEquilibrium
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_EQUILIBRIUM), // Token`LongName`Equilibrium
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_DOUBLELEFTARROW), // Token`LongName`DoubleLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOUBLEUPARROW), // Token`LongName`DoubleUpArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_DOUBLERIGHTARROW), // Token`LongName`DoubleRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOUBLEDOWNARROW), // Token`LongName`DoubleDownArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_DOUBLELEFTRIGHTARROW), // Token`LongName`DoubleLeftRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOUBLEUPDOWNARROW), // Token`LongName`DoubleUpDownArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_LEFTARROWBAR), // Token`LongName`LeftArrowBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_RIGHTARROWBAR), // Token`LongName`RightArrowBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOWNARROWUPARROW), // Token`LongName`DownArrowUpArrow
  &infixImplicitTimesParselet, // Token`LongName`ForAll
  &infixAssertFalseParselet, // Token`LongName`PartialD
  &infixImplicitTimesParselet, // Token`LongName`Exists
  &infixImplicitTimesParselet, // Token`LongName`NotExists
  &infixAssertFalseParselet, // Token`LongName`Laplacian
  &infixImplicitTimesParselet, // Token`LongName`Del
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_ELEMENT), // Token`LongName`Element
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTELEMENT), // Token`LongName`NotElement
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_REVERSEELEMENT), // Token`LongName`ReverseElement
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTREVERSEELEMENT), // Token`LongName`NotReverseElement
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_SUCHTHAT, SYMBOL_SUCHTHAT), // Token`LongName`SuchThat
  &infixImplicitTimesParselet, // Token`LongName`Product
  &InfixOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_COPRODUCT, SYMBOL_COPRODUCT), // Token`LongName`Coproduct
  &infixImplicitTimesParselet, // Token`LongName`Sum
  &InfixOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_MINUS, SYMBOL_PLUS), // Token`LongName`Minus
  &BinaryOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_MINUSPLUS, SYMBOL_MINUSPLUS), // Token`LongName`MinusPlus
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DIVISIONSLASH, SYMBOL_DIVIDE), // Token`LongName`DivisionSlash
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_BACKSLASH, SYMBOL_BACKSLASH), // Token`LongName`Backslash
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_SMALLCIRCLE, SYMBOL_SMALLCIRCLE), // Token`LongName`SmallCircle
  &infixImplicitTimesParselet, // Token`LongName`Sqrt
  &infixImplicitTimesParselet, // Token`LongName`CubeRoot
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_PROPORTIONAL), // Token`LongName`Proportional
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_DIVIDES, SYMBOL_DIVISIBLE), // Token`LongName`Divides
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_DOUBLEVERTICALBAR, SYMBOL_DOUBLEVERTICALBAR), // Token`LongName`DoubleVerticalBar
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NOTDOUBLEVERTICALBAR, SYMBOL_NOTDOUBLEVERTICALBAR), // Token`LongName`NotDoubleVerticalBar
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_AND, SYMBOL_AND), // Token`LongName`And
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_OR, SYMBOL_OR), // Token`LongName`Or
  &infixImplicitTimesParselet, // Token`LongName`Integral
  &infixImplicitTimesParselet, // Token`LongName`ContourIntegral
  &infixImplicitTimesParselet, // Token`LongName`DoubleContourIntegral
  &infixImplicitTimesParselet, // Token`LongName`ClockwiseContourIntegral
  &infixImplicitTimesParselet, // Token`LongName`CounterClockwiseContourIntegral
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_THEREFORE, SYMBOL_THEREFORE), // Token`LongName`Therefore
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_BECAUSE, SYMBOL_BECAUSE), // Token`LongName`Because
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_COLON, SYMBOL_COLON), // Token`LongName`Colon
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_PROPORTION), // Token`LongName`Proportion
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_TILDE), // Token`LongName`Tilde
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VERTICALTILDE, SYMBOL_VERTICALTILDE), // Token`LongName`VerticalTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTTILDE), // Token`LongName`NotTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_EQUALTILDE), // Token`LongName`EqualTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_TILDEEQUAL), // Token`LongName`TildeEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTTILDEEQUAL), // Token`LongName`NotTildeEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_TILDEFULLEQUAL), // Token`LongName`TildeFullEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTTILDEFULLEQUAL), // Token`LongName`NotTildeFullEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_TILDETILDE), // Token`LongName`TildeTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTTILDETILDE), // Token`LongName`NotTildeTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_CUPCAP), // Token`LongName`CupCap
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_HUMPDOWNHUMP), // Token`LongName`HumpDownHump
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_HUMPEQUAL), // Token`LongName`HumpEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_DOTEQUAL), // Token`LongName`DotEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_CONGRUENT), // Token`LongName`Congruent
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTCONGRUENT), // Token`LongName`NotCongruent
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessFullEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterFullEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLessFullEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreaterFullEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTCUPCAP), // Token`LongName`NotCupCap
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLessEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreaterEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLessTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreaterTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLessGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreaterLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_PRECEDES), // Token`LongName`Precedes
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_SUCCEEDS), // Token`LongName`Succeeds
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_PRECEDESSLANTEQUAL), // Token`LongName`PrecedesSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_SUCCEEDSSLANTEQUAL), // Token`LongName`SucceedsSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_PRECEDESTILDE), // Token`LongName`PrecedesTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_SUCCEEDSTILDE), // Token`LongName`SucceedsTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTPRECEDES), // Token`LongName`NotPrecedes
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTSUCCEEDS), // Token`LongName`NotSucceeds
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SUBSET), // Token`LongName`Subset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SUPERSET), // Token`LongName`Superset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSUBSET), // Token`LongName`NotSubset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSUPERSET), // Token`LongName`NotSuperset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SUBSETEQUAL), // Token`LongName`SubsetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SUPERSETEQUAL), // Token`LongName`SupersetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSUBSETEQUAL), // Token`LongName`NotSubsetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSUPERSETEQUAL), // Token`LongName`NotSupersetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_UNIONOPERATORS, SYMBOL_UNIONPLUS), // Token`LongName`UnionPlus
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SQUARESUBSET), // Token`LongName`SquareSubset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SQUARESUPERSET), // Token`LongName`SquareSuperset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SQUARESUBSETEQUAL), // Token`LongName`SquareSubsetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_SQUARESUPERSETEQUAL), // Token`LongName`SquareSupersetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INTERSECTIONOPERATORS, SYMBOL_SQUAREINTERSECTION), // Token`LongName`SquareIntersection
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_UNIONOPERATORS, SYMBOL_SQUAREUNION), // Token`LongName`SquareUnion
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CIRCLEPLUS, SYMBOL_CIRCLEPLUS), // Token`LongName`CirclePlus
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_CIRCLEMINUS, SYMBOL_CIRCLEMINUS), // Token`LongName`CircleMinus
  &InfixOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_CIRCLETIMES, SYMBOL_CIRCLETIMES), // Token`LongName`CircleTimes
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CIRCLEDOT, SYMBOL_CIRCLEDOT), // Token`LongName`CircleDot
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_RIGHTTEE, SYMBOL_RIGHTTEE), // Token`LongName`RightTee
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_LEFTTEE, SYMBOL_LEFTTEE), // Token`LongName`LeftTee
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DOWNTEE, SYMBOL_DOWNTEE), // Token`LongName`DownTee
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_UPTEE, SYMBOL_UPTEE), // Token`LongName`UpTee
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DOUBLERIGHTTEE, SYMBOL_DOUBLERIGHTTEE), // Token`LongName`DoubleRightTee
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_LEFTTRIANGLE), // Token`LongName`LeftTriangle
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_RIGHTTRIANGLE), // Token`LongName`RightTriangle
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_LEFTTRIANGLEEQUAL), // Token`LongName`LeftTriangleEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_RIGHTTRIANGLEEQUAL), // Token`LongName`RightTriangleEqual
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_XOR, SYMBOL_XOR), // Token`LongName`Xor
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NAND, SYMBOL_NAND), // Token`LongName`Nand
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NOR, SYMBOL_NOR), // Token`LongName`Nor
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_WEDGE, SYMBOL_WEDGE), // Token`LongName`Wedge
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VEE, SYMBOL_VEE), // Token`LongName`Vee
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INTERSECTIONOPERATORS, SYMBOL_INTERSECTION), // Token`LongName`Intersection
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_UNIONOPERATORS, SYMBOL_UNION), // Token`LongName`Union
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_DIAMOND, SYMBOL_DIAMOND), // Token`LongName`Diamond
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_STAR, SYMBOL_STAR), // Token`LongName`Star
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessEqualGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterEqualLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTPRECEDESSLANTEQUAL), // Token`LongName`NotPrecedesSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTSUCCEEDSSLANTEQUAL), // Token`LongName`NotSucceedsSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSQUARESUBSETEQUAL), // Token`LongName`NotSquareSubsetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSQUARESUPERSETEQUAL), // Token`LongName`NotSquareSupersetEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTPRECEDESTILDE), // Token`LongName`NotPrecedesTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTSUCCEEDSTILDE), // Token`LongName`NotSucceedsTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTLEFTTRIANGLE), // Token`LongName`NotLeftTriangle
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTRIGHTTRIANGLE), // Token`LongName`NotRightTriangle
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTLEFTTRIANGLEEQUAL), // Token`LongName`NotLeftTriangleEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTRIGHTTRIANGLEEQUAL), // Token`LongName`NotRightTriangleEqual
  &infixImplicitTimesParselet, // Token`LongName`LeftCeiling
  &infixAssertFalseParselet, // Token`LongName`RightCeiling
  &infixImplicitTimesParselet, // Token`LongName`LeftFloor
  &infixAssertFalseParselet, // Token`LongName`RightFloor
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CAP, SYMBOL_CAP), // Token`LongName`Cap
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CUP, SYMBOL_CUP), // Token`LongName`Cup
  &infixImplicitTimesParselet, // Token`LongName`LeftAngleBracket
  &infixAssertFalseParselet, // Token`LongName`RightAngleBracket
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_PERPENDICULAR, SYMBOL_PERPENDICULAR), // Token`LongName`Perpendicular
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_LONGLEFTARROW), // Token`LongName`LongLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_LONGRIGHTARROW), // Token`LongName`LongRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_LONGLEFTRIGHTARROW), // Token`LongName`LongLeftRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOUBLELONGLEFTARROW), // Token`LongName`DoubleLongLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOUBLELONGRIGHTARROW), // Token`LongName`DoubleLongRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOUBLELONGLEFTRIGHTARROW), // Token`LongName`DoubleLongLeftRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_UPARROWBAR), // Token`LongName`UpArrowBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_DOWNARROWBAR), // Token`LongName`DownArrowBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_LEFTRIGHTVECTOR), // Token`LongName`LeftRightVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTUPDOWNVECTOR), // Token`LongName`RightUpDownVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNLEFTRIGHTVECTOR), // Token`LongName`DownLeftRightVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTUPDOWNVECTOR), // Token`LongName`LeftUpDownVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_LEFTVECTORBAR), // Token`LongName`LeftVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_RIGHTVECTORBAR), // Token`LongName`RightVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTUPVECTORBAR), // Token`LongName`RightUpVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTDOWNVECTORBAR), // Token`LongName`RightDownVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNLEFTVECTORBAR), // Token`LongName`DownLeftVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNRIGHTVECTORBAR), // Token`LongName`DownRightVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTUPVECTORBAR), // Token`LongName`LeftUpVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTDOWNVECTORBAR), // Token`LongName`LeftDownVectorBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_LEFTTEEVECTOR), // Token`LongName`LeftTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_RIGHTTEEVECTOR), // Token`LongName`RightTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTUPTEEVECTOR), // Token`LongName`RightUpTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_RIGHTDOWNTEEVECTOR), // Token`LongName`RightDownTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNLEFTTEEVECTOR), // Token`LongName`DownLeftTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, SYMBOL_DOWNRIGHTTEEVECTOR), // Token`LongName`DownRightTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTUPTEEVECTOR), // Token`LongName`LeftUpTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_LEFTDOWNTEEVECTOR), // Token`LongName`LeftDownTeeVector
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_UPEQUILIBRIUM), // Token`LongName`UpEquilibrium
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, SYMBOL_REVERSEUPEQUILIBRIUM), // Token`LongName`ReverseUpEquilibrium
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_ROUNDIMPLIES, SYMBOL_ROUNDIMPLIES), // Token`LongName`RoundImplies
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_LEFTTRIANGLEBAR), // Token`LongName`LeftTriangleBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_RIGHTTRIANGLEBAR), // Token`LongName`RightTriangleBar
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_EQUIVALENT, SYMBOL_EQUIVALENT), // Token`LongName`Equivalent
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LessSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`GreaterSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NestedLessLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NestedGreaterGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_PRECEDESEQUAL), // Token`LongName`PrecedesEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_SUCCEEDSEQUAL), // Token`LongName`SucceedsEqual
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DOUBLELEFTTEE, SYMBOL_DOUBLELEFTTEE), // Token`LongName`DoubleLeftTee
  &(CallParselet::new(&doubleBracketGroupParselet)), // Token`LongName`LeftDoubleBracket
  &infixAssertFalseParselet, // Token`LongName`RightDoubleBracket
  &infixImplicitTimesParselet, // Token`LongName`LeftAssociation
  &infixAssertFalseParselet, // Token`LongName`RightAssociation
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_TWOWAYRULE, SYMBOL_TWOWAYRULE), // Token`LongName`TwoWayRule
  &infixImplicitTimesParselet, // Token`LongName`Piecewise
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_IMPLICITPLUS, SYMBOL_PLUS), // Token`LongName`ImplicitPlus
  &infixAssertFalseParselet, // Token`LongName`AutoLeftMatch
  &infixAssertFalseParselet, // Token`LongName`AutoRightMatch
  &infixImplicitTimesParselet, // Token`LongName`InvisiblePrefixScriptBase
  &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, SYMBOL_INVISIBLEPOSTFIXSCRIPTBASE), // Token`LongName`InvisiblePostfixScriptBase
  &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_TRANSPOSE, SYMBOL_TRANSPOSE), // Token`LongName`Transpose
  &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_CONJUGATE, SYMBOL_CONJUGATE), // Token`LongName`Conjugate
  &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE, SYMBOL_CONJUGATETRANSPOSE), // Token`LongName`ConjugateTranspose
  &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_HERMITIANCONJUGATE, SYMBOL_HERMITIANCONJUGATE), // Token`LongName`HermitianConjugate
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VERTICALBAR, SYMBOL_VERTICALBAR), // Token`LongName`VerticalBar
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NOTVERTICALBAR, SYMBOL_NOTVERTICALBAR), // Token`LongName`NotVerticalBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_DISTRIBUTED), // Token`LongName`Distributed
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CONDITIONED, SYMBOL_CONDITIONED), // Token`LongName`Conditioned
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, SYMBOL_UNDIRECTEDEDGE), // Token`LongName`UndirectedEdge
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DIRECTEDEDGE, SYMBOL_DIRECTEDEDGE), // Token`LongName`DirectedEdge
  &infixAssertFalseParselet, // Token`LongName`Gradient
  &infixAssertFalseParselet, // Token`LongName`Divergence
  &infixAssertFalseParselet, // Token`LongName`Curl
  &infixImplicitTimesParselet, // Token`LongName`ContinuedFractionK
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_TENSORPRODUCT, SYMBOL_TENSORPRODUCT), // Token`LongName`TensorProduct
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_TENSORWEDGE, SYMBOL_TENSORWEDGE), // Token`LongName`TensorWedge
  &infixImplicitTimesParselet, // Token`LongName`ProbabilityPr
  &infixImplicitTimesParselet, // Token`LongName`ExpectationE
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_PERMUTATIONPRODUCT, SYMBOL_PERMUTATIONPRODUCT), // Token`LongName`PermutationProduct
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTEQUALTILDE), // Token`LongName`NotEqualTilde
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTHUMPEQUAL), // Token`LongName`NotHumpEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTHUMPDOWNHUMP), // Token`LongName`NotHumpDownHump
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTLEFTTRIANGLEBAR), // Token`LongName`NotLeftTriangleBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTRIGHTTRIANGLEBAR), // Token`LongName`NotRightTriangleBar
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLessLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotNestedLessLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotLessSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreaterGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotNestedGreaterGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`NotGreaterSlantEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTPRECEDESEQUAL), // Token`LongName`NotPrecedesEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, SYMBOL_NOTSUCCEEDSEQUAL), // Token`LongName`NotSucceedsEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSQUARESUBSET), // Token`LongName`NotSquareSubset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, SYMBOL_NOTSQUARESUPERSET), // Token`LongName`NotSquareSuperset
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`Equal
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VERTICALSEPARATOR, SYMBOL_VERTICALSEPARATOR), // Token`LongName`VerticalSeparator
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`VectorGreater
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`VectorGreaterEqual
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`VectorLess
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`VectorLessEqual
  &infixAssertFalseParselet, // Token`LongName`Limit
  &infixAssertFalseParselet, // Token`LongName`MaxLimit
  &infixAssertFalseParselet, // Token`LongName`MinLimit
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CROSS, SYMBOL_CROSS), // Token`LongName`Cross
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_FUNCTION, SYMBOL_FUNCTION), // Token`LongName`Function
  &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_XNOR, SYMBOL_XNOR), // Token`LongName`Xnor
  &infixAssertFalseParselet, // Token`LongName`DiscreteShift
  &infixAssertFalseParselet, // Token`LongName`DifferenceDelta
  &infixAssertFalseParselet, // Token`LongName`DiscreteRatio
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_RULEDELAYED, SYMBOL_RULEDELAYED), // Token`LongName`RuleDelayed
  &infixImplicitTimesParselet, // Token`LongName`Square
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_RULE, SYMBOL_RULE), // Token`LongName`Rule
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_IMPLIES, SYMBOL_IMPLIES), // Token`LongName`Implies
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_SHORTRIGHTARROW), // Token`LongName`ShortRightArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, SYMBOL_SHORTLEFTARROW), // Token`LongName`ShortLeftArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_SHORTUPARROW), // Token`LongName`ShortUpArrow
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, SYMBOL_SHORTDOWNARROW), // Token`LongName`ShortDownArrow
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_APPLICATION, SYMBOL_APPLICATION), // Token`LongName`Application
  &infixImplicitTimesParselet, // Token`LongName`LeftBracketingBar
  &infixAssertFalseParselet, // Token`LongName`RightBracketingBar
  &infixImplicitTimesParselet, // Token`LongName`LeftDoubleBracketingBar
  &infixAssertFalseParselet, // Token`LongName`RightDoubleBracketingBar
  &infixDifferentialDParselet, // Token`LongName`CapitalDifferentialD
  &infixDifferentialDParselet, // Token`LongName`DifferentialD
  &commaParselet, // Token`LongName`InvisibleComma
  &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, SYMBOL_CODEPARSER_BINARYAT), // Token`LongName`InvisibleApplication
  &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, SYMBOL_CODEPARSER_INFIXINEQUALITY), // Token`LongName`LongEqual
];
