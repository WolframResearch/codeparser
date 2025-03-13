//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(non_upper_case_globals)]

use wolfram_expr::symbol::SymbolRef;

use crate::{
	token::TokenKind,
	symbol::Symbol,
	symbol_registration::*,
	precedence::*,
	parselet::*
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

pub(crate) const under1Parselet: UnderParselet = UnderParselet::new(Operator::Blank, Operator::CodeParser_PatternBlank);
pub(crate) const under2Parselet: UnderParselet = UnderParselet::new(Operator::BlankSequence, Operator::CodeParser_PatternBlankSequence);
pub(crate) const under3Parselet: UnderParselet = UnderParselet::new(Operator::BlankNullSequence, Operator::CodeParser_PatternBlankNullSequence);

pub(crate) const underDotParselet: UnderDotParselet = UnderDotParselet {};

pub(crate) const squareGroupParselet: GroupParselet = GroupParselet::new(TokenKind::OpenSquare, Operator::CodeParser_GroupSquare);

pub(crate) const doubleBracketGroupParselet: GroupParselet = GroupParselet::new(TokenKind::LongName_LeftDoubleBracket, Operator::CodeParser_GroupDoubleBracket);

pub(crate) const timesParselet: TimesParselet = TimesParselet {};

//
//
//

pub(crate) const PREFIX_PARSELETS: [PrefixParseletPtr; TokenKind::Count.value() as usize] = [
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
    &GroupParselet::new(TokenKind::OpenParen, Operator::CodeParser_GroupParen), // Token`OpenParen
    &prefixCloserParselet, // Token`CloseParen
    &squareGroupParselet, // Token`OpenSquare
    &prefixCloserParselet, // Token`CloseSquare
    &prefixCommaParselet, // Token`Comma
    &GroupParselet::new(TokenKind::OpenCurly, Operator::List), // Token`OpenCurly
    &prefixCloserParselet, // Token`CloseCurly
    &prefixUnhandledParselet, // Token`Equal
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_BANG, Operator::Not), // Token`Bang
    &under1Parselet, // Token`Under
    &prefixUnhandledParselet, // Token`Less
    &prefixUnhandledParselet, // Token`Greater
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_MINUS, Operator::Minus), // Token`Minus
    &prefixUnhandledParselet, // Token`Bar
    &prefixUnhandledParselet, // Token`Semi
    &HashParselet {}, // Token`Hash
    &prefixUnhandledParselet, // Token`Amp
    &prefixUnhandledParselet, // Token`Slash
    &prefixUnhandledParselet, // Token`At
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_PLUS, Operator::Plus), // Token`Plus
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
    &GroupParselet::new(TokenKind::LessBar, Operator::Association), // Token`LessBar
    &(LessLessParselet {}), // Token`LessLess
    &prefixUnhandledParselet, // Token`LessGreater
    &prefixUnhandledParselet, // Token`LessEqual
    &prefixUnhandledParselet, // Token`GreaterGreater
    &prefixUnhandledParselet, // Token`GreaterEqual
    &prefixUnhandledParselet, // Token`MinusGreater
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_MINUSMINUS, Operator::PreDecrement), // Token`MinusMinus
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
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_PLUSPLUS, Operator::PreIncrement), // Token`PlusPlus
    &prefixUnhandledParselet, // Token`PlusEqual
    &prefixUnhandledParselet, // Token`TildeTilde
    &prefixUnhandledParselet, // Token`StarEqual
    &prefixUnhandledParselet, // Token`StarStar
    &prefixUnhandledParselet, // Token`CaretEqual
    &HashHashParselet {}, // Token`HashHash
    &prefixUnhandledParselet, // Token`BangEqual
    &PrefixOperatorParselet::new(PRECEDENCE_FAKE_PREFIX_BANGBANG, Operator::CodeParser_PrefixNot2), // Token`BangBang
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
    &GroupParselet::new(TokenKind::ColonColonOpenSquare, Operator::CodeParser_GroupTypeSpecifier), // Token`ColonColonOpenSquare
    &leafParselet, // Token`PercentPercent
    &PrefixOperatorParselet::new(PRECEDENCE_LINEARSYNTAX_BANG, Operator::CodeParser_PrefixLinearSyntaxBang), // Token`LinearSyntax`Bang
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
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_NOT, Operator::Not), // Token`LongName`Not
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS, Operator::PlusMinus), // Token`LongName`PlusMinus
    &prefixUnhandledParselet, // Token`LongName`CenterDot
    &prefixUnhandledParselet, // Token`LongName`Times
    &prefixUnhandledParselet, // Token`LongName`Divide
    &GroupParselet::new(TokenKind::LongName_OpenCurlyQuote, Operator::CurlyQuote), // Token`LongName`OpenCurlyQuote
    &prefixCloserParselet, // Token`LongName`CloseCurlyQuote
    &GroupParselet::new(TokenKind::LongName_OpenCurlyDoubleQuote, Operator::CurlyDoubleQuote), // Token`LongName`OpenCurlyDoubleQuote
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
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_FORALL, Operator::ForAll), // Token`LongName`ForAll
    &prefixUnsupportedTokenParselet, // Token`LongName`PartialD
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_EXISTS, Operator::Exists), // Token`LongName`Exists
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_NOTEXISTS, Operator::NotExists), // Token`LongName`NotExists
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_DEL, Operator::Del), // Token`LongName`Del
    &prefixUnhandledParselet, // Token`LongName`Element
    &prefixUnhandledParselet, // Token`LongName`NotElement
    &prefixUnhandledParselet, // Token`LongName`ReverseElement
    &prefixUnhandledParselet, // Token`LongName`NotReverseElement
    &prefixUnhandledParselet, // Token`LongName`SuchThat
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_PRODUCT, Operator::Product), // Token`LongName`Product
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_COPRODUCT, Operator::Coproduct), // Token`LongName`Coproduct
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_SUM, Operator::Sum), // Token`LongName`Sum
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_MINUS, Operator::Minus), // Token`LongName`Minus
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS, Operator::MinusPlus), // Token`LongName`MinusPlus
    &prefixUnhandledParselet, // Token`LongName`DivisionSlash
    &prefixUnhandledParselet, // Token`LongName`Backslash
    &prefixUnhandledParselet, // Token`LongName`SmallCircle
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_SQRT, Operator::Sqrt), // Token`LongName`Sqrt
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_CUBEROOT, Operator::CubeRoot), // Token`LongName`CubeRoot
    &prefixUnhandledParselet, // Token`LongName`Proportional
    &prefixUnhandledParselet, // Token`LongName`Divides
    &prefixUnhandledParselet, // Token`LongName`DoubleVerticalBar
    &prefixUnhandledParselet, // Token`LongName`NotDoubleVerticalBar
    &prefixUnhandledParselet, // Token`LongName`And
    &prefixUnhandledParselet, // Token`LongName`Or
    &IntegralParselet::new(Operator::Integrate, Operator::Integral), // Token`LongName`Integral
    &IntegralParselet::new(Operator::ContourIntegral, Operator::ContourIntegral), // Token`LongName`ContourIntegral
    &IntegralParselet::new(Operator::DoubleContourIntegral, Operator::DoubleContourIntegral), // Token`LongName`DoubleContourIntegral
    &IntegralParselet::new(Operator::ClockwiseContourIntegral, Operator::ClockwiseContourIntegral), // Token`LongName`ClockwiseContourIntegral
    &IntegralParselet::new(Operator::CounterClockwiseContourIntegral, Operator::CounterClockwiseContourIntegral), // Token`LongName`CounterClockwiseContourIntegral
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
    &PrefixOperatorParselet::new(PRECEDENCE_PREFIX_LONGNAME_CIRCLETIMES, Operator::CircleTimes), // Token`LongName`CircleTimes
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
    &GroupParselet::new(TokenKind::LongName_LeftCeiling, Operator::Ceiling), // Token`LongName`LeftCeiling
    &prefixCloserParselet, // Token`LongName`RightCeiling
    &GroupParselet::new(TokenKind::LongName_LeftFloor, Operator::Floor), // Token`LongName`LeftFloor
    &prefixCloserParselet, // Token`LongName`RightFloor
    &prefixUnhandledParselet, // Token`LongName`Cap
    &prefixUnhandledParselet, // Token`LongName`Cup
    &GroupParselet::new(TokenKind::LongName_LeftAngleBracket, Operator::AngleBracket), // Token`LongName`LeftAngleBracket
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
    &GroupParselet::new(TokenKind::LongName_LeftAssociation, Operator::Association), // Token`LongName`LeftAssociation
    &prefixCloserParselet, // Token`LongName`RightAssociation
    &prefixUnhandledParselet, // Token`LongName`TwoWayRule
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_PIECEWISE, Operator::Piecewise), // Token`LongName`Piecewise
    &prefixUnhandledParselet, // Token`LongName`ImplicitPlus
    &prefixUnsupportedTokenParselet, // Token`LongName`AutoLeftMatch
    &prefixUnsupportedTokenParselet, // Token`LongName`AutoRightMatch
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, Operator::InvisiblePrefixScriptBase), // Token`LongName`InvisiblePrefixScriptBase
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
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_CONTINUEDFRACTIONK, Operator::ContinuedFractionK), // Token`LongName`ContinuedFractionK
    &prefixUnhandledParselet, // Token`LongName`TensorProduct
    &prefixUnhandledParselet, // Token`LongName`TensorWedge
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_PROBABILITYPR, Operator::ProbabilityPr), // Token`LongName`ProbabilityPr
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_EXPECTATIONE, Operator::ExpectationE), // Token`LongName`ExpectationE
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
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_SQUARE, Operator::Square), // Token`LongName`Square
    &prefixUnhandledParselet, // Token`LongName`Rule
    &prefixUnhandledParselet, // Token`LongName`Implies
    &prefixUnhandledParselet, // Token`LongName`ShortRightArrow
    &prefixUnhandledParselet, // Token`LongName`ShortLeftArrow
    &prefixUnhandledParselet, // Token`LongName`ShortUpArrow
    &prefixUnhandledParselet, // Token`LongName`ShortDownArrow
    &prefixUnhandledParselet, // Token`LongName`Application
    &GroupParselet::new(TokenKind::LongName_LeftBracketingBar, Operator::BracketingBar), // Token`LongName`LeftBracketingBar
    &prefixCloserParselet, // Token`LongName`RightBracketingBar
    &GroupParselet::new(TokenKind::LongName_LeftDoubleBracketingBar, Operator::DoubleBracketingBar), // Token`LongName`LeftDoubleBracketingBar
    &prefixCloserParselet, // Token`LongName`RightDoubleBracketingBar
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_CAPITALDIFFERENTIALD, Operator::CapitalDifferentialD), // Token`LongName`CapitalDifferentialD
    &PrefixOperatorParselet::new(PRECEDENCE_LONGNAME_DIFFERENTIALD, Operator::DifferentialD), // Token`LongName`DifferentialD
    &prefixCommaParselet, // Token`LongName`InvisibleComma
    &prefixUnhandledParselet, // Token`LongName`InvisibleApplication
    &prefixUnhandledParselet, // Token`LongName`LongEqual
];

//
//
//
pub(crate) const INFIX_PARSELETS: [InfixParseletPtr; TokenKind::Count.value() as usize] = [
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
    &InfixOperatorParselet::new(PRECEDENCE_DOT, Operator::Dot), // Token`Dot
    &colonParselet, // Token`Colon
    &infixImplicitTimesParselet, // Token`OpenParen
    &infixAssertFalseParselet, // Token`CloseParen
    &(CallParselet::new(&squareGroupParselet)), // Token`OpenSquare
    &infixAssertFalseParselet, // Token`CloseSquare
    &commaParselet, // Token`Comma
    &infixImplicitTimesParselet, // Token`OpenCurly
    &infixAssertFalseParselet, // Token`CloseCurly
    &equalParselet, // Token`Equal
    &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_BANG, Operator::Factorial), // Token`Bang
    &infixImplicitTimesParselet, // Token`Under
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`Less
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`Greater
    &InfixOperatorParselet::new(PRECEDENCE_INFIX_MINUS, Operator::Plus), // Token`Minus
    &InfixOperatorParselet::new(PRECEDENCE_BAR, Operator::Alternatives), // Token`Bar
    &semiParselet, // Token`Semi
    &infixImplicitTimesParselet, // Token`Hash
    &PostfixOperatorParselet::new(PRECEDENCE_AMP, Operator::Function), // Token`Amp
    &BinaryOperatorParselet::new(PRECEDENCE_SLASH, Operator::Divide), // Token`Slash
    &BinaryOperatorParselet::new(PRECEDENCE_AT, Operator::CodeParser_BinaryAt), // Token`At
    &InfixOperatorParselet::new(PRECEDENCE_INFIX_PLUS, Operator::Plus), // Token`Plus
    (&TildeParselet {}), // Token`Tilde
    &timesParselet, // Token`Star
    &BinaryOperatorParselet::new(PRECEDENCE_CARET, Operator::Power), // Token`Caret
    &PostfixOperatorParselet::new(PRECEDENCE_SINGLEQUOTE, Operator::Derivative), // Token`SingleQuote
    &infixImplicitTimesParselet, // Token`Percent
    &BinaryOperatorParselet::new(PRECEDENCE_INFIX_QUESTION, Operator::PatternTest), // Token`Question
    &PostfixOperatorParselet::new(PRECEDENCE_DOTDOT, Operator::Repeated), // Token`DotDot
    (&ColonColonParselet {}), // Token`ColonColon
    &colonEqualParselet, // Token`ColonEqual
    &BinaryOperatorParselet::new(PRECEDENCE_COLONGREATER, Operator::RuleDelayed), // Token`ColonGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`EqualEqual
    &infixImplicitTimesParselet, // Token`UnderUnder
    &infixImplicitTimesParselet, // Token`UnderDot
    &infixImplicitTimesParselet, // Token`LessBar
    &infixImplicitTimesParselet, // Token`LessLess
    &InfixOperatorParselet::new(PRECEDENCE_LESSGREATER, Operator::StringJoin), // Token`LessGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LessEqual
    (&GreaterGreaterParselet {}), // Token`GreaterGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`GreaterEqual
    &BinaryOperatorParselet::new(PRECEDENCE_MINUSGREATER, Operator::Rule), // Token`MinusGreater
    &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_MINUSMINUS, Operator::Decrement), // Token`MinusMinus
    &BinaryOperatorParselet::new(PRECEDENCE_MINUSEQUAL, Operator::SubtractFrom), // Token`MinusEqual
    &InfixOperatorParselet::new(PRECEDENCE_BARBAR, Operator::Or), // Token`BarBar
    &infixAssertFalseParselet, // Token`BarGreater
    &semiSemiParselet, // Token`SemiSemi
    &InfixOperatorParselet::new(PRECEDENCE_AMPAMP, Operator::And), // Token`AmpAmp
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHAT, Operator::Map), // Token`SlashAt
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHSEMI, Operator::Condition), // Token`SlashSemi
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHDOT, Operator::ReplaceAll), // Token`SlashDot
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASH, Operator::CodeParser_BinarySlashSlash), // Token`SlashSlash
    &slashColonParselet, // Token`SlashColon
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHEQUAL, Operator::DivideBy), // Token`SlashEqual
    &InfixOperatorParselet::new(PRECEDENCE_SLASHSTAR, Operator::RightComposition), // Token`SlashStar
    &BinaryOperatorParselet::new(PRECEDENCE_ATAT, Operator::Apply), // Token`AtAt
    &InfixOperatorParselet::new(PRECEDENCE_ATSTAR, Operator::Composition), // Token`AtStar
    &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_PLUSPLUS, Operator::Increment), // Token`PlusPlus
    &BinaryOperatorParselet::new(PRECEDENCE_PLUSEQUAL, Operator::AddTo), // Token`PlusEqual
    &InfixOperatorParselet::new(PRECEDENCE_TILDETILDE, Operator::StringExpression), // Token`TildeTilde
    &BinaryOperatorParselet::new(PRECEDENCE_STAREQUAL, Operator::TimesBy), // Token`StarEqual
    &InfixOperatorParselet::new(PRECEDENCE_STARSTAR, Operator::NonCommutativeMultiply), // Token`StarStar
    &BinaryOperatorParselet::new(PRECEDENCE_CARETEQUAL, Operator::UpSet), // Token`CaretEqual
    &infixImplicitTimesParselet, // Token`HashHash
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`BangEqual
    &PostfixOperatorParselet::new(PRECEDENCE_POSTFIX_BANGBANG, Operator::Factorial2), // Token`BangBang
    &infixAssertFalseParselet, // Token`QuestionQuestion
    &PostfixOperatorParselet::new(PRECEDENCE_DOTDOTDOT, Operator::RepeatedNull), // Token`DotDotDot
    &InfixOperatorParselet::new(PRECEDENCE_EQUALEQUALEQUAL, Operator::SameQ), // Token`EqualEqualEqual
    &InfixOperatorParselet::new(PRECEDENCE_EQUALBANGEQUAL, Operator::UnsameQ), // Token`EqualBangEqual
    &infixImplicitTimesParselet, // Token`UnderUnderUnder
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASHDOT, Operator::ReplaceRepeated), // Token`SlashSlashDot
    &BinaryOperatorParselet::new(PRECEDENCE_ATATAT, Operator::MapApply), // Token`AtAtAt
    &BinaryOperatorParselet::new(PRECEDENCE_LESSMINUSGREATER, Operator::TwoWayRule), // Token`LessMinusGreater
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASHAT, Operator::MapAll), // Token`SlashSlashAt
    &BinaryOperatorParselet::new(PRECEDENCE_CARETCOLONEQUAL, Operator::UpSetDelayed), // Token`CaretColonEqual
    (&GreaterGreaterGreaterParselet {}), // Token`GreaterGreaterGreater
    &BinaryOperatorParselet::new(PRECEDENCE_BARMINUSGREATER, Operator::Function), // Token`BarMinusGreater
    &BinaryOperatorParselet::new(PRECEDENCE_SLASHSLASHEQUAL, Operator::ApplyTo), // Token`SlashSlashEqual
    &(CallParselet::new(&GroupParselet::new(TokenKind::ColonColonOpenSquare, Operator::CodeParser_GroupTypeSpecifier))), // Token`ColonColonOpenSquare
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
    &BinaryOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_PLUSMINUS, Operator::PlusMinus), // Token`LongName`PlusMinus
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CENTERDOT, Operator::CenterDot), // Token`LongName`CenterDot
    &timesParselet, // Token`LongName`Times
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DIVIDE, Operator::Divide), // Token`LongName`Divide
    &infixImplicitTimesParselet, // Token`LongName`OpenCurlyQuote
    &infixAssertFalseParselet, // Token`LongName`CloseCurlyQuote
    &infixImplicitTimesParselet, // Token`LongName`OpenCurlyDoubleQuote
    &infixAssertFalseParselet, // Token`LongName`CloseCurlyDoubleQuote
    &timesParselet, // Token`LongName`InvisibleTimes
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::LeftArrow), // Token`LongName`LeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::UpArrow), // Token`LongName`UpArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::RightArrow), // Token`LongName`RightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DownArrow), // Token`LongName`DownArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::LeftRightArrow), // Token`LongName`LeftRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::UpDownArrow), // Token`LongName`UpDownArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, Operator::UpperLeftArrow), // Token`LongName`UpperLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, Operator::UpperRightArrow), // Token`LongName`UpperRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, Operator::LowerRightArrow), // Token`LongName`LowerRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_DIAGONALARROWOPERATORS, Operator::LowerLeftArrow), // Token`LongName`LowerLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::LeftTeeArrow), // Token`LongName`LeftTeeArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::UpTeeArrow), // Token`LongName`UpTeeArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::RightTeeArrow), // Token`LongName`RightTeeArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DownTeeArrow), // Token`LongName`DownTeeArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::LeftVector), // Token`LongName`LeftVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownLeftVector), // Token`LongName`DownLeftVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightUpVector), // Token`LongName`RightUpVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftUpVector), // Token`LongName`LeftUpVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::RightVector), // Token`LongName`RightVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownRightVector), // Token`LongName`DownRightVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightDownVector), // Token`LongName`RightDownVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftDownVector), // Token`LongName`LeftDownVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::RightArrowLeftArrow), // Token`LongName`RightArrowLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::UpArrowDownArrow), // Token`LongName`UpArrowDownArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::LeftArrowRightArrow), // Token`LongName`LeftArrowRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::ReverseEquilibrium), // Token`LongName`ReverseEquilibrium
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Equilibrium), // Token`LongName`Equilibrium
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::DoubleLeftArrow), // Token`LongName`DoubleLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DoubleUpArrow), // Token`LongName`DoubleUpArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::DoubleRightArrow), // Token`LongName`DoubleRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DoubleDownArrow), // Token`LongName`DoubleDownArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::DoubleLeftRightArrow), // Token`LongName`DoubleLeftRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DoubleUpDownArrow), // Token`LongName`DoubleUpDownArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::LeftArrowBar), // Token`LongName`LeftArrowBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::RightArrowBar), // Token`LongName`RightArrowBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DownArrowUpArrow), // Token`LongName`DownArrowUpArrow
    &infixImplicitTimesParselet, // Token`LongName`ForAll
    &infixAssertFalseParselet, // Token`LongName`PartialD
    &infixImplicitTimesParselet, // Token`LongName`Exists
    &infixImplicitTimesParselet, // Token`LongName`NotExists
    &infixImplicitTimesParselet, // Token`LongName`Del
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::Element), // Token`LongName`Element
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotElement), // Token`LongName`NotElement
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::ReverseElement), // Token`LongName`ReverseElement
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotReverseElement), // Token`LongName`NotReverseElement
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_SUCHTHAT, Operator::SuchThat), // Token`LongName`SuchThat
    &infixImplicitTimesParselet, // Token`LongName`Product
    &InfixOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_COPRODUCT, Operator::Coproduct), // Token`LongName`Coproduct
    &infixImplicitTimesParselet, // Token`LongName`Sum
    &InfixOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_MINUS, Operator::Plus), // Token`LongName`Minus
    &BinaryOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_MINUSPLUS, Operator::MinusPlus), // Token`LongName`MinusPlus
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DIVISIONSLASH, Operator::Divide), // Token`LongName`DivisionSlash
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_BACKSLASH, Operator::Backslash), // Token`LongName`Backslash
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_SMALLCIRCLE, Operator::SmallCircle), // Token`LongName`SmallCircle
    &infixImplicitTimesParselet, // Token`LongName`Sqrt
    &infixImplicitTimesParselet, // Token`LongName`CubeRoot
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Proportional), // Token`LongName`Proportional
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_DIVIDES, Operator::Divisible), // Token`LongName`Divides
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_DOUBLEVERTICALBAR, Operator::DoubleVerticalBar), // Token`LongName`DoubleVerticalBar
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NOTDOUBLEVERTICALBAR, Operator::NotDoubleVerticalBar), // Token`LongName`NotDoubleVerticalBar
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_AND, Operator::And), // Token`LongName`And
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_OR, Operator::Or), // Token`LongName`Or
    &infixImplicitTimesParselet, // Token`LongName`Integral
    &infixImplicitTimesParselet, // Token`LongName`ContourIntegral
    &infixImplicitTimesParselet, // Token`LongName`DoubleContourIntegral
    &infixImplicitTimesParselet, // Token`LongName`ClockwiseContourIntegral
    &infixImplicitTimesParselet, // Token`LongName`CounterClockwiseContourIntegral
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_THEREFORE, Operator::Therefore), // Token`LongName`Therefore
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_BECAUSE, Operator::Because), // Token`LongName`Because
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_COLON, Operator::Colon), // Token`LongName`Colon
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Proportion), // Token`LongName`Proportion
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Tilde), // Token`LongName`Tilde
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VERTICALTILDE, Operator::VerticalTilde), // Token`LongName`VerticalTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotTilde), // Token`LongName`NotTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::EqualTilde), // Token`LongName`EqualTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::TildeEqual), // Token`LongName`TildeEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotTildeEqual), // Token`LongName`NotTildeEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::TildeFullEqual), // Token`LongName`TildeFullEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotTildeFullEqual), // Token`LongName`NotTildeFullEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::TildeTilde), // Token`LongName`TildeTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotTildeTilde), // Token`LongName`NotTildeTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::CupCap), // Token`LongName`CupCap
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::HumpDownHump), // Token`LongName`HumpDownHump
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::HumpEqual), // Token`LongName`HumpEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::DotEqual), // Token`LongName`DotEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Congruent), // Token`LongName`Congruent
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotCongruent), // Token`LongName`NotCongruent
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessFullEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterFullEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLessFullEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreaterFullEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotCupCap), // Token`LongName`NotCupCap
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLessEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreaterEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLessTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreaterTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLessGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreaterLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Precedes), // Token`LongName`Precedes
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::Succeeds), // Token`LongName`Succeeds
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::PrecedesSlantEqual), // Token`LongName`PrecedesSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::SucceedsSlantEqual), // Token`LongName`SucceedsSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::PrecedesTilde), // Token`LongName`PrecedesTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::SucceedsTilde), // Token`LongName`SucceedsTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotPrecedes), // Token`LongName`NotPrecedes
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotSucceeds), // Token`LongName`NotSucceeds
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::Subset), // Token`LongName`Subset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::Superset), // Token`LongName`Superset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSubset), // Token`LongName`NotSubset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSuperset), // Token`LongName`NotSuperset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::SubsetEqual), // Token`LongName`SubsetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::SupersetEqual), // Token`LongName`SupersetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSubsetEqual), // Token`LongName`NotSubsetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSupersetEqual), // Token`LongName`NotSupersetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_UNIONOPERATORS, Operator::UnionPlus), // Token`LongName`UnionPlus
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::SquareSubset), // Token`LongName`SquareSubset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::SquareSuperset), // Token`LongName`SquareSuperset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::SquareSubsetEqual), // Token`LongName`SquareSubsetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::SquareSupersetEqual), // Token`LongName`SquareSupersetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INTERSECTIONOPERATORS, Operator::SquareIntersection), // Token`LongName`SquareIntersection
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_UNIONOPERATORS, Operator::SquareUnion), // Token`LongName`SquareUnion
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CIRCLEPLUS, Operator::CirclePlus), // Token`LongName`CirclePlus
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_CIRCLEMINUS, Operator::CircleMinus), // Token`LongName`CircleMinus
    &InfixOperatorParselet::new(PRECEDENCE_INFIX_LONGNAME_CIRCLETIMES, Operator::CircleTimes), // Token`LongName`CircleTimes
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CIRCLEDOT, Operator::CircleDot), // Token`LongName`CircleDot
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_RIGHTTEE, Operator::RightTee), // Token`LongName`RightTee
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_LEFTTEE, Operator::LeftTee), // Token`LongName`LeftTee
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DOWNTEE, Operator::DownTee), // Token`LongName`DownTee
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_UPTEE, Operator::UpTee), // Token`LongName`UpTee
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DOUBLERIGHTTEE, Operator::DoubleRightTee), // Token`LongName`DoubleRightTee
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::LeftTriangle), // Token`LongName`LeftTriangle
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::RightTriangle), // Token`LongName`RightTriangle
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::LeftTriangleEqual), // Token`LongName`LeftTriangleEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::RightTriangleEqual), // Token`LongName`RightTriangleEqual
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_XOR, Operator::Xor), // Token`LongName`Xor
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NAND, Operator::Nand), // Token`LongName`Nand
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NOR, Operator::Nor), // Token`LongName`Nor
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_WEDGE, Operator::Wedge), // Token`LongName`Wedge
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VEE, Operator::Vee), // Token`LongName`Vee
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INTERSECTIONOPERATORS, Operator::Intersection), // Token`LongName`Intersection
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_UNIONOPERATORS, Operator::Union), // Token`LongName`Union
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_DIAMOND, Operator::Diamond), // Token`LongName`Diamond
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_STAR, Operator::Star), // Token`LongName`Star
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessEqualGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterEqualLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotPrecedesSlantEqual), // Token`LongName`NotPrecedesSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotSucceedsSlantEqual), // Token`LongName`NotSucceedsSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSquareSubsetEqual), // Token`LongName`NotSquareSubsetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSquareSupersetEqual), // Token`LongName`NotSquareSupersetEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotPrecedesTilde), // Token`LongName`NotPrecedesTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotSucceedsTilde), // Token`LongName`NotSucceedsTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotLeftTriangle), // Token`LongName`NotLeftTriangle
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotRightTriangle), // Token`LongName`NotRightTriangle
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotLeftTriangleEqual), // Token`LongName`NotLeftTriangleEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotRightTriangleEqual), // Token`LongName`NotRightTriangleEqual
    &infixImplicitTimesParselet, // Token`LongName`LeftCeiling
    &infixAssertFalseParselet, // Token`LongName`RightCeiling
    &infixImplicitTimesParselet, // Token`LongName`LeftFloor
    &infixAssertFalseParselet, // Token`LongName`RightFloor
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CAP, Operator::Cap), // Token`LongName`Cap
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CUP, Operator::Cup), // Token`LongName`Cup
    &infixImplicitTimesParselet, // Token`LongName`LeftAngleBracket
    &infixAssertFalseParselet, // Token`LongName`RightAngleBracket
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_PERPENDICULAR, Operator::Perpendicular), // Token`LongName`Perpendicular
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::LongLeftArrow), // Token`LongName`LongLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::LongRightArrow), // Token`LongName`LongRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::LongLeftRightArrow), // Token`LongName`LongLeftRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DoubleLongLeftArrow), // Token`LongName`DoubleLongLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DoubleLongRightArrow), // Token`LongName`DoubleLongRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DoubleLongLeftRightArrow), // Token`LongName`DoubleLongLeftRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::UpArrowBar), // Token`LongName`UpArrowBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::DownArrowBar), // Token`LongName`DownArrowBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::LeftRightVector), // Token`LongName`LeftRightVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightUpDownVector), // Token`LongName`RightUpDownVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownLeftRightVector), // Token`LongName`DownLeftRightVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftUpDownVector), // Token`LongName`LeftUpDownVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::LeftVectorBar), // Token`LongName`LeftVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::RightVectorBar), // Token`LongName`RightVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightUpVectorBar), // Token`LongName`RightUpVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightDownVectorBar), // Token`LongName`RightDownVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownLeftVectorBar), // Token`LongName`DownLeftVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownRightVectorBar), // Token`LongName`DownRightVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftUpVectorBar), // Token`LongName`LeftUpVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftDownVectorBar), // Token`LongName`LeftDownVectorBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::LeftTeeVector), // Token`LongName`LeftTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::RightTeeVector), // Token`LongName`RightTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightUpTeeVector), // Token`LongName`RightUpTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::RightDownTeeVector), // Token`LongName`RightDownTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownLeftTeeVector), // Token`LongName`DownLeftTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VECTOROPERATORS, Operator::DownRightTeeVector), // Token`LongName`DownRightTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftUpTeeVector), // Token`LongName`LeftUpTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::LeftDownTeeVector), // Token`LongName`LeftDownTeeVector
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::UpEquilibrium), // Token`LongName`UpEquilibrium
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALVECTOROPERATORS, Operator::ReverseUpEquilibrium), // Token`LongName`ReverseUpEquilibrium
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_ROUNDIMPLIES, Operator::RoundImplies), // Token`LongName`RoundImplies
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::LeftTriangleBar), // Token`LongName`LeftTriangleBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::RightTriangleBar), // Token`LongName`RightTriangleBar
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_EQUIVALENT, Operator::Equivalent), // Token`LongName`Equivalent
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LessSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`GreaterSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NestedLessLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NestedGreaterGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::PrecedesEqual), // Token`LongName`PrecedesEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::SucceedsEqual), // Token`LongName`SucceedsEqual
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DOUBLELEFTTEE, Operator::DoubleLeftTee), // Token`LongName`DoubleLeftTee
    &(CallParselet::new(&doubleBracketGroupParselet)), // Token`LongName`LeftDoubleBracket
    &infixAssertFalseParselet, // Token`LongName`RightDoubleBracket
    &infixImplicitTimesParselet, // Token`LongName`LeftAssociation
    &infixAssertFalseParselet, // Token`LongName`RightAssociation
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_TWOWAYRULE, Operator::TwoWayRule), // Token`LongName`TwoWayRule
    &infixImplicitTimesParselet, // Token`LongName`Piecewise
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_IMPLICITPLUS, Operator::Plus), // Token`LongName`ImplicitPlus
    &infixAssertFalseParselet, // Token`LongName`AutoLeftMatch
    &infixAssertFalseParselet, // Token`LongName`AutoRightMatch
    &infixImplicitTimesParselet, // Token`LongName`InvisiblePrefixScriptBase
    &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, Operator::InvisiblePostfixScriptBase), // Token`LongName`InvisiblePostfixScriptBase
    &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_TRANSPOSE, Operator::Transpose), // Token`LongName`Transpose
    &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_CONJUGATE, Operator::Conjugate), // Token`LongName`Conjugate
    &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE, Operator::ConjugateTranspose), // Token`LongName`ConjugateTranspose
    &PostfixOperatorParselet::new(PRECEDENCE_LONGNAME_HERMITIANCONJUGATE, Operator::HermitianConjugate), // Token`LongName`HermitianConjugate
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VERTICALBAR, Operator::VerticalBar), // Token`LongName`VerticalBar
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_NOTVERTICALBAR, Operator::NotVerticalBar), // Token`LongName`NotVerticalBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::Distributed), // Token`LongName`Distributed
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CONDITIONED, Operator::Conditioned), // Token`LongName`Conditioned
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, Operator::UndirectedEdge), // Token`LongName`UndirectedEdge
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_DIRECTEDEDGE, Operator::DirectedEdge), // Token`LongName`DirectedEdge
    &infixImplicitTimesParselet, // Token`LongName`ContinuedFractionK
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_TENSORPRODUCT, Operator::TensorProduct), // Token`LongName`TensorProduct
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_TENSORWEDGE, Operator::TensorWedge), // Token`LongName`TensorWedge
    &infixImplicitTimesParselet, // Token`LongName`ProbabilityPr
    &infixImplicitTimesParselet, // Token`LongName`ExpectationE
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_PERMUTATIONPRODUCT, Operator::PermutationProduct), // Token`LongName`PermutationProduct
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotEqualTilde), // Token`LongName`NotEqualTilde
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotHumpEqual), // Token`LongName`NotHumpEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotHumpDownHump), // Token`LongName`NotHumpDownHump
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotLeftTriangleBar), // Token`LongName`NotLeftTriangleBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotRightTriangleBar), // Token`LongName`NotRightTriangleBar
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLessLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotNestedLessLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotLessSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreaterGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotNestedGreaterGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`NotGreaterSlantEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotPrecedesEqual), // Token`LongName`NotPrecedesEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_ORDERINGOPERATORS, Operator::NotSucceedsEqual), // Token`LongName`NotSucceedsEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSquareSubset), // Token`LongName`NotSquareSubset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_SETRELATIONS, Operator::NotSquareSuperset), // Token`LongName`NotSquareSuperset
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`Equal
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_VERTICALSEPARATOR, Operator::VerticalSeparator), // Token`LongName`VerticalSeparator
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`VectorGreater
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`VectorGreaterEqual
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`VectorLess
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`VectorLessEqual
    &infixAssertFalseParselet, // Token`LongName`Limit
    &infixAssertFalseParselet, // Token`LongName`MaxLimit
    &infixAssertFalseParselet, // Token`LongName`MinLimit
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_CROSS, Operator::Cross), // Token`LongName`Cross
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_FUNCTION, Operator::Function), // Token`LongName`Function
    &InfixOperatorParselet::new(PRECEDENCE_LONGNAME_XNOR, Operator::Xnor), // Token`LongName`Xnor
    &infixAssertFalseParselet, // Token`LongName`DiscreteShift
    &infixAssertFalseParselet, // Token`LongName`DifferenceDelta
    &infixAssertFalseParselet, // Token`LongName`DiscreteRatio
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_RULEDELAYED, Operator::RuleDelayed), // Token`LongName`RuleDelayed
    &infixImplicitTimesParselet, // Token`LongName`Square
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_RULE, Operator::Rule), // Token`LongName`Rule
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_IMPLIES, Operator::Implies), // Token`LongName`Implies
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::ShortRightArrow), // Token`LongName`ShortRightArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_HORIZONTALARROWS, Operator::ShortLeftArrow), // Token`LongName`ShortLeftArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::ShortUpArrow), // Token`LongName`ShortUpArrow
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_VERTICALARROWOPERATORS, Operator::ShortDownArrow), // Token`LongName`ShortDownArrow
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_APPLICATION, Operator::Application), // Token`LongName`Application
    &infixImplicitTimesParselet, // Token`LongName`LeftBracketingBar
    &infixAssertFalseParselet, // Token`LongName`RightBracketingBar
    &infixImplicitTimesParselet, // Token`LongName`LeftDoubleBracketingBar
    &infixAssertFalseParselet, // Token`LongName`RightDoubleBracketingBar
    &infixDifferentialDParselet, // Token`LongName`CapitalDifferentialD
    &infixDifferentialDParselet, // Token`LongName`DifferentialD
    &commaParselet, // Token`LongName`InvisibleComma
    &BinaryOperatorParselet::new(PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, Operator::CodeParser_BinaryAt), // Token`LongName`InvisibleApplication
    &InfixOperatorParselet::new(PRECEDENCE_CLASS_INEQUALITY, Operator::CodeParser_InfixInequality), // Token`LongName`LongEqual
];

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Times,
    Span,
    Pattern,
    Optional,
    Blank,
    BlankSequence,
    BlankNullSequence,
    Set,
    SetDelayed,
    Unset,
    TagSet,
    TagSetDelayed,
    TagUnset,
    CompoundExpression,
    MessageName,
    Put,
    PutAppend,
    Get,
    Slot,
    SlotSequence,
    Out,
    Token_Comment,
    CodeParser_InternalInvalid,
    CodeParser_Comma,
    CodeParser_PatternBlank,
    CodeParser_PatternBlankSequence,
    CodeParser_PatternBlankNullSequence,
    CodeParser_PatternOptionalDefault,
    CodeParser_TernaryTilde,
    CodeParser_TernaryOptionalPattern,
    CodeParser_InfixTilde,
    Minus,
    Plus,
    Not,
    PreIncrement,
    PreDecrement,
    CodeParser_PrefixNot2,
    PlusMinus,
    Sum,
    Sqrt,
    MinusPlus,
    DifferentialD,
    CapitalDifferentialD,
    Del,
    Square,
    Product,
    ContinuedFractionK,
    CircleTimes,
    ForAll,
    Exists,
    NotExists,
    Coproduct,
    Piecewise,
    InvisiblePrefixScriptBase,
    ExpectationE,
    CubeRoot,
    ProbabilityPr,
    CodeParser_PrefixLinearSyntaxBang,
    CodeParser_GroupParen,
    CodeParser_GroupSquare,
    List,
    Association,
    CodeParser_GroupTypeSpecifier,
    AngleBracket,
    Ceiling,
    Floor,
    CodeParser_GroupDoubleBracket,
    BracketingBar,
    DoubleBracketingBar,
    CurlyQuote,
    CurlyDoubleQuote,
    Integrate,
    Integral,
    ContourIntegral,
    DoubleContourIntegral,
    ClockwiseContourIntegral,
    CounterClockwiseContourIntegral,
    Divide,
    Power,
    UpSet,
    UpSetDelayed,
    Map,
    Rule,
    Apply,
    Condition,
    ReplaceAll,
    RuleDelayed,
    ReplaceRepeated,
    AddTo,
    TimesBy,
    SubtractFrom,
    DivideBy,
    TwoWayRule,
    MapAll,
    CodeParser_BinaryAt,
    MapApply,
    CodeParser_BinarySlashSlash,
    PatternTest,
    Function,
    ApplyTo,
    Implies,
    RoundImplies,
    DirectedEdge,
    UndirectedEdge,
    CircleMinus,
    SuchThat,
    Perpendicular,
    Because,
    Therefore,
    RightTee,
    LeftTee,
    DoubleRightTee,
    DoubleLeftTee,
    UpTee,
    DownTee,
    Application,
    SameQ,
    UnsameQ,
    Dot,
    NonCommutativeMultiply,
    And,
    Or,
    Alternatives,
    StringJoin,
    StringExpression,
    Composition,
    RightComposition,
    Element,
    Subset,
    Superset,
    SubsetEqual,
    SupersetEqual,
    NotElement,
    NotSubset,
    NotSuperset,
    NotSubsetEqual,
    NotSupersetEqual,
    SquareSubset,
    SquareSuperset,
    NotSquareSubset,
    NotSquareSuperset,
    SquareSubsetEqual,
    SquareSupersetEqual,
    NotSquareSubsetEqual,
    NotSquareSupersetEqual,
    ReverseElement,
    NotReverseElement,
    Distributed,
    Xor,
    Nand,
    Nor,
    LeftArrow,
    RightArrow,
    LeftRightArrow,
    LeftTeeArrow,
    RightTeeArrow,
    RightArrowLeftArrow,
    LeftArrowRightArrow,
    DoubleLeftArrow,
    DoubleRightArrow,
    DoubleLeftRightArrow,
    LeftArrowBar,
    RightArrowBar,
    ShortRightArrow,
    ShortLeftArrow,
    UpperLeftArrow,
    UpperRightArrow,
    LowerRightArrow,
    LowerLeftArrow,
    LeftVector,
    RightVector,
    LeftRightVector,
    LeftVectorBar,
    RightVectorBar,
    LeftTeeVector,
    RightTeeVector,
    DownLeftVector,
    DownRightVector,
    DownLeftRightVector,
    DownLeftVectorBar,
    DownRightVectorBar,
    DownLeftTeeVector,
    DownRightTeeVector,
    UpArrow,
    DownArrow,
    UpDownArrow,
    UpTeeArrow,
    DownTeeArrow,
    UpArrowDownArrow,
    DoubleUpArrow,
    DoubleDownArrow,
    DoubleUpDownArrow,
    DownArrowUpArrow,
    LongLeftArrow,
    LongRightArrow,
    LongLeftRightArrow,
    DoubleLongLeftArrow,
    DoubleLongRightArrow,
    DoubleLongLeftRightArrow,
    UpArrowBar,
    DownArrowBar,
    ShortUpArrow,
    ShortDownArrow,
    RightUpVector,
    LeftUpVector,
    RightDownVector,
    LeftDownVector,
    RightUpDownVector,
    LeftUpDownVector,
    RightUpVectorBar,
    RightDownVectorBar,
    LeftUpVectorBar,
    LeftDownVectorBar,
    RightUpTeeVector,
    RightDownTeeVector,
    LeftUpTeeVector,
    LeftDownTeeVector,
    UpEquilibrium,
    ReverseUpEquilibrium,
    CenterDot,
    Equivalent,
    CircleDot,
    Conditioned,
    Union,
    SquareUnion,
    UnionPlus,
    Intersection,
    SquareIntersection,
    TensorWedge,
    TensorProduct,
    Cross,
    SmallCircle,
    Divisible,
    VerticalSeparator,
    Backslash,
    Diamond,
    Wedge,
    Vee,
    Star,
    VerticalTilde,
    Cap,
    Cup,
    CirclePlus,
    VerticalBar,
    DoubleVerticalBar,
    NotVerticalBar,
    NotDoubleVerticalBar,
    LeftTriangle,
    RightTriangle,
    NotLeftTriangle,
    NotRightTriangle,
    LeftTriangleEqual,
    RightTriangleEqual,
    NotLeftTriangleEqual,
    NotRightTriangleEqual,
    LeftTriangleBar,
    RightTriangleBar,
    NotLeftTriangleBar,
    NotRightTriangleBar,
    TildeEqual,
    NotTildeEqual,
    TildeFullEqual,
    NotTildeFullEqual,
    Tilde,
    NotTilde,
    EqualTilde,
    NotEqualTilde,
    TildeTilde,
    NotTildeTilde,
    Proportional,
    Proportion,
    Congruent,
    NotCongruent,
    Equilibrium,
    ReverseEquilibrium,
    DotEqual,
    Precedes,
    Succeeds,
    PrecedesEqual,
    SucceedsEqual,
    PrecedesTilde,
    SucceedsTilde,
    PrecedesSlantEqual,
    SucceedsSlantEqual,
    NotPrecedes,
    NotSucceeds,
    NotPrecedesEqual,
    NotSucceedsEqual,
    NotPrecedesTilde,
    NotSucceedsTilde,
    NotPrecedesSlantEqual,
    NotSucceedsSlantEqual,
    CupCap,
    NotCupCap,
    HumpEqual,
    HumpDownHump,
    NotHumpEqual,
    NotHumpDownHump,
    CodeParser_InfixInequality,
    PermutationProduct,
    Colon,
    Xnor,
    Repeated,
    Factorial,
    Decrement,
    Increment,
    RepeatedNull,
    Factorial2,
    Derivative,
    Transpose,
    Conjugate,
    ConjugateTranspose,
    HermitianConjugate,
    InvisiblePostfixScriptBase,
}

impl Operator {
    #[allow(dead_code)]
    pub(crate) fn to_symbol(self) -> Symbol {
        match self {
            Operator::Times => SYMBOL_TIMES,
            Operator::Span => SYMBOL_SPAN,
            Operator::Pattern => SYMBOL_PATTERN,
            Operator::Optional => SYMBOL_OPTIONAL,
            Operator::Blank => SYMBOL_BLANK,
            Operator::BlankSequence => SYMBOL_BLANKSEQUENCE,
            Operator::BlankNullSequence => SYMBOL_BLANKNULLSEQUENCE,
            Operator::Set => SYMBOL_SET,
            Operator::SetDelayed => SYMBOL_SETDELAYED,
            Operator::Unset => SYMBOL_UNSET,
            Operator::TagSet => SYMBOL_TAGSET,
            Operator::TagSetDelayed => SYMBOL_TAGSETDELAYED,
            Operator::TagUnset => SYMBOL_TAGUNSET,
            Operator::CompoundExpression => SYMBOL_COMPOUNDEXPRESSION,
            Operator::MessageName => SYMBOL_MESSAGENAME,
            Operator::Put => SYMBOL_PUT,
            Operator::PutAppend => SYMBOL_PUTAPPEND,
            Operator::Get => SYMBOL_GET,
            Operator::Slot => SYMBOL_SLOT,
            Operator::SlotSequence => SYMBOL_SLOTSEQUENCE,
            Operator::Out => SYMBOL_OUT,
            Operator::Token_Comment => SYMBOL_TOKEN_COMMENT,
            Operator::CodeParser_InternalInvalid => SYMBOL_CODEPARSER_INTERNALINVALID,
            Operator::CodeParser_Comma => SYMBOL_CODEPARSER_COMMA,
            Operator::CodeParser_PatternBlank => SYMBOL_CODEPARSER_PATTERNBLANK,
            Operator::CodeParser_PatternBlankSequence => SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE,
            Operator::CodeParser_PatternBlankNullSequence => SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE,
            Operator::CodeParser_PatternOptionalDefault => SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT,
            Operator::CodeParser_TernaryTilde => SYMBOL_CODEPARSER_TERNARYTILDE,
            Operator::CodeParser_TernaryOptionalPattern => SYMBOL_CODEPARSER_TERNARYOPTIONALPATTERN,
            Operator::CodeParser_InfixTilde => SYMBOL_CODEPARSER_INFIXTILDE,
            Operator::Minus => SYMBOL_MINUS,
            Operator::Plus => SYMBOL_PLUS,
            Operator::Not => SYMBOL_NOT,
            Operator::PreIncrement => SYMBOL_PREINCREMENT,
            Operator::PreDecrement => SYMBOL_PREDECREMENT,
            Operator::CodeParser_PrefixNot2 => SYMBOL_CODEPARSER_PREFIXNOT2,
            Operator::PlusMinus => SYMBOL_PLUSMINUS,
            Operator::Sum => SYMBOL_SUM,
            Operator::Sqrt => SYMBOL_SQRT,
            Operator::MinusPlus => SYMBOL_MINUSPLUS,
            Operator::DifferentialD => SYMBOL_DIFFERENTIALD,
            Operator::CapitalDifferentialD => SYMBOL_CAPITALDIFFERENTIALD,
            Operator::Del => SYMBOL_DEL,
            Operator::Square => SYMBOL_SQUARE,
            Operator::Product => SYMBOL_PRODUCT,
            Operator::ContinuedFractionK => SYMBOL_CONTINUEDFRACTIONK,
            Operator::CircleTimes => SYMBOL_CIRCLETIMES,
            Operator::ForAll => SYMBOL_FORALL,
            Operator::Exists => SYMBOL_EXISTS,
            Operator::NotExists => SYMBOL_NOTEXISTS,
            Operator::Coproduct => SYMBOL_COPRODUCT,
            Operator::Piecewise => SYMBOL_PIECEWISE,
            Operator::InvisiblePrefixScriptBase => SYMBOL_INVISIBLEPREFIXSCRIPTBASE,
            Operator::ExpectationE => SYMBOL_EXPECTATIONE,
            Operator::CubeRoot => SYMBOL_CUBEROOT,
            Operator::ProbabilityPr => SYMBOL_PROBABILITYPR,
            Operator::CodeParser_PrefixLinearSyntaxBang => SYMBOL_CODEPARSER_PREFIXLINEARSYNTAXBANG,
            Operator::CodeParser_GroupParen => SYMBOL_CODEPARSER_GROUPPAREN,
            Operator::CodeParser_GroupSquare => SYMBOL_CODEPARSER_GROUPSQUARE,
            Operator::List => SYMBOL_LIST,
            Operator::Association => SYMBOL_ASSOCIATION,
            Operator::CodeParser_GroupTypeSpecifier => SYMBOL_CODEPARSER_GROUPTYPESPECIFIER,
            Operator::AngleBracket => SYMBOL_ANGLEBRACKET,
            Operator::Ceiling => SYMBOL_CEILING,
            Operator::Floor => SYMBOL_FLOOR,
            Operator::CodeParser_GroupDoubleBracket => SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET,
            Operator::BracketingBar => SYMBOL_BRACKETINGBAR,
            Operator::DoubleBracketingBar => SYMBOL_DOUBLEBRACKETINGBAR,
            Operator::CurlyQuote => SYMBOL_CURLYQUOTE,
            Operator::CurlyDoubleQuote => SYMBOL_CURLYDOUBLEQUOTE,
            Operator::Integrate => SYMBOL_INTEGRATE,
            Operator::Integral => SYMBOL_INTEGRAL,
            Operator::ContourIntegral => SYMBOL_CONTOURINTEGRAL,
            Operator::DoubleContourIntegral => SYMBOL_DOUBLECONTOURINTEGRAL,
            Operator::ClockwiseContourIntegral => SYMBOL_CLOCKWISECONTOURINTEGRAL,
            Operator::CounterClockwiseContourIntegral => SYMBOL_COUNTERCLOCKWISECONTOURINTEGRAL,
            Operator::Divide => SYMBOL_DIVIDE,
            Operator::Power => SYMBOL_POWER,
            Operator::UpSet => SYMBOL_UPSET,
            Operator::UpSetDelayed => SYMBOL_UPSETDELAYED,
            Operator::Map => SYMBOL_MAP,
            Operator::Rule => SYMBOL_RULE,
            Operator::Apply => SYMBOL_APPLY,
            Operator::Condition => SYMBOL_CONDITION,
            Operator::ReplaceAll => SYMBOL_REPLACEALL,
            Operator::RuleDelayed => SYMBOL_RULEDELAYED,
            Operator::ReplaceRepeated => SYMBOL_REPLACEREPEATED,
            Operator::AddTo => SYMBOL_ADDTO,
            Operator::TimesBy => SYMBOL_TIMESBY,
            Operator::SubtractFrom => SYMBOL_SUBTRACTFROM,
            Operator::DivideBy => SYMBOL_DIVIDEBY,
            Operator::TwoWayRule => SYMBOL_TWOWAYRULE,
            Operator::MapAll => SYMBOL_MAPALL,
            Operator::CodeParser_BinaryAt => SYMBOL_CODEPARSER_BINARYAT,
            Operator::MapApply => SYMBOL_MAPAPPLY,
            Operator::CodeParser_BinarySlashSlash => SYMBOL_CODEPARSER_BINARYSLASHSLASH,
            Operator::PatternTest => SYMBOL_PATTERNTEST,
            Operator::Function => SYMBOL_FUNCTION,
            Operator::ApplyTo => SYMBOL_APPLYTO,
            Operator::Implies => SYMBOL_IMPLIES,
            Operator::RoundImplies => SYMBOL_ROUNDIMPLIES,
            Operator::DirectedEdge => SYMBOL_DIRECTEDEDGE,
            Operator::UndirectedEdge => SYMBOL_UNDIRECTEDEDGE,
            Operator::CircleMinus => SYMBOL_CIRCLEMINUS,
            Operator::SuchThat => SYMBOL_SUCHTHAT,
            Operator::Perpendicular => SYMBOL_PERPENDICULAR,
            Operator::Because => SYMBOL_BECAUSE,
            Operator::Therefore => SYMBOL_THEREFORE,
            Operator::RightTee => SYMBOL_RIGHTTEE,
            Operator::LeftTee => SYMBOL_LEFTTEE,
            Operator::DoubleRightTee => SYMBOL_DOUBLERIGHTTEE,
            Operator::DoubleLeftTee => SYMBOL_DOUBLELEFTTEE,
            Operator::UpTee => SYMBOL_UPTEE,
            Operator::DownTee => SYMBOL_DOWNTEE,
            Operator::Application => SYMBOL_APPLICATION,
            Operator::SameQ => SYMBOL_SAMEQ,
            Operator::UnsameQ => SYMBOL_UNSAMEQ,
            Operator::Dot => SYMBOL_DOT,
            Operator::NonCommutativeMultiply => SYMBOL_NONCOMMUTATIVEMULTIPLY,
            Operator::And => SYMBOL_AND,
            Operator::Or => SYMBOL_OR,
            Operator::Alternatives => SYMBOL_ALTERNATIVES,
            Operator::StringJoin => SYMBOL_STRINGJOIN,
            Operator::StringExpression => SYMBOL_STRINGEXPRESSION,
            Operator::Composition => SYMBOL_COMPOSITION,
            Operator::RightComposition => SYMBOL_RIGHTCOMPOSITION,
            Operator::Element => SYMBOL_ELEMENT,
            Operator::Subset => SYMBOL_SUBSET,
            Operator::Superset => SYMBOL_SUPERSET,
            Operator::SubsetEqual => SYMBOL_SUBSETEQUAL,
            Operator::SupersetEqual => SYMBOL_SUPERSETEQUAL,
            Operator::NotElement => SYMBOL_NOTELEMENT,
            Operator::NotSubset => SYMBOL_NOTSUBSET,
            Operator::NotSuperset => SYMBOL_NOTSUPERSET,
            Operator::NotSubsetEqual => SYMBOL_NOTSUBSETEQUAL,
            Operator::NotSupersetEqual => SYMBOL_NOTSUPERSETEQUAL,
            Operator::SquareSubset => SYMBOL_SQUARESUBSET,
            Operator::SquareSuperset => SYMBOL_SQUARESUPERSET,
            Operator::NotSquareSubset => SYMBOL_NOTSQUARESUBSET,
            Operator::NotSquareSuperset => SYMBOL_NOTSQUARESUPERSET,
            Operator::SquareSubsetEqual => SYMBOL_SQUARESUBSETEQUAL,
            Operator::SquareSupersetEqual => SYMBOL_SQUARESUPERSETEQUAL,
            Operator::NotSquareSubsetEqual => SYMBOL_NOTSQUARESUBSETEQUAL,
            Operator::NotSquareSupersetEqual => SYMBOL_NOTSQUARESUPERSETEQUAL,
            Operator::ReverseElement => SYMBOL_REVERSEELEMENT,
            Operator::NotReverseElement => SYMBOL_NOTREVERSEELEMENT,
            Operator::Distributed => SYMBOL_DISTRIBUTED,
            Operator::Xor => SYMBOL_XOR,
            Operator::Nand => SYMBOL_NAND,
            Operator::Nor => SYMBOL_NOR,
            Operator::LeftArrow => SYMBOL_LEFTARROW,
            Operator::RightArrow => SYMBOL_RIGHTARROW,
            Operator::LeftRightArrow => SYMBOL_LEFTRIGHTARROW,
            Operator::LeftTeeArrow => SYMBOL_LEFTTEEARROW,
            Operator::RightTeeArrow => SYMBOL_RIGHTTEEARROW,
            Operator::RightArrowLeftArrow => SYMBOL_RIGHTARROWLEFTARROW,
            Operator::LeftArrowRightArrow => SYMBOL_LEFTARROWRIGHTARROW,
            Operator::DoubleLeftArrow => SYMBOL_DOUBLELEFTARROW,
            Operator::DoubleRightArrow => SYMBOL_DOUBLERIGHTARROW,
            Operator::DoubleLeftRightArrow => SYMBOL_DOUBLELEFTRIGHTARROW,
            Operator::LeftArrowBar => SYMBOL_LEFTARROWBAR,
            Operator::RightArrowBar => SYMBOL_RIGHTARROWBAR,
            Operator::ShortRightArrow => SYMBOL_SHORTRIGHTARROW,
            Operator::ShortLeftArrow => SYMBOL_SHORTLEFTARROW,
            Operator::UpperLeftArrow => SYMBOL_UPPERLEFTARROW,
            Operator::UpperRightArrow => SYMBOL_UPPERRIGHTARROW,
            Operator::LowerRightArrow => SYMBOL_LOWERRIGHTARROW,
            Operator::LowerLeftArrow => SYMBOL_LOWERLEFTARROW,
            Operator::LeftVector => SYMBOL_LEFTVECTOR,
            Operator::RightVector => SYMBOL_RIGHTVECTOR,
            Operator::LeftRightVector => SYMBOL_LEFTRIGHTVECTOR,
            Operator::LeftVectorBar => SYMBOL_LEFTVECTORBAR,
            Operator::RightVectorBar => SYMBOL_RIGHTVECTORBAR,
            Operator::LeftTeeVector => SYMBOL_LEFTTEEVECTOR,
            Operator::RightTeeVector => SYMBOL_RIGHTTEEVECTOR,
            Operator::DownLeftVector => SYMBOL_DOWNLEFTVECTOR,
            Operator::DownRightVector => SYMBOL_DOWNRIGHTVECTOR,
            Operator::DownLeftRightVector => SYMBOL_DOWNLEFTRIGHTVECTOR,
            Operator::DownLeftVectorBar => SYMBOL_DOWNLEFTVECTORBAR,
            Operator::DownRightVectorBar => SYMBOL_DOWNRIGHTVECTORBAR,
            Operator::DownLeftTeeVector => SYMBOL_DOWNLEFTTEEVECTOR,
            Operator::DownRightTeeVector => SYMBOL_DOWNRIGHTTEEVECTOR,
            Operator::UpArrow => SYMBOL_UPARROW,
            Operator::DownArrow => SYMBOL_DOWNARROW,
            Operator::UpDownArrow => SYMBOL_UPDOWNARROW,
            Operator::UpTeeArrow => SYMBOL_UPTEEARROW,
            Operator::DownTeeArrow => SYMBOL_DOWNTEEARROW,
            Operator::UpArrowDownArrow => SYMBOL_UPARROWDOWNARROW,
            Operator::DoubleUpArrow => SYMBOL_DOUBLEUPARROW,
            Operator::DoubleDownArrow => SYMBOL_DOUBLEDOWNARROW,
            Operator::DoubleUpDownArrow => SYMBOL_DOUBLEUPDOWNARROW,
            Operator::DownArrowUpArrow => SYMBOL_DOWNARROWUPARROW,
            Operator::LongLeftArrow => SYMBOL_LONGLEFTARROW,
            Operator::LongRightArrow => SYMBOL_LONGRIGHTARROW,
            Operator::LongLeftRightArrow => SYMBOL_LONGLEFTRIGHTARROW,
            Operator::DoubleLongLeftArrow => SYMBOL_DOUBLELONGLEFTARROW,
            Operator::DoubleLongRightArrow => SYMBOL_DOUBLELONGRIGHTARROW,
            Operator::DoubleLongLeftRightArrow => SYMBOL_DOUBLELONGLEFTRIGHTARROW,
            Operator::UpArrowBar => SYMBOL_UPARROWBAR,
            Operator::DownArrowBar => SYMBOL_DOWNARROWBAR,
            Operator::ShortUpArrow => SYMBOL_SHORTUPARROW,
            Operator::ShortDownArrow => SYMBOL_SHORTDOWNARROW,
            Operator::RightUpVector => SYMBOL_RIGHTUPVECTOR,
            Operator::LeftUpVector => SYMBOL_LEFTUPVECTOR,
            Operator::RightDownVector => SYMBOL_RIGHTDOWNVECTOR,
            Operator::LeftDownVector => SYMBOL_LEFTDOWNVECTOR,
            Operator::RightUpDownVector => SYMBOL_RIGHTUPDOWNVECTOR,
            Operator::LeftUpDownVector => SYMBOL_LEFTUPDOWNVECTOR,
            Operator::RightUpVectorBar => SYMBOL_RIGHTUPVECTORBAR,
            Operator::RightDownVectorBar => SYMBOL_RIGHTDOWNVECTORBAR,
            Operator::LeftUpVectorBar => SYMBOL_LEFTUPVECTORBAR,
            Operator::LeftDownVectorBar => SYMBOL_LEFTDOWNVECTORBAR,
            Operator::RightUpTeeVector => SYMBOL_RIGHTUPTEEVECTOR,
            Operator::RightDownTeeVector => SYMBOL_RIGHTDOWNTEEVECTOR,
            Operator::LeftUpTeeVector => SYMBOL_LEFTUPTEEVECTOR,
            Operator::LeftDownTeeVector => SYMBOL_LEFTDOWNTEEVECTOR,
            Operator::UpEquilibrium => SYMBOL_UPEQUILIBRIUM,
            Operator::ReverseUpEquilibrium => SYMBOL_REVERSEUPEQUILIBRIUM,
            Operator::CenterDot => SYMBOL_CENTERDOT,
            Operator::Equivalent => SYMBOL_EQUIVALENT,
            Operator::CircleDot => SYMBOL_CIRCLEDOT,
            Operator::Conditioned => SYMBOL_CONDITIONED,
            Operator::Union => SYMBOL_UNION,
            Operator::SquareUnion => SYMBOL_SQUAREUNION,
            Operator::UnionPlus => SYMBOL_UNIONPLUS,
            Operator::Intersection => SYMBOL_INTERSECTION,
            Operator::SquareIntersection => SYMBOL_SQUAREINTERSECTION,
            Operator::TensorWedge => SYMBOL_TENSORWEDGE,
            Operator::TensorProduct => SYMBOL_TENSORPRODUCT,
            Operator::Cross => SYMBOL_CROSS,
            Operator::SmallCircle => SYMBOL_SMALLCIRCLE,
            Operator::Divisible => SYMBOL_DIVISIBLE,
            Operator::VerticalSeparator => SYMBOL_VERTICALSEPARATOR,
            Operator::Backslash => SYMBOL_BACKSLASH,
            Operator::Diamond => SYMBOL_DIAMOND,
            Operator::Wedge => SYMBOL_WEDGE,
            Operator::Vee => SYMBOL_VEE,
            Operator::Star => SYMBOL_STAR,
            Operator::VerticalTilde => SYMBOL_VERTICALTILDE,
            Operator::Cap => SYMBOL_CAP,
            Operator::Cup => SYMBOL_CUP,
            Operator::CirclePlus => SYMBOL_CIRCLEPLUS,
            Operator::VerticalBar => SYMBOL_VERTICALBAR,
            Operator::DoubleVerticalBar => SYMBOL_DOUBLEVERTICALBAR,
            Operator::NotVerticalBar => SYMBOL_NOTVERTICALBAR,
            Operator::NotDoubleVerticalBar => SYMBOL_NOTDOUBLEVERTICALBAR,
            Operator::LeftTriangle => SYMBOL_LEFTTRIANGLE,
            Operator::RightTriangle => SYMBOL_RIGHTTRIANGLE,
            Operator::NotLeftTriangle => SYMBOL_NOTLEFTTRIANGLE,
            Operator::NotRightTriangle => SYMBOL_NOTRIGHTTRIANGLE,
            Operator::LeftTriangleEqual => SYMBOL_LEFTTRIANGLEEQUAL,
            Operator::RightTriangleEqual => SYMBOL_RIGHTTRIANGLEEQUAL,
            Operator::NotLeftTriangleEqual => SYMBOL_NOTLEFTTRIANGLEEQUAL,
            Operator::NotRightTriangleEqual => SYMBOL_NOTRIGHTTRIANGLEEQUAL,
            Operator::LeftTriangleBar => SYMBOL_LEFTTRIANGLEBAR,
            Operator::RightTriangleBar => SYMBOL_RIGHTTRIANGLEBAR,
            Operator::NotLeftTriangleBar => SYMBOL_NOTLEFTTRIANGLEBAR,
            Operator::NotRightTriangleBar => SYMBOL_NOTRIGHTTRIANGLEBAR,
            Operator::TildeEqual => SYMBOL_TILDEEQUAL,
            Operator::NotTildeEqual => SYMBOL_NOTTILDEEQUAL,
            Operator::TildeFullEqual => SYMBOL_TILDEFULLEQUAL,
            Operator::NotTildeFullEqual => SYMBOL_NOTTILDEFULLEQUAL,
            Operator::Tilde => SYMBOL_TILDE,
            Operator::NotTilde => SYMBOL_NOTTILDE,
            Operator::EqualTilde => SYMBOL_EQUALTILDE,
            Operator::NotEqualTilde => SYMBOL_NOTEQUALTILDE,
            Operator::TildeTilde => SYMBOL_TILDETILDE,
            Operator::NotTildeTilde => SYMBOL_NOTTILDETILDE,
            Operator::Proportional => SYMBOL_PROPORTIONAL,
            Operator::Proportion => SYMBOL_PROPORTION,
            Operator::Congruent => SYMBOL_CONGRUENT,
            Operator::NotCongruent => SYMBOL_NOTCONGRUENT,
            Operator::Equilibrium => SYMBOL_EQUILIBRIUM,
            Operator::ReverseEquilibrium => SYMBOL_REVERSEEQUILIBRIUM,
            Operator::DotEqual => SYMBOL_DOTEQUAL,
            Operator::Precedes => SYMBOL_PRECEDES,
            Operator::Succeeds => SYMBOL_SUCCEEDS,
            Operator::PrecedesEqual => SYMBOL_PRECEDESEQUAL,
            Operator::SucceedsEqual => SYMBOL_SUCCEEDSEQUAL,
            Operator::PrecedesTilde => SYMBOL_PRECEDESTILDE,
            Operator::SucceedsTilde => SYMBOL_SUCCEEDSTILDE,
            Operator::PrecedesSlantEqual => SYMBOL_PRECEDESSLANTEQUAL,
            Operator::SucceedsSlantEqual => SYMBOL_SUCCEEDSSLANTEQUAL,
            Operator::NotPrecedes => SYMBOL_NOTPRECEDES,
            Operator::NotSucceeds => SYMBOL_NOTSUCCEEDS,
            Operator::NotPrecedesEqual => SYMBOL_NOTPRECEDESEQUAL,
            Operator::NotSucceedsEqual => SYMBOL_NOTSUCCEEDSEQUAL,
            Operator::NotPrecedesTilde => SYMBOL_NOTPRECEDESTILDE,
            Operator::NotSucceedsTilde => SYMBOL_NOTSUCCEEDSTILDE,
            Operator::NotPrecedesSlantEqual => SYMBOL_NOTPRECEDESSLANTEQUAL,
            Operator::NotSucceedsSlantEqual => SYMBOL_NOTSUCCEEDSSLANTEQUAL,
            Operator::CupCap => SYMBOL_CUPCAP,
            Operator::NotCupCap => SYMBOL_NOTCUPCAP,
            Operator::HumpEqual => SYMBOL_HUMPEQUAL,
            Operator::HumpDownHump => SYMBOL_HUMPDOWNHUMP,
            Operator::NotHumpEqual => SYMBOL_NOTHUMPEQUAL,
            Operator::NotHumpDownHump => SYMBOL_NOTHUMPDOWNHUMP,
            Operator::CodeParser_InfixInequality => SYMBOL_CODEPARSER_INFIXINEQUALITY,
            Operator::PermutationProduct => SYMBOL_PERMUTATIONPRODUCT,
            Operator::Colon => SYMBOL_COLON,
            Operator::Xnor => SYMBOL_XNOR,
            Operator::Repeated => SYMBOL_REPEATED,
            Operator::Factorial => SYMBOL_FACTORIAL,
            Operator::Decrement => SYMBOL_DECREMENT,
            Operator::Increment => SYMBOL_INCREMENT,
            Operator::RepeatedNull => SYMBOL_REPEATEDNULL,
            Operator::Factorial2 => SYMBOL_FACTORIAL2,
            Operator::Derivative => SYMBOL_DERIVATIVE,
            Operator::Transpose => SYMBOL_TRANSPOSE,
            Operator::Conjugate => SYMBOL_CONJUGATE,
            Operator::ConjugateTranspose => SYMBOL_CONJUGATETRANSPOSE,
            Operator::HermitianConjugate => SYMBOL_HERMITIANCONJUGATE,
            Operator::InvisiblePostfixScriptBase => SYMBOL_INVISIBLEPOSTFIXSCRIPTBASE,
        }
    }

    pub(crate) fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            SYMBOL_TIMES => Operator::Times,
            SYMBOL_SPAN => Operator::Span,
            SYMBOL_PATTERN => Operator::Pattern,
            SYMBOL_OPTIONAL => Operator::Optional,
            SYMBOL_BLANK => Operator::Blank,
            SYMBOL_BLANKSEQUENCE => Operator::BlankSequence,
            SYMBOL_BLANKNULLSEQUENCE => Operator::BlankNullSequence,
            SYMBOL_SET => Operator::Set,
            SYMBOL_SETDELAYED => Operator::SetDelayed,
            SYMBOL_UNSET => Operator::Unset,
            SYMBOL_TAGSET => Operator::TagSet,
            SYMBOL_TAGSETDELAYED => Operator::TagSetDelayed,
            SYMBOL_TAGUNSET => Operator::TagUnset,
            SYMBOL_COMPOUNDEXPRESSION => Operator::CompoundExpression,
            SYMBOL_MESSAGENAME => Operator::MessageName,
            SYMBOL_PUT => Operator::Put,
            SYMBOL_PUTAPPEND => Operator::PutAppend,
            SYMBOL_GET => Operator::Get,
            SYMBOL_SLOT => Operator::Slot,
            SYMBOL_SLOTSEQUENCE => Operator::SlotSequence,
            SYMBOL_OUT => Operator::Out,
            SYMBOL_TOKEN_COMMENT => Operator::Token_Comment,
            SYMBOL_CODEPARSER_INTERNALINVALID => Operator::CodeParser_InternalInvalid,
            SYMBOL_CODEPARSER_COMMA => Operator::CodeParser_Comma,
            SYMBOL_CODEPARSER_PATTERNBLANK => Operator::CodeParser_PatternBlank,
            SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE => Operator::CodeParser_PatternBlankSequence,
            SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE => Operator::CodeParser_PatternBlankNullSequence,
            SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT => Operator::CodeParser_PatternOptionalDefault,
            SYMBOL_CODEPARSER_TERNARYTILDE => Operator::CodeParser_TernaryTilde,
            SYMBOL_CODEPARSER_TERNARYOPTIONALPATTERN => Operator::CodeParser_TernaryOptionalPattern,
            SYMBOL_CODEPARSER_INFIXTILDE => Operator::CodeParser_InfixTilde,
            SYMBOL_MINUS => Operator::Minus,
            SYMBOL_PLUS => Operator::Plus,
            SYMBOL_NOT => Operator::Not,
            SYMBOL_PREINCREMENT => Operator::PreIncrement,
            SYMBOL_PREDECREMENT => Operator::PreDecrement,
            SYMBOL_CODEPARSER_PREFIXNOT2 => Operator::CodeParser_PrefixNot2,
            SYMBOL_PLUSMINUS => Operator::PlusMinus,
            SYMBOL_SUM => Operator::Sum,
            SYMBOL_SQRT => Operator::Sqrt,
            SYMBOL_MINUSPLUS => Operator::MinusPlus,
            SYMBOL_DIFFERENTIALD => Operator::DifferentialD,
            SYMBOL_CAPITALDIFFERENTIALD => Operator::CapitalDifferentialD,
            SYMBOL_DEL => Operator::Del,
            SYMBOL_SQUARE => Operator::Square,
            SYMBOL_PRODUCT => Operator::Product,
            SYMBOL_CONTINUEDFRACTIONK => Operator::ContinuedFractionK,
            SYMBOL_CIRCLETIMES => Operator::CircleTimes,
            SYMBOL_FORALL => Operator::ForAll,
            SYMBOL_EXISTS => Operator::Exists,
            SYMBOL_NOTEXISTS => Operator::NotExists,
            SYMBOL_COPRODUCT => Operator::Coproduct,
            SYMBOL_PIECEWISE => Operator::Piecewise,
            SYMBOL_INVISIBLEPREFIXSCRIPTBASE => Operator::InvisiblePrefixScriptBase,
            SYMBOL_EXPECTATIONE => Operator::ExpectationE,
            SYMBOL_CUBEROOT => Operator::CubeRoot,
            SYMBOL_PROBABILITYPR => Operator::ProbabilityPr,
            SYMBOL_CODEPARSER_PREFIXLINEARSYNTAXBANG => Operator::CodeParser_PrefixLinearSyntaxBang,
            SYMBOL_CODEPARSER_GROUPPAREN => Operator::CodeParser_GroupParen,
            SYMBOL_CODEPARSER_GROUPSQUARE => Operator::CodeParser_GroupSquare,
            SYMBOL_LIST => Operator::List,
            SYMBOL_ASSOCIATION => Operator::Association,
            SYMBOL_CODEPARSER_GROUPTYPESPECIFIER => Operator::CodeParser_GroupTypeSpecifier,
            SYMBOL_ANGLEBRACKET => Operator::AngleBracket,
            SYMBOL_CEILING => Operator::Ceiling,
            SYMBOL_FLOOR => Operator::Floor,
            SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET => Operator::CodeParser_GroupDoubleBracket,
            SYMBOL_BRACKETINGBAR => Operator::BracketingBar,
            SYMBOL_DOUBLEBRACKETINGBAR => Operator::DoubleBracketingBar,
            SYMBOL_CURLYQUOTE => Operator::CurlyQuote,
            SYMBOL_CURLYDOUBLEQUOTE => Operator::CurlyDoubleQuote,
            SYMBOL_INTEGRATE => Operator::Integrate,
            SYMBOL_INTEGRAL => Operator::Integral,
            SYMBOL_CONTOURINTEGRAL => Operator::ContourIntegral,
            SYMBOL_DOUBLECONTOURINTEGRAL => Operator::DoubleContourIntegral,
            SYMBOL_CLOCKWISECONTOURINTEGRAL => Operator::ClockwiseContourIntegral,
            SYMBOL_COUNTERCLOCKWISECONTOURINTEGRAL => Operator::CounterClockwiseContourIntegral,
            SYMBOL_DIVIDE => Operator::Divide,
            SYMBOL_POWER => Operator::Power,
            SYMBOL_UPSET => Operator::UpSet,
            SYMBOL_UPSETDELAYED => Operator::UpSetDelayed,
            SYMBOL_MAP => Operator::Map,
            SYMBOL_RULE => Operator::Rule,
            SYMBOL_APPLY => Operator::Apply,
            SYMBOL_CONDITION => Operator::Condition,
            SYMBOL_REPLACEALL => Operator::ReplaceAll,
            SYMBOL_RULEDELAYED => Operator::RuleDelayed,
            SYMBOL_REPLACEREPEATED => Operator::ReplaceRepeated,
            SYMBOL_ADDTO => Operator::AddTo,
            SYMBOL_TIMESBY => Operator::TimesBy,
            SYMBOL_SUBTRACTFROM => Operator::SubtractFrom,
            SYMBOL_DIVIDEBY => Operator::DivideBy,
            SYMBOL_TWOWAYRULE => Operator::TwoWayRule,
            SYMBOL_MAPALL => Operator::MapAll,
            SYMBOL_CODEPARSER_BINARYAT => Operator::CodeParser_BinaryAt,
            SYMBOL_MAPAPPLY => Operator::MapApply,
            SYMBOL_CODEPARSER_BINARYSLASHSLASH => Operator::CodeParser_BinarySlashSlash,
            SYMBOL_PATTERNTEST => Operator::PatternTest,
            SYMBOL_FUNCTION => Operator::Function,
            SYMBOL_APPLYTO => Operator::ApplyTo,
            SYMBOL_IMPLIES => Operator::Implies,
            SYMBOL_ROUNDIMPLIES => Operator::RoundImplies,
            SYMBOL_DIRECTEDEDGE => Operator::DirectedEdge,
            SYMBOL_UNDIRECTEDEDGE => Operator::UndirectedEdge,
            SYMBOL_CIRCLEMINUS => Operator::CircleMinus,
            SYMBOL_SUCHTHAT => Operator::SuchThat,
            SYMBOL_PERPENDICULAR => Operator::Perpendicular,
            SYMBOL_BECAUSE => Operator::Because,
            SYMBOL_THEREFORE => Operator::Therefore,
            SYMBOL_RIGHTTEE => Operator::RightTee,
            SYMBOL_LEFTTEE => Operator::LeftTee,
            SYMBOL_DOUBLERIGHTTEE => Operator::DoubleRightTee,
            SYMBOL_DOUBLELEFTTEE => Operator::DoubleLeftTee,
            SYMBOL_UPTEE => Operator::UpTee,
            SYMBOL_DOWNTEE => Operator::DownTee,
            SYMBOL_APPLICATION => Operator::Application,
            SYMBOL_SAMEQ => Operator::SameQ,
            SYMBOL_UNSAMEQ => Operator::UnsameQ,
            SYMBOL_DOT => Operator::Dot,
            SYMBOL_NONCOMMUTATIVEMULTIPLY => Operator::NonCommutativeMultiply,
            SYMBOL_AND => Operator::And,
            SYMBOL_OR => Operator::Or,
            SYMBOL_ALTERNATIVES => Operator::Alternatives,
            SYMBOL_STRINGJOIN => Operator::StringJoin,
            SYMBOL_STRINGEXPRESSION => Operator::StringExpression,
            SYMBOL_COMPOSITION => Operator::Composition,
            SYMBOL_RIGHTCOMPOSITION => Operator::RightComposition,
            SYMBOL_ELEMENT => Operator::Element,
            SYMBOL_SUBSET => Operator::Subset,
            SYMBOL_SUPERSET => Operator::Superset,
            SYMBOL_SUBSETEQUAL => Operator::SubsetEqual,
            SYMBOL_SUPERSETEQUAL => Operator::SupersetEqual,
            SYMBOL_NOTELEMENT => Operator::NotElement,
            SYMBOL_NOTSUBSET => Operator::NotSubset,
            SYMBOL_NOTSUPERSET => Operator::NotSuperset,
            SYMBOL_NOTSUBSETEQUAL => Operator::NotSubsetEqual,
            SYMBOL_NOTSUPERSETEQUAL => Operator::NotSupersetEqual,
            SYMBOL_SQUARESUBSET => Operator::SquareSubset,
            SYMBOL_SQUARESUPERSET => Operator::SquareSuperset,
            SYMBOL_NOTSQUARESUBSET => Operator::NotSquareSubset,
            SYMBOL_NOTSQUARESUPERSET => Operator::NotSquareSuperset,
            SYMBOL_SQUARESUBSETEQUAL => Operator::SquareSubsetEqual,
            SYMBOL_SQUARESUPERSETEQUAL => Operator::SquareSupersetEqual,
            SYMBOL_NOTSQUARESUBSETEQUAL => Operator::NotSquareSubsetEqual,
            SYMBOL_NOTSQUARESUPERSETEQUAL => Operator::NotSquareSupersetEqual,
            SYMBOL_REVERSEELEMENT => Operator::ReverseElement,
            SYMBOL_NOTREVERSEELEMENT => Operator::NotReverseElement,
            SYMBOL_DISTRIBUTED => Operator::Distributed,
            SYMBOL_XOR => Operator::Xor,
            SYMBOL_NAND => Operator::Nand,
            SYMBOL_NOR => Operator::Nor,
            SYMBOL_LEFTARROW => Operator::LeftArrow,
            SYMBOL_RIGHTARROW => Operator::RightArrow,
            SYMBOL_LEFTRIGHTARROW => Operator::LeftRightArrow,
            SYMBOL_LEFTTEEARROW => Operator::LeftTeeArrow,
            SYMBOL_RIGHTTEEARROW => Operator::RightTeeArrow,
            SYMBOL_RIGHTARROWLEFTARROW => Operator::RightArrowLeftArrow,
            SYMBOL_LEFTARROWRIGHTARROW => Operator::LeftArrowRightArrow,
            SYMBOL_DOUBLELEFTARROW => Operator::DoubleLeftArrow,
            SYMBOL_DOUBLERIGHTARROW => Operator::DoubleRightArrow,
            SYMBOL_DOUBLELEFTRIGHTARROW => Operator::DoubleLeftRightArrow,
            SYMBOL_LEFTARROWBAR => Operator::LeftArrowBar,
            SYMBOL_RIGHTARROWBAR => Operator::RightArrowBar,
            SYMBOL_SHORTRIGHTARROW => Operator::ShortRightArrow,
            SYMBOL_SHORTLEFTARROW => Operator::ShortLeftArrow,
            SYMBOL_UPPERLEFTARROW => Operator::UpperLeftArrow,
            SYMBOL_UPPERRIGHTARROW => Operator::UpperRightArrow,
            SYMBOL_LOWERRIGHTARROW => Operator::LowerRightArrow,
            SYMBOL_LOWERLEFTARROW => Operator::LowerLeftArrow,
            SYMBOL_LEFTVECTOR => Operator::LeftVector,
            SYMBOL_RIGHTVECTOR => Operator::RightVector,
            SYMBOL_LEFTRIGHTVECTOR => Operator::LeftRightVector,
            SYMBOL_LEFTVECTORBAR => Operator::LeftVectorBar,
            SYMBOL_RIGHTVECTORBAR => Operator::RightVectorBar,
            SYMBOL_LEFTTEEVECTOR => Operator::LeftTeeVector,
            SYMBOL_RIGHTTEEVECTOR => Operator::RightTeeVector,
            SYMBOL_DOWNLEFTVECTOR => Operator::DownLeftVector,
            SYMBOL_DOWNRIGHTVECTOR => Operator::DownRightVector,
            SYMBOL_DOWNLEFTRIGHTVECTOR => Operator::DownLeftRightVector,
            SYMBOL_DOWNLEFTVECTORBAR => Operator::DownLeftVectorBar,
            SYMBOL_DOWNRIGHTVECTORBAR => Operator::DownRightVectorBar,
            SYMBOL_DOWNLEFTTEEVECTOR => Operator::DownLeftTeeVector,
            SYMBOL_DOWNRIGHTTEEVECTOR => Operator::DownRightTeeVector,
            SYMBOL_UPARROW => Operator::UpArrow,
            SYMBOL_DOWNARROW => Operator::DownArrow,
            SYMBOL_UPDOWNARROW => Operator::UpDownArrow,
            SYMBOL_UPTEEARROW => Operator::UpTeeArrow,
            SYMBOL_DOWNTEEARROW => Operator::DownTeeArrow,
            SYMBOL_UPARROWDOWNARROW => Operator::UpArrowDownArrow,
            SYMBOL_DOUBLEUPARROW => Operator::DoubleUpArrow,
            SYMBOL_DOUBLEDOWNARROW => Operator::DoubleDownArrow,
            SYMBOL_DOUBLEUPDOWNARROW => Operator::DoubleUpDownArrow,
            SYMBOL_DOWNARROWUPARROW => Operator::DownArrowUpArrow,
            SYMBOL_LONGLEFTARROW => Operator::LongLeftArrow,
            SYMBOL_LONGRIGHTARROW => Operator::LongRightArrow,
            SYMBOL_LONGLEFTRIGHTARROW => Operator::LongLeftRightArrow,
            SYMBOL_DOUBLELONGLEFTARROW => Operator::DoubleLongLeftArrow,
            SYMBOL_DOUBLELONGRIGHTARROW => Operator::DoubleLongRightArrow,
            SYMBOL_DOUBLELONGLEFTRIGHTARROW => Operator::DoubleLongLeftRightArrow,
            SYMBOL_UPARROWBAR => Operator::UpArrowBar,
            SYMBOL_DOWNARROWBAR => Operator::DownArrowBar,
            SYMBOL_SHORTUPARROW => Operator::ShortUpArrow,
            SYMBOL_SHORTDOWNARROW => Operator::ShortDownArrow,
            SYMBOL_RIGHTUPVECTOR => Operator::RightUpVector,
            SYMBOL_LEFTUPVECTOR => Operator::LeftUpVector,
            SYMBOL_RIGHTDOWNVECTOR => Operator::RightDownVector,
            SYMBOL_LEFTDOWNVECTOR => Operator::LeftDownVector,
            SYMBOL_RIGHTUPDOWNVECTOR => Operator::RightUpDownVector,
            SYMBOL_LEFTUPDOWNVECTOR => Operator::LeftUpDownVector,
            SYMBOL_RIGHTUPVECTORBAR => Operator::RightUpVectorBar,
            SYMBOL_RIGHTDOWNVECTORBAR => Operator::RightDownVectorBar,
            SYMBOL_LEFTUPVECTORBAR => Operator::LeftUpVectorBar,
            SYMBOL_LEFTDOWNVECTORBAR => Operator::LeftDownVectorBar,
            SYMBOL_RIGHTUPTEEVECTOR => Operator::RightUpTeeVector,
            SYMBOL_RIGHTDOWNTEEVECTOR => Operator::RightDownTeeVector,
            SYMBOL_LEFTUPTEEVECTOR => Operator::LeftUpTeeVector,
            SYMBOL_LEFTDOWNTEEVECTOR => Operator::LeftDownTeeVector,
            SYMBOL_UPEQUILIBRIUM => Operator::UpEquilibrium,
            SYMBOL_REVERSEUPEQUILIBRIUM => Operator::ReverseUpEquilibrium,
            SYMBOL_CENTERDOT => Operator::CenterDot,
            SYMBOL_EQUIVALENT => Operator::Equivalent,
            SYMBOL_CIRCLEDOT => Operator::CircleDot,
            SYMBOL_CONDITIONED => Operator::Conditioned,
            SYMBOL_UNION => Operator::Union,
            SYMBOL_SQUAREUNION => Operator::SquareUnion,
            SYMBOL_UNIONPLUS => Operator::UnionPlus,
            SYMBOL_INTERSECTION => Operator::Intersection,
            SYMBOL_SQUAREINTERSECTION => Operator::SquareIntersection,
            SYMBOL_TENSORWEDGE => Operator::TensorWedge,
            SYMBOL_TENSORPRODUCT => Operator::TensorProduct,
            SYMBOL_CROSS => Operator::Cross,
            SYMBOL_SMALLCIRCLE => Operator::SmallCircle,
            SYMBOL_DIVISIBLE => Operator::Divisible,
            SYMBOL_VERTICALSEPARATOR => Operator::VerticalSeparator,
            SYMBOL_BACKSLASH => Operator::Backslash,
            SYMBOL_DIAMOND => Operator::Diamond,
            SYMBOL_WEDGE => Operator::Wedge,
            SYMBOL_VEE => Operator::Vee,
            SYMBOL_STAR => Operator::Star,
            SYMBOL_VERTICALTILDE => Operator::VerticalTilde,
            SYMBOL_CAP => Operator::Cap,
            SYMBOL_CUP => Operator::Cup,
            SYMBOL_CIRCLEPLUS => Operator::CirclePlus,
            SYMBOL_VERTICALBAR => Operator::VerticalBar,
            SYMBOL_DOUBLEVERTICALBAR => Operator::DoubleVerticalBar,
            SYMBOL_NOTVERTICALBAR => Operator::NotVerticalBar,
            SYMBOL_NOTDOUBLEVERTICALBAR => Operator::NotDoubleVerticalBar,
            SYMBOL_LEFTTRIANGLE => Operator::LeftTriangle,
            SYMBOL_RIGHTTRIANGLE => Operator::RightTriangle,
            SYMBOL_NOTLEFTTRIANGLE => Operator::NotLeftTriangle,
            SYMBOL_NOTRIGHTTRIANGLE => Operator::NotRightTriangle,
            SYMBOL_LEFTTRIANGLEEQUAL => Operator::LeftTriangleEqual,
            SYMBOL_RIGHTTRIANGLEEQUAL => Operator::RightTriangleEqual,
            SYMBOL_NOTLEFTTRIANGLEEQUAL => Operator::NotLeftTriangleEqual,
            SYMBOL_NOTRIGHTTRIANGLEEQUAL => Operator::NotRightTriangleEqual,
            SYMBOL_LEFTTRIANGLEBAR => Operator::LeftTriangleBar,
            SYMBOL_RIGHTTRIANGLEBAR => Operator::RightTriangleBar,
            SYMBOL_NOTLEFTTRIANGLEBAR => Operator::NotLeftTriangleBar,
            SYMBOL_NOTRIGHTTRIANGLEBAR => Operator::NotRightTriangleBar,
            SYMBOL_TILDEEQUAL => Operator::TildeEqual,
            SYMBOL_NOTTILDEEQUAL => Operator::NotTildeEqual,
            SYMBOL_TILDEFULLEQUAL => Operator::TildeFullEqual,
            SYMBOL_NOTTILDEFULLEQUAL => Operator::NotTildeFullEqual,
            SYMBOL_TILDE => Operator::Tilde,
            SYMBOL_NOTTILDE => Operator::NotTilde,
            SYMBOL_EQUALTILDE => Operator::EqualTilde,
            SYMBOL_NOTEQUALTILDE => Operator::NotEqualTilde,
            SYMBOL_TILDETILDE => Operator::TildeTilde,
            SYMBOL_NOTTILDETILDE => Operator::NotTildeTilde,
            SYMBOL_PROPORTIONAL => Operator::Proportional,
            SYMBOL_PROPORTION => Operator::Proportion,
            SYMBOL_CONGRUENT => Operator::Congruent,
            SYMBOL_NOTCONGRUENT => Operator::NotCongruent,
            SYMBOL_EQUILIBRIUM => Operator::Equilibrium,
            SYMBOL_REVERSEEQUILIBRIUM => Operator::ReverseEquilibrium,
            SYMBOL_DOTEQUAL => Operator::DotEqual,
            SYMBOL_PRECEDES => Operator::Precedes,
            SYMBOL_SUCCEEDS => Operator::Succeeds,
            SYMBOL_PRECEDESEQUAL => Operator::PrecedesEqual,
            SYMBOL_SUCCEEDSEQUAL => Operator::SucceedsEqual,
            SYMBOL_PRECEDESTILDE => Operator::PrecedesTilde,
            SYMBOL_SUCCEEDSTILDE => Operator::SucceedsTilde,
            SYMBOL_PRECEDESSLANTEQUAL => Operator::PrecedesSlantEqual,
            SYMBOL_SUCCEEDSSLANTEQUAL => Operator::SucceedsSlantEqual,
            SYMBOL_NOTPRECEDES => Operator::NotPrecedes,
            SYMBOL_NOTSUCCEEDS => Operator::NotSucceeds,
            SYMBOL_NOTPRECEDESEQUAL => Operator::NotPrecedesEqual,
            SYMBOL_NOTSUCCEEDSEQUAL => Operator::NotSucceedsEqual,
            SYMBOL_NOTPRECEDESTILDE => Operator::NotPrecedesTilde,
            SYMBOL_NOTSUCCEEDSTILDE => Operator::NotSucceedsTilde,
            SYMBOL_NOTPRECEDESSLANTEQUAL => Operator::NotPrecedesSlantEqual,
            SYMBOL_NOTSUCCEEDSSLANTEQUAL => Operator::NotSucceedsSlantEqual,
            SYMBOL_CUPCAP => Operator::CupCap,
            SYMBOL_NOTCUPCAP => Operator::NotCupCap,
            SYMBOL_HUMPEQUAL => Operator::HumpEqual,
            SYMBOL_HUMPDOWNHUMP => Operator::HumpDownHump,
            SYMBOL_NOTHUMPEQUAL => Operator::NotHumpEqual,
            SYMBOL_NOTHUMPDOWNHUMP => Operator::NotHumpDownHump,
            SYMBOL_CODEPARSER_INFIXINEQUALITY => Operator::CodeParser_InfixInequality,
            SYMBOL_PERMUTATIONPRODUCT => Operator::PermutationProduct,
            SYMBOL_COLON => Operator::Colon,
            SYMBOL_XNOR => Operator::Xnor,
            SYMBOL_REPEATED => Operator::Repeated,
            SYMBOL_FACTORIAL => Operator::Factorial,
            SYMBOL_DECREMENT => Operator::Decrement,
            SYMBOL_INCREMENT => Operator::Increment,
            SYMBOL_REPEATEDNULL => Operator::RepeatedNull,
            SYMBOL_FACTORIAL2 => Operator::Factorial2,
            SYMBOL_DERIVATIVE => Operator::Derivative,
            SYMBOL_TRANSPOSE => Operator::Transpose,
            SYMBOL_CONJUGATE => Operator::Conjugate,
            SYMBOL_CONJUGATETRANSPOSE => Operator::ConjugateTranspose,
            SYMBOL_HERMITIANCONJUGATE => Operator::HermitianConjugate,
            SYMBOL_INVISIBLEPOSTFIXSCRIPTBASE => Operator::InvisiblePostfixScriptBase,
            _ => return None,
        };

        Some(operator)
    }
}
