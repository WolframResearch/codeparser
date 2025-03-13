//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(non_upper_case_globals)]

use wolfram_expr::symbol::SymbolRef;

use crate::{
	token::TokenKind,
	symbol::Symbol,
	symbol_registration as sym,
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

pub(crate) const under1Parselet: UnderParselet = UnderParselet::new(CompoundOperator::Blank, CompoundOperator::CodeParser_PatternBlank);
pub(crate) const under2Parselet: UnderParselet = UnderParselet::new(CompoundOperator::BlankSequence, CompoundOperator::CodeParser_PatternBlankSequence);
pub(crate) const under3Parselet: UnderParselet = UnderParselet::new(CompoundOperator::BlankNullSequence, CompoundOperator::CodeParser_PatternBlankNullSequence);

pub(crate) const underDotParselet: UnderDotParselet = UnderDotParselet {};

pub(crate) const squareGroupParselet: GroupParselet = GroupParselet::new(TokenKind::OpenSquare, GroupOperator::CodeParser_GroupSquare);

pub(crate) const doubleBracketGroupParselet: GroupParselet = GroupParselet::new(TokenKind::LongName_LeftDoubleBracket, GroupOperator::CodeParser_GroupDoubleBracket);

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
    &GroupParselet::new(TokenKind::OpenParen, GroupOperator::CodeParser_GroupParen), // Token`OpenParen
    &prefixCloserParselet, // Token`CloseParen
    &squareGroupParselet, // Token`OpenSquare
    &prefixCloserParselet, // Token`CloseSquare
    &prefixCommaParselet, // Token`Comma
    &GroupParselet::new(TokenKind::OpenCurly, GroupOperator::List), // Token`OpenCurly
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
    &GroupParselet::new(TokenKind::LessBar, GroupOperator::Association), // Token`LessBar
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
    &GroupParselet::new(TokenKind::ColonColonOpenSquare, GroupOperator::CodeParser_GroupTypeSpecifier), // Token`ColonColonOpenSquare
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
    &GroupParselet::new(TokenKind::LongName_OpenCurlyQuote, GroupOperator::CurlyQuote), // Token`LongName`OpenCurlyQuote
    &prefixCloserParselet, // Token`LongName`CloseCurlyQuote
    &GroupParselet::new(TokenKind::LongName_OpenCurlyDoubleQuote, GroupOperator::CurlyDoubleQuote), // Token`LongName`OpenCurlyDoubleQuote
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
    &IntegralParselet::new(PrefixBinaryOperator::Integrate, Operator::Integral), // Token`LongName`Integral
    &IntegralParselet::new(PrefixBinaryOperator::ContourIntegral, Operator::ContourIntegral), // Token`LongName`ContourIntegral
    &IntegralParselet::new(PrefixBinaryOperator::DoubleContourIntegral, Operator::DoubleContourIntegral), // Token`LongName`DoubleContourIntegral
    &IntegralParselet::new(PrefixBinaryOperator::ClockwiseContourIntegral, Operator::ClockwiseContourIntegral), // Token`LongName`ClockwiseContourIntegral
    &IntegralParselet::new(PrefixBinaryOperator::CounterClockwiseContourIntegral, Operator::CounterClockwiseContourIntegral), // Token`LongName`CounterClockwiseContourIntegral
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
    &GroupParselet::new(TokenKind::LongName_LeftCeiling, GroupOperator::Ceiling), // Token`LongName`LeftCeiling
    &prefixCloserParselet, // Token`LongName`RightCeiling
    &GroupParselet::new(TokenKind::LongName_LeftFloor, GroupOperator::Floor), // Token`LongName`LeftFloor
    &prefixCloserParselet, // Token`LongName`RightFloor
    &prefixUnhandledParselet, // Token`LongName`Cap
    &prefixUnhandledParselet, // Token`LongName`Cup
    &GroupParselet::new(TokenKind::LongName_LeftAngleBracket, GroupOperator::AngleBracket), // Token`LongName`LeftAngleBracket
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
    &GroupParselet::new(TokenKind::LongName_LeftAssociation, GroupOperator::Association), // Token`LongName`LeftAssociation
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
    &GroupParselet::new(TokenKind::LongName_LeftBracketingBar, GroupOperator::BracketingBar), // Token`LongName`LeftBracketingBar
    &prefixCloserParselet, // Token`LongName`RightBracketingBar
    &GroupParselet::new(TokenKind::LongName_LeftDoubleBracketingBar, GroupOperator::DoubleBracketingBar), // Token`LongName`LeftDoubleBracketingBar
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
    &(CallParselet::new(&GroupParselet::new(TokenKind::ColonColonOpenSquare, GroupOperator::CodeParser_GroupTypeSpecifier))), // Token`ColonColonOpenSquare
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
    Set,
    SetDelayed,
    Unset,
    CompoundExpression,
    MessageName,
    Put,
    PutAppend,
    Get,
    CodeParser_InternalInvalid,
    CodeParser_Comma,
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

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrefixBinaryOperator {
    Integrate,
    ContourIntegral,
    DoubleContourIntegral,
    ClockwiseContourIntegral,
    CounterClockwiseContourIntegral,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GroupOperator {
    Token_Comment,
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
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CompoundOperator {
    Blank,
    BlankSequence,
    BlankNullSequence,
    Slot,
    SlotSequence,
    Out,
    CodeParser_PatternBlank,
    CodeParser_PatternBlankSequence,
    CodeParser_PatternBlankNullSequence,
    CodeParser_PatternOptionalDefault,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TernaryOperator {
    CodeParser_TernaryTilde,
    CodeParser_TernaryOptionalPattern,
    TagSet,
    TagSetDelayed,
    TagUnset,
    Span,
}

impl Operator {
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn to_symbol(self) -> Symbol {
        match self {
            Operator::Times => sym::Times,
            Operator::Span => sym::Span,
            Operator::Pattern => sym::Pattern,
            Operator::Optional => sym::Optional,
            Operator::Set => sym::Set,
            Operator::SetDelayed => sym::SetDelayed,
            Operator::Unset => sym::Unset,
            Operator::CompoundExpression => sym::CompoundExpression,
            Operator::MessageName => sym::MessageName,
            Operator::Put => sym::Put,
            Operator::PutAppend => sym::PutAppend,
            Operator::Get => sym::Get,
            Operator::CodeParser_InternalInvalid => sym::CodeParser_InternalInvalid,
            Operator::CodeParser_Comma => sym::CodeParser_Comma,
            Operator::CodeParser_InfixTilde => sym::CodeParser_InfixTilde,
            Operator::Minus => sym::Minus,
            Operator::Plus => sym::Plus,
            Operator::Not => sym::Not,
            Operator::PreIncrement => sym::PreIncrement,
            Operator::PreDecrement => sym::PreDecrement,
            Operator::CodeParser_PrefixNot2 => sym::CodeParser_PrefixNot2,
            Operator::PlusMinus => sym::PlusMinus,
            Operator::Sum => sym::Sum,
            Operator::Sqrt => sym::Sqrt,
            Operator::MinusPlus => sym::MinusPlus,
            Operator::DifferentialD => sym::DifferentialD,
            Operator::CapitalDifferentialD => sym::CapitalDifferentialD,
            Operator::Del => sym::Del,
            Operator::Square => sym::Square,
            Operator::Product => sym::Product,
            Operator::ContinuedFractionK => sym::ContinuedFractionK,
            Operator::CircleTimes => sym::CircleTimes,
            Operator::ForAll => sym::ForAll,
            Operator::Exists => sym::Exists,
            Operator::NotExists => sym::NotExists,
            Operator::Coproduct => sym::Coproduct,
            Operator::Piecewise => sym::Piecewise,
            Operator::InvisiblePrefixScriptBase => sym::InvisiblePrefixScriptBase,
            Operator::ExpectationE => sym::ExpectationE,
            Operator::CubeRoot => sym::CubeRoot,
            Operator::ProbabilityPr => sym::ProbabilityPr,
            Operator::CodeParser_PrefixLinearSyntaxBang => sym::CodeParser_PrefixLinearSyntaxBang,
            Operator::Integral => sym::Integral,
            Operator::ContourIntegral => sym::ContourIntegral,
            Operator::DoubleContourIntegral => sym::DoubleContourIntegral,
            Operator::ClockwiseContourIntegral => sym::ClockwiseContourIntegral,
            Operator::CounterClockwiseContourIntegral => sym::CounterClockwiseContourIntegral,
            Operator::Divide => sym::Divide,
            Operator::Power => sym::Power,
            Operator::UpSet => sym::UpSet,
            Operator::UpSetDelayed => sym::UpSetDelayed,
            Operator::Map => sym::Map,
            Operator::Rule => sym::Rule,
            Operator::Apply => sym::Apply,
            Operator::Condition => sym::Condition,
            Operator::ReplaceAll => sym::ReplaceAll,
            Operator::RuleDelayed => sym::RuleDelayed,
            Operator::ReplaceRepeated => sym::ReplaceRepeated,
            Operator::AddTo => sym::AddTo,
            Operator::TimesBy => sym::TimesBy,
            Operator::SubtractFrom => sym::SubtractFrom,
            Operator::DivideBy => sym::DivideBy,
            Operator::TwoWayRule => sym::TwoWayRule,
            Operator::MapAll => sym::MapAll,
            Operator::CodeParser_BinaryAt => sym::CodeParser_BinaryAt,
            Operator::MapApply => sym::MapApply,
            Operator::CodeParser_BinarySlashSlash => sym::CodeParser_BinarySlashSlash,
            Operator::PatternTest => sym::PatternTest,
            Operator::Function => sym::Function,
            Operator::ApplyTo => sym::ApplyTo,
            Operator::Implies => sym::Implies,
            Operator::RoundImplies => sym::RoundImplies,
            Operator::DirectedEdge => sym::DirectedEdge,
            Operator::UndirectedEdge => sym::UndirectedEdge,
            Operator::CircleMinus => sym::CircleMinus,
            Operator::SuchThat => sym::SuchThat,
            Operator::Perpendicular => sym::Perpendicular,
            Operator::Because => sym::Because,
            Operator::Therefore => sym::Therefore,
            Operator::RightTee => sym::RightTee,
            Operator::LeftTee => sym::LeftTee,
            Operator::DoubleRightTee => sym::DoubleRightTee,
            Operator::DoubleLeftTee => sym::DoubleLeftTee,
            Operator::UpTee => sym::UpTee,
            Operator::DownTee => sym::DownTee,
            Operator::Application => sym::Application,
            Operator::SameQ => sym::SameQ,
            Operator::UnsameQ => sym::UnsameQ,
            Operator::Dot => sym::Dot,
            Operator::NonCommutativeMultiply => sym::NonCommutativeMultiply,
            Operator::And => sym::And,
            Operator::Or => sym::Or,
            Operator::Alternatives => sym::Alternatives,
            Operator::StringJoin => sym::StringJoin,
            Operator::StringExpression => sym::StringExpression,
            Operator::Composition => sym::Composition,
            Operator::RightComposition => sym::RightComposition,
            Operator::Element => sym::Element,
            Operator::Subset => sym::Subset,
            Operator::Superset => sym::Superset,
            Operator::SubsetEqual => sym::SubsetEqual,
            Operator::SupersetEqual => sym::SupersetEqual,
            Operator::NotElement => sym::NotElement,
            Operator::NotSubset => sym::NotSubset,
            Operator::NotSuperset => sym::NotSuperset,
            Operator::NotSubsetEqual => sym::NotSubsetEqual,
            Operator::NotSupersetEqual => sym::NotSupersetEqual,
            Operator::SquareSubset => sym::SquareSubset,
            Operator::SquareSuperset => sym::SquareSuperset,
            Operator::NotSquareSubset => sym::NotSquareSubset,
            Operator::NotSquareSuperset => sym::NotSquareSuperset,
            Operator::SquareSubsetEqual => sym::SquareSubsetEqual,
            Operator::SquareSupersetEqual => sym::SquareSupersetEqual,
            Operator::NotSquareSubsetEqual => sym::NotSquareSubsetEqual,
            Operator::NotSquareSupersetEqual => sym::NotSquareSupersetEqual,
            Operator::ReverseElement => sym::ReverseElement,
            Operator::NotReverseElement => sym::NotReverseElement,
            Operator::Distributed => sym::Distributed,
            Operator::Xor => sym::Xor,
            Operator::Nand => sym::Nand,
            Operator::Nor => sym::Nor,
            Operator::LeftArrow => sym::LeftArrow,
            Operator::RightArrow => sym::RightArrow,
            Operator::LeftRightArrow => sym::LeftRightArrow,
            Operator::LeftTeeArrow => sym::LeftTeeArrow,
            Operator::RightTeeArrow => sym::RightTeeArrow,
            Operator::RightArrowLeftArrow => sym::RightArrowLeftArrow,
            Operator::LeftArrowRightArrow => sym::LeftArrowRightArrow,
            Operator::DoubleLeftArrow => sym::DoubleLeftArrow,
            Operator::DoubleRightArrow => sym::DoubleRightArrow,
            Operator::DoubleLeftRightArrow => sym::DoubleLeftRightArrow,
            Operator::LeftArrowBar => sym::LeftArrowBar,
            Operator::RightArrowBar => sym::RightArrowBar,
            Operator::ShortRightArrow => sym::ShortRightArrow,
            Operator::ShortLeftArrow => sym::ShortLeftArrow,
            Operator::UpperLeftArrow => sym::UpperLeftArrow,
            Operator::UpperRightArrow => sym::UpperRightArrow,
            Operator::LowerRightArrow => sym::LowerRightArrow,
            Operator::LowerLeftArrow => sym::LowerLeftArrow,
            Operator::LeftVector => sym::LeftVector,
            Operator::RightVector => sym::RightVector,
            Operator::LeftRightVector => sym::LeftRightVector,
            Operator::LeftVectorBar => sym::LeftVectorBar,
            Operator::RightVectorBar => sym::RightVectorBar,
            Operator::LeftTeeVector => sym::LeftTeeVector,
            Operator::RightTeeVector => sym::RightTeeVector,
            Operator::DownLeftVector => sym::DownLeftVector,
            Operator::DownRightVector => sym::DownRightVector,
            Operator::DownLeftRightVector => sym::DownLeftRightVector,
            Operator::DownLeftVectorBar => sym::DownLeftVectorBar,
            Operator::DownRightVectorBar => sym::DownRightVectorBar,
            Operator::DownLeftTeeVector => sym::DownLeftTeeVector,
            Operator::DownRightTeeVector => sym::DownRightTeeVector,
            Operator::UpArrow => sym::UpArrow,
            Operator::DownArrow => sym::DownArrow,
            Operator::UpDownArrow => sym::UpDownArrow,
            Operator::UpTeeArrow => sym::UpTeeArrow,
            Operator::DownTeeArrow => sym::DownTeeArrow,
            Operator::UpArrowDownArrow => sym::UpArrowDownArrow,
            Operator::DoubleUpArrow => sym::DoubleUpArrow,
            Operator::DoubleDownArrow => sym::DoubleDownArrow,
            Operator::DoubleUpDownArrow => sym::DoubleUpDownArrow,
            Operator::DownArrowUpArrow => sym::DownArrowUpArrow,
            Operator::LongLeftArrow => sym::LongLeftArrow,
            Operator::LongRightArrow => sym::LongRightArrow,
            Operator::LongLeftRightArrow => sym::LongLeftRightArrow,
            Operator::DoubleLongLeftArrow => sym::DoubleLongLeftArrow,
            Operator::DoubleLongRightArrow => sym::DoubleLongRightArrow,
            Operator::DoubleLongLeftRightArrow => sym::DoubleLongLeftRightArrow,
            Operator::UpArrowBar => sym::UpArrowBar,
            Operator::DownArrowBar => sym::DownArrowBar,
            Operator::ShortUpArrow => sym::ShortUpArrow,
            Operator::ShortDownArrow => sym::ShortDownArrow,
            Operator::RightUpVector => sym::RightUpVector,
            Operator::LeftUpVector => sym::LeftUpVector,
            Operator::RightDownVector => sym::RightDownVector,
            Operator::LeftDownVector => sym::LeftDownVector,
            Operator::RightUpDownVector => sym::RightUpDownVector,
            Operator::LeftUpDownVector => sym::LeftUpDownVector,
            Operator::RightUpVectorBar => sym::RightUpVectorBar,
            Operator::RightDownVectorBar => sym::RightDownVectorBar,
            Operator::LeftUpVectorBar => sym::LeftUpVectorBar,
            Operator::LeftDownVectorBar => sym::LeftDownVectorBar,
            Operator::RightUpTeeVector => sym::RightUpTeeVector,
            Operator::RightDownTeeVector => sym::RightDownTeeVector,
            Operator::LeftUpTeeVector => sym::LeftUpTeeVector,
            Operator::LeftDownTeeVector => sym::LeftDownTeeVector,
            Operator::UpEquilibrium => sym::UpEquilibrium,
            Operator::ReverseUpEquilibrium => sym::ReverseUpEquilibrium,
            Operator::CenterDot => sym::CenterDot,
            Operator::Equivalent => sym::Equivalent,
            Operator::CircleDot => sym::CircleDot,
            Operator::Conditioned => sym::Conditioned,
            Operator::Union => sym::Union,
            Operator::SquareUnion => sym::SquareUnion,
            Operator::UnionPlus => sym::UnionPlus,
            Operator::Intersection => sym::Intersection,
            Operator::SquareIntersection => sym::SquareIntersection,
            Operator::TensorWedge => sym::TensorWedge,
            Operator::TensorProduct => sym::TensorProduct,
            Operator::Cross => sym::Cross,
            Operator::SmallCircle => sym::SmallCircle,
            Operator::Divisible => sym::Divisible,
            Operator::VerticalSeparator => sym::VerticalSeparator,
            Operator::Backslash => sym::Backslash,
            Operator::Diamond => sym::Diamond,
            Operator::Wedge => sym::Wedge,
            Operator::Vee => sym::Vee,
            Operator::Star => sym::Star,
            Operator::VerticalTilde => sym::VerticalTilde,
            Operator::Cap => sym::Cap,
            Operator::Cup => sym::Cup,
            Operator::CirclePlus => sym::CirclePlus,
            Operator::VerticalBar => sym::VerticalBar,
            Operator::DoubleVerticalBar => sym::DoubleVerticalBar,
            Operator::NotVerticalBar => sym::NotVerticalBar,
            Operator::NotDoubleVerticalBar => sym::NotDoubleVerticalBar,
            Operator::LeftTriangle => sym::LeftTriangle,
            Operator::RightTriangle => sym::RightTriangle,
            Operator::NotLeftTriangle => sym::NotLeftTriangle,
            Operator::NotRightTriangle => sym::NotRightTriangle,
            Operator::LeftTriangleEqual => sym::LeftTriangleEqual,
            Operator::RightTriangleEqual => sym::RightTriangleEqual,
            Operator::NotLeftTriangleEqual => sym::NotLeftTriangleEqual,
            Operator::NotRightTriangleEqual => sym::NotRightTriangleEqual,
            Operator::LeftTriangleBar => sym::LeftTriangleBar,
            Operator::RightTriangleBar => sym::RightTriangleBar,
            Operator::NotLeftTriangleBar => sym::NotLeftTriangleBar,
            Operator::NotRightTriangleBar => sym::NotRightTriangleBar,
            Operator::TildeEqual => sym::TildeEqual,
            Operator::NotTildeEqual => sym::NotTildeEqual,
            Operator::TildeFullEqual => sym::TildeFullEqual,
            Operator::NotTildeFullEqual => sym::NotTildeFullEqual,
            Operator::Tilde => sym::Tilde,
            Operator::NotTilde => sym::NotTilde,
            Operator::EqualTilde => sym::EqualTilde,
            Operator::NotEqualTilde => sym::NotEqualTilde,
            Operator::TildeTilde => sym::TildeTilde,
            Operator::NotTildeTilde => sym::NotTildeTilde,
            Operator::Proportional => sym::Proportional,
            Operator::Proportion => sym::Proportion,
            Operator::Congruent => sym::Congruent,
            Operator::NotCongruent => sym::NotCongruent,
            Operator::Equilibrium => sym::Equilibrium,
            Operator::ReverseEquilibrium => sym::ReverseEquilibrium,
            Operator::DotEqual => sym::DotEqual,
            Operator::Precedes => sym::Precedes,
            Operator::Succeeds => sym::Succeeds,
            Operator::PrecedesEqual => sym::PrecedesEqual,
            Operator::SucceedsEqual => sym::SucceedsEqual,
            Operator::PrecedesTilde => sym::PrecedesTilde,
            Operator::SucceedsTilde => sym::SucceedsTilde,
            Operator::PrecedesSlantEqual => sym::PrecedesSlantEqual,
            Operator::SucceedsSlantEqual => sym::SucceedsSlantEqual,
            Operator::NotPrecedes => sym::NotPrecedes,
            Operator::NotSucceeds => sym::NotSucceeds,
            Operator::NotPrecedesEqual => sym::NotPrecedesEqual,
            Operator::NotSucceedsEqual => sym::NotSucceedsEqual,
            Operator::NotPrecedesTilde => sym::NotPrecedesTilde,
            Operator::NotSucceedsTilde => sym::NotSucceedsTilde,
            Operator::NotPrecedesSlantEqual => sym::NotPrecedesSlantEqual,
            Operator::NotSucceedsSlantEqual => sym::NotSucceedsSlantEqual,
            Operator::CupCap => sym::CupCap,
            Operator::NotCupCap => sym::NotCupCap,
            Operator::HumpEqual => sym::HumpEqual,
            Operator::HumpDownHump => sym::HumpDownHump,
            Operator::NotHumpEqual => sym::NotHumpEqual,
            Operator::NotHumpDownHump => sym::NotHumpDownHump,
            Operator::CodeParser_InfixInequality => sym::CodeParser_InfixInequality,
            Operator::PermutationProduct => sym::PermutationProduct,
            Operator::Colon => sym::Colon,
            Operator::Xnor => sym::Xnor,
            Operator::Repeated => sym::Repeated,
            Operator::Factorial => sym::Factorial,
            Operator::Decrement => sym::Decrement,
            Operator::Increment => sym::Increment,
            Operator::RepeatedNull => sym::RepeatedNull,
            Operator::Factorial2 => sym::Factorial2,
            Operator::Derivative => sym::Derivative,
            Operator::Transpose => sym::Transpose,
            Operator::Conjugate => sym::Conjugate,
            Operator::ConjugateTranspose => sym::ConjugateTranspose,
            Operator::HermitianConjugate => sym::HermitianConjugate,
            Operator::InvisiblePostfixScriptBase => sym::InvisiblePostfixScriptBase,
        }
    }

    #[doc(hidden)]
    pub fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Times => Operator::Times,
            sym::Span => Operator::Span,
            sym::Pattern => Operator::Pattern,
            sym::Optional => Operator::Optional,
            sym::Set => Operator::Set,
            sym::SetDelayed => Operator::SetDelayed,
            sym::Unset => Operator::Unset,
            sym::CompoundExpression => Operator::CompoundExpression,
            sym::MessageName => Operator::MessageName,
            sym::Put => Operator::Put,
            sym::PutAppend => Operator::PutAppend,
            sym::Get => Operator::Get,
            sym::CodeParser_InternalInvalid => Operator::CodeParser_InternalInvalid,
            sym::CodeParser_Comma => Operator::CodeParser_Comma,
            sym::CodeParser_InfixTilde => Operator::CodeParser_InfixTilde,
            sym::Minus => Operator::Minus,
            sym::Plus => Operator::Plus,
            sym::Not => Operator::Not,
            sym::PreIncrement => Operator::PreIncrement,
            sym::PreDecrement => Operator::PreDecrement,
            sym::CodeParser_PrefixNot2 => Operator::CodeParser_PrefixNot2,
            sym::PlusMinus => Operator::PlusMinus,
            sym::Sum => Operator::Sum,
            sym::Sqrt => Operator::Sqrt,
            sym::MinusPlus => Operator::MinusPlus,
            sym::DifferentialD => Operator::DifferentialD,
            sym::CapitalDifferentialD => Operator::CapitalDifferentialD,
            sym::Del => Operator::Del,
            sym::Square => Operator::Square,
            sym::Product => Operator::Product,
            sym::ContinuedFractionK => Operator::ContinuedFractionK,
            sym::CircleTimes => Operator::CircleTimes,
            sym::ForAll => Operator::ForAll,
            sym::Exists => Operator::Exists,
            sym::NotExists => Operator::NotExists,
            sym::Coproduct => Operator::Coproduct,
            sym::Piecewise => Operator::Piecewise,
            sym::InvisiblePrefixScriptBase => Operator::InvisiblePrefixScriptBase,
            sym::ExpectationE => Operator::ExpectationE,
            sym::CubeRoot => Operator::CubeRoot,
            sym::ProbabilityPr => Operator::ProbabilityPr,
            sym::CodeParser_PrefixLinearSyntaxBang => Operator::CodeParser_PrefixLinearSyntaxBang,
            sym::Integral => Operator::Integral,
            sym::ContourIntegral => Operator::ContourIntegral,
            sym::DoubleContourIntegral => Operator::DoubleContourIntegral,
            sym::ClockwiseContourIntegral => Operator::ClockwiseContourIntegral,
            sym::CounterClockwiseContourIntegral => Operator::CounterClockwiseContourIntegral,
            sym::Divide => Operator::Divide,
            sym::Power => Operator::Power,
            sym::UpSet => Operator::UpSet,
            sym::UpSetDelayed => Operator::UpSetDelayed,
            sym::Map => Operator::Map,
            sym::Rule => Operator::Rule,
            sym::Apply => Operator::Apply,
            sym::Condition => Operator::Condition,
            sym::ReplaceAll => Operator::ReplaceAll,
            sym::RuleDelayed => Operator::RuleDelayed,
            sym::ReplaceRepeated => Operator::ReplaceRepeated,
            sym::AddTo => Operator::AddTo,
            sym::TimesBy => Operator::TimesBy,
            sym::SubtractFrom => Operator::SubtractFrom,
            sym::DivideBy => Operator::DivideBy,
            sym::TwoWayRule => Operator::TwoWayRule,
            sym::MapAll => Operator::MapAll,
            sym::CodeParser_BinaryAt => Operator::CodeParser_BinaryAt,
            sym::MapApply => Operator::MapApply,
            sym::CodeParser_BinarySlashSlash => Operator::CodeParser_BinarySlashSlash,
            sym::PatternTest => Operator::PatternTest,
            sym::Function => Operator::Function,
            sym::ApplyTo => Operator::ApplyTo,
            sym::Implies => Operator::Implies,
            sym::RoundImplies => Operator::RoundImplies,
            sym::DirectedEdge => Operator::DirectedEdge,
            sym::UndirectedEdge => Operator::UndirectedEdge,
            sym::CircleMinus => Operator::CircleMinus,
            sym::SuchThat => Operator::SuchThat,
            sym::Perpendicular => Operator::Perpendicular,
            sym::Because => Operator::Because,
            sym::Therefore => Operator::Therefore,
            sym::RightTee => Operator::RightTee,
            sym::LeftTee => Operator::LeftTee,
            sym::DoubleRightTee => Operator::DoubleRightTee,
            sym::DoubleLeftTee => Operator::DoubleLeftTee,
            sym::UpTee => Operator::UpTee,
            sym::DownTee => Operator::DownTee,
            sym::Application => Operator::Application,
            sym::SameQ => Operator::SameQ,
            sym::UnsameQ => Operator::UnsameQ,
            sym::Dot => Operator::Dot,
            sym::NonCommutativeMultiply => Operator::NonCommutativeMultiply,
            sym::And => Operator::And,
            sym::Or => Operator::Or,
            sym::Alternatives => Operator::Alternatives,
            sym::StringJoin => Operator::StringJoin,
            sym::StringExpression => Operator::StringExpression,
            sym::Composition => Operator::Composition,
            sym::RightComposition => Operator::RightComposition,
            sym::Element => Operator::Element,
            sym::Subset => Operator::Subset,
            sym::Superset => Operator::Superset,
            sym::SubsetEqual => Operator::SubsetEqual,
            sym::SupersetEqual => Operator::SupersetEqual,
            sym::NotElement => Operator::NotElement,
            sym::NotSubset => Operator::NotSubset,
            sym::NotSuperset => Operator::NotSuperset,
            sym::NotSubsetEqual => Operator::NotSubsetEqual,
            sym::NotSupersetEqual => Operator::NotSupersetEqual,
            sym::SquareSubset => Operator::SquareSubset,
            sym::SquareSuperset => Operator::SquareSuperset,
            sym::NotSquareSubset => Operator::NotSquareSubset,
            sym::NotSquareSuperset => Operator::NotSquareSuperset,
            sym::SquareSubsetEqual => Operator::SquareSubsetEqual,
            sym::SquareSupersetEqual => Operator::SquareSupersetEqual,
            sym::NotSquareSubsetEqual => Operator::NotSquareSubsetEqual,
            sym::NotSquareSupersetEqual => Operator::NotSquareSupersetEqual,
            sym::ReverseElement => Operator::ReverseElement,
            sym::NotReverseElement => Operator::NotReverseElement,
            sym::Distributed => Operator::Distributed,
            sym::Xor => Operator::Xor,
            sym::Nand => Operator::Nand,
            sym::Nor => Operator::Nor,
            sym::LeftArrow => Operator::LeftArrow,
            sym::RightArrow => Operator::RightArrow,
            sym::LeftRightArrow => Operator::LeftRightArrow,
            sym::LeftTeeArrow => Operator::LeftTeeArrow,
            sym::RightTeeArrow => Operator::RightTeeArrow,
            sym::RightArrowLeftArrow => Operator::RightArrowLeftArrow,
            sym::LeftArrowRightArrow => Operator::LeftArrowRightArrow,
            sym::DoubleLeftArrow => Operator::DoubleLeftArrow,
            sym::DoubleRightArrow => Operator::DoubleRightArrow,
            sym::DoubleLeftRightArrow => Operator::DoubleLeftRightArrow,
            sym::LeftArrowBar => Operator::LeftArrowBar,
            sym::RightArrowBar => Operator::RightArrowBar,
            sym::ShortRightArrow => Operator::ShortRightArrow,
            sym::ShortLeftArrow => Operator::ShortLeftArrow,
            sym::UpperLeftArrow => Operator::UpperLeftArrow,
            sym::UpperRightArrow => Operator::UpperRightArrow,
            sym::LowerRightArrow => Operator::LowerRightArrow,
            sym::LowerLeftArrow => Operator::LowerLeftArrow,
            sym::LeftVector => Operator::LeftVector,
            sym::RightVector => Operator::RightVector,
            sym::LeftRightVector => Operator::LeftRightVector,
            sym::LeftVectorBar => Operator::LeftVectorBar,
            sym::RightVectorBar => Operator::RightVectorBar,
            sym::LeftTeeVector => Operator::LeftTeeVector,
            sym::RightTeeVector => Operator::RightTeeVector,
            sym::DownLeftVector => Operator::DownLeftVector,
            sym::DownRightVector => Operator::DownRightVector,
            sym::DownLeftRightVector => Operator::DownLeftRightVector,
            sym::DownLeftVectorBar => Operator::DownLeftVectorBar,
            sym::DownRightVectorBar => Operator::DownRightVectorBar,
            sym::DownLeftTeeVector => Operator::DownLeftTeeVector,
            sym::DownRightTeeVector => Operator::DownRightTeeVector,
            sym::UpArrow => Operator::UpArrow,
            sym::DownArrow => Operator::DownArrow,
            sym::UpDownArrow => Operator::UpDownArrow,
            sym::UpTeeArrow => Operator::UpTeeArrow,
            sym::DownTeeArrow => Operator::DownTeeArrow,
            sym::UpArrowDownArrow => Operator::UpArrowDownArrow,
            sym::DoubleUpArrow => Operator::DoubleUpArrow,
            sym::DoubleDownArrow => Operator::DoubleDownArrow,
            sym::DoubleUpDownArrow => Operator::DoubleUpDownArrow,
            sym::DownArrowUpArrow => Operator::DownArrowUpArrow,
            sym::LongLeftArrow => Operator::LongLeftArrow,
            sym::LongRightArrow => Operator::LongRightArrow,
            sym::LongLeftRightArrow => Operator::LongLeftRightArrow,
            sym::DoubleLongLeftArrow => Operator::DoubleLongLeftArrow,
            sym::DoubleLongRightArrow => Operator::DoubleLongRightArrow,
            sym::DoubleLongLeftRightArrow => Operator::DoubleLongLeftRightArrow,
            sym::UpArrowBar => Operator::UpArrowBar,
            sym::DownArrowBar => Operator::DownArrowBar,
            sym::ShortUpArrow => Operator::ShortUpArrow,
            sym::ShortDownArrow => Operator::ShortDownArrow,
            sym::RightUpVector => Operator::RightUpVector,
            sym::LeftUpVector => Operator::LeftUpVector,
            sym::RightDownVector => Operator::RightDownVector,
            sym::LeftDownVector => Operator::LeftDownVector,
            sym::RightUpDownVector => Operator::RightUpDownVector,
            sym::LeftUpDownVector => Operator::LeftUpDownVector,
            sym::RightUpVectorBar => Operator::RightUpVectorBar,
            sym::RightDownVectorBar => Operator::RightDownVectorBar,
            sym::LeftUpVectorBar => Operator::LeftUpVectorBar,
            sym::LeftDownVectorBar => Operator::LeftDownVectorBar,
            sym::RightUpTeeVector => Operator::RightUpTeeVector,
            sym::RightDownTeeVector => Operator::RightDownTeeVector,
            sym::LeftUpTeeVector => Operator::LeftUpTeeVector,
            sym::LeftDownTeeVector => Operator::LeftDownTeeVector,
            sym::UpEquilibrium => Operator::UpEquilibrium,
            sym::ReverseUpEquilibrium => Operator::ReverseUpEquilibrium,
            sym::CenterDot => Operator::CenterDot,
            sym::Equivalent => Operator::Equivalent,
            sym::CircleDot => Operator::CircleDot,
            sym::Conditioned => Operator::Conditioned,
            sym::Union => Operator::Union,
            sym::SquareUnion => Operator::SquareUnion,
            sym::UnionPlus => Operator::UnionPlus,
            sym::Intersection => Operator::Intersection,
            sym::SquareIntersection => Operator::SquareIntersection,
            sym::TensorWedge => Operator::TensorWedge,
            sym::TensorProduct => Operator::TensorProduct,
            sym::Cross => Operator::Cross,
            sym::SmallCircle => Operator::SmallCircle,
            sym::Divisible => Operator::Divisible,
            sym::VerticalSeparator => Operator::VerticalSeparator,
            sym::Backslash => Operator::Backslash,
            sym::Diamond => Operator::Diamond,
            sym::Wedge => Operator::Wedge,
            sym::Vee => Operator::Vee,
            sym::Star => Operator::Star,
            sym::VerticalTilde => Operator::VerticalTilde,
            sym::Cap => Operator::Cap,
            sym::Cup => Operator::Cup,
            sym::CirclePlus => Operator::CirclePlus,
            sym::VerticalBar => Operator::VerticalBar,
            sym::DoubleVerticalBar => Operator::DoubleVerticalBar,
            sym::NotVerticalBar => Operator::NotVerticalBar,
            sym::NotDoubleVerticalBar => Operator::NotDoubleVerticalBar,
            sym::LeftTriangle => Operator::LeftTriangle,
            sym::RightTriangle => Operator::RightTriangle,
            sym::NotLeftTriangle => Operator::NotLeftTriangle,
            sym::NotRightTriangle => Operator::NotRightTriangle,
            sym::LeftTriangleEqual => Operator::LeftTriangleEqual,
            sym::RightTriangleEqual => Operator::RightTriangleEqual,
            sym::NotLeftTriangleEqual => Operator::NotLeftTriangleEqual,
            sym::NotRightTriangleEqual => Operator::NotRightTriangleEqual,
            sym::LeftTriangleBar => Operator::LeftTriangleBar,
            sym::RightTriangleBar => Operator::RightTriangleBar,
            sym::NotLeftTriangleBar => Operator::NotLeftTriangleBar,
            sym::NotRightTriangleBar => Operator::NotRightTriangleBar,
            sym::TildeEqual => Operator::TildeEqual,
            sym::NotTildeEqual => Operator::NotTildeEqual,
            sym::TildeFullEqual => Operator::TildeFullEqual,
            sym::NotTildeFullEqual => Operator::NotTildeFullEqual,
            sym::Tilde => Operator::Tilde,
            sym::NotTilde => Operator::NotTilde,
            sym::EqualTilde => Operator::EqualTilde,
            sym::NotEqualTilde => Operator::NotEqualTilde,
            sym::TildeTilde => Operator::TildeTilde,
            sym::NotTildeTilde => Operator::NotTildeTilde,
            sym::Proportional => Operator::Proportional,
            sym::Proportion => Operator::Proportion,
            sym::Congruent => Operator::Congruent,
            sym::NotCongruent => Operator::NotCongruent,
            sym::Equilibrium => Operator::Equilibrium,
            sym::ReverseEquilibrium => Operator::ReverseEquilibrium,
            sym::DotEqual => Operator::DotEqual,
            sym::Precedes => Operator::Precedes,
            sym::Succeeds => Operator::Succeeds,
            sym::PrecedesEqual => Operator::PrecedesEqual,
            sym::SucceedsEqual => Operator::SucceedsEqual,
            sym::PrecedesTilde => Operator::PrecedesTilde,
            sym::SucceedsTilde => Operator::SucceedsTilde,
            sym::PrecedesSlantEqual => Operator::PrecedesSlantEqual,
            sym::SucceedsSlantEqual => Operator::SucceedsSlantEqual,
            sym::NotPrecedes => Operator::NotPrecedes,
            sym::NotSucceeds => Operator::NotSucceeds,
            sym::NotPrecedesEqual => Operator::NotPrecedesEqual,
            sym::NotSucceedsEqual => Operator::NotSucceedsEqual,
            sym::NotPrecedesTilde => Operator::NotPrecedesTilde,
            sym::NotSucceedsTilde => Operator::NotSucceedsTilde,
            sym::NotPrecedesSlantEqual => Operator::NotPrecedesSlantEqual,
            sym::NotSucceedsSlantEqual => Operator::NotSucceedsSlantEqual,
            sym::CupCap => Operator::CupCap,
            sym::NotCupCap => Operator::NotCupCap,
            sym::HumpEqual => Operator::HumpEqual,
            sym::HumpDownHump => Operator::HumpDownHump,
            sym::NotHumpEqual => Operator::NotHumpEqual,
            sym::NotHumpDownHump => Operator::NotHumpDownHump,
            sym::CodeParser_InfixInequality => Operator::CodeParser_InfixInequality,
            sym::PermutationProduct => Operator::PermutationProduct,
            sym::Colon => Operator::Colon,
            sym::Xnor => Operator::Xnor,
            sym::Repeated => Operator::Repeated,
            sym::Factorial => Operator::Factorial,
            sym::Decrement => Operator::Decrement,
            sym::Increment => Operator::Increment,
            sym::RepeatedNull => Operator::RepeatedNull,
            sym::Factorial2 => Operator::Factorial2,
            sym::Derivative => Operator::Derivative,
            sym::Transpose => Operator::Transpose,
            sym::Conjugate => Operator::Conjugate,
            sym::ConjugateTranspose => Operator::ConjugateTranspose,
            sym::HermitianConjugate => Operator::HermitianConjugate,
            sym::InvisiblePostfixScriptBase => Operator::InvisiblePostfixScriptBase,
            _ => return None,
        };

        Some(operator)
    }
}
impl PrefixBinaryOperator {
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn to_symbol(self) -> Symbol {
        match self {
            PrefixBinaryOperator::Integrate => sym::Integrate,
            PrefixBinaryOperator::ContourIntegral => sym::ContourIntegral,
            PrefixBinaryOperator::DoubleContourIntegral => sym::DoubleContourIntegral,
            PrefixBinaryOperator::ClockwiseContourIntegral => sym::ClockwiseContourIntegral,
            PrefixBinaryOperator::CounterClockwiseContourIntegral => sym::CounterClockwiseContourIntegral,
        }
    }

    #[doc(hidden)]
    pub fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Integrate => PrefixBinaryOperator::Integrate,
            sym::ContourIntegral => PrefixBinaryOperator::ContourIntegral,
            sym::DoubleContourIntegral => PrefixBinaryOperator::DoubleContourIntegral,
            sym::ClockwiseContourIntegral => PrefixBinaryOperator::ClockwiseContourIntegral,
            sym::CounterClockwiseContourIntegral => PrefixBinaryOperator::CounterClockwiseContourIntegral,
            _ => return None,
        };

        Some(operator)
    }
}
impl GroupOperator {
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn to_symbol(self) -> Symbol {
        match self {
            GroupOperator::Token_Comment => sym::Token_Comment,
            GroupOperator::CodeParser_GroupParen => sym::CodeParser_GroupParen,
            GroupOperator::CodeParser_GroupSquare => sym::CodeParser_GroupSquare,
            GroupOperator::List => sym::List,
            GroupOperator::Association => sym::Association,
            GroupOperator::CodeParser_GroupTypeSpecifier => sym::CodeParser_GroupTypeSpecifier,
            GroupOperator::AngleBracket => sym::AngleBracket,
            GroupOperator::Ceiling => sym::Ceiling,
            GroupOperator::Floor => sym::Floor,
            GroupOperator::CodeParser_GroupDoubleBracket => sym::CodeParser_GroupDoubleBracket,
            GroupOperator::BracketingBar => sym::BracketingBar,
            GroupOperator::DoubleBracketingBar => sym::DoubleBracketingBar,
            GroupOperator::CurlyQuote => sym::CurlyQuote,
            GroupOperator::CurlyDoubleQuote => sym::CurlyDoubleQuote,
        }
    }

    #[doc(hidden)]
    pub fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Token_Comment => GroupOperator::Token_Comment,
            sym::CodeParser_GroupParen => GroupOperator::CodeParser_GroupParen,
            sym::CodeParser_GroupSquare => GroupOperator::CodeParser_GroupSquare,
            sym::List => GroupOperator::List,
            sym::Association => GroupOperator::Association,
            sym::CodeParser_GroupTypeSpecifier => GroupOperator::CodeParser_GroupTypeSpecifier,
            sym::AngleBracket => GroupOperator::AngleBracket,
            sym::Ceiling => GroupOperator::Ceiling,
            sym::Floor => GroupOperator::Floor,
            sym::CodeParser_GroupDoubleBracket => GroupOperator::CodeParser_GroupDoubleBracket,
            sym::BracketingBar => GroupOperator::BracketingBar,
            sym::DoubleBracketingBar => GroupOperator::DoubleBracketingBar,
            sym::CurlyQuote => GroupOperator::CurlyQuote,
            sym::CurlyDoubleQuote => GroupOperator::CurlyDoubleQuote,
            _ => return None,
        };

        Some(operator)
    }
}
impl CompoundOperator {
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn to_symbol(self) -> Symbol {
        match self {
            CompoundOperator::Blank => sym::Blank,
            CompoundOperator::BlankSequence => sym::BlankSequence,
            CompoundOperator::BlankNullSequence => sym::BlankNullSequence,
            CompoundOperator::Slot => sym::Slot,
            CompoundOperator::SlotSequence => sym::SlotSequence,
            CompoundOperator::Out => sym::Out,
            CompoundOperator::CodeParser_PatternBlank => sym::CodeParser_PatternBlank,
            CompoundOperator::CodeParser_PatternBlankSequence => sym::CodeParser_PatternBlankSequence,
            CompoundOperator::CodeParser_PatternBlankNullSequence => sym::CodeParser_PatternBlankNullSequence,
            CompoundOperator::CodeParser_PatternOptionalDefault => sym::CodeParser_PatternOptionalDefault,
        }
    }

    #[doc(hidden)]
    pub fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Blank => CompoundOperator::Blank,
            sym::BlankSequence => CompoundOperator::BlankSequence,
            sym::BlankNullSequence => CompoundOperator::BlankNullSequence,
            sym::Slot => CompoundOperator::Slot,
            sym::SlotSequence => CompoundOperator::SlotSequence,
            sym::Out => CompoundOperator::Out,
            sym::CodeParser_PatternBlank => CompoundOperator::CodeParser_PatternBlank,
            sym::CodeParser_PatternBlankSequence => CompoundOperator::CodeParser_PatternBlankSequence,
            sym::CodeParser_PatternBlankNullSequence => CompoundOperator::CodeParser_PatternBlankNullSequence,
            sym::CodeParser_PatternOptionalDefault => CompoundOperator::CodeParser_PatternOptionalDefault,
            _ => return None,
        };

        Some(operator)
    }
}
impl TernaryOperator {
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn to_symbol(self) -> Symbol {
        match self {
            TernaryOperator::CodeParser_TernaryTilde => sym::CodeParser_TernaryTilde,
            TernaryOperator::CodeParser_TernaryOptionalPattern => sym::CodeParser_TernaryOptionalPattern,
            TernaryOperator::TagSet => sym::TagSet,
            TernaryOperator::TagSetDelayed => sym::TagSetDelayed,
            TernaryOperator::TagUnset => sym::TagUnset,
            TernaryOperator::Span => sym::Span,
        }
    }

    #[doc(hidden)]
    pub fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::CodeParser_TernaryTilde => TernaryOperator::CodeParser_TernaryTilde,
            sym::CodeParser_TernaryOptionalPattern => TernaryOperator::CodeParser_TernaryOptionalPattern,
            sym::TagSet => TernaryOperator::TagSet,
            sym::TagSetDelayed => TernaryOperator::TagSetDelayed,
            sym::TagUnset => TernaryOperator::TagUnset,
            sym::Span => TernaryOperator::Span,
            _ => return None,
        };

        Some(operator)
    }
}
