//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#![allow(non_upper_case_globals)]

use wolfram_expr::symbol::SymbolRef;

use crate::{
	cst::Operator,
	tokenize::TokenKind,
	symbol::Symbol,
	symbols as sym,
	precedence::Precedence,
	parse::parselet::*
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

pub(crate) const PREFIX_PARSELETS: [&dyn PrefixParselet; TokenKind::Count.value() as usize] = [
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
    &PrefixOperatorParselet::new(Precedence::PREFIX_BANG, PrefixOperator::Not), // Token`Bang
    &under1Parselet, // Token`Under
    &prefixUnhandledParselet, // Token`Less
    &prefixUnhandledParselet, // Token`Greater
    &PrefixOperatorParselet::new(Precedence::PREFIX_MINUS, PrefixOperator::Minus), // Token`Minus
    &prefixUnhandledParselet, // Token`Bar
    &prefixUnhandledParselet, // Token`Semi
    &HashParselet {}, // Token`Hash
    &prefixUnhandledParselet, // Token`Amp
    &prefixUnhandledParselet, // Token`Slash
    &prefixUnhandledParselet, // Token`At
    &PrefixOperatorParselet::new(Precedence::PREFIX_PLUS, PrefixOperator::Plus), // Token`Plus
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
    &PrefixOperatorParselet::new(Precedence::PREFIX_MINUSMINUS, PrefixOperator::PreDecrement), // Token`MinusMinus
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
    &PrefixOperatorParselet::new(Precedence::PREFIX_PLUSPLUS, PrefixOperator::PreIncrement), // Token`PlusPlus
    &prefixUnhandledParselet, // Token`PlusEqual
    &prefixUnhandledParselet, // Token`TildeTilde
    &prefixUnhandledParselet, // Token`StarEqual
    &prefixUnhandledParselet, // Token`StarStar
    &prefixUnhandledParselet, // Token`CaretEqual
    &HashHashParselet {}, // Token`HashHash
    &prefixUnhandledParselet, // Token`BangEqual
    &PrefixOperatorParselet::new(Precedence::FAKE_PREFIX_BANGBANG, PrefixOperator::CodeParser_PrefixNot2), // Token`BangBang
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
    &PrefixOperatorParselet::new(Precedence::LINEARSYNTAX_BANG, PrefixOperator::CodeParser_PrefixLinearSyntaxBang), // Token`LinearSyntax`Bang
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
    &PrefixOperatorParselet::new(Precedence::LONGNAME_NOT, PrefixOperator::Not), // Token`LongName`Not
    &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_PLUSMINUS, PrefixOperator::PlusMinus), // Token`LongName`PlusMinus
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
    &PrefixOperatorParselet::new(Precedence::LONGNAME_FORALL, PrefixOperator::ForAll), // Token`LongName`ForAll
    &prefixUnsupportedTokenParselet, // Token`LongName`PartialD
    &PrefixOperatorParselet::new(Precedence::LONGNAME_EXISTS, PrefixOperator::Exists), // Token`LongName`Exists
    &PrefixOperatorParselet::new(Precedence::LONGNAME_NOTEXISTS, PrefixOperator::NotExists), // Token`LongName`NotExists
    &PrefixOperatorParselet::new(Precedence::LONGNAME_DEL, PrefixOperator::Del), // Token`LongName`Del
    &prefixUnhandledParselet, // Token`LongName`Element
    &prefixUnhandledParselet, // Token`LongName`NotElement
    &prefixUnhandledParselet, // Token`LongName`ReverseElement
    &prefixUnhandledParselet, // Token`LongName`NotReverseElement
    &prefixUnhandledParselet, // Token`LongName`SuchThat
    &PrefixOperatorParselet::new(Precedence::LONGNAME_PRODUCT, PrefixOperator::Product), // Token`LongName`Product
    &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_COPRODUCT, PrefixOperator::Coproduct), // Token`LongName`Coproduct
    &PrefixOperatorParselet::new(Precedence::LONGNAME_SUM, PrefixOperator::Sum), // Token`LongName`Sum
    &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_MINUS, PrefixOperator::Minus), // Token`LongName`Minus
    &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_MINUSPLUS, PrefixOperator::MinusPlus), // Token`LongName`MinusPlus
    &prefixUnhandledParselet, // Token`LongName`DivisionSlash
    &prefixUnhandledParselet, // Token`LongName`Backslash
    &prefixUnhandledParselet, // Token`LongName`SmallCircle
    &PrefixOperatorParselet::new(Precedence::LONGNAME_SQRT, PrefixOperator::Sqrt), // Token`LongName`Sqrt
    &PrefixOperatorParselet::new(Precedence::LONGNAME_CUBEROOT, PrefixOperator::CubeRoot), // Token`LongName`CubeRoot
    &prefixUnhandledParselet, // Token`LongName`Proportional
    &prefixUnhandledParselet, // Token`LongName`Divides
    &prefixUnhandledParselet, // Token`LongName`DoubleVerticalBar
    &prefixUnhandledParselet, // Token`LongName`NotDoubleVerticalBar
    &prefixUnhandledParselet, // Token`LongName`And
    &prefixUnhandledParselet, // Token`LongName`Or
    &IntegralParselet::new(PrefixBinaryOperator::Integrate, PrefixOperator::Integral), // Token`LongName`Integral
    &IntegralParselet::new(PrefixBinaryOperator::ContourIntegral, PrefixOperator::ContourIntegral), // Token`LongName`ContourIntegral
    &IntegralParselet::new(PrefixBinaryOperator::DoubleContourIntegral, PrefixOperator::DoubleContourIntegral), // Token`LongName`DoubleContourIntegral
    &IntegralParselet::new(PrefixBinaryOperator::ClockwiseContourIntegral, PrefixOperator::ClockwiseContourIntegral), // Token`LongName`ClockwiseContourIntegral
    &IntegralParselet::new(PrefixBinaryOperator::CounterClockwiseContourIntegral, PrefixOperator::CounterClockwiseContourIntegral), // Token`LongName`CounterClockwiseContourIntegral
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
    &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_CIRCLETIMES, PrefixOperator::CircleTimes), // Token`LongName`CircleTimes
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
    &PrefixOperatorParselet::new(Precedence::LONGNAME_PIECEWISE, PrefixOperator::Piecewise), // Token`LongName`Piecewise
    &prefixUnhandledParselet, // Token`LongName`ImplicitPlus
    &prefixUnsupportedTokenParselet, // Token`LongName`AutoLeftMatch
    &prefixUnsupportedTokenParselet, // Token`LongName`AutoRightMatch
    &PrefixOperatorParselet::new(Precedence::LONGNAME_INVISIBLEPREFIXSCRIPTBASE, PrefixOperator::InvisiblePrefixScriptBase), // Token`LongName`InvisiblePrefixScriptBase
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
    &PrefixOperatorParselet::new(Precedence::LONGNAME_CONTINUEDFRACTIONK, PrefixOperator::ContinuedFractionK), // Token`LongName`ContinuedFractionK
    &prefixUnhandledParselet, // Token`LongName`TensorProduct
    &prefixUnhandledParselet, // Token`LongName`TensorWedge
    &PrefixOperatorParselet::new(Precedence::LONGNAME_PROBABILITYPR, PrefixOperator::ProbabilityPr), // Token`LongName`ProbabilityPr
    &PrefixOperatorParselet::new(Precedence::LONGNAME_EXPECTATIONE, PrefixOperator::ExpectationE), // Token`LongName`ExpectationE
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
    &PrefixOperatorParselet::new(Precedence::LONGNAME_SQUARE, PrefixOperator::Square), // Token`LongName`Square
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
    &PrefixOperatorParselet::new(Precedence::LONGNAME_CAPITALDIFFERENTIALD, PrefixOperator::CapitalDifferentialD), // Token`LongName`CapitalDifferentialD
    &PrefixOperatorParselet::new(Precedence::LONGNAME_DIFFERENTIALD, PrefixOperator::DifferentialD), // Token`LongName`DifferentialD
    &prefixCommaParselet, // Token`LongName`InvisibleComma
    &prefixUnhandledParselet, // Token`LongName`InvisibleApplication
    &prefixUnhandledParselet, // Token`LongName`LongEqual
];

//
//
//
pub(crate) const INFIX_PARSELETS: [&dyn InfixParselet; TokenKind::Count.value() as usize] = [
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
    &InfixOperatorParselet::new(Precedence::DOT, InfixOperator::Dot), // Token`Dot
    &colonParselet, // Token`Colon
    &infixImplicitTimesParselet, // Token`OpenParen
    &infixAssertFalseParselet, // Token`CloseParen
    &(CallParselet::new(&squareGroupParselet)), // Token`OpenSquare
    &infixAssertFalseParselet, // Token`CloseSquare
    &commaParselet, // Token`Comma
    &infixImplicitTimesParselet, // Token`OpenCurly
    &infixAssertFalseParselet, // Token`CloseCurly
    &equalParselet, // Token`Equal
    &PostfixOperatorParselet::new(Precedence::POSTFIX_BANG, PostfixOperator::Factorial), // Token`Bang
    &infixImplicitTimesParselet, // Token`Under
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`Less
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`Greater
    &InfixOperatorParselet::new(Precedence::INFIX_MINUS, InfixOperator::Plus), // Token`Minus
    &InfixOperatorParselet::new(Precedence::BAR, InfixOperator::Alternatives), // Token`Bar
    &semiParselet, // Token`Semi
    &infixImplicitTimesParselet, // Token`Hash
    &PostfixOperatorParselet::new(Precedence::AMP, PostfixOperator::Function), // Token`Amp
    &BinaryOperatorParselet::new(Precedence::SLASH, BinaryOperator::Divide), // Token`Slash
    &BinaryOperatorParselet::new(Precedence::AT, BinaryOperator::CodeParser_BinaryAt), // Token`At
    &InfixOperatorParselet::new(Precedence::INFIX_PLUS, InfixOperator::Plus), // Token`Plus
    (&TildeParselet {}), // Token`Tilde
    &timesParselet, // Token`Star
    &BinaryOperatorParselet::new(Precedence::CARET, BinaryOperator::Power), // Token`Caret
    &PostfixOperatorParselet::new(Precedence::SINGLEQUOTE, PostfixOperator::Derivative), // Token`SingleQuote
    &infixImplicitTimesParselet, // Token`Percent
    &BinaryOperatorParselet::new(Precedence::INFIX_QUESTION, BinaryOperator::PatternTest), // Token`Question
    &PostfixOperatorParselet::new(Precedence::DOTDOT, PostfixOperator::Repeated), // Token`DotDot
    (&ColonColonParselet {}), // Token`ColonColon
    &colonEqualParselet, // Token`ColonEqual
    &BinaryOperatorParselet::new(Precedence::COLONGREATER, BinaryOperator::RuleDelayed), // Token`ColonGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`EqualEqual
    &infixImplicitTimesParselet, // Token`UnderUnder
    &infixImplicitTimesParselet, // Token`UnderDot
    &infixImplicitTimesParselet, // Token`LessBar
    &infixImplicitTimesParselet, // Token`LessLess
    &InfixOperatorParselet::new(Precedence::LESSGREATER, InfixOperator::StringJoin), // Token`LessGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LessEqual
    (&GreaterGreaterParselet {}), // Token`GreaterGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`GreaterEqual
    &BinaryOperatorParselet::new(Precedence::MINUSGREATER, BinaryOperator::Rule), // Token`MinusGreater
    &PostfixOperatorParselet::new(Precedence::POSTFIX_MINUSMINUS, PostfixOperator::Decrement), // Token`MinusMinus
    &BinaryOperatorParselet::new(Precedence::MINUSEQUAL, BinaryOperator::SubtractFrom), // Token`MinusEqual
    &InfixOperatorParselet::new(Precedence::BARBAR, InfixOperator::Or), // Token`BarBar
    &infixAssertFalseParselet, // Token`BarGreater
    &semiSemiParselet, // Token`SemiSemi
    &InfixOperatorParselet::new(Precedence::AMPAMP, InfixOperator::And), // Token`AmpAmp
    &BinaryOperatorParselet::new(Precedence::SLASHAT, BinaryOperator::Map), // Token`SlashAt
    &BinaryOperatorParselet::new(Precedence::SLASHSEMI, BinaryOperator::Condition), // Token`SlashSemi
    &BinaryOperatorParselet::new(Precedence::SLASHDOT, BinaryOperator::ReplaceAll), // Token`SlashDot
    &BinaryOperatorParselet::new(Precedence::SLASHSLASH, BinaryOperator::CodeParser_BinarySlashSlash), // Token`SlashSlash
    &slashColonParselet, // Token`SlashColon
    &BinaryOperatorParselet::new(Precedence::SLASHEQUAL, BinaryOperator::DivideBy), // Token`SlashEqual
    &InfixOperatorParselet::new(Precedence::SLASHSTAR, InfixOperator::RightComposition), // Token`SlashStar
    &BinaryOperatorParselet::new(Precedence::ATAT, BinaryOperator::Apply), // Token`AtAt
    &InfixOperatorParselet::new(Precedence::ATSTAR, InfixOperator::Composition), // Token`AtStar
    &PostfixOperatorParselet::new(Precedence::POSTFIX_PLUSPLUS, PostfixOperator::Increment), // Token`PlusPlus
    &BinaryOperatorParselet::new(Precedence::PLUSEQUAL, BinaryOperator::AddTo), // Token`PlusEqual
    &InfixOperatorParselet::new(Precedence::TILDETILDE, InfixOperator::StringExpression), // Token`TildeTilde
    &BinaryOperatorParselet::new(Precedence::STAREQUAL, BinaryOperator::TimesBy), // Token`StarEqual
    &InfixOperatorParselet::new(Precedence::STARSTAR, InfixOperator::NonCommutativeMultiply), // Token`StarStar
    &BinaryOperatorParselet::new(Precedence::CARETEQUAL, BinaryOperator::UpSet), // Token`CaretEqual
    &infixImplicitTimesParselet, // Token`HashHash
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`BangEqual
    &PostfixOperatorParselet::new(Precedence::POSTFIX_BANGBANG, PostfixOperator::Factorial2), // Token`BangBang
    &infixAssertFalseParselet, // Token`QuestionQuestion
    &PostfixOperatorParselet::new(Precedence::DOTDOTDOT, PostfixOperator::RepeatedNull), // Token`DotDotDot
    &InfixOperatorParselet::new(Precedence::EQUALEQUALEQUAL, InfixOperator::SameQ), // Token`EqualEqualEqual
    &InfixOperatorParselet::new(Precedence::EQUALBANGEQUAL, InfixOperator::UnsameQ), // Token`EqualBangEqual
    &infixImplicitTimesParselet, // Token`UnderUnderUnder
    &BinaryOperatorParselet::new(Precedence::SLASHSLASHDOT, BinaryOperator::ReplaceRepeated), // Token`SlashSlashDot
    &BinaryOperatorParselet::new(Precedence::ATATAT, BinaryOperator::MapApply), // Token`AtAtAt
    &BinaryOperatorParselet::new(Precedence::LESSMINUSGREATER, BinaryOperator::TwoWayRule), // Token`LessMinusGreater
    &BinaryOperatorParselet::new(Precedence::SLASHSLASHAT, BinaryOperator::MapAll), // Token`SlashSlashAt
    &BinaryOperatorParselet::new(Precedence::CARETCOLONEQUAL, BinaryOperator::UpSetDelayed), // Token`CaretColonEqual
    (&GreaterGreaterGreaterParselet {}), // Token`GreaterGreaterGreater
    &BinaryOperatorParselet::new(Precedence::BARMINUSGREATER, BinaryOperator::Function), // Token`BarMinusGreater
    &BinaryOperatorParselet::new(Precedence::SLASHSLASHEQUAL, BinaryOperator::ApplyTo), // Token`SlashSlashEqual
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
    &BinaryOperatorParselet::new(Precedence::INFIX_LONGNAME_PLUSMINUS, BinaryOperator::PlusMinus), // Token`LongName`PlusMinus
    &InfixOperatorParselet::new(Precedence::LONGNAME_CENTERDOT, InfixOperator::CenterDot), // Token`LongName`CenterDot
    &timesParselet, // Token`LongName`Times
    &BinaryOperatorParselet::new(Precedence::LONGNAME_DIVIDE, BinaryOperator::Divide), // Token`LongName`Divide
    &infixImplicitTimesParselet, // Token`LongName`OpenCurlyQuote
    &infixAssertFalseParselet, // Token`LongName`CloseCurlyQuote
    &infixImplicitTimesParselet, // Token`LongName`OpenCurlyDoubleQuote
    &infixAssertFalseParselet, // Token`LongName`CloseCurlyDoubleQuote
    &timesParselet, // Token`LongName`InvisibleTimes
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftArrow), // Token`LongName`LeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpArrow), // Token`LongName`UpArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightArrow), // Token`LongName`RightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownArrow), // Token`LongName`DownArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftRightArrow), // Token`LongName`LeftRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpDownArrow), // Token`LongName`UpDownArrow
    &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::UpperLeftArrow), // Token`LongName`UpperLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::UpperRightArrow), // Token`LongName`UpperRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::LowerRightArrow), // Token`LongName`LowerRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::LowerLeftArrow), // Token`LongName`LowerLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftTeeArrow), // Token`LongName`LeftTeeArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpTeeArrow), // Token`LongName`UpTeeArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightTeeArrow), // Token`LongName`RightTeeArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownTeeArrow), // Token`LongName`DownTeeArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftVector), // Token`LongName`LeftVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftVector), // Token`LongName`DownLeftVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpVector), // Token`LongName`RightUpVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpVector), // Token`LongName`LeftUpVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::RightVector), // Token`LongName`RightVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownRightVector), // Token`LongName`DownRightVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightDownVector), // Token`LongName`RightDownVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftDownVector), // Token`LongName`LeftDownVector
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightArrowLeftArrow), // Token`LongName`RightArrowLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpArrowDownArrow), // Token`LongName`UpArrowDownArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftArrowRightArrow), // Token`LongName`LeftArrowRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::ReverseEquilibrium), // Token`LongName`ReverseEquilibrium
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Equilibrium), // Token`LongName`Equilibrium
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::DoubleLeftArrow), // Token`LongName`DoubleLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleUpArrow), // Token`LongName`DoubleUpArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::DoubleRightArrow), // Token`LongName`DoubleRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleDownArrow), // Token`LongName`DoubleDownArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::DoubleLeftRightArrow), // Token`LongName`DoubleLeftRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleUpDownArrow), // Token`LongName`DoubleUpDownArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftArrowBar), // Token`LongName`LeftArrowBar
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightArrowBar), // Token`LongName`RightArrowBar
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownArrowUpArrow), // Token`LongName`DownArrowUpArrow
    &infixImplicitTimesParselet, // Token`LongName`ForAll
    &infixAssertFalseParselet, // Token`LongName`PartialD
    &infixImplicitTimesParselet, // Token`LongName`Exists
    &infixImplicitTimesParselet, // Token`LongName`NotExists
    &infixImplicitTimesParselet, // Token`LongName`Del
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Element), // Token`LongName`Element
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotElement), // Token`LongName`NotElement
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::ReverseElement), // Token`LongName`ReverseElement
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotReverseElement), // Token`LongName`NotReverseElement
    &BinaryOperatorParselet::new(Precedence::LONGNAME_SUCHTHAT, BinaryOperator::SuchThat), // Token`LongName`SuchThat
    &infixImplicitTimesParselet, // Token`LongName`Product
    &InfixOperatorParselet::new(Precedence::INFIX_LONGNAME_COPRODUCT, InfixOperator::Coproduct), // Token`LongName`Coproduct
    &infixImplicitTimesParselet, // Token`LongName`Sum
    &InfixOperatorParselet::new(Precedence::INFIX_LONGNAME_MINUS, InfixOperator::Plus), // Token`LongName`Minus
    &BinaryOperatorParselet::new(Precedence::INFIX_LONGNAME_MINUSPLUS, BinaryOperator::MinusPlus), // Token`LongName`MinusPlus
    &BinaryOperatorParselet::new(Precedence::LONGNAME_DIVISIONSLASH, BinaryOperator::Divide), // Token`LongName`DivisionSlash
    &InfixOperatorParselet::new(Precedence::LONGNAME_BACKSLASH, InfixOperator::Backslash), // Token`LongName`Backslash
    &InfixOperatorParselet::new(Precedence::LONGNAME_SMALLCIRCLE, InfixOperator::SmallCircle), // Token`LongName`SmallCircle
    &infixImplicitTimesParselet, // Token`LongName`Sqrt
    &infixImplicitTimesParselet, // Token`LongName`CubeRoot
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Proportional), // Token`LongName`Proportional
    &InfixOperatorParselet::new(Precedence::LONGNAME_DIVIDES, InfixOperator::Divisible), // Token`LongName`Divides
    &InfixOperatorParselet::new(Precedence::LONGNAME_DOUBLEVERTICALBAR, InfixOperator::DoubleVerticalBar), // Token`LongName`DoubleVerticalBar
    &InfixOperatorParselet::new(Precedence::LONGNAME_NOTDOUBLEVERTICALBAR, InfixOperator::NotDoubleVerticalBar), // Token`LongName`NotDoubleVerticalBar
    &InfixOperatorParselet::new(Precedence::LONGNAME_AND, InfixOperator::And), // Token`LongName`And
    &InfixOperatorParselet::new(Precedence::LONGNAME_OR, InfixOperator::Or), // Token`LongName`Or
    &infixImplicitTimesParselet, // Token`LongName`Integral
    &infixImplicitTimesParselet, // Token`LongName`ContourIntegral
    &infixImplicitTimesParselet, // Token`LongName`DoubleContourIntegral
    &infixImplicitTimesParselet, // Token`LongName`ClockwiseContourIntegral
    &infixImplicitTimesParselet, // Token`LongName`CounterClockwiseContourIntegral
    &BinaryOperatorParselet::new(Precedence::LONGNAME_THEREFORE, BinaryOperator::Therefore), // Token`LongName`Therefore
    &BinaryOperatorParselet::new(Precedence::LONGNAME_BECAUSE, BinaryOperator::Because), // Token`LongName`Because
    &InfixOperatorParselet::new(Precedence::LONGNAME_COLON, InfixOperator::Colon), // Token`LongName`Colon
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Proportion), // Token`LongName`Proportion
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Tilde), // Token`LongName`Tilde
    &InfixOperatorParselet::new(Precedence::LONGNAME_VERTICALTILDE, InfixOperator::VerticalTilde), // Token`LongName`VerticalTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTilde), // Token`LongName`NotTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::EqualTilde), // Token`LongName`EqualTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::TildeEqual), // Token`LongName`TildeEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTildeEqual), // Token`LongName`NotTildeEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::TildeFullEqual), // Token`LongName`TildeFullEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTildeFullEqual), // Token`LongName`NotTildeFullEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::TildeTilde), // Token`LongName`TildeTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTildeTilde), // Token`LongName`NotTildeTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::CupCap), // Token`LongName`CupCap
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::HumpDownHump), // Token`LongName`HumpDownHump
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::HumpEqual), // Token`LongName`HumpEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::DotEqual), // Token`LongName`DotEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Congruent), // Token`LongName`Congruent
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotCongruent), // Token`LongName`NotCongruent
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessFullEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterFullEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLessFullEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreaterFullEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterGreater
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotCupCap), // Token`LongName`NotCupCap
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLessEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreaterEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessTilde
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterTilde
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLessTilde
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreaterTilde
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLessGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreaterLess
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Precedes), // Token`LongName`Precedes
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Succeeds), // Token`LongName`Succeeds
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::PrecedesSlantEqual), // Token`LongName`PrecedesSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::SucceedsSlantEqual), // Token`LongName`SucceedsSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::PrecedesTilde), // Token`LongName`PrecedesTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::SucceedsTilde), // Token`LongName`SucceedsTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedes), // Token`LongName`NotPrecedes
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceeds), // Token`LongName`NotSucceeds
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Subset), // Token`LongName`Subset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Superset), // Token`LongName`Superset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSubset), // Token`LongName`NotSubset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSuperset), // Token`LongName`NotSuperset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SubsetEqual), // Token`LongName`SubsetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SupersetEqual), // Token`LongName`SupersetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSubsetEqual), // Token`LongName`NotSubsetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSupersetEqual), // Token`LongName`NotSupersetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_UNIONOPERATORS, InfixOperator::UnionPlus), // Token`LongName`UnionPlus
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSubset), // Token`LongName`SquareSubset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSuperset), // Token`LongName`SquareSuperset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSubsetEqual), // Token`LongName`SquareSubsetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSupersetEqual), // Token`LongName`SquareSupersetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INTERSECTIONOPERATORS, InfixOperator::SquareIntersection), // Token`LongName`SquareIntersection
    &InfixOperatorParselet::new(Precedence::CLASS_UNIONOPERATORS, InfixOperator::SquareUnion), // Token`LongName`SquareUnion
    &InfixOperatorParselet::new(Precedence::LONGNAME_CIRCLEPLUS, InfixOperator::CirclePlus), // Token`LongName`CirclePlus
    &BinaryOperatorParselet::new(Precedence::LONGNAME_CIRCLEMINUS, BinaryOperator::CircleMinus), // Token`LongName`CircleMinus
    &InfixOperatorParselet::new(Precedence::INFIX_LONGNAME_CIRCLETIMES, InfixOperator::CircleTimes), // Token`LongName`CircleTimes
    &InfixOperatorParselet::new(Precedence::LONGNAME_CIRCLEDOT, InfixOperator::CircleDot), // Token`LongName`CircleDot
    &BinaryOperatorParselet::new(Precedence::LONGNAME_RIGHTTEE, BinaryOperator::RightTee), // Token`LongName`RightTee
    &BinaryOperatorParselet::new(Precedence::LONGNAME_LEFTTEE, BinaryOperator::LeftTee), // Token`LongName`LeftTee
    &BinaryOperatorParselet::new(Precedence::LONGNAME_DOWNTEE, BinaryOperator::DownTee), // Token`LongName`DownTee
    &BinaryOperatorParselet::new(Precedence::LONGNAME_UPTEE, BinaryOperator::UpTee), // Token`LongName`UpTee
    &BinaryOperatorParselet::new(Precedence::LONGNAME_DOUBLERIGHTTEE, BinaryOperator::DoubleRightTee), // Token`LongName`DoubleRightTee
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::LeftTriangle), // Token`LongName`LeftTriangle
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::RightTriangle), // Token`LongName`RightTriangle
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::LeftTriangleEqual), // Token`LongName`LeftTriangleEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::RightTriangleEqual), // Token`LongName`RightTriangleEqual
    &InfixOperatorParselet::new(Precedence::LONGNAME_XOR, InfixOperator::Xor), // Token`LongName`Xor
    &InfixOperatorParselet::new(Precedence::LONGNAME_NAND, InfixOperator::Nand), // Token`LongName`Nand
    &InfixOperatorParselet::new(Precedence::LONGNAME_NOR, InfixOperator::Nor), // Token`LongName`Nor
    &InfixOperatorParselet::new(Precedence::LONGNAME_WEDGE, InfixOperator::Wedge), // Token`LongName`Wedge
    &InfixOperatorParselet::new(Precedence::LONGNAME_VEE, InfixOperator::Vee), // Token`LongName`Vee
    &InfixOperatorParselet::new(Precedence::CLASS_INTERSECTIONOPERATORS, InfixOperator::Intersection), // Token`LongName`Intersection
    &InfixOperatorParselet::new(Precedence::CLASS_UNIONOPERATORS, InfixOperator::Union), // Token`LongName`Union
    &InfixOperatorParselet::new(Precedence::LONGNAME_DIAMOND, InfixOperator::Diamond), // Token`LongName`Diamond
    &InfixOperatorParselet::new(Precedence::LONGNAME_STAR, InfixOperator::Star), // Token`LongName`Star
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessEqualGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterEqualLess
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedesSlantEqual), // Token`LongName`NotPrecedesSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceedsSlantEqual), // Token`LongName`NotSucceedsSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSubsetEqual), // Token`LongName`NotSquareSubsetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSupersetEqual), // Token`LongName`NotSquareSupersetEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedesTilde), // Token`LongName`NotPrecedesTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceedsTilde), // Token`LongName`NotSucceedsTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotLeftTriangle), // Token`LongName`NotLeftTriangle
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotRightTriangle), // Token`LongName`NotRightTriangle
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotLeftTriangleEqual), // Token`LongName`NotLeftTriangleEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotRightTriangleEqual), // Token`LongName`NotRightTriangleEqual
    &infixImplicitTimesParselet, // Token`LongName`LeftCeiling
    &infixAssertFalseParselet, // Token`LongName`RightCeiling
    &infixImplicitTimesParselet, // Token`LongName`LeftFloor
    &infixAssertFalseParselet, // Token`LongName`RightFloor
    &InfixOperatorParselet::new(Precedence::LONGNAME_CAP, InfixOperator::Cap), // Token`LongName`Cap
    &InfixOperatorParselet::new(Precedence::LONGNAME_CUP, InfixOperator::Cup), // Token`LongName`Cup
    &infixImplicitTimesParselet, // Token`LongName`LeftAngleBracket
    &infixAssertFalseParselet, // Token`LongName`RightAngleBracket
    &BinaryOperatorParselet::new(Precedence::LONGNAME_PERPENDICULAR, BinaryOperator::Perpendicular), // Token`LongName`Perpendicular
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::LongLeftArrow), // Token`LongName`LongLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::LongRightArrow), // Token`LongName`LongRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::LongLeftRightArrow), // Token`LongName`LongLeftRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleLongLeftArrow), // Token`LongName`DoubleLongLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleLongRightArrow), // Token`LongName`DoubleLongRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleLongLeftRightArrow), // Token`LongName`DoubleLongLeftRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpArrowBar), // Token`LongName`UpArrowBar
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownArrowBar), // Token`LongName`DownArrowBar
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftRightVector), // Token`LongName`LeftRightVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpDownVector), // Token`LongName`RightUpDownVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftRightVector), // Token`LongName`DownLeftRightVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpDownVector), // Token`LongName`LeftUpDownVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftVectorBar), // Token`LongName`LeftVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::RightVectorBar), // Token`LongName`RightVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpVectorBar), // Token`LongName`RightUpVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightDownVectorBar), // Token`LongName`RightDownVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftVectorBar), // Token`LongName`DownLeftVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownRightVectorBar), // Token`LongName`DownRightVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpVectorBar), // Token`LongName`LeftUpVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftDownVectorBar), // Token`LongName`LeftDownVectorBar
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftTeeVector), // Token`LongName`LeftTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::RightTeeVector), // Token`LongName`RightTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpTeeVector), // Token`LongName`RightUpTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightDownTeeVector), // Token`LongName`RightDownTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftTeeVector), // Token`LongName`DownLeftTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownRightTeeVector), // Token`LongName`DownRightTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpTeeVector), // Token`LongName`LeftUpTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftDownTeeVector), // Token`LongName`LeftDownTeeVector
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::UpEquilibrium), // Token`LongName`UpEquilibrium
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::ReverseUpEquilibrium), // Token`LongName`ReverseUpEquilibrium
    &BinaryOperatorParselet::new(Precedence::LONGNAME_ROUNDIMPLIES, BinaryOperator::RoundImplies), // Token`LongName`RoundImplies
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::LeftTriangleBar), // Token`LongName`LeftTriangleBar
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::RightTriangleBar), // Token`LongName`RightTriangleBar
    &InfixOperatorParselet::new(Precedence::LONGNAME_EQUIVALENT, InfixOperator::Equivalent), // Token`LongName`Equivalent
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LessSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`GreaterSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NestedLessLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NestedGreaterGreater
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::PrecedesEqual), // Token`LongName`PrecedesEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::SucceedsEqual), // Token`LongName`SucceedsEqual
    &BinaryOperatorParselet::new(Precedence::LONGNAME_DOUBLELEFTTEE, BinaryOperator::DoubleLeftTee), // Token`LongName`DoubleLeftTee
    &(CallParselet::new(&doubleBracketGroupParselet)), // Token`LongName`LeftDoubleBracket
    &infixAssertFalseParselet, // Token`LongName`RightDoubleBracket
    &infixImplicitTimesParselet, // Token`LongName`LeftAssociation
    &infixAssertFalseParselet, // Token`LongName`RightAssociation
    &BinaryOperatorParselet::new(Precedence::LONGNAME_TWOWAYRULE, BinaryOperator::TwoWayRule), // Token`LongName`TwoWayRule
    &infixImplicitTimesParselet, // Token`LongName`Piecewise
    &InfixOperatorParselet::new(Precedence::LONGNAME_IMPLICITPLUS, InfixOperator::Plus), // Token`LongName`ImplicitPlus
    &infixAssertFalseParselet, // Token`LongName`AutoLeftMatch
    &infixAssertFalseParselet, // Token`LongName`AutoRightMatch
    &infixImplicitTimesParselet, // Token`LongName`InvisiblePrefixScriptBase
    &PostfixOperatorParselet::new(Precedence::LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, PostfixOperator::InvisiblePostfixScriptBase), // Token`LongName`InvisiblePostfixScriptBase
    &PostfixOperatorParselet::new(Precedence::LONGNAME_TRANSPOSE, PostfixOperator::Transpose), // Token`LongName`Transpose
    &PostfixOperatorParselet::new(Precedence::LONGNAME_CONJUGATE, PostfixOperator::Conjugate), // Token`LongName`Conjugate
    &PostfixOperatorParselet::new(Precedence::LONGNAME_CONJUGATETRANSPOSE, PostfixOperator::ConjugateTranspose), // Token`LongName`ConjugateTranspose
    &PostfixOperatorParselet::new(Precedence::LONGNAME_HERMITIANCONJUGATE, PostfixOperator::HermitianConjugate), // Token`LongName`HermitianConjugate
    &InfixOperatorParselet::new(Precedence::LONGNAME_VERTICALBAR, InfixOperator::VerticalBar), // Token`LongName`VerticalBar
    &InfixOperatorParselet::new(Precedence::LONGNAME_NOTVERTICALBAR, InfixOperator::NotVerticalBar), // Token`LongName`NotVerticalBar
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Distributed), // Token`LongName`Distributed
    &InfixOperatorParselet::new(Precedence::LONGNAME_CONDITIONED, InfixOperator::Conditioned), // Token`LongName`Conditioned
    &BinaryOperatorParselet::new(Precedence::LONGNAME_UNDIRECTEDEDGE, BinaryOperator::UndirectedEdge), // Token`LongName`UndirectedEdge
    &BinaryOperatorParselet::new(Precedence::LONGNAME_DIRECTEDEDGE, BinaryOperator::DirectedEdge), // Token`LongName`DirectedEdge
    &infixImplicitTimesParselet, // Token`LongName`ContinuedFractionK
    &InfixOperatorParselet::new(Precedence::LONGNAME_TENSORPRODUCT, InfixOperator::TensorProduct), // Token`LongName`TensorProduct
    &InfixOperatorParselet::new(Precedence::LONGNAME_TENSORWEDGE, InfixOperator::TensorWedge), // Token`LongName`TensorWedge
    &infixImplicitTimesParselet, // Token`LongName`ProbabilityPr
    &infixImplicitTimesParselet, // Token`LongName`ExpectationE
    &InfixOperatorParselet::new(Precedence::LONGNAME_PERMUTATIONPRODUCT, InfixOperator::PermutationProduct), // Token`LongName`PermutationProduct
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotEqualTilde), // Token`LongName`NotEqualTilde
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotHumpEqual), // Token`LongName`NotHumpEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotHumpDownHump), // Token`LongName`NotHumpDownHump
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotLeftTriangleBar), // Token`LongName`NotLeftTriangleBar
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotRightTriangleBar), // Token`LongName`NotRightTriangleBar
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLessLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotNestedLessLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotLessSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreaterGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotNestedGreaterGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`NotGreaterSlantEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedesEqual), // Token`LongName`NotPrecedesEqual
    &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceedsEqual), // Token`LongName`NotSucceedsEqual
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSubset), // Token`LongName`NotSquareSubset
    &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSuperset), // Token`LongName`NotSquareSuperset
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`Equal
    &InfixOperatorParselet::new(Precedence::LONGNAME_VERTICALSEPARATOR, InfixOperator::VerticalSeparator), // Token`LongName`VerticalSeparator
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`VectorGreater
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`VectorGreaterEqual
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`VectorLess
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`VectorLessEqual
    &infixAssertFalseParselet, // Token`LongName`Limit
    &infixAssertFalseParselet, // Token`LongName`MaxLimit
    &infixAssertFalseParselet, // Token`LongName`MinLimit
    &InfixOperatorParselet::new(Precedence::LONGNAME_CROSS, InfixOperator::Cross), // Token`LongName`Cross
    &BinaryOperatorParselet::new(Precedence::LONGNAME_FUNCTION, BinaryOperator::Function), // Token`LongName`Function
    &InfixOperatorParselet::new(Precedence::LONGNAME_XNOR, InfixOperator::Xnor), // Token`LongName`Xnor
    &infixAssertFalseParselet, // Token`LongName`DiscreteShift
    &infixAssertFalseParselet, // Token`LongName`DifferenceDelta
    &infixAssertFalseParselet, // Token`LongName`DiscreteRatio
    &BinaryOperatorParselet::new(Precedence::LONGNAME_RULEDELAYED, BinaryOperator::RuleDelayed), // Token`LongName`RuleDelayed
    &infixImplicitTimesParselet, // Token`LongName`Square
    &BinaryOperatorParselet::new(Precedence::LONGNAME_RULE, BinaryOperator::Rule), // Token`LongName`Rule
    &BinaryOperatorParselet::new(Precedence::LONGNAME_IMPLIES, BinaryOperator::Implies), // Token`LongName`Implies
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::ShortRightArrow), // Token`LongName`ShortRightArrow
    &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::ShortLeftArrow), // Token`LongName`ShortLeftArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::ShortUpArrow), // Token`LongName`ShortUpArrow
    &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::ShortDownArrow), // Token`LongName`ShortDownArrow
    &BinaryOperatorParselet::new(Precedence::LONGNAME_APPLICATION, BinaryOperator::Application), // Token`LongName`Application
    &infixImplicitTimesParselet, // Token`LongName`LeftBracketingBar
    &infixAssertFalseParselet, // Token`LongName`RightBracketingBar
    &infixImplicitTimesParselet, // Token`LongName`LeftDoubleBracketingBar
    &infixAssertFalseParselet, // Token`LongName`RightDoubleBracketingBar
    &infixDifferentialDParselet, // Token`LongName`CapitalDifferentialD
    &infixDifferentialDParselet, // Token`LongName`DifferentialD
    &commaParselet, // Token`LongName`InvisibleComma
    &BinaryOperatorParselet::new(Precedence::LONGNAME_INVISIBLEAPPLICATION, BinaryOperator::CodeParser_BinaryAt), // Token`LongName`InvisibleApplication
    &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality), // Token`LongName`LongEqual
];

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum InfixOperator {
    Times,
    Span,
    CompoundExpression,
    MessageName,
    CodeParser_InternalInvalid,
    CodeParser_Comma,
    CodeParser_InfixTilde,
    Plus,
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
    CircleTimes,
    Star,
    VerticalTilde,
    Coproduct,
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
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrefixOperator {
    Get,
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
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PostfixOperator {
    Function,
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
pub enum BinaryOperator {
    Pattern,
    Optional,
    Set,
    SetDelayed,
    Unset,
    Put,
    PutAppend,
    Span,
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
    PlusMinus,
    DirectedEdge,
    UndirectedEdge,
    MinusPlus,
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
pub enum CallOperator {
    CodeParser_GroupSquare,
    CodeParser_GroupDoubleBracket,
    CodeParser_GroupTypeSpecifier,
}

impl Operator for InfixOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            InfixOperator::Times => sym::Times,
            InfixOperator::Span => sym::Span,
            InfixOperator::CompoundExpression => sym::CompoundExpression,
            InfixOperator::MessageName => sym::MessageName,
            InfixOperator::CodeParser_InternalInvalid => sym::CodeParser_InternalInvalid,
            InfixOperator::CodeParser_Comma => sym::CodeParser_Comma,
            InfixOperator::CodeParser_InfixTilde => sym::CodeParser_InfixTilde,
            InfixOperator::Plus => sym::Plus,
            InfixOperator::SameQ => sym::SameQ,
            InfixOperator::UnsameQ => sym::UnsameQ,
            InfixOperator::Dot => sym::Dot,
            InfixOperator::NonCommutativeMultiply => sym::NonCommutativeMultiply,
            InfixOperator::And => sym::And,
            InfixOperator::Or => sym::Or,
            InfixOperator::Alternatives => sym::Alternatives,
            InfixOperator::StringJoin => sym::StringJoin,
            InfixOperator::StringExpression => sym::StringExpression,
            InfixOperator::Composition => sym::Composition,
            InfixOperator::RightComposition => sym::RightComposition,
            InfixOperator::Element => sym::Element,
            InfixOperator::Subset => sym::Subset,
            InfixOperator::Superset => sym::Superset,
            InfixOperator::SubsetEqual => sym::SubsetEqual,
            InfixOperator::SupersetEqual => sym::SupersetEqual,
            InfixOperator::NotElement => sym::NotElement,
            InfixOperator::NotSubset => sym::NotSubset,
            InfixOperator::NotSuperset => sym::NotSuperset,
            InfixOperator::NotSubsetEqual => sym::NotSubsetEqual,
            InfixOperator::NotSupersetEqual => sym::NotSupersetEqual,
            InfixOperator::SquareSubset => sym::SquareSubset,
            InfixOperator::SquareSuperset => sym::SquareSuperset,
            InfixOperator::NotSquareSubset => sym::NotSquareSubset,
            InfixOperator::NotSquareSuperset => sym::NotSquareSuperset,
            InfixOperator::SquareSubsetEqual => sym::SquareSubsetEqual,
            InfixOperator::SquareSupersetEqual => sym::SquareSupersetEqual,
            InfixOperator::NotSquareSubsetEqual => sym::NotSquareSubsetEqual,
            InfixOperator::NotSquareSupersetEqual => sym::NotSquareSupersetEqual,
            InfixOperator::ReverseElement => sym::ReverseElement,
            InfixOperator::NotReverseElement => sym::NotReverseElement,
            InfixOperator::Distributed => sym::Distributed,
            InfixOperator::Xor => sym::Xor,
            InfixOperator::Nand => sym::Nand,
            InfixOperator::Nor => sym::Nor,
            InfixOperator::LeftArrow => sym::LeftArrow,
            InfixOperator::RightArrow => sym::RightArrow,
            InfixOperator::LeftRightArrow => sym::LeftRightArrow,
            InfixOperator::LeftTeeArrow => sym::LeftTeeArrow,
            InfixOperator::RightTeeArrow => sym::RightTeeArrow,
            InfixOperator::RightArrowLeftArrow => sym::RightArrowLeftArrow,
            InfixOperator::LeftArrowRightArrow => sym::LeftArrowRightArrow,
            InfixOperator::DoubleLeftArrow => sym::DoubleLeftArrow,
            InfixOperator::DoubleRightArrow => sym::DoubleRightArrow,
            InfixOperator::DoubleLeftRightArrow => sym::DoubleLeftRightArrow,
            InfixOperator::LeftArrowBar => sym::LeftArrowBar,
            InfixOperator::RightArrowBar => sym::RightArrowBar,
            InfixOperator::ShortRightArrow => sym::ShortRightArrow,
            InfixOperator::ShortLeftArrow => sym::ShortLeftArrow,
            InfixOperator::UpperLeftArrow => sym::UpperLeftArrow,
            InfixOperator::UpperRightArrow => sym::UpperRightArrow,
            InfixOperator::LowerRightArrow => sym::LowerRightArrow,
            InfixOperator::LowerLeftArrow => sym::LowerLeftArrow,
            InfixOperator::LeftVector => sym::LeftVector,
            InfixOperator::RightVector => sym::RightVector,
            InfixOperator::LeftRightVector => sym::LeftRightVector,
            InfixOperator::LeftVectorBar => sym::LeftVectorBar,
            InfixOperator::RightVectorBar => sym::RightVectorBar,
            InfixOperator::LeftTeeVector => sym::LeftTeeVector,
            InfixOperator::RightTeeVector => sym::RightTeeVector,
            InfixOperator::DownLeftVector => sym::DownLeftVector,
            InfixOperator::DownRightVector => sym::DownRightVector,
            InfixOperator::DownLeftRightVector => sym::DownLeftRightVector,
            InfixOperator::DownLeftVectorBar => sym::DownLeftVectorBar,
            InfixOperator::DownRightVectorBar => sym::DownRightVectorBar,
            InfixOperator::DownLeftTeeVector => sym::DownLeftTeeVector,
            InfixOperator::DownRightTeeVector => sym::DownRightTeeVector,
            InfixOperator::UpArrow => sym::UpArrow,
            InfixOperator::DownArrow => sym::DownArrow,
            InfixOperator::UpDownArrow => sym::UpDownArrow,
            InfixOperator::UpTeeArrow => sym::UpTeeArrow,
            InfixOperator::DownTeeArrow => sym::DownTeeArrow,
            InfixOperator::UpArrowDownArrow => sym::UpArrowDownArrow,
            InfixOperator::DoubleUpArrow => sym::DoubleUpArrow,
            InfixOperator::DoubleDownArrow => sym::DoubleDownArrow,
            InfixOperator::DoubleUpDownArrow => sym::DoubleUpDownArrow,
            InfixOperator::DownArrowUpArrow => sym::DownArrowUpArrow,
            InfixOperator::LongLeftArrow => sym::LongLeftArrow,
            InfixOperator::LongRightArrow => sym::LongRightArrow,
            InfixOperator::LongLeftRightArrow => sym::LongLeftRightArrow,
            InfixOperator::DoubleLongLeftArrow => sym::DoubleLongLeftArrow,
            InfixOperator::DoubleLongRightArrow => sym::DoubleLongRightArrow,
            InfixOperator::DoubleLongLeftRightArrow => sym::DoubleLongLeftRightArrow,
            InfixOperator::UpArrowBar => sym::UpArrowBar,
            InfixOperator::DownArrowBar => sym::DownArrowBar,
            InfixOperator::ShortUpArrow => sym::ShortUpArrow,
            InfixOperator::ShortDownArrow => sym::ShortDownArrow,
            InfixOperator::RightUpVector => sym::RightUpVector,
            InfixOperator::LeftUpVector => sym::LeftUpVector,
            InfixOperator::RightDownVector => sym::RightDownVector,
            InfixOperator::LeftDownVector => sym::LeftDownVector,
            InfixOperator::RightUpDownVector => sym::RightUpDownVector,
            InfixOperator::LeftUpDownVector => sym::LeftUpDownVector,
            InfixOperator::RightUpVectorBar => sym::RightUpVectorBar,
            InfixOperator::RightDownVectorBar => sym::RightDownVectorBar,
            InfixOperator::LeftUpVectorBar => sym::LeftUpVectorBar,
            InfixOperator::LeftDownVectorBar => sym::LeftDownVectorBar,
            InfixOperator::RightUpTeeVector => sym::RightUpTeeVector,
            InfixOperator::RightDownTeeVector => sym::RightDownTeeVector,
            InfixOperator::LeftUpTeeVector => sym::LeftUpTeeVector,
            InfixOperator::LeftDownTeeVector => sym::LeftDownTeeVector,
            InfixOperator::UpEquilibrium => sym::UpEquilibrium,
            InfixOperator::ReverseUpEquilibrium => sym::ReverseUpEquilibrium,
            InfixOperator::CenterDot => sym::CenterDot,
            InfixOperator::Equivalent => sym::Equivalent,
            InfixOperator::CircleDot => sym::CircleDot,
            InfixOperator::Conditioned => sym::Conditioned,
            InfixOperator::Union => sym::Union,
            InfixOperator::SquareUnion => sym::SquareUnion,
            InfixOperator::UnionPlus => sym::UnionPlus,
            InfixOperator::Intersection => sym::Intersection,
            InfixOperator::SquareIntersection => sym::SquareIntersection,
            InfixOperator::TensorWedge => sym::TensorWedge,
            InfixOperator::TensorProduct => sym::TensorProduct,
            InfixOperator::Cross => sym::Cross,
            InfixOperator::SmallCircle => sym::SmallCircle,
            InfixOperator::Divisible => sym::Divisible,
            InfixOperator::VerticalSeparator => sym::VerticalSeparator,
            InfixOperator::Backslash => sym::Backslash,
            InfixOperator::Diamond => sym::Diamond,
            InfixOperator::Wedge => sym::Wedge,
            InfixOperator::Vee => sym::Vee,
            InfixOperator::CircleTimes => sym::CircleTimes,
            InfixOperator::Star => sym::Star,
            InfixOperator::VerticalTilde => sym::VerticalTilde,
            InfixOperator::Coproduct => sym::Coproduct,
            InfixOperator::Cap => sym::Cap,
            InfixOperator::Cup => sym::Cup,
            InfixOperator::CirclePlus => sym::CirclePlus,
            InfixOperator::VerticalBar => sym::VerticalBar,
            InfixOperator::DoubleVerticalBar => sym::DoubleVerticalBar,
            InfixOperator::NotVerticalBar => sym::NotVerticalBar,
            InfixOperator::NotDoubleVerticalBar => sym::NotDoubleVerticalBar,
            InfixOperator::LeftTriangle => sym::LeftTriangle,
            InfixOperator::RightTriangle => sym::RightTriangle,
            InfixOperator::NotLeftTriangle => sym::NotLeftTriangle,
            InfixOperator::NotRightTriangle => sym::NotRightTriangle,
            InfixOperator::LeftTriangleEqual => sym::LeftTriangleEqual,
            InfixOperator::RightTriangleEqual => sym::RightTriangleEqual,
            InfixOperator::NotLeftTriangleEqual => sym::NotLeftTriangleEqual,
            InfixOperator::NotRightTriangleEqual => sym::NotRightTriangleEqual,
            InfixOperator::LeftTriangleBar => sym::LeftTriangleBar,
            InfixOperator::RightTriangleBar => sym::RightTriangleBar,
            InfixOperator::NotLeftTriangleBar => sym::NotLeftTriangleBar,
            InfixOperator::NotRightTriangleBar => sym::NotRightTriangleBar,
            InfixOperator::TildeEqual => sym::TildeEqual,
            InfixOperator::NotTildeEqual => sym::NotTildeEqual,
            InfixOperator::TildeFullEqual => sym::TildeFullEqual,
            InfixOperator::NotTildeFullEqual => sym::NotTildeFullEqual,
            InfixOperator::Tilde => sym::Tilde,
            InfixOperator::NotTilde => sym::NotTilde,
            InfixOperator::EqualTilde => sym::EqualTilde,
            InfixOperator::NotEqualTilde => sym::NotEqualTilde,
            InfixOperator::TildeTilde => sym::TildeTilde,
            InfixOperator::NotTildeTilde => sym::NotTildeTilde,
            InfixOperator::Proportional => sym::Proportional,
            InfixOperator::Proportion => sym::Proportion,
            InfixOperator::Congruent => sym::Congruent,
            InfixOperator::NotCongruent => sym::NotCongruent,
            InfixOperator::Equilibrium => sym::Equilibrium,
            InfixOperator::ReverseEquilibrium => sym::ReverseEquilibrium,
            InfixOperator::DotEqual => sym::DotEqual,
            InfixOperator::Precedes => sym::Precedes,
            InfixOperator::Succeeds => sym::Succeeds,
            InfixOperator::PrecedesEqual => sym::PrecedesEqual,
            InfixOperator::SucceedsEqual => sym::SucceedsEqual,
            InfixOperator::PrecedesTilde => sym::PrecedesTilde,
            InfixOperator::SucceedsTilde => sym::SucceedsTilde,
            InfixOperator::PrecedesSlantEqual => sym::PrecedesSlantEqual,
            InfixOperator::SucceedsSlantEqual => sym::SucceedsSlantEqual,
            InfixOperator::NotPrecedes => sym::NotPrecedes,
            InfixOperator::NotSucceeds => sym::NotSucceeds,
            InfixOperator::NotPrecedesEqual => sym::NotPrecedesEqual,
            InfixOperator::NotSucceedsEqual => sym::NotSucceedsEqual,
            InfixOperator::NotPrecedesTilde => sym::NotPrecedesTilde,
            InfixOperator::NotSucceedsTilde => sym::NotSucceedsTilde,
            InfixOperator::NotPrecedesSlantEqual => sym::NotPrecedesSlantEqual,
            InfixOperator::NotSucceedsSlantEqual => sym::NotSucceedsSlantEqual,
            InfixOperator::CupCap => sym::CupCap,
            InfixOperator::NotCupCap => sym::NotCupCap,
            InfixOperator::HumpEqual => sym::HumpEqual,
            InfixOperator::HumpDownHump => sym::HumpDownHump,
            InfixOperator::NotHumpEqual => sym::NotHumpEqual,
            InfixOperator::NotHumpDownHump => sym::NotHumpDownHump,
            InfixOperator::CodeParser_InfixInequality => sym::CodeParser_InfixInequality,
            InfixOperator::PermutationProduct => sym::PermutationProduct,
            InfixOperator::Colon => sym::Colon,
            InfixOperator::Xnor => sym::Xnor,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Times => InfixOperator::Times,
            sym::Span => InfixOperator::Span,
            sym::CompoundExpression => InfixOperator::CompoundExpression,
            sym::MessageName => InfixOperator::MessageName,
            sym::CodeParser_InternalInvalid => InfixOperator::CodeParser_InternalInvalid,
            sym::CodeParser_Comma => InfixOperator::CodeParser_Comma,
            sym::CodeParser_InfixTilde => InfixOperator::CodeParser_InfixTilde,
            sym::Plus => InfixOperator::Plus,
            sym::SameQ => InfixOperator::SameQ,
            sym::UnsameQ => InfixOperator::UnsameQ,
            sym::Dot => InfixOperator::Dot,
            sym::NonCommutativeMultiply => InfixOperator::NonCommutativeMultiply,
            sym::And => InfixOperator::And,
            sym::Or => InfixOperator::Or,
            sym::Alternatives => InfixOperator::Alternatives,
            sym::StringJoin => InfixOperator::StringJoin,
            sym::StringExpression => InfixOperator::StringExpression,
            sym::Composition => InfixOperator::Composition,
            sym::RightComposition => InfixOperator::RightComposition,
            sym::Element => InfixOperator::Element,
            sym::Subset => InfixOperator::Subset,
            sym::Superset => InfixOperator::Superset,
            sym::SubsetEqual => InfixOperator::SubsetEqual,
            sym::SupersetEqual => InfixOperator::SupersetEqual,
            sym::NotElement => InfixOperator::NotElement,
            sym::NotSubset => InfixOperator::NotSubset,
            sym::NotSuperset => InfixOperator::NotSuperset,
            sym::NotSubsetEqual => InfixOperator::NotSubsetEqual,
            sym::NotSupersetEqual => InfixOperator::NotSupersetEqual,
            sym::SquareSubset => InfixOperator::SquareSubset,
            sym::SquareSuperset => InfixOperator::SquareSuperset,
            sym::NotSquareSubset => InfixOperator::NotSquareSubset,
            sym::NotSquareSuperset => InfixOperator::NotSquareSuperset,
            sym::SquareSubsetEqual => InfixOperator::SquareSubsetEqual,
            sym::SquareSupersetEqual => InfixOperator::SquareSupersetEqual,
            sym::NotSquareSubsetEqual => InfixOperator::NotSquareSubsetEqual,
            sym::NotSquareSupersetEqual => InfixOperator::NotSquareSupersetEqual,
            sym::ReverseElement => InfixOperator::ReverseElement,
            sym::NotReverseElement => InfixOperator::NotReverseElement,
            sym::Distributed => InfixOperator::Distributed,
            sym::Xor => InfixOperator::Xor,
            sym::Nand => InfixOperator::Nand,
            sym::Nor => InfixOperator::Nor,
            sym::LeftArrow => InfixOperator::LeftArrow,
            sym::RightArrow => InfixOperator::RightArrow,
            sym::LeftRightArrow => InfixOperator::LeftRightArrow,
            sym::LeftTeeArrow => InfixOperator::LeftTeeArrow,
            sym::RightTeeArrow => InfixOperator::RightTeeArrow,
            sym::RightArrowLeftArrow => InfixOperator::RightArrowLeftArrow,
            sym::LeftArrowRightArrow => InfixOperator::LeftArrowRightArrow,
            sym::DoubleLeftArrow => InfixOperator::DoubleLeftArrow,
            sym::DoubleRightArrow => InfixOperator::DoubleRightArrow,
            sym::DoubleLeftRightArrow => InfixOperator::DoubleLeftRightArrow,
            sym::LeftArrowBar => InfixOperator::LeftArrowBar,
            sym::RightArrowBar => InfixOperator::RightArrowBar,
            sym::ShortRightArrow => InfixOperator::ShortRightArrow,
            sym::ShortLeftArrow => InfixOperator::ShortLeftArrow,
            sym::UpperLeftArrow => InfixOperator::UpperLeftArrow,
            sym::UpperRightArrow => InfixOperator::UpperRightArrow,
            sym::LowerRightArrow => InfixOperator::LowerRightArrow,
            sym::LowerLeftArrow => InfixOperator::LowerLeftArrow,
            sym::LeftVector => InfixOperator::LeftVector,
            sym::RightVector => InfixOperator::RightVector,
            sym::LeftRightVector => InfixOperator::LeftRightVector,
            sym::LeftVectorBar => InfixOperator::LeftVectorBar,
            sym::RightVectorBar => InfixOperator::RightVectorBar,
            sym::LeftTeeVector => InfixOperator::LeftTeeVector,
            sym::RightTeeVector => InfixOperator::RightTeeVector,
            sym::DownLeftVector => InfixOperator::DownLeftVector,
            sym::DownRightVector => InfixOperator::DownRightVector,
            sym::DownLeftRightVector => InfixOperator::DownLeftRightVector,
            sym::DownLeftVectorBar => InfixOperator::DownLeftVectorBar,
            sym::DownRightVectorBar => InfixOperator::DownRightVectorBar,
            sym::DownLeftTeeVector => InfixOperator::DownLeftTeeVector,
            sym::DownRightTeeVector => InfixOperator::DownRightTeeVector,
            sym::UpArrow => InfixOperator::UpArrow,
            sym::DownArrow => InfixOperator::DownArrow,
            sym::UpDownArrow => InfixOperator::UpDownArrow,
            sym::UpTeeArrow => InfixOperator::UpTeeArrow,
            sym::DownTeeArrow => InfixOperator::DownTeeArrow,
            sym::UpArrowDownArrow => InfixOperator::UpArrowDownArrow,
            sym::DoubleUpArrow => InfixOperator::DoubleUpArrow,
            sym::DoubleDownArrow => InfixOperator::DoubleDownArrow,
            sym::DoubleUpDownArrow => InfixOperator::DoubleUpDownArrow,
            sym::DownArrowUpArrow => InfixOperator::DownArrowUpArrow,
            sym::LongLeftArrow => InfixOperator::LongLeftArrow,
            sym::LongRightArrow => InfixOperator::LongRightArrow,
            sym::LongLeftRightArrow => InfixOperator::LongLeftRightArrow,
            sym::DoubleLongLeftArrow => InfixOperator::DoubleLongLeftArrow,
            sym::DoubleLongRightArrow => InfixOperator::DoubleLongRightArrow,
            sym::DoubleLongLeftRightArrow => InfixOperator::DoubleLongLeftRightArrow,
            sym::UpArrowBar => InfixOperator::UpArrowBar,
            sym::DownArrowBar => InfixOperator::DownArrowBar,
            sym::ShortUpArrow => InfixOperator::ShortUpArrow,
            sym::ShortDownArrow => InfixOperator::ShortDownArrow,
            sym::RightUpVector => InfixOperator::RightUpVector,
            sym::LeftUpVector => InfixOperator::LeftUpVector,
            sym::RightDownVector => InfixOperator::RightDownVector,
            sym::LeftDownVector => InfixOperator::LeftDownVector,
            sym::RightUpDownVector => InfixOperator::RightUpDownVector,
            sym::LeftUpDownVector => InfixOperator::LeftUpDownVector,
            sym::RightUpVectorBar => InfixOperator::RightUpVectorBar,
            sym::RightDownVectorBar => InfixOperator::RightDownVectorBar,
            sym::LeftUpVectorBar => InfixOperator::LeftUpVectorBar,
            sym::LeftDownVectorBar => InfixOperator::LeftDownVectorBar,
            sym::RightUpTeeVector => InfixOperator::RightUpTeeVector,
            sym::RightDownTeeVector => InfixOperator::RightDownTeeVector,
            sym::LeftUpTeeVector => InfixOperator::LeftUpTeeVector,
            sym::LeftDownTeeVector => InfixOperator::LeftDownTeeVector,
            sym::UpEquilibrium => InfixOperator::UpEquilibrium,
            sym::ReverseUpEquilibrium => InfixOperator::ReverseUpEquilibrium,
            sym::CenterDot => InfixOperator::CenterDot,
            sym::Equivalent => InfixOperator::Equivalent,
            sym::CircleDot => InfixOperator::CircleDot,
            sym::Conditioned => InfixOperator::Conditioned,
            sym::Union => InfixOperator::Union,
            sym::SquareUnion => InfixOperator::SquareUnion,
            sym::UnionPlus => InfixOperator::UnionPlus,
            sym::Intersection => InfixOperator::Intersection,
            sym::SquareIntersection => InfixOperator::SquareIntersection,
            sym::TensorWedge => InfixOperator::TensorWedge,
            sym::TensorProduct => InfixOperator::TensorProduct,
            sym::Cross => InfixOperator::Cross,
            sym::SmallCircle => InfixOperator::SmallCircle,
            sym::Divisible => InfixOperator::Divisible,
            sym::VerticalSeparator => InfixOperator::VerticalSeparator,
            sym::Backslash => InfixOperator::Backslash,
            sym::Diamond => InfixOperator::Diamond,
            sym::Wedge => InfixOperator::Wedge,
            sym::Vee => InfixOperator::Vee,
            sym::CircleTimes => InfixOperator::CircleTimes,
            sym::Star => InfixOperator::Star,
            sym::VerticalTilde => InfixOperator::VerticalTilde,
            sym::Coproduct => InfixOperator::Coproduct,
            sym::Cap => InfixOperator::Cap,
            sym::Cup => InfixOperator::Cup,
            sym::CirclePlus => InfixOperator::CirclePlus,
            sym::VerticalBar => InfixOperator::VerticalBar,
            sym::DoubleVerticalBar => InfixOperator::DoubleVerticalBar,
            sym::NotVerticalBar => InfixOperator::NotVerticalBar,
            sym::NotDoubleVerticalBar => InfixOperator::NotDoubleVerticalBar,
            sym::LeftTriangle => InfixOperator::LeftTriangle,
            sym::RightTriangle => InfixOperator::RightTriangle,
            sym::NotLeftTriangle => InfixOperator::NotLeftTriangle,
            sym::NotRightTriangle => InfixOperator::NotRightTriangle,
            sym::LeftTriangleEqual => InfixOperator::LeftTriangleEqual,
            sym::RightTriangleEqual => InfixOperator::RightTriangleEqual,
            sym::NotLeftTriangleEqual => InfixOperator::NotLeftTriangleEqual,
            sym::NotRightTriangleEqual => InfixOperator::NotRightTriangleEqual,
            sym::LeftTriangleBar => InfixOperator::LeftTriangleBar,
            sym::RightTriangleBar => InfixOperator::RightTriangleBar,
            sym::NotLeftTriangleBar => InfixOperator::NotLeftTriangleBar,
            sym::NotRightTriangleBar => InfixOperator::NotRightTriangleBar,
            sym::TildeEqual => InfixOperator::TildeEqual,
            sym::NotTildeEqual => InfixOperator::NotTildeEqual,
            sym::TildeFullEqual => InfixOperator::TildeFullEqual,
            sym::NotTildeFullEqual => InfixOperator::NotTildeFullEqual,
            sym::Tilde => InfixOperator::Tilde,
            sym::NotTilde => InfixOperator::NotTilde,
            sym::EqualTilde => InfixOperator::EqualTilde,
            sym::NotEqualTilde => InfixOperator::NotEqualTilde,
            sym::TildeTilde => InfixOperator::TildeTilde,
            sym::NotTildeTilde => InfixOperator::NotTildeTilde,
            sym::Proportional => InfixOperator::Proportional,
            sym::Proportion => InfixOperator::Proportion,
            sym::Congruent => InfixOperator::Congruent,
            sym::NotCongruent => InfixOperator::NotCongruent,
            sym::Equilibrium => InfixOperator::Equilibrium,
            sym::ReverseEquilibrium => InfixOperator::ReverseEquilibrium,
            sym::DotEqual => InfixOperator::DotEqual,
            sym::Precedes => InfixOperator::Precedes,
            sym::Succeeds => InfixOperator::Succeeds,
            sym::PrecedesEqual => InfixOperator::PrecedesEqual,
            sym::SucceedsEqual => InfixOperator::SucceedsEqual,
            sym::PrecedesTilde => InfixOperator::PrecedesTilde,
            sym::SucceedsTilde => InfixOperator::SucceedsTilde,
            sym::PrecedesSlantEqual => InfixOperator::PrecedesSlantEqual,
            sym::SucceedsSlantEqual => InfixOperator::SucceedsSlantEqual,
            sym::NotPrecedes => InfixOperator::NotPrecedes,
            sym::NotSucceeds => InfixOperator::NotSucceeds,
            sym::NotPrecedesEqual => InfixOperator::NotPrecedesEqual,
            sym::NotSucceedsEqual => InfixOperator::NotSucceedsEqual,
            sym::NotPrecedesTilde => InfixOperator::NotPrecedesTilde,
            sym::NotSucceedsTilde => InfixOperator::NotSucceedsTilde,
            sym::NotPrecedesSlantEqual => InfixOperator::NotPrecedesSlantEqual,
            sym::NotSucceedsSlantEqual => InfixOperator::NotSucceedsSlantEqual,
            sym::CupCap => InfixOperator::CupCap,
            sym::NotCupCap => InfixOperator::NotCupCap,
            sym::HumpEqual => InfixOperator::HumpEqual,
            sym::HumpDownHump => InfixOperator::HumpDownHump,
            sym::NotHumpEqual => InfixOperator::NotHumpEqual,
            sym::NotHumpDownHump => InfixOperator::NotHumpDownHump,
            sym::CodeParser_InfixInequality => InfixOperator::CodeParser_InfixInequality,
            sym::PermutationProduct => InfixOperator::PermutationProduct,
            sym::Colon => InfixOperator::Colon,
            sym::Xnor => InfixOperator::Xnor,
            _ => return None,
        };

        Some(operator)
    }
}

impl Operator for PrefixOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            PrefixOperator::Get => sym::Get,
            PrefixOperator::Minus => sym::Minus,
            PrefixOperator::Plus => sym::Plus,
            PrefixOperator::Not => sym::Not,
            PrefixOperator::PreIncrement => sym::PreIncrement,
            PrefixOperator::PreDecrement => sym::PreDecrement,
            PrefixOperator::CodeParser_PrefixNot2 => sym::CodeParser_PrefixNot2,
            PrefixOperator::PlusMinus => sym::PlusMinus,
            PrefixOperator::Sum => sym::Sum,
            PrefixOperator::Sqrt => sym::Sqrt,
            PrefixOperator::MinusPlus => sym::MinusPlus,
            PrefixOperator::DifferentialD => sym::DifferentialD,
            PrefixOperator::CapitalDifferentialD => sym::CapitalDifferentialD,
            PrefixOperator::Del => sym::Del,
            PrefixOperator::Square => sym::Square,
            PrefixOperator::Product => sym::Product,
            PrefixOperator::ContinuedFractionK => sym::ContinuedFractionK,
            PrefixOperator::CircleTimes => sym::CircleTimes,
            PrefixOperator::ForAll => sym::ForAll,
            PrefixOperator::Exists => sym::Exists,
            PrefixOperator::NotExists => sym::NotExists,
            PrefixOperator::Coproduct => sym::Coproduct,
            PrefixOperator::Piecewise => sym::Piecewise,
            PrefixOperator::InvisiblePrefixScriptBase => sym::InvisiblePrefixScriptBase,
            PrefixOperator::ExpectationE => sym::ExpectationE,
            PrefixOperator::CubeRoot => sym::CubeRoot,
            PrefixOperator::ProbabilityPr => sym::ProbabilityPr,
            PrefixOperator::CodeParser_PrefixLinearSyntaxBang => sym::CodeParser_PrefixLinearSyntaxBang,
            PrefixOperator::Integral => sym::Integral,
            PrefixOperator::ContourIntegral => sym::ContourIntegral,
            PrefixOperator::DoubleContourIntegral => sym::DoubleContourIntegral,
            PrefixOperator::ClockwiseContourIntegral => sym::ClockwiseContourIntegral,
            PrefixOperator::CounterClockwiseContourIntegral => sym::CounterClockwiseContourIntegral,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Get => PrefixOperator::Get,
            sym::Minus => PrefixOperator::Minus,
            sym::Plus => PrefixOperator::Plus,
            sym::Not => PrefixOperator::Not,
            sym::PreIncrement => PrefixOperator::PreIncrement,
            sym::PreDecrement => PrefixOperator::PreDecrement,
            sym::CodeParser_PrefixNot2 => PrefixOperator::CodeParser_PrefixNot2,
            sym::PlusMinus => PrefixOperator::PlusMinus,
            sym::Sum => PrefixOperator::Sum,
            sym::Sqrt => PrefixOperator::Sqrt,
            sym::MinusPlus => PrefixOperator::MinusPlus,
            sym::DifferentialD => PrefixOperator::DifferentialD,
            sym::CapitalDifferentialD => PrefixOperator::CapitalDifferentialD,
            sym::Del => PrefixOperator::Del,
            sym::Square => PrefixOperator::Square,
            sym::Product => PrefixOperator::Product,
            sym::ContinuedFractionK => PrefixOperator::ContinuedFractionK,
            sym::CircleTimes => PrefixOperator::CircleTimes,
            sym::ForAll => PrefixOperator::ForAll,
            sym::Exists => PrefixOperator::Exists,
            sym::NotExists => PrefixOperator::NotExists,
            sym::Coproduct => PrefixOperator::Coproduct,
            sym::Piecewise => PrefixOperator::Piecewise,
            sym::InvisiblePrefixScriptBase => PrefixOperator::InvisiblePrefixScriptBase,
            sym::ExpectationE => PrefixOperator::ExpectationE,
            sym::CubeRoot => PrefixOperator::CubeRoot,
            sym::ProbabilityPr => PrefixOperator::ProbabilityPr,
            sym::CodeParser_PrefixLinearSyntaxBang => PrefixOperator::CodeParser_PrefixLinearSyntaxBang,
            sym::Integral => PrefixOperator::Integral,
            sym::ContourIntegral => PrefixOperator::ContourIntegral,
            sym::DoubleContourIntegral => PrefixOperator::DoubleContourIntegral,
            sym::ClockwiseContourIntegral => PrefixOperator::ClockwiseContourIntegral,
            sym::CounterClockwiseContourIntegral => PrefixOperator::CounterClockwiseContourIntegral,
            _ => return None,
        };

        Some(operator)
    }
}

impl Operator for PostfixOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            PostfixOperator::Function => sym::Function,
            PostfixOperator::Repeated => sym::Repeated,
            PostfixOperator::Factorial => sym::Factorial,
            PostfixOperator::Decrement => sym::Decrement,
            PostfixOperator::Increment => sym::Increment,
            PostfixOperator::RepeatedNull => sym::RepeatedNull,
            PostfixOperator::Factorial2 => sym::Factorial2,
            PostfixOperator::Derivative => sym::Derivative,
            PostfixOperator::Transpose => sym::Transpose,
            PostfixOperator::Conjugate => sym::Conjugate,
            PostfixOperator::ConjugateTranspose => sym::ConjugateTranspose,
            PostfixOperator::HermitianConjugate => sym::HermitianConjugate,
            PostfixOperator::InvisiblePostfixScriptBase => sym::InvisiblePostfixScriptBase,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Function => PostfixOperator::Function,
            sym::Repeated => PostfixOperator::Repeated,
            sym::Factorial => PostfixOperator::Factorial,
            sym::Decrement => PostfixOperator::Decrement,
            sym::Increment => PostfixOperator::Increment,
            sym::RepeatedNull => PostfixOperator::RepeatedNull,
            sym::Factorial2 => PostfixOperator::Factorial2,
            sym::Derivative => PostfixOperator::Derivative,
            sym::Transpose => PostfixOperator::Transpose,
            sym::Conjugate => PostfixOperator::Conjugate,
            sym::ConjugateTranspose => PostfixOperator::ConjugateTranspose,
            sym::HermitianConjugate => PostfixOperator::HermitianConjugate,
            sym::InvisiblePostfixScriptBase => PostfixOperator::InvisiblePostfixScriptBase,
            _ => return None,
        };

        Some(operator)
    }
}

impl Operator for BinaryOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            BinaryOperator::Pattern => sym::Pattern,
            BinaryOperator::Optional => sym::Optional,
            BinaryOperator::Set => sym::Set,
            BinaryOperator::SetDelayed => sym::SetDelayed,
            BinaryOperator::Unset => sym::Unset,
            BinaryOperator::Put => sym::Put,
            BinaryOperator::PutAppend => sym::PutAppend,
            BinaryOperator::Span => sym::Span,
            BinaryOperator::Divide => sym::Divide,
            BinaryOperator::Power => sym::Power,
            BinaryOperator::UpSet => sym::UpSet,
            BinaryOperator::UpSetDelayed => sym::UpSetDelayed,
            BinaryOperator::Map => sym::Map,
            BinaryOperator::Rule => sym::Rule,
            BinaryOperator::Apply => sym::Apply,
            BinaryOperator::Condition => sym::Condition,
            BinaryOperator::ReplaceAll => sym::ReplaceAll,
            BinaryOperator::RuleDelayed => sym::RuleDelayed,
            BinaryOperator::ReplaceRepeated => sym::ReplaceRepeated,
            BinaryOperator::AddTo => sym::AddTo,
            BinaryOperator::TimesBy => sym::TimesBy,
            BinaryOperator::SubtractFrom => sym::SubtractFrom,
            BinaryOperator::DivideBy => sym::DivideBy,
            BinaryOperator::TwoWayRule => sym::TwoWayRule,
            BinaryOperator::MapAll => sym::MapAll,
            BinaryOperator::CodeParser_BinaryAt => sym::CodeParser_BinaryAt,
            BinaryOperator::MapApply => sym::MapApply,
            BinaryOperator::CodeParser_BinarySlashSlash => sym::CodeParser_BinarySlashSlash,
            BinaryOperator::PatternTest => sym::PatternTest,
            BinaryOperator::Function => sym::Function,
            BinaryOperator::ApplyTo => sym::ApplyTo,
            BinaryOperator::Implies => sym::Implies,
            BinaryOperator::RoundImplies => sym::RoundImplies,
            BinaryOperator::PlusMinus => sym::PlusMinus,
            BinaryOperator::DirectedEdge => sym::DirectedEdge,
            BinaryOperator::UndirectedEdge => sym::UndirectedEdge,
            BinaryOperator::MinusPlus => sym::MinusPlus,
            BinaryOperator::CircleMinus => sym::CircleMinus,
            BinaryOperator::SuchThat => sym::SuchThat,
            BinaryOperator::Perpendicular => sym::Perpendicular,
            BinaryOperator::Because => sym::Because,
            BinaryOperator::Therefore => sym::Therefore,
            BinaryOperator::RightTee => sym::RightTee,
            BinaryOperator::LeftTee => sym::LeftTee,
            BinaryOperator::DoubleRightTee => sym::DoubleRightTee,
            BinaryOperator::DoubleLeftTee => sym::DoubleLeftTee,
            BinaryOperator::UpTee => sym::UpTee,
            BinaryOperator::DownTee => sym::DownTee,
            BinaryOperator::Application => sym::Application,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::Pattern => BinaryOperator::Pattern,
            sym::Optional => BinaryOperator::Optional,
            sym::Set => BinaryOperator::Set,
            sym::SetDelayed => BinaryOperator::SetDelayed,
            sym::Unset => BinaryOperator::Unset,
            sym::Put => BinaryOperator::Put,
            sym::PutAppend => BinaryOperator::PutAppend,
            sym::Span => BinaryOperator::Span,
            sym::Divide => BinaryOperator::Divide,
            sym::Power => BinaryOperator::Power,
            sym::UpSet => BinaryOperator::UpSet,
            sym::UpSetDelayed => BinaryOperator::UpSetDelayed,
            sym::Map => BinaryOperator::Map,
            sym::Rule => BinaryOperator::Rule,
            sym::Apply => BinaryOperator::Apply,
            sym::Condition => BinaryOperator::Condition,
            sym::ReplaceAll => BinaryOperator::ReplaceAll,
            sym::RuleDelayed => BinaryOperator::RuleDelayed,
            sym::ReplaceRepeated => BinaryOperator::ReplaceRepeated,
            sym::AddTo => BinaryOperator::AddTo,
            sym::TimesBy => BinaryOperator::TimesBy,
            sym::SubtractFrom => BinaryOperator::SubtractFrom,
            sym::DivideBy => BinaryOperator::DivideBy,
            sym::TwoWayRule => BinaryOperator::TwoWayRule,
            sym::MapAll => BinaryOperator::MapAll,
            sym::CodeParser_BinaryAt => BinaryOperator::CodeParser_BinaryAt,
            sym::MapApply => BinaryOperator::MapApply,
            sym::CodeParser_BinarySlashSlash => BinaryOperator::CodeParser_BinarySlashSlash,
            sym::PatternTest => BinaryOperator::PatternTest,
            sym::Function => BinaryOperator::Function,
            sym::ApplyTo => BinaryOperator::ApplyTo,
            sym::Implies => BinaryOperator::Implies,
            sym::RoundImplies => BinaryOperator::RoundImplies,
            sym::PlusMinus => BinaryOperator::PlusMinus,
            sym::DirectedEdge => BinaryOperator::DirectedEdge,
            sym::UndirectedEdge => BinaryOperator::UndirectedEdge,
            sym::MinusPlus => BinaryOperator::MinusPlus,
            sym::CircleMinus => BinaryOperator::CircleMinus,
            sym::SuchThat => BinaryOperator::SuchThat,
            sym::Perpendicular => BinaryOperator::Perpendicular,
            sym::Because => BinaryOperator::Because,
            sym::Therefore => BinaryOperator::Therefore,
            sym::RightTee => BinaryOperator::RightTee,
            sym::LeftTee => BinaryOperator::LeftTee,
            sym::DoubleRightTee => BinaryOperator::DoubleRightTee,
            sym::DoubleLeftTee => BinaryOperator::DoubleLeftTee,
            sym::UpTee => BinaryOperator::UpTee,
            sym::DownTee => BinaryOperator::DownTee,
            sym::Application => BinaryOperator::Application,
            _ => return None,
        };

        Some(operator)
    }
}

impl Operator for TernaryOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            TernaryOperator::CodeParser_TernaryTilde => sym::CodeParser_TernaryTilde,
            TernaryOperator::CodeParser_TernaryOptionalPattern => sym::CodeParser_TernaryOptionalPattern,
            TernaryOperator::TagSet => sym::TagSet,
            TernaryOperator::TagSetDelayed => sym::TagSetDelayed,
            TernaryOperator::TagUnset => sym::TagUnset,
            TernaryOperator::Span => sym::Span,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
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

impl Operator for PrefixBinaryOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            PrefixBinaryOperator::Integrate => sym::Integrate,
            PrefixBinaryOperator::ContourIntegral => sym::ContourIntegral,
            PrefixBinaryOperator::DoubleContourIntegral => sym::DoubleContourIntegral,
            PrefixBinaryOperator::ClockwiseContourIntegral => sym::ClockwiseContourIntegral,
            PrefixBinaryOperator::CounterClockwiseContourIntegral => sym::CounterClockwiseContourIntegral,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
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

impl Operator for CompoundOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
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

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
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

impl Operator for GroupOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
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

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
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

impl Operator for CallOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            CallOperator::CodeParser_GroupSquare => sym::CodeParser_GroupSquare,
            CallOperator::CodeParser_GroupDoubleBracket => sym::CodeParser_GroupDoubleBracket,
            CallOperator::CodeParser_GroupTypeSpecifier => sym::CodeParser_GroupTypeSpecifier,
        }
    }

    fn try_from_symbol(symbol: SymbolRef) -> Option<Self> {
        let operator = match symbol {
            sym::CodeParser_GroupSquare => CallOperator::CodeParser_GroupSquare,
            sym::CodeParser_GroupDoubleBracket => CallOperator::CodeParser_GroupDoubleBracket,
            sym::CodeParser_GroupTypeSpecifier => CallOperator::CodeParser_GroupTypeSpecifier,
            _ => return None,
        };

        Some(operator)
    }
}

