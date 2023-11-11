//! Mapping [`TokenKind`] variants to parselet implementations

#![allow(non_upper_case_globals)]

use crate::parse::{operators::CompoundOperator, parselet::*};


pub(crate) const under1Parselet: UnderParselet = UnderParselet::new(
    CompoundOperator::Blank,
    CompoundOperator::CodeParser_PatternBlank,
);
pub(crate) const under2Parselet: UnderParselet = UnderParselet::new(
    CompoundOperator::BlankSequence,
    CompoundOperator::CodeParser_PatternBlankSequence,
);
pub(crate) const under3Parselet: UnderParselet = UnderParselet::new(
    CompoundOperator::BlankNullSequence,
    CompoundOperator::CodeParser_PatternBlankNullSequence,
);

macro_rules! token_kind_to_prefix_parselet {
    ($ty:ty; $kind:ident) => {{
        use crate::{
            tokenize::TokenKind as TK,
            parse::parselet::*,
            precedence::Precedence,
        };

    match $kind {
        TK::EndOfFile => &PrefixEndOfFileParselet {},

        TK::String
        | TK::Integer
        | TK::Real
        | TK::Rational
        | TK::LinearSyntaxBlob => &LeafParselet {},

        TK::Unknown
        | TK::Whitespace
        | TK::InternalNewline
        | TK::Comment => &PrefixErrorParselet {},


        TK::Error_ExpectedEqual
        | TK::Error_Number
        | TK::Error_UnhandledCharacter
        | TK::Error_ExpectedLetterlike
        | TK::Error_Aborted
        | TK::Error_ExpectedOperand
        | TK::Error_ExpectedTag
        | TK::Error_ExpectedFile
        | TK::Error_UnterminatedComment
        | TK::Error_UnterminatedString
        | TK::Error_UnterminatedFileString
        | TK::Error_UnterminatedLinearSyntaxBlob
        | TK::Error_UnsupportedToken
        | TK::Error_UnexpectedCloser
        | TK::Error_UnsafeCharacterEncoding
        | TK::Error_UnexpectedCommentCloser => &PrefixErrorParselet {},


        TK::BarGreater
        | TK::CloseCurly
        | TK::CloseParen
        | TK::CloseSquare
        | TK::LongName_CloseCurlyDoubleQuote
        | TK::LongName_CloseCurlyQuote
        | TK::LongName_RightAngleBracket
        | TK::LongName_RightAssociation
        | TK::LongName_RightBracketingBar
        | TK::LongName_RightCeiling
        | TK::LongName_RightDoubleBracket
        | TK::LongName_RightDoubleBracketingBar
        | TK::LongName_RightFloor => &PrefixCloserParselet {},


        TK::Minus      => &PrefixOperatorParselet::new(Precedence::PREFIX_MINUS, PrefixOperator::Minus),
        TK::Plus       => &PrefixOperatorParselet::new(Precedence::PREFIX_PLUS, PrefixOperator::Plus),
        TK::Bang       => &PrefixOperatorParselet::new(Precedence::PREFIX_BANG, PrefixOperator::Not),
        TK::PlusPlus   => &PrefixOperatorParselet::new(Precedence::PREFIX_PLUSPLUS, PrefixOperator::PreIncrement),
        TK::MinusMinus => &PrefixOperatorParselet::new(Precedence::PREFIX_MINUSMINUS, PrefixOperator::PreDecrement),

        TK::BangBang => &PrefixOperatorParselet::new(Precedence::FAKE_PREFIX_BANGBANG, PrefixOperator::CodeParser_PrefixNot2),

        TK::LongName_PlusMinus            => &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_PLUSMINUS, PrefixOperator::PlusMinus),
        TK::LongName_Sum                  => &PrefixOperatorParselet::new(Precedence::LONGNAME_SUM, PrefixOperator::Sum),
        TK::LongName_Not                  => &PrefixOperatorParselet::new(Precedence::LONGNAME_NOT, PrefixOperator::Not),
        TK::LongName_Sqrt                 => &PrefixOperatorParselet::new(Precedence::LONGNAME_SQRT, PrefixOperator::Sqrt),
        TK::LongName_MinusPlus            => &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_MINUSPLUS, PrefixOperator::MinusPlus),
        TK::LongName_DifferentialD        => &PrefixOperatorParselet::new(Precedence::LONGNAME_DIFFERENTIALD, PrefixOperator::DifferentialD),
        TK::LongName_CapitalDifferentialD => &PrefixOperatorParselet::new(Precedence::LONGNAME_CAPITALDIFFERENTIALD, PrefixOperator::CapitalDifferentialD),
        TK::LongName_Minus                => &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_MINUS, PrefixOperator::Minus),
        TK::LongName_Del                  => &PrefixOperatorParselet::new(Precedence::LONGNAME_DEL, PrefixOperator::Del),
        TK::LongName_Square               => &PrefixOperatorParselet::new(Precedence::LONGNAME_SQUARE, PrefixOperator::Square),


        TK::Comma
        | TK::LongName_InvisibleComma => &PrefixCommaParselet {},


        TK::LongName_Product                   => &PrefixOperatorParselet::new(Precedence::LONGNAME_PRODUCT, PrefixOperator::Product),
        TK::LongName_ContinuedFractionK        => &PrefixOperatorParselet::new(Precedence::LONGNAME_CONTINUEDFRACTIONK, PrefixOperator::ContinuedFractionK),
        TK::LongName_CircleTimes               => &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_CIRCLETIMES, PrefixOperator::CircleTimes),
        TK::LongName_ForAll                    => &PrefixOperatorParselet::new(Precedence::LONGNAME_FORALL, PrefixOperator::ForAll),
        TK::LongName_Exists                    => &PrefixOperatorParselet::new(Precedence::LONGNAME_EXISTS, PrefixOperator::Exists),
        TK::LongName_NotExists                 => &PrefixOperatorParselet::new(Precedence::LONGNAME_NOTEXISTS,PrefixOperator:: NotExists),
        TK::LongName_Coproduct                 => &PrefixOperatorParselet::new(Precedence::PREFIX_LONGNAME_COPRODUCT, PrefixOperator::Coproduct),
        TK::LongName_Piecewise                 => &PrefixOperatorParselet::new(Precedence::LONGNAME_PIECEWISE, PrefixOperator::Piecewise),
        TK::LongName_InvisiblePrefixScriptBase => &PrefixOperatorParselet::new(Precedence::LONGNAME_INVISIBLEPREFIXSCRIPTBASE, PrefixOperator::InvisiblePrefixScriptBase),
        TK::LongName_ExpectationE              => &PrefixOperatorParselet::new(Precedence::LONGNAME_EXPECTATIONE, PrefixOperator::ExpectationE),
        TK::LongName_CubeRoot                  => &PrefixOperatorParselet::new(Precedence::LONGNAME_CUBEROOT, PrefixOperator::CubeRoot),
        TK::LongName_ProbabilityPr             => &PrefixOperatorParselet::new(Precedence::LONGNAME_PROBABILITYPR, PrefixOperator::ProbabilityPr),

        TK::LinearSyntax_Bang => &PrefixOperatorParselet::new(Precedence::LINEARSYNTAX_BANG, PrefixOperator::CodeParser_PrefixLinearSyntaxBang),
        | TK::LinearSyntax_At
        | TK::LinearSyntax_Amp
        | TK::LinearSyntax_Star
        | TK::LinearSyntax_Under
        | TK::LinearSyntax_Caret
        | TK::LinearSyntax_Space
        | TK::LinearSyntax_Percent
        | TK::LinearSyntax_Plus
        | TK::LinearSyntax_Slash
        | TK::LinearSyntax_BackTick
        | TK::LinearSyntax_CloseParen => &PrefixUnsupportedTokenParselet {},


        //
        // Groups
        //
        TK::OpenParen                        => &GroupParselet::new(TK::OpenParen, GroupOperator::CodeParser_GroupParen),
        TK::OpenSquare                       => &GroupParselet::new(TK::OpenSquare, GroupOperator::CodeParser_GroupSquare),
        TK::OpenCurly                        => &GroupParselet::new(TK::OpenCurly, GroupOperator::List),
        TK::LessBar                          => &GroupParselet::new(TK::LessBar, GroupOperator::Association),
        TK::ColonColonOpenSquare             => &GroupParselet::new(TK::ColonColonOpenSquare, GroupOperator::CodeParser_GroupTypeSpecifier),
        TK::LongName_LeftAngleBracket        => &GroupParselet::new(TK::LongName_LeftAngleBracket, GroupOperator::AngleBracket),
        TK::LongName_LeftCeiling             => &GroupParselet::new(TK::LongName_LeftCeiling, GroupOperator::Ceiling),
        TK::LongName_LeftFloor               => &GroupParselet::new(TK::LongName_LeftFloor, GroupOperator::Floor),
        TK::LongName_LeftDoubleBracket       => &GroupParselet::new(TK::LongName_LeftDoubleBracket, GroupOperator::CodeParser_GroupDoubleBracket),
        TK::LongName_LeftBracketingBar       => &GroupParselet::new(TK::LongName_LeftBracketingBar, GroupOperator::BracketingBar),
        TK::LongName_LeftDoubleBracketingBar => &GroupParselet::new(TK::LongName_LeftDoubleBracketingBar, GroupOperator::DoubleBracketingBar),
        TK::LongName_LeftAssociation         => &GroupParselet::new(TK::LongName_LeftAssociation, GroupOperator::Association),
        TK::LongName_OpenCurlyQuote          => &GroupParselet::new(TK::LongName_OpenCurlyQuote, GroupOperator::CurlyQuote),
        TK::LongName_OpenCurlyDoubleQuote    => &GroupParselet::new(TK::LongName_OpenCurlyDoubleQuote, GroupOperator::CurlyDoubleQuote),

        //----------------------------
        // Special
        //----------------------------

        //
        // context sensitive parsing of  x_
        //
        TK::Symbol => &SymbolParselet {},

        //
        // context sensitive parsing of _x
        //
        TK::Under           => &UnderParselet::new(CompoundOperator::Blank, CompoundOperator::CodeParser_PatternBlank),
        TK::UnderUnder      => &UnderParselet::new(CompoundOperator::BlankSequence, CompoundOperator::CodeParser_PatternBlankSequence),
        TK::UnderUnderUnder => &UnderParselet::new(CompoundOperator::BlankNullSequence, CompoundOperator::CodeParser_PatternBlankNullSequence),

        TK::UnderDot => &UnderDotParselet {},


        TK::Hash => &HashParselet {},
        TK::HashHash => &HashHashParselet {},

        TK::Percent => &PercentParselet {},
        TK::PercentPercent => &LeafParselet {},

        // prefix, infix, postfix
        TK::SemiSemi => &SemiSemiParselet {},

        //
        // Has to handle \[Integral] f \[DifferentialD] x
        //
        TK::LongName_Integral                        => &IntegralParselet::new(PrefixBinaryOperator::Integrate, PrefixOperator::Integral),
        TK::LongName_ContourIntegral                 => &IntegralParselet::new(PrefixBinaryOperator::ContourIntegral, PrefixOperator::ContourIntegral),
        TK::LongName_DoubleContourIntegral           => &IntegralParselet::new(PrefixBinaryOperator::DoubleContourIntegral, PrefixOperator::DoubleContourIntegral),
        TK::LongName_ClockwiseContourIntegral        => &IntegralParselet::new(PrefixBinaryOperator::ClockwiseContourIntegral, PrefixOperator::ClockwiseContourIntegral),
        TK::LongName_CounterClockwiseContourIntegral => &IntegralParselet::new(PrefixBinaryOperator::CounterClockwiseContourIntegral, PrefixOperator::CounterClockwiseContourIntegral),

        // stringify next token (as a file]
        TK::LessLess => &LessLessParselet {},


        TK::QuestionQuestion => &PrefixUnsupportedTokenParselet {},

        // Also use for operators that are only valid in StandardForm.
        // e.g., \[Limit] does not have an interpretation in InputForm
        //
        // \[Limit] is not letterlike, so it needs some kind of categorization,
        // but it also needs to be prevented from making any valid parses.
        TK::LongName_Limit
        | TK::LongName_MaxLimit
        | TK::LongName_MinLimit => &PrefixUnsupportedTokenParselet {},

        // technically, \[AutoLeftMatch] foo \[AutoRightMatch] does parse as
        // AutoMatch[foo] in InputForm but this is not documented,
        // and I'm not going to support it
        TK::LongName_AutoLeftMatch
        | TK::LongName_AutoRightMatch
        | TK::LongName_DiscreteShift
        | TK::LongName_DifferenceDelta
        | TK::LongName_DiscreteRatio
        | TK::LongName_PartialD => &PrefixUnsupportedTokenParselet {},


        _ => &PrefixUnhandledParselet {},
    } }}
}

//======================================
// Infix Parselets
//======================================

macro_rules! token_kind_to_infix_parselet {
    ($ty:ty; $kind:ident) => {{

    use crate::{
        tokenize::TokenKind as TK,
        parse::parselet::*,
        precedence::Precedence
    };

    match $kind {
        TK::EndOfFile => &InfixAssertFalseParselet {},

        TK::Unknown
        | TK::Whitespace
        | TK::InternalNewline
        | TK::Comment => &InfixAssertFalseParselet {},

        TK::ToplevelNewline => &InfixToplevelNewlineParselet {},


        TK::Error_ExpectedEqual
        | TK::Error_Number
        | TK::Error_UnhandledCharacter
        | TK::Error_ExpectedLetterlike
        | TK::Error_Aborted
        | TK::Error_ExpectedOperand
        | TK::Error_ExpectedTag
        | TK::Error_ExpectedFile
        | TK::Error_UnterminatedComment
        | TK::Error_UnterminatedString
        | TK::Error_UnterminatedFileString
        | TK::Error_UnterminatedLinearSyntaxBlob
        | TK::Error_UnsupportedToken
        | TK::Error_UnexpectedCloser
        | TK::Error_UnsafeCharacterEncoding
        | TK::Error_UnexpectedCommentCloser => &InfixAssertFalseParselet {},

        TK::BarGreater
        | TK::CloseCurly
        | TK::CloseParen
        | TK::CloseSquare
        | TK::LongName_CloseCurlyDoubleQuote
        | TK::LongName_CloseCurlyQuote
        | TK::LongName_RightAngleBracket
        | TK::LongName_RightAssociation
        | TK::LongName_RightBracketingBar
        | TK::LongName_RightCeiling
        | TK::LongName_RightDoubleBracket
        | TK::LongName_RightDoubleBracketingBar
        | TK::LongName_RightFloor => &InfixAssertFalseParselet {},

        TK::LongName_DifferentialD
        | TK::LongName_CapitalDifferentialD => &InfixDifferentialDParselet {},


        //
        // Binary
        //

        TK::Slash            => &BinaryOperatorParselet::new(Precedence::SLASH, BinaryOperator::Divide),
        TK::Caret            => &BinaryOperatorParselet::new(Precedence::CARET, BinaryOperator::Power),
        TK::CaretEqual       => &BinaryOperatorParselet::new(Precedence::CARETEQUAL, BinaryOperator::UpSet),
        TK::CaretColonEqual  => &BinaryOperatorParselet::new(Precedence::CARETCOLONEQUAL, BinaryOperator::UpSetDelayed),
        TK::SlashAt          => &BinaryOperatorParselet::new(Precedence::SLASHAT, BinaryOperator::Map),
        TK::MinusGreater     => &BinaryOperatorParselet::new(Precedence::MINUSGREATER, BinaryOperator::Rule),
        TK::AtAt             => &BinaryOperatorParselet::new(Precedence::ATAT, BinaryOperator::Apply),
        TK::SlashSemi        => &BinaryOperatorParselet::new(Precedence::SLASHSEMI, BinaryOperator::Condition),
        TK::SlashDot         => &BinaryOperatorParselet::new(Precedence::SLASHDOT, BinaryOperator::ReplaceAll),
        TK::ColonGreater     => &BinaryOperatorParselet::new(Precedence::COLONGREATER, BinaryOperator::RuleDelayed),
        TK::SlashSlashDot    => &BinaryOperatorParselet::new(Precedence::SLASHSLASHDOT, BinaryOperator::ReplaceRepeated),
        TK::PlusEqual        => &BinaryOperatorParselet::new(Precedence::PLUSEQUAL, BinaryOperator::AddTo),
        TK::StarEqual        => &BinaryOperatorParselet::new(Precedence::STAREQUAL, BinaryOperator::TimesBy),
        TK::MinusEqual       => &BinaryOperatorParselet::new(Precedence::MINUSEQUAL, BinaryOperator::SubtractFrom),
        TK::SlashEqual       => &BinaryOperatorParselet::new(Precedence::SLASHEQUAL, BinaryOperator::DivideBy),
        TK::LessMinusGreater => &BinaryOperatorParselet::new(Precedence::LESSMINUSGREATER, BinaryOperator::TwoWayRule),
        TK::SlashSlashAt     => &BinaryOperatorParselet::new(Precedence::SLASHSLASHAT, BinaryOperator::MapAll),
        TK::At               => &BinaryOperatorParselet::new(Precedence::AT, BinaryOperator::CodeParser_BinaryAt),
        TK::AtAtAt           => &BinaryOperatorParselet::new(Precedence::ATATAT, BinaryOperator::MapApply),
        TK::SlashSlash       => &BinaryOperatorParselet::new(Precedence::SLASHSLASH, BinaryOperator::CodeParser_BinarySlashSlash),
        TK::Question         => &BinaryOperatorParselet::new(Precedence::INFIX_QUESTION, BinaryOperator::PatternTest),
        TK::BarMinusGreater  => &BinaryOperatorParselet::new(Precedence::BARMINUSGREATER, BinaryOperator::Function),
        TK::SlashSlashEqual  => &BinaryOperatorParselet::new(Precedence::SLASHSLASHEQUAL, BinaryOperator::ApplyTo),

        TK::LongName_Divide               => &BinaryOperatorParselet::new(Precedence::LONGNAME_DIVIDE, BinaryOperator::Divide),
        TK::LongName_DivisionSlash        => &BinaryOperatorParselet::new(Precedence::LONGNAME_DIVISIONSLASH, BinaryOperator::Divide),
        TK::LongName_Implies              => &BinaryOperatorParselet::new(Precedence::LONGNAME_IMPLIES, BinaryOperator::Implies),
        TK::LongName_RoundImplies         => &BinaryOperatorParselet::new(Precedence::LONGNAME_ROUNDIMPLIES, BinaryOperator::RoundImplies),
        TK::LongName_PlusMinus            => &BinaryOperatorParselet::new(Precedence::INFIX_LONGNAME_PLUSMINUS, BinaryOperator::PlusMinus),
        TK::LongName_DirectedEdge         => &BinaryOperatorParselet::new(Precedence::LONGNAME_DIRECTEDEDGE, BinaryOperator::DirectedEdge),
        TK::LongName_Rule                 => &BinaryOperatorParselet::new(Precedence::LONGNAME_RULE, BinaryOperator::Rule),
        TK::LongName_RuleDelayed          => &BinaryOperatorParselet::new(Precedence::LONGNAME_RULEDELAYED, BinaryOperator::RuleDelayed),
        TK::LongName_UndirectedEdge       => &BinaryOperatorParselet::new(Precedence::LONGNAME_UNDIRECTEDEDGE, BinaryOperator::UndirectedEdge),
        TK::LongName_Function             => &BinaryOperatorParselet::new(Precedence::LONGNAME_FUNCTION, BinaryOperator::Function),
        TK::LongName_MinusPlus            => &BinaryOperatorParselet::new(Precedence::INFIX_LONGNAME_MINUSPLUS, BinaryOperator::MinusPlus),
        TK::LongName_TwoWayRule           => &BinaryOperatorParselet::new(Precedence::LONGNAME_TWOWAYRULE, BinaryOperator::TwoWayRule),
        TK::LongName_InvisibleApplication => &BinaryOperatorParselet::new(Precedence::LONGNAME_INVISIBLEAPPLICATION, BinaryOperator::CodeParser_BinaryAt),
        TK::LongName_CircleMinus          => &BinaryOperatorParselet::new(Precedence::LONGNAME_CIRCLEMINUS, BinaryOperator::CircleMinus),
        TK::LongName_SuchThat             => &BinaryOperatorParselet::new(Precedence::LONGNAME_SUCHTHAT, BinaryOperator::SuchThat),
        TK::LongName_Perpendicular        => &BinaryOperatorParselet::new(Precedence::LONGNAME_PERPENDICULAR, BinaryOperator::Perpendicular),
        TK::LongName_Because              => &BinaryOperatorParselet::new(Precedence::LONGNAME_BECAUSE, BinaryOperator::Because),
        TK::LongName_Therefore            => &BinaryOperatorParselet::new(Precedence::LONGNAME_THEREFORE, BinaryOperator::Therefore),
        TK::LongName_RightTee             => &BinaryOperatorParselet::new(Precedence::LONGNAME_RIGHTTEE, BinaryOperator::RightTee),
        TK::LongName_LeftTee              => &BinaryOperatorParselet::new(Precedence::LONGNAME_LEFTTEE, BinaryOperator::LeftTee),
        TK::LongName_DoubleRightTee       => &BinaryOperatorParselet::new(Precedence::LONGNAME_DOUBLERIGHTTEE, BinaryOperator::DoubleRightTee),
        TK::LongName_DoubleLeftTee        => &BinaryOperatorParselet::new(Precedence::LONGNAME_DOUBLELEFTTEE, BinaryOperator::DoubleLeftTee),
        TK::LongName_UpTee                => &BinaryOperatorParselet::new(Precedence::LONGNAME_UPTEE, BinaryOperator::UpTee),
        TK::LongName_DownTee              => &BinaryOperatorParselet::new(Precedence::LONGNAME_DOWNTEE, BinaryOperator::DownTee),
        TK::LongName_Application          => &BinaryOperatorParselet::new(Precedence::LONGNAME_APPLICATION, BinaryOperator::Application),


        //
        // Infix
        //
        // Note that these are the operators that make sense to be infix in WL source code.
        //
        // These may not necessarily correspond to Flat functions in WL.
        //
        TK::Minus           => &InfixOperatorParselet::new(Precedence::INFIX_MINUS, InfixOperator::Plus),
        TK::EqualEqualEqual => &InfixOperatorParselet::new(Precedence::EQUALEQUALEQUAL, InfixOperator::SameQ),
        TK::EqualBangEqual  => &InfixOperatorParselet::new(Precedence::EQUALBANGEQUAL, InfixOperator::UnsameQ),
        TK::Plus            => &InfixOperatorParselet::new(Precedence::INFIX_PLUS, InfixOperator::Plus),
        TK::Dot             => &InfixOperatorParselet::new(Precedence::DOT, InfixOperator::Dot),
        TK::StarStar        => &InfixOperatorParselet::new(Precedence::STARSTAR, InfixOperator::NonCommutativeMultiply),
        TK::AmpAmp          => &InfixOperatorParselet::new(Precedence::AMPAMP, InfixOperator::And),
        TK::BarBar          => &InfixOperatorParselet::new(Precedence::BARBAR, InfixOperator::Or),
        TK::Bar             => &InfixOperatorParselet::new(Precedence::BAR, InfixOperator::Alternatives),
        TK::LessGreater     => &InfixOperatorParselet::new(Precedence::LESSGREATER, InfixOperator::StringJoin),
        TK::TildeTilde      => &InfixOperatorParselet::new(Precedence::TILDETILDE, InfixOperator::StringExpression),
        TK::AtStar          => &InfixOperatorParselet::new(Precedence::ATSTAR, InfixOperator::Composition),
        TK::SlashStar       => &InfixOperatorParselet::new(Precedence::SLASHSTAR, InfixOperator::RightComposition),

        //
        // Times
        //
        TK::Star
        | TK::LongName_Times
        | TK::LongName_InvisibleTimes
        | TK::Fake_ImplicitTimes => &TimesParselet {},


        //
        // Set relations
        //
        TK::LongName_Element                => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Element),
        TK::LongName_Subset                 => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Subset),
        TK::LongName_Superset               => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Superset),
        TK::LongName_SubsetEqual            => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SubsetEqual),
        TK::LongName_SupersetEqual          => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SupersetEqual),
        TK::LongName_NotElement             => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotElement),
        TK::LongName_NotSubset              => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSubset),
        TK::LongName_NotSuperset            => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSuperset),
        TK::LongName_NotSubsetEqual         => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSubsetEqual),
        TK::LongName_NotSupersetEqual       => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSupersetEqual),
        TK::LongName_SquareSubset           => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSubset),
        TK::LongName_SquareSuperset         => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSuperset),
        TK::LongName_NotSquareSubset        => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSubset),
        TK::LongName_NotSquareSuperset      => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSuperset),
        TK::LongName_SquareSubsetEqual      => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSubsetEqual),
        TK::LongName_SquareSupersetEqual    => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::SquareSupersetEqual),
        TK::LongName_NotSquareSubsetEqual   => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSubsetEqual),
        TK::LongName_NotSquareSupersetEqual => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotSquareSupersetEqual),
        TK::LongName_ReverseElement         => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::ReverseElement),
        TK::LongName_NotReverseElement      => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::NotReverseElement),
        TK::LongName_Distributed            => &InfixOperatorParselet::new(Precedence::CLASS_SETRELATIONS, InfixOperator::Distributed),

        TK::LongName_ImplicitPlus => &InfixOperatorParselet::new(Precedence::LONGNAME_IMPLICITPLUS, InfixOperator::Plus),
        TK::LongName_And          => &InfixOperatorParselet::new(Precedence::LONGNAME_AND, InfixOperator::And),
        TK::LongName_Or           => &InfixOperatorParselet::new(Precedence::LONGNAME_OR, InfixOperator::Or),
        TK::LongName_Xor          => &InfixOperatorParselet::new(Precedence::LONGNAME_XOR, InfixOperator::Xor),
        TK::LongName_Nand         => &InfixOperatorParselet::new(Precedence::LONGNAME_NAND, InfixOperator::Nand),
        TK::LongName_Nor          => &InfixOperatorParselet::new(Precedence::LONGNAME_NOR, InfixOperator::Nor),

        //
        // Horizontal arrows
        //
        TK::LongName_LeftArrow            => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftArrow),
        TK::LongName_RightArrow           => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightArrow),
        TK::LongName_LeftRightArrow       => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftRightArrow),
        TK::LongName_LeftTeeArrow         => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftTeeArrow),
        TK::LongName_RightTeeArrow        => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightTeeArrow),
        TK::LongName_RightArrowLeftArrow  => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightArrowLeftArrow),
        TK::LongName_LeftArrowRightArrow  => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftArrowRightArrow),
        TK::LongName_DoubleLeftArrow      => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::DoubleLeftArrow),
        TK::LongName_DoubleRightArrow     => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::DoubleRightArrow),
        TK::LongName_DoubleLeftRightArrow => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::DoubleLeftRightArrow),
        TK::LongName_LeftArrowBar         => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::LeftArrowBar),
        TK::LongName_RightArrowBar        => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::RightArrowBar),
        TK::LongName_ShortRightArrow      => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::ShortRightArrow),
        TK::LongName_ShortLeftArrow       => &InfixOperatorParselet::new(Precedence::CLASS_HORIZONTALARROWS, InfixOperator::ShortLeftArrow),

        //
        // Diagonal arrow operators
        //
        TK::LongName_UpperLeftArrow  => &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::UpperLeftArrow),
        TK::LongName_UpperRightArrow => &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::UpperRightArrow),
        TK::LongName_LowerRightArrow => &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::LowerRightArrow),
        TK::LongName_LowerLeftArrow  => &InfixOperatorParselet::new(Precedence::CLASS_DIAGONALARROWOPERATORS, InfixOperator::LowerLeftArrow),

        //
        // Vector operators
        //
        TK::LongName_LeftVector          => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftVector),
        TK::LongName_RightVector         => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::RightVector),
        TK::LongName_LeftRightVector     => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftRightVector),
        TK::LongName_LeftVectorBar       => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftVectorBar),
        TK::LongName_RightVectorBar      => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::RightVectorBar),
        TK::LongName_LeftTeeVector       => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::LeftTeeVector),
        TK::LongName_RightTeeVector      => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::RightTeeVector),
        TK::LongName_DownLeftVector      => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftVector),
        TK::LongName_DownRightVector     => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownRightVector),
        TK::LongName_DownLeftRightVector => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftRightVector),
        TK::LongName_DownLeftVectorBar   => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftVectorBar),
        TK::LongName_DownRightVectorBar  => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownRightVectorBar),
        TK::LongName_DownLeftTeeVector   => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownLeftTeeVector),
        TK::LongName_DownRightTeeVector  => &InfixOperatorParselet::new(Precedence::CLASS_VECTOROPERATORS, InfixOperator::DownRightTeeVector),

        //
        // Vertical arrow operators
        //
        TK::LongName_UpArrow           => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpArrow),
        TK::LongName_DownArrow         => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownArrow),
        TK::LongName_UpDownArrow       => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpDownArrow),
        TK::LongName_UpTeeArrow        => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpTeeArrow),
        TK::LongName_DownTeeArrow      => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownTeeArrow),
        TK::LongName_UpArrowDownArrow  => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpArrowDownArrow),
        TK::LongName_DoubleUpArrow     => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleUpArrow),
        TK::LongName_DoubleDownArrow   => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleDownArrow),
        TK::LongName_DoubleUpDownArrow => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleUpDownArrow),
        TK::LongName_DownArrowUpArrow  => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownArrowUpArrow),
        //
        // itai asking about precedence of "long" arrows:
        // https://mail-archive.wolfram.com/archive/l-typeset/2021/Jul00/0000.html
        //
        TK::LongName_LongLeftArrow            => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::LongLeftArrow),
        TK::LongName_LongRightArrow           => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::LongRightArrow),
        TK::LongName_LongLeftRightArrow       => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::LongLeftRightArrow),
        TK::LongName_DoubleLongLeftArrow      => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleLongLeftArrow),
        TK::LongName_DoubleLongRightArrow     => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleLongRightArrow),
        TK::LongName_DoubleLongLeftRightArrow => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DoubleLongLeftRightArrow),
        TK::LongName_UpArrowBar               => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::UpArrowBar),
        TK::LongName_DownArrowBar             => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::DownArrowBar),
        TK::LongName_ShortUpArrow             => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::ShortUpArrow),
        TK::LongName_ShortDownArrow           => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALARROWOPERATORS, InfixOperator::ShortDownArrow),


        //
        // Vertical vector operators
        //
        TK::LongName_RightUpVector        => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpVector),
        TK::LongName_LeftUpVector         => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpVector),
        TK::LongName_RightDownVector      => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightDownVector),
        TK::LongName_LeftDownVector       => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftDownVector),
        TK::LongName_RightUpDownVector    => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpDownVector),
        TK::LongName_LeftUpDownVector     => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpDownVector),
        TK::LongName_RightUpVectorBar     => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpVectorBar),
        TK::LongName_RightDownVectorBar   => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightDownVectorBar),
        TK::LongName_LeftUpVectorBar      => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpVectorBar),
        TK::LongName_LeftDownVectorBar    => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftDownVectorBar),
        TK::LongName_RightUpTeeVector     => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightUpTeeVector),
        TK::LongName_RightDownTeeVector   => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::RightDownTeeVector),
        TK::LongName_LeftUpTeeVector      => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftUpTeeVector),
        TK::LongName_LeftDownTeeVector    => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::LeftDownTeeVector),
        TK::LongName_UpEquilibrium        => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::UpEquilibrium),
        TK::LongName_ReverseUpEquilibrium => &InfixOperatorParselet::new(Precedence::CLASS_VERTICALVECTOROPERATORS, InfixOperator::ReverseUpEquilibrium),


        TK::LongName_CenterDot   => &InfixOperatorParselet::new(Precedence::LONGNAME_CENTERDOT, InfixOperator::CenterDot),
        TK::LongName_Equivalent  => &InfixOperatorParselet::new(Precedence::LONGNAME_EQUIVALENT, InfixOperator::Equivalent),
        TK::LongName_CircleDot   => &InfixOperatorParselet::new(Precedence::LONGNAME_CIRCLEDOT, InfixOperator::CircleDot),
        TK::LongName_Conditioned => &InfixOperatorParselet::new(Precedence::LONGNAME_CONDITIONED, InfixOperator::Conditioned),

        //
        // Union operators
        //
        TK::LongName_Union       => &InfixOperatorParselet::new(Precedence::CLASS_UNIONOPERATORS, InfixOperator::Union),
        TK::LongName_SquareUnion => &InfixOperatorParselet::new(Precedence::CLASS_UNIONOPERATORS, InfixOperator::SquareUnion),
        TK::LongName_UnionPlus   => &InfixOperatorParselet::new(Precedence::CLASS_UNIONOPERATORS, InfixOperator::UnionPlus),

        //
        // Intersection operators
        //
        TK::LongName_Intersection       => &InfixOperatorParselet::new(Precedence::CLASS_INTERSECTIONOPERATORS, InfixOperator::Intersection),
        TK::LongName_SquareIntersection => &InfixOperatorParselet::new(Precedence::CLASS_INTERSECTIONOPERATORS, InfixOperator::SquareIntersection),


        TK::LongName_TensorWedge          => &InfixOperatorParselet::new(Precedence::LONGNAME_TENSORWEDGE, InfixOperator::TensorWedge),
        TK::LongName_TensorProduct        => &InfixOperatorParselet::new(Precedence::LONGNAME_TENSORPRODUCT, InfixOperator::TensorProduct),
        TK::LongName_Cross                => &InfixOperatorParselet::new(Precedence::LONGNAME_CROSS, InfixOperator::Cross),
        TK::LongName_SmallCircle          => &InfixOperatorParselet::new(Precedence::LONGNAME_SMALLCIRCLE, InfixOperator::SmallCircle),
        TK::LongName_Divides              => &InfixOperatorParselet::new(Precedence::LONGNAME_DIVIDES, InfixOperator::Divisible),
        TK::LongName_VerticalSeparator    => &InfixOperatorParselet::new(Precedence::LONGNAME_VERTICALSEPARATOR, InfixOperator::VerticalSeparator),
        TK::LongName_Backslash            => &InfixOperatorParselet::new(Precedence::LONGNAME_BACKSLASH, InfixOperator::Backslash),
        TK::LongName_Diamond              => &InfixOperatorParselet::new(Precedence::LONGNAME_DIAMOND, InfixOperator::Diamond),
        TK::LongName_Wedge                => &InfixOperatorParselet::new(Precedence::LONGNAME_WEDGE, InfixOperator::Wedge),
        TK::LongName_Vee                  => &InfixOperatorParselet::new(Precedence::LONGNAME_VEE, InfixOperator::Vee),
        TK::LongName_CircleTimes          => &InfixOperatorParselet::new(Precedence::INFIX_LONGNAME_CIRCLETIMES, InfixOperator::CircleTimes),
        TK::LongName_Star                 => &InfixOperatorParselet::new(Precedence::LONGNAME_STAR, InfixOperator::Star),
        TK::LongName_VerticalTilde        => &InfixOperatorParselet::new(Precedence::LONGNAME_VERTICALTILDE, InfixOperator::VerticalTilde),
        TK::LongName_Coproduct            => &InfixOperatorParselet::new(Precedence::INFIX_LONGNAME_COPRODUCT, InfixOperator::Coproduct),
        TK::LongName_Cap                  => &InfixOperatorParselet::new(Precedence::LONGNAME_CAP, InfixOperator::Cap),
        TK::LongName_Cup                  => &InfixOperatorParselet::new(Precedence::LONGNAME_CUP, InfixOperator::Cup),
        TK::LongName_CirclePlus           => &InfixOperatorParselet::new(Precedence::LONGNAME_CIRCLEPLUS, InfixOperator::CirclePlus),
        TK::LongName_VerticalBar          => &InfixOperatorParselet::new(Precedence::LONGNAME_VERTICALBAR, InfixOperator::VerticalBar),
        TK::LongName_DoubleVerticalBar    => &InfixOperatorParselet::new(Precedence::LONGNAME_DOUBLEVERTICALBAR, InfixOperator::DoubleVerticalBar),
        TK::LongName_NotVerticalBar       => &InfixOperatorParselet::new(Precedence::LONGNAME_NOTVERTICALBAR, InfixOperator::NotVerticalBar),
        TK::LongName_NotDoubleVerticalBar => &InfixOperatorParselet::new(Precedence::LONGNAME_NOTDOUBLEVERTICALBAR, InfixOperator::NotDoubleVerticalBar),

        //
        // Ordering operators
        //
        TK::LongName_LeftTriangle          => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::LeftTriangle),
        TK::LongName_RightTriangle         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::RightTriangle),
        TK::LongName_NotLeftTriangle       => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotLeftTriangle),
        TK::LongName_NotRightTriangle      => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotRightTriangle),
        TK::LongName_LeftTriangleEqual     => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::LeftTriangleEqual),
        TK::LongName_RightTriangleEqual    => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::RightTriangleEqual),
        TK::LongName_NotLeftTriangleEqual  => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotLeftTriangleEqual),
        TK::LongName_NotRightTriangleEqual => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotRightTriangleEqual),
        TK::LongName_LeftTriangleBar       => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::LeftTriangleBar),
        TK::LongName_RightTriangleBar      => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::RightTriangleBar),
        TK::LongName_NotLeftTriangleBar    => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotLeftTriangleBar),
        TK::LongName_NotRightTriangleBar   => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotRightTriangleBar),
        TK::LongName_TildeEqual            => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::TildeEqual),
        TK::LongName_NotTildeEqual         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTildeEqual),
        TK::LongName_TildeFullEqual        => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::TildeFullEqual),
        TK::LongName_NotTildeFullEqual     => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTildeFullEqual),
        TK::LongName_Tilde                 => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Tilde),
        TK::LongName_NotTilde              => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTilde),
        TK::LongName_EqualTilde            => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::EqualTilde),
        TK::LongName_NotEqualTilde         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotEqualTilde),
        TK::LongName_TildeTilde            => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::TildeTilde),
        TK::LongName_NotTildeTilde         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotTildeTilde),
        TK::LongName_Proportional          => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Proportional),
        TK::LongName_Proportion            => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Proportion),
        TK::LongName_Congruent             => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Congruent),
        TK::LongName_NotCongruent          => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotCongruent),
        TK::LongName_Equilibrium           => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Equilibrium),
        TK::LongName_ReverseEquilibrium    => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::ReverseEquilibrium),
        TK::LongName_DotEqual              => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::DotEqual),
        TK::LongName_Precedes              => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Precedes),
        TK::LongName_Succeeds              => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::Succeeds),
        TK::LongName_PrecedesEqual         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::PrecedesEqual),
        TK::LongName_SucceedsEqual         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::SucceedsEqual),
        TK::LongName_PrecedesTilde         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::PrecedesTilde),
        TK::LongName_SucceedsTilde         => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::SucceedsTilde),
        TK::LongName_PrecedesSlantEqual    => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::PrecedesSlantEqual),
        TK::LongName_SucceedsSlantEqual    => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::SucceedsSlantEqual),
        TK::LongName_NotPrecedes           => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedes),
        TK::LongName_NotSucceeds           => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceeds),
        TK::LongName_NotPrecedesEqual      => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedesEqual),
        TK::LongName_NotSucceedsEqual      => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceedsEqual),
        TK::LongName_NotPrecedesTilde      => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedesTilde),
        TK::LongName_NotSucceedsTilde      => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceedsTilde),
        TK::LongName_NotPrecedesSlantEqual => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotPrecedesSlantEqual),
        TK::LongName_NotSucceedsSlantEqual => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotSucceedsSlantEqual),
        TK::LongName_CupCap                => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::CupCap),
        TK::LongName_NotCupCap             => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotCupCap),
        TK::LongName_HumpEqual             => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::HumpEqual),
        TK::LongName_HumpDownHump          => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::HumpDownHump),
        TK::LongName_NotHumpEqual          => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotHumpEqual),
        TK::LongName_NotHumpDownHump       => &InfixOperatorParselet::new(Precedence::CLASS_ORDERINGOPERATORS, InfixOperator::NotHumpDownHump),

        //
        // special Inequality
        //
        TK::BangEqual                        => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::EqualEqual                       => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::Greater                          => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::GreaterEqual                     => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LessEqual                        => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::Less                             => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_Equal                   => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterEqual            => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterEqualLess        => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterFullEqual        => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterGreater          => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterLess             => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterSlantEqual       => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_GreaterTilde            => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessEqual               => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessEqualGreater        => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessFullEqual           => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessGreater             => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessLess                => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessSlantEqual          => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LessTilde               => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_LongEqual               => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NestedGreaterGreater    => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NestedLessLess          => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotEqual                => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreater              => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreaterEqual         => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreaterFullEqual     => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreaterGreater       => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreaterLess          => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreaterSlantEqual    => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotGreaterTilde         => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLess                 => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLessEqual            => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLessFullEqual        => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLessGreater          => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLessLess             => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLessSlantEqual       => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotLessTilde            => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotNestedGreaterGreater => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_NotNestedLessLess       => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        //
        // special VectorInequality
        //
        TK::LongName_VectorGreater      => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_VectorGreaterEqual => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_VectorLess         => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),
        TK::LongName_VectorLessEqual    => &InfixOperatorParselet::new(Precedence::CLASS_INEQUALITY, InfixOperator::CodeParser_InfixInequality),


        TK::LongName_PermutationProduct => &InfixOperatorParselet::new(Precedence::LONGNAME_PERMUTATIONPRODUCT, InfixOperator::PermutationProduct),
        TK::LongName_Colon              => &InfixOperatorParselet::new(Precedence::LONGNAME_COLON, InfixOperator::Colon),
        TK::LongName_Xnor               => &InfixOperatorParselet::new(Precedence::LONGNAME_XNOR, InfixOperator::Xnor),
        TK::LongName_Minus              => &InfixOperatorParselet::new(Precedence::INFIX_LONGNAME_MINUS, InfixOperator::Plus),


        //
        // Postfix
        //
        TK::Amp                                 => &PostfixOperatorParselet::new(Precedence::AMP, PostfixOperator::Function),
        TK::DotDot                              => &PostfixOperatorParselet::new(Precedence::DOTDOT, PostfixOperator::Repeated),
        TK::Bang                                => &PostfixOperatorParselet::new(Precedence::POSTFIX_BANG, PostfixOperator::Factorial),
        TK::MinusMinus                          => &PostfixOperatorParselet::new(Precedence::POSTFIX_MINUSMINUS, PostfixOperator::Decrement),
        TK::PlusPlus                            => &PostfixOperatorParselet::new(Precedence::POSTFIX_PLUSPLUS, PostfixOperator::Increment),
        TK::DotDotDot                           => &PostfixOperatorParselet::new(Precedence::DOTDOTDOT, PostfixOperator::RepeatedNull),
        TK::BangBang                            => &PostfixOperatorParselet::new(Precedence::POSTFIX_BANGBANG, PostfixOperator::Factorial2),
        TK::SingleQuote                         => &PostfixOperatorParselet::new(Precedence::SINGLEQUOTE, PostfixOperator::Derivative),
        TK::LongName_Transpose                  => &PostfixOperatorParselet::new(Precedence::LONGNAME_TRANSPOSE, PostfixOperator::Transpose),
        TK::LongName_Conjugate                  => &PostfixOperatorParselet::new(Precedence::LONGNAME_CONJUGATE, PostfixOperator::Conjugate),
        TK::LongName_ConjugateTranspose         => &PostfixOperatorParselet::new(Precedence::LONGNAME_CONJUGATETRANSPOSE, PostfixOperator::ConjugateTranspose),
        TK::LongName_HermitianConjugate         => &PostfixOperatorParselet::new(Precedence::LONGNAME_HERMITIANCONJUGATE, PostfixOperator::HermitianConjugate),
        TK::LongName_InvisiblePostfixScriptBase => &PostfixOperatorParselet::new(Precedence::LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, PostfixOperator::InvisiblePostfixScriptBase),


        //
        // Calls
        //
        TK::OpenSquare                 => &CallParselet::new(GroupParselet::new(TK::OpenSquare, GroupOperator::CodeParser_GroupSquare)),
        TK::LongName_LeftDoubleBracket => &CallParselet::new(GroupParselet::new(TK::LongName_LeftDoubleBracket, GroupOperator::CodeParser_GroupDoubleBracket)),
        TK::ColonColonOpenSquare       => &CallParselet::new(GroupParselet::new(TK::ColonColonOpenSquare, GroupOperator::CodeParser_GroupTypeSpecifier)),




        //
        // trailing ; and , is allowed
        //
        TK::Semi => &SemiParselet {},

        TK::Comma => &CommaParselet {},
        TK::LongName_InvisibleComma => &CommaParselet {},

        //
        // prefix, infix, postfix
        //
        TK::SemiSemi => &SemiSemiParselet {},

        //
        // ternary
        //
        TK::Tilde => &TildeParselet {},

        //
        // context sensitive parsing of sym:obj and pat:v
        //
        TK::Colon => &ColonParselet {},

        //
        // ternary, with different possibilities for second operator
        //
        TK::SlashColon => &SlashColonParselet {},

        //
        // Has to handle  a =.  and  a = .
        //
        TK::Equal => &EqualParselet::new(),
        TK::ColonEqual => &ColonEqualParselet::new(),

        //
        // stringify next token (as a symbol)
        //
        TK::ColonColon => &ColonColonParselet {},

        //
        // stringify next token (as a file)
        //
        TK::GreaterGreater => &GreaterGreaterParselet {},
        TK::GreaterGreaterGreater => &GreaterGreaterGreaterParselet {},


        TK::QuestionQuestion => &InfixAssertFalseParselet {},

        //
        // Also use for operators that are only valid in StandardForm.
        // e.g., \[Limit] does not have an interpretation in InputForm
        //
        // \[Limit] is not letterlike, so it needs some kind of categorization,
        // but it also needs to be prevented from making any valid parses.
        //
        TK::LongName_Limit
        | TK::LongName_MaxLimit
        | TK::LongName_MinLimit => &InfixAssertFalseParselet {},

        //
        // technically, \[AutoLeftMatch] foo \[AutoRightMatch] does parse as
        // AutoMatch[foo] in InputForm but this is not documented,
        // and I'm not going to support it
        //
        | TK::LongName_AutoLeftMatch
        | TK::LongName_AutoRightMatch
        | TK::LongName_DiscreteShift
        | TK::LongName_DifferenceDelta
        | TK::LongName_DiscreteRatio
        | TK::LongName_PartialD => &InfixAssertFalseParselet {},

        // TODO: Debug assert renaming variants are isPossibleBeginning()
        _ => {
            &InfixImplicitTimesParselet {}
        },
    }

    }}
}

pub(crate) use {token_kind_to_infix_parselet, token_kind_to_prefix_parselet};
