//! Operator enums used in parsing
//!
//! Every operator enum in this module implements the [`Operator`] trait:
//!
//! * [`InfixOperator`]
//! * [`PrefixOperator`]
//! * [`PostfixOperator`]
//! * [`BinaryOperator`]
//! * [`TernaryOperator`]
//! * [`PrefixBinaryOperator`]
//! * [`CompoundOperator`]
//! * [`GroupOperator`]
//! * [`CallOperator`]

#![allow(non_upper_case_globals)]
#![cfg_attr(rustfmt, rustfmt_skip)]

use wolfram_expr::symbol::SymbolRef;

use crate::{
	symbol::Symbol,
	symbols as sym
};

/// Marker denoting enums whose variants represent named operators, which
/// can be converted to or from a corresponding canonical symbol representation.
pub trait Operator: Sized + 'static {
    fn to_symbol(&self) -> crate::symbol::Symbol;

    fn try_from_symbol(symbol: wolfram_expr::symbol::SymbolRef)
        -> Option<Self>;
}

//==========================================================
// Operator enum definitions
//==========================================================

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum InfixOperator {
    Times,
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
    /// Prefix '?' on its own line, currently created only during box parsing.
    Information,
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
    /// Created when parsing boxes.
    CodeParser_Comment,
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

//==========================================================
// Operator enum inherent impls
//==========================================================

impl GroupOperator {
    // FIXME: Make this function unnecessary by removing the GroupOperator
    //        variants that overlap with CallOperator. This will require some
    //        refactoring of how the parser parsing of CallParselet works.
    pub(crate) fn try_to_call_operator(self) -> Option<CallOperator> {
        let op = match self {
            GroupOperator::CodeParser_GroupSquare => {
                CallOperator::CodeParser_GroupSquare
            },
            GroupOperator::CodeParser_GroupTypeSpecifier => {
                CallOperator::CodeParser_GroupTypeSpecifier
            },
            GroupOperator::CodeParser_GroupDoubleBracket => {
                CallOperator::CodeParser_GroupDoubleBracket
            },
            GroupOperator::Token_Comment
            | GroupOperator::CodeParser_Comment
            | GroupOperator::CodeParser_GroupParen
            | GroupOperator::List
            | GroupOperator::Association
            | GroupOperator::AngleBracket
            | GroupOperator::Ceiling
            | GroupOperator::Floor
            | GroupOperator::BracketingBar
            | GroupOperator::DoubleBracketingBar
            | GroupOperator::CurlyQuote
            | GroupOperator::CurlyDoubleQuote => {
                panic!("GroupOperator::{self:?} cannot be converted to CallOperator")
            },
        };

        Some(op)
    }
}

//==========================================================
// Operator trait impls
//==========================================================

impl Operator for InfixOperator {
    #[allow(dead_code)]
    fn to_symbol(&self) -> Symbol {
        match self {
            InfixOperator::Times => sym::Times,
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
            PrefixOperator::Information => sym::Information,
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
            sym::Information => PrefixOperator::Information,
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
            GroupOperator::Token_Comment => sym::Token::Comment,
            GroupOperator::CodeParser_Comment => sym::CodeParser::Comment,
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
            sym::Token::Comment => GroupOperator::Token_Comment,
            sym::CodeParser::Comment => GroupOperator::CodeParser_Comment,
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

