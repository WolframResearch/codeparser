//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

use crate::symbol::Symbol;

use wolfram_expr::symbol::SymbolRef;

//
// Computing TokenKind variant value
//

#[rustfmt::skip]
pub(crate) enum Group1 {
    PossibleBeginning = 0b01 << 9,
    Closer            = 0b10 << 9,
    Error             = 0b11 << 9,
    None              = 0b00 << 9,
}

#[rustfmt::skip]
pub(crate) enum Group2 {
    Empty        = 0b01 << 11,
    Unterminated = 0b10 << 11,
    None         = 0b00 << 11,
}

impl Group1 {
    /// Group 1 matches: 0b0000_0xx0_0000_0000 (x is unknown)
    pub(crate) const MASK: u16 = 0b11 << 9;
}

impl Group2 {
    /// Group 2 matches: 0b000x_x000_0000_0000 (x is unknown)
    pub(crate) const MASK: u16 = 0b11 << 11;
}

macro_rules! variant {
    ($count:literal, $group1:ident, $group2:ident) => {
        variant_value($count, Group1::$group1, Group2::$group2)
    };

    ($count:literal, Empty) => {
        variant_value($count, Group1::None, Group2::Empty)
    };

    ($count:literal, $group1:ident) => {
        variant_value($count, Group1::$group1, Group2::None)
    };

    ($count:literal) => {
        variant_value($count, Group1::None, Group2::None)
    };
}

#[rustfmt::skip]
const fn variant_value(count: u16, group1: Group1, group2: Group2) -> u16 {
    let group1 = group1 as u16;
    let group2 = group2 as u16;

    // The unique count should only use the first 9 bits.
    // Group1 uses the next two bits, and Group2 the two bits after that.
    debug_assert!(count  & 0b0000_0001_1111_1111 == count );
    debug_assert!(group1 & 0b0000_0110_0000_0000 == group1);
    debug_assert!(group2 & 0b0001_1000_0000_0000 == group2);

    count | group1 | group2
}

//
// All token enums
//

#[allow(non_camel_case_types)]
#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u16)]
pub enum TokenKind {
    Unknown                                  = variant!(0),
    EndOfFile                                = variant!(1, Empty),
    Symbol                                   = variant!(2, PossibleBeginning),
    String                                   = variant!(3, PossibleBeginning),
    Integer                                  = variant!(4, PossibleBeginning),
    Real                                     = variant!(5, PossibleBeginning),
    Rational                                 = variant!(6, PossibleBeginning),
    LinearSyntaxBlob                         = variant!(7, PossibleBeginning),
    InternalNewline                          = variant!(8),
    Comment                                  = variant!(9),
    Whitespace                               = variant!(10),
    Buffer1                                  = variant!(11),
    ToplevelNewline                          = variant!(12),
    Buffer2                                  = variant!(13),
    Buffer3                                  = variant!(14),
    Buffer4                                  = variant!(15),
    Error_ExpectedEqual                      = variant!(16, Error),
    Error_Number                             = variant!(17, Error),
    Error_UnhandledCharacter                 = variant!(18, Error),
    Error_ExpectedLetterlike                 = variant!(19, Error),
    Error_Aborted                            = variant!(20, Error, Empty),
    Error_ExpectedOperand                    = variant!(21, Error, Empty),
    Error_ExpectedTag                        = variant!(22, Error, Empty),
    Error_ExpectedFile                       = variant!(23, Error, Empty),
    Error_UnexpectedCloser                   = variant!(24, Error),
    Error_PrefixImplicitNull                 = variant!(25, Error, Empty),
    Error_InfixImplicitNull                  = variant!(26, Error, Empty),
    Error_UnsafeCharacterEncoding            = variant!(27, Error),
    Error_UnterminatedComment                = variant!(28, Error, Unterminated),
    Error_UnterminatedString                 = variant!(29, Error, Unterminated),
    Error_UnterminatedFileString             = variant!(30, Error, Unterminated),
    Error_UnterminatedLinearSyntaxBlob       = variant!(31, Error, Unterminated),
    Error_UnsupportedToken                   = variant!(32, Error),
    Error_UnexpectedCommentCloser            = variant!(33, Error),
    Dot                                      = variant!(34),
    Colon                                    = variant!(35),
    OpenParen                                = variant!(36, PossibleBeginning),
    CloseParen                               = variant!(37, Closer),
    OpenSquare                               = variant!(38),
    CloseSquare                              = variant!(39, Closer),
    Comma                                    = variant!(40),
    OpenCurly                                = variant!(41, PossibleBeginning),
    CloseCurly                               = variant!(42, Closer),
    Equal                                    = variant!(43),
    Bang                                     = variant!(44, PossibleBeginning),
    Under                                    = variant!(45, PossibleBeginning),
    Less                                     = variant!(46),
    Greater                                  = variant!(47),
    Minus                                    = variant!(48, PossibleBeginning),
    Bar                                      = variant!(49),
    Semi                                     = variant!(50),
    Hash                                     = variant!(51, PossibleBeginning),
    Amp                                      = variant!(52),
    Slash                                    = variant!(53),
    At                                       = variant!(54),
    Plus                                     = variant!(55, PossibleBeginning),
    Tilde                                    = variant!(56),
    Star                                     = variant!(57),
    Caret                                    = variant!(58),
    SingleQuote                              = variant!(59),
    Percent                                  = variant!(60, PossibleBeginning),
    Question                                 = variant!(61),
    DotDot                                   = variant!(62),
    ColonColon                               = variant!(63),
    ColonEqual                               = variant!(64),
    ColonGreater                             = variant!(65),
    EqualEqual                               = variant!(66),
    UnderUnder                               = variant!(67, PossibleBeginning),
    UnderDot                                 = variant!(68, PossibleBeginning),
    LessBar                                  = variant!(69, PossibleBeginning),
    LessLess                                 = variant!(70, PossibleBeginning),
    LessGreater                              = variant!(71),
    LessEqual                                = variant!(72),
    GreaterGreater                           = variant!(73),
    GreaterEqual                             = variant!(74),
    MinusGreater                             = variant!(75),
    MinusMinus                               = variant!(76, PossibleBeginning),
    MinusEqual                               = variant!(77),
    BarBar                                   = variant!(78),
    BarGreater                               = variant!(79, Closer),
    SemiSemi                                 = variant!(80, PossibleBeginning),
    AmpAmp                                   = variant!(81),
    SlashAt                                  = variant!(82),
    SlashSemi                                = variant!(83),
    SlashDot                                 = variant!(84),
    SlashSlash                               = variant!(85),
    SlashColon                               = variant!(86),
    SlashEqual                               = variant!(87),
    SlashStar                                = variant!(88),
    AtAt                                     = variant!(89),
    AtStar                                   = variant!(90),
    PlusPlus                                 = variant!(91, PossibleBeginning),
    PlusEqual                                = variant!(92),
    TildeTilde                               = variant!(93),
    StarEqual                                = variant!(94),
    StarStar                                 = variant!(95),
    CaretEqual                               = variant!(96),
    HashHash                                 = variant!(97, PossibleBeginning),
    BangEqual                                = variant!(98),
    BangBang                                 = variant!(99, PossibleBeginning),
    QuestionQuestion                         = variant!(100),
    DotDotDot                                = variant!(101),
    EqualEqualEqual                          = variant!(102),
    EqualBangEqual                           = variant!(103),
    UnderUnderUnder                          = variant!(104, PossibleBeginning),
    SlashSlashDot                            = variant!(105),
    AtAtAt                                   = variant!(106),
    LessMinusGreater                         = variant!(107),
    SlashSlashAt                             = variant!(108),
    CaretColonEqual                          = variant!(109),
    GreaterGreaterGreater                    = variant!(110),
    BarMinusGreater                          = variant!(111),
    SlashSlashEqual                          = variant!(112),
    ColonColonOpenSquare                     = variant!(113),
    PercentPercent                           = variant!(114, PossibleBeginning),
    LinearSyntax_Bang                        = variant!(115, PossibleBeginning),
    LinearSyntax_CloseParen                  = variant!(116),
    LinearSyntax_At                          = variant!(117),
    LinearSyntax_Amp                         = variant!(118),
    LinearSyntax_Star                        = variant!(119),
    LinearSyntax_Under                       = variant!(120),
    LinearSyntax_Caret                       = variant!(121),
    LinearSyntax_Space                       = variant!(122),
    LinearSyntax_Percent                     = variant!(123),
    LinearSyntax_Plus                        = variant!(124),
    LinearSyntax_Slash                       = variant!(125),
    LinearSyntax_BackTick                    = variant!(126),
    Fake_ImplicitTimes                       = variant!(127, Empty),
    Fake_ImplicitNull                        = variant!(128, Empty),
    Fake_ImplicitOne                         = variant!(129, Empty),
    Fake_ImplicitAll                         = variant!(130, Empty),
    Boxes_OpenParenStar                      = variant!(131),
    Boxes_StarCloseParen                     = variant!(132),
    Boxes_MultiSingleQuote                   = variant!(133),
    Boxes_MultiWhitespace                    = variant!(134),
    LongName_Not                             = variant!(135, PossibleBeginning),
    LongName_PlusMinus                       = variant!(136, PossibleBeginning),
    LongName_CenterDot                       = variant!(137),
    LongName_Times                           = variant!(138),
    LongName_Divide                          = variant!(139),
    LongName_OpenCurlyQuote                  = variant!(140, PossibleBeginning),
    LongName_CloseCurlyQuote                 = variant!(141, Closer),
    LongName_OpenCurlyDoubleQuote            = variant!(142, PossibleBeginning),
    LongName_CloseCurlyDoubleQuote           = variant!(143, Closer),
    LongName_InvisibleTimes                  = variant!(144),
    LongName_LeftArrow                       = variant!(145),
    LongName_UpArrow                         = variant!(146),
    LongName_RightArrow                      = variant!(147),
    LongName_DownArrow                       = variant!(148),
    LongName_LeftRightArrow                  = variant!(149),
    LongName_UpDownArrow                     = variant!(150),
    LongName_UpperLeftArrow                  = variant!(151),
    LongName_UpperRightArrow                 = variant!(152),
    LongName_LowerRightArrow                 = variant!(153),
    LongName_LowerLeftArrow                  = variant!(154),
    LongName_LeftTeeArrow                    = variant!(155),
    LongName_UpTeeArrow                      = variant!(156),
    LongName_RightTeeArrow                   = variant!(157),
    LongName_DownTeeArrow                    = variant!(158),
    LongName_LeftVector                      = variant!(159),
    LongName_DownLeftVector                  = variant!(160),
    LongName_RightUpVector                   = variant!(161),
    LongName_LeftUpVector                    = variant!(162),
    LongName_RightVector                     = variant!(163),
    LongName_DownRightVector                 = variant!(164),
    LongName_RightDownVector                 = variant!(165),
    LongName_LeftDownVector                  = variant!(166),
    LongName_RightArrowLeftArrow             = variant!(167),
    LongName_UpArrowDownArrow                = variant!(168),
    LongName_LeftArrowRightArrow             = variant!(169),
    LongName_ReverseEquilibrium              = variant!(170),
    LongName_Equilibrium                     = variant!(171),
    LongName_DoubleLeftArrow                 = variant!(172),
    LongName_DoubleUpArrow                   = variant!(173),
    LongName_DoubleRightArrow                = variant!(174),
    LongName_DoubleDownArrow                 = variant!(175),
    LongName_DoubleLeftRightArrow            = variant!(176),
    LongName_DoubleUpDownArrow               = variant!(177),
    LongName_LeftArrowBar                    = variant!(178),
    LongName_RightArrowBar                   = variant!(179),
    LongName_DownArrowUpArrow                = variant!(180),
    LongName_ForAll                          = variant!(181, PossibleBeginning),
    LongName_PartialD                        = variant!(182),
    LongName_Exists                          = variant!(183, PossibleBeginning),
    LongName_NotExists                       = variant!(184, PossibleBeginning),
    LongName_Del                             = variant!(185, PossibleBeginning),
    LongName_Element                         = variant!(186),
    LongName_NotElement                      = variant!(187),
    LongName_ReverseElement                  = variant!(188),
    LongName_NotReverseElement               = variant!(189),
    LongName_SuchThat                        = variant!(190),
    LongName_Product                         = variant!(191, PossibleBeginning),
    LongName_Coproduct                       = variant!(192, PossibleBeginning),
    LongName_Sum                             = variant!(193, PossibleBeginning),
    LongName_Minus                           = variant!(194, PossibleBeginning),
    LongName_MinusPlus                       = variant!(195, PossibleBeginning),
    LongName_DivisionSlash                   = variant!(196),
    LongName_Backslash                       = variant!(197),
    LongName_SmallCircle                     = variant!(198),
    LongName_Sqrt                            = variant!(199, PossibleBeginning),
    LongName_CubeRoot                        = variant!(200, PossibleBeginning),
    LongName_Proportional                    = variant!(201),
    LongName_Divides                         = variant!(202),
    LongName_DoubleVerticalBar               = variant!(203),
    LongName_NotDoubleVerticalBar            = variant!(204),
    LongName_And                             = variant!(205),
    LongName_Or                              = variant!(206),
    LongName_Integral                        = variant!(207, PossibleBeginning),
    LongName_ContourIntegral                 = variant!(208, PossibleBeginning),
    LongName_DoubleContourIntegral           = variant!(209, PossibleBeginning),
    LongName_ClockwiseContourIntegral        = variant!(210, PossibleBeginning),
    LongName_CounterClockwiseContourIntegral = variant!(211, PossibleBeginning),
    LongName_Therefore                       = variant!(212),
    LongName_Because                         = variant!(213),
    LongName_Colon                           = variant!(214),
    LongName_Proportion                      = variant!(215),
    LongName_Tilde                           = variant!(216),
    LongName_VerticalTilde                   = variant!(217),
    LongName_NotTilde                        = variant!(218),
    LongName_EqualTilde                      = variant!(219),
    LongName_TildeEqual                      = variant!(220),
    LongName_NotTildeEqual                   = variant!(221),
    LongName_TildeFullEqual                  = variant!(222),
    LongName_NotTildeFullEqual               = variant!(223),
    LongName_TildeTilde                      = variant!(224),
    LongName_NotTildeTilde                   = variant!(225),
    LongName_CupCap                          = variant!(226),
    LongName_HumpDownHump                    = variant!(227),
    LongName_HumpEqual                       = variant!(228),
    LongName_DotEqual                        = variant!(229),
    LongName_NotEqual                        = variant!(230),
    LongName_Congruent                       = variant!(231),
    LongName_NotCongruent                    = variant!(232),
    LongName_LessEqual                       = variant!(233),
    LongName_GreaterEqual                    = variant!(234),
    LongName_LessFullEqual                   = variant!(235),
    LongName_GreaterFullEqual                = variant!(236),
    LongName_NotLessFullEqual                = variant!(237),
    LongName_NotGreaterFullEqual             = variant!(238),
    LongName_LessLess                        = variant!(239),
    LongName_GreaterGreater                  = variant!(240),
    LongName_NotCupCap                       = variant!(241),
    LongName_NotLess                         = variant!(242),
    LongName_NotGreater                      = variant!(243),
    LongName_NotLessEqual                    = variant!(244),
    LongName_NotGreaterEqual                 = variant!(245),
    LongName_LessTilde                       = variant!(246),
    LongName_GreaterTilde                    = variant!(247),
    LongName_NotLessTilde                    = variant!(248),
    LongName_NotGreaterTilde                 = variant!(249),
    LongName_LessGreater                     = variant!(250),
    LongName_GreaterLess                     = variant!(251),
    LongName_NotLessGreater                  = variant!(252),
    LongName_NotGreaterLess                  = variant!(253),
    LongName_Precedes                        = variant!(254),
    LongName_Succeeds                        = variant!(255),
    LongName_PrecedesSlantEqual              = variant!(256),
    LongName_SucceedsSlantEqual              = variant!(257),
    LongName_PrecedesTilde                   = variant!(258),
    LongName_SucceedsTilde                   = variant!(259),
    LongName_NotPrecedes                     = variant!(260),
    LongName_NotSucceeds                     = variant!(261),
    LongName_Subset                          = variant!(262),
    LongName_Superset                        = variant!(263),
    LongName_NotSubset                       = variant!(264),
    LongName_NotSuperset                     = variant!(265),
    LongName_SubsetEqual                     = variant!(266),
    LongName_SupersetEqual                   = variant!(267),
    LongName_NotSubsetEqual                  = variant!(268),
    LongName_NotSupersetEqual                = variant!(269),
    LongName_UnionPlus                       = variant!(270),
    LongName_SquareSubset                    = variant!(271),
    LongName_SquareSuperset                  = variant!(272),
    LongName_SquareSubsetEqual               = variant!(273),
    LongName_SquareSupersetEqual             = variant!(274),
    LongName_SquareIntersection              = variant!(275),
    LongName_SquareUnion                     = variant!(276),
    LongName_CirclePlus                      = variant!(277),
    LongName_CircleMinus                     = variant!(278),
    LongName_CircleTimes                     = variant!(279, PossibleBeginning),
    LongName_CircleDot                       = variant!(280),
    LongName_RightTee                        = variant!(281),
    LongName_LeftTee                         = variant!(282),
    LongName_DownTee                         = variant!(283),
    LongName_UpTee                           = variant!(284),
    LongName_DoubleRightTee                  = variant!(285),
    LongName_LeftTriangle                    = variant!(286),
    LongName_RightTriangle                   = variant!(287),
    LongName_LeftTriangleEqual               = variant!(288),
    LongName_RightTriangleEqual              = variant!(289),
    LongName_Xor                             = variant!(290),
    LongName_Nand                            = variant!(291),
    LongName_Nor                             = variant!(292),
    LongName_Wedge                           = variant!(293),
    LongName_Vee                             = variant!(294),
    LongName_Intersection                    = variant!(295),
    LongName_Union                           = variant!(296),
    LongName_Diamond                         = variant!(297),
    LongName_Star                            = variant!(298),
    LongName_LessEqualGreater                = variant!(299),
    LongName_GreaterEqualLess                = variant!(300),
    LongName_NotPrecedesSlantEqual           = variant!(301),
    LongName_NotSucceedsSlantEqual           = variant!(302),
    LongName_NotSquareSubsetEqual            = variant!(303),
    LongName_NotSquareSupersetEqual          = variant!(304),
    LongName_NotPrecedesTilde                = variant!(305),
    LongName_NotSucceedsTilde                = variant!(306),
    LongName_NotLeftTriangle                 = variant!(307),
    LongName_NotRightTriangle                = variant!(308),
    LongName_NotLeftTriangleEqual            = variant!(309),
    LongName_NotRightTriangleEqual           = variant!(310),
    LongName_LeftCeiling                     = variant!(311, PossibleBeginning),
    LongName_RightCeiling                    = variant!(312, Closer),
    LongName_LeftFloor                       = variant!(313, PossibleBeginning),
    LongName_RightFloor                      = variant!(314, Closer),
    LongName_Cap                             = variant!(315),
    LongName_Cup                             = variant!(316),
    LongName_LeftAngleBracket                = variant!(317, PossibleBeginning),
    LongName_RightAngleBracket               = variant!(318, Closer),
    LongName_Perpendicular                   = variant!(319),
    LongName_LongLeftArrow                   = variant!(320),
    LongName_LongRightArrow                  = variant!(321),
    LongName_LongLeftRightArrow              = variant!(322),
    LongName_DoubleLongLeftArrow             = variant!(323),
    LongName_DoubleLongRightArrow            = variant!(324),
    LongName_DoubleLongLeftRightArrow        = variant!(325),
    LongName_UpArrowBar                      = variant!(326),
    LongName_DownArrowBar                    = variant!(327),
    LongName_LeftRightVector                 = variant!(328),
    LongName_RightUpDownVector               = variant!(329),
    LongName_DownLeftRightVector             = variant!(330),
    LongName_LeftUpDownVector                = variant!(331),
    LongName_LeftVectorBar                   = variant!(332),
    LongName_RightVectorBar                  = variant!(333),
    LongName_RightUpVectorBar                = variant!(334),
    LongName_RightDownVectorBar              = variant!(335),
    LongName_DownLeftVectorBar               = variant!(336),
    LongName_DownRightVectorBar              = variant!(337),
    LongName_LeftUpVectorBar                 = variant!(338),
    LongName_LeftDownVectorBar               = variant!(339),
    LongName_LeftTeeVector                   = variant!(340),
    LongName_RightTeeVector                  = variant!(341),
    LongName_RightUpTeeVector                = variant!(342),
    LongName_RightDownTeeVector              = variant!(343),
    LongName_DownLeftTeeVector               = variant!(344),
    LongName_DownRightTeeVector              = variant!(345),
    LongName_LeftUpTeeVector                 = variant!(346),
    LongName_LeftDownTeeVector               = variant!(347),
    LongName_UpEquilibrium                   = variant!(348),
    LongName_ReverseUpEquilibrium            = variant!(349),
    LongName_RoundImplies                    = variant!(350),
    LongName_LeftTriangleBar                 = variant!(351),
    LongName_RightTriangleBar                = variant!(352),
    LongName_Equivalent                      = variant!(353),
    LongName_LessSlantEqual                  = variant!(354),
    LongName_GreaterSlantEqual               = variant!(355),
    LongName_NestedLessLess                  = variant!(356),
    LongName_NestedGreaterGreater            = variant!(357),
    LongName_PrecedesEqual                   = variant!(358),
    LongName_SucceedsEqual                   = variant!(359),
    LongName_DoubleLeftTee                   = variant!(360),
    LongName_LeftDoubleBracket               = variant!(361),
    LongName_RightDoubleBracket              = variant!(362, Closer),
    LongName_LeftAssociation                 = variant!(363, PossibleBeginning),
    LongName_RightAssociation                = variant!(364, Closer),
    LongName_TwoWayRule                      = variant!(365),
    LongName_Piecewise                       = variant!(366, PossibleBeginning),
    LongName_ImplicitPlus                    = variant!(367),
    LongName_AutoLeftMatch                   = variant!(368),
    LongName_AutoRightMatch                  = variant!(369),
    LongName_InvisiblePrefixScriptBase       = variant!(370, PossibleBeginning),
    LongName_InvisiblePostfixScriptBase      = variant!(371),
    LongName_Transpose                       = variant!(372),
    LongName_Conjugate                       = variant!(373),
    LongName_ConjugateTranspose              = variant!(374),
    LongName_HermitianConjugate              = variant!(375),
    LongName_VerticalBar                     = variant!(376),
    LongName_NotVerticalBar                  = variant!(377),
    LongName_Distributed                     = variant!(378),
    LongName_Conditioned                     = variant!(379),
    LongName_UndirectedEdge                  = variant!(380),
    LongName_DirectedEdge                    = variant!(381),
    LongName_ContinuedFractionK              = variant!(382, PossibleBeginning),
    LongName_TensorProduct                   = variant!(383),
    LongName_TensorWedge                     = variant!(384),
    LongName_ProbabilityPr                   = variant!(385, PossibleBeginning),
    LongName_ExpectationE                    = variant!(386, PossibleBeginning),
    LongName_PermutationProduct              = variant!(387),
    LongName_NotEqualTilde                   = variant!(388),
    LongName_NotHumpEqual                    = variant!(389),
    LongName_NotHumpDownHump                 = variant!(390),
    LongName_NotLeftTriangleBar              = variant!(391),
    LongName_NotRightTriangleBar             = variant!(392),
    LongName_NotLessLess                     = variant!(393),
    LongName_NotNestedLessLess               = variant!(394),
    LongName_NotLessSlantEqual               = variant!(395),
    LongName_NotGreaterGreater               = variant!(396),
    LongName_NotNestedGreaterGreater         = variant!(397),
    LongName_NotGreaterSlantEqual            = variant!(398),
    LongName_NotPrecedesEqual                = variant!(399),
    LongName_NotSucceedsEqual                = variant!(400),
    LongName_NotSquareSubset                 = variant!(401),
    LongName_NotSquareSuperset               = variant!(402),
    LongName_Equal                           = variant!(403),
    LongName_VerticalSeparator               = variant!(404),
    LongName_VectorGreater                   = variant!(405),
    LongName_VectorGreaterEqual              = variant!(406),
    LongName_VectorLess                      = variant!(407),
    LongName_VectorLessEqual                 = variant!(408),
    LongName_Limit                           = variant!(409),
    LongName_MaxLimit                        = variant!(410),
    LongName_MinLimit                        = variant!(411),
    LongName_Cross                           = variant!(412),
    LongName_Function                        = variant!(413),
    LongName_Xnor                            = variant!(414),
    LongName_DiscreteShift                   = variant!(415),
    LongName_DifferenceDelta                 = variant!(416),
    LongName_DiscreteRatio                   = variant!(417),
    LongName_RuleDelayed                     = variant!(418),
    LongName_Square                          = variant!(419, PossibleBeginning),
    LongName_Rule                            = variant!(420),
    LongName_Implies                         = variant!(421),
    LongName_ShortRightArrow                 = variant!(422),
    LongName_ShortLeftArrow                  = variant!(423),
    LongName_ShortUpArrow                    = variant!(424),
    LongName_ShortDownArrow                  = variant!(425),
    LongName_Application                     = variant!(426),
    LongName_LeftBracketingBar               = variant!(427, PossibleBeginning),
    LongName_RightBracketingBar              = variant!(428, Closer),
    LongName_LeftDoubleBracketingBar         = variant!(429, PossibleBeginning),
    LongName_RightDoubleBracketingBar        = variant!(430, Closer),
    LongName_CapitalDifferentialD            = variant!(431, PossibleBeginning),
    LongName_DifferentialD                   = variant!(432, PossibleBeginning),
    LongName_InvisibleComma                  = variant!(433),
    LongName_InvisibleApplication            = variant!(434),
    LongName_LongEqual                       = variant!(435),
}

impl TokenKind {
	pub const COUNT: usize = 436;
}

//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

use crate::symbols as st;

//
// TokenKind::Integer must be 0x4 to allow setting the 0b1 bit to convert to TokenKind::REAL, and 0b10 bit to convert to TokenKind::Rational
//
const _: () = assert!(TokenKind::Integer.id() == 0x4, "Check your assumptions");
const _: () = assert!(TokenKind::Real.id() == 0x5, "Check your assumptions");
const _: () = assert!(TokenKind::Rational.id() == 0x6, "Check your assumptions");

//
// TokenKind::InternalNewline must be 0x8 to allow setting the 0b100 bit to convert to TokenKind::ToplevelNewline
//
const _: () = assert!(TokenKind::InternalNewline.id() == 0b1000, "Check your assumptions");
const _: () = assert!(TokenKind::ToplevelNewline.id() == 0b1100, "Check your assumptions");
//const _: () = assert!(TokenKind::Error_First.id() == 0x10, "Check your assumptions");


#[allow(dead_code)]
pub fn TokenToSymbol(token: TokenKind) -> Symbol {
    use TokenKind::*;
    match token {
        Unknown => return st::Token_Unknown,
        EndOfFile => return st::EndOfFile,
        Symbol => return st::Symbol,
        String => return st::String,
        Integer => return st::Integer,
        Real => return st::Real,
        Rational => return st::Rational,
        LinearSyntaxBlob => return st::Token_LinearSyntaxBlob,
        InternalNewline => return st::Token_Newline,
        Comment => return st::Token_Comment,
        Whitespace => return st::Whitespace,
        Buffer1 => return st::Token_Buffer1,
        ToplevelNewline => return st::Token_Newline,
        Buffer2 => return st::Token_Buffer2,
        Buffer3 => return st::Token_Buffer3,
        Buffer4 => return st::Token_Buffer4,
        Error_ExpectedEqual => return st::Token_Error_ExpectedEqual,
        Error_Number => return st::Token_Error_Number,
        Error_UnhandledCharacter => return st::Token_Error_UnhandledCharacter,
        Error_ExpectedLetterlike => return st::Token_Error_ExpectedLetterlike,
        Error_Aborted => return st::Token_Error_Aborted,
        Error_ExpectedOperand => return st::Token_Error_ExpectedOperand,
        Error_ExpectedTag => return st::Token_Error_ExpectedTag,
        Error_ExpectedFile => return st::Token_Error_ExpectedFile,
        Error_UnexpectedCloser => return st::Token_Error_UnexpectedCloser,
        Error_PrefixImplicitNull => return st::Token_Error_PrefixImplicitNull,
        Error_InfixImplicitNull => return st::Token_Error_InfixImplicitNull,
        Error_UnsafeCharacterEncoding => return st::Token_Error_UnsafeCharacterEncoding,
        Error_UnterminatedComment => return st::Token_Error_UnterminatedComment,
        Error_UnterminatedString => return st::Token_Error_UnterminatedString,
        Error_UnterminatedFileString => return st::Token_Error_UnterminatedFileString,
        Error_UnterminatedLinearSyntaxBlob => return st::Token_Error_UnterminatedLinearSyntaxBlob,
        Error_UnsupportedToken => return st::Token_Error_UnsupportedToken,
        Error_UnexpectedCommentCloser => return st::Token_Error_UnexpectedCommentCloser,
        Dot => return st::Token_Dot,
        Colon => return st::Token_Colon,
        OpenParen => return st::Token_OpenParen,
        CloseParen => return st::Token_CloseParen,
        OpenSquare => return st::Token_OpenSquare,
        CloseSquare => return st::Token_CloseSquare,
        Comma => return st::Token_Comma,
        OpenCurly => return st::Token_OpenCurly,
        CloseCurly => return st::Token_CloseCurly,
        Equal => return st::Token_Equal,
        Bang => return st::Token_Bang,
        Under => return st::Token_Under,
        Less => return st::Token_Less,
        Greater => return st::Token_Greater,
        Minus => return st::Token_Minus,
        Bar => return st::Token_Bar,
        Semi => return st::Token_Semi,
        Hash => return st::Token_Hash,
        Amp => return st::Token_Amp,
        Slash => return st::Token_Slash,
        At => return st::Token_At,
        Plus => return st::Token_Plus,
        Tilde => return st::Token_Tilde,
        Star => return st::Token_Star,
        Caret => return st::Token_Caret,
        SingleQuote => return st::Token_SingleQuote,
        Percent => return st::Token_Percent,
        Question => return st::Token_Question,
        DotDot => return st::Token_DotDot,
        ColonColon => return st::Token_ColonColon,
        ColonEqual => return st::Token_ColonEqual,
        ColonGreater => return st::Token_ColonGreater,
        EqualEqual => return st::Token_EqualEqual,
        UnderUnder => return st::Token_UnderUnder,
        UnderDot => return st::Token_UnderDot,
        LessBar => return st::Token_LessBar,
        LessLess => return st::Token_LessLess,
        LessGreater => return st::Token_LessGreater,
        LessEqual => return st::Token_LessEqual,
        GreaterGreater => return st::Token_GreaterGreater,
        GreaterEqual => return st::Token_GreaterEqual,
        MinusGreater => return st::Token_MinusGreater,
        MinusMinus => return st::Token_MinusMinus,
        MinusEqual => return st::Token_MinusEqual,
        BarBar => return st::Token_BarBar,
        BarGreater => return st::Token_BarGreater,
        SemiSemi => return st::Token_SemiSemi,
        AmpAmp => return st::Token_AmpAmp,
        SlashAt => return st::Token_SlashAt,
        SlashSemi => return st::Token_SlashSemi,
        SlashDot => return st::Token_SlashDot,
        SlashSlash => return st::Token_SlashSlash,
        SlashColon => return st::Token_SlashColon,
        SlashEqual => return st::Token_SlashEqual,
        SlashStar => return st::Token_SlashStar,
        AtAt => return st::Token_AtAt,
        AtStar => return st::Token_AtStar,
        PlusPlus => return st::Token_PlusPlus,
        PlusEqual => return st::Token_PlusEqual,
        TildeTilde => return st::Token_TildeTilde,
        StarEqual => return st::Token_StarEqual,
        StarStar => return st::Token_StarStar,
        CaretEqual => return st::Token_CaretEqual,
        HashHash => return st::Token_HashHash,
        BangEqual => return st::Token_BangEqual,
        BangBang => return st::Token_BangBang,
        QuestionQuestion => return st::Token_QuestionQuestion,
        DotDotDot => return st::Token_DotDotDot,
        EqualEqualEqual => return st::Token_EqualEqualEqual,
        EqualBangEqual => return st::Token_EqualBangEqual,
        UnderUnderUnder => return st::Token_UnderUnderUnder,
        SlashSlashDot => return st::Token_SlashSlashDot,
        AtAtAt => return st::Token_AtAtAt,
        LessMinusGreater => return st::Token_LessMinusGreater,
        SlashSlashAt => return st::Token_SlashSlashAt,
        CaretColonEqual => return st::Token_CaretColonEqual,
        GreaterGreaterGreater => return st::Token_GreaterGreaterGreater,
        BarMinusGreater => return st::Token_BarMinusGreater,
        SlashSlashEqual => return st::Token_SlashSlashEqual,
        ColonColonOpenSquare => return st::Token_ColonColonOpenSquare,
        PercentPercent => return st::Token_PercentPercent,
        LinearSyntax_Bang => return st::Token_LinearSyntax_Bang,
        LinearSyntax_CloseParen => return st::Token_LinearSyntax_CloseParen,
        LinearSyntax_At => return st::Token_LinearSyntax_At,
        LinearSyntax_Amp => return st::Token_LinearSyntax_Amp,
        LinearSyntax_Star => return st::Token_LinearSyntax_Star,
        LinearSyntax_Under => return st::Token_LinearSyntax_Under,
        LinearSyntax_Caret => return st::Token_LinearSyntax_Caret,
        LinearSyntax_Space => return st::Token_LinearSyntax_Space,
        LinearSyntax_Percent => return st::Token_LinearSyntax_Percent,
        LinearSyntax_Plus => return st::Token_LinearSyntax_Plus,
        LinearSyntax_Slash => return st::Token_LinearSyntax_Slash,
        LinearSyntax_BackTick => return st::Token_LinearSyntax_BackTick,
        Fake_ImplicitTimes => return st::Token_Fake_ImplicitTimes,
        Fake_ImplicitNull => return st::Token_Fake_ImplicitNull,
        Fake_ImplicitOne => return st::Token_Fake_ImplicitOne,
        Fake_ImplicitAll => return st::Token_Fake_ImplicitAll,
        Boxes_OpenParenStar => return st::Token_Boxes_OpenParenStar,
        Boxes_StarCloseParen => return st::Token_Boxes_StarCloseParen,
        Boxes_MultiSingleQuote => return st::Token_Boxes_MultiSingleQuote,
        Boxes_MultiWhitespace => return st::Token_Boxes_MultiWhitespace,
        LongName_Not => return st::Token_LongName_Not,
        LongName_PlusMinus => return st::Token_LongName_PlusMinus,
        LongName_CenterDot => return st::Token_LongName_CenterDot,
        LongName_Times => return st::Token_LongName_Times,
        LongName_Divide => return st::Token_LongName_Divide,
        LongName_OpenCurlyQuote => return st::Token_LongName_OpenCurlyQuote,
        LongName_CloseCurlyQuote => return st::Token_LongName_CloseCurlyQuote,
        LongName_OpenCurlyDoubleQuote => return st::Token_LongName_OpenCurlyDoubleQuote,
        LongName_CloseCurlyDoubleQuote => return st::Token_LongName_CloseCurlyDoubleQuote,
        LongName_InvisibleTimes => return st::Token_LongName_InvisibleTimes,
        LongName_LeftArrow => return st::Token_LongName_LeftArrow,
        LongName_UpArrow => return st::Token_LongName_UpArrow,
        LongName_RightArrow => return st::Token_LongName_RightArrow,
        LongName_DownArrow => return st::Token_LongName_DownArrow,
        LongName_LeftRightArrow => return st::Token_LongName_LeftRightArrow,
        LongName_UpDownArrow => return st::Token_LongName_UpDownArrow,
        LongName_UpperLeftArrow => return st::Token_LongName_UpperLeftArrow,
        LongName_UpperRightArrow => return st::Token_LongName_UpperRightArrow,
        LongName_LowerRightArrow => return st::Token_LongName_LowerRightArrow,
        LongName_LowerLeftArrow => return st::Token_LongName_LowerLeftArrow,
        LongName_LeftTeeArrow => return st::Token_LongName_LeftTeeArrow,
        LongName_UpTeeArrow => return st::Token_LongName_UpTeeArrow,
        LongName_RightTeeArrow => return st::Token_LongName_RightTeeArrow,
        LongName_DownTeeArrow => return st::Token_LongName_DownTeeArrow,
        LongName_LeftVector => return st::Token_LongName_LeftVector,
        LongName_DownLeftVector => return st::Token_LongName_DownLeftVector,
        LongName_RightUpVector => return st::Token_LongName_RightUpVector,
        LongName_LeftUpVector => return st::Token_LongName_LeftUpVector,
        LongName_RightVector => return st::Token_LongName_RightVector,
        LongName_DownRightVector => return st::Token_LongName_DownRightVector,
        LongName_RightDownVector => return st::Token_LongName_RightDownVector,
        LongName_LeftDownVector => return st::Token_LongName_LeftDownVector,
        LongName_RightArrowLeftArrow => return st::Token_LongName_RightArrowLeftArrow,
        LongName_UpArrowDownArrow => return st::Token_LongName_UpArrowDownArrow,
        LongName_LeftArrowRightArrow => return st::Token_LongName_LeftArrowRightArrow,
        LongName_ReverseEquilibrium => return st::Token_LongName_ReverseEquilibrium,
        LongName_Equilibrium => return st::Token_LongName_Equilibrium,
        LongName_DoubleLeftArrow => return st::Token_LongName_DoubleLeftArrow,
        LongName_DoubleUpArrow => return st::Token_LongName_DoubleUpArrow,
        LongName_DoubleRightArrow => return st::Token_LongName_DoubleRightArrow,
        LongName_DoubleDownArrow => return st::Token_LongName_DoubleDownArrow,
        LongName_DoubleLeftRightArrow => return st::Token_LongName_DoubleLeftRightArrow,
        LongName_DoubleUpDownArrow => return st::Token_LongName_DoubleUpDownArrow,
        LongName_LeftArrowBar => return st::Token_LongName_LeftArrowBar,
        LongName_RightArrowBar => return st::Token_LongName_RightArrowBar,
        LongName_DownArrowUpArrow => return st::Token_LongName_DownArrowUpArrow,
        LongName_ForAll => return st::Token_LongName_ForAll,
        LongName_PartialD => return st::Token_LongName_PartialD,
        LongName_Exists => return st::Token_LongName_Exists,
        LongName_NotExists => return st::Token_LongName_NotExists,
        LongName_Del => return st::Token_LongName_Del,
        LongName_Element => return st::Token_LongName_Element,
        LongName_NotElement => return st::Token_LongName_NotElement,
        LongName_ReverseElement => return st::Token_LongName_ReverseElement,
        LongName_NotReverseElement => return st::Token_LongName_NotReverseElement,
        LongName_SuchThat => return st::Token_LongName_SuchThat,
        LongName_Product => return st::Token_LongName_Product,
        LongName_Coproduct => return st::Token_LongName_Coproduct,
        LongName_Sum => return st::Token_LongName_Sum,
        LongName_Minus => return st::Token_LongName_Minus,
        LongName_MinusPlus => return st::Token_LongName_MinusPlus,
        LongName_DivisionSlash => return st::Token_LongName_DivisionSlash,
        LongName_Backslash => return st::Token_LongName_Backslash,
        LongName_SmallCircle => return st::Token_LongName_SmallCircle,
        LongName_Sqrt => return st::Token_LongName_Sqrt,
        LongName_CubeRoot => return st::Token_LongName_CubeRoot,
        LongName_Proportional => return st::Token_LongName_Proportional,
        LongName_Divides => return st::Token_LongName_Divides,
        LongName_DoubleVerticalBar => return st::Token_LongName_DoubleVerticalBar,
        LongName_NotDoubleVerticalBar => return st::Token_LongName_NotDoubleVerticalBar,
        LongName_And => return st::Token_LongName_And,
        LongName_Or => return st::Token_LongName_Or,
        LongName_Integral => return st::Token_LongName_Integral,
        LongName_ContourIntegral => return st::Token_LongName_ContourIntegral,
        LongName_DoubleContourIntegral => return st::Token_LongName_DoubleContourIntegral,
        LongName_ClockwiseContourIntegral => return st::Token_LongName_ClockwiseContourIntegral,
        LongName_CounterClockwiseContourIntegral => return st::Token_LongName_CounterClockwiseContourIntegral,
        LongName_Therefore => return st::Token_LongName_Therefore,
        LongName_Because => return st::Token_LongName_Because,
        LongName_Colon => return st::Token_LongName_Colon,
        LongName_Proportion => return st::Token_LongName_Proportion,
        LongName_Tilde => return st::Token_LongName_Tilde,
        LongName_VerticalTilde => return st::Token_LongName_VerticalTilde,
        LongName_NotTilde => return st::Token_LongName_NotTilde,
        LongName_EqualTilde => return st::Token_LongName_EqualTilde,
        LongName_TildeEqual => return st::Token_LongName_TildeEqual,
        LongName_NotTildeEqual => return st::Token_LongName_NotTildeEqual,
        LongName_TildeFullEqual => return st::Token_LongName_TildeFullEqual,
        LongName_NotTildeFullEqual => return st::Token_LongName_NotTildeFullEqual,
        LongName_TildeTilde => return st::Token_LongName_TildeTilde,
        LongName_NotTildeTilde => return st::Token_LongName_NotTildeTilde,
        LongName_CupCap => return st::Token_LongName_CupCap,
        LongName_HumpDownHump => return st::Token_LongName_HumpDownHump,
        LongName_HumpEqual => return st::Token_LongName_HumpEqual,
        LongName_DotEqual => return st::Token_LongName_DotEqual,
        LongName_NotEqual => return st::Token_LongName_NotEqual,
        LongName_Congruent => return st::Token_LongName_Congruent,
        LongName_NotCongruent => return st::Token_LongName_NotCongruent,
        LongName_LessEqual => return st::Token_LongName_LessEqual,
        LongName_GreaterEqual => return st::Token_LongName_GreaterEqual,
        LongName_LessFullEqual => return st::Token_LongName_LessFullEqual,
        LongName_GreaterFullEqual => return st::Token_LongName_GreaterFullEqual,
        LongName_NotLessFullEqual => return st::Token_LongName_NotLessFullEqual,
        LongName_NotGreaterFullEqual => return st::Token_LongName_NotGreaterFullEqual,
        LongName_LessLess => return st::Token_LongName_LessLess,
        LongName_GreaterGreater => return st::Token_LongName_GreaterGreater,
        LongName_NotCupCap => return st::Token_LongName_NotCupCap,
        LongName_NotLess => return st::Token_LongName_NotLess,
        LongName_NotGreater => return st::Token_LongName_NotGreater,
        LongName_NotLessEqual => return st::Token_LongName_NotLessEqual,
        LongName_NotGreaterEqual => return st::Token_LongName_NotGreaterEqual,
        LongName_LessTilde => return st::Token_LongName_LessTilde,
        LongName_GreaterTilde => return st::Token_LongName_GreaterTilde,
        LongName_NotLessTilde => return st::Token_LongName_NotLessTilde,
        LongName_NotGreaterTilde => return st::Token_LongName_NotGreaterTilde,
        LongName_LessGreater => return st::Token_LongName_LessGreater,
        LongName_GreaterLess => return st::Token_LongName_GreaterLess,
        LongName_NotLessGreater => return st::Token_LongName_NotLessGreater,
        LongName_NotGreaterLess => return st::Token_LongName_NotGreaterLess,
        LongName_Precedes => return st::Token_LongName_Precedes,
        LongName_Succeeds => return st::Token_LongName_Succeeds,
        LongName_PrecedesSlantEqual => return st::Token_LongName_PrecedesSlantEqual,
        LongName_SucceedsSlantEqual => return st::Token_LongName_SucceedsSlantEqual,
        LongName_PrecedesTilde => return st::Token_LongName_PrecedesTilde,
        LongName_SucceedsTilde => return st::Token_LongName_SucceedsTilde,
        LongName_NotPrecedes => return st::Token_LongName_NotPrecedes,
        LongName_NotSucceeds => return st::Token_LongName_NotSucceeds,
        LongName_Subset => return st::Token_LongName_Subset,
        LongName_Superset => return st::Token_LongName_Superset,
        LongName_NotSubset => return st::Token_LongName_NotSubset,
        LongName_NotSuperset => return st::Token_LongName_NotSuperset,
        LongName_SubsetEqual => return st::Token_LongName_SubsetEqual,
        LongName_SupersetEqual => return st::Token_LongName_SupersetEqual,
        LongName_NotSubsetEqual => return st::Token_LongName_NotSubsetEqual,
        LongName_NotSupersetEqual => return st::Token_LongName_NotSupersetEqual,
        LongName_UnionPlus => return st::Token_LongName_UnionPlus,
        LongName_SquareSubset => return st::Token_LongName_SquareSubset,
        LongName_SquareSuperset => return st::Token_LongName_SquareSuperset,
        LongName_SquareSubsetEqual => return st::Token_LongName_SquareSubsetEqual,
        LongName_SquareSupersetEqual => return st::Token_LongName_SquareSupersetEqual,
        LongName_SquareIntersection => return st::Token_LongName_SquareIntersection,
        LongName_SquareUnion => return st::Token_LongName_SquareUnion,
        LongName_CirclePlus => return st::Token_LongName_CirclePlus,
        LongName_CircleMinus => return st::Token_LongName_CircleMinus,
        LongName_CircleTimes => return st::Token_LongName_CircleTimes,
        LongName_CircleDot => return st::Token_LongName_CircleDot,
        LongName_RightTee => return st::Token_LongName_RightTee,
        LongName_LeftTee => return st::Token_LongName_LeftTee,
        LongName_DownTee => return st::Token_LongName_DownTee,
        LongName_UpTee => return st::Token_LongName_UpTee,
        LongName_DoubleRightTee => return st::Token_LongName_DoubleRightTee,
        LongName_LeftTriangle => return st::Token_LongName_LeftTriangle,
        LongName_RightTriangle => return st::Token_LongName_RightTriangle,
        LongName_LeftTriangleEqual => return st::Token_LongName_LeftTriangleEqual,
        LongName_RightTriangleEqual => return st::Token_LongName_RightTriangleEqual,
        LongName_Xor => return st::Token_LongName_Xor,
        LongName_Nand => return st::Token_LongName_Nand,
        LongName_Nor => return st::Token_LongName_Nor,
        LongName_Wedge => return st::Token_LongName_Wedge,
        LongName_Vee => return st::Token_LongName_Vee,
        LongName_Intersection => return st::Token_LongName_Intersection,
        LongName_Union => return st::Token_LongName_Union,
        LongName_Diamond => return st::Token_LongName_Diamond,
        LongName_Star => return st::Token_LongName_Star,
        LongName_LessEqualGreater => return st::Token_LongName_LessEqualGreater,
        LongName_GreaterEqualLess => return st::Token_LongName_GreaterEqualLess,
        LongName_NotPrecedesSlantEqual => return st::Token_LongName_NotPrecedesSlantEqual,
        LongName_NotSucceedsSlantEqual => return st::Token_LongName_NotSucceedsSlantEqual,
        LongName_NotSquareSubsetEqual => return st::Token_LongName_NotSquareSubsetEqual,
        LongName_NotSquareSupersetEqual => return st::Token_LongName_NotSquareSupersetEqual,
        LongName_NotPrecedesTilde => return st::Token_LongName_NotPrecedesTilde,
        LongName_NotSucceedsTilde => return st::Token_LongName_NotSucceedsTilde,
        LongName_NotLeftTriangle => return st::Token_LongName_NotLeftTriangle,
        LongName_NotRightTriangle => return st::Token_LongName_NotRightTriangle,
        LongName_NotLeftTriangleEqual => return st::Token_LongName_NotLeftTriangleEqual,
        LongName_NotRightTriangleEqual => return st::Token_LongName_NotRightTriangleEqual,
        LongName_LeftCeiling => return st::Token_LongName_LeftCeiling,
        LongName_RightCeiling => return st::Token_LongName_RightCeiling,
        LongName_LeftFloor => return st::Token_LongName_LeftFloor,
        LongName_RightFloor => return st::Token_LongName_RightFloor,
        LongName_Cap => return st::Token_LongName_Cap,
        LongName_Cup => return st::Token_LongName_Cup,
        LongName_LeftAngleBracket => return st::Token_LongName_LeftAngleBracket,
        LongName_RightAngleBracket => return st::Token_LongName_RightAngleBracket,
        LongName_Perpendicular => return st::Token_LongName_Perpendicular,
        LongName_LongLeftArrow => return st::Token_LongName_LongLeftArrow,
        LongName_LongRightArrow => return st::Token_LongName_LongRightArrow,
        LongName_LongLeftRightArrow => return st::Token_LongName_LongLeftRightArrow,
        LongName_DoubleLongLeftArrow => return st::Token_LongName_DoubleLongLeftArrow,
        LongName_DoubleLongRightArrow => return st::Token_LongName_DoubleLongRightArrow,
        LongName_DoubleLongLeftRightArrow => return st::Token_LongName_DoubleLongLeftRightArrow,
        LongName_UpArrowBar => return st::Token_LongName_UpArrowBar,
        LongName_DownArrowBar => return st::Token_LongName_DownArrowBar,
        LongName_LeftRightVector => return st::Token_LongName_LeftRightVector,
        LongName_RightUpDownVector => return st::Token_LongName_RightUpDownVector,
        LongName_DownLeftRightVector => return st::Token_LongName_DownLeftRightVector,
        LongName_LeftUpDownVector => return st::Token_LongName_LeftUpDownVector,
        LongName_LeftVectorBar => return st::Token_LongName_LeftVectorBar,
        LongName_RightVectorBar => return st::Token_LongName_RightVectorBar,
        LongName_RightUpVectorBar => return st::Token_LongName_RightUpVectorBar,
        LongName_RightDownVectorBar => return st::Token_LongName_RightDownVectorBar,
        LongName_DownLeftVectorBar => return st::Token_LongName_DownLeftVectorBar,
        LongName_DownRightVectorBar => return st::Token_LongName_DownRightVectorBar,
        LongName_LeftUpVectorBar => return st::Token_LongName_LeftUpVectorBar,
        LongName_LeftDownVectorBar => return st::Token_LongName_LeftDownVectorBar,
        LongName_LeftTeeVector => return st::Token_LongName_LeftTeeVector,
        LongName_RightTeeVector => return st::Token_LongName_RightTeeVector,
        LongName_RightUpTeeVector => return st::Token_LongName_RightUpTeeVector,
        LongName_RightDownTeeVector => return st::Token_LongName_RightDownTeeVector,
        LongName_DownLeftTeeVector => return st::Token_LongName_DownLeftTeeVector,
        LongName_DownRightTeeVector => return st::Token_LongName_DownRightTeeVector,
        LongName_LeftUpTeeVector => return st::Token_LongName_LeftUpTeeVector,
        LongName_LeftDownTeeVector => return st::Token_LongName_LeftDownTeeVector,
        LongName_UpEquilibrium => return st::Token_LongName_UpEquilibrium,
        LongName_ReverseUpEquilibrium => return st::Token_LongName_ReverseUpEquilibrium,
        LongName_RoundImplies => return st::Token_LongName_RoundImplies,
        LongName_LeftTriangleBar => return st::Token_LongName_LeftTriangleBar,
        LongName_RightTriangleBar => return st::Token_LongName_RightTriangleBar,
        LongName_Equivalent => return st::Token_LongName_Equivalent,
        LongName_LessSlantEqual => return st::Token_LongName_LessSlantEqual,
        LongName_GreaterSlantEqual => return st::Token_LongName_GreaterSlantEqual,
        LongName_NestedLessLess => return st::Token_LongName_NestedLessLess,
        LongName_NestedGreaterGreater => return st::Token_LongName_NestedGreaterGreater,
        LongName_PrecedesEqual => return st::Token_LongName_PrecedesEqual,
        LongName_SucceedsEqual => return st::Token_LongName_SucceedsEqual,
        LongName_DoubleLeftTee => return st::Token_LongName_DoubleLeftTee,
        LongName_LeftDoubleBracket => return st::Token_LongName_LeftDoubleBracket,
        LongName_RightDoubleBracket => return st::Token_LongName_RightDoubleBracket,
        LongName_LeftAssociation => return st::Token_LongName_LeftAssociation,
        LongName_RightAssociation => return st::Token_LongName_RightAssociation,
        LongName_TwoWayRule => return st::Token_LongName_TwoWayRule,
        LongName_Piecewise => return st::Token_LongName_Piecewise,
        LongName_ImplicitPlus => return st::Token_LongName_ImplicitPlus,
        LongName_AutoLeftMatch => return st::Token_LongName_AutoLeftMatch,
        LongName_AutoRightMatch => return st::Token_LongName_AutoRightMatch,
        LongName_InvisiblePrefixScriptBase => return st::Token_LongName_InvisiblePrefixScriptBase,
        LongName_InvisiblePostfixScriptBase => return st::Token_LongName_InvisiblePostfixScriptBase,
        LongName_Transpose => return st::Token_LongName_Transpose,
        LongName_Conjugate => return st::Token_LongName_Conjugate,
        LongName_ConjugateTranspose => return st::Token_LongName_ConjugateTranspose,
        LongName_HermitianConjugate => return st::Token_LongName_HermitianConjugate,
        LongName_VerticalBar => return st::Token_LongName_VerticalBar,
        LongName_NotVerticalBar => return st::Token_LongName_NotVerticalBar,
        LongName_Distributed => return st::Token_LongName_Distributed,
        LongName_Conditioned => return st::Token_LongName_Conditioned,
        LongName_UndirectedEdge => return st::Token_LongName_UndirectedEdge,
        LongName_DirectedEdge => return st::Token_LongName_DirectedEdge,
        LongName_ContinuedFractionK => return st::Token_LongName_ContinuedFractionK,
        LongName_TensorProduct => return st::Token_LongName_TensorProduct,
        LongName_TensorWedge => return st::Token_LongName_TensorWedge,
        LongName_ProbabilityPr => return st::Token_LongName_ProbabilityPr,
        LongName_ExpectationE => return st::Token_LongName_ExpectationE,
        LongName_PermutationProduct => return st::Token_LongName_PermutationProduct,
        LongName_NotEqualTilde => return st::Token_LongName_NotEqualTilde,
        LongName_NotHumpEqual => return st::Token_LongName_NotHumpEqual,
        LongName_NotHumpDownHump => return st::Token_LongName_NotHumpDownHump,
        LongName_NotLeftTriangleBar => return st::Token_LongName_NotLeftTriangleBar,
        LongName_NotRightTriangleBar => return st::Token_LongName_NotRightTriangleBar,
        LongName_NotLessLess => return st::Token_LongName_NotLessLess,
        LongName_NotNestedLessLess => return st::Token_LongName_NotNestedLessLess,
        LongName_NotLessSlantEqual => return st::Token_LongName_NotLessSlantEqual,
        LongName_NotGreaterGreater => return st::Token_LongName_NotGreaterGreater,
        LongName_NotNestedGreaterGreater => return st::Token_LongName_NotNestedGreaterGreater,
        LongName_NotGreaterSlantEqual => return st::Token_LongName_NotGreaterSlantEqual,
        LongName_NotPrecedesEqual => return st::Token_LongName_NotPrecedesEqual,
        LongName_NotSucceedsEqual => return st::Token_LongName_NotSucceedsEqual,
        LongName_NotSquareSubset => return st::Token_LongName_NotSquareSubset,
        LongName_NotSquareSuperset => return st::Token_LongName_NotSquareSuperset,
        LongName_Equal => return st::Token_LongName_Equal,
        LongName_VerticalSeparator => return st::Token_LongName_VerticalSeparator,
        LongName_VectorGreater => return st::Token_LongName_VectorGreater,
        LongName_VectorGreaterEqual => return st::Token_LongName_VectorGreaterEqual,
        LongName_VectorLess => return st::Token_LongName_VectorLess,
        LongName_VectorLessEqual => return st::Token_LongName_VectorLessEqual,
        LongName_Limit => return st::Token_LongName_Limit,
        LongName_MaxLimit => return st::Token_LongName_MaxLimit,
        LongName_MinLimit => return st::Token_LongName_MinLimit,
        LongName_Cross => return st::Token_LongName_Cross,
        LongName_Function => return st::Token_LongName_Function,
        LongName_Xnor => return st::Token_LongName_Xnor,
        LongName_DiscreteShift => return st::Token_LongName_DiscreteShift,
        LongName_DifferenceDelta => return st::Token_LongName_DifferenceDelta,
        LongName_DiscreteRatio => return st::Token_LongName_DiscreteRatio,
        LongName_RuleDelayed => return st::Token_LongName_RuleDelayed,
        LongName_Square => return st::Token_LongName_Square,
        LongName_Rule => return st::Token_LongName_Rule,
        LongName_Implies => return st::Token_LongName_Implies,
        LongName_ShortRightArrow => return st::Token_LongName_ShortRightArrow,
        LongName_ShortLeftArrow => return st::Token_LongName_ShortLeftArrow,
        LongName_ShortUpArrow => return st::Token_LongName_ShortUpArrow,
        LongName_ShortDownArrow => return st::Token_LongName_ShortDownArrow,
        LongName_Application => return st::Token_LongName_Application,
        LongName_LeftBracketingBar => return st::Token_LongName_LeftBracketingBar,
        LongName_RightBracketingBar => return st::Token_LongName_RightBracketingBar,
        LongName_LeftDoubleBracketingBar => return st::Token_LongName_LeftDoubleBracketingBar,
        LongName_RightDoubleBracketingBar => return st::Token_LongName_RightDoubleBracketingBar,
        LongName_CapitalDifferentialD => return st::Token_LongName_CapitalDifferentialD,
        LongName_DifferentialD => return st::Token_LongName_DifferentialD,
        LongName_InvisibleComma => return st::Token_LongName_InvisibleComma,
        LongName_InvisibleApplication => return st::Token_LongName_InvisibleApplication,
        LongName_LongEqual => return st::Token_LongName_LongEqual,
    }
}

pub fn SymbolToToken(symbol: SymbolRef) -> Option<TokenKind> {
    use TokenKind::*;
    let token = match symbol {
        st::Token_Unknown => Unknown,
        st::EndOfFile => EndOfFile,
        st::Symbol => Symbol,
        st::String => String,
        st::Integer => Integer,
        st::Real => Real,
        st::Rational => Rational,
        st::Token_LinearSyntaxBlob => LinearSyntaxBlob,
        st::Token_Newline => InternalNewline,
        st::Token_Comment => Comment,
        st::Whitespace => Whitespace,
        st::Token_Buffer1 => Buffer1,
        st::Token_Newline => ToplevelNewline,
        st::Token_Buffer2 => Buffer2,
        st::Token_Buffer3 => Buffer3,
        st::Token_Buffer4 => Buffer4,
        st::Token_Error_ExpectedEqual => Error_ExpectedEqual,
        st::Token_Error_Number => Error_Number,
        st::Token_Error_UnhandledCharacter => Error_UnhandledCharacter,
        st::Token_Error_ExpectedLetterlike => Error_ExpectedLetterlike,
        st::Token_Error_Aborted => Error_Aborted,
        st::Token_Error_ExpectedOperand => Error_ExpectedOperand,
        st::Token_Error_ExpectedTag => Error_ExpectedTag,
        st::Token_Error_ExpectedFile => Error_ExpectedFile,
        st::Token_Error_UnexpectedCloser => Error_UnexpectedCloser,
        st::Token_Error_PrefixImplicitNull => Error_PrefixImplicitNull,
        st::Token_Error_InfixImplicitNull => Error_InfixImplicitNull,
        st::Token_Error_UnsafeCharacterEncoding => Error_UnsafeCharacterEncoding,
        st::Token_Error_UnterminatedComment => Error_UnterminatedComment,
        st::Token_Error_UnterminatedString => Error_UnterminatedString,
        st::Token_Error_UnterminatedFileString => Error_UnterminatedFileString,
        st::Token_Error_UnterminatedLinearSyntaxBlob => Error_UnterminatedLinearSyntaxBlob,
        st::Token_Error_UnsupportedToken => Error_UnsupportedToken,
        st::Token_Error_UnexpectedCommentCloser => Error_UnexpectedCommentCloser,
        st::Token_Dot => Dot,
        st::Token_Colon => Colon,
        st::Token_OpenParen => OpenParen,
        st::Token_CloseParen => CloseParen,
        st::Token_OpenSquare => OpenSquare,
        st::Token_CloseSquare => CloseSquare,
        st::Token_Comma => Comma,
        st::Token_OpenCurly => OpenCurly,
        st::Token_CloseCurly => CloseCurly,
        st::Token_Equal => Equal,
        st::Token_Bang => Bang,
        st::Token_Under => Under,
        st::Token_Less => Less,
        st::Token_Greater => Greater,
        st::Token_Minus => Minus,
        st::Token_Bar => Bar,
        st::Token_Semi => Semi,
        st::Token_Hash => Hash,
        st::Token_Amp => Amp,
        st::Token_Slash => Slash,
        st::Token_At => At,
        st::Token_Plus => Plus,
        st::Token_Tilde => Tilde,
        st::Token_Star => Star,
        st::Token_Caret => Caret,
        st::Token_SingleQuote => SingleQuote,
        st::Token_Percent => Percent,
        st::Token_Question => Question,
        st::Token_DotDot => DotDot,
        st::Token_ColonColon => ColonColon,
        st::Token_ColonEqual => ColonEqual,
        st::Token_ColonGreater => ColonGreater,
        st::Token_EqualEqual => EqualEqual,
        st::Token_UnderUnder => UnderUnder,
        st::Token_UnderDot => UnderDot,
        st::Token_LessBar => LessBar,
        st::Token_LessLess => LessLess,
        st::Token_LessGreater => LessGreater,
        st::Token_LessEqual => LessEqual,
        st::Token_GreaterGreater => GreaterGreater,
        st::Token_GreaterEqual => GreaterEqual,
        st::Token_MinusGreater => MinusGreater,
        st::Token_MinusMinus => MinusMinus,
        st::Token_MinusEqual => MinusEqual,
        st::Token_BarBar => BarBar,
        st::Token_BarGreater => BarGreater,
        st::Token_SemiSemi => SemiSemi,
        st::Token_AmpAmp => AmpAmp,
        st::Token_SlashAt => SlashAt,
        st::Token_SlashSemi => SlashSemi,
        st::Token_SlashDot => SlashDot,
        st::Token_SlashSlash => SlashSlash,
        st::Token_SlashColon => SlashColon,
        st::Token_SlashEqual => SlashEqual,
        st::Token_SlashStar => SlashStar,
        st::Token_AtAt => AtAt,
        st::Token_AtStar => AtStar,
        st::Token_PlusPlus => PlusPlus,
        st::Token_PlusEqual => PlusEqual,
        st::Token_TildeTilde => TildeTilde,
        st::Token_StarEqual => StarEqual,
        st::Token_StarStar => StarStar,
        st::Token_CaretEqual => CaretEqual,
        st::Token_HashHash => HashHash,
        st::Token_BangEqual => BangEqual,
        st::Token_BangBang => BangBang,
        st::Token_QuestionQuestion => QuestionQuestion,
        st::Token_DotDotDot => DotDotDot,
        st::Token_EqualEqualEqual => EqualEqualEqual,
        st::Token_EqualBangEqual => EqualBangEqual,
        st::Token_UnderUnderUnder => UnderUnderUnder,
        st::Token_SlashSlashDot => SlashSlashDot,
        st::Token_AtAtAt => AtAtAt,
        st::Token_LessMinusGreater => LessMinusGreater,
        st::Token_SlashSlashAt => SlashSlashAt,
        st::Token_CaretColonEqual => CaretColonEqual,
        st::Token_GreaterGreaterGreater => GreaterGreaterGreater,
        st::Token_BarMinusGreater => BarMinusGreater,
        st::Token_SlashSlashEqual => SlashSlashEqual,
        st::Token_ColonColonOpenSquare => ColonColonOpenSquare,
        st::Token_PercentPercent => PercentPercent,
        st::Token_LinearSyntax_Bang => LinearSyntax_Bang,
        st::Token_LinearSyntax_CloseParen => LinearSyntax_CloseParen,
        st::Token_LinearSyntax_At => LinearSyntax_At,
        st::Token_LinearSyntax_Amp => LinearSyntax_Amp,
        st::Token_LinearSyntax_Star => LinearSyntax_Star,
        st::Token_LinearSyntax_Under => LinearSyntax_Under,
        st::Token_LinearSyntax_Caret => LinearSyntax_Caret,
        st::Token_LinearSyntax_Space => LinearSyntax_Space,
        st::Token_LinearSyntax_Percent => LinearSyntax_Percent,
        st::Token_LinearSyntax_Plus => LinearSyntax_Plus,
        st::Token_LinearSyntax_Slash => LinearSyntax_Slash,
        st::Token_LinearSyntax_BackTick => LinearSyntax_BackTick,
        st::Token_Fake_ImplicitTimes => Fake_ImplicitTimes,
        st::Token_Fake_ImplicitNull => Fake_ImplicitNull,
        st::Token_Fake_ImplicitOne => Fake_ImplicitOne,
        st::Token_Fake_ImplicitAll => Fake_ImplicitAll,
        st::Token_Boxes_OpenParenStar => Boxes_OpenParenStar,
        st::Token_Boxes_StarCloseParen => Boxes_StarCloseParen,
        st::Token_Boxes_MultiSingleQuote => Boxes_MultiSingleQuote,
        st::Token_Boxes_MultiWhitespace => Boxes_MultiWhitespace,
        st::Token_LongName_Not => LongName_Not,
        st::Token_LongName_PlusMinus => LongName_PlusMinus,
        st::Token_LongName_CenterDot => LongName_CenterDot,
        st::Token_LongName_Times => LongName_Times,
        st::Token_LongName_Divide => LongName_Divide,
        st::Token_LongName_OpenCurlyQuote => LongName_OpenCurlyQuote,
        st::Token_LongName_CloseCurlyQuote => LongName_CloseCurlyQuote,
        st::Token_LongName_OpenCurlyDoubleQuote => LongName_OpenCurlyDoubleQuote,
        st::Token_LongName_CloseCurlyDoubleQuote => LongName_CloseCurlyDoubleQuote,
        st::Token_LongName_InvisibleTimes => LongName_InvisibleTimes,
        st::Token_LongName_LeftArrow => LongName_LeftArrow,
        st::Token_LongName_UpArrow => LongName_UpArrow,
        st::Token_LongName_RightArrow => LongName_RightArrow,
        st::Token_LongName_DownArrow => LongName_DownArrow,
        st::Token_LongName_LeftRightArrow => LongName_LeftRightArrow,
        st::Token_LongName_UpDownArrow => LongName_UpDownArrow,
        st::Token_LongName_UpperLeftArrow => LongName_UpperLeftArrow,
        st::Token_LongName_UpperRightArrow => LongName_UpperRightArrow,
        st::Token_LongName_LowerRightArrow => LongName_LowerRightArrow,
        st::Token_LongName_LowerLeftArrow => LongName_LowerLeftArrow,
        st::Token_LongName_LeftTeeArrow => LongName_LeftTeeArrow,
        st::Token_LongName_UpTeeArrow => LongName_UpTeeArrow,
        st::Token_LongName_RightTeeArrow => LongName_RightTeeArrow,
        st::Token_LongName_DownTeeArrow => LongName_DownTeeArrow,
        st::Token_LongName_LeftVector => LongName_LeftVector,
        st::Token_LongName_DownLeftVector => LongName_DownLeftVector,
        st::Token_LongName_RightUpVector => LongName_RightUpVector,
        st::Token_LongName_LeftUpVector => LongName_LeftUpVector,
        st::Token_LongName_RightVector => LongName_RightVector,
        st::Token_LongName_DownRightVector => LongName_DownRightVector,
        st::Token_LongName_RightDownVector => LongName_RightDownVector,
        st::Token_LongName_LeftDownVector => LongName_LeftDownVector,
        st::Token_LongName_RightArrowLeftArrow => LongName_RightArrowLeftArrow,
        st::Token_LongName_UpArrowDownArrow => LongName_UpArrowDownArrow,
        st::Token_LongName_LeftArrowRightArrow => LongName_LeftArrowRightArrow,
        st::Token_LongName_ReverseEquilibrium => LongName_ReverseEquilibrium,
        st::Token_LongName_Equilibrium => LongName_Equilibrium,
        st::Token_LongName_DoubleLeftArrow => LongName_DoubleLeftArrow,
        st::Token_LongName_DoubleUpArrow => LongName_DoubleUpArrow,
        st::Token_LongName_DoubleRightArrow => LongName_DoubleRightArrow,
        st::Token_LongName_DoubleDownArrow => LongName_DoubleDownArrow,
        st::Token_LongName_DoubleLeftRightArrow => LongName_DoubleLeftRightArrow,
        st::Token_LongName_DoubleUpDownArrow => LongName_DoubleUpDownArrow,
        st::Token_LongName_LeftArrowBar => LongName_LeftArrowBar,
        st::Token_LongName_RightArrowBar => LongName_RightArrowBar,
        st::Token_LongName_DownArrowUpArrow => LongName_DownArrowUpArrow,
        st::Token_LongName_ForAll => LongName_ForAll,
        st::Token_LongName_PartialD => LongName_PartialD,
        st::Token_LongName_Exists => LongName_Exists,
        st::Token_LongName_NotExists => LongName_NotExists,
        st::Token_LongName_Del => LongName_Del,
        st::Token_LongName_Element => LongName_Element,
        st::Token_LongName_NotElement => LongName_NotElement,
        st::Token_LongName_ReverseElement => LongName_ReverseElement,
        st::Token_LongName_NotReverseElement => LongName_NotReverseElement,
        st::Token_LongName_SuchThat => LongName_SuchThat,
        st::Token_LongName_Product => LongName_Product,
        st::Token_LongName_Coproduct => LongName_Coproduct,
        st::Token_LongName_Sum => LongName_Sum,
        st::Token_LongName_Minus => LongName_Minus,
        st::Token_LongName_MinusPlus => LongName_MinusPlus,
        st::Token_LongName_DivisionSlash => LongName_DivisionSlash,
        st::Token_LongName_Backslash => LongName_Backslash,
        st::Token_LongName_SmallCircle => LongName_SmallCircle,
        st::Token_LongName_Sqrt => LongName_Sqrt,
        st::Token_LongName_CubeRoot => LongName_CubeRoot,
        st::Token_LongName_Proportional => LongName_Proportional,
        st::Token_LongName_Divides => LongName_Divides,
        st::Token_LongName_DoubleVerticalBar => LongName_DoubleVerticalBar,
        st::Token_LongName_NotDoubleVerticalBar => LongName_NotDoubleVerticalBar,
        st::Token_LongName_And => LongName_And,
        st::Token_LongName_Or => LongName_Or,
        st::Token_LongName_Integral => LongName_Integral,
        st::Token_LongName_ContourIntegral => LongName_ContourIntegral,
        st::Token_LongName_DoubleContourIntegral => LongName_DoubleContourIntegral,
        st::Token_LongName_ClockwiseContourIntegral => LongName_ClockwiseContourIntegral,
        st::Token_LongName_CounterClockwiseContourIntegral => LongName_CounterClockwiseContourIntegral,
        st::Token_LongName_Therefore => LongName_Therefore,
        st::Token_LongName_Because => LongName_Because,
        st::Token_LongName_Colon => LongName_Colon,
        st::Token_LongName_Proportion => LongName_Proportion,
        st::Token_LongName_Tilde => LongName_Tilde,
        st::Token_LongName_VerticalTilde => LongName_VerticalTilde,
        st::Token_LongName_NotTilde => LongName_NotTilde,
        st::Token_LongName_EqualTilde => LongName_EqualTilde,
        st::Token_LongName_TildeEqual => LongName_TildeEqual,
        st::Token_LongName_NotTildeEqual => LongName_NotTildeEqual,
        st::Token_LongName_TildeFullEqual => LongName_TildeFullEqual,
        st::Token_LongName_NotTildeFullEqual => LongName_NotTildeFullEqual,
        st::Token_LongName_TildeTilde => LongName_TildeTilde,
        st::Token_LongName_NotTildeTilde => LongName_NotTildeTilde,
        st::Token_LongName_CupCap => LongName_CupCap,
        st::Token_LongName_HumpDownHump => LongName_HumpDownHump,
        st::Token_LongName_HumpEqual => LongName_HumpEqual,
        st::Token_LongName_DotEqual => LongName_DotEqual,
        st::Token_LongName_NotEqual => LongName_NotEqual,
        st::Token_LongName_Congruent => LongName_Congruent,
        st::Token_LongName_NotCongruent => LongName_NotCongruent,
        st::Token_LongName_LessEqual => LongName_LessEqual,
        st::Token_LongName_GreaterEqual => LongName_GreaterEqual,
        st::Token_LongName_LessFullEqual => LongName_LessFullEqual,
        st::Token_LongName_GreaterFullEqual => LongName_GreaterFullEqual,
        st::Token_LongName_NotLessFullEqual => LongName_NotLessFullEqual,
        st::Token_LongName_NotGreaterFullEqual => LongName_NotGreaterFullEqual,
        st::Token_LongName_LessLess => LongName_LessLess,
        st::Token_LongName_GreaterGreater => LongName_GreaterGreater,
        st::Token_LongName_NotCupCap => LongName_NotCupCap,
        st::Token_LongName_NotLess => LongName_NotLess,
        st::Token_LongName_NotGreater => LongName_NotGreater,
        st::Token_LongName_NotLessEqual => LongName_NotLessEqual,
        st::Token_LongName_NotGreaterEqual => LongName_NotGreaterEqual,
        st::Token_LongName_LessTilde => LongName_LessTilde,
        st::Token_LongName_GreaterTilde => LongName_GreaterTilde,
        st::Token_LongName_NotLessTilde => LongName_NotLessTilde,
        st::Token_LongName_NotGreaterTilde => LongName_NotGreaterTilde,
        st::Token_LongName_LessGreater => LongName_LessGreater,
        st::Token_LongName_GreaterLess => LongName_GreaterLess,
        st::Token_LongName_NotLessGreater => LongName_NotLessGreater,
        st::Token_LongName_NotGreaterLess => LongName_NotGreaterLess,
        st::Token_LongName_Precedes => LongName_Precedes,
        st::Token_LongName_Succeeds => LongName_Succeeds,
        st::Token_LongName_PrecedesSlantEqual => LongName_PrecedesSlantEqual,
        st::Token_LongName_SucceedsSlantEqual => LongName_SucceedsSlantEqual,
        st::Token_LongName_PrecedesTilde => LongName_PrecedesTilde,
        st::Token_LongName_SucceedsTilde => LongName_SucceedsTilde,
        st::Token_LongName_NotPrecedes => LongName_NotPrecedes,
        st::Token_LongName_NotSucceeds => LongName_NotSucceeds,
        st::Token_LongName_Subset => LongName_Subset,
        st::Token_LongName_Superset => LongName_Superset,
        st::Token_LongName_NotSubset => LongName_NotSubset,
        st::Token_LongName_NotSuperset => LongName_NotSuperset,
        st::Token_LongName_SubsetEqual => LongName_SubsetEqual,
        st::Token_LongName_SupersetEqual => LongName_SupersetEqual,
        st::Token_LongName_NotSubsetEqual => LongName_NotSubsetEqual,
        st::Token_LongName_NotSupersetEqual => LongName_NotSupersetEqual,
        st::Token_LongName_UnionPlus => LongName_UnionPlus,
        st::Token_LongName_SquareSubset => LongName_SquareSubset,
        st::Token_LongName_SquareSuperset => LongName_SquareSuperset,
        st::Token_LongName_SquareSubsetEqual => LongName_SquareSubsetEqual,
        st::Token_LongName_SquareSupersetEqual => LongName_SquareSupersetEqual,
        st::Token_LongName_SquareIntersection => LongName_SquareIntersection,
        st::Token_LongName_SquareUnion => LongName_SquareUnion,
        st::Token_LongName_CirclePlus => LongName_CirclePlus,
        st::Token_LongName_CircleMinus => LongName_CircleMinus,
        st::Token_LongName_CircleTimes => LongName_CircleTimes,
        st::Token_LongName_CircleDot => LongName_CircleDot,
        st::Token_LongName_RightTee => LongName_RightTee,
        st::Token_LongName_LeftTee => LongName_LeftTee,
        st::Token_LongName_DownTee => LongName_DownTee,
        st::Token_LongName_UpTee => LongName_UpTee,
        st::Token_LongName_DoubleRightTee => LongName_DoubleRightTee,
        st::Token_LongName_LeftTriangle => LongName_LeftTriangle,
        st::Token_LongName_RightTriangle => LongName_RightTriangle,
        st::Token_LongName_LeftTriangleEqual => LongName_LeftTriangleEqual,
        st::Token_LongName_RightTriangleEqual => LongName_RightTriangleEqual,
        st::Token_LongName_Xor => LongName_Xor,
        st::Token_LongName_Nand => LongName_Nand,
        st::Token_LongName_Nor => LongName_Nor,
        st::Token_LongName_Wedge => LongName_Wedge,
        st::Token_LongName_Vee => LongName_Vee,
        st::Token_LongName_Intersection => LongName_Intersection,
        st::Token_LongName_Union => LongName_Union,
        st::Token_LongName_Diamond => LongName_Diamond,
        st::Token_LongName_Star => LongName_Star,
        st::Token_LongName_LessEqualGreater => LongName_LessEqualGreater,
        st::Token_LongName_GreaterEqualLess => LongName_GreaterEqualLess,
        st::Token_LongName_NotPrecedesSlantEqual => LongName_NotPrecedesSlantEqual,
        st::Token_LongName_NotSucceedsSlantEqual => LongName_NotSucceedsSlantEqual,
        st::Token_LongName_NotSquareSubsetEqual => LongName_NotSquareSubsetEqual,
        st::Token_LongName_NotSquareSupersetEqual => LongName_NotSquareSupersetEqual,
        st::Token_LongName_NotPrecedesTilde => LongName_NotPrecedesTilde,
        st::Token_LongName_NotSucceedsTilde => LongName_NotSucceedsTilde,
        st::Token_LongName_NotLeftTriangle => LongName_NotLeftTriangle,
        st::Token_LongName_NotRightTriangle => LongName_NotRightTriangle,
        st::Token_LongName_NotLeftTriangleEqual => LongName_NotLeftTriangleEqual,
        st::Token_LongName_NotRightTriangleEqual => LongName_NotRightTriangleEqual,
        st::Token_LongName_LeftCeiling => LongName_LeftCeiling,
        st::Token_LongName_RightCeiling => LongName_RightCeiling,
        st::Token_LongName_LeftFloor => LongName_LeftFloor,
        st::Token_LongName_RightFloor => LongName_RightFloor,
        st::Token_LongName_Cap => LongName_Cap,
        st::Token_LongName_Cup => LongName_Cup,
        st::Token_LongName_LeftAngleBracket => LongName_LeftAngleBracket,
        st::Token_LongName_RightAngleBracket => LongName_RightAngleBracket,
        st::Token_LongName_Perpendicular => LongName_Perpendicular,
        st::Token_LongName_LongLeftArrow => LongName_LongLeftArrow,
        st::Token_LongName_LongRightArrow => LongName_LongRightArrow,
        st::Token_LongName_LongLeftRightArrow => LongName_LongLeftRightArrow,
        st::Token_LongName_DoubleLongLeftArrow => LongName_DoubleLongLeftArrow,
        st::Token_LongName_DoubleLongRightArrow => LongName_DoubleLongRightArrow,
        st::Token_LongName_DoubleLongLeftRightArrow => LongName_DoubleLongLeftRightArrow,
        st::Token_LongName_UpArrowBar => LongName_UpArrowBar,
        st::Token_LongName_DownArrowBar => LongName_DownArrowBar,
        st::Token_LongName_LeftRightVector => LongName_LeftRightVector,
        st::Token_LongName_RightUpDownVector => LongName_RightUpDownVector,
        st::Token_LongName_DownLeftRightVector => LongName_DownLeftRightVector,
        st::Token_LongName_LeftUpDownVector => LongName_LeftUpDownVector,
        st::Token_LongName_LeftVectorBar => LongName_LeftVectorBar,
        st::Token_LongName_RightVectorBar => LongName_RightVectorBar,
        st::Token_LongName_RightUpVectorBar => LongName_RightUpVectorBar,
        st::Token_LongName_RightDownVectorBar => LongName_RightDownVectorBar,
        st::Token_LongName_DownLeftVectorBar => LongName_DownLeftVectorBar,
        st::Token_LongName_DownRightVectorBar => LongName_DownRightVectorBar,
        st::Token_LongName_LeftUpVectorBar => LongName_LeftUpVectorBar,
        st::Token_LongName_LeftDownVectorBar => LongName_LeftDownVectorBar,
        st::Token_LongName_LeftTeeVector => LongName_LeftTeeVector,
        st::Token_LongName_RightTeeVector => LongName_RightTeeVector,
        st::Token_LongName_RightUpTeeVector => LongName_RightUpTeeVector,
        st::Token_LongName_RightDownTeeVector => LongName_RightDownTeeVector,
        st::Token_LongName_DownLeftTeeVector => LongName_DownLeftTeeVector,
        st::Token_LongName_DownRightTeeVector => LongName_DownRightTeeVector,
        st::Token_LongName_LeftUpTeeVector => LongName_LeftUpTeeVector,
        st::Token_LongName_LeftDownTeeVector => LongName_LeftDownTeeVector,
        st::Token_LongName_UpEquilibrium => LongName_UpEquilibrium,
        st::Token_LongName_ReverseUpEquilibrium => LongName_ReverseUpEquilibrium,
        st::Token_LongName_RoundImplies => LongName_RoundImplies,
        st::Token_LongName_LeftTriangleBar => LongName_LeftTriangleBar,
        st::Token_LongName_RightTriangleBar => LongName_RightTriangleBar,
        st::Token_LongName_Equivalent => LongName_Equivalent,
        st::Token_LongName_LessSlantEqual => LongName_LessSlantEqual,
        st::Token_LongName_GreaterSlantEqual => LongName_GreaterSlantEqual,
        st::Token_LongName_NestedLessLess => LongName_NestedLessLess,
        st::Token_LongName_NestedGreaterGreater => LongName_NestedGreaterGreater,
        st::Token_LongName_PrecedesEqual => LongName_PrecedesEqual,
        st::Token_LongName_SucceedsEqual => LongName_SucceedsEqual,
        st::Token_LongName_DoubleLeftTee => LongName_DoubleLeftTee,
        st::Token_LongName_LeftDoubleBracket => LongName_LeftDoubleBracket,
        st::Token_LongName_RightDoubleBracket => LongName_RightDoubleBracket,
        st::Token_LongName_LeftAssociation => LongName_LeftAssociation,
        st::Token_LongName_RightAssociation => LongName_RightAssociation,
        st::Token_LongName_TwoWayRule => LongName_TwoWayRule,
        st::Token_LongName_Piecewise => LongName_Piecewise,
        st::Token_LongName_ImplicitPlus => LongName_ImplicitPlus,
        st::Token_LongName_AutoLeftMatch => LongName_AutoLeftMatch,
        st::Token_LongName_AutoRightMatch => LongName_AutoRightMatch,
        st::Token_LongName_InvisiblePrefixScriptBase => LongName_InvisiblePrefixScriptBase,
        st::Token_LongName_InvisiblePostfixScriptBase => LongName_InvisiblePostfixScriptBase,
        st::Token_LongName_Transpose => LongName_Transpose,
        st::Token_LongName_Conjugate => LongName_Conjugate,
        st::Token_LongName_ConjugateTranspose => LongName_ConjugateTranspose,
        st::Token_LongName_HermitianConjugate => LongName_HermitianConjugate,
        st::Token_LongName_VerticalBar => LongName_VerticalBar,
        st::Token_LongName_NotVerticalBar => LongName_NotVerticalBar,
        st::Token_LongName_Distributed => LongName_Distributed,
        st::Token_LongName_Conditioned => LongName_Conditioned,
        st::Token_LongName_UndirectedEdge => LongName_UndirectedEdge,
        st::Token_LongName_DirectedEdge => LongName_DirectedEdge,
        st::Token_LongName_ContinuedFractionK => LongName_ContinuedFractionK,
        st::Token_LongName_TensorProduct => LongName_TensorProduct,
        st::Token_LongName_TensorWedge => LongName_TensorWedge,
        st::Token_LongName_ProbabilityPr => LongName_ProbabilityPr,
        st::Token_LongName_ExpectationE => LongName_ExpectationE,
        st::Token_LongName_PermutationProduct => LongName_PermutationProduct,
        st::Token_LongName_NotEqualTilde => LongName_NotEqualTilde,
        st::Token_LongName_NotHumpEqual => LongName_NotHumpEqual,
        st::Token_LongName_NotHumpDownHump => LongName_NotHumpDownHump,
        st::Token_LongName_NotLeftTriangleBar => LongName_NotLeftTriangleBar,
        st::Token_LongName_NotRightTriangleBar => LongName_NotRightTriangleBar,
        st::Token_LongName_NotLessLess => LongName_NotLessLess,
        st::Token_LongName_NotNestedLessLess => LongName_NotNestedLessLess,
        st::Token_LongName_NotLessSlantEqual => LongName_NotLessSlantEqual,
        st::Token_LongName_NotGreaterGreater => LongName_NotGreaterGreater,
        st::Token_LongName_NotNestedGreaterGreater => LongName_NotNestedGreaterGreater,
        st::Token_LongName_NotGreaterSlantEqual => LongName_NotGreaterSlantEqual,
        st::Token_LongName_NotPrecedesEqual => LongName_NotPrecedesEqual,
        st::Token_LongName_NotSucceedsEqual => LongName_NotSucceedsEqual,
        st::Token_LongName_NotSquareSubset => LongName_NotSquareSubset,
        st::Token_LongName_NotSquareSuperset => LongName_NotSquareSuperset,
        st::Token_LongName_Equal => LongName_Equal,
        st::Token_LongName_VerticalSeparator => LongName_VerticalSeparator,
        st::Token_LongName_VectorGreater => LongName_VectorGreater,
        st::Token_LongName_VectorGreaterEqual => LongName_VectorGreaterEqual,
        st::Token_LongName_VectorLess => LongName_VectorLess,
        st::Token_LongName_VectorLessEqual => LongName_VectorLessEqual,
        st::Token_LongName_Limit => LongName_Limit,
        st::Token_LongName_MaxLimit => LongName_MaxLimit,
        st::Token_LongName_MinLimit => LongName_MinLimit,
        st::Token_LongName_Cross => LongName_Cross,
        st::Token_LongName_Function => LongName_Function,
        st::Token_LongName_Xnor => LongName_Xnor,
        st::Token_LongName_DiscreteShift => LongName_DiscreteShift,
        st::Token_LongName_DifferenceDelta => LongName_DifferenceDelta,
        st::Token_LongName_DiscreteRatio => LongName_DiscreteRatio,
        st::Token_LongName_RuleDelayed => LongName_RuleDelayed,
        st::Token_LongName_Square => LongName_Square,
        st::Token_LongName_Rule => LongName_Rule,
        st::Token_LongName_Implies => LongName_Implies,
        st::Token_LongName_ShortRightArrow => LongName_ShortRightArrow,
        st::Token_LongName_ShortLeftArrow => LongName_ShortLeftArrow,
        st::Token_LongName_ShortUpArrow => LongName_ShortUpArrow,
        st::Token_LongName_ShortDownArrow => LongName_ShortDownArrow,
        st::Token_LongName_Application => LongName_Application,
        st::Token_LongName_LeftBracketingBar => LongName_LeftBracketingBar,
        st::Token_LongName_RightBracketingBar => LongName_RightBracketingBar,
        st::Token_LongName_LeftDoubleBracketingBar => LongName_LeftDoubleBracketingBar,
        st::Token_LongName_RightDoubleBracketingBar => LongName_RightDoubleBracketingBar,
        st::Token_LongName_CapitalDifferentialD => LongName_CapitalDifferentialD,
        st::Token_LongName_DifferentialD => LongName_DifferentialD,
        st::Token_LongName_InvisibleComma => LongName_InvisibleComma,
        st::Token_LongName_InvisibleApplication => LongName_InvisibleApplication,
        st::Token_LongName_LongEqual => LongName_LongEqual,
        _ => return None,
    };

    Some(token)
}

impl TokenKind {
    pub(crate) const VARIANTS: [TokenKind; TokenKind::COUNT] = [
        TokenKind::Unknown,                                  // 0
        TokenKind::EndOfFile,                                // 1
        TokenKind::Symbol,                                   // 2
        TokenKind::String,                                   // 3
        TokenKind::Integer,                                  // 4
        TokenKind::Real,                                     // 5
        TokenKind::Rational,                                 // 6
        TokenKind::LinearSyntaxBlob,                         // 7
        TokenKind::InternalNewline,                          // 8
        TokenKind::Comment,                                  // 9
        TokenKind::Whitespace,                               // 10
        TokenKind::Buffer1,                                  // 11
        TokenKind::ToplevelNewline,                          // 12
        TokenKind::Buffer2,                                  // 13
        TokenKind::Buffer3,                                  // 14
        TokenKind::Buffer4,                                  // 15
        TokenKind::Error_ExpectedEqual,                      // 16
        TokenKind::Error_Number,                             // 17
        TokenKind::Error_UnhandledCharacter,                 // 18
        TokenKind::Error_ExpectedLetterlike,                 // 19
        TokenKind::Error_Aborted,                            // 20
        TokenKind::Error_ExpectedOperand,                    // 21
        TokenKind::Error_ExpectedTag,                        // 22
        TokenKind::Error_ExpectedFile,                       // 23
        TokenKind::Error_UnexpectedCloser,                   // 24
        TokenKind::Error_PrefixImplicitNull,                 // 25
        TokenKind::Error_InfixImplicitNull,                  // 26
        TokenKind::Error_UnsafeCharacterEncoding,            // 27
        TokenKind::Error_UnterminatedComment,                // 28
        TokenKind::Error_UnterminatedString,                 // 29
        TokenKind::Error_UnterminatedFileString,             // 30
        TokenKind::Error_UnterminatedLinearSyntaxBlob,       // 31
        TokenKind::Error_UnsupportedToken,                   // 32
        TokenKind::Error_UnexpectedCommentCloser,            // 33
        TokenKind::Dot,                                      // 34
        TokenKind::Colon,                                    // 35
        TokenKind::OpenParen,                                // 36
        TokenKind::CloseParen,                               // 37
        TokenKind::OpenSquare,                               // 38
        TokenKind::CloseSquare,                              // 39
        TokenKind::Comma,                                    // 40
        TokenKind::OpenCurly,                                // 41
        TokenKind::CloseCurly,                               // 42
        TokenKind::Equal,                                    // 43
        TokenKind::Bang,                                     // 44
        TokenKind::Under,                                    // 45
        TokenKind::Less,                                     // 46
        TokenKind::Greater,                                  // 47
        TokenKind::Minus,                                    // 48
        TokenKind::Bar,                                      // 49
        TokenKind::Semi,                                     // 50
        TokenKind::Hash,                                     // 51
        TokenKind::Amp,                                      // 52
        TokenKind::Slash,                                    // 53
        TokenKind::At,                                       // 54
        TokenKind::Plus,                                     // 55
        TokenKind::Tilde,                                    // 56
        TokenKind::Star,                                     // 57
        TokenKind::Caret,                                    // 58
        TokenKind::SingleQuote,                              // 59
        TokenKind::Percent,                                  // 60
        TokenKind::Question,                                 // 61
        TokenKind::DotDot,                                   // 62
        TokenKind::ColonColon,                               // 63
        TokenKind::ColonEqual,                               // 64
        TokenKind::ColonGreater,                             // 65
        TokenKind::EqualEqual,                               // 66
        TokenKind::UnderUnder,                               // 67
        TokenKind::UnderDot,                                 // 68
        TokenKind::LessBar,                                  // 69
        TokenKind::LessLess,                                 // 70
        TokenKind::LessGreater,                              // 71
        TokenKind::LessEqual,                                // 72
        TokenKind::GreaterGreater,                           // 73
        TokenKind::GreaterEqual,                             // 74
        TokenKind::MinusGreater,                             // 75
        TokenKind::MinusMinus,                               // 76
        TokenKind::MinusEqual,                               // 77
        TokenKind::BarBar,                                   // 78
        TokenKind::BarGreater,                               // 79
        TokenKind::SemiSemi,                                 // 80
        TokenKind::AmpAmp,                                   // 81
        TokenKind::SlashAt,                                  // 82
        TokenKind::SlashSemi,                                // 83
        TokenKind::SlashDot,                                 // 84
        TokenKind::SlashSlash,                               // 85
        TokenKind::SlashColon,                               // 86
        TokenKind::SlashEqual,                               // 87
        TokenKind::SlashStar,                                // 88
        TokenKind::AtAt,                                     // 89
        TokenKind::AtStar,                                   // 90
        TokenKind::PlusPlus,                                 // 91
        TokenKind::PlusEqual,                                // 92
        TokenKind::TildeTilde,                               // 93
        TokenKind::StarEqual,                                // 94
        TokenKind::StarStar,                                 // 95
        TokenKind::CaretEqual,                               // 96
        TokenKind::HashHash,                                 // 97
        TokenKind::BangEqual,                                // 98
        TokenKind::BangBang,                                 // 99
        TokenKind::QuestionQuestion,                         // 100
        TokenKind::DotDotDot,                                // 101
        TokenKind::EqualEqualEqual,                          // 102
        TokenKind::EqualBangEqual,                           // 103
        TokenKind::UnderUnderUnder,                          // 104
        TokenKind::SlashSlashDot,                            // 105
        TokenKind::AtAtAt,                                   // 106
        TokenKind::LessMinusGreater,                         // 107
        TokenKind::SlashSlashAt,                             // 108
        TokenKind::CaretColonEqual,                          // 109
        TokenKind::GreaterGreaterGreater,                    // 110
        TokenKind::BarMinusGreater,                          // 111
        TokenKind::SlashSlashEqual,                          // 112
        TokenKind::ColonColonOpenSquare,                     // 113
        TokenKind::PercentPercent,                           // 114
        TokenKind::LinearSyntax_Bang,                        // 115
        TokenKind::LinearSyntax_CloseParen,                  // 116
        TokenKind::LinearSyntax_At,                          // 117
        TokenKind::LinearSyntax_Amp,                         // 118
        TokenKind::LinearSyntax_Star,                        // 119
        TokenKind::LinearSyntax_Under,                       // 120
        TokenKind::LinearSyntax_Caret,                       // 121
        TokenKind::LinearSyntax_Space,                       // 122
        TokenKind::LinearSyntax_Percent,                     // 123
        TokenKind::LinearSyntax_Plus,                        // 124
        TokenKind::LinearSyntax_Slash,                       // 125
        TokenKind::LinearSyntax_BackTick,                    // 126
        TokenKind::Fake_ImplicitTimes,                       // 127
        TokenKind::Fake_ImplicitNull,                        // 128
        TokenKind::Fake_ImplicitOne,                         // 129
        TokenKind::Fake_ImplicitAll,                         // 130
        TokenKind::Boxes_OpenParenStar,                      // 131
        TokenKind::Boxes_StarCloseParen,                     // 132
        TokenKind::Boxes_MultiSingleQuote,                   // 133
        TokenKind::Boxes_MultiWhitespace,                    // 134
        TokenKind::LongName_Not,                             // 135
        TokenKind::LongName_PlusMinus,                       // 136
        TokenKind::LongName_CenterDot,                       // 137
        TokenKind::LongName_Times,                           // 138
        TokenKind::LongName_Divide,                          // 139
        TokenKind::LongName_OpenCurlyQuote,                  // 140
        TokenKind::LongName_CloseCurlyQuote,                 // 141
        TokenKind::LongName_OpenCurlyDoubleQuote,            // 142
        TokenKind::LongName_CloseCurlyDoubleQuote,           // 143
        TokenKind::LongName_InvisibleTimes,                  // 144
        TokenKind::LongName_LeftArrow,                       // 145
        TokenKind::LongName_UpArrow,                         // 146
        TokenKind::LongName_RightArrow,                      // 147
        TokenKind::LongName_DownArrow,                       // 148
        TokenKind::LongName_LeftRightArrow,                  // 149
        TokenKind::LongName_UpDownArrow,                     // 150
        TokenKind::LongName_UpperLeftArrow,                  // 151
        TokenKind::LongName_UpperRightArrow,                 // 152
        TokenKind::LongName_LowerRightArrow,                 // 153
        TokenKind::LongName_LowerLeftArrow,                  // 154
        TokenKind::LongName_LeftTeeArrow,                    // 155
        TokenKind::LongName_UpTeeArrow,                      // 156
        TokenKind::LongName_RightTeeArrow,                   // 157
        TokenKind::LongName_DownTeeArrow,                    // 158
        TokenKind::LongName_LeftVector,                      // 159
        TokenKind::LongName_DownLeftVector,                  // 160
        TokenKind::LongName_RightUpVector,                   // 161
        TokenKind::LongName_LeftUpVector,                    // 162
        TokenKind::LongName_RightVector,                     // 163
        TokenKind::LongName_DownRightVector,                 // 164
        TokenKind::LongName_RightDownVector,                 // 165
        TokenKind::LongName_LeftDownVector,                  // 166
        TokenKind::LongName_RightArrowLeftArrow,             // 167
        TokenKind::LongName_UpArrowDownArrow,                // 168
        TokenKind::LongName_LeftArrowRightArrow,             // 169
        TokenKind::LongName_ReverseEquilibrium,              // 170
        TokenKind::LongName_Equilibrium,                     // 171
        TokenKind::LongName_DoubleLeftArrow,                 // 172
        TokenKind::LongName_DoubleUpArrow,                   // 173
        TokenKind::LongName_DoubleRightArrow,                // 174
        TokenKind::LongName_DoubleDownArrow,                 // 175
        TokenKind::LongName_DoubleLeftRightArrow,            // 176
        TokenKind::LongName_DoubleUpDownArrow,               // 177
        TokenKind::LongName_LeftArrowBar,                    // 178
        TokenKind::LongName_RightArrowBar,                   // 179
        TokenKind::LongName_DownArrowUpArrow,                // 180
        TokenKind::LongName_ForAll,                          // 181
        TokenKind::LongName_PartialD,                        // 182
        TokenKind::LongName_Exists,                          // 183
        TokenKind::LongName_NotExists,                       // 184
        TokenKind::LongName_Del,                             // 185
        TokenKind::LongName_Element,                         // 186
        TokenKind::LongName_NotElement,                      // 187
        TokenKind::LongName_ReverseElement,                  // 188
        TokenKind::LongName_NotReverseElement,               // 189
        TokenKind::LongName_SuchThat,                        // 190
        TokenKind::LongName_Product,                         // 191
        TokenKind::LongName_Coproduct,                       // 192
        TokenKind::LongName_Sum,                             // 193
        TokenKind::LongName_Minus,                           // 194
        TokenKind::LongName_MinusPlus,                       // 195
        TokenKind::LongName_DivisionSlash,                   // 196
        TokenKind::LongName_Backslash,                       // 197
        TokenKind::LongName_SmallCircle,                     // 198
        TokenKind::LongName_Sqrt,                            // 199
        TokenKind::LongName_CubeRoot,                        // 200
        TokenKind::LongName_Proportional,                    // 201
        TokenKind::LongName_Divides,                         // 202
        TokenKind::LongName_DoubleVerticalBar,               // 203
        TokenKind::LongName_NotDoubleVerticalBar,            // 204
        TokenKind::LongName_And,                             // 205
        TokenKind::LongName_Or,                              // 206
        TokenKind::LongName_Integral,                        // 207
        TokenKind::LongName_ContourIntegral,                 // 208
        TokenKind::LongName_DoubleContourIntegral,           // 209
        TokenKind::LongName_ClockwiseContourIntegral,        // 210
        TokenKind::LongName_CounterClockwiseContourIntegral, // 211
        TokenKind::LongName_Therefore,                       // 212
        TokenKind::LongName_Because,                         // 213
        TokenKind::LongName_Colon,                           // 214
        TokenKind::LongName_Proportion,                      // 215
        TokenKind::LongName_Tilde,                           // 216
        TokenKind::LongName_VerticalTilde,                   // 217
        TokenKind::LongName_NotTilde,                        // 218
        TokenKind::LongName_EqualTilde,                      // 219
        TokenKind::LongName_TildeEqual,                      // 220
        TokenKind::LongName_NotTildeEqual,                   // 221
        TokenKind::LongName_TildeFullEqual,                  // 222
        TokenKind::LongName_NotTildeFullEqual,               // 223
        TokenKind::LongName_TildeTilde,                      // 224
        TokenKind::LongName_NotTildeTilde,                   // 225
        TokenKind::LongName_CupCap,                          // 226
        TokenKind::LongName_HumpDownHump,                    // 227
        TokenKind::LongName_HumpEqual,                       // 228
        TokenKind::LongName_DotEqual,                        // 229
        TokenKind::LongName_NotEqual,                        // 230
        TokenKind::LongName_Congruent,                       // 231
        TokenKind::LongName_NotCongruent,                    // 232
        TokenKind::LongName_LessEqual,                       // 233
        TokenKind::LongName_GreaterEqual,                    // 234
        TokenKind::LongName_LessFullEqual,                   // 235
        TokenKind::LongName_GreaterFullEqual,                // 236
        TokenKind::LongName_NotLessFullEqual,                // 237
        TokenKind::LongName_NotGreaterFullEqual,             // 238
        TokenKind::LongName_LessLess,                        // 239
        TokenKind::LongName_GreaterGreater,                  // 240
        TokenKind::LongName_NotCupCap,                       // 241
        TokenKind::LongName_NotLess,                         // 242
        TokenKind::LongName_NotGreater,                      // 243
        TokenKind::LongName_NotLessEqual,                    // 244
        TokenKind::LongName_NotGreaterEqual,                 // 245
        TokenKind::LongName_LessTilde,                       // 246
        TokenKind::LongName_GreaterTilde,                    // 247
        TokenKind::LongName_NotLessTilde,                    // 248
        TokenKind::LongName_NotGreaterTilde,                 // 249
        TokenKind::LongName_LessGreater,                     // 250
        TokenKind::LongName_GreaterLess,                     // 251
        TokenKind::LongName_NotLessGreater,                  // 252
        TokenKind::LongName_NotGreaterLess,                  // 253
        TokenKind::LongName_Precedes,                        // 254
        TokenKind::LongName_Succeeds,                        // 255
        TokenKind::LongName_PrecedesSlantEqual,              // 256
        TokenKind::LongName_SucceedsSlantEqual,              // 257
        TokenKind::LongName_PrecedesTilde,                   // 258
        TokenKind::LongName_SucceedsTilde,                   // 259
        TokenKind::LongName_NotPrecedes,                     // 260
        TokenKind::LongName_NotSucceeds,                     // 261
        TokenKind::LongName_Subset,                          // 262
        TokenKind::LongName_Superset,                        // 263
        TokenKind::LongName_NotSubset,                       // 264
        TokenKind::LongName_NotSuperset,                     // 265
        TokenKind::LongName_SubsetEqual,                     // 266
        TokenKind::LongName_SupersetEqual,                   // 267
        TokenKind::LongName_NotSubsetEqual,                  // 268
        TokenKind::LongName_NotSupersetEqual,                // 269
        TokenKind::LongName_UnionPlus,                       // 270
        TokenKind::LongName_SquareSubset,                    // 271
        TokenKind::LongName_SquareSuperset,                  // 272
        TokenKind::LongName_SquareSubsetEqual,               // 273
        TokenKind::LongName_SquareSupersetEqual,             // 274
        TokenKind::LongName_SquareIntersection,              // 275
        TokenKind::LongName_SquareUnion,                     // 276
        TokenKind::LongName_CirclePlus,                      // 277
        TokenKind::LongName_CircleMinus,                     // 278
        TokenKind::LongName_CircleTimes,                     // 279
        TokenKind::LongName_CircleDot,                       // 280
        TokenKind::LongName_RightTee,                        // 281
        TokenKind::LongName_LeftTee,                         // 282
        TokenKind::LongName_DownTee,                         // 283
        TokenKind::LongName_UpTee,                           // 284
        TokenKind::LongName_DoubleRightTee,                  // 285
        TokenKind::LongName_LeftTriangle,                    // 286
        TokenKind::LongName_RightTriangle,                   // 287
        TokenKind::LongName_LeftTriangleEqual,               // 288
        TokenKind::LongName_RightTriangleEqual,              // 289
        TokenKind::LongName_Xor,                             // 290
        TokenKind::LongName_Nand,                            // 291
        TokenKind::LongName_Nor,                             // 292
        TokenKind::LongName_Wedge,                           // 293
        TokenKind::LongName_Vee,                             // 294
        TokenKind::LongName_Intersection,                    // 295
        TokenKind::LongName_Union,                           // 296
        TokenKind::LongName_Diamond,                         // 297
        TokenKind::LongName_Star,                            // 298
        TokenKind::LongName_LessEqualGreater,                // 299
        TokenKind::LongName_GreaterEqualLess,                // 300
        TokenKind::LongName_NotPrecedesSlantEqual,           // 301
        TokenKind::LongName_NotSucceedsSlantEqual,           // 302
        TokenKind::LongName_NotSquareSubsetEqual,            // 303
        TokenKind::LongName_NotSquareSupersetEqual,          // 304
        TokenKind::LongName_NotPrecedesTilde,                // 305
        TokenKind::LongName_NotSucceedsTilde,                // 306
        TokenKind::LongName_NotLeftTriangle,                 // 307
        TokenKind::LongName_NotRightTriangle,                // 308
        TokenKind::LongName_NotLeftTriangleEqual,            // 309
        TokenKind::LongName_NotRightTriangleEqual,           // 310
        TokenKind::LongName_LeftCeiling,                     // 311
        TokenKind::LongName_RightCeiling,                    // 312
        TokenKind::LongName_LeftFloor,                       // 313
        TokenKind::LongName_RightFloor,                      // 314
        TokenKind::LongName_Cap,                             // 315
        TokenKind::LongName_Cup,                             // 316
        TokenKind::LongName_LeftAngleBracket,                // 317
        TokenKind::LongName_RightAngleBracket,               // 318
        TokenKind::LongName_Perpendicular,                   // 319
        TokenKind::LongName_LongLeftArrow,                   // 320
        TokenKind::LongName_LongRightArrow,                  // 321
        TokenKind::LongName_LongLeftRightArrow,              // 322
        TokenKind::LongName_DoubleLongLeftArrow,             // 323
        TokenKind::LongName_DoubleLongRightArrow,            // 324
        TokenKind::LongName_DoubleLongLeftRightArrow,        // 325
        TokenKind::LongName_UpArrowBar,                      // 326
        TokenKind::LongName_DownArrowBar,                    // 327
        TokenKind::LongName_LeftRightVector,                 // 328
        TokenKind::LongName_RightUpDownVector,               // 329
        TokenKind::LongName_DownLeftRightVector,             // 330
        TokenKind::LongName_LeftUpDownVector,                // 331
        TokenKind::LongName_LeftVectorBar,                   // 332
        TokenKind::LongName_RightVectorBar,                  // 333
        TokenKind::LongName_RightUpVectorBar,                // 334
        TokenKind::LongName_RightDownVectorBar,              // 335
        TokenKind::LongName_DownLeftVectorBar,               // 336
        TokenKind::LongName_DownRightVectorBar,              // 337
        TokenKind::LongName_LeftUpVectorBar,                 // 338
        TokenKind::LongName_LeftDownVectorBar,               // 339
        TokenKind::LongName_LeftTeeVector,                   // 340
        TokenKind::LongName_RightTeeVector,                  // 341
        TokenKind::LongName_RightUpTeeVector,                // 342
        TokenKind::LongName_RightDownTeeVector,              // 343
        TokenKind::LongName_DownLeftTeeVector,               // 344
        TokenKind::LongName_DownRightTeeVector,              // 345
        TokenKind::LongName_LeftUpTeeVector,                 // 346
        TokenKind::LongName_LeftDownTeeVector,               // 347
        TokenKind::LongName_UpEquilibrium,                   // 348
        TokenKind::LongName_ReverseUpEquilibrium,            // 349
        TokenKind::LongName_RoundImplies,                    // 350
        TokenKind::LongName_LeftTriangleBar,                 // 351
        TokenKind::LongName_RightTriangleBar,                // 352
        TokenKind::LongName_Equivalent,                      // 353
        TokenKind::LongName_LessSlantEqual,                  // 354
        TokenKind::LongName_GreaterSlantEqual,               // 355
        TokenKind::LongName_NestedLessLess,                  // 356
        TokenKind::LongName_NestedGreaterGreater,            // 357
        TokenKind::LongName_PrecedesEqual,                   // 358
        TokenKind::LongName_SucceedsEqual,                   // 359
        TokenKind::LongName_DoubleLeftTee,                   // 360
        TokenKind::LongName_LeftDoubleBracket,               // 361
        TokenKind::LongName_RightDoubleBracket,              // 362
        TokenKind::LongName_LeftAssociation,                 // 363
        TokenKind::LongName_RightAssociation,                // 364
        TokenKind::LongName_TwoWayRule,                      // 365
        TokenKind::LongName_Piecewise,                       // 366
        TokenKind::LongName_ImplicitPlus,                    // 367
        TokenKind::LongName_AutoLeftMatch,                   // 368
        TokenKind::LongName_AutoRightMatch,                  // 369
        TokenKind::LongName_InvisiblePrefixScriptBase,       // 370
        TokenKind::LongName_InvisiblePostfixScriptBase,      // 371
        TokenKind::LongName_Transpose,                       // 372
        TokenKind::LongName_Conjugate,                       // 373
        TokenKind::LongName_ConjugateTranspose,              // 374
        TokenKind::LongName_HermitianConjugate,              // 375
        TokenKind::LongName_VerticalBar,                     // 376
        TokenKind::LongName_NotVerticalBar,                  // 377
        TokenKind::LongName_Distributed,                     // 378
        TokenKind::LongName_Conditioned,                     // 379
        TokenKind::LongName_UndirectedEdge,                  // 380
        TokenKind::LongName_DirectedEdge,                    // 381
        TokenKind::LongName_ContinuedFractionK,              // 382
        TokenKind::LongName_TensorProduct,                   // 383
        TokenKind::LongName_TensorWedge,                     // 384
        TokenKind::LongName_ProbabilityPr,                   // 385
        TokenKind::LongName_ExpectationE,                    // 386
        TokenKind::LongName_PermutationProduct,              // 387
        TokenKind::LongName_NotEqualTilde,                   // 388
        TokenKind::LongName_NotHumpEqual,                    // 389
        TokenKind::LongName_NotHumpDownHump,                 // 390
        TokenKind::LongName_NotLeftTriangleBar,              // 391
        TokenKind::LongName_NotRightTriangleBar,             // 392
        TokenKind::LongName_NotLessLess,                     // 393
        TokenKind::LongName_NotNestedLessLess,               // 394
        TokenKind::LongName_NotLessSlantEqual,               // 395
        TokenKind::LongName_NotGreaterGreater,               // 396
        TokenKind::LongName_NotNestedGreaterGreater,         // 397
        TokenKind::LongName_NotGreaterSlantEqual,            // 398
        TokenKind::LongName_NotPrecedesEqual,                // 399
        TokenKind::LongName_NotSucceedsEqual,                // 400
        TokenKind::LongName_NotSquareSubset,                 // 401
        TokenKind::LongName_NotSquareSuperset,               // 402
        TokenKind::LongName_Equal,                           // 403
        TokenKind::LongName_VerticalSeparator,               // 404
        TokenKind::LongName_VectorGreater,                   // 405
        TokenKind::LongName_VectorGreaterEqual,              // 406
        TokenKind::LongName_VectorLess,                      // 407
        TokenKind::LongName_VectorLessEqual,                 // 408
        TokenKind::LongName_Limit,                           // 409
        TokenKind::LongName_MaxLimit,                        // 410
        TokenKind::LongName_MinLimit,                        // 411
        TokenKind::LongName_Cross,                           // 412
        TokenKind::LongName_Function,                        // 413
        TokenKind::LongName_Xnor,                            // 414
        TokenKind::LongName_DiscreteShift,                   // 415
        TokenKind::LongName_DifferenceDelta,                 // 416
        TokenKind::LongName_DiscreteRatio,                   // 417
        TokenKind::LongName_RuleDelayed,                     // 418
        TokenKind::LongName_Square,                          // 419
        TokenKind::LongName_Rule,                            // 420
        TokenKind::LongName_Implies,                         // 421
        TokenKind::LongName_ShortRightArrow,                 // 422
        TokenKind::LongName_ShortLeftArrow,                  // 423
        TokenKind::LongName_ShortUpArrow,                    // 424
        TokenKind::LongName_ShortDownArrow,                  // 425
        TokenKind::LongName_Application,                     // 426
        TokenKind::LongName_LeftBracketingBar,               // 427
        TokenKind::LongName_RightBracketingBar,              // 428
        TokenKind::LongName_LeftDoubleBracketingBar,         // 429
        TokenKind::LongName_RightDoubleBracketingBar,        // 430
        TokenKind::LongName_CapitalDifferentialD,            // 431
        TokenKind::LongName_DifferentialD,                   // 432
        TokenKind::LongName_InvisibleComma,                  // 433
        TokenKind::LongName_InvisibleApplication,            // 434
        TokenKind::LongName_LongEqual,                       // 435
    ];
}