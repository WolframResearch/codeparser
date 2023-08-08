use crate::{
    source::{NextPolicy, NextPolicyBits::RETURN_TOPLEVELNEWLINE},
    symbol::Symbol,
    utils::contains,
};

use wolfram_expr::symbol::SymbolRef;

//======================================
// Computing TokenKind variant value
//======================================

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

const POSSIBLE_BEGINNING: &[&str] = &[
    "Symbol",
    "String",
    "Integer",
    "Real",
    "Rational",
    "LinearSyntaxBlob",
    "Percent",
    "PercentPercent",
    "Hash",
    "HashHash",
    "Under",
    "UnderUnder",
    "UnderUnderUnder",
    "UnderDot",
    "SemiSemi",
    //
    // Prefix operators
    //
    "Bang",
    "Minus",
    "Plus",
    "LessLess",
    "MinusMinus",
    "PlusPlus",
    "BangBang",
    //
    // Openers
    //
    "OpenParen",
    "OpenCurly",
    "LessBar",
    "LongName_LeftCeiling",
    "LongName_LeftFloor",
    "LongName_LeftAngleBracket",
    "LongName_LeftBracketingBar",
    "LongName_LeftDoubleBracketingBar",
    "LongName_LeftAssociation",
    "LongName_OpenCurlyQuote",
    "LongName_OpenCurlyDoubleQuote",
    //
    // these openers are Call syntax and NOT possible beginning
    //
    // "OpenSquare"
    // | "ColonColonOpenSquare"
    // | "LongName_LeftDoubleBracket"
    //

    //
    // Prefix linear syntax operators
    //
    "LinearSyntax_Bang",
    //
    // Integration operators
    //
    "LongName_Integral",
    "LongName_ContourIntegral",
    "LongName_DoubleContourIntegral",
    "LongName_ClockwiseContourIntegral",
    "LongName_CounterClockwiseContourIntegral",
    //
    // Prefix LongName operators
    //
    "LongName_Not",
    "LongName_PlusMinus",
    "LongName_Sum",
    "LongName_ForAll",
    "LongName_Exists",
    "LongName_NotExists",
    "LongName_Del",
    "LongName_Product",
    "LongName_Coproduct",
    "LongName_Minus",
    "LongName_MinusPlus",
    "LongName_Sqrt",
    "LongName_CubeRoot",
    "LongName_CircleTimes",
    "LongName_Piecewise",
    "LongName_InvisiblePrefixScriptBase",
    "LongName_ContinuedFractionK",
    "LongName_ProbabilityPr",
    "LongName_ExpectationE",
    "LongName_CapitalDifferentialD",
    "LongName_DifferentialD",
    "LongName_Square",
];

const EMPTY: &[&str] = &[
    // EndOfFile is not empty
    // It is a single byte 0xff
    // FIXME: Update the Rust port to use this optimization (and benchmark it)
    "EndOfFile",
    "Fake_ImplicitTimes",
    "Error_Aborted",
    "Fake_ImplicitNull",
    "Fake_ImplicitOne",
    "Fake_ImplicitAll",
    "Error_ExpectedOperand",
    "Error_ExpectedTag",
    "Error_ExpectedFile",
    "Error_PrefixImplicitNull",
    "Error_InfixImplicitNull",
    //
    // Newlines are not empty
    //
    // Token`ToplevelNewline
    // Token`InternalNewline
];

const UNTERMINATED: &[&str] = &[
    "Error_UnterminatedComment",
    "Error_UnterminatedString",
    "Error_UnterminatedFileString",
    "Error_UnterminatedLinearSyntaxBlob",
];

const CLOSERS: &[&str] = &[
    "BarGreater",
    "CloseCurly",
    "CloseParen",
    "CloseSquare",
    "LongName_CloseCurlyDoubleQuote",
    "LongName_CloseCurlyQuote",
    "LongName_RightAngleBracket",
    "LongName_RightAssociation",
    "LongName_RightBracketingBar",
    "LongName_RightCeiling",
    "LongName_RightDoubleBracket",
    "LongName_RightDoubleBracketingBar",
    "LongName_RightFloor",
];

const ERROR: &[&str] = &[
    "Error_Unknown",
    "Error_ExpectedEqual",
    "Error_Number",
    "Error_UnhandledCharacter",
    "Error_ExpectedLetterlike",
    "Error_Aborted",
    "Error_ExpectedOperand",
    "Error_ExpectedTag",
    "Error_ExpectedFile",
    "Error_UnsupportedToken",
    "Error_UnexpectedCloser",
    "Error_UnterminatedComment",
    "Error_UnterminatedString",
    "Error_UnterminatedFileString",
    "Error_UnterminatedLinearSyntaxBlob",
    "Error_PrefixImplicitNull",
    "Error_InfixImplicitNull",
    "Error_UnsafeCharacterEncoding",
    "Error_UnexpectedCommentCloser",
];

const fn is_possible_beginning(variant: &str) -> bool {
    contains(POSSIBLE_BEGINNING, variant)
}

const fn is_empty(variant: &str) -> bool {
    contains(EMPTY, variant)
}

const fn is_unterminated(variant: &str) -> bool {
    contains(UNTERMINATED, variant)
}

const fn is_closer(variant: &str) -> bool {
    contains(CLOSERS, variant)
}

const fn is_error(variant: &str) -> bool {
    contains(ERROR, variant)
}

#[rustfmt::skip]
const fn variant(id: u16, name: &str) -> u16 {
    let group1 = if is_possible_beginning(name) {
        Group1::PossibleBeginning
    } else if is_closer(name) {
        Group1::Closer
    } else if is_error(name) {
        Group1::Error
    } else {
        Group1::None
    };

    let group2 = if is_empty(name) {
        Group2::Empty
    } else if is_unterminated(name) {
        Group2::Unterminated
    } else {
        Group2::None
    };

    //
    // Construct the `u16` representation
    //

    let group1 = group1 as u16;
    let group2 = group2 as u16;

    // The unique id should only use the first 9 bits.
    // Group1 uses the next two bits, and Group2 the two bits after that.
    debug_assert!(id     & 0b0000_0001_1111_1111 == id);
    debug_assert!(group1 & 0b0000_0110_0000_0000 == group1);
    debug_assert!(group2 & 0b0001_1000_0000_0000 == group2);

    id | group1 | group2
}

/// Used to define the [`TokenKind`] enum.
macro_rules! token_kind {
    (
        $(
            $( #[$cfgs:meta] )*
            $variant:ident = $id:literal

        ),* $(,)?
    ) => {
        /// Complete enumeration of all tokens in Wolfram Language
        #[allow(non_camel_case_types)]
        #[derive(Debug, Copy, Clone, PartialEq)]
        #[repr(u16)]
        pub enum TokenKind {
            $(
                $( #[$cfgs] )*
                $variant = variant($id, stringify!($variant)),
            )*
        }

        impl TokenKind {
            pub const VARIANTS: &[TokenKind] = &[
                $( TokenKind::$variant, )*
            ];

            pub const COUNT: usize = TokenKind::VARIANTS.len();
        }
    };
}

//
// All token enum variants
//

token_kind! {
    Unknown                                  = 0,
    EndOfFile                                = 1,
    Symbol                                   = 2,
    String                                   = 3,
    Integer                                  = 4,
    Real                                     = 5,
    Rational                                 = 6,
    LinearSyntaxBlob                         = 7,

    // trivia
    //
    // Any Buffers before trivia and any Buffers after trivia serve the purpose of
    // allowing fast testing of trivia (just a bit mask)
    InternalNewline                          = 8, // 8
    Comment                                  = 9,
    Whitespace                               = 10,
    Buffer1                                  = 11,
    ToplevelNewline                          = 12,

    Buffer2                                  = 13,
    Buffer3                                  = 14,
    Buffer4                                  = 15,

    //----------------------------------
    // errors
    //----------------------------------

    Error_ExpectedEqual                      = 16, // 16
    Error_Number                             = 17,
    Error_UnhandledCharacter                 = 18,
    Error_ExpectedLetterlike                 = 19,
    Error_Aborted                            = 20,
    Error_ExpectedOperand                    = 21,
    Error_ExpectedTag                        = 22,
    Error_ExpectedFile                       = 23,
    Error_UnexpectedCloser                   = 24,
    /// Implicit `Null` in `f[,2]`
    Error_PrefixImplicitNull                 = 25,
    /// Implicit `Null` in `f[1,]`
    Error_InfixImplicitNull                  = 26,

    Error_UnsafeCharacterEncoding            = 27,

    // Unterminated errors
    //
    // Any Buffers before trivia and any Buffers after trivia serve the purpose
    // of giving the correct values to Token`InternalNewline and
    // Token`ToplevelNewline so that the single bit 0b100 can be set to turn
    // Token`InternalNewline into Token`ToplevelNewline while also allowing fast
    // testing of trivia (just a bit mask) and also fast testing of
    // non-ToplevelNewline trivia (also just a bit mask)
    Error_UnterminatedComment                = 28, // 28
    Error_UnterminatedString                 = 29,
    Error_UnterminatedFileString             = 30,
    Error_UnterminatedLinearSyntaxBlob       = 31,
    Error_UnsupportedToken                   = 32, // 32
    Error_UnexpectedCommentCloser            = 33,

    //----------------------------------
    // 1 character tokens
    //----------------------------------

    /** `.` */ Dot                           = 34,
    /** `:` */ Colon                         = 35,
    /** `(` */ OpenParen                     = 36,
    /** `)` */ CloseParen                    = 37,
    /** `[` */ OpenSquare                    = 38,
    /** `]` */ CloseSquare                   = 39,
    /** `,` */ Comma                         = 40,
    /** `{` */ OpenCurly                     = 41,
    /** `}` */ CloseCurly                    = 42,
    /** `=` */ Equal                         = 43,
    /** `!` */ Bang                          = 44,
    /** `_` */ Under                         = 45,
    /** `<` */ Less                          = 46,
    /** `>` */ Greater                       = 47,
    /** `-` */ Minus                         = 48,
    /** `|` */ Bar                           = 49,
    /** `;` */ Semi                          = 50,
    /** `#` */ Hash                          = 51,
    /** `&` */ Amp                           = 52,
    /** `/` */ Slash                         = 53,
    /** `@` */ At                            = 54,
    /** `+` */ Plus                          = 55,
    /** `~` */ Tilde                         = 56,
    /** `*` */ Star                          = 57,
    /** `^` */ Caret                         = 58,
    /** `'` */ SingleQuote                   = 59,
    /** `%` */ Percent                       = 60,
    /** `?` */ Question                      = 61,

    //----------------------------------
    // 2 character tokens
    //----------------------------------

    /** `..` */ DotDot                       = 62,
    /** `::` */ ColonColon                   = 63,
    /** `:=` */ ColonEqual                   = 64,
    /** `:>` */ ColonGreater                 = 65,
    /** `==` */ EqualEqual                   = 66,
    /** `__` */ UnderUnder                   = 67,
    /** `_.` */ UnderDot                     = 68,
    /** `<|` */ LessBar                      = 69,
    /** `<<` */ LessLess                     = 70,
    /** `<>` */ LessGreater                  = 71,
    /** `<=` */ LessEqual                    = 72,
    /** `>>` */ GreaterGreater               = 73,
    /** `>=` */ GreaterEqual                 = 74,
    /** `->` */ MinusGreater                 = 75,
    /** `--` */ MinusMinus                   = 76,
    /** `-=` */ MinusEqual                   = 77,
    /** `||` */ BarBar                       = 78,
    /** `|>` */ BarGreater                   = 79,
    /** `;;` */ SemiSemi                     = 80,
    /** `&&` */ AmpAmp                       = 81,
    /** `/@` */ SlashAt                      = 82,
    /** `/;` */ SlashSemi                    = 83,
    /** `/.` */ SlashDot                     = 84,
    /** `//` */ SlashSlash                   = 85,
    /** `/;` */ SlashColon                   = 86,
    /** `/=` */ SlashEqual                   = 87,
    /// `/*`
    SlashStar                                = 88,
    /** `@@` */ AtAt                         = 89,
    /** `@*` */ AtStar                       = 90,
    /** `++` */ PlusPlus                     = 91,
    /** `+=` */ PlusEqual                    = 92,
    /** `~~` */ TildeTilde                   = 93,
    /** `*=` */ StarEqual                    = 94,
    /** `**` */ StarStar                     = 95,
    /** `^=` */ CaretEqual                   = 96,
    /** `##` */ HashHash                     = 97,
    /** `!=` */ BangEqual                    = 98,
    /// `!!`
    ///
    /// `!!` is a real token: postfix for `Factorial2`,
    /// so when prefix `!!` is encountered, it is convenient to also treat it as
    /// a single token
    ///
    /// `!!a` is `Not[Not[a]]`
    BangBang                                 = 99,
    /** `??` */ QuestionQuestion             = 100,

    //----------------------------------
    // 3 character tokens
    //----------------------------------

    /** `...` */ DotDotDot                   = 101,
    /** `===` */ EqualEqualEqual             = 102,
    /** `=!=` */ EqualBangEqual              = 103,
    /** `___` */ UnderUnderUnder             = 104,
    /** `//.` */ SlashSlashDot               = 105,
    /** `@@@` */ AtAtAt                      = 106,
    /** `<->` */ LessMinusGreater            = 107,
    /** `//@` */ SlashSlashAt                = 108,
    /** `^:=` */ CaretColonEqual             = 109,
    /** `>>>` */ GreaterGreaterGreater       = 110,
    /// `|->` — new in 12.2
    BarMinusGreater                          = 111,
    /// `//=` — new in 12.2
    SlashSlashEqual                          = 112,
    /// `::[` — new in 13.1
    ColonColonOpenSquare                     = 113,

    //----------------------------------
    // variable length character tokens
    //----------------------------------

    /** `%%` */ PercentPercent               = 114,

    //----------------------------------
    // Linear syntax tokens
    //----------------------------------

    /** `\!` */ LinearSyntax_Bang            = 115,
    /** `\)` */ LinearSyntax_CloseParen      = 116,
    /** `\@` */ LinearSyntax_At              = 117,
    /** `\&` */ LinearSyntax_Amp             = 118,
    /** `\*` */ LinearSyntax_Star            = 119,
    /** `\_` */ LinearSyntax_Under           = 120,
    /** `\^` */ LinearSyntax_Caret           = 121,
    /** `\ ` */ LinearSyntax_Space           = 122,
    /** `\%` */ LinearSyntax_Percent         = 123,
    /** `\+` */ LinearSyntax_Plus            = 124,
    /** `\/` */ LinearSyntax_Slash           = 125,
    /** `` \` `` */ LinearSyntax_BackTick    = 126,

    //----------------------------------
    // Fake tokens
    //----------------------------------

    /// Implicit `Times` operator in `a b`
    Fake_ImplicitTimes                       = 127,
    /// Implicit `Null` in `a;`
    Fake_ImplicitNull                        = 128,
    /// Implicit `1` in `;;b`
    Fake_ImplicitOne                         = 129,
    /// Implicit `All` in `a;;`
    Fake_ImplicitAll                         = 130,

    /// Used when parsing boxes
    ///
    /// The FE treats `(*` and `*)` as tokens
    Boxes_OpenParenStar                      = 131,

    //
    // variable length character tokens
    //

    /// The FE treats `*****)` as a single token
    Boxes_StarCloseParen                     = 132,
    /// The FE treats `''''` as a single token
    Boxes_MultiSingleQuote                   = 133,
    /// The FE treats `<space><space><space>` as a single token
    Boxes_MultiWhitespace                    = 134,

    // Token`Boxes`LongName`LeftSkeleton -> Next,
    // Token`Boxes`LongName`RightSkeleton -> Next,

    // (*
    // Parsing  f.m  as a leaf from the front end (from example input such as <<f.m)
    // *)
    // (*Token`Other -> Next,*)

    //----------------------------------
    // All multi-byte character tokens
    //
    // Luckily, they all have long names to use for identification
    //----------------------------------

    LongName_Not                             = 135,
    LongName_PlusMinus                       = 136,
    LongName_CenterDot                       = 137,
    LongName_Times                           = 138,
    LongName_Divide                          = 139,
    LongName_OpenCurlyQuote                  = 140,
    LongName_CloseCurlyQuote                 = 141,
    LongName_OpenCurlyDoubleQuote            = 142,
    LongName_CloseCurlyDoubleQuote           = 143,
    LongName_InvisibleTimes                  = 144,
    LongName_LeftArrow                       = 145,
    LongName_UpArrow                         = 146,
    LongName_RightArrow                      = 147,
    LongName_DownArrow                       = 148,
    LongName_LeftRightArrow                  = 149,
    LongName_UpDownArrow                     = 150,
    LongName_UpperLeftArrow                  = 151,
    LongName_UpperRightArrow                 = 152,
    LongName_LowerRightArrow                 = 153,
    LongName_LowerLeftArrow                  = 154,
    LongName_LeftTeeArrow                    = 155,
    LongName_UpTeeArrow                      = 156,
    LongName_RightTeeArrow                   = 157,
    LongName_DownTeeArrow                    = 158,
    LongName_LeftVector                      = 159,
    LongName_DownLeftVector                  = 160,
    LongName_RightUpVector                   = 161,
    LongName_LeftUpVector                    = 162,
    LongName_RightVector                     = 163,
    LongName_DownRightVector                 = 164,
    LongName_RightDownVector                 = 165,
    LongName_LeftDownVector                  = 166,
    LongName_RightArrowLeftArrow             = 167,
    LongName_UpArrowDownArrow                = 168,
    LongName_LeftArrowRightArrow             = 169,
    LongName_ReverseEquilibrium              = 170,
    LongName_Equilibrium                     = 171,
    LongName_DoubleLeftArrow                 = 172,
    LongName_DoubleUpArrow                   = 173,
    LongName_DoubleRightArrow                = 174,
    LongName_DoubleDownArrow                 = 175,
    LongName_DoubleLeftRightArrow            = 176,
    LongName_DoubleUpDownArrow               = 177,
    LongName_LeftArrowBar                    = 178,
    LongName_RightArrowBar                   = 179,
    LongName_DownArrowUpArrow                = 180,
    LongName_ForAll                          = 181,
    LongName_PartialD                        = 182,
    LongName_Exists                          = 183,
    LongName_NotExists                       = 184,
    LongName_Del                             = 185,
    LongName_Element                         = 186,
    LongName_NotElement                      = 187,
    LongName_ReverseElement                  = 188,
    LongName_NotReverseElement               = 189,
    LongName_SuchThat                        = 190,
    LongName_Product                         = 191,
    LongName_Coproduct                       = 192,
    LongName_Sum                             = 193,
    LongName_Minus                           = 194,
    LongName_MinusPlus                       = 195,
    LongName_DivisionSlash                   = 196,
    LongName_Backslash                       = 197,
    LongName_SmallCircle                     = 198,
    LongName_Sqrt                            = 199,
    LongName_CubeRoot                        = 200,
    LongName_Proportional                    = 201,
    LongName_Divides                         = 202,
    LongName_DoubleVerticalBar               = 203,
    LongName_NotDoubleVerticalBar            = 204,
    LongName_And                             = 205,
    LongName_Or                              = 206,
    LongName_Integral                        = 207,
    LongName_ContourIntegral                 = 208,
    LongName_DoubleContourIntegral           = 209,
    LongName_ClockwiseContourIntegral        = 210,
    LongName_CounterClockwiseContourIntegral = 211,
    LongName_Therefore                       = 212,
    LongName_Because                         = 213,
    LongName_Colon                           = 214,
    LongName_Proportion                      = 215,
    LongName_Tilde                           = 216,
    LongName_VerticalTilde                   = 217,
    LongName_NotTilde                        = 218,
    LongName_EqualTilde                      = 219,
    LongName_TildeEqual                      = 220,
    LongName_NotTildeEqual                   = 221,
    LongName_TildeFullEqual                  = 222,
    LongName_NotTildeFullEqual               = 223,
    LongName_TildeTilde                      = 224,
    LongName_NotTildeTilde                   = 225,
    LongName_CupCap                          = 226,
    LongName_HumpDownHump                    = 227,
    LongName_HumpEqual                       = 228,
    LongName_DotEqual                        = 229,
    LongName_NotEqual                        = 230,
    LongName_Congruent                       = 231,
    LongName_NotCongruent                    = 232,
    LongName_LessEqual                       = 233,
    LongName_GreaterEqual                    = 234,
    LongName_LessFullEqual                   = 235,
    LongName_GreaterFullEqual                = 236,
    LongName_NotLessFullEqual                = 237,
    LongName_NotGreaterFullEqual             = 238,
    LongName_LessLess                        = 239,
    LongName_GreaterGreater                  = 240,
    LongName_NotCupCap                       = 241,
    LongName_NotLess                         = 242,
    LongName_NotGreater                      = 243,
    LongName_NotLessEqual                    = 244,
    LongName_NotGreaterEqual                 = 245,
    LongName_LessTilde                       = 246,
    LongName_GreaterTilde                    = 247,
    LongName_NotLessTilde                    = 248,
    LongName_NotGreaterTilde                 = 249,
    LongName_LessGreater                     = 250,
    LongName_GreaterLess                     = 251,
    LongName_NotLessGreater                  = 252,
    LongName_NotGreaterLess                  = 253,
    LongName_Precedes                        = 254,
    LongName_Succeeds                        = 255,
    LongName_PrecedesSlantEqual              = 256,
    LongName_SucceedsSlantEqual              = 257,
    LongName_PrecedesTilde                   = 258,
    LongName_SucceedsTilde                   = 259,
    LongName_NotPrecedes                     = 260,
    LongName_NotSucceeds                     = 261,
    LongName_Subset                          = 262,
    LongName_Superset                        = 263,
    LongName_NotSubset                       = 264,
    LongName_NotSuperset                     = 265,
    LongName_SubsetEqual                     = 266,
    LongName_SupersetEqual                   = 267,
    LongName_NotSubsetEqual                  = 268,
    LongName_NotSupersetEqual                = 269,
    LongName_UnionPlus                       = 270,
    LongName_SquareSubset                    = 271,
    LongName_SquareSuperset                  = 272,
    LongName_SquareSubsetEqual               = 273,
    LongName_SquareSupersetEqual             = 274,
    LongName_SquareIntersection              = 275,
    LongName_SquareUnion                     = 276,
    LongName_CirclePlus                      = 277,
    LongName_CircleMinus                     = 278,
    LongName_CircleTimes                     = 279,
    LongName_CircleDot                       = 280,
    LongName_RightTee                        = 281,
    LongName_LeftTee                         = 282,
    LongName_DownTee                         = 283,
    LongName_UpTee                           = 284,
    LongName_DoubleRightTee                  = 285,
    LongName_LeftTriangle                    = 286,
    LongName_RightTriangle                   = 287,
    LongName_LeftTriangleEqual               = 288,
    LongName_RightTriangleEqual              = 289,
    LongName_Xor                             = 290,
    LongName_Nand                            = 291,
    LongName_Nor                             = 292,
    LongName_Wedge                           = 293,
    LongName_Vee                             = 294,
    LongName_Intersection                    = 295,
    LongName_Union                           = 296,
    LongName_Diamond                         = 297,
    LongName_Star                            = 298,
    LongName_LessEqualGreater                = 299,
    LongName_GreaterEqualLess                = 300,
    LongName_NotPrecedesSlantEqual           = 301,
    LongName_NotSucceedsSlantEqual           = 302,
    LongName_NotSquareSubsetEqual            = 303,
    LongName_NotSquareSupersetEqual          = 304,
    LongName_NotPrecedesTilde                = 305,
    LongName_NotSucceedsTilde                = 306,
    LongName_NotLeftTriangle                 = 307,
    LongName_NotRightTriangle                = 308,
    LongName_NotLeftTriangleEqual            = 309,
    LongName_NotRightTriangleEqual           = 310,
    LongName_LeftCeiling                     = 311,
    LongName_RightCeiling                    = 312,
    LongName_LeftFloor                       = 313,
    LongName_RightFloor                      = 314,
    LongName_Cap                             = 315,
    LongName_Cup                             = 316,
    LongName_LeftAngleBracket                = 317,
    LongName_RightAngleBracket               = 318,
    LongName_Perpendicular                   = 319,
    LongName_LongLeftArrow                   = 320,
    LongName_LongRightArrow                  = 321,
    LongName_LongLeftRightArrow              = 322,
    LongName_DoubleLongLeftArrow             = 323,
    LongName_DoubleLongRightArrow            = 324,
    LongName_DoubleLongLeftRightArrow        = 325,
    LongName_UpArrowBar                      = 326,
    LongName_DownArrowBar                    = 327,
    LongName_LeftRightVector                 = 328,
    LongName_RightUpDownVector               = 329,
    LongName_DownLeftRightVector             = 330,
    LongName_LeftUpDownVector                = 331,
    LongName_LeftVectorBar                   = 332,
    LongName_RightVectorBar                  = 333,
    LongName_RightUpVectorBar                = 334,
    LongName_RightDownVectorBar              = 335,
    LongName_DownLeftVectorBar               = 336,
    LongName_DownRightVectorBar              = 337,
    LongName_LeftUpVectorBar                 = 338,
    LongName_LeftDownVectorBar               = 339,
    LongName_LeftTeeVector                   = 340,
    LongName_RightTeeVector                  = 341,
    LongName_RightUpTeeVector                = 342,
    LongName_RightDownTeeVector              = 343,
    LongName_DownLeftTeeVector               = 344,
    LongName_DownRightTeeVector              = 345,
    LongName_LeftUpTeeVector                 = 346,
    LongName_LeftDownTeeVector               = 347,
    LongName_UpEquilibrium                   = 348,
    LongName_ReverseUpEquilibrium            = 349,
    LongName_RoundImplies                    = 350,
    LongName_LeftTriangleBar                 = 351,
    LongName_RightTriangleBar                = 352,
    LongName_Equivalent                      = 353,
    LongName_LessSlantEqual                  = 354,
    LongName_GreaterSlantEqual               = 355,
    LongName_NestedLessLess                  = 356,
    LongName_NestedGreaterGreater            = 357,
    LongName_PrecedesEqual                   = 358,
    LongName_SucceedsEqual                   = 359,
    LongName_DoubleLeftTee                   = 360,
    LongName_LeftDoubleBracket               = 361,
    LongName_RightDoubleBracket              = 362,
    LongName_LeftAssociation                 = 363,
    LongName_RightAssociation                = 364,
    LongName_TwoWayRule                      = 365,
    LongName_Piecewise                       = 366,
    LongName_ImplicitPlus                    = 367,
    LongName_AutoLeftMatch                   = 368,
    LongName_AutoRightMatch                  = 369,
    LongName_InvisiblePrefixScriptBase       = 370,
    LongName_InvisiblePostfixScriptBase      = 371,
    LongName_Transpose                       = 372,
    LongName_Conjugate                       = 373,
    LongName_ConjugateTranspose              = 374,
    LongName_HermitianConjugate              = 375,
    LongName_VerticalBar                     = 376,
    LongName_NotVerticalBar                  = 377,
    LongName_Distributed                     = 378,
    LongName_Conditioned                     = 379,
    LongName_UndirectedEdge                  = 380,
    LongName_DirectedEdge                    = 381,
    LongName_ContinuedFractionK              = 382,
    LongName_TensorProduct                   = 383,
    LongName_TensorWedge                     = 384,
    LongName_ProbabilityPr                   = 385,
    LongName_ExpectationE                    = 386,
    LongName_PermutationProduct              = 387,
    LongName_NotEqualTilde                   = 388,
    LongName_NotHumpEqual                    = 389,
    LongName_NotHumpDownHump                 = 390,
    LongName_NotLeftTriangleBar              = 391,
    LongName_NotRightTriangleBar             = 392,
    LongName_NotLessLess                     = 393,
    LongName_NotNestedLessLess               = 394,
    LongName_NotLessSlantEqual               = 395,
    LongName_NotGreaterGreater               = 396,
    LongName_NotNestedGreaterGreater         = 397,
    LongName_NotGreaterSlantEqual            = 398,
    LongName_NotPrecedesEqual                = 399,
    LongName_NotSucceedsEqual                = 400,
    LongName_NotSquareSubset                 = 401,
    LongName_NotSquareSuperset               = 402,
    LongName_Equal                           = 403,
    LongName_VerticalSeparator               = 404,
    LongName_VectorGreater                   = 405,
    LongName_VectorGreaterEqual              = 406,
    LongName_VectorLess                      = 407,
    LongName_VectorLessEqual                 = 408,
    LongName_Limit                           = 409,
    LongName_MaxLimit                        = 410,
    LongName_MinLimit                        = 411,
    LongName_Cross                           = 412,
    LongName_Function                        = 413,
    LongName_Xnor                            = 414,
    LongName_DiscreteShift                   = 415,
    LongName_DifferenceDelta                 = 416,
    LongName_DiscreteRatio                   = 417,
    LongName_RuleDelayed                     = 418,
    LongName_Square                          = 419,
    LongName_Rule                            = 420,
    LongName_Implies                         = 421,
    LongName_ShortRightArrow                 = 422,
    LongName_ShortLeftArrow                  = 423,
    LongName_ShortUpArrow                    = 424,
    LongName_ShortDownArrow                  = 425,
    LongName_Application                     = 426,
    LongName_LeftBracketingBar               = 427,
    LongName_RightBracketingBar              = 428,
    LongName_LeftDoubleBracketingBar         = 429,
    LongName_RightDoubleBracketingBar        = 430,
    LongName_CapitalDifferentialD            = 431,
    LongName_DifferentialD                   = 432,
    LongName_InvisibleComma                  = 433,
    LongName_InvisibleApplication            = 434,
    LongName_LongEqual                       = 435,
}

impl TokenKind {
    pub const fn bits(self) -> u16 {
        let bits: u16 = self as u16;
        return bits;
    }

    // TODO: This is only used with TOKEN_COUNT -- remove this?
    pub(crate) const fn id(self) -> u16 {
        let value: u16 = self as u16;
        return value & 0x1ff;
    }

    /// Returns either [`TokenKind::ToplevelNewline`] or [`TokenKind::InternalNewline`]
    pub(crate) fn newline_with_policy(policy: NextPolicy) -> Self {
        if policy.contains(RETURN_TOPLEVELNEWLINE) {
            return TokenKind::ToplevelNewline;
        } else {
            TokenKind::InternalNewline
        }
    }

    //
    // All trivia matches: 0b0_0000_1xxx (x is unknown)
    //
    //         Mask off 0b1_1111_1000 (0x1f8)
    // And test against 0b0_0000_1000 (0x08)
    //
    pub const fn isTrivia(self) -> bool {
        return (self.bits() & 0x1f8) == 0x08;
    }

    //
    // All trivia but ToplevelNewline matches: 0b0_0000_10xx (x is unknown)
    //
    //         Mask off 0b1_1111_1100 (0x1fc)
    // And test against 0b0_0000_1000 (0x08)
    //
    pub const fn isTriviaButNotToplevelNewline(self) -> bool {
        return (self.bits() & 0x1fc) == 0x08;
    }

    pub const fn isPossibleBeginning(self) -> bool {
        return self.bits() & Group1::MASK == Group1::PossibleBeginning as u16;
    }

    pub const fn isCloser(self) -> bool {
        return self.bits() & Group1::MASK == Group1::Closer as u16;
    }

    pub const fn isError(self) -> bool {
        return self.bits() & Group1::MASK == Group1::Error as u16;
    }

    pub const fn isUnterminated(self) -> bool {
        return self.bits() & Group2::MASK == Group2::Unterminated as u16;
    }

    pub const fn isEmpty(self) -> bool {
        return self.bits() & Group2::MASK == Group2::Empty as u16;
    }
}



//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

use crate::symbols as st;

//
// TokenKind::Integer must be 0x4 to allow setting the 0b1 bit to convert to TokenKind::REAL, and 0b10 bit to convert to TokenKind::Rational
//
const _: () = assert!(TokenKind::Integer.id() == 0x4);
const _: () = assert!(TokenKind::Real.id() == 0x5);
const _: () = assert!(TokenKind::Rational.id() == 0x6);

//
// TokenKind::InternalNewline must be 0x8 to allow setting the 0b100 bit to convert to TokenKind::ToplevelNewline
//
const _: () = assert!(TokenKind::InternalNewline.id() == 0b1000,);
const _: () = assert!(TokenKind::ToplevelNewline.id() == 0b1100,);
//const _: () = assert!(TokenKind::Error_First.id() == 0x10, "Check your assumptions");

/// All group closers
#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Closer {
    BarGreater,
    CloseCurly,
    CloseParen,
    CloseSquare,
    LongName_CloseCurlyDoubleQuote,
    LongName_CloseCurlyQuote,
    LongName_RightAngleBracket,
    LongName_RightAssociation,
    LongName_RightBracketingBar,
    LongName_RightCeiling,
    LongName_RightDoubleBracket,
    LongName_RightDoubleBracketingBar,
    LongName_RightFloor,
    // UNUSED
    AssertFalse,
}


#[allow(dead_code)]
#[rustfmt::skip]
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

#[rustfmt::skip]
#[allow(dead_code)]
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

//======================================
// Verify some TokenKind properties
//======================================

const _: () = assert!(TokenKind::EndOfFile.isEmpty());

#[test]
fn test_newline_policy() {
    assert_eq!(
        TokenKind::newline_with_policy(RETURN_TOPLEVELNEWLINE),
        TokenKind::ToplevelNewline
    );
}

//======================================
// Closers
//======================================

#[rustfmt::skip]
pub(crate) const fn GroupOpenerToCloser(token: TokenKind) -> Closer {
    match token {
        TokenKind::ColonColonOpenSquare => Closer::CloseSquare,
        TokenKind::LongName_LeftAngleBracket => Closer::LongName_RightAngleBracket,
        TokenKind::LongName_LeftAssociation => Closer::LongName_RightAssociation,
        TokenKind::LongName_LeftBracketingBar => Closer::LongName_RightBracketingBar,
        TokenKind::LongName_LeftCeiling => Closer::LongName_RightCeiling,
        TokenKind::LongName_LeftDoubleBracket => Closer::LongName_RightDoubleBracket,
        TokenKind::LongName_LeftDoubleBracketingBar => Closer::LongName_RightDoubleBracketingBar,
        TokenKind::LongName_LeftFloor => Closer::LongName_RightFloor,
        TokenKind::LessBar => Closer::BarGreater,
        TokenKind::OpenCurly => Closer::CloseCurly,
        TokenKind::LongName_OpenCurlyDoubleQuote => Closer::LongName_CloseCurlyDoubleQuote,
        TokenKind::LongName_OpenCurlyQuote => Closer::LongName_CloseCurlyQuote,
        TokenKind::OpenParen => Closer::CloseParen,
        TokenKind::OpenSquare => Closer::CloseSquare,
        _ => panic!("Unhandled token"),
    }
}

#[rustfmt::skip]
pub(crate) fn TokenToCloser(token: TokenKind) -> Closer {
    match token {
        TokenKind::BarGreater => Closer::BarGreater,
        TokenKind::CloseCurly => Closer::CloseCurly,
        TokenKind::LongName_CloseCurlyDoubleQuote => Closer::LongName_CloseCurlyDoubleQuote,
        TokenKind::LongName_CloseCurlyQuote => Closer::LongName_CloseCurlyQuote,
        TokenKind::CloseParen => Closer::CloseParen,
        TokenKind::CloseSquare => Closer::CloseSquare,
        TokenKind::LongName_RightAngleBracket => Closer::LongName_RightAngleBracket,
        TokenKind::LongName_RightAssociation => Closer::LongName_RightAssociation,
        TokenKind::LongName_RightBracketingBar => Closer::LongName_RightBracketingBar,
        TokenKind::LongName_RightCeiling => Closer::LongName_RightCeiling,
        TokenKind::LongName_RightDoubleBracket => Closer::LongName_RightDoubleBracket,
        TokenKind::LongName_RightDoubleBracketingBar => Closer::LongName_RightDoubleBracketingBar,
        TokenKind::LongName_RightFloor => Closer::LongName_RightFloor,
        _ => Closer::AssertFalse,
    }
}
