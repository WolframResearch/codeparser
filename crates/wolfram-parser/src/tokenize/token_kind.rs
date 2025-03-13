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

#[test]
fn test_token_kind_property_arrays() {
    let mut named_variants =
        [POSSIBLE_BEGINNING, EMPTY, UNTERMINATED, CLOSERS, ERROR]
            .iter()
            .map(|slice| slice.iter())
            .flatten();

    if let Some(unknown) = named_variants.find(|v| !is_token_kind_variant(v)) {
        panic!("unknown variant: {unknown}")
    }
}

// Ensure that every TokenKind variant has a unique `id` value. I.e. that there
// aren't two variants with the same `id` that differ only by one of their bit
// flags.
#[test]
fn test_token_kinds_are_sorted() {
    for window in TokenKind::VARIANTS.windows(2) {
        let [a, b]: [_; 2] = window.try_into().unwrap();
        if b.id() != a.id() + 1 {
            panic!("TokenKind variant ids are not in order: {a:?}, {b:?}");
        }
    }
}

#[cfg(test)]
fn is_token_kind_variant(name: &str) -> bool {
    TokenKind::VARIANTS
        .iter()
        .find(|v| format!("{v:?}") == name)
        .is_some()
}

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

/// Compute numeric value assigned to each [`TokenKind`] variant.
///
/// [`TokenKind`] values are specially computed instead of assigned
/// automatically by the compiler so that they can encode bit flag properties
/// that can be accessed efficiently.
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
            pub(crate) const VARIANTS: &[TokenKind] = &[
                $( TokenKind::$variant, )*
            ];

            pub const COUNT: usize = TokenKind::VARIANTS.len();

            pub fn from_symbol(symbol: SymbolRef) -> Option<Self> {
                match symbol {
                    $(
                        token_to_symbol!($variant) => Some(TokenKind::$variant),
                    )*
                    _ => None
                }
            }

            pub fn to_symbol(&self) -> Symbol {
                match self {
                    $(
                        TokenKind::$variant => token_to_symbol!($variant),
                    )*
                }
            }
        }
    };
}

#[rustfmt::skip]
macro_rules! token_to_symbol {
    //
    // Use the System` context symbols for literals when we can
    //
    (EndOfFile) => { st::EndOfFile };

    (Symbol) => { st::Symbol };
    (String) => { st::String };
    (Integer) => { st::Integer };
    (Real) => { st::Real };
    (Rational) => { st::Rational };

    (Whitespace) => { st::Whitespace };

    // For parser code, there is a distinction between toplevel and internal newlines
    //
    // But once a token Expr is actually created, just treat them both the same
    (ToplevelNewline) => { st::Token::Newline };
    (InternalNewline) => { st::Token::Newline };

    // Everything else will be Token`<variant>
    ($ident:ident) => {
        st::Token::$ident
    };
}

//======================================
// All token enum variants
//======================================

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
