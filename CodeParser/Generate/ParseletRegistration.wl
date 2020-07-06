BeginPackage["CodeParser`Generate`ParseletRegistration`"]

LeafParselet
PrefixNullPointerParselet
PrefixCloserParselet
PrefixErrorParselet
PrefixEndOfFileParselet
PrefixUnhandledParselet
PrefixCommaParselet
PrefixUnsupportedTokenParselet

InfixToplevelNewlineParselet
InfixNullPointerParselet
InfixAssertFalseParselet
InfixDifferentialDParselet
InfixImplicitTimesParselet

PrefixOperatorParselet
GroupParselet
BinaryOperatorParselet
InfixOperatorParselet
PostfixOperatorParselet
CallParselet

IntegralParselet
CommaParselet
SemiParselet
SemiSemiParselet
LessLessParselet
LinearSyntaxOpenParenParselet
SlashColonParselet
SymbolParselet
TildeParselet
UnderDotParselet
UnderParselet
HashParselet
HashHashParselet
PercentParselet
PercentPercentParselet
ColonColonParselet
ColonEqualParselet
GreaterGreaterParselet
ColonParselet
EqualParselet
GreaterGreaterGreaterParselet

PrefixOperatorToParselet
InfixOperatorToParselet

Begin["`Private`"]

Needs["CodeParser`Generate`TokenEnum`"]
Needs["CodeParser`Generate`GenerateSources`"]



PrefixOperatorToParselet[Token`String] = LeafParselet[]
PrefixOperatorToParselet[Token`Integer] = LeafParselet[]
PrefixOperatorToParselet[Token`Real] = LeafParselet[]
PrefixOperatorToParselet[Token`Rational] = LeafParselet[]

PrefixOperatorToParselet[Token`Unknown] = PrefixNullPointerParselet[]
PrefixOperatorToParselet[Token`Whitespace] = PrefixNullPointerParselet[]
PrefixOperatorToParselet[Token`InternalNewline] = PrefixNullPointerParselet[]
PrefixOperatorToParselet[Token`Comment] = PrefixNullPointerParselet[]

PrefixOperatorToParselet[Token`EndOfFile] = PrefixEndOfFileParselet[]

(*
Error handling for prefix parselets
*)

errors = Cases[DownValues[isError][[All, 1]], Verbatim[HoldPattern][HoldPattern[isError][tok_Symbol]] :> tok]

Scan[(
  PrefixOperatorToParselet[#] = PrefixErrorParselet[]
)&, errors]


closers = Cases[DownValues[isCloser][[All, 1]], Verbatim[HoldPattern][HoldPattern[isCloser][tok_Symbol]] :> tok]

Scan[(
  PrefixOperatorToParselet[#] = PrefixCloserParselet[]
)&, closers]




PrefixOperatorToParselet[Token`Minus] = PrefixOperatorParselet[Token`Minus, Precedence`Prefix`Minus, Minus]
PrefixOperatorToParselet[Token`Plus] = PrefixOperatorParselet[Token`Plus, Precedence`Prefix`Plus, Plus]
PrefixOperatorToParselet[Token`Bang] = PrefixOperatorParselet[Token`Bang, Precedence`Prefix`Bang, Not]
PrefixOperatorToParselet[Token`PlusPlus] = PrefixOperatorParselet[Token`PlusPlus, Precedence`Prefix`PlusPlus, PreIncrement]
PrefixOperatorToParselet[Token`MinusMinus] = PrefixOperatorParselet[Token`MinusMinus, Precedence`Prefix`MinusMinus, PreDecrement]

PrefixOperatorToParselet[Token`BangBang] = PrefixOperatorParselet[Token`BangBang, Precedence`Fake`Prefix`BangBang, CodeParser`PrefixNot2]

PrefixOperatorToParselet[Token`LongName`PlusMinus] = PrefixOperatorParselet[Token`LongName`PlusMinus, Precedence`Prefix`LongName`PlusMinus, PlusMinus]
PrefixOperatorToParselet[Token`LongName`Sum] = PrefixOperatorParselet[Token`LongName`Sum, Precedence`LongName`Sum, Sum]
PrefixOperatorToParselet[Token`LongName`Not] = PrefixOperatorParselet[Token`LongName`Not, Precedence`LongName`Not, Not]
PrefixOperatorToParselet[Token`LongName`Sqrt] = PrefixOperatorParselet[Token`LongName`Sqrt, Precedence`LongName`Sqrt, Sqrt]
PrefixOperatorToParselet[Token`LongName`MinusPlus] = PrefixOperatorParselet[Token`LongName`MinusPlus, Precedence`Prefix`LongName`MinusPlus, MinusPlus]
PrefixOperatorToParselet[Token`LongName`DifferentialD] = PrefixOperatorParselet[Token`LongName`DifferentialD, Precedence`LongName`DifferentialD, DifferentialD]
PrefixOperatorToParselet[Token`LongName`CapitalDifferentialD] = PrefixOperatorParselet[Token`LongName`CapitalDifferentialD, Precedence`LongName`CapitalDifferentialD, CapitalDifferentialD]
PrefixOperatorToParselet[Token`LongName`Minus] = PrefixOperatorParselet[Token`LongName`Minus, Precedence`Prefix`LongName`Minus, Minus]
PrefixOperatorToParselet[Token`LongName`Del] = PrefixOperatorParselet[Token`LongName`Del, Precedence`LongName`Del, Del]
PrefixOperatorToParselet[Token`LongName`Square] = PrefixOperatorParselet[Token`LongName`Square, Precedence`LongName`Square, Square]


PrefixOperatorToParselet[Token`Comma] = PrefixCommaParselet[]


(*
Integration operators
*)
PrefixOperatorToParselet[Token`LongName`ContourIntegral] = PrefixOperatorParselet[Token`LongName`ContourIntegral, Precedence`Class`IntegrationOperators, ContourIntegral]
PrefixOperatorToParselet[Token`LongName`DoubleContourIntegral] = PrefixOperatorParselet[Token`LongName`DoubleContourIntegral, Precedence`Class`IntegrationOperators, DoubleContourIntegral]
PrefixOperatorToParselet[Token`LongName`ClockwiseContourIntegral] = PrefixOperatorParselet[Token`LongName`ClockwiseContourIntegral, Precedence`Class`IntegrationOperators, ClockwiseContourIntegral]
PrefixOperatorToParselet[Token`LongName`CounterClockwiseContourIntegral] = PrefixOperatorParselet[Token`LongName`CounterClockwiseContourIntegral, Precedence`Class`IntegrationOperators, CounterClockwiseContourIntegral]


PrefixOperatorToParselet[Token`LongName`Product] = PrefixOperatorParselet[Token`LongName`Product, Precedence`LongName`Product, Product]
PrefixOperatorToParselet[Token`LongName`ContinuedFractionK] = PrefixOperatorParselet[Token`LongName`ContinuedFractionK, Precedence`LongName`ContinuedFractionK, ContinuedFractionK]
PrefixOperatorToParselet[Token`LongName`CircleTimes] = PrefixOperatorParselet[Token`LongName`CircleTimes, Precedence`Prefix`LongName`CircleTimes, CircleTimes]
PrefixOperatorToParselet[Token`LongName`ForAll] = PrefixOperatorParselet[Token`LongName`ForAll, Precedence`LongName`ForAll, ForAll]
PrefixOperatorToParselet[Token`LongName`Exists] = PrefixOperatorParselet[Token`LongName`Exists, Precedence`LongName`Exists, Exists]
PrefixOperatorToParselet[Token`LongName`NotExists] = PrefixOperatorParselet[Token`LongName`NotExists, Precedence`LongName`NotExists, NotExists]
PrefixOperatorToParselet[Token`LongName`Coproduct] = PrefixOperatorParselet[Token`LongName`Coproduct, Precedence`Prefix`LongName`Coproduct, Coproduct]
PrefixOperatorToParselet[Token`LongName`Piecewise] = PrefixOperatorParselet[Token`LongName`Piecewise, Precedence`LongName`Piecewise, Piecewise]
PrefixOperatorToParselet[Token`LongName`InvisiblePrefixScriptBase] = PrefixOperatorParselet[Token`LongName`InvisiblePrefixScriptBase, Precedence`LongName`InvisiblePrefixScriptBase, System`InvisiblePrefixScriptBase]
PrefixOperatorToParselet[Token`LongName`ExpectationE] = PrefixOperatorParselet[Token`LongName`ExpectationE, Precedence`LongName`ExpectationE, ExpectationE]
PrefixOperatorToParselet[Token`LongName`CubeRoot] = PrefixOperatorParselet[Token`LongName`CubeRoot, Precedence`LongName`CubeRoot, CubeRoot]
PrefixOperatorToParselet[Token`LongName`ProbabilityPr] = PrefixOperatorParselet[Token`LongName`ProbabilityPr, Precedence`LongName`ProbabilityPr, ProbabilityPr]

PrefixOperatorToParselet[Token`LinearSyntax`Bang] = PrefixOperatorParselet[Token`LinearSyntax`Bang, Precedence`LinearSyntax`Bang, CodeParser`PrefixLinearSyntaxBang]

(*
Groups
*)
PrefixOperatorToParselet[Token`OpenParen] = GroupParselet[Token`OpenParen, CodeParser`GroupParen]
PrefixOperatorToParselet[Token`OpenSquare] = GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]
PrefixOperatorToParselet[Token`OpenCurly] = GroupParselet[Token`OpenCurly, List]
PrefixOperatorToParselet[Token`LessBar] = GroupParselet[Token`LessBar, Association]
PrefixOperatorToParselet[Token`LongName`LeftAngleBracket] = GroupParselet[Token`LongName`LeftAngleBracket, AngleBracket]
PrefixOperatorToParselet[Token`LongName`LeftCeiling] = GroupParselet[Token`LongName`LeftCeiling, Ceiling]
PrefixOperatorToParselet[Token`LongName`LeftFloor] = GroupParselet[Token`LongName`LeftFloor, Floor]
PrefixOperatorToParselet[Token`LongName`LeftDoubleBracket] = GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]
PrefixOperatorToParselet[Token`LongName`LeftBracketingBar] = GroupParselet[Token`LongName`LeftBracketingBar, BracketingBar]
PrefixOperatorToParselet[Token`LongName`LeftDoubleBracketingBar] = GroupParselet[Token`LongName`LeftDoubleBracketingBar, DoubleBracketingBar]
PrefixOperatorToParselet[Token`LongName`LeftAssociation] = GroupParselet[Token`LongName`LeftAssociation, Association]
PrefixOperatorToParselet[Token`LongName`OpenCurlyQuote] = GroupParselet[Token`LongName`OpenCurlyQuote, CurlyQuote]
PrefixOperatorToParselet[Token`LongName`OpenCurlyDoubleQuote] = GroupParselet[Token`LongName`OpenCurlyDoubleQuote, CurlyDoubleQuote]

(*
Special
*)

(*
context sensitive parsing of  x_
*)
PrefixOperatorToParselet[Token`Symbol] = SymbolParselet[]

(*
context sensitive parsing of _x
*)
PrefixOperatorToParselet[Token`Under] = UnderParselet[1]
PrefixOperatorToParselet[Token`UnderUnder] = UnderParselet[2]
PrefixOperatorToParselet[Token`UnderUnderUnder] = UnderParselet[3]

PrefixOperatorToParselet[Token`UnderDot] = UnderDotParselet[]


PrefixOperatorToParselet[Token`Hash] = HashParselet[]
PrefixOperatorToParselet[Token`HashHash] = HashHashParselet[]

PrefixOperatorToParselet[Token`Percent] = PercentParselet[]
PrefixOperatorToParselet[Token`PercentPercent] = PercentPercentParselet[]

(*
prefix, infix, postfix
*)
PrefixOperatorToParselet[Token`SemiSemi] = SemiSemiParselet[]

(*
FIXME: punt on parsing box syntax, reads tokens with no parsing
*)
PrefixOperatorToParselet[Token`LinearSyntax`OpenParen] = LinearSyntaxOpenParenParselet[]

(*
Has to handle \[Integral] f \[DifferentialD] x
*)
PrefixOperatorToParselet[Token`LongName`Integral] = IntegralParselet[]

(*
stringify next token (as a file]
*)
PrefixOperatorToParselet[Token`LessLess] = LessLessParselet[]


(*
PrefixOperatorToParselet[Token`LinearSyntax`Bang]
PrefixOperatorToParselet[Token`LinearSyntax`OpenParen]

Token`LinearSyntax`Bang and Token`LinearSyntax`OpenParen are supported
*)
PrefixOperatorToParselet[Token`LinearSyntax`Star] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`CloseParen] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`At] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Caret] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Under] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Percent] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Plus] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Backtick] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Slash] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Amp] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LinearSyntax`Space] = PrefixUnsupportedTokenParselet[]

PrefixOperatorToParselet[Token`QuestionQuestion] = PrefixUnsupportedTokenParselet[]

(*
Also use for operators that are only valid in StandardForm.
e.g., \[Gradient] does not have an interpretation in InputForm

\[Gradient] is not letterlike, so it needs some kind of categorization,
but it also needs to be prevented from making any valid parses.
*)
PrefixOperatorToParselet[Token`LongName`Gradient] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`Divergence] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`Curl] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`Limit] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`MaxLimit] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`MinLimit] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`AutoLeftMatch] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`AutoRightMatch] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`DiscreteShift] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`DifferenceDelta] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`DiscreteRatio] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`Laplacian] = PrefixUnsupportedTokenParselet[]
PrefixOperatorToParselet[Token`LongName`PartialD] = PrefixUnsupportedTokenParselet[]


PrefixOperatorToParselet[_] = PrefixUnhandledParselet[]





(*
Infix
*)


InfixOperatorToParselet[Token`Unknown] = InfixNullPointerParselet[]
InfixOperatorToParselet[Token`Whitespace] = InfixNullPointerParselet[]
InfixOperatorToParselet[Token`InternalNewline] = InfixNullPointerParselet[]
InfixOperatorToParselet[Token`Comment] = InfixNullPointerParselet[]

InfixOperatorToParselet[Token`EndOfFile] = InfixAssertFalseParselet[]

(*
Error handling for infix parselets
*)

errors = Cases[DownValues[isError][[All, 1]], Verbatim[HoldPattern][HoldPattern[isError][tok_Symbol]] :> tok]

Scan[(
  InfixOperatorToParselet[#] = InfixAssertFalseParselet[]
)&, errors]


closers = Cases[DownValues[isCloser][[All, 1]], Verbatim[HoldPattern][HoldPattern[isCloser][tok_Symbol]] :> tok]

Scan[(
  InfixOperatorToParselet[#] = InfixAssertFalseParselet[]
)&, closers]


differentialds = Cases[DownValues[isDifferentialD][[All, 1]], Verbatim[HoldPattern][HoldPattern[isDifferentialD][tok_Symbol]] :> tok]

Scan[(
  InfixOperatorToParselet[#] = InfixDifferentialDParselet[]
)&, differentialds]



InfixOperatorToParselet[Token`ToplevelNewline] = InfixToplevelNewlineParselet[]



(*
Binary
*)

InfixOperatorToParselet[Token`Slash] = BinaryOperatorParselet[Token`Slash, Precedence`Slash, Divide]
InfixOperatorToParselet[Token`Caret] = BinaryOperatorParselet[Token`Caret, Precedence`Caret, Power]
InfixOperatorToParselet[Token`CaretEqual] = BinaryOperatorParselet[Token`CaretEqual, Precedence`CaretEqual, UpSet]
InfixOperatorToParselet[Token`CaretColonEqual] = BinaryOperatorParselet[Token`CaretColonEqual, Precedence`CaretColonEqual, UpSetDelayed]
InfixOperatorToParselet[Token`SlashAt] = BinaryOperatorParselet[Token`SlashAt, Precedence`SlashAt, Map]
InfixOperatorToParselet[Token`MinusGreater] = BinaryOperatorParselet[Token`MinusGreater, Precedence`MinusGreater, Rule]
InfixOperatorToParselet[Token`AtAt] = BinaryOperatorParselet[Token`AtAt, Precedence`AtAt, Apply]
InfixOperatorToParselet[Token`SlashSemi] = BinaryOperatorParselet[Token`SlashSemi, Precedence`SlashSemi, Condition]
InfixOperatorToParselet[Token`SlashDot] = BinaryOperatorParselet[Token`SlashDot, Precedence`SlashDot, ReplaceAll]
InfixOperatorToParselet[Token`ColonGreater] = BinaryOperatorParselet[Token`ColonGreater, Precedence`ColonGreater, RuleDelayed]
InfixOperatorToParselet[Token`SlashSlashDot] = BinaryOperatorParselet[Token`SlashSlashDot, Precedence`SlashSlashDot, ReplaceRepeated]
InfixOperatorToParselet[Token`PlusEqual] = BinaryOperatorParselet[Token`PlusEqual, Precedence`PlusEqual, AddTo]
InfixOperatorToParselet[Token`StarEqual] = BinaryOperatorParselet[Token`StarEqual, Precedence`StarEqual, TimesBy]
InfixOperatorToParselet[Token`MinusEqual] = BinaryOperatorParselet[Token`MinusEqual, Precedence`MinusEqual, SubtractFrom]
InfixOperatorToParselet[Token`SlashEqual] = BinaryOperatorParselet[Token`SlashEqual, Precedence`SlashEqual, DivideBy]
InfixOperatorToParselet[Token`LessMinusGreater] = BinaryOperatorParselet[Token`LessMinusGreater, Precedence`LessMinusGreater, System`TwoWayRule]
InfixOperatorToParselet[Token`SlashSlashAt] = BinaryOperatorParselet[Token`SlashSlashAt, Precedence`SlashSlashAt, MapAll]
InfixOperatorToParselet[Token`At] = BinaryOperatorParselet[Token`At, Precedence`At, CodeParser`BinaryAt]
InfixOperatorToParselet[Token`AtAtAt] = BinaryOperatorParselet[Token`AtAtAt, Precedence`AtAtAt, CodeParser`BinaryAtAtAt]
InfixOperatorToParselet[Token`SlashSlash] = BinaryOperatorParselet[Token`SlashSlash, Precedence`SlashSlash, CodeParser`BinarySlashSlash]
InfixOperatorToParselet[Token`Question] = BinaryOperatorParselet[Token`Question, Precedence`Infix`Question, PatternTest]

InfixOperatorToParselet[Token`LongName`Divide] = BinaryOperatorParselet[Token`LongName`Divide, Precedence`LongName`Divide, Divide]
InfixOperatorToParselet[Token`LongName`DivisionSlash] = BinaryOperatorParselet[Token`LongName`DivisionSlash, Precedence`LongName`DivisionSlash, Divide]
InfixOperatorToParselet[Token`LongName`Implies] = BinaryOperatorParselet[Token`LongName`Implies, Precedence`LongName`Implies, Implies]
InfixOperatorToParselet[Token`LongName`RoundImplies] = BinaryOperatorParselet[Token`LongName`RoundImplies, Precedence`LongName`RoundImplies, RoundImplies]
InfixOperatorToParselet[Token`LongName`PlusMinus] = BinaryOperatorParselet[Token`LongName`PlusMinus, Precedence`Infix`LongName`PlusMinus, PlusMinus]
InfixOperatorToParselet[Token`LongName`DirectedEdge] = BinaryOperatorParselet[Token`LongName`DirectedEdge, Precedence`LongName`DirectedEdge, DirectedEdge]
InfixOperatorToParselet[Token`LongName`Rule] = BinaryOperatorParselet[Token`LongName`Rule, Precedence`LongName`Rule, Rule]
InfixOperatorToParselet[Token`LongName`RuleDelayed] = BinaryOperatorParselet[Token`LongName`RuleDelayed, Precedence`LongName`RuleDelayed, RuleDelayed]
InfixOperatorToParselet[Token`LongName`UndirectedEdge] = BinaryOperatorParselet[Token`LongName`UndirectedEdge, Precedence`LongName`UndirectedEdge, UndirectedEdge]
InfixOperatorToParselet[Token`LongName`Function] = BinaryOperatorParselet[Token`LongName`Function, Precedence`LongName`Function, Function]
InfixOperatorToParselet[Token`LongName`MinusPlus] = BinaryOperatorParselet[Token`LongName`MinusPlus, Precedence`Infix`LongName`MinusPlus, MinusPlus]
InfixOperatorToParselet[Token`LongName`TwoWayRule] = BinaryOperatorParselet[Token`LongName`TwoWayRule, Precedence`LongName`TwoWayRule, System`TwoWayRule]
InfixOperatorToParselet[Token`LongName`InvisibleApplication] = BinaryOperatorParselet[Token`LongName`InvisibleApplication, Precedence`LongName`InvisibleApplication, CodeParser`BinaryAt]
InfixOperatorToParselet[Token`LongName`CircleMinus] = BinaryOperatorParselet[Token`LongName`CircleMinus, Precedence`LongName`CircleMinus, CircleMinus]
InfixOperatorToParselet[Token`LongName`SuchThat] = BinaryOperatorParselet[Token`LongName`SuchThat, Precedence`LongName`SuchThat, SuchThat]
InfixOperatorToParselet[Token`LongName`Perpendicular] = BinaryOperatorParselet[Token`LongName`Perpendicular, Precedence`LongName`Perpendicular, Perpendicular]
InfixOperatorToParselet[Token`LongName`Because] = BinaryOperatorParselet[Token`LongName`Because, Precedence`LongName`Because, Because]
InfixOperatorToParselet[Token`LongName`Therefore] = BinaryOperatorParselet[Token`LongName`Therefore, Precedence`LongName`Therefore, Therefore]
InfixOperatorToParselet[Token`LongName`RightTee] = BinaryOperatorParselet[Token`LongName`RightTee, Precedence`LongName`RightTee, RightTee]
InfixOperatorToParselet[Token`LongName`LeftTee] = BinaryOperatorParselet[Token`LongName`LeftTee, Precedence`LongName`LeftTee, LeftTee]
InfixOperatorToParselet[Token`LongName`DoubleRightTee] = BinaryOperatorParselet[Token`LongName`DoubleRightTee, Precedence`LongName`DoubleRightTee, DoubleRightTee]
InfixOperatorToParselet[Token`LongName`DoubleLeftTee] = BinaryOperatorParselet[Token`LongName`DoubleLeftTee, Precedence`LongName`DoubleLeftTee, DoubleLeftTee]
InfixOperatorToParselet[Token`LongName`UpTee] = BinaryOperatorParselet[Token`LongName`UpTee, Precedence`LongName`UpTee, UpTee]
InfixOperatorToParselet[Token`LongName`DownTee] = BinaryOperatorParselet[Token`LongName`DownTee, Precedence`LongName`DownTee, DownTee]


(*
Infix

Note that these are the operators that make sense to be infix in WL source code.

These may not necessarily correspond to Flat functions in WL.
*)
InfixOperatorToParselet[Token`Minus] = InfixOperatorParselet[Token`Minus, Precedence`Infix`Minus, Plus]
InfixOperatorToParselet[Token`EqualEqualEqual] = InfixOperatorParselet[Token`EqualEqualEqual, Precedence`EqualEqualEqual, SameQ]
InfixOperatorToParselet[Token`EqualBangEqual] = InfixOperatorParselet[Token`EqualBangEqual, Precedence`EqualBangEqual, UnsameQ]
InfixOperatorToParselet[Token`Plus] = InfixOperatorParselet[Token`Plus, Precedence`Infix`Plus, Plus]
InfixOperatorToParselet[Token`Star] = InfixOperatorParselet[Token`Star, Precedence`Star, Times]
InfixOperatorToParselet[Token`Dot] = InfixOperatorParselet[Token`Dot, Precedence`Dot, Dot]
InfixOperatorToParselet[Token`StarStar] = InfixOperatorParselet[Token`StarStar, Precedence`StarStar, NonCommutativeMultiply]
InfixOperatorToParselet[Token`AmpAmp] = InfixOperatorParselet[Token`AmpAmp, Precedence`AmpAmp, And]
InfixOperatorToParselet[Token`BarBar] = InfixOperatorParselet[Token`BarBar, Precedence`BarBar, Or]
InfixOperatorToParselet[Token`Bar] = InfixOperatorParselet[Token`Bar, Precedence`Bar, Alternatives]
InfixOperatorToParselet[Token`LessGreater] = InfixOperatorParselet[Token`LessGreater, Precedence`LessGreater, StringJoin]
InfixOperatorToParselet[Token`TildeTilde] = InfixOperatorParselet[Token`TildeTilde, Precedence`TildeTilde, StringExpression]
InfixOperatorToParselet[Token`AtStar] = InfixOperatorParselet[Token`AtStar, Precedence`AtStar, Composition]
InfixOperatorToParselet[Token`SlashStar] = InfixOperatorParselet[Token`SlashStar, Precedence`SlashStar, RightComposition]

(*
Set relations
*)
InfixOperatorToParselet[Token`LongName`Element] = InfixOperatorParselet[Token`LongName`Element, Precedence`Class`SetRelations, Element]
InfixOperatorToParselet[Token`LongName`Subset] = InfixOperatorParselet[Token`LongName`Subset, Precedence`Class`SetRelations, Subset]
InfixOperatorToParselet[Token`LongName`Superset] = InfixOperatorParselet[Token`LongName`Superset, Precedence`Class`SetRelations, Superset]
InfixOperatorToParselet[Token`LongName`SubsetEqual] = InfixOperatorParselet[Token`LongName`SubsetEqual, Precedence`Class`SetRelations, SubsetEqual]
InfixOperatorToParselet[Token`LongName`SupersetEqual] = InfixOperatorParselet[Token`LongName`SupersetEqual, Precedence`Class`SetRelations, SupersetEqual]
InfixOperatorToParselet[Token`LongName`NotElement] = InfixOperatorParselet[Token`LongName`NotElement, Precedence`Class`SetRelations, NotElement]
InfixOperatorToParselet[Token`LongName`NotSubset] = InfixOperatorParselet[Token`LongName`NotSubset, Precedence`Class`SetRelations, NotSubset]
InfixOperatorToParselet[Token`LongName`NotSuperset] = InfixOperatorParselet[Token`LongName`NotSuperset, Precedence`Class`SetRelations, NotSuperset]
InfixOperatorToParselet[Token`LongName`NotSubsetEqual] = InfixOperatorParselet[Token`LongName`NotSubsetEqual, Precedence`Class`SetRelations, NotSubsetEqual]
InfixOperatorToParselet[Token`LongName`NotSupersetEqual] = InfixOperatorParselet[Token`LongName`NotSupersetEqual, Precedence`Class`SetRelations, NotSupersetEqual]
InfixOperatorToParselet[Token`LongName`SquareSubset] = InfixOperatorParselet[Token`LongName`SquareSubset, Precedence`Class`SetRelations, SquareSubset]
InfixOperatorToParselet[Token`LongName`SquareSuperset] = InfixOperatorParselet[Token`LongName`SquareSuperset, Precedence`Class`SetRelations, SquareSuperset]
InfixOperatorToParselet[Token`LongName`NotSquareSubset] = InfixOperatorParselet[Token`LongName`NotSquareSubset, Precedence`Class`SetRelations, NotSquareSubset]
InfixOperatorToParselet[Token`LongName`NotSquareSuperset] = InfixOperatorParselet[Token`LongName`NotSquareSuperset, Precedence`Class`SetRelations, NotSquareSuperset]
InfixOperatorToParselet[Token`LongName`SquareSubsetEqual] = InfixOperatorParselet[Token`LongName`SquareSubsetEqual, Precedence`Class`SetRelations, SquareSubsetEqual]
InfixOperatorToParselet[Token`LongName`SquareSupersetEqual] = InfixOperatorParselet[Token`LongName`SquareSupersetEqual, Precedence`Class`SetRelations, SquareSupersetEqual]
InfixOperatorToParselet[Token`LongName`NotSquareSubsetEqual] = InfixOperatorParselet[Token`LongName`NotSquareSubsetEqual, Precedence`Class`SetRelations, NotSquareSubsetEqual]
InfixOperatorToParselet[Token`LongName`NotSquareSupersetEqual] = InfixOperatorParselet[Token`LongName`NotSquareSupersetEqual, Precedence`Class`SetRelations, NotSquareSupersetEqual]
InfixOperatorToParselet[Token`LongName`ReverseElement] = InfixOperatorParselet[Token`LongName`ReverseElement, Precedence`Class`SetRelations, ReverseElement]
InfixOperatorToParselet[Token`LongName`NotReverseElement] = InfixOperatorParselet[Token`LongName`NotReverseElement, Precedence`Class`SetRelations, NotReverseElement]
InfixOperatorToParselet[Token`LongName`Distributed] = InfixOperatorParselet[Token`LongName`Distributed, Precedence`Class`SetRelations, Distributed]

InfixOperatorToParselet[Token`LongName`ImplicitPlus] = InfixOperatorParselet[Token`LongName`ImplicitPlus, Precedence`LongName`ImplicitPlus, Plus]
InfixOperatorToParselet[Token`LongName`Times] = InfixOperatorParselet[Token`LongName`Times, Precedence`LongName`Times, Times]
InfixOperatorToParselet[Token`LongName`InvisibleTimes] = InfixOperatorParselet[Token`LongName`InvisibleTimes, Precedence`LongName`InvisibleTimes, Times]
InfixOperatorToParselet[Token`LongName`And] = InfixOperatorParselet[Token`LongName`And, Precedence`LongName`And, And]
InfixOperatorToParselet[Token`LongName`Or] = InfixOperatorParselet[Token`LongName`Or, Precedence`LongName`Or, Or]
InfixOperatorToParselet[Token`LongName`Xor] = InfixOperatorParselet[Token`LongName`Xor, Precedence`LongName`Xor, Xor]
InfixOperatorToParselet[Token`LongName`Nand] = InfixOperatorParselet[Token`LongName`Nand, Precedence`LongName`Nand, Nand]
InfixOperatorToParselet[Token`LongName`Nor] = InfixOperatorParselet[Token`LongName`Nor, Precedence`LongName`Nor, Nor]

(*
Horizontal arrows
*)
InfixOperatorToParselet[Token`LongName`LeftArrow] = InfixOperatorParselet[Token`LongName`LeftArrow, Precedence`Class`HorizontalArrows, LeftArrow]
InfixOperatorToParselet[Token`LongName`RightArrow] = InfixOperatorParselet[Token`LongName`RightArrow, Precedence`Class`HorizontalArrows, RightArrow]
InfixOperatorToParselet[Token`LongName`LeftRightArrow] = InfixOperatorParselet[Token`LongName`LeftRightArrow, Precedence`Class`HorizontalArrows, LeftRightArrow]
InfixOperatorToParselet[Token`LongName`LeftTeeArrow] = InfixOperatorParselet[Token`LongName`LeftTeeArrow, Precedence`Class`HorizontalArrows, LeftTeeArrow]
InfixOperatorToParselet[Token`LongName`RightTeeArrow] = InfixOperatorParselet[Token`LongName`RightTeeArrow, Precedence`Class`HorizontalArrows, RightTeeArrow]
InfixOperatorToParselet[Token`LongName`RightArrowLeftArrow] = InfixOperatorParselet[Token`LongName`RightArrowLeftArrow, Precedence`Class`HorizontalArrows, RightArrowLeftArrow]
InfixOperatorToParselet[Token`LongName`LeftArrowRightArrow] = InfixOperatorParselet[Token`LongName`LeftArrowRightArrow, Precedence`Class`HorizontalArrows, LeftArrowRightArrow]
InfixOperatorToParselet[Token`LongName`DoubleLeftArrow] = InfixOperatorParselet[Token`LongName`DoubleLeftArrow, Precedence`Class`HorizontalArrows, DoubleLeftArrow]
InfixOperatorToParselet[Token`LongName`DoubleRightArrow] = InfixOperatorParselet[Token`LongName`DoubleRightArrow, Precedence`Class`HorizontalArrows, DoubleRightArrow]
InfixOperatorToParselet[Token`LongName`DoubleLeftRightArrow] = InfixOperatorParselet[Token`LongName`DoubleLeftRightArrow, Precedence`Class`HorizontalArrows, DoubleLeftRightArrow]
InfixOperatorToParselet[Token`LongName`LeftArrowBar] = InfixOperatorParselet[Token`LongName`LeftArrowBar, Precedence`Class`HorizontalArrows, LeftArrowBar]
InfixOperatorToParselet[Token`LongName`RightArrowBar] = InfixOperatorParselet[Token`LongName`RightArrowBar, Precedence`Class`HorizontalArrows, RightArrowBar]
InfixOperatorToParselet[Token`LongName`ShortRightArrow] = InfixOperatorParselet[Token`LongName`ShortRightArrow, Precedence`Class`HorizontalArrows, ShortRightArrow]
InfixOperatorToParselet[Token`LongName`ShortLeftArrow] = InfixOperatorParselet[Token`LongName`ShortLeftArrow, Precedence`Class`HorizontalArrows, ShortLeftArrow]

(*
Diagonal arrow operators
*)
InfixOperatorToParselet[Token`LongName`UpperLeftArrow] = InfixOperatorParselet[Token`LongName`UpperLeftArrow, Precedence`Class`DiagonalArrowOperators, UpperLeftArrow]
InfixOperatorToParselet[Token`LongName`UpperRightArrow] = InfixOperatorParselet[Token`LongName`UpperRightArrow, Precedence`Class`DiagonalArrowOperators, UpperRightArrow]
InfixOperatorToParselet[Token`LongName`LowerRightArrow] = InfixOperatorParselet[Token`LongName`LowerRightArrow, Precedence`Class`DiagonalArrowOperators, LowerRightArrow]
InfixOperatorToParselet[Token`LongName`LowerLeftArrow] = InfixOperatorParselet[Token`LongName`LowerLeftArrow, Precedence`Class`DiagonalArrowOperators, LowerLeftArrow]

(*
Vector operators
*)
InfixOperatorToParselet[Token`LongName`LeftVector] = InfixOperatorParselet[Token`LongName`LeftVector, Precedence`Class`VectorOperators, LeftVector]
InfixOperatorToParselet[Token`LongName`RightVector] = InfixOperatorParselet[Token`LongName`RightVector, Precedence`Class`VectorOperators, RightVector]
InfixOperatorToParselet[Token`LongName`LeftRightVector] = InfixOperatorParselet[Token`LongName`LeftRightVector, Precedence`Class`VectorOperators, LeftRightVector]
InfixOperatorToParselet[Token`LongName`LeftVectorBar] = InfixOperatorParselet[Token`LongName`LeftVectorBar, Precedence`Class`VectorOperators, LeftVectorBar]
InfixOperatorToParselet[Token`LongName`RightVectorBar] = InfixOperatorParselet[Token`LongName`RightVectorBar, Precedence`Class`VectorOperators, RightVectorBar]
InfixOperatorToParselet[Token`LongName`LeftTeeVector] = InfixOperatorParselet[Token`LongName`LeftTeeVector, Precedence`Class`VectorOperators, LeftTeeVector]
InfixOperatorToParselet[Token`LongName`RightTeeVector] = InfixOperatorParselet[Token`LongName`RightTeeVector, Precedence`Class`VectorOperators, RightTeeVector]
InfixOperatorToParselet[Token`LongName`DownLeftVector] = InfixOperatorParselet[Token`LongName`DownLeftVector, Precedence`Class`VectorOperators, DownLeftVector]
InfixOperatorToParselet[Token`LongName`DownRightVector] = InfixOperatorParselet[Token`LongName`DownRightVector, Precedence`Class`VectorOperators, DownRightVector]
InfixOperatorToParselet[Token`LongName`DownLeftRightVector] = InfixOperatorParselet[Token`LongName`DownLeftRightVector, Precedence`Class`VectorOperators, DownLeftRightVector]
InfixOperatorToParselet[Token`LongName`DownLeftVectorBar] = InfixOperatorParselet[Token`LongName`DownLeftVectorBar, Precedence`Class`VectorOperators, DownLeftVectorBar]
InfixOperatorToParselet[Token`LongName`DownRightVectorBar] = InfixOperatorParselet[Token`LongName`DownRightVectorBar, Precedence`Class`VectorOperators, DownRightVectorBar]
InfixOperatorToParselet[Token`LongName`DownLeftTeeVector] = InfixOperatorParselet[Token`LongName`DownLeftTeeVector, Precedence`Class`VectorOperators, DownLeftTeeVector]
InfixOperatorToParselet[Token`LongName`DownRightTeeVector] = InfixOperatorParselet[Token`LongName`DownRightTeeVector, Precedence`Class`VectorOperators, DownRightTeeVector]

(*
Vertical arrow operators
*)
InfixOperatorToParselet[Token`LongName`UpArrow] = InfixOperatorParselet[Token`LongName`UpArrow, Precedence`Class`VerticalArrowOperators, UpArrow]
InfixOperatorToParselet[Token`LongName`DownArrow] = InfixOperatorParselet[Token`LongName`DownArrow, Precedence`Class`VerticalArrowOperators, DownArrow]
InfixOperatorToParselet[Token`LongName`UpDownArrow] = InfixOperatorParselet[Token`LongName`UpDownArrow, Precedence`Class`VerticalArrowOperators, UpDownArrow]
InfixOperatorToParselet[Token`LongName`UpTeeArrow] = InfixOperatorParselet[Token`LongName`UpTeeArrow, Precedence`Class`VerticalArrowOperators, UpTeeArrow]
InfixOperatorToParselet[Token`LongName`DownTeeArrow] = InfixOperatorParselet[Token`LongName`DownTeeArrow, Precedence`Class`VerticalArrowOperators, DownTeeArrow]
InfixOperatorToParselet[Token`LongName`UpArrowDownArrow] = InfixOperatorParselet[Token`LongName`UpArrowDownArrow, Precedence`Class`VerticalArrowOperators, UpArrowDownArrow]
InfixOperatorToParselet[Token`LongName`DoubleUpArrow] = InfixOperatorParselet[Token`LongName`DoubleUpArrow, Precedence`Class`VerticalArrowOperators, DoubleUpArrow]
InfixOperatorToParselet[Token`LongName`DoubleDownArrow] = InfixOperatorParselet[Token`LongName`DoubleDownArrow, Precedence`Class`VerticalArrowOperators, DoubleDownArrow]
InfixOperatorToParselet[Token`LongName`DoubleUpDownArrow] = InfixOperatorParselet[Token`LongName`DoubleUpDownArrow, Precedence`Class`VerticalArrowOperators, DoubleUpDownArrow]
InfixOperatorToParselet[Token`LongName`DownArrowUpArrow] = InfixOperatorParselet[Token`LongName`DownArrowUpArrow, Precedence`Class`VerticalArrowOperators, DownArrowUpArrow]
InfixOperatorToParselet[Token`LongName`LongLeftArrow] = InfixOperatorParselet[Token`LongName`LongLeftArrow, Precedence`Class`VerticalArrowOperators, LongLeftArrow]
InfixOperatorToParselet[Token`LongName`LongRightArrow] = InfixOperatorParselet[Token`LongName`LongRightArrow, Precedence`Class`VerticalArrowOperators, LongRightArrow]
InfixOperatorToParselet[Token`LongName`LongLeftRightArrow] = InfixOperatorParselet[Token`LongName`LongLeftRightArrow, Precedence`Class`VerticalArrowOperators, LongLeftRightArrow]
InfixOperatorToParselet[Token`LongName`DoubleLongLeftArrow] = InfixOperatorParselet[Token`LongName`DoubleLongLeftArrow, Precedence`Class`VerticalArrowOperators, DoubleLongLeftArrow]
InfixOperatorToParselet[Token`LongName`DoubleLongRightArrow] = InfixOperatorParselet[Token`LongName`DoubleLongRightArrow, Precedence`Class`VerticalArrowOperators, DoubleLongRightArrow]
InfixOperatorToParselet[Token`LongName`DoubleLongLeftRightArrow] = InfixOperatorParselet[Token`LongName`DoubleLongLeftRightArrow, Precedence`Class`VerticalArrowOperators, DoubleLongLeftRightArrow]
InfixOperatorToParselet[Token`LongName`UpArrowBar] = InfixOperatorParselet[Token`LongName`UpArrowBar, Precedence`Class`VerticalArrowOperators, UpArrowBar]
InfixOperatorToParselet[Token`LongName`DownArrowBar] = InfixOperatorParselet[Token`LongName`DownArrowBar, Precedence`Class`VerticalArrowOperators, DownArrowBar]
InfixOperatorToParselet[Token`LongName`ShortUpArrow] = InfixOperatorParselet[Token`LongName`ShortUpArrow, Precedence`Class`VerticalArrowOperators, ShortUpArrow]
InfixOperatorToParselet[Token`LongName`ShortDownArrow] = InfixOperatorParselet[Token`LongName`ShortDownArrow, Precedence`Class`VerticalArrowOperators, ShortDownArrow]


(*
Vertical vector operators
*)
InfixOperatorToParselet[Token`LongName`RightUpVector] = InfixOperatorParselet[Token`LongName`RightUpVector, Precedence`Class`VerticalVectorOperators, RightUpVector]
InfixOperatorToParselet[Token`LongName`LeftUpVector] = InfixOperatorParselet[Token`LongName`LeftUpVector, Precedence`Class`VerticalVectorOperators, LeftUpVector]
InfixOperatorToParselet[Token`LongName`RightDownVector] = InfixOperatorParselet[Token`LongName`RightDownVector, Precedence`Class`VerticalVectorOperators, RightDownVector]
InfixOperatorToParselet[Token`LongName`LeftDownVector] = InfixOperatorParselet[Token`LongName`LeftDownVector, Precedence`Class`VerticalVectorOperators, LeftDownVector]
InfixOperatorToParselet[Token`LongName`RightUpDownVector] = InfixOperatorParselet[Token`LongName`RightUpDownVector, Precedence`Class`VerticalVectorOperators, RightUpDownVector]
InfixOperatorToParselet[Token`LongName`LeftUpDownVector] = InfixOperatorParselet[Token`LongName`LeftUpDownVector, Precedence`Class`VerticalVectorOperators, LeftUpDownVector]
InfixOperatorToParselet[Token`LongName`RightUpVectorBar] = InfixOperatorParselet[Token`LongName`RightUpVectorBar, Precedence`Class`VerticalVectorOperators, RightUpVectorBar]
InfixOperatorToParselet[Token`LongName`RightDownVectorBar] = InfixOperatorParselet[Token`LongName`RightDownVectorBar, Precedence`Class`VerticalVectorOperators, RightDownVectorBar]
InfixOperatorToParselet[Token`LongName`LeftUpVectorBar] = InfixOperatorParselet[Token`LongName`LeftUpVectorBar, Precedence`Class`VerticalVectorOperators, LeftUpVectorBar]
InfixOperatorToParselet[Token`LongName`LeftDownVectorBar] = InfixOperatorParselet[Token`LongName`LeftDownVectorBar, Precedence`Class`VerticalVectorOperators, LeftDownVectorBar]
InfixOperatorToParselet[Token`LongName`RightUpTeeVector] = InfixOperatorParselet[Token`LongName`RightUpTeeVector, Precedence`Class`VerticalVectorOperators, RightUpTeeVector]
InfixOperatorToParselet[Token`LongName`RightDownTeeVector] = InfixOperatorParselet[Token`LongName`RightDownTeeVector, Precedence`Class`VerticalVectorOperators, RightDownTeeVector]
InfixOperatorToParselet[Token`LongName`LeftUpTeeVector] = InfixOperatorParselet[Token`LongName`LeftUpTeeVector, Precedence`Class`VerticalVectorOperators, LeftUpTeeVector]
InfixOperatorToParselet[Token`LongName`LeftDownTeeVector] = InfixOperatorParselet[Token`LongName`LeftDownTeeVector, Precedence`Class`VerticalVectorOperators, LeftDownTeeVector]
InfixOperatorToParselet[Token`LongName`UpEquilibrium] = InfixOperatorParselet[Token`LongName`UpEquilibrium, Precedence`Class`VerticalVectorOperators, UpEquilibrium]
InfixOperatorToParselet[Token`LongName`ReverseUpEquilibrium] = InfixOperatorParselet[Token`LongName`ReverseUpEquilibrium, Precedence`Class`VerticalVectorOperators, ReverseUpEquilibrium]


InfixOperatorToParselet[Token`LongName`CenterDot] = InfixOperatorParselet[Token`LongName`CenterDot, Precedence`LongName`CenterDot, CenterDot]
InfixOperatorToParselet[Token`LongName`Equivalent] = InfixOperatorParselet[Token`LongName`Equivalent, Precedence`LongName`Equivalent, Equivalent]
InfixOperatorToParselet[Token`LongName`CircleDot] = InfixOperatorParselet[Token`LongName`CircleDot, Precedence`LongName`CircleDot, CircleDot]
InfixOperatorToParselet[Token`LongName`Conditioned] = InfixOperatorParselet[Token`LongName`Conditioned, Precedence`LongName`Conditioned, Conditioned]

(*
Union operators
*)
InfixOperatorToParselet[Token`LongName`Union] = InfixOperatorParselet[Token`LongName`Union, Precedence`Class`UnionOperators, Union]
InfixOperatorToParselet[Token`LongName`SquareUnion] = InfixOperatorParselet[Token`LongName`SquareUnion, Precedence`Class`UnionOperators, SquareUnion]
InfixOperatorToParselet[Token`LongName`UnionPlus] = InfixOperatorParselet[Token`LongName`UnionPlus, Precedence`Class`UnionOperators, UnionPlus]

(*
Intersection operators
*)
InfixOperatorToParselet[Token`LongName`Intersection] = InfixOperatorParselet[Token`LongName`Intersection, Precedence`Class`IntersectionOperators, Intersection]
InfixOperatorToParselet[Token`LongName`SquareIntersection] = InfixOperatorParselet[Token`LongName`SquareIntersection, Precedence`Class`IntersectionOperators, SquareIntersection]


InfixOperatorToParselet[Token`LongName`TensorWedge] = InfixOperatorParselet[Token`LongName`TensorWedge, Precedence`LongName`TensorWedge, TensorWedge]
InfixOperatorToParselet[Token`LongName`TensorProduct] = InfixOperatorParselet[Token`LongName`TensorProduct, Precedence`LongName`TensorProduct, TensorProduct]
InfixOperatorToParselet[Token`LongName`Cross] = InfixOperatorParselet[Token`LongName`Cross, Precedence`LongName`Cross, Cross]
InfixOperatorToParselet[Token`LongName`SmallCircle] = InfixOperatorParselet[Token`LongName`SmallCircle, Precedence`LongName`SmallCircle, SmallCircle]
InfixOperatorToParselet[Token`LongName`Divides] = InfixOperatorParselet[Token`LongName`Divides, Precedence`LongName`Divides, Divisible]
InfixOperatorToParselet[Token`LongName`VerticalSeparator] = InfixOperatorParselet[Token`LongName`VerticalSeparator, Precedence`LongName`VerticalSeparator, VerticalSeparator]
InfixOperatorToParselet[Token`LongName`Backslash] = InfixOperatorParselet[Token`LongName`Backslash, Precedence`LongName`Backslash, Backslash]
InfixOperatorToParselet[Token`LongName`Diamond] = InfixOperatorParselet[Token`LongName`Diamond, Precedence`LongName`Diamond, Diamond]
InfixOperatorToParselet[Token`LongName`Wedge] = InfixOperatorParselet[Token`LongName`Wedge, Precedence`LongName`Wedge, Wedge]
InfixOperatorToParselet[Token`LongName`Vee] = InfixOperatorParselet[Token`LongName`Vee, Precedence`LongName`Vee, Vee]
InfixOperatorToParselet[Token`LongName`CircleTimes] = InfixOperatorParselet[Token`LongName`CircleTimes, Precedence`Infix`LongName`CircleTimes, CircleTimes]
InfixOperatorToParselet[Token`LongName`Star] = InfixOperatorParselet[Token`LongName`Star, Precedence`LongName`Star, Star]
InfixOperatorToParselet[Token`LongName`VerticalTilde] = InfixOperatorParselet[Token`LongName`VerticalTilde, Precedence`LongName`VerticalTilde, VerticalTilde]
InfixOperatorToParselet[Token`LongName`Coproduct] = InfixOperatorParselet[Token`LongName`Coproduct, Precedence`Infix`LongName`Coproduct, Coproduct]
InfixOperatorToParselet[Token`LongName`Cap] = InfixOperatorParselet[Token`LongName`Cap, Precedence`LongName`Cap, Cap]
InfixOperatorToParselet[Token`LongName`Cup] = InfixOperatorParselet[Token`LongName`Cup, Precedence`LongName`Cup, Cup]
InfixOperatorToParselet[Token`LongName`CirclePlus] = InfixOperatorParselet[Token`LongName`CirclePlus, Precedence`LongName`CirclePlus, CirclePlus]
InfixOperatorToParselet[Token`LongName`VerticalBar] = InfixOperatorParselet[Token`LongName`VerticalBar, Precedence`LongName`VerticalBar, VerticalBar]
InfixOperatorToParselet[Token`LongName`DoubleVerticalBar] = InfixOperatorParselet[Token`LongName`DoubleVerticalBar, Precedence`LongName`DoubleVerticalBar, DoubleVerticalBar]
InfixOperatorToParselet[Token`LongName`NotVerticalBar] = InfixOperatorParselet[Token`LongName`NotVerticalBar, Precedence`LongName`NotVerticalBar, NotVerticalBar]
InfixOperatorToParselet[Token`LongName`NotDoubleVerticalBar] = InfixOperatorParselet[Token`LongName`NotDoubleVerticalBar, Precedence`LongName`NotDoubleVerticalBar, NotDoubleVerticalBar]

(*
Ordering operators
*)
InfixOperatorToParselet[Token`LongName`LeftTriangle] = InfixOperatorParselet[Token`LongName`LeftTriangle, Precedence`Class`OrderingOperators, LeftTriangle]
InfixOperatorToParselet[Token`LongName`RightTriangle] = InfixOperatorParselet[Token`LongName`RightTriangle, Precedence`Class`OrderingOperators, RightTriangle]
InfixOperatorToParselet[Token`LongName`NotLeftTriangle] = InfixOperatorParselet[Token`LongName`NotLeftTriangle, Precedence`Class`OrderingOperators, NotLeftTriangle]
InfixOperatorToParselet[Token`LongName`NotRightTriangle] = InfixOperatorParselet[Token`LongName`NotRightTriangle, Precedence`Class`OrderingOperators, NotRightTriangle]
InfixOperatorToParselet[Token`LongName`LeftTriangleEqual] = InfixOperatorParselet[Token`LongName`LeftTriangleEqual, Precedence`Class`OrderingOperators, LeftTriangleEqual]
InfixOperatorToParselet[Token`LongName`RightTriangleEqual] = InfixOperatorParselet[Token`LongName`RightTriangleEqual, Precedence`Class`OrderingOperators, RightTriangleEqual]
InfixOperatorToParselet[Token`LongName`NotLeftTriangleEqual] = InfixOperatorParselet[Token`LongName`NotLeftTriangleEqual, Precedence`Class`OrderingOperators, NotLeftTriangleEqual]
InfixOperatorToParselet[Token`LongName`NotRightTriangleEqual] = InfixOperatorParselet[Token`LongName`NotRightTriangleEqual, Precedence`Class`OrderingOperators, NotRightTriangleEqual]
InfixOperatorToParselet[Token`LongName`LeftTriangleBar] = InfixOperatorParselet[Token`LongName`LeftTriangleBar, Precedence`Class`OrderingOperators, LeftTriangleBar]
InfixOperatorToParselet[Token`LongName`RightTriangleBar] = InfixOperatorParselet[Token`LongName`RightTriangleBar, Precedence`Class`OrderingOperators, RightTriangleBar]
InfixOperatorToParselet[Token`LongName`NotLeftTriangleBar] = InfixOperatorParselet[Token`LongName`NotLeftTriangleBar, Precedence`Class`OrderingOperators, NotLeftTriangleBar]
InfixOperatorToParselet[Token`LongName`NotRightTriangleBar] = InfixOperatorParselet[Token`LongName`NotRightTriangleBar, Precedence`Class`OrderingOperators, NotRightTriangleBar]
InfixOperatorToParselet[Token`LongName`TildeEqual] = InfixOperatorParselet[Token`LongName`TildeEqual, Precedence`Class`OrderingOperators, TildeEqual]
InfixOperatorToParselet[Token`LongName`NotTildeEqual] = InfixOperatorParselet[Token`LongName`NotTildeEqual, Precedence`Class`OrderingOperators, NotTildeEqual]
InfixOperatorToParselet[Token`LongName`TildeFullEqual] = InfixOperatorParselet[Token`LongName`TildeFullEqual, Precedence`Class`OrderingOperators, TildeFullEqual]
InfixOperatorToParselet[Token`LongName`NotTildeFullEqual] = InfixOperatorParselet[Token`LongName`NotTildeFullEqual, Precedence`Class`OrderingOperators, NotTildeFullEqual]
InfixOperatorToParselet[Token`LongName`Tilde] = InfixOperatorParselet[Token`LongName`Tilde, Precedence`Class`OrderingOperators, Tilde]
InfixOperatorToParselet[Token`LongName`NotTilde] = InfixOperatorParselet[Token`LongName`NotTilde, Precedence`Class`OrderingOperators, NotTilde]
InfixOperatorToParselet[Token`LongName`EqualTilde] = InfixOperatorParselet[Token`LongName`EqualTilde, Precedence`Class`OrderingOperators, EqualTilde]
InfixOperatorToParselet[Token`LongName`NotEqualTilde] = InfixOperatorParselet[Token`LongName`NotEqualTilde, Precedence`Class`OrderingOperators, NotEqualTilde]
InfixOperatorToParselet[Token`LongName`TildeTilde] = InfixOperatorParselet[Token`LongName`TildeTilde, Precedence`Class`OrderingOperators, TildeTilde]
InfixOperatorToParselet[Token`LongName`NotTildeTilde] = InfixOperatorParselet[Token`LongName`NotTildeTilde, Precedence`Class`OrderingOperators, NotTildeTilde]
InfixOperatorToParselet[Token`LongName`Proportional] = InfixOperatorParselet[Token`LongName`Proportional, Precedence`Class`OrderingOperators, Proportional]
InfixOperatorToParselet[Token`LongName`Proportion] = InfixOperatorParselet[Token`LongName`Proportion, Precedence`Class`OrderingOperators, Proportion]
InfixOperatorToParselet[Token`LongName`Congruent] = InfixOperatorParselet[Token`LongName`Congruent, Precedence`Class`OrderingOperators, Congruent]
InfixOperatorToParselet[Token`LongName`NotCongruent] = InfixOperatorParselet[Token`LongName`NotCongruent, Precedence`Class`OrderingOperators, NotCongruent]
InfixOperatorToParselet[Token`LongName`Equilibrium] = InfixOperatorParselet[Token`LongName`Equilibrium, Precedence`Class`OrderingOperators, Equilibrium]
InfixOperatorToParselet[Token`LongName`ReverseEquilibrium] = InfixOperatorParselet[Token`LongName`ReverseEquilibrium, Precedence`Class`OrderingOperators, ReverseEquilibrium]
InfixOperatorToParselet[Token`LongName`DotEqual] = InfixOperatorParselet[Token`LongName`DotEqual, Precedence`Class`OrderingOperators, DotEqual]
InfixOperatorToParselet[Token`LongName`Precedes] = InfixOperatorParselet[Token`LongName`Precedes, Precedence`Class`OrderingOperators, Precedes]
InfixOperatorToParselet[Token`LongName`Succeeds] = InfixOperatorParselet[Token`LongName`Succeeds, Precedence`Class`OrderingOperators, Succeeds]
InfixOperatorToParselet[Token`LongName`PrecedesEqual] = InfixOperatorParselet[Token`LongName`PrecedesEqual, Precedence`Class`OrderingOperators, PrecedesEqual]
InfixOperatorToParselet[Token`LongName`SucceedsEqual] = InfixOperatorParselet[Token`LongName`SucceedsEqual, Precedence`Class`OrderingOperators, SucceedsEqual]
InfixOperatorToParselet[Token`LongName`PrecedesTilde] = InfixOperatorParselet[Token`LongName`PrecedesTilde, Precedence`Class`OrderingOperators, PrecedesTilde]
InfixOperatorToParselet[Token`LongName`SucceedsTilde] = InfixOperatorParselet[Token`LongName`SucceedsTilde, Precedence`Class`OrderingOperators, SucceedsTilde]
InfixOperatorToParselet[Token`LongName`PrecedesSlantEqual] = InfixOperatorParselet[Token`LongName`PrecedesSlantEqual, Precedence`Class`OrderingOperators, PrecedesSlantEqual]
InfixOperatorToParselet[Token`LongName`SucceedsSlantEqual] = InfixOperatorParselet[Token`LongName`SucceedsSlantEqual, Precedence`Class`OrderingOperators, SucceedsSlantEqual]
InfixOperatorToParselet[Token`LongName`NotPrecedes] = InfixOperatorParselet[Token`LongName`NotPrecedes, Precedence`Class`OrderingOperators, NotPrecedes]
InfixOperatorToParselet[Token`LongName`NotSucceeds] = InfixOperatorParselet[Token`LongName`NotSucceeds, Precedence`Class`OrderingOperators, NotSucceeds]
InfixOperatorToParselet[Token`LongName`NotPrecedesEqual] = InfixOperatorParselet[Token`LongName`NotPrecedesEqual, Precedence`Class`OrderingOperators, NotPrecedesEqual]
InfixOperatorToParselet[Token`LongName`NotSucceedsEqual] = InfixOperatorParselet[Token`LongName`NotSucceedsEqual, Precedence`Class`OrderingOperators, NotSucceedsEqual]
InfixOperatorToParselet[Token`LongName`NotPrecedesTilde] = InfixOperatorParselet[Token`LongName`NotPrecedesTilde, Precedence`Class`OrderingOperators, NotPrecedesTilde]
InfixOperatorToParselet[Token`LongName`NotSucceedsTilde] = InfixOperatorParselet[Token`LongName`NotSucceedsTilde, Precedence`Class`OrderingOperators, NotSucceedsTilde]
InfixOperatorToParselet[Token`LongName`NotPrecedesSlantEqual] = InfixOperatorParselet[Token`LongName`NotPrecedesSlantEqual, Precedence`Class`OrderingOperators, NotPrecedesSlantEqual]
InfixOperatorToParselet[Token`LongName`NotSucceedsSlantEqual] = InfixOperatorParselet[Token`LongName`NotSucceedsSlantEqual, Precedence`Class`OrderingOperators, NotSucceedsSlantEqual]
InfixOperatorToParselet[Token`LongName`CupCap] = InfixOperatorParselet[Token`LongName`CupCap, Precedence`Class`OrderingOperators, CupCap]
InfixOperatorToParselet[Token`LongName`NotCupCap] = InfixOperatorParselet[Token`LongName`NotCupCap, Precedence`Class`OrderingOperators, NotCupCap]
InfixOperatorToParselet[Token`LongName`HumpEqual] = InfixOperatorParselet[Token`LongName`HumpEqual, Precedence`Class`OrderingOperators, HumpEqual]
InfixOperatorToParselet[Token`LongName`HumpDownHump] = InfixOperatorParselet[Token`LongName`HumpDownHump, Precedence`Class`OrderingOperators, HumpDownHump]
InfixOperatorToParselet[Token`LongName`NotHumpEqual] = InfixOperatorParselet[Token`LongName`NotHumpEqual, Precedence`Class`OrderingOperators, NotHumpEqual]
InfixOperatorToParselet[Token`LongName`NotHumpDownHump] = InfixOperatorParselet[Token`LongName`NotHumpDownHump, Precedence`Class`OrderingOperators, NotHumpDownHump]

(*
special Inequality
*)
InfixOperatorToParselet[Token`BangEqual] = InfixOperatorParselet[Token`BangEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`EqualEqual] = InfixOperatorParselet[Token`EqualEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`Greater] = InfixOperatorParselet[Token`Greater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`GreaterEqual] = InfixOperatorParselet[Token`GreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LessEqual] = InfixOperatorParselet[Token`LessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`Less] = InfixOperatorParselet[Token`Less, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`Equal] = InfixOperatorParselet[Token`LongName`Equal, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterEqual] = InfixOperatorParselet[Token`LongName`GreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterEqualLess] = InfixOperatorParselet[Token`LongName`GreaterEqualLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterFullEqual] = InfixOperatorParselet[Token`LongName`GreaterFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterGreater] = InfixOperatorParselet[Token`LongName`GreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterLess] = InfixOperatorParselet[Token`LongName`GreaterLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterSlantEqual] = InfixOperatorParselet[Token`LongName`GreaterSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`GreaterTilde] = InfixOperatorParselet[Token`LongName`GreaterTilde, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessEqual] = InfixOperatorParselet[Token`LongName`LessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessEqualGreater] = InfixOperatorParselet[Token`LongName`LessEqualGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessFullEqual] = InfixOperatorParselet[Token`LongName`LessFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessGreater] = InfixOperatorParselet[Token`LongName`LessGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessLess] = InfixOperatorParselet[Token`LongName`LessLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessSlantEqual] = InfixOperatorParselet[Token`LongName`LessSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LessTilde] = InfixOperatorParselet[Token`LongName`LessTilde, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`LongEqual] = InfixOperatorParselet[Token`LongName`LongEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NestedGreaterGreater] = InfixOperatorParselet[Token`LongName`NestedGreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NestedLessLess] = InfixOperatorParselet[Token`LongName`NestedLessLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotEqual] = InfixOperatorParselet[Token`LongName`NotEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreater] = InfixOperatorParselet[Token`LongName`NotGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreaterEqual] = InfixOperatorParselet[Token`LongName`NotGreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreaterFullEqual] = InfixOperatorParselet[Token`LongName`NotGreaterFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreaterGreater] = InfixOperatorParselet[Token`LongName`NotGreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreaterLess] = InfixOperatorParselet[Token`LongName`NotGreaterLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreaterSlantEqual] = InfixOperatorParselet[Token`LongName`NotGreaterSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotGreaterTilde] = InfixOperatorParselet[Token`LongName`NotGreaterTilde, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLess] = InfixOperatorParselet[Token`LongName`NotLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLessEqual] = InfixOperatorParselet[Token`LongName`NotLessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLessFullEqual] = InfixOperatorParselet[Token`LongName`NotLessFullEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLessGreater] = InfixOperatorParselet[Token`LongName`NotLessGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLessLess] = InfixOperatorParselet[Token`LongName`NotLessLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLessSlantEqual] = InfixOperatorParselet[Token`LongName`NotLessSlantEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotLessTilde] = InfixOperatorParselet[Token`LongName`NotLessTilde, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotNestedGreaterGreater] = InfixOperatorParselet[Token`LongName`NotNestedGreaterGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`NotNestedLessLess] = InfixOperatorParselet[Token`LongName`NotNestedLessLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
(*
special VectorInequality
*)
InfixOperatorToParselet[Token`LongName`VectorGreater] = InfixOperatorParselet[Token`LongName`VectorGreater, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`VectorGreaterEqual] = InfixOperatorParselet[Token`LongName`VectorGreaterEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`VectorLess] = InfixOperatorParselet[Token`LongName`VectorLess, Precedence`Class`Inequality, CodeParser`InfixInequality]
InfixOperatorToParselet[Token`LongName`VectorLessEqual] = InfixOperatorParselet[Token`LongName`VectorLessEqual, Precedence`Class`Inequality, CodeParser`InfixInequality]


InfixOperatorToParselet[Token`LongName`PermutationProduct] = InfixOperatorParselet[Token`LongName`PermutationProduct, Precedence`LongName`PermutationProduct, PermutationProduct]
InfixOperatorToParselet[Token`LongName`Colon] = InfixOperatorParselet[Token`LongName`Colon, Precedence`LongName`Colon, Colon]
InfixOperatorToParselet[Token`LongName`Xnor] = InfixOperatorParselet[Token`LongName`Xnor, Precedence`LongName`Xnor, Xnor]
InfixOperatorToParselet[Token`LongName`Minus] = InfixOperatorParselet[Token`LongName`Minus, Precedence`Infix`LongName`Minus, Plus]

InfixOperatorToParselet[Token`Fake`ImplicitTimes] = InfixOperatorParselet[Token`Fake`ImplicitTimes, Precedence`Star, Times]


(*
Postfix
*)
InfixOperatorToParselet[Token`Amp] = PostfixOperatorParselet[Token`Amp, Precedence`Amp, Function]
InfixOperatorToParselet[Token`DotDot] = PostfixOperatorParselet[Token`DotDot, Precedence`DotDot, Repeated]
InfixOperatorToParselet[Token`Bang] = PostfixOperatorParselet[Token`Bang, Precedence`Postfix`Bang, Factorial]
InfixOperatorToParselet[Token`MinusMinus] = PostfixOperatorParselet[Token`MinusMinus, Precedence`Postfix`MinusMinus, Decrement]
InfixOperatorToParselet[Token`PlusPlus] = PostfixOperatorParselet[Token`PlusPlus, Precedence`Postfix`PlusPlus, Increment]
InfixOperatorToParselet[Token`DotDotDot] = PostfixOperatorParselet[Token`DotDotDot, Precedence`DotDotDot, RepeatedNull]
InfixOperatorToParselet[Token`BangBang] = PostfixOperatorParselet[Token`BangBang, Precedence`Postfix`BangBang, Factorial2]
InfixOperatorToParselet[Token`SingleQuote] = PostfixOperatorParselet[Token`SingleQuote, Precedence`SingleQuote, Derivative]
InfixOperatorToParselet[Token`LongName`Transpose] = PostfixOperatorParselet[Token`LongName`Transpose, Precedence`LongName`Transpose, Transpose]
InfixOperatorToParselet[Token`LongName`Conjugate] = PostfixOperatorParselet[Token`LongName`Conjugate, Precedence`LongName`Conjugate, Conjugate]
InfixOperatorToParselet[Token`LongName`ConjugateTranspose] = PostfixOperatorParselet[Token`LongName`ConjugateTranspose, Precedence`LongName`ConjugateTranspose, ConjugateTranspose]
InfixOperatorToParselet[Token`LongName`HermitianConjugate] = PostfixOperatorParselet[Token`LongName`HermitianConjugate, Precedence`LongName`HermitianConjugate, System`HermitianConjugate]
InfixOperatorToParselet[Token`LongName`InvisiblePostfixScriptBase] = PostfixOperatorParselet[Token`LongName`InvisiblePostfixScriptBase, Precedence`LongName`InvisiblePostfixScriptBase, System`InvisiblePostfixScriptBase]


(*
Calls
*)
InfixOperatorToParselet[Token`OpenSquare] = CallParselet[GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]]
InfixOperatorToParselet[Token`LongName`LeftDoubleBracket] = CallParselet[GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]]




(*
trailing ; and , is allowed
*)
InfixOperatorToParselet[Token`Semi] = SemiParselet[]

InfixOperatorToParselet[Token`Comma] = CommaParselet[]
InfixOperatorToParselet[Token`LongName`InvisibleComma] = CommaParselet[]

(*
prefix, infix, postfix
*)
InfixOperatorToParselet[Token`SemiSemi] = SemiSemiParselet[]

(*
ternary
*)
InfixOperatorToParselet[Token`Tilde] = TildeParselet[]

(*
context sensitive parsing of sym:obj and pat:v
*)
InfixOperatorToParselet[Token`Colon] = ColonParselet[]

(*
ternary, with different possibilities for second operator
*)
InfixOperatorToParselet[Token`SlashColon] = SlashColonParselet[]

(*
Has to handle  a =.  and  a = .
*)
InfixOperatorToParselet[Token`Equal] = EqualParselet[]
InfixOperatorToParselet[Token`ColonEqual] = ColonEqualParselet[]

(*
stringify next token (as a symbol)
*)
InfixOperatorToParselet[Token`ColonColon] = ColonColonParselet[]

(*
stringify next token (as a file)
*)
InfixOperatorToParselet[Token`GreaterGreater] = GreaterGreaterParselet[]
InfixOperatorToParselet[Token`GreaterGreaterGreater] = GreaterGreaterGreaterParselet[]


(*
InfixOperatorToParselet[Token`LinearSyntax`Bang]
InfixOperatorToParselet[Token`LinearSyntax`OpenParen]

Token`LinearSyntax`Bang and Token`LinearSyntax`OpenParen are supported
*)
InfixOperatorToParselet[Token`LinearSyntax`Star] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`CloseParen] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`At] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Caret] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Under] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Percent] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Plus] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Backtick] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Slash] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Amp] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Space] = InfixAssertFalseParselet[]

InfixOperatorToParselet[Token`QuestionQuestion] = InfixAssertFalseParselet[]

(*
Also use for operators that are only valid in StandardForm.
e.g., \[Gradient] does not have an interpretation in InputForm

\[Gradient] is not letterlike, so it needs some kind of categorization,
but it also needs to be prevented from making any valid parses.
*)
InfixOperatorToParselet[Token`LongName`Gradient] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`Divergence] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`Curl] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`Limit] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`MaxLimit] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`MinLimit] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`AutoLeftMatch] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`AutoRightMatch] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`DiscreteShift] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`DifferenceDelta] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`DiscreteRatio] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`Laplacian] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LongName`PartialD] = InfixAssertFalseParselet[]



InfixOperatorToParselet[_] = InfixImplicitTimesParselet[]










Print["Generating ParseletRegistration..."]




parseletRegistrationCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"TokenEnum.h\"

#include <array>

class PrefixParselet;
class InfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
class PrefixToplevelCloserParselet;

using PrefixParseletPtr = PrefixParselet *;
using InfixParseletPtr = InfixParselet *;
using ContextSensitivePrefixParseletPtr = ContextSensitivePrefixParselet *;
using ContextSensitiveInfixParseletPtr = ContextSensitiveInfixParselet *;
using PrefixToplevelCloserParseletPtr = PrefixToplevelCloserParselet *;

extern std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets;
extern std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets;

extern ContextSensitivePrefixParseletPtr contextSensitiveSymbolParselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder1Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder2Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder3Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnderDotParselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveColonParselet;

extern PrefixToplevelCloserParseletPtr contextSensitivePrefixToplevelCloserParselet;
"}

Print["exporting ParseletRegistration.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "ParseletRegistration.h"}], Column[parseletRegistrationCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]




tokensSansCount = DeleteCases[tokens, Token`Count]



formatPrefix[PrefixNullPointerParselet[]] := "nullptr"

formatPrefix[PrefixEndOfFileParselet[]] := "&prefixEndOfFileParselet"

formatPrefix[PrefixErrorParselet[]] := "&prefixErrorParselet"

formatPrefix[PrefixCloserParselet[]] := "&prefixCloserParselet"

formatPrefix[PrefixUnsupportedTokenParselet[]] := "&prefixUnsupportedTokenParselet"

formatPrefix[PrefixUnhandledParselet[]] := "&prefixUnhandledParselet"

formatPrefix[PrefixCommaParselet[]] := "&prefixCommaParselet"

formatPrefix[LeafParselet[]] := "&leafParselet"

formatPrefix[SymbolParselet[]] := "&symbolParselet"

formatPrefix[UnderParselet[1]] := "&under1Parselet"

formatPrefix[UnderParselet[2]] := "&under2Parselet"

formatPrefix[UnderParselet[3]] := "&under3Parselet"

formatPrefix[UnderDotParselet[]] := "&underDotParselet"

formatPrefix[HashParselet[]] := "new HashParselet()"

formatPrefix[HashHashParselet[]] := "new HashHashParselet()"

formatPrefix[PercentParselet[]] := "new PercentParselet()"

formatPrefix[PercentPercentParselet[]] := "new PercentPercentParselet()"

formatPrefix[LessLessParselet[]] := "new LessLessParselet()"

formatPrefix[SemiSemiParselet[]] := "&semiSemiParselet"

formatPrefix[LinearSyntaxOpenParenParselet[]] := "new LinearSyntaxOpenParenParselet()"

formatPrefix[IntegralParselet[]] := "new IntegralParselet()"

formatPrefix[PrefixOperatorParselet[tok_, precedence_, op_]] := "new PrefixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatPrefix[GroupParselet[Token`OpenSquare, CodeParser`GroupSquare]] := "&squareGroupParselet"

formatPrefix[GroupParselet[Token`LongName`LeftDoubleBracket, CodeParser`GroupDoubleBracket]] := "&doubleBracketGroupParselet"

formatPrefix[GroupParselet[tok_, op_]] := "new GroupParselet(" <> toGlobal[tok] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"


formatInfix[InfixNullPointerParselet[]] := "nullptr"

formatInfix[InfixAssertFalseParselet[]] := "&infixAssertFalseParselet"

formatInfix[InfixImplicitTimesParselet[]] := "&infixImplicitTimesParselet"



formatInfix[BinaryOperatorParselet[tok_, precedence_, op_]] := "new BinaryOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[InfixOperatorParselet[tok_, precedence_, op_]] := "new InfixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[PostfixOperatorParselet[tok_, precedence_, op_]] := "new PostfixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> "SYMBOL_" <> toGlobal[op] <> ")"

formatInfix[ColonParselet[]] := "&colonParselet"

formatInfix[CallParselet[groupParselet_]] := "new CallParselet(" <> formatPrefix[groupParselet] <> ")"

formatInfix[EqualParselet[]] := "new EqualParselet()"

formatInfix[ColonEqualParselet[]] := "new ColonEqualParselet()"

formatInfix[TildeParselet[]] := "new TildeParselet()"

formatInfix[SlashColonParselet[]] := "new SlashColonParselet()"

formatInfix[CommaParselet[]] := "&commaParselet"

formatInfix[SemiParselet[]] := "&semiParselet"

formatInfix[SemiSemiParselet[]] := "&semiSemiParselet"

formatInfix[ColonColonParselet[]] := "new ColonColonParselet()"

formatInfix[GreaterGreaterParselet[]] := "new GreaterGreaterParselet()"

formatInfix[GreaterGreaterGreaterParselet[]] := "new GreaterGreaterGreaterParselet()"

formatInfix[InfixDifferentialDParselet[]] := "&infixDifferentialDParselet"

formatInfix[InfixToplevelNewlineParselet[]] := "new InfixToplevelNewlineParselet()"





parseletRegistrationCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"ParseletRegistration.h\"

#include \"Parselet.h\" // for SymbolParselet, UnderParselet, etc.
#include \"ByteDecoder.h\" // for TheByteDecoder

#include <cassert>

auto symbolParselet = SymbolParselet();

auto leafParselet = LeafParselet();

auto prefixEndOfFileParselet = PrefixEndOfFileParselet();

auto prefixErrorParselet = PrefixErrorParselet();

auto prefixCloserParselet = PrefixCloserParselet();

auto prefixToplevelCloserParselet = PrefixToplevelCloserParselet();

auto prefixUnsupportedTokenParselet = PrefixUnsupportedTokenParselet();

auto prefixUnhandledParselet = PrefixUnhandledParselet();

auto prefixCommaParselet = PrefixCommaParselet();

auto infixAssertFalseParselet = InfixAssertFalseParselet();

auto infixImplicitTimesParselet = InfixImplicitTimesParselet();

auto commaParselet = CommaParselet();

auto semiParselet = SemiParselet();

auto semiSemiParselet = SemiSemiParselet();

auto colonParselet = ColonParselet();

auto infixDifferentialDParselet = InfixDifferentialDParselet();

auto under1Parselet = UnderParselet(SYMBOL_BLANK, SYMBOL_CODEPARSER_PATTERNBLANK);

auto under2Parselet = UnderParselet(SYMBOL_BLANKSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE);

auto under3Parselet = UnderParselet(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE);

auto underDotParselet = UnderDotParselet();

auto squareGroupParselet = GroupParselet(TOKEN_OPENSQUARE, SYMBOL_CODEPARSER_GROUPSQUARE);

auto doubleBracketGroupParselet = GroupParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, SYMBOL_CODEPARSER_GROUPDOUBLEBRACKET);

//
//
//
std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets {{"} ~Join~

(Row[{"  ", formatPrefix[PrefixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"}};

//
//
//
std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets {{"} ~Join~

(Row[{"  ", formatInfix[InfixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"}};

ContextSensitivePrefixParseletPtr contextSensitiveSymbolParselet(&symbolParselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder1Parselet(&under1Parselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder2Parselet(&under2Parselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder3Parselet(&under3Parselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnderDotParselet(&underDotParselet);
ContextSensitiveInfixParseletPtr contextSensitiveColonParselet(&colonParselet);

PrefixToplevelCloserParseletPtr contextSensitivePrefixToplevelCloserParselet(&prefixToplevelCloserParselet);
"}

Print["exporting ParseletRegistration.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "ParseletRegistration.cpp"}], Column[parseletRegistrationCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done ParseletRegistration"]

End[]

EndPackage[]

