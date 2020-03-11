BeginPackage["CodeParser`Generate`ParseletRegistration`"]

Begin["`Private`"]

Needs["CodeParser`Generate`TokenEnum`"]
Needs["CodeParser`Generate`GenerateSources`"]



PrefixOperatorToParselet[Token`String] = LeafParselet[Precedence`Highest]
PrefixOperatorToParselet[Token`Integer] = LeafParselet[Precedence`Highest]
PrefixOperatorToParselet[Token`Real] = LeafParselet[Precedence`Highest]

PrefixOperatorToParselet[Token`Percent] = LeafParselet[Precedence`Highest]

PrefixOperatorToParselet[Token`Hash] = LeafParselet[Precedence`Highest]
PrefixOperatorToParselet[Token`HashHash] = LeafParselet[Precedence`Highest]

PrefixOperatorToParselet[Token`Unknown] = LeafParselet[Precedence`AssertFalse]
PrefixOperatorToParselet[Token`Whitespace] = LeafParselet[Precedence`AssertFalse]
PrefixOperatorToParselet[Token`InternalNewline] = LeafParselet[Precedence`AssertFalse]
PrefixOperatorToParselet[Token`Comment] = LeafParselet[Precedence`AssertFalse]
PrefixOperatorToParselet[Token`LineContinuation] = LeafParselet[Precedence`AssertFalse]

PrefixOperatorToParselet[Token`EndOfFile] = LeafParselet[Precedence`Lowest]

errors = Cases[DownValues[isError][[All, 1]], Verbatim[HoldPattern][HoldPattern[isError][tok_Symbol]] :> tok]

Scan[(
  PrefixOperatorToParselet[#] = LeafParselet[Precedence`Lowest]
)&, errors]


closers = Cases[DownValues[isCloser][[All, 1]], Verbatim[HoldPattern][HoldPattern[isCloser][tok_Symbol]] :> tok]

Scan[(
  PrefixOperatorToParselet[#] = LeafParselet[Precedence`Lowest]
)&, closers]




PrefixOperatorToParselet[Token`Minus] = PrefixOperatorParselet[Token`Minus, Precedence`Prefix`Minus]
PrefixOperatorToParselet[Token`Plus] = PrefixOperatorParselet[Token`Plus, Precedence`Prefix`Plus]
PrefixOperatorToParselet[Token`Bang] = PrefixOperatorParselet[Token`Bang, Precedence`Prefix`Bang]
PrefixOperatorToParselet[Token`PlusPlus] = PrefixOperatorParselet[Token`PlusPlus, Precedence`Prefix`PlusPlus]
PrefixOperatorToParselet[Token`MinusMinus] = PrefixOperatorParselet[Token`MinusMinus, Precedence`Prefix`MinusMinus]
PrefixOperatorToParselet[Token`LongName`PlusMinus] = PrefixOperatorParselet[Token`LongName`PlusMinus, Precedence`Prefix`LongName`PlusMinus]
PrefixOperatorToParselet[Token`LongName`Sum] = PrefixOperatorParselet[Token`LongName`Sum, Precedence`LongName`Sum]
PrefixOperatorToParselet[Token`LongName`Not] = PrefixOperatorParselet[Token`LongName`Not, Precedence`LongName`Not]
PrefixOperatorToParselet[Token`LongName`Sqrt] = PrefixOperatorParselet[Token`LongName`Sqrt, Precedence`LongName`Sqrt]
PrefixOperatorToParselet[Token`LongName`MinusPlus] = PrefixOperatorParselet[Token`LongName`MinusPlus, Precedence`Prefix`LongName`MinusPlus]
PrefixOperatorToParselet[Token`LongName`DifferentialD] = PrefixOperatorParselet[Token`LongName`DifferentialD, Precedence`LongName`DifferentialD]
PrefixOperatorToParselet[Token`LongName`CapitalDifferentialD] = PrefixOperatorParselet[Token`LongName`CapitalDifferentialD, Precedence`LongName`CapitalDifferentialD]
PrefixOperatorToParselet[Token`LongName`Minus] = PrefixOperatorParselet[Token`LongName`Minus, Precedence`Prefix`LongName`Minus]
PrefixOperatorToParselet[Token`LongName`Del] = PrefixOperatorParselet[Token`LongName`Del, Precedence`LongName`Del]
PrefixOperatorToParselet[Token`LongName`Square] = PrefixOperatorParselet[Token`LongName`Square, Precedence`LongName`Square]


(*
Integration operators
*)
PrefixOperatorToParselet[Token`LongName`ContourIntegral] = PrefixOperatorParselet[Token`LongName`ContourIntegral, Precedence`Class`IntegrationOperators]
PrefixOperatorToParselet[Token`LongName`DoubleContourIntegral] = PrefixOperatorParselet[Token`LongName`DoubleContourIntegral, Precedence`Class`IntegrationOperators]
PrefixOperatorToParselet[Token`LongName`ClockwiseContourIntegral] = PrefixOperatorParselet[Token`LongName`ClockwiseContourIntegral, Precedence`Class`IntegrationOperators]
PrefixOperatorToParselet[Token`LongName`CounterClockwiseContourIntegral] = PrefixOperatorParselet[Token`LongName`CounterClockwiseContourIntegral, Precedence`Class`IntegrationOperators]


PrefixOperatorToParselet[Token`LongName`Product] = PrefixOperatorParselet[Token`LongName`Product, Precedence`LongName`Product]
PrefixOperatorToParselet[Token`LongName`ContinuedFractionK] = PrefixOperatorParselet[Token`LongName`ContinuedFractionK, Precedence`LongName`ContinuedFractionK]
PrefixOperatorToParselet[Token`LongName`CircleTimes] = PrefixOperatorParselet[Token`LongName`CircleTimes, Precedence`Prefix`LongName`CircleTimes]
PrefixOperatorToParselet[Token`LongName`ForAll] = PrefixOperatorParselet[Token`LongName`ForAll, Precedence`LongName`ForAll]
PrefixOperatorToParselet[Token`LongName`Exists] = PrefixOperatorParselet[Token`LongName`Exists, Precedence`LongName`Exists]
PrefixOperatorToParselet[Token`LongName`NotExists] = PrefixOperatorParselet[Token`LongName`NotExists, Precedence`LongName`NotExists]
PrefixOperatorToParselet[Token`LongName`Coproduct] = PrefixOperatorParselet[Token`LongName`Coproduct, Precedence`Prefix`LongName`Coproduct]
PrefixOperatorToParselet[Token`LongName`Piecewise] = PrefixOperatorParselet[Token`LongName`Piecewise, Precedence`LongName`Piecewise]
PrefixOperatorToParselet[Token`LongName`InvisiblePrefixScriptBase] = PrefixOperatorParselet[Token`LongName`InvisiblePrefixScriptBase, Precedence`LongName`InvisiblePrefixScriptBase]
PrefixOperatorToParselet[Token`LongName`ExpectationE] = PrefixOperatorParselet[Token`LongName`ExpectationE, Precedence`LongName`ExpectationE]
PrefixOperatorToParselet[Token`LongName`CubeRoot] = PrefixOperatorParselet[Token`LongName`CubeRoot, Precedence`LongName`CubeRoot]
PrefixOperatorToParselet[Token`LongName`ProbabilityPr] = PrefixOperatorParselet[Token`LongName`ProbabilityPr, Precedence`LongName`ProbabilityPr]
PrefixOperatorToParselet[Token`BangBang] = PrefixOperatorParselet[Token`BangBang, Precedence`Fake`Prefix`BangBang]
PrefixOperatorToParselet[Token`LinearSyntax`Bang] = PrefixOperatorParselet[Token`LinearSyntax`Bang, Precedence`LinearSyntax`bang]

(*
Groups
*)
PrefixOperatorToParselet[Token`OpenParen] = GroupParselet[Token`OpenParen]
PrefixOperatorToParselet[Token`OpenSquare] = GroupParselet[Token`OpenSquare]
PrefixOperatorToParselet[Token`OpenCurly] = GroupParselet[Token`OpenCurly]
PrefixOperatorToParselet[Token`LessBar] = GroupParselet[Token`LessBar]
PrefixOperatorToParselet[Token`LongName`LeftAngleBracket] = GroupParselet[Token`LongName`LeftAngleBracket]
PrefixOperatorToParselet[Token`LongName`LeftCeiling] = GroupParselet[Token`LongName`LeftCeiling]
PrefixOperatorToParselet[Token`LongName`LeftFloor] = GroupParselet[Token`LongName`LeftFloor]
PrefixOperatorToParselet[Token`LongName`LeftDoubleBracket] = GroupParselet[Token`LongName`LeftDoubleBracket]
PrefixOperatorToParselet[Token`LongName`LeftBracketingBar] = GroupParselet[Token`LongName`LeftBracketingBar]
PrefixOperatorToParselet[Token`LongName`LeftDoubleBracketingBar] = GroupParselet[Token`LongName`LeftDoubleBracketingBar]
PrefixOperatorToParselet[Token`LongName`LeftAssociation] = GroupParselet[Token`LongName`LeftAssociation]
PrefixOperatorToParselet[Token`LongName`OpenCurlyQuote] = GroupParselet[Token`LongName`OpenCurlyQuote]
PrefixOperatorToParselet[Token`LongName`OpenCurlyDoubleQuote] = GroupParselet[Token`LongName`OpenCurlyDoubleQuote]

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
You might think that it makes sense to have the default case be an ErrorNode, but it really should just be a LeafNode

For example, with the input  +  the output is
MakePrefixNode[Plus, List[
  MakeLeafNode[Token`Plus, +, 1112],
  MakeErrorNode[Token`Error`ExpectedOperand, , 1212], ], 1112]

The ErrorNode is handled correctly
*)
PrefixOperatorToParselet[_] = LeafParselet[Precedence`Highest]





(*
Infix
*)


InfixOperatorToParselet[Token`Unknown] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`Whitespace] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`InternalNewline] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`Comment] = InfixAssertFalseParselet[]
InfixOperatorToParselet[Token`LineContinuation] = InfixAssertFalseParselet[]

InfixOperatorToParselet[Token`EndOfFile] = InfixEndOfFileParselet[]

errors = Cases[DownValues[isError][[All, 1]], Verbatim[HoldPattern][HoldPattern[isError][tok_Symbol]] :> tok]

Scan[(
  InfixOperatorToParselet[#] = InfixErrorParselet[]
)&, errors]


closers = Cases[DownValues[isCloser][[All, 1]], Verbatim[HoldPattern][HoldPattern[isCloser][tok_Symbol]] :> tok]

Scan[(
  InfixOperatorToParselet[#] = InfixCloserParselet[]
)&, closers]


differentialds = Cases[DownValues[isDifferentialD][[All, 1]], Verbatim[HoldPattern][HoldPattern[isDifferentialD][tok_Symbol]] :> tok]

Scan[(
  InfixOperatorToParselet[#] = DifferentialDParselet[]
)&, differentialds]



InfixOperatorToParselet[Token`ToplevelNewline] = ToplevelNewlineParselet[]



(*
Binary
*)

InfixOperatorToParselet[Token`Slash] = BinaryOperatorParselet[Token`Slash, Precedence`Slash, Associativity`Left]
InfixOperatorToParselet[Token`Caret] = BinaryOperatorParselet[Token`Caret, Precedence`Caret, Associativity`Right]
InfixOperatorToParselet[Token`CaretEqual] = BinaryOperatorParselet[Token`CaretEqual, Precedence`CaretEqual, Associativity`Right]
InfixOperatorToParselet[Token`CaretColonEqual] = BinaryOperatorParselet[Token`CaretColonEqual, Precedence`CaretColonEqual, Associativity`Right]
InfixOperatorToParselet[Token`SlashAt] = BinaryOperatorParselet[Token`SlashAt, Precedence`SlashAt, Associativity`Right]
InfixOperatorToParselet[Token`MinusGreater] = BinaryOperatorParselet[Token`MinusGreater, Precedence`MinusGreater, Associativity`Right]
InfixOperatorToParselet[Token`AtAt] = BinaryOperatorParselet[Token`AtAt, Precedence`AtAt, Associativity`Right]
InfixOperatorToParselet[Token`SlashSemi] = BinaryOperatorParselet[Token`SlashSemi, Precedence`SlashSemi, Associativity`Left]
InfixOperatorToParselet[Token`SlashDot] = BinaryOperatorParselet[Token`SlashDot, Precedence`SlashDot, Associativity`Left]
InfixOperatorToParselet[Token`ColonGreater] = BinaryOperatorParselet[Token`ColonGreater, Precedence`ColonGreater, Associativity`Right]
InfixOperatorToParselet[Token`SlashSlashDot] = BinaryOperatorParselet[Token`SlashSlashDot, Precedence`SlashSlashDot, Associativity`Left]
InfixOperatorToParselet[Token`PlusEqual] = BinaryOperatorParselet[Token`PlusEqual, Precedence`PlusEqual, Associativity`Right]
InfixOperatorToParselet[Token`StarEqual] = BinaryOperatorParselet[Token`StarEqual, Precedence`StarEqual, Associativity`Right]
InfixOperatorToParselet[Token`MinusEqual] = BinaryOperatorParselet[Token`MinusEqual, Precedence`MinusEqual, Associativity`Right]
InfixOperatorToParselet[Token`SlashEqual] = BinaryOperatorParselet[Token`SlashEqual, Precedence`SlashEqual, Associativity`Right]
InfixOperatorToParselet[Token`LessMinusGreater] = BinaryOperatorParselet[Token`LessMinusGreater, Precedence`LessMinusGreater, Associativity`Right]
InfixOperatorToParselet[Token`SlashSlashAt] = BinaryOperatorParselet[Token`SlashSlashAt, Precedence`SlashSlashAt, Associativity`Right]
InfixOperatorToParselet[Token`At] = BinaryOperatorParselet[Token`At, Precedence`At, Associativity`Right]
InfixOperatorToParselet[Token`AtAtAt] = BinaryOperatorParselet[Token`AtAtAt, Precedence`AtAtAt, Associativity`Right]
InfixOperatorToParselet[Token`SlashSlash] = BinaryOperatorParselet[Token`SlashSlash, Precedence`SlashSlash, Associativity`Left]
InfixOperatorToParselet[Token`ColonEqual] = BinaryOperatorParselet[Token`ColonEqual, Precedence`ColonEqual, Associativity`Right]
InfixOperatorToParselet[Token`Question] = BinaryOperatorParselet[Token`Question, Precedence`Infix`Question, Associativity`NonAssociative]
InfixOperatorToParselet[Token`LongName`Divide] = BinaryOperatorParselet[Token`LongName`Divide, Precedence`LongName`Divide, Associativity`Left]
InfixOperatorToParselet[Token`LongName`DivisionSlash] = BinaryOperatorParselet[Token`LongName`DivisionSlash, Precedence`LongName`DivisionSlash, Associativity`Left]
InfixOperatorToParselet[Token`LongName`Implies] = BinaryOperatorParselet[Token`LongName`Implies, Precedence`LongName`Implies, Associativity`Right]
InfixOperatorToParselet[Token`LongName`RoundImplies] = BinaryOperatorParselet[Token`LongName`RoundImplies, Precedence`LongName`RoundImplies, Associativity`Right]
InfixOperatorToParselet[Token`LongName`PlusMinus] = BinaryOperatorParselet[Token`LongName`PlusMinus, Precedence`Infix`LongName`PlusMinus, Associativity`Left]
InfixOperatorToParselet[Token`LongName`DirectedEdge] = BinaryOperatorParselet[Token`LongName`DirectedEdge, Precedence`LongName`DirectedEdge, Associativity`Right]
InfixOperatorToParselet[Token`LongName`Rule] = BinaryOperatorParselet[Token`LongName`Rule, Precedence`LongName`Rule, Associativity`Right]
InfixOperatorToParselet[Token`LongName`RuleDelayed] = BinaryOperatorParselet[Token`LongName`RuleDelayed, Precedence`LongName`RuleDelayed, Associativity`Right]
InfixOperatorToParselet[Token`LongName`UndirectedEdge] = BinaryOperatorParselet[Token`LongName`UndirectedEdge, Precedence`LongName`UndirectedEdge, Associativity`Right]
InfixOperatorToParselet[Token`LongName`Function] = BinaryOperatorParselet[Token`LongName`Function, Precedence`LongName`Function, Associativity`Right]
InfixOperatorToParselet[Token`LongName`MinusPlus] = BinaryOperatorParselet[Token`LongName`MinusPlus, Precedence`Infix`LongName`MinusPlus, Associativity`Left]
InfixOperatorToParselet[Token`LongName`TwoWayRule] = BinaryOperatorParselet[Token`LongName`TwoWayRule, Precedence`LongName`TwoWayRule, Associativity`Right]
InfixOperatorToParselet[Token`LongName`InvisibleApplication] = BinaryOperatorParselet[Token`LongName`InvisibleApplication, Precedence`LongName`InvisibleApplication, Associativity`Right]
InfixOperatorToParselet[Token`LongName`CircleMinus] = BinaryOperatorParselet[Token`LongName`CircleMinus, Precedence`LongName`CircleMinus, Associativity`Left]
InfixOperatorToParselet[Token`LongName`SuchThat] = BinaryOperatorParselet[Token`LongName`SuchThat, Precedence`LongName`SuchThat, Associativity`Right]
InfixOperatorToParselet[Token`LongName`Perpendicular] = BinaryOperatorParselet[Token`LongName`Perpendicular, Precedence`LongName`Perpendicular, Associativity`Left]
InfixOperatorToParselet[Token`LongName`Because] = BinaryOperatorParselet[Token`LongName`Because, Precedence`LongName`Because, Associativity`Left]
InfixOperatorToParselet[Token`LongName`Therefore] = BinaryOperatorParselet[Token`LongName`Therefore, Precedence`LongName`Therefore, Associativity`Right]
InfixOperatorToParselet[Token`LongName`RightTee] = BinaryOperatorParselet[Token`LongName`RightTee, Precedence`LongName`RightTee, Associativity`Right]
InfixOperatorToParselet[Token`LongName`LeftTee] = BinaryOperatorParselet[Token`LongName`LeftTee, Precedence`LongName`LeftTee, Associativity`Left]
InfixOperatorToParselet[Token`LongName`DoubleRightTee] = BinaryOperatorParselet[Token`LongName`DoubleRightTee, Precedence`LongName`DoubleRightTee, Associativity`Right]
InfixOperatorToParselet[Token`LongName`DoubleLeftTee] = BinaryOperatorParselet[Token`LongName`DoubleLeftTee, Precedence`LongName`DoubleLeftTee, Associativity`Left]
InfixOperatorToParselet[Token`LongName`UpTee] = BinaryOperatorParselet[Token`LongName`UpTee, Precedence`LongName`UpTee, Associativity`Left]
InfixOperatorToParselet[Token`LongName`DownTee] = BinaryOperatorParselet[Token`LongName`DownTee, Precedence`LongName`DownTee, Associativity`Left]


(*
Infix

Note that these are the operators that make sense to be infix in WL source code.

These may not necessarily correspond to Flat functions in WL.
*)
InfixOperatorToParselet[Token`Minus] = InfixOperatorParselet[Token`Minus, Precedence`Infix`Minus]
InfixOperatorToParselet[Token`EqualEqualEqual] = InfixOperatorParselet[Token`EqualEqualEqual, Precedence`EqualEqualEqual]
InfixOperatorToParselet[Token`EqualBangEqual] = InfixOperatorParselet[Token`EqualBangEqual, Precedence`EqualBangEqual]
InfixOperatorToParselet[Token`Plus] = InfixOperatorParselet[Token`Plus, Precedence`Infix`Plus]
InfixOperatorToParselet[Token`Star] = InfixOperatorParselet[Token`Star, Precedence`Star]
InfixOperatorToParselet[Token`Dot] = InfixOperatorParselet[Token`Dot, Precedence`Dot]
InfixOperatorToParselet[Token`StarStar] = InfixOperatorParselet[Token`StarStar, Precedence`StarStar]
InfixOperatorToParselet[Token`AmpAmp] = InfixOperatorParselet[Token`AmpAmp, Precedence`AmpAmp]
InfixOperatorToParselet[Token`BarBar] = InfixOperatorParselet[Token`BarBar, Precedence`BarBar]
InfixOperatorToParselet[Token`Bar] = InfixOperatorParselet[Token`Bar, Precedence`Bar]
InfixOperatorToParselet[Token`LessGreater] = InfixOperatorParselet[Token`LessGreater, Precedence`LessGreater]
InfixOperatorToParselet[Token`TildeTilde] = InfixOperatorParselet[Token`TildeTilde, Precedence`TildeTilde]
InfixOperatorToParselet[Token`AtStar] = InfixOperatorParselet[Token`AtStar, Precedence`AtStar]
InfixOperatorToParselet[Token`SlashStar] = InfixOperatorParselet[Token`SlashStar, Precedence`SlashStar]

(*
Set relations
*)
InfixOperatorToParselet[Token`LongName`Element] = InfixOperatorParselet[Token`LongName`Element, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`Subset] = InfixOperatorParselet[Token`LongName`Subset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`Superset] = InfixOperatorParselet[Token`LongName`Superset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`SubsetEqual] = InfixOperatorParselet[Token`LongName`SubsetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`SupersetEqual] = InfixOperatorParselet[Token`LongName`SupersetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotElement] = InfixOperatorParselet[Token`LongName`NotElement, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSubset] = InfixOperatorParselet[Token`LongName`NotSubset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSuperset] = InfixOperatorParselet[Token`LongName`NotSuperset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSubsetEqual] = InfixOperatorParselet[Token`LongName`NotSubsetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSupersetEqual] = InfixOperatorParselet[Token`LongName`NotSupersetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`SquareSubset] = InfixOperatorParselet[Token`LongName`SquareSubset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`SquareSuperset] = InfixOperatorParselet[Token`LongName`SquareSuperset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSquareSubset] = InfixOperatorParselet[Token`LongName`NotSquareSubset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSquareSuperset] = InfixOperatorParselet[Token`LongName`NotSquareSuperset, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`SquareSubsetEqual] = InfixOperatorParselet[Token`LongName`SquareSubsetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`SquareSupersetEqual] = InfixOperatorParselet[Token`LongName`SquareSupersetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSquareSubsetEqual] = InfixOperatorParselet[Token`LongName`NotSquareSubsetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotSquareSupersetEqual] = InfixOperatorParselet[Token`LongName`NotSquareSupersetEqual, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`ReverseElement] = InfixOperatorParselet[Token`LongName`ReverseElement, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`NotReverseElement] = InfixOperatorParselet[Token`LongName`NotReverseElement, Precedence`Class`SetRelations]
InfixOperatorToParselet[Token`LongName`Distributed] = InfixOperatorParselet[Token`LongName`Distributed, Precedence`Class`SetRelations]

InfixOperatorToParselet[Token`LongName`ImplicitPlus] = InfixOperatorParselet[Token`LongName`ImplicitPlus, Precedence`LongName`ImplicitPlus]
InfixOperatorToParselet[Token`LongName`Times] = InfixOperatorParselet[Token`LongName`Times, Precedence`LongName`Times]
InfixOperatorToParselet[Token`LongName`InvisibleTimes] = InfixOperatorParselet[Token`LongName`InvisibleTimes, Precedence`LongName`InvisibleTimes]
InfixOperatorToParselet[Token`LongName`And] = InfixOperatorParselet[Token`LongName`And, Precedence`LongName`And]
InfixOperatorToParselet[Token`LongName`Or] = InfixOperatorParselet[Token`LongName`Or, Precedence`LongName`Or]
InfixOperatorToParselet[Token`LongName`Xor] = InfixOperatorParselet[Token`LongName`Xor, Precedence`LongName`Xor]
InfixOperatorToParselet[Token`LongName`Nand] = InfixOperatorParselet[Token`LongName`Nand, Precedence`LongName`Nand]
InfixOperatorToParselet[Token`LongName`Nor] = InfixOperatorParselet[Token`LongName`Nor, Precedence`LongName`Nor]

(*
Horizontal arrows
*)
InfixOperatorToParselet[Token`LongName`LeftArrow] = InfixOperatorParselet[Token`LongName`LeftArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`RightArrow] = InfixOperatorParselet[Token`LongName`RightArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`LeftRightArrow] = InfixOperatorParselet[Token`LongName`LeftRightArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`LeftTeeArrow] = InfixOperatorParselet[Token`LongName`LeftTeeArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`RightTeeArrow] = InfixOperatorParselet[Token`LongName`RightTeeArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`RightArrowLeftArrow] = InfixOperatorParselet[Token`LongName`RightArrowLeftArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`LeftArrowRightArrow] = InfixOperatorParselet[Token`LongName`LeftArrowRightArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`DoubleLeftArrow] = InfixOperatorParselet[Token`LongName`DoubleLeftArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`DoubleRightArrow] = InfixOperatorParselet[Token`LongName`DoubleRightArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`DoubleLeftRightArrow] = InfixOperatorParselet[Token`LongName`DoubleLeftRightArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`LeftArrowBar] = InfixOperatorParselet[Token`LongName`LeftArrowBar, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`RightArrowBar] = InfixOperatorParselet[Token`LongName`RightArrowBar, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`ShortRightArrow] = InfixOperatorParselet[Token`LongName`ShortRightArrow, Precedence`Class`HorizontalArrows]
InfixOperatorToParselet[Token`LongName`ShortLeftArrow] = InfixOperatorParselet[Token`LongName`ShortLeftArrow, Precedence`Class`HorizontalArrows]

(*
Diagonal arrow operators
*)
InfixOperatorToParselet[Token`LongName`UpperLeftArrow] = InfixOperatorParselet[Token`LongName`UpperLeftArrow, Precedence`Class`DiagonalArrowOperators]
InfixOperatorToParselet[Token`LongName`UpperRightArrow] = InfixOperatorParselet[Token`LongName`UpperRightArrow, Precedence`Class`DiagonalArrowOperators]
InfixOperatorToParselet[Token`LongName`LowerRightArrow] = InfixOperatorParselet[Token`LongName`LowerRightArrow, Precedence`Class`DiagonalArrowOperators]
InfixOperatorToParselet[Token`LongName`LowerLeftArrow] = InfixOperatorParselet[Token`LongName`LowerLeftArrow, Precedence`Class`DiagonalArrowOperators]

(*
Vector operators
*)
InfixOperatorToParselet[Token`LongName`LeftVector] = InfixOperatorParselet[Token`LongName`LeftVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`RightVector] = InfixOperatorParselet[Token`LongName`RightVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`LeftRightVector] = InfixOperatorParselet[Token`LongName`LeftRightVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`LeftVectorBar] = InfixOperatorParselet[Token`LongName`LeftVectorBar, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`RightVectorBar] = InfixOperatorParselet[Token`LongName`RightVectorBar, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`LeftTeeVector] = InfixOperatorParselet[Token`LongName`LeftTeeVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`RightTeeVector] = InfixOperatorParselet[Token`LongName`RightTeeVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownLeftVector] = InfixOperatorParselet[Token`LongName`DownLeftVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownRightVector] = InfixOperatorParselet[Token`LongName`DownRightVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownLeftRightVector] = InfixOperatorParselet[Token`LongName`DownLeftRightVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownLeftVectorBar] = InfixOperatorParselet[Token`LongName`DownLeftVectorBar, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownRightVectorBar] = InfixOperatorParselet[Token`LongName`DownRightVectorBar, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownLeftTeeVector] = InfixOperatorParselet[Token`LongName`DownLeftTeeVector, Precedence`Class`VectorOperators]
InfixOperatorToParselet[Token`LongName`DownRightTeeVector] = InfixOperatorParselet[Token`LongName`DownRightTeeVector, Precedence`Class`VectorOperators]

(*
Vertical arrow operators
*)
InfixOperatorToParselet[Token`LongName`UpArrow] = InfixOperatorParselet[Token`LongName`UpArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DownArrow] = InfixOperatorParselet[Token`LongName`DownArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`UpDownArrow] = InfixOperatorParselet[Token`LongName`UpDownArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`UpTeeArrow] = InfixOperatorParselet[Token`LongName`UpTeeArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DownTeeArrow] = InfixOperatorParselet[Token`LongName`DownTeeArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`UpArrowDownArrow] = InfixOperatorParselet[Token`LongName`UpArrowDownArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DoubleUpArrow] = InfixOperatorParselet[Token`LongName`DoubleUpArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DoubleDownArrow] = InfixOperatorParselet[Token`LongName`DoubleDownArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DoubleUpDownArrow] = InfixOperatorParselet[Token`LongName`DoubleUpDownArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DownArrowUpArrow] = InfixOperatorParselet[Token`LongName`DownArrowUpArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`LongLeftArrow] = InfixOperatorParselet[Token`LongName`LongLeftArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`LongRightArrow] = InfixOperatorParselet[Token`LongName`LongRightArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`LongLeftRightArrow] = InfixOperatorParselet[Token`LongName`LongLeftRightArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DoubleLongLeftArrow] = InfixOperatorParselet[Token`LongName`DoubleLongLeftArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DoubleLongRightArrow] = InfixOperatorParselet[Token`LongName`DoubleLongRightArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DoubleLongLeftRightArrow] = InfixOperatorParselet[Token`LongName`DoubleLongLeftRightArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`UpArrowBar] = InfixOperatorParselet[Token`LongName`UpArrowBar, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`DownArrowBar] = InfixOperatorParselet[Token`LongName`DownArrowBar, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`ShortUpArrow] = InfixOperatorParselet[Token`LongName`ShortUpArrow, Precedence`Class`VerticalArrowOperators]
InfixOperatorToParselet[Token`LongName`ShortDownArrow] = InfixOperatorParselet[Token`LongName`ShortDownArrow, Precedence`Class`VerticalArrowOperators]


(*
Vertical vector operators
*)
InfixOperatorToParselet[Token`LongName`RightUpVector] = InfixOperatorParselet[Token`LongName`RightUpVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftUpVector] = InfixOperatorParselet[Token`LongName`LeftUpVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`RightDownVector] = InfixOperatorParselet[Token`LongName`RightDownVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftDownVector] = InfixOperatorParselet[Token`LongName`LeftDownVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`RightUpDownVector] = InfixOperatorParselet[Token`LongName`RightUpDownVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftUpDownVector] = InfixOperatorParselet[Token`LongName`LeftUpDownVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`RightUpVectorBar] = InfixOperatorParselet[Token`LongName`RightUpVectorBar, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`RightDownVectorBar] = InfixOperatorParselet[Token`LongName`RightDownVectorBar, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftUpVectorBar] = InfixOperatorParselet[Token`LongName`LeftUpVectorBar, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftDownVectorBar] = InfixOperatorParselet[Token`LongName`LeftDownVectorBar, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`RightUpTeeVector] = InfixOperatorParselet[Token`LongName`RightUpTeeVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`RightDownTeeVector] = InfixOperatorParselet[Token`LongName`RightDownTeeVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftUpTeeVector] = InfixOperatorParselet[Token`LongName`LeftUpTeeVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`LeftDownTeeVector] = InfixOperatorParselet[Token`LongName`LeftDownTeeVector, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`UpEquilibrium] = InfixOperatorParselet[Token`LongName`UpEquilibrium, Precedence`Class`VerticalVectorOperators]
InfixOperatorToParselet[Token`LongName`ReverseUpEquilibrium] = InfixOperatorParselet[Token`LongName`ReverseUpEquilibrium, Precedence`Class`VerticalVectorOperators]


InfixOperatorToParselet[Token`LongName`CenterDot] = InfixOperatorParselet[Token`LongName`CenterDot, Precedence`LongName`CenterDot]
InfixOperatorToParselet[Token`LongName`Equivalent] = InfixOperatorParselet[Token`LongName`Equivalent, Precedence`LongName`Equivalent]
InfixOperatorToParselet[Token`LongName`CircleDot] = InfixOperatorParselet[Token`LongName`CircleDot, Precedence`LongName`CircleDot]
InfixOperatorToParselet[Token`LongName`Conditioned] = InfixOperatorParselet[Token`LongName`Conditioned, Precedence`LongName`Conditioned]

(*
Union operators
*)
InfixOperatorToParselet[Token`LongName`Union] = InfixOperatorParselet[Token`LongName`Union, Precedence`Class`UnionOperators]
InfixOperatorToParselet[Token`LongName`SquareUnion] = InfixOperatorParselet[Token`LongName`SquareUnion, Precedence`Class`UnionOperators]
InfixOperatorToParselet[Token`LongName`UnionPlus] = InfixOperatorParselet[Token`LongName`UnionPlus, Precedence`Class`UnionOperators]

(*
Intersection operators
*)
InfixOperatorToParselet[Token`LongName`Intersection] = InfixOperatorParselet[Token`LongName`Intersection, Precedence`Class`IntersectionOperators]
InfixOperatorToParselet[Token`LongName`SquareIntersection] = InfixOperatorParselet[Token`LongName`SquareIntersection, Precedence`Class`IntersectionOperators]


InfixOperatorToParselet[Token`LongName`TensorWedge] = InfixOperatorParselet[Token`LongName`TensorWedge, Precedence`LongName`TensorWedge]
InfixOperatorToParselet[Token`LongName`TensorProduct] = InfixOperatorParselet[Token`LongName`TensorProduct, Precedence`LongName`TensorProduct]
InfixOperatorToParselet[Token`LongName`Cross] = InfixOperatorParselet[Token`LongName`Cross, Precedence`LongName`Cross]
InfixOperatorToParselet[Token`LongName`SmallCircle] = InfixOperatorParselet[Token`LongName`SmallCircle, Precedence`LongName`SmallCircle]
InfixOperatorToParselet[Token`LongName`Divides] = InfixOperatorParselet[Token`LongName`Divides, Precedence`LongName`Divides]
InfixOperatorToParselet[Token`LongName`VerticalSeparator] = InfixOperatorParselet[Token`LongName`VerticalSeparator, Precedence`LongName`VerticalSeparator]
InfixOperatorToParselet[Token`LongName`Backslash] = InfixOperatorParselet[Token`LongName`Backslash, Precedence`LongName`Backslash]
InfixOperatorToParselet[Token`LongName`Diamond] = InfixOperatorParselet[Token`LongName`Diamond, Precedence`LongName`Diamond]
InfixOperatorToParselet[Token`LongName`Wedge] = InfixOperatorParselet[Token`LongName`Wedge, Precedence`LongName`Wedge]
InfixOperatorToParselet[Token`LongName`Vee] = InfixOperatorParselet[Token`LongName`Vee, Precedence`LongName`Vee]
InfixOperatorToParselet[Token`LongName`CircleTimes] = InfixOperatorParselet[Token`LongName`CircleTimes, Precedence`Infix`LongName`CircleTimes]
InfixOperatorToParselet[Token`LongName`Star] = InfixOperatorParselet[Token`LongName`Star, Precedence`LongName`Star]
InfixOperatorToParselet[Token`LongName`VerticalTilde] = InfixOperatorParselet[Token`LongName`VerticalTilde, Precedence`LongName`VerticalTilde]
InfixOperatorToParselet[Token`LongName`Coproduct] = InfixOperatorParselet[Token`LongName`Coproduct, Precedence`Infix`LongName`Coproduct]
InfixOperatorToParselet[Token`LongName`Cap] = InfixOperatorParselet[Token`LongName`Cap, Precedence`LongName`Cap]
InfixOperatorToParselet[Token`LongName`Cup] = InfixOperatorParselet[Token`LongName`Cup, Precedence`LongName`Cup]
InfixOperatorToParselet[Token`LongName`CirclePlus] = InfixOperatorParselet[Token`LongName`CirclePlus, Precedence`LongName`CirclePlus]
InfixOperatorToParselet[Token`LongName`VerticalBar] = InfixOperatorParselet[Token`LongName`VerticalBar, Precedence`LongName`VerticalBar]
InfixOperatorToParselet[Token`LongName`DoubleVerticalBar] = InfixOperatorParselet[Token`LongName`DoubleVerticalBar, Precedence`LongName`DoubleVerticalBar]
InfixOperatorToParselet[Token`LongName`NotVerticalBar] = InfixOperatorParselet[Token`LongName`NotVerticalBar, Precedence`LongName`NotVerticalBar]
InfixOperatorToParselet[Token`LongName`NotDoubleVerticalBar] = InfixOperatorParselet[Token`LongName`NotDoubleVerticalBar, Precedence`LongName`NotDoubleVerticalBar]

(*
Ordering operators
*)
InfixOperatorToParselet[Token`LongName`LeftTriangle] = InfixOperatorParselet[Token`LongName`LeftTriangle, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`RightTriangle] = InfixOperatorParselet[Token`LongName`RightTriangle, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotLeftTriangle] = InfixOperatorParselet[Token`LongName`NotLeftTriangle, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotRightTriangle] = InfixOperatorParselet[Token`LongName`NotRightTriangle, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`LeftTriangleEqual] = InfixOperatorParselet[Token`LongName`LeftTriangleEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`RightTriangleEqual] = InfixOperatorParselet[Token`LongName`RightTriangleEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotLeftTriangleEqual] = InfixOperatorParselet[Token`LongName`NotLeftTriangleEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotRightTriangleEqual] = InfixOperatorParselet[Token`LongName`NotRightTriangleEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`LeftTriangleBar] = InfixOperatorParselet[Token`LongName`LeftTriangleBar, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`RightTriangleBar] = InfixOperatorParselet[Token`LongName`RightTriangleBar, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotLeftTriangleBar] = InfixOperatorParselet[Token`LongName`NotLeftTriangleBar, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotRightTriangleBar] = InfixOperatorParselet[Token`LongName`NotRightTriangleBar, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`TildeEqual] = InfixOperatorParselet[Token`LongName`TildeEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotTildeEqual] = InfixOperatorParselet[Token`LongName`NotTildeEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`TildeFullEqual] = InfixOperatorParselet[Token`LongName`TildeFullEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotTildeFullEqual] = InfixOperatorParselet[Token`LongName`NotTildeFullEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Tilde] = InfixOperatorParselet[Token`LongName`Tilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotTilde] = InfixOperatorParselet[Token`LongName`NotTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`EqualTilde] = InfixOperatorParselet[Token`LongName`EqualTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotEqualTilde] = InfixOperatorParselet[Token`LongName`NotEqualTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`TildeTilde] = InfixOperatorParselet[Token`LongName`TildeTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotTildeTilde] = InfixOperatorParselet[Token`LongName`NotTildeTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Proportional] = InfixOperatorParselet[Token`LongName`Proportional, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Proportion] = InfixOperatorParselet[Token`LongName`Proportion, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Congruent] = InfixOperatorParselet[Token`LongName`Congruent, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotCongruent] = InfixOperatorParselet[Token`LongName`NotCongruent, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Equilibrium] = InfixOperatorParselet[Token`LongName`Equilibrium, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`ReverseEquilibrium] = InfixOperatorParselet[Token`LongName`ReverseEquilibrium, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`DotEqual] = InfixOperatorParselet[Token`LongName`DotEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Precedes] = InfixOperatorParselet[Token`LongName`Precedes, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`Succeeds] = InfixOperatorParselet[Token`LongName`Succeeds, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`PrecedesEqual] = InfixOperatorParselet[Token`LongName`PrecedesEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`SucceedsEqual] = InfixOperatorParselet[Token`LongName`SucceedsEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`PrecedesTilde] = InfixOperatorParselet[Token`LongName`PrecedesTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`SucceedsTilde] = InfixOperatorParselet[Token`LongName`SucceedsTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`PrecedesSlantEqual] = InfixOperatorParselet[Token`LongName`PrecedesSlantEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`SucceedsSlantEqual] = InfixOperatorParselet[Token`LongName`SucceedsSlantEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotPrecedes] = InfixOperatorParselet[Token`LongName`NotPrecedes, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotSucceeds] = InfixOperatorParselet[Token`LongName`NotSucceeds, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotPrecedesEqual] = InfixOperatorParselet[Token`LongName`NotPrecedesEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotSucceedsEqual] = InfixOperatorParselet[Token`LongName`NotSucceedsEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotPrecedesTilde] = InfixOperatorParselet[Token`LongName`NotPrecedesTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotSucceedsTilde] = InfixOperatorParselet[Token`LongName`NotSucceedsTilde, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotPrecedesSlantEqual] = InfixOperatorParselet[Token`LongName`NotPrecedesSlantEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotSucceedsSlantEqual] = InfixOperatorParselet[Token`LongName`NotSucceedsSlantEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`CupCap] = InfixOperatorParselet[Token`LongName`CupCap, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotCupCap] = InfixOperatorParselet[Token`LongName`NotCupCap, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`HumpEqual] = InfixOperatorParselet[Token`LongName`HumpEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`HumpDownHump] = InfixOperatorParselet[Token`LongName`HumpDownHump, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotHumpEqual] = InfixOperatorParselet[Token`LongName`NotHumpEqual, Precedence`Class`OrderingOperators]
InfixOperatorToParselet[Token`LongName`NotHumpDownHump] = InfixOperatorParselet[Token`LongName`NotHumpDownHump, Precedence`Class`OrderingOperators]

(*
special Inequality
*)
InfixOperatorToParselet[Token`BangEqual] = InfixOperatorParselet[Token`BangEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`EqualEqual] = InfixOperatorParselet[Token`EqualEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`Greater] = InfixOperatorParselet[Token`Greater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`GreaterEqual] = InfixOperatorParselet[Token`GreaterEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LessEqual] = InfixOperatorParselet[Token`LessEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`Less] = InfixOperatorParselet[Token`Less, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`Equal] = InfixOperatorParselet[Token`LongName`Equal, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterEqual] = InfixOperatorParselet[Token`LongName`GreaterEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterEqualLess] = InfixOperatorParselet[Token`LongName`GreaterEqualLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterFullEqual] = InfixOperatorParselet[Token`LongName`GreaterFullEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterGreater] = InfixOperatorParselet[Token`LongName`GreaterGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterLess] = InfixOperatorParselet[Token`LongName`GreaterLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterSlantEqual] = InfixOperatorParselet[Token`LongName`GreaterSlantEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`GreaterTilde] = InfixOperatorParselet[Token`LongName`GreaterTilde, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessEqual] = InfixOperatorParselet[Token`LongName`LessEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessEqualGreater] = InfixOperatorParselet[Token`LongName`LessEqualGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessFullEqual] = InfixOperatorParselet[Token`LongName`LessFullEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessGreater] = InfixOperatorParselet[Token`LongName`LessGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessLess] = InfixOperatorParselet[Token`LongName`LessLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessSlantEqual] = InfixOperatorParselet[Token`LongName`LessSlantEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LessTilde] = InfixOperatorParselet[Token`LongName`LessTilde, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`LongEqual] = InfixOperatorParselet[Token`LongName`LongEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NestedGreaterGreater] = InfixOperatorParselet[Token`LongName`NestedGreaterGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NestedLessLess] = InfixOperatorParselet[Token`LongName`NestedLessLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotEqual] = InfixOperatorParselet[Token`LongName`NotEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreater] = InfixOperatorParselet[Token`LongName`NotGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreaterEqual] = InfixOperatorParselet[Token`LongName`NotGreaterEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreaterFullEqual] = InfixOperatorParselet[Token`LongName`NotGreaterFullEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreaterGreater] = InfixOperatorParselet[Token`LongName`NotGreaterGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreaterLess] = InfixOperatorParselet[Token`LongName`NotGreaterLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreaterSlantEqual] = InfixOperatorParselet[Token`LongName`NotGreaterSlantEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotGreaterTilde] = InfixOperatorParselet[Token`LongName`NotGreaterTilde, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLess] = InfixOperatorParselet[Token`LongName`NotLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLessEqual] = InfixOperatorParselet[Token`LongName`NotLessEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLessFullEqual] = InfixOperatorParselet[Token`LongName`NotLessFullEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLessGreater] = InfixOperatorParselet[Token`LongName`NotLessGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLessLess] = InfixOperatorParselet[Token`LongName`NotLessLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLessSlantEqual] = InfixOperatorParselet[Token`LongName`NotLessSlantEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotLessTilde] = InfixOperatorParselet[Token`LongName`NotLessTilde, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotNestedGreaterGreater] = InfixOperatorParselet[Token`LongName`NotNestedGreaterGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`NotNestedLessLess] = InfixOperatorParselet[Token`LongName`NotNestedLessLess, Precedence`Class`Inequality]
(*
special VectorInequality
*)
InfixOperatorToParselet[Token`LongName`VectorGreater] = InfixOperatorParselet[Token`LongName`VectorGreater, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`VectorGreaterEqual] = InfixOperatorParselet[Token`LongName`VectorGreaterEqual, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`VectorLess] = InfixOperatorParselet[Token`LongName`VectorLess, Precedence`Class`Inequality]
InfixOperatorToParselet[Token`LongName`VectorLessEqual] = InfixOperatorParselet[Token`LongName`VectorLessEqual, Precedence`Class`Inequality]


InfixOperatorToParselet[Token`LongName`PermutationProduct] = InfixOperatorParselet[Token`LongName`PermutationProduct, Precedence`LongName`PermutationProduct]
InfixOperatorToParselet[Token`LongName`Colon] = InfixOperatorParselet[Token`LongName`Colon, Precedence`LongName`Colon]
InfixOperatorToParselet[Token`LongName`Xnor] = InfixOperatorParselet[Token`LongName`Xnor, Precedence`LongName`Xnor]
InfixOperatorToParselet[Token`LongName`Minus] = InfixOperatorParselet[Token`LongName`Minus, Precedence`Infix`LongName`Minus]
InfixOperatorToParselet[Token`Fake`ImplicitTimes] = InfixOperatorParselet[Token`Fake`ImplicitTimes, Precedence`Star]


(*
Postfix
*)
InfixOperatorToParselet[Token`Amp] = PostfixOperatorParselet[Token`Amp, Precedence`Amp]
InfixOperatorToParselet[Token`DotDot] = PostfixOperatorParselet[Token`DotDot, Precedence`DotDot]
InfixOperatorToParselet[Token`Bang] = PostfixOperatorParselet[Token`Bang, Precedence`Postfix`Bang]
InfixOperatorToParselet[Token`MinusMinus] = PostfixOperatorParselet[Token`MinusMinus, Precedence`Postfix`MinusMinus]
InfixOperatorToParselet[Token`PlusPlus] = PostfixOperatorParselet[Token`PlusPlus, Precedence`Postfix`PlusPlus]
InfixOperatorToParselet[Token`DotDotDot] = PostfixOperatorParselet[Token`DotDotDot, Precedence`DotDotDot]
InfixOperatorToParselet[Token`BangBang] = PostfixOperatorParselet[Token`BangBang, Precedence`Postfix`BangBang]
InfixOperatorToParselet[Token`SingleQuote] = PostfixOperatorParselet[Token`SingleQuote, Precedence`SingleQuote]
InfixOperatorToParselet[Token`LongName`Transpose] = PostfixOperatorParselet[Token`LongName`Transpose, Precedence`LongName`Transpose]
InfixOperatorToParselet[Token`LongName`Conjugate] = PostfixOperatorParselet[Token`LongName`Conjugate, Precedence`LongName`Conjugate]
InfixOperatorToParselet[Token`LongName`ConjugateTranspose] = PostfixOperatorParselet[Token`LongName`ConjugateTranspose, Precedence`LongName`ConjugateTranspose]
InfixOperatorToParselet[Token`LongName`HermitianConjugate] = PostfixOperatorParselet[Token`LongName`HermitianConjugate, Precedence`LongName`HermitianConjugate]
InfixOperatorToParselet[Token`LongName`InvisiblePostfixScriptBase] = PostfixOperatorParselet[Token`LongName`InvisiblePostfixScriptBase, Precedence`LongName`InvisiblePostfixScriptBase]


(*
Calls
*)
InfixOperatorToParselet[Token`OpenSquare] = CallParselet[GroupParselet[Token`OpenSquare]]
InfixOperatorToParselet[Token`LongName`LeftDoubleBracket] = CallParselet[GroupParselet[Token`LongName`LeftDoubleBracket]]




(*
trailing ; and , is allowed
*)
InfixOperatorToParselet[Token`Semi] = InfixOperatorWithTrailingParselet[Token`Semi, Precedence`Semi]
InfixOperatorToParselet[Token`Comma] = InfixOperatorWithTrailingParselet[Token`Comma, Precedence`Comma]
InfixOperatorToParselet[Token`LongName`InvisibleComma] = InfixOperatorWithTrailingParselet[Token`LongName`InvisibleComma, Precedence`LongName`InvisibleComma]

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
InfixOperatorToParselet[Token`EqualDot] = EqualDotParselet[]

(*
stringify next token (as a symbol)
*)
InfixOperatorToParselet[Token`ColonColon] = ColonColonParselet[]

(*
stringify next token (as a file)
*)
InfixOperatorToParselet[Token`GreaterGreater] = GreaterGreaterParselet[]
InfixOperatorToParselet[Token`GreaterGreaterGreater] = GreaterGreaterGreaterParselet[]



InfixOperatorToParselet[Token`LinearSyntax`Bang] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`OpenParen] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Star] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`CloseParen] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`At] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Caret] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Under] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Percent] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Plus] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Backtick] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Slash] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Amp] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LinearSyntax`Space] = InfixUnsupportedTokenParselet[]

InfixOperatorToParselet[Token`QuestionQuestion] = InfixUnsupportedTokenParselet[]

(*
Also use for operators that are only valid in StandardForm.
e.g., \[Gradient] does not have an interpretation in InputForm

\[Gradient] is not letterlike, so it needs some kind of categorization,
but it also needs to be prevented from making any valid parses.
*)
InfixOperatorToParselet[Token`LongName`Gradient] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`Divergence] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`Curl] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`Limit] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`MaxLimit] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`MinLimit] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`AutoLeftMatch] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`AutoRightMatch] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`DiscreteShift] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`DifferenceDelta] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`DiscreteRatio] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`Laplacian] = InfixUnsupportedTokenParselet[]
InfixOperatorToParselet[Token`LongName`PartialD] = InfixUnsupportedTokenParselet[]



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
class CallParselet;
class PostfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
#if STARTOFLINE
class StartOfLineParselet;
class StartOfFileParselet;
#endif // STARTOFLINE
class GroupParselet;
class Parselet;
class Parser;

using PrefixParseletPtr = PrefixParselet *;
using InfixParseletPtr = InfixParselet *;
using ContextSensitiveInfixParseletPtr = ContextSensitiveInfixParselet *;


extern std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets;
extern std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets;

extern PrefixParseletPtr contextSensitiveSymbolParselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder1Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder2Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnder3Parselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveUnderDotParselet;
extern ContextSensitiveInfixParseletPtr contextSensitiveColonParselet;
"}

Print["exporting ParseletRegistration.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "ParseletRegistration.h"}], Column[parseletRegistrationCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]




tokensSansCount = DeleteCases[tokens, Token`Count]




formatPrefix[LeafParselet[Precedence`Highest]] := "&highestLeafParselet"

formatPrefix[LeafParselet[Precedence`Lowest]] := "&lowestLeafParselet"

formatPrefix[LeafParselet[Precedence`AssertFalse]] := "&assertingLeafParselet"

formatPrefix[LeafParselet[precedence_]] := "new LeafParselet(" <> toGlobal[precedence] <> ")"


formatPrefix[SymbolParselet[]] := "&symbolParselet"

formatPrefix[UnderParselet[1]] := "new UnderParselet<BlankNode, PatternBlankNode>()"

formatPrefix[UnderParselet[2]] := "new UnderParselet<BlankSequenceNode, PatternBlankSequenceNode>()"

formatPrefix[UnderParselet[3]] := "new UnderParselet<BlankNullSequenceNode, PatternBlankNullSequenceNode>()"

formatPrefix[UnderDotParselet[]] := "new UnderDotParselet()"

formatPrefix[LessLessParselet[]] := "&lessLessParselet"

formatPrefix[SemiSemiParselet[]] := "&semiSemiParselet"

formatPrefix[LinearSyntaxOpenParenParselet[]] := "&linearSyntaxOpenParenParselet"

formatPrefix[IntegralParselet[]] := "&integralParselet"

formatPrefix[PrefixOperatorParselet[tok_, precedence_]] := "new PrefixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ")"

formatPrefix[GroupParselet[tok_]] := "new GroupParselet(" <> toGlobal[tok] <> ")"

formatInfix[InfixAssertFalseParselet[]] := "&infixAssertFalseParselet"

formatInfix[InfixEndOfFileParselet[]] := "&infixEndOfFileParselet"

formatInfix[InfixErrorParselet[]] := "&infixErrorParselet"

formatInfix[InfixCloserParselet[]] := "&infixCloserParselet"

formatInfix[InfixUnsupportedTokenParselet[]] := "&infixUnsupportedTokenParselet"

formatInfix[InfixImplicitTimesParselet[]] := "&infixImplicitTimesParselet"



formatInfix[BinaryOperatorParselet[tok_, precedence_, associativity_]] := "new BinaryOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ", " <> toGlobal[associativity] <> ")"

formatInfix[InfixOperatorParselet[tok_, precedence_]] := "new InfixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ")"

formatInfix[InfixOperatorWithTrailingParselet[tok_, precedence_]] := "new InfixOperatorWithTrailingParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ")"

formatInfix[PostfixOperatorParselet[tok_, precedence_]] := "new PostfixOperatorParselet(" <> toGlobal[tok] <> ", " <> toGlobal[precedence] <> ")"

formatInfix[ColonParselet[]] := "&colonParselet"

formatInfix[CallParselet[groupParselet_]] := "new CallParselet(" <> formatPrefix[groupParselet] <> ")"

formatInfix[EqualParselet[]] := "new EqualParselet()"

formatInfix[EqualDotParselet[]] := "new EqualDotParselet()"

formatInfix[TildeParselet[]] := "&tildeParselet"

formatInfix[SlashColonParselet[]] := "&slashColonParselet"

formatInfix[SemiSemiParselet[]] := "&semiSemiParselet"

formatInfix[ColonColonParselet[]] := "&colonColonParselet"

formatInfix[GreaterGreaterParselet[]] := "&greaterGreaterParselet"

formatInfix[GreaterGreaterGreaterParselet[]] := "&greaterGreaterGreaterParselet"

formatInfix[DifferentialDParselet[]] := "&differentialDParselet"

formatInfix[ToplevelNewlineParselet[]] := "&toplevelNewlineParselet"





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

auto highestLeafParselet = LeafParselet(PRECEDENCE_HIGHEST);

auto lowestLeafParselet = LeafParselet(PRECEDENCE_LOWEST);

auto assertingLeafParselet = LeafParselet(PRECEDENCE_ASSERTFALSE);

auto infixImplicitTimesParselet = InfixImplicitTimesParselet();

auto infixAssertFalseParselet = InfixAssertFalseParselet();

auto infixEndOfFileParselet = InfixEndOfFileParselet();

auto infixErrorParselet = InfixErrorParselet();

auto infixUnsupportedTokenParselet = InfixUnsupportedTokenParselet();

auto infixCloserParselet = InfixCloserParselet();


auto symbolParselet = SymbolParselet();

auto lessLessParselet = LessLessParselet();

auto semiSemiParselet = SemiSemiParselet();

auto linearSyntaxOpenParenParselet = LinearSyntaxOpenParenParselet();

auto integralParselet = IntegralParselet();

auto toplevelNewlineParselet = ToplevelNewlineParselet();

auto colonParselet = ColonParselet();

auto tildeParselet = TildeParselet();

auto colonColonParselet = ColonColonParselet();

auto greaterGreaterParselet = GreaterGreaterParselet();

auto slashColonParselet = SlashColonParselet();

auto greaterGreaterGreaterParselet = GreaterGreaterGreaterParselet();

auto differentialDParselet = DifferentialDParselet();


//
// T: BlankNode, BlankSequenceNode, BlankNullSequenceNode
// U: PatternBlankNode, PatternBlankSequenceNode, PatternBlankNullSequenceNode
//
template<class T, class U>
class UnderParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
public:
    
    UnderParselet() {}

        NodePtr parse0(Token TokIn, ParserContext Ctxt) const {
        
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        auto Tok = TheParser->currentToken(Ctxt);
        
        NodePtr Blank;
        if (Tok.Tok == TOKEN_SYMBOL) {
            
            auto Sym2 = contextSensitiveSymbolParselet->parse(Tok, Ctxt);
            
            NodeSeq Args(1 + 1);
            Args.append(std::move(Under));
            Args.append(std::move(Sym2));
            
            Blank = NodePtr(new T(std::move(Args)));
            
            Tok = TheParser->currentToken(Ctxt);
            
        } else if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
            
            //
            // Something like  _a`
            //
            // It's nice to include the error inside of the blank
            //
            
            auto parselet = prefixParselets[Tok.Tok.value()];
            
            auto ErrorSym2 = parselet->parse(Tok, Ctxt);
            
            NodeSeq Args(1 + 1);
            Args.append(std::move(Under));
            Args.append(std::move(ErrorSym2));
            
            Blank = NodePtr(new T(std::move(Args)));
            
            Tok = TheParser->currentToken(Ctxt);
            
        } else {
            Blank = std::move(Under);
        }
        
        return Blank;
    }
    
    NodePtr parse1(NodePtr Blank, Token Tok, ParserContext Ctxt) const {
        
        LeafSeq ArgsTest;
        
        Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest);
        
        //
        // For something like _:\"\"  when parsing _
        // ColonFlag == false
        // the : here is Optional, and so we want to go parse with ColonParselet's parseContextSensitive method
        //
        // For something like a:_:\"\"  when parsing _
        // ColonFlag == true
        // make sure to not parse the second : here
        // We are already inside ColonParselet from the first :, and so ColonParselet will also handle the second :
        //
        if (Tok.Tok == TOKEN_COLON) {
            
            if ((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
                
                NodeSeq BlankSeq(1 + 1);
                BlankSeq.append(std::move(Blank));
                BlankSeq.appendIfNonEmpty(std::move(ArgsTest));
                
                return contextSensitiveColonParselet->parseContextSensitive(std::move(BlankSeq), Tok, Ctxt);
            }
        }
        
        return Blank;
    }
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    NodePtr parse(Token TokIn, ParserContext Ctxt) const override {
        
        auto Blank = parse0(TokIn, Ctxt);
        
        auto Tok = TheParser->currentToken(Ctxt);
        
        return parse1(std::move(Blank), Tok, Ctxt);
    }
    

    //
    // infix
    //
    // Something like  a_b
    //
    // Called from other parselets
    //
    NodePtr parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const override {
        
        NodeSeq Args(1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        
        auto Blank = parse0(TokIn, Ctxt);
        Args.append(NodePtr(std::move(Blank)));

        auto Pat = NodePtr(new U(std::move(Args)));
        
        auto Tok = TheParser->currentToken(Ctxt);

        return parse1(std::move(Pat), Tok, Ctxt);
    }
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_UNDER;
    }
};

class UnderDotParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
public:
    
    UnderDotParselet() {}

    NodePtr parse0(Token TokIn, ParserContext Ctxt) const {
        
        auto UnderDot = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        NodePtr Blank;
        
        NodeSeq Args(1);
        Args.append(std::move(UnderDot));
        
        Blank = NodePtr(new OptionalDefaultNode(std::move(Args)));
        
        return Blank;
    }
    
    //
    // prefix
    //
    // Something like  _.
    //
    NodePtr parse(Token TokIn, ParserContext Ctxt) const override {
        
        auto Blank = parse0(TokIn, Ctxt);
        
        TheParser->nextToken(TokIn);
        
        return Blank;
    }
    
    //
    // infix
    //
    // Something like  a_.
    //
    // Called from other parselets
    //
    NodePtr parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const override {
        
        NodeSeq Args(1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        
        auto Blank = parse0(TokIn, Ctxt);
        Args.append(NodePtr(std::move(Blank)));

        auto Pat = NodePtr(new PatternOptionalDefaultNode(std::move(Args)));

        return Pat;
    }
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_UNDER;
    }
};



std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets {{"} ~Join~

(Row[{"  ", formatPrefix[PrefixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"}};

std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets {{"} ~Join~

(Row[{"  ", formatInfix[InfixOperatorToParselet[#]], ", ", "// ", ToString[#]}]& /@ tokensSansCount) ~Join~

{"}};

PrefixParseletPtr contextSensitiveSymbolParselet(&highestLeafParselet);
ContextSensitiveInfixParseletPtr contextSensitiveUnder1Parselet(new UnderParselet<BlankNode, PatternBlankNode>());
ContextSensitiveInfixParseletPtr contextSensitiveUnder2Parselet(new UnderParselet<BlankSequenceNode, PatternBlankSequenceNode>());
ContextSensitiveInfixParseletPtr contextSensitiveUnder3Parselet(new UnderParselet<BlankNullSequenceNode, PatternBlankNullSequenceNode>());
ContextSensitiveInfixParseletPtr contextSensitiveUnderDotParselet(new UnderDotParselet());
ContextSensitiveInfixParseletPtr contextSensitiveColonParselet(&colonParselet);
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

