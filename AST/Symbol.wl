BeginPackage["AST`Symbol`"]

(*
list of all node symbols
*)
$Nodes

(*
list of all option symbols
*)
$Options

(*
list of symbols used for tokens
*)
$Tokens

(*
list of all other symbols used by parser
*)
$Miscellaneous


(*
list of symbols used for groups
*)
$Groups


$Characters


Begin["`Private`"]

Needs["AST`"]


$Nodes = {
SymbolNode,
StringNode,
NumberNode,

BlankNode,
BlankSequenceNode,
BlankNullSequenceNode,
OptionalDefaultNode,
PatternBlankNode,
PatternBlankSequenceNode,
PatternBlankNullSequenceNode,
OptionalDefaultPatternNode,

SlotNode,
SlotSequenceNode,
OutNode,
(*InternalEmptyNode,*)
PrefixNode,
BinaryNode,
TernaryNode,
InfixNode,
PostfixNode,
GroupNode,
CallNode,

InternalTokenNode,
InternalAllNode,
InternalDotNode,
InternalNullNode,
InternalOneNode,


FileNode,

SyntaxErrorNode,
CallMissingCloserNode

}

$Options = {
DerivativeOrder,
Source,
SyntaxIssues
}


$Miscellaneous = {
All,
(* when parsing f[1,] then we need to parse as f[1,Null] *)
Null,
SyntaxIssue,

(* for Nodes *)
File,

Out,
Slot,
SlotSequence,

Blank,
BlankSequence,
BlankNullSequence,
OptionalDefault,
PatternBlank,
PatternBlankSequence,
PatternBlankNullSequence,
OptionalDefaultPattern,

TernarySlashColon,
TagSet,
TagSetDelayed,
TagUnset,

TernaryTilde,

(*InternalEmpty,*)
InternalInvalid,

HoldForm[Nothing],

Nothing
}

$Tokens = {
Token
}

$Characters = {
WLCharacter
}

$Groups = {
List,
GroupMissingOpenerList,
GroupMissingCloserList,

Association,
GroupMissingOpenerAssociation,
GroupMissingCloserAssociation,

AngleBracket,
GroupMissingOpenerAngleBracket,
GroupMissingCloserAngleBracket,

Ceiling,
GroupMissingOpenerCeiling,
GroupMissingCloserCeiling,

Floor,
GroupMissingOpenerFloor,
GroupMissingCloserFloor,

GroupDoubleBracket,
GroupMissingOpenerDoubleBracket,
GroupMissingCloserDoubleBracket,

GroupSquare,
GroupMissingOpenerSquare,
GroupMissingCloserSquare,

BracketingBar,
GroupMissingOpenerBracketingBar,
GroupMissingCloserBracketingBar,

DoubleBracketingBar,
GroupMissingOpenerDoubleBracketingBar,
GroupMissingCloserDoubleBracketingBar,

GroupParen,
GroupMissingOpenerParen,
GroupMissingCloserParen,

GroupLinearSyntaxParen,
GroupMissingOpenerLinearSyntaxParen,
GroupMissingCloserLinearSyntaxParen,

Nothing
}




(*
These are nice strings, appropriate for using in other source languages.
So they are escaped accordingly.
*)







PrefixOperatorToSymbol[Token`Operator`Bang] = Not
PrefixOperatorToSymbol[Token`Operator`PlusPlus] = PreIncrement
PrefixOperatorToSymbol[Token`Operator`LessLess] = Get
PrefixOperatorToSymbol[Token`Operator`MinusMinus] = PreDecrement
PrefixOperatorToSymbol[Token`Operator`Plus] = Plus
PrefixOperatorToSymbol[Token`Operator`Minus] = Minus

PrefixOperatorToSymbol[Token`Operator`LongName`Sqrt] = Sqrt
PrefixOperatorToSymbol[Token`Operator`LongName`Not] = Not
PrefixOperatorToSymbol[Token`Operator`LongName`PlusMinus] = PlusMinus
PrefixOperatorToSymbol[Token`Operator`LongName`Sum] = Sum
PrefixOperatorToSymbol[Token`Operator`LongName`MinusPlus] = MinusPlus
PrefixOperatorToSymbol[Token`Operator`LongName`DifferentialD] = DifferentialD
PrefixOperatorToSymbol[Token`Operator`LongName`Minus] = Minus
PrefixOperatorToSymbol[Token`Operator`LongName`Del] = Del
PrefixOperatorToSymbol[Token`Operator`LongName`Square] = Square
PrefixOperatorToSymbol[Token`Operator`LongName`Integral] = Integral
PrefixOperatorToSymbol[Token`Operator`LongName`ContourIntegral] = ContourIntegral
PrefixOperatorToSymbol[Token`Operator`LongName`DoubleContourIntegral] = DoubleContourIntegral
PrefixOperatorToSymbol[Token`Operator`LongName`ClockwiseContourIntegral] = ClockwiseContourIntegral
PrefixOperatorToSymbol[Token`Operator`LongName`CounterClockwiseContourIntegral] = CounterClockwiseContourIntegral
PrefixOperatorToSymbol[Token`Operator`LongName`Product] = Product
PrefixOperatorToSymbol[Token`Operator`LongName`InvisiblePrefixScriptBase] = PrefixInvisiblePrefixScriptBase

PrefixOperatorToSymbol[Token`Operator`LinearSyntax`Bang] = PrefixLinearSyntaxBang

SymbolToPrefixOperatorString[Not] = "!"
(* extra space, prevent confusion with --, with e.g., - - 12.34 *)
SymbolToPrefixOperatorString[Minus] = " -"
SymbolToPrefixOperatorString[PreIncrement] = "++"
SymbolToPrefixOperatorString[PreDecrement] = "--"
(* extra space *)
SymbolToPrefixOperatorString[Plus] = " +"
SymbolToPrefixOperatorString[PrefixLinearSyntaxBang] = "\\!"
SymbolToPrefixOperatorString[Get] = "<<"
SymbolToPrefixOperatorString[Sqrt] = "\\[Sqrt]"
SymbolToPrefixOperatorString[PlusMinus] = "\\[PlusMinus]"
SymbolToPrefixOperatorString[MinusPlus] = "\\[MinusPlus]"
SymbolToPrefixOperatorString[DifferentialD] = "\\[DifferentialD]"
SymbolToPrefixOperatorString[Integral] = "\\[Integral]"
SymbolToPrefixOperatorString[ContourIntegral] = "\\[ContourIntegral]"
SymbolToPrefixOperatorString[DoubleContourIntegral] = "\\[DoubleContourIntegral]"
SymbolToPrefixOperatorString[ClockwiseContourIntegral] = "\\[ClockwiseContourIntegral]"
SymbolToPrefixOperatorString[CounterClockwiseContourIntegral] = "\\[CounterClockwiseContourIntegral]"
SymbolToPrefixOperatorString[Sum] = "\\[Sum]"
SymbolToPrefixOperatorString[Product] = "\\[Product]"
SymbolToPrefixOperatorString[Square] = "\\[Square]"
SymbolToPrefixOperatorString[Del] = "\\[Del]"
SymbolToPrefixOperatorString[PrefixInvisiblePrefixScriptBase] = "\\[InvisiblePrefixScriptBase]"










PostfixOperatorToSymbol[Token`Operator`DotDot] = Repeated
PostfixOperatorToSymbol[Token`Operator`Bang] = Factorial
PostfixOperatorToSymbol[Token`Operator`MinusMinus] = Decrement
PostfixOperatorToSymbol[Token`Operator`PlusPlus] = Increment
PostfixOperatorToSymbol[Token`Operator`DotDotDot] = RepeatedNull
PostfixOperatorToSymbol[Token`Operator`Amp] = Function
PostfixOperatorToSymbol[Token`Operator`BangBang] = Factorial2
PostfixOperatorToSymbol[Token`Operator`Tick] = Derivative
   
PostfixOperatorToSymbol[Token`Operator`LongName`Transpose] = Transpose
PostfixOperatorToSymbol[Token`Operator`LongName`Conjugate] = Conjugate
PostfixOperatorToSymbol[Token`Operator`LongName`ConjugateTranspose] = ConjugateTranspose
PostfixOperatorToSymbol[Token`Operator`LongName`HermitianConjugate] = PostfixHermitianConjugate
PostfixOperatorToSymbol[Token`Operator`LongName`InvisiblePostfixScriptBase] = PostfixInvisiblePostfixScriptBase

(* extra space , for e.g. a& & b *)
SymbolToPostfixOperatorString[Function] = "& "
(* extra space on both sides, for e.g. 0. .. *)
SymbolToPostfixOperatorString[Repeated] = " .. "
SymbolToPostfixOperatorString[Increment] = "++"
SymbolToPostfixOperatorString[Decrement] = "--"
(* extra space *)
SymbolToPostfixOperatorString[RepeatedNull] = " ... "
(* extra space *)
SymbolToPostfixOperatorString[Factorial] = "! "
(* extra space *)
SymbolToPostfixOperatorString[Factorial2] = "!! "
(* not used *)
SymbolToPostfixOperatorString[Derivative] = "INVALID"

SymbolToPostfixOperatorString[Transpose] = "\\[Transpose]"
SymbolToPostfixOperatorString[Conjugate] = "\\[Conjugate]"
SymbolToPostfixOperatorString[ConjugateTranspose] = "\\[ConjugateTranspose]"
SymbolToPostfixOperatorString[PostfixHermitianConjugate] = "\\[HermitianConjugate]"
SymbolToPostfixOperatorString[PostfixInvisiblePostfixScriptBase] = "\\[InvisiblePostfixScriptBase]"








(*
Binary
*)
(* inequality operators *)
BinaryOperatorToSymbol[Token`Operator`EqualEqual] = Equal
BinaryOperatorToSymbol[Token`Operator`BangEqual] = Unequal
BinaryOperatorToSymbol[Token`Operator`Less] = Less
BinaryOperatorToSymbol[Token`Operator`Greater] = Greater
BinaryOperatorToSymbol[Token`Operator`LessEqual] = LessEqual
BinaryOperatorToSymbol[Token`Operator`GreaterEqual] = GreaterEqual

(* other flattening operators *)
BinaryOperatorToSymbol[Token`Operator`EqualEqualEqual] = SameQ
BinaryOperatorToSymbol[Token`Operator`EqualBangEqual] = UnsameQ
BinaryOperatorToSymbol[Token`Operator`AtStar] = Composition
BinaryOperatorToSymbol[Token`Operator`SlashStar] = RightComposition

BinaryOperatorToSymbol[Token`Operator`SlashAt] = Map
BinaryOperatorToSymbol[Token`Operator`Equal] = Set
BinaryOperatorToSymbol[Token`Operator`Caret] = Power
BinaryOperatorToSymbol[Token`Operator`ColonEqual] = SetDelayed
BinaryOperatorToSymbol[Token`Operator`MinusGreater] = Rule
BinaryOperatorToSymbol[Token`Operator`Question] = PatternTest
BinaryOperatorToSymbol[Token`Operator`AtAt] = Apply
BinaryOperatorToSymbol[Token`Operator`SlashSemi] = Condition
BinaryOperatorToSymbol[Token`Operator`SlashDot] = ReplaceAll
BinaryOperatorToSymbol[Token`Operator`ColonGreater] = RuleDelayed
BinaryOperatorToSymbol[Token`Operator`SlashSlashDot] = ReplaceRepeated
BinaryOperatorToSymbol[Token`Operator`PlusEqual] = AddTo
BinaryOperatorToSymbol[Token`Operator`StarEqual] = TimesBy
BinaryOperatorToSymbol[Token`Operator`MinusEqual] = SubtractFrom
BinaryOperatorToSymbol[Token`Operator`SlashEqual] = DivideBy
BinaryOperatorToSymbol[Token`Operator`CaretEqual] = UpSet
BinaryOperatorToSymbol[Token`Operator`CaretColonEqual] = UpSetDelayed
If[$VersionNumber >= 11.1, ToExpression["BinaryOperatorToSymbol[Token`Operator`LessMinusGreater] = TwoWayRule"]]
BinaryOperatorToSymbol[Token`Operator`SlashSlashAt] = MapAll
BinaryOperatorToSymbol[Token`Operator`ColonColon] = MessageName
BinaryOperatorToSymbol[Token`Operator`GreaterGreater] = Put
BinaryOperatorToSymbol[Token`Operator`SlashSlash] = BinarySlashSlash
BinaryOperatorToSymbol[Token`Operator`SemiSemi] = Span
BinaryOperatorToSymbol[Token`Operator`At] = BinaryAt
BinaryOperatorToSymbol[Token`Operator`AtAtAt] = BinaryAtAtAt

BinaryOperatorToSymbol[Token`Operator`Fake`EqualDot] = Unset
BinaryOperatorToSymbol[Token`Operator`Fake`PatternColon] = Pattern
BinaryOperatorToSymbol[Token`Operator`Fake`OptionalColon] = Optional
   
BinaryOperatorToSymbol[Token`Operator`LongName`Element] = Element
BinaryOperatorToSymbol[Token`Operator`LongName`RightTeeArrow] = RightTeeArrow
BinaryOperatorToSymbol[Token`Operator`LongName`TildeTilde] = TildeTilde
BinaryOperatorToSymbol[Token`Operator`LongName`SubsetEqual] = SubsetEqual
BinaryOperatorToSymbol[Token`Operator`LongName`Subset] = Subset
BinaryOperatorToSymbol[Token`Operator`LongName`Implies] = Implies
BinaryOperatorToSymbol[Token`Operator`LongName`NotTildeTilde] = NotTildeTilde
BinaryOperatorToSymbol[Token`Operator`LongName`PlusMinus] = PlusMinus
BinaryOperatorToSymbol[Token`Operator`LongName`Equivalent] = Equivalent
BinaryOperatorToSymbol[Token`Operator`LongName`LeftTriangleEqual] = LeftTriangleEqual
BinaryOperatorToSymbol[Token`Operator`LongName`TildeEqual] = TildeEqual
BinaryOperatorToSymbol[Token`Operator`LongName`SupersetEqual] = SupersetEqual
BinaryOperatorToSymbol[Token`Operator`LongName`TildeFullEqual] = TildeFullEqual
BinaryOperatorToSymbol[Token`Operator`LongName`DirectedEdge] = DirectedEdge
BinaryOperatorToSymbol[Token`Operator`LongName`NotElement] = NotElement
BinaryOperatorToSymbol[Token`Operator`LongName`NotTildeFullEqual] = NotTildeFullEqual
BinaryOperatorToSymbol[Token`Operator`LongName`CircleDot] = CircleDot
BinaryOperatorToSymbol[Token`Operator`LongName`Rule] = Rule
BinaryOperatorToSymbol[Token`Operator`LongName`Equal] = Equal
BinaryOperatorToSymbol[Token`Operator`LongName`LessEqual] = LessEqual
BinaryOperatorToSymbol[Token`Operator`LongName`RuleDelayed] = RuleDelayed
BinaryOperatorToSymbol[Token`Operator`LongName`UndirectedEdge] = UndirectedEdge
BinaryOperatorToSymbol[Token`Operator`LongName`Function] = Function
BinaryOperatorToSymbol[Token`Operator`LongName`Distributed] = Distributed
BinaryOperatorToSymbol[Token`Operator`LongName`Conditioned] = Conditioned
BinaryOperatorToSymbol[Token`Operator`LongName`Union] = Union
BinaryOperatorToSymbol[Token`Operator`LongName`Intersection] = Intersection
BinaryOperatorToSymbol[Token`Operator`LongName`NotEqual] = Unequal
BinaryOperatorToSymbol[Token`Operator`LongName`TensorWedge] = TensorWedge
BinaryOperatorToSymbol[Token`Operator`LongName`CenterDot] = CenterDot
BinaryOperatorToSymbol[Token`Operator`LongName`Cross] = Cross
BinaryOperatorToSymbol[Token`Operator`LongName`GreaterTilde] = GreaterTilde
BinaryOperatorToSymbol[Token`Operator`LongName`Proportional] = Proportional
BinaryOperatorToSymbol[Token`Operator`LongName`LessLess] = LessLess
BinaryOperatorToSymbol[Token`Operator`LongName`Congruent] = Congruent
BinaryOperatorToSymbol[Token`Operator`LongName`Tilde] = Tilde
BinaryOperatorToSymbol[Token`Operator`LongName`MinusPlus] = MinusPlus
BinaryOperatorToSymbol[Token`Operator`LongName`DoubleLongLeftRightArrow] = DoubleLongLeftRightArrow
BinaryOperatorToSymbol[Token`Operator`LongName`RightArrow] = RightArrow
BinaryOperatorToSymbol[Token`Operator`LongName`SmallCircle] = SmallCircle
BinaryOperatorToSymbol[Token`Operator`LongName`DoubleLongRightArrow] = DoubleLongRightArrow
BinaryOperatorToSymbol[Token`Operator`LongName`Divides] = Divisible
BinaryOperatorToSymbol[Token`Operator`LongName`LeftRightArrow] = LeftRightArrow
BinaryOperatorToSymbol[Token`Operator`LongName`VerticalSeparator] = VerticalSeparator
BinaryOperatorToSymbol[Token`Operator`LongName`LongRightArrow] = LongRightArrow
If[$VersionNumber >= 11.1, ToExpression["BinaryOperatorToSymbol[Token`Operator`LongName`TwoWayRule] = TwoWayRule"]]
BinaryOperatorToSymbol[Token`Operator`LongName`InvisibleApplication] = BinaryInvisibleApplication
BinaryOperatorToSymbol[Token`Operator`LongName`Backslash] = Backslash
BinaryOperatorToSymbol[Token`Operator`LongName`Diamond] = Diamond
BinaryOperatorToSymbol[Token`Operator`LongName`Wedge] = Wedge
BinaryOperatorToSymbol[Token`Operator`LongName`Vee] = Vee
BinaryOperatorToSymbol[Token`Operator`LongName`CircleTimes] = CircleTimes
BinaryOperatorToSymbol[Token`Operator`LongName`Star] = Star
BinaryOperatorToSymbol[Token`Operator`LongName`VerticalTilde] = VerticalTilde
BinaryOperatorToSymbol[Token`Operator`LongName`Coproduct] = Coproduct
BinaryOperatorToSymbol[Token`Operator`LongName`Cap] = Cap
BinaryOperatorToSymbol[Token`Operator`LongName`Cup] = Cup
BinaryOperatorToSymbol[Token`Operator`LongName`CirclePlus] = CirclePlus
BinaryOperatorToSymbol[Token`Operator`LongName`CircleMinus] = CircleMinus
BinaryOperatorToSymbol[Token`Operator`LongName`RightTriangle] = RightTriangle
BinaryOperatorToSymbol[Token`Operator`LongName`LeftTriangle] = LeftTriangle

(* inequality operators *)
SymbolToBinaryOperatorString[Equal] = "=="
SymbolToBinaryOperatorString[Unequal] = "!="
SymbolToBinaryOperatorString[Less] = "<"
SymbolToBinaryOperatorString[Greater] = ">"
SymbolToBinaryOperatorString[LessEqual] = "<="
SymbolToBinaryOperatorString[GreaterEqual] = ">="

(* other flattening operators *)
SymbolToBinaryOperatorString[SameQ] = "==="
SymbolToBinaryOperatorString[UnsameQ] = "=!="
SymbolToBinaryOperatorString[Composition] = "@*"
SymbolToBinaryOperatorString[RightComposition] = "/*"

SymbolToBinaryOperatorString[Map] = "/@"
SymbolToBinaryOperatorString[Set] = "="
SymbolToBinaryOperatorString[SetDelayed] = ":="
SymbolToBinaryOperatorString[Rule] = "->"
SymbolToBinaryOperatorString[PatternTest] = "?"
SymbolToBinaryOperatorString[MessageName] = "::"
SymbolToBinaryOperatorString[Pattern] = ":"
SymbolToBinaryOperatorString[Apply] = "@@"
SymbolToBinaryOperatorString[Condition] = "/;"
(* extra space, ending in . *)
SymbolToBinaryOperatorString[ReplaceAll] = "/. "
SymbolToBinaryOperatorString[RuleDelayed] = ":>"
SymbolToBinaryOperatorString[Span] = ";;"
SymbolToBinaryOperatorString[BinaryAt] = "@"
SymbolToBinaryOperatorString[Power] = "^"
SymbolToBinaryOperatorString[BinaryAtAtAt] = "@@@"
SymbolToBinaryOperatorString[BinarySlashSlash] = "//"
(* extra space, ending in . *)
SymbolToBinaryOperatorString[ReplaceRepeated] = "//. "
SymbolToBinaryOperatorString[AddTo] = "+="
SymbolToBinaryOperatorString[Optional] = ":"
SymbolToBinaryOperatorString[SubtractFrom] = "-="
SymbolToBinaryOperatorString[TimesBy] = "*="
SymbolToBinaryOperatorString[DivideBy] = "/="
SymbolToBinaryOperatorString[UpSetDelayed] = "^:="
SymbolToBinaryOperatorString[UpSet] = "^="
SymbolToBinaryOperatorString[MapAll] = "//@"
SymbolToBinaryOperatorString[Put] = ">>"
(* extra space, ending in . *)
SymbolToBinaryOperatorString[Unset] = "=. "

SymbolToBinaryOperatorString[Element] = "\\[Element]"
SymbolToBinaryOperatorString[SubsetEqual] = "\\[SubsetEqual]"
SymbolToBinaryOperatorString[RightTeeArrow] = "\\[RightTeeArrow]"
SymbolToBinaryOperatorString[LeftTriangleEqual] = "\\[LeftTriangleEqual]"
SymbolToBinaryOperatorString[TildeFullEqual] = "\\[TildeFullEqual]"
SymbolToBinaryOperatorString[NotTildeFullEqual] = "\\[NotTildeFullEqual]"
SymbolToBinaryOperatorString[TildeTilde] = "\\[TildeTilde]"
SymbolToBinaryOperatorString[NotTildeTilde] = "\\[NotTildeTilde]"
SymbolToBinaryOperatorString[TildeEqual] = "\\[TildeEqual]"
SymbolToBinaryOperatorString[Subset] = "\\[Subset]"
SymbolToBinaryOperatorString[PlusMinus] = "\\[PlusMinus]"
SymbolToBinaryOperatorString[NotElement] = "\\[NotElement]"
SymbolToBinaryOperatorString[Implies] = "\\[Implies]"
SymbolToBinaryOperatorString[Equivalent] = "\\[Equivalent]"
SymbolToBinaryOperatorString[DirectedEdge] = "\\[DirectedEdge]"
SymbolToBinaryOperatorString[SupersetEqual] = "\\[SupersetEqual]"
If[$VersionNumber >= 11.1, ToExpression["SymbolToBinaryOperatorString[TwoWayRule] = \"\\\\[TwoWayRule]\""]]
SymbolToBinaryOperatorString[UndirectedEdge] = "\\[UndirectedEdge]"
SymbolToBinaryOperatorString[Function] = "\\[Function]"
SymbolToBinaryOperatorString[Intersection] = "\\[Intersection]"
SymbolToBinaryOperatorString[Union] = "\\[Union]"
SymbolToBinaryOperatorString[Distributed] = "\\[Distributed]"
SymbolToBinaryOperatorString[Conditioned] = "\\[Conditioned]"
SymbolToBinaryOperatorString[CircleDot] = "\\[CircleDot]"
SymbolToBinaryOperatorString[TensorWedge] = "\\[TensorWedge]"
SymbolToBinaryOperatorString[CenterDot] = "\\[CenterDot]"
SymbolToBinaryOperatorString[Cross] = "\\[Cross]"
SymbolToBinaryOperatorString[GreaterTilde] = "\\[GreaterTilde]"
SymbolToBinaryOperatorString[Proportional] = "\\[Proportional]"
SymbolToBinaryOperatorString[LessLess] = "\\[LessLess]"
SymbolToBinaryOperatorString[Congruent] = "\\[Congruent]"
SymbolToBinaryOperatorString[Tilde] = "\\[Tilde]"
SymbolToBinaryOperatorString[MinusPlus] = "\\[MinusPlus]"
SymbolToBinaryOperatorString[DoubleLongLeftRightArrow] = "\\[DoubleLongLeftRightArrow]"
SymbolToBinaryOperatorString[RightArrow] = "\\[RightArrow]"
SymbolToBinaryOperatorString[SmallCircle] = "\\[SmallCircle]"
SymbolToBinaryOperatorString[DoubleLongRightArrow] = "\\[DoubleLongRightArrow]"
SymbolToBinaryOperatorString[Divisible] = "\\[Divides]"
SymbolToBinaryOperatorString[LeftRightArrow] = "\\[LeftRightArrow]"
SymbolToBinaryOperatorString[VerticalSeparator] = "\\[VerticalSeparator]"
SymbolToBinaryOperatorString[LongRightArrow] = "\\[LongRightArrow]"
SymbolToBinaryOperatorString[BinaryInvisibleApplication] = "\\[InvisibleApplication]"
SymbolToBinaryOperatorString[CirclePlus] = "\\[CirclePlus]"
SymbolToBinaryOperatorString[RightTriangle] = "\\[RightTriangle]"
SymbolToBinaryOperatorString[CircleTimes] = "\\[CircleTimes]"
SymbolToBinaryOperatorString[LeftTriangle] = "\\[LeftTriangle]"
SymbolToBinaryOperatorString[Backslash] = "\\[Backslash]"
SymbolToBinaryOperatorString[Cap] = "\\[Cap]"
SymbolToBinaryOperatorString[CircleMinus] = "\\[CircleMinus]"
SymbolToBinaryOperatorString[Coproduct] = "\\[Coproduct]"
SymbolToBinaryOperatorString[Cup] = "\\[Cup]"
SymbolToBinaryOperatorString[Diamond] = "\\[Diamond]"
SymbolToBinaryOperatorString[Star] = "\\[Star]"
SymbolToBinaryOperatorString[Vee] = "\\[Vee]"
SymbolToBinaryOperatorString[VerticalTilde] = "\\[VerticalTilde]"
SymbolToBinaryOperatorString[Wedge] = "\\[Wedge]"

(*
Infix
*)
InfixOperatorToSymbol[Token`Operator`Semi] = CompoundExpression

(* Plus and Times *)
InfixOperatorToSymbol[Token`Operator`Plus] = Plus
InfixOperatorToSymbol[Token`Operator`Minus] = Minus

InfixOperatorToSymbol[Token`Operator`LongName`ImplicitPlus] = InfixImplicitPlus

InfixOperatorToSymbol[Token`Operator`Star] = Times
InfixOperatorToSymbol[Token`Operator`Slash] = Divide

InfixOperatorToSymbol[Token`Operator`Fake`ImplicitTimes] = ImplicitTimes

InfixOperatorToSymbol[Token`Operator`LongName`Times] = InfixTimes
InfixOperatorToSymbol[Token`Operator`LongName`InvisibleTimes] = InfixInvisibleTimes

InfixOperatorToSymbol[Token`Operator`StarStar] = NonCommutativeMultiply
InfixOperatorToSymbol[Token`Operator`Dot] = Dot
InfixOperatorToSymbol[Token`Operator`AmpAmp] = And
InfixOperatorToSymbol[Token`Operator`Bar] = Alternatives
InfixOperatorToSymbol[Token`Operator`BarBar] = Or
InfixOperatorToSymbol[Token`Operator`LessGreater] = StringJoin
InfixOperatorToSymbol[Token`Operator`TildeTilde] = StringExpression

(* inequality operators *)
InfixOperatorToSymbol[Token`Operator`EqualEqual] = Equal
InfixOperatorToSymbol[Token`Operator`BangEqual] = Unequal
InfixOperatorToSymbol[Token`Operator`Less] = Less
InfixOperatorToSymbol[Token`Operator`Greater] = Greater
InfixOperatorToSymbol[Token`Operator`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`Operator`GreaterEqual] = GreaterEqual

(* other flattening operators *)
InfixOperatorToSymbol[Token`Operator`EqualEqualEqual] = SameQ
InfixOperatorToSymbol[Token`Operator`EqualBangEqual] = UnsameQ
InfixOperatorToSymbol[Token`Operator`AtStar] = Composition
InfixOperatorToSymbol[Token`Operator`SlashStar] = RightComposition

InfixOperatorToSymbol[Token`Operator`LongName`And] = And
InfixOperatorToSymbol[Token`Operator`LongName`Or] = Or
InfixOperatorToSymbol[Token`Operator`LongName`Xor] = Xor
InfixOperatorToSymbol[Token`Operator`LongName`Nand] = Nand
InfixOperatorToSymbol[Token`Operator`LongName`Nor] = Nor

(* extra space *)
SymbolToInfixOperatorString[CompoundExpression] = " ; "

(* Plus and Times *)
(* extra space, prevent 9.8` + 3 from turning into 9.8`+3 *)
SymbolToInfixOperatorString[Plus] = " + "
SymbolToInfixOperatorString[Minus] = " - "
SymbolToInfixOperatorString[InfixImplicitPlus] = "\\[ImplicitPlus]"

SymbolToInfixOperatorString[Times] = "*"
SymbolToInfixOperatorString[Divide] = "/"
SymbolToInfixOperatorString[ImplicitTimes] = " "
SymbolToInfixOperatorString[InfixInvisibleTimes] = "\\[InvisibleTimes]"
SymbolToInfixOperatorString[InfixTimes] = "\\[Times]"

SymbolToInfixOperatorString[NonCommutativeMultiply] = "**"
(* extra space *)
SymbolToInfixOperatorString[Dot] = " . "
SymbolToInfixOperatorString[Alternatives] = "|"
SymbolToInfixOperatorString[And] = "&&"
SymbolToInfixOperatorString[Or] = "||"
SymbolToInfixOperatorString[StringJoin] = "<>"
SymbolToInfixOperatorString[StringExpression] = "~~"

(* inequality operators *)
SymbolToInfixOperatorString[Equal] = "=="
SymbolToInfixOperatorString[Unequal] = "!="
SymbolToInfixOperatorString[Less] = "<"
SymbolToInfixOperatorString[Greater] = ">"
SymbolToInfixOperatorString[LessEqual] = "<="
SymbolToInfixOperatorString[GreaterEqual] = ">="

(* other flattening operators *)
SymbolToInfixOperatorString[SameQ] = "==="
SymbolToInfixOperatorString[UnsameQ] = "=!="
SymbolToInfixOperatorString[Composition] = "@*"
SymbolToInfixOperatorString[RightComposition] = "/*"

SymbolToInfixOperatorString[Nand] = "\\[Nand]"
SymbolToInfixOperatorString[Nor] = "\\[Nor]"
SymbolToInfixOperatorString[Xor] = "\\[Xor]"



(*
Ternary
*)
TernaryOperatorsToSymbol[Token`Operator`SlashColon, Token`Operator`Equal] = TagSet
TernaryOperatorsToSymbol[Token`Operator`SlashColon, Token`Operator`ColonEqual] = TagSetDelayed
TernaryOperatorsToSymbol[Token`Operator`SlashColon, Token`Operator`Fake`EqualDot] = TagUnset
TernaryOperatorsToSymbol[Token`Operator`Tilde, Token`Operator`Tilde] = TernaryTilde
TernaryOperatorsToSymbol[Token`Operator`SemiSemi, Token`Operator`SemiSemi] = Span
TernaryOperatorsToSymbol[Token`Operator`ColonColon, Token`Operator`ColonColon] = MessageName

SymbolToTernaryPair[TagSet] = {TernarySlashColon, Set}
SymbolToTernaryPair[TagSetDelayed] = {TernarySlashColon, SetDelayed}
SymbolToTernaryPair[TagUnset] = {TernarySlashColon, Unset}
SymbolToTernaryPair[TernaryTilde] = {TernaryTilde, TernaryTilde}
SymbolToTernaryPair[Span] = {Span, Span}
SymbolToTernaryPair[MessageName] = {MessageName, MessageName}

SymbolToTernaryOperatorString[MessageName] = "::"
SymbolToTernaryOperatorString[TernaryTilde] = "~"
SymbolToTernaryOperatorString[TernarySlashColon] = "/:"
SymbolToTernaryOperatorString[Set] = "="
SymbolToTernaryOperatorString[SetDelayed] = ":="
SymbolToTernaryOperatorString[Span] = ";;"
(* extra space, ending in . *)
SymbolToTernaryOperatorString[Unset] = "=. "






GroupOpenerToSymbol[Token`Operator`OpenCurly] = List
GroupOpenerToSymbol[Token`Operator`LessBar] = Association
GroupOpenerToSymbol[Token`Operator`OpenSquare] = GroupSquare
GroupOpenerToSymbol[Token`Operator`OpenParen] = GroupParen

GroupOpenerToSymbol[Token`Operator`LongName`LeftAngleBracket] = AngleBracket
GroupOpenerToSymbol[Token`Operator`LongName`LeftCeiling] = Ceiling
GroupOpenerToSymbol[Token`Operator`LongName`LeftFloor] = Floor
GroupOpenerToSymbol[Token`Operator`LongName`LeftDoubleBracket] = GroupDoubleBracket
GroupOpenerToSymbol[Token`Operator`LongName`LeftBracketingBar] = BracketingBar
GroupOpenerToSymbol[Token`Operator`LongName`LeftDoubleBracketingBar] = DoubleBracketingBar
GroupOpenerToSymbol[Token`Operator`LongName`LeftAssociation] = Association

GroupOpenerToSymbol[Token`Operator`LinearSyntax`OpenParen] = GroupLinearSyntaxParen


GroupCloserToSymbol[Token`Operator`CloseCurly] = List
GroupCloserToSymbol[Token`Operator`BarGreater] = Association
GroupCloserToSymbol[Token`Operator`CloseSquare] = GroupSquare
GroupCloserToSymbol[Token`Operator`CloseParen] = GroupParen

GroupCloserToSymbol[Token`Operator`LongName`RightAngleBracket] = AngleBracket
GroupCloserToSymbol[Token`Operator`LongName`RightCeiling] = Ceiling
GroupCloserToSymbol[Token`Operator`LongName`RightFloor] = Floor
GroupCloserToSymbol[Token`Operator`LongName`RightDoubleBracket] = GroupDoubleBracket
GroupCloserToSymbol[Token`Operator`LongName`RightBracketingBar] = BracketingBar
GroupCloserToSymbol[Token`Operator`LongName`RightDoubleBracketingBar] = DoubleBracketingBar
GroupCloserToSymbol[Token`Operator`LongName`RightAssociation] = Association

GroupCloserToSymbol[Token`Operator`LinearSyntax`CloseParen] = GroupLinearSyntaxParen


SymbolToGroupPair[List] = {"{", "}"}
SymbolToGroupPair[GroupMissingOpenerList] = {"", "}"}
SymbolToGroupPair[GroupMissingCloserList] = {"{", ""}

SymbolToGroupPair[Association] = {"<|", "|>"}
SymbolToGroupPair[GroupMissingOpenerAssociation] = {"", "|>"}
SymbolToGroupPair[GroupMissingCloserAssociation] = {"<|", ""}

SymbolToGroupPair[AngleBracket] = {"\\[LeftAngleBracket]", "\\[RightAngleBracket]"}
SymbolToGroupPair[GroupMissingOpenerAngleBracket] = {"", "\\[RightAngleBracket]"}
SymbolToGroupPair[GroupMissingCloserAngleBracket] = {"\\[LeftAngleBracket]", ""}

SymbolToGroupPair[Ceiling] = {"\\[LeftCeiling]", "\\[RightCeiling]"}
SymbolToGroupPair[GroupMissingOpenerCeiling] = {"", "\\[RightCeiling]"}
SymbolToGroupPair[GroupMissingCloserCeiling] = {"\\[LeftCeiling]", ""}

SymbolToGroupPair[Floor] = {"\\[LeftFloor]", "\\[RightFloor]"}
SymbolToGroupPair[GroupMissingOpenerFloor] = {"", "\\[RightFloor]"}
SymbolToGroupPair[GroupMissingCloserFloor] = {"\\[LeftFloor]", ""}

SymbolToGroupPair[GroupDoubleBracket] = {"[[", "]]"}
SymbolToGroupPair[GroupMissingOpenerDoubleBracket] = {"", "]]"}
SymbolToGroupPair[GroupMissingCloserDoubleBracket] = {"[[", ""}

SymbolToGroupPair[GroupSquare] = {"[", "]"}
SymbolToGroupPair[GroupMissingOpenerSquare] = {"", "]"}
SymbolToGroupPair[GroupMissingCloserSquare] = {"[", ""}

SymbolToGroupPair[BracketingBar] = {"\\[LeftBracketingBar]", "\\[RightBracketingBar]"}
SymbolToGroupPair[GroupMissingOpenerBracketingBar] = {"", "\\[RightBracketingBar]"}
SymbolToGroupPair[GroupMissingCloserBracketingBar] = {"\\[LeftBracketingBar]", ""}

SymbolToGroupPair[DoubleBracketingBar] = {"\\[LeftDoubleBracketingBar]", "\\[RightDoubleBracketingBar]"}
SymbolToGroupPair[GroupMissingOpenerDoubleBracketingBar] = {"", "\\[RightDoubleBracketingBar]"}
SymbolToGroupPair[GroupMissingCloserDoubleBracketingBar] = {"\\[LeftDoubleBracketingBar]", ""}

SymbolToGroupPair[GroupParen] = {"(", ")"}
SymbolToGroupPair[GroupMissingOpenerParen] = {"", ")"}
SymbolToGroupPair[GroupMissingCloserParen] = {"(", ""}

SymbolToGroupPair[GroupLinearSyntaxParen] = {"\\(", "\\)"}
SymbolToGroupPair[GroupMissingOpenerLinearSyntaxParen] = {"", "\\)"}
SymbolToGroupPair[GroupMissingCloserLinearSyntaxParen] = {"\\(", ""}



GroupOpenerToCloser[Token`Operator`OpenCurly] = Token`Operator`CloseCurly
GroupOpenerToCloser[Token`Operator`LessBar] = Token`Operator`BarGreater
GroupOpenerToCloser[Token`Operator`OpenSquare] = Token`Operator`CloseSquare
GroupOpenerToCloser[Token`Operator`OpenParen] = Token`Operator`CloseParen

GroupOpenerToCloser[Token`Operator`LongName`LeftAngleBracket] = Token`Operator`LongName`RightAngleBracket
GroupOpenerToCloser[Token`Operator`LongName`LeftCeiling] = Token`Operator`LongName`RightCeiling
GroupOpenerToCloser[Token`Operator`LongName`LeftFloor] = Token`Operator`LongName`RightFloor
GroupOpenerToCloser[Token`Operator`LongName`LeftDoubleBracket] = Token`Operator`LongName`RightDoubleBracket
GroupOpenerToCloser[Token`Operator`LongName`LeftBracketingBar] = Token`Operator`LongName`RightBracketingBar
GroupOpenerToCloser[Token`Operator`LongName`LeftDoubleBracketingBar] = Token`Operator`LongName`RightDoubleBracketingBar
GroupOpenerToCloser[Token`Operator`LongName`LeftAssociation] = Token`Operator`LongName`RightAssociation

GroupOpenerToCloser[Token`Operator`LinearSyntax`OpenParen] = Token`Operator`LinearSyntax`CloseParen




GroupOpenerToMissingCloserSymbol[Token`Operator`OpenCurly] = GroupMissingCloserList
GroupOpenerToMissingCloserSymbol[Token`Operator`LessBar] = GroupMissingCloserAssociation
GroupOpenerToMissingCloserSymbol[Token`Operator`OpenSquare] = GroupMissingCloserSquare
GroupOpenerToMissingCloserSymbol[Token`Operator`OpenParen] = GroupMissingCloserParen

GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftAngleBracket] = GroupMissingCloserAngleBracket
GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftCeiling] = GroupMissingCloserCeiling
GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftFloor] = GroupMissingCloserFloor
GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftDoubleBracket] = GroupMissingCloserDoubleBracket
GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftBracketingBar] = GroupMissingCloserBracketingBar
GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftDoubleBracketingBar] = GroupMissingCloserDoubleBracketingBar
GroupOpenerToMissingCloserSymbol[Token`Operator`LongName`LeftAssociation] = GroupMissingCloserAssociation

GroupOpenerToMissingCloserSymbol[Token`Operator`LinearSyntax`OpenParen] = GroupMissingCloserLinearSyntaxParen

GroupCloserToMissingOpenerSymbol[Token`Operator`CloseCurly] = GroupMissingOpenerList
GroupCloserToMissingOpenerSymbol[Token`Operator`BarGreater] = GroupMissingOpenerAssociation
GroupCloserToMissingOpenerSymbol[Token`Operator`CloseSquare] = GroupMissingOpenerSquare
GroupCloserToMissingOpenerSymbol[Token`Operator`CloseParen] = GroupMissingOpenerParen

GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightAngleBracket] = GroupMissingOpenerAngleBracket
GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightCeiling] = GroupMissingOpenerCeiling
GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightFloor] = GroupMissingOpenerFloor
GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightDoubleBracket] = GroupMissingOpenerDoubleBracket
GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightBracketingBar] = GroupMissingOpenerBracketingBar
GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightDoubleBracketingBar] = GroupMissingOpenerDoubleBracketingBar
GroupCloserToMissingOpenerSymbol[Token`Operator`LongName`RightAssociation] = GroupMissingOpenerAssociation

GroupCloserToMissingOpenerSymbol[Token`Operator`LinearSyntax`CloseParen] = GroupMissingOpenerLinearSyntaxParen






End[]

EndPackage[]
