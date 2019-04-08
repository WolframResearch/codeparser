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
IntegerNode,
RealNode,

SlotNode,
SlotSequenceNode,
OutNode,

PrefixNode,
BinaryNode,
TernaryNode,
InfixNode,
PostfixNode,
GroupNode,
CallNode,

BlankNode,
BlankSequenceNode,
BlankNullSequenceNode,
OptionalDefaultNode,
PatternBlankNode,
PatternBlankSequenceNode,
PatternBlankNullSequenceNode,
OptionalDefaultPatternNode,

TokenNode,
InternalAllNode,
InternalDotNode,
InternalNullNode,
InternalOneNode,

FileNode,

CommentNode,

SyntaxErrorNode,
AbstractSyntaxErrorNode,

PackageNode,
ContextNode,
StaticAnalysisIgnoreNode

}

$Options = {
Source,
SyntaxIssues,
Comments
}


$Miscellaneous = {
All,
(* when parsing f[1,] then we need to parse as f[1,Null] *)
Null,
True,
False,
SyntaxIssue,
Comment,

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







PrefixOperatorToSymbol[Token`Bang] = Not
PrefixOperatorToSymbol[Token`PlusPlus] = PreIncrement
PrefixOperatorToSymbol[Token`LessLess] = Get
PrefixOperatorToSymbol[Token`MinusMinus] = PreDecrement
PrefixOperatorToSymbol[Token`Plus] = Plus
PrefixOperatorToSymbol[Token`Minus] = Minus

PrefixOperatorToSymbol[Token`LongName`Sqrt] = Sqrt
PrefixOperatorToSymbol[Token`LongName`Not] = Not
PrefixOperatorToSymbol[Token`LongName`PlusMinus] = PlusMinus
PrefixOperatorToSymbol[Token`LongName`Sum] = Sum
PrefixOperatorToSymbol[Token`LongName`MinusPlus] = MinusPlus
PrefixOperatorToSymbol[Token`LongName`DifferentialD] = DifferentialD
PrefixOperatorToSymbol[Token`LongName`Minus] = Minus
PrefixOperatorToSymbol[Token`LongName`Del] = Del
PrefixOperatorToSymbol[Token`LongName`Square] = Square
PrefixOperatorToSymbol[Token`LongName`Integral] = Integral
PrefixOperatorToSymbol[Token`LongName`ContourIntegral] = ContourIntegral
PrefixOperatorToSymbol[Token`LongName`DoubleContourIntegral] = DoubleContourIntegral
PrefixOperatorToSymbol[Token`LongName`ClockwiseContourIntegral] = ClockwiseContourIntegral
PrefixOperatorToSymbol[Token`LongName`CounterClockwiseContourIntegral] = CounterClockwiseContourIntegral
PrefixOperatorToSymbol[Token`LongName`Product] = Product
PrefixOperatorToSymbol[Token`LongName`InvisiblePrefixScriptBase] = PrefixInvisiblePrefixScriptBase
PrefixOperatorToSymbol[Token`LongName`CircleTimes] = CircleTimes

PrefixOperatorToSymbol[Token`LinearSyntax`Bang] = PrefixLinearSyntaxBang

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
SymbolToPrefixOperatorString[CircleTimes] = "\\[CircleTimes]"










PostfixOperatorToSymbol[Token`DotDot] = Repeated
PostfixOperatorToSymbol[Token`Bang] = Factorial
PostfixOperatorToSymbol[Token`MinusMinus] = Decrement
PostfixOperatorToSymbol[Token`PlusPlus] = Increment
PostfixOperatorToSymbol[Token`DotDotDot] = RepeatedNull
PostfixOperatorToSymbol[Token`Amp] = Function
PostfixOperatorToSymbol[Token`BangBang] = Factorial2
PostfixOperatorToSymbol[Token`SingleQuote] = Derivative
   
PostfixOperatorToSymbol[Token`LongName`Transpose] = Transpose
PostfixOperatorToSymbol[Token`LongName`Conjugate] = Conjugate
PostfixOperatorToSymbol[Token`LongName`ConjugateTranspose] = ConjugateTranspose
PostfixOperatorToSymbol[Token`LongName`HermitianConjugate] = PostfixHermitianConjugate
PostfixOperatorToSymbol[Token`LongName`InvisiblePostfixScriptBase] = PostfixInvisiblePostfixScriptBase

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
SymbolToPostfixOperatorString[Derivative] = "'"

SymbolToPostfixOperatorString[Transpose] = "\\[Transpose]"
SymbolToPostfixOperatorString[Conjugate] = "\\[Conjugate]"
SymbolToPostfixOperatorString[ConjugateTranspose] = "\\[ConjugateTranspose]"
SymbolToPostfixOperatorString[PostfixHermitianConjugate] = "\\[HermitianConjugate]"
SymbolToPostfixOperatorString[PostfixInvisiblePostfixScriptBase] = "\\[InvisiblePostfixScriptBase]"








(*
Binary
*)
(* inequality operators *)
BinaryOperatorToSymbol[Token`EqualEqual] = Equal
BinaryOperatorToSymbol[Token`BangEqual] = Unequal
BinaryOperatorToSymbol[Token`Less] = Less
BinaryOperatorToSymbol[Token`Greater] = Greater
BinaryOperatorToSymbol[Token`LessEqual] = LessEqual
BinaryOperatorToSymbol[Token`GreaterEqual] = GreaterEqual

(* other flattening operators *)
BinaryOperatorToSymbol[Token`EqualEqualEqual] = SameQ
BinaryOperatorToSymbol[Token`EqualBangEqual] = UnsameQ
BinaryOperatorToSymbol[Token`AtStar] = Composition
BinaryOperatorToSymbol[Token`SlashStar] = RightComposition

BinaryOperatorToSymbol[Token`Minus] = Minus
BinaryOperatorToSymbol[Token`Slash] = Divide
BinaryOperatorToSymbol[Token`SlashAt] = Map
BinaryOperatorToSymbol[Token`Equal] = Set
BinaryOperatorToSymbol[Token`Caret] = Power
BinaryOperatorToSymbol[Token`ColonEqual] = SetDelayed
BinaryOperatorToSymbol[Token`MinusGreater] = Rule
BinaryOperatorToSymbol[Token`Question] = PatternTest
BinaryOperatorToSymbol[Token`AtAt] = Apply
BinaryOperatorToSymbol[Token`SlashSemi] = Condition
BinaryOperatorToSymbol[Token`SlashDot] = ReplaceAll
BinaryOperatorToSymbol[Token`ColonGreater] = RuleDelayed
BinaryOperatorToSymbol[Token`SlashSlashDot] = ReplaceRepeated
BinaryOperatorToSymbol[Token`PlusEqual] = AddTo
BinaryOperatorToSymbol[Token`StarEqual] = TimesBy
BinaryOperatorToSymbol[Token`MinusEqual] = SubtractFrom
BinaryOperatorToSymbol[Token`SlashEqual] = DivideBy
BinaryOperatorToSymbol[Token`CaretEqual] = UpSet
BinaryOperatorToSymbol[Token`CaretColonEqual] = UpSetDelayed
(*
TwoWayRule was added in 11.1, but we support building with 11.0
force symbol to be in System`
*)
BinaryOperatorToSymbol[Token`LessMinusGreater] = System`TwoWayRule
BinaryOperatorToSymbol[Token`SlashSlashAt] = MapAll
BinaryOperatorToSymbol[Token`GreaterGreater] = Put
BinaryOperatorToSymbol[Token`GreaterGreaterGreater] = PutAppend
BinaryOperatorToSymbol[Token`SlashSlash] = BinarySlashSlash
BinaryOperatorToSymbol[Token`SemiSemi] = Span
BinaryOperatorToSymbol[Token`At] = BinaryAt
BinaryOperatorToSymbol[Token`AtAtAt] = BinaryAtAtAt

BinaryOperatorToSymbol[Token`Fake`EqualDot] = Unset
BinaryOperatorToSymbol[Token`Fake`PatternColon] = Pattern
BinaryOperatorToSymbol[Token`Fake`OptionalColon] = Optional

(* inequality operators *)
BinaryOperatorToSymbol[Token`LongName`Equal] = Equal
BinaryOperatorToSymbol[Token`LongName`LessEqual] = LessEqual
BinaryOperatorToSymbol[Token`LongName`GreaterEqual] = GreaterEqual
BinaryOperatorToSymbol[Token`LongName`NotEqual] = Unequal

(* set relation operators *)
BinaryOperatorToSymbol[Token`LongName`Element] = Element
BinaryOperatorToSymbol[Token`LongName`Subset] = Subset
BinaryOperatorToSymbol[Token`LongName`Superset] = Superset
BinaryOperatorToSymbol[Token`LongName`SubsetEqual] = SubsetEqual
BinaryOperatorToSymbol[Token`LongName`SupersetEqual] = SupersetEqual
BinaryOperatorToSymbol[Token`LongName`NotElement] = NotElement
BinaryOperatorToSymbol[Token`LongName`NotSubset] = NotSubset
BinaryOperatorToSymbol[Token`LongName`NotSuperset] = NotSuperset
BinaryOperatorToSymbol[Token`LongName`NotSubsetEqual] = NotSubsetEqual
BinaryOperatorToSymbol[Token`LongName`NotSupersetEqual] = NotSupersetEqual

BinaryOperatorToSymbol[Token`LongName`Implies] = Implies
BinaryOperatorToSymbol[Token`LongName`PlusMinus] = PlusMinus
BinaryOperatorToSymbol[Token`LongName`DirectedEdge] = DirectedEdge
BinaryOperatorToSymbol[Token`LongName`Rule] = Rule
BinaryOperatorToSymbol[Token`LongName`RuleDelayed] = RuleDelayed
BinaryOperatorToSymbol[Token`LongName`UndirectedEdge] = UndirectedEdge
BinaryOperatorToSymbol[Token`LongName`Function] = Function
BinaryOperatorToSymbol[Token`LongName`MinusPlus] = MinusPlus
(*
force TwoWayRule to be in System`
*)
BinaryOperatorToSymbol[Token`LongName`TwoWayRule] = System`TwoWayRule
BinaryOperatorToSymbol[Token`LongName`InvisibleApplication] = BinaryInvisibleApplication
BinaryOperatorToSymbol[Token`LongName`CircleMinus] = CircleMinus
(*
force to be in System`:
VectorGreater
VectorGreaterEqual
VectorLess
VectorLessEqual
*)
BinaryOperatorToSymbol[Token`LongName`VectorGreater] = System`VectorGreater
BinaryOperatorToSymbol[Token`LongName`VectorGreaterEqual] = System`VectorGreaterEqual
BinaryOperatorToSymbol[Token`LongName`VectorLess] = System`VectorLess
BinaryOperatorToSymbol[Token`LongName`VectorLessEqual] = System`VectorLessEqual

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

SymbolToBinaryOperatorString[Minus] = " - "
SymbolToBinaryOperatorString[Divide] = "/"
SymbolToBinaryOperatorString[Map] = "/@"
SymbolToBinaryOperatorString[Set] = "="
SymbolToBinaryOperatorString[SetDelayed] = ":="
SymbolToBinaryOperatorString[Rule] = "->"
SymbolToBinaryOperatorString[PatternTest] = "?"
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
SymbolToBinaryOperatorString[PutAppend] = ">>>"
(* extra space, ending in . *)
SymbolToBinaryOperatorString[Unset] = "=. "

(* set relation operators *)
SymbolToBinaryOperatorString[Element] = "\\[Element]"
SymbolToBinaryOperatorString[Subset] = "\\[Subset]"
SymbolToBinaryOperatorString[Superset] = "\\[Superset]"
SymbolToBinaryOperatorString[SubsetEqual] = "\\[SubsetEqual]"
SymbolToBinaryOperatorString[SupersetEqual] = "\\[SupersetEqual]"
SymbolToBinaryOperatorString[NotElement] = "\\[NotElement]"
SymbolToBinaryOperatorString[NotSubset] = "\\[NotSubset]"
SymbolToBinaryOperatorString[NotSuperset] = "\\[NotSuperset]"
SymbolToBinaryOperatorString[NotSubsetEqual] = "\\[NotSubsetEqual]"
SymbolToBinaryOperatorString[NotSupersetEqual] = "\\[NotSupersetEqual]"

SymbolToBinaryOperatorString[PlusMinus] = "\\[PlusMinus]"
SymbolToBinaryOperatorString[Implies] = "\\[Implies]"
SymbolToBinaryOperatorString[DirectedEdge] = "\\[DirectedEdge]"
(*
force to be in System`
*)
SymbolToBinaryOperatorString[System`TwoWayRule] = "\\[TwoWayRule]"
SymbolToBinaryOperatorString[UndirectedEdge] = "\\[UndirectedEdge]"
SymbolToBinaryOperatorString[Function] = "\\[Function]"
SymbolToBinaryOperatorString[BinaryInvisibleApplication] = "\\[InvisibleApplication]"
SymbolToBinaryOperatorString[CircleMinus] = "\\[CircleMinus]"
SymbolToBinaryOperatorString[MinusPlus] = "\\[MinusPlus]"
(*
force to be in System`
*)
SymbolToBinaryOperatorString[System`VectorGreater] = "\\[VectorGreater]"
SymbolToBinaryOperatorString[System`VectorGreaterEqual] = "\\[VectorGreaterEqual]"
SymbolToBinaryOperatorString[System`VectorLess] = "\\[VectorLess]"
SymbolToBinaryOperatorString[System`VectorLessEqual] = "\\[VectorLessEqual]"

(*
Infix
*)
InfixOperatorToSymbol[Token`Semi] = CompoundExpression

(* Plus and Times *)
InfixOperatorToSymbol[Token`Plus] = Plus

InfixOperatorToSymbol[Token`LongName`ImplicitPlus] = InfixImplicitPlus

InfixOperatorToSymbol[Token`Star] = Times

InfixOperatorToSymbol[Token`Fake`ImplicitTimes] = ImplicitTimes

InfixOperatorToSymbol[Token`LongName`Times] = InfixTimes
InfixOperatorToSymbol[Token`LongName`InvisibleTimes] = InfixInvisibleTimes

InfixOperatorToSymbol[Token`StarStar] = NonCommutativeMultiply
InfixOperatorToSymbol[Token`Dot] = Dot
InfixOperatorToSymbol[Token`AmpAmp] = And
InfixOperatorToSymbol[Token`Bar] = Alternatives
InfixOperatorToSymbol[Token`BarBar] = Or
InfixOperatorToSymbol[Token`LessGreater] = StringJoin
InfixOperatorToSymbol[Token`TildeTilde] = StringExpression
InfixOperatorToSymbol[Token`ColonColon] = MessageName

InfixOperatorToSymbol[Token`LongName`And] = And
InfixOperatorToSymbol[Token`LongName`Or] = Or
InfixOperatorToSymbol[Token`LongName`Xor] = Xor
InfixOperatorToSymbol[Token`LongName`Nand] = Nand
InfixOperatorToSymbol[Token`LongName`Nor] = Nor

InfixOperatorToSymbol[Token`LongName`CenterDot] = CenterDot
InfixOperatorToSymbol[Token`LongName`RightTeeArrow] = RightTeeArrow
InfixOperatorToSymbol[Token`LongName`TildeTilde] = TildeTilde
InfixOperatorToSymbol[Token`LongName`NotTildeTilde] = NotTildeTilde
InfixOperatorToSymbol[Token`LongName`Equivalent] = Equivalent
InfixOperatorToSymbol[Token`LongName`LeftTriangleEqual] = LeftTriangleEqual
InfixOperatorToSymbol[Token`LongName`TildeEqual] = TildeEqual
InfixOperatorToSymbol[Token`LongName`TildeFullEqual] = TildeFullEqual
InfixOperatorToSymbol[Token`LongName`NotTildeFullEqual] = NotTildeFullEqual
InfixOperatorToSymbol[Token`LongName`CircleDot] = CircleDot
InfixOperatorToSymbol[Token`LongName`Distributed] = Distributed
InfixOperatorToSymbol[Token`LongName`Conditioned] = Conditioned
InfixOperatorToSymbol[Token`LongName`Union] = Union
InfixOperatorToSymbol[Token`LongName`Intersection] = Intersection
InfixOperatorToSymbol[Token`LongName`TensorWedge] = TensorWedge
InfixOperatorToSymbol[Token`LongName`Cross] = Cross
InfixOperatorToSymbol[Token`LongName`GreaterTilde] = GreaterTilde
InfixOperatorToSymbol[Token`LongName`Proportional] = Proportional
InfixOperatorToSymbol[Token`LongName`LessLess] = LessLess
InfixOperatorToSymbol[Token`LongName`Congruent] = Congruent
InfixOperatorToSymbol[Token`LongName`Tilde] = Tilde
InfixOperatorToSymbol[Token`LongName`DoubleLongLeftRightArrow] = DoubleLongLeftRightArrow
InfixOperatorToSymbol[Token`LongName`RightArrow] = RightArrow
InfixOperatorToSymbol[Token`LongName`SmallCircle] = SmallCircle
InfixOperatorToSymbol[Token`LongName`DoubleLongRightArrow] = DoubleLongRightArrow
InfixOperatorToSymbol[Token`LongName`Divides] = Divisible
InfixOperatorToSymbol[Token`LongName`LeftRightArrow] = LeftRightArrow
InfixOperatorToSymbol[Token`LongName`VerticalSeparator] = VerticalSeparator
InfixOperatorToSymbol[Token`LongName`LongRightArrow] = LongRightArrow
InfixOperatorToSymbol[Token`LongName`Backslash] = Backslash
InfixOperatorToSymbol[Token`LongName`Diamond] = Diamond
InfixOperatorToSymbol[Token`LongName`Wedge] = Wedge
InfixOperatorToSymbol[Token`LongName`Vee] = Vee
InfixOperatorToSymbol[Token`LongName`CircleTimes] = CircleTimes
InfixOperatorToSymbol[Token`LongName`Star] = Star
InfixOperatorToSymbol[Token`LongName`VerticalTilde] = VerticalTilde
InfixOperatorToSymbol[Token`LongName`Coproduct] = Coproduct
InfixOperatorToSymbol[Token`LongName`Cap] = Cap
InfixOperatorToSymbol[Token`LongName`Cup] = Cup
InfixOperatorToSymbol[Token`LongName`CirclePlus] = CirclePlus
InfixOperatorToSymbol[Token`LongName`RightTriangle] = RightTriangle
InfixOperatorToSymbol[Token`LongName`LeftTriangle] = LeftTriangle

(* extra space *)
SymbolToInfixOperatorString[CompoundExpression] = " ; "

(* Plus and Times *)
(* extra space, prevent 9.8` + 3 from turning into 9.8`+3 *)
SymbolToInfixOperatorString[Plus] = " + "
SymbolToInfixOperatorString[InfixImplicitPlus] = "\\[ImplicitPlus]"

SymbolToInfixOperatorString[Times] = "*"
SymbolToInfixOperatorString[ImplicitTimes] = " "
SymbolToInfixOperatorString[InfixInvisibleTimes] = "\\[InvisibleTimes]"
SymbolToInfixOperatorString[InfixTimes] = "\\[Times]"

SymbolToInfixOperatorString[NonCommutativeMultiply] = "**"
(* extra space *)
SymbolToInfixOperatorString[Dot] = " . "
SymbolToInfixOperatorString[Alternatives] = "|"
SymbolToInfixOperatorString[StringJoin] = "<>"
SymbolToInfixOperatorString[StringExpression] = "~~"
SymbolToInfixOperatorString[MessageName] = "::"

SymbolToInfixOperatorString[And] = "&&"
SymbolToInfixOperatorString[Or] = "||"
SymbolToInfixOperatorString[Nand] = "\\[Nand]"
SymbolToInfixOperatorString[Nor] = "\\[Nor]"
SymbolToInfixOperatorString[Xor] = "\\[Xor]"

SymbolToInfixOperatorString[CenterDot] = "\\[CenterDot]"
SymbolToInfixOperatorString[RightTeeArrow] = "\\[RightTeeArrow]"
SymbolToInfixOperatorString[LeftTriangleEqual] = "\\[LeftTriangleEqual]"
SymbolToInfixOperatorString[TildeFullEqual] = "\\[TildeFullEqual]"
SymbolToInfixOperatorString[NotTildeFullEqual] = "\\[NotTildeFullEqual]"
SymbolToInfixOperatorString[TildeTilde] = "\\[TildeTilde]"
SymbolToInfixOperatorString[NotTildeTilde] = "\\[NotTildeTilde]"
SymbolToInfixOperatorString[TildeEqual] = "\\[TildeEqual]"
SymbolToInfixOperatorString[Equivalent] = "\\[Equivalent]"
SymbolToInfixOperatorString[Intersection] = "\\[Intersection]"
SymbolToInfixOperatorString[Union] = "\\[Union]"
SymbolToInfixOperatorString[Distributed] = "\\[Distributed]"
SymbolToInfixOperatorString[Conditioned] = "\\[Conditioned]"
SymbolToInfixOperatorString[CircleDot] = "\\[CircleDot]"
SymbolToInfixOperatorString[TensorWedge] = "\\[TensorWedge]"
SymbolToInfixOperatorString[Cross] = "\\[Cross]"
SymbolToInfixOperatorString[GreaterTilde] = "\\[GreaterTilde]"
SymbolToInfixOperatorString[Proportional] = "\\[Proportional]"
SymbolToInfixOperatorString[LessLess] = "\\[LessLess]"
SymbolToInfixOperatorString[Congruent] = "\\[Congruent]"
SymbolToInfixOperatorString[Tilde] = "\\[Tilde]"
SymbolToInfixOperatorString[DoubleLongLeftRightArrow] = "\\[DoubleLongLeftRightArrow]"
SymbolToInfixOperatorString[RightArrow] = "\\[RightArrow]"
SymbolToInfixOperatorString[SmallCircle] = "\\[SmallCircle]"
SymbolToInfixOperatorString[DoubleLongRightArrow] = "\\[DoubleLongRightArrow]"
SymbolToInfixOperatorString[Divisible] = "\\[Divides]"
SymbolToInfixOperatorString[LeftRightArrow] = "\\[LeftRightArrow]"
SymbolToInfixOperatorString[VerticalSeparator] = "\\[VerticalSeparator]"
SymbolToInfixOperatorString[LongRightArrow] = "\\[LongRightArrow]"
SymbolToInfixOperatorString[CirclePlus] = "\\[CirclePlus]"
SymbolToInfixOperatorString[RightTriangle] = "\\[RightTriangle]"
SymbolToInfixOperatorString[CircleTimes] = "\\[CircleTimes]"
SymbolToInfixOperatorString[LeftTriangle] = "\\[LeftTriangle]"
SymbolToInfixOperatorString[Backslash] = "\\[Backslash]"
SymbolToInfixOperatorString[Cap] = "\\[Cap]"
SymbolToInfixOperatorString[Coproduct] = "\\[Coproduct]"
SymbolToInfixOperatorString[Cup] = "\\[Cup]"
SymbolToInfixOperatorString[Diamond] = "\\[Diamond]"
SymbolToInfixOperatorString[Star] = "\\[Star]"
SymbolToInfixOperatorString[Vee] = "\\[Vee]"
SymbolToInfixOperatorString[VerticalTilde] = "\\[VerticalTilde]"
SymbolToInfixOperatorString[Wedge] = "\\[Wedge]"

(*
Ternary
*)
TernaryOperatorsToSymbol[Token`SlashColon, Token`Equal] = TagSet
TernaryOperatorsToSymbol[Token`SlashColon, Token`ColonEqual] = TagSetDelayed
TernaryOperatorsToSymbol[Token`SlashColon, Token`Fake`EqualDot] = TagUnset
TernaryOperatorsToSymbol[Token`Tilde, Token`Tilde] = TernaryTilde
TernaryOperatorsToSymbol[Token`SemiSemi, Token`SemiSemi] = Span

SymbolToTernaryPair[TagSet] = {TernarySlashColon, Set}
SymbolToTernaryPair[TagSetDelayed] = {TernarySlashColon, SetDelayed}
SymbolToTernaryPair[TagUnset] = {TernarySlashColon, Unset}
SymbolToTernaryPair[TernaryTilde] = {TernaryTilde, TernaryTilde}
SymbolToTernaryPair[Span] = {Span, Span}

SymbolToTernaryOperatorString[TernaryTilde] = "~"
SymbolToTernaryOperatorString[TernarySlashColon] = "/:"
SymbolToTernaryOperatorString[Set] = "="
SymbolToTernaryOperatorString[SetDelayed] = ":="
SymbolToTernaryOperatorString[Span] = ";;"
(* extra space, ending in . *)
SymbolToTernaryOperatorString[Unset] = "=. "






GroupOpenerToSymbol[Token`OpenCurly] = List
GroupOpenerToSymbol[Token`LessBar] = Association
GroupOpenerToSymbol[Token`OpenSquare] = GroupSquare
GroupOpenerToSymbol[Token`OpenParen] = GroupParen

GroupOpenerToSymbol[Token`LongName`LeftAngleBracket] = AngleBracket
GroupOpenerToSymbol[Token`LongName`LeftCeiling] = Ceiling
GroupOpenerToSymbol[Token`LongName`LeftFloor] = Floor
GroupOpenerToSymbol[Token`LongName`LeftDoubleBracket] = GroupDoubleBracket
GroupOpenerToSymbol[Token`LongName`LeftBracketingBar] = BracketingBar
GroupOpenerToSymbol[Token`LongName`LeftDoubleBracketingBar] = DoubleBracketingBar
GroupOpenerToSymbol[Token`LongName`LeftAssociation] = Association

GroupOpenerToSymbol[Token`LinearSyntax`OpenParen] = GroupLinearSyntaxParen


GroupCloserToSymbol[Token`CloseCurly] = List
GroupCloserToSymbol[Token`BarGreater] = Association
GroupCloserToSymbol[Token`CloseSquare] = GroupSquare
GroupCloserToSymbol[Token`CloseParen] = GroupParen

GroupCloserToSymbol[Token`LongName`RightAngleBracket] = AngleBracket
GroupCloserToSymbol[Token`LongName`RightCeiling] = Ceiling
GroupCloserToSymbol[Token`LongName`RightFloor] = Floor
GroupCloserToSymbol[Token`LongName`RightDoubleBracket] = GroupDoubleBracket
GroupCloserToSymbol[Token`LongName`RightBracketingBar] = BracketingBar
GroupCloserToSymbol[Token`LongName`RightDoubleBracketingBar] = DoubleBracketingBar
GroupCloserToSymbol[Token`LongName`RightAssociation] = Association

GroupCloserToSymbol[Token`LinearSyntax`CloseParen] = GroupLinearSyntaxParen


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



GroupOpenerToCloser[Token`OpenCurly] = Token`CloseCurly
GroupOpenerToCloser[Token`LessBar] = Token`BarGreater
GroupOpenerToCloser[Token`OpenSquare] = Token`CloseSquare
GroupOpenerToCloser[Token`OpenParen] = Token`CloseParen

GroupOpenerToCloser[Token`LongName`LeftAngleBracket] = Token`LongName`RightAngleBracket
GroupOpenerToCloser[Token`LongName`LeftCeiling] = Token`LongName`RightCeiling
GroupOpenerToCloser[Token`LongName`LeftFloor] = Token`LongName`RightFloor
GroupOpenerToCloser[Token`LongName`LeftDoubleBracket] = Token`LongName`RightDoubleBracket
GroupOpenerToCloser[Token`LongName`LeftBracketingBar] = Token`LongName`RightBracketingBar
GroupOpenerToCloser[Token`LongName`LeftDoubleBracketingBar] = Token`LongName`RightDoubleBracketingBar
GroupOpenerToCloser[Token`LongName`LeftAssociation] = Token`LongName`RightAssociation

GroupOpenerToCloser[Token`LinearSyntax`OpenParen] = Token`LinearSyntax`CloseParen




GroupOpenerToMissingCloserSymbol[Token`OpenCurly] = GroupMissingCloserList
GroupOpenerToMissingCloserSymbol[Token`LessBar] = GroupMissingCloserAssociation
GroupOpenerToMissingCloserSymbol[Token`OpenSquare] = GroupMissingCloserSquare
GroupOpenerToMissingCloserSymbol[Token`OpenParen] = GroupMissingCloserParen

GroupOpenerToMissingCloserSymbol[Token`LongName`LeftAngleBracket] = GroupMissingCloserAngleBracket
GroupOpenerToMissingCloserSymbol[Token`LongName`LeftCeiling] = GroupMissingCloserCeiling
GroupOpenerToMissingCloserSymbol[Token`LongName`LeftFloor] = GroupMissingCloserFloor
GroupOpenerToMissingCloserSymbol[Token`LongName`LeftDoubleBracket] = GroupMissingCloserDoubleBracket
GroupOpenerToMissingCloserSymbol[Token`LongName`LeftBracketingBar] = GroupMissingCloserBracketingBar
GroupOpenerToMissingCloserSymbol[Token`LongName`LeftDoubleBracketingBar] = GroupMissingCloserDoubleBracketingBar
GroupOpenerToMissingCloserSymbol[Token`LongName`LeftAssociation] = GroupMissingCloserAssociation

GroupOpenerToMissingCloserSymbol[Token`LinearSyntax`OpenParen] = GroupMissingCloserLinearSyntaxParen

GroupCloserToMissingOpenerSymbol[Token`CloseCurly] = GroupMissingOpenerList
GroupCloserToMissingOpenerSymbol[Token`BarGreater] = GroupMissingOpenerAssociation
GroupCloserToMissingOpenerSymbol[Token`CloseSquare] = GroupMissingOpenerSquare
GroupCloserToMissingOpenerSymbol[Token`CloseParen] = GroupMissingOpenerParen

GroupCloserToMissingOpenerSymbol[Token`LongName`RightAngleBracket] = GroupMissingOpenerAngleBracket
GroupCloserToMissingOpenerSymbol[Token`LongName`RightCeiling] = GroupMissingOpenerCeiling
GroupCloserToMissingOpenerSymbol[Token`LongName`RightFloor] = GroupMissingOpenerFloor
GroupCloserToMissingOpenerSymbol[Token`LongName`RightDoubleBracket] = GroupMissingOpenerDoubleBracket
GroupCloserToMissingOpenerSymbol[Token`LongName`RightBracketingBar] = GroupMissingOpenerBracketingBar
GroupCloserToMissingOpenerSymbol[Token`LongName`RightDoubleBracketingBar] = GroupMissingOpenerDoubleBracketingBar
GroupCloserToMissingOpenerSymbol[Token`LongName`RightAssociation] = GroupMissingOpenerAssociation

GroupCloserToMissingOpenerSymbol[Token`LinearSyntax`CloseParen] = GroupMissingOpenerLinearSyntaxParen






End[]

EndPackage[]
