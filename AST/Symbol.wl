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

InternalTokenNode,
InternalAllNode,
InternalDotNode,
InternalNullNode,
InternalOneNode,

FileNode,

CommentNode,

SyntaxErrorNode,
CallMissingCloserNode

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
PostfixOperatorToSymbol[Token`Operator`SingleQuote] = Derivative
   
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

BinaryOperatorToSymbol[Token`Operator`Minus] = Minus
BinaryOperatorToSymbol[Token`Operator`Slash] = Divide
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
BinaryOperatorToSymbol[Token`Operator`GreaterGreaterGreater] = PutAppend
BinaryOperatorToSymbol[Token`Operator`SlashSlash] = BinarySlashSlash
BinaryOperatorToSymbol[Token`Operator`SemiSemi] = Span
BinaryOperatorToSymbol[Token`Operator`At] = BinaryAt
BinaryOperatorToSymbol[Token`Operator`AtAtAt] = BinaryAtAtAt

BinaryOperatorToSymbol[Token`Operator`Fake`EqualDot] = Unset
BinaryOperatorToSymbol[Token`Operator`Fake`PatternColon] = Pattern
BinaryOperatorToSymbol[Token`Operator`Fake`OptionalColon] = Optional

(* set relation operators *)
BinaryOperatorToSymbol[Token`Operator`LongName`Element] = Element
BinaryOperatorToSymbol[Token`Operator`LongName`Subset] = Subset
BinaryOperatorToSymbol[Token`Operator`LongName`Superset] = Superset
BinaryOperatorToSymbol[Token`Operator`LongName`SubsetEqual] = SubsetEqual
BinaryOperatorToSymbol[Token`Operator`LongName`SupersetEqual] = SupersetEqual
BinaryOperatorToSymbol[Token`Operator`LongName`NotElement] = NotElement
BinaryOperatorToSymbol[Token`Operator`LongName`NotSubset] = NotSubset
BinaryOperatorToSymbol[Token`Operator`LongName`NotSuperset] = NotSuperset
BinaryOperatorToSymbol[Token`Operator`LongName`NotSubsetEqual] = NotSubsetEqual
BinaryOperatorToSymbol[Token`Operator`LongName`NotSupersetEqual] = NotSupersetEqual

BinaryOperatorToSymbol[Token`Operator`LongName`Implies] = Implies
BinaryOperatorToSymbol[Token`Operator`LongName`PlusMinus] = PlusMinus
BinaryOperatorToSymbol[Token`Operator`LongName`DirectedEdge] = DirectedEdge
BinaryOperatorToSymbol[Token`Operator`LongName`Rule] = Rule
BinaryOperatorToSymbol[Token`Operator`LongName`RuleDelayed] = RuleDelayed
BinaryOperatorToSymbol[Token`Operator`LongName`UndirectedEdge] = UndirectedEdge
BinaryOperatorToSymbol[Token`Operator`LongName`Function] = Function
BinaryOperatorToSymbol[Token`Operator`LongName`MinusPlus] = MinusPlus
If[$VersionNumber >= 11.1, ToExpression["BinaryOperatorToSymbol[Token`Operator`LongName`TwoWayRule] = TwoWayRule"]]
BinaryOperatorToSymbol[Token`Operator`LongName`InvisibleApplication] = BinaryInvisibleApplication
BinaryOperatorToSymbol[Token`Operator`LongName`CircleMinus] = CircleMinus

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
If[$VersionNumber >= 11.1, ToExpression["SymbolToBinaryOperatorString[TwoWayRule] = \"\\\\[TwoWayRule]\""]]
SymbolToBinaryOperatorString[UndirectedEdge] = "\\[UndirectedEdge]"
SymbolToBinaryOperatorString[Function] = "\\[Function]"
SymbolToBinaryOperatorString[BinaryInvisibleApplication] = "\\[InvisibleApplication]"
SymbolToBinaryOperatorString[CircleMinus] = "\\[CircleMinus]"
SymbolToBinaryOperatorString[MinusPlus] = "\\[MinusPlus]"

(*
Infix
*)
InfixOperatorToSymbol[Token`Operator`Semi] = CompoundExpression

(* Plus and Times *)
InfixOperatorToSymbol[Token`Operator`Plus] = Plus

InfixOperatorToSymbol[Token`Operator`LongName`ImplicitPlus] = InfixImplicitPlus

InfixOperatorToSymbol[Token`Operator`Star] = Times

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
InfixOperatorToSymbol[Token`Operator`Less] = Less
InfixOperatorToSymbol[Token`Operator`Greater] = Greater
InfixOperatorToSymbol[Token`Operator`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`Operator`GreaterEqual] = GreaterEqual
InfixOperatorToSymbol[Token`Operator`BangEqual] = Unequal

InfixOperatorToSymbol[Token`Operator`LongName`Equal] = Equal
InfixOperatorToSymbol[Token`Operator`LongName`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`Operator`LongName`GreaterEqual] = GreaterEqual
InfixOperatorToSymbol[Token`Operator`LongName`NotEqual] = Unequal


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

InfixOperatorToSymbol[Token`Operator`LongName`CenterDot] = CenterDot
InfixOperatorToSymbol[Token`Operator`LongName`RightTeeArrow] = RightTeeArrow
InfixOperatorToSymbol[Token`Operator`LongName`TildeTilde] = TildeTilde
InfixOperatorToSymbol[Token`Operator`LongName`NotTildeTilde] = NotTildeTilde
InfixOperatorToSymbol[Token`Operator`LongName`Equivalent] = Equivalent
InfixOperatorToSymbol[Token`Operator`LongName`LeftTriangleEqual] = LeftTriangleEqual
InfixOperatorToSymbol[Token`Operator`LongName`TildeEqual] = TildeEqual
InfixOperatorToSymbol[Token`Operator`LongName`TildeFullEqual] = TildeFullEqual
InfixOperatorToSymbol[Token`Operator`LongName`NotTildeFullEqual] = NotTildeFullEqual
InfixOperatorToSymbol[Token`Operator`LongName`CircleDot] = CircleDot
InfixOperatorToSymbol[Token`Operator`LongName`Distributed] = Distributed
InfixOperatorToSymbol[Token`Operator`LongName`Conditioned] = Conditioned
InfixOperatorToSymbol[Token`Operator`LongName`Union] = Union
InfixOperatorToSymbol[Token`Operator`LongName`Intersection] = Intersection
InfixOperatorToSymbol[Token`Operator`LongName`TensorWedge] = TensorWedge
InfixOperatorToSymbol[Token`Operator`LongName`Cross] = Cross
InfixOperatorToSymbol[Token`Operator`LongName`GreaterTilde] = GreaterTilde
InfixOperatorToSymbol[Token`Operator`LongName`Proportional] = Proportional
InfixOperatorToSymbol[Token`Operator`LongName`LessLess] = LessLess
InfixOperatorToSymbol[Token`Operator`LongName`Congruent] = Congruent
InfixOperatorToSymbol[Token`Operator`LongName`Tilde] = Tilde
InfixOperatorToSymbol[Token`Operator`LongName`DoubleLongLeftRightArrow] = DoubleLongLeftRightArrow
InfixOperatorToSymbol[Token`Operator`LongName`RightArrow] = RightArrow
InfixOperatorToSymbol[Token`Operator`LongName`SmallCircle] = SmallCircle
InfixOperatorToSymbol[Token`Operator`LongName`DoubleLongRightArrow] = DoubleLongRightArrow
InfixOperatorToSymbol[Token`Operator`LongName`Divides] = Divisible
InfixOperatorToSymbol[Token`Operator`LongName`LeftRightArrow] = LeftRightArrow
InfixOperatorToSymbol[Token`Operator`LongName`VerticalSeparator] = VerticalSeparator
InfixOperatorToSymbol[Token`Operator`LongName`LongRightArrow] = LongRightArrow
InfixOperatorToSymbol[Token`Operator`LongName`Backslash] = Backslash
InfixOperatorToSymbol[Token`Operator`LongName`Diamond] = Diamond
InfixOperatorToSymbol[Token`Operator`LongName`Wedge] = Wedge
InfixOperatorToSymbol[Token`Operator`LongName`Vee] = Vee
InfixOperatorToSymbol[Token`Operator`LongName`CircleTimes] = CircleTimes
InfixOperatorToSymbol[Token`Operator`LongName`Star] = Star
InfixOperatorToSymbol[Token`Operator`LongName`VerticalTilde] = VerticalTilde
InfixOperatorToSymbol[Token`Operator`LongName`Coproduct] = Coproduct
InfixOperatorToSymbol[Token`Operator`LongName`Cap] = Cap
InfixOperatorToSymbol[Token`Operator`LongName`Cup] = Cup
InfixOperatorToSymbol[Token`Operator`LongName`CirclePlus] = CirclePlus
InfixOperatorToSymbol[Token`Operator`LongName`RightTriangle] = RightTriangle
InfixOperatorToSymbol[Token`Operator`LongName`LeftTriangle] = LeftTriangle

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
