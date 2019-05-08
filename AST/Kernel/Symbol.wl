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
OptionalDefaultNode,
TokenNode,
InternalAllNode,
InternalNullNode,
InternalOneNode,

PrefixNode,
BinaryNode,
TernaryNode,
InfixNode,
PostfixNode,
GroupNode,
CallNode,
PrefixBinaryNode,

BlankNode,
BlankSequenceNode,
BlankNullSequenceNode,
PatternBlankNode,
PatternBlankSequenceNode,
PatternBlankNullSequenceNode,
OptionalDefaultPatternNode,

FileNode,

SyntaxErrorNode,
GroupMissingCloserNode,
GroupMissingOpenerNode,
AbstractSyntaxErrorNode,

PackageNode,
ContextNode,
StaticAnalysisIgnoreNode

}

$Options = {
Source,
SyntaxIssues
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

Symbol,
String,
Real,
Integer,
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

$Characters = {
WLCharacter
}

$Groups = {
List,
Association,
AngleBracket,
Ceiling,
Floor,
GroupDoubleBracket,
GroupSquare,
BracketingBar,
DoubleBracketingBar,
GroupParen,
GroupLinearSyntaxParen,

Nothing
}


PrefixOperatorToSymbol[Token`Bang] = Not
PrefixOperatorToSymbol[Token`PlusPlus] = PreIncrement
PrefixOperatorToSymbol[Token`LessLess] = Get
PrefixOperatorToSymbol[Token`MinusMinus] = PreDecrement
PrefixOperatorToSymbol[Token`Plus] = Plus
PrefixOperatorToSymbol[Token`Minus] = Minus
PrefixOperatorToSymbol[Token`Question] = Information
PrefixOperatorToSymbol[Token`QuestionQuestion] = Information

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
(*
Use BinaryAt here
*)
BinaryOperatorToSymbol[Token`LongName`InvisibleApplication] = BinaryAt
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
InfixOperatorToSymbol[Token`LongName`PermutationProduct] = PermutationProduct






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



GroupCloserToOpener[Token`CloseCurly] = Token`OpenCurly
GroupCloserToOpener[Token`BarGreater] = Token`LessBar
GroupCloserToOpener[Token`CloseSquare] = Token`OpenSquare
GroupCloserToOpener[Token`CloseParen] = Token`OpenParen

GroupCloserToOpener[Token`LongName`RightAngleBracket] = Token`LongName`LeftAngleBracket
GroupCloserToOpener[Token`LongName`RightCeiling] = Token`LongName`LeftCeiling
GroupCloserToOpener[Token`LongName`RightFloor] = Token`LongName`LeftFloor
GroupCloserToOpener[Token`LongName`RightDoubleBracket] = Token`LongName`LeftDoubleBracket
GroupCloserToOpener[Token`LongName`RightBracketingBar] = Token`LongName`LeftBracketingBar
GroupCloserToOpener[Token`LongName`RightDoubleBracketingBar] = Token`LongName`LeftDoubleBracketingBar
GroupCloserToOpener[Token`LongName`RightAssociation] = Token`LongName`LeftAssociation

GroupCloserToOpener[Token`LinearSyntax`CloseParen] = Token`LinearSyntax`OpenParen




PrefixBinaryOperatorToSymbol[Token`LongName`Integral] = Integrate







End[]

EndPackage[]
