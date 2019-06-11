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
LeafNode,

PrefixNode,
BinaryNode,
TernaryNode,
InfixNode,
PostfixNode,
GroupNode,
CallNode,
PrefixBinaryNode,

StartOfLineNode,
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
Metadata,
EndOfFile,

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

Comma,

(*InternalEmpty,*)
InternalInvalid,

Pattern,
Optional,

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
PrefixOperatorToSymbol[Token`LongName`CapitalDifferentialD] = CapitalDifferentialD
PrefixOperatorToSymbol[Token`LongName`Minus] = Minus
PrefixOperatorToSymbol[Token`LongName`Del] = Del
PrefixOperatorToSymbol[Token`LongName`Square] = Square
PrefixOperatorToSymbol[Token`LongName`Integral] = Integral
PrefixOperatorToSymbol[Token`LongName`ContourIntegral] = ContourIntegral
PrefixOperatorToSymbol[Token`LongName`DoubleContourIntegral] = DoubleContourIntegral
PrefixOperatorToSymbol[Token`LongName`ClockwiseContourIntegral] = ClockwiseContourIntegral
PrefixOperatorToSymbol[Token`LongName`CounterClockwiseContourIntegral] = CounterClockwiseContourIntegral
PrefixOperatorToSymbol[Token`LongName`Product] = Product
PrefixOperatorToSymbol[Token`LongName`ContinuedFractionK] = ContinuedFractionK
PrefixOperatorToSymbol[Token`LongName`CircleTimes] = CircleTimes
PrefixOperatorToSymbol[Token`LongName`ForAll] = ForAll
PrefixOperatorToSymbol[Token`LongName`Exists] = Exists
PrefixOperatorToSymbol[Token`LongName`NotExists] = NotExists
PrefixOperatorToSymbol[Token`LongName`Coproduct] = Coproduct
PrefixOperatorToSymbol[Token`LongName`Piecewise] = Piecewise

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



(*
Binary
*)

BinaryOperatorToSymbol[Token`LongName`Because] = Because
BinaryOperatorToSymbol[Token`LongName`Therefore] = Therefore

BinaryOperatorToSymbol[Token`LongName`RightTee] = RightTee
BinaryOperatorToSymbol[Token`LongName`LeftTee] = LeftTee

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

BinaryOperatorToSymbol[Token`LongName`Divide] = Divide
BinaryOperatorToSymbol[Token`LongName`DivisionSlash] = Divide

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

BinaryOperatorToSymbol[Token`LongName`SuchThat] = SuchThat






(*
Infix
*)
InfixOperatorToSymbol[Token`Semi] = CompoundExpression
InfixOperatorToSymbol[Token`Comma] = Comma

InfixOperatorToSymbol[Token`LongName`InvisibleComma] = Comma

InfixOperatorToSymbol[Token`EqualEqual] = Equal
InfixOperatorToSymbol[Token`BangEqual] = Unequal
InfixOperatorToSymbol[Token`Less] = Less
InfixOperatorToSymbol[Token`Greater] = Greater
InfixOperatorToSymbol[Token`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`GreaterEqual] = GreaterEqual
InfixOperatorToSymbol[Token`LongName`LessFullEqual] = LessFullEqual
InfixOperatorToSymbol[Token`LongName`NestedLessLess] = NestedLessLess
InfixOperatorToSymbol[Token`LongName`NotLess] = NotLess
InfixOperatorToSymbol[Token`LongName`NotLessLess] = NotLessLess
InfixOperatorToSymbol[Token`LongName`LongEqual] = Equal
InfixOperatorToSymbol[Token`LongName`Equal] = Equal
InfixOperatorToSymbol[Token`LongName`LessEqual] = LessEqual
InfixOperatorToSymbol[Token`LongName`GreaterEqual] = GreaterEqual
InfixOperatorToSymbol[Token`LongName`NotEqual] = Unequal
(*
force to be in System`:
VectorGreater
VectorGreaterEqual
VectorLess
VectorLessEqual
*)
InfixOperatorToSymbol[Token`LongName`VectorGreater] = System`VectorGreater
InfixOperatorToSymbol[Token`LongName`VectorGreaterEqual] = System`VectorGreaterEqual
InfixOperatorToSymbol[Token`LongName`VectorLess] = System`VectorLess
InfixOperatorToSymbol[Token`LongName`VectorLessEqual] = System`VectorLessEqual

InfixOperatorToSymbol[Token`LongName`NotEqualTilde] = NotEqualTilde
InfixOperatorToSymbol[Token`LongName`NotHumpEqual] = NotHumpEqual
InfixOperatorToSymbol[Token`LongName`NotHumpDownHump] = NotHumpDownHump
InfixOperatorToSymbol[Token`LongName`NotLeftTriangleBar] = NotLeftTriangleBar
InfixOperatorToSymbol[Token`LongName`NotRightTriangleBar] = NotRightTriangleBar
InfixOperatorToSymbol[Token`LongName`NotNestedLessLess] = NotNestedLessLess
InfixOperatorToSymbol[Token`LongName`NotLessSlantEqual] = NotLessSlantEqual
InfixOperatorToSymbol[Token`LongName`NotGreaterGreater] = NotGreaterGreater
InfixOperatorToSymbol[Token`LongName`NotNestedGreaterGreater] = NotNestedGreaterGreater
InfixOperatorToSymbol[Token`LongName`NotGreaterSlantEqual] = NotGreaterSlantEqual
InfixOperatorToSymbol[Token`LongName`NotPrecedesEqual] = NotPrecedesEqual
InfixOperatorToSymbol[Token`LongName`NotSucceedsEqual] = NotSucceedsEqual

(* other flattening operators *)
InfixOperatorToSymbol[Token`EqualEqualEqual] = SameQ
InfixOperatorToSymbol[Token`EqualBangEqual] = UnsameQ

InfixOperatorToSymbol[Token`AtStar] = Composition
InfixOperatorToSymbol[Token`SlashStar] = RightComposition

(* set relation operators *)
InfixOperatorToSymbol[Token`LongName`Element] = Element
InfixOperatorToSymbol[Token`LongName`Subset] = Subset
InfixOperatorToSymbol[Token`LongName`Superset] = Superset
InfixOperatorToSymbol[Token`LongName`SubsetEqual] = SubsetEqual
InfixOperatorToSymbol[Token`LongName`SupersetEqual] = SupersetEqual
InfixOperatorToSymbol[Token`LongName`NotElement] = NotElement
InfixOperatorToSymbol[Token`LongName`NotSubset] = NotSubset
InfixOperatorToSymbol[Token`LongName`NotSuperset] = NotSuperset
InfixOperatorToSymbol[Token`LongName`NotSubsetEqual] = NotSubsetEqual
InfixOperatorToSymbol[Token`LongName`NotSupersetEqual] = NotSupersetEqual
InfixOperatorToSymbol[Token`LongName`NotSquareSubset] = NotSquareSubset
InfixOperatorToSymbol[Token`LongName`NotSquareSuperset] = NotSquareSuperset

(* Plus and Times *)
InfixOperatorToSymbol[Token`Plus] = Plus

InfixOperatorToSymbol[Token`LongName`ImplicitPlus] = Plus

InfixOperatorToSymbol[Token`Star] = Times

InfixOperatorToSymbol[Token`Fake`ImplicitTimes] = Times

InfixOperatorToSymbol[Token`LongName`Times] = Times
InfixOperatorToSymbol[Token`LongName`InvisibleTimes] = Times

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

InfixOperatorToSymbol[Token`LongName`LeftArrow] = LeftArrow
InfixOperatorToSymbol[Token`LongName`RightArrow] = RightArrow
InfixOperatorToSymbol[Token`LongName`LeftRightArrow] = LeftRightArrow
InfixOperatorToSymbol[Token`LongName`UpperLeftArrow] = UpperLeftArrow
InfixOperatorToSymbol[Token`LongName`UpperRightArrow] = UpperRightArrow
InfixOperatorToSymbol[Token`LongName`LowerRightArrow] = LowerRightArrow
InfixOperatorToSymbol[Token`LongName`LowerLeftArrow] = LowerLeftArrow
InfixOperatorToSymbol[Token`LongName`LeftTeeArrow] = LeftTeeArrow
InfixOperatorToSymbol[Token`LongName`RightTeeArrow] = RightTeeArrow
InfixOperatorToSymbol[Token`LongName`LeftVector] = LeftVector
InfixOperatorToSymbol[Token`LongName`DownLeftVector] = DownLeftVector
InfixOperatorToSymbol[Token`LongName`RightVector] = RightVector
InfixOperatorToSymbol[Token`LongName`DownRightVector] = DownRightVector
InfixOperatorToSymbol[Token`LongName`RightArrowLeftArrow] = RightArrowLeftArrow
InfixOperatorToSymbol[Token`LongName`LeftArrowRightArrow] = LeftArrowRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLeftArrow] = DoubleLeftArrow
InfixOperatorToSymbol[Token`LongName`DoubleRightArrow] = DoubleRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLeftRightArrow] = DoubleLeftRightArrow
InfixOperatorToSymbol[Token`LongName`LeftArrowBar] = LeftArrowBar
InfixOperatorToSymbol[Token`LongName`RightArrowBar] = RightArrowBar
InfixOperatorToSymbol[Token`LongName`LeftRightVector] = LeftRightVector
InfixOperatorToSymbol[Token`LongName`DownLeftRightVector] = DownLeftRightVector
InfixOperatorToSymbol[Token`LongName`LeftVectorBar] = LeftVectorBar
InfixOperatorToSymbol[Token`LongName`RightVectorBar] = RightVectorBar
InfixOperatorToSymbol[Token`LongName`DownLeftVectorBar] = DownLeftVectorBar
InfixOperatorToSymbol[Token`LongName`DownRightVectorBar] = DownRightVectorBar
InfixOperatorToSymbol[Token`LongName`LeftTeeVector] = LeftTeeVector
InfixOperatorToSymbol[Token`LongName`RightTeeVector] = RightTeeVector
InfixOperatorToSymbol[Token`LongName`DownLeftTeeVector] = DownLeftTeeVector
InfixOperatorToSymbol[Token`LongName`DownRightTeeVector] = DownRightTeeVector
InfixOperatorToSymbol[Token`LongName`ShortRightArrow] = ShortRightArrow
InfixOperatorToSymbol[Token`LongName`ShortLeftArrow] = ShortDownArrow

InfixOperatorToSymbol[Token`LongName`UpArrow] = UpArrow
InfixOperatorToSymbol[Token`LongName`DownArrow] = DownArrow
InfixOperatorToSymbol[Token`LongName`UpDownArrow] = UpDownArrow
InfixOperatorToSymbol[Token`LongName`UpTeeArrow] = UpTeeArrow
InfixOperatorToSymbol[Token`LongName`DownTeeArrow] = DownTeeArrow
InfixOperatorToSymbol[Token`LongName`RightUpVector] = RightUpVector
InfixOperatorToSymbol[Token`LongName`LeftUpVector] = LeftUpVector
InfixOperatorToSymbol[Token`LongName`RightDownVector] = RightDownVector
InfixOperatorToSymbol[Token`LongName`LeftDownVector] = LeftDownVector
InfixOperatorToSymbol[Token`LongName`UpArrowDownArrow] = UpArrowDownArrow
InfixOperatorToSymbol[Token`LongName`DoubleUpArrow] = DoubleUpArrow
InfixOperatorToSymbol[Token`LongName`DoubleDownArrow] = DoubleDownArrow
InfixOperatorToSymbol[Token`LongName`DoubleUpDownArrow] = DoubleUpDownArrow
InfixOperatorToSymbol[Token`LongName`DownArrowUpArrow] = DownArrowUpArrow
InfixOperatorToSymbol[Token`LongName`LongLeftArrow] = LongLeftArrow
InfixOperatorToSymbol[Token`LongName`LongRightArrow] = LongRightArrow
InfixOperatorToSymbol[Token`LongName`LongLeftRightArrow] = LongLeftRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLongLeftArrow] = DoubleLongLeftArrow
InfixOperatorToSymbol[Token`LongName`DoubleLongRightArrow] = DoubleLongRightArrow
InfixOperatorToSymbol[Token`LongName`DoubleLongLeftRightArrow] = DoubleLongLeftRightArrow
InfixOperatorToSymbol[Token`LongName`UpArrowBar] = UpArrowBar
InfixOperatorToSymbol[Token`LongName`DownArrowBar] = DownArrowBar
InfixOperatorToSymbol[Token`LongName`RightUpDownVector] = RightUpDownVector
InfixOperatorToSymbol[Token`LongName`LeftUpDownVector] = LeftUpDownVector
InfixOperatorToSymbol[Token`LongName`RightUpVectorBar] = RightUpVectorBar
InfixOperatorToSymbol[Token`LongName`RightDownVectorBar] = RightDownVectorBar
InfixOperatorToSymbol[Token`LongName`LeftUpVectorBar] = LeftUpVectorBar
InfixOperatorToSymbol[Token`LongName`LeftDownVectorBar] = LeftDownVectorBar
InfixOperatorToSymbol[Token`LongName`RightUpTeeVector] = RightUpTeeVector
InfixOperatorToSymbol[Token`LongName`RightDownTeeVector] = RightDownTeeVector
InfixOperatorToSymbol[Token`LongName`LeftUpTeeVector] = LeftUpTeeVector
InfixOperatorToSymbol[Token`LongName`LeftDownTeeVector] = LeftDownTeeVector
InfixOperatorToSymbol[Token`LongName`UpEquilibrium] = UpEquilibrium
InfixOperatorToSymbol[Token`LongName`ReverseUpEquilibrium] = ReverseUpEquilibrium
InfixOperatorToSymbol[Token`LongName`ShortUpArrow] = ShortUpArrow
InfixOperatorToSymbol[Token`LongName`ShortDownArrow] = ShortDownArrow

InfixOperatorToSymbol[Token`LongName`CenterDot] = CenterDot
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
InfixOperatorToSymbol[Token`LongName`TensorProduct] = TensorProduct
InfixOperatorToSymbol[Token`LongName`Cross] = Cross
InfixOperatorToSymbol[Token`LongName`GreaterTilde] = GreaterTilde
InfixOperatorToSymbol[Token`LongName`Proportional] = Proportional
InfixOperatorToSymbol[Token`LongName`Proportion] = Proportion
InfixOperatorToSymbol[Token`LongName`LessLess] = LessLess
InfixOperatorToSymbol[Token`LongName`Congruent] = Congruent
InfixOperatorToSymbol[Token`LongName`Tilde] = Tilde
InfixOperatorToSymbol[Token`LongName`SmallCircle] = SmallCircle
InfixOperatorToSymbol[Token`LongName`Divides] = Divisible
InfixOperatorToSymbol[Token`LongName`VerticalSeparator] = VerticalSeparator
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
InfixOperatorToSymbol[Token`LongName`Equilibrium] = Equilibrium
InfixOperatorToSymbol[Token`LongName`ReverseEquilibrium] = ReverseEquilibrium
InfixOperatorToSymbol[Token`LongName`ReverseElement] = ReverseElement
InfixOperatorToSymbol[Token`LongName`NotReverseElement] = NotReverseElement
InfixOperatorToSymbol[Token`LongName`NotTilde] = NotTilde
InfixOperatorToSymbol[Token`LongName`EqualTilde] = EqualTilde
InfixOperatorToSymbol[Token`LongName`PrecedesSlantEqual] = PrecedesSlantEqual
InfixOperatorToSymbol[Token`LongName`SucceedsSlantEqual] = SucceedsSlantEqual
(*
LessSlantEqual and GreaterSlandEqual parse to LessEqual and GreaterEqual
*)
InfixOperatorToSymbol[Token`LongName`LessSlantEqual] = LessEqual
InfixOperatorToSymbol[Token`LongName`GreaterSlantEqual] = GreaterEqual
InfixOperatorToSymbol[Token`LongName`NotPrecedesSlantEqual] = NotPrecedesSlantEqual
InfixOperatorToSymbol[Token`LongName`NotSucceedsSlantEqual] = NotSucceedsSlantEqual





StartOfLineOperatorToSymbol[Token`Question] = Information
StartOfLineOperatorToSymbol[Token`QuestionQuestion] = Information
StartOfLineOperatorToSymbol[Token`Bang] = Run
StartOfLineOperatorToSymbol[Token`BangBang] = FilePrint





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
GroupOpenerToSymbol[Token`LongName`OpenCurlyQuote] = CurlyQuote
GroupOpenerToSymbol[Token`LongName`OpenCurlyDoubleQuote] = CurlyDoubleQuote

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
GroupOpenerToCloser[Token`LongName`OpenCurlyQuote] = Token`LongName`CloseCurlyQuote
GroupOpenerToCloser[Token`LongName`OpenCurlyDoubleQuote] = Token`LongName`CloseCurlyDoubleQuote

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
GroupCloserToOpener[Token`LongName`CloseCurlyQuote] = Token`LongName`OpenCurlyQuote
GroupCloserToOpener[Token`LongName`CloseCurlyDoubleQuote] = Token`LongName`OpenCurlyDoubleQuote

GroupCloserToOpener[Token`LinearSyntax`CloseParen] = Token`LinearSyntax`OpenParen




PrefixBinaryOperatorToSymbol[Token`LongName`Integral] = Integrate







End[]

EndPackage[]
