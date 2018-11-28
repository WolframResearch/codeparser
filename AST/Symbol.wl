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
SyntaxErrorNode,
BlankNode,
BlankSequenceNode,
BlankNullSequenceNode,
PatternBlankNode,
PatternBlankSequenceNode,
PatternBlankNullSequenceNode,
OptionalDefaultNode,
SlotNode,
SlotSequenceNode,
OutNode,
InternalEmptyNode,
PrefixNode,
BinaryNode,
TernaryNode,
InfixNode,
PostfixNode,
GroupNode,
CallNode,
PartNode,
InternalMinusNode,
InternalTokenNode,
FileNode
}

$Options = {
DerivativeOrder,
Source,
SyntaxIssues
}


$Miscellaneous = {
(* when parsing f[1,] then we need to parse as f[1,Null] *)
Null,
SyntaxIssue,

(* for Nodes *)
File,
InternalMinus,
InternalEmpty,
Out,
Slot,
SlotSequence,
OptionalDefault,
Blank,
BlankSequence,
BlankNullSequence,
PatternBlank,
PatternBlankSequence,
PatternBlankNullSequence,

TernarySlashColon,
TagSet,
TagSetDelayed,
TagUnset,

TernaryTilde,

InternalInvalid,

Nothing
}

$Tokens = {
Token
}

$Characters = {
Character
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





PrefixOperatorToSymbol[Operator`BANG] = Not
PrefixOperatorToSymbol[Operator`PLUSPLUS] = PreIncrement
PrefixOperatorToSymbol[Operator`LESSLESS] = Get
PrefixOperatorToSymbol[Operator`MINUSMINUS] = PreDecrement
PrefixOperatorToSymbol[Operator`PLUS] = Plus
PrefixOperatorToSymbol[Operator`MINUS] = Minus
PrefixOperatorToSymbol[Operator`LONGNAME`SQRT] = Sqrt
PrefixOperatorToSymbol[Operator`LONGNAME`NOT] = Not
   
PrefixOperatorToSymbol[Operator`LONGNAME`PLUSMINUS] = PlusMinus
PrefixOperatorToSymbol[Operator`LONGNAME`SUM] = Sum
PrefixOperatorToSymbol[Operator`LONGNAME`MINUSPLUS] = MinusPlus
PrefixOperatorToSymbol[Operator`LONGNAME`DIFFERENTIALD] = DifferentialD
PrefixOperatorToSymbol[Operator`LONGNAME`MINUS] = Minus
PrefixOperatorToSymbol[Operator`LONGNAME`DEL] = Del
PrefixOperatorToSymbol[Operator`LONGNAME`SQUARE] = Square

PrefixOperatorToSymbol[Operator`LINEARSYNTAX`BANG] = LinearSyntaxBang




PostfixOperatorToSymbol[Operator`DOTDOT] = Repeated
PostfixOperatorToSymbol[Operator`BANG] = Factorial
PostfixOperatorToSymbol[Operator`MINUSMINUS] = Decrement
PostfixOperatorToSymbol[Operator`PLUSPLUS] = Increment
PostfixOperatorToSymbol[Operator`DOTDOTDOT] = RepeatedNull
PostfixOperatorToSymbol[Operator`AMP] = Function
PostfixOperatorToSymbol[Operator`BANGBANG] = Factorial2
PostfixOperatorToSymbol[Operator`TICK] = Derivative
   
PostfixOperatorToSymbol[Operator`LONGNAME`TRANSPOSE] = Transpose
PostfixOperatorToSymbol[Operator`LONGNAME`CONJUGATE] = Conjugate
PostfixOperatorToSymbol[Operator`LONGNAME`CONJUGATETRANSPOSE] = ConjugateTranspose
PostfixOperatorToSymbol[Operator`LONGNAME`HERMITIANCONJUGATE] = PostfixHermitianConjugate


InfixOperatorToSymbol[Operator`PLUS] = Plus
InfixOperatorToSymbol[Operator`STAR] = Times
InfixOperatorToSymbol[Operator`SLASHAT] = Map
InfixOperatorToSymbol[Operator`EQUAL] = Set
InfixOperatorToSymbol[Operator`CARET] = Power
InfixOperatorToSymbol[Operator`COLONEQUAL] = SetDelayed
InfixOperatorToSymbol[Operator`MINUSGREATER] = Rule
InfixOperatorToSymbol[Operator`QUESTION] = PatternTest
InfixOperatorToSymbol[Operator`ATAT] = Apply
InfixOperatorToSymbol[Operator`EQUALEQUAL] = Equal
InfixOperatorToSymbol[Operator`AMPAMP] = And
InfixOperatorToSymbol[Operator`GREATER] = Greater
InfixOperatorToSymbol[Operator`BAR] = Alternatives
InfixOperatorToSymbol[Operator`SLASHSEMICOLON] = Condition
InfixOperatorToSymbol[Operator`LESSEQUAL] = LessEqual
InfixOperatorToSymbol[Operator`BARBAR] = Or
InfixOperatorToSymbol[Operator`EQUALEQUALEQUAL] = SameQ
InfixOperatorToSymbol[Operator`SLASHDOT] = ReplaceAll
InfixOperatorToSymbol[Operator`COLONGREATER] = RuleDelayed
InfixOperatorToSymbol[Operator`LESSGREATER] = StringJoin
InfixOperatorToSymbol[Operator`EQUALBANGEQUAL] = UnsameQ
InfixOperatorToSymbol[Operator`GREATEREQUAL] = GreaterEqual
InfixOperatorToSymbol[Operator`SLASHSLASHDOT] = ReplaceRepeated
InfixOperatorToSymbol[Operator`LESS] = Less
InfixOperatorToSymbol[Operator`PLUSEQUAL] = AddTo
InfixOperatorToSymbol[Operator`TILDETILDE] = StringExpression
InfixOperatorToSymbol[Operator`BANGEQUAL] = Unequal
InfixOperatorToSymbol[Operator`DOT] = Dot
InfixOperatorToSymbol[Operator`STAREQUAL] = TimesBy
InfixOperatorToSymbol[Operator`MINUSEQUAL] = SubtractFrom
InfixOperatorToSymbol[Operator`SLASHEQUAL] = DivideBy
InfixOperatorToSymbol[Operator`CARETEQUAL] = UpSet
InfixOperatorToSymbol[Operator`SLASHSTAR] = RightComposition
InfixOperatorToSymbol[Operator`ATSTAR] = Composition
InfixOperatorToSymbol[Operator`CARETCOLONEQUAL] = UpSetDelayed
InfixOperatorToSymbol[Operator`STARSTAR] = NonCommutativeMultiply
If[$VersionNumber >= 11.1, InfixOperatorToSymbol[Operator`LESSMINUSGREATER] = TwoWayRule]
InfixOperatorToSymbol[Operator`SLASHSLASHAT] = MapAll
InfixOperatorToSymbol[Operator`COLONCOLON] = MessageName
InfixOperatorToSymbol[Operator`GREATERGREATER] = Put
InfixOperatorToSymbol[Operator`SLASHSLASH] = BinarySlashSlash
InfixOperatorToSymbol[Operator`MINUS] = Subtract
InfixOperatorToSymbol[Operator`SLASH] = Divide
InfixOperatorToSymbol[Operator`SEMICOLONSEMICOLON] = Span
InfixOperatorToSymbol[Operator`AT] = BinaryAt
InfixOperatorToSymbol[Operator`ATATAT] = BinaryAtAtAt
InfixOperatorToSymbol[Operator`SEMICOLON] = CompoundExpression
InfixOperatorToSymbol[Operator`FAKE`EQUALDOT] = Unset
   
InfixOperatorToSymbol[Operator`FAKE`IMPLICITTIMES] = InfixImplicitTimes
InfixOperatorToSymbol[Operator`FAKE`PATTERNCOLON] = Pattern
InfixOperatorToSymbol[Operator`FAKE`OPTIONALCOLON] = Optional
   
InfixOperatorToSymbol[Operator`LONGNAME`ELEMENT] = Element
InfixOperatorToSymbol[Operator`LONGNAME`RIGHTTEEARROW] = RightTeeArrow
InfixOperatorToSymbol[Operator`LONGNAME`TILDETILDE] = TildeTilde
InfixOperatorToSymbol[Operator`LONGNAME`SUBSETEQUAL] = SubsetEqual
InfixOperatorToSymbol[Operator`LONGNAME`SUBSET] = Subset
InfixOperatorToSymbol[Operator`LONGNAME`IMPLIES] = Implies
InfixOperatorToSymbol[Operator`LONGNAME`NOTTILDETILDE] = NotTildeTilde
InfixOperatorToSymbol[Operator`LONGNAME`PLUSMINUS] = PlusMinus
InfixOperatorToSymbol[Operator`LONGNAME`EQUIVALENT] = Equivalent
InfixOperatorToSymbol[Operator`LONGNAME`LEFTTRIANGLEEQUAL] = LeftTriangleEqual
InfixOperatorToSymbol[Operator`LONGNAME`TILDEEQUAL] = TildeEqual
InfixOperatorToSymbol[Operator`LONGNAME`SUPERSETEQUAL] = SupersetEqual
InfixOperatorToSymbol[Operator`LONGNAME`TILDEFULLEQUAL] = TildeFullEqual
InfixOperatorToSymbol[Operator`LONGNAME`DIRECTEDEDGE] = DirectedEdge
InfixOperatorToSymbol[Operator`LONGNAME`NOTELEMENT] = NotElement
InfixOperatorToSymbol[Operator`LONGNAME`NOTTILDEFULLEQUAL] = NotTildeFullEqual
InfixOperatorToSymbol[Operator`LONGNAME`CIRCLEDOT] = CircleDot
InfixOperatorToSymbol[Operator`LONGNAME`TIMES] = Times
InfixOperatorToSymbol[Operator`LONGNAME`RULE] = Rule
InfixOperatorToSymbol[Operator`LONGNAME`EQUAL] = Equal
InfixOperatorToSymbol[Operator`LONGNAME`LESSEQUAL] = LessEqual
InfixOperatorToSymbol[Operator`LONGNAME`RULEDELAYED] = RuleDelayed
InfixOperatorToSymbol[Operator`LONGNAME`UNDIRECTEDEDGE] = UndirectedEdge
InfixOperatorToSymbol[Operator`LONGNAME`FUNCTION] = Function
InfixOperatorToSymbol[Operator`LONGNAME`XOR] = Xor
InfixOperatorToSymbol[Operator`LONGNAME`DISTRIBUTED] = Distributed
InfixOperatorToSymbol[Operator`LONGNAME`CONDITIONED] = Conditioned
InfixOperatorToSymbol[Operator`LONGNAME`UNION] = Union
InfixOperatorToSymbol[Operator`LONGNAME`INTERSECTION] = Intersection
InfixOperatorToSymbol[Operator`LONGNAME`AND] = And
InfixOperatorToSymbol[Operator`LONGNAME`OR] = Or
InfixOperatorToSymbol[Operator`LONGNAME`NOTEQUAL] = Unequal
InfixOperatorToSymbol[Operator`LONGNAME`TENSORWEDGE] = TensorWedge
InfixOperatorToSymbol[Operator`LONGNAME`CENTERDOT] = CenterDot
InfixOperatorToSymbol[Operator`LONGNAME`CROSS] = Cross
InfixOperatorToSymbol[Operator`LONGNAME`GREATERTILDE] = GreaterTilde
InfixOperatorToSymbol[Operator`LONGNAME`PROPORTIONAL] = Proportional
InfixOperatorToSymbol[Operator`LONGNAME`LESSLESS] = LessLess
InfixOperatorToSymbol[Operator`LONGNAME`CONGRUENT] = Congruent
InfixOperatorToSymbol[Operator`LONGNAME`TILDE] = Tilde
InfixOperatorToSymbol[Operator`LONGNAME`MINUSPLUS] = MinusPlus
InfixOperatorToSymbol[Operator`LONGNAME`DOUBLELONGLEFTRIGHTARROW] = DoubleLongLeftRightArrow
InfixOperatorToSymbol[Operator`LONGNAME`RIGHTARROW] = RightArrow
InfixOperatorToSymbol[Operator`LONGNAME`SMALLCIRCLE] = SmallCircle
InfixOperatorToSymbol[Operator`LONGNAME`DOUBLELONGRIGHTARROW] = DoubleLongRightArrow
InfixOperatorToSymbol[Operator`LONGNAME`DIVIDES] = Divisible
InfixOperatorToSymbol[Operator`LONGNAME`LEFTRIGHTARROW] = LeftRightArrow
InfixOperatorToSymbol[Operator`LONGNAME`VERTICALSEPARATOR] = VerticalSeparator
InfixOperatorToSymbol[Operator`LONGNAME`LONGRIGHTARROW] = LongRightArrow
If[$VersionNumber >= 11.1, InfixOperatorToSymbol[Operator`LONGNAME`TWOWAYRULE] = TwoWayRule]
InfixOperatorToSymbol[Operator`LONGNAME`INVISIBLEAPPLICATION] = BinaryInvisibleApplication
InfixOperatorToSymbol[Operator`LONGNAME`BACKSLASH] = Backslash
InfixOperatorToSymbol[Operator`LONGNAME`DIAMOND] = Diamond
InfixOperatorToSymbol[Operator`LONGNAME`WEDGE] = Wedge
InfixOperatorToSymbol[Operator`LONGNAME`VEE] = Vee
InfixOperatorToSymbol[Operator`LONGNAME`CIRCLETIMES] = CircleTimes
InfixOperatorToSymbol[Operator`LONGNAME`STAR] = Star
InfixOperatorToSymbol[Operator`LONGNAME`VERTICALTILDE] = VerticalTilde
InfixOperatorToSymbol[Operator`LONGNAME`COPRODUCT] = Coproduct
InfixOperatorToSymbol[Operator`LONGNAME`CAP] = Cap
InfixOperatorToSymbol[Operator`LONGNAME`CUP] = Cup
InfixOperatorToSymbol[Operator`LONGNAME`CIRCLEPLUS] = CirclePlus
InfixOperatorToSymbol[Operator`LONGNAME`CIRCLEMINUS] = CircleMinus


GroupOpenerToSymbol[Operator`OPENCURLY] = List
GroupOpenerToSymbol[Operator`LESSBAR] = Association
GroupOpenerToSymbol[Operator`LONGNAME`LEFTANGLEBRACKET] = AngleBracket
GroupOpenerToSymbol[Operator`LONGNAME`LEFTCEILING] = Ceiling
GroupOpenerToSymbol[Operator`LONGNAME`LEFTFLOOR] = Floor
GroupOpenerToSymbol[Operator`LONGNAME`LEFTDOUBLEBRACKET] = GroupDoubleBracket
GroupOpenerToSymbol[Operator`OPENSQUARE] = GroupSquare
GroupOpenerToSymbol[Operator`LONGNAME`LEFTBRACKETINGBAR] = BracketingBar
GroupOpenerToSymbol[Operator`LONGNAME`LEFTDOUBLEBRACKETINGBAR] = DoubleBracketingBar
GroupOpenerToSymbol[Operator`OPENPAREN] = GroupParen
GroupOpenerToSymbol[Operator`LINEARSYNTAX`OPENPAREN] = GroupLinearSyntaxParen

GroupOpenerToCloser[Operator`OPENCURLY] = Operator`CLOSECURLY
GroupOpenerToCloser[Operator`LESSBAR] = Operator`BARGREATER
GroupOpenerToCloser[Operator`LONGNAME`LEFTANGLEBRACKET] = Operator`LONGNAME`RIGHTANGLEBRACKET
GroupOpenerToCloser[Operator`LONGNAME`LEFTCEILING] = Operator`LONGNAME`RIGHTCEILING
GroupOpenerToCloser[Operator`LONGNAME`LEFTFLOOR] = Operator`LONGNAME`RIGHTFLOOR
GroupOpenerToCloser[Operator`LONGNAME`LEFTDOUBLEBRACKET] = Operator`LONGNAME`RIGHTDOUBLEBRACKET
GroupOpenerToCloser[Operator`OPENSQUARE] = Operator`CLOSESQUARE
GroupOpenerToCloser[Operator`LONGNAME`LEFTBRACKETINGBAR] = Operator`LONGNAME`RIGHTBRACKETINGBAR
GroupOpenerToCloser[Operator`LONGNAME`LEFTDOUBLEBRACKETINGBAR] = Operator`LONGNAME`RIGHTDOUBLEBRACKETINGBAR
GroupOpenerToCloser[Operator`OPENPAREN] = Operator`CLOSEPAREN
GroupOpenerToCloser[Operator`LINEARSYNTAX`OPENPAREN] = Operator`LINEARSYNTAX`CLOSEPAREN

GroupCloserToSymbol[Operator`CLOSECURLY] = List
GroupCloserToSymbol[Operator`BARGREATER] = Association
GroupCloserToSymbol[Operator`LONGNAME`RIGHTANGLEBRACKET] = AngleBracket
GroupCloserToSymbol[Operator`LONGNAME`RIGHTCEILING] = Ceiling
GroupCloserToSymbol[Operator`LONGNAME`RIGHTFLOOR] = Floor
GroupCloserToSymbol[Operator`LONGNAME`RIGHTDOUBLEBRACKET] = GroupDoubleBracket
GroupCloserToSymbol[Operator`CLOSESQUARE] = GroupSquare
GroupCloserToSymbol[Operator`LONGNAME`RIGHTBRACKETINGBAR] = BracketingBar
GroupCloserToSymbol[Operator`LONGNAME`RIGHTDOUBLEBRACKETINGBAR] = DoubleBracketingBar
GroupCloserToSymbol[Operator`CLOSEPAREN] = GroupParen
GroupCloserToSymbol[Operator`LINEARSYNTAX`CLOSEPAREN] = GroupLinearSyntaxParen

GroupOpenerToMissingCloserSymbol[Operator`OPENCURLY] = GroupMissingCloserList
GroupOpenerToMissingCloserSymbol[Operator`LESSBAR] = GroupMissingCloserAssociation
GroupOpenerToMissingCloserSymbol[Operator`LONGNAME`LEFTANGLEBRACKET] = GroupMissingCloserAngleBracket
GroupOpenerToMissingCloserSymbol[Operator`LONGNAME`LEFTCEILING] = GroupMissingCloserCeiling
GroupOpenerToMissingCloserSymbol[Operator`LONGNAME`LEFTFLOOR] = GroupMissingCloserFloor
GroupOpenerToMissingCloserSymbol[Operator`LONGNAME`LEFTDOUBLEBRACKET] = GroupMissingCloserDoubleBracket
GroupOpenerToMissingCloserSymbol[Operator`OPENSQUARE] = GroupMissingCloserSquare
GroupOpenerToMissingCloserSymbol[Operator`LONGNAME`LEFTBRACKETINGBAR] = GroupMissingCloserBracketingBar
GroupOpenerToMissingCloserSymbol[Operator`LONGNAME`LEFTDOUBLEBRACKETINGBAR] = GroupMissingCloserDoubleBracketingBar
GroupOpenerToMissingCloserSymbol[Operator`OPENPAREN] = GroupMissingCloserParen
GroupOpenerToMissingCloserSymbol[Operator`LINEARSYNTAX`OPENPAREN] = GroupMissingCloserLinearSyntaxParen

GroupCloserToMissingOpenerSymbol[Operator`CLOSECURLY] = GroupMissingOpenerList
GroupCloserToMissingOpenerSymbol[Operator`BARGREATER] = GroupMissingOpenerAssociation
GroupCloserToMissingOpenerSymbol[Operator`LONGNAME`RIGHTANGLEBRACKET] = GroupMissingOpenerAngleBracket
GroupCloserToMissingOpenerSymbol[Operator`LONGNAME`RIGHTCEILING] = GroupMissingOpenerCeiling
GroupCloserToMissingOpenerSymbol[Operator`LONGNAME`RIGHTFLOOR] = GroupMissingOpenerFloor
GroupCloserToMissingOpenerSymbol[Operator`LONGNAME`RIGHTDOUBLEBRACKET] = GroupMissingOpenerDoubleBracket
GroupCloserToMissingOpenerSymbol[Operator`CLOSESQUARE] = GroupMissingOpenerSquare
GroupCloserToMissingOpenerSymbol[Operator`LONGNAME`RIGHTBRACKETINGBAR] = GroupMissingOpenerBracketingBar
GroupCloserToMissingOpenerSymbol[Operator`LONGNAME`RIGHTDOUBLEBRACKETINGBAR] = GroupMissingOpenerDoubleBracketingBar
GroupCloserToMissingOpenerSymbol[Operator`CLOSEPAREN] = GroupMissingOpenerParen
GroupCloserToMissingOpenerSymbol[Operator`LINEARSYNTAX`CLOSEPAREN] = GroupMissingOpenerLinearSyntaxParen




(*
These are nice strings, appropriate for using in other source languages.
So they are escaped accordingly.
*)


SymbolToPrefixOperatorString[Not] = "!"
SymbolToPrefixOperatorString[Minus] = "-"
SymbolToPrefixOperatorString[PreIncrement] = "++"
SymbolToPrefixOperatorString[PreDecrement] = "--"
SymbolToPrefixOperatorString[Plus] = "+"
SymbolToPrefixOperatorString[LinearSyntaxBang] = "\\!"
SymbolToPrefixOperatorString[Get] = "<<"

SymbolToPrefixOperatorString[Sqrt] = "\\[Sqrt]"
SymbolToPrefixOperatorString[PlusMinus] = "\\[PlusMinus]"
SymbolToPrefixOperatorString[MinusPlus] = "\\[MinusPlus]"
SymbolToPrefixOperatorString[DifferentialD] = "\\[DifferentialD]"


SymbolToInfixOperatorString[Map] = "/@"
SymbolToInfixOperatorString[Set] = "="
SymbolToInfixOperatorString[SetDelayed] = ":="
SymbolToInfixOperatorString[Rule] = "->"
SymbolToInfixOperatorString[PatternTest] = "?"
SymbolToInfixOperatorString[MessageName] = "::"
SymbolToInfixOperatorString[Divide] = "/"
SymbolToInfixOperatorString[Pattern] = ":"
SymbolToInfixOperatorString[Apply] = "@@"
SymbolToInfixOperatorString[Equal] = "=="
SymbolToInfixOperatorString[Condition] = "/;"
SymbolToInfixOperatorString[Greater] = ">"
SymbolToInfixOperatorString[LessEqual] = "<="
SymbolToInfixOperatorString[SameQ] = "==="
(* extra space *)
SymbolToInfixOperatorString[ReplaceAll] = "/. "
SymbolToInfixOperatorString[RuleDelayed] = ":>"
SymbolToInfixOperatorString[Span] = ";;"
SymbolToInfixOperatorString[GreaterEqual] = ">="
SymbolToInfixOperatorString[BinaryAt] = "@"
SymbolToInfixOperatorString[Power] = "^"
SymbolToInfixOperatorString[UnsameQ] = "=!="
SymbolToInfixOperatorString[BinaryAtAtAt] = "@@@"
SymbolToInfixOperatorString[BinarySlashSlash] = "//"
SymbolToInfixOperatorString[Less] = "<"
SymbolToInfixOperatorString[ReplaceRepeated] = "//."
SymbolToInfixOperatorString[AddTo] = "+="
SymbolToInfixOperatorString[Optional] = ":"
SymbolToInfixOperatorString[Unequal] = "!="
(* extra space *)
SymbolToInfixOperatorString[Dot] = " . "
SymbolToInfixOperatorString[SubtractFrom] = "-="
SymbolToInfixOperatorString[TimesBy] = "*="
SymbolToInfixOperatorString[DivideBy] = "/="
SymbolToInfixOperatorString[Composition] = "@*"
SymbolToInfixOperatorString[UpSetDelayed] = "^:="
SymbolToInfixOperatorString[UpSet] = "^="
SymbolToInfixOperatorString[RightComposition] = "/*"
SymbolToInfixOperatorString[MapAll] = "//@"
SymbolToInfixOperatorString[Put] = ">>"
SymbolToInfixOperatorString[Unset] = "=."
SymbolToInfixOperatorString[TernarySlashColon] = "/:"
SymbolToInfixOperatorString[TernaryTilde] = "~"

SymbolToInfixOperatorString[Element] = "\\[Element]"
SymbolToInfixOperatorString[SubsetEqual] = "\\[SubsetEqual]"
SymbolToInfixOperatorString[RightTeeArrow] = "\\[RightTeeArrow]"
SymbolToInfixOperatorString[LeftTriangleEqual] = "\\[LeftTriangleEqual]"
SymbolToInfixOperatorString[TildeFullEqual] = "\\[TildeFullEqual]"
SymbolToInfixOperatorString[NotTildeFullEqual] = "\\[NotTildeFullEqual]"
SymbolToInfixOperatorString[TildeTilde] = "\\[TildeTilde]"
SymbolToInfixOperatorString[NotTildeTilde] = "\\[NotTildeTilde]"
SymbolToInfixOperatorString[TildeEqual] = "\\[TildeEqual]"
SymbolToInfixOperatorString[Subset] = "\\[Subset]"
SymbolToInfixOperatorString[PlusMinus] = "\\[PlusMinus]"
SymbolToInfixOperatorString[NotElement] = "\\[NotElement]"
SymbolToInfixOperatorString[Implies] = "\\[Implies]"
SymbolToInfixOperatorString[Equivalent] = "\\[Equivalent]"
SymbolToInfixOperatorString[DirectedEdge] = "\\[DirectedEdge]"
SymbolToInfixOperatorString[SupersetEqual] = "\\[SupersetEqual]"
If[$VersionNumber >= 11.1, SymbolToInfixOperatorString[TwoWayRule] = "\\[TwoWayRule]"]
SymbolToInfixOperatorString[UndirectedEdge] = "\\[UndirectedEdge]"
SymbolToInfixOperatorString[Xor] = "\\[Xor]"
SymbolToInfixOperatorString[Function] = "\\[Function]"
SymbolToInfixOperatorString[Intersection] = "\\[Intersection]"
SymbolToInfixOperatorString[Union] = "\\[Union]"
SymbolToInfixOperatorString[Distributed] = "\\[Distributed]"
SymbolToInfixOperatorString[Conditioned] = "\\[Conditioned]"
SymbolToInfixOperatorString[CircleDot] = "\\[CircleDot]"
SymbolToInfixOperatorString[TensorWedge] = "\\[TensorWedge]"
SymbolToInfixOperatorString[CenterDot] = "\\[CenterDot]"
SymbolToInfixOperatorString[Cross] = "\\[Cross]"
SymbolToInfixOperatorString[GreaterTilde] = "\\[GreaterTilde]"
SymbolToInfixOperatorString[Proportional] = "\\[Proportional]"
SymbolToInfixOperatorString[LessLess] = "\\[LessLess]"
SymbolToInfixOperatorString[Congruent] = "\\[Congruent]"
SymbolToInfixOperatorString[Tilde] = "\\[Tilde]"
SymbolToInfixOperatorString[MinusPlus] = "\\[MinusPlus]"
SymbolToInfixOperatorString[DoubleLongLeftRightArrow] = "\\[DoubleLongLeftRightArrow]"
SymbolToInfixOperatorString[RightArrow] = "\\[RightArrow]"
SymbolToInfixOperatorString[SmallCircle] = "\\[SmallCircle]"
SymbolToInfixOperatorString[DoubleLongRightArrow] = "\\[DoubleLongRightArrow]"
SymbolToInfixOperatorString[Divisible] = "\\[Divides]"
SymbolToInfixOperatorString[LeftRightArrow] = "\\[LeftRightArrow]"
SymbolToInfixOperatorString[VerticalSeparator] = "\\[VerticalSeparator]"
SymbolToInfixOperatorString[LongRightArrow] = "\\[LongRightArrow]"
SymbolToInfixOperatorString[BinaryInvisibleApplication] = "\\[InvisibleApplication]"


(* extra space *)
SymbolToInfixOperatorString[CompoundExpression] = "; "
SymbolToInfixOperatorString[Times] = "*"
SymbolToInfixOperatorString[Alternatives] = "|"
SymbolToInfixOperatorString[And] = "&&"
SymbolToInfixOperatorString[Or] = "||"
SymbolToInfixOperatorString[InfixImplicitTimes] = " "
SymbolToInfixOperatorString[StringJoin] = "<>"
SymbolToInfixOperatorString[StringExpression] = "~~"
SymbolToInfixOperatorString[NonCommutativeMultiply] = "**"




SymbolToPostfixOperatorString[Function] = "&"
(* extra space *)
SymbolToPostfixOperatorString[Repeated] = " .. "
SymbolToPostfixOperatorString[Increment] = "++"
SymbolToPostfixOperatorString[Decrement] = "--"
(* extra space *)
SymbolToPostfixOperatorString[RepeatedNull] = " ... "
(* extra space *)
SymbolToPostfixOperatorString[Factorial] = "! "
(* extra space *)
SymbolToPostfixOperatorString[Factorial2] = "!! "

SymbolToPostfixOperatorString[Transpose] = "\\[Transpose]"
SymbolToPostfixOperatorString[Conjugate] = "\\[Conjugate]"




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


SymbolToTernaryOperatorPair[TagSet] = {TernarySlashColon, Set}
SymbolToTernaryOperatorPair[TagSetDelayed] = {TernarySlashColon, SetDelayed}
SymbolToTernaryOperatorPair[TagUnset] = {TernarySlashColon, Unset}
SymbolToTernaryOperatorPair[TernaryTilde] = {TernaryTilde, TernaryTilde}
SymbolToTernaryOperatorPair[Span] = {Span, Span}
SymbolToTernaryOperatorPair[MessageName] = {MessageName, MessageName}

End[]

EndPackage[]
