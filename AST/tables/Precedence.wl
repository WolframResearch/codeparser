<|
Precedence`UNUSED -> -1,

Precedence`LOWEST -> 0,

Precedence`SEMICOLON -> 10, (* Precedence[CompoundExpression] *)

Precedence`GREATERGREATER -> 30, (* Precedence[Put] *)

Precedence`LONGNAME`CONDITIONED -> Next,
Precedence`LONGNAME`DISTRIBUTED -> Next,

Precedence`EQUAL -> 40, (* Precedence[Set] *)
Precedence`COLONEQUAL -> Precedence`EQUAL,
Precedence`CARETEQUAL -> Precedence`EQUAL,
Precedence`CARETCOLONEQUAL -> Precedence`EQUAL,
Precedence`SLASHCOLON -> Precedence`EQUAL,
Precedence`LONGNAME`FUNCTION -> Precedence`EQUAL,

Precedence`LONGNAME`THEREFORE -> 50, (* Precedence[ThereFore] *)

Precedence`LONGNAME`VERTICALSEPARATOR -> 60, (* Precedence[VerticalSeparator] *)

Precedence`SLASHSLASH -> Next,

Precedence`LONGNAME`COLON -> 80, (* Precedence[Colon] *)

Precedence`AMP -> 90, (* Precedence[Function] *)

Precedence`PLUSEQUAL -> 100, (* Precedence[AddTo] *)
Precedence`STAREQUAL -> Precedence`PLUSEQUAL,
Precedence`MINUSEQUAL -> Precedence`PLUSEQUAL,
Precedence`SLASHEQUAL -> Precedence`PLUSEQUAL,

Precedence`SLASHDOT -> 110, (* Precedence[ReplaceAll] *)
Precedence`SLASHSLASHDOT -> Precedence`SLASHDOT,

Precedence`MINUSGREATER -> 120, (* Precedence[Rule] *)
Precedence`COLONGREATER -> Precedence`MINUSGREATER,
Precedence`LONGNAME`RULE -> Precedence`MINUSGREATER,
Precedence`LONGNAME`RULEDELAYED -> Precedence`MINUSGREATER,

Precedence`LESSMINUSGREATER -> 125, (* Precedence[TwoWayRule] *)
Precedence`LONGNAME`TWOWAYRULE -> Precedence`LESSMINUSGREATER,

Precedence`SLASHSEMICOLON -> 130, (* Precedence[Condition] *)

Precedence`TILDETILDE -> 135, (* Precedence[StringExpression] *)

Precedence`OPTIONALCOLON -> 140, (* Precedence[Optional] *)

Precedence`PATTERNCOLON -> 150, (* Precedence[Pattern] *)

Precedence`BAR -> 160, (* Precedence[Alternatives] *)

Precedence`DOTDOT -> 170, (* Precedence[Repeated] *)
Precedence`DOTDOTDOT -> 170, (* Precedence[RepeatedNull] *)

Precedence`LONGNAME`SUCHTHAT -> 180, (* Precedence[SuchThat] *)

Precedence`LONGNAME`RIGHTTEE -> 190, (* Precedence[RightTee] *)

Precedence`LONGNAME`IMPLIES -> 200, (* Precedence[Implies] *)

Precedence`LONGNAME`EQUIVALENT -> 205, (* Precedence[Equivalent] *)

Precedence`BARBAR -> 215, (* Precedence[Or] *)
Precedence`LONGNAME`OR -> Precedence`BARBAR,
Precedence`LONGNAME`NOR -> Precedence`BARBAR,

(*
Precedence[Xor] is 215, which is equal to Or.
But this is wrong, a \[Xor] b is between || and &&
*)
Precedence`LONGNAME`XOR -> Next,
Precedence`LONGNAME`XNOR -> Precedence`LONGNAME`XOR,

(*
Precedence[And] is 215, which is equal to Or.
But this is wrong, a && b has higher precedence than a || b
*)
Precedence`AMPAMP -> Next,
Precedence`LONGNAME`AND -> Precedence`AMPAMP,
Precedence`LONGNAME`NAND -> Precedence`AMPAMP,

Precedence`PREFIX`BANG -> 230, (* Precedence[Not] *)
Precedence`LONGNAME`NOT -> Precedence`PREFIX`BANG,

Precedence`LONGNAME`ELEMENT -> 250, (* Precedence[Element] *)
Precedence`LONGNAME`SUBSETEQUAL -> Precedence`LONGNAME`ELEMENT,
Precedence`LONGNAME`SUBSET -> Precedence`LONGNAME`ELEMENT,
Precedence`LONGNAME`NOTELEMENT -> Precedence`LONGNAME`ELEMENT,
Precedence`LONGNAME`SUPERSETEQUAL -> Precedence`LONGNAME`ELEMENT,

(*
Precedence[SameQ] is 290, but empirically it is between \[Element] and \[RightTeeArrow]
*)
Precedence`EQUALEQUALEQUAL -> Next,
Precedence`EQUALBANGEQUAL -> Precedence`EQUALEQUALEQUAL,

Precedence`LONGNAME`RIGHTTEEARROW -> 270, (* Precedence[RightTeeArrow] *)
Precedence`LONGNAME`RIGHTARROW -> 270,
Precedence`LONGNAME`LEFTRIGHTARROW -> 270,

Precedence`LONGNAME`VERTICALBAR -> 280, (* Precedence[VerticalBar] *)
Precedence`LONGNAME`NOTVERTICALBAR -> Precedence`LONGNAME`VERTICALBAR,
Precedence`LONGNAME`DOUBLEVERTICALBAR -> Precedence`LONGNAME`VERTICALBAR,
Precedence`LONGNAME`NOTDOUBLEVERTICALBAR -> Precedence`LONGNAME`VERTICALBAR,

Precedence`EQUALEQUAL -> 290, (* Precedence[Equal] *)
Precedence`GREATER -> 290, (* Precedence[Greater] *)
Precedence`LESSEQUAL -> 290, (* Precedence[LessEqual] *)
Precedence`GREATEREQUAL -> 290, (* Precedence[GreaterEqual] *)
Precedence`LESS -> 290, (* Precedence[Less] *)
Precedence`BANGEQUAL -> 290, (* Precedence[Unequal] *)
Precedence`LONGNAME`EQUAL -> Precedence`EQUALEQUAL,
Precedence`LONGNAME`LESSEQUAL -> Precedence`LESSEQUAL,
Precedence`LONGNAME`NOTEQUAL -> Precedence`BANGEQUAL,
Precedence`LONGNAME`GREATEREQUAL -> Precedence`GREATEREQUAL,
Precedence`LONGNAME`TILDETILDE -> 290, (* Precedence[TildeTilde] *)
Precedence`LONGNAME`NOTTILDETILDE -> 290, (* Precedence[NotTildeTilde] *)
Precedence`LONGNAME`LEFTTRIANGLEEQUAL -> 290, (* Precedence[LeftTriangleEqual] *)
Precedence`LONGNAME`TILDEEQUAL -> 290, (* Precedence[TildeEqual] *)
Precedence`LONGNAME`TILDEFULLEQUAL -> 290, (* Precedence[TildeFullEqual] *)
Precedence`LONGNAME`NOTTILDEFULLEQUAL -> 290, (* Precedence[NotTildeFullEqual] *)
Precedence`LONGNAME`GREATERTILDE -> 290,
Precedence`LONGNAME`TILDE -> 290,
Precedence`LONGNAME`PROPORTIONAL -> Precedence`EQUALEQUAL,
Precedence`LONGNAME`LESSLESS -> Precedence`EQUALEQUAL,
Precedence`LONGNAME`CONGRUENT -> Precedence`EQUALEQUAL,

Precedence`LONGNAME`DIRECTEDEDGE -> 295, (* Precedence[DirectedEdge] *)
Precedence`LONGNAME`UNDIRECTEDEDGE -> 295, (* Precedence[UndirectedEdge] *)

Precedence`SEMICOLONSEMICOLON -> 305, (* Precedence[Span] *)

Precedence`LONGNAME`UNION -> Next,
Precedence`LONGNAME`INTERSECTION -> Next,

Precedence`INFIX`PLUS -> 310, (* Precedence[Plus] *)
Precedence`INFIX`MINUS -> 310, (* Precedence[Minus] *)
Precedence`LONGNAME`PLUSMINUS -> 310, (* Precedence[PlusMinus] *)
Precedence`LONGNAME`MINUSPLUS -> 310,

Precedence`LONGNAME`SUM -> 320, (* Precedence[Sum] *)

Precedence`LONGNAME`CIRCLEPLUS -> 330, (* Precedence[CirclePlus] *)
Precedence`LONGNAME`CIRCLEMINUS -> Precedence`LONGNAME`CIRCLEPLUS,

Precedence`LONGNAME`CUP-> 340, (* Precedence[Cup] *)

Precedence`LONGNAME`CAP -> 350, (* Precedence[Cap] *)

Precedence`LONGNAME`COPRODUCT -> 360, (* Precedence[Coproduct] *)

Precedence`LONGNAME`VERTICALTILDE -> 370, (* Precedence[VerticalTilde] *)

Precedence`LONGNAME`STAR -> 390, (* Precedence[Star] *)

Precedence`STAR -> 400, (* Precedence[Times] *)
Precedence`LONGNAME`TIMES -> Precedence`STAR,
Precedence`FAKE`IMPLICITTIMES -> Precedence`STAR,

Precedence`LONGNAME`CENTERDOT -> 410, (* Precedence[CenterDot] *)

Precedence`LONGNAME`CIRCLETIMES -> 420, (* Precedence[CircleTimes] *)

(*
Precedence[TensorWedge] is 500, but empirically it is between \[CircleTimes] and \[Vee]
*)
Precedence`LONGNAME`TENSORWEDGE -> Next,

Precedence`LONGNAME`VEE -> 430, (* Precedence[Vee] *)

Precedence`LONGNAME`WEDGE -> 440, (* Precedence[Wedge] *)

Precedence`LONGNAME`DIAMOND -> 450, (* Precedence[Backslash] *)

Precedence`LONGNAME`BACKSLASH -> 460, (* Precedence[Backslash] *)

Precedence`SLASH -> 470, (* Precedence[Divide] *)
Precedence`LONGNAME`DIVIDES -> Precedence`SLASH,

Precedence`PREFIX`PLUS -> Next,
Precedence`PREFIX`MINUS -> Precedence`PREFIX`PLUS,
Precedence`PREFIX`LONGNAME`PLUSMINUS -> Precedence`PREFIX`PLUS,
Precedence`PREFIX`LONGNAME`MINUSPLUS -> Precedence`PREFIX`PLUS,
Precedence`PREFIX`LONGNAME`MINUS -> Precedence`PREFIX`PLUS,

Precedence`DOT -> 490, (* Precedence[Dot] *)
Precedence`LONGNAME_CROSS -> Next,

Precedence`STARSTAR -> 510, (* Precedence[NonCommutativeMultiply] *)

Precedence`LONGNAME`CIRCLEDOT -> 520, (* Precedence[CircleDot] *)

Precedence`LONGNAME`SMALLCIRCLE -> 530, (* Precedence[SmallCircle] *)
Precedence`LONGNAME`SQUARE -> Precedence`LONGNAME`SMALLCIRCLE,

Precedence`LONGNAME`DIFFERENTIALD -> 550, (* Precedence[DifferentialD] *)
Precedence`LONGNAME`DEL -> Precedence`LONGNAME`DIFFERENTIALD,

Precedence`LONGNAME`SQRT -> Next,

Precedence`LONGNAME`DOUBLELONGLEFTRIGHTARROW -> 580, (* Precedence[DoubleLongLeftRightArrow] *)
Precedence`LONGNAME`DOUBLELONGRIGHTARROW -> 580,
Precedence`LONGNAME`LONGRIGHTARROW -> 580,

Precedence`CARET -> 590, (* Precedence[Power] *)

Precedence`LESSGREATER -> 600, (* Precedence[StringJoin] *)

Precedence`POSTFIX`BANG -> 610, (* Precedence[Factorial] *)
Precedence`TICK -> Precedence`POSTFIX`BANG,
Precedence`BANGBANG -> 610, (* Precedence[Factorial2] *)

Precedence`LONGNAME`TRANSPOSE -> Next,
Precedence`LONGNAME`CONJUGATE -> Precedence`LONGNAME`TRANSPOSE,
Precedence`LONGNAME`CONJUGATETRANSPOSE -> Precedence`LONGNAME`TRANSPOSE,
Precedence`LONGNAME`HERMITIANCONJUGATE -> Precedence`LONGNAME`TRANSPOSE,

Precedence`ATAT -> 620, (* Precedence[Apply] *)
Precedence`SLASHAT -> 620, (* Precedence[Map] *)
Precedence`ATATAT -> Precedence`ATAT,
Precedence`SLASHSLASHAT -> 620, (* Precedence[MapAll] *)

Precedence`TILDE -> Next,

Precedence`AT -> Next,
Precedence`LONGNAME`INVISIBLEAPPLICATION -> Precedence`AT,

Precedence`SLASHSTAR -> 624, (* Precedence[RightComposition] *)
Precedence`ATSTAR -> 625, (* Precedence[Composition] *)

Precedence`PREFIX`PLUSPLUS -> 660, (* Precedence[PreIncrement] *)
Precedence`PREFIX`MINUSMINUS -> 660, (* Precedence[PreDecrement] *)
Precedence`POSTFIX`PLUSPLUS -> 660, (* Precedence[Increment] *)
Precedence`POSTFIX`MINUSMINUS -> 660, (* Precedence[Decrement] *)

Precedence`CALL -> 670, (* Precedence[f], Precedence[Sin], etc. *)

Precedence`INFIX`QUESTION -> 680, (* Precedence[PatternTest] *)

Precedence`LINEARSYNTAX`BANG -> Next,

Precedence`LESSLESS -> 720, (* Precedence[Get] *)

Precedence`COLONCOLON -> 750, (* Precedence[MessageName] *)

Precedence`HIGHEST -> Next,
Precedence`CONTEXT`SENSITIVE -> Precedence`HIGHEST,

Nothing
|>