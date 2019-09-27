BeginPackage["AST`Boxes`"]

Begin["`Private`"]

Needs["AST`"]


(*

ParseBox accepts boxes, which are concrete syntax

and returns concrete syntax

The semantics of RowBox are here

But no other box has semantics.
Do not want to reimplement MakeExpression.

*)

ConcreteParseBox[box_] :=
  parseBox[box, {}]


parseBox[RowBox[children_], pos_] :=
Catch[
Module[{handledChildren, aggregatedChildren},

  handledChildren = children;

	handledChildren = MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ #2]&, handledChildren];
  
  aggregatedChildren = DeleteCases[handledChildren, LeafNode[Token`Newline | Token`WhiteSpace, _, _] | GroupNode[Comment, _, _]];
  
  If[$Debug,
    Print["aggregatedChildren: ", aggregatedChildren]
  ];

  If[Length[aggregatedChildren] == 1,
    Throw[BoxNode[RowBox, {handledChildren}, <|Source->pos|>]]
  ];

  Switch[aggregatedChildren,

    (*
    Infix
    *)
    {_, LeafNode[Token`Plus, _, _], ___}, InfixNode[Plus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Times | Token`Star, _, _], ___}, InfixNode[Times, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Minus, _, _], ___}, InfixNode[Minus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`WhiteSpace, _, _], ___}, InfixNode[Times, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Bar, _, _], ___}, InfixNode[Alternatives, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LessEqual | Token`LongName`LessEqual | Token`Greater | 
       Token`Less | Token`LongName`Equal | 
       Token`LongName`GreaterEqual | Token`EqualEqual | 
       Token`LessGreater | Token`LongName`NotEqual |
       Token`GreaterEqual | Token`BangEqual, _, _], ___}, InfixNode[Inequality, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VectorGreater | Token`LongName`VectorGreaterEqual |
       Token`LongName`VectorLess | Token`LongName`VectorLessEqual , _, _], ___}, InfixNode[Developer`VectorInequality, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`And | Token`AmpAmp, _, _], ___}, InfixNode[And, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Or | Token`BarBar, _, _], ___}, InfixNode[Or, handledChildren, <|Source->Append[pos, 1]|>],
    
    {_, LeafNode[Token`Semi, _, _], ___}, InfixNode[CompoundExpression, handledChildren ~Join~
                                              If[MatchQ[handledChildren[[-1]], LeafNode[Token`Semi, _, _]],
                                                { LeafNode[Token`Fake`ImplicitNull, "", handledChildren[[-1, 3]]] },
                                                {}], <|Source->Append[pos, 1]|>],

    {_, LeafNode[Token`Comma, _, _], ___}, InfixNode[Comma, handledChildren ~Join~
                                              If[MatchQ[handledChildren[[-1]], LeafNode[Token`Comma, _, _]],
                                                { LeafNode[Token`Fake`ImplicitNull, "", handledChildren[[-1, 3]]] },
                                                {}], <|Source->Append[pos, 1]|>],

    {_, LeafNode[Token`LongName`Element, _, _], ___}, InfixNode[Element, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Distributed, _, _], ___}, InfixNode[Distributed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Dot, _, _], ___}, InfixNode[Dot, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`TildeTilde, _, _], ___}, InfixNode[StringExpression, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotElement, _, _], ___}, InfixNode[NotElement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`EqualBangEqual, _, _], ___}, InfixNode[UnsameQ, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`ColonColon, _, _], ___}, InfixNode[MessageName, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CenterDot, _, _], ___}, InfixNode[CenterDot, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotReverseElement, _, _], ___}, InfixNode[NotReverseElement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CircleTimes, _, _], ___}, InfixNode[CircleTimes, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Backslash, _, _], ___}, InfixNode[Backslash, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Nand, _, _], ___}, InfixNode[Nand, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Nor, _, _], ___}, InfixNode[Nor, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Cap, _, _], ___}, InfixNode[Cap, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CircleDot, _, _], ___}, InfixNode[CircleDot, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CircleMinus, _, _], ___}, InfixNode[CircleMinus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CirclePlus, _, _], ___}, InfixNode[CirclePlus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Colon, _, _], ___}, InfixNode[Colon, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`AtStar, _, _], ___}, InfixNode[Composition, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Conditioned, _, _], ___}, InfixNode[Conditioned, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Congruent, _, _], ___}, InfixNode[Congruent, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Coproduct, _, _], ___}, InfixNode[Coproduct, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Cross, _, _], ___}, InfixNode[Cross, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Cup, _, _], ___}, InfixNode[Cup, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CupCap, _, _], ___}, InfixNode[CupCap, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Diamond, _, _], ___}, InfixNode[Diamond, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DotEqual, _, _], ___}, InfixNode[DotEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleDownArrow, _, _], ___}, InfixNode[DoubleDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLeftArrow, _, _], ___}, InfixNode[DoubleLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLeftRightArrow, _, _], ___}, InfixNode[DoubleLeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLeftTee, _, _], ___}, InfixNode[DoubleLeftTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLongLeftArrow, _, _], ___}, InfixNode[DoubleLongLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLongLeftRightArrow, _, _], ___}, InfixNode[DoubleLongLeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLongRightArrow, _, _], ___}, InfixNode[DoubleLongRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleRightArrow, _, _], ___}, InfixNode[DoubleRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleRightTee, _, _], ___}, InfixNode[DoubleRightTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleUpArrow, _, _], ___}, InfixNode[DoubleUpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleUpDownArrow, _, _], ___}, InfixNode[DoubleUpDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleVerticalBar, _, _], ___}, InfixNode[DoubleVerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownArrow, _, _], ___}, InfixNode[DownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownArrowBar, _, _], ___}, InfixNode[DownArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownArrowUpArrow, _, _], ___}, InfixNode[DownArrowUpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftRightVector, _, _], ___}, InfixNode[DownLeftRightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftTeeVector, _, _], ___}, InfixNode[DownLeftTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftVector, _, _], ___}, InfixNode[DownLeftVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftVectorBar, _, _], ___}, InfixNode[DownLeftVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownRightTeeVector, _, _], ___}, InfixNode[DownRightTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownRightVector, _, _], ___}, InfixNode[DownRightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownRightVectorBar, _, _], ___}, InfixNode[DownRightVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownTee, _, _], ___}, InfixNode[DownTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownTeeArrow, _, _], ___}, InfixNode[DownTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`EqualTilde, _, _], ___}, InfixNode[EqualTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Equilibrium, _, _], ___}, InfixNode[Equilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Equivalent, _, _], ___}, InfixNode[Equivalent, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`GreaterEqualLess, _, _], ___}, InfixNode[GreaterEqualLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`GreaterFullEqual, _, _], ___}, InfixNode[GreaterFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`GreaterGreater, _, _], ___}, InfixNode[GreaterGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`GreaterLess, _, _], ___}, InfixNode[GreaterLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`GreaterTilde, _, _], ___}, InfixNode[GreaterTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`HumpDownHump, _, _], ___}, InfixNode[HumpDownHump, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`HumpEqual, _, _], ___}, InfixNode[HumpEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Implies, _, _], ___}, InfixNode[Implies, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Intersection, _, _], ___}, InfixNode[Intersection, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftArrow, _, _], ___}, InfixNode[LeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftArrowBar, _, _], ___}, InfixNode[LeftArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftArrowRightArrow, _, _], ___}, InfixNode[LeftArrowRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDownTeeVector, _, _], ___}, InfixNode[LeftDownTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDownVector, _, _], ___}, InfixNode[LeftDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDownVectorBar, _, _], ___}, InfixNode[LeftDownVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftRightArrow, _, _], ___}, InfixNode[LeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftRightVector, _, _], ___}, InfixNode[LeftRightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTee, _, _], ___}, InfixNode[LeftTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTeeArrow, _, _], ___}, InfixNode[LeftTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTeeVector, _, _], ___}, InfixNode[LeftTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTriangle, _, _], ___}, InfixNode[LeftTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTriangleBar, _, _], ___}, InfixNode[LeftTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTriangleEqual, _, _], ___}, InfixNode[LeftTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpDownVector, _, _], ___}, InfixNode[LeftUpDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpTeeVector, _, _], ___}, InfixNode[LeftUpTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpVector, _, _], ___}, InfixNode[LeftUpVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpVectorBar, _, _], ___}, InfixNode[LeftUpVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftVector, _, _], ___}, InfixNode[LeftVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftVectorBar, _, _], ___}, InfixNode[LeftVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LessEqualGreater, _, _], ___}, InfixNode[LessEqualGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LessFullEqual, _, _], ___}, InfixNode[LessFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LessGreater, _, _], ___}, InfixNode[LessGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LessLess, _, _], ___}, InfixNode[LessLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LessTilde, _, _], ___}, InfixNode[LessTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LongLeftArrow, _, _], ___}, InfixNode[LongLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LongLeftRightArrow, _, _], ___}, InfixNode[LongLeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LongRightArrow, _, _], ___}, InfixNode[LongRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LowerLeftArrow, _, _], ___}, InfixNode[LowerLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LowerRightArrow, _, _], ___}, InfixNode[LowerRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NestedGreaterGreater, _, _], ___}, InfixNode[NestedGreaterGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NestedLessLess, _, _], ___}, InfixNode[NestedLessLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`StarStar, _, _], ___}, InfixNode[NonCommutativeMultiply, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotCongruent, _, _], ___}, InfixNode[NotCongruent, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotCupCap, _, _], ___}, InfixNode[NotCupCap, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotDoubleVerticalBar, _, _], ___}, InfixNode[NotDoubleVerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotEqualTilde, _, _], ___}, InfixNode[NotEqualTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreater, _, _], ___}, InfixNode[NotGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreaterEqual, _, _], ___}, InfixNode[NotGreaterEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreaterFullEqual, _, _], ___}, InfixNode[NotGreaterFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreaterGreater, _, _], ___}, InfixNode[NotGreaterGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreaterLess, _, _], ___}, InfixNode[NotGreaterLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreaterSlantEqual, _, _], ___}, InfixNode[NotGreaterSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotGreaterTilde, _, _], ___}, InfixNode[NotGreaterTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotHumpDownHump, _, _], ___}, InfixNode[NotHumpDownHump, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotHumpEqual, _, _], ___}, InfixNode[NotHumpEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLeftTriangle, _, _], ___}, InfixNode[NotLeftTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLeftTriangleBar, _, _], ___}, InfixNode[NotLeftTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLeftTriangleEqual, _, _], ___}, InfixNode[NotLeftTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLess, _, _], ___}, InfixNode[NotLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLessEqual, _, _], ___}, InfixNode[NotLessEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLessFullEqual, _, _], ___}, InfixNode[NotLessFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLessGreater, _, _], ___}, InfixNode[NotLessGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLessLess, _, _], ___}, InfixNode[NotLessLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLessSlantEqual, _, _], ___}, InfixNode[NotLessSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLessTilde, _, _], ___}, InfixNode[NotLessTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotNestedGreaterGreater, _, _], ___}, InfixNode[NotNestedGreaterGreater, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotNestedLessLess, _, _], ___}, InfixNode[NotNestedLessLess, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedes, _, _], ___}, InfixNode[NotPrecedes, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedesEqual, _, _], ___}, InfixNode[NotPrecedesEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedesSlantEqual, _, _], ___}, InfixNode[NotPrecedesSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedesTilde, _, _], ___}, InfixNode[NotPrecedesTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotRightTriangle, _, _], ___}, InfixNode[NotRightTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotRightTriangleBar, _, _], ___}, InfixNode[NotRightTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotRightTriangleEqual, _, _], ___}, InfixNode[NotRightTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSubset, _, _], ___}, InfixNode[NotSquareSubset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSubsetEqual, _, _], ___}, InfixNode[NotSquareSubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSuperset, _, _], ___}, InfixNode[NotSquareSuperset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSupersetEqual, _, _], ___}, InfixNode[NotSquareSupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSubset, _, _], ___}, InfixNode[NotSubset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSubsetEqual, _, _], ___}, InfixNode[NotSubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceeds, _, _], ___}, InfixNode[NotSucceeds, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceedsEqual, _, _], ___}, InfixNode[NotSucceedsEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceedsSlantEqual, _, _], ___}, InfixNode[NotSucceedsSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceedsTilde, _, _], ___}, InfixNode[NotSucceedsTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSuperset, _, _], ___}, InfixNode[NotSuperset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSupersetEqual, _, _], ___}, InfixNode[NotSupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTilde, _, _], ___}, InfixNode[NotTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTildeEqual, _, _], ___}, InfixNode[NotTildeEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTildeFullEqual, _, _], ___}, InfixNode[NotTildeFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTildeTilde, _, _], ___}, InfixNode[NotTildeTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotVerticalBar, _, _], ___}, InfixNode[NotVerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Precedes, _, _], ___}, InfixNode[Precedes, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PrecedesEqual, _, _], ___}, InfixNode[PrecedesEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PrecedesSlantEqual, _, _], ___}, InfixNode[PrecedesSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PrecedesTilde, _, _], ___}, InfixNode[PrecedesTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Proportion, _, _], ___}, InfixNode[Proportion, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Proportional, _, _], ___}, InfixNode[Proportional, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ReverseElement, _, _], ___}, InfixNode[ReverseElement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ReverseEquilibrium, _, _], ___}, InfixNode[ReverseEquilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ReverseUpEquilibrium, _, _], ___}, InfixNode[ReverseUpEquilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightArrow, _, _], ___}, InfixNode[RightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightArrowBar, _, _], ___}, InfixNode[RightArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightArrowLeftArrow, _, _], ___}, InfixNode[RightArrowLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightDownTeeVector, _, _], ___}, InfixNode[RightDownTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightDownVector, _, _], ___}, InfixNode[RightDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightDownVectorBar, _, _], ___}, InfixNode[RightDownVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTee, _, _], ___}, InfixNode[RightTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTeeArrow, _, _], ___}, InfixNode[RightTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTeeVector, _, _], ___}, InfixNode[RightTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTriangle, _, _], ___}, InfixNode[RightTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTriangleBar, _, _], ___}, InfixNode[RightTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTriangleEqual, _, _], ___}, InfixNode[RightTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpDownVector, _, _], ___}, InfixNode[RightUpDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpTeeVector, _, _], ___}, InfixNode[RightUpTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpVector, _, _], ___}, InfixNode[RightUpVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpVectorBar, _, _], ___}, InfixNode[RightUpVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightVector, _, _], ___}, InfixNode[RightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightVectorBar, _, _], ___}, InfixNode[RightVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortDownArrow, _, _], ___}, InfixNode[ShortDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortLeftArrow, _, _], ___}, InfixNode[ShortLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortRightArrow, _, _], ___}, InfixNode[ShortRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortUpArrow, _, _], ___}, InfixNode[ShortUpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SmallCircle, _, _], ___}, InfixNode[SmallCircle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareIntersection, _, _], ___}, InfixNode[SquareIntersection, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSubset, _, _], ___}, InfixNode[SquareSubset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSubsetEqual, _, _], ___}, InfixNode[SquareSubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSuperset, _, _], ___}, InfixNode[SquareSuperset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSupersetEqual, _, _], ___}, InfixNode[SquareSupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareUnion, _, _], ___}, InfixNode[SquareUnion, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Star, _, _], ___}, InfixNode[Star, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Subset, _, _], ___}, InfixNode[Subset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SubsetEqual, _, _], ___}, InfixNode[SubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Succeeds, _, _], ___}, InfixNode[Succeeds, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SucceedsEqual, _, _], ___}, InfixNode[SucceedsEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SucceedsSlantEqual, _, _], ___}, InfixNode[SucceedsSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SucceedsTilde, _, _], ___}, InfixNode[SucceedsTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SuchThat, _, _], ___}, InfixNode[SuchThat, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Superset, _, _], ___}, InfixNode[Superset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SupersetEqual, _, _], ___}, InfixNode[SupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TensorProduct, _, _], ___}, InfixNode[TensorProduct, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TensorWedge, _, _], ___}, InfixNode[TensorWedge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Therefore, _, _], ___}, InfixNode[Therefore, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Tilde, _, _], ___}, InfixNode[Tilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TildeEqual, _, _], ___}, InfixNode[TildeEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TildeFullEqual, _, _], ___}, InfixNode[TildeFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TildeTilde, _, _], ___}, InfixNode[TildeTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Union, _, _], ___}, InfixNode[Union, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UnionPlus, _, _], ___}, InfixNode[UnionPlus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpArrow, _, _], ___}, InfixNode[UpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpArrowBar, _, _], ___}, InfixNode[UpArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpArrowDownArrow, _, _], ___}, InfixNode[UpArrowDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpDownArrow, _, _], ___}, InfixNode[UpDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpEquilibrium, _, _], ___}, InfixNode[UpEquilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpperLeftArrow, _, _], ___}, InfixNode[UpperLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpperRightArrow, _, _], ___}, InfixNode[UpperRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpTee, _, _], ___}, InfixNode[UpTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpTeeArrow, _, _], ___}, InfixNode[UpTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VectorGreater, _, _], ___}, InfixNode[UpperRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Vee, _, _], ___}, InfixNode[Vee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VerticalBar, _, _], ___}, InfixNode[VerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VerticalSeparator, _, _], ___}, InfixNode[VerticalSeparator, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VerticalTilde, _, _], ___}, InfixNode[VerticalTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Wedge, _, _], ___}, InfixNode[Wedge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Xnor, _, _], ___}, InfixNode[Xnor, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Xor, _, _], ___}, InfixNode[Xor, handledChildren, <|Source->Append[pos, 1]|>],


    (*
    Binary
    *)
    {_, LeafNode[Token`MinusGreater | Token`LongName`Rule, _, _], ___}, BinaryNode[Rule, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`ColonGreater | Token`LongName`RuleDelayed, _, _], ___}, BinaryNode[RuleDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSlash, _, _], ___}, BinaryNode[BinarySlashSlash, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UndirectedEdge, _, _], ___}, BinaryNode[UndirectedEdge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DirectedEdge, _, _], ___}, BinaryNode[DirectedEdge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Caret, _, _], ___}, BinaryNode[Power, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`ColonEqual, _, _], ___}, BinaryNode[SetDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`CaretColonEqual, _, _], ___}, BinaryNode[UpSetDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`EqualDot, _, _]}, BinaryNode[Unset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Equal, _, _], ___}, BinaryNode[Set, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Slash, _, _], ___}, BinaryNode[Divide, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashAt, _, _], ___}, BinaryNode[Map, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`At, _, _], ___}, BinaryNode[BinaryAt, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashDot, _, _], ___}, BinaryNode[ReplaceAll, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`AtAt, _, _], ___}, BinaryNode[Apply, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Question, _, _], ___}, BinaryNode[PatternTest, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Function, _, _], ___}, BinaryNode[Function, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`EqualEqualEqual, _, _], ___}, BinaryNode[SameQ, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSemi, _, _], ___}, BinaryNode[Condition, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SemiSemi, _, _], ___}, BinaryNode[Span, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`AtAtAt, _, _], ___}, BinaryNode[BinaryAtAtAt, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSlashDot, _, _], ___}, BinaryNode[ReplaceRepeated, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`PlusEqual, _, _], ___}, BinaryNode[AddTo, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Because, _, _], ___}, BinaryNode[Because, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Divide, _, _], ___}, BinaryNode[Divide, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashEqual, _, _], ___}, BinaryNode[DivideBy, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSlashAt, _, _], ___}, BinaryNode[MapAll, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`MinusPlus, _, _], ___}, BinaryNode[MinusPlus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PlusMinus, _, _], ___}, BinaryNode[PlusMinus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`GreaterGreater, _, _], ___}, BinaryNode[Put, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`GreaterGreaterGreater, _, _], ___}, BinaryNode[PutAppend, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashStar, _, _], ___}, BinaryNode[RightComposition, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`MinusEqual, _, _], ___}, BinaryNode[SubtractFrom, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`StarEqual, _, _], ___}, BinaryNode[TimesBy, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LessMinusGreater, _, _], ___}, BinaryNode[UndirectedEdge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`CaretEqual, _, _], ___}, BinaryNode[UpSet, handledChildren, <|Source->Append[pos, 1]|>],

    {LeafNode[Symbol, _, _], LeafNode[Token`Colon, _, _], ___}, BinaryNode[Pattern, handledChildren, <|Source->Append[pos, 1]|>],
    {PatternBlankNode[_, _, _], LeafNode[Token`Colon, _, _], ___}, BinaryNode[Optional, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Colon, _, _], ___}, SyntaxErrorNode[SyntaxError`ColonError, handledChildren, <|Source -> Append[pos, 1]|>],

    (*
    Ternary
    *)
    {_, LeafNode[Token`SemiSemi, _, _], _, LeafNode[Token`SemiSemi, _, _], _}, TernaryNode[Span, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`ColonEqual, _, _], _}, TernaryNode[TagSetDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], _}, TernaryNode[TagSet, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`EqualDot, _, _], _}, TernaryNode[TagUnset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _}, SyntaxErrorNode[SyntaxError`ExpectedSet, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Tilde, _, _], _, LeafNode[Token`Tilde, _, _], _}, TernaryNode[TernaryTilde, handledChildren, <|Source->Append[pos, 1]|>],
    
    (*
    Prefix
    *)
    {LeafNode[Token`Minus, _, _], ___}, PrefixNode[Minus, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`Integral, _, _], LeafNode[Token`LongName`DifferentialD, _, _]}, PrefixNode[Integral, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LessLess, _, _], ___}, PrefixNode[Get, handledChildren, <|Source->Append[pos, 1]|>],

    (*
    StartOfLine
    *)
    {LeafNode[Token`Question, _, _], ___}, StartOfLineNode[Information, handledChildren, <|Source->Append[pos, 1]|>],
    
    (*
    Postfix
    *)
    {_, LeafNode[Token`Amp, _, _]}, PostfixNode[Function, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`PlusPlus, _, _]}, PostfixNode[Increment, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Bang, _, _]}, PostfixNode[Factorial, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SingleQuote, _, _]}, PostfixNode[Derivative, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Boxes`MultiSingleQuote, _, _]}, PostfixNode[Derivative, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Transpose, _, _]}, PostfixNode[Transpose, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`DotDot, _, _]}, PostfixNode[Repeated, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Conjugate, _, _]}, PostfixNode[Conjugate, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ConjugateTranspose, _, _]}, PostfixNode[ConjugateTranspose, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`MinusMinus, _, _]}, PostfixNode[Decrement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`DotDotDot, _, _]}, PostfixNode[RepeatedNull, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`BangBang, _, _]}, PostfixNode[Factorial2, handledChildren, <|Source->Append[pos, 1]|>],

    (*
    Calls
    *)
    {_, LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, CallNode[{handledChildren[[1]]}, {GroupNode[GroupSquare, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDoubleBracket, _, _], ___, LeafNode[Token`LongName`RightDoubleBracket, _, _]}, CallNode[{handledChildren[[1]]}, {GroupNode[GroupDoubleBracket, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>],
    
    (*
    Groups
    *)
    {LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, GroupNode[GroupSquare, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`OpenCurly, _, _], ___, LeafNode[Token`CloseCurly, _, _]}, GroupNode[List, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LessBar, _, _], ___, LeafNode[Token`BarGreater, _, _]}, GroupNode[Association, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`OpenParen, _, _], ___, LeafNode[Token`CloseParen, _, _]}, GroupNode[GroupParen, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftAssociation, _, _], ___, LeafNode[Token`LongName`RightAssociation, _, _]}, GroupNode[Association, handledChildren, <|Source->Append[pos, 1]|>],
    
    (*
    Treat comments like groups
    *)
    {LeafNode[Token`Boxes`OpenParenStar, _, _], ___, LeafNode[Token`Boxes`StarCloseParen, _, _]}, GroupNode[Comment, handledChildren, <|Source->Append[pos, 1]|>],

    {___, LeafNode[Token`CloseCurly, _, _]}, SyntaxErrorNode[SyntaxError`ExpectedPossibleExpression, handledChildren, <|Source -> Append[pos, 1]|>],

    (*
    the second arg is a box, so we know it is implicit Times
    *)
    {_, LeafNode[Symbol | Integer | Slot | String | Real | Out, _, _] |
        BinaryNode[_, _, _] | CallNode[_, _, _] | GroupNode[_, _, _] | BoxNode[_, _, _] | InfixNode[_, _, _] |
        PostfixNode[_, _, _] | PatternBlankNode[_, _, _], ___},
        InfixNode[Times, Riffle[handledChildren, LeafNode[Token`Fake`ImplicitTimes, "", <||>]], <|Source->Append[pos, 1]|>],

    (*
    if there is an error, then just return the last non-trivia node
    *)
    {_, LeafNode[Token`Error`UnhandledCharacter, _, _], ___},
        aggregatedChildren[[-1]],
    _,
    Failure["InternalUnhandled", <|"Function"->parseBox, "Arguments"->HoldForm[RowBox[children]]|>]
    ]
   ]]






parseBox[{args___}, pos_] :=
  Module[{children},
    children = MapIndexed[parseBox[#1, pos ~Join~ #2]&, {args}];
    BoxNode[List, children, <|Source->pos|>]
  ]



Attributes[applyCodeNodesToRest] = {HoldAll}

applyCodeNodesToRest[rest___] := List @@ Map[Function[arg, With[{assoc = <||>}, CodeNode[Null, arg, assoc]], {HoldFirst}], Hold[rest]]



parseBox[SubscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[SubscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SuperscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[SuperscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SubsuperscriptBox[a_, b_, c_, rest___], pos_] :=
  BoxNode[SubsuperscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]], parseBox[c, Append[pos, 3]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[UnderoverscriptBox[a_, b_, c_, rest___], pos_] :=
  BoxNode[UnderoverscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]], parseBox[c, Append[pos, 3]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[FractionBox[a_, b_, rest___], pos_] :=
  BoxNode[FractionBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[OverscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[OverscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[UnderscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[UnderscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SqrtBox[a_, rest___], pos_] :=
  BoxNode[SqrtBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RadicalBox[a_, b_, rest___], pos_] :=
  BoxNode[RadicalBox, {parseBox[a, Append[pos, 1]]} ~Join~ {parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TooltipBox[a_, b_, rest___], pos_] :=
  BoxNode[TooltipBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]



parseBox[TagBox[rest___], pos_] :=
  BoxNode[TagBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicBox[rest___], pos_] :=
  BoxNode[DynamicBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicModuleBox[first_, a_, rest___], pos_] :=
  BoxNode[DynamicModuleBox, applyCodeNodesToRest[first] ~Join~ {parseBox[a, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[NamespaceBox[first_, a_, rest___], pos_] :=
  BoxNode[NamespaceBox, applyCodeNodesToRest[first] ~Join~ {parseBox[a, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

(*
a may be a List

GraphicsBox is HoldAll, so cannot recurse into child boxes without evaling

parseBox[GraphicsBox[a_, rest___], pos_] :=
  BoxNode[GraphicsBox, {parseBoxPossibleListPossibleDirective[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]
*)
parseBox[GraphicsBox[rest___], pos_] :=
  BoxNode[GraphicsBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Graphics3DBox[rest___], pos_] :=
  BoxNode[Graphics3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsComplexBox[rest___], pos_] :=
  BoxNode[GraphicsComplexBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsComplex3DBox[rest___], pos_] :=
  BoxNode[GraphicsComplex3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsGroupBox[rest___], pos_] :=
  BoxNode[GraphicsGroupBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsGroup3DBox[rest___], pos_] :=
  BoxNode[GraphicsGroup3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DiskBox[rest___], pos_] :=
  BoxNode[DiskBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[LineBox[rest___], pos_] :=
  BoxNode[LineBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Line3DBox[rest___], pos_] :=
  BoxNode[Line3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RectangleBox[rest___], pos_] :=
  BoxNode[RectangleBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PointBox[rest___], pos_] :=
  BoxNode[PointBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Point3DBox[rest___], pos_] :=
  BoxNode[Point3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[CuboidBox[rest___], pos_] :=
  BoxNode[CuboidBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Polygon3DBox[rest___], pos_] :=
  BoxNode[Polygon3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SphereBox[rest___], pos_] :=
  BoxNode[SphereBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RotationBox[a_, rest___], pos_] :=
  BoxNode[RotationBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BSplineCurveBox[rest___], pos_] :=
  BoxNode[BSplineCurveBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BSplineCurve3DBox[rest___], pos_] :=
  BoxNode[BSplineCurve3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BSplineSurface3DBox[rest___], pos_] :=
  BoxNode[BSplineSurface3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PolygonBox[rest___], pos_] :=
  BoxNode[PolygonBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ConicHullRegion3DBox[rest___], pos_] :=
  BoxNode[ConicHullRegion3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TubeBox[rest___], pos_] :=
  BoxNode[TubeBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Arrow3DBox[rest___], pos_] :=
  BoxNode[Arrow3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GeometricTransformation3DBox[rest___], pos_] :=
  BoxNode[GeometricTransformation3DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

(*
InterpretationBox is HoldAllComplete
*)
parseBox[InterpretationBox[rest___], pos_] :=
  BoxNode[InterpretationBox, applyCodeNodesToRest[rest], <|Source->pos|>]

(*
too complicated to handle first arg as boxes

For example:
TemplateBox[{"\"https://www.wolframcloud.com/objects/user-b0c28e9f-876d-4478-9d8b-9e7d18a9ea81/nameFormatExample.wl\"", 
  "https://www.wolframcloud.com/objects/user-b0c28e9f-876d-4478-9d8b-9e7d18a9ea81/nameFormatExample.wl"}, "HyperlinkURL"]

The second arg is a link, but it's not like we can do ParseLeaf["https://www.wolframcloud.com/objects/user-b0c28e9f-876d-4478-9d8b-9e7d18a9ea81/nameFormatExample.wl"]

*)
parseBox[TemplateBox[rest___], pos_] :=
  BoxNode[TemplateBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[StyleBox[a_, rest___], pos_] :=
  BoxNode[StyleBox, {parseBoxPossibleListPossibleDirective[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[FormBox[a_, rest___], pos_] :=
  BoxNode[FormBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RasterBox[rest___], pos_] :=
  BoxNode[RasterBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ButtonBox[a_, rest___], pos_] :=
  BoxNode[ButtonBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PaneSelectorBox[rest___], pos_] :=
  BoxNode[PaneSelectorBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PanelBox[a_, rest___], pos_] :=
  BoxNode[PanelBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ActionMenuBox[a_, rest___], pos_] :=
  BoxNode[ActionMenuBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

(*
a is a List of Lists
*)
parseBox[GridBox[a_, rest___], pos_] :=
  BoxNode[GridBox, {parseBoxPossibleList[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ItemBox[a_, rest___], pos_] :=
  BoxNode[ItemBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[InsetBox[a_, rest___], pos_] :=
  BoxNode[InsetBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[AdjustmentBox[a_, rest___], pos_] :=
  BoxNode[AdjustmentBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[LocatorPaneBox[first_, a_, rest___], pos_] :=
  BoxNode[LocatorPaneBox, applyCodeNodesToRest[first] ~Join~ {parseBox[a, Append[pos, 2]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[AnimatorBox[rest___], pos_] :=
  BoxNode[AnimatorBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[OpenerBox[rest___], pos_] :=
  BoxNode[OpenerBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SliderBox[rest___], pos_] :=
  BoxNode[SliderBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[CylinderBox[rest___], pos_] :=
  BoxNode[CylinderBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[FrameBox[a_, rest___], pos_] :=
  BoxNode[FrameBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[CheckboxBox[rest___], pos_] :=
  BoxNode[CheckboxBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ColorSetterBox[rest___], pos_] :=
  BoxNode[ColorSetterBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[InputFieldBox[rest___], pos_] :=
  BoxNode[InputFieldBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TabViewBox[rest___], pos_] :=
  BoxNode[TabViewBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RadioButtonBox[rest___], pos_] :=
  BoxNode[RadioButtonBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PopupMenuBox[rest___], pos_] :=
  BoxNode[PopupMenuBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SetterBox[rest___], pos_] :=
  BoxNode[SetterBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Slider2DBox[rest___], pos_] :=
  BoxNode[Slider2DBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicWrapperBox[a_, rest___], pos_] :=
  BoxNode[DynamicWrapperBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PaneBox[a_, rest___], pos_] :=
  BoxNode[PaneBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ListPickerBox[rest___], pos_] :=
  BoxNode[ListPickerBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[OverlayBox[rest___], pos_] :=
  BoxNode[OverlayBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ProgressIndicatorBox[rest___], pos_] :=
  BoxNode[ProgressIndicatorBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TogglerBox[rest___], pos_] :=
  BoxNode[TogglerBox, applyCodeNodesToRest[rest], <|Source->pos|>]






parseBoxPossibleList[l_List, pos_] := MapIndexed[parseBoxPossibleList[#, pos ~Join~ #2]&, l]

parseBoxPossibleList[box_, pos_] := parseBox[box, pos]






parseBoxPossibleListPossibleDirective[l_List, pos_] := MapIndexed[parseBoxPossibleListPossibleDirective[#, pos ~Join~ #2]&, l]

parseBoxPossibleListPossibleDirective[GrayLevel[l_], pos_] := DirectiveNode[GrayLevel, {l}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[AbsoluteThickness[t_], pos_] := DirectiveNode[AbsoluteThickness, {t}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[Opacity[o_], pos_] := DirectiveNode[Opacity, {o}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[RGBColor[r_, g_, b_], pos_] := DirectiveNode[RGBColor, {r, g, b}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[EdgeForm[f_], pos_] := DirectiveNode[EdgeForm, {f}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[PointSize[s_], pos_] := DirectiveNode[PointSize, {s}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[CapForm[f_], pos_] := DirectiveNode[CapForm, {f}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[Thickness[t_], pos_] := DirectiveNode[Thickness, {t}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[JoinForm[f_], pos_] := DirectiveNode[JoinForm, {f}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[FaceForm[f_], pos_] := DirectiveNode[FaceForm, {f}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[Specularity[args___], pos_] := DirectiveNode[Specularity, {args}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[AbsoluteDashing[args___], pos_] := DirectiveNode[AbsoluteDashing, {args}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[AbsolutePointSize[args___], pos_] := DirectiveNode[AbsolutePointSize, {args}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[Arrowheads[args___], pos_] := DirectiveNode[Arrowheads, {args}, <|Source->pos|>]

parseBoxPossibleListPossibleDirective[box_, pos_] := parseBox[box, pos]




(*
a_b

Front End treats a_b as single token
*)
parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "_"], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "_" :> {a, "_"}][[1]];
  PatternBlankNode[PatternBlank, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "__"], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "__" :> {a, "__"}][[1]];
  PatternBlankSequenceNode[PatternBlankSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "___"], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "___" :> {a, "___"}][[1]];
  PatternBlankNullSequenceNode[PatternBlankNullSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "_."], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "_." :> {a, "_."}][[1]];
  OptionalDefaultPatternNode[OptionalDefaultPattern, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]


parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "_" ~~ Except["_"|"\""]..], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "_" ~~ b:Except["_"|"\""].. :> {a, "_", b}][[1]];
  PatternBlankNode[PatternBlank, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "__" ~~ Except["_"|"\""]..], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "__" ~~ b:Except["_"|"\""].. :> {a, "__", b}][[1]];
  PatternBlankSequenceNode[PatternBlankSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, Except["_"|"\""].. ~~ "___" ~~ Except["_"|"\""]..], pos_] :=
Module[{cases},
  cases = StringCases[str, a:Except["_"|"\""].. ~~ "___" ~~ b:Except["_"|"\""].. :> {a, "___", b}][[1]];
  PatternBlankNullSequenceNode[PatternBlankNullSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]


parseBox[str_String /; StringMatchQ[str, "_" ~~ Except["_"|"\""]..], pos_] :=
Module[{cases},
  cases = StringCases[str, "_" ~~ b:Except["_"|"\""].. :> {"_", b}][[1]];
  BlankNode[Blank, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, "__" ~~ Except["_"|"\""]..], pos_] :=
Module[{cases},
  cases = StringCases[str, "__" ~~ b:Except["_"|"\""].. :> {"__", b}][[1]];
  BlankSequenceNode[BlankSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]

parseBox[str_String /; StringMatchQ[str, "___" ~~ Except["_"|"\""]..], pos_] :=
Module[{cases},
  cases = StringCases[str, "___" ~~ b:Except["_"|"\""].. :> {"___", b}][[1]];
  BlankNullSequenceNode[BlankNullSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
]



(*
The Front End treats comments as a collection of code, and not a single token
*)
parseBox["(*", pos_] := LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> pos|>]
parseBox["*)", pos_] := LeafNode[Token`Boxes`StarCloseParen, "*)", <|Source -> pos|>]



(*
The Front End treats ''' as a single token
*)
parseBox[s_String /; StringMatchQ[s, "'"~~___], pos_] := LeafNode[Token`Boxes`MultiSingleQuote, s, <|Source -> pos|>]


parseBox[str_String, pos_] :=
Module[{parsed, data, issues},
  parsed = ParseLeaf[str, "SourceStyle"->"Null"];

  (*
    Source is filled in here
  *)
  data = parsed[[3]];
  data[Source] = pos;

  issues = data[SyntaxIssues];
  issues = SyntaxIssue[#[[1]], #[[2]], #[[3]], <| #[[4]], Source->pos |>]& /@ issues;
  data[SyntaxIssues] = issues;

  parsed[[3]] = data;
  parsed
]

parseBox[args___] := Failure["InternalUnhandled", <|"Function"->parseBox, "Arguments"->HoldForm[{args}]|>]



















ToStandardFormBoxes[cst_] :=
Block[{$RecursionLimit = Infinity},
Module[{implicitsRemoved},

  implicitsRemoved = DeleteCases[cst, LeafNode[Token`Fake`ImplicitTimes, _, _], Infinity];

  toStandardFormBoxes[implicitsRemoved]
]]





toStandardFormBoxes[LeafNode[_, str_, _]] :=
  str





(*
BoxNodes that may contain CodeNodes have to be handled individually
*)

toStandardFormBoxes[BoxNode[DynamicBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[DynamicModuleBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, Hold]& /@ {first};
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  (*
  Need to wrap boxes in Hold, so that ReleaseHold does not descend into the boxes (which may contain code that has Hold)
  *)
  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicModuleBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[NamespaceBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, Hold]& /@ {first};
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  (*
  Need to wrap boxes in Hold, so that ReleaseHold does not descend into the boxes (which may contain code that has Hold)
  *)
  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[NamespaceBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[RasterBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RasterBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[TagBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TagBox @@ heldChildren]]
]]


toStandardFormBoxes[BoxNode[GraphicsBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Graphics3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Graphics3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsComplexBox, {rest___}, _]] :=
Catch[
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, Hold]& /@ {first};
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsComplexBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsComplex3DBox, {rest___}, _]] :=
Catch[
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, Hold]& /@ {first};
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsComplex3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsGroupBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsGroupBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsGroup3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsGroup3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[DiskBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DiskBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[LineBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[LineBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Line3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Line3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RectangleBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RectangleBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PointBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PointBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Point3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Point3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CuboidBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CuboidBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Polygon3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Polygon3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SphereBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SphereBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RotationBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[RotationBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineCurveBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineCurveBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineCurve3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineCurve3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineSurface3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineSurface3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PolygonBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PolygonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ConicHullRegion3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ConicHullRegion3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TubeBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TubeBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Arrow3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Arrow3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GeometricTransformation3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GeometricTransformation3DBox @@ heldChildren]]
]]

(*
too complicated to handle first arg as boxes
*)
toStandardFormBoxes[BoxNode[TemplateBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TemplateBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[FormBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[FormBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ButtonBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ButtonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ActionMenuBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ActionMenuBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PaneSelectorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PaneSelectorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PanelBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[PanelBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InterpretationBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[InterpretationBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[StyleBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[StyleBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SuperscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { Hold[aBox], Hold[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SuperscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SubsuperscriptBox, {a_, b_, c_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b], cBox = toStandardFormBoxes[c]},
    heldChildren = { Hold[aBox], Hold[bBox], Hold[cBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SubsuperscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ItemBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ItemBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InsetBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[InsetBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[AdjustmentBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[AdjustmentBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[LocatorPaneBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, Hold]& /@ {first};
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  (*
  Need to wrap boxes in Hold, so that ReleaseHold does not descend into the boxes (which may contain code that has Hold)
  *)
  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[LocatorPaneBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[AnimatorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[AnimatorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OpenerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[OpenerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SliderBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SliderBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CylinderBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CylinderBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OverscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { Hold[aBox], Hold[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[OverscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[UnderscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { Hold[aBox], Hold[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[UnderscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[FrameBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[FrameBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CheckboxBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CheckboxBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ColorSetterBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ColorSetterBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InputFieldBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[InputFieldBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TabViewBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TabViewBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RadioButtonBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RadioButtonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PopupMenuBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PopupMenuBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SetterBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SetterBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Slider2DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Slider2DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[DynamicWrapperBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicWrapperBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PaneBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[PaneBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SqrtBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]},
    heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SqrtBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RadicalBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]},
    heldChildren = { Hold[aBox], Hold[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[RadicalBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ListPickerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ListPickerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OverlayBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[OverlayBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ProgressIndicatorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ProgressIndicatorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TogglerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TogglerBox @@ heldChildren]]
]]

(*
a is a List of Lists
*)
toStandardFormBoxes[BoxNode[GridBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, Hold]& /@ {rest};

  With[{aBox = Map[toStandardFormBoxes, a, {2}]}, heldChildren = { Hold[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[GridBox @@ heldChildren]]
]]

(*
a is a List of boxes
*)
toStandardFormBoxes[BoxNode[RowBox, {a_}, _]] :=
Catch[
Module[{aBox, boxes},

  aBox = toStandardFormBoxes /@ a;

  boxes = {aBox};

  RowBox @@ boxes
]]

(*
For BoxNodes that do not contain CodeNodes ( SqrtBox, FractionBox, etc )
*)
toStandardFormBoxes[BoxNode[box_, children_, _]] :=
Catch[
Module[{nodeBoxes},

  nodeBoxes = toStandardFormBoxes /@ children;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];

  box @@ nodeBoxes
]]



toStandardFormBoxes[CodeNode[Null, code_, data_]] :=
  Failure["CannotConvertToStandardFormBoxes", <|"Node"->CodeNode[Null, code, data]|>]


toStandardFormBoxes[l_List] := Map[toStandardFormBoxes, l]

toStandardFormBoxes[DirectiveNode[dir_, children_, _]] :=
  dir @@ children





toStandardFormBoxes[PrefixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[BinaryNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]





toStandardFormBoxes[InfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]



toStandardFormBoxes[TernaryNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


toStandardFormBoxes[PostfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[CallNode[op_, { node_ }, data_]] :=
Catch[
Module[{opBoxes, nodeBox},
  opBoxes = toStandardFormBoxes /@ op;
  If[AnyTrue[opBoxes, FailureQ],
    Throw[SelectFirst[opBoxes, FailureQ]]
  ];
  nodeBox = toStandardFormBoxes[node];
  If[FailureQ[nodeBox],
    Throw[nodeBox]
  ];
  RowBox[opBoxes ~Join~ nodeBox[[1]]]
]]

toStandardFormBoxes[GroupNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[PrefixBinaryNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]




toStandardFormBoxes[StartOfLineNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]





toStandardFormBoxes[BlankNode[Blank, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

toStandardFormBoxes[BlankSequenceNode[BlankSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

toStandardFormBoxes[BlankNullSequenceNode[BlankNullSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a_ token
*)
toStandardFormBoxes[PatternBlankNode[PatternBlank, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a__ token
*)
toStandardFormBoxes[PatternBlankSequenceNode[PatternBlankSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a___ token
*)
toStandardFormBoxes[PatternBlankNullSequenceNode[PatternBlankNullSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a_. token
*)
toStandardFormBoxes[OptionalDefaultPatternNode[OptionalDefaultPattern, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]






toStandardFormBoxes[SyntaxErrorNode[tag_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[GroupMissingCloserNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[GroupMissingOpenerNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]




toStandardFormBoxes[FileNode[File, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[Riffle[nodeBoxes, "\n"]]
]]





toStandardFormBoxes[HoldNode[Hold, nodes_, data_]] :=
Module[{processed},
  
  processed = Riffle[nodes, LeafNode[Token`Comma, ",", <||>]];

  toStandardFormBoxes[CallNode[LeafNode[Symbol, "Hold", <||>], {
                GroupNode[GroupSquare, {
                  LeafNode[Token`OpenSquare, "[", <||>] } ~Join~
                  { InfixNode[Comma, processed, <||>] } ~Join~
                  { LeafNode[Token`CloseSquare, "]", <||>] }, <||>] }, <||> ]]
]


(*
ConcreteParseString[""] returns Null, so handle that
*)
toStandardFormBoxes[Null] := ""




toStandardFormBoxes[f_Failure] := f

toStandardFormBoxes[args___] := Failure["InternalUnhandled", <|"Function"->ToStandardFormBoxes, "Arguments"->HoldForm[{args}]|>]














End[]

EndPackage[]
