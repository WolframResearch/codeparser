BeginPackage["CodeParser`Boxes`"]

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

(*

ParseBox accepts boxes, which are concrete syntax

and returns concrete syntax

The semantics of RowBox are here

But no other box has semantics.
Do not want to reimplement MakeExpression.

*)

CodeConcreteParseBox[boxs_List] :=
Catch[
Module[{children},

  children = MapIndexed[parseBox[#1, {} ~Join~ #2]&, boxs];

  If[AnyTrue[children, FailureQ],
    Throw[SelectFirst[children, FailureQ]]
  ];

  ContainerNode[Box, children, <||>]
]]

CodeConcreteParseBox[box_] :=
Catch[
Module[{children},

  children = {parseBox[box, {}]};

  If[AnyTrue[children, FailureQ],
    Throw[SelectFirst[children, FailureQ]]
  ];

  ContainerNode[Box, children, <||>]
]]



Options[parseBox] = {
  (*
  0: normal
  1: symbol segment or a quoted string (RHS of :: or #)
  2: file
  3: passthrough
  *)
  "StringifyMode" -> 0
}


(*
This is reached from within 
*)

parseBox[Cell[d:BoxData[_], rest___], pos_] :=
  BoxNode[Cell, {parseBox[d, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BoxData[a_], pos_] :=
  BoxNode[BoxData, {parseBox[a, Append[pos, 1]]}, <|Source->pos|>]




parseBox[ErrorBox[a_], pos_] :=
  BoxNode[ErrorBox, {parseBox[a, Append[pos, 1]]}, <|Source->pos|>]




parseBox[RowBox[children_], pos_] :=
Catch[
Module[{handledChildren, aggregatedChildren},

  handledChildren = children;

  handledChildren = MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ #2]&, handledChildren];
  
  aggregatedChildren = DeleteCases[handledChildren, LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _]];

  If[Length[aggregatedChildren] == 1,
    Throw[BoxNode[RowBox, {handledChildren}, <|Source->pos|>]]
  ];

  Switch[aggregatedChildren,

    (*
    Calls

    Must be before ] is handled as GroupMissingOpenerNode
    *)
    {_, LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, CallNode[{handledChildren[[1]]}, {GroupNode[GroupSquare, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDoubleBracket, _, _], ___, LeafNode[Token`LongName`RightDoubleBracket, _, _]}, CallNode[{handledChildren[[1]]}, {GroupNode[GroupDoubleBracket, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`OpenSquare, _, _], ___}, CallNode[{handledChildren[[1]]}, {GroupMissingCloserNode[GroupSquare, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>],

    (*
    Unrecognized LongName

    Must be before ] is handled as GroupMissingOpenerNode
    *)
    {ErrorNode[Token`Error`UnhandledCharacter, "\\[", _], _, LeafNode[Token`CloseSquare, "]", _]}, parseBox["\\[" <> children[[2]] <> "]", Append[pos, 1]], 

    (*
    We want to have Groups before Infix, so that { + } is first handled by Groups, and then Infix
    *)

    (*
    Groups
    *)
    {LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, GroupNode[GroupSquare, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`OpenCurly, _, _], ___, LeafNode[Token`CloseCurly, _, _]}, GroupNode[List, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LessBar, _, _], ___, LeafNode[Token`BarGreater, _, _]}, GroupNode[Association, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`OpenParen, _, _], ___, LeafNode[Token`CloseParen, _, _]}, GroupNode[GroupParen, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftAssociation, _, _], ___, LeafNode[Token`LongName`RightAssociation, _, _]}, GroupNode[Association, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftAngleBracket, _, _], ___, LeafNode[Token`LongName`RightAngleBracket, _, _]}, GroupNode[AngleBracket, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftBracketingBar, _, _], ___, LeafNode[Token`LongName`RightBracketingBar, _, _]}, GroupNode[BracketingBar, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftDoubleBracketingBar, _, _], ___, LeafNode[Token`LongName`RightDoubleBracketingBar, _, _]}, GroupNode[DoubleBracketingBar, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftCeiling, _, _], ___, LeafNode[Token`LongName`RightCeiling, _, _]}, GroupNode[Ceiling, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftFloor, _, _], ___, LeafNode[Token`LongName`RightFloor, _, _]}, GroupNode[Floor, handledChildren, <|Source->Append[pos, 1]|>],

    (*
    Treat comments like groups
    *)
    {LeafNode[Token`Boxes`OpenParenStar, _, _], ___, LeafNode[Token`Boxes`StarCloseParen, _, _]},
      GroupNode[Comment,
        {parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
        MapIndexed[
          parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), "StringifyMode" -> 3]&, children[[2;;-2]]] ~Join~
        {parseBox[children[[-1]], Append[pos, 1] ~Join~ {Length[children]}]}, <|Source->Append[pos, 1]|>],

    (*
    Unexpected openers and unexpected closers

    The structure of unexpected closers in boxes is different than in strings

    Unexpected closers in boxes never mate with an opener

    So retain the box structure here

    GroupMissingOpenerNode is only used in Boxes
    *)
    {LeafNode[Token`OpenSquare, _, _], ___}, GroupMissingCloserNode[GroupSquare, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`CloseSquare, _, _]}, GroupMissingOpenerNode[GroupSquare, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`OpenCurly, _, _], ___}, GroupMissingCloserNode[List, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`CloseCurly, _, _]}, GroupMissingOpenerNode[List, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LessBar, _, _], ___}, GroupMissingCloserNode[Association, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`BarGreater, _, _]}, GroupMissingOpenerNode[Association, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`OpenParen, _, _], ___}, GroupMissingCloserNode[GroupParen, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`CloseParen, _, _]}, GroupMissingOpenerNode[GroupParen, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftAssociation, _, _], ___}, GroupMissingCloserNode[Association, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`LongName`RightAssociation, _, _]}, GroupMissingOpenerNode[Association, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftAngleBracket, _, _], ___}, GroupMissingCloserNode[AngleBracket, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`LongName`RightAngleBracket, _, _]}, GroupMissingOpenerNode[AngleBracket, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftBracketingBar, _, _], ___}, GroupMissingCloserNode[BracketingBar, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`LongName`RightBracketingBar, _, _]}, GroupMissingOpenerNode[BracketingBar, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftDoubleBracketingBar, _, _], ___}, GroupMissingCloserNode[DoubleBracketingBar, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`LongName`RightDoubleBracketingBar, _, _]}, GroupMissingOpenerNode[DoubleBracketingBar, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftCeiling, _, _], ___}, GroupMissingCloserNode[Ceiling, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`LongName`RightCeiling, _, _]}, GroupMissingOpenerNode[Ceiling, handledChildren, <|Source -> Append[pos, 1]|>],
    {LeafNode[Token`LongName`LeftFloor, _, _], ___}, GroupMissingCloserNode[Floor, handledChildren, <|Source -> Append[pos, 1]|>],
    {___, LeafNode[Token`LongName`RightFloor, _, _]}, GroupMissingOpenerNode[Floor, handledChildren, <|Source -> Append[pos, 1]|>],

    {LeafNode[Token`Boxes`OpenParenStar, _, _], ___},
      GroupMissingCloserNode[Comment,
        {parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
        MapIndexed[
          parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), "StringifyMode" -> 3]&, children[[2;;]]], <|Source->Append[pos, 1]|>],
    {___, LeafNode[Token`Boxes`StarCloseParen, _, _]},
      GroupMissingOpenerNode[Comment,
        MapIndexed[
          parseBox[#1, Append[pos, 1] ~Join~ (#2 + 1 - 1), "StringifyMode" -> 3]&, children[[;;-2]]] ~Join~
        {parseBox[children[[-1]], Append[pos, 1] ~Join~ {Length[children]}]}, <|Source->Append[pos, 1]|>],


    (*
    Infix
    *)
    {_, LeafNode[Token`Plus | Token`Minus, _, _], _, ___}, InfixNode[Plus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Times | Token`Star, _, _], _, ___}, InfixNode[Times, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Bar, _, _], _, ___}, InfixNode[Alternatives, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LessEqual | Token`LongName`LessEqual | Token`Greater | 
       Token`Less | Token`LongName`Equal | 
       Token`LongName`GreaterEqual | Token`EqualEqual | 
       Token`LongName`NotEqual |
       Token`GreaterEqual | Token`BangEqual |
       Token`LongName`GreaterEqualLess | Token`LongName`GreaterFullEqual |
       Token`LongName`GreaterGreater | Token`LongName`GreaterLess |
       Token`LongName`GreaterSlantEqual |
       Token`LongName`GreaterTilde | Token`LongName`LessEqualGreater |
       Token`LongName`LessFullEqual | Token`LongName`LessGreater |
       Token`LongName`LessLess | Token`LongName`LessTilde |
       Token`LongName`NestedGreaterGreater | Token`LongName`NestedLessLess |
       Token`LongName`NotGreater | Token`LongName`NotGreaterEqual |
       Token`LongName`NotLess | Token`LongName`NotLessEqual |
       Token`LongName`NotGreaterFullEqual | Token`LongName`NotLessFullEqual |
       Token`LongName`NotGreaterGreater | Token`LongName`NotLessLess |
       Token`LongName`NotGreaterLess | Token`LongName`NotLessGreater |
       Token`LongName`NotGreaterSlantEqual | Token`LongName`NotLessSlantEqual |
       Token`LongName`NotGreaterTilde | Token`LongName`NotLessTilde |
       Token`LongName`NotNestedGreaterGreater | Token`LongName`NotNestedLessLess |
       Token`LongName`VectorGreater | Token`LongName`VectorGreaterEqual |
       Token`LongName`VectorLess | Token`LongName`VectorLessEqual, _, _], _, ___}, InfixNode[InfixInequality, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`And | Token`AmpAmp, _, _], _, ___}, InfixNode[And, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Or | Token`BarBar, _, _], _, ___}, InfixNode[Or, handledChildren, <|Source->Append[pos, 1]|>],

    {_, LeafNode[Token`LongName`Element, _, _], _, ___}, InfixNode[Element, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Distributed, _, _], _, ___}, InfixNode[Distributed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Dot, _, _], _, ___}, InfixNode[Dot, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`TildeTilde, _, _], _, ___}, InfixNode[StringExpression, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotElement, _, _], _, ___}, InfixNode[NotElement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`EqualBangEqual, _, _], _, ___}, InfixNode[UnsameQ, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CenterDot, _, _], _, ___}, InfixNode[CenterDot, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotReverseElement, _, _], _, ___}, InfixNode[NotReverseElement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CircleTimes, _, _], _, ___}, InfixNode[CircleTimes, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Backslash, _, _], _, ___}, InfixNode[Backslash, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Nand, _, _], _, ___}, InfixNode[Nand, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Nor, _, _], _, ___}, InfixNode[Nor, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Cap, _, _], _, ___}, InfixNode[Cap, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CircleDot, _, _], _, ___}, InfixNode[CircleDot, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CirclePlus, _, _], _, ___}, InfixNode[CirclePlus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Colon, _, _], _, ___}, InfixNode[Colon, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`AtStar, _, _], _, ___}, InfixNode[Composition, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Conditioned, _, _], _, ___}, InfixNode[Conditioned, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Congruent, _, _], _, ___}, InfixNode[Congruent, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Coproduct, _, _], _, ___}, InfixNode[Coproduct, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Cross, _, _], _, ___}, InfixNode[Cross, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Cup, _, _], _, ___}, InfixNode[Cup, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CupCap, _, _], _, ___}, InfixNode[CupCap, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Diamond, _, _], _, ___}, InfixNode[Diamond, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Divides, _, _], _, ___}, InfixNode[Divisible, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DotEqual, _, _], _, ___}, InfixNode[DotEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleDownArrow, _, _], _, ___}, InfixNode[DoubleDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLeftArrow, _, _], _, ___}, InfixNode[DoubleLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLeftRightArrow, _, _], _, ___}, InfixNode[DoubleLeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLongLeftArrow, _, _], _, ___}, InfixNode[DoubleLongLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLongLeftRightArrow, _, _], _, ___}, InfixNode[DoubleLongLeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLongRightArrow, _, _], _, ___}, InfixNode[DoubleLongRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleRightArrow, _, _], _, ___}, InfixNode[DoubleRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleUpArrow, _, _], _, ___}, InfixNode[DoubleUpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleUpDownArrow, _, _], _, ___}, InfixNode[DoubleUpDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleVerticalBar, _, _], _, ___}, InfixNode[DoubleVerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownArrow, _, _], _, ___}, InfixNode[DownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownArrowBar, _, _], _, ___}, InfixNode[DownArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownArrowUpArrow, _, _], _, ___}, InfixNode[DownArrowUpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftRightVector, _, _], _, ___}, InfixNode[DownLeftRightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftTeeVector, _, _], _, ___}, InfixNode[DownLeftTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftVector, _, _], _, ___}, InfixNode[DownLeftVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownLeftVectorBar, _, _], _, ___}, InfixNode[DownLeftVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownRightTeeVector, _, _], _, _, ___}, InfixNode[DownRightTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownRightVector, _, _], _, ___}, InfixNode[DownRightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownRightVectorBar, _, _], _, ___}, InfixNode[DownRightVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownTeeArrow, _, _], _, ___}, InfixNode[DownTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`EqualTilde, _, _], _, ___}, InfixNode[EqualTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Equilibrium, _, _], _, ___}, InfixNode[Equilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Equivalent, _, _], _, ___}, InfixNode[Equivalent, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`HumpDownHump, _, _], _, ___}, InfixNode[HumpDownHump, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`HumpEqual, _, _], _, ___}, InfixNode[HumpEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Intersection, _, _], _, ___}, InfixNode[Intersection, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftArrow, _, _], _, ___}, InfixNode[LeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftArrowBar, _, _], _, ___}, InfixNode[LeftArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftArrowRightArrow, _, _], _, ___}, InfixNode[LeftArrowRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDownTeeVector, _, _], _, ___}, InfixNode[LeftDownTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDownVector, _, _], _, ___}, InfixNode[LeftDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftDownVectorBar, _, _], _, ___}, InfixNode[LeftDownVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftRightArrow, _, _], _, ___}, InfixNode[LeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftRightVector, _, _], _, ___}, InfixNode[LeftRightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTeeArrow, _, _], _, ___}, InfixNode[LeftTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTeeVector, _, _], _, ___}, InfixNode[LeftTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTriangle, _, _], _, ___}, InfixNode[LeftTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTriangleBar, _, _], _, ___}, InfixNode[LeftTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTriangleEqual, _, _], _, ___}, InfixNode[LeftTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpDownVector, _, _], _, ___}, InfixNode[LeftUpDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpTeeVector, _, _], _, ___}, InfixNode[LeftUpTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpVector, _, _], _, ___}, InfixNode[LeftUpVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftUpVectorBar, _, _], _, ___}, InfixNode[LeftUpVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftVector, _, _], _, ___}, InfixNode[LeftVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftVectorBar, _, _], _, ___}, InfixNode[LeftVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LongLeftArrow, _, _], _, ___}, InfixNode[LongLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LongLeftRightArrow, _, _], _, ___}, InfixNode[LongLeftRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LongRightArrow, _, _], _, ___}, InfixNode[LongRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LowerLeftArrow, _, _], _, ___}, InfixNode[LowerLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LowerRightArrow, _, _], _, ___}, InfixNode[LowerRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`StarStar, _, _], _, ___}, InfixNode[NonCommutativeMultiply, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotCongruent, _, _], _, ___}, InfixNode[NotCongruent, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotCupCap, _, _], _, ___}, InfixNode[NotCupCap, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotDoubleVerticalBar, _, _], _, ___}, InfixNode[NotDoubleVerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotEqualTilde, _, _], _, ___}, InfixNode[NotEqualTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotHumpDownHump, _, _], _, ___}, InfixNode[NotHumpDownHump, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotHumpEqual, _, _], _, ___}, InfixNode[NotHumpEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLeftTriangle, _, _], _, ___}, InfixNode[NotLeftTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLeftTriangleBar, _, _], _, ___}, InfixNode[NotLeftTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotLeftTriangleEqual, _, _], _, ___}, InfixNode[NotLeftTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedes, _, _], _, ___}, InfixNode[NotPrecedes, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedesEqual, _, _], _, ___}, InfixNode[NotPrecedesEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedesSlantEqual, _, _], _, ___}, InfixNode[NotPrecedesSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotPrecedesTilde, _, _], _, ___}, InfixNode[NotPrecedesTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotRightTriangle, _, _], _, ___}, InfixNode[NotRightTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotRightTriangleBar, _, _], _, ___}, InfixNode[NotRightTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotRightTriangleEqual, _, _], _, ___}, InfixNode[NotRightTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSubset, _, _], _, ___}, InfixNode[NotSquareSubset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSubsetEqual, _, _], _, ___}, InfixNode[NotSquareSubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSuperset, _, _], _, ___}, InfixNode[NotSquareSuperset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSquareSupersetEqual, _, _], _, ___}, InfixNode[NotSquareSupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSubset, _, _], _, ___}, InfixNode[NotSubset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSubsetEqual, _, _], _, ___}, InfixNode[NotSubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceeds, _, _], _, ___}, InfixNode[NotSucceeds, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceedsEqual, _, _], _, ___}, InfixNode[NotSucceedsEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceedsSlantEqual, _, _], _, ___}, InfixNode[NotSucceedsSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSucceedsTilde, _, _], _, ___}, InfixNode[NotSucceedsTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSuperset, _, _], _, ___}, InfixNode[NotSuperset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotSupersetEqual, _, _], _, ___}, InfixNode[NotSupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTilde, _, _], _, ___}, InfixNode[NotTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTildeEqual, _, _], _, ___}, InfixNode[NotTildeEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTildeFullEqual, _, _], _, ___}, InfixNode[NotTildeFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotTildeTilde, _, _], _, ___}, InfixNode[NotTildeTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`NotVerticalBar, _, _], _, ___}, InfixNode[NotVerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PermutationProduct, _, _], _, ___}, InfixNode[PermutationProduct, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Precedes, _, _], _, ___}, InfixNode[Precedes, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PrecedesEqual, _, _], _, ___}, InfixNode[PrecedesEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PrecedesSlantEqual, _, _], _, ___}, InfixNode[PrecedesSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PrecedesTilde, _, _], _, ___}, InfixNode[PrecedesTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Proportion, _, _], _, ___}, InfixNode[Proportion, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Proportional, _, _], _, ___}, InfixNode[Proportional, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ReverseElement, _, _], _, ___}, InfixNode[ReverseElement, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ReverseEquilibrium, _, _], _, ___}, InfixNode[ReverseEquilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ReverseUpEquilibrium, _, _], _, ___}, InfixNode[ReverseUpEquilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightArrow, _, _], _, ___}, InfixNode[RightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightArrowBar, _, _], _, ___}, InfixNode[RightArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightArrowLeftArrow, _, _], _, ___}, InfixNode[RightArrowLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightDownTeeVector, _, _], _, ___}, InfixNode[RightDownTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightDownVector, _, _], _, ___}, InfixNode[RightDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightDownVectorBar, _, _], _, ___}, InfixNode[RightDownVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTeeArrow, _, _], _, ___}, InfixNode[RightTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTeeVector, _, _], _, ___}, InfixNode[RightTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTriangle, _, _], _, ___}, InfixNode[RightTriangle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTriangleBar, _, _], _, ___}, InfixNode[RightTriangleBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTriangleEqual, _, _], _, ___}, InfixNode[RightTriangleEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpDownVector, _, _], _, ___}, InfixNode[RightUpDownVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpTeeVector, _, _], _, ___}, InfixNode[RightUpTeeVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpVector, _, _], _, ___}, InfixNode[RightUpVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightUpVectorBar, _, _], _, ___}, InfixNode[RightUpVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightVector, _, _], _, ___}, InfixNode[RightVector, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightVectorBar, _, _], _, ___}, InfixNode[RightVectorBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortDownArrow, _, _], _, ___}, InfixNode[ShortDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortLeftArrow, _, _], _, ___}, InfixNode[ShortLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortRightArrow, _, _], _, ___}, InfixNode[ShortRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`ShortUpArrow, _, _], _, ___}, InfixNode[ShortUpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SmallCircle, _, _], _, ___}, InfixNode[SmallCircle, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareIntersection, _, _], _, ___}, InfixNode[SquareIntersection, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSubset, _, _], _, ___}, InfixNode[SquareSubset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSubsetEqual, _, _], _, ___}, InfixNode[SquareSubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSuperset, _, _], _, ___}, InfixNode[SquareSuperset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareSupersetEqual, _, _], _, ___}, InfixNode[SquareSupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SquareUnion, _, _], _, ___}, InfixNode[SquareUnion, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Star, _, _], _, ___}, InfixNode[Star, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Subset, _, _], _, ___}, InfixNode[Subset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SubsetEqual, _, _], _, ___}, InfixNode[SubsetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Succeeds, _, _], _, ___}, InfixNode[Succeeds, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SucceedsEqual, _, _], _, ___}, InfixNode[SucceedsEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SucceedsSlantEqual, _, _], _, ___}, InfixNode[SucceedsSlantEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SucceedsTilde, _, _], _, ___}, InfixNode[SucceedsTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Superset, _, _], _, ___}, InfixNode[Superset, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SupersetEqual, _, _], _, ___}, InfixNode[SupersetEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TensorProduct, _, _], _, ___}, InfixNode[TensorProduct, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TensorWedge, _, _], _, ___}, InfixNode[TensorWedge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Tilde, _, _], _, ___}, InfixNode[Tilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TildeEqual, _, _], _, ___}, InfixNode[TildeEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TildeFullEqual, _, _], _, ___}, InfixNode[TildeFullEqual, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`TildeTilde, _, _], _, ___}, InfixNode[TildeTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Union, _, _], _, ___}, InfixNode[Union, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UnionPlus, _, _], _, ___}, InfixNode[UnionPlus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpArrow, _, _], _, ___}, InfixNode[UpArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpArrowBar, _, _], _, ___}, InfixNode[UpArrowBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpArrowDownArrow, _, _], _, ___}, InfixNode[UpArrowDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpDownArrow, _, _], _, ___}, InfixNode[UpDownArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpEquilibrium, _, _], _, ___}, InfixNode[UpEquilibrium, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpperLeftArrow, _, _], _, ___}, InfixNode[UpperLeftArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpperRightArrow, _, _], _, ___}, InfixNode[UpperRightArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpTeeArrow, _, _], _, ___}, InfixNode[UpTeeArrow, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Vee, _, _], _, ___}, InfixNode[Vee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VerticalBar, _, _], _, ___}, InfixNode[VerticalBar, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VerticalSeparator, _, _], _, ___}, InfixNode[VerticalSeparator, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`VerticalTilde, _, _], _, ___}, InfixNode[VerticalTilde, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Wedge, _, _], _, ___}, InfixNode[Wedge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Xnor, _, _], _, ___}, InfixNode[Xnor, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Xor, _, _], _, ___}, InfixNode[Xor, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`EqualEqualEqual, _, _], _, ___}, InfixNode[SameQ, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LessGreater, _, _], _, ___}, InfixNode[StringJoin, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashStar, _, _], _, ___}, InfixNode[RightComposition, handledChildren, <|Source->Append[pos, 1]|>],

    (*
    :: stringifies its args
    *)
    {_, LeafNode[Token`ColonColon, _, _], _, ___},
      InfixNode[MessageName,
        {parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
        {parseBox[children[[2]], Append[pos, 1] ~Join~ {2}]} ~Join~
        MapIndexed[
          If[Mod[#2[[1]], 2] == 1,
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + 3 - 1), "StringifyMode" -> 1],
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + 3 - 1)]]&, children[[3;;]]],
      <|Source->Append[pos, 1]|>],


    (*
    Infix with trailing allowed
    *)
    {_, LeafNode[Token`Semi, _, _], ___}, InfixNode[CompoundExpression, handledChildren ~Join~
                                              If[MatchQ[handledChildren[[-1]], LeafNode[Token`Semi, _, _]],
                                                { LeafNode[Token`Fake`ImplicitNull, "", handledChildren[[-1, 3]]] },
                                                {}], <|Source->Append[pos, 1]|>],

    {_, LeafNode[Token`Comma, _, _], ___}, InfixNode[Comma, handledChildren ~Join~
                                              If[MatchQ[handledChildren[[-1]], LeafNode[Token`Comma, _, _]],
                                                { LeafNode[Token`Fake`ImplicitNull, "", handledChildren[[-1, 3]]] },
                                                {}], <|Source->Append[pos, 1]|>],

    (*
    Binary
    *)
    {_, LeafNode[Token`MinusGreater | Token`LongName`Rule, _, _], _}, BinaryNode[Rule, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`ColonGreater | Token`LongName`RuleDelayed, _, _], _}, BinaryNode[RuleDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSlash, _, _], _}, BinaryNode[BinarySlashSlash, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UndirectedEdge, _, _], _}, BinaryNode[UndirectedEdge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DirectedEdge, _, _], _}, BinaryNode[DirectedEdge, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Caret, _, _], _}, BinaryNode[Power, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`ColonEqual, _, _], _}, BinaryNode[SetDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`CaretColonEqual, _, _], _}, BinaryNode[UpSetDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Equal, _, _], _}, BinaryNode[Set, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Slash, _, _], _}, BinaryNode[Divide, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashAt, _, _], _}, BinaryNode[Map, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`At, _, _], _}, BinaryNode[BinaryAt, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashDot, _, _], _}, BinaryNode[ReplaceAll, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`AtAt, _, _], _}, BinaryNode[Apply, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Question, _, _], _}, BinaryNode[PatternTest, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Function, _, _], _}, BinaryNode[Function, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSemi, _, _], _}, BinaryNode[Condition, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SemiSemi, _, _], _}, BinaryNode[Span, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`AtAtAt, _, _], _}, BinaryNode[BinaryAtAtAt, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSlashDot, _, _], _}, BinaryNode[ReplaceRepeated, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`PlusEqual, _, _], _}, BinaryNode[AddTo, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Because, _, _], _}, BinaryNode[Because, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Divide, _, _], _}, BinaryNode[Divide, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashEqual, _, _], _}, BinaryNode[DivideBy, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashSlashAt, _, _], _}, BinaryNode[MapAll, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`MinusPlus, _, _], _}, BinaryNode[MinusPlus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`PlusMinus, _, _], _}, BinaryNode[PlusMinus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`MinusEqual, _, _], _}, BinaryNode[SubtractFrom, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`StarEqual, _, _], _}, BinaryNode[TimesBy, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LessMinusGreater | Token`LongName`TwoWayRule, _, _], _}, BinaryNode[TwoWayRule, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`CaretEqual, _, _], _}, BinaryNode[UpSet, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`CircleMinus, _, _], _}, BinaryNode[CircleMinus, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleLeftTee, _, _], _}, BinaryNode[DoubleLeftTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DoubleRightTee, _, _], _}, BinaryNode[DoubleRightTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`DownTee, _, _], _}, BinaryNode[DownTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`UpTee, _, _], _}, BinaryNode[UpTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`LeftTee, _, _], _}, BinaryNode[LeftTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`RightTee, _, _], _}, BinaryNode[RightTee, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Implies, _, _], _}, BinaryNode[Implies, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`SuchThat, _, _], _, _}, BinaryNode[SuchThat, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`LongName`Therefore, _, _], _, _}, BinaryNode[Therefore, handledChildren, <|Source->Append[pos, 1]|>],

    {LeafNode[Token`Under, _, _], LeafNode[Token`Colon, _, _], _}, BinaryNode[Optional, handledChildren, <|Source->Append[pos, 1]|>],
    {CompoundNode[PatternBlank, _, _], LeafNode[Token`Colon, _, _], _}, BinaryNode[Optional, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`Colon, _, _], _}, BinaryNode[Pattern, handledChildren, <|Source->Append[pos, 1]|>],

    {_, LeafNode[Token`Boxes`EqualDot, _, _]},
      BinaryNode[Unset,
        handledChildren /. {
          LeafNode[Token`Boxes`EqualDot, _, data_] :> Sequence @@ {LeafNode[Token`Equal, "=", data], LeafNode[Token`Dot, ".", data]}
        }, <|Source->Append[pos, 1]|>],

    (*
    >> stringifies its args
    *)
    {_, LeafNode[Token`GreaterGreater, _, _], _},
      BinaryNode[Put, {
        parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
          {parseBox[children[[2]], Append[pos, 1] ~Join~ {2}]} ~Join~
          {parseBox[children[[3]], Append[pos, 1] ~Join~ {3}, "StringifyMode" -> 2]}
        ,
        <|Source->Append[pos, 1]|>],

    (*
    >>> stringifies its args
    *)
    {_, LeafNode[Token`GreaterGreaterGreater, _, _], _},
      BinaryNode[PutAppend, {
        parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
          {parseBox[children[[2]], Append[pos, 1] ~Join~ {2}]} ~Join~
          {parseBox[children[[3]], Append[pos, 1] ~Join~ {3}, "StringifyMode" -> 2]}
        ,
        <|Source->Append[pos, 1]|>],

    (*
    Ternary
    *)
    {_, LeafNode[Token`SemiSemi, _, _], _, LeafNode[Token`SemiSemi, _, _], _}, TernaryNode[Span, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`ColonEqual, _, _], _}, TernaryNode[TagSetDelayed, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], _}, TernaryNode[TagSet, handledChildren, <|Source->Append[pos, 1]|>],
    {_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Boxes`EqualDot, _, _]},
      TernaryNode[TagUnset, 
        handledChildren /. {
          LeafNode[Token`Boxes`EqualDot, _, data_] :> Sequence @@ {LeafNode[Token`Equal, "=", data], LeafNode[Token`Dot, ".", data]}
        }, <|Source->Append[pos, 1]|>],

    (*
    older style that may be possible?
    *)
    (*
    {_, LeafNode[Token`SlashColon, _, _], BinaryNode[Unset, _, _]},
      xx,
    *)

    {_, LeafNode[Token`Tilde, _, _], _, LeafNode[Token`Tilde, _, _], _}, TernaryNode[TernaryTilde, handledChildren, <|Source->Append[pos, 1]|>],
    
    (*
    Prefix
    *)
    {LeafNode[Token`Minus | Token`LongName`Minus, _, _], _}, PrefixNode[Minus, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`Bang | Token`LongName`Not, _, _], _}, PrefixNode[Not, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`DifferentialD, _, _], _}, PrefixNode[DifferentialD, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`CapitalDifferentialD, _, _], _}, PrefixNode[CapitalDifferentialD, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`Del, _, _], _}, PrefixNode[Del, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`PlusPlus, _, _], _}, PrefixNode[PreIncrement, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`MinusMinus, _, _], _}, PrefixNode[PreDecrement, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`Plus, _, _], _}, PrefixNode[Plus, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`Square, _, _], _}, PrefixNode[Square, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`BangBang, _, _], _}, PrefixNode[PrefixNot2, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`Sqrt, _, _], _}, PrefixNode[Sqrt, handledChildren, <|Source->Append[pos, 1]|>],
    {LeafNode[Token`LongName`CubeRoot, _, _], _}, PrefixNode[CubeRoot, handledChildren, <|Source->Append[pos, 1]|>],

    {LeafNode[Token`SemiSemi, _, _], _}, BinaryNode[Span, {LeafNode[Token`Fake`ImplicitOne, "", <||>]} ~Join~ handledChildren, <|Source->Append[pos, 1]|>],

    (*
    << stringifies its args
    There might be whitespace after the arg, e.g.
    '<' '<' 'f' 'o' 'o' '`' ' ' ' ' ' '
    *)
    {LeafNode[Token`LessLess, _, _], _, ___},
      PrefixNode[Get, {
        parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
          MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), "StringifyMode" -> 2]&, children[[2;;]]]
        ,
        <|Source->Append[pos, 1]|>],

    (*
    Prefix ? only works with boxes
    *)
    {LeafNode[Token`Question, _, _], _},
      PrefixNode[Information, {
        parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
          MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), "StringifyMode" -> 3]&, children[[2;;]]]
        ,
        <|Source->Append[pos, 1]|>],
    {LeafNode[Token`QuestionQuestion, _, _], _},
      PrefixNode[Information, {
        parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
          MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), "StringifyMode" -> 3]&, children[[2;;]]]
        ,
        <|Source->Append[pos, 1]|>],


    (*
    PrefixBinary
    *)
    {LeafNode[Token`LongName`Integral, _, _], _},
      Switch[children,
        {"\[Integral]", RowBox[{_, ___, RowBox[{"\[DifferentialD]", _}]}]},
          (*
          Successful match for Integral syntax
          *)
          PrefixBinaryNode[Integrate, {
            LeafNode[Token`LongName`Integral, "\[Integral]", <|Source->Append[pos, 1] ~Join~ {1}|>]} ~Join~
            MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ {2, 1} ~Join~ (#2 + 1 - 1)]&, children[[2, 1]]], <|Source->Append[pos, 1]|>]
        ,
        _,
          (*
          Does not match Integral syntax, so treat as generic RowBox
          *)
          BoxNode[RowBox, {handledChildren}, <|Source->Append[pos, 1]|>]
      ],

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

    {_, LeafNode[Token`SemiSemi, _, _]}, BinaryNode[Span, handledChildren ~Join~ {LeafNode[Token`Fake`ImplicitAll, "", <||>]}, <|Source->Append[pos, 1]|>],

    (*
    if there is an error, then just return the last non-trivia node
    *)
    {_, ErrorNode[Token`Error`UnhandledCharacter, _, _], ___},
        aggregatedChildren[[-1]],
        
    _,
    (*Failure["InternalUnhandled", <|"Function" -> parseBox, "Arguments"->HoldForm[RowBox[children]]|>]*)
    BoxNode[RowBox, {handledChildren}, <|Source->Append[pos, 1]|>]
    ]
   ]]

(*
There may be RowBoxs inside of comments
*)
parseBox[RowBox[children_], pos_, "StringifyMode" -> 3] :=
  Module[{handledChildren},

    handledChildren = children;

    handledChildren = MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ #2, "StringifyMode" -> 3]&, handledChildren];

    BoxNode[RowBox, {handledChildren}, <|Source->Append[pos, 1]|>]
  ]






Attributes[applyCodeNodesToRest] = {HoldAllComplete}

applyCodeNodesToRest[rest___] := List @@ Map[Function[arg, With[{assoc = <||>}, CodeNode[Null, arg, assoc]], {HoldAllComplete}], HoldComplete[rest]]



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

(*
FullNotationPalette has something like:

TooltipBox[xxx, "\[EscapeKey]notation\[EscapeKey]. Notation template that parses and formats."]

2nd arg is a string, not a box
*)
parseBox[TooltipBox[a_, rest___], pos_] :=
  BoxNode[TooltipBox,
    {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]



parseBox[TagBox[a_, rest___], pos_] :=
  BoxNode[TagBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicBox[rest___], pos_] :=
  BoxNode[DynamicBox, applyCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicModuleBox[rest___], pos_] :=
  BoxNode[DynamicModuleBox, applyCodeNodesToRest[rest], <|Source->pos|>]

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

parseBox[TableViewBox[rest___], pos_] :=
  BoxNode[TableViewBox, applyCodeNodesToRest[rest], <|Source->pos|>]





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
The Front End treats comments as a collection of code, and not a single token
*)
parseBox["(*", pos_] :=
  LeafNode[Token`Boxes`OpenParenStar, "(*", <|Source -> pos|>]

parseBox["=.", pos_] := LeafNode[Token`Boxes`EqualDot, "=.", <|Source -> pos|>]


(*
Do not add SyntaxIssues saying that \[IndentingNewLine] is strange
*)
parseBox["\[IndentingNewLine]", pos_] :=
  LeafNode[Token`Newline, "\[IndentingNewLine]", <|Source -> pos|>]


(*
FIXME: if there is ever integration with the build system, then remove this
hard-coded list and use the build system
*)
$mbWhitespace = {
"\[NonBreakingSpace]",
"\[ThickSpace]",
"\[ThinSpace]",
"\[VeryThinSpace]",
"\[MediumSpace]",
"\[NoBreak]",
"\[SpaceIndicator]",
"\[InvisibleSpace]",
"\[NegativeVeryThinSpace]",
"\[NegativeThinSpace]",
"\[NegativeMediumSpace]",
"\[NegativeThickSpace]",
"\[AutoSpace]",
"\[Continuation]",
"\[RoundSpaceIndicator]",
"\[PageBreakAbove]",
"\[PageBreakBelow]",
"\[DiscretionaryPageBreakAbove]",
"\[DiscretionaryPageBreakBelow]",
"\[AlignmentMarker]"
}

$whitespacePat = Alternatives @@ ({" " | "\t"} ~Join~ $mbWhitespace)

(*
Just do simple thing here and disallow _ and " and .

It is hard to have a pattern for all multibyte letterlike characters
*)
letterlikeStartPat = Except["_"|"\""|"."|"#"|"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"]

letterlikePat = Except["_"|"\""|"."|"#"]


digitPat = DigitCharacter


parseBox[str_String, pos_, OptionsPattern[]] :=
Catch[
Module[{parsed, data, issues, stringifyMode, oldLeafSrc, len, src, cases},

  (*
  Bypass calling into ParseLeaf if only whitespace,
  we handle multiple whitespace characters here,
  because the FE treats  <space><space><space>  as a single token
  *)

  stringifyMode = OptionValue["StringifyMode"];

  Which[
    (*
    Handle all of the CompoundNodes

    a_b

    Front End treats a_b as single token

    Split things like a_b into correct structures
    *)
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "_"],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "_" :> {a, "_"}][[1]];
      parsed = CompoundNode[PatternBlank, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "__"],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "__" :> {a, "__"}][[1]];
      parsed = CompoundNode[PatternBlankSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "___"],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "___" :> {a, "___"}][[1]];
      parsed = CompoundNode[PatternBlankNullSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "_."],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "_." :> {a, "_."}][[1]];
      parsed = CompoundNode[PatternOptionalDefault, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "_" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "_" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {a, "_", b}][[1]];
      parsed = CompoundNode[PatternBlank, {
        parseBox[cases[[1]], pos],
        CompoundNode[Blank, {parseBox[cases[[2]], pos], parseBox[cases[[3]], pos]}, <|Source -> pos|>]}, <|Source -> pos|>]
    ,
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "__" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "__" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {a, "__", b}][[1]];
      parsed = CompoundNode[PatternBlankSequence, {
        parseBox[cases[[1]], pos],
        CompoundNode[BlankSequence, {parseBox[cases[[2]], pos], parseBox[cases[[3]], pos]}, <|Source -> pos|>]}, <|Source -> pos|>]
    ,
    StringMatchQ[str, (letterlikeStartPat ~~ letterlikePat...) ~~ "___" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, a:(letterlikeStartPat ~~ letterlikePat...) ~~ "___" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {a, "___", b}][[1]];
      parsed = CompoundNode[PatternBlankNullSequence, {
        parseBox[cases[[1]], pos],
        CompoundNode[BlankNullSequence, {parseBox[cases[[2]], pos], parseBox[cases[[3]], pos]}, <|Source -> pos|>]}, <|Source -> pos|>]
    ,
    StringMatchQ[str, "_" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, "_" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {"_", b}][[1]];
      parsed = CompoundNode[Blank, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, "__" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, "__" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {"__", b}][[1]];
      parsed = CompoundNode[BlankSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, "___" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, "___" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {"___", b}][[1]];
      parsed = CompoundNode[BlankNullSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, "#" ~~ digitPat..],
      cases = StringCases[str, "#" ~~ b:digitPat.. :> {"#", b}][[1]];
      parsed = CompoundNode[Slot, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, "#" ~~ (letterlikeStartPat ~~ letterlikePat...)],
      cases = StringCases[str, "#" ~~ b:(letterlikeStartPat ~~ letterlikePat...) :> {"#", b}][[1]];
      parsed = CompoundNode[Slot, {parseBox[cases[[1]], pos], parseBox[cases[[2]], pos, "StringifyMode" -> 1]}, <|Source -> pos|>]
    ,
    StringMatchQ[str, "#" ~~ ("\""~~___)],
      cases = StringCases[str, "#" ~~ b:("\""~~___) :> {"#", b}][[1]];
      parsed = CompoundNode[Slot, {parseBox[cases[[1]], pos], parseBox[cases[[2]], pos, "StringifyMode" -> 1]}, <|Source -> pos|>]
    ,
    StringMatchQ[str, "##" ~~ digitPat..],
      cases = StringCases[str, "##" ~~ b:digitPat.. :> {"##", b}][[1]];
      parsed = CompoundNode[SlotSequence, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, "%" ~~ digitPat..],
      cases = StringCases[str, "%" ~~ b:digitPat.. :> {"%", b}][[1]];
      parsed = CompoundNode[Out, parseBox[#, pos]& /@ cases, <|Source -> pos|>]
    ,
    StringMatchQ[str, $whitespacePat..] && (stringifyMode == 0 || stringifyMode == 2),
      parsed = LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>];
    ,
    StringMatchQ[str, ("'")..] && stringifyMode == 0,
      parsed = LeafNode[Token`Boxes`MultiSingleQuote, str, <|Source -> pos|>];
    ,
    StringMatchQ[str, Verbatim["*"].. ~~ ")"],
      (*
      The FE parses (***) as RowBox[{"(*", "***)"}], so need to handle variable-length *** ) token
      *)
      parsed = LeafNode[Token`Boxes`StarCloseParen, str, <|Source -> pos|>]
    ,
    True,
      parsed = CodeConcreteParseLeaf[str, "StringifyMode" -> stringifyMode];
      If[FailureQ[parsed],
        Throw[parsed]
      ];
      parsed[[3, Key[Source]]] = pos;
  ];

  If[$Debug,

    data = parsed[[3]];

    issues = Lookup[data, SyntaxIssues, {}];

    (*
    issues = replacePosition[#, pos, oldLeafSrc]& /@ issues;
    data[SyntaxIssues] = issues;
    *)

    If[!empty[issues],
      Message[CodeConcreteParseBox::needtohandle, issues]
    ];

    (*
    parsed[[3]] = data;
    *)
  ];

  parsed
]]

(*
replacePosition[(head:SyntaxIssue|FormatIssue)[tag_, msg_, severity_, dataIn_], pos_, leafSrc_] :=
Module[{data, actions, newSrc, oldSyntaxIssueSrc},

    data = dataIn;

    oldSyntaxIssueSrc = data[Source];

    newSrc = pos;

    If[!(oldSyntaxIssueSrc[[1, 2]] == leafSrc[[1, 2]] && oldSyntaxIssueSrc[[2, 2]] == leafSrc[[2, 2]]),
        (* this is some sub-part of the leaf *)
        newSrc = newSrc ~Join~ { Intra[oldSyntaxIssueSrc[[1, 2]], oldSyntaxIssueSrc[[2, 2]]] };
    ];

    data[Source] = newSrc;
    actions = data[CodeActions];
    If[!empty[actions],
        actions = replacePosition[#, newSrc]& /@ actions;
        data[CodeActions] = actions
    ];
    head[tag, msg, severity, data]
]
*)

(*
replacePosition[CodeAction[label_, command_, dataIn_], newSrc_] :=
Module[{data, src},
    data = dataIn;

    src = data[Source];
    (*src = pos ~Join~ {LineColumn[src]};*)
    src = newSrc;
    data[Source] = src;
    CodeAction[label, command, data]
]
*)

parseBox[args___] := Failure["InternalUnhandled", <|"Function"->"parseBox", "Arguments"->HoldForm[{args}]|>]



removeImplicits[node_] := DeleteCases[node, LeafNode[Token`Fake`ImplicitTimes, _, _], Infinity]

(*
ToStandardFormBoxes[ContainerNode[Box, children_, _]] :=
Block[{$RecursionLimit = Infinity},
Module[{},
  Replace[children, {
    LeafNode[Token`Newline, str_, _] :> str,
    else_ :> toStandardFormBoxes[removeImplicits[else]]
  }, {1}]
]]
*)
ToStandardFormBoxes[ContainerNode[Box, {child_}, _]] :=
Block[{$RecursionLimit = Infinity},
  toStandardFormBoxes[removeImplicits[child]]
]



toStandardFormBoxes[LeafNode[Token`Fake`ImplicitNull, _, _]] := Nothing

toStandardFormBoxes[LeafNode[Token`Fake`ImplicitOne, _, _]] := Nothing

toStandardFormBoxes[LeafNode[Token`Fake`ImplicitAll, _, _]] := Nothing

toStandardFormBoxes[LeafNode[_, str_, _]] :=
  str


toStandardFormBoxes[ErrorNode[_, str_, _]] :=
  str





toStandardFormBoxes[BoxNode[Cell, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[Cell @@ heldChildren]]
]]


(*
BoxNodes that may contain CodeNodes have to be handled individually
*)

toStandardFormBoxes[BoxNode[DynamicBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[DynamicModuleBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicModuleBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[NamespaceBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  (*
  Need to wrap boxes in HoldComplete, so that ReleaseHold does not descend into the boxes (which may contain code that has HoldComplete)
  *)
  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[NamespaceBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[RasterBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RasterBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[TagBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[TagBox @@ heldChildren]]
]]


toStandardFormBoxes[BoxNode[GraphicsBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Graphics3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Graphics3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsComplexBox, {rest___}, _]] :=
Catch[
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsComplexBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsComplex3DBox, {rest___}, _]] :=
Catch[
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsComplex3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsGroupBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsGroupBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsGroup3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsGroup3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[DiskBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DiskBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[LineBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[LineBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Line3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Line3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RectangleBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RectangleBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PointBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PointBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Point3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Point3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CuboidBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CuboidBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Polygon3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Polygon3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SphereBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SphereBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RotationBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[RotationBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineCurveBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineCurveBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineCurve3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineCurve3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineSurface3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineSurface3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PolygonBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PolygonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ConicHullRegion3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ConicHullRegion3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TubeBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TubeBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Arrow3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Arrow3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GeometricTransformation3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GeometricTransformation3DBox @@ heldChildren]]
]]

(*
too complicated to handle first arg as boxes
*)
toStandardFormBoxes[BoxNode[TemplateBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TemplateBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[FormBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[FormBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ButtonBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ButtonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ActionMenuBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ActionMenuBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PaneSelectorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PaneSelectorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PanelBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[PanelBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InterpretationBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[InterpretationBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[StyleBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[StyleBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SuperscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SuperscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SubsuperscriptBox, {a_, b_, c_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b], cBox = toStandardFormBoxes[c]},
    heldChildren = { HoldComplete[aBox], HoldComplete[bBox], HoldComplete[cBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SubsuperscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[UnderoverscriptBox, {a_, b_, c_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b], cBox = toStandardFormBoxes[c]},
    heldChildren = { HoldComplete[aBox], HoldComplete[bBox], HoldComplete[cBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[UnderoverscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ItemBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ItemBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InsetBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[InsetBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[AdjustmentBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[AdjustmentBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[LocatorPaneBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  (*
  Need to wrap boxes in HoldComplete, so that ReleaseHold does not descend into the boxes (which may contain code that has HoldComplete)
  *)
  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[LocatorPaneBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[AnimatorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[AnimatorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OpenerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[OpenerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SliderBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SliderBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CylinderBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CylinderBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OverscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[OverscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[UnderscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[UnderscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[FrameBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[FrameBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CheckboxBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CheckboxBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ColorSetterBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ColorSetterBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InputFieldBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[InputFieldBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TabViewBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TabViewBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RadioButtonBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RadioButtonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PopupMenuBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PopupMenuBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SetterBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SetterBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Slider2DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Slider2DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[DynamicWrapperBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicWrapperBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PaneBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[PaneBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SqrtBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]},
    heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SqrtBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RadicalBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]},
    heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[RadicalBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ListPickerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ListPickerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OverlayBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[OverlayBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ProgressIndicatorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ProgressIndicatorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TogglerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TogglerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TooltipBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]},
    heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[TooltipBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TableViewBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TableViewBox @@ heldChildren]]
]]


(*
a is a List of Lists
*)
toStandardFormBoxes[BoxNode[GridBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = Map[toStandardFormBoxes, a, {2}]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

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



toStandardFormBoxes[BinaryNode[Unset, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;

  nodeBoxes = nodeBoxes /. {most___, "=", "."} :> {most, "=."};

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


toStandardFormBoxes[TernaryNode[TagUnset, nodes:{
  _, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;

  nodeBoxes = {nodeBoxes[[1]], nodeBoxes[[2]], nodeBoxes[[3]], "=."};

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

toStandardFormBoxes[CallNode[op_List, { node_ }, data_]] :=
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
  RowBox[opBoxes ~Join~ nodeBox[[1]] ]
]]

(*
this is a convenience, and is not technically correct
FIXME: need a way to turn Aggregate syntax into boxes
*)
toStandardFormBoxes[CallNode[op_, { node_ }, data_]] :=
Catch[
Module[{opBoxes, nodeBox},
  opBoxes = toStandardFormBoxes[op];
  If[AnyTrue[opBoxes, FailureQ],
    Throw[SelectFirst[opBoxes, FailureQ]]
  ];
  nodeBox = toStandardFormBoxes[node];
  If[FailureQ[nodeBox],
    Throw[nodeBox]
  ];
  RowBox[ {opBoxes} ~Join~ nodeBox[[1]] ]
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



toStandardFormBoxes[PrefixBinaryNode[Integrate, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[{First[nodeBoxes], RowBox[Rest[nodeBoxes]]}]
]]



toStandardFormBoxes[CompoundNode[Blank, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

toStandardFormBoxes[CompoundNode[BlankSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

toStandardFormBoxes[CompoundNode[BlankNullSequence, nodes_, _]] :=
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
toStandardFormBoxes[CompoundNode[PatternBlank, nodes_, _]] :=
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
toStandardFormBoxes[CompoundNode[PatternBlankSequence, nodes_, _]] :=
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
toStandardFormBoxes[CompoundNode[PatternBlankNullSequence, nodes_, _]] :=
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
toStandardFormBoxes[CompoundNode[PatternOptionalDefault, nodes_, _]] :=
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

Single #1 token
*)
toStandardFormBoxes[CompoundNode[Slot, nodes_, _]] :=
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

Single ##1 token
*)
toStandardFormBoxes[CompoundNode[SlotSequence, nodes_, _]] :=
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

Single %123 token
*)
toStandardFormBoxes[CompoundNode[Out, nodes_, _]] :=
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




toStandardFormBoxes[ContainerNode[File, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[Riffle[nodeBoxes, "\n"]]
]]





toStandardFormBoxes[ContainerNode[Hold, nodes_, data_]] :=
Module[{processed},
  
  processed = Riffle[nodes, LeafNode[Token`Comma, ",", <||>]];

  toStandardFormBoxes[CallNode[LeafNode[Symbol, "Hold", <||>], {
                GroupNode[GroupSquare, {
                  LeafNode[Token`OpenSquare, "[", <||>] } ~Join~
                  { InfixNode[Comma, processed, <||>] } ~Join~
                  { LeafNode[Token`CloseSquare, "]", <||>] }, <||>] }, <||> ]]
]




toStandardFormBoxes[f_Failure] := f

toStandardFormBoxes[args___] := Failure["InternalUnhandled", <|"Function"->ToStandardFormBoxes, "Arguments"->HoldForm[{args}]|>]














End[]

EndPackage[]
