BeginPackage["CodeParser`Generate`RowBox`"]

Begin["`Private`"]

Needs["CodeParser`Generate`Common`"]
Needs["CodeTools`Generate`GenerateSources`"]


normalPrefixParselets = Normal[importedPrefixParselets]

normalInfixParselets = Normal[importedInfixParselets]

(*
Token`Star will be handled specially
*)
normalInfixParselets = DeleteCases[normalInfixParselets, Token`Star -> Parselet`InfixOperatorParselet[Token`Star, Precedence`Star, Times]]



calls = {
"
(*
Calls

Must be before ] is handled as GroupMissingOpenerNode
*)

prbDispatch[{_, LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupNode[GroupSquare, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`LongName`LeftDoubleBracket, _, _], ___, LeafNode[Token`LongName`RightDoubleBracket, _, _]}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupNode[GroupDoubleBracket, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`OpenSquare, _, _], ___}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupMissingCloserNode[GroupSquare, Rest[handledChildren], <||>]}, <|Source->Append[pos, 1]|>]

(*
Unrecognized LongName

Must be before ] is handled as GroupMissingOpenerNode
*)
prbDispatch[{ErrorNode[Token`Error`UnhandledCharacter, \"\\\\[\", _], _, LeafNode[Token`CloseSquare, \"]\", _]}, handledChildren_, children_, pos_] :=
  parseBox[\"\\\\[\" <> children[[2]] <> \"]\", Append[pos, 1]]
"}


groups = {
"
(*
Groups

We want to have Groups before Infix, so that { + } is first handled by Groups, and then Infix
*)

(*
Once inside a comment, then will stay inside a comment
*)
parseCommentRowBox[RowBox[children_], pos_] :=
  Module[{handledChildren},

    handledChildren = children;

    handledChildren = MapIndexed[parseCommentRowBox[#1, Append[pos, 1] ~Join~ #2]&, handledChildren];

    (*
    toBeSpliced[handledChildren]
    *)
    BoxNode[RowBox, {handledChildren}, <|Source->pos|>]
  ]

parseCommentRowBox[child_String, pos_] :=
  LeafNode[String, child, <|Source -> pos|>]
  
prbDispatch[{LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[GroupSquare, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`OpenCurly, _, _], ___, LeafNode[Token`CloseCurly, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[List, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LessBar, _, _], ___, LeafNode[Token`BarGreater, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[Association, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`OpenParen, _, _], ___, LeafNode[Token`CloseParen, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[GroupParen, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftAssociation, _, _], ___, LeafNode[Token`LongName`RightAssociation, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[Association, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftAngleBracket, _, _], ___, LeafNode[Token`LongName`RightAngleBracket, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[AngleBracket, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftBracketingBar, _, _], ___, LeafNode[Token`LongName`RightBracketingBar, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[BracketingBar, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftDoubleBracketingBar, _, _], ___, LeafNode[Token`LongName`RightDoubleBracketingBar, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[DoubleBracketingBar, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftCeiling, _, _], ___, LeafNode[Token`LongName`RightCeiling, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[Ceiling, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftFloor, _, _], ___, LeafNode[Token`LongName`RightFloor, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[Floor, handledChildren, <|Source->Append[pos, 1]|>]

(*
Treat comments like groups
*)
prbDispatch[{LeafNode[Token`Boxes`OpenParenStar, _, _], ___, LeafNode[Token`Boxes`StarCloseParen, _, _]}, handledChildren_, children_, pos_] :=
  Module[{rehandledChildren},

    rehandledChildren =
      {LeafNode[Token`Boxes`OpenParenStar, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
      MapIndexed[
        parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;-2]]] ~Join~
      {LeafNode[Token`Boxes`StarCloseParen, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]};
    
    GroupNode[Comment, rehandledChildren, <|Source->Append[pos, 1]|>]
  ]

(*
Treat skeletons like groups
*)
(*
prbDispatch[{LeafNode[Token`Boxes`LongName`LeftSkeleton, _, _], ___, LeafNode[Token`Boxes`LongName`RightSkeleton, _, _]}, handledChildren_, children_, pos_] :=
  Module[{rehandledChildren},

    rehandledChildren =
      {LeafNode[Token`Boxes`LongName`LeftSkeleton, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
      MapIndexed[
        parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;-2]]] ~Join~
      {LeafNode[Token`Boxes`LongName`RightSkeleton, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]};
    
    GroupNode[Skeleton, rehandledChildren, <|Source->Append[pos, 1]|>]
  ]
*)

(*
Unexpected openers and unexpected closers

The structure of unexpected closers in boxes is different than in strings

Unexpected closers in boxes never mate with an opener

So retain the box structure here

GroupMissingOpenerNode is only used in Boxes
*)
prbDispatch[{LeafNode[Token`OpenSquare, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[GroupSquare, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`CloseSquare, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[GroupSquare, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`OpenCurly, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[List, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`CloseCurly, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[List, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LessBar, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[Association, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`BarGreater, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[Association, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`OpenParen, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[GroupParen, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`CloseParen, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[GroupParen, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftAssociation, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[Association, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`LongName`RightAssociation, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[Association, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftAngleBracket, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[AngleBracket, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`LongName`RightAngleBracket, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[AngleBracket, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftBracketingBar, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[BracketingBar, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`LongName`RightBracketingBar, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[BracketingBar, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftDoubleBracketingBar, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[DoubleBracketingBar, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`LongName`RightDoubleBracketingBar, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[DoubleBracketingBar, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftCeiling, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[Ceiling, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`LongName`RightCeiling, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[Ceiling, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`LeftFloor, _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[Floor, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`LongName`RightFloor, _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[Floor, handledChildren, <|Source -> Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`Boxes`OpenParenStar, _, _], ___}, handledChildren_, children_, pos_] :=
  GroupMissingCloserNode[Comment,
    {LeafNode[Token`Boxes`OpenParenStar, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
    MapIndexed[
      parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;]]], <|Source->Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`Boxes`StarCloseParen, _, _]}, handledChildren_, children_, pos_] :=
  GroupMissingOpenerNode[Comment,
    MapIndexed[
      parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 1 - 1)]&, children[[;;-2]]] ~Join~
    {LeafNode[Token`Boxes`StarCloseParen, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]}, <|Source->Append[pos, 1]|>]

(*
prbDispatch[{LeafNode[Token`Boxes`LongName`LeftSkeleton, _, _], ___}, handledChildren_, children_, pos_] :=
  GroupMissingCloserNode[Skeleton,
    {LeafNode[Token`Boxes`LongName`LeftSkeleton, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
    MapIndexed[
      parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;]]], <|Source->Append[pos, 1]|>]

prbDispatch[{___, LeafNode[Token`Boxes`LongName`RightSkeleton, _, _]}, handledChildren_, children_, pos_] :=
  GroupMissingOpenerNode[Skeleton,
    MapIndexed[
      parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 1 - 1)]&, children[[;;-2]]] ~Join~
    {LeafNode[Token`Boxes`LongName`RightSkeleton, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]}, <|Source->Append[pos, 1]|>]
*)
"}



infixOperatorParselets = Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`InfixOperatorParselet[_, _, op_]] :> {tok, op}]

infix = {
"
(*
Infix
*)
"} ~Join~

(
"prbDispatch[{_, LeafNode[" <> ToString[#[[1]]] <> ", _, _], _, ___}, handledChildren_, ignored_, pos_] := \n\
  InfixNode[" <> ToString[#[[2]]] <> ", handledChildren, <|Source->Append[pos, 1]|>]
" & /@ infixOperatorParselets
)


binaryOperatorParselets = Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`BinaryOperatorParselet[_, _, op_]] :> {tok, op}]

binary = {
"
(*
Binary
*)
"} ~Join~

(
"prbDispatch[{_, LeafNode[" <> ToString[#[[1]]] <> ", _, _], _}, handledChildren_, ignored_, pos_] := \n\
  BinaryNode[" <> ToString[#[[2]]] <> ", handledChildren, <|Source->Append[pos, 1]|>]
" & /@ binaryOperatorParselets
)


ternary = {
"
(*
Ternary
*)
prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`ColonEqual, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TagSetDelayed, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TagSet, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Boxes`EqualDot, _, _]}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TagUnset, handledChildren, <|Source->Append[pos, 1]|>]

(*
older style that may be possible?
*)
(*
{_, LeafNode[Token`SlashColon, _, _], BinaryNode[Unset, _, _]},
  xx,
*)

prbDispatch[{_, LeafNode[Token`Tilde, _, _], _, LeafNode[Token`Tilde, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TernaryTilde, handledChildren, <|Source->Append[pos, 1]|>]
"}


prefix = {
"
(*
Prefix
*)
prbDispatch[{LeafNode[Token`Minus, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Minus, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`Minus, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Minus, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`Bang, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Not, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`Not, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Not, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`DifferentialD, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[DifferentialD, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`CapitalDifferentialD, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[CapitalDifferentialD, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`Del, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Del, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`PlusPlus, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[PreIncrement, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`MinusMinus, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[PreDecrement, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`Plus, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Plus, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`Square, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Square, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`BangBang, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[PrefixNot2, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`Sqrt, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[Sqrt, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`LongName`CubeRoot, _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[CubeRoot, handledChildren, <|Source->Append[pos, 1]|>]
"}


prefixBinary = {
"
(*
PrefixBinary
*)
prbDispatch[{LeafNode[Token`LongName`Integral, _, _], _}, handledChildren_, children_, pos_] :=
  Switch[children,
    {\"\\[Integral]\", RowBox[{_, ___, RowBox[{\"\\[DifferentialD]\", _}]}]},
      (*
      Successful match for Integral syntax
      *)
      PrefixBinaryNode[Integrate, {
        LeafNode[Token`LongName`Integral, \"\\[Integral]\", <|Source->Append[pos, 1] ~Join~ {1}|>]} ~Join~
        MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ {2, 1} ~Join~ (#2 + 1 - 1)]&, children[[2, 1]]], <|Source->Append[pos, 1]|>]
    ,
    _,
      (*
      Does not match Integral syntax, so treat as generic RowBox
      *)
      BoxNode[RowBox, {handledChildren}, <|Source->Append[pos, 1]|>]
  ]
"}


postfix = {
"
(*
Postfix
*)
prbDispatch[{_, LeafNode[Token`Amp, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Function, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`PlusPlus, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Increment, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`Bang, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Factorial, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`SingleQuote, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Derivative, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`Boxes`MultiSingleQuote, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Derivative, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`LongName`Transpose, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Transpose, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`DotDot, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Repeated, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`LongName`Conjugate, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Conjugate, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`LongName`ConjugateTranspose, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[ConjugateTranspose, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`MinusMinus, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Decrement, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`DotDotDot, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[RepeatedNull, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`BangBang, _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[Factorial2, handledChildren, <|Source->Append[pos, 1]|>]
"}


special = {
"
(*
>> stringifies its args
*)
prbDispatch[{_, LeafNode[Token`GreaterGreater, _, _], _}, handledChildren_, children_, pos_] :=
  BinaryNode[Put, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      {parseBox[children[[2]], Append[pos, 1] ~Join~ {2}]} ~Join~
      {parseBox[children[[3]], Append[pos, 1] ~Join~ {3}, \"StringifyMode\" -> 2]}
    ,
    <|Source->Append[pos, 1]|>]

(*
>>> stringifies its args
*)
prbDispatch[{_, LeafNode[Token`GreaterGreaterGreater, _, _], _}, handledChildren_, children_, pos_] :=
  BinaryNode[PutAppend, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      {parseBox[children[[2]], Append[pos, 1] ~Join~ {2}]} ~Join~
      {parseBox[children[[3]], Append[pos, 1] ~Join~ {3}, \"StringifyMode\" -> 2]}
    ,
    <|Source->Append[pos, 1]|>]

(*
<< stringifies its args
There might be whitespace after the arg, e.g.
'<' '<' 'f' 'o' 'o' '`' ' ' ' ' ' '
*)
prbDispatch[{LeafNode[Token`LessLess, _, _], _, ___}, handledChildren_, children_, pos_] :=
  PrefixNode[Get, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), \"StringifyMode\" -> 2]&, children[[2;;]]]
    ,
    <|Source->Append[pos, 1]|>]

(*
:: stringifies its args

Must actually do work here to stringify the middle
*)
prbDispatch[{_, LeafNode[Token`ColonColon, _, _], _, ___}, handledChildren_, children_, pos_] :=
  Module[{poss, first, rest},

    poss = Position[children, \"::\"];

    first = children[[;; poss[[1, 1]]-1]];

    InfixNode[MessageName,
      MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ #2]&, first] ~Join~
      MapIndexed[
        Switch[#1,
          \"::\",
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1)]
          ,
          ws_String /; StringMatchQ[ws, (\" \") ~~ ___],
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1)]
          ,
          _String,
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1), \"StringifyMode\" -> 1]
          ,
          _,
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1)]
        ]&, children[[ poss[[1, 1]] ;;]]],
    <|Source->Append[pos, 1]|>]
  ]

(*
Prefix ? only works with boxes
*)
prbDispatch[{LeafNode[Token`Question, _, _], _}, handledChildren_, children_, pos_] :=
  PrefixNode[Information, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      MapIndexed[LeafNode[String, #1, <| Source -> Append[pos, 1] ~Join~ (#2 + 2 - 1) |>]&, children[[2;;]]]
    ,
    <|Source->Append[pos, 1]|>]

(*
Prefix ?? only works with boxes
*)
prbDispatch[{LeafNode[Token`QuestionQuestion, _, _], _}, handledChildren_, children_, pos_] :=
  PrefixNode[Information, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      MapIndexed[LeafNode[String, #1, <| Source -> Append[pos, 1] ~Join~ (#2 + 2 - 1) |>]&, children[[2;;]]]
    ,
    <|Source->Append[pos, 1]|>]

(*
Does not have a regular parselet
*)
prbDispatch[{_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Unset, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`Equal, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Set, handledChildren, <|Source->Append[pos, 1]|>]

(*
Does not have a regular parselet
*)
prbDispatch[{_, LeafNode[Token`ColonEqual, _, _], _}, handledChildren_, ignored_, pos_] := 
  BinaryNode[SetDelayed, handledChildren, <|Source->Append[pos, 1]|>]

(*

*)
prbDispatch[{_, LeafNode[Token`Boxes`EqualDot, _, _]}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Unset, handledChildren, <|Source->Append[pos, 1]|>]


(*
Span
*)
prbDispatch[{_, LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Span, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`SemiSemi, _, _]}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Span, handledChildren ~Join~ {LeafNode[Token`Fake`ImplicitAll, \"\", <||>]}, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Span, {LeafNode[Token`Fake`ImplicitOne, \"\", <||>]} ~Join~ handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`SemiSemi, _, _], _, LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[Span, handledChildren, <|Source->Append[pos, 1]|>]

(*
Must actually do work here to insert in the middle
*)
prbDispatch[{LeafNode[Token`SemiSemi, _, _], LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  Module[{poss},

    poss = Position[handledChildren, LeafNode[Token`SemiSemi, _, _]];

    TernaryNode[Span, {LeafNode[Token`Fake`ImplicitOne, \"\", <||>]} ~Join~
        Insert[handledChildren, LeafNode[Token`Fake`ImplicitAll, \"\", <||>], poss[[1]]+1], <|Source->Append[pos, 1]|>]
  ]

(*
Must actually do work here to insert in the middle
*)
prbDispatch[{_, LeafNode[Token`SemiSemi, _, _], LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  Module[{poss},

    poss = Position[handledChildren, LeafNode[Token`SemiSemi, _, _]];

    TernaryNode[Span, Insert[handledChildren, LeafNode[Token`Fake`ImplicitAll, \"\", <||>], poss[[1]]+1], <|Source->Append[pos, 1]|>]
  ]


(*
Infix with trailing allowed
*)
prbDispatch[{_, ___, LeafNode[Token`Semi, _, _]}, handledChildren_, ignored_, pos_] :=
  Module[{childrenWithImplicitNull},

    childrenWithImplicitNull = handledChildren ~Join~ {LeafNode[Token`Fake`ImplicitNull, \"\", <||>]};

    (*
    DO NOT COMMIT THIS!!

    SEQUENCEREPLACE IS SLOW!!
    *)
    childrenWithImplicitNull = SequenceReplace[childrenWithImplicitNull, {
        s1:LeafNode[Token`Semi, _, _], ws:(LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _])..., s2:LeafNode[Token`Semi, _, _]
      } :> Sequence[s1, LeafNode[Token`Fake`ImplicitNull, \"\", <||>], ws, s2]];

    InfixNode[CompoundExpression, childrenWithImplicitNull, <|Source->Append[pos, 1]|>]
  ]

prbDispatch[{_, LeafNode[Token`Semi, _, _], ___}, handledChildren_, ignored_, pos_] :=
  Module[{childrenWithImplicitNull},

    childrenWithImplicitNull = handledChildren;

    (*
    DO NOT COMMIT THIS!!

    SEQUENCEREPLACE IS SLOW!!
    *)
    childrenWithImplicitNull = SequenceReplace[childrenWithImplicitNull, {
        s1:LeafNode[Token`Semi, _, _], ws:(LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _])..., s2:LeafNode[Token`Semi, _, _]
      } :> Sequence[s1, LeafNode[Token`Fake`ImplicitNull, \"\", <||>], ws, s2]];

    InfixNode[CompoundExpression, handledChildren, <|Source->Append[pos, 1]|>]
  ]

prbDispatch[{_, LeafNode[Token`Comma, _, _], ___}, handledChildren_, ignored_, pos_] :=
  InfixNode[Comma, handledChildren ~Join~
    If[MatchQ[handledChildren[[-1]], LeafNode[Token`Comma, _, _]],
      { LeafNode[Token`Fake`ImplicitNull, \"\", handledChildren[[-1, 3]]] },
      {}], <|Source->Append[pos, 1]|>]

(*

*)
prbDispatch[{_, LeafNode[Token`Colon, _, _], _, LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TernaryOptionalPattern, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _], LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Optional, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, _, _], LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Optional, handledChildren, <|Source->Append[pos, 1]|>]

prbDispatch[{_, LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Pattern, handledChildren, <|Source->Append[pos, 1]|>]
"}

epilog = {
"
insertImplicitTimes[node_] :=
  Switch[node,
    LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _],
      (*
      Do not insert implicit Times after whitespace
      *)
      {node}
    ,
    GroupNode[Comment, _, _],
      {node}
    ,
    LeafNode[Token`Star, _, _],
      (*
      Do not insert implicit Times after *
      *)
      {node}
    ,
    _,
      {node, LeafNode[Token`Fake`ImplicitTimes, \"\", <||>]}
  ]


(*
Make sure to handle both * and implicit Times in the same RowBox
*)
prbDispatch[{_, LeafNode[Token`Star, _, _], _, ___}, handledChildren_, ignored_, pos_] :=
  Module[{childrenWithImplicitTimes},

    childrenWithImplicitTimes = Flatten[(insertImplicitTimes /@ Most[handledChildren]) ~Join~ {Last[handledChildren]}];

    (*
    Remove ImplicitTimes from the end
    *)
    childrenWithImplicitTimes = Replace[childrenWithImplicitTimes, {most___,
        LeafNode[Token`Fake`ImplicitTimes, _, _], ws:(LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _])...
      } :> {most, ws}];

    (*
    DO NOT COMMIT THIS!!

    SEQUENCEREPLACE IS SLOW!!
    *)
    childrenWithImplicitTimes = SequenceReplace[childrenWithImplicitTimes, {
        LeafNode[Token`Fake`ImplicitTimes, _, _], ws:(LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _])..., s:LeafNode[Token`Star, _, _]
      } :> Sequence[ws, s]];

    InfixNode[Times, childrenWithImplicitTimes, <|Source->Append[pos, 1]|>]
  ]


(*
Something like \\[Alpha
*)
prbDispatch[{ErrorNode[Token`Error`UnhandledCharacter, \"\\\\[\", _], _}, handledChildren_, ignored_, pos_] :=
    parseBox[\"\\\\[\" <> children[[2]], Append[pos, 1]]

(*
if there is an error, then just return the last non-trivia node
*)
prbDispatch[{_, ErrorNode[Token`Error`UnhandledCharacter, _, _], ___}, handledChildren_, ignored_, pos_] :=
    BoxNode[RowBox, {handledChildren}, <|Source -> pos|>]

prbDispatch[_, handledChildren_, ignored_, pos_] /; TrueQ[$PreserveRowBox] := (
  BoxNode[RowBox, {handledChildren}, <|Source -> pos|>]
)


(*
Only comments or something
*)
prbDispatch[{}, handledChildren_, ignored_, pos_] := (
  toBeSpliced[handledChildren]
)

(*
Catch the case of RowBox[{\"a\", \"\\n\", \"b\"}] before hitting the implicit Times fallthrough

Related bugs: 395301
*)
prbDispatch[_, handledChildren_ /;
    !FreeQ[handledChildren, LeafNode[Token`Newline | Token`Boxes`LineContinuation, _, _], 1] &&
    FreeQ[handledChildren, LeafNode[Token`Star, _, _], 1], ignored_, pos_] /; !TrueQ[$ProbablyImplicitTimes] := (
  (*
  make sure to return the concrete children
  *)
  toBeSpliced[handledChildren]
)


(*
Anything that is left over is considered implicit Times

Make sure to handle both * and implicit Times in the same RowBox
*)
prbDispatch[_, handledChildren_, ignored_, pos_] :=
  Module[{childrenWithImplicitTimes},

    childrenWithImplicitTimes = Flatten[(insertImplicitTimes /@ Most[handledChildren]) ~Join~ {Last[handledChildren]}];

    (*
    Remove ImplicitTimes from the end
    *)
    childrenWithImplicitTimes = Replace[childrenWithImplicitTimes, {most___,
      LeafNode[Token`Fake`ImplicitTimes, _, _], ws:(LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _])...
    } :> {most, ws}];
    
    (*
    DO NOT COMMIT THIS!!

    SEQUENCEREPLACE IS SLOW!!
    *)
    childrenWithImplicitTimes = SequenceReplace[childrenWithImplicitTimes, {
        LeafNode[Token`Fake`ImplicitTimes, _, _], ws:LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _]..., s:LeafNode[Token`Star, _, _]
      } :> Sequence[ws, s]];

    InfixNode[Times, childrenWithImplicitTimes, <|Source->Append[pos, 1]|>]
  ]
"}

rowBoxWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

BeginPackage[\"CodeParser`RowBox`\"]

prbDispatch

Begin[\"`Private`\"]

Needs[\"CodeParser`\"]
Needs[\"CodeParser`Boxes`\"]
Needs[\"CodeParser`Utils`\"]
"} ~Join~

calls ~Join~

groups ~Join~

infix ~Join~

binary ~Join~

ternary ~Join~

prefix ~Join~

prefixBinary ~Join~

postfix ~Join~

special ~Join~

epilog ~Join~

{"

End[]

EndPackage[]
"}


generate[] := (

Print["Generating RowBox..."];

Print["exporting RowBox.wl"];
res = Export[FileNameJoin[{generatedWLDir, "RowBox.wl"}], Column[rowBoxWL], "String"];

If[FailureQ[res],
  Print[res];
  Quit[1]
];

Print["Done RowBox..."]
)

If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
