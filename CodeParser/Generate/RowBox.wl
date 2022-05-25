(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`RowBox`"]

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeParser`Generate`Common`"];
Needs["CodeParser`Generate`Symbol`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


normalPrefixParselets = Normal[importedPrefixParselets]

normalInfixParselets = Normal[importedInfixParselets]

(*
Token`Star will be handled specially in the epilog
*)
normalInfixParselets = DeleteCases[normalInfixParselets, Token`Star -> Parselet`InfixOperatorParselet[Token`Star, Precedence`Star, Times]]

(*
Token`SingleQuote is not used in boxes
*)
normalInfixParselets = DeleteCases[normalInfixParselets, Token`SingleQuote -> Parselet`PostfixOperatorParselet[Token`SingleQuote, Precedence`SingleQuote, Derivative]]

(*
Token`Tilde will be handled as Infix
*)
normalInfixParselets = DeleteCases[normalInfixParselets, Token`Tilde -> Parselet`TildeParselet[]]

(*
Token`MultiSingleQuote is used in boxes
*)
normalInfixParselets = Append[normalInfixParselets, Token`Boxes`MultiSingleQuote -> Parselet`PostfixOperatorParselet[Token`Boxes`MultiSingleQuote, Precedence`SingleQuote, Derivative]]

(*
Token`Tilde will be handled as Infix
*)
normalInfixParselets = Append[normalInfixParselets, Token`Tilde -> Parselet`InfixOperatorParselet[Token`Tilde, Precedence`Tilde, InfixTilde]]


groupParselets = Cases[normalPrefixParselets, Verbatim[Rule][tok_, Parselet`GroupParselet[tok_, op_]] :> {tok, op}]

infixOperatorParselets = Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`InfixOperatorParselet[_, _, op_]] :> {tok, op}]

binaryOperatorParselets = Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`BinaryOperatorParselet[_, _, op_]] :> {tok, op}]

prefixOperatorParselets = Cases[normalPrefixParselets, Verbatim[Rule][tok_, Parselet`PrefixOperatorParselet[_, _, op_]] :> {tok, op}]

postfixOperatorParselets = Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`PostfixOperatorParselet[_, _, op_]] :> {tok, op}]



calls = {
"
(*
Calls

Must be before ] is handled as GroupMissingOpenerNode
*)

prbDispatch[{_, LeafNode[Token`OpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupNode[GroupSquare, Rest[handledChildren], <||>]}, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`LongName`LeftDoubleBracket, _, _], ___, LeafNode[Token`LongName`RightDoubleBracket, _, _]}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupNode[GroupDoubleBracket, Rest[handledChildren], <||>]}, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`ColonColonOpenSquare, _, _], ___, LeafNode[Token`CloseSquare, _, _]}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupNode[GroupTypeSpecifier, Rest[handledChildren], <||>]}, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`OpenSquare, _, _], ___}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupMissingCloserNode[GroupSquare, Rest[handledChildren], <||>]}, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`ColonColonOpenSquare, _, _], ___}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupMissingCloserNode[GroupTypeSpecifier, Rest[handledChildren], <||>]}, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`LongName`LeftDoubleBracket, _, _], ___}, handledChildren_, ignored_, pos_] :=
  CallNode[{handledChildren[[1]]}, {GroupMissingCloserNode[GroupDoubleBracket, Rest[handledChildren], <||>]}, <| Source -> pos |>]

(*
Unrecognized LongName

Must be before ] is handled as GroupMissingOpenerNode
*)
prbDispatch[{ErrorNode[Token`Error`UnhandledCharacter, \"\\\\[\", _], ___, LeafNode[Token`CloseSquare, \"]\", _]}, handledChildren_, children_, pos_] :=
Module[{},
  SyntaxErrorNode[SyntaxError`UnhandledCharacter, children, <| Source -> pos |>]
]
"}

groups = {
"
(*
Groups

We want to have Groups before Infix, so that { + } is first handled by Groups, and then Infix



Unexpected openers and unexpected closers

The structure of unexpected closers in boxes is different than in strings

Unexpected closers in boxes never mate with an opener

So retain the box structure here

GroupMissingOpenerNode is only used in Boxes
*)
"} ~Join~
(
With[{openerTokStr = ToString[#[[1]]], closerTokStr = StringReplace[ToString[GroupOpenerToCloser[#[[1]]]], "Closer`" -> "Token`"], tagStr = ToString[#[[2]]]},
"
(*
" <> openerTokStr <> "
*)

prbDispatch[{LeafNode[" <> openerTokStr <> ", _, _], ___, LeafNode[" <> closerTokStr <> ", _, _]}, handledChildren_, ignored_, pos_] :=
  GroupNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]

(*
Unexpected openers and unexpected closers
*)
prbDispatch[{LeafNode[" <> openerTokStr <> ", _, _], ___}, handledChildren_, ignored_, pos_] :=
  GroupMissingCloserNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]
" <>

If[!(closerTokStr == "Token`CloseSquare" && tagStr == "CodeParser`GroupTypeSpecifier"),

"
prbDispatch[{___, LeafNode[" <> closerTokStr <> ", _, _]}, handledChildren_, ignored_, pos_] :=
  GroupMissingOpenerNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]
",

"
(*
skipped

do not generate GroupMissingOpenerNode for GroupTypeSpecifier

f] is assumed to be GroupSquare
*)"]
]& /@ groupParselets
)

groupsEpilog = {
"
(*
Groups Epilog
*)

(*
Strange old FE syntax RowBox[{\"a\", \"(\", \"b\", \")\"}]

Unclear how it was authored
*)
prbDispatch[{_, LeafNode[Token`OpenParen, _, _], ___, LeafNode[Token`CloseParen, _, _]}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`OldFESyntax, handledChildren, <| Source -> pos |>]


(*
Token`Boxes`OpenParenStar

Treat comments like groups
*)

prbDispatch[{LeafNode[Token`Boxes`OpenParenStar, _, _], ___, LeafNode[Token`Boxes`StarCloseParen, _, _]}, handledChildren_, children_, pos_] :=
  Module[{rehandledChildren},

    rehandledChildren =
      {LeafNode[Token`Boxes`OpenParenStar, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
      MapIndexed[parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;-2]]] ~Join~
      {LeafNode[Token`Boxes`StarCloseParen, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]};
    
    GroupNode[Comment, rehandledChildren, <| Source -> pos |>]
  ]

(*
Unexpected openers and unexpected closers
*)
prbDispatch[{LeafNode[Token`Boxes`OpenParenStar, _, _], ___}, handledChildren_, children_, pos_] :=
  GroupMissingCloserNode[Comment,
    {LeafNode[Token`Boxes`OpenParenStar, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
    MapIndexed[parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;]]]
    ,
    <| Source -> pos |>
  ]

prbDispatch[{___, LeafNode[Token`Boxes`StarCloseParen, _, _]}, handledChildren_, children_, pos_] :=
  GroupMissingOpenerNode[Comment,
    MapIndexed[parseCommentRowBox[#1, Append[pos, 1] ~Join~ (#2 + 1 - 1)]&, children[[;;-2]]] ~Join~
    {LeafNode[Token`Boxes`StarCloseParen, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]}
    ,
    <| Source -> pos |>
  ]

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
    BoxNode[RowBox, {handledChildren}, <| Source -> pos |>]
  ]

parseCommentRowBox[child_String, pos_] :=
  LeafNode[String, child, <| Source -> pos |>]


(*
Token`LinearSyntax`OpenParen

Treat linear syntax like groups
*)

prbDispatch[{LeafNode[Token`LinearSyntax`OpenParen, _, _], ___, LeafNode[Token`LinearSyntax`CloseParen, _, _]}, handledChildren_, children_, pos_] :=
  Module[{rehandledChildren},

    rehandledChildren =
      {LeafNode[Token`LinearSyntax`OpenParen, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
      MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;-2]]] ~Join~
      {LeafNode[Token`LinearSyntax`CloseParen, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]};
    
    GroupNode[GroupLinearSyntax, rehandledChildren, <| Source -> pos |>]
  ]

(*
Unexpected openers and unexpected closers
*)
prbDispatch[{LeafNode[Token`LinearSyntax`OpenParen, _, _], ___}, handledChildren_, children_, pos_] :=
  GroupMissingCloserNode[GroupLinearSyntax,
    {LeafNode[Token`LinearSyntax`OpenParen, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
    MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;]]]
    ,
    <| Source -> pos |>
  ]

prbDispatch[{___, LeafNode[Token`LinearSyntax`CloseParen, _, _]}, handledChildren_, children_, pos_] :=
  GroupMissingOpenerNode[GroupLinearSyntax,
    MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 1 - 1)]&, children[[;;-2]]] ~Join~
    {LeafNode[Token`LinearSyntax`CloseParen, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]}
    ,
    <| Source -> pos |>
  ]

" <> "
(*
Token`Boxes`LongName`LeftSkeleton

Treat skeletons like groups
*)
(*
prbDispatch[{LeafNode[Token`Boxes`LongName`LeftSkeleton, _, _], ___, LeafNode[Token`Boxes`LongName`RightSkeleton, _, _]}, handledChildren_, children_, pos_] :=
  Module[{rehandledChildren},

    rehandledChildren =
      {LeafNode[Token`Boxes`LongName`LeftSkeleton, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
      MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;-2]]] ~Join~
      {LeafNode[Token`Boxes`LongName`RightSkeleton, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]};
    
    GroupNode[Skeleton, rehandledChildren, <| Source -> pos |>]
  ]
*)

(*
Unexpected openers and unexpected closers
*)
(*
prbDispatch[{LeafNode[Token`Boxes`LongName`LeftSkeleton, _, _], ___}, handledChildren_, children_, pos_] :=
  GroupMissingCloserNode[Skeleton,
    {LeafNode[Token`Boxes`LongName`LeftSkeleton, children[[1]], <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
    MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]&, children[[2;;]]], <| Source -> pos |>]

prbDispatch[{___, LeafNode[Token`Boxes`LongName`RightSkeleton, _, _]}, handledChildren_, children_, pos_] :=
  GroupMissingOpenerNode[Skeleton,
    MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ (#2 + 1 - 1)]&, children[[;;-2]]] ~Join~
    {LeafNode[Token`Boxes`LongName`RightSkeleton, children[[-1]], <| Source -> Append[pos, 1] ~Join~ {Length[children]} |>]}, <| Source -> pos |>]
*)
"}


infix = {
"
(*
Infix
*)
"} ~Join~

(
With[{tokStr = ToString[#[[1]]], tagStr = ToString[#[[2]]]},
"
(*
" <> tokStr <> "
*)

" <>
If[MemberQ[prefixOperatorParselets[[All, 1]], #[[1]]], "
(*
" <> tokStr <> " is also a Prefix operator, so do not handle missing first argument here
*)
", "
(*
" <> tokStr <> " Missing first rand
*)
prbDispatch[pat:{LeafNode[" <> tokStr <> ", _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    ignored
    ,
    pos
  ]
]
"] <>
"
(*
" <> tokStr <> " Missing second rand
*)
prbDispatch[pat:{_, LeafNode[" <> tokStr <> ", _, _], LeafNode[" <> tokStr <> ", _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;2]] ~Join~
    {err} ~Join~
    pat[[3;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
" <> tokStr <> " Simple
*)
prbDispatch[{_, LeafNode[" <> tokStr <> ", _, _], _}, handledChildren_, ignored_, pos_] :=\n\
  InfixNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]

(*
" <> tokStr <> " Catch-all
*)
prbDispatch[pat:{_, LeafNode[" <> tokStr <> ", _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{ratorsWithNoFollowingRand},
  ratorsWithNoFollowingRand = Cases[Split[pat[[2;;]], (!MatchQ[#2, LeafNode[" <> tokStr <> ", _, _]])&], {rator_} :> rator];
  InfixNode[" <> tagStr <> ",
    Fold[
      Function[{handled, rator},
        Insert[
          handled,
          ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[rator[[3, Key[Source]]]] |>],
          Position[handledChildren, rator][[1, 1]] + 1
        ]
      ]
      ,
      handledChildren
      ,
      Reverse[ratorsWithNoFollowingRand]
    ]
    ,
    <| Source -> pos |>
  ]
]
"
]& /@ infixOperatorParselets
)


binary = {
"
(*
Binary
*)
"} ~Join~

(
With[{tokStr = ToString[#[[1]]], tagStr = ToString[#[[2]]]},
"
(*
" <> tokStr <> "
*)

" <>
If[MemberQ[prefixOperatorParselets[[All, 1]], #[[1]]], "
(*
" <> tokStr <> " is also a Prefix operator, so do not handle missing first argument here
*)
", "
(*
" <> tokStr <> " Missing first rand
*)
prbDispatch[pat:{LeafNode[" <> tokStr <> ", _, _], _}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    ignored
    ,
    pos
  ]
]
"]
<>
"
(*
" <> tokStr <> " Missing last rand
*)
prbDispatch[pat:{_, LeafNode[" <> tokStr <> ", _, _]}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;2]] ~Join~
    {err} ~Join~
    pat[[3;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
" <> tokStr <> " Good
*)
prbDispatch[{_, LeafNode[" <> tokStr <> ", _, _], _}, handledChildren_, ignored_, pos_] :=\n\
  BinaryNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]
"
]& /@ binaryOperatorParselets
)


ternary = {
"
(*
Ternary
*)


(*
Token`SlashColon
*)

(*
Token`SlashColon Missing first rand and Syntax error
*)
prbDispatch[pat:{LeafNode[Token`SlashColon, _, _], ___}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
Token`SlashColon Missing second rand
*)
prbDispatch[pat:{_, LeafNode[Token`SlashColon, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  BinaryNode[TagSet,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
This is from buggy FE behavior

type in:
a /: <space> b =.

the box structure is:
RowBox[{\"a\", \"/:\", \" \", RowBox[{\"b\", \"=.\"}]}]

Related bugs: 414540
*)
prbDispatch[pat:{_, LeafNode[Token`SlashColon, _, _], BinaryNode[Unset, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, unset},
  pos1 = Position[handledChildren, pat[[3]]][[1, 1]];
  TernaryNode[TagUnset,
    handledChildren[[;;pos1-1]] ~Join~
    handledChildren[[pos1, 2]] ~Join~
    handledChildren[[pos1+1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`SlashColon Syntax error
*)
prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`ExpectedSet, handledChildren, <| Source -> pos |>]

(*
Token`SlashColon Token`Equal Token`Dot Good
*)
prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TagUnset, handledChildren, <| Source -> pos |>]

(*
Token`SlashColon Syntax error

This can happen with e.g.  a /: b =

Technically should be ExpectedOperand error, but don't put too much effort into ultra-precise errors here
*)
prbDispatch[{_, LeafNode[Token`SlashColon, _, _], LeafNode[Token`ColonEqual, _, _], _}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, handledChildren, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`SlashColon, _, _], LeafNode[Token`Equal, _, _], _}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, handledChildren, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`ColonEqual, _, _]}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, handledChildren, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _]}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, handledChildren, <| Source -> pos |>]


(*
Token`SlashColon Token`ColonEqual
*)

(*
Token`SlashColon Token`ColonEqual Missing third rand
*)
prbDispatch[pat:{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`ColonEqual, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[4]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[4, 3, Key[Source]]]] |>];
  TernaryNode[TagSet,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`SlashColon Token`ColonEqual Good
*)
prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`ColonEqual, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TagSetDelayed, handledChildren, <| Source -> pos |>]


(*
Token`SlashColon Token`Equal
*)

(*
Token`SlashColon Token`Equal Missing third rand
*)
prbDispatch[pat:{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[4]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[4, 3, Key[Source]]]] |>];
  TernaryNode[TagSetDelayed,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`SlashColon Token`Equal Good
*)
prbDispatch[{_, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TagSet, handledChildren, <| Source -> pos |>]
"}


prefix = {
"
(*
Prefix
*)
"} ~Join~
(
With[{tokStr = ToString[#[[1]]], tagStr = ToString[#[[2]]]},
"
(*
" <> tokStr <> "
*)

prbDispatch[{LeafNode[" <> tokStr <> ", _, _], _}, handledChildren_, ignored_, pos_] :=
  PrefixNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]
"
]& /@ prefixOperatorParselets
)


prefixBinary = {
"
(*
PrefixBinary
*)

(*
Token`LongName`Integral
*)
prbDispatch[{LeafNode[Token`LongName`Integral, _, _], _}, handledChildren_, children_, pos_] :=
  Switch[children,
    {\"\\[Integral]\", RowBox[{_, RowBox[{\"\\[DifferentialD]\", _}]}]},
      (*
      Successful match for Integral syntax

      Single argument
      *)
      PrefixBinaryNode[Integrate, {
        LeafNode[Token`LongName`Integral, \"\\[Integral]\", <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
        {parseBox[children[[2, 1, 1]], Append[pos, 1] ~Join~ {2, 1} ~Join~ ({1} + 1 - 1)]} ~Join~
        {parseBox[children[[2, 1, 2]], Append[pos, 1] ~Join~ {2, 1} ~Join~ ({2} + 1 - 1)]}
        ,
        <| Source -> pos |>
      ]
    ,
    {\"\\[Integral]\", RowBox[{_, _, ___, RowBox[{\"\\[DifferentialD]\", _}]}]},
      (*
      Successful match for Integral syntax

      Multiple arguments, treat as implicit Times
      *)
      PrefixBinaryNode[Integrate, {
        LeafNode[Token`LongName`Integral, \"\\[Integral]\", <| Source -> Append[pos, 1] ~Join~ {1} |>]} ~Join~
        (*
        create a fake RowBox to induce the creation of InfixNode[Times, ...]
        *)
        {parseBox[RowBox[children[[2, 1, ;;-2]]], Append[pos, 1] ~Join~ {2}]} ~Join~
        {parseBox[children[[2, 1, -1]], Append[pos, 1] ~Join~ {2, 1, Length[children[[2, 1]]]}]}
        ,
        <| Source -> pos |>
      ]
    ,
    _,
      (*
      Does not match Integral syntax, so treat as generic RowBox
      *)
      BoxNode[RowBox, {handledChildren}, <| Source -> pos |>]
  ]
"}


postfix = {
"
(*
Postfix
*)
"} ~Join~
(
With[{tokStr = ToString[#[[1]]], tagStr = ToString[#[[2]]]},
"
(*
" <> tokStr <> "
*)

prbDispatch[{_, LeafNode[" <> tokStr <> ", _, _]}, handledChildren_, ignored_, pos_] :=
  PostfixNode[" <> tagStr <> ", handledChildren, <| Source -> pos |>]
"
]& /@ postfixOperatorParselets
)


special = {
"
(*
Special
*)

(*
Token`GreaterGreater

>> stringifies its args
*)

(*
Token`GreaterGreater Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`GreaterGreater, _, _], _}, handledChildren_, children_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    children[[;;pos1-1]] ~Join~
    {err} ~Join~
    children[[pos1;;]]
    ,
    pos
  ]
]

(*
Token`GreaterGreater Missing last rand
*)
prbDispatch[pat:{_, LeafNode[Token`GreaterGreater, _, _]}, handledChildren_, children_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[-1, 3, Key[Source]]]] |>];
  prbDispatch[
    pat ~Join~
    {err}
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    (*
    also modify children because we may need to do StringifyMode -> 2 later
    *)
    children[[;;pos1]] ~Join~
    {err} ~Join~
    children[[pos1+1;;]]
    ,
    pos
  ]
]

(*
Token`GreaterGreater Good
*)
prbDispatch[pat:{_, LeafNode[Token`GreaterGreater, _, _], _}, handledChildren_, children_, pos_] :=
Module[{pos1},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  BinaryNode[Put,
    handledChildren[[;;pos1-1]] ~Join~
    {Switch[children[[pos1]],
      _String,
        (*
        only pass in \"StringifyMode\" -> 2 if arg is a String
        *)
        parseBox[children[[pos1]], Append[pos, 1] ~Join~ {pos1}, \"StringifyMode\" -> 2]
      ,
      _ErrorNode,
        children[[pos1]]
      ,
      _,
        parseBox[children[[pos1]], Append[pos, 1] ~Join~ {pos1}]
    ]} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`GreaterGreaterGreater

>>> stringifies its args
*)

(*
Token`GreaterGreaterGreater Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`GreaterGreaterGreater, _, _], _}, handledChildren_, children_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    children[[;;pos1-1]] ~Join~
    {err} ~Join~
    children[[pos1;;]]
    ,
    pos
  ]
]

(*
Token`GreaterGreaterGreater Missing last rand
*)
prbDispatch[pat:{_, LeafNode[Token`GreaterGreaterGreater, _, _]}, handledChildren_, children_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[-1, 3, Key[Source]]]] |>];
  prbDispatch[
    pat ~Join~
    {err}
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    (*
    also modify children because we may need to do StringifyMode -> 2 later
    *)
    children[[;;pos1]] ~Join~
    {err} ~Join~
    children[[pos1+1;;]]
    ,
    pos
  ]
]

(*
Token`GreaterGreaterGreater Good
*)
prbDispatch[pat:{_, LeafNode[Token`GreaterGreaterGreater, _, _], _}, handledChildren_, children_, pos_] :=
Module[{pos1},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  BinaryNode[PutAppend,
    handledChildren[[;;pos1-1]] ~Join~
    {Switch[children[[pos1]],
      _String,
        (*
        only pass in \"StringifyMode\" -> 2 if arg is a String
        *)
        parseBox[children[[pos1]], Append[pos, 1] ~Join~ {pos1}, \"StringifyMode\" -> 2]
      ,
      _ErrorNode,
        children[[pos1]]
      ,
      _,
        parseBox[children[[pos1]], Append[pos, 1] ~Join~ {pos1}]
    ]} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    <| Source -> pos |>
  ]
]


(*
Token`LessLess

<< stringifies its args
There might be whitespace after the arg, e.g.
'<' '<' 'f' 'o' 'o' '`' ' ' ' ' ' '
*)

prbDispatch[{LeafNode[Token`LessLess, _, _], _, ___}, handledChildren_, children_, pos_] :=
  PrefixNode[Get, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      MapIndexed[
        If[MatchQ[#1, _String],
          (*
          only pass in \"StringifyMode\" -> 2 if arg is a String
          *)
          parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1), \"StringifyMode\" -> 2]
          ,
          parseBox[#1, Append[pos, 1] ~Join~ (#2 + 2 - 1)]
        ]&
        ,
        children[[2;;]]
      ]
    ,
    <| Source -> pos |>
  ]


(*
Token`ColonColon

:: stringifies its args

Must actually do work here to stringify the middle
*)

(*
Token`ColonColon Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`ColonColon, _, _], ___}, handledChildren_, children_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`ColonColon Missing second rand
*)
prbDispatch[pat:{_, LeafNode[Token`ColonColon, _, _], LeafNode[Token`ColonColon, _, _], ___}, handledChildren_, children_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;2]] ~Join~
    {err} ~Join~
    pat[[3;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
Token`ColonColon Good
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
            Function[{tag},
              If[!MatchQ[tag, ErrorNode[Token`Error`ExpectedTag, _, _]],
                (*
                everything is fine
                *)
                tag
                ,
                (*
                something like a::111 but 111 cannot be a tag

                so replace with sequence of ErrorNode (which is empty) and regular LeafNode
                *)
                Sequence @@ {
                  ErrorNode[Token`Error`ExpectedTag, \"\", <| Source -> Before[Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1)]|>]
                  ,
                  (*
                  parse again WITHOUT \"StringifyMode\" -> 1
                  *)
                  parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1)]
                }
              ]
            ][parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1), \"StringifyMode\" -> 1]]
          ,
          _,
            parseBox[#1, Append[pos, 1] ~Join~ (#2 + poss[[1, 1]]-1)]
        ]&, children[[poss[[1, 1]];;]]
      ]
      ,
      <| Source -> pos |>
    ]
  ]


(*
Token`Question

Prefix ? only works with boxes
*)
prbDispatch[{LeafNode[Token`Question, _, _], _}, handledChildren_, children_, pos_] :=
  PrefixNode[Information, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      MapIndexed[LeafNode[String, #1, <| Source -> Append[pos, 1] ~Join~ (#2 + 2 - 1) |>]&, children[[2;;]]]
    ,
    <| Source -> pos |>
  ]

(*
Token`QuestionQuestion

Prefix ?? only works with boxes
*)
prbDispatch[{LeafNode[Token`QuestionQuestion, _, _], _}, handledChildren_, children_, pos_] :=
  PrefixNode[Information, {
    parseBox[children[[1]], Append[pos, 1] ~Join~ {1}]} ~Join~
      MapIndexed[LeafNode[String, #1, <| Source -> Append[pos, 1] ~Join~ (#2 + 2 - 1) |>]&, children[[2;;]]]
    ,
    <| Source -> pos |>
  ]

(*
Token`Equal Token`Dot

Does not have a regular parselet
*)

(*
Token`Equal Token`Dot Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  BinaryNode[Unset,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`Equal Token`Dot Good
*)
prbDispatch[{_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Unset, handledChildren, <| Source -> pos |>]


(*
Token`Equal
*)

(*
Token`Equal Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Equal, _, _], _}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  BinaryNode[Set,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`Equal Missing last rand
*)
prbDispatch[pat:{_, LeafNode[Token`Equal, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[-1, 3, Key[Source]]]] |>];
  BinaryNode[Set,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`Equal Good
*)
prbDispatch[{_, LeafNode[Token`Equal, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Set, handledChildren, <| Source -> pos |>]


(*
Token`ColonEqual

Does not have a regular parselet
*)

(*
Token`ColonEqual Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`ColonEqual, _, _], _}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  BinaryNode[SetDelayed,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`ColonEqual Missing last rand
*)
prbDispatch[{_, LeafNode[Token`ColonEqual, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[-1, 3, Key[Source]]]] |>];
  BinaryNode[SetDelayed,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    <| Source -> pos |>
  ]
]

(*
Token`ColonEqual Good
*)
prbDispatch[{_, LeafNode[Token`ColonEqual, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[SetDelayed, handledChildren, <| Source -> pos |>]


(*
Token`SemiSemi
*)

prbDispatch[{_, LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Span, handledChildren, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`SemiSemi, _, _]}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Span, handledChildren ~Join~ {LeafNode[Token`Fake`ImplicitAll, \"\", <||>]}, <| Source -> pos |>]

prbDispatch[{LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Span, {LeafNode[Token`Fake`ImplicitOne, \"\", <||>]} ~Join~ handledChildren, <| Source -> pos |>]

prbDispatch[{_, LeafNode[Token`SemiSemi, _, _], _, LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[Span, handledChildren, <| Source -> pos |>]

(*
Must actually do work here to insert in the middle
*)
prbDispatch[{LeafNode[Token`SemiSemi, _, _], LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  Module[{poss},

    poss = Position[handledChildren, LeafNode[Token`SemiSemi, _, _]];

    TernaryNode[Span,
      {LeafNode[Token`Fake`ImplicitOne, \"\", <||>]} ~Join~
      Insert[handledChildren, LeafNode[Token`Fake`ImplicitAll, \"\", <||>], poss[[1]]+1]
      ,
      <| Source -> pos |>
    ]
  ]

(*
Must actually do work here to insert in the middle
*)
prbDispatch[{_, LeafNode[Token`SemiSemi, _, _], LeafNode[Token`SemiSemi, _, _], _}, handledChildren_, ignored_, pos_] :=
  Module[{poss},

    poss = Position[handledChildren, LeafNode[Token`SemiSemi, _, _]];

    TernaryNode[Span,
      Insert[handledChildren, LeafNode[Token`Fake`ImplicitAll, \"\", <||>], poss[[1]]+1]
      ,
      <| Source -> pos |>
    ]
  ]


(*
Token`Semi
*)

(*
Token`Semi Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Semi, _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
Token`Semi Missing second rand
*)
prbDispatch[pat:{_, LeafNode[Token`Semi, _, _], LeafNode[Token`Semi, _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, null},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  null = LeafNode[Token`Fake`ImplicitNull, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;2]] ~Join~
    {null} ~Join~
    pat[[3;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {null} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
Token`Semi Simple
*)
prbDispatch[pat:{_, LeafNode[Token`Semi, _, _]}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, null},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  null = LeafNode[Token`Fake`ImplicitNull, \"\", <| Source -> After[pat[[-1, 3, Key[Source]]]] |>];
  InfixNode[CompoundExpression,
    handledChildren ~Join~
    {null}
    ,
    <| Source -> pos |>
  ]
]

(*
Token`Semi Simple
*)
prbDispatch[{_, LeafNode[Token`Semi, _, _], _}, handledChildren_, ignored_, pos_] :=\n\
  InfixNode[CompoundExpression, handledChildren, <| Source -> pos |>]

(*
Token`Semi Catch-all
*)
prbDispatch[pat:{_, LeafNode[Token`Semi, _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{ratorsWithNoFollowingRand},
  ratorsWithNoFollowingRand = Cases[Split[pat[[2;;]], (!MatchQ[#2, LeafNode[Token`Semi, _, _]])&], {rator_} :> rator];
  InfixNode[CompoundExpression,
    Fold[
      Function[{handled, rator},
        Insert[
          handled,
          LeafNode[Token`Fake`ImplicitNull, \"\", <| Source -> After[rator[[3, Key[Source]]]] |>],
          Position[handledChildren, rator][[1, 1]] + 1
        ]
      ]
      ,
      handledChildren
      ,
      Reverse[ratorsWithNoFollowingRand]
    ]
    ,
    <| Source -> pos |>
  ]
]


(*
Token`Comma
*)

(*
Token`Comma Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Comma, _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`PrefixImplicitNull, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
Token`Comma Missing second rand
*)
prbDispatch[pat:{_, LeafNode[Token`Comma, _, _], LeafNode[Token`Comma, _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  err = ErrorNode[Token`Error`InfixImplicitNull, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;2]] ~Join~
    {err} ~Join~
    pat[[3;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    ignored
    ,
    pos
  ]
]

(*
Token`Comma Simple
*)
prbDispatch[{_, LeafNode[Token`Comma, _, _], _}, handledChildren_, ignored_, pos_] :=\n\
  InfixNode[Comma, handledChildren, <| Source -> pos |>]

(*
Token`Comma Catch-all
*)
prbDispatch[pat:{_, LeafNode[Token`Comma, _, _], ___}, handledChildren_, ignored_, pos_] :=\n\
Module[{ratorsWithNoFollowingRand},
  ratorsWithNoFollowingRand = Cases[Split[pat[[2;;]], (!MatchQ[#2, LeafNode[Token`Comma, _, _]])&], {rator_} :> rator];
  InfixNode[Comma,
    Fold[
      Function[{handled, rator},
        Insert[
          handled,
          ErrorNode[Token`Error`InfixImplicitNull, \"\", <| Source -> After[rator[[3, Key[Source]]]] |>],
          Position[handledChildren, rator][[1, 1]] + 1
        ]
      ]
      ,
      handledChildren
      ,
      Reverse[ratorsWithNoFollowingRand]
    ]
    ,
    <| Source -> pos |>
  ]
]


(*
Token`Colon Token`Colon
*)

(*
Token`Colon Token`Colon Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Colon, _, _], _, LeafNode[Token`Colon, _, _], ___}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`Colon Token`Colon Missing second rand
*)
prbDispatch[pat:{_, LeafNode[Token`Colon, _, _], LeafNode[Token`Colon, _, _], ___}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[2]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[2, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;2]] ~Join~
    {err} ~Join~
    pat[[3;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`Colon Token`Colon Missing third rand
*)
prbDispatch[pat:{_, LeafNode[Token`Colon, _, _], _, LeafNode[Token`Colon, _, _]}, handledChildren_, ignored_, pos_] :=
(*               ^ first rand                    ^ second rand                 ^ missing third rand *)
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[4]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[4, 3, Key[Source]]]] |>];
  prbDispatch[
    pat[[;;4]] ~Join~
    {err} ~Join~
    pat[[5;;]]
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`Colon Token`Colon Extra rands

The FE treats a:b:c:d as a single RowBox

It is agreed that this is a bug, but it will never be fixed

related bugs: 379318
*)
prbDispatch[pat:{_, LeafNode[Token`Colon, _, _], _, LeafNode[Token`Colon, _, _], _, LeafNode[Token`Colon, _, _], ___}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`BuggyFESyntax, handledChildren, <| Source -> pos |>]

(*
Token`Colon Token`Colon Good
*)
prbDispatch[{_, LeafNode[Token`Colon, _, _], _, LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  TernaryNode[TernaryOptionalPattern, handledChildren, <| Source -> pos |>]


(*
Token`Colon
*)

(*
Token`Colon Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`Colon Missing last rand
*)
prbDispatch[pat:{_, LeafNode[Token`Colon, _, _]}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[-1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> After[pat[[-1, 3, Key[Source]]]] |>];
  prbDispatch[
    pat ~Join~
    {err}
    ,
    handledChildren[[;;pos1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1+1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`Colon Good
*)
prbDispatch[{LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder, _, _], LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Optional, handledChildren, <| Source -> pos |>]

(*
Token`Colon Good
*)
prbDispatch[{CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, _, _], LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Optional, handledChildren, <| Source -> pos |>]

(*
Token`Colon Good
*)
prbDispatch[{_, LeafNode[Token`Colon, _, _], _}, handledChildren_, ignored_, pos_] :=
  BinaryNode[Pattern, handledChildren, <| Source -> pos |>]

(*
Old FE syntax

Something like RowBox[{RowBox[{\"a_\", \":\"}], \"b\"}]
*)
prbDispatch[{BinaryNode[Optional, {CompoundNode[PatternBlank, {_, _}, _], LeafNode[Token`Colon, _, _], ErrorNode[Token`Error`ExpectedOperand, _, _]}, _], _}, handledChildren_, ignored_, pos_] :=
  SyntaxErrorNode[SyntaxError`OldFESyntax, handledChildren, <| Source -> pos |>]
"}

epilog = {
"
(*
Epilog
*)

insertImplicitTimesAfter[node_] :=
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
    toBeSpliced[_],
      (*
      Do not insert implicit Times after toBeSpliced[_]
      *)
      {node}
    ,
    _,
      {node, LeafNode[Token`Fake`ImplicitTimes, \"\", <| Source -> After[node[[3, Key[Source]]]] |>]}
  ]


(*
Token`Star Missing first rand
*)
prbDispatch[pat:{LeafNode[Token`Star, _, _], _, ___}, handledChildren_, ignored_, pos_] :=
Module[{pos1, err},
  pos1 = Position[handledChildren, pat[[1]]][[1, 1]];
  err = ErrorNode[Token`Error`ExpectedOperand, \"\", <| Source -> Before[pat[[1, 3, Key[Source]]]] |>];
  prbDispatch[
    {err} ~Join~
    pat
    ,
    handledChildren[[;;pos1-1]] ~Join~
    {err} ~Join~
    handledChildren[[pos1;;]]
    ,
    children
    ,
    pos
  ]
]

(*
Token`Star

Make sure to handle both * and implicit Times in the same RowBox
*)
prbDispatch[{_, LeafNode[Token`Star, _, _], _, ___}, handledChildren_, ignored_, pos_] :=
  Module[{childrenWithImplicitTimes},

    childrenWithImplicitTimes = Flatten[(insertImplicitTimesAfter /@ Most[handledChildren]) ~Join~ {Last[handledChildren]}];

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

    InfixNode[Times, childrenWithImplicitTimes, <| Source -> pos |>]
  ]


(*
Something like \\[Alpha
*)
prbDispatch[{ErrorNode[Token`Error`UnhandledCharacter, \"\\\\[\", _], rest_}, handledChildren_, ignored_, pos_] :=
  parseBox[\"\\\\[\" <> rest[[2]], pos]

(*
if there is an error, then just return the last non-trivia node
*)
prbDispatch[{_, ErrorNode[Token`Error`UnhandledCharacter, _, _], ___}, handledChildren_, ignored_, pos_] :=
  BoxNode[RowBox, {handledChildren}, <| Source -> pos |>]

prbDispatch[_, handledChildren_, ignored_, pos_] /; TrueQ[$PreserveRowBox] := (
  BoxNode[RowBox, {handledChildren}, <| Source -> pos |>]
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
prbDispatch[_, handledChildren_, ignored_, posIgnored_] :=
  Module[{childrenWithImplicitTimes, calculatedPos},

    childrenWithImplicitTimes = Flatten[(insertImplicitTimesAfter /@ Most[handledChildren]) ~Join~ {Last[handledChildren]}];

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

    calculatedPos = longestPrefix[take[childrenWithImplicitTimes[[1]]], take[childrenWithImplicitTimes[[-1]]]];

    (*
    strip off the trailing 1
    *)
    calculatedPos = Most[calculatedPos];

    InfixNode[Times, childrenWithImplicitTimes, <| Source -> calculatedPos |>]
  ]


take[_[_, _, KeyValuePattern[Source -> src_]]] :=
  src

take[toBeSpliced[children_]] :=
  longestPrefix[take[children[[1]]], take[children[[-1]]]]


longestPrefix[l1_, l2_] /; Length[l1] > Length[l2] :=
  longestPrefix[l2, l1]

longestPrefix[l1_, l2_] /; Length[l1] <= Length[l2] :=
  NestWhile[Most, l1, !MatchQ[l2, {PatternSequence @@ (# ~Join~ {___})}]&]


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

groupsEpilog ~Join~

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
res = Export[FileNameJoin[{generatedWLDir, "Kernel", "RowBox.wl"}], Column[rowBoxWL], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done RowBox"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
