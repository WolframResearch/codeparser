BeginPackage["CodeParser`Utils`"]

escapeString

(*
char
*)

empty

SourceMemberQ

SourceMemberIntersectingQ

SourceMemberQFunction

contiguousQ


removeIgnoredNodes


Begin["`Private`"]

Needs["CodeParser`"]



escapeString[s_] :=
	ToString[s, InputForm, CharacterEncoding -> "ASCII"]


(*
char["A"] evaluate to 65
Only string literals of a single character may be given as arguments
*)
(*
Attributes[char] = {HoldAll}
char[s_String] /; StringLength[s] == 1 := ToCharacterCode[s][[1]]

char[args___] := (Message[char::unhandled, Hold[args]];$Failed)
*)


empty[l_List] := Length[l] == 0

empty[a_Association] := Length[a] == 0

empty[s_String] := s == ""





(*
Construct a SourceMemberQFunction to be used later
*)

SourceMemberQ[srcs_] := SourceMemberQFunction[srcs]

(*
Define what SourceMemberQFunction should do
*)
SourceMemberQFunction[srcs_][cursor_] := SourceMemberQ[srcs, cursor]






spanPat = {_Integer, _Integer}

srcPat = {spanPat, spanPat}


(*
test that a cursor Source is a Member of any of a List of Sources

This tests for membership of ANY src

This does NOT test for membership of ALL srcs

*)
SourceMemberQ[srcs:{srcPat...}, cursor:srcPat] :=
	AnyTrue[srcs, SourceMemberQ[#, cursor]&]

SourceMemberQ[srcs:{srcPat...}, cursor:spanPat] :=
	AnyTrue[srcs, SourceMemberQ[#, cursor]&]


(*
test that a cursor Source is a Member of a src Source
*)
SourceMemberQ[src:srcPat, {cursor1:spanPat, cursor2:spanPat}] :=
	SourceMemberQ[src, cursor1] && SourceMemberQ[src, cursor2]

SourceMemberQ[src:srcPat, cursor:spanPat] :=
	SourceMemberQ[src, cursor]

(*
Do the actual work

SourceMemberQ[{{1,3},{2,0}}, {1,4}] => True
SourceMemberQ[{{1,3},{2,0}}, {2,4}] => False
*)
SourceMemberQ[{{srcLine1_Integer, srcCol1_Integer}, {srcLine2_Integer, srcCol2_Integer}}, {cursorLine_Integer, cursorCol_Integer}] :=
Which[
	(* not in-between the lines of the spec, so no *)
	!(srcLine1 <= cursorLine <= srcLine2),
		False
	,
	(* everything is on 1 line, so now test cols *)
	cursorLine == srcLine1 == srcLine2,
		srcCol1 <= cursorCol <= srcCol2
	,
	(* on srcLine1, so test that cursor comes after srcCol1 *)
	cursorLine == srcLine1,
		srcCol1 <= cursorCol
	,
	(* on srcLine2, so test that cursor comes before srcCol2 *)
	cursorLine == srcLine2,
		cursorCol <= srcCol2
	,
	(* exclusively in-between start and end, so yes *)
	True,
		True
]




(*
Also handle Position Sources
*)
SourceMemberQ[{srcInts___Integer}, cursorPos:{_Integer...}] := MatchQ[cursorPos, {srcInts, ___}]




(*

contiguousQ

input: src1:{{line,col}, {line,col}}   src2:{{line,col}, {line,col}}

*)

contiguousQ[srcs_List] := And @@ contiguousQ @@@ Partition[srcs, 2, 1]

(*
LineCol-style
*)
contiguousQ[{{_, _}, {line_, col1_}}, {{line_, col2_}, {_, _}}] := col1 == col2

(*
Position-style
*)
contiguousQ[{_Integer..., idx1_Integer}, {_Integer..., idx2_Integer}] := idx1 + 1 == idx2

contiguousQ[_, _] := False




expandSrc[{{line_, col1_}, {line_, col2_}}] :=
  <|line -> {col1, col2}|>

expandSrc[{{line1_, col1_}, {line2_, col2_}}] :=
  <|
    line1 -> {col1, Infinity}, 
    Table[l -> {1, Infinity}, {l, line1 + 1, line2 - 1}], 
    line2 -> {1, col2}
  |>

intervalTest[{_}] :=
  False
intervalTest[{int1_, int2_}] := 
   IntervalIntersection[Interval[int1], Interval[int2]] =!= Interval[]

SourceMemberIntersectingQ[
  src1:{{_Integer, _Integer}, {_Integer, _Integer}},
  src2:{{_Integer, _Integer}, {_Integer, _Integer}}] :=
  Catch[
  Module[{expanded1, expanded2},
    expanded1 = expandSrc[src1];
    expanded2 = expandSrc[src2];
    Or @@ Merge[{expanded1, expanded2}, intervalTest]
  ]]




(*
Used by CodeInspector for removing nodes ignored because of BeginStaticAnalysisIgnore[]
*)

removeIgnoredNodes[Null, _SourceMemberQFunction] := Null

removeIgnoredNodes[l_LeafNode, _SourceMemberQFunction] := l

(* optimization *)
removeIgnoredNodes[node_, SourceMemberQFunction[{}]] :=
  node

removeIgnoredNodes[node_[tag_, childrenIn_, dataIn_], ignoredNodesSrcMemberFunc_SourceMemberQFunction] :=
Module[{children, data, syntaxIssues, abstractSyntaxIssues},

  children = childrenIn;
  data = dataIn;

  children = DeleteCases[children, n_ /; ignoredNodesSrcMemberFunc[n[[3, Key[Source]]]]];
  children = removeIgnoredNodes[#, ignoredNodesSrcMemberFunc]& /@ children;
  
  syntaxIssues = Lookup[data, SyntaxIssues, {}];
  If[syntaxIssues != {},
    syntaxIssues = DeleteCases[syntaxIssues, n_ /; ignoredNodesSrcMemberFunc[n[[4, Key[Source]]]]];
    data[SyntaxIssues] = syntaxIssues;
  ];

  abstractSyntaxIssues = Lookup[data, AbstractSyntaxIssues, {}];
  If[abstractSyntaxIssues != {},
    abstractSyntaxIssues = DeleteCases[abstractSyntaxIssues, n_ /; ignoredNodesSrcMemberFunc[n[[4, Key[Source]]]]];
    data[AbstractSyntaxIssues] = abstractSyntaxIssues;
  ];

  node[tag, children, data]
]




End[]

EndPackage[]
