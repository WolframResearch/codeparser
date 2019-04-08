BeginPackage["AST`Utils`"]

escapeString

(*
char
*)

empty

SourceMemberQ

Begin["`Private`"]

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
SourceMemberQ[{{1,3},{2,0}}, {1,4}] => True
SourceMemberQ[{{1,3},{2,0}}, {2,4}] => False
*)
SourceMemberQ[sourceSpec_, {cursorLine_, cursorCol_}] :=
Which[
	!(sourceSpec[[1, 1]] <= cursorLine <= sourceSpec[[1, 1]]),
	False
	,
	cursorLine == sourceSpec[[1, 1]] == sourceSpec[[2, 1]],
	sourceSpec[[1, 2]] <= cursorCol <= sourceSpec[[2, 2]]
	,
	cursorLine == sourceSpec[[1, 1]],
	sourceSpec[[1, 2]] <= cursorCol
	,
	cursorLine == sourceSpec[[2, 1]],
	cursorCol <= sourceSpec[[2, 2]]
	,
	(* in-between start and end, so yes *)
	True,
	True
]





End[]

EndPackage[]
