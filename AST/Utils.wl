BeginPackage["AST`Utils`"]

escapeString

(*
char
*)

empty

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



End[]

EndPackage[]
