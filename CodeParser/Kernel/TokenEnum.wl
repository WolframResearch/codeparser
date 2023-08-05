BeginPackage["CodeParser`TokenEnum`"]

tokenIsEmpty

Begin["`Private`"]

Needs["CodeParser`Library`"] (* For tokenIsEmptyFunc *)

tokenIsEmpty[tok_] := tokenIsEmptyFunc[tok]


End[]

EndPackage[]
