BeginPackage["CodeParser`Node`"]

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
Some selectors
*)

LeafNode[_, str_, _]["String"] := str



(*
Some attributes
*)

Attributes[CodeNode] = {HoldAllComplete}



(*
ToNode[sym] returns a LeafNode[Symbol]
*)
ToNode[s_Symbol] :=
  If[Context[s] == "System`",
    LeafNode[Symbol, SymbolName[s], <||>]
    ,
    (*
    Play it safe for now and fully qualify any non-System` symbol
    *)
    LeafNode[Symbol, Context[s]<>SymbolName[s], <||>]
  ]

(*
ToNode[string] returns a LeafNode[String]
*)
ToNode[s_String] := LeafNode[String, escapeString[s], <||>]

(*
ToNode[integer] returns a LeafNode[Integer]
ToNode[real] returns a LeafNode[Real]
*)
ToNode[i_Integer] := LeafNode[Integer, ToString[i], <||>]
ToNode[r_Real] := LeafNode[Real, ToString[r, InputForm], <||>]

(*
ToNode[rational] returns:
  if possible to convert to Rational literal then return LeafNode[Rational]
  otherwise, return CallNode[Rational]
*)
ToNode[r_Rational] :=
Catch[
Module[{num, den, e},
  (*
  TODO: when targeting 12.0 as a minimum, use NumeratorDenominator[r]
  *)
  {num, den} = {Numerator[r], Denominator[r]};
  (*
  loop between 2 and 36 and test if the base works

  loop from 36 to 2, going down

  out of all of these ways of constructing 1/16:
  2^^1*^-4
  4^^1*^-2
  8^^4*^-2
  16^^1*^-1

  prefer to do 16^^1*^-1
  that is, prefer the highest base
  *)
  Do[
    e = IntegerExponent[den, b];
    If[e != 0,
     Throw[LeafNode[Rational, ToString[b] <> "^^" <> IntegerString[num, b] <> "*^-" <> ToString[e], <||>]]
    ]
    ,
    {b, 36, 2, -1}
  ];
  CallNode[LeafNode[Symbol, "Rational", <||>], {ToNode[num], ToNode[den]}, <||>]
]]


ToNode[args___] := Failure["Unhandled", <| "Function" -> ToNode, "Arguments" -> HoldForm[{args}] |>]



FromNode[LeafNode[Symbol, s_, _]] :=
  Symbol[s]

(*
No simple way to convert "\"123\"" to "123"
*)
FromNode[LeafNode[String, s_, _]] :=
  ToExpression[s]

(*
No simple way to convert "123.456``7" to 123.456``7
*)
FromNode[LeafNode[Integer, i_, _]] :=
  ToExpression[i]

FromNode[LeafNode[Real, r_, _]] :=
  ToExpression[r]

FromNode[LeafNode[Rational, r_, _]] :=
  ToExpression[r]


FromNode[args___] := Failure["InternalUnhandled", <| "Function" -> FromNode, "Arguments" -> HoldForm[{args}] |>]



End[]

EndPackage[]
