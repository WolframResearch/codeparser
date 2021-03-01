BeginPackage["CodeParser`Definitions`"]


DefinitionSymbols


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
given an LHS AST node, determine the symbol that gives the definition
*)

DefinitionSymbols[n:LeafNode[Symbol, _, _]] := {n}

DefinitionSymbols[LeafNode[_, _, _]] := {}


DefinitionSymbols[CallNode[LeafNode[Symbol, "Condition", _], {node_, _}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "Pattern", _], {_, node_}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "PatternTest", _], {node_, _}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "HoldPattern", _], {node_}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "MessageName", _], {node_, _, ___}, _]] := DefinitionSymbols[node]

DefinitionSymbols[CallNode[LeafNode[Symbol, "Blank", _], {node_}, _]] := DefinitionSymbols[node]

(*
Something like a /: (b|c)[a] := d

When scanning over (b|c)[a], we want to treat both b and c as definitions
*)
DefinitionSymbols[CallNode[LeafNode[Symbol, "Alternatives", _], children_, _]] := Flatten[DefinitionSymbols /@ children]

DefinitionSymbols[CallNode[node_, _, _]] := DefinitionSymbols[node]


DefinitionSymbols[args___] := Failure["InternalUnhandled", <|"Function"->DefinitionSymbols, "Arguments"->{args}|>]




(*
DeclarationName is appropriate for when you want a single name string

If there are 0 names or if there is more than 1 name, then a Failure is returned
*)
DeclarationName[node_] :=
Catch[
Module[{syms},
  
  syms = DefinitionSymbols[node];

  If[empty[syms],
    Throw[Failure["NoDefinitions", <| "Node" -> node |>]]
  ];

  If[Length[syms] > 1,
    Throw[Failure["TooManyDefinitions", <| "Node" -> node |>]]
  ];

  syms[[1, 2]]
]]



End[]

EndPackage[]
