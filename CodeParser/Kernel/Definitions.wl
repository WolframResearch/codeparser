BeginPackage["CodeParser`Definitions`"]


DefinitionSymbols


Begin["`Private`"]

Needs["CodeParser`"]


(*
given an LHS node, determine the symbol that gives the definition
*)

DefinitionSymbols[n:LeafNode[Symbol, _, _]] := {n}

DefinitionSymbols[LeafNode[_, _, _]] := {}


DefinitionSymbols[CallNode[LeafNode[Symbol, "Condition", _], {node_, _}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "Pattern", _], {_, node_}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "PatternTest", _], {node_, _}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "HoldPattern", _], {node_}, _]] := DefinitionSymbols[node]
DefinitionSymbols[CallNode[LeafNode[Symbol, "MessageName", _], {node_, _, ___}, _]] := DefinitionSymbols[node]

DefinitionSymbols[CallNode[LeafNode[Symbol, "Blank", _], {node_}, _]] := DefinitionSymbols[node]

DefinitionSymbols[CallNode[LeafNode[Symbol, "Alternatives", _], children_, _]] := Flatten[DefinitionSymbols /@ children]

DefinitionSymbols[CallNode[node_, _, _]] := DefinitionSymbols[node]


DefinitionSymbols[args___] := Failure["InternalUnhandled", <|"Function"->DefinitionSymbols, "Arguments"->{args}|>]


End[]

EndPackage[]
