BeginPackage["AST`DeclarationName`"]

Begin["`Private`"]

Needs["AST`"]

(*

given an LHS node, determine its declared name


DeclarationName will try to work with concrete syntax and abstract syntax


*)


DeclarationName[s_SymbolNode] := s[[1]]

DeclarationName[CallNode[node_, _, _]] := DeclarationName[node]






DeclarationName[BinaryNode[Condition, {node_, _}, _]] := DeclarationName[node]

DeclarationName[CallNode[SymbolNode["Condition", _, _], {node_, _}, _]] := DeclarationName[node]








(*
handle something like:
(pg_PointerGraph)["toGraph", opts___] := ToGraph[pg, opts]
*)
DeclarationName[GroupNode[GroupParen, {node_}, _]] := DeclarationName[node]






DeclarationName[PatternBlankNode[_, {node_, _}, _]] := DeclarationName[node]

DeclarationName[CallNode[SymbolNode["Pattern", _, _], {node_, _}, _]] := DeclarationName[node]




(*
DeclarationName[_StringNode] := String

DeclarationName[_SlotNode] := Slot

DeclarationName[_BlankNode] := Blank

DeclarationName[GroupNode[List, _, _]] := List

DeclarationName[InfixNode[Plus, _, _]] := Plus

DeclarationName[InfixNode[NonCommutativeMultiply, _, _]] := NonCommutativeMultiply

DeclarationName[InfixNode[Dot, _, _]] := Dot

DeclarationName[InfixNode[Alternatives, _, _]] := Alternatives

DeclarationName[BinaryNode[MessageName, _, _]] := MessageName

DeclarationName[BinaryNode[Rule, _, _]] := Rule

DeclarationName[BinaryNode[BinarySlashSlash, {_, node_}, _]] := DeclarationName[node]

DeclarationName[BinaryNode[Pattern, {node_, _}, _]] := DeclarationName[node]

DeclarationName[BinaryNode[BinaryAt, {node_, _}, _]] := DeclarationName[node]

DeclarationName[BinaryNode[PatternTest, {node_, _}, _]] := DeclarationName[node]

DeclarationName[PartNode[node_, _, _]] := DeclarationName[node]
*)

DeclarationName[args___] := Failure["Unhandled", <|"Function"->DeclarationName, "Arguments"->HoldForm[{args}]|>]


End[]

EndPackage[]
