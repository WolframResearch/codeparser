BeginPackage["CodeParser`Definitions`"]

Begin["`Private`"]

Needs["CodeParser`"]


(*

given an LHS node, determine its declared name

DeclarationName will try to work with concrete syntax, aggregate syntax and abstract syntax
*)

DeclarationName[LeafNode[Symbol, s_, _]] := s

DeclarationName[CallNode[LeafNode[Symbol, "HoldPattern", _], {GroupNode[GroupSquare, {_, node_, _}, _]}, _]] := DeclarationName[node]

DeclarationName[CallNode[LeafNode[Symbol, "Condition", _], {node_, _}, _]] := DeclarationName[node]
DeclarationName[CallNode[LeafNode[Symbol, "Pattern", _], {_, node_}, _]] := DeclarationName[node]
DeclarationName[CallNode[LeafNode[Symbol, "PatternTest", _], {node_, _}, _]] := DeclarationName[node]
DeclarationName[CallNode[LeafNode[Symbol, "HoldPattern", _], {node_}, _]] := DeclarationName[node]
DeclarationName[CallNode[LeafNode[Symbol, "MessageName", _], _, _]] := "MessageName"

DeclarationName[CallNode[{node_, ___}, _, _]] := DeclarationName[node]

DeclarationName[CallNode[node_, _, _]] := DeclarationName[node]

DeclarationName[BinaryNode[Condition, {node_, _, _}, _]] := DeclarationName[node]
DeclarationName[BinaryNode[Pattern, {_, _, node_}, _]] := DeclarationName[node]
DeclarationName[BinaryNode[BinaryAt, {node_, _, _}, _]] := DeclarationName[node]
DeclarationName[BinaryNode[PatternTest, {node_, _, _}, _]] := DeclarationName[node]

DeclarationName[InfixNode[MessageName, _, _]] := "MessageName"

DeclarationName[GroupNode[GroupParen, {_, node_, _}, _]] := DeclarationName[node]

DeclarationName[CompoundNode[PatternBlank, {_, node_}, _]] := DeclarationName[node]
DeclarationName[CompoundNode[Blank, {_, node_}, _]] := DeclarationName[node]

DeclarationName[args___] := Failure["InternalUnhandled", <|"Function"->DeclarationName, "Arguments"->{args}|>]


End[]

EndPackage[]
