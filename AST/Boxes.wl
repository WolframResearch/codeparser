BeginPackage["AST`Boxes`"]

Begin["`Private`"]

Needs["AST`"]


CSTToBoxes[SymbolNode[s_, {}, _]] := s

(*
Collapse all long names


Both:
FrontEndExecute[FrontEnd`UndocumentedTestFEParserPacket["\"a\[Alpha]b\""]]
and
FrontEndExecute[FrontEnd`UndocumentedTestFEParserPacket["\"a\\[Alpha]b\""]]
give the same output:
{BoxData["\"a\[Alpha]b\""], StandardForm}

to emulate this, we must collapse characters down first

Cannot just do ToString[ToExpression[s, InputForm], InputForm],
because not ALL characters are collapsed. \n is NOT collapsed.
So build up a fragile list of replacements instead
*)
CSTToBoxes[StringNode[s_ /; StringStartsQ[s, "\""], {}, _]] :=
  StringReplace[s, {"\\[Alpha]"->"\[Alpha]", "\\[Rule]"->"\[Rule]"}]

CSTToBoxes[StringNode[s_, {}, _]] := s

CSTToBoxes[NumberNode[s_, {}, _]] := s

CSTToBoxes[SlotNode[s_, {}, _]] := s

CSTToBoxes[SlotSequenceNode[s_, {}, _]] := s


CSTToBoxes[PrefixNode[op_, {arg_}, _]] := 
 RowBox[{StringTrim[SymbolToPrefixOperatorString[op]], CSTToBoxes[arg]}]

CSTToBoxes[PostfixNode[op_, {arg_}, _]] := 
 RowBox[{CSTToBoxes[arg], StringTrim[SymbolToPostfixOperatorString[op]]}]


CSTToBoxes[BinaryNode[LessEqual, {BinaryNode[LessEqual, {a_, b_}, _], c_}, _]] :=
	RowBox[{CSTToBoxes[a], "<=", CSTToBoxes[b], "<=", CSTToBoxes[c]}]

CSTToBoxes[BinaryNode[SameQ, {BinaryNode[SameQ, {a_, b_}, _], c_}, _]] :=
  RowBox[{CSTToBoxes[a], "===", CSTToBoxes[b], "===", CSTToBoxes[c]}]

(*
Match the buggy behavior in FE
bug 367063
*)
CSTToBoxes[BinaryNode[Optional, {PatternBlankNode[PatternBlank, {a_}, _], c_}, _]] :=
  RowBox[{CSTToBoxes[a]<>"_"<>":", CSTToBoxes[c]}]

CSTToBoxes[BinaryNode[Optional, {PatternBlankNode[PatternBlank, {a_, b_}, _], c_}, _]] :=
  RowBox[{CSTToBoxes[a]<>"_"<>CSTToBoxes[b]<>":", CSTToBoxes[c]}]



CSTToBoxes[BinaryNode[op_, {a_, b_}, _]] := 
 Replace[ReplaceAll[
   RowBox[{CSTToBoxes[a], StringTrim[SymbolToBinaryOperatorString[op]], 
     CSTToBoxes[b]}], {$toRemove -> Nothing}], {RowBox[{arg_}] :> arg}]



CSTToBoxes[InfixNode[Plus, args_, _]] := 
 RowBox[Flatten[{CSTToBoxes[First[args]], 
    Switch[#, _InternalMinusNode, {"-", 
        CSTToBoxes[#[[2]][[1]]]}, _, {"+", CSTToBoxes[#]}] & /@ Rest[args]}]]

CSTToBoxes[InfixNode[ImplicitTimes, args_, _]] := 
	RowBox[CSTToBoxes /@ args]

CSTToBoxes[InfixNode[op_, args_, _]] := 
 ReplaceAll[
  RowBox[Riffle[CSTToBoxes /@ (args), 
    StringTrim[SymbolToInfixOperatorString[op]]]], {$toRemove -> 
    Nothing}]

CSTToBoxes[TernaryNode[TernaryTilde, {a_, b_, c_}, _]] :=
	RowBox[{CSTToBoxes[a], "~", CSTToBoxes[b], "~", CSTToBoxes[c]}]


CSTToBoxes[InternalNullNode[Null, {}, _]] := $toRemove

CSTToBoxes[InternalAllNode[All, {}, _]] := $toRemove

CSTToBoxes[InternalOneNode[1, {}, _]] := $toRemove


CSTToBoxes[GroupNode[op_, {}, _]] := 
 Module[{pair = SymbolToGroupPair[op]}, RowBox[{pair[[1]], pair[[2]]}]]

CSTToBoxes[GroupNode[op_, {arg_}, _]] := 
 Module[{pair = SymbolToGroupPair[op]}, 
  RowBox[{pair[[1]], CSTToBoxes[arg], pair[[2]]}]]

CSTToBoxes[GroupNode[op_, args_, _]] := 
 Module[{pair = SymbolToGroupPair[op]}, 
  RowBox[{pair[[1]], RowBox[CSTToBoxes /@ args], pair[[2]]}]]

CSTToBoxes[InternalTokenNode[",", {}, _]] := ","


CSTToBoxes[CallNode[h_, {GroupNode[GroupSquare, {}, _]}, _]] := 
 RowBox[{CSTToBoxes[h], "[", "]"}]

CSTToBoxes[CallNode[h_, {GroupNode[GroupSquare, {arg_}, _]}, _]] := 
 RowBox[{CSTToBoxes[h], "[", CSTToBoxes[arg], "]"}]

CSTToBoxes[CallNode[h_, {GroupNode[GroupSquare, args_, _]}, _]] := 
 RowBox[{CSTToBoxes[h], "[", RowBox[CSTToBoxes /@ args], "]"}]



CSTToBoxes[BlankNode[Blank, {}, _]] := "_"

CSTToBoxes[BlankNode[Blank, {sym1_}, _]] := "_" <> CSTToBoxes[sym1]

CSTToBoxes[BlankSequenceNode[BlankSequence, {}, _]] := "__"

CSTToBoxes[BlankSequenceNode[BlankSequence, {sym1_}, _]] := "__" <> CSTToBoxes[sym1]

CSTToBoxes[BlankNullSequenceNode[BlankNullSequence, {}, _]] := "___"

CSTToBoxes[BlankNullSequenceNode[BlankNullSequence, {sym1_}, _]] := "___" <> CSTToBoxes[sym1]

CSTToBoxes[PatternBlankNode[PatternBlank, {sym1_}, _]] := CSTToBoxes[sym1] <> "_"

CSTToBoxes[PatternBlankNode[PatternBlank, {sym1_, sym2_}, _]] := CSTToBoxes[sym1] <> "_" <> CSTToBoxes[sym2]

CSTToBoxes[PatternBlankSequenceNode[PatternBlankSequence, {sym1_}, _]] := CSTToBoxes[sym1] <> "__"

CSTToBoxes[PatternBlankSequenceNode[PatternBlankSequence, {sym1_, sym2_}, _]] := CSTToBoxes[sym1] <> "__" <> CSTToBoxes[sym2]

CSTToBoxes[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_}, _]] := CSTToBoxes[sym1] <> "___"

CSTToBoxes[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym1_, sym2_}, _]] := CSTToBoxes[sym1] <> "___" <> CSTToBoxes[sym2]


End[]

EndPackage[]
