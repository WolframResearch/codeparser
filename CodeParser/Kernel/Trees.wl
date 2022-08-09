BeginPackage["CodeParser`Trees`"]

ToTree

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



ToTree[parseTree_] :=
  NestTree[codeChildren, parseTree, Infinity, codeData]



codeChildren[head_[tag_, children_, data_]] := children

codeData[head_[tag_, children_, data_]] :=
  {head, tag, data}



codeChildren[LeafNode[tag_, str_, data_]] := None

codeData[LeafNode[tag_, str_, data_]] :=
  {LeafNode, tag, str, data}


codeChildren[ErrorNode[tag_, str_, data_]] := None

codeData[ErrorNode[tag_, str_, data_]] :=
  {ErrorNode, tag, str, data}


End[]

EndPackage[]
