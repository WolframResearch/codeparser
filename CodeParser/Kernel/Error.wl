BeginPackage["CodeParser`Error`"]


reparseMissingCloserNode

reparseUnterminatedCommentErrorNode


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
Annotation Comments

::Package::
etc.

https://mathematica.stackexchange.com/questions/76192/annotation-specifier-like-author-and-sectioning-like-section-in-package-co
*)
annotationPat = "^\\(\\* *:.*$"

(*
Common functions like:

Begin["`Foo`"]

*)
directivePat = "^(\
BeginPackage|\
Begin|\
Needs|\
End|\
EndPackage|\
Clear|\
ClearAll|\
SetOptions|\
SetAttributes|\
System`Private`NewContextPath|\
System`Private`RestoreContextPath|\
Protect|\
Unprotect|\
Package|\
PackageImport|\
PackageScope|\
PackageExport|\
Get|\
SetDelayed|\
UpSetDelayed|\
TagSetDelayed\
)\\[.*$"

(*
Assignments like:

x = 1

*)
assignmentPat = "^[a-zA-Z$].*(:?\\=).*$"



chunkPat = RegularExpression["("<>annotationPat<>")|("<>directivePat<>")|("<>assignmentPat<>")"]


(*
return: better GroupMissingCloserNode

Do not return the previous children, because they are uselss any way.

But return the opener to make ToString stuff easier
*)
reparseMissingCloserNode[GroupMissingCloserNeedsReparseNode[tag_, children_, dataIn_], bytes_List] :=
Module[{lines, chunks, src, firstChunk, betterSrc, data, lastGoodLine, lastGoodLineIndex, str, leaves,
  ignoredNodesSrcMemberFunc},

  str = SafeString[bytes];

  lines = StringSplit[str, {"\r\n", "\n", "\r"}, All];

  data = dataIn;
  src = data[Source];
  (*
  lines of the node
  *)
  lines = lines[[src[[1, 1]];;src[[2, 1]] ]];
  
  chunks = Split[lines, !StringMatchQ[#2, chunkPat]&];
  
  firstChunk = chunks[[1]];
  
  lastGoodLineIndex = Length[firstChunk];
  lastGoodLine = firstChunk[[lastGoodLineIndex]];
  While[lastGoodLine == "",
    lastGoodLineIndex--;
    lastGoodLine = firstChunk[[lastGoodLineIndex]];
  ];

  betterSrc = { src[[1]], { src[[1, 1]] + lastGoodLineIndex - 1, StringLength[lastGoodLine]+1 } };

  data[Source] = betterSrc;

  srcMemberFunc = SourceMemberQ[{ betterSrc } ];

  (*
  Flatten out children, because there may be parsing errors from missing bracket, and
  we do not want to propagate
  *)
  leaves = Cases[children, LeafNode[_, _, data_ /; srcMemberFunc[data[Source]]], Infinity];

  GroupMissingCloserNode[tag, leaves, data]
]


(*
return: better ErrorNode

Do not return the previous children, because they are uselss any way.
*)
reparseUnterminatedCommentErrorNode[ErrorNode[Token`Error`UnterminatedComment, _, dataIn_], bytes_List] :=
Module[{lines, chunks, src, firstChunk, betterSrc, data, lastGoodLine, lastGoodLineIndex, str},

  str = SafeString[bytes];

  lines = StringSplit[str, {"\r\n", "\n", "\r"}, All];

  data = dataIn;
  src = data[Source];
  (*
  lines of the node
  *)
  lines = lines[[src[[1, 1]];;src[[2, 1]] ]];
  
  chunks = Split[lines, !StringMatchQ[#2, chunkPat]&];
  
  firstChunk = chunks[[1]];
  
  lastGoodLineIndex = Length[firstChunk];
  lastGoodLine = firstChunk[[lastGoodLineIndex]];
  While[lastGoodLine == "",
    lastGoodLineIndex--;
    lastGoodLine = firstChunk[[lastGoodLineIndex]];
  ];

  betterSrc = { src[[1]], { src[[1, 1]] + lastGoodLineIndex - 1, StringLength[lastGoodLine]+1 } };

  data[Source] = betterSrc;

  ErrorNode[Token`Error`UnterminatedComment, "", data]
]



End[]

EndPackage[]
