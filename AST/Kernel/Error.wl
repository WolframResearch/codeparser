BeginPackage["AST`Error`"]


reparseMissingCloserNode


Begin["`Private`"]

Needs["AST`"]


(*
Annotations

::Package::
etc.

https://mathematica.stackexchange.com/questions/76192/annotation-specifier-like-author-and-sectioning-like-section-in-package-co
*)
annotationPat = "^\\(\\* *:.*$"

(*
Common functions like:

Begin["`Foo`"]

*)
directivePat = "^(BeginPackage|Begin|Needs|End|EndPackage|Clear|ClearAll|SetOptions|SetAttributes|System`Private`NewContextPath|System`Private`RestoreContextPath|Protect|Unprotect|Package|PackageImport|PackageScope|PackageExport|Get|SetDelayed|UpSetDelayed|TagSetDelayed)\\[.*$"

(*
Assignments like:

x = 1

*)
assignmentPat = "^[a-zA-Z$].*(:?\\=).*$"



chunkPat = RegularExpression["("<>annotationPat<>")|("<>directivePat<>")|("<>assignmentPat<>")"]


(*
return: better GroupMissingCloserNode
*)
reparseMissingCloserNode[GroupMissingCloserNode[tag_, _, dataIn_], bytes_List] :=
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

  GroupMissingCloserNode[tag, {}, data]
]




End[]

EndPackage[]
