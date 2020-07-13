BeginPackage["CodeParser`Error`"]


reparseMissingCloserNode

reparseUnterminatedTokenErrorNode


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



chunkPat = RegularExpression["(?m)("<>annotationPat<>")|("<>directivePat<>")|("<>assignmentPat<>")"]


(*
return: better GroupMissingCloserNode

Do not return the previous children, because they are useless any way.

But return the opener to make ToString stuff easier
*)

Options[reparseMissingCloserNode] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  ContainerNode -> Automatic
}

reparseMissingCloserNode[{tag_, children_, dataIn_}, bytes_List, OptionsPattern[]] :=
Catch[
Module[{lines, chunks, src, firstChunk, betterSrc, data, lastGoodLine, lastGoodLineIndex, str, leaves, convention, test,
  lineLens, takeSpecsOfLines, poss},

  convention = OptionValue[SourceConvention];

  str = SafeString[bytes];

  If[FailureQ[str],
    Throw[str]
  ];

  lines = StringSplit[str, {"\r\n", "\n", "\r"}, All];

  data = dataIn;
  src = data[Source];

  Switch[convention,
    "LineColumn",
      (*
      lines of the node
      *)
      lines = lines[[src[[1, 1]];;src[[2, 1]]]];
    ,
    "SourceCharacterIndex",
      
      lineLens = StringLength /@ lines;

      takeSpecsOfLines = {#[[1]] + 1, #[[2]]}& /@ Partition[FoldList[Plus, 0, lineLens + 1], 2, 1];

      test = (IntervalIntersection[Interval[#], Interval[src]] =!= Interval[])&;

      poss = Position[takeSpecsOfLines, _?test, {1}, Heads -> False];

      lines = Extract[lines, poss];
  ];

  chunks = Split[lines, !StringMatchQ[#2, chunkPat]&];
  
  firstChunk = chunks[[1]];
  
  lastGoodLineIndex = Length[firstChunk];
  lastGoodLine = firstChunk[[lastGoodLineIndex]];
  While[lastGoodLine == "",
    lastGoodLineIndex--;
    lastGoodLine = firstChunk[[lastGoodLineIndex]];
  ];

  (*
  Use original src Start, but readjust src End to be the EndOfLine of the last good line of the chunk
  *)
  Switch[convention,
    "LineColumn",
      betterSrc = { src[[1]], { src[[1, 1]] + lastGoodLineIndex - 1, StringLength[lastGoodLine]+1 } };

      data[Source] = betterSrc;

      srcMemberFunc = SourceMemberQ[{ betterSrc } ];

      (*
      Flatten out children, because there may be parsing errors from missing bracket, and
      we do not want to propagate
      *)
      leaves = Cases[children, (LeafNode|ErrorNode)[_, _, data_ /; srcMemberFunc[data[Source]]], Infinity];
    ,
    "SourceCharacterIndex",
      betterSrc = { src[[1]], takeSpecsOfLines[[poss[[1, 1]] + lastGoodLineIndex - 1, 2]] };

      data[Source] = betterSrc;

      (*
      Flatten out children, because there may be parsing errors from missing bracket, and
      we do not want to propagate
      *)
      leaves = Cases[children, (LeafNode|ErrorNode)[_, _, data_ /; IntervalMemberQ[Interval[src], Interval[data[Source]]]], Infinity];
  ];

  GroupMissingCloserNode[tag, leaves, data]
]]


(*
return: better ErrorNode

Do not return the previous children, because they are useless any way.
*)

Options[reparseUnterminatedTokenErrorNode] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  ContainerNode -> Automatic
}

reparseUnterminatedTokenErrorNode[{tok_, _, dataIn_}, bytes_List, OptionsPattern[]] :=
Catch[
Module[{lines, chunks, src, firstChunk, betterSrc, data, lastGoodLine, lastGoodLineIndex, str, convention, test,
  lineLens, takeSpecsOfLines, poss},

  convention = OptionValue[SourceConvention];

  str = SafeString[bytes];

  If[FailureQ[str],
    Throw[str]
  ];

  lines = StringSplit[str, {"\r\n", "\n", "\r"}, All];

  data = dataIn;

  src = data[Source];
  
  Switch[convention,
    "LineColumn",
      (*
      lines of the node
      *)
      lines = lines[[src[[1, 1]];;src[[2, 1]]]];
    ,
    "SourceCharacterIndex",
      
      lineLens = StringLength /@ lines;

      takeSpecsOfLines = {#[[1]] + 1, #[[2]]}& /@ Partition[FoldList[Plus, 0, lineLens + 1], 2, 1];

      test = (IntervalIntersection[Interval[#], Interval[src]] =!= Interval[])&;

      poss = Position[takeSpecsOfLines, _?test, {1}, Heads -> False];

      lines = Extract[lines, poss];
  ];
  
  chunks = Split[lines, !StringMatchQ[#2, chunkPat]&];
  
  firstChunk = chunks[[1]];
  
  lastGoodLineIndex = Length[firstChunk];
  lastGoodLine = firstChunk[[lastGoodLineIndex]];
  While[lastGoodLine == "",
    lastGoodLineIndex--;
    lastGoodLine = firstChunk[[lastGoodLineIndex]];
  ];

  (*
  Use original src Start, but readjust src End to be the EndOfLine of the last good line of the chunk
  *)
  Switch[convention,
    "LineColumn",
      betterSrc = { src[[1]], { src[[1, 1]] + lastGoodLineIndex - 1, StringLength[lastGoodLine]+1 } };

      data[Source] = betterSrc;
    ,
    "SourceCharacterIndex",
      betterSrc = { src[[1]], takeSpecsOfLines[[poss[[1, 1]] + lastGoodLineIndex - 1, 2]] };

      data[Source] = betterSrc;
  ];

  (*
  deliberately empty content
  *)
  ErrorNode[tok, "", data]
]]



End[]

EndPackage[]
