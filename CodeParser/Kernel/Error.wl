BeginPackage["CodeParser`Error`"]


reparseUnterminatedGroupNode

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
return: better UnterminatedGroupNode

Do not return the previous children, because they are useless any way.

But return the opener to make ToString stuff easier
*)

Options[reparseUnterminatedGroupNode] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  ContainerNode -> Automatic,
  "TabWidth" :> ("TabWidth" /. Options[CodeConcreteParse])
}

reparseUnterminatedGroupNode[{tag_, children_, dataIn_}, bytes_List, opts:OptionsPattern[]] :=
Catch[
Module[{lines, chunks, src, firstChunk, betterSrc, data, lastGoodLine, lastGoodLineIndex, str, betterLeaves, convention, test,
  takeSpecsOfLines, poss, tabWidth},

  If[$Debug,
    Print["reparseUnterminatedGroupNode: ", {{tag, children, dataIn}, bytes, opts}];
  ];

  convention = OptionValue[SourceConvention];
  tabWidth = OptionValue["TabWidth"];

  str = SafeString[bytes];

  If[FailureQ[str],
    Throw[str]
  ];

  (*
  lines is list of {line characters, newline or "" if end of str with no newline at the end}
  *)
  lines = StringCases[str, Shortest[line:___ ~~ newline:("\n" | "\r\n" | "\r" | EndOfString)] :> {line, newline}];

  lines = {replaceTabs[#[[1]], 1, #[[2]] /. "" -> "\n", tabWidth], #[[2]]}& /@ lines;

  If[$Debug,
    Print["lines: ", lines //InputForm];
  ];

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

      (*
      Include the newline at the end
      *)
      takeSpecsOfLines = {#[[1]] + 1, #[[2]]}& /@ Partition[FoldList[#1 + StringLength[#2[[1]]] + StringLength[#2[[2]]]&, 0, lines], 2, 1];

      If[$Debug,
        Print["takeSpecsOfLines: ", takeSpecsOfLines];
      ];

      test = (IntervalIntersection[Interval[#], Interval[src]] =!= Interval[])&;

      poss = Position[takeSpecsOfLines, _?test, {1}, Heads -> False];

      lines = Extract[lines, poss];
  ];

  If[$Debug,
    Print["lines: ", lines //InputForm];
  ];

  chunks = Split[lines, !StringMatchQ[#2[[1]], chunkPat]&];
  
  If[$Debug,
    Print["chunks: ", chunks //InputForm];
  ];

  firstChunk = chunks[[1]];
  
  lastGoodLineIndex = Length[firstChunk];
  lastGoodLine = firstChunk[[lastGoodLineIndex]];
  While[lastGoodLine[[1]] == "",
    lastGoodLineIndex--;
    lastGoodLine = firstChunk[[lastGoodLineIndex]];
  ];

  If[$Debug,
    Print["lastGoodLineIndex: ", lastGoodLineIndex];
    Print["lastGoodLine: ", lastGoodLine //InputForm];
  ];

  (*
  Use original src Start, but readjust src End to be the EndOfLine of the last good line of the chunk
  *)
  Switch[convention,
    "LineColumn",
      (*
      This will NOT include newline at the end
      FIXME?
      *)
      betterSrc = { src[[1]], { src[[1, 1]] + lastGoodLineIndex - 1, StringLength[lastGoodLine[[1]]]+1 } };

      data[Source] = betterSrc;

      srcMemberFunc = SourceMemberQ[{ betterSrc } ];

      (*
      Flatten out children, because there may be parsing errors from missing bracket, and
      we do not want to propagate
      *)
      betterLeaves = Cases[children, (LeafNode|ErrorNode)[_, _, data_ /; srcMemberFunc[data[Source]]], Infinity];
    ,
    "SourceCharacterIndex",
      (*
      This WILL include newline at the end
      FIXME?
      *)
      betterSrc = { src[[1]], takeSpecsOfLines[[poss[[1, 1]] + lastGoodLineIndex - 1, 2]] };

      data[Source] = betterSrc;

      (*
      Flatten out children, because there may be parsing errors from missing bracket, and
      we do not want to propagate
      *)
      betterLeaves = Cases[children, (LeafNode|ErrorNode)[_, _, data_ /; IntervalMemberQ[Interval[src], Interval[data[Source]]]], Infinity];
  ];

  (*
  Purposely only returning leaves that are in the "better" Source

  Rationale: there is not a useful purpose for returning the rest of the file, which may be massive.
  *)
  UnterminatedGroupNode[tag, betterLeaves, data]
]]


(*
return: better ErrorNode

Do not return the previous children, because they are useless any way.
*)

Options[reparseUnterminatedTokenErrorNode] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  ContainerNode -> Automatic,
  "TabWidth" :> ("TabWidth" /. Options[CodeConcreteParse])
}

reparseUnterminatedTokenErrorNode[{tok_, _, dataIn_}, bytes_List, OptionsPattern[]] :=
Catch[
Module[{lines, chunks, src, firstChunk, betterSrc, data, lastGoodLine, lastGoodLineIndex, str, convention, test,
  takeSpecsOfLines, poss, tabWidth, betterStr},

  convention = OptionValue[SourceConvention];
  tabWidth = OptionValue["TabWidth"];

  str = SafeString[bytes];

  If[FailureQ[str],
    Throw[str]
  ];

  (*
  lines is list of {line characters, newline or "" if end of str with no newline at the end}
  *)
  lines = StringCases[str, Shortest[line:___ ~~ newline:("\n" | "\r\n" | "\r" | EndOfString)] :> {line, newline}];

  lines = {replaceTabs[#[[1]], 1, #[[2]] /. "" -> "\n", tabWidth], #[[2]]}& /@ lines;

  If[$Debug,
    Print["lines: ", lines //InputForm];
  ];

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

      (*
      Include the newline at the end
      *)
      takeSpecsOfLines = {#[[1]] + 1, #[[2]]}& /@ Partition[FoldList[#1 + StringLength[#2[[1]]] + StringLength[#2[[2]]]&, 0, lines], 2, 1];

      If[$Debug,
        Print["takeSpecsOfLines: ", takeSpecsOfLines];
      ];

      test = (IntervalIntersection[Interval[#], Interval[src]] =!= Interval[])&;

      poss = Position[takeSpecsOfLines, _?test, {1}, Heads -> False];

      lines = Extract[lines, poss];
  ];
  
  If[$Debug,
    Print["lines: ", lines //InputForm];
  ];

  chunks = Split[lines, !StringMatchQ[#2[[1]], chunkPat]&];
  
  If[$Debug,
    Print["chunks: ", chunks //InputForm];
  ];

  firstChunk = chunks[[1]];
  
  lastGoodLineIndex = Length[firstChunk];
  lastGoodLine = firstChunk[[lastGoodLineIndex]];
  While[lastGoodLine[[1]] == "",
    lastGoodLineIndex--;
    lastGoodLine = firstChunk[[lastGoodLineIndex]];
  ];

  If[$Debug,
    Print["lastGoodLineIndex: ", lastGoodLineIndex];
    Print["lastGoodLine: ", lastGoodLine //InputForm];
  ];

  (*
  Use original src Start, but readjust src End to be the EndOfLine of the last good line of the chunk
  *)
  Switch[convention,
    "LineColumn",
      (*
      This will NOT include newline at the end
      FIXME?
      *)
      betterSrc = { src[[1]], { src[[1, 1]] + lastGoodLineIndex - 1, StringLength[lastGoodLine[[1]]]+1 } };

      data[Source] = betterSrc;

      (*
      Make sure to drop whatever characters are in the first line, but are before this token
      *)
      betterStr = StringJoin[Riffle[{StringDrop[firstChunk[[1, 1]], betterSrc[[1, 2]] - 1]} ~Join~ firstChunk[[2;;lastGoodLineIndex]][[All, 1]], "\n"]]
    ,
    "SourceCharacterIndex",
      (*
      This WILL include newline at the end
      FIXME?
      *)
      betterSrc = { src[[1]], takeSpecsOfLines[[poss[[1, 1]] + lastGoodLineIndex - 1, 2]] };

      data[Source] = betterSrc;

      betterStr = StringTake[str, betterSrc]
  ];

  (*
  Purposely only returning the characters that are in the "better" Source

  Rationale: there is not a useful purpose for returning the rest of the file, which may be massive.
  *)
  ErrorNode[tok, betterStr, data]
]]



End[]

EndPackage[]
