(* ::Package:: *)

BeginPackage["鳥物`"]
鳥言う::usage = "鳥言う[物] 鳥に何か言うように頼む"
Begin["`私的`"]
鳥言う[ア_] := ResourceFunction["BirdSay"][ア]
End[]
EndPackage[]
