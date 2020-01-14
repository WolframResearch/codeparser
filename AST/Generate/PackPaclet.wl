
BeginPackage["AST`Generate`PackPaclet`"]

Begin["`Private`"]

If[$VersionNumber < 12.1,
  Needs["PacletManager`"]
]

pacletDirFlagPosition = FirstPosition[$CommandLine, "-pacletDir"]

If[MissingQ[pacletDirFlagPosition],
  Print["Cannot proceed; Unsupported paclet directory"];
  Quit[1]
]

pacletDir = $CommandLine[[pacletDirFlagPosition[[1]] + 1]]

If[FileType[pacletDir] =!= Directory,
  Print["Cannot proceed; Unsupported paclet directory"];
  Quit[1]
]


Print["Calling PackPaclet..."]

res = PackPaclet[pacletDir]

Print[res]

Print["Done"]



End[]

EndPackage[]


(*
Needed to guarantee exit code of 0 in older versions
*)
Quit[0]
