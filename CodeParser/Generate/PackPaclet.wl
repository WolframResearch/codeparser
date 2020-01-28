BeginPackage["CodeParser`Generate`PackPaclet`"]

Begin["`Private`"]

If[$VersionNumber < 12.1,
  Needs["PacletManager`"]
]

pacletDirFlagPosition = FirstPosition[$CommandLine, "-pacletDir"]

If[MissingQ[pacletDirFlagPosition],
  Print[OutputForm["Cannot proceed; Unsupported paclet directory"]];
  Quit[1]
]

pacletDir = $CommandLine[[pacletDirFlagPosition[[1]] + 1]]

If[FileType[pacletDir] =!= Directory,
  Print[OutputForm["Cannot proceed; Unsupported paclet directory"]];
  Quit[1]
]


Print[OutputForm["Calling PackPaclet..."]]

res = PackPaclet[pacletDir]

Print[res]

Print[OutputForm["Done"]]



End[]

EndPackage[]
