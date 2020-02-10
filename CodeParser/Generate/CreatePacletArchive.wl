BeginPackage["CodeTools`Generate`CreatePacletArchive`"]

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


Print[OutputForm["Calling CreatePacletArchive..."]]

If[$VersionNumber >= 12.1,
  res = System`CreatePacletArchive[pacletDir]
  ,
  res = PacletManager`PackPaclet[pacletDir]
]

Print[res]

Print[OutputForm["Done"]]



End[]

EndPackage[]
