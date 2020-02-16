BeginPackage["CodeTools`Generate`CreatePacletArchive`"]

Begin["`Private`"]

If[$VersionNumber < 12.1,
  Needs["PacletManager`"]
]

buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[FileType[buildDir] =!= Directory,
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

pacletFlagPosition = FirstPosition[$CommandLine, "-paclet"]

If[MissingQ[pacletFlagPosition],
  Print["Cannot proceed; Unsupported paclet"];
  Quit[1]
]

paclet = $CommandLine[[pacletFlagPosition[[1]] + 1]]


pacletDir = FileNameJoin[{buildDir, "paclet", paclet}]

If[FileType[pacletDir] =!= Directory,
  Print["Cannot proceed; Unsupported paclet directory"];
  Quit[1]
]


Print["Calling CreatePacletArchive..."]

If[$VersionNumber >= 12.1,
  res = System`CreatePacletArchive[pacletDir]
  ,
  res = PacletManager`PackPaclet[pacletDir]
]

Print[res]

Print["Done"]



End[]

EndPackage[]
