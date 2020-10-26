BeginPackage["CodeTools`Generate`CreatePacletArchive`"]

Begin["`Private`"]

Needs["CodeTools`Generate`GenerateSources`"]
If[$VersionNumber < 12.1,
  Needs["PacletManager`"]
]

pacletFlagPosition = FirstPosition[$CommandLine, "-paclet"]

paclet = $CommandLine[[pacletFlagPosition[[1]] + 1]]

pacletDir = FileNameJoin[{buildDir, "paclet", paclet}]


generate[] := (

If[MissingQ[pacletFlagPosition],
  Print["Cannot proceed; Unsupported paclet"];
  Quit[1]
];

If[!DirectoryQ[pacletDir],
  Print["Cannot proceed; Unsupported paclet directory"];
  Quit[1]
];

Print["Calling CreatePacletArchive..."];

If[$VersionNumber >= 12.1,
  res = System`CreatePacletArchive[pacletDir]
  ,
  res = PacletManager`PackPaclet[pacletDir]
];

Print[res];

Print["Done"]
)

If[script === $InputFileName,
generate[]
]

End[]

EndPackage[]
