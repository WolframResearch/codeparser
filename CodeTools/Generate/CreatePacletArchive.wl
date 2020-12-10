
If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

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

If[!StringQ[res],
  Quit[1]
];

Print["Done"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
