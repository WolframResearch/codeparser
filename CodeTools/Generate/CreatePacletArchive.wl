
If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeTools`Generate`CreatePacletArchive`"]

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeTools`Generate`GenerateSources`"];
]
If[$VersionNumber < 12.1,
  Needs["PacletManager`"]
]

checkBuildDir[]
checkPaclet[]
checkPacletLayoutDir[]


If[retry,
  (*
  CreatePacletArchive may be slow on RE machines, so allow re-trying if JLink connection timeout is hit

  Set $connectTimeout to some large value and cross fingers (default is 20)

  See: RE-515885
  *)
  Needs["JLink`"];
  JLink`InstallJava`Private`$connectTimeout = 300.0
]


generate[] := (

Print["Calling CreatePacletArchive..."];

If[$VersionNumber >= 12.1,
  res = System`CreatePacletArchive[FileNameJoin[{pacletLayoutDir, paclet}], FileNameJoin[{buildDir, "paclet"}]]
  ,
  res = PacletManager`PackPaclet[FileNameJoin[{pacletLayoutDir, paclet}], FileNameJoin[{buildDir, "paclet"}]]
];

Print[res];

If[!StringQ[res],
  Quit[1]
];

Print["Done CreatePacletArchive"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
