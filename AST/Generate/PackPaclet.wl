
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

Seen with v11.1 on Windows if not calling Quit[0]:
error MSB6006: "cmd.exe" exited with code -1073741819. [C:\Jenkins\workspace\Component.AST.Windows-x86-64.prototype\ast\build\paclet.vcxproj]
Related bugs: ?
*)
Quit[0]
