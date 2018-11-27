
Print["Packing paclet"]

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

PackPaclet[pacletDir]

Print["Done packing paclet"]
