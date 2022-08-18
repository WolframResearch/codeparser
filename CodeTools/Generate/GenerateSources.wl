BeginPackage["CodeTools`Generate`GenerateSources`"]

buildDirFlagPosition

buildDir

srcDirFlagPosition

srcDir

script

generatedWLDir

pacletFlagPosition

paclet

retryFlagPosition

retry

pacletLayoutDirFlagPosition

pacletLayoutDir


checkBuildDir

checkSrcDir

checkPaclet

checkPacletLayoutDir


Begin["`Private`"]

buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

buildDir := buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

srcDirFlagPosition = FirstPosition[$CommandLine, "-srcDir"]

srcDir := srcDir = $CommandLine[[srcDirFlagPosition[[1]] + 1]]

scriptPosition = FirstPosition[$CommandLine, "-script"]

script := script = $CommandLine[[scriptPosition[[1]] + 1]]

generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]

pacletFlagPosition = FirstPosition[$CommandLine, "-paclet"]

paclet := paclet = $CommandLine[[pacletFlagPosition[[1]] + 1]]

retryFlagPosition = FirstPosition[$CommandLine, "-retry"]

retry = !MissingQ[retryFlagPosition]

pacletLayoutDirFlagPosition = FirstPosition[$CommandLine, "-pacletLayoutDir"]

pacletLayoutDir := pacletLayoutDir = $CommandLine[[pacletLayoutDirFlagPosition[[1]] + 1]]


checkBuildDir[] :=
Module[{},
  If[MissingQ[buildDirFlagPosition],
    Print["Cannot proceed; buildDir flag missing"];
    Quit[1]
  ];

  If[!DirectoryQ[buildDir],
    Print["Cannot proceed; Unsupported buildDir: ", buildDir];
    Quit[1]
  ];
]


checkSrcDir[] :=
Module[{},
  If[MissingQ[srcDirFlagPosition],
    Print["Cannot proceed; srcDir flag missing"];
    Quit[1]
  ];

  If[!DirectoryQ[srcDir],
    Print["Cannot proceed; Unsupported srcDir: ", srcDir];
    Quit[1]
  ];
]


checkPaclet[] :=
Module[{},
  If[MissingQ[pacletFlagPosition],
    Print["Cannot proceed; paclet flag missing"];
    Quit[1]
  ];
]


checkPacletLayoutDir[] :=
Module[{},
  If[MissingQ[pacletLayoutDirFlagPosition],
    Print["Cannot proceed; pacletLayoutDir flag missing"];
    Quit[1]
  ];

  If[!DirectoryQ[pacletLayoutDir],
    Print["Cannot proceed; Unsupported pacletLayoutDir: ", pacletLayoutDir];
    Quit[1]
  ];

  If[FileNameTake[pacletLayoutDir, -1] =!= "paclet",
    Print["Cannot proceed; Unsupported pacletLayoutDir: ", pacletLayoutDir];
    Quit[1]
  ]; 
]

End[]

EndPackage[]
