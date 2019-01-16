
Print["Building PacletInfo.m"]

sourcePosition = FirstPosition[$CommandLine, "-source"]

If[MissingQ[sourcePosition],
  Print["Cannot proceed; Unsupported sourcePosition"];
  Quit[1]
]

source = $CommandLine[[sourcePosition[[1]] + 1]]

builtPosition = FirstPosition[$CommandLine, "-built"]

If[MissingQ[builtPosition],
  Print["Cannot proceed; Unsupported builtPosition"];
  Quit[1]
]

built = $CommandLine[[builtPosition[[1]] + 1]]

pacletVersionPosition = FirstPosition[$CommandLine, "-pacletVersion"]

If[MissingQ[pacletVersionPosition],
  Print["Cannot proceed; Unsupported pacletVersionPosition"];
  Quit[1]
]

pacletVersion = $CommandLine[[pacletVersionPosition[[1]] + 1]]


Print["importing PacletInfo.m"]
importedPacletInfo = Get[source]

If[FailureQ[importedPacletInfo],
  Print[importedPacletInfo];
  Quit[1]
]

replacedPacletInfo = importedPacletInfo /. {(Version -> _) -> (Version -> pacletVersion)}


Print["exporting built PacletInfo.m"]
res = Put[replacedPacletInfo, built]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


Print["Done building PacletInfo.m"]
