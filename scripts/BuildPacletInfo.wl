
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

replacedPacletInfo = importedPacletInfo /. {
  (Version -> _) -> (Version -> pacletVersion),
  (*
  Use $VersionNumber of build system
  *)
  (WolframVersion -> _) -> (WolframVersion -> ToString[$VersionNumber]<>"+")
}

builtPacletInfoM = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)
"} ~Join~ { ToString[replacedPacletInfo, InputForm, PageWidth->80] } ~Join~ {""}

Print["exporting built PacletInfo.m"]
res = Export[built, Column[builtPacletInfoM], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


Print["Done building PacletInfo.m"]
