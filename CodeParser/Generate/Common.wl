BeginPackage["CodeParser`Generate`Common`"]

toGlobal

generatedCPPDir
generatedCPPIncludeDir
generatedCPPSrcDir

dataDir

importedPrefixParselets

importedInfixParselets

importedLongNames

importedPrecedenceSource

importedTokenEnumSource

Begin["`Private`"]

Needs["CodeTools`Generate`GenerateSources`"]


(*
uppercases and replaces ` with _
*)
toGlobal[n_] :=
  StringReplace[ToUpperCase[ToString[n]], {"`" -> "_", "$" -> "_"}]

generatedCPPDir = FileNameJoin[{buildDir, "generated", "cpp"}]
generatedCPPIncludeDir = FileNameJoin[{generatedCPPDir, "include"}]
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir, "src", "lib"}]

dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]

importedPrefixParselets := importedPrefixParselets = Get[FileNameJoin[{dataDir, "PrefixParselets.wl"}]]

importedInfixParselets := importedInfixParselets = Get[FileNameJoin[{dataDir, "InfixParselets.wl"}]]

importedLongNames := importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]

importedPrecedenceSource := importedPrecedenceSource = Get[FileNameJoin[{dataDir, "Precedence.wl"}]]

importedTokenEnumSource := importedTokenEnumSource = Get[FileNameJoin[{dataDir, "TokenEnum.wl"}]]

End[]

EndPackage[]
