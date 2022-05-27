
Needs["CodeParser`"]

Needs["PacletManager`"] (* for PacletInformation *)


(*

Test options

*)



(*
TODO: when targeting 12.1 as a minimum, then look into doing paclet["AssetLocation", "LibraryResources"] or similar
*)
location = "Location" /. PacletInformation["CodeParser"]

pacletInfoFile = FileNameJoin[{location, "PacletInfo.wl"}]

Block[{$ContextPath = {"System`"}, $Context = "Global`"},
	pacletInfo = Get[pacletInfoFile];
]

{useExprLib, useMathLink} = {UseExprLib, UseMathLink} /. List @@ pacletInfo;




TestMatch[
	CodeParse["Plot[f[x,y],{x,0,1},{y,0,1},PlotRange\[Rule]All];", SourceConvention -> "Test"]
	,
	_Failure
	,
	Which[
		useExprLib,
			{LibraryFunction::unevaluated}
		,
		useMathLink,
			{}
	]
	,
	TestID->"CodeParser-20200312-G4J9U7"
]



