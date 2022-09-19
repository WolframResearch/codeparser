
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

Block[{$ContextPath = {"PacletManager`", "System`"}, $Context = "Global`"},
	(*
	put PacletManager` on $ContextPath to guarantee using PacletManager`Paclet symbol
	*)
	pacletInfo = Get[pacletInfoFile];
]

transport = Transport /. List @@ pacletInfo;




TestMatch[
	CodeParse["Plot[f[x,y],{x,0,1},{y,0,1},PlotRange\[Rule]All];", SourceConvention -> "Test"]
	,
	_Failure
	,
	Which[
		transport === "ExprLib",
			{LibraryFunction::unevaluated}
		,
		transport === "MathLink",
			{}
	]
	,
	TestID->"CodeParser-20200312-G4J9U7"
]



Test[
	CodeParse["\\[Pi]"]
	,
	ContainerNode[String, {
		LeafNode[Symbol, "Pi", <|Source->{{1,1}, {1,6}}|>]}, <|Source->{{1,1}, {1,6}}|>]
	,
	TestID->"CodeParser-20220910-I3Q6U1"
]

Test[
	CodeParse["\\[Degree]"]
	,
	ContainerNode[String, {
		LeafNode[Symbol, "Degree", <|Source->{{1,1}, {1,10}}|>]}, <|Source->{{1,1}, {1,10}}|>]
	,
	TestID->"CodeParser-20220910-Z3K4F3"
]

Test[
	CodeParse["\\[Infinity]"]
	,
	ContainerNode[String, {
		LeafNode[Symbol, "Infinity", <|Source->{{1,1}, {1,12}}|>]}, <|Source->{{1,1}, {1,12}}|>]
	,
	TestID->"CodeParser-20220910-T2T3W7"
]

Test[
	CodeParse["\\[ExponentialE]"]
	,
	ContainerNode[String, {
		LeafNode[Symbol, "E", <|Source->{{1,1}, {1,16}}|>]}, <|Source->{{1,1}, {1,16}}|>]
	,
	TestID->"CodeParser-20220910-H2B2B6"
]

Test[
	CodeParse["\\[ImaginaryI]"]
	,
	ContainerNode[String, {
		LeafNode[Symbol, "I", <|Source->{{1,1}, {1,14}}|>]}, <|Source->{{1,1}, {1,14}}|>]
	,
	TestID->"CodeParser-20220910-M6R5R1"
]

Test[
	CodeParse["\\[ImaginaryJ]"]
	,
	ContainerNode[String, {
		LeafNode[Symbol, "I", <|Source->{{1,1}, {1,14}}|>]}, <|Source->{{1,1}, {1,14}}|>]
	,
	TestID->"CodeParser-20220910-C4S7C2"
]


