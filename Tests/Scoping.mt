
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]


ast = CodeParse["Module[{x, y}, Block[{x, z}, x]]"];

(*
Test that x is not marked as unused in the Module

bug 414554
*)
Test[
	ScopingData[ast]
	,
	{
 scopingDataObject[{{1, 30}, {1, 31}}, {"Module", "Block"}, {"shadowed"}, "x"], 
 scopingDataObject[{{1, 23}, {1, 24}}, {"Module", "Block"}, {"shadowed"}, "x"], 
 scopingDataObject[{{1, 9}, {1, 10}}, {"Module"}, {}, "x"], 
 scopingDataObject[{{1, 26}, {1, 27}}, {"Block"}, {"unused"}, "z"], 
 scopingDataObject[{{1, 12}, {1, 13}}, {"Module"}, {"unused"}, "y"]}
	,
	TestID->"Scoping-20210921-U4U6T2"
]






box = RowBox[{SuperscriptBox["u", 
     TagBox[RowBox[{"(", RowBox[{"dx_", ",", "0"}], ")"}], 
      Derivative]], "\[RuleDelayed]", "a"}];

cst = CodeConcreteParseBox[box];

agg = CodeParser`Abstract`Aggregate[cst];

ast = CodeParser`Abstract`Abstract[agg];

Test[
	ScopingData[ast]
	,
	{scopingDataObject[{1, 1, 2, 1, 1, 2, 1, 1}, {"RuleDelayed"}, {"unused"}, "dx"]}
	,
	TestID->"Scoping-20220211-E8N5O8"
]






ast = CodeParse["foo[] := \\!\\(\\*s\\)"]

Test[
	ScopingData[ast]
	,
	{scopingDataObject[{{1, 1}, {1, 4}}, {"Defined"}, {"definition"}, "foo"]}
	,
	TestID->"Scoping-20220316-D3G1W4"
]





