
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Utils`"]


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












ast = CodeParse["

foo[x_]:=x+1

Module[{a}, a+1]

Module[{y},
  y + 2
]

Module[{b}, b+1]

bar[z_]:=z+3

"]


Test[
	ScopingData[ast]
	,
	{scopingDataObject[{{3, 1}, {3, 4}}, {"Defined"}, {"definition"}, "foo"],
	scopingDataObject[{{3, 10}, {3, 11}}, {"SetDelayed"}, {}, "x"],
	scopingDataObject[{{3, 5}, {3, 6}}, {"SetDelayed"}, {}, "x"],
	scopingDataObject[{{5, 13}, {5, 14}}, {"Module"}, {}, "a"],
	scopingDataObject[{{5, 9}, {5, 10}}, {"Module"}, {}, "a"],
	scopingDataObject[{{8, 3}, {8, 4}}, {"Module"}, {}, "y"],
	scopingDataObject[{{7, 9}, {7, 10}}, {"Module"}, {}, "y"],
	scopingDataObject[{{11, 13}, {11, 14}}, {"Module"}, {}, "b"],
	scopingDataObject[{{11, 9}, {11, 10}}, {"Module"}, {}, "b"],
	scopingDataObject[{{13, 1}, {13, 4}}, {"Defined"}, {"definition"}, "bar"],
	scopingDataObject[{{13, 10}, {13, 11}}, {"SetDelayed"}, {}, "z"],
	scopingDataObject[{{13, 5}, {13, 6}}, {"SetDelayed"}, {}, "z"]}
	,
	TestID->"Scoping-20220830-W8Q8Y1"
]

Test[
	ScopingData[ast, SourceMemberQ[#[[3, Key[Source]]], {8, 3}]&]
	,
	{scopingDataObject[{{8, 3}, {8, 4}}, {"Module"}, {}, "y"],
	scopingDataObject[{{7, 9}, {7, 10}}, {"Module"}, {}, "y"]}
	,
	TestID->"Scoping-20220830-X8E0N5"
]







