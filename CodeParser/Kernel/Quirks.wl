(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`Quirks`"]

setupQuirks


$Quirks


processInfixBinaryAtQuirk


Begin["`Private`"]

Needs["CodeParser`"]



setupQuirks[] :=
Module[{},
	
	$Quirks = <||>;

	(*
	Setup "FlattenTimes" quirk

	In 12.1 and before:
		a / b / c is parsed as Times[a, Power[b, -1], Power[c, -1]]
		-a / b is parsed as Times[-1, a, Power[b, -1]]

	In 12.2 and after:
		a / b / c is parsed as Times[Times[a, Power[b, -1]], Power[c, -1]]
		-a / b is parsed as Times[Times[-1, a], Power[b, -1]]

	TODO: when targeting v12.2 as a minimum, remove this quirk

	Related bugs: 57064, 139531, 153875, 160919
	*)
	If[$VersionNumber <= 12.1,
		$Quirks["FlattenTimes"] = True
	];

	(*
	Setup "InfixBinaryAt" quirk

	The kernel parses  a<>StringJoin@b  as  StringJoin[a, b]

	Most infix operators can be used with this syntax.
	Notably, SameQ and UnsameQ do NOT work with this syntax.

	Related bugs: 365013
	*)
	$Quirks["InfixBinaryAt"] = True;

	(*
	changed in 13.1:
	@@@

	In 13.0 and before:
	a @@@ b parsed as Apply[a, b, {1}]

	In 13.1 and after:
	a @@@ b parses as MapApply[a, b]
	*)
	If[$VersionNumber <= 13.0,
		$Quirks["OldAtAtAt"] = True
	];
]




processInfixBinaryAtQuirk[
	BinaryNode[BinaryAt, {
		LeafNode[Symbol, symName_, symData_],
		LeafNode[Token`At, _, atData_], rhsIn_}, _]
	,
	symName_
] /; $Quirks["InfixBinaryAt"] :=
Module[{data, rhs, issues, synthesizedSource},

	rhs = rhsIn;

	data = rhs[[3]];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	synthesizedSource = {symData[[Key[Source], 1]], atData[[Key[Source], 2]]};

	AppendTo[issues, SyntaxIssue["InfixBinaryAtQuirk", "Unexpected parse.", "Remark", <| Source -> synthesizedSource, ConfidenceLevel -> 1.0 |>]];

	AssociateTo[data, AbstractSyntaxIssues -> issues];

	rhs[[3]] = data;

	rhs
]

processInfixBinaryAtQuirk[node_, _] := node




End[]

EndPackage[]
