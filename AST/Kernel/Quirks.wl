BeginPackage["AST`Quirks`"]

setupQuirks


$Quirks

Begin["`Private`"]

Needs["AST`"]



setupQuirks[] :=
Module[{},
	
	$Quirks = <||>;

	(*
	Setup "FlattenTimes" quirk

	In non-Prototype builds:
		a / b / c is parsed as Times[a, Power[b, -1], Power[c, -1]]
		-a / b is parsed as Times[-1, a, Power[b, -1]]

	In Prototype builds:
		a / b / c is parsed as Times[Times[a, Power[b, -1]], Power[c, -1]]
		-a / b is parsed as Times[Times[-1, a], Power[b, -1]]
	This is considered the correct behavior going into the future.

	This is setup on bugfix/139531_et_al branch
	Related bugs: 139531, 160919
	*)
	If[!Internal`$PrototypeBuild,
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
]




End[]

EndPackage[]
