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




processInfixBinaryAtQuirk[
	BinaryNode[BinaryAt, {LeafNode[Symbol, symName_, symData_], LeafNode[Token`At, _, atData_], rhsIn_}, _], symName_] /; $Quirks["InfixBinaryAt"] :=
Module[{data, rhs},

	rhs = rhsIn;

	data = rhs[[3]];

	issues = Lookup[data, AbstractSyntaxIssues, {}];

	synthesizedSource = {symData[[Key[Source], 1 ]], atData[[Key[Source], 2 ]]};

	AppendTo[issues, SyntaxIssue["InfixBinaryAtQuirk", "Unexpected parse.", "Remark", <|Source->synthesizedSource, ConfidenceLevel -> 1.0|>]];

	AssociateTo[data, AbstractSyntaxIssues -> issues];

	rhs[[3]] = data;

	rhs
]

processInfixBinaryAtQuirk[node_, _] := node




End[]

EndPackage[]
