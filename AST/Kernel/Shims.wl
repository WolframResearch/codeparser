BeginPackage["AST`Shims`"]

setupShims

Begin["`Private`"]


setupShims[] := (
	setupStackShim[]
)


setupStackShim[] := (

	System`CreateDataStructure["ExpressionStack"] :=
		With[{stack = Unique["stack"], stackVal = Unique["stackVal"]},

			stackVal = {};

			stack /: stack["Push", expr_] := (
				AppendTo[stackVal, expr];
				Null
			);

			stack /: stack["Pop"] :=
				Module[{tmp},
					tmp = stackVal[[-1]];
					stackVal = Drop[stackVal, -1];
					tmp
				];

			stack /: stack["Peek"] := stackVal[[-1]];

			(*
			The actual ExpressionStack DataStructure behaves like this:

			s = CreateDataStructure["ExpressionStack"];
			s["Push", 1]
			s["Push", 2]

			Normal[s] => {2, 1}

			I do not agree with this behavior, but must match it for compatibility
			*)
			stack /: Normal[stack] := Reverse[stackVal];

			stack /: stack["Length"] := Length[stackVal];

			stack
		]
)




End[]

EndPackage[]
