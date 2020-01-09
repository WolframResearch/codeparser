BeginPackage["AST`Shims`"]

setupShims

Begin["`Private`"]


setupShims[] := (
  setupStackShim[]
)


(*
TODO when fixes for bugs  385114 and 385768 have completely filtered through the 12.1 builds, then add a conditional

Also: remove use of $TopLevelExpressionLimit in Abstract.wl

*)
setupStackShim[] := (

  System`CreateDataStructure["ExpressionStack"] :=
    Module[{stack, stackVal},
    
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

      stack /: Normal[stack] := stackVal;

      stack /: stack["Length"] := Length[stackVal];

      stack
    ]
)









End[]

EndPackage[]
