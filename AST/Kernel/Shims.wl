BeginPackage["AST`Shims`"]

setupShims

$TopLevelExpressionLimit

cleanupStackShimMemoryLeak


Begin["`Private`"]


(*
How many top-level expressions are allowed?

Beyond this limit, parsing is infeasible
*)
$TopLevelExpressionLimit = Infinity


setupShims[] := (
  If[$VersionNumber < 12.1,
    setupStackShim[]
  ]
)


setupStackShim[] := (

  (*
  For versions before 12.1, we are using O(n^2) AppendTo to create the stack of top-level expressions.

  Limit to some low number.
  *)
  $TopLevelExpressionLimit = 5000;

  (*
  Define CreateDataStructure for earlier versions
  *)
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
    ];

  cleanupStackShimMemoryLeak[] := (
    (*
    Hack to prevent memory leak with shims
    *)
    Quiet[Remove["AST`Shims`Private`stack*"];, {Remove::rmnsm}];
  )
)









End[]

EndPackage[]
