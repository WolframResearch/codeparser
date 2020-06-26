BeginPackage["CodeParser`Shims`"]

setupShims

cleanupStackShimMemoryLeak


Begin["`Private`"]


setupShims[] := (
  If[$VersionNumber < 12.1,
    setupStackShim[]
  ]
)


setupStackShim[] := (

  (*
  For versions before 12.1, we implement our own stack to store top-level expressions.

  The push, pop and peek operations take O(1), while Normal takes O(n).
  *)

  (*
  Define CreateDataStructure for earlier versions
  *)
  System`CreateDataStructure["Stack"] :=
    Module[{stack, stackVal, stackDepth, stackCons},

      stackVal = stackCons[];

      stackDepth = 0;

      stack /: stack["Push", expr_] := (
        stackVal = stackCons[stackVal, expr];
        stackDepth += 1;
        Null
      );

      stack /: stack["Pop"] :=
        Module[{tmp},
          If[stackDepth != 0,
            stackDepth -= 1;
            {stackVal, tmp} = List @@ stackVal;
            tmp
          ]
        ];

      stack /: stack["Peek"] := Last[stackVal];

      stack /: Normal[stack] := (
        Flatten[{stackVal}, Infinity, stackCons]
      );

      stack /: stack["Length"] := stackDepth;

      stack
    ];

  cleanupStackShimMemoryLeak[] := (
    (*
    Hack to prevent memory leak with shims
    *)
    Quiet[Remove["CodeParser`Shims`Private`stack*$*"];, {Remove::rmnsm}];
  )
)









End[]

EndPackage[]
