BeginPackage["Collatz`"]

Collatz::usage "Collatz[n] gives a list of the iterates in the 3n+1 problem,
        starting from n. The conjecture is that this sequence always
        terminates."
              (*intentional implicit times*)

Begin["`Private`"]

Collatz[1] := {1};

Collatz[n_Integer]  := Prepend[Collatz[(3 n + 1)/2], n] /; OddQ[n] && n > 0;;
                                                                           (*intentional ;;*)

Collatz[n_Integer] := Prepend[Collatz[n/2], n] /; EvenQ[n] && n > 0;


(*
The call DummyFunction1[] can be replaced with CallSite[DummyFunction1[]] when profiling
to enable CallSite analysis.

CallSite analysis enables the profiling of time between when a function is called to when its body is entered.
In this example, the Pause[0.01] would be kept track of.

The CallSite wrapper is removed during instrumentation and does not affect the result.

Make sure to call InstrumentProfile with the updated code and to reload the packages under profile.
*)
Collatz[n_Integer] := (DummyFunction1[];Prepend[Collatz[3 n + 1], n]) /; OddQ[n] && n > 0;


DummyFunction1[] /; (Pause[0.01];True) :=
Module[{},
	Null
]


End[ ]

EndPackage[ ]