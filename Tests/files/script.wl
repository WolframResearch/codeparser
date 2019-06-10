#!/usr/local/bin/wolframscript

(* generate high-precision samples of a mixed distribution *)
Print /@ \
RandomVariate[MixtureDistribution[
    {1,2},
    {NormalDistribution[1,2/10],
     NormalDistribution[3,1/10]}],
    10,  WorkingPrecision -> 50]
