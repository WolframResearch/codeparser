BeginPackage["CodeParserTestUtils`Boxes`"]

parseBoxTest

continue

$Interactive

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

parseBoxTest[box_, name_, n_, i_, j_] :=
Catch[
Module[{cst, back, diff, sourceStr, agg, inputStr, cst2, agg2, aggToCompare, agg2ToCompare, inputStr2, ast},

cst =
  Catch[
    CodeConcreteParseBox[box]
    ,
    {CodeParser`Boxes`Private`parseBox, "Unhandled"} |
      {CodeParser`Boxes`Private`parseBox, RowBox, CodeParser`Boxes`Private`Unhandled}
    ,
    (Print["bad: ", # // InputForm];) &
  ];

back = ToStandardFormBoxes[cst];

If[back =!= box,
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}]; Throw[continue]];
  Which[
    FailureQ[back],
      diff = back
    ,
    FailureQ[box],
      diff = box
    ,
    True,
      diff = findDiff[back, box];
  ];
  (* some weird string from the Example build process *)
  
  If[!FreeQ[diff, outputFormPatterns], 
   (* Print[{"weird diff", diff, {name, n, i, j}}]; *)
   Print["weird diff contains: ", Cases[diff, outputFormPatterns, Infinity], " ", {name, n, i, j}];
   Throw[continue]
  ];
  If[$Interactive,
    Print[Style[{"concrete", diff, {name, n, i, j}}, Red]];
  ];
  $Error = True;
  Throw[{"concrete", diff, {name, n, i, j}}]
];

sourceStr = ToSourceCharacterString[cst];
If[!StringQ[sourceStr],
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}];
   $Error = True;
   Throw[continue]
  ];
  If[FailureQ[sourceStr] && 
    sourceStr[[1]] == "CannotConvertBoxesToSourceCharacterString",
   If[$Interactive,
     Print[Style[Row[{"-",sourceStr[[2, Key["Arguments"], 1, 1]],"-"}], Darker[Orange]]];
     If[$Debug,
       Print[Style[sourceStr[[2, Key["Arguments"], 1, 1]], Darker[Orange]]];
     ];
   ];
   (*
   Print[sourceStr[[2]]["Arguments"]];
   *)
   Throw[continue]
   ];
  If[$Interactive,
     Print[Style[Row[{"-",sourceStr[[2, Key["Arguments"], 1, 1]],"-"}], Red]];
     If[$Debug,
       Print[Style[sourceStr[[2, Key["Arguments"]]], Red]];
     ];
   ];
  $Error = True;
  Throw[{sourceStr, {name, n, i, j}}]
];

(* Check[agg = CodeParser`Folds`aggregateButNotToplevelNewlines[cst];, $Error = True;Throw[{"aggregate ", box, {name, n, i, j}}]];

agg = cleanupImplicitNull[agg];

inputStr = CodeParser`ToString`toInputFormStringButNotToplevelNewlines[agg]; *)
Check[agg = CodeParser`Folds`aggregate[cst];, $Error = True;Throw[{"aggregate ", box, {name, n, i, j}}]];

agg = cleanupImplicitNull[agg];

inputStr = CodeParser`ToString`toInputFormString[agg];

If[!StringQ[inputStr],
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}];
   $Error = True;
   Throw[continue]
  ];
  If[FailureQ[inputStr] && 
    inputStr[[1]] == "CannotConvertBoxesToInputFormString",
   If[$Interactive,
     Print[Style[Row[{"-",inputStr[[2, Key["Arguments"], 1, 1]],"-"}], Darker[Orange]]];
     If[$Debug,
       Print[Style[inputStr[[2, Key["Arguments"]]], Darker[Orange]]];
     ];
   ];
   (*
   Print[inputStr[[2]]["Arguments"]];
   *)
   Throw[continue]
   ];
  If[$Interactive,
    Print[Style[{inputStr, {name, n, i, j}}, Red]];
  ];
  $Error = True;
  Throw[{inputStr, {name, n, i, j}}]
  ];

  aggToCompare = agg;
  aggToCompare = convertMultiSingleQuote[aggToCompare];
  aggToCompare = flattenChildrenInGroupMissingCloser[aggToCompare];
  aggToCompare = expandEqualDot[aggToCompare];
  aggToCompare = aggToCompare /. {_Association -> <||>};

  cst2 = CodeConcreteParse[inputStr];

  (*
  reset to Box
  *)
  cst2[[1]] = Box;
(*   cst2 = cleanupQuestionQuestion[cst2]; *)
  cst2 = cleanupImplicitNull[cst2];
  (* agg2 = CodeParser`Folds`aggregateButNotToplevelNewlines[cst2]; *)
  agg2 = CodeParser`Folds`aggregate[cst2];

  agg2Compare = agg2;
  agg2Compare = rowBoxify[agg2Compare];
  agg2Compare = convertedUnterminatedToGroupMissingCloser[agg2Compare];
  agg2ToCompare = agg2Compare /. {_Association -> <||>};

  If[agg2ToCompare =!= aggToCompare,
    If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
     Print[Style[{"exception", {name, n, i, j}}, Red]];
     $Error = True;
     Throw[continue]
    ];

    If[!FreeQ[aggToCompare, PrefixNode[Information, _, _]],
      Print[Style[{"using prefix ?", {name, n, i, j}}, Red]];
      $Error = True;
      Throw[continue]
    ];

    If[!FreeQ[aggToCompare, InfixNode[Times, children_ /; MemberQ[children, LeafNode[Token`Star, _, _]] &&
        MemberQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _]], _]],
      Print[Style[{"using * AND implicit Times (not currently supported)", {name, n, i, j}}, Red]];
      $Error = True;
      Throw[continue]
    ];

    diff = findDiff[agg2ToCompare, aggToCompare];
    If[!FreeQ[diff, outputFormPatterns], 
     Print[{"weird diff contains: ", Cases[diff, outputFormPatterns, Infinity], " ", {name, n, i, j}}];
     $Error = True;
     Throw[continue]
    ];

    (*
    Print[Style["AGGS DO NOT AGREE", Red]];

    Print[Style[Column[{Row[{"from boxes:  ", Pane[aggToCompare]}], 
       Row[{"from input string: ", Pane[agg2ToCompare]}]}], Red]];
    *)
    If[$Interactive,

      inputStr2 = CodeParser`ToString`toInputFormStringButNotToplevelNewlines[agg2];

      Print[Column[{
        Style[Row[{"AGGS DO NOT AGREE: (hopefully they are equivalent)"}], Red],
        Style["<<<<<<<<<<", Red],
        Style[Pane[inputStr], Red],
        Style["==========", Red],
        Style[Pane[inputStr2], Red],
        Style[">>>>>>>>>>", Red]
      }]];
    ];

    $Error = True;
    Print[{"comparing aggs", {name, n, i, j}}];
    Throw[{"comparing aggs", name, Hash[box]}]
  ];

  Check[ast = CodeParser`Abstract`Abstract[agg];, $Error = True;Throw[{"abstract ", box, {name, n, i, j}}]];

  If[$Interactive,
    Print[Style[sourceStr, Bold, Black]];
  ];

  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[Style[{"exception is UNUSED", {name, n, i, j}}, Red]]];

  Null
]]


(*
rowBoxify[ContainerNode[Box, {first_, second_, rest___}, data_]] :=
  ContainerNode[Box, {BoxNode[RowBox, {rowBoxify /@ {first, second, rest}}, <||>]}, data]
*)

rowBoxify[ContainerNode[Box, children_, data_]] :=
  ContainerNode[Box, rowBoxify /@ children, data]

rowBoxify[BoxNode[RowBox, {child_}, data_]] :=
  BoxNode[RowBox, {rowBoxify /@ child}, data]

(* rowBoxify[InfixNode[Times, children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1], data_]] :=
  Module[{stripped},

    stripped = DeleteCases[children, LeafNode[Token`Fake`ImplicitTimes, _, _]];

    BoxNode[RowBox, {rowBoxify /@ stripped}, data]
  ] *)

(*
Handle ?x
*)
rowBoxify[BinaryNode[PatternTest, {ErrorNode[Token`Error`ExpectedOperand, _, _], op:LeafNode[Token`Question, _, _], rand_}, data_]] :=
  PrefixNode[Information, {op, rand}, data]


(* rowBoxify[PostfixNode[Derivative, {rand_, LeafNode[Token`SingleQuote, str_, data1_]}, data_]] :=
  Module[{processed, processedRand, processedOp},

    processed = rowBoxify[rand];

    Switch[processed,
      PostfixNode[Derivative, {_, LeafNode[Token`Boxes`MultiSingleQuote, _, _]}, _],
        processedRand = processed[[2, 1]];
        processedOp = processed[[2, 2]];
        PostfixNode[Derivative, {processedRand, LeafNode[Token`Boxes`MultiSingleQuote, processedOp[[2]]<>str, data1]}, data]
      ,
      _,
        PostfixNode[Derivative, {processed, LeafNode[Token`Boxes`MultiSingleQuote, str, data1]}, data]
    ]
  ] *)

rowBoxify[node_LeafNode] := node

rowBoxify[CallNode[head_, children_, data_]] :=
  CallNode[rowBoxify[head], rowBoxify /@ children, data]


rowBoxify[node_[tag_, children_, data_]] :=
  node[tag, rowBoxify /@ children, data]

convertMultiSingleQuote[agg_] :=
  agg /. {
    PostfixNode[Derivative, {a_, LeafNode[Token`Boxes`MultiSingleQuote, d_, dPos_]}, pos_] :>
      Nest[PostfixNode[Derivative, {#, LeafNode[Token`SingleQuote, "'", <||>]}, <||>] &, a, StringLength[d]]
  }

(*
Clear[cleanupQuestionQuestion]
cleanupQuestionQuestion[node_] :=
  Module[{poss},
    poss = Position[node, ErrorNode[Token`Error`UnsupportedToken, _, _]];
    Fold[(
        If[Length[Extract[#1, Most[#2]]] > Last[#2], 
          ReplacePart[#1, Most[#2] ->
            SequenceReplace[Extract[#1, Most[#2]],
              {op:ErrorNode[Token`Error`UnsupportedToken, "??", _], rand_} :>
                PrefixNode[Information, {LeafNode[Token`QuestionQuestion, op[[2]], op[[3]]], rand}, <||>]]]
          ,
          #1
        ]
      )&
      ,
      node
      ,
      poss
    ]
  ]
*)

cleanupImplicitNull[node_] := DeleteCases[node, LeafNode[Token`Fake`ImplicitNull, _, _], Infinity]


flattenChildrenInGroupMissingCloser[node_] := node /. {
  GroupMissingCloserNode[op_, children_, data_] :> GroupMissingCloserNode[op, Cases[children, (LeafNode|ErrorNode)[_, _, _], Infinity], data]
}

convertedUnterminatedToGroupMissingCloser[node_] := node /. {
  UnterminatedGroupNode[op_, children_, data_] :> GroupMissingCloserNode[op, children, data]
}

expandEqualDot[agg_] :=
    agg /. {
        BinaryNode[Unset, {rand_, LeafNode[Token`Boxes`EqualDot, _, _]}, _] :>
            BinaryNode[Unset, {rand, LeafNode[Token`Equal, "=", <||>], LeafNode[Token`Dot, ".", <||>]}, <||>]
    }


exceptions = <|
   (* TraditionalForm cells *)
   
   "ArrayFilter" -> {{3, 4}},
   "Format" -> {{1, 2}},
   "FormBox" -> {{1, 2}, {2, 2}},
   "Inactive" -> {{3, 4}},
   "SingleLetterItalics" -> {{2, 2}},
   
   (* weird RawArray thing *)
   "ConstantArrayLayer" -> {{2, 4}},
   "LearningRateMultipliers" -> {{1, 8}, {1, 10}, {1, 12}},
   "NetInformation" -> {{1, 10}},
   "NetInitialize" -> {{1, 6}},
   "NetSharedArray" -> {{3, 6}, {3, 8}, {3, 10}},
   "NumericArray" -> {{1, 2}, {2, 2}},
   "NumericArrayType" -> {{2, 2}},
   
   (* ErrorBox and purposeful bad syntax *)
   (* "DelimiterFlashTime" -> {{1, 2}}, *)
   "ErrorBox" -> {{1, 4}, {2, 2}},
   (* "ShowAutoStyles" -> {{1, 3}, {1, 4}}, *)

   (* weird empty RowBox[{}] thing *)
   "IncludePods" -> {{1, 2}},
   
   (* FE bug 379427 with multiple ~ *)
   "Join" -> {{2, 1}},
   
   (* Recursion errors from using crazy Dataset typesetting *)
   "NetworkPacketTrace" -> {{2, 2}, {3, 2}},
   
   (* weird StyleBox problem *)
   "PlotMarkers" -> {{3, 3}},
   
   (* weird -foo- formatting *)
   "AnomalyDetection" -> {{1, 2}},
   "AnomalyDetector" -> {{1, 2}, {1, 10}, {2, 2}, {2, 4}, {2, 6}, {3, 2}, {3, 14}},
   "AnomalyDetectorFunction" -> {{1, 2}, {2, 2}},
   "Audio" -> {{1, 2}},
   "AudioAmplify" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioBlockMap" -> {{1, 1}},
   "AudioCapture" -> {{1, 4}},
   "AudioChannelAssignment" -> {{1, 2}, {1, 4}, {2, 3}},
   "AudioChannelCombine" -> {{1, 7}},
   "AudioChannelMix" -> {{1, 3}, {1, 5}},
   "AudioChannels" -> {{1, 1}, {2, 1}},
   "AudioData" -> {{1, 1}},
   "AudioDelay" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioDelete" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioFade" -> {{1, 1}, {1, 4}, {2, 1}, {2, 2}},
   "AudioFrequencyShift" -> {{1, 1}, {1, 4}},
   "AudioGenerator" -> {{1, 2}, {2, 2}, {3, 2}},
   "AudioIdentify" -> {{1, 1}, {2, 1}},
   "AudioInputDevice" -> {{1, 4}},
   "AudioInsert" -> {{1, 1}, {1, 3}},
   "AudioLabel" -> {{1, 3}, {1, 5}, {2, 2}, {2, 4}},
   "AudioLength" -> {{1, 1}, {2, 1}},
   "AudioLocalMeasurements" -> {{1, 1}, {2, 1}},
   "AudioNormalize" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioOutputDevice" -> {{1, 4}, {1, 8}},
   "AudioOverlay" -> {{1, 5}},
   "AudioPad" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioPartition" -> {{1, 2}, {1, 4}},
   "AudioPitchShift" -> {{1, 1}, {1, 3}, {2, 1}, {2, 3}},
   "AudioPlot" -> {{1, 1}, {2, 1}},
   "AudioQ" -> {{1, 1}, {2, 1}},
   "AudioReplace" -> {{1, 1}, {1, 3}},
   "AudioReverb" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioReverse" -> {{1, 3}},
   "AudioSampleRate" -> {{1, 1}, {2, 2}},
   "AudioSplit" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
   "AudioTimeStretch" -> {{1, 1}, {1, 4}},
   "AudioTrim" -> {{1, 1}, {1, 3}, {1, 5}, {1, 7}, {1, 9}},
   "AudioType" -> {{1, 1}, {2, 1}, {3, 1}},
   "BandpassFilter" -> {{2, 1}, {2, 2}},
   "BandstopFilter" -> {{2, 1}, {2, 2}},
   "ClassifierFunction" -> {{1, 2}, {2, 2}},
   "Classify" -> {{1, 3}, {2, 2}},
   "ClusterClassify" -> {{1, 2}, {2, 6}, {3, 3}},
   "CompressionLevel" -> {{1, 2}},
   "ComputeUncertainty" -> {{1, 2}},
   "CriterionFunction" -> {{1, 10}, {1, 12}},
   "DiscreteWaveletPacketTransform" -> {{2, 2}, {2, 6}, {2, 8}},
   "DiscreteWaveletTransform" -> {{2, 2}, {2, 6}, {2, 8}},
   "Duration" -> {{1, 1}},
   "FindGeometricConjectures" -> {{1, 2}, {1, 4}, {2, 2}},
   "GeometricAssertion" -> {{1, 2}, {2, 2}, {3, 2}},
   "GeometricScene" -> {{1, 4}, {2, 2}, {3, 2}},
   "GeometricStep" -> {{1, 2}, {1, 4}},
   "InverseShortTimeFourier" -> {{1, 2}, {1, 6}},
   "InverseSpectrogram" -> {{1, 6}},
   "InverseWaveletTransform" -> {{3, 5}},
   "LiftingWaveletTransform" -> {{2, 2}, {2, 6}, {2, 8}},
   "Looping" -> {{1, 2}},
   "LowpassFilter" -> {{2, 1}, {2, 2}},
   "PaddingSize" -> {{1, 1}, {1, 5}},
   "Predict" -> {{1, 3}, {2, 2}},
   "PredictorFunction" -> {{1, 4}, {2, 2}},
   "RandomInstance" -> {{1, 4}, {1, 7}},
   "RemoteBatchJobObject" -> {{1, 1}, {1, 8}},
   "SpeechCases" -> {{1, 1}, {2, 1}},
   "SpeechInterpreter" -> {{1, 1}, {2, 1}},
   "SpeechRecognize" -> {{1, 1}},
   "SpeechSynthesize" -> {{1, 2}, {2, 2}},
   "StationaryWaveletPacketTransform" -> {{2, 2}, {2, 6}, {2, 10}},
   "StationaryWaveletTransform" -> {{2, 2}, {2, 6}, {2, 10}},
   "UnconstrainedParameters" -> {{1, 2}},
   "UtilityFunction" -> {{1, 3}, {1, 11}},
   "ValidationSet" -> {{1, 3}, {1, 7}, {2, 3}, {2, 7}},
   "VideoCombine" -> {{1, 4}},
   "VideoMap" -> {{3, 1}},
   "$VoiceStyles" -> {{1, 4}, {1, 6}},

   (* weird <<>> Skeleton formatting *)
   "Authentication" -> {{5, 7}},
   "CombinedEntityClass" -> {{5, 2}, {6, 4}},
   "PolyhedronDecomposition" -> {{1, 3}},
   "SecuredAuthenticationKey" -> {{5, 7}},
   "Shallow" -> {{1, 2}, {2, 4}},
   "ShortestPathFunction" -> {{1, 4}},
   "Skeleton" -> {{1, 2}, {2, 2}},
   "WikipediaData" -> {{2, 2}, {2, 4}, {5, 2}, {5, 4}, {5, 9}},
   
   (* weird Cell[foo] thing *)
   "AllowInlineCells" -> {{1, 27}},
   "TextGrid" -> {{1, 2}, {2, 2}},
   "WolframLanguageData" -> {{5, 12}},
   
   (* weird RowBox[{""}] thing *)
   "AsymptoticIntegrate" -> {{2, 2}, {2, 6}},
   "BernoulliProcess" -> {{1, 2}},
   (* "BetaRegularized" -> {{1, 2}}, *)
   (* "BlankNullSequence" -> {{3, 3}}, *)
   (* "Chop" -> {{1, 2}}, *)
   (* "CircularOrthogonalMatrixDistribution" -> {{1, 2}}, *)
   (* "CircularQuaternionMatrixDistribution" -> {{2, 2}}, *)
   (* "CircularSymplecticMatrixDistribution" -> {{2, 2}}, *)
   (* "CircularUnitaryMatrixDistribution" -> {{1, 2}, {2, 2}}, *)
   (* "CompiledFunction" -> {{1, 8}}, *)
   "ConfidenceLevel" -> {{1, 3}, {1, 7}},
   "CovarianceEstimatorFunction" -> {{1, 3}, {1, 7}},
   (* "CubeRoot" -> {{3, 4}}, *)
   (* "DedekindEta" -> {{1, 2}}, *)
   (* "EllipticLog" -> {{1, 2}, {1, 4}}, *)
   (* "ExponentialFamily" -> {{1, 3}}, *)
   "Fit" -> {{1, 7}},
   "FittedModel" -> {{1, 5}},
   (* "Fourier" -> {{1, 2}}, *)
   (* "FourierParameters" -> {{1, 2}}, *)
   "FullInformationOutputRegulator" -> {{1, 3}},
   (* "Gamma" -> {{3, 2}}, *)
   (* "GaussianUnitaryMatrixDistribution" -> {{1, 2}}, *)
   (* "HankelH1" -> {{1, 2}}, *)
   (* "HankelH2" -> {{1, 2}}, *)
   (* "HeunCPrime" -> {{1, 2}}, *)
   (* "HeunG" -> {{1, 2}}, *)
   (* "HeunGPrime" -> {{1, 2}}, *)
   (* "HurwitzLerchPhi" -> {{1, 2}}, *)
   (* "Hypergeometric2F1" -> {{1, 2}}, *)
   (* "I" -> {{3, 4}}, *)
   "InflationMethod" -> {{1, 2}, {2, 2}},
   (* "InverseFourier" -> {{1, 2}, {2, 2}}, *)
   (*"InverseJacobiCS" -> {{1, 2}},*)
   (* "InverseJacobiDN" -> {{1, 2}, {1, 4}}, *)
   (* "InverseJacobiNC" -> {{1, 2}, {1, 4}}, *)
   (* "InverseJacobiND" -> {{1, 2}, {1, 4}}, *)
   (*"InverseJacobiSC" -> {{1, 2}},*)
   (* "JuliaSetBoettcher" -> {{1, 2}, {2, 2}}, *)
   (* "JuliaSetIterationCount" -> {{2, 1}}, *)
   (*"KleinInvariantJ" -> {{1, 2}},*)
   "LerchPhi" -> {{2, 2}},
   (* "LinearModelFit" -> {{1, 5}}, *)
   (* "LinearOffsetFunction" -> {{1, 3}, {1, 5}}, *)
   (* "LinkFunction" -> {{1, 5}}, *)
   (* "MandelbrotSetBoettcher" -> {{1, 2}}, *)
   (* "NominalVariables" -> {{1, 5}, {1, 7}}, *)
   "NonlinearModelFit" -> {{1, 5}},
   (* "NSolve" -> {{1, 2}, {3, 2}}, *)
   "OutputResponse" -> {{3, 2}},
   (* "RamanujanTauL" -> {{1, 2}}, *)
   (* "RandomComplex" -> {{1, 2}, {2, 2}, {3, 2}, {4, 2}, {5, 2}}, *)
   (* "Root" -> {{1, 4}}, *)
   (*"SiegelTheta" -> {{1, 2}},*)
   (* "Sign" -> {{2, 2}}, *)
   (* "SphericalHankelH1" -> {{1, 2}}, *)
   (* "SphericalHankelH2" -> {{1, 2}}, *)
   (* "Surd" -> {{3, 4}}, *)
   (* "TransferFunctionModel" -> {{4, 4}}, *)
   (* "WeierstrassE1" -> {{2, 2}}, *)
   (* "WeierstrassEta1" -> {{2, 4}}, *)
   (* "WeierstrassEta3" -> {{2, 2}}, *)
   (* "WeierstrassHalfPeriods" -> {{1, 2}, {3, 2}}, *)
   (* "WeierstrassHalfPeriodW1" -> {{3, 4}}, *)
   (* "WeierstrassHalfPeriodW3" -> {{1, 2}}, *)
   (* "WeierstrassInvariantG2" -> {{1, 2}}, *)
   (* "WeierstrassInvariantG3" -> {{1, 2}}, *)
   (* "WeierstrassInvariants" -> {{1, 4}, {1, 6}}, *)
   (*"WeierstrassP" -> {{1, 2}},*)
   (*"WeierstrassPPrime" -> {{1, 2}},*)
   (* "WeierstrassSigma" -> {{1, 2}}, *)
   (* "WeierstrassZeta" -> {{1, 2}}, *)
   (* "ZetaZero" -> {{1, 2}}, *)
   (* "$Post" -> {{2, 5}}, *)
   (* "$PrePrint" -> {{2, 5}}, *)
   (* "$PreRead" -> {{2, 2}}, *)
   
   (* ? for Information *)
   "BeginPackage" -> {{1, 9}},
   "Encode" -> {{1, 6}},
   "Install" -> {{1, 4}},
   "LinkPatterns" -> {{1, 6}},
   "MessageName" -> {{1, 2}},
   "Needs" -> {{1, 2}},
   "Remove" -> {{1, 2}},
   "Uninstall" -> {{1, 4}},
   "$CurrentLink" -> {{1, 6}},

   (* ?? for Information *)
   "Assert" -> {{2, 3}},
   "Begin" -> {{1, 6}, {1, 7}},
   "DumpSave" -> {{1, 8}, {1, 10}},
   "In" -> {{2, 10}},
   
   (*
   RowBox[{"x_:","0"}] idiocy
   Related bugs: 373953, 378774
   *)
   (*"OneIdentity" -> {{1, 6}},*)
   (*"Optional" -> {{1, 1}},*)
   
   (*
   \[Integral] syntax
   *)
   (*
   "DifferentialD" -> {{1, 1}},
   *)
   "PrincipalValue" -> {{1, 4}},
   
   (*
   dumb BoxData[""] bad input
   *)
   "ServiceSubmit" -> {{1, 7}},
   
   (*
   bad ; ;
   *)
   "WindowMargins" -> {{3, 1}},
   (* "SmoothKernelDistribution" -> {{5947572171989938388, 0}}, *)
   
   (*
   RowBox[{"string","."}]
   *)
   "Paste" -> {{1, 4}},
   
   (*
    bad "{...}"
   *)
   "NetGraph" -> {{1, 8}, {1, 10}},

   (*
    bad boxes for a /: b =.
   *)
   "TagUnset" -> {{1, 2}},

   (*
    weird RowBox[{"-ExampleData/Caminandes.mp4-"}]
   *)
   "Video" -> {{1, 2}},

   (*

   FE bug? notebook bug?

   RowBox[] around top-level newlines
   *)
   "BenktanderWeibullDistribution" -> {{2, 3}},
   "ConformAudio" -> {{1, 4}},
   "EntityClass" -> {{4, 1}},
   "Fibonacci" -> {{5, 1}, {6, 1}},
   "HermiteH" -> {{5, 1}},
   "LabelVisibility" -> {{1, 1}, {2, 1}},
   "LaguerreL" -> {{6, 1}},
   "OwenT" -> {{5, 1}, {6, 1}},
   "RemoveInputStreamMethod" -> {{1, 1}, {1, 3}},
   "RemoveOutputStreamMethod" -> {{1, 1}, {1, 3}},
   "WhittakerM" -> {{6, 1}},
   "WhittakerW" -> {{6, 1}},

   (* combined *)
   "CoxModel" -> {{1, 3}, {1, 9}},
   "Information" -> {{1, 3}, {1, 4}, {1, 5}},
   "Integrate" -> {{3, 4}},
   "TraditionalForm" -> {{1, 2}, {2, 2}, {3, 2}},

   "FindKPlex" -> {{1, 1}, {2, 3}},


    (*
    Using \[FilledSmallCircle] instead of \[Application]
    *)
   "Application" -> {{1, 1}, {1, 2}, {2, 1}},
   "CombinatorB" -> {{1, 1}},
   "CombinatorC" -> {{1, 1}, {1, 2}},
   "CombinatorI" -> {{1, 1}},
   "CombinatorK" -> {{1, 1}, {2, 1}},
   "CombinatorS" -> {{1, 1}, {1, 2}, {2, 1}},
   "CombinatorW" -> {{1, 1}, {1, 2}}
   |>;

outputFormPatterns = ToExpression["\"\\[Backslash]\""]

End[]

EndPackage[]
