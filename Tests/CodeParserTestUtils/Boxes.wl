BeginPackage["CodeParserTestUtils`Boxes`"]

parseBoxTest

continue

$Interactive

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

parseBoxTest[box_, name_, n_, i_, j_] :=
Catch[
Module[{},

cst = Catch[
   CodeConcreteParseBox[
    box], {CodeParser`Boxes`Private`parseBox, 
     "Unhandled"} | {CodeParser`Boxes`Private`parseBox, RowBox, 
     CodeParser`Boxes`Private`Unhandled}, (Print[
       "bad: ", # // InputForm];) &];

back = ToStandardFormBoxes[cst];

If[back =!= box,
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}]; Throw[continue]];
  diff = findDiff[back, box];
  (* some weird string from the Example build process *)
  
  If[MatchQ[diff, outputFormPatterns], 
   Print[{"weird diff", diff, {name, n, i, j}}]; Throw[continue]];
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
     Print[Style[Row[{"-",sourceStr[[2]]["Arguments"][[1, 1, 1]],"-"}], Darker[Orange]]];
     If[$Debug,
       Print[Style[sourceStr[[2]]["Arguments"], Darker[Orange]]];
     ];
   ];
   (*
   Print[sourceStr[[2]]["Arguments"]];
   *)
   Throw[continue]
   ];
  If[$Interactive,
     Print[Style[Row[{"-",sourceStr[[2]]["Arguments"][[1, 1, 1]],"-"}], Red]];
     If[$Debug,
       Print[Style[sourceStr[[2]]["Arguments"], Red]];
     ];
   ];
  $Error = True;
  Throw[{sourceStr, {name, n, i, j}}]
];

Check[agg = CodeParser`Folds`aggregateButNotToplevelNewlines[cst];, $Error = True;Throw[{"aggregate ", box, {name, n, i, j}}]];

agg = cleanupImplicitNull[agg];

inputStr = CodeParser`ToString`toInputFormStringButNotToplevelNewlines[agg];

If[!StringQ[inputStr],
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}];
   $Error = True;
   Throw[continue]
  ];
  If[FailureQ[inputStr] && 
    inputStr[[1]] == "CannotConvertBoxesToInputFormString",
   If[$Interactive,
     Print[Style[Row[{"-",inputStr[[2]]["Arguments"][[1,1,1]],"-"}], Darker[Orange]]];
     If[$Debug,
       Print[Style[inputStr[[2]]["Arguments"], Darker[Orange]]];
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
  aggToCompare = aggToCompare /. {_Association -> <||>};

  cst2 = CodeConcreteParse[inputStr];

  (*
  reset to Box
  *)
  cst2[[1]] = Box;
  cst2 = cleanupQuestionQuestion[cst2];
  cst2 = cleanupImplicitNull[cst2];
  agg2 = CodeParser`Folds`aggregateButNotToplevelNewlines[cst2];

  agg2Compare = agg2;
  agg2Compare = rowBoxify[agg2Compare];
  agg2ToCompare = agg2Compare /. {_Association -> <||>};

  If[agg2ToCompare =!= aggToCompare,
    If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
     Print[Style[{"exception", {name, n, i, j}}, Red]];
     $Error = True;
     Throw[continue]
    ];
    diff = findDiff[agg2ToCompare, aggToCompare];
    If[MatchQ[diff, outputFormPatterns], 
     Print[{"weird diff", diff, {name, n, i, j}}];
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


rowBoxify[ContainerNode[Box, {first_, second_, rest___}, data_]] :=
  ContainerNode[Box, {BoxNode[RowBox, {rowBoxify /@ {first, second, rest}}, <||>]}, data]

rowBoxify[ContainerNode[Box, children_, data_]] :=
  ContainerNode[Box, rowBoxify /@ children, data]

rowBoxify[BoxNode[RowBox, {child_}, data_]] :=
  BoxNode[RowBox, {rowBoxify /@ child}, data]

rowBoxify[InfixNode[Times, children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1], data_]] :=
  Module[{stripped},

    stripped = DeleteCases[children, LeafNode[Token`Fake`ImplicitTimes, _, _]];

    BoxNode[RowBox, {rowBoxify /@ stripped}, data]
  ]

(*
Handle ?x
*)
rowBoxify[BinaryNode[PatternTest, {ErrorNode[Token`Error`ExpectedOperand, _, _], op:LeafNode[Token`Question, _, _], rand_}, data_]] :=
  PrefixNode[Information, {op, rand}, data]


rowBoxify[PostfixNode[Derivative, {rand_, LeafNode[Token`SingleQuote, str_, data1_]}, data_]] :=
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
  ]

rowBoxify[node_LeafNode] := node

rowBoxify[CallNode[head_, children_, data_]] :=
  CallNode[rowBoxify[head], rowBoxify /@ children, data]


rowBoxify[node_[tag_, children_, data_]] :=
  node[tag, rowBoxify /@ children, data]



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

cleanupImplicitNull[node_] := DeleteCases[node, LeafNode[Token`Fake`ImplicitNull, _, _], Infinity]





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
   "DelimiterFlashTime" -> {{1, 2}},
   "ErrorBox" -> {{1, 4}, {2, 2}},
   (*
   "ShowAutoStyles" -> {{1, 3}, {1, 4}},
   *)

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
   "ClassifierFunction" -> {{1, 2}, {2, 2}},
   "Classify" -> {{1, 3}, {2, 2}},
   "ClusterClassify" -> {{1, 2}, {2, 6}, {3, 3}},
   "ComputeUncertainty" -> {{1, 2}},
   "CriterionFunction" -> {{1, 10}, {1, 12}},
   "Predict" -> {{1, 3}, {2, 2}},
   "PredictorFunction" -> {{1, 4}, {2, 2}},
   "UtilityFunction" -> {{1, 3}, {1, 11}},
   "ValidationSet" -> {{1, 3}, {1, 7}, {2, 3}, {2, 7}},

   (* weird <<>> formatting *)
   "CombinedEntityClass" -> {{5, 2}, {6, 4}},
   "FindKPlex" -> {{2, 3}},
   "PolyhedronDecomposition" -> {{1, 3}},
   "Shallow" -> {{1, 2}, {2, 4}},
   "ShortestPathFunction" -> {{1, 4}},
   "Skeleton" -> {{1, 2}, {2, 2}},
   "WikipediaData" -> {{2, 2}, {2, 4}, {5, 2}, {5, 4}, {5, 9}},
   
   (* weird Cell[foo] thing *)
   "AllowInlineCells" -> {{1, 27}},
   "TextGrid" -> {{1, 2}, {2, 2}},
   "WolframLanguageData" -> {{5, 12}},
   
   (* weird RowBox[{""}] thing *)
   "AsymptoticIntegrate" -> {{2, 2}, {2, 6}, {3, 2}, {3, 6}},
   "BernoulliProcess" -> {{1, 2}},
   "BetaRegularized" -> {{1, 2}},
   "BlankNullSequence" -> {{3, 3}},
   "Chop" -> {{1, 2}},
   "CircularOrthogonalMatrixDistribution" -> {{1, 2}},
   "CircularQuaternionMatrixDistribution" -> {{2, 2}},
   "CircularSymplecticMatrixDistribution" -> {{2, 2}},
   "CircularUnitaryMatrixDistribution" -> {{1, 2}, {2, 2}},
   "CompiledFunction" -> {{1, 8}},
   "ConfidenceLevel" -> {{1, 3}, {1, 7}},
   "CovarianceEstimatorFunction" -> {{1, 3}, {1, 7}},
   "CubeRoot" -> {{3, 4}},
   "DedekindEta" -> {{1, 2}},
   "EllipticLog" -> {{1, 2}, {1, 4}},
   "ExponentialFamily" -> {{1, 3}},
   "Fit" -> {{1, 3}, {1, 7}},
   "FittedModel" -> {{1, 5}},
   "Fourier" -> {{1, 2}},
   "FourierParameters" -> {{1, 2}},
   "FullInformationOutputRegulator" -> {{1, 3}},
   "Gamma" -> {{3, 2}},
   "GaussianUnitaryMatrixDistribution" -> {{1, 2}},
   "HankelH1" -> {{1, 2}},
   "HankelH2" -> {{1, 2}},
   "HeunCPrime" -> {{1, 2}},
   "HeunG" -> {{1, 2}},
   "HeunGPrime" -> {{1, 2}},
   "HurwitzLerchPhi" -> {{1, 2}},
   "Hypergeometric2F1" -> {{1, 2}},
   "I" -> {{3, 4}},
   "InflationMethod" -> {{1, 2}, {2, 2}},
   "InverseFourier" -> {{1, 2}, {2, 2}},
   (*"InverseJacobiCS" -> {{1, 2}},*)
   "InverseJacobiDN" -> {{1, 2}, {1, 4}},
   "InverseJacobiNC" -> {{1, 2}, {1, 4}},
   "InverseJacobiND" -> {{1, 2}, {1, 4}},
   (*"InverseJacobiSC" -> {{1, 2}},*)
   "JuliaSetBoettcher" -> {{1, 2}, {2, 2}},
   "JuliaSetIterationCount" -> {{2, 1}},
   (*"KleinInvariantJ" -> {{1, 2}},*)
   "LerchPhi" -> {{1, 2}, {2, 2}},
   "LinearModelFit" -> {{1, 5}},
   "LinearOffsetFunction" -> {{1, 3}, {1, 5}},
   "LinkFunction" -> {{1, 5}},
   "MandelbrotSetBoettcher" -> {{1, 2}},
   "NominalVariables" -> {{1, 5}, {1, 7}},
   "NonlinearModelFit" -> {{1, 5}},
   "NSolve" -> {{1, 2}, {3, 2}},
   "OutputResponse" -> {{3, 2}},
   "RamanujanTauL" -> {{1, 2}},
   "RandomComplex" -> {{1, 2}, {2, 2}, {3, 2}, {4, 2}, {5, 2}},
   "Root" -> {{1, 4}},
   (*"SiegelTheta" -> {{1, 2}},*)
   "Sign" -> {{2, 2}},
   "SphericalHankelH1" -> {{1, 2}},
   "SphericalHankelH2" -> {{1, 2}},
   "Surd" -> {{3, 4}},
   "TransferFunctionModel" -> {{4, 4}},
   "WeierstrassE1" -> {{2, 2}},
   "WeierstrassEta1" -> {{2, 4}},
   "WeierstrassEta3" -> {{2, 2}},
   "WeierstrassHalfPeriods" -> {{1, 2}, {3, 2}},
   "WeierstrassHalfPeriodW1" -> {{3, 4}},
   "WeierstrassHalfPeriodW3" -> {{1, 2}},
   "WeierstrassInvariantG2" -> {{1, 2}},
   "WeierstrassInvariantG3" -> {{1, 2}},
   "WeierstrassInvariants" -> {{1, 4}, {1, 6}},
   (*"WeierstrassP" -> {{1, 2}},*)
   (*"WeierstrassPPrime" -> {{1, 2}},*)
   "WeierstrassSigma" -> {{1, 2}},
   "WeierstrassZeta" -> {{1, 2}},
   "ZetaZero" -> {{1, 2}},
   "$Post" -> {{2, 5}},
   "$PrePrint" -> {{2, 5}},
   "$PreRead" -> {{2, 2}},
   
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
   (*"ServiceSubmit" -> {{1, 7}},*)
   
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

   (* combined *)
   "CoxModel" -> {{1, 3}, {1, 9}},
   "Information" -> {{1, 3}, {1, 4}, {1, 5}},
   "Integrate" -> {{3, 4}},
   "TraditionalForm" -> {{1, 2}, {2, 2}, {3, 2}}
   |>;

outputFormPatterns = 
  "SearchIndexObject" | "SearchResultObject" | "Failure" | 
   "TemporalData" | "Molecule" | "StructuredArray" | "Polyhedron" | 
   "SecuredAuthenticationKey" | "AutocompletionFunction" | 
   "ProofObject" | "DataDistribution" | "ByteArray" | "OutputStream" |
   "PrivateKey" | "BlockchainTransaction" | "BooleanFunction" | 
   "HypothesisTestData" | "Databin" | "ChannelListener" | 
   "InputStream" | "SocketObject" | "CloudExpression" | 
   "ColorDataFunction" | "ColorProfileData" | "HoldForm" | 
   "ClassifierMeasurementsObject" | "LearnedDistribution" | 
   "EventData" | ToExpression["\"\\[Backslash]\""] | "Sequence" | 
   "ContentObject" | "SystemModelSimulationData" | "Root" | 
   "DatabaseReference" | "Success" | "SymmetricKey" | "DerivedKey" | 
   "DeviceObject" | "DigitalSignature" | "Dispatch" | "LinkObject" | 
   "EncryptedObject" | "EntityStore" | "ExternalFunction" | 
   "ExternalSessionObject" | "ExternalObject" | "HTTPResponse" | 
   "FormFunction" | "GeoPosition" | "GeoVector" | "GeoGridVector" | 
   "GeoVectorENU" | "GeoVectorXYZ" | "HTTPRequest" | 
   "RelationalDatabase" | "PersistenceLocation" | 
   "InterpolatingFunction" | "LibraryFunction" | "SparseArray" | 
   "NBodySimulationData" | "NearestFunction" | "NetInformation" | 
   "Polygon" | "PIDData" | "ProcessObject" | "RegionMemberFunction" | 
   "RegionNearestFunction" | "RemoteConnectionObject" | 
   "ResourceObject" | "ResourceSubmissionObject" | "ServiceObject" | 
   "ServiceRequest" | "ShortTimeFourierData" | 
   "RegionDistanceFunction" | "WebSessionObject" | 
   "ParametricFunction" | "SystemsConnectionsModel" | 
   "TestResultObject" | "TestReportObject" | "TimeSeriesModel" | 
   "TravelDirectionsData" | "WebElementObject" | 
   "WebUnit`WebSessionObject" | "WebWindowObject" | "WeightedData" |
   "KernelObject" | "PacletObject" | "BezierFunction" |
   "CategoricalDistribution" | "DateInterval" | "ExternalStorageObject" |
   "FeatureExtractorFunction" | "QuantityArray" | "Take" |
   "MailServerConnection" | "MailFolder" | "MailItem" |
   "SymmetrizedArray" | "SystemCredentialData" | "SystemCredentialStoreObject";


Clear[findDiff, findDiff0, findDiffShallow];
findDiff[a_, b_] :=
  Catch[
   If[a === b, Throw[SameQ]];
   findDiff0[a, b];
   Throw[Indeterminate]
   ];
findDiff0[a_, b_] := (
  If[Length[a] != Length[b],
   Throw[{Length, Length[a], Length[b], Catch[Do[findDiffShallow[a[[i]], b[[i]], i], {i, 1, Min[Length[a], Length[b]]}]]}]
  ];
  If[Length[a] == 0,
   If[a =!= b, Throw[a]]
   ];
  Do[findDiff0[a[[i]], b[[i]]], {i, 1, Length[a]}]
  )
findDiffShallow[a_, b_, i_] := (
  If[a =!= b,
    Throw[{a, b, i}]
  ];
  )


End[]

EndPackage[]
