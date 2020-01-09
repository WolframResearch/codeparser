BeginPackage["ASTTestUtils`Boxes`"]

parseBoxTest

continue

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]

parseBoxTest[box_, name_, n_, i_, j_] :=
Catch[
Module[{},

cst = Catch[
   ConcreteParseBox[
    box], {AST`Boxes`Private`parseBox, 
     "Unhandled"} | {AST`Boxes`Private`parseBox, RowBox, 
     AST`Boxes`Private`Unhandled}, (Print[
       "bad: ", # // InputForm];) &];
back = ToStandardFormBoxes[cst];

If[back =!= box,
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}]; Throw[continue]];
  diff = findDiff[back, box];
  (* some weird string from the Example build process *)
  
  If[MatchQ[diff, outputFormPatterns], 
   Print[{"weird diff", diff, {name, n, i, j}}]; Throw[continue]];
  Throw[{"concrete", diff, {name, n, i, j}}]
  ];

Check[agg = AST`Abstract`Aggregate[cst];, 
  Throw[{"aggregate ", box, {name, n, i, j}}]];
Check[ast = AST`Abstract`Abstract[agg];, 
  Throw[{"abstract ", box, {name, n, i, j}}]];
str = ToInputFormString[agg];
If[! StringQ[str],
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}]; Throw[continue]];
  If[FailureQ[str] && 
    str[[1]] == "CannotConvertBoxesToInputFormString",
   (*Print[{"cannot convert boxes to InputForm string",str[[2]][
   "Arguments"][[1,1,1]],{name,n,i,j}}];*)
   Throw[continue]
   ];
  Throw[{str, {name, n, i, j}}]
  ];

cst2 = ConcreteParseString[str];
agg2 = AST`Abstract`Aggregate[cst2];
agg = agg[[2]];
agg2 = agg2[[2]];
aggToCompare = agg /. {_Association -> <||>};
agg2ToCompare = 
  agg2 /. {_Association -> <||>, 
     Whitespace -> Token`Boxes`MultiWhitespace, 
     Token`SingleQuote -> 
      Token`Boxes`MultiSingleQuote} //. {PostfixNode[
      Derivative, {PostfixNode[
        Derivative, {a_, 
         LeafNode[Token`Boxes`MultiSingleQuote, b_, <||>]}, <||>], 
       LeafNode[Token`Boxes`MultiSingleQuote, c_, <||>]}, <||>] :> 
     PostfixNode[
      Derivative, {a, 
       LeafNode[Token`Boxes`MultiSingleQuote, b <> c, <||>]}, <||>]};

If[agg2ToCompare =!= aggToCompare,
  If[MemberQ[Lookup[exceptions, name, {}], {i, j}], 
   Print[{"exception", {name, n, i, j}}]; Throw[continue]];
  diff = findDiff[agg2ToCompare, aggToCompare];
  If[MatchQ[diff, outputFormPatterns], 
   Print[{"weird diff", diff, {name, n, i, j}}]; Throw[continue]];
  Print[Column[{Row[{"from boxes:  ", Pane[aggToCompare]}], 
     Row[{"from string: ", Pane[agg2ToCompare]}]}]];
  Throw[{"comparing aggs", name, n, i, j}]
  ];

]]





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
   
   "ErrorBox" -> {{1, 4}, {2, 2}},
   "ShowAutoStyles" -> {{1, 3}, {1, 4}},
   
   (* weird empty RowBox[{}] thing *)
   "IncludePods" -> {{1, 2}},
   
   (* FE bug 379427 with multiple ~ *)
   "Join" -> {{2, 1}},
   
   (* Recursion errors from using crazy Dataset typesetting *)
   
   "NetworkPacketTrace" -> {{2, 2}, {3, 2}},
   
   (* weird StyleBox problem *)
   "PlotMarkers" -> {{3, 3}},
   
   (* weird -foo- formatting *)
   "Predict" -> {{1, 3}, {2, 2}},
   "PredictorFunction" -> {{1, 4}, {2, 2}},
   "UtilityFunction" -> {{1, 3}, {1, 11}},
   "AnomalyDetection" -> {{1, 2}},
   "AnomalyDetectorFunction" -> {{1, 2}, {2, 2}},
   "ClassifierFunction" -> {{1, 2}, {2, 2}},
   "Classify" -> {{1, 3}, {2, 2}},
   "ComputeUncertainty" -> {{1, 2}},
   
   (* weird <<>> formatting *)
   "ShortestPathFunction" -> {{1, 4}},
   
   (* weird Cell[foo] thing *)
   "TextGrid" -> {{1, 2}, {2, 2}},
   "WolframLanguageData" -> {{5, 10}},
   "AllowInlineCells" -> {{1, 27}},
   
   (* weird RowBox[{""}] thing *)
   
   "AsymptoticIntegrate" -> {{2, 2}, {2, 6}},
   "BernoulliProcess" -> {{1, 2}},
   "BetaRegularized" -> {{1, 2}},
   "BlankNullSequence" -> {{1, 3}, {3, 3}},
   "Chop" -> {{1, 2}},
   "CircularOrthogonalMatrixDistribution" -> {{1, 2}},
   "CircularQuaternionMatrixDistribution" -> {{2, 2}},
   "CircularSymplecticMatrixDistribution" -> {{2, 2}},
   "CircularUnitaryMatrixDistribution" -> {{1, 2}, {2, 2}},
   "CompiledFunction" -> {{1, 8}},
   "ConfidenceLevel" -> {{1, 3}, {1, 7}},
   "CovarianceEstimatorFunction" -> {{1, 3}, {1, 7}},
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
   "Hypergeometric2F1" -> {{1, 2}},
   "I" -> {{3, 4}},
   "InflationMethod" -> {{1, 2}, {2, 2}},
   "InverseFourier" -> {{1, 2}, {2, 2}},
   "InverseJacobiCS" -> {{1, 2}},
   "InverseJacobiDN" -> {{1, 2}, {1, 4}},
   "InverseJacobiNC" -> {{1, 2}, {1, 4}},
   "InverseJacobiND" -> {{1, 2}, {1, 4}},
   "InverseJacobiSC" -> {{1, 2}},
   "JuliaSetBoettcher" -> {{1, 2}, {2, 2}},
   "JuliaSetIterationCount" -> {{2, 1}},
   "KleinInvariantJ" -> {{1, 2}},
   "LerchPhi" -> {{2, 2}},
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
   "SiegelTheta" -> {{1, 2}},
   "SphericalHankelH1" -> {{1, 2}},
   "SphericalHankelH2" -> {{1, 2}},
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
   "WeierstrassP" -> {{1, 2}},
   "WeierstrassPPrime" -> {{1, 2}},
   "WeierstrassSigma" -> {{1, 2}},
   "ZetaZero" -> {{1, 2}},
   "$Post" -> {{2, 5}},
   "$PrePrint" -> {{2, 5}},
   "$PreRead" -> {{2, 2}},
   
   (* ? for Information *)
   "BeginPackage" -> {{1, 9}},
   "Encode" -> {{1, 6}},
   "Information" -> {{1, 4}},
   "Install" -> {{1, 4}},
   "LinkPatterns" -> {{1, 6}},
   "MessageName" -> {{1, 2}},
   "Needs" -> {{1, 2}},
   "Remove" -> {{1, 2}},
   "Uninstall" -> {{1, 4}},
   "$CurrentLink" -> {{1, 6}},
   
   (*
   RowBox[{"x_:","0"}] idiocy
   Related bugs: 373953, 378774
   *)
   "OneIdentity" -> {{1, 6}},
   "Optional" -> {{1, 1}},
   
   (*
   \[Integral] syntax
   *)
   "DifferentialD" -> {{1, 1}},
   "PrincipalValue" -> {{1, 4}},
   
   (*
   dumb BoxData[""] bad input
   *)
   "ServiceSubmit" -> {{1, 7}},
   
   (*
   bad ; ;
   *)
   "WindowMargins" -> {{3, 1}},
   
   (*
   RowBox[{"string","."}]
   *)
   "Paste" -> {{1, 4}},
   
   (* combined *)
   "TraditionalForm" -> {{1, 2}, {2, 2}, {3, 2}},
   "CoxModel" -> {{1, 3}, {1, 9}},
   "Integrate" -> {{3, 1}, {3, 4}}
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
   "WebUnit`WebSessionObject" | "WebWindowObject" | "WeightedData";


Clear[findDiff, findDiff0];
findDiff[a_, b_] :=
  Catch[
   If[a === b, Throw[SameQ]];
   findDiff0[a, b];
   Throw[Indeterminate]
   ];
findDiff0[a_, b_] := (
  If[Length[a] != Length[b],
   Throw[{Length, a}]
   ];
  If[Length[a] == 0,
   If[a =!= b, Throw[a]]
   ];
  Do[findDiff0[a[[i]], b[[i]]], {i, 1, Length[a]}]
  )



End[]

EndPackage[]
