BeginPackage["CodeParser`Library`"]

loadFunc

loadAllFuncs

libraryFunctionWrapper


(*
library functions calling INTO lib
*)
createParserSessionFunc
destroyParserSessionFunc
concreteParseBytesFunc
concreteParseFileFunc
tokenizeBytesFunc
tokenizeFileFunc
concreteParseLeafFunc
safeStringFunc



(*
library functions coming FROM lib
*)
LongNameSuggestion
(*
SetConcreteParseProgress
*)


$ParserSession


$ConcreteParseProgress
$ConcreteParseTime
$ConcreteParseStart



Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)


CodeParser::old2 = "`1` functionality is only supported in versions 13.1+ and $VersionNumber is `2`."

CodeParser::notransport = "No transport specified."

CodeParser::noload = "`1` could not be loaded. `2`"



(*
TODO: when targeting 12.1 as a minimum, then look into doing paclet["AssetLocation", "LibraryResources"] or similar
*)
location = "Location" /. PacletInformation["CodeParser"]

libraryResources = FileNameJoin[{location, "LibraryResources", $SystemID}]

pacletInfoFile = FileNameJoin[{location, "PacletInfo.wl"}]

Block[{$ContextPath = {"PacletManager`", "System`"}, $Context = "CodeParser`Library`Private`"},
  (*
  put PacletManager` on $ContextPath to guarantee using PacletManager`Paclet symbol
  *)
  pacletInfo = Get[pacletInfoFile];
]

transport = Transport /. List @@ pacletInfo


$ParserSession := $ParserSession =
Module[{},

  If[$Debug,
    Print["memoizing $ParserSession"]
  ];

  libraryFunctionWrapper[createParserSessionFunc]
]


$CodeParserLib := $CodeParserLib =
Catch[
Module[{lib},

  If[$Debug,
    Print["memoizing $CodeParserLib"]
  ];

  lib = FileNameJoin[{libraryResources, "CodeParser."<>$sharedExt}];

  If[!FileExistsQ[lib],
    Throw[Failure["CodeParserNativeLibraryNotFound", <| "ExpectedLocation" -> lib |>]]
  ];

  lib
]]

$ExprLib := $ExprLib =
Catch[
Module[{lib},

  If[$Debug,
    Print["memoizing $ExprLib"]
  ];

  lib = FileNameJoin[{libraryResources, "libexpr."<>$sharedExt}];

  If[!FileExistsQ[lib],
    Throw[Failure["ExprNativeLibraryNotFound", <| "ExpectedLocation" -> lib |>]]
  ];

  lib
]]



$sharedExt = 
  Switch[$OperatingSystem, 
    "MacOSX", "dylib",
    "Windows", "dll",
    _, "so"
  ]



loadFunc[name_String, params_, ret_] :=
Catch[
Module[{res, loaded, linkObject},

  If[FailureQ[$CodeParserLib],
    With[{fail = $CodeParserLib},
      Throw[
        Function[fail]
      ]
    ]
  ];

  If[{params, ret} =!= {LinkObject, LinkObject},

    (*
    "regular" LibraryLink with no MathLink silliness
    *)
    loaded = LibraryFunctionLoad[$CodeParserLib, name, params, ret];

    If[Head[loaded] =!= LibraryFunction,
      With[{fail = Failure["LibraryFunctionLoad", <| "Arguments" -> {$CodeParserLib, name, params, ret}, "Result" -> loaded |>]},
        Throw[
          Function[fail]
        ]
      ]
    ];

    (*
    give a message and return a failure if called with arguments that do not match pattern
    *)
    With[{loaded = loaded},
      Throw[
        Function[
          Function[{res},
            If[MatchQ[res, HoldPattern[LibraryFunction[___]][___]],
              Message[LibraryFunction::unevaluated, loaded, {##}];
              Failure["Unevaluated", <| "Function" -> loaded, "Arguments" -> {##} |>]
              ,
              res
            ]
          ][loaded[##]]
        ]
      ]
    ]
  ];

  (*
  LibraryLink creates a separate loopback link for each library function
  *)
  res = newestLinkObject[LibraryFunctionLoad[$CodeParserLib, name, params, ret]];

  If[FailureQ[res],
    With[{fail = res},
      Throw[
        Function[fail]
      ]
    ]
  ];

  {loaded, linkObject} = res;

  If[FailureQ[loaded],
    With[{fail = loaded},
      Throw[
        Function[fail]
      ]
    ]
  ];

  If[Head[loaded] =!= LibraryFunction,
    With[{fail = Failure["LibraryFunctionLoad",  <| "Arguments" -> {$CodeParserLib, name, params, ret}, "Result" -> loaded |>]},
      Throw[
        Function[fail]
      ]
    ]
  ];

  (*
  send fully-qualified symbol names over the wire
  library->kernel traffic has fully-qualified symbols.
  This allows LibraryLink traffic to work when CodeParser` is not on $ContextPath.
  And we want kernel->library traffic to match this behavior, to minimize surprises.
  Note: this still does not enable sending fully-qualified System` symbols
  bug 283291
  bug 284492
  *)
  MathLink`LinkSetPrintFullSymbols[linkObject, True];

  (*
  give a message and return a failure if called with arguments that do not match pattern
  *)
  With[{loaded = loaded},
    Function[
      Function[{res},
        If[MatchQ[res, HoldPattern[LibraryFunction[___]][___]],
          Message[LibraryFunction::unevaluated, loaded, {##}];
          Failure["Unevaluated", <| "Function" -> loaded, "Arguments" -> {##} |>]
          ,
          res
        ]
      ][loaded[##]]
    ]
  ]
]]



Which[
  transport === "ExprLib",

    createParserSessionFunc := createParserSessionFunc = fromPointerA @* (If[$Debug, Print["memoizing createParserSessionFunc"]]; $ExprLibCompiledLibFuns; loadFunc["CreateParserSession_LibraryLink", { }, Integer]);

    destroyParserSessionFunc := destroyParserSessionFunc = fromPointerA @* (If[$Debug, Print["memoizing destroyParserSessionFunc"]]; $ExprLibCompiledLibFuns; loadFunc["DestroyParserSession_LibraryLink", { Integer }, Integer]);

    concreteParseBytesFunc := concreteParseBytesFunc = fromPointerA @* (If[$Debug, Print["memoizing concreteParseBytesFunc"]]; $ExprLibCompiledLibFuns; loadFunc["ConcreteParseBytes_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"}, Integer, Integer, Integer }, Integer]);

    concreteParseFileFunc := concreteParseFileFunc = fromPointerA @* (If[$Debug, Print["memoizing concreteParseFileFunc"]]; $ExprLibCompiledLibFuns; loadFunc["ConcreteParseFile_LibraryLink", { Integer, "UTF8String", Integer, Integer, Integer }, Integer]);

    tokenizeBytesFunc := tokenizeBytesFunc = fromPointerA @* (If[$Debug, Print["memoizing tokenizeBytesFunc"]]; $ExprLibCompiledLibFuns; loadFunc["TokenizeBytes_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"}, Integer, Integer, Integer }, Integer]);

    tokenizeFileFunc := tokenizeFileFunc = fromPointerA @* (If[$Debug, Print["memoizing tokenizeFileFunc"]]; $ExprLibCompiledLibFuns; loadFunc["TokenizeFile_LibraryLink", { Integer, "UTF8String", Integer, Integer, Integer }, Integer]);

    concreteParseLeafFunc := concreteParseLeafFunc = fromPointerA @* (If[$Debug, Print["memoizing concreteParseLeafFunc"]]; $ExprLibCompiledLibFuns; loadFunc["ConcreteParseLeaf_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"}, Integer, Integer, Integer, Integer, Integer }, Integer]);

    safeStringFunc := safeStringFunc = fromPointerA @* (If[$Debug, Print["memoizing safeStringFunc"]]; $ExprLibCompiledLibFuns; loadFunc["SafeString_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"} }, Integer]);
  ,
  transport === "MathLink",

    createParserSessionFunc := createParserSessionFunc = (If[$Debug, Print["memoizing createParserSessionFunc"]]; loadFunc["CreateParserSession_LibraryLink", LinkObject, LinkObject]);

    destroyParserSessionFunc := destroyParserSessionFunc = (If[$Debug, Print["memoizing destroyParserSessionFunc"]]; loadFunc["DestroyParserSession_LibraryLink", LinkObject, LinkObject]);

    concreteParseBytesFunc := concreteParseBytesFunc = (If[$Debug, Print["memoizing concreteParseBytesFunc"]]; loadFunc["ConcreteParseBytes_LibraryLink", LinkObject, LinkObject]);

    concreteParseFileFunc := concreteParseFileFunc = (If[$Debug, Print["memoizing concreteParseFileFunc"]]; loadFunc["ConcreteParseFile_LibraryLink", LinkObject, LinkObject]);

    tokenizeBytesFunc := tokenizeBytesFunc = (If[$Debug, Print["memoizing tokenizeBytesFunc"]]; loadFunc["TokenizeBytes_LibraryLink", LinkObject, LinkObject]);

    tokenizeFileFunc := tokenizeFileFunc = (If[$Debug, Print["memoizing tokenizeFileFunc"]]; loadFunc["TokenizeFile_LibraryLink", LinkObject, LinkObject]);

    concreteParseLeafFunc := concreteParseLeafFunc = (If[$Debug, Print["memoizing concreteParseLeafFunc"]]; loadFunc["ConcreteParseLeaf_LibraryLink", LinkObject, LinkObject]);

    safeStringFunc := safeStringFunc = (If[$Debug, Print["memoizing safeStringFunc"]]; loadFunc["SafeString_LibraryLink", LinkObject, LinkObject]);
  ,
  True,
    Message[CodeParser::notransport]
]



$ExprLibCompiledLibFuns := $ExprLibCompiledLibFuns =
Catch[
Module[{exprLibCompiledLib},

  If[$Debug,
    Print["memoizing $ExprLibCompiledLibFuns"]
  ];

  If[$VersionNumber < 13.1,
    Message[CodeParser::old2, "ExprLib", $VersionNumber];
    Throw[Failure["CodeParserOld2", <||>]]
  ];

  If[FailureQ[$ExprLib],
    Message[CodeParser::noload, "ExprLib", $ExprLib];
    Throw[$ExprLib]
  ];

  Needs["CompiledLibrary`"];

  exprLibCompiledLib = CompiledLibrary`CompiledLibrary[$ExprLib];

  CompiledLibrary`CompiledLibraryLoadFunctions[exprLibCompiledLib]
]]



fromPointerA[res_Integer] :=
  $ExprLibCompiledLibFuns["Expr_FromPointerA"][res]

(*
all other Failure or LibraryFunctionError
*)
fromPointerA[f_] :=
  f



Attributes[newestLinkObject] = {HoldFirst}

(*
Return the LinkObject that is created when evaluating expr along with the result of evaluating expr

this is all just to find the LinkObject associated with this LibraryFunction

TODO: If there is ever a nicer way to find the LinkObject, then use that
*)
newestLinkObject[expr_] :=
Catch[
Module[{before, after, res, set, first},
  before = Links[];
  (*evaluate*)
  res = expr;
  If[FailureQ[res],
    Throw[res]
  ];
  after = Links[];
  If[before == after,
    Throw[Failure["LinksDidNotChange", <||>]]
  ];
  set = Complement[after, before];
  If[Length[set] != 1,
    Throw[Failure["InternalLinksError", <| "Before" -> before, "After" -> after |>]]
  ];
  first = set[[1]];
  {res, first}
]]


(*
Handle the errors that may occur when calling LibraryLink functions
*)
libraryFunctionWrapper[libFunc_, args___] :=
Catch[
Module[{res},

  If[FailureQ[libFunc],
    Throw[libFunc]
  ];

  If[AnyTrue[{args}, FailureQ],
    Throw[SelectFirst[{args}, FailureQ]]
  ];

  (*
  in the event of an abort, force reload of functions
  This will fix the transient error that can happen when an abort occurs
  and the next use throws LIBRARY_FUNCTION_ERROR
  *)
  CheckAbort[
  res = libFunc[args];
  ,
  loadAllFuncs[];
  Abort[]
  ];

  (*
  There may still be a hiccup when there is a LIBRARY_FUNCTION_ERROR and the next
  use of the function returns unevaluated
  *)
  If[MatchQ[res, _LibraryFunctionError | Verbatim[LibraryFunction][___][___]],
    (*
    Need to specify PageWidth, or else ToString does not do anything with Short
    Related bugs: ?
    *)
    Throw[Failure["LibraryFunctionError", <|
        "ShortResult" -> ToString[Short[res], OutputForm, PageWidth -> 100],
        (*
        "ShortArguments" and "Arguments" is really just taking up space to force "FullResult" to be hidden by default
        *)
        "ShortArguments" -> ToString[Short[{args}], OutputForm, PageWidth -> 100],
        "Arguments" -> {args},
        "FullResult" -> res
      |>]
    ]
  ];

  If[MatchQ[res, _libFunc],
    Throw[Failure["Unevaluated", <| "Result" -> res |>]]
  ];

  res
]]



(*
SetConcreteParseProgress[prog_] := (
  $ConcreteParseProgress = prog;
  If[prog == 100,
    $ConcreteParseTime = Now - $ConcreteParseStart;
  ];
)
*)



$longNames

(* TODO(cleanup): This has been replaced with a native implementation based on
	edit-distance. Remove this definition and the ExprLib binding to it?
	Also remove the generated LongNames.wl file? *)
LongNameSuggestion[input_String] :=
Catch[
Module[{nearest, longNamesFile},
  If[$Debug,
    Print["input: ", input];
  ];

  (*
  lazy initialization of $longNames
  *)
  If[!ListQ[$longNames],

    longNamesFile = FileNameJoin[{location, "Resources", "Generated", "LongNames.wl"}];

    $longNames = Quiet[Get[longNamesFile], {Get::noopen}];
    If[FailureQ[$longNames],
      Throw["LongNameSuggestionFailure"]
    ];
  ];
  
  nearest = Nearest[$longNames, input, {1, 2}];
  If[nearest == {},
    Throw[""]
  ];

  If[$Debug,
    Print["nearest: ", nearest[[1]]];
  ];
  nearest[[1]]
]]



End[]

EndPackage[]
