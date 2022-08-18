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


CodeParser::old2 = "ExprLibrary functionality is only supported in versions 13.1+."

CodeParser::notransport = "No transport specified."

CodeParser::exprlib = "ExprLib could not be loaded. `1`"



(*
TODO: when targeting 12.1 as a minimum, then look into doing paclet["AssetLocation", "LibraryResources"] or similar
*)
location = "Location" /. PacletInformation["CodeParser"]

libraryResources = FileNameJoin[{location, "LibraryResources", $SystemID}]



$ParserSession := $ParserSession =
  libraryFunctionWrapper[createParserSessionFunc]



$CodeParserLib := $CodeParserLib =
Catch[
Module[{lib},

  lib = FileNameJoin[{libraryResources, "CodeParser."<>$sharedExt}];

  If[!FileExistsQ[lib],
    Throw[Failure["CodeParserNativeLibraryNotFound", <| "ExpectedLocation" -> lib |>]]
  ];

  lib
]]

$ExprLib := $ExprLib =
Catch[
Module[{lib},

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




loadAllFuncs[] :=
Module[{pacletInfo, pacletInfoFile, transport},

  pacletInfoFile = FileNameJoin[{location, "PacletInfo.wl"}];

  Block[{$ContextPath = {"System`"}, $Context = "CodeParser`Library`Private`"},
    pacletInfo = Get[pacletInfoFile];
  ];

  transport = Transport /. List @@ pacletInfo;

  Which[
    transport === "ExprLib",

      loadExprLibFuncs[];

      createParserSessionFunc := createParserSessionFunc = fromPointerA @* loadFunc["CreateParserSession_LibraryLink", { }, Integer];

      destroyParserSessionFunc := destroyParserSessionFunc = fromPointerA @* loadFunc["DestroyParserSession_LibraryLink", { Integer }, Integer];

      concreteParseBytesFunc := concreteParseBytesFunc = fromPointerA @* loadFunc["ConcreteParseBytes_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"}, Integer, Integer, Integer }, Integer];

      concreteParseFileFunc := concreteParseFileFunc = fromPointerA @* loadFunc["ConcreteParseFile_LibraryLink", { Integer, "UTF8String", Integer, Integer, Integer }, Integer];

      tokenizeBytesFunc := tokenizeBytesFunc = fromPointerA @* loadFunc["TokenizeBytes_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"}, Integer, Integer, Integer }, Integer];

      tokenizeFileFunc := tokenizeFileFunc = fromPointerA @* loadFunc["TokenizeFile_LibraryLink", { Integer, "UTF8String", Integer, Integer, Integer }, Integer];

      concreteParseLeafFunc := concreteParseLeafFunc = fromPointerA @* loadFunc["ConcreteParseLeaf_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"}, Integer, Integer, Integer, Integer, Integer }, Integer];

      safeStringFunc := safeStringFunc = fromPointerA @* loadFunc["SafeString_LibraryLink", { Integer, {LibraryDataType[ByteArray], "Shared"} }, Integer];
    ,
    transport === "MathLink",

      createParserSessionFunc := createParserSessionFunc = loadFunc["CreateParserSession_LibraryLink", LinkObject, LinkObject];

      destroyParserSessionFunc := destroyParserSessionFunc = loadFunc["DestroyParserSession_LibraryLink", LinkObject, LinkObject];

      concreteParseBytesFunc := concreteParseBytesFunc = loadFunc["ConcreteParseBytes_LibraryLink", LinkObject, LinkObject];

      concreteParseFileFunc := concreteParseFileFunc = loadFunc["ConcreteParseFile_LibraryLink", LinkObject, LinkObject];

      tokenizeBytesFunc := tokenizeBytesFunc = loadFunc["TokenizeBytes_LibraryLink", LinkObject, LinkObject];

      tokenizeFileFunc := tokenizeFileFunc = loadFunc["TokenizeFile_LibraryLink", LinkObject, LinkObject];

      concreteParseLeafFunc := concreteParseLeafFunc = loadFunc["ConcreteParseLeaf_LibraryLink", LinkObject, LinkObject];

      safeStringFunc := safeStringFunc = loadFunc["SafeString_LibraryLink", LinkObject, LinkObject];
    ,
    True,
      Message[CodeParser::notransport]
  ]
]


loadExprLibFuncs[] :=
Catch[
Module[{exprCompiledLib},
  
  If[FailureQ[$ExprLib],
    Message[CodeParser::exprlib, $ExprLib];
    Throw[Null]
  ];

  Needs["CompiledLibrary`"];

  exprCompiledLib = CompiledLibrary`CompiledLibrary[$ExprLib];

  If[$VersionNumber < 13.1,
    Message[CodeParser::old2]
  ];

  $exprCompiledLibFuns = CompiledLibrary`CompiledLibraryLoadFunctions[exprCompiledLib];

  Null
]]


fromPointerA[f_?FailureQ] :=
  f

fromPointerA[res_Integer] :=
  $exprCompiledLibFuns["Expr_FromPointerA"][res]



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
