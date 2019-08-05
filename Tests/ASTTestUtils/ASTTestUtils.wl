BeginPackage["ASTTestUtils`"]


parseEquivalenceFunction



parseTest



$LastFailedFileIn
$LastFailedFile
$LastFailedActual
$LastFailedActualString
$LastFailedActualAST
$LastFailedActualCST
$LastFailedExpected
$LastFailedExpectedText
$LastFailedExpectedTextReplaced


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["PacletManager`"]








parseEquivalenceFunction[text_, expectedIgnored_] :=
Catch[
Module[{cst, agg, ast, good, expected, actual},
	
	expected = DeleteCases[ToExpression[text, InputForm, Hold], Null];
	
	(*
	Concrete
	*)
	cst = ConcreteParseString[text, HoldNode[Hold, #[[1]], <||>]&];
	If[FailureQ[cst],
		Throw[cst]
	];
	
	agg = AST`Abstract`Aggregate[cst];
	
	actual = ToExpression[ToInputFormString[agg], InputForm];
	
	good = SameQ[expected, actual];
	If[good,
		True
		,
		Throw[unhandled[<|"actual1"->ToString[actual, InputForm], "expected1"->ToString[expected, InputForm]|>]]
	];
	
	(*
	Abstract
	*)
	ast = ParseString[text, HoldNode[Hold, #[[1]], <||>]&];
	If[FailureQ[ast],
		Throw[ast]
	];
	actual = DeleteCases[ToExpression[ToFullFormString[ast], InputForm], Null];
	
	good = SameQ[expected, actual];
	If[good,
		True
		,
		unhandled[<|"actual2"->ToString[actual, InputForm], "expected2"->ToString[expected, InputForm]|>]
	]
]]
















Options[parseTest] = {"FileNamePrefixPattern" -> "", "FileSizeLimit" -> Infinity};

parseTest[fileIn_String, i_Integer, OptionsPattern[]] :=
 Module[{prefix, res, cst, agg, ast, expected, errors, f, 
   tmp, text, file, errs, tryString, actual, skipFirstLine, 
   firstLine, msgs, textReplaced, version, limit, savedFailure},
  Catch[
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   
   Internal`WithLocalSettings[
    Null
    ,
    file = fileIn;
    If[$Debug, Print["file1: ", file]];
    
    (*If[StringEndsQ[file,".tr"],
    tmp=CreateFile[];
    DeleteFile[tmp];
    CopyFile[file,tmp];
    res=RunProcess[{"sed","-i","''","s/@\\|.*//",tmp}];
    If[res["ExitCode"]=!=0,
    f=Failure["SedFailed",res];
    Print[Row[{"index: ",i," ",AST`Utils`drop[file,prefix]}]];
    Print[Row[{"index: ",i," ",f}]];
    Throw[f]
    ];
    res=RunProcess[{"sed","-i","''","s/@@resource.*//",tmp}];
    If[res["ExitCode"]=!=0,
    f=Failure["SedFailed",res];
    Print[Row[{"index: ",i," ",AST`Utils`drop[file,prefix]}]];
    Print[Row[{"index: ",i," ",f}]];
    Throw[f]
    ];
    file=tmp;
    ];*)
    
    If[FileType[file] === File && FileByteCount[file] > limit,
     ast = 
      Failure["FileTooLarge", <|"FileName" -> file, 
        "FileSize" -> FileSize[file]|>];
     Print[
      Row[{"index: ", i, " ", 
        StringReplace[fileIn, StartOfString ~~ prefix -> ""]}]];
     Print[Row[{"index: ", i, " ", ast[[1]], "; skipping"}]];
     Throw[ast, "OK"]
     ];


    version = convertVersionString[PacletFind["AST"][[1]]["Version"]];
    Which[
      version >= 12,
      cst = 
       ConcreteParseFile[file, 
        HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1]][[1]][[3]][Source][[1]], #[[1]][[-1]][[3]][Source][[2]]}]|>] &];
      ,
     version >= 11,
     cst = 
       ConcreteParseFile[file, 
        HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &];
     ,
     version >= 10,
     cst = 
       ConcreteParseFile[file, 
        HoldNode[Hold, Most[#], <|SyntaxIssues -> Last[#]|>] &];
     ,
     version >= 9,
     cst = ConcreteParseFile[file, HoldNode[Hold, #, <||>] &];
     ,
     IntegerQ[version],
     cst = ConcreteParseFile[file, HoldNode[Hold, {##}, <||>] &];
     ];
    
    (*
    If[$Debug, Print["version: ", version]];
    If[$Debug, Print["cst1: ", cst]];
    If[$Debug, Print["file: ", file, " bytes: ", FileByteCount[file]]];
    If[$Debug, Print[importFile[file]]];
    *)
    
    If[FailureQ[cst],
     If[cst === System`$Failed,
      f = Failure["ConcreteParseFileFailed", <|"FileName" -> file|>];
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Red]];
      Print[Style[Row[{"index: ", i, " ", f}], Red]];
      Throw[f, "Unhandled"]
      ];
     Switch[cst[[1]],
      "FindFileFailed" | "EncodedFile",
      Print[
       Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}]];
      Print[Row[{"index: ", i, " ", cst[[1]], "; skipping"}]];
      Throw[cst, "OK"]
      ,
      "ExitCode",
      f = cst;
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Red]];
      Print[Style[Row[{"index: ", i, " ", f}], Red]];
      Throw[f, "Unhandled"]
      ,
      _,
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[Style[Row[{"index: ", i, " ", cst}], Darker[Orange]]];
      Throw[cst, "Unhandled"]
      ];
     ];
    
    If[MatchQ[cst, FileNode[File, {Null}, _]],
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
       Darker[Orange]]];
     Print[
      Style[Row[{"index: ", i, " ", "No expressions"}], 
       Darker[Orange]]];
     Throw[cst, "OK"]
     ];
    
    If[! FreeQ[cst, _SyntaxErrorNode],
     errs = Cases[cst, _SyntaxErrorNode, {0, Infinity}];
     f = Failure[
       "SyntaxError", <|"FileName" -> file, 
        "SyntaxErrors" -> errs|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Red]];
     Print[Style[Row[{"index: ", i, " ", Shallow[errs]}], Red]];
     savedFailure = f;
     ];
    
    (*If[!($VersionNumber\[GreaterEqual]11.2),
    If[!FreeQ[cst,BinaryNode[TwoWayRule,_,_]],
    f=Failure["TwoWayRule",<|"FileName"\[Rule]file|>];
    Print[Style[Row[{"index: ",i," ",StringReplace[fileIn,
    StartOfString~~prefix\[Rule]""]}],Red]];
    Print[Style[Row[{"index: ",i," ","Use of <->"}],Red]];
    Throw[f,"OK"]
    ];
    ];*)
    
    (*
    issues = cst[[3]][SyntaxIssues];
    errors = 
     Cases[issues, 
      SyntaxIssue[_, _,(*(*"Formatting"|*)"Remark"|"Warning"|"Error"|
       "Fatal"*)_, _]];
    exclude = Cases[errors, SyntaxIssue[_, _, "Formatting", _]];
    errors = Complement[errors, exclude];
    If[errors =!= {},
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
       Darker[Orange]]];
     Scan[Print[Style[Row[{"index: ", i, " ", #}], Darker[Orange]]] &,
       errors];
     ];
    *)

    (*
    Package symbol cannot be imported
    bug 347012
    *)
    If[!FreeQ[cst, LeafNode[Symbol, "Package", _]],
     Print[
      Row[{"index: ", i, " ", 
        StringReplace[fileIn, StartOfString ~~ prefix -> ""]}]];
     Print[
      Row[{"index: ", i, " ", 
        "Package symbol detected (bug 347012); rewriting Package\[Rule]PackageXXX"}]];
     tmp = CreateFile[];
     DeleteFile[tmp];
     CopyFile[file, tmp];
     (*
     If[$Debug, 
      Print["Package file: ", file, " bytes: ", 
       FileByteCount[file]]];
     If[$Debug, Print[importFile[file]]];
     If[$Debug, 
      Print["Package: tmp: ", tmp, " bytes: ", FileByteCount[tmp]]];
     If[$Debug, Print[importFile[tmp]]];
     *)
     res = 
      RunProcess[{"sed", "-i", "''", "s/Package/PackageXXX/", tmp}];
     If[res["ExitCode"] =!= 0,
      f = Failure["SedFailed", res];
      Print[
       Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}]];
      Print[Row[{"index: ", i, " ", f}]];
      Throw[f, "OK"]
      ];
     file = tmp;
     (*
     If[$Debug, 
      Print["file2: ", file, " bytes: ", FileByteCount[file]]];
     If[$Debug, Print[importFile[file]]];
     *)
     
     version = convertVersionString[PacletFind["AST"][[1]]["Version"]];
     Which[
     version >= 12,
      cst = 
        ConcreteParseFile[file, 
         HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1]][[1]][[3]][Source][[1]], #[[1]][[-1]][[3]][Source][[2]]}]|>]&];
      ,
      version >= 11,
      cst = 
        ConcreteParseFile[file, 
         HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &];
      ,
      version >= 10,
      cst = 
        ConcreteParseFile[file, 
         HoldNode[Hold, Most[#], <|SyntaxIssues -> Last[#]|>] &];
      ,
      version >= 9,
      cst = ConcreteParseFile[file, HoldNode[Hold, #, <||>] &];
      ,
      True,
      cst = ConcreteParseFile[file, HoldNode[Hold, {##}, <||>] &];
      ];
     
     ];
     (*
    If[$Debug, Print["cst2: ", cst]];
    *)

    (*
    skip over #! shebang
    *)
    skipFirstLine = False;
    If[FileByteCount[file] > 0,
     firstLine = Import[file, {"Lines", 1}];
     If[StringStartsQ[firstLine, "#!"],
      skipFirstLine = True
      ];
     ];
     
    (*
    after Package has been scanned for, so reading in expected will not hang
    *)
    {text, expected} = importExpected[file, i, prefix, skipFirstLine];
    
    (*
    Now it is ok to throw if there were syntax errors
    
    If expected also had syntax errors, then it would have thrown already,
    so we know that only actual has syntax errors
    
    *)
    If[FailureQ[savedFailure],
    	Throw[savedFailure, "Unhandled"]	
    ];
    
    
    
    (*If[Depth[cst]>170,
    Print[Row[{"index: ",i," ",StringReplace[fileIn,StartOfString~~
    prefix\[Rule]""]}]];
    Print[Row[{"index: ",i," ",
    "Depth > 170; cannot call ToInputFormString by default"}]];
    Throw[cst,"OK"]
    ];*)
    
    agg = AST`Abstract`Aggregate[cst];

    verifyAggregate[agg];

    tryString = ToInputFormString[agg];
    
    If[$DebugString, Print["tryString: ", tryString]];
    
    If[! StringQ[tryString],
     f = Failure[
       "ToInputFormString", <|"FileName" -> file, 
        "tryString" -> tryString|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Red]];
     Print[Style[Row[{"index: ", i, " ", f}], Red]];
     Throw[f, "Unhandled"]
     ];
    
    Quiet@Check[
      actual = DeleteCases[ToExpression[tryString, InputForm], Null];
      (*
      If[$Debug, Print["actual: ", actual]];
      *)
      ,
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[
       Style[Row[{"index: ", i, " ", 
          "Messages while processing actual input (possibly from previous files):"}], Darker[Orange]]];
      Print[
       Style[If[$MessageList =!= {}, $MessageList, 
         "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      msgs = Cases[$MessageList, HoldForm[_::shdw]];
      If[msgs != {},
       Print[
        Style[Row[{"index: ", i, " ", 
           "There were General::shdw messages; rerunning"}], 
         Darker[Orange]]];
       actual = 
        DeleteCases[ToExpression[tryString, InputForm], Null];
       ]
      ];
    
    If[actual =!= expected,
     
     (* occurrences of \[UndirectedEdge] 
     which is confused before 11.2 *)
     If[$VersionNumber < 11.2,
      If[MemberQ[{
          prefix <> 
           "SystemFiles/Components/NeuralNetworks/Types/Inference.m",
          Nothing
          }, file],
        f = Failure["UsingTwoWayRuleBefore112", <|"FileName" -> file|>];
        Print[
         Style[Row[{"index: ", i, " ", 
            StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
        Throw[f, "OK"]
        ];
      ];
     
     $LastFailedFileIn = fileIn;
     $LastFailedFile = file;
     $LastFailedActual = actual;
     $LastFailedActualString = tryString;
     $LastFailedActualCST = cst;
     $LastFailedExpected = expected;
     $LastFailedExpectedText = text;
     f = Failure["ParsingFailure", <|"FileName" -> fileIn|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Bold,
        Red]];
     Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
     Throw[f, "Uncaught"]
     ];
    
    (* boxes *)
    
    (*expectedBoxes=MathLink`CallFrontEnd[
    FrontEnd`ReparseBoxStructurePacket[StringTrim[text]]];
    (*expectedBoxes=expectedBoxes[[1,1]];*)
    (*expectedBoxes=
    expectedBoxes/.{s_String\[RuleDelayed]StringReplace[s,
    "\\\n"\[Rule]""]};*)
    (*If[!ListQ[expectedBoxes],
    expectedBoxes={expectedBoxes};
    ];*)
    If[!FreeQ[expectedBoxes,"\n"],
    expectedBoxes=DeleteCases[expectedBoxes,"\n",2];
    ];
    actualBoxes=CSTToBoxes/@cst[[2]];
    actualBoxes=RowBox[actualBoxes];
    
    If[actualBoxes=!=expectedBoxes,
    $LastFailedFileIn=fileIn;
    $LastFailedFile=file;
    $LastFailedActualBoxes=actualBoxes;
    $LastFailedActualString=tryString;
    $LastFailedActualCST=cst;
    $LastFailedExpectedBoxes=expectedBoxes;
    $LastFailedExpectedText=text;
    f=Failure["ParsingFailureBoxes",<|"FileName"\[Rule]fileIn|>];
    Print[Style[Row[{"index: ",i," ",StringReplace[fileIn,
    StartOfString~~prefix\[Rule]""]}],Bold,Red]];
    Print[Style[Row[{"index: ",i," ",f}],Bold,Red]];
    Throw[f,"Uncaught"]
    ];*)
    
    
    
    (*
    abstracting
    *)

    ast = AST`Abstract`Abstract[agg];

    If[FailureQ[ast],
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Red]];
     Print[Style[Row[{"index: ", i, " ", cst}], Red]];
     Throw[ast, "Unhandled"]
     ];
    
    If[! FreeQ[ast, _SyntaxErrorNode | _AbstractSyntaxErrorNode],
     errs = 
      Cases[ast, _SyntaxErrorNode | _AbstractSyntaxErrorNode, {0, 
        Infinity}];
     f = Failure[
       "SyntaxError2", <|"FileName" -> file, "SyntaxErrors" -> errs|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Red]];
     Print[Style[Row[{"index: ", i, " ", Shallow[errs]}], Red]];
     Throw[f, "OK"]
     ];
    
    (*
    issues = 
     Flatten[Cases[ast, 
       KeyValuePattern[AbstractSyntaxIssues -> issues_] :> issues, {0,
         Infinity}]];
    errors = 
     Cases[issues, SyntaxIssue[_, _,(*"Error"|"Fatal"*)_, _]];
    exclude = Cases[errors, SyntaxIssue[_, _, "Formatting", _]];
    errors = Complement[errors, exclude];
    If[errors =!= {},
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
       Darker[Orange]]];
     Scan[Print[Style[Row[{"index: ", i, " ", #}], Darker[Orange]]] &,
       errors];
     ];
    *)

    tryString = ToFullFormString[ast];

    If[$DebugString, Print["tryString: ", tryString]];

    If[!StringQ[tryString],
     f = Failure[
       "ToFullFormString", <|"FileName" -> file, 
        "tryString" -> tryString|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Red]];
     Print[Style[Row[{"index: ", i, " ", f}], Red]];
     Throw[f, "Unhandled"]
     ];
    
    
    Quiet@Check[
      actual = DeleteCases[ToExpression[tryString, InputForm], Null];
      ,
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[
       Style[Row[{"index: ", i, " ", 
          "Messages while processing actual input (possibly from previous files):"}], Darker[Orange]]];
      Print[
       Style[If[$MessageList =!= {}, $MessageList, 
         "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      msgs = Cases[$MessageList, HoldForm[_::shdw]];
      If[msgs != {},
       Print[
        Style[Row[{"index: ", i, " ", 
           "There were General::shdw messages; rerunning"}], 
         Darker[Orange]]];
       actual = 
        DeleteCases[ToExpression[tryString, InputForm], Null];
       ]
      ];
    
    (*
    in an attempt to mimic to process of abstracting syntax, 
    we make sure to strip off the trailing ; after BeginPackage[],
    Begin[],End, and EndPackage[]
    
    cross our fingers that all occurrences of BeginPackage,Begin,End,
    EndPackage:
    at the beginning of the line
    just has whitespace before ;
    
    *)
    
    textReplaced = text;
    
    (*
    if there are no Package errors, 
    then proceed with replacing the text
    *)
    If[!MemberQ[errors, SyntaxIssue["Package", _, _, _]],
     textReplaced = 
      StringReplace[textReplaced, 
       RegularExpression[
         "(?m)(^(BeginPackage|Begin|End|EndPackage)\\s*\\[[\"a-zA-Z0-9`,\\{\\} \\n\\t\\r]*\\])\\s*;"] -> "$1"];
     (* enough people have End[(**)] that it is worth also replacing *)

          textReplaced = 
      StringReplace[textReplaced, 
       RegularExpression[
         "(?m)(^(End|EndPackage)\\[\\(\\*.*\\*\\)\\])\\s*;"] -> 
        "$1"];
     ];
    
    expected = 
     DeleteCases[ToExpression[textReplaced, InputForm, Hold], Null];
    
    If[actual =!= expected,
     
     (*
     the files here have weird uses of Begin[] etc and it is hard to use regex to remove the trailing ;
     *)
     If[MemberQ[{
        prefix <> "SystemFiles/Components/RobotTools/Kernel/Menu.m",
        
        prefix <> "SystemFiles/Components/Yelp/Kernel/YelpFunctions.m",
        prefix <> "serviceconnections/Yelp/Kernel/YelpFunctions.m",

        prefix <> "SystemFiles/Links/RLink/Kernel/RCodeHighlighter.m",
        prefix <> "rlink/RLink/Kernel/RCodeHighlighter.m",

        prefix <> "SystemFiles/Components/OpenLibrary/Kernel/OpenLibraryFunctions.m",
        prefix <> "serviceconnections/OpenLibrary/Kernel/OpenLibraryFunctions.m",

        prefix <> "SystemFiles/Components/Pushbullet/Kernel/PushbulletAPIFunctions.m",
        prefix <> "serviceconnections/Pushbullet/Kernel/PushbulletAPIFunctions.m",

        prefix <> "alphasource/CalculateParse/ExportedFunctions.m",
        prefix <> "alphasource/CalculateScan/FiniteFieldAndGroupScanner.m",
        prefix <> "alphasource/CalculateScan/TuringMachineScanner.m",
        prefix <> "visualization/Splines/BSplineFunction/Bugs/ModelMaker.m",
        prefix <> "bloomberglink/BloombergLink/UI.m",
        prefix <> "serviceconnections/NYTimes/Kernel/NYTimesAPIFunctions.m",
        prefix <> "TestTools/FrontEnd/CoreGraphicsGrammar.m",
        prefix <> "TestTools/Statistics/NIST/NISTTestTools.m",
        prefix <> "Pubs/OnlineProduction/Applications/DocumentationBuild/Tests/UnitTests/UnitTests.m",
        prefix <> "SystemFiles/Links/JLink/Examples/Part1/BouncingBalls.nb",
        prefix <> "SystemFiles/Links/JLink/Examples/Part1/Palette.nb",
        prefix <> "SystemFiles/Links/JLink/Examples/Part1/Spirograph.nb",
        prefix <> "SystemFiles/Links/NETLink/Examples/Part1/Windows and Dialogs/AsteroidsGame/AsteroidsGame.nb",
        Nothing
        }, fileIn],
      f = Failure["CannotRegexTooWeird", <|"FileName" -> fileIn|>];
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      Throw[f, "OK"]
      ];

      If[MemberQ[{
        (*
        broken BeginPackage[] / EndPackage[] or something
        *)
        prefix <> "codeanalysis/sources/MSource/CodeAnalysis.m",
        prefix <> "applications/ControlSystems/auxTest.m",
        prefix <> "applications/PolynomialControlSystems/PCSauxTest.m",
        prefix <> "control/auxTest2.m",
        prefix <> "control/UpdatedCSP/auxTest.m",
        Nothing
        }, fileIn],
      f = Failure["CannotRegexTooBroken", <|"FileName" -> fileIn|>];
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      Throw[f, "OK"]
      ];
     
      If[MemberQ[{
        (*
        too deep
        *)
        prefix <> "Documentation/English/System/ExamplePages/SymbolicGeometricTransformations.nb",
        Nothing
        }, fileIn],
      f = Failure["TooDeep", <|"FileName" -> fileIn|>];
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      Throw[f, "OK"]
      ];

     (*
     the files here have uses of < - > 
     and it is an older version
     *)
     If[MemberQ[{
        prefix <> 
         "SystemFiles/Components/NeuralNetworks/Inference.m",
        prefix <> "Kernel/StartUp/Language/EquationalProof.m",
        prefix <> "Kernel/StartUp/Language/TreeObjects.m",
        prefix <> 
         "Kernel/StartUp/PlaneGeometry/GeometryConjecture.m",
        prefix <> "Kernel/StartUp/Regions/RegionFunctions/Perimeter.m",
        prefix <> 
         "Kernel/StartUp/Regions/RegionRelations/RegionRelationsLibrary.m",
        prefix <> "Kernel/StartUp/SpatialAnalysis/RegionUtilities.m",
        prefix <> 
         "NaturalLanguageProcessing/NaturalLanguageProcessing/TextCases/PartOfSpeech/Sentences.m",
        prefix <> "NeuralNetworks/NeuralNetworks/Define/Shapes.m",
        prefix <> 
         "NeuralNetworks/NeuralNetworks/Layers/Structural/Transpose.m",
        prefix <> "NeuralNetworks/NeuralNetworks/Types/Inference.m",
        prefix <> "NeuralNetworks/Tests/Formats/Upgrade.m",
        prefix <> "NeuralNetworks/Tests/Training/CopyNet.m",
        Nothing
        }, fileIn],
      f = Failure["OldTwoWayRule", <|"FileName" -> fileIn|>];
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      Throw[f, "OK"]
      ];
     
     (*
     
     file has something like  a; ?b   and the kernel and AST disagree \
(AST is correct)
     *)
     
     If[!FreeQ[expected, 
        HoldPattern[Information][_, LongForm -> False]],
      f = Failure["Information?Syntax", <|"FileName" -> file|>];
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      Throw[f, "OK"]
      ];
     
     $LastFailedFileIn = fileIn;
     $LastFailedFile = file;
     $LastFailedActual = actual;
     $LastFailedActualString = tryString;
     $LastFailedActualAST = ast;
     $LastFailedExpected = expected;
     $LastFailedExpectedText = text;
     $LastFailedExpectedTextReplaced = textReplaced;
     f = Failure["ParsingFailure2", <|"FileName" -> fileIn|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Bold,
        Red]];
     Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
     Throw[f, "Unhandled"]
     ];
    ,
    Null
    ]
   ,
   "OK"
   ,
   (If[StringQ[tmp] && FileExistsQ[tmp], DeleteFile[tmp]];) &
   ]
  ]



(*
convert "0.9" to 9
*)
Clear[convertVersionString]
convertVersionString[s_String /; StringMatchQ[s, "0." ~~ _]] := 
 FromDigits[StringDrop[s, 2]]
convertVersionString[s_String /; StringMatchQ[s, "0." ~~ _ ~~ _]] := 
 FromDigits[StringDrop[s, 2]]



(*

Cannot use Import[file, "Text"] because it drops \r from \r\n

Cannot use Import[file, "String"] because it assumes "Unicode" character encoding

*)
importFile[file_String] := FromCharacterCode[Import[file, "Byte"], "UTF8"]





(*
Verify that the source of a parent node accurately reflects the sources of children nodes
*)
verifyAggregate[LeafNode[_, _, _]] := Null

verifyAggregate[agg:(_[tag_, children_, data_])] :=
Catch[
Module[{f, first, last, firstSource, lastSource},
  
  If[empty[children],
    If[KeyExistsQ[data, Source],
      f = Failure["EmptyChildren", <|"Aggregate" -> agg|>];
      Throw[f, "Unhandled"]
    ];

    Throw[Null]
  ];

  first = children[[1]];
  last = children[[-1]];
  firstSource = first[[3]][Source];
  lastSource = last[[3]][Source];

  src = data[Source];

  If[src[[1]] =!= firstSource[[1]],
    f = Failure["MismatchedFirst", <|"Aggregate" -> agg, "Expected"->src[[1]], "Actual"->firstSource[[1]]|>];
    Throw[f, "Unhandled"]
  ];

  If[src[[2]] =!= lastSource[[2]],
    f = Failure["MismatchedLast", <|"Aggregate" -> agg, "Expected"->src[[2]], "Actual"->lastSource[[2]]|>];
    Throw[f, "Unhandled"]
  ];
]]











importExpected[file_, i_, prefix_, skipFirstLine_] :=
Module[{text, f, expected, msgs, crPos, lfPos},
       (*
      expected
     *)
         Quiet@Check[
      
      text = importFile[file];
      If[skipFirstLine,
      	(*
      	find first line and skip
      	*)
        crPos = StringPosition[text, "\r"];
        lfPos = StringPosition[text, "\n"];
        Which[
        	(* no newlines *)
        	crPos == {} && lfPos == {},
        		f = Failure["EmptyScript", <|"FileName" -> file|>];
			     Print[
			      Style[Row[{"index: ", i, " ", 
			         StringReplace[file, StartOfString ~~ prefix -> ""]}], Bold,
			        Red]];
			     Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
			     Throw[f, "Unhandled"]
        	,
        	(* stray \r as newlines *)
        	crPos != {} && lfPos == {},
        		text = StringDrop[text, crPos[[1,1]]];
        	,
        	(* \n newlines *)
        	crPos == {} && lfPos != {},
        		text = StringDrop[text, lfPos[[1,1]]];
        	,
        	(*crPos != {} && lfPos != {}*)
        	(* Windows \r\n newline *)
        	crPos[[1,1]] + 1 == lfPos[[1,1]],
        		text = StringDrop[text, lfPos[[1,1]]];
        	,
        	(* stray \r as newlines *)
        	crPos[[1,1]] < lfPos[[1,1]],
        		text = StringDrop[text, crPos[[1,1]]];
        	,
        	(* crPos[[1,1]] > lfPos[[1,1]] *)
        	True,
        		text = StringDrop[text, lfPos[[1,1]]];
        ]
       ];
       (*
      text = StringJoin[Riffle[text, "\n"]];
      *)
      
      (*
      Handle unsupported characters
      *)
      
      (*
      FIXME: does not handle  \\\[NumberComma]
      that is, the \ that is before \[NumberComma] may be escaped...
      I don't feel like doing this properly right now...
      *)
      text = StringReplace[text, {RegularExpression["(?<!\\\\)\\\\\\[NumberComma\\]"] -> "\\:f7fc"}];

      expected = 
       DeleteCases[ToExpression[text, InputForm, Hold], Null];
      (*
      If[$Debug, Print["expected: ", expected]];
      *)
      ,
      Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[file, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[
       Style[Row[{"index: ", i, " ", 
          "Messages while processing expected input (possibly from previous files):"}], Darker[Orange]]];
      Print[
       Style[If[$MessageList =!= {}, $MessageList, 
         "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      msgs = Cases[$MessageList, HoldForm[_::shdw]];
      If[msgs != {},
       Print[
        Style[Row[{"index: ", i, " ", 
           "There were General::shdw messages; rerunning"}], 
         Darker[Orange]]];
       expected = 
        DeleteCases[ToExpression[text, InputForm, Hold], Null];
       ]
      ];
    
    If[expected === System`$Failed,
     f = Failure["SyntaxError", <|"FileName" -> file|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[file, StartOfString ~~ prefix -> ""]}], Red]];
     Print[Style[Row[{"index: ", i, " ", f}], Red]];
     Throw[f, "OK"]
     ];

     {text, expected}
]












End[]

EndPackage[]
