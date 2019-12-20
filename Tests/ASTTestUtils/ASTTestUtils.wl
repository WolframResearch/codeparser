BeginPackage["ASTTestUtils`"]


parseEquivalenceFunction



parseTest

ok



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
Module[{cst, agg, ast, good, expected, actual, str, str1},
	
	expected = DeleteCases[ToExpression[text, InputForm, Hold], Null];
	
	(*
	Concrete
	*)
	cst = ConcreteParseString[text, HoldNode[Hold, #[[1]], <||>]&];
	If[FailureQ[cst],
		Throw[cst]
	];
	
	If[!TrueQ[testLeafNodeOrder[cst]],
		Throw[OutOfOrder]
	];
	
	str = ToSourceCharacterString[cst];
	If[!FailureQ[str],
		actual = DeleteCases[ToExpression[str, InputForm], Null];
		,
		actual = $Failed;
	];

  good = SameQ[expected, actual];
  If[good,
    True
    ,
    Throw[unhandled[<|"actualConcrete"->ToString[actual, InputForm, CharacterEncoding -> "PrintableASCII"],
    					"expectedConcrete"->ToString[expected, InputForm, CharacterEncoding -> "PrintableASCII"],
    					"str"->ToString[str, InputForm, CharacterEncoding -> "PrintableASCII"]|>]]
  ];

  If[$Debug,
    Print["cst: ", cst];
    Print["concrete expected: ", expected];
    Print["concrete actual: ", actual]
  ];

  (*
  Aggregate
  *)
	agg = AST`Abstract`Aggregate[cst];
	
	str = ToInputFormString[agg];
	If[!FailureQ[actual],
		actual = DeleteCases[ToExpression[str, InputForm], Null];
		,
		actual = $Failed;
	];
	
	good = SameQ[expected, actual];
	If[good,
		True
		,
		Throw[unhandled[<|"actualAggregate"->ToString[actual, InputForm], "expectedAggregate"->ToString[expected, InputForm]|>]]
	];
	
  If[$Debug,
    Print["agg concrete expected: ", expected];
    Print["agg concrete actual: ", actual]
  ];

	(*
	Abstract
	*)
	ast = ParseString[text, HoldNode[Hold, #[[1]], <||>]&];
	If[FailureQ[ast],
		Throw[ast]
	];
	
	str1 = ToFullFormString[ast];
	If[!FailureQ[str1],
		actual = DeleteCases[ToExpression[str1, InputForm], Null];
		,
		actual = $Failed
	];
	
	good = SameQ[expected, actual];
	If[!good,
		Throw[unhandled[<|"actualAbstract"->ToString[actual, InputForm], "expectedAbstract"->ToString[expected, InputForm]|>]]
	];


  (*
  Now test agreement
  *)

  If[FailureQ[str1],
    (* actual is failure, expected is NOT failure *)
    If[!FailureQ[expected],
      Throw[unhandled["ActualFailure", <|"actualAbstract"->ToString[str1, InputForm], "expectedAbstract"->ToString[expected, InputForm]|>]]
    ]
    ,
    (* actual is NOT failure, expected is failure *)
    If[FailureQ[expected],
      Throw[unhandled["ActualNotFailure", <|"actualAbstract"->ToString[str1, InputForm], "expectedAbstract"->ToString[expected, InputForm]|>]]
    ]
  ];

  If[$Debug,
    Print["abs expected: ", expected];
    Print["abs actual: ", actual]
  ];

  True

]]
















Options[parseTest] = {"FileNamePrefixPattern" -> "", "FileSizeLimit" -> {0, Infinity}};

parseTest[fileIn_String, i_Integer, OptionsPattern[]] :=
 Module[{prefix, res, cst, agg, ast, expected, errors, f, 
   tmp, text, file, errs, tryString, actual, msgs, textReplaced, version, limit, savedFailure,
   firstLine},
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
    
    If[FileType[file] =!= File,
    	Throw[Failure["NotAFile", <|"File"->file|>], "OK"]
    ];
    
     If[FileByteCount[file] > limit[[2]],
      ast = 
      Failure["FileTooLarge", <|"FileName" -> file, 
        "FileSize" -> FileSize[file]|>];
     Throw[ast, "OK"]
     ];
     If[FileByteCount[file] < limit[[1]],
      ast = 
      Failure["FileTooSmall", <|"FileName" -> file, 
        "FileSize" -> FileSize[file]|>];
     Throw[ast, "OK"]
     ];

    (*
    figure out if first line is special
    *)
    If[FileByteCount[file] > 0,
      Quiet[
        (*
        Importing a file containing only \n gives a slew of different messages and fails
        bug 363161
        Remove this Quiet when bug is resolved
        *)
        firstLine = Import[file, {"Lines", 1}];
        If[FailureQ[firstLine],
          firstLine = "";
        ]
      ];
      Which[
        (* special encoded file format *)
        StringMatchQ[firstLine, "(*!1"~~("A"|"B"|"C"|"D"|"H"|"I"|"N"|"O")~~"!*)mcm"],
        Throw[Failure["EncodedFile", <|"FileName"->file|>], "OK"]
        ,
        (* wl script *)
        StringStartsQ[firstLine, "#!"],
        Throw[Failure["WLScript", <|"FileName"->file|>], "OK"]
      ];
    ];


    version = convertVersionString[PacletFind["AST"][[1]]["Version"]];
    If[$Debug,
    	Print["version: ", version]
    ];
    Which[
      version >= 12,
      cst = 
       ConcreteParseFile[File[file], 
        HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>] &];
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
      "EncodedFile",
      Throw[cst, "OK"]
      ,
      "FindFileFailed",
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
    
    If[!FreeQ[cst, _SyntaxErrorNode],
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


     If[!FreeQ[cst, FormatIssue["CharacterEncoding", _, _, _]],
      Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
       Darker[Orange]]];
     Print[
      Style[Row[{"index: ", i, " ", "Bad UTF-8 encoding"}], 
       Darker[Orange]]];
     Throw[cst, "OK"]
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
        ConcreteParseFile[File[file], 
         HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>]&];
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

    If[$Debug, Print["cst: ", cst]];

    (*
    skip over #! shebang
    *)
    (*
    skipFirstLine = False;
    If[FileByteCount[file] > 0,
     firstLine = Import[file, {"Lines", 1}];
     If[StringStartsQ[firstLine, "#!"],
      skipFirstLine = True
      ];
     ];
     *)
    (*
    after Package has been scanned for, so reading in expected will not hang
    *)
    {text, expected} = importExpected[file, i, prefix];
    
    If[$Debug,
      Print["importExpected: {text, expected}: ", {text, expected}]
    ];

    (*
    Now it is ok to throw if there were syntax errors
    
    If expected also had syntax errors, then it would have thrown already,
    so we know that only actual has syntax errors
    
    *)
    If[FailureQ[savedFailure],
    	Throw[savedFailure, "Unhandled"]	
    ];
    
    If[!TrueQ[testLeafNodeOrder[cst]],
		Throw[OutOfOrder]
	];
    
    tryString = ToSourceCharacterString[cst];
    
    If[$DebugString, Print["ToSourceCharacterString[cst]: ", tryString]];
    
    If[! StringQ[tryString],
     f = Failure[
       "ToSourceCharacterString", <|"FileName" -> file, 
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
     f = Failure["ConcreteParsingFailure", <|"FileName" -> fileIn|>];
     Print[
      Style[Row[{"index: ", i, " ", 
         StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Bold,
        Red]];
     Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
     Throw[f, "Uncaught"]
     ];
    
    (*If[Depth[cst]>170,
    Print[Row[{"index: ",i," ",StringReplace[fileIn,StartOfString~~
    prefix\[Rule]""]}]];
    Print[Row[{"index: ",i," ",
    "Depth > 170; cannot call ToInputFormString by default"}]];
    Throw[cst,"OK"]
    ];*)
    

    (*
    Aggregate
    *)
    agg = AST`Abstract`Aggregate[cst];

    If[$Debug, Print["agg: ", agg]];

    (*
    verifyAggregate[agg];
    *)
    
    tryString = ToInputFormString[agg];
    
    If[$DebugString, Print["ToInputFormString[agg]: ", tryString]];
    
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
     
     $LastFailedFileIn = fileIn;
     $LastFailedFile = file;
     $LastFailedActual = actual;
     $LastFailedActualString = tryString;
     $LastFailedActualAgg = agg;
     $LastFailedExpected = expected;
     $LastFailedExpectedText = text;
     f = Failure["AggregateParsingFailure", <|"FileName" -> fileIn|>];
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

    If[$Debug, Print["ast: ", ast]];

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

    If[$DebugString, Print["ToFullFormString[ast]: ", tryString]];

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
    
    If[$Debug,
      Print["textReplaced1: ", textReplaced];
    ];

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
    
    If[$Debug,
      Print["textReplaced2: ", textReplaced];
    ];

    expected = DeleteCases[ToExpression[textReplaced, InputForm, Hold], Null];
    
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
        prefix <> "CompileUtilities/CompileUtilities/RuntimeChecks/RuntimeChecks.m",
        prefix <> "SystemFiles/Components/CompileUtilities/RuntimeChecks/RuntimeChecks.m",
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
     
     file has something like  a; ?b   and the kernel and AST disagree
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
     f = Failure["AbstractParsingFailure", <|"FileName" -> fileIn|>];
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
   (If[StringQ[tmp] && FileExistsQ[tmp], DeleteFile[tmp]];ok) &
   ]
  ]



(*
convert "0.9" to 9
*)
Clear[convertVersionString]
convertVersionString[s_String /; StringMatchQ[s, "0." ~~ _]] := FromDigits[StringDrop[s, 2]]
convertVersionString[s_String /; StringMatchQ[s, "0." ~~ _ ~~ _]] := FromDigits[StringDrop[s, 2]]
convertVersionString[s_String /; StringMatchQ[s, "999"]] := 9990



(*

Cannot use Import[file, "Text"] because it drops \r from \r\n

Cannot use Import[file, "String"] because it assumes "Unicode" character encoding

*)
importFile[file_String] := FromCharacterCode[Import[file, "Byte"], "UTF8"]





(*
Verify that the source of a parent node accurately reflects the sources of children nodes
*)
(*
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
*)










importExpected[file_, i_, prefix_] :=
Module[{text, f, expected, msgs},
       (*
      expected
     *)
         Quiet@Check[
      
      text = importFile[file];
      
       (*
      text = StringJoin[Riffle[text, "\n"]];
      *)

      (*
      Handle unsupported characters
      
      FIXME: does not handle  \\\[NumberComma]
      that is, the \ that is before \[NumberComma] may be escaped...
      I don't feel like doing this properly right now...
      *)
      text = StringReplace[text, {RegularExpression["(?<!\\\\)\\\\\\[NumberComma\\]"] -> "\\:f7fc"}];

      If[$Debug, Print["text: ", text//InputForm]];

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






testLeafNodeOrder[cst_] :=
Module[{leaves},
	leaves = Cases[cst, _LeafNode, Infinity];
	OrderedQ[leaves, OrderedQ[{#1[[3]][Source], #2[[3]][Source]}]&]
]





End[]

EndPackage[]
