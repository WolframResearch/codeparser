BeginPackage["CodeParserTestUtils`"]


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

Needs["CodeParser`"]
Needs["CodeParser`ToString`"] (* for ToInputFormString *)
Needs["CodeParser`Utils`"]
Needs["PacletManager`"]



parseEquivalenceFunction[text_, expectedIgnored_] :=
Catch[
Module[{cst, agg, ast, good, expected, actual, str, str1, expectedStr, actualStr},
	
	expected = DeleteCases[ToExpression[text, InputForm, Hold], Null];
	
	(*
	Concrete
	*)
	cst = CodeConcreteParse[text, ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)];
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
	agg = CodeParser`Abstract`Aggregate[cst];
	
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
	ast = CodeParse[text, ContainerNode -> (ContainerNode[Hold, #[[1]], <||>]&)];
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
    ];
    Throw[True]
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


	
	
	(*
	FullForm
	*)
	
	If[!FreeQ[ast, LeafNode[Token`LinearSyntaxBlob, _, _]],
		Throw[True]
	];
	
	expectedStr = ToString[FullForm[expected]];

	Quiet[
	 actualStr = ToFullFormString[ast /. {
	      LeafNode[Real, r_, data_] :> LeafNode[Real, ToString[FullForm[ToExpression[r]]], data],
	      LeafNode[Rational, r_, data_] :> LeafNode[Rational, ToString[FullForm[ToExpression[r]]], data],
	      LeafNode[Integer, i_, data_] :> LeafNode[Integer, ToString[FullForm[ToExpression[i]]], data],
	      LeafNode[String, s_ /; StringStartsQ[s, "\""], data_] :> LeafNode[String, ToString[FullForm[ToExpression[s]]], data],
	      LeafNode[Symbol, s_, data_] :> LeafNode[Symbol, ToExpression[s, InputForm, Function[xx, ToString[Unevaluated[FullForm[xx]]], {HoldFirst}]], data]}];
	 ];

	good = SameQ[expectedStr, actualStr];
	If[!good,
		Throw[unhandled[<|"actualStr"->actualStr, "expectedStr"->expectedStr|>]]
	];



  True

]]


tokensToString[ts_] := StringJoin[ts[[All, 2]]]














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
    If[$Debug, Print["file1: ", File[file]]];
    
    (*If[StringEndsQ[file,".tr"],
    tmp=CreateFile[];
    DeleteFile[tmp];
    CopyFile[file,tmp];
    res=RunProcess[{"sed","-i","''","s/@\\|.*//",tmp}];
    If[res["ExitCode"]=!=0,
    f=Failure["SedFailed",res];
    Print[Row[{"index: ",i," ",CodeParser`Utils`drop[file,prefix]}]];
    Print[Row[{"index: ",i," ",f}]];
    Throw[f]
    ];
    res=RunProcess[{"sed","-i","''","s/@@resource.*//",tmp}];
    If[res["ExitCode"]=!=0,
    f=Failure["SedFailed",res];
    Print[Row[{"index: ",i," ",CodeParser`Utils`drop[file,prefix]}]];
    Print[Row[{"index: ",i," ",f}]];
    Throw[f]
    ];
    file=tmp;
    ];*)
    
    If[FileType[file] =!= File,
    	Throw[Failure["NotAFile", <|"File"->fileIn|>], "Handled"]
    ];
    
     If[FileByteCount[file] > limit[[2]],
      ast = 
      Failure["FileTooLarge", <|"File" -> File[fileIn], 
        "FileSize" -> FileSize[file]|>];
     Throw[ast, "Handled"]
     ];
     If[FileByteCount[file] < limit[[1]],
      ast = 
      Failure["FileTooSmall", <|"File" -> File[fileIn], 
        "FileSize" -> FileSize[file]|>];
     Throw[ast, "Handled"]
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
        Throw[Failure["EncodedFile", <|"File" -> File[fileIn]|>], "Handled"]
        ,
        (* wl script *)
        StringStartsQ[firstLine, "#!"],
        Throw[Failure["WLScript", <|"File" -> File[fileIn]|>], "Handled"]
      ];
    ];


    version = convertVersionString[PacletFind["CodeParser"][[1]]["Version"]];
    If[$Debug,
    	Print["version: ", version]
    ];
    Which[
      version ~versionGreaterEqual~ {1, 1},
            cst = 
       CodeConcreteParse[File[file], 
        ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}],
            If[!empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
            If[!empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
            If[!empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
            If[!empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing]|>]&)];
      ,
      version ~versionGreaterEqual~ {0, 16},
            cst = 
       CodeConcreteParse[File[file], 
        ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>]&)];
      ,
      version ~versionGreaterEqual~ {0, 15},
            cst = 
       ConcreteParseFile[File[file], 
        ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>] &];
      ,
      version ~versionGreaterEqual~ {0, 12},
      cst = 
       ConcreteParseFile[File[file], 
        HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>] &];
      ,
     version ~versionGreaterEqual~ {0, 11},
     cst = 
       ConcreteParseFile[file, 
        HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &];
     ,
     version ~versionGreaterEqual~ {0, 10},
     cst = 
       ConcreteParseFile[file, 
        HoldNode[Hold, Most[#], <|SyntaxIssues -> Last[#]|>] &];
     ,
     version ~versionGreaterEqual~ {0, 9},
     cst = ConcreteParseFile[file, HoldNode[Hold, #, <||>] &];
     ,
     True,
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
      f = Failure["ConcreteParseFileFailed", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Red]];
        Print[Style[Row[{"index: ", i, " ", f}], Red]];
      ];
      Throw[f, "Uncaught"]
      ];
     Switch[cst[[1]],
      "EncodedFile",
      Throw[cst, "Handled"]
      ,
      "FindFileFailed",
      If[$Interactive,
        Print[
         Row[{"index: ", i, " ", File[fileIn]}]];
        Print[Row[{"index: ", i, " ", cst[[1]], "; skipping"}]];
      ];
      Throw[cst, "Handled"]
      ,
      "ExitCode",
      f = cst;
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Red]];
        Print[Style[Row[{"index: ", i, " ", f}], Red]];
      ];
      Throw[f, "Uncaught"]
      ,
      _,
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", cst}], Darker[Orange]]];
      ];
      Throw[cst, "Uncaught"]
      ];
     ];
    
    If[MatchQ[cst, ContainerNode[File, {Null}, _]],
      If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], 
         Darker[Orange]]];
       Print[
        Style[Row[{"index: ", i, " ", "No expressions"}], 
         Darker[Orange]]];
      ];
     Throw[cst, "Handled"]
     ];
    
    If[!FreeQ[cst, _SyntaxErrorNode | _ErrorNode],
     errs = Cases[cst, _SyntaxErrorNode | _ErrorNode, {0, Infinity}];
     f = Failure[
       "SyntaxError", <|"File" -> File[fileIn], 
        "SyntaxErrors" -> errs|>];
      If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Red]];
       Print[Style[Row[{"index: ", i, " ", Shallow[errs]}], Red]];
      ];
     savedFailure = f;
     ];


     If[!FreeQ[cst, FormatIssue["CharacterEncoding", _, _, _]],
      If[$Interactive,
        Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], 
         Darker[Orange]]];
       Print[
        Style[Row[{"index: ", i, " ", "Bad UTF-8 encoding"}], 
         Darker[Orange]]];
      ];
     Throw[cst, "Handled"]
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
      If[$Interactive,
       Print[
        Row[{"index: ", i, " ", File[fileIn]}]];
       Print[
        Row[{"index: ", i, " ", 
          "Package symbol detected (bug 347012); rewriting Package\[Rule]PackageXXX"}]];
      ];
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
      If[$Interactive,
        Print[
         Row[{"index: ", i, " ", File[fileIn]}]];
        Print[Row[{"index: ", i, " ", f}]];
      ];
      Throw[f, "Handled"]
      ];
     file = tmp;
     (*
     If[$Debug, 
      Print["file2: ", file, " bytes: ", FileByteCount[file]]];
     If[$Debug, Print[importFile[file]]];
     *)
     
     version = convertVersionString[PacletFind["CodeParser"][[1]]["Version"]];
     Which[
     version ~versionGreaterEqual~ {1, 1},
            cst = 
       CodeConcreteParse[File[file], 
        ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}],
            If[!empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
            If[!empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
            If[!empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
            If[!empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing]|>]&)];
      ,
     version ~versionGreaterEqual~ {0, 16},
      cst = 
        CodeConcreteParse[File[file], 
         ContainerNode -> (ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>]&)];
      ,
     version ~versionGreaterEqual~ {0, 15},
      cst = 
        ConcreteParseFile[File[file], 
         ContainerNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>]&];
      ,
     version ~versionGreaterEqual~ {0, 12},
      cst = 
        ConcreteParseFile[File[file], 
         HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[2]], If[empty[#[[1]]], Nothing, Source -> {#[[1, 1, 3, Key[Source], 1]], #[[1, -1, 3, Key[Source], 2]]}]|>]&];
      ,
      version ~versionGreaterEqual~ {0, 11},
      cst = 
        ConcreteParseFile[file, 
         HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &];
      ,
      version ~versionGreaterEqual~ {0, 10},
      cst = 
        ConcreteParseFile[file, 
         HoldNode[Hold, Most[#], <|SyntaxIssues -> Last[#]|>] &];
      ,
      version ~versionGreaterEqual~ {0, 9},
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
    	Throw[savedFailure, "Uncaught"]	
    ];
    
    If[!TrueQ[testLeafNodeOrder[cst]],
		Throw[OutOfOrder]
	];
    
    tryString = ToSourceCharacterString[cst];
    
    If[$DebugString, Print["ToSourceCharacterString[cst]: ", tryString]];
    
    If[! StringQ[tryString],
     f = Failure[
       "ToSourceCharacterString", <|"File" -> File[fileIn], 
        "tryString" -> tryString|>];
      If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Red]];
      ];
     Throw[f, "Uncaught"]
     ];
    
    Quiet@Check[
      actual = DeleteCases[ToExpression[tryString, InputForm], Null];
      (*
      If[$Debug, Print["actual: ", actual]];
      *)
      ,
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[
         Style[Row[{"index: ", i, " ", 
            "Messages while processing actual input (possibly from previous files):"}], Darker[Orange]]];
        Print[
         Style[If[$MessageList =!= {}, $MessageList, 
           "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      ];
      msgs = Cases[$MessageList, HoldForm[_::shdw]];
      If[msgs != {},
        If[$Interactive,
         Print[
          Style[Row[{"index: ", i, " ", 
             "There were General::shdw messages; rerunning"}], 
           Darker[Orange]]];
        ];
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
        f = Failure["UsingTwoWayRuleBefore112", <|"File" -> File[fileIn]|>];
        If[$Interactive,
          Print[
           Style[Row[{"index: ", i, " ", File[fileIn]}], 
            Darker[Orange]]];
          Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
        ];
        Throw[f, "Handled"]
        ];
      ];
     
     $LastFailedFileIn = fileIn;
     $LastFailedFile = file;
     $LastFailedActual = actual;
     $LastFailedActualString = tryString;
     $LastFailedActualCST = cst;
     $LastFailedExpected = expected;
     $LastFailedExpectedText = text;
     f = Failure["ConcreteParsingFailure", <|"File" -> File[fileIn]|>];
     If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Bold,
          Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
      ];
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
    agg = CodeParser`Abstract`Aggregate[cst];

    If[$Debug, Print["agg: ", agg]];

    If[$DebugTopLevelExpressions,
      Print[{"index: ", i, " ", File[fileIn]}];
      Print["# top-level exprs: ", Length[agg[[2]] ]]
    ];

    (*
    verifyAggregate[agg];
    *)
    
    tryString = ToInputFormString[agg];
    
    If[$DebugString, Print["ToInputFormString[agg]: ", tryString]];
    
    If[! StringQ[tryString],
     f = Failure[
       "ToInputFormString", <|"File" -> File[fileIn], 
        "tryString" -> tryString|>];
      If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Red]];
      ];
     Throw[f, "Uncaught"]
     ];
    
    Quiet@Check[
      actual = DeleteCases[ToExpression[tryString, InputForm], Null];
      (*
      If[$Debug, Print["actual: ", actual]];
      *)
      ,
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[
         Style[Row[{"index: ", i, " ", 
            "Messages while processing actual input (possibly from previous files):"}], Darker[Orange]]];
        Print[
         Style[If[$MessageList =!= {}, $MessageList, 
           "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      ];
      msgs = Cases[$MessageList, HoldForm[_::shdw]];
      If[msgs != {},
        If[$Interactive,
         Print[
          Style[Row[{"index: ", i, " ", 
             "There were General::shdw messages; rerunning"}], 
           Darker[Orange]]];
        ];
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
     f = Failure["AggregateParsingFailure", <|"File" -> File[fileIn]|>];
     If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Bold,
          Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
      ];
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

    ast = CodeParser`Abstract`Abstract[agg];

    If[$Debug, Print["ast: ", ast]];

    If[FailureQ[ast],

      If[ast[[1]] == "TooManyTopLevelExpressions",
        If[$Interactive,
          Print[Style[Row[{"index: ", i, " ", File[fileIn]}], Darker[Orange]]];
          Print[Style[Row[{"index: ", i, " ", ast}], Darker[Orange]]];
        ];
        Throw[ast, "Handled"]
      ];

      If[$Interactive,
        Print[
        Style[Row[{"index: ", i, " ", 
           StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Red]];
        Print[Style[Row[{"index: ", i, " ", ast}], Red]];
      ];
      Throw[ast, "Uncaught"]
    ];
    
    If[! FreeQ[ast, _SyntaxErrorNode | _ErrorNode | _AbstractSyntaxErrorNode],
     errs = 
      Cases[ast, _SyntaxErrorNode | _ErrorNode | _AbstractSyntaxErrorNode, {0, 
        Infinity}];
     f = Failure[
       "SyntaxError2", <|"File" -> File[fileIn], "SyntaxErrors" -> errs|>];
      If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Red]];
       Print[Style[Row[{"index: ", i, " ", Shallow[errs]}], Red]];
      ];
     Throw[f, "Handled"]
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
       "ToFullFormString", <|"File" -> File[fileIn], 
        "tryString" -> tryString|>];
      If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Red]];
      ];
     Throw[f, "Uncaught"]
     ];
    
    
    Quiet@Check[
      actual = DeleteCases[ToExpression[tryString, InputForm], Null];
      ,
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[
         Style[Row[{"index: ", i, " ", 
            "Messages while processing actual input (possibly from previous files):"}], Darker[Orange]]];
        Print[
         Style[If[$MessageList =!= {}, $MessageList, 
           "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      ];
      msgs = Cases[$MessageList, HoldForm[_::shdw]];
      If[msgs != {},
        If[$Interactive,
         Print[
          Style[Row[{"index: ", i, " ", 
             "There were General::shdw messages; rerunning"}], 
           Darker[Orange]]];
        ];
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
      Print["textReplaced1: ", textReplaced //InputForm];
    ];

    (*
    if there are no Package errors, 
    then proceed with replacing the text
    *)
    If[!MemberQ[errors, SyntaxIssue["Package", _, _, _]],
     textReplaced = 
      StringReplace[textReplaced, 
       RegularExpression[
         "(?m)(^\\s*(BeginPackage|Begin|End|EndPackage|System`Private`NewContextPath|System`Private`RestoreContextPath)\\s*\\[\\s*[\"a-zA-Z0-9`,\\{\\} \\n\\t\\r]*\\])\\s*;"] -> "$1"];
     (* enough people have End[(**)] that it is worth also replacing *)

          textReplaced = 
      StringReplace[textReplaced, 
       RegularExpression[
         "(?m)(^(End|EndPackage)\\[\\(\\*.*\\*\\)\\])\\s*;"] -> 
        "$1"];
     ];
    

    (*
    Work around bug 382857 where line continuations with \r\n are not treated correctly
    Related bugs: 382857

    Also make it easier to deal with embedded newlines
    *)
    textReplaced = StringReplace[textReplaced, "\r\n" -> "\n"];


    If[$Debug,
      Print["textReplaced2: ", textReplaced //InputForm];
    ];

    expected = DeleteCases[ToExpression[textReplaced, InputForm, Hold], Null];
    
    If[actual =!= expected,
     
     (*
     the files here have weird uses of Begin[] etc and it is hard to use regex to remove the trailing ;
     *)
     If[MemberQ[{
        prefix <> "SystemFiles/Components/RobotTools/Kernel/Menu.m",
        prefix <> "robottools/RobotTools/Kernel/Menu.m",
        
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

        prefix <> "TestTools/Legacy/Statistics/NIST/NISTTestTools.m",
        (*
        System`Private`NewContextPath[{"System`"(*, "XXXXXXX"*)}];
        *)
        prefix <> "Kernel/StartUp/Audio/Internals/Internals.m",
        Nothing
        }, fileIn],
      f = Failure["CannotRegexTooWeird", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
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
        prefix <> "TestTools/FrontEnd/GrammarTestGenerator.m",
        prefix <> "SystemFiles/Components/GoogleTextToSpeech/Kernel/GoogleSpeech.m",
        prefix <> "SystemFiles/Components/IBMTextToSpeech/Kernel/IBMWatsonSTT.m",
        prefix <> "SystemFiles/Components/IBMTextToSpeech/Kernel/IBMWatsonTTS.m",
        prefix <> "SystemFiles/Components/MicrosoftTextToSpeech/Kernel/MicrosoftSpeech.m",

        prefix <> "SystemFiles/Links/TravelDirectionsClient/Kernel/TravelDirectionsClientRequests.m",
        prefix <> "traveldirectionsclient/TravelDirectionsClient/Kernel/TravelDirectionsClientRequests.m",

        (*
        System`Private`NewContextPath[LocalObjects`Nodump`defaultContextPath];
        *)
        prefix <> "kernel/StartUp/LocalObjects/Common.m",
        prefix <> "kernel/StartUp/LocalObjects/LocalCache.m",
        prefix <> "kernel/StartUp/LocalObjects/LocalObject.m",
        prefix <> "kernel/StartUp/LocalObjects/LocalSymbol.m",
        prefix <> "kernel/StartUp/Persistence/BuildUtilities.m",
        prefix <> "kernel/StartUp/Persistence/Common.m",
        prefix <> "kernel/StartUp/Persistence/InitializationGlobals.m",
        prefix <> "kernel/StartUp/Persistence/InitializationValue.m",
        prefix <> "kernel/StartUp/Persistence/KernelInit.m",
        prefix <> "kernel/StartUp/Persistence/Once.m",
        prefix <> "kernel/StartUp/Persistence/PersistenceGlobals.m",
        prefix <> "kernel/StartUp/Persistence/PersistenceLocations.m",
        prefix <> "kernel/StartUp/Persistence/PersistentObject.m",
        prefix <> "kernel/StartUp/Persistence/PersistentValue.m",
        prefix <> "kernel/StartUp/Persistence/StandardLocations.m",

        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Common.m",

        (*
        Unbalanced directives
        *)
        prefix <> "cudafinancialderivative/CUDAFinancialDerivative/CUDAFinancialDerivative.m",
        prefix <> "serviceconnections/ATTSpeech/Kernel/ATTSpeech.m",
        prefix <> "serviceconnections/Automile/Kernel/Automile.m",
        prefix <> "serviceconnections/BingSearch/build/ServiceConnection_BingSearch/Kernel/BingSearch.m",
        prefix <> "serviceconnections/CensusBureau/Kernel/CensusBureau.m",
        prefix <> "serviceconnections/Dropbox/Kernel/Dropbox.m",
        prefix <> "serviceconnections/Facebook/Kernel/Facebook.m",
        prefix <> "serviceconnections/Fitbit/Kernel/Fitbit.m",
        prefix <> "serviceconnections/GitHub/Kernel/GitHub.m",
        prefix <> "serviceconnections/GoogleBigQuery/Kernel/GoogleBigQuery.m",
        prefix <> "serviceconnections/GoogleFit/Kernel/GoogleFit.m",
        prefix <> "serviceconnections/GooglePlus/Kernel/GooglePlus.m",
        prefix <> "serviceconnections/Gracenote/Kernel/Gracenote.m",
        prefix <> "serviceconnections/Instagram/Kernel/Instagram.m",
        prefix <> "serviceconnections/LinkedIn/Kernel/LinkedIn.m",
        prefix <> "serviceconnections/Nuance/Kernel/Nuance.m",
        prefix <> "serviceconnections/NYTimes/Kernel/NYTimes.m",
        prefix <> "serviceconnections/Quandl/Kernel/Quandl.m",
        prefix <> "serviceconnections/RunKeeper/Kernel/RunKeeper.m",
        prefix <> "serviceconnections/SugarCRM/Kernel/SugarCRM.m",
        prefix <> "serviceconnections/Tumblr/Kernel/Tumblr.m",
        prefix <> "serviceconnections/Twitter/Kernel/Twitter.m",
        prefix <> "serviceconnections/USAToday/Kernel/USAToday.m",
        prefix <> "serviceconnections/Yandex/Kernel/Yandex.m",

        prefix <> "FFmpegTools/FFmpegTools/Kernel/FFmpegTools.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/FFmpegTools.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Audio/Audio.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/ExportUtility.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/FilterGraph.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/ImportUtility.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/VideoPlayback.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/VideoProperties.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/VideoReader.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/VideoUtility.m",
        prefix <> "SystemFiles/Links/FFmpegTools/Kernel/Video/VideoWriter.m",

        prefix <> "SystemFiles/Links/FFmpegTools/LibraryResources/LibraryLinkUtilities.wl",
        prefix <> "SystemFiles/Links/ImageFileTools/LibraryResources/LibraryLinkUtilities.wl",
        prefix <> "SystemFiles/Links/ITKLink/LibraryResources/LibraryLinkUtilities.wl",
        prefix <> "SystemFiles/Links/PDFTools/LibraryResources/LibraryLinkUtilities.wl",
        prefix <> "SystemFiles/Links/RAWTools/LibraryResources/LibraryLinkUtilities.wl",
        prefix <> "SystemFiles/Links/XMPTools/LibraryResources/LibraryLinkUtilities.wl",

        Nothing
        }, fileIn],
      f = Failure["CannotRegexTooBroken", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
      ];
     
      If[MemberQ[{
        (*
        programmatic use of BeginPackage[] / EndPackage[] or something
        *)
        prefix <> "kernel/StartUp/sysinit.m",
        prefix <> "SystemFiles/Kernel/SystemResources/MacOSX-x86-64/sysinit.m",

        prefix <> "kernel/StartUp/sysmake.m",
        
        prefix <> "kernel/StartUp/Convert/Load.m",
        prefix <> "kernel/StartUp/Convert/MathMLConvert.m",

        prefix <> "kernel/StartUp/Devices/DeviceAPI/Device.m",
        prefix <> "kernel/StartUp/Messages.m",
        prefix <> "kernel/StartUp/NotebookCompatibility.m",

        prefix <> "AddOns/Applications/UnitTable/Kernel/UnitTable.m",
        prefix <> "unittable/UnitTable/Kernel/UnitTable.m",

        prefix <> "SystemFiles/Components/Chemistry/Kernel/init.wl",
        prefix <> "SystemFiles/Components/Chemistry/Kernel/Chemistry.wl",
        prefix <> "chemistry/Chemistry/Kernel/Chemistry.wl",

        prefix <> "SystemFiles/Components/MUnit/Kernel/TestRun.m",
        prefix <> "munit/MUnit/Kernel/TestRun.m",

        prefix <> "SystemFiles/Components/RobotTools/Kernel/FrontEnd.m",
        prefix <> "robottools/RobotTools/Kernel/FrontEnd.m",
        
        prefix <> "SystemFiles/Links/JLink/Kernel/CallJava.m",
        prefix <> "jlink/src/Mathematica/Kernel/CallJava.m",

        prefix <> "SystemFiles/Links/NETLink/Kernel/CallNET.m",
        prefix <> "netlink/Source/Mathematica/CallNET.m",

        prefix <> "alphasource/CalculateParse/BuildLexicon.m",
        prefix <> "alphasource/CalculateParse/Content/ExamplePage.m",
        prefix <> "alphasource/CalculateParse/TemplateParser4.m",
        prefix <> "alphasource/CalculateUtilities/CustomBuildUtilities.m",
        prefix <> "alphasource/CalculateUtilities/DataCloud/Test/DataCloudSpecialcaseTests.mt",
        prefix <> "alphasource/DataScience/Procedures/DataScienceUtilities.m",
        prefix <> "datapaclettools/Compiler/Code.m",
        prefix <> "datapaclettools/Compiler/DatabaseCompiler.m",
        prefix <> "datapaclettools/Compiler.m",
        prefix <> "datapaclettools/Repository/Editors.m",
        prefix <> "datapaclettools/Repository/Interface.m",

        prefix <> "pacletmanager/PacletManager/Notebooks/PublicPacletInstall.wl",

        prefix <> "predictiveinterface/PredictiveInterface/Kernel/PredictiveRuleCompiler.m",

        prefix <> "quantityunits/QuantityUnits/Kernel/QuantityUnits.m",

        prefix <> "entityindex/Tools/GenerateEntityIndex/InputGenerator.m",
        prefix <> "entityindex/Tools/MakeSummaryBoxes/CommonFunctions.m",
        prefix <> "entityindex/Tools/MakeSummaryBoxes/MakeSummaryBoxes.m",
        prefix <> "entityindex/Tools/SynthesizeEssays/CommonFunctions.m",
        prefix <> "entityindex/Tools/SynthesizeEssays/MakeSummaryBoxes.m",
        prefix <> "entityindex/Tools/SynthesizeEssays/SynthesizeEssays.m",
        prefix <> "recognizer/RecognizerUtilities.m",

        Nothing
        }, fileIn],
      f = Failure["CannotRegexTooProgrammatic", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
      ];

      If[MemberQ[{
        (*
        too deep
        *)
        prefix <> "Documentation/English/System/ExamplePages/SymbolicGeometricTransformations.nb",
        Nothing
        }, fileIn],
      f = Failure["TooDeep", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
      ];

     (*
     the files here have uses of < - > 
     and it is an older version
     *)
     If[MemberQ[{
        prefix <> "SystemFiles/Components/NeuralNetworks/Inference.m",
        prefix <> "kernel/StartUp/Language/EquationalProof.m",
        prefix <> "kernel/StartUp/Language/TreeObjects.m",
        prefix <> "kernel/StartUp/PlaneGeometry/GeometryConjecture.m",
        prefix <> "kernel/StartUp/Regions/RegionFunctions/Perimeter.m",
        prefix <> "kernel/StartUp/Regions/RegionRelations/RegionRelationsLibrary.m",
        prefix <> "kernel/StartUp/SpatialAnalysis/RegionUtilities.m",
        prefix <> "NaturalLanguageProcessing/NaturalLanguageProcessing/TextCases/PartOfSpeech/Sentences.m",
        prefix <> "NeuralNetworks/NeuralNetworks/Define/Shapes.m",
        prefix <> "NeuralNetworks/NeuralNetworks/Layers/Structural/Transpose.m",
        prefix <> "NeuralNetworks/NeuralNetworks/Types/Inference.m",
        prefix <> "NeuralNetworks/Tests/Formats/Upgrade.m",
        prefix <> "NeuralNetworks/Tests/Training/CopyNet.m",
        Nothing
        }, fileIn],
      f = Failure["OldTwoWayRule", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
      ];
     
     (*
     
     file has something like  a; ?b   and the kernel and CodeParser disagree
     (CodeParser is correct)
     *)
     
     If[!FreeQ[expected, 
        HoldPattern[Information][_, LongForm -> False]],
      f = Failure["Information?Syntax", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
      ];

     If[MemberQ[{
        (*
        embedded \r\n in a string in linear syntax (so the \r\n is preserved, but we replace \r\n -> \n up above)
        *)
        prefix <> "SystemFiles/Links/MailLink/Kernel/icons.m",
        Nothing
        }, fileIn],
      f = Failure["EmbeddedCRLFInLinearSyntax", <|"File" -> File[fileIn]|>];
      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[fileIn]}], 
          Darker[Orange]]];
        Print[Style[Row[{"index: ", i, " ", f}], Darker[Orange]]];
      ];
      Throw[f, "Handled"]
      ];
     
     $LastFailedFileIn = fileIn;
     $LastFailedFile = file;
     $LastFailedActual = actual;
     $LastFailedActualString = tryString;
     $LastFailedActualAST = ast;
     $LastFailedExpected = expected;
     $LastFailedExpectedText = text;
     $LastFailedExpectedTextReplaced = textReplaced;
     f = Failure["AbstractParsingFailure", <|"File" -> File[fileIn]|>];
     If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[fileIn]}], Bold,
          Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Bold, Red]];
      ];
     Throw[f, "Uncaught"]
     ];
    ,
    Null
    ]
   ,
   "Handled"
   ,
   Function[{value, tag}, If[StringQ[tmp] && FileExistsQ[tmp], DeleteFile[tmp]]; value]
   ]
  ]



(*
convert "0.9" to {0, 9}
*)
convertVersionString[s_String] := 
  FromDigits /@ StringSplit[s, "."]


versionGreaterEqual[{___}, {}] := True

versionGreaterEqual[{}, {__}] := False

versionGreaterEqual[{aFirst_, aRest___}, {bFirst_, bRest___}] :=
  Which[
    aFirst > bFirst,
      True
    ,
    aFirst < bFirst,
      False
    ,
    True,
      versionGreaterEqual[{aRest}, {bRest}]
  ]




(*

Cannot use Import[file, "Text"] because it drops \r from \r\n

Cannot use Import[file, "String"] because it assumes "Unicode" character encoding

Was:
Import[file, "Byte"]

but this is slow
*)
importFile[file_String] := FromCharacterCode[Normal[ReadByteArray[file]] /. EndOfFile -> {}, "UTF8"]





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

      msgList = $MessageList;

      If[$Interactive,
        Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Darker[Orange]]];
        Print[
         Style[Row[{"index: ", i, " ", 
            "Messages while processing expected input (possibly from previous files):"}], Darker[Orange]]];
        Print[
         Style[If[msgList =!= {}, msgList, 
           "{} (Most likely Syntax Messages, but Syntax Messages don't show up in $MessageList: bug 210020)"], Darker[Orange]]];
      ];
      msgs = Cases[msgList, HoldForm[_::shdw]];
      If[msgs != {},
        If[$Interactive,
         Print[
          Style[Row[{"index: ", i, " ", 
             "There were General::shdw messages; rerunning"}], 
           Darker[Orange]]];
        ];
       expected = 
        DeleteCases[ToExpression[text, InputForm, Hold], Null];
       ];

      (*
      Treat encoding errors as syntax errors
      *)
      msgs = Cases[msgList, HoldForm[$CharacterEncoding::utf8]];
      If[msgs != {},
        expected = System`$Failed
      ];

      ];
    
    If[expected === System`$Failed,
     f = Failure["SyntaxError", <|"File" -> File[file]|>];
     If[$Interactive,
       Print[
        Style[Row[{"index: ", i, " ", File[file]}], Red]];
       Print[Style[Row[{"index: ", i, " ", f}], Red]];
      ];
     Throw[f, "Handled"]
     ];

     {text, expected}
]






testLeafNodeOrder[cst_] :=
Catch[
Module[{leaves, a, b},
	leaves = Cases[cst, _LeafNode, Infinity];
	If[OrderedQ[leaves, OrderedQ[{#1[[3]][Source], #2[[3]][Source]}]&],
    Throw[True]
  ];

  Do[
    a = leaves[[i]];
    b = leaves[[i+1]];
    If[!OrderedQ[{a, b}, OrderedQ[{#1[[3]][Source], #2[[3]][Source]}]&],
      Throw[{a, b}, OutOfOrder]
    ];
    ,
    {i, 1, Length[leaves]-1}
  ]

]]





End[]

EndPackage[]
