
(*

Calling this script like this:

wolfram -script scripts/Generate.wl -buildDir /path/to/build/dir

will generate additional required files in these directories:

/path/to/build/dir/generated/cpp
/path/to/build/dir/paclet/AST

*)


Print["Generating additional required C++ and WL files"]

If[!($VersionNumber >= 11.0),
  Print["Expected $VersionNumber of at least 11.0"];
  Print["Actual $VersionNumber is: ", $VersionNumber];
  Quit[1]
]



packageDir = Directory[]

If[FileNameSplit[packageDir][[-1]] =!= "ast",
  Print["Cannot proceed; Not inside ast directory: ", packageDir];
  Quit[1]
]



buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[FileType[buildDir] =!= Directory,
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]



generatedCPPDir = FileNameJoin[{buildDir, "generated", "cpp"}]
generatedCPPIncludeDir = FileNameJoin[{generatedCPPDir, "include"}]
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir, "src"}]


pacletASTDir = FileNameJoin[{buildDir, "paclet", "AST"}]

tablesDir = FileNameJoin[{packageDir, "AST", "tables"}]

(* setup *)
Print["Setup"]

If[$VersionNumber >= 12.0,
res = PacletDirectoryAdd[pacletASTDir];
Print["PacletDirectoryAdd returned: ", res]
,
(*else*)
PrependTo[$Path, pacletASTDir];
res = PacletDirectoryAdd[pacletASTDir];
Print["PacletDirectoryAdd returned: ", res]
]

If[FailureQ[FindFile["AST`"]],
  Print["AST could not be found."];
  Quit[1]
]

If[FindFile["AST`"] =!= FileNameJoin[{pacletASTDir, "AST.wl"}],
  Print["Conflicting location for AST was found."];
  Print["Expected to find AST here: ", FileNameJoin[{pacletASTDir, "AST.wl"}]];
  Print["Actually found AST here: ", FindFile["AST`"]];
  If[FindFile["AST`"] === FileNameJoin[{packageDir, "AST", "AST.wl"}],
    Print["It looks like the AST source is being used. This is not supported."];
    Print["There may be a problem with the version of Wolfram Engine that is being used."];
  ];
  Quit[1]
]


res = Needs["AST`"]

If[FailureQ[res],
  Print["Needs[\"AST`\"] failed: ", res];
  Quit[1];
]

res = Needs["AST`Utils`"]

If[FailureQ[res],
  Print["Needs[\"AST`Utils`\"] failed: ", res];
  Quit[1];
]


(*
assume being called like toCPPDefine["Alpha"]
*)
toCPPDefine[n_] := "LONGNAME_" <> ToUpperCase[n]

(*
assume being called like toWLSet["Alpha"]
*)
toWLSet[n_] := "LongName`" <> ToUpperCase[n]

toEnum[n_] := 
 StringReplace[ToUpperCase[ToString[n]], "`" -> "_"]



(* clean *)
Print["Clean"]

Quiet[DeleteDirectory[generatedCPPDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedCPPDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPIncludeDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPSrcDir], CreateDirectory::filex]



(* LongNameDefines *)
Print["generating LongNameDefines"]

importedLongNames = Get[FileNameJoin[{tablesDir, "LongNames.wl"}]]

If[FailureQ[importedLongNames],
  Print[importedLongNames];
  Quit[1]
]

Check[
defines = ("#define " <> toCPPDefine[#] <> " " <> "0x" <> 
      IntegerString[
       ToCharacterCode[ToExpression["\"\\[" <> # <> "]\""]], 16, 4])& /@ importedLongNames
,
Print["Message while generating LongNameDefines"];
Quit[1]
]

Print["exporting LongNameDefines.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNameDefines.h"}], defines, "Text"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Check[
sets = (toWLSet[#] <> " = " <> "16^^" <> 
      IntegerString[
       ToCharacterCode[ToExpression["\"\\[" <> # <> "]\""]], 16, 4])& /@ importedLongNames
,
Print["Message while generating LongNameDefines"];
Quit[1]
]

Print["exporting LongNameDefines.wl"]
res = Export[FileNameJoin[{pacletASTDir, "LongNameDefines.wl"}], sets, "Text"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]




(* LongNameMap *)
Print["generating LongNameMap"]

longNameToCodePointMap = {
"std::map <std::string, int> LongNameToCodePointMap {"} ~Join~ (Row[{"{", AST`Utils`escapeString[#], ",", toCPPDefine[#], "}", ","}]& /@ importedLongNames) ~Join~ {"};"}

codePointToLongNameMap = {
"std::map <int, std::string> CodePointToLongNameMap {"} ~Join~ (Row[{"{", toCPPDefine[#], ",", AST`Utils`escapeString[#], "}", ","}] & /@ importedLongNames)~Join~{"};"}

Print["exporting LongNameMap.cpp"]
res = Export[
 FileNameJoin[{generatedCPPSrcDir, "LongNameMap.cpp"}], 
 Column[{"#include \"LongNameMap.h\"", 
    "#include \"LongNameDefines.h\"", "#include <map>", 
    "#include <string>"}~Join~longNameToCodePointMap ~Join~ codePointToLongNameMap], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


Print["exporting LongNameMap.h"]
res = Export[
 FileNameJoin[{generatedCPPIncludeDir, "LongNameMap.h"}], 
 Column[{"#include <map>", "#include <string>", 
   "extern std::map<std::string, int>LongNameToCodePointMap;", 
   "extern std::map<int, std::string>CodePointToLongNameMap;"}], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


longNameToCodePointAssociation = {
"LongNameToCodePointAssociation = <|"} ~Join~ (Row[{AST`Utils`escapeString[#], "->", toWLSet[#], ","}]& /@ 
     importedLongNames) ~Join~ {"Nothing"} ~Join~ {"|>"}

codePointToLongNameAssociation = {
"CodePointToLongNameAssociation = <|"} ~Join~ (Row[{toWLSet[#], "->", AST`Utils`escapeString[#], ","}]& /@ 
     importedLongNames) ~Join~ {"Nothing"} ~Join~ {"|>"}

Print["exporting LongNameMap.wl"]
res = Export[
 FileNameJoin[{pacletASTDir, "LongNameMap.wl"}], 
 Column[longNameToCodePointAssociation ~Join~
   codePointToLongNameAssociation ~Join~ {"Null"}], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]







(* CodePoint *)
Print["generating CodePoint"]

importedLetterlikeLongNames = 
  Get[FileNameJoin[{tablesDir, "LetterlikeLongNames.wl"}]]

If[FailureQ[importedLetterlikeLongNames],
  Print[importedLetterlikeLongNames];
  Quit[1]
]

importedOperatorLongNames = 
  Get[FileNameJoin[{tablesDir, "OperatorLongNames.wl"}]]

If[FailureQ[importedOperatorLongNames],
  Print[importedOperatorLongNames];
  Quit[1]
]

importedCommaLongNames = 
  Get[FileNameJoin[{tablesDir, "CommaLongNames.wl"}]]

If[FailureQ[importedCommaLongNames],
  Print[importedCommaLongNames];
  Quit[1]
]

importedNewlineLongNames = 
  Get[FileNameJoin[{tablesDir, "NewlineLongNames.wl"}]]

If[FailureQ[importedNewlineLongNames],
  Print[importedNewlineLongNames];
  Quit[1]
]

importedSpaceLongNames = 
  Get[FileNameJoin[{tablesDir, "SpaceLongNames.wl"}]]

If[FailureQ[importedSpaceLongNames],
  Print[importedSpaceLongNames];
  Quit[1]
]

importedStrangeLetterlikeCodePoints = 
  Get[FileNameJoin[{tablesDir, "StrangeLetterlikeCodePoints.wl"}]]

If[FailureQ[importedStrangeLetterlikeCodePoints],
  Print[importedStrangeLetterlikeCodePoints];
  Quit[1]
]

codePointHeader = {"
   #include \"Token.h\"
   
   #include <string>
   
   bool isLetterlikeCodePoint(int i);
   bool isStrangeLetterlikeCodePoint(int i);
   bool isOperatorCodePoint(int i);
   bool isSpaceCodePoint(int i);
   bool isNewlineCodePoint(int i);
   bool isCommaCodePoint(int i);
   
   Token LongNameCodePointToOperator(int c);
   int LongNameOperatorToCodePoint(Token t);
   "}

Print["exporting CodePoint.h"]
res = Export[
 FileNameJoin[{generatedCPPIncludeDir, "CodePoint.h"}], 
 Column[codePointHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


letterlikeSource = 
  Flatten[{"std::unordered_set<int> letterlikeCodePoints {", 
    Row[{toCPPDefine[#], ","}] & /@ importedLetterlikeLongNames, 
    Row[{#, ","}] & /@ importedStrangeLetterlikeCodePoints, "};", 
    "bool isLetterlikeCodePoint(int i) { return letterlikeCodePoints.find(i) != letterlikeCodePoints.end();}"}]

strangeLetterlikeSource = 
  Flatten[{"std::unordered_set<int> strangeLetterlikeCodePoints {", 
    Row[{#, ","}] & /@ importedStrangeLetterlikeCodePoints, "};" , 
    "bool isStrangeLetterlikeCodePoint(int i) { return strangeLetterlikeCodePoints.find(i) != strangeLetterlikeCodePoints.end();}"}]

operatorSource = 
  Flatten[{"std::unordered_set<int> operatorCodePoints {", 
    Row[{toCPPDefine[#], ","}] & /@ importedOperatorLongNames, "};" , 
    "bool isOperatorCodePoint(int i) { return operatorCodePoints.find(i) != operatorCodePoints.end(); }"}]

spaceSource = 
  Flatten[{"std::unordered_set<int> spaceCodePoints {", 
    Row[{toCPPDefine[#], ","}] & /@ importedSpaceLongNames, "};", 
    "bool isSpaceCodePoint(int i) { return spaceCodePoints.find(i) != spaceCodePoints.end(); }"}]

newlineSource = 
  Flatten[{"std::unordered_set<int> newlineCodePoints {", 
    Row[{toCPPDefine[#], ","}] & /@ importedNewlineLongNames, "};", 
    "bool isNewlineCodePoint(int i) { return newlineCodePoints.find(i) != newlineCodePoints.end();}"}]

commaSource = 
  Flatten[{"std::unordered_set<int> commaCodePoints {", 
    Row[{toCPPDefine[#], ","}] & /@ importedCommaLongNames, "};", 
    "bool isCommaCodePoint(int i) { return commaCodePoints.find(i) != commaCodePoints.end(); }"}]

LongNameCodePointToOperatorSource = 
  Flatten[{"Token LongNameCodePointToOperator(int c) {
        switch (c) {",
    Row[{"case", " ", toCPPDefine[#], ":", " ", "return", " ", 
        "OPERATOR_", toCPPDefine[#], ";"}] & /@ 
     importedOperatorLongNames
    ,
    "        default:
                std::cerr << \"Need to add operator: 0x\" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec << \"\\n\";
                assert(false && \"Need to add operator\");
                return ERROR_INTERNAL;
        }
    }"}]

LongNameOperatorToCodePointSource = 
  Flatten[{"int LongNameOperatorToCodePoint(Token t) {
        switch (t) {",
    Row[{"case", " ", "OPERATOR_", toCPPDefine[#], ":", " ", "return",
         " ", toCPPDefine[#], ";"}] & /@ importedOperatorLongNames
    ,
    "        default:
                std::cerr << \"Need to add operator: 0x\" << std::setfill('0') << std::setw(4) << std::hex << t << std::dec << \"\\n\";
                assert(false && \"Need to add operator\");
                return ERROR_INTERNAL;
        }
    }"}]

codePointSource = Column[Join[{
     "#include \"CodePoint.h\"
     
     #include \"LongNameDefines.h\"
     
     #include <unordered_set>
     #include <iostream>
     #include <iomanip>
     #include <cassert>"}, letterlikeSource, strangeLetterlikeSource, 
    operatorSource, spaceSource, newlineSource, commaSource, 
    LongNameCodePointToOperatorSource, 
    LongNameOperatorToCodePointSource]]

Print["exporting CodePoint.cpp"]
res = Export[
 FileNameJoin[{generatedCPPSrcDir, 
   "CodePoint.cpp"}], codePointSource, "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]











(* Token *)
Print["generating Token"]

operatorMacros = 
  Association[
   ToExpression["Operator`" <> toWLSet[#]] -> Next & /@ importedOperatorLongNames]

importedTokenEnumSource = 
  Get[FileNameJoin[{tablesDir, "TokenEnum.wl"}]]

If[FailureQ[importedTokenEnumSource],
  Print[importedTokenEnumSource];
  Quit[1]
]

joined = importedTokenEnumSource ~Join~ operatorMacros

cur = 0
enumMap = <||>
KeyValueMap[(
    Which[
     IntegerQ[#2], cur = #2,
     #2 === Next, cur = cur + 1,
     True, cur = enumMap[#2]];
    AssociateTo[enumMap, #1 -> cur]) &, joined]

Print["exporting Token.h"]
res = Export[
 FileNameJoin[{generatedCPPIncludeDir, "Token.h"}], 
 Column[{"#pragma once", "#include <string>", "enum Token {"} ~Join~
   KeyValueMap[(Row[{toEnum[#], " = ", #2, ","}]) &, enumMap] ~Join~
   {"};"} ~Join~
   {"std::string TokenToString(Token type);",
   "bool isOperator(Token type);",
   "bool isError(Token type);"}]
  ,
  "String"
]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



Print["exporting Token.wl"]
res = Export[
 FileNameJoin[{pacletASTDir, "Token.wl"}], 
 Column[{"<|"} ~Join~
 (KeyValueMap[(Row[{#1, " -> ", #2, ","}]) &, enumMap]) ~Join~
 {"Nothing", "|>"}]
 ,
 "String"
]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



res = Get[FileNameJoin[{pacletASTDir, "LongNameMap.wl"}]]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



(*
remove values like Error`UNKNOWN in:
<|
Error`FIRST -> Next,
Error`UNKNOWN -> Error`FIRST
|>

because C switch statements cannot have duplicate cases

*)
uniqueEnums = DeleteCases[importedTokenEnumSource, v_ /; !IntegerQ[v] && UnsameQ[v, Next]]

tokenStrings = Association[# -> AST`Utils`escapeString[ToString[#]]& /@ Keys[uniqueEnums]]

operatorMacros = 
  Association[ToExpression["Operator`" <> toWLSet[#]] -> AST`Utils`escapeString["Operator`" <> toWLSet[#]] & /@ importedOperatorLongNames]

joined = tokenStrings ~Join~ operatorMacros

cases = KeyValueMap[Row[{"case ", toEnum[#1], ": return ", #2, ";"}]&, joined]

Print["exporting Token.cpp"]
res = Export[
 FileNameJoin[{generatedCPPSrcDir, "Token.cpp"}], 
 Column[{"#include \"Token.h\"", "#include <iostream>", "#include <cassert>"} ~Join~
    {"std::string TokenToString(Token Tok) {"} ~Join~
    {"switch (Tok) {"} ~Join~
    cases ~Join~
    {"default:"} ~Join~
    {"std::cerr << \"Unhandled token type: \" << std::to_string(Tok) << \"\\n\"; assert(false && \"Unhandled token type\"); return \"\";"} ~Join~
    {"}"} ~Join~
    {"}"} ~Join~
    {"bool isOperator(Token Tok) { return OPERATOR_FIRST <= Tok && Tok < OPERATOR_END; }"} ~Join~
    {"bool isError(Token Tok) { return ERROR_FIRST <= Tok && Tok < ERROR_END; }"} ~Join~
    {}
  ]
  ,
  "String"
]

If[FailureQ[res],
  Print[res];
  Quit[1]
]









(* Precedence *)
Print["generating Precedence"]

importedPrecedenceSource = 
  Get[FileNameJoin[{tablesDir, "Precedence.wl"}]]

If[FailureQ[importedPrecedenceSource],
  Print[importedPrecedenceSource];
  Quit[1]
]

cur = 0;
enumMap = <||>;
KeyValueMap[(
    Which[
     IntegerQ[#2], cur = #2,
     #2 === Next, cur = cur + 1,
     True, cur = enumMap[#2]];
    AssociateTo[enumMap, #1 -> cur]) &, importedPrecedenceSource]

Print["exporting Precedence.h"]
res = Export[
 FileNameJoin[{generatedCPPIncludeDir, "Precedence.h"}], 
 Column[{"#pragma once", "enum precedence_t {"} ~Join~
   KeyValueMap[(Row[{toEnum[#1], " = ", #2, ","}]) &, enumMap] ~Join~ {"};"} ~Join~ {}], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



Print["exporting Precedence.wl"]
res = Export[
 FileNameJoin[{pacletASTDir, 
   "Precedence.wl"}], 
 Column[{"<|"} ~Join~ (KeyValueMap[(Row[{#1, " -> ", #2, ","}]) &, enumMap]) ~Join~ {"Nothing", "|>"}], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]










(* Symbol *)
Print["generating Symbol"]

(*
Obtain all symbols by inspecting DownValues of functions
*)

symbols = Union[Join[DownValues[PrefixOperatorToSymbol][[All, 2]],
    DownValues[PostfixOperatorToSymbol][[All, 2]],
    DownValues[InfixOperatorToSymbol][[All, 2]],
    AST`Symbol`$Nodes,
    AST`Symbol`$Options,
    AST`Symbol`$Miscellaneous,
    AST`Symbol`$Tokens,
    AST`Symbol`$Groups,
    AST`Symbol`$Characters
    ]]

symbolHeader = Flatten[{"
    #pragma once
    
    #include \"Token.h\"\[IndentingNewLine]
    #include <string>
    #include <utility>
    
    class Symbol {
    std::string Name;
    public:Symbol(std::string Name):Name(Name) {}
    std::string name() const;
    };
    
    bool operator==(const Symbol& lhs, const Symbol& rhs);
    
    Symbol* PostfixOperatorToSymbol(Token Type);
    Symbol* PrefixOperatorToSymbol(Token Type);
    Symbol* InfixOperatorToSymbol(Token);
    Symbol* GroupOpenerToSymbol(Token);
    Token GroupOpenerToCloser(Token);
    Symbol* GroupCloserToSymbol(Token);
    Symbol* GroupOpenerToMissingCloserSymbol(Token);
    Symbol* GroupCloserToMissingOpenerSymbol(Token);
    
    std::string SymbolToPrefixOperatorString(Symbol*);
    std::string SymbolToPostfixOperatorString(Symbol*);
    std::string SymbolToInfixOperatorString(Symbol*);
    std::pair<std::string, std::string> SymbolToGroupPair(Symbol*);
    std::pair<Symbol*, Symbol*> SymbolToTernaryOperatorPair(Symbol*);
    
    ",
    Row[{"extern", " ", "Symbol*", " ", "SYMBOL_"<>ToUpperCase[ToString[#]], ";"}]& /@ symbols
    }]

Print["exporting Symbol.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Symbol.h"}], 
 Column[symbolHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



symbolSource = Flatten[{"
     
     #include \"Symbol.h\"
     
     #include <cassert>
     #include <iostream>
     
     bool operator==(const Symbol& lhs, const Symbol& rhs) {
         return lhs.name() == rhs.name();
     }
     
     std::string Symbol::name() const {
         return Name;
     }
     
     "} ~Join~ (Row[{"Symbol*", " ", "SYMBOL_"<>ToUpperCase[ToString[#]], " ",
          "=", " ", "new Symbol(\"", ToString[#, InputForm], "\")", ";"}]& /@symbols)

    ~Join~ {
     "Symbol* PrefixOperatorToSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[PrefixOperatorToSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "Symbol* PostfixOperatorToSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[PostfixOperatorToSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "Symbol* InfixOperatorToSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[InfixOperatorToSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "Symbol* GroupOpenerToSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[GroupOpenerToSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "Token GroupOpenerToCloser(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", toEnum[#[[2]]], ";"}]&, 
       DownValues[GroupOpenerToCloser]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return TOKEN_UNKNOWN;"],
     "} }"
     } ~Join~ {
     "Symbol* GroupCloserToSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[GroupCloserToSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "Symbol* GroupOpenerToMissingCloserSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[GroupOpenerToMissingCloserSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "Symbol* GroupCloserToMissingOpenerSymbol(Token Type) { switch (Type) {",
     Append[
      Map[Row[{"case", " ", toEnum[#[[1, 1, 1]]], ":", " ", "return", " ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2]]]], ";"}]&, 
       DownValues[GroupCloserToMissingOpenerSymbol]], 
      "default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return nullptr;"],
     "} }"
     } ~Join~ {
     "std::pair<std::string, std::string> SymbolToGroupPair(Symbol* Sym) {",
     Append[
      Row[{"if (*Sym == ", "*", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return std::make_pair(std::string(", 
          AST`Utils`escapeString[#[[2, 1]]], "), std::string(", 
          AST`Utils`escapeString[#[[2, 2]]], "));", "}"}]& /@ 
       DownValues[SymbolToGroupPair], 
      "return std::make_pair(std::string(\"XXX\"), std::string(\"XXX\"));"],
     "}"
     } ~Join~ {
     "std::string SymbolToInfixOperatorString(Symbol* Sym) {",
     Append[
      Row[{"if (*Sym == ", "*", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", AST`Utils`escapeString[#[[2]]], ";", "}"}]& /@ 
       DownValues[SymbolToInfixOperatorString], "return \"XXX\";"],
     "}"
     } ~Join~ {
     "std::string SymbolToPrefixOperatorString(Symbol* Sym) {",
     Append[
      Row[{"if (*Sym == ", "*", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", AST`Utils`escapeString[#[[2]]], ";", "}"}]& /@ 
       DownValues[SymbolToPrefixOperatorString], "return \"XXX\";"],
     "}"
     } ~Join~ {
     "std::string SymbolToPostfixOperatorString(Symbol* Sym) {",
     Append[
      Row[{"if (*Sym == ", "*", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", AST`Utils`escapeString[#[[2]]], ";", "}"}]& /@ 
       DownValues[SymbolToPostfixOperatorString], "return \"XXX\";"],
     "}"
     } ~Join~ {
     "std::pair<Symbol*, Symbol*> SymbolToTernaryOperatorPair(Symbol* Sym) {",
     Append[
      Row[{"if (*Sym == ", "*", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return std::make_pair(", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2, 1]]]], ", ", 
          "SYMBOL_"<>ToUpperCase[ToString[#[[2, 2]]]], ");", "}"}]& /@ 
       DownValues[SymbolToTernaryOperatorPair], 
      "return std::make_pair(nullptr, nullptr);"],
     "}"
     }
   ]

Print["exporting Symbol.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Symbol.cpp"}], Column[symbolSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]







(* ToInputFormString *)
Print["generating ToInputFormString"]

toInputFormStringHeader = Flatten[{"
    #pragma once
    
    #include \"Token.h\"
    
    #include <string>
    
    std::string ToInputFormString(std::shared_ptr<Node>);
    
    std::string ErrorTokenToInputFormString(Token);
    
    "}]

Print["exporting ToInputFormString.h"]
res = Export[
 FileNameJoin[{generatedCPPIncludeDir, 
   "ToInputFormString.h"}], Column[toInputFormStringHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]






Print["Done generating additional required C++ and WL files"]



