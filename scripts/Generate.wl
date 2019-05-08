
(*

Calling this script like this:

wolfram -script scripts/Generate.wl -buildDir /path/to/build/dir

will generate additional required files in these directories:

/path/to/build/dir/generated/cpp/include
/path/to/build/dir/generated/cpp/src
/path/to/build/dir/generated/wl
/path/to/build/dir/paclet/AST

*)


Print["Generating additional required C++ and WL files..."]


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
generatedCPPSrcDir = FileNameJoin[{generatedCPPDir, "src", "lib"}]

generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]

pacletASTDir = FileNameJoin[{buildDir, "paclet", "AST"}]

tablesDir = FileNameJoin[{packageDir, "tables"}]

(* setup *)
Print["Setup"]

If[!($VersionNumber >= 12.0),
PrependTo[$Path, pacletASTDir];
]

res = PacletDirectoryAdd[pacletASTDir];
Print["PacletDirectoryAdd returned: ", res]

If[FailureQ[FindFile["AST`"]],
  Print["AST could not be found."];
  Quit[1]
]

If[FindFile["AST`"] =!= FileNameJoin[{pacletASTDir, "Kernel", "AST.wl"}],
  Print["Conflicting location for AST was found."];
  Print["Expected to find AST here: ", FileNameJoin[{pacletASTDir, "Kernel", "AST.wl"}]];
  Print["Actually found AST here: ", FindFile["AST`"]];
  If[FindFile["AST`"] === FileNameJoin[{packageDir, "AST", "AST.wl"}],
    Print["It looks like the AST source is being used. This is not supported during build time."];
    Print["There may be a problem with the version of Wolfram Engine that is being used."];
    ,
    Print["Consider running:\nPacletUninstall[\"AST\"]\nto ensure that no other installations of AST interfere with the build."];
  ];
  Quit[1]
]



Catch[
res = Needs[#];

If[FailureQ[res],
  Print["Needs[" <> # <> "] failed: ", res];
  Quit[1];
]
,
_
,
(Print[#];Quit[1])&
]& /@ {"AST`", "AST`Utils`", "AST`Build`"}




$WorkaroundBug321344 = checkBug321344[]
Print["Work around Bug 321344: ", $WorkaroundBug321344];



(* clean *)
Print["Clean..."]

Quiet[DeleteDirectory[generatedCPPDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedCPPDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPIncludeDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPSrcDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedWLDir], CreateDirectory::filex]

Print["Done Clean"]


(* LongNameDefines *)
Print["Generating LongNameDefines..."]

importedLongNames = Get[FileNameJoin[{tablesDir, "LongNames.wl"}]]

validateLongNameMap[importedLongNames]




importedPunctuationLongNames = Keys[Select[importedLongNames, # === Punctuation &]]

importedSpaceLongNames = Keys[Select[importedLongNames, # === Space &]]

importedNewlineLongNames = Keys[Select[importedLongNames, # === Newline &]]

importedCommaLongNames = Keys[Select[importedLongNames, # === Comma &]]

importedUninterpretableLongNames = Keys[Select[importedLongNames, # === Uninterpretable &]]




Check[
longNameDefines = ("constexpr int " <> toGlobal["CodePoint`LongName`" <> #] <> "(" <> "0x" <> longNameToHexDigits[#] <> ");")& /@ Keys[importedLongNames]
,
Print["Message while generating LongNameDefines"];
Quit[1]
]


longNameDefinesCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once
"} ~Join~ longNameDefines ~Join~ {""}

Print["exporting LongNameDefines.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNameDefines.h"}], Column[longNameDefinesCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

(*

LongNameDefines.wl is not used right now

Check[
sets = ("WLCharacter`LongName`"<># <> " = " <> "16^^" <> longNameToHexDigits[#])& /@ importedLongNames
,
Print["Message while generating LongNameDefines"];
Quit[1]
]

longNameDefinesWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)
"} ~Join~ sets ~Join~ {
"
Null
"}

Print["exporting LongNameDefines.wl"]
res = Export[FileNameJoin[{pacletASTDir, "LongNameDefines.wl"}], longNameDefinesWL, "Text"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]
*)

Print["Done LongNameDefines"]



(* LongNameMap *)
Print["Generating LongNameMap..."]

longNameToCodePointMap = {
"std::map <std::string, int> LongNameToCodePointMap {"} ~Join~ (Row[{"{", escapeString[#], ",", " ", toGlobal["CodePoint`LongName`"<>#], "}", ","}]& /@ Keys[importedLongNames]) ~Join~ {"};", ""}

codePointToLongNameMap = {
"std::map <int, std::string> CodePointToLongNameMap {"} ~Join~ (Row[{"{", toGlobal["CodePoint`LongName`"<>#], ",", " ", escapeString[#], "}", ","}] & /@ Keys[importedLongNames])~Join~{"};", ""}

longNameMapCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"LongNameMap.h\"

#include \"LongNameDefines.h\"

#include <map>
#include <string>
"} ~Join~ longNameToCodePointMap ~Join~ codePointToLongNameMap

Print["exporting LongNameMap.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "LongNameMap.cpp"}], Column[longNameMapCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


longNameMapCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include <map>
#include <string>

extern std::map<std::string, int> LongNameToCodePointMap;
extern std::map<int, std::string> CodePointToLongNameMap;
"}

Print["exporting LongNameMap.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNameMap.h"}], Column[longNameMapCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


longNameToCodePointAssociation = {
"LongNameToCodePointAssociation = <|"} ~Join~ (Row[{escapeString[#], " -> ", "WLCharacter`LongName`" <> #, ","}]& /@ Keys[importedLongNames]) ~Join~ {"Nothing"} ~Join~ {"|>"}

codePointToLongNameAssociation = {
"CodePointToLongNameAssociation = <|"} ~Join~ (Row[{"WLCharacter`LongName`" <> #, " -> ", escapeString[#], ","}]& /@ Keys[importedLongNames]) ~Join~ {"Nothing"} ~Join~ {"|>"}




longNameMapWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)
"} ~Join~ longNameToCodePointAssociation ~Join~ {""} ~Join~ codePointToLongNameAssociation ~Join~ {
"
Null
"}

Print["exporting LongNameMap.wl"]
res = Export[FileNameJoin[{generatedWLDir, "LongNameMap.wl"}], Column[longNameMapWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



longNameReplacementsWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

BeginPackage[\"AST`LongNameReplacements`\"]

$LongNameReplacements

Begin[\"`Private`\"]

$LongNameReplacements = {"
} ~Join~ (("\"\\\\["<>#<>"]\"" -> "\"\\:"<>longNameToHexDigits[#]<>"\",")& /@ Keys[importedLongNames]) ~Join~ {
"
Nothing
}

End[]

EndPackage[]
"}

Print["exporting LongNameReplacements.wl"]
res = Export[FileNameJoin[{pacletASTDir, "Kernel", "LongNameReplacements.wl"}], Column[longNameReplacementsWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done LongNameMap"]




(* CodePoint *)
Print["Generating CodePoint..."]

codePointCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"Token.h\"

//
// These are the actual WL code points for linear syntax characters
//
constexpr int CODEPOINT_LINEARSYNTAX_CLOSEPAREN(0xf7c0);
constexpr int CODEPOINT_LINEARSYNTAX_BANG(0xf7c1);
constexpr int CODEPOINT_LINEARSYNTAX_AT(0xf7c2);
//UNUSED: constexpr int CODEPOINT_LINEARSYNTAX_HASH(0xf7c3);
//UNUSED: constexpr int CODEPOINT_LINEARSYNTAX_DOLLAR(0xf7c4);
constexpr int CODEPOINT_LINEARSYNTAX_PERCENT(0xf7c5);
constexpr int CODEPOINT_LINEARSYNTAX_CARET(0xf7c6);
constexpr int CODEPOINT_LINEARSYNTAX_AMP(0xf7c7);
constexpr int CODEPOINT_LINEARSYNTAX_STAR(0xf7c8);
constexpr int CODEPOINT_LINEARSYNTAX_OPENPAREN(0xf7c9);
constexpr int CODEPOINT_LINEARSYNTAX_UNDER(0xf7ca);
constexpr int CODEPOINT_LINEARSYNTAX_PLUS(0xf7cb);
constexpr int CODEPOINT_LINEARSYNTAX_SLASH(0xf7cc);
constexpr int CODEPOINT_LINEARSYNTAX_BACKTICK(0xf7cd);

//
// Do the simple thing and have -1 be EOF
//
constexpr int CODEPOINT_ENDOFFILE(EOF);

constexpr int CODEPOINT_ERROR_INTERNAL(-2);

//
// There is an inconsistency in WL, such that LINEARSYNTAX_SPACE does not have a dedicated code point
// So invent one here.
//
constexpr int CODEPOINT_LINEARSYNTAX_SPACE(-3);

//
// The string meta characters \\< and \\> will have code points here, but they are not actual characters and do not have real code points
// 
constexpr int CODEPOINT_STRINGMETA_OPEN(-4);
constexpr int CODEPOINT_STRINGMETA_CLOSE(-5);

TokenEnum LongNameCodePointToOperator(int c);
int LongNameOperatorToCodePoint(TokenEnum t);
"}

Print["exporting CodePoint.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "CodePoint.h"}], Column[codePointCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



punctuationSource = 
  {"std::unordered_set<int> punctuationCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedPunctuationLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isPunctuationCharacter() const { return punctuationCodePoints.find(value_) != punctuationCodePoints.end(); }", ""}

spaceSource = 
  {"std::unordered_set<int> spaceCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedSpaceLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isSpaceCharacter() const { return spaceCodePoints.find(value_) != spaceCodePoints.end(); }", ""}

newlineSource = 
  {"std::unordered_set<int> newlineCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedNewlineLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isNewlineCharacter() const { return newlineCodePoints.find(value_) != newlineCodePoints.end();}", ""}

commaSource = 
  {"std::unordered_set<int> commaCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedCommaLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isCommaCharacter() const { return commaCodePoints.find(value_) != commaCodePoints.end(); }", ""}

uninterpretableSource = 
  {"std::unordered_set<int> uninterpretableCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedUninterpretableLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isUninterpretableCharacter() const { return uninterpretableCodePoints.find(value_) != uninterpretableCodePoints.end(); }", ""}

LongNameCodePointToOperatorSource = 
  {"TokenEnum LongNameCodePointToOperator(int c) {
switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", 
        toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
    {"default:
std::cerr << \"Need to add operator: 0x\" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec << \"\\n\";
assert(false && \"Need to add operator\");
return TOKEN_ERROR_UNKNOWN;
}
}"}

LongNameOperatorToCodePointSource = 
  {"
int LongNameOperatorToCodePoint(TokenEnum t) {
switch (t) {"} ~Join~
    (Row[{"case", " ", toGlobal["Token`LongName`"<>#], ":", " ", "return",
         " ", toGlobal["CodePoint`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
{"default:
std::cerr << \"Need to add operator: 0x\" << std::setfill('0') << std::setw(4) << std::hex << t << std::dec << \"\\n\";
assert(false && \"Need to add operator\");
return CODEPOINT_ERROR_INTERNAL;
}
}
"}

codePointCPPSource = Join[{
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"CodePoint.h\"

#include \"LongNameDefines.h\"
#include \"CharacterDecoder.h\"

#include <unordered_set>
#include <iostream>
#include <iomanip>
#include <cassert>
"}, punctuationSource, spaceSource, newlineSource, commaSource, uninterpretableSource,
    LongNameCodePointToOperatorSource, 
    LongNameOperatorToCodePointSource]

Print["exporting CodePoint.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "CodePoint.cpp"}], Column[codePointCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done CodePoint"]









(* Token *)
Print["Generating Token..."]

operatorMacros = 
  Association[
   ToExpression["Token`LongName`" <> #] -> Next& /@ importedPunctuationLongNames]

importedTokenEnumSource = 
  Get[FileNameJoin[{tablesDir, "TokenEnum.wl"}]]

If[FailureQ[importedTokenEnumSource],
  Print[importedTokenEnumSource];
  Quit[1]
]

joined = importedTokenEnumSource ~Join~ operatorMacros

oldTokens = ToString /@ Keys[joined]


cur = 0
enumMap = <||>
KeyValueMap[(
    Which[
     IntegerQ[#2], cur = #2,
     #2 === Next, cur = cur + 1,
     True, cur = enumMap[#2]];
    AssociateTo[enumMap, #1 -> cur]) &, joined]


tokenCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include <string>
#include <unordered_map>

enum TokenEnum {"} ~Join~
   KeyValueMap[(Row[{toGlobal[#], " = ", #2, ","}])&, enumMap] ~Join~
   {"};", ""} ~Join~
   {"std::string TokenToString(TokenEnum type);",
   "bool isError(TokenEnum type);",
"
namespace std {
    //
    // for std::unordered_map
    //
    template <> struct hash<TokenEnum> {
        size_t operator()(const TokenEnum &x) const {
            return x;
        }
    };
}
"
}

Print["exporting Token.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Token.h"}], Column[tokenCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



tokenWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)
"} ~Join~ {"<|"} ~Join~
 (KeyValueMap[(Row[{#1, " -> ", #2, ","}])&, enumMap]) ~Join~
 {"Nothing", "|>", ""}

Print["exporting Token.wl"]
res = Export[FileNameJoin[{pacletASTDir, "Kernel", "Token.wl"}], Column[tokenWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



res = Get[FileNameJoin[{generatedWLDir, "LongNameMap.wl"}]]

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

tokenStrings = Association[# -> escapeString[ToString[#]]& /@ Keys[uniqueEnums]]

operatorMacros = 
  Association[ToExpression["Token`LongName`" <> #] -> escapeString["Token`LongName`" <> #]& /@ importedPunctuationLongNames]



joined = tokenStrings ~Join~ operatorMacros

cases = KeyValueMap[Row[{"case ", toGlobal[#1], ": return ", #2, ";"}]&, joined]


tokenCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"Token.h\"

#include <iostream>
#include <cassert>
"} ~Join~
    {"std::string TokenToString(TokenEnum Tok) {"} ~Join~
    {"switch (Tok) {"} ~Join~
    cases ~Join~
    {"default:"} ~Join~
    {"std::cerr << \"Unhandled token type: \" << std::to_string(Tok) << \"\\n\"; assert(false && \"Unhandled token type\"); return \"\";"} ~Join~
    {"}"} ~Join~
    {"}"} ~Join~
    {""} ~Join~
    {"bool isError(TokenEnum Tok) { return TOKEN_ERROR_FIRST <= Tok && Tok < TOKEN_ERROR_END; }"} ~Join~
    {""}

Print["exporting Token.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Token.cpp"}], Column[tokenCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Token"]







(* Precedence *)
Print["Generating Precedence..."]

importedPrecedenceSource = Get[FileNameJoin[{tablesDir, "Precedence.wl"}]]

If[FailureQ[importedPrecedenceSource],
  Print[importedPrecedenceSource];
  Quit[1]
]

oldPrecedences = Join[Names["Precedence`*"], Names["Precedence`*`*"]]

(*
resolve the symbolic values in the Precedence table to integer values
*)
cur = -Infinity;
enumMap = <||>;
KeyValueMap[(
    Which[
     NumberQ[#2], cur = #2,
     #2 === Indeterminate, cur = Indeterminate,
     #2 === Next, cur = cur + 1,
     True, cur = enumMap[#2]];
    AssociateTo[enumMap, #1 -> cur]) &, importedPrecedenceSource]

(*
sanity check that all precedences are in order
*)
cur = -Infinity;
KeyValueMap[
 If[#2 =!= Indeterminate && cur =!= Indeterminate && !TrueQ[#2 >= cur],
  Print["Precedence is out of order: ", #1->#2];
  Quit[1]
  ,
  cur = #2]&, enumMap]


precedenceCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

enum Precedence {"} ~Join~
   KeyValueMap[(Row[{toGlobal[#1], " = ",
    Which[
      NumberQ[#2], Floor[#2],
      #2 === Indeterminate, -1
    ], ","}]) &, enumMap] ~Join~ {"};", ""}

Print["exporting Precedence.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Precedence.h"}], Column[precedenceCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


precedenceWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)
"} ~Join~ {"<|"} ~Join~ (KeyValueMap[(Row[{#1, " -> ", #2, ","}]) &, enumMap]) ~Join~ {"Nothing", "|>", ""}

Print["exporting Precedence.wl"]
res = Export[FileNameJoin[{pacletASTDir, "Kernel", "Precedence.wl"}], Column[precedenceWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Precedence"]








(* Symbol *)
Print["Generating Symbol..."]

(*
Obtain all symbols by inspecting DownValues of functions
*)

symbols = Union[Join[DownValues[PrefixOperatorToSymbol][[All, 2]],
    DownValues[PostfixOperatorToSymbol][[All, 2]],
    DownValues[BinaryOperatorToSymbol][[All, 2]],
    DownValues[InfixOperatorToSymbol][[All, 2]],
    DownValues[TernaryOperatorToSymbol][[All, 2]],
    DownValues[GroupOpenerToSymbol][[All, 2]],
    DownValues[PrefixBinaryOperatorToSymbol][[All, 2]],
    AST`Symbol`$Nodes,
    AST`Symbol`$Options,
    AST`Symbol`$Miscellaneous,
    AST`Symbol`$Groups,
    AST`Symbol`$Characters
    ]]

symbolCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"Token.h\"
#include \"API.h\"

#include \"mathlink.h\"

#include <string>
#include <utility>

class ASTLIB_EXPORTED Symbol {
public:
  constexpr Symbol(const char *Name) : Name(Name) {}
  const char *name() const;

  void put(MLINK mlp) const;

private:
  const char *Name;
};

void allocSymbols();

void freeSymbols();

const Symbol* PrefixOperatorToSymbol(TokenEnum);
const Symbol* PostfixOperatorToSymbol(TokenEnum);
const Symbol* BinaryOperatorToSymbol(TokenEnum);
const Symbol* InfixOperatorToSymbol(TokenEnum);
const Symbol* GroupOpenerToSymbol(TokenEnum);
const Symbol* PrefixBinaryOperatorToSymbol(TokenEnum);

TokenEnum GroupOpenerToCloser(TokenEnum);
TokenEnum GroupCloserToOpener(TokenEnum);

bool isCloser(TokenEnum);

"} ~Join~
(Row[{"ASTLIB_EXPORTED", " ", "extern", " ", "const", " ", "Symbol*", " ", toGlobal["Symbol`"<>ToString[#]], ";"}]& /@ symbols) ~Join~
{""}

Print["exporting Symbol.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "Symbol.h"}], Column[symbolCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

(*
We want to fully-qualify symbol names over the wire.
This allows library->kernel traffic to work when AST` is not on $ContextPath.
However, it is still not possible to fully-qualify System` symbols
bug 283291
bug 284492
So also make library->kernel traffic match this behavior
*)
stringifyForTransmitting[sym_Symbol] :=
Module[{ctxt},
  ctxt = Context[sym];
  If[ctxt == "System`",
    SymbolName[sym]
    ,
    Context[sym]<>SymbolName[sym]
  ]
]


symbolCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"Symbol.h\"

#include <cassert>
#include <iostream>

const char *Symbol::name() const {
   return Name;
}

void Symbol::put(MLINK mlp) const {
  MLPutSymbol(mlp, Name);
}

"} ~Join~ { "void allocSymbols() {" } ~Join~

    (If[# === String && $WorkaroundBug321344,
      (*
      handle String specially because of bug 321344
      *)
      "SYMBOL_STRING = new Symbol(\"String\");"
      ,
      Row[{toGlobal["Symbol`"<>ToString[#]], " = new Symbol(\"", stringifyForTransmitting[#], "\");"}]]& /@ symbols) ~Join~
{"}
"} ~Join~ { "void freeSymbols() {" } ~Join~

    (If[# === String && $WorkaroundBug321344,
      (*
      handle String specially because of bug 321344
      *)
      "delete SYMBOL_STRING;"
      ,
      Row[{"delete ", toGlobal["Symbol`"<>ToString[#]], ";"}]]& /@ symbols) ~Join~
{"}
"} ~Join~

  (If[# === String && $WorkaroundBug321344,
      (*
      handle String specially because of bug 321344
      *)
      "const Symbol* SYMBOL_STRING = nullptr;"
      ,
      Row[{"const Symbol* ", toGlobal["Symbol`"<>ToString[#]], " = nullptr;"}]]& /@ symbols) ~Join~

      {""} ~Join~

      {"const Symbol* PrefixOperatorToSymbol(TokenEnum Type) {\nswitch (Type) {"} ~Join~
     
     Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PrefixOperatorToSymbol]] ~Join~ 
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
      "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol* PostfixOperatorToSymbol(TokenEnum Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PostfixOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol* BinaryOperatorToSymbol(TokenEnum Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[BinaryOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol* InfixOperatorToSymbol(TokenEnum Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[InfixOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol* GroupOpenerToSymbol(TokenEnum Type) {"} ~Join~
     {"switch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[GroupOpenerToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"TokenEnum GroupOpenerToCloser(TokenEnum Type) {"} ~Join~
     {"switch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[GroupOpenerToCloser]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return TOKEN_UNKNOWN;",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"TokenEnum GroupCloserToOpener(TokenEnum Type) {"} ~Join~
     {"switch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[GroupCloserToOpener]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return TOKEN_UNKNOWN;",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"bool isCloser(TokenEnum Type) {"} ~Join~
     {"switch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " true;"}]&, DownValues[GroupCloserToOpener]] ~Join~
      {"default: return false;",
     "}"} ~Join~
     {"}"} ~Join~

     {""} ~Join~

     {"const Symbol* PrefixBinaryOperatorToSymbol(TokenEnum Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PrefixBinaryOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""}

Print["exporting Symbol.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Symbol.cpp"}], Column[symbolCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Symbol"]





Print["Done generating additional required C++ and WL files"]


newTokens = Join[Names["Token`*"], Names["Token`*`*"]]

If[Complement[newTokens, oldTokens] =!= {},
  Print["New Tokens: ", Complement[newTokens, oldTokens]];
  Quit[1]
]

longNames = Join[Names["LongName`*"], Names["LongName`*`*"]]

If[Length[longNames] =!= 0,
  Print["LongName: ", longNames];
  Quit[1]
]

newPrecedences = Join[Names["Precedence`*"], Names["Precedence`*`*"]]

If[Complement[newPrecedences, oldPrecedences] =!= {},
  Print["New Precedences: ", Complement[newPrecedences, oldPrecedences]];
  Quit[1]
]








