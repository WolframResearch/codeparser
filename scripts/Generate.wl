
(*

Calling this script like this:

wolfram -script scripts/Generate.wl -buildDir /path/to/build/dir

will generate additional required files in these directories:

/path/to/build/dir/generated/cpp/include
/path/to/build/dir/generated/cpp/src
/path/to/build/dir/generated/wl
/path/to/build/dir/paclet/AST

*)


Print["Generating additional required C++ and WL files"]



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

tablesDir = FileNameJoin[{packageDir, "AST", "tables"}]

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

If[FindFile["AST`"] =!= FileNameJoin[{pacletASTDir, "AST.wl"}],
  Print["Conflicting location for AST was found."];
  Print["Expected to find AST here: ", FileNameJoin[{pacletASTDir, "AST.wl"}]];
  Print["Actually found AST here: ", FindFile["AST`"]];
  If[FindFile["AST`"] === FileNameJoin[{packageDir, "AST", "AST.wl"}],
    Print["It looks like the AST source is being used. This is not supported during build time."];
    Print["There may be a problem with the version of Wolfram Engine that is being used."];
    ,
    Print["Consider running:\nPacletUninstall[\"AST\"]\nto ensure that no other installations of AST interfere with the build."];
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
uppercases and replaces ` with _
*)
toGlobal[n_] := 
 StringReplace[ToUpperCase[ToString[n]], {"`" -> "_", "$" -> "_"}]


(*
longNameToHexDigits["Alpha"] is "03b1"
*)
longNameToHexDigits[longName_String] :=
  IntegerString[ToCharacterCode[ToExpression["\"\\[" <> longName <> "]\""]], 16, 4]

integerToHexDigits[int_Integer] :=
  IntegerString[int, 16, 4]




(* clean *)
Print["Clean"]

Quiet[DeleteDirectory[generatedCPPDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedCPPDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPIncludeDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedCPPSrcDir], CreateDirectory::filex]

Quiet[CreateDirectory[generatedWLDir], CreateDirectory::filex]


(* LongNameDefines *)
Print["generating LongNameDefines"]

importedLongNames = Get[FileNameJoin[{tablesDir, "LongNames.wl"}]]

If[FailureQ[importedLongNames],
  Print[importedLongNames];
  Quit[1]
]

If[!DuplicateFreeQ[importedLongNames],
  Print["LongNames.wl has duplicates"];
  Quit[1]
]

Check[
longNameDefines = ("constexpr int " <> toGlobal["CodePoint`LongName`" <> #] <> "(" <> "0x" <> longNameToHexDigits[#] <> ");")& /@ importedLongNames
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



(* LongNameMap *)
Print["generating LongNameMap"]

longNameToCodePointMap = {
"std::map <std::string, int> LongNameToCodePointMap {"} ~Join~ (Row[{"{", escapeString[#], ",", " ", toGlobal["CodePoint`LongName`"<>#], "}", ","}]& /@ importedLongNames) ~Join~ {"};", ""}

codePointToLongNameMap = {
"std::map <int, std::string> CodePointToLongNameMap {"} ~Join~ (Row[{"{", toGlobal["CodePoint`LongName`"<>#], ",", " ", escapeString[#], "}", ","}] & /@ importedLongNames)~Join~{"};", ""}

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
"LongNameToCodePointAssociation = <|"} ~Join~ (Row[{escapeString[#], " -> ", "WLCharacter`LongName`" <> #, ","}]& /@ 
     importedLongNames) ~Join~ {"Nothing"} ~Join~ {"|>"}

codePointToLongNameAssociation = {
"CodePointToLongNameAssociation = <|"} ~Join~ (Row[{"WLCharacter`LongName`" <> #, " -> ", escapeString[#], ","}]& /@ 
     importedLongNames) ~Join~ {"Nothing"} ~Join~ {"|>"}




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
} ~Join~ (("\"\\\\["<>#<>"]\"" -> "\"\\["<>#<>"]\",")& /@ importedLongNames) ~Join~ {
"
Nothing
}

End[]

EndPackage[]
"}

Print["exporting LongNameReplacements.wl"]
res = Export[FileNameJoin[{pacletASTDir, "LongNameReplacements.wl"}], Column[longNameReplacementsWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]





(* CodePoint *)
Print["generating CodePoint"]

importedOperatorLongNames = Get[FileNameJoin[{tablesDir, "OperatorLongNames.wl"}]]

If[FailureQ[importedOperatorLongNames],
  Print[importedOperatorLongNames];
  Quit[1]
]

If[!DuplicateFreeQ[importedOperatorLongNames],
  Print["OperatorLongNames.wl has duplicates"];
  Quit[1]
]

importedCommaLongNames = Get[FileNameJoin[{tablesDir, "CommaLongNames.wl"}]]

If[FailureQ[importedCommaLongNames],
  Print[importedCommaLongNames];
  Quit[1]
]

If[!DuplicateFreeQ[importedCommaLongNames],
  Print["CommaLongNames.wl has duplicates"];
  Quit[1]
]

importedNewlineLongNames = Get[FileNameJoin[{tablesDir, "NewlineLongNames.wl"}]]

If[FailureQ[importedNewlineLongNames],
  Print[importedNewlineLongNames];
  Quit[1]
]

If[!DuplicateFreeQ[importedNewlineLongNames],
  Print["NewlineLongNames.wl has duplicates"];
  Quit[1]
]

importedSpaceLongNames = Get[FileNameJoin[{tablesDir, "SpaceLongNames.wl"}]]

If[FailureQ[importedSpaceLongNames],
  Print[importedSpaceLongNames];
  Quit[1]
]

If[!DuplicateFreeQ[importedSpaceLongNames],
  Print["SpaceLongNames.wl has duplicates"];
  Quit[1]
]

importedUninterpretableLongNames = Get[FileNameJoin[{tablesDir, "UninterpretableLongNames.wl"}]]

If[FailureQ[importedUninterpretableLongNames],
  Print[importedUninterpretableLongNames];
  Quit[1]
]

If[!DuplicateFreeQ[importedUninterpretableLongNames],
  Print["UninterpretableLongNames.wl has duplicates"];
  Quit[1]
]



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
constexpr int CODEPOINT_EOF(EOF);

constexpr int CODEPOINT_ERROR_INTERNAL(-2);

//
// There is an inconsistency in WL, such that LINEARSYNTAX_SPACE does not have a dedicated code point
// So invent one here.
//
constexpr int CODEPOINT_LINEARSYNTAX_SPACE(-3);

//
// The string meta characters will have code points here, but they are not actual characters and do not have real code points
// 
constexpr int CODEPOINT_STRINGMETA_OPEN(-4);
constexpr int CODEPOINT_STRINGMETA_CLOSE(-5);

Token LongNameCodePointToOperator(int c);
int LongNameOperatorToCodePoint(Token t);
"}

Print["exporting CodePoint.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "CodePoint.h"}], Column[codePointCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]



operatorSource = 
  {"std::unordered_set<int> operatorCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedOperatorLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isOperatorCharacter() const { return operatorCodePoints.find(value_) != operatorCodePoints.end(); }", ""}

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
  {"Token LongNameCodePointToOperator(int c) {
switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", 
        toGlobal["Token`Operator`LongName`"<>#], ";"}]& /@ importedOperatorLongNames) ~Join~
    {"default:
std::cerr << \"Need to add operator: 0x\" << std::setfill('0') << std::setw(4) << std::hex << c << std::dec << \"\\n\";
assert(false && \"Need to add operator\");
return TOKEN_ERROR_INTERNAL;
}
}"}

LongNameOperatorToCodePointSource = 
  {"
int LongNameOperatorToCodePoint(Token t) {
switch (t) {"} ~Join~
    (Row[{"case", " ", toGlobal["Token`Operator`LongName`"<>#], ":", " ", "return",
         " ", toGlobal["CodePoint`LongName`"<>#], ";"}]& /@ importedOperatorLongNames) ~Join~
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
"}, operatorSource, spaceSource, newlineSource, commaSource, uninterpretableSource,
    LongNameCodePointToOperatorSource, 
    LongNameOperatorToCodePointSource]

Print["exporting CodePoint.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "CodePoint.cpp"}], Column[codePointCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]











(* Token *)
Print["generating Token"]

operatorMacros = 
  Association[
   ToExpression["Token`Operator`LongName`" <> #] -> Next& /@ importedOperatorLongNames]

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

enum Token {"} ~Join~
   KeyValueMap[(Row[{toGlobal[#], " = ", #2, ","}])&, enumMap] ~Join~
   {"};", ""} ~Join~
   {"std::string TokenToString(Token type);",
   "bool isOperator(Token type);",
   "bool isError(Token type);",
   ""}

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
res = Export[FileNameJoin[{pacletASTDir, "Token.wl"}], Column[tokenWL], "String"]

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
  Association[ToExpression["Token`Operator`LongName`" <> #] -> escapeString["Token`Operator`LongName`" <> #]& /@ importedOperatorLongNames]



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
    {"std::string TokenToString(Token Tok) {"} ~Join~
    {"switch (Tok) {"} ~Join~
    cases ~Join~
    {"default:"} ~Join~
    {"std::cerr << \"Unhandled token type: \" << std::to_string(Tok) << \"\\n\"; assert(false && \"Unhandled token type\"); return \"\";"} ~Join~
    {"}"} ~Join~
    {"}"} ~Join~
    {""} ~Join~
    {"bool isOperator(Token Tok) { return TOKEN_OPERATOR_FIRST <= Tok && Tok < TOKEN_OPERATOR_END; }"} ~Join~
    {"bool isError(Token Tok) { return TOKEN_ERROR_FIRST <= Tok && Tok < TOKEN_ERROR_END; }"} ~Join~
    {""}

Print["exporting Token.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Token.cpp"}], Column[tokenCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]









(* Precedence *)
Print["generating Precedence"]

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

enum precedence_t {"} ~Join~
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
res = Export[FileNameJoin[{pacletASTDir, "Precedence.wl"}], Column[precedenceWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]










(* Symbol *)
Print["generating Symbol"]

(*
sanity check Prefix operators
*)
If[Union[DownValues[PrefixOperatorToSymbol][[All, 2]]] =!= Union[DownValues[SymbolToPrefixOperatorString][[All, 1, 1, 1]]],
  Print["PrefixOperatorToSymbol and SymbolToPrefixOperatorString do not match"];
  Print["PrefixOperatorToSymbol:       ", Union[DownValues[PrefixOperatorToSymbol][[All, 2]]]];
  Print["SymbolToPrefixOperatorString: ", Union[DownValues[SymbolToPrefixOperatorString][[All, 1, 1, 1]]]];
  Quit[1]
]

(*
sanity check Postfix operators
*)
If[Union[DownValues[PostfixOperatorToSymbol][[All, 2]]] =!= Union[DownValues[SymbolToPostfixOperatorString][[All, 1, 1, 1]]],
  Print["PostfixOperatorToSymbol and SymbolToPostfixOperatorString do not match"];
  Print["PostfixOperatorToSymbol:       ", Union[DownValues[PostfixOperatorToSymbol][[All, 2]]]];
  Print["SymbolToPostfixOperatorString: ", Union[DownValues[SymbolToPostfixOperatorString][[All, 1, 1, 1]]]];
  Quit[1]
]

(*
sanity check Binary operators
*)
If[Union[DownValues[BinaryOperatorToSymbol][[All, 2]]] =!= Union[DownValues[SymbolToBinaryOperatorString][[All, 1, 1, 1]]],
  Print["BinaryOperatorToSymbol and SymbolToBinaryOperatorString do not match"];
  Print["BinaryOperatorToSymbol:       ", Union[DownValues[BinaryOperatorToSymbol][[All, 2]]]];
  Print["SymbolToBinaryOperatorString: ", Union[DownValues[SymbolToBinaryOperatorString][[All, 1, 1, 1]]]];
  Quit[1]
]

(*
sanity check Infix operators
*)
If[Union[DownValues[InfixOperatorToSymbol][[All, 2]]] =!= Union[DownValues[SymbolToInfixOperatorString][[All, 1, 1, 1]]],
  Print["InfixOperatorToSymbol and SymbolToInfixOperatorString do not match"];
  Print["InfixOperatorToSymbol:       ", Union[DownValues[InfixOperatorToSymbol][[All, 2]]]];
  Print["SymbolToInfixOperatorString: ", Union[DownValues[SymbolToInfixOperatorString][[All, 1, 1, 1]]]];
  Quit[1]
]

(*
sanity check Ternary operators
*)
If[Union[DownValues[TernaryOperatorsToSymbol][[All, 2]]] =!= Union[DownValues[SymbolToTernaryPair][[All, 1, 1, 1]]],
  Print["TernaryOperatorsToSymbol and SymbolToTernaryPair do not match"];
  Print["TernaryOperatorsToSymbol: ", Union[DownValues[TernaryOperatorsToSymbol][[All, 2]]]];
  Print["SymbolToTernaryPair:      ", Union[DownValues[SymbolToTernaryPair][[All, 1, 1, 1]]]];
  Quit[1]
]

If[Union[Flatten[DownValues[SymbolToTernaryPair][[All, 2]]]] =!= Union[DownValues[SymbolToTernaryOperatorString][[All, 1, 1, 1]]],
  Print["SymbolToTernaryPair and SymbolToTernaryOperatorString do not match"];
  Print["SymbolToTernaryPair:           ", Union[Flatten[DownValues[SymbolToTernaryPair][[All, 2]]]]];
  Print["SymbolToTernaryOperatorString: ", Union[DownValues[SymbolToTernaryOperatorString][[All, 1, 1, 1]]]];
  Quit[1]
]

(*
sanity check Group operators
*)
If[Union[Flatten[{DownValues[GroupOpenerToSymbol][[All, 2]],
                    DownValues[GroupCloserToSymbol][[All, 2]],
                    DownValues[GroupOpenerToMissingCloserSymbol][[All, 2]],
                    DownValues[GroupCloserToMissingOpenerSymbol][[All, 2]]}]] =!=
    Union[DownValues[SymbolToGroupPair][[All, 1, 1, 1]]],
  Print["GroupOpenerToSymbol ... and SymbolToGroupPair do not match"];
  Print["GroupOpenerToSymbol ...: ", Union[Flatten[{DownValues[GroupOpenerToSymbol][[All, 2]],
                                                      DownValues[GroupCloserToSymbol][[All, 2]],
                                                      DownValues[GroupOpenerToMissingCloserSymbol][[All, 2]],
                                                      DownValues[GroupCloserToMissingOpenerSymbol][[All, 2]]}]]];
  Print["SymbolToGroupPair:       ", Union[DownValues[SymbolToGroupPair][[All, 1, 1, 1]]]];
  Quit[1]
]

(*
Obtain all symbols by inspecting DownValues of functions
*)

symbols = Union[Join[DownValues[PrefixOperatorToSymbol][[All, 2]],
    DownValues[PostfixOperatorToSymbol][[All, 2]],
    DownValues[BinaryOperatorToSymbol][[All, 2]],
    DownValues[InfixOperatorToSymbol][[All, 2]],
    DownValues[TernaryOperatorToSymbol][[All, 2]],
    AST`Symbol`$Nodes,
    AST`Symbol`$Options,
    AST`Symbol`$Miscellaneous,
    AST`Symbol`$Tokens,
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

#include \"mathlink.h\"

#include <string>
#include <utility>

class Symbol {
public:
  constexpr Symbol(const char *Name) : Name(Name) {}
  const char *name() const;

  void put(MLINK mlp) const;

private:
  const char *Name;
};

bool operator==(const Symbol& lhs, const Symbol& rhs);

const Symbol& PostfixOperatorToSymbol(Token);
const Symbol& PrefixOperatorToSymbol(Token);
const Symbol& BinaryOperatorToSymbol(Token);
const Symbol& InfixOperatorToSymbol(Token);
const Symbol& TernaryOperatorsToSymbol(Token, Token);
const Symbol& GroupOpenerToSymbol(Token);

Token GroupOpenerToCloser(Token);

const Symbol& GroupCloserToSymbol(Token);
const Symbol& GroupOpenerToMissingCloserSymbol(Token);
const Symbol& GroupCloserToMissingOpenerSymbol(Token);

std::string SymbolToPrefixOperatorString(const Symbol&);
std::string SymbolToPostfixOperatorString(const Symbol&);
std::string SymbolToBinaryOperatorString(const Symbol&);
std::string SymbolToInfixOperatorString(const Symbol&);
std::string SymbolToTernaryOperatorString(const Symbol&);

std::pair<std::string, std::string> SymbolToGroupPair(const Symbol&);
std::pair<const Symbol&, const Symbol&> SymbolToTernaryOperatorPair(const Symbol&);
"} ~Join~
(Row[{"extern", " ", "const", " ", "Symbol&", " ", toGlobal["Symbol`"<>ToString[#]], ";"}]& /@ symbols) ~Join~
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
    ToString[sym]
    ,
    Context[sym]<>ToString[sym]
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

bool operator==(const Symbol& lhs, const Symbol& rhs) {
   return lhs.name() == rhs.name();
}

const char *Symbol::name() const {
   return Name;
}

void Symbol::put(MLINK mlp) const {
  MLPutSymbol(mlp, Name);
}

"} ~Join~ 

    (Row[{"const", " ", "Symbol&", " ", toGlobal["Symbol`"<>ToString[#]], " ", "=", " ", "Symbol(\"", stringifyForTransmitting[#], "\")", ";"}]& /@ symbols) ~Join~

      {""} ~Join~

      {"const Symbol& PrefixOperatorToSymbol(Token Type) {\nswitch (Type) {"} ~Join~
     
     Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PrefixOperatorToSymbol]] ~Join~ 
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
      "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& PostfixOperatorToSymbol(Token Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[PostfixOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& BinaryOperatorToSymbol(Token Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[BinaryOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& InfixOperatorToSymbol(Token Type) {\nswitch (Type) {"} ~Join~
     
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[InfixOperatorToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& TernaryOperatorsToSymbol(Token Type1, Token Type2) {"} ~Join~

     (*
      cannot use C++ switch statements with 2 arguments, so just do a series of if statements
     *)
     Map[Row[{"if (Type1 == ", toGlobal[#[[1, 1, 1]]], " && Type2 == ", toGlobal[#[[1, 1, 2]]], ") {", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";", "}"}]&, DownValues[TernaryOperatorsToSymbol]] ~Join~
      {"std::cerr << \"Unhandled Tokens: \" << TokenToString(Type1) << \" \" << TokenToString(Type2) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}"} ~Join~

     {""} ~Join~

     {"const Symbol& GroupOpenerToSymbol(Token Type) {\nswitch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[GroupOpenerToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"Token GroupOpenerToCloser(Token Type) {\nswitch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal[#[[2]]], ";"}]&, DownValues[GroupOpenerToCloser]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return TOKEN_UNKNOWN;",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& GroupCloserToSymbol(Token Type) {\nswitch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[GroupCloserToSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& GroupOpenerToMissingCloserSymbol(Token Type) {\nswitch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[GroupOpenerToMissingCloserSymbol]] ~Join~
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~

     {"const Symbol& GroupCloserToMissingOpenerSymbol(Token Type) {\nswitch (Type) {"} ~Join~
      Map[Row[{"case", " ", toGlobal[#[[1, 1, 1]]], ":", " ", "return", " ", toGlobal["Symbol`"<>ToString[#[[2]]]], ";"}]&, DownValues[GroupCloserToMissingOpenerSymbol]] ~Join~ 
      {"default: std::cerr << \"Unhandled Token: \" << TokenToString(Type) << \"\\n\"; assert(false && \"Unhandled token\"); return " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ";",
     "}\n}"} ~Join~

     {""} ~Join~
     {"std::pair<std::string, std::string> SymbolToGroupPair(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return std::make_pair(std::string(", 
          escapeString[#[[2, 1]]], "), std::string(", 
          escapeString[#[[2, 2]]], "));", "}"}]& /@ DownValues[SymbolToGroupPair]) ~Join~ 
      {"std::cerr << \"Unhandled Symbol: \" << Sym.name() << \"\\n\"; assert(false && \"Unhandled Symbol\"); return std::make_pair(std::string(\"XXX\"), std::string(\"XXX\"));",
     "}"} ~Join~

     {""} ~Join~
     {"std::string SymbolToPrefixOperatorString(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", escapeString[#[[2]]], ";", "}"}]& /@ DownValues[SymbolToPrefixOperatorString]) ~Join~
      {"std::cerr << \"Unhandled Symbol: \" << Sym.name() << \"\\n\"; assert(false && \"Unhandled Symbol\"); return \"XXX\";",
      "}",
     ""} ~Join~

     {"std::string SymbolToPostfixOperatorString(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", escapeString[#[[2]]], ";", "}"}]& /@ DownValues[SymbolToPostfixOperatorString]) ~Join~
      {" std::cerr << \"Unhandled Symbol: \" << Sym.name() << \"\\n\"; assert(false && \"Unhandled Symbol\"); return \"XXX\";",
     "}"} ~Join~
     {""} ~Join~

     {"std::string SymbolToBinaryOperatorString(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", escapeString[#[[2]]], ";", "}"}]& /@ DownValues[SymbolToBinaryOperatorString]) ~Join~
      {"std::cerr << \"Unhandled Symbol: \" << Sym.name() << \"\\n\"; assert(false && \"Unhandled Symbol\"); return \"XXX\";",
     "}",
     ""} ~Join~

     {"std::string SymbolToInfixOperatorString(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", escapeString[#[[2]]], ";", "}"}]& /@ DownValues[SymbolToInfixOperatorString]) ~Join~
      {"std::cerr << \"Unhandled Symbol: \" << Sym.name() << \"\\n\"; assert(false && \"Unhandled Symbol\"); return \"XXX\";",
     "}",
     ""} ~Join~

     {"std::string SymbolToTernaryOperatorString(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return ", escapeString[#[[2]]], ";", "}"}]& /@ DownValues[SymbolToTernaryOperatorString]) ~Join~
      {" std::cerr << \"Unhandled Symbol: \" << Sym.name() << \"\\n\"; assert(false && \"Unhandled Symbol\"); return \"XXX\";",
     "}",
     ""} ~Join~

     {"std::pair<const Symbol&, const Symbol&> SymbolToTernaryOperatorPair(const Symbol& Sym) {"} ~Join~
      (Row[{"if (Sym == ", toGlobal["Symbol`"<>ToString[#[[1, 1, 1]]]], ")", " ", 
          "{ return std::make_pair(", 
          toGlobal["Symbol`"<>ToString[#[[2, 1]]]], ", ", 
          toGlobal["Symbol`"<>ToString[#[[2, 2]]]], ");", "}"}]& /@ DownValues[SymbolToTernaryOperatorPair]) ~Join~
      {"return std::make_pair(" <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ", " <> toGlobal["Symbol`"<>ToString[InternalInvalid]] <> ");",
     "}",
     ""}

Print["exporting Symbol.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "Symbol.cpp"}], Column[symbolCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]






Print["Done generating additional required C++ and WL files"]


newTokens = Join[Names["Token`*"], Names["Token`*`*"]]

If[Complement[newTokens, oldTokens] =!= {},
  Print["New Tokens: ", Complement[newTokens, oldTokens]];
  Quit[1]
]

If[Length[Join[Names["LongName`*"], Names["LongName`*`*"]]] =!= 0,
  Print["LongName: ", Join[Names["LongName`*"], Names["LongName`*`*"]]];
  Quit[1]
]

newPrecedences = Join[Names["Precedence`*"], Names["Precedence`*`*"]]

If[Complement[newPrecedences, oldPrecedences] =!= {},
  Print["New Precedences: ", Complement[newPrecedences, oldPrecedences]];
  Quit[1]
]








