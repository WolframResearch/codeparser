BeginPackage["AST`Generate`CharacterMaps`"]

Begin["`Private`"]

Needs["AST`Generate`"]




escapeString[s_] :=
  ToString[s, InputForm, CharacterEncoding -> "ASCII"]






Print["Generating Character Maps..."]

longNameToCodePointMap = {
"std::map <std::string, int> LongNameToCodePointMap {"} ~Join~ (Row[{"{", escapeString[#], ",", " ", toGlobal["CodePoint`LongName`"<>#], "}", ","}]& /@ Keys[importedLongNames]) ~Join~ {"};", ""}

codePointToLongNameMap = {
"std::map <int, std::string> CodePointToLongNameMap {"} ~Join~ (Row[{"{", toGlobal["CodePoint`LongName`"<>#], ",", " ", escapeString[#], "}", ","}] & /@ Keys[importedLongNames])~Join~{"};", ""}

toSpecialMap = {
"std::map <std::string, int> ToSpecialMap {"} ~Join~
{Row[{"{", "\"22\"", ", ", "CODEPOINT_STRINGMETA_DOUBLEQUOTE", "}", ",", " // 2hex "}]} ~Join~
{Row[{"{", "\"0022\"", ", ", "CODEPOINT_STRINGMETA_DOUBLEQUOTE", "}", ",", " // 4hex"}]} ~Join~
{Row[{"{", "\"000022\"", ", ", "CODEPOINT_STRINGMETA_DOUBLEQUOTE", "}", ",", " // 6hex"}]} ~Join~
{Row[{"{", "\"42\"", ", ", "CODEPOINT_STRINGMETA_DOUBLEQUOTE", "}", ",", " // octal"}]} ~Join~
{Row[{"{", "\"5c\"", ", ", "CODEPOINT_STRINGMETA_BACKSLASH", "}", ",", " // 2hex"}]} ~Join~
{Row[{"{", "\"005c\"", ", ", "CODEPOINT_STRINGMETA_BACKSLASH", "}", ",", " // 4hex"}]} ~Join~
{Row[{"{", "\"00005c\"", ", ", "CODEPOINT_STRINGMETA_BACKSLASH", "}", ",", " // 6hex"}]} ~Join~
{Row[{"{", "\"134\"", ", ", "CODEPOINT_STRINGMETA_BACKSLASH", "}", ",", " // octal"}]} ~Join~
{"};", ""}

fromSpecialMap = {
"std::map <int, int> FromSpecialMap {"} ~Join~
{Row[{"{", "CODEPOINT_STRINGMETA_DOUBLEQUOTE", ", ", "0x22", "}", ","}]} ~Join~
{Row[{"{", "CODEPOINT_STRINGMETA_BACKSLASH", ", ", "0x5c", "}", ","}]} ~Join~
{"};", ""}

characterMapsCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"CharacterMaps.h\"

#include \"LongNameDefines.h\"
"} ~Join~ longNameToCodePointMap ~Join~ codePointToLongNameMap ~Join~ toSpecialMap ~Join~ fromSpecialMap

Print["exporting CharacterMaps.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "CharacterMaps.cpp"}], Column[characterMapsCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Character Maps"]

End[]

EndPackage[]
