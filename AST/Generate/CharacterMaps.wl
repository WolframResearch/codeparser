BeginPackage["AST`Generate`CharacterMaps`"]

Begin["`Private`"]

Needs["AST`Generate`"]




escapeString[s_] :=
  ToString[s, InputForm, CharacterEncoding -> "ASCII"]






Print["Generating Character Maps..."]

longNameToCodePointMap = {
"std::map<std::string, int> LongNameToCodePointMap {"} ~Join~
	(Row[{"{", escapeString[#], ",", " ", toGlobal["CodePoint`LongName`"<>#], "}", ","}]& /@ Keys[importedLongNames]) ~Join~
	{"};", ""}

codePointToLongNameMap = {
"std::map<int, std::string> CodePointToLongNameMap {"} ~Join~
	(Row[{"{", toGlobal["CodePoint`LongName`"<>#], ",", " ", escapeString[#], "}", ","}] & /@ Keys[importedLongNames]) ~Join~
	{"};", ""}

toSpecialMap = {
"std::map<std::string, int> ToSpecialMap {"} ~Join~
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
"std::map<int, int> FromSpecialMap {"} ~Join~
{Row[{"{", "CODEPOINT_STRINGMETA_DOUBLEQUOTE", ", ", "0x22", "}", ","}]} ~Join~
{Row[{"{", "CODEPOINT_STRINGMETA_BACKSLASH", ", ", "0x5c", "}", ","}]} ~Join~
{"};", ""}

rawSet = {
"std::unordered_set<std::string> RawSet {"} ~Join~
{Row[{"{", "\"RawTab\"", "}", ","}]} ~Join~
{Row[{"{", "\"NewLine\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawReturn\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawEscape\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawSpace\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawExclamation\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawDoubleQuote\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawNumberSign\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawDollar\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawPercent\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawAmpersand\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawQuote\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawLeftParenthesis\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawRightParenthesis\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawStar\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawPlus\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawComma\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawDash\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawDot\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawSlash\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawColon\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawSemicolon\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawLess\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawEqual\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawGreater\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawQuestion\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawAt\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawLeftBracket\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawBackslash\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawRightBracket\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawWedge\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawUnderscore\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawBackquote\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawLeftBrace\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawVerticalBar\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawRightBrace\"", "}", ","}]} ~Join~
{Row[{"{", "\"RawTilde\"", "}", ","}]} ~Join~
{"};", "

bool isRaw(std::string LongNameStr) {
	return RawSet.find(LongNameStr) != RawSet.end();
}

"}


characterMapsCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"CharacterMaps.h\"

#include \"LongNameDefines.h\"

#include <unordered_set>
"} ~Join~
longNameToCodePointMap ~Join~
codePointToLongNameMap ~Join~
toSpecialMap ~Join~
fromSpecialMap ~Join~
rawSet

Print["exporting CharacterMaps.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "CharacterMaps.cpp"}], Column[characterMapsCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Character Maps"]

End[]

EndPackage[]
