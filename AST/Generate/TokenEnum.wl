BeginPackage["AST`Generate`TokenEnum`"]

Begin["`Private`"]

Needs["AST`Generate`"]




(*
Use the System` context symbols for literals when we can

These are compiled as SYMBOL_FOO
*)
tokenToSymbol[Token`EndOfFile] = "Symbol`EndOfFile"
tokenToSymbol[Token`Symbol] = "Symbol`Symbol"
tokenToSymbol[Token`String] = "Symbol`String"
tokenToSymbol[Token`Integer] = "Symbol`Integer"
tokenToSymbol[Token`Real] = "Symbol`Real"

tokenToSymbol[Token`Hash] = "Symbol`Slot"
tokenToSymbol[Token`HashHash] = "Symbol`SlotSequence"

tokenToSymbol[Token`Percent] = "Symbol`Out"

tokenToSymbol[Token`Under] = "Symbol`Blank"
tokenToSymbol[Token`UnderUnder] = "Symbol`BlankSequence"
tokenToSymbol[Token`UnderUnderUnder] = "Symbol`BlankNullSequence"

tokenToSymbol[Token`UnderDot] = "Symbol`AST`OptionalDefault"

(*
everything else will be Symbol`Token`Foo

which is compiled as SYMBOL_TOKEN_FOO
*)
tokenToSymbol[s_] := "Symbol`"<>ToString[s]













Print["Generating TokenEnum..."]

operatorMacros = 
  Association[
   ToExpression["Token`LongName`" <> #] -> Next& /@ importedPunctuationLongNames]

joined = importedTokenEnumSource ~Join~ operatorMacros ~Join~ <| Token`Count -> Next |>

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

#include <unordered_map>
#include <cstdint> // for uint16_t

enum TokenEnum : uint16_t {"} ~Join~
   KeyValueMap[(Row[{toGlobal[#], " = ", #2, ","}])&, enumMap] ~Join~
   {"};"} ~Join~ {
"
bool isError(TokenEnum type);

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

Print["exporting TokenEnum.h"]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "TokenEnum.h"}], Column[tokenCPPHeader], "String"]

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

tokens = Keys[uniqueEnums]

operatorMacros = ToExpression["Token`LongName`" <> #]& /@ importedPunctuationLongNames



tokens = tokens ~Join~ operatorMacros

cases = Row[{"case ", toGlobal[#], ": return ", toGlobal[tokenToSymbol[#]], ";"}]& /@ tokens


tokenCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"TokenEnum.h\"

#include \"Symbol.h\"

#include <cassert>
"} ~Join~
    {"SymbolPtr& TokenToSymbol(TokenEnum Tok) {"} ~Join~
    {"switch (Tok) {"} ~Join~
    cases ~Join~
    {"default:"} ~Join~
    {"assert(false && \"Unhandled token type\"); return SYMBOL_TOKEN_UNKNOWN;"} ~Join~
    {"}"} ~Join~
    {"}"} ~Join~
    {""} ~Join~
    {"bool isError(TokenEnum Tok) { return TOKEN_ERROR_FIRST <= Tok && Tok < TOKEN_ERROR_END; }"} ~Join~
    {""}

Print["exporting TokenEnum.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "TokenEnum.cpp"}], Column[tokenCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Token"]

End[]

EndPackage[]
