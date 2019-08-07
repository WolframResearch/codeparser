
#pragma once

#include <map>
#include <string>

extern std::map<std::string, int> LongNameToCodePointMap;
extern std::map<int, std::string> CodePointToLongNameMap;

//
// Map "0022" to CODEPOINT_STRINGMETA_DOUBLEQUOTE
//
extern std::map<std::string, int> ToSpecialMap;

//
// Map CODEPOINT_STRINGMETA_DOUBLEQUOTE to 0x22 (to be used for printing out hex or octal later)
//
extern std::map<int, int> FromSpecialMap;
