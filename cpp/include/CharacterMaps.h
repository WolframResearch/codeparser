
#pragma once

#include <map>
#include <string>

extern std::map<std::string, int32_t> LongNameToCodePointMap;
extern std::map<int32_t, std::string> CodePointToLongNameMap;

//
// Map "0022" to CODEPOINT_STRINGMETA_DOUBLEQUOTE, instead of keeping it at 0x22
//
extern std::map<std::string, int32_t> ToSpecialMap;

//
// Map CODEPOINT_STRINGMETA_DOUBLEQUOTE to 0x22 (to be used for printing out hex or octal later)
//
extern std::map<int32_t, int32_t> FromSpecialMap;

//
// Is this \[Raw] something?
//
extern bool isRaw(std::string LongNameStr);
