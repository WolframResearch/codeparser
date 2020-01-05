
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Source.h" // for Source

#include <string>
#include <unordered_set> // for unordered_set

extern std::unordered_set<std::string> undocumentedLongNames;

class Utils {
public:
    
    static int parseInteger(std::string s, size_t base);
    
    static bool isStrangeLetterlikeLongName(std::string s);
    
    static bool isVeryStrangeLetterlikeLongName(std::string s);
    
    static bool isUnsupportedLongName(std::string s);
    
    static bool isUndocumentedLongName(std::string s);
    
    static bool isMBWhitespace(int32_t point);
    
    static bool isMBPunctuation(int32_t point);
    
    static bool isMBNewline(int32_t point);
    
    static bool isMBUninterpretable(int32_t point);
    
    static bool isMBNonCharacter(int32_t point);
    
#if !NISSUES
    static void strangeLetterlikeWarning(Source Src, WLCharacter c);
#endif // !NISSUES
    
};

