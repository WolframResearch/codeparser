
#pragma once

#include "Node.h"
#include "WLCharacter.h"

#include <string>
#include <memory> // for unique_ptr

class Utils {
public:
    static size_t parseInteger(std::string s, size_t base);
    
    static bool isStrangeLetterlikeLongName(std::string s);
    
    static bool isVeryStrangeLetterlikeLongName(std::string s);
    
    static bool isUnsupportedLongName(std::string s);
    
    static bool isUndocumentedLongName(std::string s);
    
    static bool isMBSpace(int32_t point);
    
    static bool isMBPunctuation(int32_t point);
    
    static bool isMBNewline(int32_t point);
    
    static bool isMBUninterpretable(int32_t point);
    
#if !NISSUES
    static void strangeLetterlikeWarning(BufferAndLength bufAndLen, WLCharacter c);
#endif // !NISSUES
    
};

