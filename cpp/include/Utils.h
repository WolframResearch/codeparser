
#pragma once

#include <string>

class Utils {
public:
    static int parseInteger(std::string s, int base);
    
    static std::string stringEscape(std::string s);
    
    static std::string makeGraphical(std::string s);
    
    static bool containsNonASCII(std::string s);
    
    static bool isStrangeLetterlikeLongName(std::string s);
    
    static bool isUnsupportedLongName(std::string s);
    
    static bool isUndocumentedLongName(std::string s);
    
    static int toDigit(int val);
    
    static int fromDigit(int d);
};
