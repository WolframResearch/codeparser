
#pragma once

#include <string>
#include <vector>

using codepoint = int32_t;

//
// Collection of utility functions for codepoints and long names
//
class LongNames {
public:
    
    static bool isMBNotStrangeLetterlike(codepoint point);

    static bool isMBPunctuation(codepoint point);

    static bool isMBWhitespace(codepoint point);

    static bool isMBNewline(codepoint point);

    static bool isMBUninterpretable(codepoint point);

    //
    // Is this \[Raw] something?
    //
    static bool isRaw(std::string LongNameStr);

    static std::vector<std::string> asciiReplacements(codepoint point);

    static std::string replacementGraphical(std::string replacement);
};
