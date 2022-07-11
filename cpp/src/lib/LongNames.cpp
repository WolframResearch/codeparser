
#include "LongNames.h"

#include "LongNamesRegistration.h"

#include <algorithm> // for lower_bound


//
//
//
bool LongNames::isRaw(std::string LongNameStr) {
    
    auto it =  std::lower_bound(RawSet.begin(), RawSet.end(), LongNameStr);
    
    return it != RawSet.end() && *it == LongNameStr;
}

//
//
//
bool LongNames::isMBNotStrangeLetterlike(codepoint point) { 
    
    auto it = std::lower_bound(mbNotStrangeLetterlikeCodePoints.begin(), mbNotStrangeLetterlikeCodePoints.end(), point);
    
    return it != mbNotStrangeLetterlikeCodePoints.end() && *it == point;
}



//
//
//
std::vector<std::string> LongNames::asciiReplacements(codepoint point) { 
    
    auto it = asciiReplacementsMap.find(point);
    
    return (it != asciiReplacementsMap.end()) ? it->second : std::vector<std::string>{};
}

//
//
//
std::string LongNames::replacementGraphical(std::string replacement) {
    
    if (replacement == " ") {
        
        //
        // \[SpaceIndicator]
        //
        
        // this was:
        // return "\u2423";
        //
        // But MSVC gave:
        // warning C4566: character represented by universal-character-name '\u2423' cannot be represented in the current code page (1252)
        //
        
        //
        // UTF-8 bytes for U+2423
        //
        return "\xe2\x90\xa3";
    }
    
    if (replacement == "\n") {
        return "\\n";
    }
    
    return replacement;
}

//
//
//
bool LongNames::isMBPunctuation(codepoint point) { 
    
    auto it = std::lower_bound(mbPunctuationCodePoints.begin(), mbPunctuationCodePoints.end(), point);
    
    return it != mbPunctuationCodePoints.end() && *it == point;
}

//
//
//
bool LongNames::isMBWhitespace(codepoint point) {
    
    auto it = std::lower_bound(mbWhitespaceCodePoints.begin(), mbWhitespaceCodePoints.end(), point);
    
    return it != mbWhitespaceCodePoints.end() && *it == point;
}

//
//
//
bool LongNames::isMBNewline(codepoint point) {
    
    auto it = std::lower_bound(mbNewlineCodePoints.begin(), mbNewlineCodePoints.end(), point);
    
    return it != mbNewlineCodePoints.end() && *it == point;
}

//
//
//
bool LongNames::isMBUninterpretable(codepoint point) {
    
    auto it = std::lower_bound(mbUninterpretableCodePoints.begin(), mbUninterpretableCodePoints.end(), point);
    
    return it != mbUninterpretableCodePoints.end() && *it == point;
}
