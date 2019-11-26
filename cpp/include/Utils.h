
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
    //
    // Warn if tokens are on different lines
    //
//    static void differentLineWarning(Token Tok1, Token Tok2);
    
    //
    // Warn if node and token are on different lines
    //
//    static void differentLineWarning(NodeSeq& Args, Token Tok2);

    //
    // Warn if end of line
    //
//    static void endOfLineWarning(Token Tok, Token EndTok);

    //
    // Warn if not contiguous
    //
//    static void notContiguousWarning(Token Tok1, Token Tok2);

    static void strangeLetterlikeWarning(BufferAndLength bufAndLen, WLCharacter c);
#endif // !NISSUES
    
};

