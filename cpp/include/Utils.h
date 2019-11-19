
#pragma once

#include "Node.h"
#include "WLCharacter.h"

#include <string>
#include <memory> // for unique_ptr

class Utils {
public:
    static int parseInteger(std::string s, int base);
    
    static bool isStrangeLetterlikeLongName(std::string s);
    
    static bool isVeryStrangeLetterlikeLongName(std::string s);
    
    static bool isUnsupportedLongName(std::string s);
    
    static bool isUndocumentedLongName(std::string s);
    
#if !NISSUES
    //
    // Warn if tokens are on different lines
    //
    static void differentLineWarning(Token Tok1, Token Tok2);
#endif
    
#if !NISSUES
    //
    // Warn if node and token are on different lines
    //
    static void differentLineWarning(NodeSeq& Args, Token Tok2);
#endif
    
#if !NISSUES
    //
    // Warn if end of line
    //
    static void endOfLineWarning(Token Tok, Token EndTok);
#endif
    
#if !NISSUES
    //
    // Warn if not contiguous
    //
    static void notContiguousWarning(Token Tok1, Token Tok2);
#endif
    
#if !NISSUES
    static void strangeLetterlikeWarning(WLCharacter c);
#endif
    
    static bool parseBooleanSymbol(const char *);
    
    static SourceStyle parseSourceStyle(const char *);
};

