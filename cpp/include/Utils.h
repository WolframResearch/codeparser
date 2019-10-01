
#pragma once

#include "Node.h"

#include <string>
#include <memory> // for unique_ptr

class Utils {
public:
    static int parseInteger(std::string s, int base);
    
    static bool isStrangeLetterlikeLongName(std::string s);
    
    static bool isUnsupportedLongName(std::string s);
    
    static bool isUndocumentedLongName(std::string s);
    
    //
    // Warn if tokens are on different lines
    //
    static void differentLineWarning(Token Tok1, Token Tok2);
    
    //
    // Warn if node and token are on different lines
    //
    static void differentLineWarning(NodeSeq& Args, Token Tok2);
    
    //
    // Warn if end of line
    //
    static void endOfLineWarning(Token Tok, Token EndTok);
    
    //
    // Warn if not contiguous
    //
    static void notContiguousWarning(Token Tok1, Token Tok2);
    
    static bool parseBooleanSymbol(const char *);
    
    static SourceStyle parseSourceStyle(const char *);
};

