
#pragma once

#include "Source.h"
#include "Node.h"
#include "Parser.h"

#include <string>

class Utils {
public:
    static int parseInteger(std::string s, int base);
    
    static std::string stringEscape(std::string s);
    
    static std::string makeGraphical(std::string s);
    
    static bool containsOnlyASCII(std::string s);
    
    static bool isStrangeLetterlikeLongName(std::string s);
    
    static bool isUnsupportedLongName(std::string s);
    
    static bool isUndocumentedLongName(std::string s);
    
    static int toDigit(int val);
    
    static int fromDigit(int d);
    
    static Token eatAll(Token Tok, ParserContext Ctxt, NodeSeq&);
    
    static Token eatAndPreserveToplevelNewlines(Token Tok, ParserContext Ctxt, NodeSeq&);
    
    static Token lastToken(NodePtr Node);
    
    //
    // Warn if tokens are on different lines
    //
    static void differentLineWarning(Token Tok1, Token Tok2, SyntaxIssueSeverity Severity);
    
    //
    // Warn if node and token are on different lines
    //
    static void differentLineWarning(NodeSeq Args, Token Tok2, SyntaxIssueSeverity Severity);
    
    //
    // Warn if end of line
    //
    static void endOfLineWarning(Token Tok, Token EndTok);
    
    //
    // Warn if not contiguous
    //
    static void notContiguousWarning(Token Tok1, Token Tok2);
};



class TimeScoper {
    
    std::chrono::microseconds *acc;
    std::chrono::high_resolution_clock::time_point t1;
    
public:
    TimeScoper(std::chrono::microseconds *acc);
    
    //
    // Define copy ctor and copy assignment to silence -Weffc++ warnings
    //
    
    TimeScoper(const TimeScoper& T) : acc(T.acc), t1(T.t1) {}
    
    TimeScoper& operator=(TimeScoper T) {
        if(&T == this) {
            return *this;
        }
        
        acc = T.acc;
        t1 = T.t1;
        
        return *this;
    }
    
    ~TimeScoper();
};


