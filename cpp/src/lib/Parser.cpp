
#include "Parser.h"

#include "Parselet.h"
#include "Tokenizer.h"
#include "Utils.h"
#include "Symbol.h"

#include <algorithm> // for generate with GCC and MSVC

Parser::Parser() : prefixParselets(), infixParselets(), startOfLineParselets(), contextSensitivePrefixParselets(), contextSensitiveInfixParselets(), expectedPossibleExpressionErrorParselet(), tokenQueue(), Issues(), currentAbortQ(nullptr), implicitTimesEnabled(true) {
    
    //
    // Setup all of the parselet lists with nullptr unique_ptrs
    //
    
    std::generate(std::begin(prefixParselets), std::end(prefixParselets), []() {
        return std::unique_ptr<PrefixParselet>(nullptr); });
    
    std::generate(std::begin(infixParselets), std::end(infixParselets), []() {
        return std::unique_ptr<InfixParselet>(nullptr); });
    
    std::generate(std::begin(startOfLineParselets), std::end(startOfLineParselets), []() {
        return std::unique_ptr<StartOfLineParselet>(nullptr); });
    
    std::generate(std::begin(contextSensitivePrefixParselets), std::end(contextSensitivePrefixParselets), []() {
        return std::unique_ptr<ContextSensitivePrefixParselet>(nullptr); });
    
    std::generate(std::begin(contextSensitiveInfixParselets), std::end(contextSensitiveInfixParselets), []() {
        return std::unique_ptr<ContextSensitiveInfixParselet>(nullptr); });
    
    //
    // Register all of the parselets
    //
    
    //
    // Prefix
    //
    registerPrefixParselet(TOKEN_MINUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUS)));
    registerPrefixParselet(TOKEN_PLUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUS)));
    registerPrefixParselet(TOKEN_BANG, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_BANG)));
    registerPrefixParselet(TOKEN_PLUSPLUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUSPLUS)));
    registerPrefixParselet(TOKEN_MINUSMINUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUSMINUS)));
    registerPrefixParselet(TOKEN_LESSLESS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LESSLESS)));
    registerPrefixParselet(TOKEN_LONGNAME_PLUSMINUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS)));
    registerPrefixParselet(TOKEN_LONGNAME_SUM, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SUM)));
    registerPrefixParselet(TOKEN_LONGNAME_NOT, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_NOT)));
    registerPrefixParselet(TOKEN_LONGNAME_SQRT, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQRT)));
    registerPrefixParselet(TOKEN_LONGNAME_MINUSPLUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS)));
    registerPrefixParselet(TOKEN_LONGNAME_DIFFERENTIALD, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DIFFERENTIALD)));
    registerPrefixParselet(TOKEN_LONGNAME_CAPITALDIFFERENTIALD, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CAPITALDIFFERENTIALD)));
    registerPrefixParselet(TOKEN_LONGNAME_MINUS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUS)));
    registerPrefixParselet(TOKEN_LONGNAME_DEL, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DEL)));
    registerPrefixParselet(TOKEN_LONGNAME_SQUARE, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQUARE)));
    registerPrefixParselet(TOKEN_LONGNAME_CONTOURINTEGRAL, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_DOUBLECONTOURINTEGRAL, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLECONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_CLOCKWISECONTOURINTEGRAL, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CLOCKWISECONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_PRODUCT, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_PRODUCT)));
    registerPrefixParselet(TOKEN_LONGNAME_CONTINUEDFRACTIONK, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CONTINUEDFRACTIONK)));
    registerPrefixParselet(TOKEN_LONGNAME_CIRCLETIMES, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_CIRCLETIMES)));
    registerPrefixParselet(TOKEN_LONGNAME_FORALL, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_FORALL)));
    registerPrefixParselet(TOKEN_LONGNAME_EXISTS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_EXISTS)));
    registerPrefixParselet(TOKEN_LONGNAME_NOTEXISTS, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_NOTEXISTS)));
    registerPrefixParselet(TOKEN_LONGNAME_COPRODUCT, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_COPRODUCT)));
    registerPrefixParselet(TOKEN_LONGNAME_PIECEWISE, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LONGNAME_PIECEWISE)));
    registerPrefixParselet(TOKEN_LINEARSYNTAX_BANG, std::unique_ptr<PrefixParselet>(new PrefixOperatorParselet(PRECEDENCE_LINEARSYNTAX_BANG)));
    
    
    //
    // Binary
    //
    
    registerInfixParselet(TOKEN_LONGNAME_BECAUSE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_BECAUSE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_THEREFORE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_THEREFORE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTEE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTEE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTEE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LEFTTEE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_MINUS, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_BINARY_MINUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_SLASH, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASH, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_CARET, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_CARET, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_CARETEQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_CARETEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_CARETCOLONEQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_CARETCOLONEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHAT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_MINUSGREATER, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_MINUSGREATER, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_ATAT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_ATAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSEMI, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHSEMI, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_SLASHDOT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHDOT, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_COLONGREATER, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_COLONGREATER, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSLASHDOT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHDOT, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_PLUSEQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_PLUSEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_STAREQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_STAREQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_MINUSEQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_MINUSEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHEQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LESSMINUSGREATER, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LESSMINUSGREATER, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSLASHAT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_AT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_AT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_ATATAT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_ATATAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSLASH, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_SLASHSLASH, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_COLONEQUAL, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_COLONEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_GREATERGREATER, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_GREATERGREATER, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_QUESTION, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_INFIX_QUESTION, ASSOCIATIVITY_NONASSOCIATIVE)));
    registerInfixParselet(TOKEN_GREATERGREATERGREATER, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_GREATERGREATERGREATER, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DIVIDE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIVIDE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DIVISIONSLASH, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIVISIONSLASH, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_IMPLIES, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_IMPLIES, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_PLUSMINUS, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_INFIX_LONGNAME_PLUSMINUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DIRECTEDEDGE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIRECTEDEDGE, ASSOCIATIVITY_NONASSOCIATIVE)));
    registerInfixParselet(TOKEN_LONGNAME_RULE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_RULEDELAYED, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULEDELAYED, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_UNDIRECTEDEDGE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, ASSOCIATIVITY_NONASSOCIATIVE)));
    registerInfixParselet(TOKEN_LONGNAME_FUNCTION, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_FUNCTION, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_MINUSPLUS, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_INFIX_LONGNAME_MINUSPLUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_TWOWAYRULE, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TWOWAYRULE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLEAPPLICATION, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLEMINUS, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEMINUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_SUCHTHAT, std::unique_ptr<InfixParselet>(new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUCHTHAT, ASSOCIATIVITY_RIGHT)));
    
    
    
    //
    // Infix
    //
    // Note that these are the operators that make sense to be infix in WL source code.
    //
    // These may not necessarily correspond to Flat functions in WL.
    //
    registerInfixParselet(TOKEN_EQUALEQUALEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_EQUALEQUALEQUAL)));
    registerInfixParselet(TOKEN_EQUALBANGEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_EQUALBANGEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_LESSFULLEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LESSFULLEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NESTEDLESSLESS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NESTEDLESSLESS)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTLESS)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSLESS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTLESSLESS)));
    registerInfixParselet(TOKEN_LONGNAME_LONGEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LONGEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTEQUALTILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTEQUALTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTHUMPEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTHUMPEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTHUMPDOWNHUMP, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTHUMPDOWNHUMP)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLEBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTLEFTTRIANGLEBAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLEBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTRIGHTTRIANGLEBAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTLESSSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERGREATER, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTGREATERGREATER)));
    registerInfixParselet(TOKEN_LONGNAME_NOTNESTEDGREATERGREATER, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTNESTEDGREATERGREATER)));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTGREATERSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTPRECEDESEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTPRECEDESEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUCCEEDSEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSUCCEEDSEQUAL)));
    registerInfixParselet(TOKEN_PLUS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_INFIX_PLUS)));
    registerInfixParselet(TOKEN_STAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_STAR)));
    registerInfixParselet(TOKEN_DOT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_DOT)));
    registerInfixParselet(TOKEN_STARSTAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_STARSTAR)));
    registerInfixParselet(TOKEN_AMPAMP, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_AMPAMP)));
    registerInfixParselet(TOKEN_BARBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_BARBAR)));
    registerInfixParselet(TOKEN_BAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_BAR)));
    registerInfixParselet(TOKEN_LESSGREATER, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LESSGREATER)));
    registerInfixParselet(TOKEN_TILDETILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_TILDETILDE)));
    registerInfixParselet(TOKEN_COLONCOLON, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_COLONCOLON)));
    registerInfixParselet(TOKEN_ATSTAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_ATSTAR)));
    registerInfixParselet(TOKEN_SLASHSTAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_SLASHSTAR)));
    registerInfixParselet(TOKEN_LONGNAME_ELEMENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_ELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_SUBSET, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_SUPERSET, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_SUBSETEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SUBSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SUPERSETEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SUPERSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTELEMENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUBSET, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUPERSET, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUBSETEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSUBSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUPERSETEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSUPERSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSQUARESUBSET, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSQUARESUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSQUARESUPERSET, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSQUARESUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_IMPLICITPLUS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_IMPLICITPLUS)));
    registerInfixParselet(TOKEN_LONGNAME_TIMES, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TIMES)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLETIMES, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLETIMES)));
    registerInfixParselet(TOKEN_LONGNAME_AND, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_AND)));
    registerInfixParselet(TOKEN_LONGNAME_OR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_OR)));
    registerInfixParselet(TOKEN_LONGNAME_XOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_XOR)));
    registerInfixParselet(TOKEN_LONGNAME_NAND, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NAND)));
    registerInfixParselet(TOKEN_LONGNAME_NOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPPERLEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPPERLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPPERRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPPERRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LOWERRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LOWERRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LOWERLEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LOWERLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTEEARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTEEARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNLEFTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNRIGHTVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNRIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTARROWLEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTARROWLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTARROWRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTARROWRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLERIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLERIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELEFTRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTARROWBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTARROWBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTRIGHTVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTRIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTRIGHTVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNLEFTRIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNLEFTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNRIGHTVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNRIGHTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNLEFTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNRIGHTTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNRIGHTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SHORTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTLEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SHORTLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPDOWNARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPTEEARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNTEEARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTUPVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTUPVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTDOWNVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOWNVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_UPARROWDOWNARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPARROWDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEUPARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLEUPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEDOWNARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLEDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEUPDOWNARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLEUPDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNARROWUPARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNARROWUPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LONGLEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LONGLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LONGRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LONGRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LONGLEFTRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LONGLEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELONGLEFTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELONGRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELONGLEFTRIGHTARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGLEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPARROWBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNARROWBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOWNARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPDOWNVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTUPDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPDOWNVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTUPDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTUPVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTDOWNVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTDOWNVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTUPVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOWNVECTORBAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTDOWNVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTUPTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTDOWNTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTDOWNTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTUPTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOWNTEEVECTOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTDOWNTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_UPEQUILIBRIUM, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UPEQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_REVERSEUPEQUILIBRIUM, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_REVERSEUPEQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTUPARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SHORTUPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTDOWNARROW, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SHORTDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_CENTERDOT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CENTERDOT)));
    registerInfixParselet(TOKEN_LONGNAME_TILDETILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDETILDE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDETILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDETILDE)));
    registerInfixParselet(TOKEN_LONGNAME_EQUIVALENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_EQUIVALENT)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTRIANGLEEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_TILDEEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_TILDEFULLEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDEFULLEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDEFULLEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDEFULLEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLEDOT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEDOT)));
    registerInfixParselet(TOKEN_LONGNAME_DISTRIBUTED, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DISTRIBUTED)));
    registerInfixParselet(TOKEN_LONGNAME_CONDITIONED, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CONDITIONED)));
    registerInfixParselet(TOKEN_LONGNAME_UNION, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_UNION)));
    registerInfixParselet(TOKEN_LONGNAME_INTERSECTION, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_INTERSECTION)));
    registerInfixParselet(TOKEN_LONGNAME_TENSORWEDGE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TENSORWEDGE)));
    registerInfixParselet(TOKEN_LONGNAME_TENSORPRODUCT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TENSORPRODUCT)));
    registerInfixParselet(TOKEN_LONGNAME_CROSS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CROSS)));
    registerInfixParselet(TOKEN_LONGNAME_GREATERTILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATERTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_PROPORTIONAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_PROPORTIONAL)));
    registerInfixParselet(TOKEN_LONGNAME_PROPORTION, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_PROPORTION)));
    registerInfixParselet(TOKEN_LONGNAME_LESSLESS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LESSLESS)));
    registerInfixParselet(TOKEN_LONGNAME_CONGRUENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT)));
    registerInfixParselet(TOKEN_LONGNAME_TILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDE)));
    registerInfixParselet(TOKEN_LONGNAME_SMALLCIRCLE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SMALLCIRCLE)));
    registerInfixParselet(TOKEN_LONGNAME_DIVIDES, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DIVIDES)));
    registerInfixParselet(TOKEN_LONGNAME_VERTICALSEPARATOR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_VERTICALSEPARATOR)));
    registerInfixParselet(TOKEN_LONGNAME_BACKSLASH, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_BACKSLASH)));
    registerInfixParselet(TOKEN_LONGNAME_DIAMOND, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DIAMOND)));
    registerInfixParselet(TOKEN_LONGNAME_WEDGE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_WEDGE)));
    registerInfixParselet(TOKEN_LONGNAME_VEE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_VEE)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLETIMES, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CIRCLETIMES)));
    registerInfixParselet(TOKEN_LONGNAME_STAR, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_STAR)));
    registerInfixParselet(TOKEN_LONGNAME_VERTICALTILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_VERTICALTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_COPRODUCT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_COPRODUCT)));
    registerInfixParselet(TOKEN_LONGNAME_CAP, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CAP)));
    registerInfixParselet(TOKEN_LONGNAME_CUP, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CUP)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLEPLUS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEPLUS)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTRIANGLE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTRIANGLE)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTRIANGLE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLE)));
    registerInfixParselet(TOKEN_LONGNAME_PERMUTATIONPRODUCT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_PERMUTATIONPRODUCT)));
    registerInfixParselet(TOKEN_LONGNAME_EQUILIBRIUM, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_EQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_REVERSEEQUILIBRIUM, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_REVERSEEQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_REVERSEELEMENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_REVERSEELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_NOTREVERSEELEMENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTREVERSEELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_EQUALTILDE, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_EQUALTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_PRECEDESSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_PRECEDESSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SUCCEEDSSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_SUCCEEDSSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_LESSSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_LESSSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_GREATERSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATERSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTPRECEDESSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTPRECEDESSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUCCEEDSSLANTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTSUCCEEDSSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_COLON, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_COLON)));
    registerInfixParselet(TOKEN_LONGNAME_CUPCAP, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_CUPCAP)));
    registerInfixParselet(TOKEN_LONGNAME_DOTEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_GREATEREQUALLESS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATEREQUALLESS)));
    registerInfixParselet(TOKEN_LONGNAME_GREATERFULLEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATERFULLEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_GREATERGREATER, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATERGREATER)));
    registerInfixParselet(TOKEN_LONGNAME_GREATERLESS, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATERLESS)));
    registerInfixParselet(TOKEN_LONGNAME_HUMPEQUAL, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_HUMPEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_HUMPDOWNHUMP, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_HUMPDOWNHUMP)));
    registerInfixParselet(TOKEN_LONGNAME_NESTEDGREATERGREATER, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NESTEDGREATERGREATER)));
    registerInfixParselet(TOKEN_LONGNAME_NOTCONGRUENT, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTCONGRUENT)));
    
    registerInfixParselet(TOKEN_FAKE_IMPLICITTIMES, std::unique_ptr<InfixParselet>(new InfixOperatorParselet(PRECEDENCE_FAKE_IMPLICITTIMES)));
    
    
    //
    // Postfix
    //
    registerInfixParselet(TOKEN_AMP, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_AMP)));
    registerInfixParselet(TOKEN_DOTDOT, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_DOTDOT)));
    registerInfixParselet(TOKEN_BANG, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_POSTFIX_BANG)));
    registerInfixParselet(TOKEN_MINUSMINUS, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_POSTFIX_MINUSMINUS)));
    registerInfixParselet(TOKEN_PLUSPLUS, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_POSTFIX_PLUSPLUS)));
    registerInfixParselet(TOKEN_DOTDOTDOT, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_DOTDOTDOT)));
    registerInfixParselet(TOKEN_BANGBANG, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_BANGBANG)));
    registerInfixParselet(TOKEN_SINGLEQUOTE, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_SINGLEQUOTE)));
    registerInfixParselet(TOKEN_LONGNAME_TRANSPOSE, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_LONGNAME_TRANSPOSE)));
    registerInfixParselet(TOKEN_LONGNAME_CONJUGATE, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT)));
    registerInfixParselet(TOKEN_LONGNAME_CONJUGATETRANSPOSE, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE)));
    registerInfixParselet(TOKEN_LONGNAME_HERMITIANCONJUGATE, std::unique_ptr<InfixParselet>(new PostfixOperatorParselet(PRECEDENCE_LONGNAME_HERMITIANCONJUGATE)));
    
    
    //
    // Calls
    //
    registerInfixParselet(TOKEN_OPENSQUARE, std::unique_ptr<InfixParselet>(new CallParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, std::unique_ptr<InfixParselet>(new CallParselet()));
    
    
    //
    // Groups
    //
    registerPrefixParselet(TOKEN_OPENPAREN, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_OPENSQUARE, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_OPENCURLY, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LESSBAR, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTANGLEBRACKET, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTCEILING, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTFLOOR, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTBRACKETINGBAR, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTASSOCIATION, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_OPENCURLYQUOTE, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    registerPrefixParselet(TOKEN_LONGNAME_OPENCURLYDOUBLEQUOTE, std::unique_ptr<PrefixParselet>(new GroupParselet()));
    
    //
    // StartOfLine
    //
    registerStartOfLineParselet(TOKEN_QUESTION, std::unique_ptr<StartOfLineParselet>(new StartOfLineParselet()));
    registerStartOfLineParselet(TOKEN_QUESTIONQUESTION, std::unique_ptr<StartOfLineParselet>(new StartOfLineParselet()));
    //
    // TODO: uncomment when there is support for different modes
    //
    //    registerStartOfLineParselet(TOKEN_BANG, std::unique_ptr<StartOfLineParselet>(new StartOfLineParselet()));
    //    registerStartOfLineParselet(TOKEN_BANGBANG, std::unique_ptr<StartOfLineParselet>(new StartOfLineParselet()));
    
    
    
    //
    // Special
    //
    
    // context sensitive parsing of  x_
    registerPrefixParselet(TOKEN_SYMBOL, std::unique_ptr<PrefixParselet>(new SymbolParselet()));
    registerContextSensitivePrefixParselet(TOKEN_SYMBOL, std::unique_ptr<ContextSensitivePrefixParselet>(new SymbolParselet()));
    
    // context sensitive parsing of _x
    registerPrefixParselet(TOKEN_UNDER, std::unique_ptr<PrefixParselet>(new UnderParselet()));
    registerContextSensitiveInfixParselet(TOKEN_UNDER, std::unique_ptr<ContextSensitiveInfixParselet>(new UnderParselet()));
    registerPrefixParselet(TOKEN_UNDERUNDER, std::unique_ptr<PrefixParselet>(new UnderParselet()));
    registerContextSensitiveInfixParselet(TOKEN_UNDERUNDER, std::unique_ptr<ContextSensitiveInfixParselet>(new UnderParselet()));
    registerPrefixParselet(TOKEN_UNDERUNDERUNDER, std::unique_ptr<PrefixParselet>(new UnderParselet()));
    registerContextSensitiveInfixParselet(TOKEN_UNDERUNDERUNDER, std::unique_ptr<ContextSensitiveInfixParselet>(new UnderParselet()));
    
    // trailing ; and , is allowed
    registerInfixParselet(TOKEN_SEMI, std::unique_ptr<InfixParselet>(new InfixOperatorWithTrailingParselet(PRECEDENCE_SEMI)));
    registerInfixParselet(TOKEN_COMMA, std::unique_ptr<InfixParselet>(new InfixOperatorWithTrailingParselet(PRECEDENCE_COMMA)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLECOMMA, std::unique_ptr<InfixParselet>(new InfixOperatorWithTrailingParselet(PRECEDENCE_LONGNAME_INVISIBLECOMMA)));
    
    // prefix, infix, postfix
    registerPrefixParselet(TOKEN_SEMISEMI, std::unique_ptr<PrefixParselet>(new SemiSemiParselet()));
    registerInfixParselet(TOKEN_SEMISEMI, std::unique_ptr<InfixParselet>(new SemiSemiParselet()));
    
    // ternary
    registerInfixParselet(TOKEN_TILDE, std::unique_ptr<InfixParselet>(new TildeParselet()));
    
    // context sensitive parsing of sym:obj and pat:v
    registerInfixParselet(TOKEN_COLON, std::unique_ptr<InfixParselet>(new ColonParselet()));
    registerContextSensitiveInfixParselet(TOKEN_COLON, std::unique_ptr<ContextSensitiveInfixParselet>(new ColonParselet()));
    
    // ternary, with different possibilities for second operator
    registerInfixParselet(TOKEN_SLASHCOLON, std::unique_ptr<InfixParselet>(new SlashColonParselet()));
    
    // FIXME: punt on parsing box syntax, reads tokens with no parsing
    registerPrefixParselet(TOKEN_LINEARSYNTAX_OPENPAREN, std::unique_ptr<PrefixParselet>(new LinearSyntaxOpenParenParselet()));
    
    // Has to handle  a =.  and  a = .
    registerInfixParselet(TOKEN_EQUAL, std::unique_ptr<InfixParselet>(new EqualParselet()));
    registerInfixParselet(TOKEN_EQUALDOT, std::unique_ptr<InfixParselet>(new EqualParselet()));
    
    // Has to handle \[Integral] f \[DifferentialD] x
    registerPrefixParselet(TOKEN_LONGNAME_INTEGRAL, std::unique_ptr<PrefixParselet>(new IntegralParselet()));
    
    // special Inequality
    registerInfixParselet(TOKEN_EQUALEQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_LESSEQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_BANGEQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_LESS, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_GREATER, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_GREATEREQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_EQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSEQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATEREQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTEQUAL, std::unique_ptr<InfixParselet>(new InequalityParselet()));
    
    // special VectorInequality
    registerInfixParselet(TOKEN_LONGNAME_VECTORGREATER, std::unique_ptr<InfixParselet>(new VectorInequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_VECTORGREATEREQUAL, std::unique_ptr<InfixParselet>(new VectorInequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_VECTORLESS, std::unique_ptr<InfixParselet>(new VectorInequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_VECTORLESSEQUAL, std::unique_ptr<InfixParselet>(new VectorInequalityParselet()));
    
    // Error handling
    expectedPossibleExpressionErrorParselet = std::unique_ptr<ExpectedPossibleExpressionErrorParselet>(new ExpectedPossibleExpressionErrorParselet());
}

Parser::~Parser() {}


void Parser::init(std::function<bool ()> AbortQ, const std::deque<Token>& queued) {
    
    tokenQueue = queued;
    Issues.clear();
    
    currentAbortQ = AbortQ;
    
    implicitTimesEnabled = true;
}

void Parser::deinit() {
    
    tokenQueue.clear();
    Issues.clear();
    
    currentAbortQ = nullptr;
}

void Parser::registerPrefixParselet(TokenEnum token, std::unique_ptr<PrefixParselet> P) {
    
    assert(prefixParselets[token] == nullptr);
    
    prefixParselets[token] = std::move(P);
}

void Parser::registerInfixParselet(TokenEnum token, std::unique_ptr<InfixParselet> P) {
    
    assert(infixParselets[token] == nullptr);
    
    infixParselets[token] = std::move(P);
}

void Parser::registerStartOfLineParselet(TokenEnum token, std::unique_ptr<StartOfLineParselet> P) {
    
    assert(startOfLineParselets[token] == nullptr);
    
    startOfLineParselets[token] = std::move(P);
}

void Parser::registerContextSensitivePrefixParselet(TokenEnum token, std::unique_ptr<ContextSensitivePrefixParselet> P) {
    
    assert(contextSensitivePrefixParselets[token] == nullptr);
    
    contextSensitivePrefixParselets[token] = std::move(P);
}

void Parser::registerContextSensitiveInfixParselet(TokenEnum token, std::unique_ptr<ContextSensitiveInfixParselet> P) {
    
    assert(contextSensitiveInfixParselets[token] == nullptr);
    
    contextSensitiveInfixParselets[token] = std::move(P);
}



void Parser::nextToken0(ParserContext Ctxt) {
    
    //
    // handle the queue before anything else
    //
    // We do not know anything about how many Tokens should be read
    //
    if (!tokenQueue.empty()) {
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return;
    }
    
    //
    // if Ctxt.LinearSyntaxFlag, then we are in linear syntax, and we disable stringifying next tokens
    //
    auto EnableStringifyNextToken = !Ctxt.LinearSyntaxFlag;
    TokenizerContext tokenizerCtxt(false, EnableStringifyNextToken, Ctxt.StringifyCurrentLine);
    
    TheTokenizer->nextToken(tokenizerCtxt);
}

Token Parser::nextToken(ParserContext Ctxt) {
    
    nextToken0(Ctxt);
    
    auto T = currentToken();
    
    
    return T;
}

Token Parser::currentToken() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    auto Tok = TheTokenizer->currentToken();
    
    return Tok;
}

//
// Used to insert ImplicitTimes
//
void Parser::prepend(const Token& current) {
    tokenQueue.insert(tokenQueue.begin(), current);
}

void Parser::prependInReverse(std::unique_ptr<LeafSeq> N) {
    
    if (N->empty()) {
        return;
    }
    
    auto V = N->getVectorDestructive();
    auto i = V->rbegin();
    for (; i != V->rend(); ++i ) {
        prepend((*i)->getToken());
    }
    delete V;
}

std::vector<SyntaxIssue> Parser::getIssues() const {
    return Issues;
}

//
// Only to be used by Parselets
//
void Parser::addIssue(SyntaxIssue I) {
    Issues.push_back(I);
}

bool Parser::isPossibleBeginningOfExpression(const Token& Tok, ParserContext CtxtIn) const {
    
    if (isError(Tok.Tok)) {
        return false;
    }
    
    if (Tok.Tok == TOKEN_ENDOFFILE) {
        return false;
    }
    
    //
    // trivia
    //
    if (Tok.Tok == TOKEN_WHITESPACE ||
        Tok.Tok == TOKEN_NEWLINE ||
        Tok.Tok == TOKEN_COMMENT ||
        Tok.Tok == TOKEN_LINECONTINUATION) {
        return false;
    }
    
    //
    // StartOfLine parselet?
    //
    if (CtxtIn.getGroupDepth() == 0) {
        if (Tok.Span.lines.start.Col == 1) {
            auto& S = startOfLineParselets[Tok.Tok];
            if (S != nullptr) {
                return true;
            }
        }
    }
    
    //
    // Prefix parselet?
    //
    // We want to test if prefix here because a token may be both prefix and infix, so must return true.
    // But if we removed this prefix test first, then we would hit the test for infix first, and then mistakenly return false.
    //
    auto& P = prefixParselets[Tok.Tok];
    if (P != nullptr) {
        return true;
    }
    
    //
    // Infix parselet?
    //
    auto& I = infixParselets[Tok.Tok];
    if (I != nullptr) {
        return false;
    }
    
    //
    // TODO: when closers are registered in parser, then check that
    //
    if (isCloser(Tok.Tok)) {
        return false;
    }
    
    //
    // Literal or Unhandled
    //
    
    return true;
}

const std::unique_ptr<InfixParselet>& Parser::findInfixParselet(TokenEnum Tok) const {
    auto& I = infixParselets[Tok];
    assert(I != nullptr);
    return I;
}

const std::unique_ptr<ContextSensitivePrefixParselet>& Parser::findContextSensitivePrefixParselet(TokenEnum Tok) const {
    auto& I = contextSensitivePrefixParselets[Tok];
    assert(I != nullptr);
    return I;
}

const std::unique_ptr<ContextSensitiveInfixParselet>& Parser::findContextSensitiveInfixParselet(TokenEnum Tok) const {
    auto& I = contextSensitiveInfixParselets[Tok];
    assert(I != nullptr);
    return I;
}

Precedence Parser::getCurrentTokenPrecedence(Token& TokIn, ParserContext Ctxt) {
    
    assert(TokIn.Tok != TOKEN_UNKNOWN);
    assert(TokIn.Tok != TOKEN_WHITESPACE);
    // allow top-level newlines
    assert(TokIn.Tok != TOKEN_NEWLINE || Ctxt.getGroupDepth() == 0);
    assert(TokIn.Tok != TOKEN_COMMENT);
    assert(TokIn.Tok != TOKEN_LINECONTINUATION);
    
    if (isError(TokIn.Tok)) {
        return PRECEDENCE_LOWEST;
    }
    
    if (TokIn.Tok == TOKEN_ENDOFFILE) {
        return PRECEDENCE_LOWEST;
    }
    
    //
    // TODO: review when closers have their own parselets
    //
    if (isCloser(TokIn.Tok)) {
        return PRECEDENCE_LOWEST;
    }
    
    auto& Infix = infixParselets[TokIn.Tok];
    
    if (Infix != nullptr) {
        
        //
        // There is an ambiguity with tokens that are both infix and prefix, e.g.
        // +  -  ;;
        //
        // Given the input  ;;;;
        // when parsing the second ;;, we could get here because ;; is registered as infix
        // But this particular ;; is a new expression, it is not actually infix
        //
        // Given the input  1+2
        // when parsing the +, make sure to treat it as infix and NOT prefix
        //
        // Solution is to handle infix parselets where needed, i.e., SemiSemiParselet
        //
        
        return Infix->getPrecedence();
    }
    
    if (TokIn.Tok == TOKEN_LONGNAME_DIFFERENTIALD && Ctxt.IntegralFlag) {
        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //
        
        return PRECEDENCE_LOWEST;
    }
    
    //
    // Literals or unhandled
    //
    
    //
    // Do not do ImplicitTimes across lines
    //
    if (TokIn.Tok == TOKEN_NEWLINE && Ctxt.getGroupDepth() == 0) {
        return PRECEDENCE_LOWEST;
    }
    
    if (!implicitTimesEnabled) {
        
        implicitTimesEnabled = true;
        
        return PRECEDENCE_LOWEST;
    }
    
    //
    // ImplicitTimes should not have gotten here
    //
    assert(!Ctxt.LinearSyntaxFlag);
    
    //
    // Note: Only insert token if input will actually be parsed as ImplicitTimes
    //
    // Here is an example:  a ~f x
    // When parsing the x, Ctxt.Precedence is PRECEDENCE_TILDE which is greater than PRECEDENCE_IMPLICITTIMES
    // We do not want to treat  f x  as a single ImplicitTimes expression here.
    //
    if (Ctxt.Prec <= PRECEDENCE_FAKE_IMPLICITTIMES) {
        
        //
        // ImplicitTimes is special because there is no source
        //
        // So invent source
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITTIMES, "", Source(TokIn.Span.lines.start));
        
        prepend(Implicit);
    }
    
    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

NodePtr Parser::parse(ParserContext CtxtIn) {
    
    if (isAbort()) {
        
        auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
        
        //        const auto& Aborted = std::unique_ptr<const Node>(new LeafNode(A));
        NodePtr Aborted = std::unique_ptr<Node>(new LeafNode(A));
        
        return Aborted;
    }
    
    auto Ctxt = CtxtIn;
    
    auto token = currentToken();
    
    assert(token.Tok != TOKEN_UNKNOWN);
    assert(token.Tok != TOKEN_WHITESPACE);
    assert(token.Tok != TOKEN_NEWLINE);
    assert(token.Tok != TOKEN_COMMENT);
    assert(token.Tok != TOKEN_LINECONTINUATION);
    
    //
    // Prefix start
    //
    
    NodePtr Left = nullptr;
    
    //
    // StartOfLine
    //
    if (CtxtIn.getGroupDepth() == 0) {
        if (token.Span.lines.start.Col == 1) {
            auto& S = startOfLineParselets[token.Tok];
            if (S != nullptr) {
                
                Left = S->parse(Ctxt);
            }
        }
    }
    
    if (Left == nullptr) {
        
        auto& I = prefixParselets[token.Tok];
        
        if (I != nullptr) {
            
            //
            // Prefix
            //
            
            Left = I->parse(Ctxt);
            
        } else {
            
            //
            // Literal or Unhandled
            //
            
            if (!isPossibleBeginningOfExpression(token, Ctxt)) {
                
                //
                // Some kind of error
                //
                
                auto& I = infixParselets[token.Tok];
                if (I != nullptr) {
                    
                    //
                    // Do not take next token
                    //
                    // Important to not duplicate token's Str here, it may also appear later
                    //
                    // Also, invent Source
                    //
                    
                    auto createdToken = Token( token.Tok == TOKEN_COMMA ? TOKEN_FAKE_IMPLICITNULL : TOKEN_ERROR_EXPECTEDOPERAND, "", Source(token.Span.lines.start, token.Span.lines.start));
                    
                    Left = std::unique_ptr<Node>(new LeafNode(createdToken));
                    
                } else if (token.Tok == Ctxt.Closer) {
                    //
                    // Handle the special cases of:
                    // { + }
                    // ( a + }
                    // ( a @ }
                    // We are here parsing the operators, but we don't want to descend and treat the } as the problem
                    //
                    
                    //
                    // Do not take next token
                    //
                    
                    auto createdToken = Token(token.Tok == TOKEN_COMMA ? TOKEN_FAKE_IMPLICITNULL : TOKEN_ERROR_EXPECTEDOPERAND, "", Source(token.Span.lines.start, token.Span.lines.start));
                    
                    Left = std::unique_ptr<Node>(new LeafNode(createdToken));
                    
                } else {
                    
                    //
                    // Some other kind of error
                    //
                    Left = expectedPossibleExpressionErrorParselet->parse(Ctxt);
                }
                
                //
                // We know this is an error, so make sure to disable ImplicitTimes
                //
                // We don't want to treat \ABC as \A * BC
                //
                
                implicitTimesEnabled = false;
                
            } else {

                nextToken(Ctxt);

                Left = std::unique_ptr<Node>(new LeafNode(token));
            }
        }
    }
    
    
    //
    // Infix loop
    //
    
    auto Args = std::unique_ptr<LeafSeq>(new LeafSeq);
    
    while (true) {
        
        if (isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            return std::unique_ptr<Node>(new LeafNode(A));
        }
        
        
        token = currentToken();
        
        token = Parser::eatAndPreserveToplevelNewlines(token, Ctxt, Args);
        
        auto TokenPrecedence = getCurrentTokenPrecedence(token, Ctxt);
        
        //
        // getCurrentTokenPrecedence() may have inserted something like a new IMPLICITTIMES token, so grab again
        //
        token = currentToken();
        
        if (Ctxt.Prec > TokenPrecedence) {
            break;
        }
        if (Ctxt.Prec == TokenPrecedence) {
            if (Ctxt.Assoc != ASSOCIATIVITY_RIGHT) {
                break;
            }
        }
        
        auto LeftSeq = std::unique_ptr<NodeSeq>(new NodeSeq);
        LeftSeq->reserve(1 + Args->size());
        
        LeftSeq->append(std::move(Left));
        
        LeftSeq->append(std::move(Args));
        
        auto LeftTmp = parse0(std::move(LeftSeq), TokenPrecedence, Ctxt);
        
        Left.reset(LeftTmp.release());
        
        Args.reset(new LeafSeq);
        
    } // while
    
    prependInReverse(std::move(Args));
    
    return Left;
}

NodePtr Parser::parse0(std::unique_ptr<NodeSeq> Left, Precedence TokenPrecedence, ParserContext CtxtIn) {
    
    //
    // getCurrentTokenPrecedence() may have inserted something like a new IMPLICITTIMES token, so grab again
    //
    auto token = currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = TokenPrecedence;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto& I = infixParselets[token.Tok];
    
    assert(I != nullptr);
    
    auto Res = I->parse(std::move(Left), Ctxt);
    
    return Res;
}

bool Parser::isAbort() const {
    if (!currentAbortQ) {
        return false;
    }
    
    return currentAbortQ();
}

const Token Parser::eatAll(const Token& TokIn, ParserContext Ctxt, std::unique_ptr<NodeSeq>& Args) {
    
    auto Tok = TokIn;
    
    while (Tok.Tok == TOKEN_WHITESPACE ||
           Tok.Tok == TOKEN_NEWLINE ||
           Tok.Tok == TOKEN_COMMENT ||
           Tok.Tok == TOKEN_LINECONTINUATION) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        Args->append(std::unique_ptr<Node>(new LeafNode(Tok)));
        
        Tok = TheParser->nextToken(Ctxt);
    }
    
    return Tok;
}

const Token Parser::eatAll(const Token& TokIn, ParserContext Ctxt, std::unique_ptr<LeafSeq>& Args) {
    
    auto Tok = TokIn;
    
    while (Tok.Tok == TOKEN_WHITESPACE ||
           Tok.Tok == TOKEN_NEWLINE ||
           Tok.Tok == TOKEN_COMMENT ||
           Tok.Tok == TOKEN_LINECONTINUATION) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        Args->append(std::unique_ptr<LeafNode>(new LeafNode(Tok)));
        
        Tok = TheParser->nextToken(Ctxt);
    }
    
    return Tok;
}

const Token Parser::eatAndPreserveToplevelNewlines(const Token& TokIn, ParserContext Ctxt, std::unique_ptr<NodeSeq>& Args) {
    
    auto Tok = TokIn;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        if (Tok.Tok == TOKEN_WHITESPACE ||
            Tok.Tok == TOKEN_COMMENT ||
            Tok.Tok == TOKEN_LINECONTINUATION) {
            
            Args->append(std::unique_ptr<LeafNode>(new LeafNode(Tok)));
            
            Tok = TheParser->nextToken(Ctxt);
            
        } else if (Tok.Tok == TOKEN_NEWLINE) {
            
            if (Ctxt.getGroupDepth() == 0) {
                
                break;
                
            } else {
                
                Args->append(std::unique_ptr<LeafNode>(new LeafNode(Tok)));
                
                Tok = TheParser->nextToken(Ctxt);
            }
            
        } else {
            break;
        }
    }
    
    return Tok;
}

const Token Parser::eatAndPreserveToplevelNewlines(const Token& TokIn, ParserContext Ctxt, std::unique_ptr<LeafSeq>& Args) {
    
    auto Tok = TokIn;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        if (Tok.Tok == TOKEN_WHITESPACE ||
            Tok.Tok == TOKEN_COMMENT ||
            Tok.Tok == TOKEN_LINECONTINUATION) {
            
            Args->append(std::unique_ptr<LeafNode>(new LeafNode(Tok)));
            
            Tok = TheParser->nextToken(Ctxt);
            
        } else if (Tok.Tok == TOKEN_NEWLINE) {
            
            if (Ctxt.getGroupDepth() == 0) {
                
                break;
                
            } else {
                
                Args->append(std::unique_ptr<LeafNode>(new LeafNode(Tok)));
                
                Tok = TheParser->nextToken(Ctxt);
            }
            
        } else {
            break;
        }
    }
    
    return Tok;
}

std::unique_ptr<Parser> TheParser = nullptr;



