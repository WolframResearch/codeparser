
#include "Parser.h"

#include "API.h" // for TheParserSession
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
//#include "ByteDecoder.h"
//#include "ByteBuffer.h"
//#include "Utils.h"
//#include "Symbol.h"

#include <algorithm> // for generate with GCC and MSVC

Parser::Parser() : prefixParselets(), infixParselets(), contextSensitiveSymbolParselet(ContextSensitivePrefixParseletPtr(new SymbolParselet())), contextSensitiveUnderParselet(ContextSensitiveInfixParseletPtr(new UnderParselet())), contextSensitiveColonParselet(ContextSensitiveInfixParseletPtr(new ColonParselet())), tokenQueue(), Issues() {
    
    //
    // Setup all of the parselet lists with nullptr unique_ptrs
    //
    
    std::generate(std::begin(prefixParselets), std::end(prefixParselets), []() {
        return PrefixParseletPtr(nullptr); });
    
    std::generate(std::begin(infixParselets), std::end(infixParselets), []() {
        return InfixParseletPtr(nullptr); });
    
#if STARTOFLINE
    std::generate(std::begin(startOfLineParselets), std::end(startOfLineParselets), []() {
        return StartOfLineParseletPtr(nullptr); });
#endif // STARTOFLINE
    
    //
    // Register all of the parselets
    //
    
    //
    // Prefix
    //
    registerPrefixParselet(TOKEN_MINUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_MINUS, PRECEDENCE_PREFIX_MINUS)));
    registerPrefixParselet(TOKEN_PLUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_PLUS, PRECEDENCE_PREFIX_PLUS)));
    registerPrefixParselet(TOKEN_BANG.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_BANG, PRECEDENCE_PREFIX_BANG)));
    registerPrefixParselet(TOKEN_PLUSPLUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_PLUSPLUS, PRECEDENCE_PREFIX_PLUSPLUS)));
    registerPrefixParselet(TOKEN_MINUSMINUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_MINUSMINUS, PRECEDENCE_PREFIX_MINUSMINUS)));
    registerPrefixParselet(TOKEN_LONGNAME_PLUSMINUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_PLUSMINUS, PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS)));
    registerPrefixParselet(TOKEN_LONGNAME_SUM.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_SUM, PRECEDENCE_LONGNAME_SUM)));
    registerPrefixParselet(TOKEN_LONGNAME_NOT.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_NOT, PRECEDENCE_LONGNAME_NOT)));
    registerPrefixParselet(TOKEN_LONGNAME_SQRT.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_SQRT, PRECEDENCE_LONGNAME_SQRT)));
    registerPrefixParselet(TOKEN_LONGNAME_MINUSPLUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_MINUSPLUS, PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS)));
    registerPrefixParselet(TOKEN_LONGNAME_DIFFERENTIALD.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_DIFFERENTIALD, PRECEDENCE_LONGNAME_DIFFERENTIALD)));
    registerPrefixParselet(TOKEN_LONGNAME_CAPITALDIFFERENTIALD.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_CAPITALDIFFERENTIALD, PRECEDENCE_LONGNAME_CAPITALDIFFERENTIALD)));
    registerPrefixParselet(TOKEN_LONGNAME_MINUS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_MINUS, PRECEDENCE_PREFIX_LONGNAME_MINUS)));
    registerPrefixParselet(TOKEN_LONGNAME_DEL.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_DEL, PRECEDENCE_LONGNAME_DEL)));
    registerPrefixParselet(TOKEN_LONGNAME_SQUARE.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_SQUARE, PRECEDENCE_LONGNAME_SQUARE)));
    registerPrefixParselet(TOKEN_LONGNAME_CONTOURINTEGRAL.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_CONTOURINTEGRAL, PRECEDENCE_LONGNAME_CONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_DOUBLECONTOURINTEGRAL.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_DOUBLECONTOURINTEGRAL, PRECEDENCE_LONGNAME_DOUBLECONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_CLOCKWISECONTOURINTEGRAL.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_CLOCKWISECONTOURINTEGRAL, PRECEDENCE_LONGNAME_CLOCKWISECONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, PRECEDENCE_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL)));
    registerPrefixParselet(TOKEN_LONGNAME_PRODUCT.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_PRODUCT, PRECEDENCE_LONGNAME_PRODUCT)));
    registerPrefixParselet(TOKEN_LONGNAME_CONTINUEDFRACTIONK.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_CONTINUEDFRACTIONK, PRECEDENCE_LONGNAME_CONTINUEDFRACTIONK)));
    registerPrefixParselet(TOKEN_LONGNAME_CIRCLETIMES.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_CIRCLETIMES, PRECEDENCE_PREFIX_LONGNAME_CIRCLETIMES)));
    registerPrefixParselet(TOKEN_LONGNAME_FORALL.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_FORALL, PRECEDENCE_LONGNAME_FORALL)));
    registerPrefixParselet(TOKEN_LONGNAME_EXISTS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_EXISTS, PRECEDENCE_LONGNAME_EXISTS)));
    registerPrefixParselet(TOKEN_LONGNAME_NOTEXISTS.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_NOTEXISTS, PRECEDENCE_LONGNAME_NOTEXISTS)));
    registerPrefixParselet(TOKEN_LONGNAME_COPRODUCT.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_COPRODUCT, PRECEDENCE_PREFIX_LONGNAME_COPRODUCT)));
    registerPrefixParselet(TOKEN_LONGNAME_PIECEWISE.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_PIECEWISE, PRECEDENCE_LONGNAME_PIECEWISE)));
    registerPrefixParselet(TOKEN_LONGNAME_INVISIBLEPREFIXSCRIPTBASE.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, PRECEDENCE_LONGNAME_INVISIBLEPREFIXSCRIPTBASE)));
    registerPrefixParselet(TOKEN_LONGNAME_EXPECTATIONE.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_EXPECTATIONE, PRECEDENCE_LONGNAME_EXPECTATIONE)));
    registerPrefixParselet(TOKEN_LONGNAME_CUBEROOT.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_CUBEROOT, PRECEDENCE_LONGNAME_CUBEROOT)));
    registerPrefixParselet(TOKEN_LONGNAME_PROBABILITYPR.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LONGNAME_PROBABILITYPR, PRECEDENCE_LONGNAME_PROBABILITYPR)));
    registerPrefixParselet(TOKEN_BANGBANG.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_BANGBANG, PRECEDENCE_FAKE_PREFIX_BANGBANG)));
    registerPrefixParselet(TOKEN_LINEARSYNTAX_BANG.value(), PrefixParseletPtr(new PrefixOperatorParselet(TOKEN_LINEARSYNTAX_BANG, PRECEDENCE_LINEARSYNTAX_BANG)));
    
    
    //
    // Binary
    //
    
    registerInfixParselet(TOKEN_SLASH.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASH, PRECEDENCE_SLASH, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_CARET.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_CARET, PRECEDENCE_CARET, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_CARETEQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_CARETEQUAL, PRECEDENCE_CARETEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_CARETCOLONEQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_CARETCOLONEQUAL, PRECEDENCE_CARETCOLONEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHAT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHAT, PRECEDENCE_SLASHAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_MINUSGREATER.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_MINUSGREATER, PRECEDENCE_MINUSGREATER, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_ATAT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_ATAT, PRECEDENCE_ATAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSEMI.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHSEMI, PRECEDENCE_SLASHSEMI, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_SLASHDOT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHDOT, PRECEDENCE_SLASHDOT, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_COLONGREATER.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_COLONGREATER, PRECEDENCE_COLONGREATER, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSLASHDOT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHSLASHDOT, PRECEDENCE_SLASHSLASHDOT, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_PLUSEQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_PLUSEQUAL, PRECEDENCE_PLUSEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_STAREQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_STAREQUAL, PRECEDENCE_STAREQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_MINUSEQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_MINUSEQUAL, PRECEDENCE_MINUSEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHEQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHEQUAL, PRECEDENCE_SLASHEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LESSMINUSGREATER.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LESSMINUSGREATER, PRECEDENCE_LESSMINUSGREATER, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSLASHAT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHSLASHAT, PRECEDENCE_SLASHSLASHAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_AT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_AT, PRECEDENCE_AT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_ATATAT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_ATATAT, PRECEDENCE_ATATAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_SLASHSLASH.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_SLASHSLASH, PRECEDENCE_SLASHSLASH, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_COLONEQUAL.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_QUESTION.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_QUESTION, PRECEDENCE_INFIX_QUESTION, ASSOCIATIVITY_NONASSOCIATIVE)));
    registerInfixParselet(TOKEN_LONGNAME_DIVIDE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_DIVIDE, PRECEDENCE_LONGNAME_DIVIDE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DIVISIONSLASH.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_DIVISIONSLASH, PRECEDENCE_LONGNAME_DIVISIONSLASH, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_IMPLIES.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_IMPLIES, PRECEDENCE_LONGNAME_IMPLIES, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_ROUNDIMPLIES.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_ROUNDIMPLIES, PRECEDENCE_LONGNAME_ROUNDIMPLIES, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_PLUSMINUS.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_PLUSMINUS, PRECEDENCE_INFIX_LONGNAME_PLUSMINUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DIRECTEDEDGE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_DIRECTEDEDGE, PRECEDENCE_LONGNAME_DIRECTEDEDGE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_RULE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_RULE, PRECEDENCE_LONGNAME_RULE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_RULEDELAYED.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_RULEDELAYED, PRECEDENCE_LONGNAME_RULEDELAYED, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_UNDIRECTEDEDGE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_UNDIRECTEDEDGE, PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_FUNCTION.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_FUNCTION, PRECEDENCE_LONGNAME_FUNCTION, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_MINUSPLUS.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_MINUSPLUS, PRECEDENCE_INFIX_LONGNAME_MINUSPLUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_TWOWAYRULE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_TWOWAYRULE, PRECEDENCE_LONGNAME_TWOWAYRULE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLEAPPLICATION.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_INVISIBLEAPPLICATION, PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLEMINUS.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_CIRCLEMINUS, PRECEDENCE_LONGNAME_CIRCLEMINUS, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_SUCHTHAT.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_SUCHTHAT, PRECEDENCE_LONGNAME_SUCHTHAT, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_PERPENDICULAR.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_PERPENDICULAR, PRECEDENCE_LONGNAME_PERPENDICULAR, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_BECAUSE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_BECAUSE, PRECEDENCE_LONGNAME_BECAUSE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_THEREFORE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_THEREFORE, PRECEDENCE_LONGNAME_THEREFORE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTEE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_RIGHTTEE, PRECEDENCE_LONGNAME_RIGHTTEE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTEE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_LEFTTEE, PRECEDENCE_LONGNAME_LEFTTEE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLERIGHTTEE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_DOUBLERIGHTTEE, PRECEDENCE_LONGNAME_DOUBLERIGHTTEE, ASSOCIATIVITY_RIGHT)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELEFTTEE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_DOUBLELEFTTEE, PRECEDENCE_LONGNAME_DOUBLELEFTTEE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_UPTEE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_UPTEE, PRECEDENCE_LONGNAME_UPTEE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNTEE.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_DOWNTEE, PRECEDENCE_LONGNAME_DOWNTEE, ASSOCIATIVITY_LEFT)));
    registerInfixParselet(TOKEN_LONGNAME_MINUS.value(), InfixParseletPtr(new BinaryOperatorParselet(TOKEN_LONGNAME_MINUS, PRECEDENCE_INFIX_LONGNAME_MINUS, ASSOCIATIVITY_LEFT)));
    
    
    //
    // Infix
    //
    // Note that these are the operators that make sense to be infix in WL source code.
    //
    // These may not necessarily correspond to Flat functions in WL.
    //
    registerInfixParselet(TOKEN_MINUS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_MINUS, PRECEDENCE_INFIX_MINUS)));
    registerInfixParselet(TOKEN_EQUALEQUALEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_EQUALEQUALEQUAL, PRECEDENCE_EQUALEQUALEQUAL)));
    registerInfixParselet(TOKEN_EQUALBANGEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_EQUALBANGEQUAL, PRECEDENCE_EQUALBANGEQUAL)));
    registerInfixParselet(TOKEN_PLUS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_PLUS, PRECEDENCE_INFIX_PLUS)));
    registerInfixParselet(TOKEN_STAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_STAR, PRECEDENCE_STAR)));
    registerInfixParselet(TOKEN_DOT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_DOT, PRECEDENCE_DOT)));
    registerInfixParselet(TOKEN_STARSTAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_STARSTAR, PRECEDENCE_STARSTAR)));
    registerInfixParselet(TOKEN_AMPAMP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_AMPAMP, PRECEDENCE_AMPAMP)));
    registerInfixParselet(TOKEN_BARBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_BARBAR, PRECEDENCE_BARBAR)));
    registerInfixParselet(TOKEN_BAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_BAR, PRECEDENCE_BAR)));
    registerInfixParselet(TOKEN_LESSGREATER.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LESSGREATER, PRECEDENCE_LESSGREATER)));
    registerInfixParselet(TOKEN_TILDETILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_TILDETILDE, PRECEDENCE_TILDETILDE)));
    registerInfixParselet(TOKEN_ATSTAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_ATSTAR, PRECEDENCE_ATSTAR)));
    registerInfixParselet(TOKEN_SLASHSTAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_SLASHSTAR, PRECEDENCE_SLASHSTAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTHUMPEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTHUMPEQUAL, PRECEDENCE_LONGNAME_NOTHUMPEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTHUMPDOWNHUMP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTHUMPDOWNHUMP, PRECEDENCE_LONGNAME_NOTHUMPDOWNHUMP)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTRIANGLEBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTTRIANGLEBAR, PRECEDENCE_LONGNAME_LEFTTRIANGLEBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTRIANGLEBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTTRIANGLEBAR, PRECEDENCE_LONGNAME_RIGHTTRIANGLEBAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLEBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLEBAR, PRECEDENCE_LONGNAME_NOTLEFTTRIANGLEBAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLEBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLEBAR, PRECEDENCE_LONGNAME_NOTRIGHTTRIANGLEBAR)));
    registerInfixParselet(TOKEN_LONGNAME_ELEMENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_ELEMENT, PRECEDENCE_LONGNAME_ELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_SUBSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUBSET, PRECEDENCE_LONGNAME_SUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_SUPERSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUPERSET, PRECEDENCE_LONGNAME_SUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_SUBSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUBSETEQUAL, PRECEDENCE_LONGNAME_SUBSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SUPERSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUPERSETEQUAL, PRECEDENCE_LONGNAME_SUPERSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTELEMENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTELEMENT, PRECEDENCE_LONGNAME_NOTELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUBSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUBSET, PRECEDENCE_LONGNAME_NOTSUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUPERSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUPERSET, PRECEDENCE_LONGNAME_NOTSUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUBSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUBSETEQUAL, PRECEDENCE_LONGNAME_NOTSUBSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUPERSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUPERSETEQUAL, PRECEDENCE_LONGNAME_NOTSUPERSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SQUARESUBSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SQUARESUBSET, PRECEDENCE_LONGNAME_SQUARESUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_SQUARESUPERSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SQUARESUPERSET, PRECEDENCE_LONGNAME_SQUARESUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSQUARESUBSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSQUARESUBSET, PRECEDENCE_LONGNAME_NOTSQUARESUBSET)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSQUARESUPERSET.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSQUARESUPERSET, PRECEDENCE_LONGNAME_NOTSQUARESUPERSET)));
    registerInfixParselet(TOKEN_LONGNAME_SQUARESUBSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SQUARESUBSETEQUAL, PRECEDENCE_LONGNAME_SQUARESUBSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SQUARESUPERSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SQUARESUPERSETEQUAL, PRECEDENCE_LONGNAME_SQUARESUPERSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSQUARESUBSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSQUARESUBSETEQUAL, PRECEDENCE_LONGNAME_NOTSQUARESUBSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSQUARESUPERSETEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSQUARESUPERSETEQUAL, PRECEDENCE_LONGNAME_NOTSQUARESUPERSETEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_IMPLICITPLUS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_IMPLICITPLUS, PRECEDENCE_LONGNAME_IMPLICITPLUS)));
    registerInfixParselet(TOKEN_LONGNAME_TIMES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TIMES, PRECEDENCE_LONGNAME_TIMES)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLETIMES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_INVISIBLETIMES, PRECEDENCE_LONGNAME_INVISIBLETIMES)));
    registerInfixParselet(TOKEN_LONGNAME_AND.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_AND, PRECEDENCE_LONGNAME_AND)));
    registerInfixParselet(TOKEN_LONGNAME_OR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_OR, PRECEDENCE_LONGNAME_OR)));
    registerInfixParselet(TOKEN_LONGNAME_XOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_XOR, PRECEDENCE_LONGNAME_XOR)));
    registerInfixParselet(TOKEN_LONGNAME_NAND.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NAND, PRECEDENCE_LONGNAME_NAND)));
    registerInfixParselet(TOKEN_LONGNAME_NOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOR, PRECEDENCE_LONGNAME_NOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTARROW, PRECEDENCE_LONGNAME_LEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTARROW, PRECEDENCE_LONGNAME_RIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTRIGHTARROW, PRECEDENCE_LONGNAME_LEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPPERLEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPPERLEFTARROW, PRECEDENCE_LONGNAME_UPPERLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPPERRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPPERRIGHTARROW, PRECEDENCE_LONGNAME_UPPERRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LOWERRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LOWERRIGHTARROW, PRECEDENCE_LONGNAME_LOWERRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LOWERLEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LOWERLEFTARROW, PRECEDENCE_LONGNAME_LOWERLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTEEARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTTEEARROW, PRECEDENCE_LONGNAME_LEFTTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTEEARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTTEEARROW, PRECEDENCE_LONGNAME_RIGHTTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTVECTOR, PRECEDENCE_LONGNAME_LEFTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNLEFTVECTOR, PRECEDENCE_LONGNAME_DOWNLEFTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTVECTOR, PRECEDENCE_LONGNAME_RIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNRIGHTVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNRIGHTVECTOR, PRECEDENCE_LONGNAME_DOWNRIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTARROWLEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTARROWLEFTARROW, PRECEDENCE_LONGNAME_RIGHTARROWLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTARROWRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTARROWRIGHTARROW, PRECEDENCE_LONGNAME_LEFTARROWRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLELEFTARROW, PRECEDENCE_LONGNAME_DOUBLELEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLERIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLERIGHTARROW, PRECEDENCE_LONGNAME_DOUBLERIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELEFTRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLELEFTRIGHTARROW, PRECEDENCE_LONGNAME_DOUBLELEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTARROWBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTARROWBAR, PRECEDENCE_LONGNAME_LEFTARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTARROWBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTARROWBAR, PRECEDENCE_LONGNAME_RIGHTARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTRIGHTVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTRIGHTVECTOR, PRECEDENCE_LONGNAME_LEFTRIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTRIGHTVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNLEFTRIGHTVECTOR, PRECEDENCE_LONGNAME_DOWNLEFTRIGHTVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTVECTORBAR, PRECEDENCE_LONGNAME_LEFTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTVECTORBAR, PRECEDENCE_LONGNAME_RIGHTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNLEFTVECTORBAR, PRECEDENCE_LONGNAME_DOWNLEFTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNRIGHTVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNRIGHTVECTORBAR, PRECEDENCE_LONGNAME_DOWNRIGHTVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTTEEVECTOR, PRECEDENCE_LONGNAME_LEFTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTTEEVECTOR, PRECEDENCE_LONGNAME_RIGHTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNLEFTTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNLEFTTEEVECTOR, PRECEDENCE_LONGNAME_DOWNLEFTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNRIGHTTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNRIGHTTEEVECTOR, PRECEDENCE_LONGNAME_DOWNRIGHTTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SHORTRIGHTARROW, PRECEDENCE_LONGNAME_SHORTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTLEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SHORTLEFTARROW, PRECEDENCE_LONGNAME_SHORTLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPARROW, PRECEDENCE_LONGNAME_UPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNARROW, PRECEDENCE_LONGNAME_DOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPDOWNARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPDOWNARROW, PRECEDENCE_LONGNAME_UPDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPTEEARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPTEEARROW, PRECEDENCE_LONGNAME_UPTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNTEEARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNTEEARROW, PRECEDENCE_LONGNAME_DOWNTEEARROW)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTUPVECTOR, PRECEDENCE_LONGNAME_RIGHTUPVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTUPVECTOR, PRECEDENCE_LONGNAME_LEFTUPVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTDOWNVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTDOWNVECTOR, PRECEDENCE_LONGNAME_RIGHTDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOWNVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTDOWNVECTOR, PRECEDENCE_LONGNAME_LEFTDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_UPARROWDOWNARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPARROWDOWNARROW, PRECEDENCE_LONGNAME_UPARROWDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEUPARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLEUPARROW, PRECEDENCE_LONGNAME_DOUBLEUPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEDOWNARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLEDOWNARROW, PRECEDENCE_LONGNAME_DOUBLEDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEUPDOWNARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLEUPDOWNARROW, PRECEDENCE_LONGNAME_DOUBLEUPDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNARROWUPARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNARROWUPARROW, PRECEDENCE_LONGNAME_DOWNARROWUPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LONGLEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LONGLEFTARROW, PRECEDENCE_LONGNAME_LONGLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LONGRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LONGRIGHTARROW, PRECEDENCE_LONGNAME_LONGRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_LONGLEFTRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LONGLEFTRIGHTARROW, PRECEDENCE_LONGNAME_LONGLEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELONGLEFTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLELONGLEFTARROW, PRECEDENCE_LONGNAME_DOUBLELONGLEFTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELONGRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLELONGRIGHTARROW, PRECEDENCE_LONGNAME_DOUBLELONGRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLELONGLEFTRIGHTARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLELONGLEFTRIGHTARROW, PRECEDENCE_LONGNAME_DOUBLELONGLEFTRIGHTARROW)));
    registerInfixParselet(TOKEN_LONGNAME_UPARROWBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPARROWBAR, PRECEDENCE_LONGNAME_UPARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOWNARROWBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOWNARROWBAR, PRECEDENCE_LONGNAME_DOWNARROWBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPDOWNVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTUPDOWNVECTOR, PRECEDENCE_LONGNAME_RIGHTUPDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPDOWNVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTUPDOWNVECTOR, PRECEDENCE_LONGNAME_LEFTUPDOWNVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTUPVECTORBAR, PRECEDENCE_LONGNAME_RIGHTUPVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTDOWNVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTDOWNVECTORBAR, PRECEDENCE_LONGNAME_RIGHTDOWNVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTUPVECTORBAR, PRECEDENCE_LONGNAME_LEFTUPVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOWNVECTORBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTDOWNVECTORBAR, PRECEDENCE_LONGNAME_LEFTDOWNVECTORBAR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTUPTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTUPTEEVECTOR, PRECEDENCE_LONGNAME_RIGHTUPTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTDOWNTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTDOWNTEEVECTOR, PRECEDENCE_LONGNAME_RIGHTDOWNTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTUPTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTUPTEEVECTOR, PRECEDENCE_LONGNAME_LEFTUPTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOWNTEEVECTOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTDOWNTEEVECTOR, PRECEDENCE_LONGNAME_LEFTDOWNTEEVECTOR)));
    registerInfixParselet(TOKEN_LONGNAME_UPEQUILIBRIUM.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UPEQUILIBRIUM, PRECEDENCE_LONGNAME_UPEQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_REVERSEUPEQUILIBRIUM.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_REVERSEUPEQUILIBRIUM, PRECEDENCE_LONGNAME_REVERSEUPEQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTUPARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SHORTUPARROW, PRECEDENCE_LONGNAME_SHORTUPARROW)));
    registerInfixParselet(TOKEN_LONGNAME_SHORTDOWNARROW.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SHORTDOWNARROW, PRECEDENCE_LONGNAME_SHORTDOWNARROW)));
    registerInfixParselet(TOKEN_LONGNAME_CENTERDOT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CENTERDOT, PRECEDENCE_LONGNAME_CENTERDOT)));
    registerInfixParselet(TOKEN_LONGNAME_TILDETILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TILDETILDE, PRECEDENCE_LONGNAME_TILDETILDE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDETILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTTILDETILDE, PRECEDENCE_LONGNAME_NOTTILDETILDE)));
    registerInfixParselet(TOKEN_LONGNAME_EQUIVALENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_EQUIVALENT, PRECEDENCE_LONGNAME_EQUIVALENT)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTRIANGLEEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTTRIANGLEEQUAL, PRECEDENCE_LONGNAME_LEFTTRIANGLEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTRIANGLEEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTTRIANGLEEQUAL, PRECEDENCE_LONGNAME_RIGHTTRIANGLEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLEEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLEEQUAL, PRECEDENCE_LONGNAME_NOTLEFTTRIANGLEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLEEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLEEQUAL, PRECEDENCE_LONGNAME_NOTRIGHTTRIANGLEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_TILDEEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TILDEEQUAL, PRECEDENCE_LONGNAME_TILDEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDEEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTTILDEEQUAL, PRECEDENCE_LONGNAME_NOTTILDEEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_TILDEFULLEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TILDEFULLEQUAL, PRECEDENCE_LONGNAME_TILDEFULLEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDEFULLEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTTILDEFULLEQUAL, PRECEDENCE_LONGNAME_NOTTILDEFULLEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLEDOT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CIRCLEDOT, PRECEDENCE_LONGNAME_CIRCLEDOT)));
    registerInfixParselet(TOKEN_LONGNAME_DISTRIBUTED.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DISTRIBUTED, PRECEDENCE_LONGNAME_DISTRIBUTED)));
    registerInfixParselet(TOKEN_LONGNAME_CONDITIONED.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CONDITIONED, PRECEDENCE_LONGNAME_CONDITIONED)));
    registerInfixParselet(TOKEN_LONGNAME_UNION.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UNION, PRECEDENCE_LONGNAME_UNION)));
    registerInfixParselet(TOKEN_LONGNAME_INTERSECTION.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_INTERSECTION, PRECEDENCE_LONGNAME_INTERSECTION)));
    registerInfixParselet(TOKEN_LONGNAME_TENSORWEDGE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TENSORWEDGE, PRECEDENCE_LONGNAME_TENSORWEDGE)));
    registerInfixParselet(TOKEN_LONGNAME_TENSORPRODUCT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TENSORPRODUCT, PRECEDENCE_LONGNAME_TENSORPRODUCT)));
    registerInfixParselet(TOKEN_LONGNAME_CROSS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CROSS, PRECEDENCE_LONGNAME_CROSS)));
    registerInfixParselet(TOKEN_LONGNAME_PROPORTIONAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PROPORTIONAL, PRECEDENCE_LONGNAME_PROPORTIONAL)));
    registerInfixParselet(TOKEN_LONGNAME_PROPORTION.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PROPORTION, PRECEDENCE_LONGNAME_PROPORTION)));
    registerInfixParselet(TOKEN_LONGNAME_CONGRUENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CONGRUENT, PRECEDENCE_LONGNAME_CONGRUENT)));
    registerInfixParselet(TOKEN_LONGNAME_TILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_TILDE, PRECEDENCE_LONGNAME_TILDE)));
    registerInfixParselet(TOKEN_LONGNAME_SMALLCIRCLE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SMALLCIRCLE, PRECEDENCE_LONGNAME_SMALLCIRCLE)));
    registerInfixParselet(TOKEN_LONGNAME_DIVIDES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DIVIDES, PRECEDENCE_LONGNAME_DIVIDES)));
    registerInfixParselet(TOKEN_LONGNAME_VERTICALSEPARATOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_VERTICALSEPARATOR, PRECEDENCE_LONGNAME_VERTICALSEPARATOR)));
    registerInfixParselet(TOKEN_LONGNAME_BACKSLASH.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_BACKSLASH, PRECEDENCE_LONGNAME_BACKSLASH)));
    registerInfixParselet(TOKEN_LONGNAME_DIAMOND.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DIAMOND, PRECEDENCE_LONGNAME_DIAMOND)));
    registerInfixParselet(TOKEN_LONGNAME_WEDGE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_WEDGE, PRECEDENCE_LONGNAME_WEDGE)));
    registerInfixParselet(TOKEN_LONGNAME_VEE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_VEE, PRECEDENCE_LONGNAME_VEE)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLETIMES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CIRCLETIMES, PRECEDENCE_INFIX_LONGNAME_CIRCLETIMES)));
    registerInfixParselet(TOKEN_LONGNAME_STAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_STAR, PRECEDENCE_LONGNAME_STAR)));
    registerInfixParselet(TOKEN_LONGNAME_VERTICALTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_VERTICALTILDE, PRECEDENCE_LONGNAME_VERTICALTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_COPRODUCT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_COPRODUCT, PRECEDENCE_INFIX_LONGNAME_COPRODUCT)));
    registerInfixParselet(TOKEN_LONGNAME_CAP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CAP, PRECEDENCE_LONGNAME_CAP)));
    registerInfixParselet(TOKEN_LONGNAME_CUP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CUP, PRECEDENCE_LONGNAME_CUP)));
    registerInfixParselet(TOKEN_LONGNAME_CIRCLEPLUS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CIRCLEPLUS, PRECEDENCE_LONGNAME_CIRCLEPLUS)));
    registerInfixParselet(TOKEN_LONGNAME_VERTICALBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_VERTICALBAR, PRECEDENCE_LONGNAME_VERTICALBAR)));
    registerInfixParselet(TOKEN_LONGNAME_DOUBLEVERTICALBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOUBLEVERTICALBAR, PRECEDENCE_LONGNAME_DOUBLEVERTICALBAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTVERTICALBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTVERTICALBAR, PRECEDENCE_LONGNAME_NOTVERTICALBAR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTDOUBLEVERTICALBAR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTDOUBLEVERTICALBAR, PRECEDENCE_LONGNAME_NOTDOUBLEVERTICALBAR)));
    registerInfixParselet(TOKEN_LONGNAME_LEFTTRIANGLE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_LEFTTRIANGLE, PRECEDENCE_LONGNAME_LEFTTRIANGLE)));
    registerInfixParselet(TOKEN_LONGNAME_RIGHTTRIANGLE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_RIGHTTRIANGLE, PRECEDENCE_LONGNAME_RIGHTTRIANGLE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTLEFTTRIANGLE, PRECEDENCE_LONGNAME_NOTLEFTTRIANGLE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTRIGHTTRIANGLE, PRECEDENCE_LONGNAME_NOTRIGHTTRIANGLE)));
    registerInfixParselet(TOKEN_LONGNAME_PERMUTATIONPRODUCT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PERMUTATIONPRODUCT, PRECEDENCE_LONGNAME_PERMUTATIONPRODUCT)));
    registerInfixParselet(TOKEN_LONGNAME_EQUILIBRIUM.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_EQUILIBRIUM, PRECEDENCE_LONGNAME_EQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_REVERSEEQUILIBRIUM.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_REVERSEEQUILIBRIUM, PRECEDENCE_LONGNAME_REVERSEEQUILIBRIUM)));
    registerInfixParselet(TOKEN_LONGNAME_REVERSEELEMENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_REVERSEELEMENT, PRECEDENCE_LONGNAME_REVERSEELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_NOTREVERSEELEMENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTREVERSEELEMENT, PRECEDENCE_LONGNAME_NOTREVERSEELEMENT)));
    registerInfixParselet(TOKEN_LONGNAME_NOTTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTTILDE, PRECEDENCE_LONGNAME_NOTTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_EQUALTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_EQUALTILDE, PRECEDENCE_LONGNAME_EQUALTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_COLON.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_COLON, PRECEDENCE_LONGNAME_COLON)));
    registerInfixParselet(TOKEN_LONGNAME_CUPCAP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CUPCAP, PRECEDENCE_LONGNAME_CUPCAP)));
    registerInfixParselet(TOKEN_LONGNAME_NOTCUPCAP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTCUPCAP, PRECEDENCE_LONGNAME_NOTCUPCAP)));
    registerInfixParselet(TOKEN_LONGNAME_DOTEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DOTEQUAL, PRECEDENCE_LONGNAME_DOTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_HUMPEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_HUMPEQUAL, PRECEDENCE_LONGNAME_HUMPEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_HUMPDOWNHUMP.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_HUMPDOWNHUMP, PRECEDENCE_LONGNAME_HUMPDOWNHUMP)));
    registerInfixParselet(TOKEN_LONGNAME_NOTCONGRUENT.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTCONGRUENT, PRECEDENCE_LONGNAME_NOTCONGRUENT)));
    registerInfixParselet(TOKEN_LONGNAME_PRECEDES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PRECEDES, PRECEDENCE_LONGNAME_PRECEDES)));
    registerInfixParselet(TOKEN_LONGNAME_SUCCEEDS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUCCEEDS, PRECEDENCE_LONGNAME_SUCCEEDS)));
    registerInfixParselet(TOKEN_LONGNAME_PRECEDESEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PRECEDESEQUAL, PRECEDENCE_LONGNAME_PRECEDESEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SUCCEEDSEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUCCEEDSEQUAL, PRECEDENCE_LONGNAME_SUCCEEDSEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_PRECEDESTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PRECEDESTILDE, PRECEDENCE_LONGNAME_PRECEDESTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_SUCCEEDSTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUCCEEDSTILDE, PRECEDENCE_LONGNAME_SUCCEEDSTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_PRECEDESSLANTEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_PRECEDESSLANTEQUAL, PRECEDENCE_LONGNAME_PRECEDESSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SUCCEEDSSLANTEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SUCCEEDSSLANTEQUAL, PRECEDENCE_LONGNAME_SUCCEEDSSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTPRECEDES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTPRECEDES, PRECEDENCE_LONGNAME_NOTPRECEDES)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUCCEEDS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUCCEEDS, PRECEDENCE_LONGNAME_NOTSUCCEEDS)));
    registerInfixParselet(TOKEN_LONGNAME_NOTPRECEDESEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTPRECEDESEQUAL, PRECEDENCE_LONGNAME_NOTPRECEDESEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUCCEEDSEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUCCEEDSEQUAL, PRECEDENCE_LONGNAME_NOTSUCCEEDSEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTPRECEDESTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTPRECEDESTILDE, PRECEDENCE_LONGNAME_NOTPRECEDESTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUCCEEDSTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUCCEEDSTILDE, PRECEDENCE_LONGNAME_NOTSUCCEEDSTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_NOTPRECEDESSLANTEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTPRECEDESSLANTEQUAL, PRECEDENCE_LONGNAME_NOTPRECEDESSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_NOTSUCCEEDSSLANTEQUAL.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTSUCCEEDSSLANTEQUAL, PRECEDENCE_LONGNAME_NOTSUCCEEDSSLANTEQUAL)));
    registerInfixParselet(TOKEN_LONGNAME_SQUAREUNION.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SQUAREUNION, PRECEDENCE_LONGNAME_SQUAREUNION)));
    registerInfixParselet(TOKEN_LONGNAME_SQUAREINTERSECTION.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_SQUAREINTERSECTION, PRECEDENCE_LONGNAME_SQUAREINTERSECTION)));
    registerInfixParselet(TOKEN_LONGNAME_XNOR.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_XNOR, PRECEDENCE_LONGNAME_XNOR)));
    registerInfixParselet(TOKEN_LONGNAME_NOTEQUALTILDE.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_NOTEQUALTILDE, PRECEDENCE_LONGNAME_NOTEQUALTILDE)));
    registerInfixParselet(TOKEN_LONGNAME_UNIONPLUS.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_UNIONPLUS, PRECEDENCE_LONGNAME_UNIONPLUS)));
    registerInfixParselet(TOKEN_FAKE_IMPLICITTIMES.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_FAKE_IMPLICITTIMES, PRECEDENCE_STAR)));
    
    
    //
    // Postfix
    //
    registerInfixParselet(TOKEN_AMP.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_AMP, PRECEDENCE_AMP)));
    registerInfixParselet(TOKEN_DOTDOT.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_DOTDOT, PRECEDENCE_DOTDOT)));
    registerInfixParselet(TOKEN_BANG.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_BANG, PRECEDENCE_POSTFIX_BANG)));
    registerInfixParselet(TOKEN_MINUSMINUS.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_MINUSMINUS, PRECEDENCE_POSTFIX_MINUSMINUS)));
    registerInfixParselet(TOKEN_PLUSPLUS.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_PLUSPLUS, PRECEDENCE_POSTFIX_PLUSPLUS)));
    registerInfixParselet(TOKEN_DOTDOTDOT.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_DOTDOTDOT, PRECEDENCE_DOTDOTDOT)));
    registerInfixParselet(TOKEN_BANGBANG.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_BANGBANG, PRECEDENCE_POSTFIX_BANGBANG)));
    registerInfixParselet(TOKEN_SINGLEQUOTE.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_SINGLEQUOTE, PRECEDENCE_SINGLEQUOTE)));
    registerInfixParselet(TOKEN_LONGNAME_TRANSPOSE.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_LONGNAME_TRANSPOSE, PRECEDENCE_LONGNAME_TRANSPOSE)));
    registerInfixParselet(TOKEN_LONGNAME_CONJUGATE.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_LONGNAME_CONJUGATE, PRECEDENCE_LONGNAME_CONJUGATE)));
    registerInfixParselet(TOKEN_LONGNAME_CONJUGATETRANSPOSE.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_LONGNAME_CONJUGATETRANSPOSE, PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE)));
    registerInfixParselet(TOKEN_LONGNAME_HERMITIANCONJUGATE.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_LONGNAME_HERMITIANCONJUGATE, PRECEDENCE_LONGNAME_HERMITIANCONJUGATE)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE.value(), InfixParseletPtr(new PostfixOperatorParselet(TOKEN_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, PRECEDENCE_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE)));
    
    
    //
    // Calls
    //
    registerInfixParselet(TOKEN_OPENSQUARE.value(), InfixParseletPtr(new CallParselet(PrefixParseletPtr(new GroupParselet(TOKEN_OPENSQUARE)))));
    registerInfixParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET.value(), InfixParseletPtr(new CallParselet(PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET)))));
    
    
    //
    // Groups
    //
    registerPrefixParselet(TOKEN_OPENPAREN.value(), PrefixParseletPtr(new GroupParselet(TOKEN_OPENPAREN)));
    registerPrefixParselet(TOKEN_OPENSQUARE.value(), PrefixParseletPtr(new GroupParselet(TOKEN_OPENSQUARE)));
    registerPrefixParselet(TOKEN_OPENCURLY.value(), PrefixParseletPtr(new GroupParselet(TOKEN_OPENCURLY)));
    registerPrefixParselet(TOKEN_LESSBAR.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LESSBAR)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTANGLEBRACKET.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTANGLEBRACKET)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTCEILING.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTCEILING)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTFLOOR.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTFLOOR)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKET)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTBRACKETINGBAR.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTBRACKETINGBAR)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR)));
    registerPrefixParselet(TOKEN_LONGNAME_LEFTASSOCIATION.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_LEFTASSOCIATION)));
    registerPrefixParselet(TOKEN_LONGNAME_OPENCURLYQUOTE.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_OPENCURLYQUOTE)));
    registerPrefixParselet(TOKEN_LONGNAME_OPENCURLYDOUBLEQUOTE.value(), PrefixParseletPtr(new GroupParselet(TOKEN_LONGNAME_OPENCURLYDOUBLEQUOTE)));
    
#if STARTOFLINE
    //
    // StartOfLine
    //
    registerStartOfLineParselet(TOKEN_QUESTION, StartOfLineParseletPtr(new StartOfLineParselet()));
    registerStartOfLineParselet(TOKEN_QUESTIONQUESTION, StartOfLineParseletPtr(new StartOfLineParselet()));
    //
    // TODO: uncomment when there is support for different modes
    //
    //    registerStartOfLineParselet(TOKEN_BANG, StartOfLineParseletPtr(new StartOfLineParselet()));
    //    registerStartOfLineParselet(TOKEN_BANGBANG, StartOfLineParseletPtr(new StartOfLineParselet()));
    
    //
    // StartOfFile
    //
    registerStartOfFileParselet(TOKEN_HASHBANG, StartOfFileParseletPtr(new StartOfFileParselet()));
#endif // STARTOFLINE
    
    //
    // Special
    //
    
    // context sensitive parsing of  x_
    registerPrefixParselet(TOKEN_SYMBOL.value(), PrefixParseletPtr(new SymbolParselet()));
    
    // context sensitive parsing of _x
    registerPrefixParselet(TOKEN_UNDER.value(), PrefixParseletPtr(new UnderParselet()));
    registerPrefixParselet(TOKEN_UNDERUNDER.value(), PrefixParseletPtr(new UnderParselet()));
    registerPrefixParselet(TOKEN_UNDERUNDERUNDER.value(), PrefixParseletPtr(new UnderParselet()));
    
    // trailing ; and , is allowed
    registerInfixParselet(TOKEN_SEMI.value(), InfixParseletPtr(new InfixOperatorWithTrailingParselet(TOKEN_SEMI, PRECEDENCE_SEMI)));
    registerInfixParselet(TOKEN_COMMA.value(), InfixParseletPtr(new InfixOperatorWithTrailingParselet(TOKEN_COMMA, PRECEDENCE_COMMA)));
    registerInfixParselet(TOKEN_LONGNAME_INVISIBLECOMMA.value(), InfixParseletPtr(new InfixOperatorWithTrailingParselet(TOKEN_LONGNAME_INVISIBLECOMMA, PRECEDENCE_LONGNAME_INVISIBLECOMMA)));
    
    // prefix, infix, postfix
    registerPrefixParselet(TOKEN_SEMISEMI.value(), PrefixParseletPtr(new SemiSemiParselet()));
    registerInfixParselet(TOKEN_SEMISEMI.value(), InfixParseletPtr(new SemiSemiParselet()));
    
    // ternary
    registerInfixParselet(TOKEN_TILDE.value(), InfixParseletPtr(new TildeParselet()));
    
    // context sensitive parsing of sym:obj and pat:v
    registerInfixParselet(TOKEN_COLON.value(), InfixParseletPtr(new ColonParselet()));
    
    // ternary, with different possibilities for second operator
    registerInfixParselet(TOKEN_SLASHCOLON.value(), InfixParseletPtr(new SlashColonParselet()));
    
    // FIXME: punt on parsing box syntax, reads tokens with no parsing
    registerPrefixParselet(TOKEN_LINEARSYNTAX_OPENPAREN.value(), PrefixParseletPtr(new LinearSyntaxOpenParenParselet()));
    
    // Has to handle  a =.  and  a = .
    registerInfixParselet(TOKEN_EQUAL.value(), InfixParseletPtr(new EqualParselet(TOKEN_EQUAL)));
    registerInfixParselet(TOKEN_EQUALDOT.value(), InfixParseletPtr(new EqualParselet(TOKEN_EQUALDOT)));
    
    // Has to handle \[Integral] f \[DifferentialD] x
    registerPrefixParselet(TOKEN_LONGNAME_INTEGRAL.value(), PrefixParseletPtr(new IntegralParselet(TOKEN_LONGNAME_INTEGRAL)));
    //
    // Register \[DifferentialD] as infix, to return precedence of LOWEST
    // This is to prevent  \[Integral] f \[DifferentialD] x  from being parsed as  \[Integral] f * \[DifferentialD] x
    //
    registerInfixParselet(TOKEN_LONGNAME_DIFFERENTIALD.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_DIFFERENTIALD, PRECEDENCE_LOWEST)));
    registerInfixParselet(TOKEN_LONGNAME_CAPITALDIFFERENTIALD.value(), InfixParseletPtr(new InfixOperatorParselet(TOKEN_LONGNAME_CAPITALDIFFERENTIALD, PRECEDENCE_LOWEST)));
    
    // special Inequality
    registerInfixParselet(TOKEN_BANGEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_EQUALEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_GREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_GREATEREQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LESSEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LESS.value(), InfixParseletPtr(new InequalityParselet()));
    
    registerInfixParselet(TOKEN_LONGNAME_EQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATEREQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATEREQUALLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATERFULLEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATERGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATERLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATERSLANTEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_GREATERTILDE.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSEQUALGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSFULLEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSSLANTEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LESSTILDE.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_LONGEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NESTEDGREATERGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NESTEDLESSLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATEREQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERFULLEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERSLANTEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTGREATERTILDE.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSFULLEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSLESS.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSSLANTEQUAL.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTLESSTILDE.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTNESTEDGREATERGREATER.value(), InfixParseletPtr(new InequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_NOTNESTEDLESSLESS.value(), InfixParseletPtr(new InequalityParselet()));
    
    // special VectorInequality
    registerInfixParselet(TOKEN_LONGNAME_VECTORGREATER.value(), InfixParseletPtr(new VectorInequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_VECTORGREATEREQUAL.value(), InfixParseletPtr(new VectorInequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_VECTORLESS.value(), InfixParseletPtr(new VectorInequalityParselet()));
    registerInfixParselet(TOKEN_LONGNAME_VECTORLESSEQUAL.value(), InfixParseletPtr(new VectorInequalityParselet()));
    
    // stringify next token (as a symbol)
    registerInfixParselet(TOKEN_COLONCOLON.value(), InfixParseletPtr(new ColonColonParselet()));
    
    // stringify next token (as a file)
    registerInfixParselet(TOKEN_GREATERGREATER.value(), InfixParseletPtr(new GreaterGreaterParselet()));
    registerInfixParselet(TOKEN_GREATERGREATERGREATER.value(), InfixParseletPtr(new GreaterGreaterGreaterParselet()));
    registerPrefixParselet(TOKEN_LESSLESS.value(), PrefixParseletPtr(new LessLessParselet()));
    
    
    //
    // Literals and Unhandled
    //
    for (size_t i = 0; i < prefixParselets.size(); i++) {
        auto& P = prefixParselets[i];
        if (P != nullptr) {
            continue;
        }
        
        //
        // Fill in the gaps
        //
        registerPrefixParselet(i, PrefixParseletPtr(new LeafParselet()));
    }
    
//    for (size_t i = 0; i < infixParselets.size(); i++) {
//        auto& I = infixParselets[i];
//        if (I != nullptr) {
//            continue;
//        }
//        
//        //
//        // Fill in the gaps
//        //
//        infixParselets(i, InfixParseletPtr(new DummyParselet()));
//    }
}

Parser::~Parser() {}

void Parser::init() {
    
    Issues.clear();
}

void Parser::deinit() {
    
    tokenQueue.clear();
    Issues.clear();
}

inline void Parser::registerPrefixParselet(size_t i, PrefixParseletPtr P) {
    
    assert(prefixParselets[i] == nullptr);
    
    prefixParselets[i] = std::move(P);
}

inline void Parser::registerInfixParselet(size_t i, InfixParseletPtr P) {
    
    assert(infixParselets[i] == nullptr);
    
    infixParselets[i] = std::move(P);
}


#if STARTOFLINE

void Parser::registerStartOfLineParselet(size_t i, StartOfLineParseletPtr P) {
    
    assert(startOfLineParselets[i] == nullptr);
    
    startOfLineParselets[i] = std::move(P);
}

void Parser::registerStartOfFileParselet(size_t i, StartOfFileParseletPtr P) {

    assert(startOfFileParselets[i] == nullptr);

    startOfFileParselets[i] = std::move(P);
}

#endif // STARTOFLINE


const ContextSensitivePrefixParseletPtr& Parser::getContextSensitiveSymbolParselet() const {
    return contextSensitiveSymbolParselet;
}

const ContextSensitiveInfixParseletPtr& Parser::getContextSensitiveUnderParselet() const {
    return contextSensitiveUnderParselet;
}

const ContextSensitiveInfixParseletPtr& Parser::getContextSensitiveColonParselet() const {
    return contextSensitiveColonParselet;
}

void Parser::nextToken() {
    
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
    
    TheTokenizer->nextToken(TOPLEVEL);
}


#if STARTOFLINE

void Parser::nextToken_stringifyLine() {
    
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
    
    TheTokenizer->nextToken_stringifyLine();
}

#endif // STARTOFLINE


void Parser::nextToken_stringifySymbol() {
    
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
    
    TheTokenizer->nextToken_stringifySymbol();
}

void Parser::nextToken_stringifyFile() {
    
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
    
    TheTokenizer->nextToken_stringifyFile();
}

Token Parser::nextToken0() {
    
    //
    // handle the queue before anything else
    //
    // We do not know anything about how many Tokens should be read
    //
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return Tok;
    }
    
    return TheTokenizer->nextToken0(TOPLEVEL);
}

Token Parser::currentToken() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken(TOPLEVEL);
}


#if STARTOFLINE

Token Parser::currentToken_stringifyLine() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken_stringifyLine();
}

#endif // STARTOFLINE


Token Parser::currentToken_stringifySymbol() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken_stringifySymbol();
}

Token Parser::currentToken_stringifyFile() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken_stringifyFile();
}

void Parser::prependInReverse(std::vector<LeafNodePtr>& V) {
    
    if (V.empty()) {
        return;
    }
    
    auto i = V.rbegin();
    for (; i != V.rend(); ++i ) {
        
        auto& T = (*i)->getToken();
        
        tokenQueue.insert(tokenQueue.begin(), T);
    }
}

#if !NISSUES
std::vector<IssuePtr>& Parser::getIssues() {
    return Issues;
}

//
// Only to be used by Parselets
//
void Parser::addIssue(IssuePtr I) {
    Issues.push_back(std::move(I));
}
#endif // !NISSUES

const PrefixParseletPtr& Parser::findPrefixParselet(TokenEnum T) const {
    auto& P = prefixParselets[T.value()];
    assert(P != nullptr);
    return P;
}

const InfixParseletPtr& Parser::findInfixParselet(TokenEnum T) const {
    auto& I = infixParselets[T.value()];
    assert(I != nullptr);
    return I;
}


Precedence Parser::getTokenPrecedence(Token& TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok != TOKEN_UNKNOWN);
    assert(TokIn.Tok != TOKEN_WHITESPACE);
    // allow top-level newlines
    assert(TokIn.Tok != TOKEN_NEWLINE || Ctxt.getGroupDepth() == 0);
    assert(TokIn.Tok != TOKEN_COMMENT);
    assert(TokIn.Tok != TOKEN_LINECONTINUATION);
    
    if (TokIn.Tok.isError()) {
        
        return PRECEDENCE_LOWEST;
    }
    
    if (TokIn.Tok == TOKEN_ENDOFFILE) {
        
        return PRECEDENCE_LOWEST;
    }
    
    //
    // TODO: review when closers have their own parselets
    //
    if (TokIn.Tok.isCloser()) {
        
        return PRECEDENCE_LOWEST;
    }
        
    auto& P = prefixParselets[TokIn.Tok.value()];
    
    //
    // There is an ambiguity with tokens that are both prefix and infix, e.g.
    // +  -  ;;  !  ++  --
    //
    // Given the input  ;;;;
    // when parsing the second  ;;  , we could get here because ;; is registered as infix
    // But this particular ;; is a new expression, it is not actually infix
    //
    // Given the input  1+2
    // when parsing the +, make sure to treat it as infix and NOT prefix
    //
    // Solution is to handle infix parselets where needed, i.e., SemiSemiParselet
    //
    
    return P->getPrecedence();
}

Precedence Parser::getInfixTokenPrecedence(Token& TokIn, ParserContext Ctxt, bool *implicitTimes) const {
    
    assert(TokIn.Tok != TOKEN_UNKNOWN);
    assert(TokIn.Tok != TOKEN_WHITESPACE);
    // allow top-level newlines
    assert(TokIn.Tok != TOKEN_NEWLINE || Ctxt.getGroupDepth() == 0);
    assert(TokIn.Tok != TOKEN_COMMENT);
    assert(TokIn.Tok != TOKEN_LINECONTINUATION);
    
    if (TokIn.Tok.isError()) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    if (TokIn.Tok == TOKEN_ENDOFFILE) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    //
    // TODO: review when closers have their own parselets
    //
    if (TokIn.Tok.isCloser()) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    auto& Infix = infixParselets[TokIn.Tok.value()];
    
    if (Infix != nullptr) {
        
        *implicitTimes = false;
        
        return Infix->getPrecedence();
    }
    
    //
    // Literals or unhandled
    //
    
    //
    // Do not do Implicit Times across lines
    //
    if (TokIn.Tok == TOKEN_NEWLINE && Ctxt.getGroupDepth() == 0) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    *implicitTimes = true;
    
    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

NodePtr Parser::parse(Token firstTok, ParserContext CtxtIn) {

#if !NABORT
    if (TheParserSession->isAbort()) {
        
        return TheParserSession->handleAbort();
    }
#endif // !NABORT
    
    auto Ctxt = CtxtIn;
    
    auto token = firstTok;
    
    assert(token.Tok != TOKEN_UNKNOWN);
    assert(!token.Tok.isTrivia() && "Must handle at the call site");
    assert(token.Tok != TOKEN_ENDOFFILE && "Must handle at the call site");
    assert(token.Tok.isPossibleBeginningOfExpression() && "Must handle at the call site");
    
    //
    // Prefix start
    //
    
    auto& P = findPrefixParselet(token.Tok);
    
    auto Left = P->parse(token, Ctxt);
    
    
    //
    // Infix loop
    //
    
    while (true) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest;
        
        auto token = currentToken();
        token = Parser::eatAndPreserveToplevelNewlines(token, Ctxt, ArgsTest);
        
        bool implicitTimes;
        
        auto TokenPrecedence = getInfixTokenPrecedence(token, Ctxt, &implicitTimes);
        
        if (implicitTimes) {
            
            token = Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(token.BufLen.buffer), Source(token.Src.Start));
            
            tokenQueue.insert(tokenQueue.begin(), token);
        }
        
        if (Ctxt.Prec > TokenPrecedence) {
            break;
        }
        if (Ctxt.Prec == TokenPrecedence) {            
            auto& I = infixParselets[token.Tok.value()];
            if (I == nullptr) {
                break;
            }
            auto TokenAssociativity = I->getAssociativity();
            if (TokenAssociativity != ASSOCIATIVITY_RIGHT) {
                break;
            }
        }
        
        NodeSeq LeftSeq;
        LeftSeq.reserve(1 + 1);
        LeftSeq.append(std::move(Left));
        LeftSeq.appendIfNonEmpty(std::move(ArgsTest));
    
        auto Ctxt = CtxtIn;
        Ctxt.Prec = TokenPrecedence;
    
        auto& I = findInfixParselet(token.Tok);
    
        Left = I->parse(std::move(LeftSeq), token, Ctxt);
        
    } // while
    
    return Left;
}

NodePtr Parser::handleNotPossible(Token& tokenBad, Token& tokenAnchor, ParserContext CtxtIn, bool *wasCloser) {
    
    //
    // It is possible that possibleBeginningOfExpression could get here
    //
    // For example: \[Integral]!b
    // !b is possible beginning of expression, but ! has lower precedence than \[Integral],
    // so
    //
    //

    if (tokenBad.Tok.isPossibleBeginningOfExpression()) {

        auto operand = parse(tokenBad, CtxtIn);

        if (wasCloser != nullptr) {
            *wasCloser = false;
        }

        return operand;
    }
    
    auto& I = infixParselets[tokenBad.Tok.value()];
    if (I != nullptr) {
        
        //
        // Handle something like  f[,1]
        //
        // We want to make EXPECTEDOPERAND the first arg of the Comma node.
        //
        // Do not take next token
        //
        // Important to not duplicate token's Str here, it may also appear later
        //
        // Also, invent Source
        //
        
        auto NotPossible = NodePtr(new LeafNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.buffer), Source(tokenAnchor.Src.Start))));
        
        NodeSeq LeftSeq;
        LeftSeq.reserve(1);
        LeftSeq.append(std::move(NotPossible));
        
        auto Ctxt = CtxtIn;
        //
        // FIXME: clear other flags here also?
        //
        Ctxt.Flag &= ~(PARSER_COLON);
        
        if (wasCloser != nullptr) {
            *wasCloser = false;
        }
        
        return I->parse(std::move(LeftSeq), tokenBad, Ctxt);
    }
    
    if (tokenBad.Tok == CtxtIn.Closer) {
        //
        // Handle the special cases of:
        // { + }
        // { a + }
        // { a @ }
        // We are here parsing the operators, but we don't want to descend and treat the } as the problem
        //

        //
        // Do not take next token
        //
        
        auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.end()), Source(tokenAnchor.Src.End));

        if (wasCloser != nullptr) {
            *wasCloser = true;
        }
        
        return NodePtr(new LeafNode(createdToken));
    }
    
    if (tokenBad.Tok.isCloser()) {
        //
        // Handle  { a ) }
        // which ends up being  MissingCloser[ { a ]  EXPECTEOPERAND
        //
        
        nextToken();
        
        NodeSeq Args;
        Args.reserve(1);
        Args.append(NodePtr(new LeafNode(tokenBad)));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_UNEXPECTEDCLOSER, std::move(Args)));
        
        if (wasCloser != nullptr) {
            *wasCloser = true;
        }
        
        return Error;
    }
    
    if (tokenBad.Tok == TOKEN_ENDOFFILE) {
        
        auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.end()), Source(tokenAnchor.Src.End));
        
        if (wasCloser != nullptr) {
            *wasCloser = true;
        }
        
        return NodePtr(new LeafNode(createdToken));
    }
    
    assert(tokenBad.Tok.isError());
        
    nextToken();
    
    //
    // If there is a Token error, then use that specific error
    //
    
    if (wasCloser != nullptr) {
        *wasCloser = false;
    }
    
    return NodePtr(new LeafNode(tokenBad));
}

Token Parser::eatAll(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken();
        
        T = currentToken();
    }
    
    return T;
}

Token Parser::eatAll_stringifyFile(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken();
        
        T = currentToken_stringifyFile();
    }
    
    return T;
}

Token Parser::eatAndPreserveToplevelNewlines(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        switch (T.Tok.value()) {
            case TOKEN_NEWLINE.value(): {
                
                if (Ctxt.getGroupDepth() == 0) {
                    
                    return T;
                }
            }
            //
            // Fall through
            //
            case TOKEN_WHITESPACE.value():
            case TOKEN_COMMENT.value():
            case TOKEN_LINECONTINUATION.value(): {
                
                Args.append(LeafNodePtr(new LeafNode(std::move(T))));
                
                nextToken();
                
                T = currentToken();
            }
                break;
            default:
                return T;
        }
    }
}

Token Parser::eatAndPreserveToplevelNewlines_stringifyFile(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        switch (T.Tok.value()) {
            case TOKEN_NEWLINE.value(): {
                
                if (Ctxt.getGroupDepth() == 0) {
                    
                    return T;
                }
            }
            //
            // Fall through
            //
            case TOKEN_WHITESPACE.value():
            case TOKEN_COMMENT.value():
            case TOKEN_LINECONTINUATION.value(): {
                
                Args.append(LeafNodePtr(new LeafNode(std::move(T))));
                
                nextToken();
                
                T = currentToken_stringifyFile();
            }
                break;
            default:
                return T;
        }
    }
}

ParserPtr TheParser = nullptr;
