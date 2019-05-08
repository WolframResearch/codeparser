
#include "Parser.h"

#include "Parselet.h"
#include "Tokenizer.h"

#include <cassert>
#include <iostream>


Parser::Parser() : prefixParselets(), infixParselets(), callParselets(), postfixParselets(), contextSensitiveParselets(), startOfLineParselets(), parselets(),
    tokenQueue(), Issues(), Comments(), currentAbortQ(nullptr) {
        
    //
    // Literals
    //

    // context sensitive parsing of  _x
    registerTokenType(TOKEN_SYMBOL, new SymbolParselet());
    registerTokenType(TOKEN_INTEGER, new IntegerParselet());
    registerTokenType(TOKEN_REAL, new RealParselet());
    registerTokenType(TOKEN_STRING, new StringParselet());
    registerTokenType(TOKEN_HASH, new HashParselet());
    registerTokenType(TOKEN_HASHHASH, new HashHashParselet());
    registerTokenType(TOKEN_PERCENT, new PercentParselet());

    // literal and postfix
    registerTokenType(TOKEN_UNDERDOT, new UnderDotParselet());

    
    
    //
    // Prefix
    //
    registerTokenType(TOKEN_MINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUS));
    registerTokenType(TOKEN_PLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUS));
    registerTokenType(TOKEN_BANG, new PrefixOperatorParselet(PRECEDENCE_PREFIX_BANG));
    registerTokenType(TOKEN_PLUSPLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUSPLUS));
    registerTokenType(TOKEN_MINUSMINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUSMINUS));
    registerTokenType(TOKEN_LESSLESS, new PrefixOperatorParselet(PRECEDENCE_LESSLESS));
    
    registerTokenType(TOKEN_LONGNAME_PLUSMINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS));
    registerTokenType(TOKEN_LONGNAME_SUM, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SUM));
    registerTokenType(TOKEN_LONGNAME_NOT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_NOT));
    registerTokenType(TOKEN_LONGNAME_SQRT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQRT));
    registerTokenType(TOKEN_LONGNAME_MINUSPLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS));
    registerTokenType(TOKEN_LONGNAME_DIFFERENTIALD, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DIFFERENTIALD));
    registerTokenType(TOKEN_LONGNAME_MINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUS));
    registerTokenType(TOKEN_LONGNAME_DEL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DEL));
    registerTokenType(TOKEN_LONGNAME_SQUARE, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQUARE));
        
    registerTokenType(TOKEN_LONGNAME_CONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CONTOURINTEGRAL));
    registerTokenType(TOKEN_LONGNAME_DOUBLECONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLECONTOURINTEGRAL));
    registerTokenType(TOKEN_LONGNAME_CLOCKWISECONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CLOCKWISECONTOURINTEGRAL));
    registerTokenType(TOKEN_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL));
    registerTokenType(TOKEN_LONGNAME_PRODUCT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_PRODUCT));
    registerTokenType(TOKEN_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLEPREFIXSCRIPTBASE));
    registerTokenType(TOKEN_LONGNAME_CIRCLETIMES, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_CIRCLETIMES));

    registerTokenType(TOKEN_LINEARSYNTAX_BANG, new PrefixOperatorParselet(PRECEDENCE_LINEARSYNTAX_BANG));
    

    //
    // Binary
    //

    // inequality operators
    registerTokenType(TOKEN_EQUALEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_BANGEQUAL, new BinaryOperatorParselet(PRECEDENCE_BANGEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LESS, new BinaryOperatorParselet(PRECEDENCE_LESS, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_GREATER, new BinaryOperatorParselet(PRECEDENCE_GREATER, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LESSEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_GREATEREQUAL, new BinaryOperatorParselet(PRECEDENCE_GREATEREQUAL, ASSOCIATIVITY_LEFT));

    // other flattening operators
    registerTokenType(TOKEN_EQUALEQUALEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALEQUALEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_EQUALBANGEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALBANGEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_ATSTAR, new BinaryOperatorParselet(PRECEDENCE_ATSTAR, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_SLASHSTAR, new BinaryOperatorParselet(PRECEDENCE_SLASHSTAR, ASSOCIATIVITY_LEFT));

    registerTokenType(TOKEN_MINUS, new BinaryOperatorParselet(PRECEDENCE_BINARY_MINUS, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_SLASH, new BinaryOperatorParselet(PRECEDENCE_SLASH, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_CARET, new BinaryOperatorParselet(PRECEDENCE_CARET, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_CARETEQUAL, new BinaryOperatorParselet(PRECEDENCE_CARETEQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_CARETCOLONEQUAL, new BinaryOperatorParselet(PRECEDENCE_CARETCOLONEQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_SLASHAT, new BinaryOperatorParselet(PRECEDENCE_SLASHAT, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_MINUSGREATER, new BinaryOperatorParselet(PRECEDENCE_MINUSGREATER, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_ATAT, new BinaryOperatorParselet(PRECEDENCE_ATAT, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_SLASHSEMI, new BinaryOperatorParselet(PRECEDENCE_SLASHSEMI, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_SLASHDOT, new BinaryOperatorParselet(PRECEDENCE_SLASHDOT, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_COLONGREATER, new BinaryOperatorParselet(PRECEDENCE_COLONGREATER, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_SLASHSLASHDOT, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHDOT, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_PLUSEQUAL, new BinaryOperatorParselet(PRECEDENCE_PLUSEQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_STAREQUAL, new BinaryOperatorParselet(PRECEDENCE_STAREQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_MINUSEQUAL, new BinaryOperatorParselet(PRECEDENCE_MINUSEQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_SLASHEQUAL, new BinaryOperatorParselet(PRECEDENCE_SLASHEQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_LESSMINUSGREATER, new BinaryOperatorParselet(PRECEDENCE_LESSMINUSGREATER, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_SLASHSLASHAT, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHAT, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_AT, new BinaryOperatorParselet(PRECEDENCE_AT, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_ATATAT, new BinaryOperatorParselet(PRECEDENCE_ATATAT, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_SLASHSLASH, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASH, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_COLONEQUAL, new BinaryOperatorParselet(PRECEDENCE_COLONEQUAL, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_GREATERGREATER, new BinaryOperatorParselet(PRECEDENCE_GREATERGREATER, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_QUESTION, new BinaryOperatorParselet(PRECEDENCE_INFIX_QUESTION, ASSOCIATIVITY_NONASSOCIATIVE));
    registerTokenType(TOKEN_GREATERGREATERGREATER, new BinaryOperatorParselet(PRECEDENCE_GREATERGREATERGREATER, ASSOCIATIVITY_LEFT));
    
    registerTokenType(TOKEN_LONGNAME_EQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_EQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_LESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LESSEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_GREATEREQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_GREATEREQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_NOTEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTEQUAL, ASSOCIATIVITY_LEFT));
        
    // set relation operators
    registerTokenType(TOKEN_LONGNAME_ELEMENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_ELEMENT, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_SUBSET, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUBSET, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_SUPERSET, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUPERSET, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_SUBSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUBSETEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_SUPERSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUPERSETEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_NOTELEMENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTELEMENT, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_NOTSUBSET, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTSUBSET, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_NOTSUPERSET, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTSUPERSET, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_NOTSUBSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTSUBSETEQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_NOTSUPERSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTSUPERSETEQUAL, ASSOCIATIVITY_LEFT));

    registerTokenType(TOKEN_LONGNAME_IMPLIES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_IMPLIES, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_PLUSMINUS, new BinaryOperatorParselet(PRECEDENCE_INFIX_LONGNAME_PLUSMINUS, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_DIRECTEDEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIRECTEDEDGE, ASSOCIATIVITY_NONASSOCIATIVE));
    registerTokenType(TOKEN_LONGNAME_RULE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULE, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_LONGNAME_RULEDELAYED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULEDELAYED, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_LONGNAME_UNDIRECTEDEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, ASSOCIATIVITY_NONASSOCIATIVE));
    registerTokenType(TOKEN_LONGNAME_FUNCTION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_FUNCTION, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_LONGNAME_MINUSPLUS, new BinaryOperatorParselet(PRECEDENCE_INFIX_LONGNAME_MINUSPLUS, ASSOCIATIVITY_LEFT));
//    if (targetVersionNumber >= 1110) {
    registerTokenType(TOKEN_LONGNAME_TWOWAYRULE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TWOWAYRULE, ASSOCIATIVITY_RIGHT));
//    }
    registerTokenType(TOKEN_LONGNAME_INVISIBLEAPPLICATION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, ASSOCIATIVITY_RIGHT));
    registerTokenType(TOKEN_LONGNAME_CIRCLEMINUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEMINUS, ASSOCIATIVITY_LEFT));
//    if (targetVersionNumber >= 1200) {
    registerTokenType(TOKEN_LONGNAME_VECTORGREATER, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VECTORGREATER, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_VECTORGREATEREQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VECTORGREATEREQUAL, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_VECTORLESS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VECTORLESS, ASSOCIATIVITY_LEFT));
    registerTokenType(TOKEN_LONGNAME_VECTORLESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VECTORLESSEQUAL, ASSOCIATIVITY_LEFT));
//    }

    //
    // Infix
    //
    // Note that these are the operators that make sense to be infix in WL source code.
    //
    // These may not necessarily correspond to Flat functions in WL.
    //
    registerTokenType(TOKEN_PLUS, new InfixOperatorParselet(PRECEDENCE_INFIX_PLUS));
    registerTokenType(TOKEN_STAR, new InfixOperatorParselet(PRECEDENCE_STAR));
    registerTokenType(TOKEN_DOT, new InfixOperatorParselet(PRECEDENCE_DOT));
    registerTokenType(TOKEN_STARSTAR, new InfixOperatorParselet(PRECEDENCE_STARSTAR));
    registerTokenType(TOKEN_AMPAMP, new InfixOperatorParselet(PRECEDENCE_AMPAMP));
    registerTokenType(TOKEN_BARBAR, new InfixOperatorParselet(PRECEDENCE_BARBAR));
    registerTokenType(TOKEN_BAR, new InfixOperatorParselet(PRECEDENCE_BAR));
    registerTokenType(TOKEN_LESSGREATER, new InfixOperatorParselet(PRECEDENCE_LESSGREATER));
    registerTokenType(TOKEN_TILDETILDE, new InfixOperatorParselet(PRECEDENCE_TILDETILDE));
    registerTokenType(TOKEN_COLONCOLON, new InfixOperatorParselet(PRECEDENCE_COLONCOLON));

    registerTokenType(TOKEN_LONGNAME_IMPLICITPLUS, new InfixOperatorParselet(PRECEDENCE_LONGNAME_IMPLICITPLUS));
    registerTokenType(TOKEN_LONGNAME_TIMES, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TIMES));
    registerTokenType(TOKEN_LONGNAME_INVISIBLETIMES, new InfixOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLETIMES));

    registerTokenType(TOKEN_LONGNAME_AND, new InfixOperatorParselet(PRECEDENCE_LONGNAME_AND));
    registerTokenType(TOKEN_LONGNAME_OR, new InfixOperatorParselet(PRECEDENCE_LONGNAME_OR));
    registerTokenType(TOKEN_LONGNAME_XOR, new InfixOperatorParselet(PRECEDENCE_LONGNAME_XOR));
    registerTokenType(TOKEN_LONGNAME_NAND, new InfixOperatorParselet(PRECEDENCE_LONGNAME_NAND));
    registerTokenType(TOKEN_LONGNAME_NOR, new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOR));

    registerTokenType(TOKEN_LONGNAME_CENTERDOT, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CENTERDOT));
    registerTokenType(TOKEN_LONGNAME_RIGHTTEEARROW, new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTEEARROW));
    registerTokenType(TOKEN_LONGNAME_TILDETILDE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDETILDE));
    registerTokenType(TOKEN_LONGNAME_NOTTILDETILDE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDETILDE));
    registerTokenType(TOKEN_LONGNAME_EQUIVALENT, new InfixOperatorParselet(PRECEDENCE_LONGNAME_EQUIVALENT));
    registerTokenType(TOKEN_LONGNAME_LEFTTRIANGLEEQUAL, new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLEEQUAL));
    registerTokenType(TOKEN_LONGNAME_TILDEEQUAL, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDEEQUAL));
    registerTokenType(TOKEN_LONGNAME_TILDEFULLEQUAL, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDEFULLEQUAL));
    registerTokenType(TOKEN_LONGNAME_NOTTILDEFULLEQUAL, new InfixOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDEFULLEQUAL));
    registerTokenType(TOKEN_LONGNAME_CIRCLEDOT, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEDOT));
    registerTokenType(TOKEN_LONGNAME_DISTRIBUTED, new InfixOperatorParselet(PRECEDENCE_LONGNAME_DISTRIBUTED));
    registerTokenType(TOKEN_LONGNAME_CONDITIONED, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CONDITIONED));
    registerTokenType(TOKEN_LONGNAME_UNION, new InfixOperatorParselet(PRECEDENCE_LONGNAME_UNION));
    registerTokenType(TOKEN_LONGNAME_INTERSECTION, new InfixOperatorParselet(PRECEDENCE_LONGNAME_INTERSECTION));
    registerTokenType(TOKEN_LONGNAME_TENSORWEDGE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TENSORWEDGE));
    registerTokenType(TOKEN_LONGNAME_CROSS, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CROSS));
    registerTokenType(TOKEN_LONGNAME_GREATERTILDE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_GREATERTILDE));
    registerTokenType(TOKEN_LONGNAME_PROPORTIONAL, new InfixOperatorParselet(PRECEDENCE_LONGNAME_PROPORTIONAL));
    registerTokenType(TOKEN_LONGNAME_LESSLESS, new InfixOperatorParselet(PRECEDENCE_LONGNAME_LESSLESS));
    registerTokenType(TOKEN_LONGNAME_CONGRUENT, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT));
    registerTokenType(TOKEN_LONGNAME_TILDE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TILDE));
    registerTokenType(TOKEN_LONGNAME_DOUBLELONGLEFTRIGHTARROW, new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGLEFTRIGHTARROW));
    registerTokenType(TOKEN_LONGNAME_RIGHTARROW, new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTARROW));
    registerTokenType(TOKEN_LONGNAME_SMALLCIRCLE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_SMALLCIRCLE));
    registerTokenType(TOKEN_LONGNAME_DOUBLELONGRIGHTARROW, new InfixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGRIGHTARROW));
    registerTokenType(TOKEN_LONGNAME_DIVIDES, new InfixOperatorParselet(PRECEDENCE_LONGNAME_DIVIDES));
    registerTokenType(TOKEN_LONGNAME_LEFTRIGHTARROW, new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTRIGHTARROW));
    registerTokenType(TOKEN_LONGNAME_VERTICALSEPARATOR, new InfixOperatorParselet(PRECEDENCE_LONGNAME_VERTICALSEPARATOR));
    registerTokenType(TOKEN_LONGNAME_LONGRIGHTARROW, new InfixOperatorParselet(PRECEDENCE_LONGNAME_LONGRIGHTARROW));
    registerTokenType(TOKEN_LONGNAME_BACKSLASH, new InfixOperatorParselet(PRECEDENCE_LONGNAME_BACKSLASH));
    registerTokenType(TOKEN_LONGNAME_DIAMOND, new InfixOperatorParselet(PRECEDENCE_LONGNAME_DIAMOND));
    registerTokenType(TOKEN_LONGNAME_WEDGE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_WEDGE));
    registerTokenType(TOKEN_LONGNAME_VEE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_VEE));
    registerTokenType(TOKEN_LONGNAME_CIRCLETIMES, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CIRCLETIMES));
    registerTokenType(TOKEN_LONGNAME_STAR, new InfixOperatorParselet(PRECEDENCE_LONGNAME_STAR));
    registerTokenType(TOKEN_LONGNAME_VERTICALTILDE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_VERTICALTILDE));
    registerTokenType(TOKEN_LONGNAME_COPRODUCT, new InfixOperatorParselet(PRECEDENCE_LONGNAME_COPRODUCT));
    registerTokenType(TOKEN_LONGNAME_CAP, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CAP));
    registerTokenType(TOKEN_LONGNAME_CUP, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CUP));
    registerTokenType(TOKEN_LONGNAME_CIRCLEPLUS, new InfixOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEPLUS));
    registerTokenType(TOKEN_LONGNAME_RIGHTTRIANGLE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTRIANGLE));
    registerTokenType(TOKEN_LONGNAME_LEFTTRIANGLE, new InfixOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLE));
    registerTokenType(TOKEN_LONGNAME_PERMUTATIONPRODUCT, new InfixOperatorParselet(PRECEDENCE_LONGNAME_PERMUTATIONPRODUCT));
        
    registerTokenType(TOKEN_FAKE_IMPLICITTIMES, new InfixOperatorParselet(PRECEDENCE_FAKE_IMPLICITTIMES));


    //
    // Postfix
    //
    registerTokenType(TOKEN_AMP, new PostfixOperatorParselet(PRECEDENCE_AMP));
    registerTokenType(TOKEN_DOTDOT, new PostfixOperatorParselet(PRECEDENCE_DOTDOT));
    registerTokenType(TOKEN_BANG, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_BANG));
    registerTokenType(TOKEN_MINUSMINUS, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_MINUSMINUS));
    registerTokenType(TOKEN_PLUSPLUS, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_PLUSPLUS));
    registerTokenType(TOKEN_DOTDOTDOT, new PostfixOperatorParselet(PRECEDENCE_DOTDOTDOT));
    registerTokenType(TOKEN_BANGBANG, new PostfixOperatorParselet(PRECEDENCE_BANGBANG));
    registerTokenType(TOKEN_SINGLEQUOTE, new PostfixOperatorParselet(PRECEDENCE_SINGLEQUOTE));

    registerTokenType(TOKEN_LONGNAME_TRANSPOSE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_TRANSPOSE));
    registerTokenType(TOKEN_LONGNAME_CONJUGATE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT));
    registerTokenType(TOKEN_LONGNAME_CONJUGATETRANSPOSE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE));
    registerTokenType(TOKEN_LONGNAME_HERMITIANCONJUGATE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_HERMITIANCONJUGATE));
    registerTokenType(TOKEN_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE));

    
    //
    // Calls
    //
    registerTokenType(TOKEN_OPENSQUARE, new CallParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, new CallParselet());


    //
    // Groups
    //
    registerTokenType(TOKEN_OPENPAREN, new GroupParselet());
    registerTokenType(TOKEN_OPENSQUARE, new GroupParselet());
    registerTokenType(TOKEN_OPENCURLY, new GroupParselet());
    registerTokenType(TOKEN_LESSBAR, new GroupParselet());

    registerTokenType(TOKEN_LONGNAME_LEFTANGLEBRACKET, new GroupParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTCEILING, new GroupParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTFLOOR, new GroupParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTDOUBLEBRACKET, new GroupParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTBRACKETINGBAR, new GroupParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR, new GroupParselet());
    registerTokenType(TOKEN_LONGNAME_LEFTASSOCIATION, new GroupParselet());

    
    //
    // StartOfLine
    //
    registerTokenType(TOKEN_QUESTION, new StartOfLineParselet());
    registerTokenType(TOKEN_QUESTIONQUESTION, new StartOfLineParselet());
    
    
        
    //
    // Special
    //
    
    // context sensitive parsing of atom-like and infix
    registerTokenType(TOKEN_UNDER, new UnderParselet());
    registerTokenType(TOKEN_UNDERUNDER, new UnderParselet());
    registerTokenType(TOKEN_UNDERUNDERUNDER, new UnderParselet());
    
    // infix, prefix, postfix, everythingfix, and also binary and ternary
    registerTokenType(TOKEN_SEMISEMI, new SemiSemiParselet());
    
    // ternary
    registerTokenType(TOKEN_TILDE, new TildeParselet());
    
    // context sensitive parsing of sym:obj and pat:v
    registerTokenType(TOKEN_COLON, new ColonParselet());
    
    // ternary, with different possibilities for second operator
    registerTokenType(TOKEN_SLASHCOLON, new SlashColonParselet());
    
    // infix and postfix
    registerTokenType(TOKEN_SEMI, new SemiParselet());
    
    // FIXME: punt on parsing box syntax, reads tokens with no parsing
    registerTokenType(TOKEN_LINEARSYNTAX_OPENPAREN, new LinearSyntaxOpenParenParselet());
    
    // Has to handle a  =.
    registerTokenType(TOKEN_EQUAL, new EqualParselet());
        
    // Has to handle \[Integral] f \[DifferentialD] x
    registerTokenType(TOKEN_LONGNAME_INTEGRAL, new IntegralParselet());
}

Parser::~Parser() {
    for (auto parselet : parselets) {
        delete parselet;
    }
}



void Parser::init(std::function<bool ()> AbortQ, std::vector<Token> queued) {

    tokenQueue = queued;
    Issues.clear();
    Comments.clear();

    currentAbortQ = AbortQ;
}

void Parser::deinit() {
    
    tokenQueue.clear();
    Issues.clear();
    Comments.clear();

    currentAbortQ = nullptr;
}

void Parser::registerTokenType(TokenEnum token, Parselet *parselet) {
    
    parselets.insert(parselet);

    if (auto Prefix = dynamic_cast<PrefixParselet *>(parselet)) {
        
        assert(prefixParselets.find(token) == prefixParselets.end());
        
        prefixParselets[token] = Prefix;
    }
    
    if (auto Infix = dynamic_cast<InfixParselet *>(parselet)) {
        
        assert(infixParselets.find(token) == infixParselets.end());
        
        infixParselets[token] = Infix;
    }
    
    if (auto Call = dynamic_cast<CallParselet *>(parselet)) {
        
        assert(callParselets.find(token) == callParselets.end());
        
        callParselets[token] = Call;
    }
    
    if (auto Postfix = dynamic_cast<PostfixParselet *>(parselet)) {
        
        assert(postfixParselets.find(token) == postfixParselets.end());
        
        postfixParselets[token] = Postfix;
    }

    if (auto ContextSensitive = dynamic_cast<ContextSensitiveParselet *>(parselet)) {
        
        assert(contextSensitiveParselets.find(token) == contextSensitiveParselets.end());
        
        contextSensitiveParselets[token] = ContextSensitive;
    }
    
    if (auto StartOfLine = dynamic_cast<StartOfLineParselet *>(parselet)) {
        
        assert(startOfLineParselets.find(token) == startOfLineParselets.end());
        
        startOfLineParselets[token] = StartOfLine;
    }
}

//
// Actually get the token
//
// It is important that nextToken() and tryNextToken() are not mutually recursive and accidentally
// make recursive calls for every e.g., comment and newline encountered in a file
//
Token Parser::nextToken0(ParserContext Ctxt) {
    
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
    
    //
    // if Ctxt.LinearSyntaxFlag, then we are in linear syntax, and we disable stringifying next tokens
    //
    TokenizerContext tokenizerCtxt(false, !Ctxt.LinearSyntaxFlag, Ctxt.InformationFlag);
    
    return TheTokenizer->nextToken(tokenizerCtxt);
}

Token Parser::nextToken(ParserContext Ctxt, NextTokenPolicy Policy) {
    
    nextToken0(Ctxt);
    
    return tryNextToken(Ctxt, Policy);
}

Token Parser::tryNextToken(ParserContext Ctxt, NextTokenPolicy Policy) {
    
    auto res = currentToken();
    
    while (true) {
        
        if (res.Tok == TOKEN_NEWLINE) {
            
            if (Ctxt.isGroupTopLevel()) {
                
                if ((Policy & PRESERVE_TOPLEVEL_NEWLINES) == PRESERVE_TOPLEVEL_NEWLINES) {
                    break;
                }
                
            } else {
                
                if ((Policy & PRESERVE_OTHER_NEWLINES) == PRESERVE_OTHER_NEWLINES) {
                    break;
                }
            }
            
        } else if (res.Tok == TOKEN_WHITESPACE) {
            
            if ((Policy & PRESERVE_WHITESPACE) == PRESERVE_WHITESPACE) {
                break;
            }
            
        } else if (res.Tok == TOKEN_COMMENT) {

            auto C = res;

            Comments.push_back(C);
            
            if ((Policy & PRESERVE_COMMENTS) == PRESERVE_COMMENTS) {
                break;
            }
            
        } else {
            
            //
            // Everything else
            //
            
            break;
        }
        
        res = nextToken0(Ctxt);
    }
    
    return res;
}

Token Parser::currentToken() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken();
}

void Parser::setCurrentToken(Token current) {
    
    tokenQueue.insert(tokenQueue.begin(), current);
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

std::vector<Token> Parser::getComments() const {
    return Comments;
}

bool Parser::isPossibleBeginningOfExpression(Token Tok, ParserContext CtxtIn) const {
    
    if (isError(Tok.Tok)) {
        return false;
    }
    
    if (Tok.Tok == TOKEN_ENDOFFILE) {
        return false;
    }
    
    //
    // Prefix parselet?
    //
    auto P = prefixParselets.find(Tok.Tok);
    if (P != prefixParselets.end()) {
        return true;
    }
    
    //
    // StartOfLine parselet?
    //
    if (CtxtIn.isOperatorTopLevel()) {
        if (Tok.Span.lines.start.Col == 1) {
            auto S = startOfLineParselets.find(Tok.Tok);
            if (S != startOfLineParselets.end()) {
                return true;
            }
        }
    }
    
    return false;
}

ContextSensitiveParselet* Parser::findContextSensitiveParselet(TokenEnum Tok) const {
    auto I = contextSensitiveParselets.find(Tok);
    assert(I != contextSensitiveParselets.end());
    return I->second;
}

Precedence Parser::getCurrentTokenPrecedence(TokenEnum TokIn, ParserContext Ctxt) {
    
    if (isError(TokIn)) {
        return PRECEDENCE_LOWEST;
    }
    
    auto Call = callParselets.find(TokIn);
    if (Call != callParselets.end()) {
        
        auto parselet = Call->second;
        
        return parselet->getPrecedence();
    }
    
    auto Infix = infixParselets.find(TokIn);
    if (Infix != infixParselets.end()) {
        
        auto parselet = Infix->second;
        
        return parselet->getPrecedence();
    }
    
    auto Postfix = postfixParselets.find(TokIn);
    if (Postfix != postfixParselets.end()) {
        
        auto parselet = Postfix->second;
        
        return parselet->getPrecedence();
    }

    auto Prefix = prefixParselets.find(TokIn);
    if (Prefix != prefixParselets.end()) {
        
        if (TokIn == TOKEN_LONGNAME_DIFFERENTIALD && Ctxt.IntegralFlag) {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //
            
            return PRECEDENCE_LOWEST;
        }
        
        //
        // Make up a source string for this token. For TOKEN_FAKE_IMPLICITTIMES, this string is ""
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
            auto Start = TheSourceManager->getTokenStart();
            
            setCurrentToken(Token(TOKEN_FAKE_IMPLICITTIMES, "", Source(Start, Start)));
        }
        
        return PRECEDENCE_FAKE_IMPLICITTIMES;
    }
    
    return PRECEDENCE_LOWEST;
}

std::shared_ptr<Node> Parser::parse(ParserContext CtxtIn) {
    
    auto token = currentToken();
    
    assert(token.Tok != TOKEN_UNKNOWN);
    assert(token.Tok != TOKEN_NEWLINE);
    assert(token.Tok != TOKEN_WHITESPACE);
    assert(token.Tok != TOKEN_COMMENT);
    
    if (isAbort()) {
        
        return nullptr;
    }

    if (CtxtIn.OperatorDepth == MAX_EXPRESSION_DEPTH) {

        auto Span = TheSourceManager->getTokenSpan();

        auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXEXPRESSIONDEPTH, std::string("Max expression depth reached.\nConsider breaking up into smaller expressions."), SYNTAXISSUESEVERITY_FORMATTING, Span);

        Issues.push_back(Issue);
    }
    
    //
    // Handle the special cases of:
    // { + }
    // ( a + }
    // ( a @ }
    // We are here parsing the operators, but we don't want to descend and treat the } as the problem
    //
    if (token.Tok == CtxtIn.Closer) {
        
        std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDOPERAND, std::vector<std::shared_ptr<Node>> {
            std::make_shared<TokenNode>(token)
        });
        
        return Error;
    }
    
    if (!isPossibleBeginningOfExpression(token, CtxtIn)) {

        auto errorParselet = std::make_shared<ExpectedPossibleExpressionErrorParselet>();

        auto Error = errorParselet->parse(CtxtIn);

        return Error;
    }
    
    std::shared_ptr<Node> Left;
    
    PrefixParselet *prefix;
    {
        auto I = prefixParselets.find(token.Tok);
        
        if (I == prefixParselets.end()) {
            
            if (CtxtIn.isOperatorTopLevel()) {
                if (token.Span.lines.start.Col == 1) {
                    auto S = startOfLineParselets.find(token.Tok);
                    if (S != startOfLineParselets.end()) {
                        
                        auto start = S->second;
                        
                        auto Line = start->parse(CtxtIn);
                        return Line;
                    }
                }
            }
            
            assert(false);
        }
        
        prefix = I->second;
    }
    
    Left = prefix->parse(CtxtIn);
    
    while (true) {
        
        if (isAbort()) {
            
            return nullptr;
        }
        
        token = tryNextToken(CtxtIn, PRESERVE_TOPLEVEL_NEWLINES);
        
        auto TokenPrecedence = getCurrentTokenPrecedence(token.Tok, CtxtIn);
        
        if (CtxtIn.Prec > TokenPrecedence) {
            break;
        }
        if (CtxtIn.Prec == TokenPrecedence) {
            if (CtxtIn.Assoc != ASSOCIATIVITY_RIGHT) {
                break;
            }
        }
        
        //
        // getCurrentTokenPrecedence() may have inserted something like a new IMPLICITTIMES token, so grab again
        //
        token = currentToken();
        
        auto C = callParselets.find(token.Tok);
        if (C != callParselets.end()) {
            
            CallParselet *call;
            call = C->second;
            
            
            auto Ctxt = CtxtIn;
            Ctxt.Prec = TokenPrecedence;
            Ctxt.Assoc = ASSOCIATIVITY_NONE;
            Left = call->parse(Left, Ctxt);
            
        } else {
            
            auto I = infixParselets.find(token.Tok);
            if (I != infixParselets.end()) {
                
                InfixParselet *infix;
                infix = I->second;
                
                
                auto Ctxt = CtxtIn;
                Ctxt.Prec = TokenPrecedence;
                Ctxt.Assoc = ASSOCIATIVITY_NONE;
                Left = infix->parse(Left, Ctxt);
                
            } else {
                
                auto P = postfixParselets.find(token.Tok);
                if (P != postfixParselets.end()) {
                    
                    PostfixParselet *post;
                    post = P->second;
                    
                    auto Ctxt = CtxtIn;
                    Ctxt.Prec = TokenPrecedence;
                    Ctxt.Assoc = ASSOCIATIVITY_NONE;
                    Left = post->parse(Left, Ctxt);
                    
                } else {
                    
                    assert(false);
                    
                }
            }
        }
        
        token = currentToken();
        
        if (token.Tok == TOKEN_NEWLINE) {
            
            //
            // "Parse" newlines
            //
            // This is like a lifeline to exit all of the levels of parsing
            //
            break;
        }
        
    } // while (Precedence < getCurrentTokenPrecedence(token, Left))
    
    return Left;
}

bool Parser::isAbort() const {
    if (!currentAbortQ) {
        return false;
    }
    
    return currentAbortQ();
}

Parser *TheParser = nullptr;

