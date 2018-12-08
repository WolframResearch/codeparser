
#include "Parser.h"

#include "Parselet.h"
#include "Tokenizer.h"

#include <cassert>
#include <iostream>


Parser::Parser() : groupDepth(0), insideColon(false), currentCached(false), _currentToken(), _currentTokenString(),
    mPrefixParselets(), mInfixParselets(), mPostfixParselets(), mCleanupParselets(), tokenQueue(), Issues() {}

void Parser::init() {
    
    nextToken();
    
    //
    // Atoms and Atom-like expressions
    //
    registerTokenType(TOKEN_SYMBOL, new SymbolParselet());
    registerTokenType(TOKEN_NUMBER, new NumberParselet());
    registerTokenType(TOKEN_STRING, new StringParselet());
    registerTokenType(TOKEN_OPERATOR_UNDER, new UnderParselet());
    registerTokenType(TOKEN_OPERATOR_UNDERUNDER, new UnderUnderParselet());
    registerTokenType(TOKEN_OPERATOR_UNDERUNDERUNDER, new UnderUnderUnderParselet());
    registerTokenType(TOKEN_OPERATOR_UNDERDOT, new UnderDotParselet());
    registerTokenType(TOKEN_OPERATOR_HASH, new HashParselet());
    registerTokenType(TOKEN_OPERATOR_HASHHASH, new HashHashParselet());
    registerTokenType(TOKEN_OPERATOR_PERCENT, new PercentParselet());
    
    //
    // Prefix
    //
    registerTokenType(TOKEN_OPERATOR_MINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUS));
    registerTokenType(TOKEN_OPERATOR_PLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUS));
    registerTokenType(TOKEN_OPERATOR_BANG, new PrefixOperatorParselet(PRECEDENCE_PREFIX_BANG));
    registerTokenType(TOKEN_OPERATOR_PLUSPLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUSPLUS));
    registerTokenType(TOKEN_OPERATOR_MINUSMINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUSMINUS));
    registerTokenType(TOKEN_OPERATOR_LESSLESS, new PrefixOperatorParselet(PRECEDENCE_LESSLESS));
    
    registerTokenType(TOKEN_OPERATOR_LONGNAME_PLUSMINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SUM, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SUM));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NOT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_NOT));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SQRT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQRT));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_MINUSPLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DIFFERENTIALD, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DIFFERENTIALD));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_MINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUS));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DEL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DEL));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SQUARE, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQUARE));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_INTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_INTEGRAL));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CONTOURINTEGRAL));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DOUBLECONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DOUBLECONTOURINTEGRAL));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CLOCKWISECONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_CLOCKWISECONTOURINTEGRAL));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_PRODUCT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_PRODUCT));

    registerTokenType(TOKEN_OPERATOR_LINEARSYNTAX_BANG, new PrefixOperatorParselet(PRECEDENCE_LINEARSYNTAX_BANG));
    
    //
    // Binary
    //
    registerTokenType(TOKEN_OPERATOR_SLASH, new BinaryOperatorParselet(PRECEDENCE_SLASH, false));
    registerTokenType(TOKEN_OPERATOR_CARET, new BinaryOperatorParselet(PRECEDENCE_CARET, true));
    registerTokenType(TOKEN_OPERATOR_CARETEQUAL, new BinaryOperatorParselet(PRECEDENCE_CARETEQUAL, true));
    registerTokenType(TOKEN_OPERATOR_CARETCOLONEQUAL, new BinaryOperatorParselet(PRECEDENCE_CARETCOLONEQUAL, true));
    registerTokenType(TOKEN_OPERATOR_SLASHAT, new BinaryOperatorParselet(PRECEDENCE_SLASHAT, true));
    registerTokenType(TOKEN_OPERATOR_MINUSGREATER, new BinaryOperatorParselet(PRECEDENCE_MINUSGREATER, true));
    registerTokenType(TOKEN_OPERATOR_ATAT, new BinaryOperatorParselet(PRECEDENCE_ATAT, true));
    registerTokenType(TOKEN_OPERATOR_SLASHSEMI, new BinaryOperatorParselet(PRECEDENCE_SLASHSEMI, false));
    registerTokenType(TOKEN_OPERATOR_SLASHDOT, new BinaryOperatorParselet(PRECEDENCE_SLASHDOT, false));
    registerTokenType(TOKEN_OPERATOR_COLONGREATER, new BinaryOperatorParselet(PRECEDENCE_COLONGREATER, true));
    registerTokenType(TOKEN_OPERATOR_SLASHSLASHDOT, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHDOT, false));
    registerTokenType(TOKEN_OPERATOR_PLUSEQUAL, new BinaryOperatorParselet(PRECEDENCE_PLUSEQUAL, true));
    registerTokenType(TOKEN_OPERATOR_STAREQUAL, new BinaryOperatorParselet(PRECEDENCE_STAREQUAL, true));
    registerTokenType(TOKEN_OPERATOR_MINUSEQUAL, new BinaryOperatorParselet(PRECEDENCE_MINUSEQUAL, true));
    registerTokenType(TOKEN_OPERATOR_SLASHEQUAL, new BinaryOperatorParselet(PRECEDENCE_SLASHEQUAL, true));
    registerTokenType(TOKEN_OPERATOR_LESSMINUSGREATER, new BinaryOperatorParselet(PRECEDENCE_LESSMINUSGREATER, true));
    registerTokenType(TOKEN_OPERATOR_SLASHSLASHAT, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHAT, true));
    registerTokenType(TOKEN_OPERATOR_AT, new BinaryOperatorParselet(PRECEDENCE_AT, true));
    registerTokenType(TOKEN_OPERATOR_ATATAT, new BinaryOperatorParselet(PRECEDENCE_ATATAT, true));
    registerTokenType(TOKEN_OPERATOR_SLASHSLASH, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASH, false));
    registerTokenType(TOKEN_OPERATOR_COLONEQUAL, new BinaryOperatorParselet(PRECEDENCE_COLONEQUAL, true));
    registerTokenType(TOKEN_OPERATOR_EQUALBANGEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALBANGEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_EQUALEQUALEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALEQUALEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_SLASHSTAR, new BinaryOperatorParselet(PRECEDENCE_SLASHSTAR, false));
    registerTokenType(TOKEN_OPERATOR_ATSTAR, new BinaryOperatorParselet(PRECEDENCE_ATSTAR, false));
    registerTokenType(TOKEN_OPERATOR_LESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LESSEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LESS, new BinaryOperatorParselet(PRECEDENCE_LESS, false));
    registerTokenType(TOKEN_OPERATOR_GREATER, new BinaryOperatorParselet(PRECEDENCE_GREATER, false));
    registerTokenType(TOKEN_OPERATOR_GREATEREQUAL, new BinaryOperatorParselet(PRECEDENCE_GREATEREQUAL, false));
    registerTokenType(TOKEN_OPERATOR_EQUALEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_BANGEQUAL, new BinaryOperatorParselet(PRECEDENCE_BANGEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_GREATERGREATER, new BinaryOperatorParselet(PRECEDENCE_GREATERGREATER, false));
    registerTokenType(TOKEN_OPERATOR_QUESTION, new BinaryOperatorParselet(PRECEDENCE_INFIX_QUESTION, false));
    
    registerTokenType(TOKEN_OPERATOR_LONGNAME_ELEMENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_ELEMENT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_RIGHTTEEARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTEEARROW, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TILDETILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDETILDE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SUBSET, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUBSET, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_IMPLIES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_IMPLIES, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NOTTILDETILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDETILDE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_PLUSMINUS, new BinaryOperatorParselet(PRECEDENCE_INFIX_LONGNAME_PLUSMINUS, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_EQUIVALENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_EQUIVALENT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTTRIANGLEEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLEEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NOTELEMENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTELEMENT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TILDEEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDEEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DIRECTEDEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIRECTEDEDGE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SUPERSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUPERSETEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TILDEFULLEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDEFULLEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NOTTILDEFULLEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDEFULLEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CIRCLEDOT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEDOT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_RULE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULE, true));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_EQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_EQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LESSEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_RULEDELAYED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULEDELAYED, true));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_UNDIRECTEDEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_FUNCTION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_FUNCTION, true));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DISTRIBUTED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DISTRIBUTED, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CONDITIONED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CONDITIONED, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_UNION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_UNION, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_INTERSECTION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_INTERSECTION, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SUBSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUBSETEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NOTEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTEQUAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TENSORWEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TENSORWEDGE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CENTERDOT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CENTERDOT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CROSS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CROSS, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_GREATERTILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_GREATERTILDE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_PROPORTIONAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_PROPORTIONAL, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LESSLESS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LESSLESS, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CONGRUENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_MINUSPLUS, new BinaryOperatorParselet(PRECEDENCE_INFIX_LONGNAME_MINUSPLUS, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DOUBLELONGLEFTRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGLEFTRIGHTARROW, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_RIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RIGHTARROW, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_SMALLCIRCLE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SMALLCIRCLE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DOUBLELONGRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGRIGHTARROW, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DIVIDES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIVIDES, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LEFTRIGHTARROW, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_VERTICALSEPARATOR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VERTICALSEPARATOR, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LONGRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LONGRIGHTARROW, false));
    if (VERSION_NUMBER >= 1110) {
        registerTokenType(TOKEN_OPERATOR_LONGNAME_TWOWAYRULE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TWOWAYRULE, true));
    }
    registerTokenType(TOKEN_OPERATOR_LONGNAME_INVISIBLEAPPLICATION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, true));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_BACKSLASH, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_BACKSLASH, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_DIAMOND, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIAMOND, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_WEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_WEDGE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_VEE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VEE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CIRCLETIMES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLETIMES, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_STAR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_STAR, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_VERTICALTILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VERTICALTILDE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_COPRODUCT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_COPRODUCT, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CAP, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CAP, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CUP, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CUP, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CIRCLEPLUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEPLUS, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CIRCLEMINUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEMINUS, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_RIGHTTRIANGLE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTRIANGLE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTTRIANGLE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLE, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TIMES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TIMES, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_AND, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_AND, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_OR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_OR, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_XOR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_XOR, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NAND, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NAND, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_NOR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOR, false));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_IMPLICITPLUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_IMPLICITPLUS, false));
    
    //
    // Infix
    //
    // Note that these are the operators that make sense to be infix in WL source code.
    //
    // These may not necessarily correspond to Flat functions in WL.
    //
    // Note that OPERATOR_PLUS and OPERATOR_MINUS are not here.
    // This is because OPERATOR_PLUS and OPERATOR_MINUS are handled specially. Expressions like a + b - c are considered to be 1
    // infix + expression. Treating them as 1 expression helps to prevent Expresion Depth Errors that can happen when a + b - c is
    // considered to be a + node nested inside of a - node.
    //
    // I could have done the same thing for OPERATOR_STAR and OPERATOR_SLASH, but in practice there are not that many expressions
    // that mix * and / enough to hit the Depth limit.
    //
    // Also, we need to keep OPERATOR_STAR and OPERATOR_FAKE_IMPLICITTIMES separate in the AST.
    //
    registerTokenType(TOKEN_OPERATOR_STAR, new InfixOperatorParselet(PRECEDENCE_STAR));
    registerTokenType(TOKEN_OPERATOR_FAKE_IMPLICITTIMES, new InfixOperatorParselet(PRECEDENCE_FAKE_IMPLICITTIMES));
    registerTokenType(TOKEN_OPERATOR_DOT, new InfixOperatorParselet(PRECEDENCE_DOT));
    registerTokenType(TOKEN_OPERATOR_STARSTAR, new InfixOperatorParselet(PRECEDENCE_STARSTAR));
    registerTokenType(TOKEN_OPERATOR_AMPAMP, new InfixOperatorParselet(PRECEDENCE_AMPAMP));
    registerTokenType(TOKEN_OPERATOR_BARBAR, new InfixOperatorParselet(PRECEDENCE_BARBAR));
    registerTokenType(TOKEN_OPERATOR_BAR, new InfixOperatorParselet(PRECEDENCE_BAR));
    registerTokenType(TOKEN_OPERATOR_LESSGREATER, new InfixOperatorParselet(PRECEDENCE_LESSGREATER));
    registerTokenType(TOKEN_OPERATOR_TILDETILDE, new InfixOperatorParselet(PRECEDENCE_TILDETILDE));
    

    //
    // Postfix
    //
    registerTokenType(TOKEN_OPERATOR_AMP, new PostfixOperatorParselet(PRECEDENCE_AMP));
    registerTokenType(TOKEN_OPERATOR_DOTDOT, new PostfixOperatorParselet(PRECEDENCE_DOTDOT));
    registerTokenType(TOKEN_OPERATOR_BANG, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_BANG));
    registerTokenType(TOKEN_OPERATOR_MINUSMINUS, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_MINUSMINUS));
    registerTokenType(TOKEN_OPERATOR_PLUSPLUS, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_PLUSPLUS));
    registerTokenType(TOKEN_OPERATOR_DOTDOTDOT, new PostfixOperatorParselet(PRECEDENCE_DOTDOTDOT));
    registerTokenType(TOKEN_OPERATOR_BANGBANG, new PostfixOperatorParselet(PRECEDENCE_BANGBANG));
    
    registerTokenType(TOKEN_OPERATOR_LONGNAME_TRANSPOSE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_TRANSPOSE));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CONJUGATE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_CONJUGATETRANSPOSE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_HERMITIANCONJUGATE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_HERMITIANCONJUGATE));
    
    //
    // Calls
    //
    registerTokenType(TOKEN_OPERATOR_OPENSQUARE, new OpenSquareCallParselet());

    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTDOUBLEBRACKET, new LeftDoubleBracketCallParselet());


    //
    // Groups
    //
    registerTokenType(TOKEN_OPERATOR_OPENPAREN, new GroupParselet(TOKEN_OPERATOR_OPENPAREN));
    registerTokenType(TOKEN_OPERATOR_OPENSQUARE, new GroupParselet(TOKEN_OPERATOR_OPENSQUARE));
    registerTokenType(TOKEN_OPERATOR_OPENCURLY, new GroupParselet(TOKEN_OPERATOR_OPENCURLY));
    registerTokenType(TOKEN_OPERATOR_LESSBAR, new GroupParselet(TOKEN_OPERATOR_LESSBAR));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTANGLEBRACKET, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTANGLEBRACKET));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTCEILING, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTCEILING));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTFLOOR, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTFLOOR));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTDOUBLEBRACKET, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTDOUBLEBRACKET));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTBRACKETINGBAR, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTBRACKETINGBAR));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTDOUBLEBRACKETINGBAR, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTDOUBLEBRACKETINGBAR));
    registerTokenType(TOKEN_OPERATOR_LONGNAME_LEFTASSOCIATION, new GroupParselet(TOKEN_OPERATOR_LONGNAME_LEFTASSOCIATION));

    
    //
    // Special
    //
    
    // a + b - c parses as one infix expression
    registerTokenType(TOKEN_OPERATOR_PLUS, new InfixPlusParselet());
    registerTokenType(TOKEN_OPERATOR_MINUS, new InfixPlusParselet());
    
    // infix, prefix, postfix, everythingfix, and also binary and ternary
    registerTokenType(TOKEN_OPERATOR_SEMISEMI, new SemiSemiParselet());
    
    // ternary
    registerTokenType(TOKEN_OPERATOR_TILDE, new TildeParselet());
    
    // context sensitive parsing of sym:obj and pat:v
    registerTokenType(TOKEN_OPERATOR_COLON, new ColonParselet());
    
    // token is variable length
    registerTokenType(TOKEN_OPERATOR_TICK, new TickParselet());
    
    // ternary
    registerTokenType(TOKEN_OPERATOR_SLASHCOLON, new SlashColonParselet());
    
    // infix and postfix
    registerTokenType(TOKEN_OPERATOR_SEMI, new SemiParselet());
    
    // punt on parsing box syntax, reads token with no parsing
    registerTokenType(TOKEN_OPERATOR_LINEARSYNTAX_OPENPAREN, new LinearSyntaxOpenParenParselet());
    
    // binary and ternary
    registerTokenType(TOKEN_OPERATOR_COLONCOLON, new MessageNameParselet());
    
    // Has to handle a =.
    registerTokenType(TOKEN_OPERATOR_EQUAL, new EqualParselet());
}

void Parser::registerTokenType(Token token, Parselet *parselet) {
    
    if (auto Prefix = dynamic_cast<PrefixParselet *>(parselet)) {
        
        if (mPrefixParselets.find(token) != mPrefixParselets.end()) {
            assert(false);
        }
        
        mPrefixParselets[token] = Prefix;
    }
    
    if (auto Infix = dynamic_cast<InfixParselet *>(parselet)) {
        
        if (mInfixParselets.find(token) != mInfixParselets.end()) {
            assert(false);
        }
        
        mInfixParselets[token] = Infix;
    }
    
    if (auto Postfix = dynamic_cast<PostfixParselet *>(parselet)) {
        
        if (mPostfixParselets.find(token) != mPostfixParselets.end()) {
            assert(false);
        }
        
        mPostfixParselets[token] = Postfix;
    }
    
    if (auto Cleanup = dynamic_cast<CleanupParselet *>(parselet)) {
        
        if (mCleanupParselets.find(token) != mCleanupParselets.end()) {
            assert(false);
        }
        
        mCleanupParselets[token] = Cleanup;
    }
}

Token Parser::nextToken(NextTokenPolicy policy) {
    
    if (currentCached) {
        currentCached = false;
        
        return TheTokenizer->currentToken();
    }
    
    if (!tokenQueue.empty()) {
        
        auto p = tokenQueue[0];
        
        auto Tok = p.first;
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return Tok;
    }
    
    TheTokenizer->nextToken();
    
    return tryNextToken(policy);
}

Token Parser::tryNextToken(NextTokenPolicy policy) {
    
    auto res = TheTokenizer->currentToken();
    
    switch (policy) {
        case POLICY_DEFAULT:
            while (res == TOKEN_COMMENT || res == TOKEN_NEWLINE || res == TOKEN_SPACE) {

                // Clear String
                getString();

                res = TheTokenizer->nextToken();
            }
            break;
        case POLICY_PRESERVE_TOPLEVEL_NEWLINES:
            if (groupDepth == 0) {
                while (res == TOKEN_COMMENT || res == TOKEN_SPACE) {

                    // Clear String
                    getString();

                    res = TheTokenizer->nextToken();
                }
            } else {
                while (res == TOKEN_COMMENT || res == TOKEN_NEWLINE || res == TOKEN_SPACE) {

                    // Clear String
                    getString();

                    res = TheTokenizer->nextToken();
                }
            }
            break;
        case POLICY_PRESERVE_EVERYTHING:
            break;
    }
    
    return res;
}

Token Parser::currentToken() {
    
     if (currentCached) {
        
         return _currentToken;
     }
    
    return TheTokenizer->currentToken();
}

void Parser::setCurrentToken(Token current, std::string StringVal) {
    
    _currentToken = current;
    _currentTokenString = StringVal;
    currentCached = true;
}

std::string Parser::getString() {
    
    if (currentCached) {
        
        return _currentTokenString;
    }
    
    return TheTokenizer->getString();
}

std::vector<SyntaxIssue> Parser::getIssues() {

    auto Tmp = Issues;

    Issues.clear();
    
    auto TokenizerIssues = TheTokenizer->getIssues();
    
    std::copy(TokenizerIssues.begin(), TokenizerIssues.end(), std::back_inserter(Tmp));
    
    return Tmp;
}

bool Parser::isPossibleBeginningOfExpression(Token Tok) {
    
    auto I = mPrefixParselets.find(Tok);
    if (I != mPrefixParselets.end()) {
        return true;
    }
    
    return false;
}

precedence_t Parser::getCurrentTokenPrecedence(Token TokIn, std::shared_ptr<Node> Left) {
    
    if (isError(TokIn)) {
        return PRECEDENCE_LOWEST;
    }
    
    auto I = mInfixParselets.find(TokIn);
    if (I != mInfixParselets.end()) {
        
        auto parselet = I->second;
        
        // ColonParselet.getPrecedence is context sensitive
        if (auto colonParselet = dynamic_cast<ColonParselet *>(parselet)) {
            auto prec = colonParselet->getColonPrecedence(Left);
            return prec;
        }
        
        return parselet->getPrecedence();
    }
    
    auto IP = mPostfixParselets.find(TokIn);
    if (IP != mPostfixParselets.end()) {
        
        auto parselet = IP->second;
        
        return parselet->getPrecedence();
    }
    
    if (mPrefixParselets.find(TokIn) != mPrefixParselets.end()) {
        
        //
        // Make up a source string for this token. For OPERATOR_FAKE_IMPLICITTIMES, this string is ""
        //
        
        setCurrentToken(TOKEN_OPERATOR_FAKE_IMPLICITTIMES, "");
        
        return PRECEDENCE_STAR;
    }
    
    return PRECEDENCE_LOWEST;
}



std::shared_ptr<Node>Parser::parseTopLevel() {
    
    auto Expr = parse(PRECEDENCE_LOWEST);
    
    Expr = cleanup(Expr);
    
    return Expr;
}

std::shared_ptr<Node>Parser::parse(precedence_t Precedence) {
    
    Token token = currentToken();
    
    assert(token != TOKEN_UNKNOWN);
    assert(token != TOKEN_COMMENT);
    assert(token != TOKEN_NEWLINE);
    assert(token != TOKEN_SPACE);
    
    if (isError(token) ||
        token == TOKEN_EOF ||
        !isPossibleBeginningOfExpression(token)) {
        
        auto errorParselet = new ErrorParselet();
        
        auto Error = errorParselet->parse();
        
        Error = cleanup(Error);
        
        return Error;
    }
    
    std::shared_ptr<Node> Left;
    
    PrefixParselet *prefix;
    {
        auto I = mPrefixParselets.find(token);
        
        assert(I != mPrefixParselets.end());
        
        prefix = I->second;
    }
    
    Left = prefix->parse();
    
    token = currentToken();
    auto depth = 1;
    auto maxDepthReached = false;
    while (Precedence < getCurrentTokenPrecedence(token, Left)) {
        
        if (depth > MAX_EXPRESSION_DEPTH && !maxDepthReached) {
            
            maxDepthReached = true;

            auto Span = TheSourceManager->getTokenSpan();

            auto Issue = SyntaxIssue(TAG_MAXEXPRESSIONDEPTH, std::string("Max expression depth reached. Consider breaking up into smaller expressions."), SEVERITY_WARNING, Span);
        
            Issues.push_back(Issue);
        }
        
        //
        // getCurrentTokenPrecedence() may have inserted something like a new IMPLICITTIMES token, so grab again
        //
        token = currentToken();
        
        auto I = mInfixParselets.find(token);
        if (I != mInfixParselets.end()) {
            
            InfixParselet *infix;
            infix = I->second;
            
            Left = infix->parse(Left);
            
        } else {
            
            auto P = mPostfixParselets.find(token);
            
            assert(P != mPostfixParselets.end());
            
            PostfixParselet *post;
            post = P->second;
            
            Left = post->parse(Left);
        }
        
        token = currentToken();
        
        if (token == TOKEN_NEWLINE) {
            
            //
            // This is like a lifeline to exit all of the levels of parsing
            //
            break;
        }
        
        depth++;
        
    } // while (Precedence < getCurrentTokenPrecedence(token, Left))
    
    return Left;
}

//
// Cleanup the rest of the input
//
std::shared_ptr<Node> Parser::cleanup(std::shared_ptr<Node> Left) {
    
    auto Cleaned = Left;
    
    while (true) {
        
        Token token = currentToken();
        
        //
        // Some newline inbetween toplevel expressions
        //
        if (token == TOKEN_NEWLINE) {
            return Cleaned;
        }
        
        if (token == TOKEN_EOF) {
            return Cleaned;
        }
        
        CleanupParselet *cleanup;
        {
            auto I = mCleanupParselets.find(token);
            if (I == mCleanupParselets.end()) {
                
                //
                // If no other cleanup parselet is found, then rely on this
                //
                
                cleanup = new CleanupRestParselet();
                
            } else {
                
                cleanup = I->second;
            }
        }
        
        Cleaned = cleanup->parse(Cleaned);
    }
}

Parser *TheParser = nullptr;

