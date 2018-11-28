
#include "Parser.h"

#include "Parselet.h"
#include "Tokenizer.h"

#include <cassert>
#include <iostream>


Parser::Parser() : groupDepth(0), insideColon(false), currentCached(false), _currentToken(), _currentTokenString(),
    mPrefixParselets(), mInfixParselets(), mPostfixParselets(), mCleanupParselets(), tokenQueue() {}

void Parser::init() {
    
    nextToken();
    
    //
    // Atoms and Atom-like expressions
    //
    registerTokenType(TOKEN_SYMBOL, new SymbolParselet());
    registerTokenType(TOKEN_NUMBER, new NumberParselet());
    registerTokenType(TOKEN_STRING, new StringParselet());
    registerTokenType(OPERATOR_UNDER, new UnderParselet());
    registerTokenType(OPERATOR_UNDERUNDER, new UnderUnderParselet());
    registerTokenType(OPERATOR_UNDERUNDERUNDER, new UnderUnderUnderParselet());
    registerTokenType(OPERATOR_UNDERDOT, new UnderDotParselet());
    registerTokenType(OPERATOR_HASH, new HashParselet());
    registerTokenType(OPERATOR_HASHHASH, new HashHashParselet());
    registerTokenType(OPERATOR_PERCENT, new PercentParselet());
    
    //
    // Prefix
    //
    registerTokenType(OPERATOR_MINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUS));
    registerTokenType(OPERATOR_PLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUS));
    registerTokenType(OPERATOR_BANG, new PrefixOperatorParselet(PRECEDENCE_PREFIX_BANG));
    registerTokenType(OPERATOR_PLUSPLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_PLUSPLUS));
    registerTokenType(OPERATOR_MINUSMINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_MINUSMINUS));
    registerTokenType(OPERATOR_LESSLESS, new PrefixOperatorParselet(PRECEDENCE_LESSLESS));
    
    registerTokenType(OPERATOR_LONGNAME_PLUSMINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_PLUSMINUS));
    registerTokenType(OPERATOR_LONGNAME_SUM, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SUM));
    registerTokenType(OPERATOR_LONGNAME_NOT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_NOT));
    registerTokenType(OPERATOR_LONGNAME_SQRT, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQRT));
    registerTokenType(OPERATOR_LONGNAME_MINUSPLUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUSPLUS));
    registerTokenType(OPERATOR_LONGNAME_DIFFERENTIALD, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DIFFERENTIALD));
    registerTokenType(OPERATOR_LONGNAME_MINUS, new PrefixOperatorParselet(PRECEDENCE_PREFIX_LONGNAME_MINUS));
    registerTokenType(OPERATOR_LONGNAME_DEL, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_DEL));
    registerTokenType(OPERATOR_LONGNAME_SQUARE, new PrefixOperatorParselet(PRECEDENCE_LONGNAME_SQUARE));
    
    registerTokenType(OPERATOR_LINEARSYNTAX_BANG, new PrefixOperatorParselet(PRECEDENCE_LINEARSYNTAX_BANG));
    
    //
    // Binary
    //
    registerTokenType(OPERATOR_SLASH, new BinaryOperatorParselet(PRECEDENCE_SLASH, false));
    registerTokenType(OPERATOR_CARET, new BinaryOperatorParselet(PRECEDENCE_CARET, true));
    registerTokenType(OPERATOR_CARETEQUAL, new BinaryOperatorParselet(PRECEDENCE_CARETEQUAL, true));
    registerTokenType(OPERATOR_CARETCOLONEQUAL, new BinaryOperatorParselet(PRECEDENCE_CARETCOLONEQUAL, true));
    registerTokenType(OPERATOR_SLASHAT, new BinaryOperatorParselet(PRECEDENCE_SLASHAT, true));
    registerTokenType(OPERATOR_MINUSGREATER, new BinaryOperatorParselet(PRECEDENCE_MINUSGREATER, true));
    registerTokenType(OPERATOR_ATAT, new BinaryOperatorParselet(PRECEDENCE_ATAT, true));
    registerTokenType(OPERATOR_SLASHSEMICOLON, new BinaryOperatorParselet(PRECEDENCE_SLASHSEMICOLON, false));
    registerTokenType(OPERATOR_SLASHDOT, new BinaryOperatorParselet(PRECEDENCE_SLASHDOT, false));
    registerTokenType(OPERATOR_COLONGREATER, new BinaryOperatorParselet(PRECEDENCE_COLONGREATER, true));
    registerTokenType(OPERATOR_SLASHSLASHDOT, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHDOT, false));
    registerTokenType(OPERATOR_PLUSEQUAL, new BinaryOperatorParselet(PRECEDENCE_PLUSEQUAL, true));
    registerTokenType(OPERATOR_STAREQUAL, new BinaryOperatorParselet(PRECEDENCE_STAREQUAL, true));
    registerTokenType(OPERATOR_MINUSEQUAL, new BinaryOperatorParselet(PRECEDENCE_MINUSEQUAL, true));
    registerTokenType(OPERATOR_SLASHEQUAL, new BinaryOperatorParselet(PRECEDENCE_SLASHEQUAL, true));
    registerTokenType(OPERATOR_LESSMINUSGREATER, new BinaryOperatorParselet(PRECEDENCE_LESSMINUSGREATER, true));
    registerTokenType(OPERATOR_SLASHSLASHAT, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASHAT, true));
    registerTokenType(OPERATOR_AT, new BinaryOperatorParselet(PRECEDENCE_AT, true));
    registerTokenType(OPERATOR_ATATAT, new BinaryOperatorParselet(PRECEDENCE_ATATAT, true));
    registerTokenType(OPERATOR_SLASHSLASH, new BinaryOperatorParselet(PRECEDENCE_SLASHSLASH, false));
    registerTokenType(OPERATOR_COLONEQUAL, new BinaryOperatorParselet(PRECEDENCE_COLONEQUAL, true));
    registerTokenType(OPERATOR_EQUALBANGEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALBANGEQUAL, false));
    registerTokenType(OPERATOR_EQUALEQUALEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALEQUALEQUAL, false));
    registerTokenType(OPERATOR_SLASHSTAR, new BinaryOperatorParselet(PRECEDENCE_SLASHSTAR, false));
    registerTokenType(OPERATOR_ATSTAR, new BinaryOperatorParselet(PRECEDENCE_ATSTAR, false));
    registerTokenType(OPERATOR_LESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LESSEQUAL, false));
    registerTokenType(OPERATOR_LESS, new BinaryOperatorParselet(PRECEDENCE_LESS, false));
    registerTokenType(OPERATOR_GREATER, new BinaryOperatorParselet(PRECEDENCE_GREATER, false));
    registerTokenType(OPERATOR_GREATEREQUAL, new BinaryOperatorParselet(PRECEDENCE_GREATEREQUAL, false));
    registerTokenType(OPERATOR_EQUALEQUAL, new BinaryOperatorParselet(PRECEDENCE_EQUALEQUAL, false));
    registerTokenType(OPERATOR_BANGEQUAL, new BinaryOperatorParselet(PRECEDENCE_BANGEQUAL, false));
    registerTokenType(OPERATOR_GREATERGREATER, new BinaryOperatorParselet(PRECEDENCE_GREATERGREATER, false));
    registerTokenType(OPERATOR_QUESTION, new BinaryOperatorParselet(PRECEDENCE_INFIX_QUESTION, false));
    
    registerTokenType(OPERATOR_LONGNAME_ELEMENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_ELEMENT, false));
    registerTokenType(OPERATOR_LONGNAME_RIGHTTEEARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RIGHTTEEARROW, false));
    registerTokenType(OPERATOR_LONGNAME_TILDETILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDETILDE, false));
    registerTokenType(OPERATOR_LONGNAME_SUBSET, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUBSET, false));
    registerTokenType(OPERATOR_LONGNAME_IMPLIES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_IMPLIES, false));
    registerTokenType(OPERATOR_LONGNAME_NOTTILDETILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDETILDE, false));
    registerTokenType(OPERATOR_LONGNAME_PLUSMINUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_PLUSMINUS, false));
    registerTokenType(OPERATOR_LONGNAME_EQUIVALENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_EQUIVALENT, false));
    registerTokenType(OPERATOR_LONGNAME_LEFTTRIANGLEEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LEFTTRIANGLEEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_NOTELEMENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTELEMENT, false));
    registerTokenType(OPERATOR_LONGNAME_TILDEEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDEEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_DIRECTEDEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIRECTEDEDGE, false));
    registerTokenType(OPERATOR_LONGNAME_SUPERSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUPERSETEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_TILDEFULLEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDEFULLEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_NOTTILDEFULLEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTTILDEFULLEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_CIRCLEDOT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEDOT, false));
    registerTokenType(OPERATOR_LONGNAME_RULE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULE, true));
    registerTokenType(OPERATOR_LONGNAME_EQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_EQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_LESSEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LESSEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_RULEDELAYED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RULEDELAYED, true));
    registerTokenType(OPERATOR_LONGNAME_UNDIRECTEDEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_UNDIRECTEDEDGE, false));
    registerTokenType(OPERATOR_LONGNAME_FUNCTION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_FUNCTION, true));
    registerTokenType(OPERATOR_LONGNAME_XOR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_XOR, false));
    registerTokenType(OPERATOR_LONGNAME_DISTRIBUTED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DISTRIBUTED, false));
    registerTokenType(OPERATOR_LONGNAME_CONDITIONED, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CONDITIONED, false));
    registerTokenType(OPERATOR_LONGNAME_UNION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_UNION, false));
    registerTokenType(OPERATOR_LONGNAME_INTERSECTION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_INTERSECTION, false));
    registerTokenType(OPERATOR_LONGNAME_SUBSETEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SUBSETEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_NOTEQUAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_NOTEQUAL, false));
    registerTokenType(OPERATOR_LONGNAME_TENSORWEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TENSORWEDGE, false));
    registerTokenType(OPERATOR_LONGNAME_CENTERDOT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CENTERDOT, false));
    registerTokenType(OPERATOR_LONGNAME_CROSS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CROSS, false));
    registerTokenType(OPERATOR_LONGNAME_GREATERTILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_GREATERTILDE, false));
    registerTokenType(OPERATOR_LONGNAME_PROPORTIONAL, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_PROPORTIONAL, false));
    registerTokenType(OPERATOR_LONGNAME_LESSLESS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LESSLESS, false));
    registerTokenType(OPERATOR_LONGNAME_CONGRUENT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT, false));
    registerTokenType(OPERATOR_LONGNAME_TILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TILDE, false));
    registerTokenType(OPERATOR_LONGNAME_MINUSPLUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_MINUSPLUS, false));
    registerTokenType(OPERATOR_LONGNAME_DOUBLELONGLEFTRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGLEFTRIGHTARROW, false));
    registerTokenType(OPERATOR_LONGNAME_RIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_RIGHTARROW, false));
    registerTokenType(OPERATOR_LONGNAME_SMALLCIRCLE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_SMALLCIRCLE, false));
    registerTokenType(OPERATOR_LONGNAME_DOUBLELONGRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DOUBLELONGRIGHTARROW, false));
    registerTokenType(OPERATOR_LONGNAME_DIVIDES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIVIDES, false));
    registerTokenType(OPERATOR_LONGNAME_LEFTRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LEFTRIGHTARROW, false));
    registerTokenType(OPERATOR_LONGNAME_VERTICALSEPARATOR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VERTICALSEPARATOR, false));
    registerTokenType(OPERATOR_LONGNAME_LONGRIGHTARROW, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_LONGRIGHTARROW, false));
#if VERSIONNUMBER >= 1110
    registerTokenType(OPERATOR_LONGNAME_TWOWAYRULE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_TWOWAYRULE, true));
#endif
    registerTokenType(OPERATOR_LONGNAME_INVISIBLEAPPLICATION, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_INVISIBLEAPPLICATION, true));
    registerTokenType(OPERATOR_LONGNAME_BACKSLASH, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_BACKSLASH, false));
    registerTokenType(OPERATOR_LONGNAME_DIAMOND, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_DIAMOND, false));
    registerTokenType(OPERATOR_LONGNAME_WEDGE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_WEDGE, false));
    registerTokenType(OPERATOR_LONGNAME_VEE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VEE, false));
    registerTokenType(OPERATOR_LONGNAME_CIRCLETIMES, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLETIMES, false));
    registerTokenType(OPERATOR_LONGNAME_STAR, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_STAR, false));
    registerTokenType(OPERATOR_LONGNAME_VERTICALTILDE, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_VERTICALTILDE, false));
    registerTokenType(OPERATOR_LONGNAME_COPRODUCT, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_COPRODUCT, false));
    registerTokenType(OPERATOR_LONGNAME_CAP, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CAP, false));
    registerTokenType(OPERATOR_LONGNAME_CUP, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CUP, false));
    registerTokenType(OPERATOR_LONGNAME_CIRCLEPLUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEPLUS, false));
    registerTokenType(OPERATOR_LONGNAME_CIRCLEMINUS, new BinaryOperatorParselet(PRECEDENCE_LONGNAME_CIRCLEMINUS, false));
    
    
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
    // I could have done the same thing for OPERATOR_TIMES and OPERATOR_SLASH, but in practice there are not that many expressions
    // that mix * and / enough to hit the Depth limit.
    //
    // Also, we need to keep OPERATOR_STAR and OPERATOR_FAKE_IMPLICITTIMES separate in the AST.
    //
    registerTokenType(OPERATOR_STAR, new InfixOperatorParselet(PRECEDENCE_STAR));
    registerTokenType(OPERATOR_FAKE_IMPLICITTIMES, new InfixOperatorParselet(PRECEDENCE_FAKE_IMPLICITTIMES));
    registerTokenType(OPERATOR_DOT, new InfixOperatorParselet(PRECEDENCE_DOT));
    registerTokenType(OPERATOR_STARSTAR, new InfixOperatorParselet(PRECEDENCE_STARSTAR));
    registerTokenType(OPERATOR_AMPAMP, new InfixOperatorParselet(PRECEDENCE_AMPAMP));
    registerTokenType(OPERATOR_BARBAR, new InfixOperatorParselet(PRECEDENCE_BARBAR));
    registerTokenType(OPERATOR_BAR, new InfixOperatorParselet(PRECEDENCE_BAR));
    registerTokenType(OPERATOR_LESSGREATER, new InfixOperatorParselet(PRECEDENCE_LESSGREATER));
    registerTokenType(OPERATOR_TILDETILDE, new InfixOperatorParselet(PRECEDENCE_TILDETILDE));
    
    registerTokenType(OPERATOR_LONGNAME_TIMES, new InfixOperatorParselet(PRECEDENCE_LONGNAME_TIMES));
    registerTokenType(OPERATOR_LONGNAME_AND, new InfixOperatorParselet(PRECEDENCE_LONGNAME_AND));
    registerTokenType(OPERATOR_LONGNAME_OR, new InfixOperatorParselet(PRECEDENCE_LONGNAME_OR));
    

    //
    // Postfix
    //
    registerTokenType(OPERATOR_AMP, new PostfixOperatorParselet(PRECEDENCE_AMP));
    registerTokenType(OPERATOR_DOTDOT, new PostfixOperatorParselet(PRECEDENCE_DOTDOT));
    registerTokenType(OPERATOR_BANG, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_BANG));
    registerTokenType(OPERATOR_MINUSMINUS, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_MINUSMINUS));
    registerTokenType(OPERATOR_PLUSPLUS, new PostfixOperatorParselet(PRECEDENCE_POSTFIX_PLUSPLUS));
    registerTokenType(OPERATOR_DOTDOTDOT, new PostfixOperatorParselet(PRECEDENCE_DOTDOTDOT));
    registerTokenType(OPERATOR_BANGBANG, new PostfixOperatorParselet(PRECEDENCE_BANGBANG));
    
    registerTokenType(OPERATOR_LONGNAME_TRANSPOSE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_TRANSPOSE));
    registerTokenType(OPERATOR_LONGNAME_CONJUGATE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONGRUENT));
    registerTokenType(OPERATOR_LONGNAME_CONJUGATETRANSPOSE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_CONJUGATETRANSPOSE));
    registerTokenType(OPERATOR_LONGNAME_HERMITIANCONJUGATE, new PostfixOperatorParselet(PRECEDENCE_LONGNAME_HERMITIANCONJUGATE));
    
    //
    // Calls
    //
    registerTokenType(OPERATOR_OPENSQUARE, new OpenSquareCallParselet());

    registerTokenType(OPERATOR_LONGNAME_LEFTDOUBLEBRACKET, new LeftDoubleBracketCallParselet());


    //
    // Groups
    //
    registerTokenType(OPERATOR_OPENPAREN, new GroupParselet(OPERATOR_OPENPAREN));
    registerTokenType(OPERATOR_OPENSQUARE, new GroupParselet(OPERATOR_OPENSQUARE));
    registerTokenType(OPERATOR_OPENCURLY, new GroupParselet(OPERATOR_OPENCURLY));
    registerTokenType(OPERATOR_LESSBAR, new GroupParselet(OPERATOR_LESSBAR));
    registerTokenType(OPERATOR_LONGNAME_LEFTANGLEBRACKET, new GroupParselet(OPERATOR_LONGNAME_LEFTANGLEBRACKET));
    registerTokenType(OPERATOR_LONGNAME_LEFTCEILING, new GroupParselet(OPERATOR_LONGNAME_LEFTCEILING));
    registerTokenType(OPERATOR_LONGNAME_LEFTFLOOR, new GroupParselet(OPERATOR_LONGNAME_LEFTFLOOR));
    registerTokenType(OPERATOR_LONGNAME_LEFTDOUBLEBRACKET, new GroupParselet(OPERATOR_LONGNAME_LEFTDOUBLEBRACKET));
    registerTokenType(OPERATOR_LONGNAME_LEFTBRACKETINGBAR, new GroupParselet(OPERATOR_LONGNAME_LEFTBRACKETINGBAR));
    registerTokenType(OPERATOR_LONGNAME_LEFTDOUBLEBRACKETINGBAR, new GroupParselet(OPERATOR_LONGNAME_LEFTDOUBLEBRACKETINGBAR));
    

    //
    // Cleanup: Groups with missing openers
    //
    registerTokenType(OPERATOR_CLOSEPAREN, new GroupMissingOpenerParselet(OPERATOR_CLOSEPAREN));
    registerTokenType(OPERATOR_CLOSESQUARE, new GroupMissingOpenerParselet(OPERATOR_CLOSESQUARE));
    registerTokenType(OPERATOR_CLOSECURLY, new GroupMissingOpenerParselet(OPERATOR_CLOSECURLY));
    registerTokenType(OPERATOR_BARGREATER, new GroupMissingOpenerParselet(OPERATOR_BARGREATER));
    registerTokenType(OPERATOR_LONGNAME_RIGHTANGLEBRACKET, new GroupMissingOpenerParselet(OPERATOR_LONGNAME_RIGHTANGLEBRACKET));
    registerTokenType(OPERATOR_LONGNAME_RIGHTCEILING, new GroupMissingOpenerParselet(OPERATOR_LONGNAME_RIGHTCEILING));
    registerTokenType(OPERATOR_LONGNAME_RIGHTFLOOR, new GroupMissingOpenerParselet(OPERATOR_LONGNAME_RIGHTFLOOR));
    registerTokenType(OPERATOR_LONGNAME_RIGHTDOUBLEBRACKET, new GroupMissingOpenerParselet(OPERATOR_LONGNAME_RIGHTDOUBLEBRACKET));
    registerTokenType(OPERATOR_LONGNAME_RIGHTBRACKETINGBAR, new GroupMissingOpenerParselet(OPERATOR_LONGNAME_RIGHTBRACKETINGBAR));
    registerTokenType(OPERATOR_LONGNAME_RIGHTDOUBLEBRACKETINGBAR, new GroupMissingOpenerParselet(OPERATOR_LONGNAME_RIGHTBRACKETINGBAR));
    
    
    //
    // Special
    //
    
    // a + b - c parses as one infix expression
    registerTokenType(OPERATOR_PLUS, new InfixPlusParselet());
    registerTokenType(OPERATOR_MINUS, new InfixPlusParselet());
    
    // infix, prefix, postfix, everythingfix, and also binary and ternary
    registerTokenType(OPERATOR_SEMICOLONSEMICOLON, new SemicolonSemicolonParselet());
    
    // ternary
    registerTokenType(OPERATOR_TILDE, new TildeParselet());
    
    // context sensitive parsing of sym:obj and pat:v
    registerTokenType(OPERATOR_COLON, new ColonParselet());
    
    // token is variable length
    registerTokenType(OPERATOR_TICK, new TickParselet());
    
    // ternary
    registerTokenType(OPERATOR_SLASHCOLON, new SlashColonParselet());
    
    // infix and postfix
    registerTokenType(OPERATOR_SEMICOLON, new SemicolonParselet());
    
    // punt on parsing box syntax, reads token with no parsing
    registerTokenType(OPERATOR_LINEARSYNTAX_OPENPAREN, new LinearSyntaxOpenParenParselet());
    
    // binary and ternary
    registerTokenType(OPERATOR_COLONCOLON, new MessageNameParselet());
    
    // Has to handle a =.
    registerTokenType(OPERATOR_EQUAL, new EqualParselet());
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
    
    return TheTokenizer->getIssues();
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
        
        setCurrentToken(OPERATOR_FAKE_IMPLICITTIMES, "");
        
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
        
        return Error;
    }
    
    std::shared_ptr<Node> Left;
    
    PrefixParselet *prefix;
    {
        auto I = mPrefixParselets.find(token);
        if (I == mPrefixParselets.end()) {
            
            auto errorParselet = new ErrorParselet();
            
            Left = errorParselet->parse();
            
            return Left;
        }
        prefix = I->second;
    }
    
    Left = prefix->parse();
    
    token = currentToken();
    int depth = 1;
    while (Precedence < getCurrentTokenPrecedence(token, Left)) {
        
        if (depth > MAXEXPRESSIONDEPTH) {
            
            auto Issues = getIssues();
            
            return std::make_shared<SyntaxErrorNode>(ERROR_MAXEXPRESSIONDEPTH, std::vector<std::shared_ptr<Node>> { Left }, Issues);
        }
        
        //
        // getCurrentTokenPrecedence() may have inserted a new SPACETIMES token, so grab again
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

