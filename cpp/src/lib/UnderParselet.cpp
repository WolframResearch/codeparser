
#include "Parselet.h"

#include "ParseletRegistration.h" // for symbolParselet
#include "Symbol.h"
#include "Parser.h"
#include "ParserSession.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


Symbol UnderParselet::getBOp() const {
    return BOp;
}

Symbol UnderParselet::getPBOp() const {
    return PBOp;
}

ParseFunction UnderParselet::parsePrefix() const {
    return UnderParselet_parsePrefix;
}

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    
    assert(P);
    
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        TheParser->pushNode(new AbortNode());
        return Parser_tryContinue(P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    TheParser->pushLeafAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  _b
        //
        
        TheParser->pushContextAndShift(PRECEDENCE_HIGHEST);
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlank(P, TokIn/*ignored*/);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        TheParser->pushContextAndShift(PRECEDENCE_HIGHEST);
        
        TheParser->pushLeafAndNext(Tok);
        
        MUSTTAIL
        return UnderParselet_reduceBlank(P, TokIn/*ignored*/);
    }
    
    MUSTTAIL
    return Parser_parseClimb(P/*ignored*/, TokIn/*ignored*/);
}

void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    //
    // infix
    //
    // Something like  a_b
    //
    
    assert(P);
    
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        TheParser->pushNode(new AbortNode());
        return;
    }
#endif // CHECK_ABORT
    
    TheParser->pushLeafAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  a_b
        //
        
        TheParser->pushContextAndShift(PRECEDENCE_HIGHEST);
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(P, TokIn/*ignored*/);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  a_b`
        //
        // It's nice to include the error inside of the blank
        //
        
        TheParser->pushContextAndShift(PRECEDENCE_HIGHEST);
        
        TheParser->pushLeafAndNext(Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(P, TokIn/*ignored*/);
    }
    
    // no call needed here
    return;
}

void UnderParselet_reduceBlank(ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<UnderParselet *>(P));
    
    TheParser->shift();
    
    auto BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    TheParser->pushNode(new CompoundNode(BOp, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(P/*ignored*/, Ignored);
}

//
// Called from other parselets
//
void UnderParselet_reduceBlankContextSensitive(ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<UnderParselet *>(P));
    
    TheParser->shift();
    
    auto BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    TheParser->pushNode(new CompoundNode(BOp, TheParser->popContext()));
    
    // no call needed here
    return;
}


ParseFunction UnderDotParselet::parsePrefix() const {
    return UnderDotParselet_parsePrefix;
}

void UnderDotParselet_parsePrefix(ParseletPtr Ignored, Token TokIn) {
    
    //
    // prefix
    //
    // Something like  _.
    //
    
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        TheParser->pushNode(new AbortNode());
        return Parser_tryContinue(Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    TheParser->pushLeafAndNext(TokIn);
    
    MUSTTAIL
    return Parser_parseClimb(Ignored, TokIn/*ignored*/);
}


//
// Called from other parselets
//
void UnderDotParselet_parseInfixContextSensitive(ParseletPtr Ignored, Token TokIn) {
    
    //
    // infix
    //
    // Something like  a_.
    
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        TheParser->pushNode(new AbortNode());
        return;
    }
#endif // CHECK_ABORT
    
    TheParser->pushLeafAndNext(TokIn);
    
    // no call needed here
    return;
}
