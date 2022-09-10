
#include "Parselet.h"

#include "ParseletRegistration.h" // for symbolParselet
#include "Parser.h"
#include "ParserSession.h"
#include "Tokenizer.h"
#include "Node.h"
#include "TokenEnumRegistration.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


UnderParselet::UnderParselet(Symbol BOp, Symbol PBOp) : PrefixParselet(UnderParselet_parsePrefix), BOp(BOp), PBOp(PBOp) {}

Symbol UnderParselet::getBOp() const {
    return BOp;
}

Symbol UnderParselet::getPBOp() const {
    return PBOp;
}

void UnderParselet_parsePrefix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    
    assert(P);
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  _b
        //
        
        Parser_pushContext(session, PRECEDENCE_HIGHEST);
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(session, symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlank(session, P, TokIn/*ignored*/);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        Parser_pushContext(session, PRECEDENCE_HIGHEST);
        
        Parser_pushLeafAndNext(session, Tok);
        
        MUSTTAIL
        return UnderParselet_reduceBlank(session, P, TokIn/*ignored*/);
    }
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, TokIn/*ignored*/);
}

void UnderParselet_parseInfixContextSensitive(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    //
    // infix
    //
    // Something like  a_b
    //
    
    assert(P);
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return;
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  a_b
        //
        
        Parser_pushContext(session, PRECEDENCE_HIGHEST);
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(session, symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P, TokIn/*ignored*/);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  a_b`
        //
        // It's nice to include the error inside of the blank
        //
        
        Parser_pushContext(session, PRECEDENCE_HIGHEST);
        
        Parser_pushLeafAndNext(session, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P, TokIn/*ignored*/);
    }
    
    // no call needed here
    return;
}

void UnderParselet_reduceBlank(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<const UnderParselet *>(P));
    
    auto BOp = dynamic_cast<const UnderParselet *>(P)->getBOp();
    
    Parser_pushNode(session, new CompoundNode(BOp, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
}

//
// Called from other parselets
//
void UnderParselet_reduceBlankContextSensitive(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<const UnderParselet *>(P));
    
    auto BOp = dynamic_cast<const UnderParselet *>(P)->getBOp();
    
    Parser_pushNode(session, new CompoundNode(BOp, Parser_popContext(session)));
    
    // no call needed here
    return;
}


UnderDotParselet::UnderDotParselet() : PrefixParselet(UnderDotParselet_parsePrefix) {}

void UnderDotParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // prefix
    //
    // Something like  _.
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}


//
// Called from other parselets
//
void UnderDotParselet_parseInfixContextSensitive(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // infix
    //
    // Something like  a_.
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return;
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    // no call needed here
    return;
}
