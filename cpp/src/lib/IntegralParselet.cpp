
#include "Parselet.h"
#include "ParseletRegistration.h" // for prefixParselets
#include "ParserSession.h"
#include "SymbolRegistration.h"
#include "Parser.h"
#include "Tokenizer.h"
#include "TokenEnumRegistration.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


ParseFunction IntegralParselet::parsePrefix() const {
    return IntegralParselet_parsePrefix;
}

void IntegralParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  \[Integral] f \[DifferentialD] x
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto& Ctxt = Parser_pushContext(session, PRECEDENCE_CLASS_INTEGRATIONOPERATORS);
    
    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.Tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD) {
        
        //
        // \[Integral] \[DifferentialD] x
        //
        
        Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITONE, BufferAndLength(Tok.Buf), Source(Tok.Src.Start)));
        
        return IntegralParselet_parse1(session, Ignored, Tok);
    }
    
    assert(!Ctxt.F);
    Ctxt.F = IntegralParselet_parse1;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void IntegralParselet_parse1(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = session->trivia1;

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL, Trivia1);
    
    if (!(Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.Tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD)) {
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return IntegralParselet_reduceIntegral(session, Ignored, Ignored2);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    auto& Ctxt = Parser_topContext(session);
    Ctxt.F = IntegralParselet_reduceIntegrate;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void IntegralParselet_reduceIntegrate(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new PrefixBinaryNode(SYMBOL_INTEGRATE, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void IntegralParselet_reduceIntegral(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new PrefixNode(SYMBOL_INTEGRAL, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence InfixDifferentialDParselet::getPrecedence(ParserSessionPtr session) const {
    
    if (Parser_topPrecedence(session) == PRECEDENCE_CLASS_INTEGRATIONOPERATORS) {

        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //

        return PRECEDENCE_LOWEST;
    }

    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

Token InfixDifferentialDParselet::processImplicitTimes(ParserSessionPtr session, Token TokIn) const {
    
    if (Parser_topPrecedence(session) == PRECEDENCE_CLASS_INTEGRATIONOPERATORS) {

        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //

        return TokIn;
    }
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(TokIn.Buf), Source(TokIn.Src.Start));
}

ParseFunction InfixDifferentialDParselet::parseInfix() const {
    
    assert(false);
    
    return nullptr;
}
