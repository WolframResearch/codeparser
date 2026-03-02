
#include "Parselet.h"

#include "ParseletRegistration.h" // for prefixParselets
#include "ParserSession.h"
#include "Parser.h"
#include "Tokenizer.h"
#include "TokenEnumRegistration.h"
#include "Node.h"

#include <cassert>

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


IntegralParselet::IntegralParselet(Symbol Op1, Symbol Op2) : PrefixParselet(IntegralParselet_parsePrefix),  Op1(Op1), Op2(Op2) {}

Symbol IntegralParselet::getOp1() const {
    return Op1;
}

Symbol IntegralParselet::getOp2() const {
    return Op2;
}

void IntegralParselet_parsePrefix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    //
    // Something like  \[Integral] f \[DifferentialD] x
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P, TokIn/*ignored*/);
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
        
        Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITONE, Tok.Buf, 0, Source(Tok.Src.Start)));
        
        return IntegralParselet_parse1(session, P, Tok);
    }
    
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = IntegralParselet_parse1;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix)(session, P2, Tok);
}

void IntegralParselet_parse1(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P, Ignored);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = session->trivia1;

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL, Trivia1);
    
    if (!(Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.Tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD)) {
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return IntegralParselet_reduceIntegral(session, P, Ignored);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    auto& Ctxt = Parser_topContext(session);
    Ctxt.F = IntegralParselet_reduceIntegrate;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix)(session, P2, Tok);
}

void IntegralParselet_reduceIntegrate(ParserSessionPtr session, ParseletPtr P, Token Ignored2) {
    
    assert(P);
    assert(dynamic_cast<const IntegralParselet *>(P));
    
    auto Op1 = dynamic_cast<const IntegralParselet *>(P)->getOp1();
    
    Parser_pushNode(session, new PrefixBinaryNode(Op1, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P, Ignored2);
}

void IntegralParselet_reduceIntegral(ParserSessionPtr session, ParseletPtr P, Token Ignored2) {
    
    assert(P);
    assert(dynamic_cast<const IntegralParselet *>(P));
    
    auto Op2 = dynamic_cast<const IntegralParselet *>(P)->getOp2();
    
    Parser_pushNode(session, new PrefixNode(Op2, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P, Ignored2);
}


InfixDifferentialDParselet::InfixDifferentialDParselet() : InfixParselet(InfixAssertFalseParselet_parseInfix) {}

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
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.Buf, 0, Source(TokIn.Src.Start));
}
