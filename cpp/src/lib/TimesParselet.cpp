
#include "Parselet.h"

#include "ParserSession.h"
#include "Parser.h"
#include "ParseletRegistration.h"
#include "Tokenizer.h"
#include "SymbolRegistration.h"
#include "Node.h"
#include "TokenEnumRegistration.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


TimesParselet::TimesParselet() : InfixParselet(TimesParselet_parseInfix) {}

Symbol TimesParselet::getOp() const {
    return SYMBOL_TIMES;
}

Precedence TimesParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_STAR;
}

void TimesParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = Tokenizer_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = Parser_identity;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix)(session, P2, Tok2);
    
    return TimesParselet_parseLoop(session, Ignored, TokIn/*ignored*/);
#else
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = TimesParselet_parseLoop;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix)(session, P2, Tok2);
#endif // !USE_MUSTTAIL
}

void TimesParselet_parseLoop(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = session->trivia1;
    
    auto Tok1 = Tokenizer_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok1, TOPLEVEL, Trivia1);
    
    auto I = infixParselets[Tok1.Tok.value()];

    Tok1 = I->processImplicitTimes(session, Tok1);

    if (Tok1.Tok == TOKEN_FAKE_IMPLICITTIMES) {

        //
        // implicit Times should not cross toplevel newlines
        //
        // so reset and try again
        //

        Trivia1.reset(session);

        Tok1 = Tokenizer_currentToken(session, TOPLEVEL);

        Parser_eatTriviaButNotToplevelNewlines(session, Tok1, TOPLEVEL, Trivia1);

        I = infixParselets[Tok1.Tok.value()];

        Tok1 = I->processImplicitTimes(session, Tok1);
    }

    I = infixParselets[Tok1.Tok.value()];
    
    //
    // Cannot just compare tokens
    //
    // May be something like  a * b c \[Times] d
    //
    // and we want only a single Infix node created
    //
    if (I != timesParselet) {
        
        //
        // Tok.Tok != TokIn.Tok, so break
        //
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return TimesParselet_reduceTimes(session, Ignored, Ignored2);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeafAndNext(session, Tok1);

    auto Tok2 = Tokenizer_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
        
    (P2->parsePrefix)(session, P2, Tok2);
    
    } // while (true)
#else
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == TimesParselet_parseLoop);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix)(session, P2, Tok2);
#endif // !USE_MUSTTAIL
}

void TimesParselet_reduceTimes(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new InfixNode(SYMBOL_TIMES, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}
