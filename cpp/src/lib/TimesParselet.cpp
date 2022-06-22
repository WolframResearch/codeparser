
#include "Parselet.h"

#include "ParserSession.h"
#include "Parser.h"
#include "ParseletRegistration.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


Symbol TimesParselet::getOp() const {
    return SYMBOL_TIMES;
}

Precedence TimesParselet::getPrecedence() const {
    return PRECEDENCE_STAR;
}

ParseFunction TimesParselet::parseInfix() const {
    return TimesParselet_parseInfix;
}

void TimesParselet_parseInfix(ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        TheParser->popContext();
        TheParser->pushNode(new AbortNode());
        return Parser_tryContinue(Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    
    TheParser->eatTrivia(Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    Ctxt.F = Parser_identity;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2);
    
    return TimesParselet_parseLoop(Ignored, TokIn/*ignored*/);
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    Ctxt.F = TimesParselet_parseLoop;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void TimesParselet_parseLoop(ParseletPtr Ignored, Token Ignored2) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        TheParser->popNode();
        TheParser->popContext();
        TheParser->pushNode(new AbortNode());
        return Parser_tryContinue(Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    auto Tok1 = TheParser->currentToken(TOPLEVEL);
    TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia1);
    
    auto I = infixParselets[Tok1.Tok.value()];

    Tok1 = I->processImplicitTimes(Tok1);

    if (Tok1.Tok == TOKEN_FAKE_IMPLICITTIMES) {

        //
        // implicit Times should not cross toplevel newlines
        //
        // so reset and try again
        //

        Trivia1.reset();

        Tok1 = TheParser->currentToken(TOPLEVEL);

        TheParser->eatTriviaButNotToplevelNewlines(Tok1, TOPLEVEL, Trivia1);

        I = infixParselets[Tok1.Tok.value()];

        Tok1 = I->processImplicitTimes(Tok1);
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
        
        Trivia1.reset();
        
        MUSTTAIL
        return TimesParselet_reduceTimes(Ignored, Ignored2);
    }
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    TheParser->appendLeafArgAndNext(Tok1);

    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    
    TheParser->eatTrivia(Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
        
    (P2->parsePrefix())(P2, Tok2);
    
    } // while (true)
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == TimesParselet_parseLoop);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void TimesParselet_reduceTimes(ParseletPtr Ignored, Token Ignored2) {
    
    TheParser->shift();
    
    TheParser->pushNode(new InfixNode(SYMBOL_TIMES, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(Ignored, Ignored2);
}
