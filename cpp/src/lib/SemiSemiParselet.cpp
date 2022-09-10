
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

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

SemiSemiParselet::SemiSemiParselet() : PrefixParselet(SemiSemiParselet_parsePrefix), InfixParselet(SemiSemiParselet_parseInfix) {}

Precedence SemiSemiParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_SEMISEMI;
}

Token SemiSemiParselet::processImplicitTimes(ParserSessionPtr session, Token TokIn) const {
    
    //
    // SemiSemi was already parsed with look-ahead with the assumption that implicit Times will be handled correctly
    //
    
    if (Parser_checkSpan(session)) {
        return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.Buf, 0, Source(TokIn.Src.Start));
    }
    
    return TokIn;
}

void SemiSemiParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITONE, TokIn.Buf, 0, Source(TokIn.Src.Start)));
    
    Parser_pushContext(session, PRECEDENCE_SEMISEMI);
    
    //
    // nextToken() is not needed after an implicit token
    //
    
    MUSTTAIL
    return SemiSemiParselet_parseInfix(session, Ignored, TokIn);
}

void SemiSemiParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    MUSTTAIL
    return SemiSemiParselet_parse1(session, Ignored, TokIn/*ignored*/);
}

void SemiSemiParselet_parse1(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto SecondTok = Tokenizer_currentToken(session, TOPLEVEL);
    
    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, SecondTok, TOPLEVEL);
    
    //
    // a;;
    //  ^~TokIn
    //
    
    if (!SecondTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;&
        //    ^SecondTok
        //
        
        Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITALL, SecondTok.Buf, 0, Source(SecondTok.Src.Start)));
        
        //
        // nextToken() is not needed after an implicit token
        //
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(session, Ignored, Ignored2);
    }
    
    if (SecondTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b
        //    ^SecondTok
        //
        
        auto& Ctxt = Parser_topContext(session);
        assert(!Ctxt.F);
        Ctxt.F = SemiSemiParselet_parse2;
        
        auto P2 = prefixParselets[SecondTok.Tok.value()];
        
        MUSTTAIL
        return (P2->parsePrefix)(session, P2, SecondTok);
    }
    
    //
    // a;;;;
    //    ^~SecondTok
    //
    
    Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITALL, SecondTok.Buf, 0, Source(SecondTok.Src.Start)));
    
    SecondTok.skip(session);
    
    auto ThirdTok = Tokenizer_currentToken(session, TOPLEVEL);
    
    auto& Trivia1 = session->trivia1;
    
    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, ThirdTok, TOPLEVEL, Trivia1);
    
    if (!ThirdTok.Tok.isPossibleBeginning() || ThirdTok.Tok == TOKEN_SEMISEMI) {
        
        //
        // a;;;;&
        //      ^ThirdTok
        //
        
        //
        // a;;;;;;
        //      ^~ThirdTok
        //
        
        Trivia1.reset(session);
        SecondTok.reset(session);
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(session, Ignored, Ignored2);
    }
    
    //
    // a;;;;b
    //      ^ThirdTok
    //
    
    Parser_pushLeaf(session, SecondTok);
    
    //
    // nextToken() already handled above
    //
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = SemiSemiParselet_reduceTernary;
    
    auto P2 = prefixParselets[ThirdTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix)(session, P2, ThirdTok);
}

void SemiSemiParselet_parse2(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = session->trivia1;
    
    auto ThirdTok = Tokenizer_currentToken(session, TOPLEVEL);
    
    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, ThirdTok, TOPLEVEL, Trivia1);
    
    if (!ThirdTok.Tok.isPossibleBeginning() || ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b&
        //     ^ThirdTok
        //
        
        //
        // \[Integral];;x\[DifferentialD]x
        //               ^~~~~~~~~~~~~~~~ThirdTok
        //
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(session, Ignored, Ignored2);
    }
    
    //
    // a;;b;;
    //     ^~ThirdTok
    //
    
    ThirdTok.skip(session);
    
    auto& Trivia2 = session->trivia2;
    
    auto FourthTok = Tokenizer_currentToken(session, TOPLEVEL);
    
    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, FourthTok, TOPLEVEL, Trivia2);
    
    if (!FourthTok.Tok.isPossibleBeginning() || FourthTok.Tok == TOKEN_SEMISEMI) {
        
        //
        // a;;b;;&
        //       ^FourthTok
        //
        
        //
        // a;;b;;;;
        //       ^~FourthTok
        //
        
        Trivia2.reset(session);
        ThirdTok.reset(session);
        Trivia1.reset(session);
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(session, Ignored, Ignored2);
    }
        
    //
    // a;;b;;c
    //       ^FourthTok
    //
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeaf(session, ThirdTok);
    
    //
    // nextToken() already handled above
    //
    
    Parser_pushTriviaSeq(session, Trivia2);
    
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == SemiSemiParselet_parse2);
    Ctxt.F = SemiSemiParselet_reduceTernary;
    
    auto P2 = prefixParselets[FourthTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix)(session, P2, FourthTok);
}

void SemiSemiParselet_reduceBinary(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_SPAN, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void SemiSemiParselet_reduceTernary(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new TernaryNode(SYMBOL_SPAN, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}
