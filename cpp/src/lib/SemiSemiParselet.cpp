
#include "Parselet.h"
#include "ParseletRegistration.h" // for prefixParselets
#include "API.h" // for ParserSession
#include "Symbol.h"
#include "MyString.h"


//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

Token SemiSemiParselet::processImplicitTimes(Token TokIn) const {
    
    if (TheParser->getNodeStackSize() > 0) {
    
        auto& N = TheParser->topNode();
        
        if (auto B = dynamic_cast<BinaryNode *>(N.get())) {
            
            auto& Op = B->getOp();
            
            if (Op == SYMBOL_SPAN) {
            
#if !NISSUES
            {
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, TokIn.Src, 0.75, {}, {}));

                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
                
                return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
            }
            
            //
            // there is a Node, but it is not a Span
            //
            
            return TokIn;
        }
        
        if (auto T = dynamic_cast<TernaryNode *>(N.get())) {
            
            auto& Op = T->getOp();
            
            if (Op == SYMBOL_SPAN) {
                
#if !NISSUES
            {
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, TokIn.Src, 0.75, {}, {}));

                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
                
                return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
            }
            
            //
            // there is a Node, but it is not a Span
            //
            
            return TokIn;
        }
        
        //
        // there is a Node, but it is not a Span
        //
        
        return TokIn;
    }
    
    //
    // no Node, so this means that Args has already started
    //
    
#if !NISSUES
    {
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, TokIn.Src, 0.75, {}, {}));

        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
            
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
}

ParseFunction SemiSemiParselet::parsePrefix() const {
    return SemiSemiParselet_parsePrefix;
}

void SemiSemiParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    TheParser->pushNode(NodePtr(new LeafNode(Implicit)));
    
    TheParser->pushArgs();
    
    TheParser->shift();
    
    MUSTTAIL
    return SemiSemiParselet_parseInfix(P, TokIn);
}


ParseFunction SemiSemiParselet::parseInfix() const {
    return SemiSemiParselet_parseInfix;
}

void SemiSemiParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    TheParser->pushPrecedence(PRECEDENCE_SEMISEMI);
    
    MUSTTAIL
    return SemiSemiParselet_parse1(P, Token());
}

void SemiSemiParselet_parse1(ParseletPtr P, Token Ignored) {
    
    auto& Args = TheParser->peekArgs();
    
    Token SecondTok;
    {
        TriviaSeq Trivia1;
        
        SecondTok = TheParser->currentToken(TOPLEVEL);
        SecondTok = TheParser->eatTriviaButNotToplevelNewlines(SecondTok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    //
    // a;;
    //  ^~TokIn
    //
    
    if (!SecondTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;&
        //    ^SecondTok
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, SecondTok.BufLen.buffer, SecondTok.Src.Start);
        
        TheParser->pushNode(NodePtr(new LeafNode(Implicit)));
        
        MUSTTAIL
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
    
    if (SecondTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b
        //    ^SecondTok
        //
        
        auto P2 = prefixParselets[SecondTok.Tok.value()];
        
//        xxx;
        (P2->parsePrefix())(P2, SecondTok);
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parse2(P, Ignored);
    }
    
    //
    // a;;;;
    //    ^~SecondTok
    //
    
    //
    // SCOPED
    //
    ScopedLeafNode SecondTokNode = ScopedLeafNode(SecondTok);
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, SecondTok.BufLen.buffer, SecondTok.Src.Start);
    
    TheParser->nextToken(SecondTok);
    
    TheParser->pushNode(NodePtr(new LeafNode(Implicit)));
    
    TriviaSeq Trivia2;
    
    auto ThirdTok = TheParser->currentToken(TOPLEVEL);
    ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, TOPLEVEL, Trivia2);
    
    if (!ThirdTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;;;&
        //      ^ThirdTok
        //
        
        Trivia2.reset();
        SecondTokNode.reset();
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
    
    if (ThirdTok.Tok == TOKEN_SEMISEMI) {
        
        //
        // a;;;;;;
        //      ^~ThirdTok
        //
        
        Trivia2.reset();
        SecondTokNode.reset();
        
    //    MUSTTAIL probably not doable
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
    
    //
    // a;;;;b
    //      ^ThirdTok
    //
    
    TheParser->shift();
    
    TheParser->pushNode(NodePtr(new LeafNode(SecondTok)));
    
    TheParser->shift();
    
    Args.appendSeq(std::move(Trivia2));
    
    auto P2 = prefixParselets[ThirdTok.Tok.value()];
    
//        xxx;
    (P2->parsePrefix())(P2, ThirdTok);
    
//        MUSTTAIL probably not doable
    return SemiSemiParselet_parseTernary(P, Ignored);
}

void SemiSemiParselet_parse2(ParseletPtr P, Token Ignored) {
    
    auto& Args = TheParser->peekArgs();
    
    TriviaSeq Trivia2;
    
    auto ThirdTok = TheParser->currentToken(TOPLEVEL);
    ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, TOPLEVEL, Trivia2);
    
    if (!ThirdTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;b&
        //     ^ThirdTok
        //
        
        Trivia2.reset();
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
    
    if (ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // \[Integral];;x\[DifferentialD]x
        //               ^~~~~~~~~~~~~~~~ThirdTok
        //
        
        Trivia2.reset();
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
    
    //
    // a;;b;;
    //     ^~ThirdTok
    //
    
    ScopedLeafNode ThirdTokNode = ScopedLeafNode(ThirdTok);
    
    TheParser->nextToken(ThirdTok);
    
    TriviaSeq Trivia3;
    
    auto FourthTok = TheParser->currentToken(TOPLEVEL);
    FourthTok = TheParser->eatTriviaButNotToplevelNewlines(FourthTok, TOPLEVEL, Trivia3);
    
    if (!FourthTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;b;;&
        //       ^FourthTok
        //
        
        Trivia3.reset();
        ThirdTokNode.reset();
        Trivia2.reset();
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
    
    if (FourthTok.Tok == TOKEN_SEMISEMI) {
        
        //
        // a;;b;;;;
        //       ^~FourthTok
        //
        
        Trivia3.reset();
        ThirdTokNode.reset();
        Trivia2.reset();
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parseBinary(P, Ignored);
    }
        
    //
    // a;;b;;c
    //       ^FourthTok
    //
    
    TheParser->shift();
    
    Args.appendSeq(std::move(Trivia2));
    
    TheParser->pushNode(NodePtr(new LeafNode(ThirdTok)));
    
    TheParser->shift();
    
    Args.appendSeq(std::move(Trivia3));
    
    auto P2 = prefixParselets[FourthTok.Tok.value()];
    
//    xxx;
    (P2->parsePrefix())(P2, FourthTok);
    
//    MUSTTAIL probably not doable
    return SemiSemiParselet_parseTernary(P, Ignored);
}

void SemiSemiParselet_parseBinary(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    {
        auto Args = TheParser->popArgs();
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->pushNode(std::move(Span));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void SemiSemiParselet_parseTernary(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    {
        auto Args = TheParser->popArgs();
        
        auto Span = NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->pushNode(std::move(Span));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}
