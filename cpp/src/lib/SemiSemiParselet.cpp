
#include "Parselet.h"
#include "ParseletRegistration.h" // for prefixParselets
#include "API.h" // for ParserSession
#include "Symbol.h"
#include "MyString.h"
#include "Parser.h"

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

Precedence SemiSemiParselet::getPrecedence() const {
    return PRECEDENCE_SEMISEMI;
}

Token SemiSemiParselet::processImplicitTimes(Token TokIn) const {
    
    //
    // SemiSemi was already parsed with look-ahead with the assumption that implicit Times will be handled correctly
    //
    
    if (TheParser->getNodeStackSize() > 0) {
    
        auto& N = TheParser->topNode();
        
        if (auto B = dynamic_cast<BinaryNode *>(N.get())) {
            
            auto Op = B->getOp();
            
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
            
            auto Op = T->getOp();
            
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
    
    auto& Args = TheParser->pushArgs(PRECEDENCE_SEMISEMI);
    
    Args.append(NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITONE, TokIn.BufLen.buffer, TokIn.Src.Start))));
    
    MUSTTAIL
    return SemiSemiParselet_parseInfix(P, TokIn);
}

ParseFunction SemiSemiParselet::parseInfix() const {
    return SemiSemiParselet_parseInfix;
}

void SemiSemiParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    MUSTTAIL
    return SemiSemiParselet_parse1(P, TokIn/*ignored*/);
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
        return SemiSemiParselet_reduceBinary(P, Ignored);
    }
    
    if (SecondTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b
        //    ^SecondTok
        //
        
        assert(Args.F == nullptr);
        assert(Args.P == nullptr);
        Args.F = SemiSemiParselet_parse2;
        Args.P = P;
        
        auto P2 = prefixParselets[SecondTok.Tok.value()];
        
        MUSTTAIL
        return (P2->parsePrefix())(P2, SecondTok);
    }
    
    //
    // a;;;;
    //    ^~SecondTok
    //
    
    Token ThirdTok;
    
    {
        TheParser->pushNode(NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITALL, SecondTok.BufLen.buffer, SecondTok.Src.Start))));
        
        TheParser->nextToken(SecondTok);
        
        TriviaSeq Trivia2;
        
        ThirdTok = TheParser->currentToken(TOPLEVEL);
        ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, TOPLEVEL, Trivia2);
        
        if (!ThirdTok.Tok.isPossibleBeginning() || ThirdTok.Tok == TOKEN_SEMISEMI) {
            
            Trivia2.reset();
            SecondTok.reset();
            
        } else {
            
            TheParser->shift();
            
            Args.append(NodePtr(new LeafNode(SecondTok)));
            
            Args.appendSeq(std::move(Trivia2));
        }
    }
    
    if (!ThirdTok.Tok.isPossibleBeginning() || ThirdTok.Tok == TOKEN_SEMISEMI) {
        
        //
        // a;;;;&
        //      ^ThirdTok
        //
        
        //
        // a;;;;;;
        //      ^~ThirdTok
        //
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(P, Ignored);
    }
    
    //
    // a;;;;b
    //      ^ThirdTok
    //
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = SemiSemiParselet_reduceTernary;
    Args.P = P;
    
    auto P2 = prefixParselets[ThirdTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, ThirdTok);
}

void SemiSemiParselet_parse2(ParseletPtr P, Token Ignored) {
    
    auto& Args = TheParser->peekArgs();
    
    Token ThirdTok;
    
    Token FourthTok;
    
    {
        TriviaSeq Trivia2;
        
        ThirdTok = TheParser->currentToken(TOPLEVEL);
        ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, TOPLEVEL, Trivia2);
        
        if (!ThirdTok.Tok.isPossibleBeginning() || ThirdTok.Tok != TOKEN_SEMISEMI) {
            
            Trivia2.reset();
            
        } else {
            
            TheParser->nextToken(ThirdTok);
            
            TriviaSeq Trivia3;
            
            FourthTok = TheParser->currentToken(TOPLEVEL);
            FourthTok = TheParser->eatTriviaButNotToplevelNewlines(FourthTok, TOPLEVEL, Trivia3);
            
            if (!FourthTok.Tok.isPossibleBeginning() || FourthTok.Tok == TOKEN_SEMISEMI) {
                
                Trivia3.reset();
                ThirdTok.reset();
                Trivia2.reset();
                
            } else {
                
                TheParser->shift();
                
                Args.appendSeq(std::move(Trivia2));
                
                Args.append(NodePtr(new LeafNode(ThirdTok)));
                
                Args.appendSeq(std::move(Trivia3));
            }
        }
    }
    
    if (!ThirdTok.Tok.isPossibleBeginning() || ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b&
        //     ^ThirdTok
        //
        
        //
        // \[Integral];;x\[DifferentialD]x
        //               ^~~~~~~~~~~~~~~~ThirdTok
        //
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(P, Ignored);
    }
    
    //
    // a;;b;;
    //     ^~ThirdTok
    //
    
    if (!FourthTok.Tok.isPossibleBeginning() || FourthTok.Tok == TOKEN_SEMISEMI) {
        
        //
        // a;;b;;&
        //       ^FourthTok
        //
        
        //
        // a;;b;;;;
        //       ^~FourthTok
        //
        
        MUSTTAIL
        return SemiSemiParselet_reduceBinary(P, Ignored);
    }
        
    //
    // a;;b;;c
    //       ^FourthTok
    //
    
    assert(Args.F == SemiSemiParselet_parse2);
    assert(Args.P == P);
    Args.F = SemiSemiParselet_reduceTernary;
    Args.P = P;
    
    auto P2 = prefixParselets[FourthTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, FourthTok);
}

void SemiSemiParselet_reduceBinary(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(NodePtr(new BinaryNode(SYMBOL_SPAN, TheParser->popArgs())));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void SemiSemiParselet_reduceTernary(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(NodePtr(new TernaryNode(SYMBOL_SPAN, TheParser->popArgs())));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}
