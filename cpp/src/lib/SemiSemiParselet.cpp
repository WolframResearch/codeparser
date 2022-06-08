
#include "Parselet.h"
#include "ParseletRegistration.h" // for prefixParselets
#include "API.h" // for ParserSession
#include "Symbol.h"
#include "MyString.h"


//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

//
// FIXME: properly place implicit Times tokens within Spans
//
// This is complicated to get right and has minimal benefits
//
// Here are some tests that will help
//
//TestMatch[
//          CodeConcreteParse["{;;\n;;}"]
//          ,
//          ContainerNode[String, {
//                        GroupNode[List, {
//                                  LeafNode[Token`OpenCurly, "{", <|Source -> {{1, 1}, {1, 2}}|>],
//                                  InfixNode[Times, {
//                                            BinaryNode[Span, {
//                                                       LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 2}, {1, 2}}|>],
//                                                       LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 2}, {1, 4}}|>],
//                                                       LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 4}, {1, 4}}|>]}, <|Source -> {{1, 2}, {1, 4}}|>],
//                                            LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 4}, {1, 4}}|>],
//                                            LeafNode[Token`Newline, "\n", <|Source -> {{1, 4}, {2, 1}}|>],
//                                            BinaryNode[Span, {
//                                                       LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{2, 1}, {2, 1}}|>],
//                                                       LeafNode[Token`SemiSemi, ";;", <|Source -> {{2, 1}, {2, 3}}|>],
//                                                       LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{2, 3}, {2, 3}}|>]}, <|Source -> {{2, 1}, {2, 3}}|>]}, <|Source -> {{1, 2}, {2, 3}}|>],
//                                  LeafNode[Token`CloseCurly, "}", <|Source -> {{2, 3}, {2, 4}}|>]}, <|Source -> {{1, 1}, {2, 4}}|>]}, _]
//          ,
//          TestID->"Concrete-20200608-O6T8X7"
//          ]
//
//TestMatch[
//          CodeConcreteParse["\\[Integral] ;; ;; ;; x \\[DifferentialD] x"]
//          ,
//          ContainerNode[String, {
//                        PrefixNode[Integral, {
//                                   LeafNode[Token`LongName`Integral, "\\[Integral]", <|Source -> {{1, 1}, {1, 12}}|>],
//                                   LeafNode[Whitespace, " ", <|Source -> {{1, 12}, {1, 13}}|>],
//                                   InfixNode[Times, {
//                                             BinaryNode[Span, {
//                                                        LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 13}, {1, 13}}|>],
//                                                        LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 13}, {1, 15}}|>],
//                                                        LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 15}, {1, 15}}|>]}, <|Source -> {{1, 13}, {1, 15}}|>],
//                                             LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 15}, {1, 15}}|>],
//                                             LeafNode[Whitespace, " ", <|Source -> {{1, 15}, {1, 16}}|>],
//                                             TernaryNode[Span, {
//                                                         LeafNode[Token`Fake`ImplicitOne, "", <|Source -> {{1, 16}, {1, 16}}|>],
//                                                         LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 16}, {1, 18}}|>],
//                                                         LeafNode[Token`Fake`ImplicitAll, "", <|Source -> {{1, 18}, {1, 18}}|>],
//                                                         LeafNode[Whitespace, " ", <|Source -> {{1, 18}, {1, 19}}|>],
//                                                         LeafNode[Token`SemiSemi, ";;", <|Source -> {{1, 19}, {1, 21}}|>],
//                                                         LeafNode[Whitespace, " ", <|Source -> {{1, 21}, {1, 22}}|>],
//                                                         LeafNode[Symbol, "x", <|Source -> {{1, 22}, {1, 23}}|>]}, <|Source -> {{1, 16}, {1, 23}}|>],
//                                             LeafNode[Token`Fake`ImplicitTimes, "", <|Source -> {{1, 23}, {1, 23}}|>],
//                                             LeafNode[Whitespace, " ", <|Source -> {{1, 23}, {1, 24}}|>],
//                                             PrefixNode[DifferentialD, {
//                                                        LeafNode[Token`LongName`DifferentialD, "\\[DifferentialD]", <|Source -> {{1, 24}, {1, 40}}|>],
//                                                        LeafNode[Whitespace, " ", <|Source -> {{1, 40}, {1, 41}}|>],
//                                                        LeafNode[Symbol, "x", <|Source -> {{1, 41}, {1, 42}}|>]}, <|Source -> {{1, 24}, {1, 42}}|>]}, <|Source -> {{1, 13}, {1, 42}}|>]}, <|Source -> {{1, 1}, {1, 42}}|>]}, _]
//          ,
//          TestID->"Concrete-20200608-R4X7L4"
//          ]


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
    
    auto& Args = TheParser->peekArgs();
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->pushInheritedContext(PRECEDENCE_SEMISEMI);
    
//    xxx;
    SemiSemiParselet_parse0(P, Token());
    
    MUSTTAIL
    return SemiSemiParselet_parse6(P, Token());
}

void SemiSemiParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok;
    
    {
        TriviaSeq Trivia2;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (!Tok.Tok.isPossibleBeginning()) {

        //
        // We are done, so return
        //
        
        {
            auto Args = TheParser->popArgs();
            
            auto Operand = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
            
            TheParser->pushNode(std::move(Operand));
        }
        
        TheParser->popContext();
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
    }
    
    if (Tok.Tok != TOKEN_SEMISEMI) {

        //
        // Something like  \[Integral];;;;;;x\[DifferentialD]x
        //

        //
        // Lower precedence, so this is just a general expression
        //
        // Must also handle  a;;!b  where there is an Implicit Times, but only a single Span
        //
        
        auto ImplicitTimes = Token(TOKEN_FAKE_IMPLICITTIMES, Tok.BufLen.buffer, Tok.Src.Start);
        
        Args.append(NodePtr(new LeafNode(ImplicitTimes)));
        
#if !NISSUES
        {
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, Tok.Src, 0.75, {}, {}));

            TheParserSession->addIssue(std::move(I));
        }
#endif // !NISSUES
        
        auto P2 = prefixParselets[Tok.Tok.value()];
        
//        xxx;
        (P2->parsePrefix())(P2, Tok);
        
        MUSTTAIL
        return SemiSemiParselet_parse1(P, Ignored);
    }

    //
    // Still within the ;;
    //

    auto ImplicitTimes = Token(TOKEN_FAKE_IMPLICITTIMES, Tok.BufLen.buffer, Tok.Src.Start);
    
    Args.append(NodePtr(new LeafNode(ImplicitTimes)));
    
#if !NISSUES
    {
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, Tok.Src, 0.75, {}, {}));

        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    auto ImplicitOne = Token(TOKEN_FAKE_IMPLICITONE, Tok.BufLen.buffer, Tok.Src.Start);
    
    TheParser->pushNode(NodePtr(new LeafNode(ImplicitOne)));
    
    TheParser->nextToken(Tok);
    
    auto& Args2 = TheParser->pushArgs();
    
    TheParser->shift();
    
    Args2.append(NodePtr(new LeafNode(Tok)));
    
//    xxx;
    SemiSemiParselet_parse0(P, Ignored);
    
    MUSTTAIL
    return SemiSemiParselet_parse7(P, Ignored);
}

void SemiSemiParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        auto Operand = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(std::move(Operand));

        //
        // We are done here, so return
        //

        auto Operand2 = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
        
        TheParser->pushNode(std::move(Operand2));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void SemiSemiParselet_parse0(ParseletPtr P, Token Ignored) {
    
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
        
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Span));
        
        return;
    }
    
    if (SecondTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b
        //    ^SecondTok
        //
        
        auto P2 = prefixParselets[SecondTok.Tok.value()];
        
//        xxx;
        (P2->parsePrefix())(P2, SecondTok);
        
        MUSTTAIL
        return SemiSemiParselet_parse3(P, Ignored);
    }
    
    //
    // a;;;;
    //    ^~SecondTok
    //
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, SecondTok.BufLen.buffer, SecondTok.Src.Start);
    
    Args.append(NodePtr(new LeafNode(Implicit)));
    
    //
    // SCOPED
    //
    ScopedLeafNode SecondTokNode = ScopedLeafNode(SecondTok);
    
    TheParser->nextToken(SecondTok);
    
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
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Span));
        
        return;
    }
    
    if (ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;;;b
        //      ^ThirdTok
        //
        
        Args.append(NodePtr(new LeafNode(SecondTok)));
        Args.appendSeq(std::move(Trivia2));
        
        auto P2 = prefixParselets[ThirdTok.Tok.value()];
        
//        xxx;
        (P2->parsePrefix())(P2, ThirdTok);
        
//        MUSTTAIL probably not doable
        return SemiSemiParselet_parse4(P, Ignored);
    }
    
    //
    // a;;;;;;
    //      ^~ThirdTok
    //
    
    Trivia2.reset();
    SecondTokNode.reset();
    
    auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
    
    TheParser->popArgs();
    
    TheParser->pushNode(std::move(Span));
    
    return;
}

void SemiSemiParselet_parse3(ParseletPtr P, Token Ignored) {
    
    auto& Args = TheParser->peekArgs();
    
    {
        auto SecondTokNode = TheParser->popNode();
        
        Args.append(std::move(SecondTokNode));
    }
    
    Token ThirdTok;
    {
        TriviaSeq Trivia2;
        
        ThirdTok = TheParser->currentToken(TOPLEVEL);
        ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (!ThirdTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;b&
        //     ^ThirdTok
        //
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Span));
        
        return;
    }
    
    if (ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // \[Integral];;x\[DifferentialD]x
        //               ^~~~~~~~~~~~~~~~ThirdTok
        //
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Span));
        
        return;
    }
    
    //
    // a;;b;;
    //     ^~ThirdTok
    //
    Token FourthTok;
    {
        ScopedLeafNode ThirdTokNode = ScopedLeafNode(ThirdTok);
        
        TheParser->nextToken(ThirdTok);
        
        TriviaSeq Trivia3;
        
        FourthTok = TheParser->currentToken(TOPLEVEL);
        FourthTok = TheParser->eatTriviaButNotToplevelNewlines(FourthTok, TOPLEVEL, Trivia3);
        
        if (!FourthTok.Tok.isPossibleBeginning() || FourthTok.Tok == TOKEN_SEMISEMI) {
            
            //
            // a;;b;;&
            //       ^FourthTok
            //
            //
            // a;;b;;;;
            //       ^~FourthTok
            //
            
            Trivia3.reset();
            ThirdTokNode.reset();
            
            auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(Span));
            
            return;
        }
            
        //
        // a;;b;;c
        //       ^FourthTok
        //
        
        Args.append(NodePtr(new LeafNode(ThirdTok)));
        Args.appendSeq(std::move(Trivia3));
    }

    auto P2 = prefixParselets[FourthTok.Tok.value()];
    
//    xxx;
    (P2->parsePrefix())(P2, FourthTok);
    
    MUSTTAIL
    return SemiSemiParselet_parse4(P, Ignored);
}

void SemiSemiParselet_parse4(ParseletPtr P, Token Ignored) {
    
    auto ThirdTokNode = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(ThirdTokNode));
    
    auto Span = NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
    
    TheParser->pushNode(std::move(Span));
    
    return;
}

void SemiSemiParselet_parse6(ParseletPtr P, Token Ignored) {
    
    {
        TriviaSeq Trivia1;
        
        auto Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, TOPLEVEL, Trivia1);
        
        if (!Tok.Tok.isPossibleBeginning()) {
            
            //
            // There is only a single ;; and there is no implicit Times
            //
            
            Trivia1.reset();
            
            TheParser->popContext();
            
//            MUSTTAIL probably not doable
            return Parser_parseClimb(nullptr, Ignored);
        }
        
        if (Tok.Tok != TOKEN_SEMISEMI) {
            
            //
            // \[Integral];;x\[DifferentialD]x
            //
            
            Trivia1.reset();
            
            TheParser->popContext();
            
//            MUSTTAIL probably not doable
            return Parser_parseClimb(nullptr, Ignored);
        }
        
        auto& Args2 = TheParser->pushArgs();
        
        TheParser->shift();
        
        Args2.appendSeq(std::move(Trivia1));
    }
    
    MUSTTAIL
    return SemiSemiParselet_parseLoop(P, Token());
}

void SemiSemiParselet_parse7(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        auto Operand = TheParser->popNode();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return SemiSemiParselet_parseLoop(P, Ignored);
}
