
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

void SemiSemiParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    auto& Left = TheParser->pushArgs();
    Left.append(NodePtr(new LeafNode(Implicit)));
    
    MUSTTAIL
    return SemiSemiParselet_parseInfix(P, TokIn, Ctxt);
}


ParseFunction SemiSemiParselet::parseInfix() const {
    return SemiSemiParselet_parseInfix;
}

void SemiSemiParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    {
        SemiSemiParselet_parse0(P, TokIn, Ctxt);
            
        TriviaSeq Trivia1;
        
        auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        if (!Tok.Tok.isPossibleBeginning()) {
            
            //
            // There is only a single ;; and there is no implicit Times
            //
            
            Trivia1.reset();
            
            return Parser_parseLoop(nullptr, Ctxt);
        }
        
        if (Tok.Tok != TOKEN_SEMISEMI) {
            
            //
            // \[Integral];;x\[DifferentialD]x
            //
            
            Trivia1.reset();
            
            return Parser_parseLoop(nullptr, Ctxt);
        }
        
        auto Operand = TheParser->popNode();
        
        auto& Args2 = TheParser->pushArgs();
        Args2.append(std::move(Operand));
        Args2.appendSeq(std::move(Trivia1));
    }
    
    return SemiSemiParselet_parseLoop(P, Ctxt);
}

void SemiSemiParselet_parseLoop(ParseletPtr P, ParserContext Ctxt) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    {
        TriviaSeq Trivia2;
        
        Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, TOPLEVEL, Trivia2);

        if (!Tok.Tok.isPossibleBeginning()) {

            //
            // We are done, so return
            //
            
            auto Args = TheParser->popArgs();
            
            auto Operand = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));

            Trivia2.reset();
            
            TheParser->pushNode(std::move(Operand));
            
//            MUSTTAIL untangle
            return Parser_parseLoop(nullptr, Ctxt);
        }
        
        Args.appendSeq(std::move(Trivia2));
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
        
#if !NISSUES
        {
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, Tok.Src, 0.75, {}, {}));

            TheParserSession->addIssue(std::move(I));
        }
#endif // !NISSUES
        
        auto ImplicitTimes = Token(TOKEN_FAKE_IMPLICITTIMES, Tok.BufLen.buffer, Tok.Src.Start);
        
        Args.append(NodePtr(new LeafNode(ImplicitTimes)));
        
        auto P2 = prefixParselets[Tok.Tok.value()];
        
        (P2->parsePrefix())(P2, Tok, Ctxt);
        
        MUSTTAIL
        return SemiSemiParselet_parse1(P, Ctxt);
    }

    //
    // Still within the ;;
    //

    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, Tok.BufLen.buffer, Tok.Src.Start);

    auto& Seq = TheParser->pushArgs();
    Seq.append(NodePtr(new LeafNode(Implicit)));
    
#if !NISSUES
    {
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", STRING_WARNING, Tok.Src, 0.75, {}, {}));

        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES

    auto ImplicitTimes = Token(TOKEN_FAKE_IMPLICITTIMES, Tok.BufLen.buffer, Tok.Src.Start);
    
    Args.append(NodePtr(new LeafNode(ImplicitTimes)));
    
    SemiSemiParselet_parse0(P, Tok, Ctxt);
    
    {
        auto Operand = TheParser->popNode();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return SemiSemiParselet_parseLoop(P, Ctxt);
}

void SemiSemiParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
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
    return Parser_parseLoop(nullptr, CtxtIn);
}

void SemiSemiParselet_parse0(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    auto& Args = TheParser->peekArgs();
    
    Ctxt.Prec = PRECEDENCE_SEMISEMI;
    
    TheParser->nextToken(TokIn);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    auto SecondTok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    {
        TriviaSeq Trivia1;

        SecondTok = TheParser->eatTriviaButNotToplevelNewlines(SecondTok, Ctxt, TOPLEVEL, Trivia1);

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
        
        (P2->parsePrefix())(P2, SecondTok, Ctxt);
        
        return SemiSemiParselet_parse3(P, Ctxt);
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
    
    auto ThirdTok = TheParser->currentToken(Ctxt, TOPLEVEL);
    ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, Ctxt, TOPLEVEL, Trivia2);
    
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
        
        (P2->parsePrefix())(P2, ThirdTok, Ctxt);
        
        return SemiSemiParselet_parse4(P, Ctxt);
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

void SemiSemiParselet_parse3(ParseletPtr P, ParserContext Ctxt) {
    
    auto SecondTokNode = TheParser->popNode();
    
    auto& Args = TheParser->peekArgs();
    
    Args.append(std::move(SecondTokNode));
        
    TriviaSeq Trivia2;
    
    auto ThirdTok = TheParser->currentToken(Ctxt, TOPLEVEL);
    ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
        
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
    
    ScopedLeafNode ThirdTokNode = ScopedLeafNode(ThirdTok);
    
    TriviaSeq Trivia3;
    
    TheParser->nextToken(ThirdTok);
    
    auto FourthTok = TheParser->currentToken(Ctxt, TOPLEVEL);
    FourthTok = TheParser->eatTriviaButNotToplevelNewlines(FourthTok, Ctxt, TOPLEVEL, Trivia3);
    
    if (!FourthTok.Tok.isPossibleBeginning()) {
        
        //
        // a;;b;;&
        //       ^FourthTok
        //
        
        Trivia3.reset();
        ThirdTokNode.reset();
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Span));
        
        return;
    }
    
    if (FourthTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b;;c
        //       ^FourthTok
        //
        
        Args.append(NodePtr(new LeafNode(ThirdTok)));
        Args.appendSeq(std::move(Trivia3));
        
        auto P2 = prefixParselets[FourthTok.Tok.value()];
        
        (P2->parsePrefix())(P2, FourthTok, Ctxt);
        
//        MUSTTAIL untangle
        return SemiSemiParselet_parse5(P, Ctxt);
    }
    
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

void SemiSemiParselet_parse4(ParseletPtr P, ParserContext CtxtIn) {
    
    auto ThirdTokNode = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(ThirdTokNode));
    
    auto Span = NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
    
    TheParser->pushNode(std::move(Span));
    
    return;
}

void SemiSemiParselet_parse5(ParseletPtr P, ParserContext CtxtIn) {
    
    auto FourthTokNode = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(FourthTokNode));
    
    auto Span = NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
    
    TheParser->pushNode(std::move(Span));
    
    return;
}
