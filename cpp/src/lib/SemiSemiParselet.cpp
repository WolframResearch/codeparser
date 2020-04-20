
#include "Parselet.h"
#include "ParseletRegistration.h"

#include "API.h" // for ParserSession

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

//
// prefix
//
// Parses a run of multiple Span expressions
//
// A run is anything like  ;;;;x;;y;;;;
//
// Multiple Span expressions are ImplicitTimes together
//
// Must also handle  ;;!b  where there is an implicit Times, but only a single Span
//
NodePtr SemiSemiParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, BufferAndLength(TokIn.BufLen.buffer), Source(TokIn.Src.Start));
    
    auto One = NodePtr(new LeafNode(Implicit));
    
    NodeSeq Left(1);
    Left.append(std::move(One));
    
    return parse(std::move(Left), TokIn, Ctxt);
}

//
// infix
//
// Parses a run of multiple Span expressions
//
// A run is anything like  a;;;;x;;y;;;;
//
// Multiple Span expressions are ImplicitTimes together
//
// Must also handle  a;;!b  where there is an implicit Times, but only a single Span
//
NodePtr SemiSemiParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    auto Operand = parse0(std::move(Left), TokIn, Ctxt);
    {
        LeafSeq Trivia1;
        
        auto Tok = TheParser->currentToken(Ctxt);
        Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, Trivia1);
        
        if (!Tok.Tok.isPossibleBeginning()) {
            
            //
            // There is only a single ;; and there is no implicit Times
            //
            
            goto retParse;
        }
        
        NodeSeq Args(1 + 1);
        Args.append(std::move(Operand));
        Args.appendIfNonEmpty(std::move(Trivia1));
        
        while (true) {
            
#if !NABORT
            //
            // Check isAbort() inside loops
            //
            if (TheParserSession->isAbort()) {
                
                return TheParserSession->handleAbort();
            }
#endif // !NABORT
            
            {
                LeafSeq Trivia2;
                
                auto Tok = TheParser->currentToken(Ctxt);
                Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, Trivia2);
                
                if (!Tok.Tok.isPossibleBeginning()) {
                    
                    //
                    // We are done, so return
                    //
                    
                    Operand = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
                    
                    goto retParse;
                }
                
                auto lowerPrec = false;
                
                if (prefixParselets[Tok.Tok.value()]->getPrecedence(Ctxt) >= PRECEDENCE_SEMISEMI) {
                    
                    //
                    // Higher precedence, so still within the ;;
                    //
                    
                    auto ImplicitOne = Token(TOKEN_FAKE_IMPLICITONE, BufferAndLength(Tok.BufLen.buffer), Source(Tok.Src.Start));
                    
                    NodeSeq ImplicitOneSeq;
                    ImplicitOneSeq.append(NodePtr(new LeafNode(ImplicitOne)));
                    
                    Operand = parse0(std::move(ImplicitOneSeq), Tok, Ctxt);
                    
                    lowerPrec = false;
                    
                } else {
                    
                    //
                    // Lower precedence, so this is just a general expression
                    //
                    // Must also handle  a;;!b  where there is an Implicit Times, but only a single Span
                    //
                    
                    Operand = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
                    
                    lowerPrec = true;
                }
                
#if !NISSUES
                auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDIMPLICITTIMES, "Unexpected implicit ``Times`` between ``Spans``.", SYNTAXISSUESEVERITY_WARNING, Tok.Src, 0.75, {}));
                
                TheParser->addIssue(std::move(I));
#endif // !NISSUES
                
                auto ImplicitTimes = Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(Tok.BufLen.buffer), Source(Tok.Src.Start));
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(Trivia2));
                Args.append(NodePtr(new LeafNode(ImplicitTimes)));
                Args.append(std::move(Operand));
                
                if (lowerPrec) {
                    
                    //
                    // We are done here, so return
                    //
                    
                    Operand = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
                    
                    goto retParse;
                }
            }
            
        } // while
    }
    
retParse:
    return TheParser->infixLoop(std::move(Operand), Ctxt);
}

//
// infix
//
// Something like  a;;b
//
// Parses a single complete Span
//
NodePtr SemiSemiParselet::parse0(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = PRECEDENCE_SEMISEMI;
    
    TheParser->nextToken(TokIn);
    
    auto SecondTok = TheParser->currentToken(Ctxt);
    
    {
        LeafSeq Trivia1;
        
        SecondTok = TheParser->eatTriviaButNotToplevelNewlines(SecondTok, Ctxt, Trivia1);
        
        //
        // a;;
        //  ^~TokIn
        //
        
        if (!SecondTok.Tok.isPossibleBeginning()) {
            
            //
            // a;;&
            //    ^SecondTok
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
            
            NodeSeq Args(1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
            return NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        }
        
        if (SecondTok.Tok != TOKEN_SEMISEMI) {
            
            //
            // a;;b
            //    ^SecondTok
            //
            
            auto FirstArg = prefixParselets[SecondTok.Tok.value()]->parse(SecondTok, Ctxt);
            
            {
                LeafSeq Trivia2;
                
                auto ThirdTok = TheParser->currentToken(Ctxt);
                ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, Ctxt, Trivia2);
                
                if (ThirdTok.Tok != TOKEN_SEMISEMI) {
                    
                    //
                    // a;;b&
                    //     ^ThirdTok
                    //
                    
                    NodeSeq Args(1 + 1 + 1 + 1);
                    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                    Args.append(NodePtr(new LeafNode(TokIn)));
                    Args.appendIfNonEmpty(std::move(Trivia1));
                    Args.append(std::move(FirstArg));
                    
                    return NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                }
                
                //
                // a;;b;;
                //     ^~ThirdTok
                //
                
                {
                    // for RAII
                    LeafSeq ThirdTokSeq;
                    ThirdTokSeq.append(LeafNodePtr(new LeafNode(ThirdTok)));
                    
                    LeafSeq Trivia3;
                    
                    TheParser->nextToken(ThirdTok);
                    
                    auto FourthTok = TheParser->currentToken(Ctxt);
                    FourthTok = TheParser->eatTriviaButNotToplevelNewlines(FourthTok, Ctxt, Trivia3);
                    
                    if (!FourthTok.Tok.isPossibleBeginning()) {
                        
                        //
                        // a;;b;;&
                        //       ^FourthTok
                        //
                        
                        NodeSeq Args(1 + 1 + 1 + 1);
                        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                        Args.append(NodePtr(new LeafNode(TokIn)));
                        Args.appendIfNonEmpty(std::move(Trivia1));
                        Args.append(std::move(FirstArg));
                        
                        return NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    }
                    
                    if (FourthTok.Tok != TOKEN_SEMISEMI) {
                        
                        //
                        // a;;b;;c
                        //       ^FourthTok
                        //
                        
                        auto SecondArg = prefixParselets[FourthTok.Tok.value()]->parse(FourthTok, Ctxt);
                        
                        NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1);
                        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                        Args.append(NodePtr(new LeafNode(TokIn)));
                        Args.appendIfNonEmpty(std::move(Trivia1));
                        Args.append(std::move(FirstArg));
                        Args.appendIfNonEmpty(std::move(Trivia2));
                        Args.append(NodePtr(new LeafSeqNode(std::move(ThirdTokSeq))));
                        Args.appendIfNonEmpty(std::move(Trivia3));
                        Args.append(std::move(SecondArg));
                        
                        return NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                    }
                    
                    //
                    // a;;b;;;;
                    //       ^~FourthTok
                    //
                    
                    NodeSeq Args(1 + 1 + 1 + 1);
                    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                    Args.append(NodePtr(new LeafNode(TokIn)));
                    Args.appendIfNonEmpty(std::move(Trivia1));
                    Args.append(std::move(FirstArg));
                    
                    return NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                }
            }
        }
        
        //
        // a;;;;
        //    ^~SecondTok
        //
        
        {
            // for RAII
            LeafSeq SecondTokSeq;
            SecondTokSeq.append(LeafNodePtr(new LeafNode(SecondTok)));
            
            LeafSeq Trivia2;
            
            TheParser->nextToken(SecondTok);
            
            auto ThirdTok = TheParser->currentToken(Ctxt);
            ThirdTok = TheParser->eatTriviaButNotToplevelNewlines(ThirdTok, Ctxt, Trivia2);
            
            if (!ThirdTok.Tok.isPossibleBeginning()) {
                
                //
                // a;;;;&
                //      ^ThirdTok
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
                
                NodeSeq Args(1 + 1 + 1);
                Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
                return NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            }
            
            if (ThirdTok.Tok != TOKEN_SEMISEMI) {
                
                //
                // a;;;;b
                //      ^ThirdTok
                //
                
                auto FirstArg = prefixParselets[ThirdTok.Tok.value()]->parse(ThirdTok, Ctxt);
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
                
                NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1 + 1);
                Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                Args.appendIfNonEmpty(std::move(Trivia1));
                Args.append(NodePtr(new LeafSeqNode(std::move(SecondTokSeq))));
                Args.appendIfNonEmpty(std::move(Trivia2));
                Args.append(std::move(FirstArg));
                
                return NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
            }
            
            //
            // a;;;;;;
            //      ^~ThirdTok
            //
            
            {
                LeafSeq Trivia3;
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
                
                NodeSeq Args(1 + 1 + 1);
                Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
                return NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            }
        }
    }
}
