
//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//
//

#include "Parselet.h"

#include "SourceManager.h"
#include "Utils.h"
#include "Symbol.h"

//
// prefix
//
// Parses a run of multiple Span expressions
//
// A run is anything like  ;;;;x;;y;;;;
//
// Multiple Span expressions are ImplicitTimes together
//
NodePtr SemiSemiParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Left;
    
    auto TokIn = TheParser->currentToken();
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, "", Source(TokIn.Src.start()));
    
    auto One = NodePtr(new LeafNode(Implicit));
    
    Left.append(std::move(One));
    
    return parse(std::move(Left), CtxtIn);
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
NodePtr SemiSemiParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {

    NodeSeq Args;
    
    auto Operand = parse0(std::move(Left), CtxtIn);
    
    //
    // LOOKAHEAD
    //
    {
        LeafSeq ArgsTest1;
    
        auto Tok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
    
        if (Tok.Tok != TOKEN_SEMISEMI) {
            
            return Operand;
        }

        Args.reserve(1 + 1);
        Args.append(std::move(Operand));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        
        while (true) {
            
            //
            // Check isAbort() inside loops
            //
            if (TheParser->isAbort()) {
                
                auto A = Token(TOKEN_ERROR_ABORTED, "", Source(TheSourceManager->getSourceLocation()));
                
                auto Aborted = NodePtr(new LeafNode(A));
                
                return Aborted;
            }

            
            //
            // LOOKAHEAD
            //
            {
                LeafSeq ArgsTest2;
                
                auto Tok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);

                if (Tok.Tok != TOKEN_SEMISEMI) {
                    
                    auto ImplicitTimes = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
                    
                    return ImplicitTimes;
                }
                
                auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMESSPAN, "Implicit ``Times`` between ``Spans``.", SYNTAXISSUESEVERITY_WARNING, Source(Tok.Src.start()), 0.75, {}));

                TheParser->addIssue(std::move(I));
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITTIMES, "", Source(Tok.Src.start()));
                
                NodeSeq OperandLeft;
                
                auto Implicit2 = Token(TOKEN_FAKE_IMPLICITONE, "", Source(Tok.Src.start()));
                
                OperandLeft.append(NodePtr(new LeafNode(Implicit2)));
                
                Operand = parse0(std::move(OperandLeft), CtxtIn);
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(ArgsTest2));
                Args.append(NodePtr(new LeafNode(Implicit)));
                Args.append(std::move(Operand));
            }

        } // while
    }
}

//
// infix
//
// Something like  a;;b
//
// Parses a single complete Span
//
NodePtr SemiSemiParselet::parse0(NodeSeq Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    Utils::differentLineWarning(Left, TokIn);
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;

    //
    // LOOKAHEAD
    //
    {
        TheParser->nextToken(Ctxt);
        
        auto SecondTok = TheParser->currentToken();
        
        Utils::endOfLineWarning(TokIn, SecondTok);
        
        LeafSeq ArgsTest1;
        
        SecondTok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
        
        //
        // a;;
        //  ^~TokIn
        //
        
        if (!TheParser->isPossibleBeginningOfExpression(SecondTok, Ctxt)) {

            //
            // a;;&
            //    ^SecondTok
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Src.end()));
            
            NodeSeq Args;
            Args.reserve(Left.size() + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
            auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            
            return Span;
        }

        Utils::differentLineWarning(TokIn, SecondTok);
        
        if (SecondTok.Tok != TOKEN_SEMISEMI) {

            //
            // a;;b
            //    ^SecondTok
            //
            
            auto FirstArg = TheParser->parse(Ctxt);

            //
            // LOOKAHEAD
            //
            {
                LeafSeq ArgsTest2;
                
                auto ThirdTok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);
                
                if (ThirdTok.Tok != TOKEN_SEMISEMI) {

                    //
                    // a;;b&
                    //     ^ThirdTok
                    //
                    
                    NodeSeq Args;
                    Args.reserve(Left.size() + 1 + 1 + 1);
                    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                    Args.append(NodePtr(new LeafNode(TokIn)));
                    Args.appendIfNonEmpty(std::move(ArgsTest1));
                    Args.append(std::move(FirstArg));
                    
                    auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    
                    return Span;
                }

                Utils::differentLineWarning(SecondTok, ThirdTok);

                //
                // a;;b;;
                //     ^~ThirdTok
                //
                
                // for RAII
                LeafSeq ThirdTokSeq;
                ThirdTokSeq.append(LeafNodePtr(new LeafNode(ThirdTok)));
                
                //
                // LOOKAHEAD
                //
                {
                    LeafSeq ArgsTest3;
                    
                    TheParser->nextToken(Ctxt);
                    
                    auto FourthTok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest3);
                    
                    if (!TheParser->isPossibleBeginningOfExpression(FourthTok, Ctxt)) {
                        
                        //
                        // a;;b;;&
                        //       ^FourthTok
                        //
                        
                        NodeSeq Args;
                        Args.reserve(1 + 1 + 1 + 1);
                        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                        Args.append(NodePtr(new LeafNode(TokIn)));
                        Args.appendIfNonEmpty(std::move(ArgsTest1));
                        Args.append(std::move(FirstArg));
                        
                        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                        
                        return Span;
                    }

                    Utils::differentLineWarning(ThirdTok, FourthTok);
                    
                    if (FourthTok.Tok != TOKEN_SEMISEMI) {

                        //
                        // a;;b;;c
                        //       ^FourthTok
                        //
                        
                        auto SecondArg = TheParser->parse(Ctxt);
                        
                        NodeSeq Args;
                        Args.reserve(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1);
                        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                        Args.append(NodePtr(new LeafNode(TokIn)));
                        Args.appendIfNonEmpty(std::move(ArgsTest1));
                        Args.append(std::move(FirstArg));
                        Args.appendIfNonEmpty(std::move(ArgsTest2));
                        Args.append(NodePtr(new LeafSeqNode(std::move(ThirdTokSeq))));
                        Args.appendIfNonEmpty(std::move(ArgsTest3));
                        Args.append(std::move(SecondArg));
                        
                        auto Span = NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                        
                        return Span;
                    }

                    //
                    // a;;b;;;;
                    //       ^~FourthTok
                    //
                    
                    NodeSeq Args;
                    Args.reserve(Left.size() + 1 + 1 + 1);
                    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                    Args.append(NodePtr(new LeafNode(TokIn)));
                    Args.appendIfNonEmpty(std::move(ArgsTest1));
                    Args.append(std::move(FirstArg));
                    
                    auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    
                    return Span;
                }
            }
        }
        
        //
        // a;;;;
        //    ^~SecondTok
        //
        
        // for RAII
        LeafSeq SecondTokSeq;
        SecondTokSeq.append(LeafNodePtr(new LeafNode(SecondTok)));
        
        //
        // LOOKAHEAD
        //
        {
            LeafSeq ArgsTest2;
            
            TheParser->nextToken(Ctxt);
            
            auto ThirdTok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);
            
            if (!TheParser->isPossibleBeginningOfExpression(ThirdTok, Ctxt)) {
                
                //
                // a;;;;&
                //      ^ThirdTok
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Src.end()));
                
                NodeSeq Args;
                Args.reserve(Left.size() + 1 + 1);
                Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
                auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                
                return Span;
            }

            Utils::differentLineWarning(SecondTok, ThirdTok);
            
            if (ThirdTok.Tok != TOKEN_SEMISEMI) {
                
                //
                // a;;;;b
                //      ^ThirdTok
                //
                
                auto FirstArg = TheParser->parse(Ctxt);
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Src.end()));
                
                NodeSeq Args;
                Args.reserve(Left.size() + 1 + 1 + 1 + 1 + 1 + 1);
                Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafSeqNode(std::move(SecondTokSeq))));
                Args.appendIfNonEmpty(std::move(ArgsTest2));
                Args.append(std::move(FirstArg));
                
                auto Span = NodePtr(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                
                return Span;
            }
            
            //
            // a;;;;;;
            //      ^~ThirdTok
            //
            
            //
            // LOOKAHEAD
            //
            {
                LeafSeq ArgsTest3;
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Src.end()));
                
                NodeSeq Args;
                Args.reserve(Left.size() + 1 + 1);
                Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
                auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                
                return Span;
            }
        }
    }
}



