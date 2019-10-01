
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
    
    auto One = std::unique_ptr<Node>(new LeafNode(Implicit));
    
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
    
        auto Tok = TheParser->currentToken();
    
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, ArgsTest1);
    
        if (Tok.Tok != TOKEN_SEMISEMI) {
            
            return Operand;
        }

        Args.append(std::move(Operand));
        Args.append(std::move(ArgsTest1));
        
        while (true) {
            
            //
            // Check isAbort() inside loops
            //
            if (TheParser->isAbort()) {
                
                auto A = Token(TOKEN_ERROR_ABORTED, "", Source(TheSourceManager->getSourceLocation()));
                
                auto Aborted = std::unique_ptr<Node>(new LeafNode(A));
                
                return Aborted;
            }

            
            //
            // LOOKAHEAD
            //
            {
                LeafSeq ArgsTest2;
                
                Tok = TheParser->currentToken();
                
                Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, ArgsTest2);

                if (Tok.Tok != TOKEN_SEMISEMI) {
                    
                    auto ImplicitTimes = std::unique_ptr<Node>(new InfixNode(SYMBOL_TIMES, std::move(Args)));
                    
                    return ImplicitTimes;
                }
                
                auto I = std::unique_ptr<Issue>(new SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMESSPAN, "Implicit ``Times`` between ``Spans``.", SYNTAXISSUESEVERITY_WARNING, Source(Tok.Src.start()), 0.75));

                TheParser->addIssue(std::move(I));
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITTIMES, "", Source(Tok.Src.start()));
                
                NodeSeq OperandLeft;
                
                auto Implicit2 = Token(TOKEN_FAKE_IMPLICITONE, "", Source(Tok.Src.start()));
                
                OperandLeft.append(std::unique_ptr<Node>(new LeafNode(Implicit2)));
                
                Operand = parse0(std::move(OperandLeft), CtxtIn);
                
                Args.append(std::move(ArgsTest2));
                Args.append(std::unique_ptr<Node>(new LeafNode(Implicit)));
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
        auto SecondTok = TheParser->nextToken(Ctxt);
        Utils::endOfLineWarning(TokIn, SecondTok);
        
        LeafSeq ArgsTest1;
        
        SecondTok = Parser::eatAndPreserveToplevelNewlines(SecondTok, CtxtIn, ArgsTest1);
        
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
            Args.append(std::move(Left));
            Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
            Args.append(std::unique_ptr<Node>(new LeafNode(Implicit)));
            
            auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            
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
                
                auto ThirdTok = TheParser->currentToken();
                
                ThirdTok = Parser::eatAndPreserveToplevelNewlines(ThirdTok, CtxtIn, ArgsTest2);
                
                if (ThirdTok.Tok != TOKEN_SEMISEMI) {

                    //
                    // a;;b&
                    //     ^ThirdTok
                    //
                    
                    NodeSeq Args;
                    Args.reserve(Left.size() + 1 + ArgsTest1.size() + 1);
                    Args.append(std::move(Left));
                    Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                    Args.append(std::move(ArgsTest1));
                    Args.append(std::move(FirstArg));
                    
                    auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    
                    return Span;
                }

                Utils::differentLineWarning(SecondTok, ThirdTok);

                //
                // a;;b;;
                //     ^~ThirdTok
                //
                
                // for RAII
                LeafSeq ThirdTokSeq;
                ThirdTokSeq.append(std::unique_ptr<LeafNode>(new LeafNode(ThirdTok)));
                
                //
                // LOOKAHEAD
                //
                {
                    LeafSeq ArgsTest3;
                    
                    auto FourthTok = TheParser->nextToken(Ctxt);
                    FourthTok = Parser::eatAndPreserveToplevelNewlines(FourthTok, CtxtIn, ArgsTest3);
                    
                    if (!TheParser->isPossibleBeginningOfExpression(FourthTok, Ctxt)) {
                        
                        //
                        // a;;b;;&
                        //       ^FourthTok
                        //
                        
                        NodeSeq Args;
                        Args.reserve(1 + 1 + ArgsTest1.size() + 1);
                        Args.append(std::move(Left));
                        Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                        Args.append(std::move(ArgsTest1));
                        Args.append(std::move(FirstArg));
                        
                        auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                        
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
                        Args.reserve(Left.size() + 1 + ArgsTest1.size() + 1 + ArgsTest2.size() + 1 + ArgsTest3.size() + 1);
                        Args.append(std::move(Left));
                        Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                        Args.append(std::move(ArgsTest1));
                        Args.append(std::move(FirstArg));
                        Args.append(std::move(ArgsTest2));
                        Args.append(std::move(ThirdTokSeq));
                        Args.append(std::move(ArgsTest3));
                        Args.append(std::move(SecondArg));
                        
                        auto Span = std::unique_ptr<Node>(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                        
                        return Span;
                    }

                    //
                    // a;;b;;;;
                    //       ^~FourthTok
                    //
                    
                    NodeSeq Args;
                    Args.reserve(Left.size() + 1 + ArgsTest1.size() + 1);
                    Args.append(std::move(Left));
                    Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                    Args.append(std::move(ArgsTest1));
                    Args.append(std::move(FirstArg));
                    
                    auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    
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
        SecondTokSeq.append(std::unique_ptr<LeafNode>(new LeafNode(SecondTok)));
        
        //
        // LOOKAHEAD
        //
        {
            LeafSeq ArgsTest2;
            
            auto ThirdTok = TheParser->nextToken(Ctxt);
            ThirdTok = Parser::eatAndPreserveToplevelNewlines(ThirdTok, CtxtIn, ArgsTest2);
            
            if (!TheParser->isPossibleBeginningOfExpression(ThirdTok, Ctxt)) {
                
                //
                // a;;;;&
                //      ^ThirdTok
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Src.end()));
                
                NodeSeq Args;
                Args.reserve(Left.size() + 1 + 1);
                Args.append(std::move(Left));
                Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                Args.append(std::unique_ptr<Node>(new LeafNode(Implicit)));
                
                auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                
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
                Args.reserve(Left.size() + 1 + 1 + ArgsTest1.size() + 1 + ArgsTest2.size() + 1);
                Args.append(std::move(Left));
                Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                Args.append(std::unique_ptr<Node>(new LeafNode(Implicit)));
                Args.append(std::move(ArgsTest1));
                Args.append(std::move(SecondTokSeq));
                Args.append(std::move(ArgsTest2));
                Args.append(std::move(FirstArg));
                
                auto Span = std::unique_ptr<Node>(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                
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
                Args.append(std::move(Left));
                Args.append(std::unique_ptr<Node>(new LeafNode(TokIn)));
                Args.append(std::unique_ptr<Node>(new LeafNode(Implicit)));
                
                auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                
                return Span;
            }
        }
    }
}



