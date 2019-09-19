
//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//
//

#include "Parselet.h"

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
    
    auto Left = std::unique_ptr<NodeSeq>(new NodeSeq);
    
    auto TokIn = TheParser->currentToken();
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, "", Source(TokIn.Span.lines.start));
    
    auto One = std::unique_ptr<Node>(new LeafNode(Implicit));
    
    Left->append(std::move(One));
    
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
NodePtr SemiSemiParselet::parse(std::unique_ptr<NodeSeq> Left, ParserContext CtxtIn) const {

    auto Args = std::unique_ptr<NodeSeq>(new NodeSeq);
    
    auto Operand = parse0(std::move(Left), CtxtIn);
    
    //
    // LOOKAHEAD
    //
    {
        auto ArgsTest = std::unique_ptr<LeafSeq>(new LeafSeq);
    
        auto Tok = TheParser->currentToken();
    
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, ArgsTest);
    
        if (Tok.Tok != TOKEN_SEMISEMI) {
            
            TheParser->nextToken(CtxtIn);
            
            //
            // Prepend in correct order
            //
            TheParser->prepend(Tok);
            TheParser->prependInReverse(std::move(ArgsTest));
            
            return Operand;
        }

        Args->append(std::move(Operand));
        Args->append(std::move(ArgsTest));
        
        while (true) {
            
            //
            // Check isAbort() inside loops
            //
            if (TheParser->isAbort()) {
                
                auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
                
                auto Aborted = std::unique_ptr<Node>(new LeafNode(A));
                
                return Aborted;
            }

            
            //
            // LOOKAHEAD
            //
            {
                auto ArgsTest2 = std::unique_ptr<LeafSeq>(new LeafSeq);
                
                Tok = TheParser->currentToken();
                
                Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, ArgsTest2);

                if (Tok.Tok != TOKEN_SEMISEMI) {
                    
                    TheParser->nextToken(CtxtIn);
                    
                    //
                    // Prepend in correct order
                    //
                    TheParser->prepend(Tok);
                    TheParser->prependInReverse(std::move(ArgsTest2));
                    
                    auto ImplicitTimes = std::unique_ptr<Node>(new InfixNode(SYMBOL_TIMES, std::move(Args)));
                    
                    return ImplicitTimes;
                }

                Args->append(std::move(ArgsTest2));
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMESSPAN, "Implicit ``Times`` between ``Spans``.", SYNTAXISSUESEVERITY_WARNING, Source(Tok.Span.lines.start));

                TheParser->addIssue(Issue);
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITTIMES, "", Source(Tok.Span.lines.start));
                
                Args->append(std::unique_ptr<Node>(new LeafNode(Implicit)));

                
                auto OperandLeft = std::unique_ptr<NodeSeq>(new NodeSeq);
                
                auto Implicit2 = Token(TOKEN_FAKE_IMPLICITONE, "", Source(Tok.Span.lines.start));
                
                OperandLeft->append(std::unique_ptr<Node>(new LeafNode(Implicit2)));
                
                Operand = parse0(std::move(OperandLeft), CtxtIn);
                
                
                Args->append(std::move(Operand));
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
NodePtr SemiSemiParselet::parse0(std::unique_ptr<NodeSeq> Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    Utils::differentLineWarning(Left, TokIn, SYNTAXISSUESEVERITY_WARNING);
    
    auto Args = std::unique_ptr<NodeSeq>(new NodeSeq);
    Args->reserve(1 + 1 + 1);
    
    Args->append(std::move(Left));
    
    Args->append(std::unique_ptr<Node>(new LeafNode(TokIn)));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;

    //
    // LOOKAHEAD
    //
    {
        auto SecondTok = TheParser->nextToken(Ctxt);
        Utils::endOfLineWarning(TokIn, SecondTok);
        
        auto ArgsTest = std::unique_ptr<LeafSeq>(new LeafSeq);
        
        SecondTok = Parser::eatAndPreserveToplevelNewlines(SecondTok, CtxtIn, ArgsTest);
        
        //
        // a;;
        //

        if (!TheParser->isPossibleBeginningOfExpression(SecondTok, Ctxt)) {

            //
            // a;;&
            //
            
            TheParser->nextToken(Ctxt);
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
            
            Args->append(std::unique_ptr<Node>(new LeafNode(Implicit)));
            
            //
            // Prepend in correct order
            //
            TheParser->prepend(SecondTok);
            TheParser->prependInReverse(std::move(ArgsTest));
            
            auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            
            return Span;
        }

        Utils::differentLineWarning(TokIn, SecondTok, SYNTAXISSUESEVERITY_WARNING);
        
        if (SecondTok.Tok != TOKEN_SEMISEMI) {

            //
            // a;;b
            //
            
            Args->append(std::move(ArgsTest));
            
            auto FirstArg = TheParser->parse(Ctxt);
            
            Args->append(std::move(FirstArg));

            //
            // LOOKAHEAD
            //
            {
                auto ArgsTest2 = std::unique_ptr<LeafSeq>(new LeafSeq);
                
                auto ThirdTok = TheParser->currentToken();
                
                ThirdTok = Parser::eatAndPreserveToplevelNewlines(ThirdTok, CtxtIn, ArgsTest2);
                
                if (ThirdTok.Tok != TOKEN_SEMISEMI) {

                    //
                    // a;;b&
                    //
                    
                    TheParser->prependInReverse(std::move(ArgsTest2));
                    
                    auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    
                    return Span;
                }

                Utils::differentLineWarning(SecondTok, ThirdTok, SYNTAXISSUESEVERITY_WARNING);

                //
                // a;;b;;
                //
                
                //
                // LOOKAHEAD
                //
                {
                    auto ArgsTest3 = std::unique_ptr<LeafSeq>(new LeafSeq);
                    
                    auto FourthTok = TheParser->nextToken(Ctxt);
                    FourthTok = Parser::eatAndPreserveToplevelNewlines(FourthTok, CtxtIn, ArgsTest3);
                    
                    if (!TheParser->isPossibleBeginningOfExpression(FourthTok, Ctxt)) {

                        //
                        // a;;b;;&
                        //
                        
                        TheParser->nextToken(Ctxt);
                        
                        auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                        
                        //
                        // Prepend in correct order
                        //
                        TheParser->prepend(FourthTok);
                        TheParser->prependInReverse(std::move(ArgsTest3));
                        TheParser->prepend(ThirdTok);
                        TheParser->prependInReverse(std::move(ArgsTest2));
                        
                        return Span;
                    }

                    Utils::differentLineWarning(ThirdTok, FourthTok, SYNTAXISSUESEVERITY_WARNING);
                    
                    if (FourthTok.Tok != TOKEN_SEMISEMI) {

                        //
                        // a;;b;;c
                        //
                        
                        auto SecondArg = TheParser->parse(Ctxt);
                        
                        Args->append(std::move(ArgsTest2));
                        Args->append(std::unique_ptr<Node>(new LeafNode(ThirdTok)));
                        Args->append(std::move(ArgsTest3));
                        Args->append(std::move(SecondArg));
                        
                        auto Span = std::unique_ptr<Node>(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                        
                        return Span;
                    }

                    //
                    // a;;b;;;;
                    //
                    
                    auto FifthTok = TheParser->nextToken(Ctxt);
                    Utils::endOfLineWarning(FourthTok, FifthTok);
                    
                    auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                    
                    //
                    // Prepend in correct order
                    //
                    TheParser->prepend(FourthTok);
                    TheParser->prependInReverse(std::move(ArgsTest3));
                    TheParser->prepend(ThirdTok);
                    TheParser->prependInReverse(std::move(ArgsTest2));
                    
                    return Span;
                }
            }
        }

        //
        // a;;;;
        //
        
        //
        // LOOKAHEAD
        //
        {

            auto ArgsTest2 = std::unique_ptr<LeafSeq>(new LeafSeq);
            
            auto ThirdTok = TheParser->nextToken(Ctxt);
            ThirdTok = Parser::eatAndPreserveToplevelNewlines(ThirdTok, CtxtIn, ArgsTest2);
            
            if (!TheParser->isPossibleBeginningOfExpression(ThirdTok, Ctxt)) {

                //
                // a;;;;&
                //
                
                TheParser->nextToken(Ctxt);
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
                
                Args->append(std::unique_ptr<Node>(new LeafNode(Implicit)));
                
                auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                
                //
                // Prepend in correct order
                //
                TheParser->prepend(ThirdTok);
                TheParser->prependInReverse(std::move(ArgsTest2));
                TheParser->prepend(SecondTok);
                TheParser->prependInReverse(std::move(ArgsTest));
                
                return Span;
            }

            Utils::differentLineWarning(SecondTok, ThirdTok, SYNTAXISSUESEVERITY_WARNING);
            
            if (ThirdTok.Tok != TOKEN_SEMISEMI) {

                //
                // a;;;;b
                //
                
                auto FirstArg = TheParser->parse(Ctxt);
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
                
                Args->append(std::unique_ptr<Node>(new LeafNode(Implicit)));
                
                Args->append(std::move(ArgsTest));
                
                Args->append(std::unique_ptr<Node>(new LeafNode(SecondTok)));
                
                Args->append(std::move(ArgsTest2));
                
                Args->append(std::move(FirstArg));
                
                auto Span = std::unique_ptr<Node>(new TernaryNode(SYMBOL_SPAN, std::move(Args)));
                
                return Span;
            }
            
            //
            // a;;;;;;
            //
            
            //
            // LOOKAHEAD
            //
            {
                auto ArgsTest3 = std::unique_ptr<LeafSeq>(new LeafSeq);
                
                auto FourthTok = TheParser->nextToken(Ctxt);
                FourthTok = Parser::eatAndPreserveToplevelNewlines(FourthTok, CtxtIn, ArgsTest3);
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
                
                Args->append(std::unique_ptr<Node>(new LeafNode(Implicit)));
                
                auto Span = std::unique_ptr<Node>(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
                
                //
                // Prepend in correct order
                //
                TheParser->prependInReverse(std::move(ArgsTest3));
                TheParser->prepend(ThirdTok);
                TheParser->prependInReverse(std::move(ArgsTest2));
                TheParser->prepend(SecondTok);
                TheParser->prependInReverse(std::move(ArgsTest));
                
                return Span;
            }
        }
    }
}



