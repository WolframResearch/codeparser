
#include "Parselet.h"

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
    
    LeafSeq ArgsTest1;

    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest1);

    if (!Tok.Tok.isPossibleBeginningOfExpression()) {
        
        //
        // There is only a single ;; and there is no implicit Times
        //
        
        return Operand;
    }
    
    NodeSeq Args(1 + 1);
    Args.append(std::move(Operand));
    Args.appendIfNonEmpty(std::move(ArgsTest1));
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest2;
        
        auto Tok = TheParser->currentToken();
        Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest2);
        
        if (!Tok.Tok.isPossibleBeginningOfExpression()) {
            
            //
            // We are done, so return
            //
            
            auto Times = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
            
            return Times;
        }
        
        auto lowerPrec = false;
        
        NodePtr Operand;
        
        if (TheParser->getPrefixTokenPrecedence(Tok, Ctxt) >= PRECEDENCE_SEMISEMI) {
            
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
            
            Operand = TheParser->parse(Tok, Ctxt);
            
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
        Args.appendIfNonEmpty(std::move(ArgsTest2));
        Args.append(NodePtr(new LeafNode(ImplicitTimes)));
        Args.append(std::move(Operand));
        
        if (lowerPrec) {
            
            //
            // We are done here, so return
            //
            
            auto Times = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
            
            return Times;
        }

    } // while
}

//
// infix
//
// Something like  a;;b
//
// Parses a single complete Span
//
NodePtr SemiSemiParselet::parse0(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence();
    
    TheParser->nextToken(TokIn);
    
    auto SecondTok = TheParser->currentToken();
    
    LeafSeq ArgsTest1;
    
    SecondTok = TheParser->eatAndPreserveToplevelNewlines(SecondTok, Ctxt, ArgsTest1);
    
    //
    // a;;
    //  ^~TokIn
    //
    
    if (!SecondTok.Tok.isPossibleBeginningOfExpression()
        || (TheParser->getPrefixTokenPrecedence(SecondTok, Ctxt) < PRECEDENCE_SEMISEMI)
        ) {

        //
        // a;;&
        //    ^SecondTok
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        return Span;
    }
    
    if (SecondTok.Tok != TOKEN_SEMISEMI) {

        //
        // a;;b
        //    ^SecondTok
        //
        
        auto FirstArg = TheParser->parse(SecondTok, Ctxt);
        
        LeafSeq ArgsTest2;
        
        auto ThirdTok = TheParser->currentToken();
        ThirdTok = TheParser->eatAndPreserveToplevelNewlines(ThirdTok, Ctxt, ArgsTest2);
        
        if (ThirdTok.Tok != TOKEN_SEMISEMI) {

            //
            // a;;b&
            //     ^ThirdTok
            //
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(std::move(FirstArg));
            
            auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            
            return Span;
        }
        
        //
        // a;;b;;
        //     ^~ThirdTok
        //
        
        // for RAII
        LeafSeq ThirdTokSeq;
        ThirdTokSeq.append(LeafNodePtr(new LeafNode(ThirdTok)));
        
        LeafSeq ArgsTest3;
        
        TheParser->nextToken(ThirdTok);
        
        auto FourthTok = TheParser->currentToken();
        FourthTok = TheParser->eatAndPreserveToplevelNewlines(FourthTok, Ctxt, ArgsTest3);
        
        if (!FourthTok.Tok.isPossibleBeginningOfExpression()
            || (TheParser->getPrefixTokenPrecedence(FourthTok, Ctxt) < PRECEDENCE_SEMISEMI)
            ) {
            
            //
            // a;;b;;&
            //       ^FourthTok
            //
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(std::move(FirstArg));
            
            auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
            
            return Span;
        }
        
        if (FourthTok.Tok != TOKEN_SEMISEMI) {

            //
            // a;;b;;c
            //       ^FourthTok
            //
            
            auto SecondArg = TheParser->parse(FourthTok, Ctxt);
            
            NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1);
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
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(FirstArg));
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        return Span;
    }
    
    //
    // a;;;;
    //    ^~SecondTok
    //
    
    // for RAII
    LeafSeq SecondTokSeq;
    SecondTokSeq.append(LeafNodePtr(new LeafNode(SecondTok)));
    
    LeafSeq ArgsTest2;
    
    TheParser->nextToken(SecondTok);
    
    auto ThirdTok = TheParser->currentToken();
    ThirdTok = TheParser->eatAndPreserveToplevelNewlines(ThirdTok, Ctxt, ArgsTest2);
    
    if (!ThirdTok.Tok.isPossibleBeginningOfExpression()
        || (TheParser->getPrefixTokenPrecedence(ThirdTok, Ctxt) < PRECEDENCE_SEMISEMI)
        ) {
        
        //
        // a;;;;&
        //      ^ThirdTok
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        return Span;
    }
    
    if (ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;;;b
        //      ^ThirdTok
        //
        
        auto FirstArg = TheParser->parse(ThirdTok, Ctxt);
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
        
        NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1 + 1);
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
    
    LeafSeq ArgsTest3;
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End));
    
    NodeSeq Args(1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.append(NodePtr(new LeafNode(Implicit)));
    
    auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
    
    return Span;
}
