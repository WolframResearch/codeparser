
//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//
//

#include "Parselet.h"

#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"
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
// Must also handle  ;;!b  where there is an implicit Times, but only a single Span
//
NodePtr SemiSemiParselet::parse(Token firstTok, ParserContext CtxtIn) const {
    
    auto TokIn = firstTok;
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, BufferAndLength(TokIn.bufferAndLength.buffer, 0, false));
    
    auto One = NodePtr(new LeafNode(Implicit));
    
    NodeSeq Left;
    Left.reserve(1);
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
// Must also handle  a;;!b  where there is an implicit Times, but only a single Span
//
NodePtr SemiSemiParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto Operand = parse0(std::move(Left), CtxtIn);
    
    LeafSeq ArgsTest1;

    auto Tok = TheParser->eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);

    if (!Tok.getTokenEnum().isPossibleBeginningOfExpression()) {
        
        //
        // There is only a single ;; and there is no implicit Times
        //
        
        return Operand;
    }
    
    NodeSeq Args;
    Args.reserve(1 + 1);
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
        
        auto Tok = TheParser->eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);
        
        if (!Tok.getTokenEnum().isPossibleBeginningOfExpression()) {
            
            //
            // We are done, so return
            //
            
            auto Times = NodePtr(new InfixNode(SYMBOL_TIMES, std::move(Args)));
            
            return Times;
        }
        
        auto lowerPrec = false;
        
        NodePtr Operand;
        if (TheParser->getTokenPrecedence(Tok, CtxtIn, true, nullptr) >= PRECEDENCE_SEMISEMI) {
            
            //
            // Higher precedence, so still within the ;;
            //
            
            auto ImplicitOne = Token(TOKEN_FAKE_IMPLICITONE, BufferAndLength(Tok.bufferAndLength.buffer, 0, false));
            
            NodeSeq ImplicitOneSeq;
            ImplicitOneSeq.append(NodePtr(new LeafNode(ImplicitOne)));
            
            Operand = parse0(std::move(ImplicitOneSeq), CtxtIn);
            
            lowerPrec = false;
            
        } else {
            
            //
            // Lower precedence, so this is just a general expression
            //
            // Must also handle  a;;!b  where there is an Implicit Times, but only a single Span
            //
            
            Operand = TheParser->parse(Tok, CtxtIn);
            
            lowerPrec = true;
        }

#if !NISSUES
        auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMES, "Implicit ``Times`` between ``Spans``.", SYNTAXISSUESEVERITY_WARNING, Source(Tok.getSource().Start), 0.75, {}));
        
        TheParser->addIssue(std::move(I));
#endif // !NISSUES
        
        auto ImplicitTimes = Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(Tok.bufferAndLength.buffer, 0, false));
        
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
NodePtr SemiSemiParselet::parse0(NodeSeq Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken();
    
    auto SecondTok = TheParser->currentToken();
    
    LeafSeq ArgsTest1;
    
    SecondTok = TheParser->eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
    
    //
    // a;;
    //  ^~TokIn
    //
    
    if (!SecondTok.getTokenEnum().isPossibleBeginningOfExpression()
        || (TheParser->getTokenPrecedence(SecondTok, Ctxt, true, nullptr) < PRECEDENCE_SEMISEMI)
        ) {

        //
        // a;;&
        //    ^SecondTok
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.bufferAndLength.end(), 0, false));
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        return Span;
    }
    
    if (SecondTok.getTokenEnum() != TOKEN_SEMISEMI) {

        //
        // a;;b
        //    ^SecondTok
        //
        
        auto FirstArg = TheParser->parse(SecondTok, Ctxt);
        
        LeafSeq ArgsTest2;
        
        auto ThirdTok = TheParser->eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);
        
        if (ThirdTok.getTokenEnum() != TOKEN_SEMISEMI) {

            //
            // a;;b&
            //     ^ThirdTok
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
        
        //
        // a;;b;;
        //     ^~ThirdTok
        //
        
        // for RAII
        LeafSeq ThirdTokSeq;
        ThirdTokSeq.append(LeafNodePtr(new LeafNode(ThirdTok)));
        
        LeafSeq ArgsTest3;
        
        TheParser->nextToken();
        
        auto FourthTok = TheParser->eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest3);
        
        if (!FourthTok.getTokenEnum().isPossibleBeginningOfExpression()
            || (TheParser->getTokenPrecedence(FourthTok, Ctxt, true, nullptr) < PRECEDENCE_SEMISEMI)
            ) {
            
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
        
        if (FourthTok.getTokenEnum() != TOKEN_SEMISEMI) {

            //
            // a;;b;;c
            //       ^FourthTok
            //
            
            auto SecondArg = TheParser->parse(FourthTok, Ctxt);
            
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
        Args.reserve(1 + 1 + 1 + 1);
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
    
    TheParser->nextToken();
    
    auto ThirdTok = TheParser->eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);
    
    if (!ThirdTok.getTokenEnum().isPossibleBeginningOfExpression()
        || (TheParser->getTokenPrecedence(ThirdTok, Ctxt, true, nullptr) < PRECEDENCE_SEMISEMI)
        ) {
        
        //
        // a;;;;&
        //      ^ThirdTok
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.bufferAndLength.end(), 0, false));
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
        
        return Span;
    }
    
    if (ThirdTok.getTokenEnum() != TOKEN_SEMISEMI) {
        
        //
        // a;;;;b
        //      ^ThirdTok
        //
        
        auto FirstArg = TheParser->parse(ThirdTok, Ctxt);
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.bufferAndLength.end(), 0, false));
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1 + 1 + 1 + 1);
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
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, BufferAndLength(TokIn.bufferAndLength.end(), 0, false));
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.append(NodePtr(new LeafNode(Implicit)));
    
    auto Span = NodePtr(new BinaryNode(SYMBOL_SPAN, std::move(Args)));
    
    return Span;
}
