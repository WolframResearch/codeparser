
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
    
    NodeSeq Left;
    
    auto TokIn = TheParser->currentToken();
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITONE, "", Source(TokIn.Span.lines.start));
    
    auto One = std::make_shared<LeafNode>(Implicit);
    
    Left.append(One);
    
    return parse(Left, CtxtIn);
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
NodePtr SemiSemiParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto Operand = parse0(Left, CtxtIn);
    
    Args.append(Operand);
    
    auto Tok = TheParser->currentToken();
    
    Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
    
    if (Tok.Tok != TOKEN_SEMISEMI) {
        return Operand;
    }
    
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        
        Tok = TheParser->currentToken();
        
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
        
        if (Tok.Tok != TOKEN_SEMISEMI) {
            break;
        }
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMESSPAN, "Implicit ``Times`` between ``Spans``.", SYNTAXISSUESEVERITY_WARNING, Source(Tok.Span.lines.start));
        
        TheParser->addIssue(Issue);
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITTIMES, "", Source(Tok.Span.lines.start));
        
        Args.append(std::make_shared<LeafNode>(Implicit));
        
        
        NodeSeq OperandLeft;
        
        auto Implicit2 = Token(TOKEN_FAKE_IMPLICITONE, "", Source(Tok.Span.lines.start));
        
        OperandLeft.append(std::make_shared<LeafNode>(Implicit2));
        
        Operand = parse0(OperandLeft, CtxtIn);
        
        
        Args.append(Operand);
    }
    
    auto ImplicitTimes = std::make_shared<InfixNode>(SYMBOL_TIMES, Args.getVector());
    
    return ImplicitTimes;
}

//
// infix
//
// Something like  a;;b
//
// Parses a single complete Span
//
NodePtr SemiSemiParselet::parse0(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    Utils::differentLineWarning(Left, TokIn, SYNTAXISSUESEVERITY_WARNING);
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto SecondTok = TheParser->nextToken(Ctxt);
    Utils::endOfLineWarning(TokIn, SecondTok);
    SecondTok = Parser::eatAndPreserveToplevelNewlines(SecondTok, CtxtIn, Args);
    
    //
    // a;;
    //
    
    if (!TheParser->isPossibleBeginningOfExpression(SecondTok, Ctxt)) {
        
        //
        // a;;&
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
        
        Args.append(std::make_shared<LeafNode>(Implicit));
        
        auto Span = std::make_shared<BinaryNode>(SYMBOL_SPAN, Args.getVector());
        
        return Span;
    }
    
    Utils::differentLineWarning(TokIn, SecondTok, SYNTAXISSUESEVERITY_WARNING);
    
    if (SecondTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;b
        //
        
        auto FirstArg = TheParser->parse(Ctxt);
        
        Args.append(FirstArg);
        
        auto ThirdTok = TheParser->currentToken();
        ThirdTok = Parser::eatAndPreserveToplevelNewlines(ThirdTok, CtxtIn, Args);
        
        if (ThirdTok.Tok != TOKEN_SEMISEMI) {
            
            //
            // a;;b&
            //
            
            auto Span = std::make_shared<BinaryNode>(SYMBOL_SPAN, Args.getVector());
            
            return Span;
        }
        
        Utils::differentLineWarning(SecondTok, ThirdTok, SYNTAXISSUESEVERITY_WARNING);
        
        //
        // a;;b;;
        //
        
        auto FourthTok = TheParser->nextToken(Ctxt);
        Utils::endOfLineWarning(ThirdTok, FourthTok);
        FourthTok = Parser::eatAndPreserveToplevelNewlines(FourthTok, CtxtIn, Args);
        
        if (!TheParser->isPossibleBeginningOfExpression(FourthTok, Ctxt)) {
            
            //
            // a;;b;;&
            //
            
            TheParser->nextToken(Ctxt);
            
            auto Span = std::make_shared<BinaryNode>(SYMBOL_SPAN, Args.getVector());
            
            assert(TheParser->getTokenQueue().empty());
            TheParser->append(ThirdTok);
            TheParser->append(FourthTok);
            
            return Span;
        }
        
        Utils::differentLineWarning(ThirdTok, FourthTok, SYNTAXISSUESEVERITY_WARNING);
        
        if (FourthTok.Tok != TOKEN_SEMISEMI) {
            
            //
            // a;;b;;c
            //
            
            auto SecondArg = TheParser->parse(Ctxt);
            
            Args.append(std::make_shared<LeafNode>(ThirdTok));
            Args.append(SecondArg);
            
            auto Span = std::make_shared<TernaryNode>(SYMBOL_SPAN, Args.getVector());
            
            return Span;
        }
        
        //
        // a;;b;;;;
        //
        
        auto FifthTok = TheParser->nextToken(Ctxt);
        Utils::endOfLineWarning(FourthTok, FifthTok);
        
        auto Span = std::make_shared<BinaryNode>(SYMBOL_SPAN, Args.getVector());
        
        assert(TheParser->getTokenQueue().empty());
        TheParser->append(ThirdTok);
        TheParser->append(FourthTok);
        
        return Span;
    }
    
    //
    // a;;;;
    //
    
    auto ThirdTok = TheParser->nextToken(Ctxt);
    Utils::endOfLineWarning(SecondTok, ThirdTok);
    ThirdTok = Parser::eatAndPreserveToplevelNewlines(ThirdTok, CtxtIn, Args);
    
    if (!TheParser->isPossibleBeginningOfExpression(ThirdTok, Ctxt)) {
        
        //
        // a;;;;&
        //
        
        TheParser->nextToken(Ctxt);
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
        
        Args.append(std::make_shared<LeafNode>(Implicit));
        
        auto Span = std::make_shared<BinaryNode>(SYMBOL_SPAN, Args.getVector());
        
        assert(TheParser->getTokenQueue().empty());
        TheParser->append(SecondTok);
        TheParser->append(ThirdTok);
        
        return Span;
    }
    
    Utils::differentLineWarning(SecondTok, ThirdTok, SYNTAXISSUESEVERITY_WARNING);
    
    if (ThirdTok.Tok != TOKEN_SEMISEMI) {
        
        //
        // a;;;;b
        //
        
        auto FirstArg = TheParser->parse(Ctxt);
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(SecondTok.Span.lines.start));
        
        Args.append(std::make_shared<LeafNode>(Implicit));
        Args.append(std::make_shared<LeafNode>(SecondTok));
        Args.append(FirstArg);
        
        auto Span = std::make_shared<TernaryNode>(SYMBOL_SPAN, Args.getVector());
        
        return Span;
    }
    
    //
    // a;;;;;;
    //
    
    auto FourthTok = TheParser->nextToken(Ctxt);
    Utils::endOfLineWarning(ThirdTok, FourthTok);
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITALL, "", Source(TokIn.Span.lines.end));
    
    Args.append(std::make_shared<LeafNode>(Implicit));
    
    auto Span = std::make_shared<BinaryNode>(SYMBOL_SPAN, Args.getVector());
    
    assert(TheParser->getTokenQueue().empty());
    TheParser->append(SecondTok);
    TheParser->append(ThirdTok);
    
    return Span;
}

