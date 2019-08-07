
#include "Parselet.h"

#include "Symbol.h"
#include "Utils.h"

//
// Symbol parselet
//

//
// parsing x in _x
//
// we know it can only be a symbol
//
// Called from other parselets
//
NodePtr SymbolParselet::parseContextSensitive(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    return std::make_shared<LeafNode>(TokIn);
}

//
// something like  x  or x_
//
NodePtr SymbolParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Sym = std::make_shared<LeafNode>(TokIn);
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    
    Args.append(Sym);
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    if (Tok.Tok == TOKEN_UNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
        
        Ctxt.UnderCount = UNDER_1;
        
        return underParselet->parseContextSensitive(Args, Ctxt);
        
    } else if (Tok.Tok == TOKEN_UNDERUNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
        
        Ctxt.UnderCount = UNDER_2;
        
        return underParselet->parseContextSensitive(Args, Ctxt);
        
    } else if (Tok.Tok == TOKEN_UNDERUNDERUNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
        
        Ctxt.UnderCount = UNDER_3;
        
        return underParselet->parseContextSensitive(Args, Ctxt);
        
    } else if (Tok.Tok == TOKEN_UNDERDOT) {
        
        Args.append(std::make_shared<LeafNode>(Tok));
        
        TheParser->nextToken(Ctxt);
        
        return std::make_shared<OptionalDefaultPatternNode>(Args.getVector());
    }
    
    NodeSeq ArgsTest;
    
    Tok = Parser::eatAll(Tok, Ctxt, ArgsTest);
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    // It is necessary to go to colonParselet->parse here (even though it seems non-contextSensitive)
    // because in e.g.,   a_*b:f[]    the b is the last node in the Times expression and needs to bind with :f[]
    // Parsing a_*b completely, and then parsing :f[] would be wrong.
    //
    if (!Ctxt.ColonFlag) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            Args.append(ArgsTest);
            
            auto& colonParselet = TheParser->findInfixParselet(Tok.Tok);
            
            return colonParselet->parse(Args, Ctxt);
        }
    }
    
    //
    // Put back unused Nodes
    //
    assert(TheParser->getTokenQueue().empty());
    for (auto A : ArgsTest.getVector()) {
        auto ALeaf = std::dynamic_pointer_cast<const LeafNode>(A);
        assert(ALeaf);
        TheParser->append(ALeaf->getToken());
    }
    
    return Sym;
}


//
// Base Operators parselets
//

NodePtr PrefixOperatorParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(TokIn, Tok, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto operand = TheParser->parse(Ctxt);
    
    Args.append(operand);
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok), Args.getVector());
}

NodePtr BinaryOperatorParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.append(Right);
    
    if (getAssociativity() == ASSOCIATIVITY_NONASSOCIATIVE) {
        
        if (auto BinLeft = std::dynamic_pointer_cast<const BinaryNode>(Left.main())) {
            
            if (BinLeft->getSymbol() == BinaryOperatorToSymbol(TokIn.Tok)) {
                
                return std::make_shared<SyntaxErrorNode>(SYNTAXERROR_NONASSOCIATIVE, Args.getVector());
            }
        }
    }
    
    return std::make_shared<BinaryNode>(BinaryOperatorToSymbol(TokIn.Tok), Args.getVector());
}

NodePtr InfixOperatorParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        
        auto Tok = TheParser->currentToken();
        
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if (isInfixOperator(Tok.Tok) &&
            InfixOperatorToSymbol(Tok.Tok) == InfixOperatorToSymbol(TokIn.Tok)) {
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt);
            
            Tok = Parser::eatAll(Tok, Ctxt, Args);
            
            auto operand = TheParser->parse(Ctxt);
            
            Args.append(operand);
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            break;
        }
        
    } // while
    
    return std::make_shared<InfixNode>(InfixOperatorToSymbol(TokIn.Tok), Args.getVector());
}

NodePtr PostfixOperatorParselet::parse(NodeSeq& Operand, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    
    Args.append(Operand);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    Utils::differentLineWarning(Operand, TokIn, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    return std::make_shared<PostfixNode>(PostfixOperatorToSymbol(TokIn.Tok), Args.getVector());
}




//
// Group parselets
//

NodePtr GroupParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    auto Opener = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(Opener));
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    auto& Op = GroupOpenerToSymbol(Opener.Tok);
    
    auto CloserTok = GroupOpenerToCloser(Opener.Tok);
    Ctxt.Closer = CloserTok;
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        
        auto Tok = TheParser->currentToken();
        
        Tok = Parser::eatAll(Tok, Ctxt, Args);
        
        if (Tok.Tok == CloserTok) {
            
            //
            // Everything is good
            //
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            
            TheParser->nextToken(Ctxt2);
            
            auto group = std::make_shared<GroupNode>(Op, Args.getVector());
            
            return group;
        }
        if (isCloser(Tok.Tok)) {
            
            //
            // some other closer
            //
            // e.g.,   { ( a }  or  { a ) }
            //
            
            auto MissingOpener = GroupCloserToOpener(Tok.Tok);
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            auto& MissingOpenerSymbol = GroupOpenerToSymbol(MissingOpener);
            
            auto group = std::make_shared<GroupMissingOpenerNode>(MissingOpenerSymbol, Args.getVector());
            
            return group;
        }
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   { a EOF
            //
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            auto& Op = GroupOpenerToSymbol(Opener.Tok);
            
            auto group = std::make_shared<GroupMissingCloserNode>(Op, Args.getVector());
            
            return group;
        }
        
        //
        // Handle the expression
        //
        
        Tok = Parser::eatAll(Tok, Ctxt, Args);
        
        auto Ctxt2 = Ctxt;
        Ctxt2.ColonFlag = false;
        Ctxt2.Prec = PRECEDENCE_LOWEST;
        Ctxt2.Assoc = ASSOCIATIVITY_NONE;
        Ctxt2.UnderCount = UNDER_UNKNOWN;
        
        auto operand = TheParser->parse(Ctxt2);
        
        Args.append(operand);
    }
}


//
// Call parselets
//

NodePtr CallParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    
    auto TokIn = TheParser->currentToken();
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.append(Right);
    
    //
    // Only warn if  Symbol [1]
    //
    // Stuff like   # & [1]  occurs a lot and it is stylistic
    //
    auto LeftLast = Left.last();
    while (auto LeftLastCall = std::dynamic_pointer_cast<const CallNode>(LeftLast)) {
        auto Children = LeftLastCall->getChildren();
        auto L = Children[Children.size()-1];
        LeftLast = L;
    }
    if (auto LeftLastLeaf = std::dynamic_pointer_cast<const LeafNode>(LeftLast)) {
        
        auto T = LeftLastLeaf->getToken();
        
        Utils::notContiguousWarning(T, TokIn);
    }
    
    assert(std::dynamic_pointer_cast<const GroupNode>(Right) ||
           std::dynamic_pointer_cast<const GroupMissingCloserNode>(Right) ||
           std::dynamic_pointer_cast<const GroupMissingOpenerNode>(Right));
    
    return std::make_shared<CallNode>(Left.getVector(), Args.getVector());
}


//
// StartOfLine
//

NodePtr StartOfLineParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.StringifyCurrentLine = true;
    
    TheParser->nextToken(Ctxt);
    
    Ctxt.StringifyCurrentLine = false;
    
    //
    // Cannot use TheParser->findPrefixParselet(TOKEN_STRING) here because TOKEN_ERROR_EMPTYSTRING
    // may be returned, and we have to handle that also
    //
    // So just use general parse
    //
    
    auto Operand = TheParser->parse(Ctxt);
    
    Args.append(Operand);
    
    return std::make_shared<StartOfLineNode>(StartOfLineOperatorToSymbol(TokIn.Tok), Args.getVector());
}






//
// Special parselets
//

//
// prefix
//
// Something like  _a
//
NodePtr UnderParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Under = std::make_shared<LeafNode>(TokIn);
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    std::shared_ptr<const Node> BlankTmp = nullptr;
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->findContextSensitivePrefixParselet(Tok.Tok);
        
        auto Sym2 = symbolParselet->parseContextSensitive(Ctxt);
        
        NodeSeq Args;
        Args.reserve(1 + 1);
        Args.append(Under);
        Args.append(Sym2);
        
        switch (TokIn.Tok) {
            case TOKEN_UNDER:
                BlankTmp = std::make_shared<BlankNode>(Args.getVector());
                break;
            case TOKEN_UNDERUNDER:
                BlankTmp = std::make_shared<BlankSequenceNode>(Args.getVector());
                break;
            case TOKEN_UNDERUNDERUNDER:
                BlankTmp = std::make_shared<BlankNullSequenceNode>(Args.getVector());
                break;
            default:
                assert(false);
                break;
        }
        
    } else {
        BlankTmp = Under;
    }
    NodePtr Blank = BlankTmp;
    
    
    NodeSeq ArgsTest;
    
    Tok = TheParser->currentToken();
    
    Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, ArgsTest);
    
    //
    // For something like _:""  when parsing _
    // ColonFlag == false
    // the : here is Optional, and so we want to go parse with ColonParselet's parseContextSensitive method
    //
    // For something like a:_:""  when parsing _
    // ColonFlag == true
    // make sure to not parse the second : here
    // We are already inside ColonParselet from the first :, and so ColonParselet will also handle the second :
    //
    if (!Ctxt.ColonFlag) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
            
            NodeSeq BlankSeq;
            BlankSeq.reserve(1 + 1);
            BlankSeq.append(Blank);
            BlankSeq.append(ArgsTest);
            return colonParselet->parseContextSensitive(BlankSeq, Ctxt);
        }
    }
    
    //
    // Put back unused Nodes
    //
    assert(TheParser->getTokenQueue().empty());
    for (auto A : ArgsTest.getVector()) {
        auto ALeaf = std::dynamic_pointer_cast<const LeafNode>(A);
        assert(ALeaf);
        TheParser->append(ALeaf->getToken());
    }
    
    return Blank;
}

//
// infix
//
// Something like  a_b
//
// Called from other parselets
//
NodePtr UnderParselet::parseContextSensitive(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto UnderCount = CtxtIn.UnderCount;
    
    auto Ctxt = CtxtIn;
    Ctxt.UnderCount = UNDER_UNKNOWN;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->findContextSensitivePrefixParselet(Tok.Tok);
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);
        
        Args.append(Right);
    }
    
    std::shared_ptr<const Node> PatTmp = nullptr;
    switch (UnderCount) {
        case UNDER_1:
            PatTmp = std::make_shared<PatternBlankNode>(Args.getVector());
            break;
        case UNDER_2:
            PatTmp = std::make_shared<PatternBlankSequenceNode>(Args.getVector());
            break;
        case UNDER_3:
            PatTmp = std::make_shared<PatternBlankNullSequenceNode>(Args.getVector());
            break;
        default:
            assert(false);
            break;
    }
    NodePtr Pat = PatTmp;
    
    
    NodeSeq ArgsTest;
    
    Tok = TheParser->currentToken();
    
    Tok = Parser::eatAll(Tok, Ctxt, ArgsTest);
    
    //
    // For something like a:b_c:d when parsing _
    // ColonFlag == true
    //
    if (!Ctxt.ColonFlag) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
            
            NodeSeq PatSeq;
            PatSeq.reserve(1 + 1);
            PatSeq.append(Pat);
            PatSeq.append(ArgsTest);
            return colonParselet->parseContextSensitive(PatSeq, Ctxt);
        }
    }
    
    //
    // Put back unused Nodes
    //
    assert(TheParser->getTokenQueue().empty());
    for (auto A : ArgsTest.getVector()) {
        auto ALeaf = std::dynamic_pointer_cast<const LeafNode>(A);
        assert(ALeaf);
        TheParser->append(ALeaf->getToken());
    }
    
    return Pat;
}

//
// Something like  a ~f~ b
//
NodePtr TildeParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1 + 1);
    
    Args.append(Left);
    
    auto FirstTilde = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(FirstTilde));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(FirstTilde, Tok, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto FirstTok = Tok;
    
    auto Middle = TheParser->parse(Ctxt);
    
    Args.append(Middle);
    
    Tok = TheParser->currentToken();
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(FirstTok, Tok, SYNTAXISSUESEVERITY_FORMATTING);
    
    if (Tok.Tok == TOKEN_TILDE) {
        
        auto isMiddleSymbol = false;
        if (auto MiddleLeaf = std::dynamic_pointer_cast<const LeafNode>(Middle)) {
            if (MiddleLeaf->getToken().Tok == TOKEN_SYMBOL) {
                isMiddleSymbol = true;
            }
        }
        
        if (!isMiddleSymbol) {
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDEXPRESSION, "Expression in middle of ``~`` is usually a symbol.", SYNTAXISSUESEVERITY_WARNING, Middle->getSourceSpan());
            
            TheParser->addIssue(Issue);
        }
        
        Args.append(std::make_shared<LeafNode>(Tok));
        
        Tok = TheParser->nextToken(Ctxt);
        
        Tok = Parser::eatAll(Tok, Ctxt, Args);
        
        auto Right = TheParser->parse(Ctxt);
        
        Args.append(Right);
        
        return std::make_shared<TernaryNode>(SYMBOL_AST_TERNARYTILDE, Args.getVector());
    }
    
    //
    // Something like   a ~f b
    //
    // Proceed with parsing as if the second ~ were present
    //
    
    if (Tok.Tok == TOKEN_ENDOFFILE) {
        
        //
        // If EndOfFile, don't bother trying to parse
        //
        
        Args.append(std::make_shared<LeafNode>(Tok));
        
        auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDTILDE, Args.getVector());
        
        return Error;
    }
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.append(Right);
    
    auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDTILDE, Args.getVector());
    
    return Error;
}



//
// Something like  symbol:object
//
// when parsing a in a:b  then ColonFlag is false
// when parsing b in a:b  then ColonFlag is true
//
NodePtr ColonParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    assert(!CtxtIn.ColonFlag);
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.ColonFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.append(Right);
    
    if (auto LeftLeaf = std::dynamic_pointer_cast<const LeafNode>(Left.main())) {
        
        if (LeftLeaf->getToken().Tok == TOKEN_SYMBOL) {
            
            auto Pat = std::make_shared<BinaryNode>(SYMBOL_PATTERN, Args.getVector());
            
            NodeSeq PatSeq;
            PatSeq.append(Pat);
            
            auto Tok = TheParser->currentToken();
            
            Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, PatSeq);
            
            if (Tok.Tok == TOKEN_COLON) {
                
                Ctxt.ColonFlag = false;
                
                return parseContextSensitive(PatSeq, Ctxt);
            }
            
            return Pat;
        }
    }
    
    auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_COLONERROR, Args.getVector());
    
    return Error;
}

//
// Something like  pattern:optional
//
// Called from other parselets
//
NodePtr ColonParselet::parseContextSensitive(NodeSeq& Left, ParserContext CtxtIn) const {
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert(!CtxtIn.ColonFlag);
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.append(Right);
    
    return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Args.getVector());
}

//
// Something like  a /: b = c
//
NodePtr SlashColonParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    auto Middle = TheParser->parse(Ctxt);
    
    if (auto BinaryMiddle = std::dynamic_pointer_cast<const BinaryNode>(Middle)) {
        
        if (BinaryMiddle->getSymbol() == SYMBOL_SET) {
            
            auto MiddleChildren = Middle->getChildren();
            
            Args.append(MiddleChildren);
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSET, Args.getVector());
        }
        if (BinaryMiddle->getSymbol() == SYMBOL_SETDELAYED) {
            
            auto MiddleChildren = Middle->getChildren();
            
            Args.append(MiddleChildren);
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSETDELAYED, Args.getVector());
        }
        if (BinaryMiddle->getSymbol() == SYMBOL_UNSET) {
            
            auto MiddleChildren = Middle->getChildren();
            
            Args.append(MiddleChildren);
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGUNSET, Args.getVector());
        }
    }
    
    //
    // Anything other than:
    // a /: b = c
    // a /: b := c
    // a /: b =.
    //
    
    Args.append(Middle);
    
    auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDSET, Args.getVector());
    
    return Error;
}



//
// Something like  \( x \)
//
NodePtr LinearSyntaxOpenParenParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    auto Opener = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    Ctxt.LinearSyntaxFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    auto CloserTok = TOKEN_LINEARSYNTAX_CLOSEPAREN;
    Ctxt.Closer = CloserTok;
    
    Args.append(std::make_shared<LeafNode>(Opener));
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   \( a EOF
            //
            
            auto group = std::make_shared<GroupMissingCloserNode>(SYMBOL_AST_GROUPLINEARSYNTAXPAREN, Args.getVector());
            
            return group;
        }
        if (Tok.Tok == CloserTok) {
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            
            TheParser->nextToken(Ctxt2);
            
            break;
        }
        
        //
        // Do not check for other closers here
        //
        // As long as \( \) parses by just doing tokenization, then cannot reliably test for other closers
        //
        // e.g., \( ( \) is completely valid syntax
        //
        
        if (Tok.Tok == TOKEN_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse(Ctxt);
            
            if (auto SubOpenParen = std::dynamic_pointer_cast<const GroupNode>(Sub)) {
                
                assert(SubOpenParen->getOperator() == SYMBOL_AST_GROUPLINEARSYNTAXPAREN);
                
                Args.append(SubOpenParen);
                
                Tok = TheParser->currentToken();
                
            } else if (auto SubOpenParen = std::dynamic_pointer_cast<const GroupMissingCloserNode>(Sub)) {
                
                assert(SubOpenParen->getOperator() == SYMBOL_AST_GROUPLINEARSYNTAXPAREN);
                
                Args.append(SubOpenParen);
                
                Tok = TheParser->currentToken();
                
            } else {
                assert(false);
            }
            
        } else {
            
            //
            // COMMENT, WHITESPACE, and NEWLINE are handled here
            //
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt);
        }
        
    } // while
    
    return std::make_shared<GroupNode>(SYMBOL_AST_GROUPLINEARSYNTAXPAREN, Args.getVector());
}

//
// Something like  a =.
//
NodePtr EqualParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    if (TokIn.Tok == TOKEN_EQUALDOT) {
        
        TheParser->nextToken(Ctxt);
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET, Args.getVector());
    }
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        Utils::notContiguousWarning(TokIn, Tok);
        
        TheParser->nextToken(Ctxt);
        
        Args.append(std::make_shared<LeafNode>(Tok));
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET, Args.getVector());
    }
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.append(Right);
    
    return std::make_shared<BinaryNode>(SYMBOL_SET, Args.getVector());
}


//
// Something like  \[Integral] f \[DifferentialD] x
//
NodePtr IntegralParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    auto TokIn = TheParser->currentToken();
    
    Args.append(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.IntegralFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(TokIn, TokIn, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto operand = TheParser->parse(Ctxt);
    
    Args.append(operand);
    
    Ctxt.IntegralFlag = false;
    
    Tok = TheParser->currentToken();
    
    Tok = Parser::eatAll(Tok, Ctxt, Args);
    
    if (Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD) {
        
        auto variable = TheParser->parse(Ctxt);
        
        Args.append(variable);
        
        return std::make_shared<PrefixBinaryNode>(PrefixBinaryOperatorToSymbol(TokIn.Tok), Args.getVector());
    }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok), Args.getVector());
}

//
// Gather all < > == <= => into a single node
//
NodePtr InequalityParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        auto Tok = TheParser->currentToken();
        
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
        
        if (isInequalityOperator(Tok.Tok)) {
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt);
            
            Tok = Parser::eatAll(Tok, Ctxt, Args);
            
            auto operand = TheParser->parse(Ctxt);
            
            Args.append(operand);
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            break;
        }
        
    } // while
    
    return std::make_shared<InfixNode>(SYMBOL_INEQUALITY, Args.getVector());
}

//
// Gather all \[VectorGreater] \[VectorLess] \[VectorGreaterEqual] \[VectorLessEqual] into a single node
//
NodePtr VectorInequalityParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        auto Tok = TheParser->currentToken();
        
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
        
        if (isVectorInequalityOperator(Tok.Tok)) {
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt);
            
            Tok = Parser::eatAll(Tok, Ctxt, Args);
            
            auto operand = TheParser->parse(Ctxt);
            
            Args.append(operand);
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            break;
        }
        
    } // while
    
    return std::make_shared<InfixNode>(SYMBOL_DEVELOPER_VECTORINEQUALITY, Args.getVector());
}

NodePtr InfixOperatorWithTrailingParselet::parse(NodeSeq& Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    
    Args.append(Left);
    
    auto TokIn = TheParser->currentToken();
    
    auto lastOperatorToken = TokIn;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source());
            
            auto Aborted = std::make_shared<LeafNode>(A);
            
            return Aborted;
        }
        
        
        auto Tok = TheParser->currentToken();
        
        Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if (isInfixOperator(Tok.Tok) &&
            InfixOperatorToSymbol(Tok.Tok) == InfixOperatorToSymbol(TokIn.Tok)) {
            
            Args.append(std::make_shared<LeafNode>(Tok));
            
            lastOperatorToken = Tok;
            
            auto allowTrailing = true;
            if (allowTrailing) {
                
                //
                // Something like  a;b  or  a,b
                //
                
                Tok = TheParser->nextToken(Ctxt);
                
                Tok = Parser::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
                
                if (isInfixOperator(Tok.Tok) &&
                    InfixOperatorToSymbol(Tok.Tok) == InfixOperatorToSymbol(TokIn.Tok)) {
                    
                    //
                    // Something like  a; ;
                    //
                    
                    lastOperatorToken = Tok;
                    
                    auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, "", Source(lastOperatorToken.Span.lines.start));
                    
                    Args.append(std::make_shared<LeafNode>(Implicit));
                    
                } else if (TheParser->isPossibleBeginningOfExpression(Tok, Ctxt)) {
                    
                    auto operand = TheParser->parse(Ctxt);
                    
                    Args.append(operand);
                    
                } else {
                    
                    //
                    // Not beginning of an expression
                    //
                    // For example:  a;&
                    //
                    
                    auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, "", Source(lastOperatorToken.Span.lines.end));
                    
                    Args.append(std::make_shared<LeafNode>(Implicit));
                    
                    break;
                }
                
            } else {
                
                Tok = TheParser->nextToken(Ctxt);
                
                Tok = Parser::eatAll(Tok, Ctxt, Args);
                
                auto operand = TheParser->parse(Ctxt);
                
                Args.append(operand);
            }
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            break;
        }
        
    } // while
    
    return std::make_shared<InfixNode>(InfixOperatorToSymbol(TokIn.Tok), Args.getVector());
}

//
// Error handling and Cleanup
//

NodePtr ExpectedPossibleExpressionErrorParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    if (isError(TokIn.Tok)) {
        
        //
        // If there is a Token error, then use that specific error
        //
        
        auto SyntaxErrorEnum = TokenErrorToSyntaxError(TokIn.Tok);
        
        auto Error = std::make_shared<SyntaxErrorNode>(SyntaxErrorEnum, std::vector<NodePtr> { std::make_shared<LeafNode>(TokIn) });
        
        return Error;
    }
    
    //
    // If NOT a Token error, then just use a generic error
    //
    
    auto SyntaxErrorEnum = SYNTAXERROR_EXPECTEDPOSSIBLEEXPRESSION;
    
    auto Error = std::make_shared<SyntaxErrorNode>(SyntaxErrorEnum, std::vector<NodePtr> { std::make_shared<LeafNode>(TokIn) });
    
    return Error;
}

