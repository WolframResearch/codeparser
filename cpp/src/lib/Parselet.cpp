
#include "Parselet.h"

#include "SourceManager.h"
#include "Symbol.h"
#include "Utils.h"

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
    
    return NodePtr(new LeafNode(TokIn));
}


//
// something like  x  or x_
//
NodePtr SymbolParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    if (Tok.Tok() == TOKEN_UNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok());
        
        Ctxt.UnderCount = UNDER_1;
        
        NodeSeq Args;
        Args.reserve(1);
        Args.append(std::move(Sym));
        
        return underParselet->parseContextSensitive(std::move(Args), Ctxt);
        
    } else if (Tok.Tok() == TOKEN_UNDERUNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok());
        
        Ctxt.UnderCount = UNDER_2;
        
        NodeSeq Args;
        Args.reserve(1);
        Args.append(std::move(Sym));
        
        return underParselet->parseContextSensitive(std::move(Args), Ctxt);
        
    } else if (Tok.Tok() == TOKEN_UNDERUNDERUNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok());
        
        Ctxt.UnderCount = UNDER_3;
        
        NodeSeq Args;
        Args.reserve(1);
        Args.append(std::move(Sym));
        
        return underParselet->parseContextSensitive(std::move(Args), Ctxt);
        
    } else if (Tok.Tok() == TOKEN_UNDERDOT) {
        
        NodeSeq Args;
        Args.reserve(1 + 1);
        Args.append(std::move(Sym));
        Args.append(NodePtr(new LeafNode(Tok)));
        
        TheParser->nextToken(Ctxt);
        
        return NodePtr(new OptionalDefaultPatternNode(std::move(Args)));
    }
    
    LeafSeq ArgsTest;
    
    Tok = Parser::eatAll(Ctxt, ArgsTest);
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    // It is necessary to go to colonParselet->parse here (even though it seems non-contextSensitive)
    // because in e.g.,  a_*b:f[]  the b is the last node in the Times expression and needs to bind with  :f[]
    // Parsing  a_*b  completely, and then parsing  :f[]  would be wrong.
    //
    if ((Ctxt.Flag & PARSER_COLON) != PARSER_COLON) {
        
        if (Tok.Tok() == TOKEN_COLON) {
            
            Ctxt.Flag |= PARSER_PARSED_SYMBOL;
            
            NodeSeq Args;
            Args.reserve(1 + 1);
            Args.append(std::move(Sym));
            Args.appendIfNonEmpty(std::move(ArgsTest));
            
            auto& colonParselet = TheParser->findInfixParselet(Tok.Tok());
            
            return colonParselet->parse(std::move(Args), Ctxt);
        }
    }
    
    return Sym;
}


NodePtr PrefixOperatorParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Operand;
    if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        Operand = TheParser->parse(Ctxt);
    } else {
        Operand = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
    }
    
    Utils::differentLineWarning(TokIn, Tok);
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Operand));
    
    return NodePtr(new PrefixNode(PrefixOperatorToSymbol(TokIn.Tok()), std::move(Args)));
}


NodePtr BinaryOperatorParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        Right = TheParser->parse(Ctxt);
    } else {
        Right = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Right));
    
    return NodePtr(new BinaryNode(BinaryOperatorToSymbol(TokIn.Tok()), std::move(Args)));
}

NodePtr InfixOperatorParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto TokIn = TheParser->currentToken();
    
    auto& Op = InfixOperatorToSymbol(TokIn.Tok());
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source(TheSourceManager->getSourceLocation()));
            
            auto Aborted = NodePtr(new LeafNode(A));
            
            return Aborted;
        }
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if (isInfixOperator(Tok1.Tok()) &&
            InfixOperatorToSymbol(Tok1.Tok()) == Op) {
            
            TheParser->nextToken(Ctxt);
            
            LeafSeq ArgsTest2;
            
            auto Tok2 = Parser::eatAll(Ctxt, ArgsTest2);
            
            auto wasCloser = false;
            
            NodePtr Operand;
            bool possible;
            if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
                Operand = TheParser->parse(Ctxt);
                possible = true;
            } else {
                Operand = TheParser->handleNotPossible(Tok1, Ctxt, &wasCloser);
                possible = false;
            }
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            if (!wasCloser) {
                Args.appendIfNonEmpty(std::move(ArgsTest2));
            }
            Args.append(std::move(Operand));
            
            if (!possible) {
                return NodePtr(new InfixNode(Op, std::move(Args)));
            }
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            return NodePtr(new InfixNode(Op, std::move(Args)));
        }
        
    } // while
}


NodePtr PostfixOperatorParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    Utils::differentLineWarning(Left, TokIn);
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    return NodePtr(new PostfixNode(PostfixOperatorToSymbol(TokIn.Tok()), std::move(Args)));
}


NodePtr GroupParselet::parse(ParserContext CtxtIn) const {
    
    auto Opener = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    
    TheParser->nextToken(Ctxt);
    
    auto& Op = GroupOpenerToSymbol(Opener.Tok());
    
    auto CloserTok = GroupOpenerToCloser(Opener.Tok());
    Ctxt.Closer = CloserTok;
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(NodePtr(new LeafNode(std::move(Opener))));
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    // ADDENDUM: Actually, there may be more than 1 good node
    // e.g. {1\\2}
    //
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
            LeafSeq ArgsTest1;
            
            auto Tok = Parser::eatAll(Ctxt, ArgsTest1);
            
            if (Tok.Tok() == CloserTok) {
                
                //
                // Everything is good
                //
                
                auto Ctxt2 = Ctxt;
                Ctxt2.GroupDepth--;
                
                TheParser->nextToken(Ctxt2);
                
                Args.reserve(Args.size() + 1 + 1);
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(std::move(Tok))));
                
                auto group = NodePtr(new GroupNode(Op, std::move(Args)));
                
                return group;
            }
            if (isCloser(Tok.Tok())) {
                
                //
                // some other closer
                //
                // e.g.,   { ( }
                //
                // FIXME: { ) }  is not handled well
                //
                
                auto group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
                
                return group;
            }
            if (Tok.Tok() == TOKEN_ENDOFFILE) {
                
                //
                // Handle something like   { a EOF
                //
                
                auto group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
                
                return group;
            }
            
            //
            // Handle the expression
            //
            
            auto Ctxt2 = Ctxt;
            Ctxt2.Flag.clear(PARSER_COLON);
            Ctxt2.Prec = PRECEDENCE_LOWEST;
            Ctxt2.Assoc = ASSOCIATIVITY_NONE;
            Ctxt2.UnderCount = UNDER_UNKNOWN;
            
            auto wasCloser = false;
            
            NodePtr Operand;
            if (TheParser->isPossibleBeginningOfExpression(Ctxt2)) {
                Operand = TheParser->parse(Ctxt2);
            } else {
                Operand = TheParser->handleNotPossible(Tok, Ctxt2, &wasCloser);
            }
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            
            assert(!wasCloser);
            
            //
            // Always append here
            //
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(std::move(Operand));
        }
        
    } // while
}


NodePtr CallParselet::parse(NodeSeq Head, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto& groupParselet = TheParser->findPrefixParselet(TokIn.Tok());
    
    auto Right = groupParselet->parse(Ctxt);
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(std::move(Right));
    
    return NodePtr(new CallNode(std::move(Head), std::move(Args)));
}


//
// StartOfLine
//

NodePtr StartOfLineParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Flag |= PARSER_STRINGIFY_CURRENTLINE;
    
    TheParser->nextToken(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
    Ctxt.Flag.clear(PARSER_STRINGIFY_CURRENTLINE);
    
    //
    // We know there is just a token here, either TOKEN_ERROR_EMPTYSTRING or a legit string
    //
    TheParser->nextToken(Ctxt);
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.append(NodePtr(new LeafNode(Tok)));
    
    return NodePtr(new StartOfLineNode(StartOfLineOperatorToSymbol(TokIn.Tok()), std::move(Args)));
}


//
// prefix
//
// Something like  _a
//
NodePtr UnderParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Under = NodePtr(new LeafNode(TokIn));
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
    NodePtr Blank;
    if (Tok.Tok() == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->findContextSensitivePrefixParselet(Tok.Tok());
        
        auto Sym2 = symbolParselet->parseContextSensitive(Ctxt);
        
        NodeSeq Args;
        Args.reserve(1 + 1);
        Args.append(std::move(Under));
        Args.append(std::move(Sym2));
        
        switch (TokIn.Tok()) {
            case TOKEN_UNDER:
                Blank = NodePtr(new BlankNode(std::move(Args)));
                break;
            case TOKEN_UNDERUNDER:
                Blank = NodePtr(new BlankSequenceNode(std::move(Args)));
                break;
            case TOKEN_UNDERUNDERUNDER:
                Blank = NodePtr(new BlankNullSequenceNode(std::move(Args)));
                break;
            default:
                assert(false);
                break;
        }
        
    } else {
        Blank = std::move(Under);
    }
    
    LeafSeq ArgsTest;
    
    Tok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest);
    
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
    if ((Ctxt.Flag & PARSER_COLON) != PARSER_COLON) {
        
        if (Tok.Tok() == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok());
            
            NodeSeq BlankSeq;
            BlankSeq.reserve(1 + 1);
            BlankSeq.append(std::move(Blank));
            BlankSeq.appendIfNonEmpty(std::move(ArgsTest));
            
            return colonParselet->parseContextSensitive(std::move(BlankSeq), Ctxt);
        }
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
NodePtr UnderParselet::parseContextSensitive(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1/*speculative for Right*/);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    auto UnderCount = CtxtIn.UnderCount;
    
    auto Ctxt = CtxtIn;
    Ctxt.UnderCount = UNDER_UNKNOWN;
    
    TheParser->nextToken(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
    if (Tok.Tok() == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->findContextSensitivePrefixParselet(Tok.Tok());
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);
        
        Args.append(std::move(Right));
    }
    
    NodePtr Pat;
    switch (UnderCount) {
        case UNDER_1:
            Pat = NodePtr(new PatternBlankNode(std::move(Args)));
            break;
        case UNDER_2:
            Pat = NodePtr(new PatternBlankSequenceNode(std::move(Args)));
            break;
        case UNDER_3:
            Pat = NodePtr(new PatternBlankNullSequenceNode(std::move(Args)));
            break;
        default:
            assert(false);
            break;
    }
    
    LeafSeq ArgsTest;
    
    Tok = Parser::eatAll(Ctxt, ArgsTest);
    
    //
    // For something like a:b_c:d when parsing _
    // ColonFlag == true
    //
    if ((Ctxt.Flag & PARSER_COLON) != PARSER_COLON) {
        
        if (Tok.Tok() == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok());
            
            NodeSeq PatSeq;
            PatSeq.reserve(1 + 1);
            PatSeq.append(std::move(Pat));
            PatSeq.appendIfNonEmpty(std::move(ArgsTest));
            
            return colonParselet->parseContextSensitive(std::move(PatSeq), Ctxt);
        }
    }
    
    return Pat;
}

//
// Something like  a ~f~ b
//
NodePtr TildeParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto FirstTilde = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest1;
    
    auto FirstTok = Parser::eatAll(Ctxt, ArgsTest1);
    
    if (!TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        
        bool wasCloser;
        
        auto NotPossible = TheParser->handleNotPossible(FirstTilde, Ctxt, &wasCloser);
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        if (!wasCloser) {
            Args.appendIfNonEmpty(std::move(ArgsTest1));
        }
        Args.append(std::move(NotPossible));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDOPERAND, std::move(Args)));
        
        return Error;
    }
    
    Utils::differentLineWarning(FirstTilde, FirstTok);
    
    auto Middle = TheParser->parse(Ctxt);
    
    LeafSeq ArgsTest2;
    
    auto Tok1 = Parser::eatAll(Ctxt, ArgsTest2);
    
    if (Tok1.Tok() != TOKEN_TILDE) {
        
        TheParser->nextToken(Ctxt);
        
        //
        // Something like   a ~f b
        //
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(Middle));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
        
        return Error;
    }
    
    Utils::differentLineWarning(FirstTok, Tok1);
    
    LeafSeq ArgsTest3;
    
    TheParser->nextToken(Ctxt);
    
    auto Tok2 = Parser::eatAll(Ctxt, ArgsTest3);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        Right = TheParser->parse(Ctxt);
    } else {
        Right = TheParser->handleNotPossible(Tok1, Ctxt, &wasCloser);
    }
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(FirstTilde)));
    Args.appendIfNonEmpty(std::move(ArgsTest1));
    Args.append(std::move(Middle));
    Args.appendIfNonEmpty(std::move(ArgsTest2));
    Args.append(NodePtr(new LeafNode(Tok1)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest3));
    }
    Args.append(std::move(Right));
    
    return NodePtr(new TernaryNode(SYMBOL_AST_TERNARYTILDE, std::move(Args)));
}



//
// Something like  symbol:object
//
// when parsing a in a:b  then ColonFlag is false
// when parsing b in a:b  then ColonFlag is true
//
NodePtr ColonParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    assert((CtxtIn.Flag & PARSER_COLON) != PARSER_COLON);
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.Flag |= PARSER_COLON;
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest1;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest1);
    
    auto wasCloser = false;
    
    NodePtr Right;
    bool possible;
    if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        Right = TheParser->parse(Ctxt);
        possible = true;
    } else {
        Right = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
        possible = false;
    }
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest1));
    }
    Args.append(std::move(Right));
    
    if (!possible) {
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_COLONERROR, std::move(Args)));
        
        return Error;
    }
    
    if ((CtxtIn.Flag & PARSER_PARSED_SYMBOL) != PARSER_PARSED_SYMBOL) {
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_COLONERROR, std::move(Args)));
        
        return Error;
    }
    
    Ctxt.Flag.clear(PARSER_PARSED_SYMBOL);
    
    auto Pat = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
    
    LeafSeq ArgsTest2;
    
    Tok = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest2);
    
    if (Tok.Tok() == TOKEN_COLON) {
        
        Ctxt.Flag.clear(PARSER_COLON);
        
        NodeSeq PatSeq;
        PatSeq.reserve(1 + 1);
        PatSeq.append(std::move(Pat));
        PatSeq.appendIfNonEmpty(std::move(ArgsTest2));
        
        return parseContextSensitive(std::move(PatSeq), Ctxt);
    }
    
    return Pat;
}

//
// Something like  pattern:optional
//
// Called from other parselets
//
NodePtr ColonParselet::parseContextSensitive(NodeSeq Left, ParserContext CtxtIn) const {
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert((CtxtIn.Flag & PARSER_COLON) != PARSER_COLON);
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        Right = TheParser->parse(Ctxt);
    } else {
        Right = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Right));
    
    return NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
}

//
// Something like  a /: b = c
//
// a   /:   b   =   c
// ^~~~~ Args at the start
//       ^~~ ArgsTest1
//           ^~~ ArgsTest2
//
NodePtr SlashColonParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest1;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest1);
    
    if (!TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        
        bool wasCloser;
        
        auto NotPossible = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        if (!wasCloser) {
            Args.appendIfNonEmpty(std::move(ArgsTest1));
        }
        Args.append(std::move(NotPossible));
        
        return NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDOPERAND, std::move(Args)));
    }
    
    auto Middle = TheParser->parse(Ctxt);
    
    LeafSeq ArgsTest2;
    
    Tok = Parser::eatAll(Ctxt, ArgsTest2);
    
    if (Tok.Tok() == TOKEN_EQUAL) {
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(Middle));
        Args.appendIfNonEmpty(std::move(ArgsTest2));
        
        Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
        
        auto& equalParselet = TheParser->findInfixParselet(Tok.Tok());
        
        auto N = equalParselet->parse(std::move(Args), Ctxt);
        
        return N;
        
    } else if (Tok.Tok() == TOKEN_COLONEQUAL) {
        
        NodeSeq Args2;
        Args2.reserve(1 + 1);
        Args2.append(std::move(Middle));
        Args2.appendIfNonEmpty(std::move(ArgsTest2));
        
        auto& colonEqualParselet = TheParser->findInfixParselet(Tok.Tok());
        
        auto N = colonEqualParselet->parse(std::move(Args2), Ctxt);
        
        auto& C = N->getChildrenDestructive();
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(NodePtr(new NodeSeqNode(std::move(C))));
        
        return NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
        
    } else if (Tok.Tok() == TOKEN_EQUALDOT) {
        
        NodeSeq Args2;
        Args2.reserve(1 + 1);
        Args2.append(std::move(Middle));
        Args2.appendIfNonEmpty(std::move(ArgsTest2));
        
        auto& equalParselet = TheParser->findInfixParselet(Tok.Tok());
        
        auto N = equalParselet->parse(std::move(Args2), Ctxt);
        
        auto& C = N->getChildrenDestructive();
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(NodePtr(new NodeSeqNode(std::move(C))));
        
        return NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
    }
    
    //
    // Anything other than:
    // a /: b = c
    // a /: b := c
    // a /: b =.
    //
    
    if (Tok.Tok() == TOKEN_ENDOFFILE) {
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(Middle));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDSET, std::move(Args)));
        
        return Error;
    }
    
    TheParser->nextToken(Ctxt);
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest1));
    Args.append(std::move(Middle));
    Args.appendIfNonEmpty(std::move(ArgsTest2));
    Args.append(NodePtr(new LeafNode(Tok)));
    
    auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDSET, std::move(Args)));
    
    return Error;
}



//
// Something like  \( x \)
//
NodePtr LinearSyntaxOpenParenParselet::parse(ParserContext CtxtIn) const {
    
    auto Opener = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    Ctxt.Flag |= PARSER_LINEARSYNTAX;
    
    TheParser->nextToken(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
    auto CloserTok = TOKEN_LINEARSYNTAX_CLOSEPAREN;
    Ctxt.Closer = CloserTok;
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(NodePtr(new LeafNode(Opener)));
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source(TheSourceManager->getSourceLocation()));
            
            auto Aborted = NodePtr(new LeafNode(A));
            
            return Aborted;
        }
        
        
        if (Tok.Tok() == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   \( a EOF
            //
            
            auto group = NodePtr(new GroupMissingCloserNode(SYMBOL_AST_GROUPLINEARSYNTAXPAREN, std::move(Args)));
            
            return group;
        }
        if (Tok.Tok() == CloserTok) {
            
            Args.reserve(Args.size() + 1);
            Args.append(NodePtr(new LeafNode(Tok)));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            
            TheParser->nextToken(Ctxt2);
            
            return NodePtr(new GroupNode(SYMBOL_AST_GROUPLINEARSYNTAXPAREN, std::move(Args)));
        }
        
        //
        // Do not check for other closers here
        //
        // As long as \( \) parses by just doing tokenization, then cannot reliably test for other closers
        //
        // e.g., \( ( \)  and  \( ) \)  is completely valid syntax
        //
        
        if (Tok.Tok() == TOKEN_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse(Ctxt);
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.append(std::move(Sub));
            
            Tok = TheParser->currentToken();
            
        } else {
            
            //
            // COMMENT, WHITESPACE, and NEWLINE are handled here
            //
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.append(NodePtr(new LeafNode(Tok)));
            
            TheParser->nextToken(Ctxt);
            
            Tok = TheParser->currentToken();
        }
        
    } // while
}

//
// Something like  a =.
//
// a /: b = c  and  a /: b = .  are also handled here
//
NodePtr EqualParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    if (TokIn.Tok() == TOKEN_EQUALDOT) {
        
        TheParser->nextToken(Ctxt);
        
        NodeSeq Args;
        Args.reserve(1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        
        if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            return NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
        }
        
        return NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
    }
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest);
    
    if (Tok.Tok() == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        Utils::notContiguousWarning(TokIn, Tok);
        
        TheParser->nextToken(Ctxt);
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest));
        Args.append(NodePtr(new LeafNode(Tok)));
        
        if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            return NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
        }
        
        return NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
    }
    
    auto wasInsideSlashColon = ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON);
    
    Ctxt.Flag.clear(PARSER_INSIDE_SLASHCOLON);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
        Right = TheParser->parse(Ctxt);
    } else {
        Right = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Right));
    
    if (wasInsideSlashColon) {
        return NodePtr(new TernaryNode(SYMBOL_TAGSET, std::move(Args)));
    }
    
    return NodePtr(new BinaryNode(SYMBOL_SET, std::move(Args)));
}


//
// Something like  \[Integral] f \[DifferentialD] x
//
NodePtr IntegralParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.Flag |= PARSER_INTEGRAL;
    
    TheParser->nextToken(Ctxt);
    
    LeafSeq ArgsTest1;
    
    auto Tok = Parser::eatAll(Ctxt, ArgsTest1);
    
    if (!TheParser->isPossibleBeginningOfExpression(Ctxt) ||
        (TheParser->getTokenPrecedence(Tok, CtxtIn, true, nullptr) < PRECEDENCE_LONGNAME_INTEGRAL)) {
        
        bool wasCloser;
        
        auto NotPossible = TheParser->handleNotPossible(TokIn, Ctxt, &wasCloser);
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        if (!wasCloser) {
            Args.appendIfNonEmpty(std::move(ArgsTest1));
        }
        Args.append(std::move(NotPossible));
        
        return NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDINTEGRAND, std::move(Args)));
    }
    
    Utils::differentLineWarning(TokIn, Tok);
    
    auto operand = TheParser->parse(Ctxt);
    
    Ctxt.Flag.clear(PARSER_INTEGRAL);
    
    LeafSeq ArgsTest2;
    
    Tok = Parser::eatAll(Ctxt, ArgsTest2);
    
    if (Tok.Tok() != TOKEN_LONGNAME_DIFFERENTIALD) {
        
        NodeSeq Args;
        Args.reserve(1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(operand));
        
        return NodePtr(new PrefixNode(PrefixOperatorToSymbol(TokIn.Tok()), std::move(Args)));
    }
    
    Utils::differentLineWarning(TokIn, Tok);
    
    auto& differentialDparselet = TheParser->findPrefixParselet(Tok.Tok());
    
    auto variable = differentialDparselet->parse(Ctxt);
    
    NodeSeq Args;
    Args.reserve(1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest1));
    Args.append(std::move(operand));
    Args.appendIfNonEmpty(std::move(ArgsTest2));
    Args.append(std::move(variable));
    
    return NodePtr(new PrefixBinaryNode(PrefixBinaryOperatorToSymbol(TokIn.Tok()), std::move(Args)));
}

//
// Gather all < > == <= => into a single node
//
NodePtr InequalityParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source(TheSourceManager->getSourceLocation()));
            
            auto Aborted = NodePtr(new LeafNode(A));
            
            return Aborted;
        }
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
        
        if (isInequalityOperator(Tok1.Tok())) {
            
            TheParser->nextToken(Ctxt);
            
            LeafSeq ArgsTest2;
            
            auto Tok2 = Parser::eatAll(Ctxt, ArgsTest2);
            
            auto wasCloser = false;
            
            NodePtr Operand;
            bool possible;
            if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
                Operand = TheParser->parse(Ctxt);
                possible = true;
            } else {
                Operand = TheParser->handleNotPossible(Tok1, Ctxt, &wasCloser);
                possible = false;
            }
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            if (!wasCloser) {
                Args.appendIfNonEmpty(std::move(ArgsTest2));
            }
            Args.append(std::move(Operand));
            
            if (!possible) {
                return NodePtr(new InfixNode(SYMBOL_INEQUALITY, std::move(Args)));
            }
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            return NodePtr(new InfixNode(SYMBOL_INEQUALITY, std::move(Args)));
        }
        
    } // while
}

//
// Gather all \[VectorGreater] \[VectorLess] \[VectorGreaterEqual] \[VectorLessEqual] into a single node
//
NodePtr VectorInequalityParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
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
            LeafSeq ArgsTest1;
            
            auto Tok1 = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
            
            if (isVectorInequalityOperator(Tok1.Tok())) {
                
                TheParser->nextToken(Ctxt);
                
                LeafSeq ArgsTest2;
                
                auto Tok2 = Parser::eatAll(Ctxt, ArgsTest2);
                
                auto wasCloser = false;
                
                NodePtr Operand;
                bool possible;
                if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
                    Operand = TheParser->parse(Ctxt);
                    possible = true;
                } else {
                    Operand = TheParser->handleNotPossible(Tok1, Ctxt, &wasCloser);
                    possible = false;
                }
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(Tok1)));
                if (!wasCloser) {
                    Args.appendIfNonEmpty(std::move(ArgsTest2));
                }
                Args.append(std::move(Operand));
                
                if (!possible) {
                    return NodePtr(new InfixNode(SYMBOL_DEVELOPER_VECTORINEQUALITY, std::move(Args)));
                }
                
            } else {
                
                //
                // Tok.Tok != TokIn.Tok, so break
                //
                
                return NodePtr(new InfixNode(SYMBOL_DEVELOPER_VECTORINEQUALITY, std::move(Args)));
            }
        }
        
    } // while
}

NodePtr InfixOperatorWithTrailingParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    Args.reserve(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto TokIn = TheParser->currentToken();
    
    auto& Op = InfixOperatorToSymbol(TokIn.Tok());
    
    auto lastOperatorToken = TokIn;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto A = Token(TOKEN_ERROR_ABORTED, "", Source(TheSourceManager->getSourceLocation()));
            
            auto Aborted = NodePtr(new LeafNode(A));
            
            return Aborted;
        }
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = Parser::eatAndPreserveToplevelNewlines(CtxtIn, ArgsTest1);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if (isInfixOperator(Tok1.Tok()) &&
            InfixOperatorToSymbol(Tok1.Tok()) == Op) {
            
            lastOperatorToken = Tok1;
            
            //
            // ALLOWTRAILING CODE
            //
            
            //
            // Something like  a;b  or  a,b
            //
            
            TheParser->nextToken(Ctxt);
            
            LeafSeq ArgsTest2;
            
            auto Tok2 = Parser::eatAndPreserveToplevelNewlines(Ctxt, ArgsTest2);
            
            if (isInfixOperator(Tok2.Tok()) &&
                InfixOperatorToSymbol(Tok2.Tok()) == Op) {
                
                //
                // Something like  a; ;
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, "", Source(lastOperatorToken.Src.start()));
                
                lastOperatorToken = Tok2;
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(Tok1)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
            } else if (TheParser->isPossibleBeginningOfExpression(Ctxt)) {
                
                auto operand = TheParser->parse(Ctxt);
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(std::move(Tok1))));
                Args.appendIfNonEmpty(std::move(ArgsTest2));
                Args.append(std::move(operand));
                
            } else {
                
                //
                // Not beginning of an expression
                //
                // For example:  a;&
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, "", Source(lastOperatorToken.Src.end()));
                
                Args.reserve(Args.size() + 1 + 1);
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(Tok1)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
                return NodePtr(new InfixNode(Op, std::move(Args)));
            }
            
        } else {
            
            return NodePtr(new InfixNode(Op, std::move(Args)));
        }
        
    } // while
}
