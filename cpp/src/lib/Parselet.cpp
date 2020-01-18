
#include "Parselet.h"

#include "API.h" // for ParserSession


NodePtr LeafParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    return NodePtr(new LeafNode(std::move(TokIn)));
}

//
// parsing x in _x
//
// we know it can only be a symbol
//
// Called from other parselets
//
NodePtr SymbolParselet::parseContextSensitive(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    return NodePtr(new LeafNode(TokIn));
}

//
// something like  x  or x_
//
NodePtr SymbolParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken();
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            auto& under1Parselet = TheParser->getContextSensitiveUnder1Parselet();
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return under1Parselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            auto& under2Parselet = TheParser->getContextSensitiveUnder2Parselet();
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return under2Parselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            auto& under3Parselet = TheParser->getContextSensitiveUnder3Parselet();
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return under3Parselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERDOT.value(): {
            
            NodeSeq Args(1 + 1);
            Args.append(std::move(Sym));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            TheParser->nextToken(Tok);
            
            return NodePtr(new OptionalDefaultPatternNode(std::move(Args)));
        }
    }
    
    LeafSeq ArgsTest;
    
    Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest);
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    // It is necessary to go to colonParselet->parse here (even though it seems non-contextSensitive)
    // because in e.g.,  a_*b:f[]  the b is the last node in the Times expression and needs to bind with  :f[]
    // Parsing  a_*b  completely, and then parsing  :f[]  would be wrong.
    //
    if ((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            Ctxt.Flag |= PARSER_PARSED_SYMBOL;
            
            NodeSeq Args(1 + 1);
            Args.append(std::move(Sym));
            Args.appendIfNonEmpty(std::move(ArgsTest));
            
            auto& colonParselet = TheParser->findInfixParselet(Tok.Tok);
            
            return colonParselet->parse(std::move(Args), Tok, Ctxt);
        }
    }
    
    return Sym;
}


PrefixOperatorParselet::PrefixOperatorParselet(TokenEnum Tok, Precedence precedence) : precedence(precedence), Op(PrefixOperatorToSymbol(Tok)) {}

NodePtr PrefixOperatorParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence();
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Operand;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Operand = TheParser->parse(Tok, Ctxt);
    } else {
        Operand = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Operand));
    
    return NodePtr(new PrefixNode(Op, std::move(Args)));
}


BinaryOperatorParselet::BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Associativity assoc) : precedence(precedence), assoc(assoc), Op(BinaryOperatorToSymbol(Tok)) {}

NodePtr BinaryOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence();
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
    } else {
        Right = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Right));
    
    return NodePtr(new BinaryNode(Op, std::move(Args)));
}


InfixOperatorParselet::InfixOperatorParselet(TokenEnum Tok, Precedence precedence) : precedence(precedence), Op(InfixOperatorToSymbol(Tok)) {}

NodePtr InfixOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    Ctxt.Prec = getPrecedence();
    
    while (true) {

#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = TheParser->currentToken();
        Tok1 = TheParser->eatAndPreserveToplevelNewlines(Tok1, Ctxt, ArgsTest1);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if (InfixOperatorToSymbol(Tok1.Tok) == Op) {
            
            TheParser->nextToken(Tok1);
            
            LeafSeq ArgsTest2;
            
            auto Tok2 = TheParser->currentToken();
            Tok2 = TheParser->eatAll(Tok2, Ctxt, ArgsTest2);
            
            auto wasCloser = false;
            
            NodePtr Operand;
            bool possible;
            if (Tok2.Tok.isPossibleBeginningOfExpression()) {
                Operand = TheParser->parse(Tok2, Ctxt);
                possible = true;
            } else {
                Operand = TheParser->handleNotPossible(Tok2, Tok1, Ctxt, &wasCloser);
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


PostfixOperatorParselet::PostfixOperatorParselet(TokenEnum Tok, Precedence precedence) : precedence(precedence), Op(PostfixOperatorToSymbol(Tok)) {}

NodePtr PostfixOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    NodeSeq Args(1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    return NodePtr(new PostfixNode(Op, std::move(Args)));
}


GroupParselet::GroupParselet(TokenEnum Opener) :
    Op(GroupOpenerToSymbol(Opener)), Closr(GroupOpenerToCloser(Opener)) {}

NodePtr GroupParselet::parse(Token firstTok, ParserContext Ctxt) const {
    
    auto OpenerT = firstTok;
    
    Ctxt.GroupDepth++;
    
    TheParser->nextToken(firstTok);
    
    Ctxt.Closr = Closr;
    
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(std::move(OpenerT))));
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    // ADDENDUM: Actually, there may be more than 1 "good" node
    // e.g. {1\\2}
    //
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest1;
        
        auto Tok = TheParser->currentToken();
        Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
        
        if (TokenToCloser(Tok.Tok) == Closr) {
            
            //
            // Everything is good
            //
            
            TheParser->nextToken(Tok);
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(NodePtr(new LeafNode(std::move(Tok))));
            
            auto group = NodePtr(new GroupNode(Op, std::move(Args)));
            
            return group;
        }
        if (Tok.Tok.isCloser()) {
            
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
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   { a EOF
            //
            
            auto group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
            
            return group;
        }
        
        //
        // Handle the expression
        //
        
        auto wasCloser = false;
        
        NodePtr Operand;
        if (Tok.Tok.isPossibleBeginningOfExpression()) {
            Operand = TheParser->parse(Tok, Ctxt);
        } else {
            Operand = TheParser->handleNotPossible(Tok, Tok, Ctxt, &wasCloser);
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
        
    } // while
}


CallParselet::CallParselet(PrefixParseletPtr GP) : GP(std::move(GP)) {}

NodePtr CallParselet::parse(NodeSeq Head, Token TokIn, ParserContext Ctxt) const {
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    
    auto Right = GP->parse(TokIn, Ctxt);
    
    NodeSeq Args(1);
    Args.append(std::move(Right));
    
    return NodePtr(new CallNode(std::move(Head), std::move(Args)));
}


#if STARTOFLINE

//
// StartOfLine
//

NodePtr StartOfLineParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken_stringifyCurrentLine(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
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
// StartOfFile
//

NodePtr StartOfFileParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken_stringifyCurrentLine(Ctxt);
    
    auto Tok = TheParser->currentToken();
    
    //
    // We know there is just a token here, either TOKEN_ERROR_EMPTYSTRING or a legit string
    //
    TheParser->nextToken(Ctxt);
    
    NodeSeq Args;
    Args.reserve(1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.append(NodePtr(new LeafNode(Tok)));
    
    return NodePtr(new StartOfFileNode(StartOfFileOperatorToSymbol(TokIn.Tok()), std::move(Args)));
}

#endif // STARTOFLINE


UnderParselet::UnderParselet(size_t count) : count(count) {}

//
// prefix
//
// Something like  _a
//
NodePtr UnderParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    auto Under = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken();
    
    NodePtr Blank;
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->getContextSensitiveSymbolParselet();
        
        auto Sym2 = symbolParselet->parseContextSensitive(Tok, Ctxt);
        
        NodeSeq Args(1 + 1);
        Args.append(std::move(Under));
        Args.append(std::move(Sym2));
        
        switch (TokIn.Tok.value()) {
            case TOKEN_UNDER.value():
                Blank = NodePtr(new BlankNode(std::move(Args)));
                break;
            case TOKEN_UNDERUNDER.value():
                Blank = NodePtr(new BlankSequenceNode(std::move(Args)));
                break;
            case TOKEN_UNDERUNDERUNDER.value():
                Blank = NodePtr(new BlankNullSequenceNode(std::move(Args)));
                break;
            default:
                assert(false);
                break;
        }
        
        Tok = TheParser->currentToken();
        
    } else if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // It's nice to include the error inside of the blank
        //
        
        auto& parselet = TheParser->findPrefixParselet(Tok.Tok);
        
        auto ErrorSym2 = parselet->parse(Tok, Ctxt);
        
        NodeSeq Args(1 + 1);
        Args.append(std::move(Under));
        Args.append(std::move(ErrorSym2));
        
        switch (TokIn.Tok.value()) {
            case TOKEN_UNDER.value():
                Blank = NodePtr(new BlankNode(std::move(Args)));
                break;
            case TOKEN_UNDERUNDER.value():
                Blank = NodePtr(new BlankSequenceNode(std::move(Args)));
                break;
            case TOKEN_UNDERUNDERUNDER.value():
                Blank = NodePtr(new BlankNullSequenceNode(std::move(Args)));
                break;
            default:
                assert(false);
                break;
        }
        
        Tok = TheParser->currentToken();
        
    } else {
        Blank = std::move(Under);
    }
    
    LeafSeq ArgsTest;
    
    Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest);
    
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
    if ((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->getContextSensitiveColonParselet();
            
            NodeSeq BlankSeq(1 + 1);
            BlankSeq.append(std::move(Blank));
            BlankSeq.appendIfNonEmpty(std::move(ArgsTest));
            
            return colonParselet->parseContextSensitive(std::move(BlankSeq), Tok, Ctxt);
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
NodePtr UnderParselet::parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1 + 1 + 1/*speculative for Right*/);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken();
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->getContextSensitiveSymbolParselet();
        
        auto Right = symbolParselet->parseContextSensitive(Tok, Ctxt);
        
        Args.append(std::move(Right));
        
        Tok = TheParser->currentToken();
        
    } else if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // It's nice to include the error inside of the blank
        //
        
        auto& parselet = TheParser->findPrefixParselet(Tok.Tok);
        
        auto ErrorRight = parselet->parse(Tok, Ctxt);
        
        Args.append(std::move(ErrorRight));
        
        Tok = TheParser->currentToken();
    }
    
    NodePtr Pat;
    switch (count) {
        case 1:
            Pat = NodePtr(new PatternBlankNode(std::move(Args)));
            break;
        case 2:
            Pat = NodePtr(new PatternBlankSequenceNode(std::move(Args)));
            break;
        case 3:
            Pat = NodePtr(new PatternBlankNullSequenceNode(std::move(Args)));
            break;
        default:
            assert(false);
            break;
    }
    
    LeafSeq ArgsTest;
    
    Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest);
    
    //
    // For something like a:b_c:d when parsing _
    // ColonFlag == true
    //
    if ((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->getContextSensitiveColonParselet();
            
            NodeSeq PatSeq(1 + 1);
            PatSeq.append(std::move(Pat));
            PatSeq.appendIfNonEmpty(std::move(ArgsTest));
            
            return colonParselet->parseContextSensitive(std::move(PatSeq), Tok, Ctxt);
        }
    }
    
    return Pat;
}

//
// Something like  a ~f~ b
//
NodePtr TildeParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    auto FirstTilde = TokIn;
    
    Ctxt.Prec = getPrecedence();
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto FirstTok = TheParser->currentToken();
    FirstTok = TheParser->eatAll(FirstTok, Ctxt, ArgsTest1);
    
    if (!FirstTok.Tok.isPossibleBeginningOfExpression()) {
        
        //
        // Something like  a ~&
        //
        
        bool wasCloser;
        
        auto NotPossible = TheParser->handleNotPossible(FirstTok, FirstTilde, Ctxt, &wasCloser);
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        if (!wasCloser) {
            Args.appendIfNonEmpty(std::move(ArgsTest1));
        }
        Args.append(std::move(NotPossible));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDOPERAND, std::move(Args)));
        
        return Error;
    }
    
    auto Middle = TheParser->parse(FirstTok, Ctxt);
    
    LeafSeq ArgsTest2;
    
    auto Tok1 = TheParser->currentToken();
    Tok1 = TheParser->eatAll(Tok1, Ctxt, ArgsTest2);
    
    if (Tok1.Tok != TOKEN_TILDE) {
        
        if (Tok1.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Something like   a ~f<EOF>
            //
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(FirstTilde)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(std::move(Middle));
            
            auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
            
            return Error;
        }
        
        TheParser->nextToken(Tok1);
        
        //
        // Something like   a ~f b
        //
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(Middle));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
        
        return Error;
    }
    
    LeafSeq ArgsTest3;
    
    TheParser->nextToken(Tok1);
    
    auto Tok2 = TheParser->currentToken();
    Tok2 = TheParser->eatAll(Tok2, Ctxt, ArgsTest3);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok2.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok2, Ctxt);
    } else {
        Right = TheParser->handleNotPossible(Tok2, Tok1, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1);
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
NodePtr ColonParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    assert((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Flag |= PARSER_INSIDE_COLON;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
    
    auto wasCloser = false;
    
    NodePtr Right;
    bool possible;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
        possible = true;
    } else {
        Right = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
        possible = false;
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
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
    
    if ((Ctxt.Flag & PARSER_PARSED_SYMBOL) != PARSER_PARSED_SYMBOL) {
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_COLONERROR, std::move(Args)));
        
        return Error;
    }
    
    Ctxt.Flag &= ~(PARSER_PARSED_SYMBOL);
    
    auto Pat = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
    
    LeafSeq ArgsTest2;
    
    Tok = TheParser->currentToken();
    Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest2);
    
    if (Tok.Tok == TOKEN_COLON) {
        
        Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
        
        NodeSeq PatSeq(1 + 1);
        PatSeq.append(std::move(Pat));
        PatSeq.appendIfNonEmpty(std::move(ArgsTest2));
        
        return parseContextSensitive(std::move(PatSeq), Tok, Ctxt);
    }
    
    return Pat;
}

//
// Something like  pattern:optional
//
// Called from other parselets
//
NodePtr ColonParselet::parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
    } else {
        Right = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
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
NodePtr SlashColonParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence();
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
    
    if (!Tok.Tok.isPossibleBeginningOfExpression()) {
        
        bool wasCloser;
        
        auto NotPossible = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        if (!wasCloser) {
            Args.appendIfNonEmpty(std::move(ArgsTest1));
        }
        Args.append(std::move(NotPossible));
        
        return NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDOPERAND, std::move(Args)));
    }
    
    auto Middle = TheParser->parse(Tok, Ctxt);
    
    LeafSeq ArgsTest2;
    
    Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest2);
    
    if (Tok.Tok == TOKEN_EQUAL) {
        
        NodeSeq Args(1 + 1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(Middle));
        Args.appendIfNonEmpty(std::move(ArgsTest2));
        
        Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
        
        auto& equalParselet = TheParser->findInfixParselet(Tok.Tok);
        
        auto N = equalParselet->parse(std::move(Args), Tok, Ctxt);
        
        return N;
        
    } else if (Tok.Tok == TOKEN_COLONEQUAL) {
        
        NodeSeq Args2(1 + 1);
        Args2.append(std::move(Middle));
        Args2.appendIfNonEmpty(std::move(ArgsTest2));
        
        auto& colonEqualParselet = TheParser->findInfixParselet(Tok.Tok);
        
        auto N = colonEqualParselet->parse(std::move(Args2), Tok, Ctxt);
        
        auto& C = N->getChildrenDestructive();
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(NodePtr(new NodeSeqNode(std::move(C))));
        
        return NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
        
    } else if (Tok.Tok == TOKEN_EQUALDOT) {
        
        NodeSeq Args2(1 + 1);
        Args2.append(std::move(Middle));
        Args2.appendIfNonEmpty(std::move(ArgsTest2));
        
        auto& equalDotParselet = TheParser->findInfixParselet(Tok.Tok);
        
        auto N = equalDotParselet->parse(std::move(Args2), Tok, Ctxt);
        
        auto& C = N->getChildrenDestructive();
        
        NodeSeq Args(1 + 1 + 1 + 1);
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
    
    if (Tok.Tok == TOKEN_ENDOFFILE) {
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(Middle));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDSET, std::move(Args)));
        
        return Error;
    }
    
    TheParser->nextToken(Tok);
    
    NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1);
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
NodePtr LinearSyntaxOpenParenParselet::parse(Token firstTok, ParserContext Ctxt) const {
    
    auto Opener = firstTok;
    
    Ctxt.GroupDepth++;
    
    TheParser->nextToken(firstTok);
    
    auto Tok = TheParser->currentToken();
    
    Ctxt.Closr = CLOSER_LINEARSYNTAX_CLOSEPAREN;
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(Opener)));
    
    while (true) {
    
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   \( a EOF
            //
            
            auto group = NodePtr(new GroupMissingCloserNode(SYMBOL_AST_GROUPLINEARSYNTAXPAREN, std::move(Args)));
            
            return group;
        }
        if (Tok.Tok == TOKEN_LINEARSYNTAX_CLOSEPAREN) {
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.append(NodePtr(new LeafNode(Tok)));
            
            TheParser->nextToken(Tok);
            
            return NodePtr(new GroupNode(SYMBOL_AST_GROUPLINEARSYNTAXPAREN, std::move(Args)));
        }
        
        //
        // Do not check for other closers here
        //
        // As long as \( \) parses by just doing tokenization, then cannot reliably test for other closers
        //
        // e.g., \( ( \)  and  \( ) \)  is completely valid syntax
        //
        
        if (Tok.Tok == TOKEN_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse(Tok, Ctxt);
            
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
            
            TheParser->nextToken(Tok);
            
            Tok = TheParser->currentToken();
        }
        
    } // while
}


//
// Something like  a =.
//
// a /: b = c  and  a /: b = .  are also handled here
//
NodePtr EqualParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence();
    
    if (TokIn.Tok == TOKEN_EQUALDOT) {
        
        TheParser->nextToken(TokIn);
        
        NodeSeq Args(1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        
        if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            return NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
        }
        
        return NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
    }
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        TheParser->nextToken(Tok);
        
        NodeSeq Args(1 + 1 + 1 + 1);
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
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
    } else {
        Right = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
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


IntegralParselet::IntegralParselet() : Op1(SYMBOL_INTEGRAL), Op2(SYMBOL_INTEGRATE) {}

//
// Something like  \[Integral] f \[DifferentialD] x
//
NodePtr IntegralParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence();
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
    
    if (!Tok.Tok.isPossibleBeginningOfExpression() ||
        (TheParser->getTokenPrecedence(Tok, Ctxt) < PRECEDENCE_CLASS_INTEGRATIONOPERATORS)) {
        
        bool wasCloser;
        
        auto NotPossible = TheParser->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        if (!wasCloser) {
            Args.appendIfNonEmpty(std::move(ArgsTest1));
        }
        Args.append(std::move(NotPossible));
        
        return NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDINTEGRAND, std::move(Args)));
    }
    
    auto operand = TheParser->parse(Tok, Ctxt);
    
    Ctxt.Flag &= ~(PARSER_INSIDE_INTEGRAL);
    
    LeafSeq ArgsTest2;
    
    Tok = TheParser->currentToken();
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest2);
    
    if (!Tok.Tok.isDifferentialD()) {
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(operand));
        
        return NodePtr(new PrefixNode(Op1, std::move(Args)));
    }
    
    auto& differentialDparselet = TheParser->findPrefixParselet(Tok.Tok);
    
    auto variable = differentialDparselet->parse(Tok, Ctxt);
    
    NodeSeq Args(1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest1));
    Args.append(std::move(operand));
    Args.appendIfNonEmpty(std::move(ArgsTest2));
    Args.append(std::move(variable));
    
    return NodePtr(new PrefixBinaryNode(Op2, std::move(Args)));
}


//
// Gather all < > == <= => into a single node
//
NodePtr InequalityParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    Ctxt.Prec = getPrecedence();
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = TheParser->currentToken();
        Tok1 = TheParser->eatAndPreserveToplevelNewlines(Tok1, Ctxt, ArgsTest1);
        
        if (Tok1.Tok.isInequalityOperator()) {
            
            TheParser->nextToken(Tok1);
            
            LeafSeq ArgsTest2;
            
            auto Tok2 = TheParser->currentToken();
            Tok2 = TheParser->eatAll(Tok2, Ctxt, ArgsTest2);
            
            auto wasCloser = false;
            
            NodePtr Operand;
            bool possible;
            if (Tok2.Tok.isPossibleBeginningOfExpression()) {
                Operand = TheParser->parse(Tok2, Ctxt);
                possible = true;
            } else {
                Operand = TheParser->handleNotPossible(Tok2, Tok1, Ctxt, &wasCloser);
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
NodePtr VectorInequalityParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    Ctxt.Prec = getPrecedence();
    
    while (true) {
      
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        //
        // LOOKAHEAD
        //
        {
            LeafSeq ArgsTest1;
            
            auto Tok1 = TheParser->currentToken();
            Tok1 = TheParser->eatAndPreserveToplevelNewlines(Tok1, Ctxt, ArgsTest1);
            
            if (Tok1.Tok.isVectorInequalityOperator()) {
                
                TheParser->nextToken(Tok1);
                
                LeafSeq ArgsTest2;
                
                auto Tok2 = TheParser->currentToken();
                Tok2 = TheParser->eatAll(Tok2, Ctxt, ArgsTest2);
                
                auto wasCloser = false;
                
                NodePtr Operand;
                bool possible;
                if (Tok2.Tok.isPossibleBeginningOfExpression()) {
                    Operand = TheParser->parse(Tok2, Ctxt);
                    possible = true;
                } else {
                    Operand = TheParser->handleNotPossible(Tok2, Tok1, Ctxt, &wasCloser);
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


InfixOperatorWithTrailingParselet::InfixOperatorWithTrailingParselet(TokenEnum Tok, Precedence precedence) : precedence(precedence), Op(InfixOperatorToSymbol(Tok)) {}

NodePtr InfixOperatorWithTrailingParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto lastOperatorToken = TokIn;
    
    Ctxt.Prec = getPrecedence();
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = TheParser->currentToken();
        Tok1 = TheParser->eatAndPreserveToplevelNewlines(Tok1, Ctxt, ArgsTest1);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a,b\[InvisibleComma]c
        //
        // and we want only a single Infix node created
        //
        if (InfixOperatorToSymbol(Tok1.Tok) == Op) {
            
            lastOperatorToken = Tok1;
            
            //
            // Something like  a;b  or  a,b
            //
            
            TheParser->nextToken(Tok1);
            
            LeafSeq ArgsTest2;
            
            auto Tok2 = TheParser->currentToken();
            Tok2 = TheParser->eatAndPreserveToplevelNewlines(Tok2, Ctxt, ArgsTest2);
            
            if (InfixOperatorToSymbol(Tok2.Tok) == Op) {
                
                //
                // Something like  a; ;
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
                
                lastOperatorToken = Tok2;
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(Tok1)));
                Args.append(NodePtr(new LeafNode(Implicit)));
                
            } else if (Tok2.Tok.isPossibleBeginningOfExpression()) {
                
                auto operand = TheParser->parse(Tok2, Ctxt);
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
                Args.appendIfNonEmpty(std::move(ArgsTest1));
                Args.append(NodePtr(new LeafNode(Tok1)));
                Args.appendIfNonEmpty(std::move(ArgsTest2));
                Args.append(std::move(operand));
                
            } else {
                
                //
                // Not beginning of an expression
                //
                // For example:  a;&
                //
                
                auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
                
                //
                // Do not reserve inside loop
                // Allow default resizing strategy, which is hopefully exponential
                //
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


//
// a::b
//
NodePtr ColonColonParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    //
    // Not used here because of the special rules for tokenizing after ::
    //
    //auto Ctxt = CtxtIn;
    //Ctxt.Prec = PRECEDENCE_COLONCOLON;
    //Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest1;
        
        auto Tok1 = TheParser->currentToken();
        Tok1 = TheParser->eatAndPreserveToplevelNewlines(Tok1, Ctxt, ArgsTest1);
        
        if (Tok1.Tok == TOKEN_COLONCOLON) {
            
            TheParser->nextToken(Tok1);
            
            //
            // Special tokenization, so must do parsing here
            //
            
            auto Tok2 = TheParser->currentToken_stringifySymbol();
            
            TheParser->nextToken_stringifySymbol();
            
            bool possible;
            if (Tok2.Tok.isPossibleBeginningOfExpression()) {
                assert(Tok2.Tok == TOKEN_STRING);
                possible = true;
            } else {
                assert(Tok2.Tok.isError());
                possible = false;
            }
            
            NodePtr Operand;
            if (possible) {
                Operand = NodePtr(new LeafNode(std::move(Tok2)));
            } else {
                Operand = NodePtr(new ErrorNode(std::move(Tok2)));
            }
            
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            Args.append(std::move(Operand));
            
            if (!possible) {
                return NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
            }
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            return NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
        }
        
    } // while
}


//
// a>>b
//
NodePtr GreaterGreaterParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = PRECEDENCE_GREATERGREATER;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok = TheParser->currentToken_stringifyFile();
    Tok = TheParser->eatAll_stringifyFile(Tok, Ctxt, ArgsTest);
    
    TheParser->nextToken_stringifyFile();

    auto Operand = NodePtr(new LeafNode(std::move(Tok)));
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest));
    Args.append(std::move(Operand));
    
    return NodePtr(new BinaryNode(SYMBOL_PUT, std::move(Args)));
}


//
// a>>>b
//
NodePtr GreaterGreaterGreaterParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = PRECEDENCE_GREATERGREATERGREATER;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok = TheParser->currentToken_stringifyFile();
    Tok = TheParser->eatAll_stringifyFile(Tok, Ctxt, ArgsTest);
    
    TheParser->nextToken_stringifyFile();
    
    auto Operand = NodePtr(new LeafNode(std::move(Tok)));
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest));
    Args.append(std::move(Operand));
    
    return NodePtr(new BinaryNode(SYMBOL_PUTAPPEND, std::move(Args)));
}


//
// <<a
//
NodePtr LessLessParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = PRECEDENCE_LESSLESS;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok = TheParser->currentToken_stringifyFile();
    Tok = TheParser->eatAll_stringifyFile(Tok, Ctxt, ArgsTest);
    
    TheParser->nextToken_stringifyFile();
    
    auto Operand = NodePtr(new LeafNode(std::move(Tok)));
    
    NodeSeq Args(1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest));
    Args.append(std::move(Operand));
    
    return NodePtr(new PrefixNode(SYMBOL_GET, std::move(Args)));
}
