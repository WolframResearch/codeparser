
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h"


LeafParselet::LeafParselet(Precedence precedence) : precedence(precedence) {}

NodePtr LeafParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    return NodePtr(new LeafNode(TokIn));
}

Precedence LeafParselet::getPrecedence(ParserContext Ctxt) const {
    assert(precedence != PRECEDENCE_ASSERTFALSE);
    return precedence;
}


ToplevelNewlineParselet::ToplevelNewlineParselet() {}

NodePtr ToplevelNewlineParselet::parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    assert(false);
    return nullptr;
}

Precedence ToplevelNewlineParselet::getPrecedence(ParserContext Ctxt, bool *implicitTimes) const {
    //
    // Do not do Implicit Times across top-level newlines
    //
    *implicitTimes = false;
    return PRECEDENCE_LOWEST;
}

Associativity ToplevelNewlineParselet::getAssociativity() const {
    return ASSOCIATIVITY_NONE;
}






//
// something like  x  or x_
//
NodePtr SymbolParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnder1Parselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnder2Parselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnder3Parselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERDOT.value(): {

            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnderDotParselet->parseContextSensitive(std::move(Args), Tok, Ctxt);
        }
        default: {
            
            LeafSeq ArgsTest;
            
            Tok = TheParser->eatAndPreserveToplevelNewlines(Tok, Ctxt, ArgsTest);
            
            //
            // when parsing a in a:b  then PARSER_INSIDE_COLON bit is 0
            // when parsing b in a:b  then PARSER_INSIDE_COLON bit is 1
            //
            // It is necessary to go to colonParselet->parse here (even though it seems non-contextSensitive)
            // because in e.g.,  a_*b:f[]  the b is the last node in the Times expression and needs to bind with  :f[]
            // Parsing  a_*b  completely, and then parsing  :f[]  would be wrong.
            //
            if (Tok.Tok == TOKEN_COLON) {
                
                if ((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
                    
                    NodeSeq Args(1 + 1);
                    Args.append(std::move(Sym));
                    Args.appendIfNonEmpty(std::move(ArgsTest));
                    
                    return infixParselets[Tok.Tok.value()]->parse(std::move(Args), Tok, Ctxt);
                }
            }
            
            return Sym;
        }
    }
}


PrefixOperatorParselet::PrefixOperatorParselet(TokenEnum Tok, Precedence precedence) : precedence(precedence), Op(PrefixOperatorToSymbol(Tok)) {}

NodePtr PrefixOperatorParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence(Ctxt);
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Operand;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Operand = TheParser->parse(Tok, Ctxt);
    } else {
        Operand = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest));
    }
    Args.append(std::move(Operand));
    
    return NodePtr(new PrefixNode(Op, std::move(Args)));
}


NodePtr InfixParselet::handleNotPossible(Token& tokenBad, Token& tokenAnchor, ParserContext CtxtIn, bool *wasCloser) const {
    
    //
    // It is possible that possibleBeginningOfExpression could get here
    //
    // For example: \[Integral]!b
    // !b is possible beginning of expression, but ! has lower precedence than \[Integral],
    // so
    //
    //
    if (tokenBad.Tok.isPossibleBeginningOfExpression()) {
        
        auto operand = TheParser->parse(tokenBad, CtxtIn);
        
        *wasCloser = false;
        
        return operand;
    }
    
    //
    // Handle something like  f[,1]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Comma node.
    //
    // Do not take next token
    //
    // Important to not duplicate token's Str here, it may also appear later
    //
    // Also, invent Source
    //
    
    auto NotPossible = NodePtr(new ErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.buffer), Source(tokenAnchor.Src.Start))));
    
    NodeSeq LeftSeq(1);
    LeftSeq.append(std::move(NotPossible));
    
    auto Ctxt = CtxtIn;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    
    *wasCloser = false;
    
    return parse(std::move(LeftSeq), tokenBad, Ctxt);
}


NullInfixParselet::NullInfixParselet() {}

NodePtr NullInfixParselet::parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    assert(false);
    return nullptr;
}

Precedence NullInfixParselet::getPrecedence(ParserContext Ctxt, bool *implicitTimes) const {
    *implicitTimes = true;
    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

Associativity NullInfixParselet::getAssociativity() const {
    return ASSOCIATIVITY_NONE;
}


LeafInfixParselet::LeafInfixParselet(Precedence precedence) : precedence(precedence) {}

NodePtr LeafInfixParselet::handleNotPossible(Token& tokenBad, Token& tokenAnchor, ParserContext CtxtIn, bool *wasCloser) const {
    
    if (TokenToCloser(tokenBad.Tok) == CtxtIn.Closr) {
        //
        // Handle the special cases of:
        // { + }
        // { a + }
        // { a @ }
        // We are here parsing the operators, but we don't want to descend and treat the } as the problem
        //
        
        //
        // Do not take next token
        //
        
        auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.end), Source(tokenAnchor.Src.End));
        
        *wasCloser = true;
        
        return NodePtr(new ErrorNode(createdToken));
    }
    
    if (tokenBad.Tok.isCloser()) {
        
        //
        // Handle  { a ) }
        // which ends up being  MissingCloser[ { a ) ]   UnexpectedCloser[ } ]
        //
        
        TheParser->nextToken(tokenBad);
        
        NodeSeq Args(1);
        Args.append(NodePtr(new LeafNode(tokenBad)));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_UNEXPECTEDCLOSER, std::move(Args)));
        
        *wasCloser = true;
        
        return Error;
    }
    
    if (tokenBad.Tok == TOKEN_ENDOFFILE) {
        
        auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.end), Source(tokenAnchor.Src.End));
        
        *wasCloser = true;
        
        return NodePtr(new ErrorNode(createdToken));
    }
    
    assert(tokenBad.Tok.isError());
    
    TheParser->nextToken(tokenBad);
    
    //
    // If there is a Token error, then use that specific error
    //
    
    *wasCloser = false;
    
    return NodePtr(new ErrorNode(tokenBad));
}

Precedence LeafInfixParselet::getPrecedence(ParserContext Ctxt, bool *implicitTimes) const {
    *implicitTimes = false;
    return precedence;
}


BinaryOperatorParselet::BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Associativity assoc) : precedence(precedence), assoc(assoc), Op(BinaryOperatorToSymbol(Tok)) {}

NodePtr BinaryOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    bool implicitTimes;
    Ctxt.Prec = getPrecedence(Ctxt, &implicitTimes);
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
    } else {
        Right = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
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
    
    bool implicitTimes;
    Ctxt.Prec = getPrecedence(Ctxt, &implicitTimes);
    
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
        
        auto Tok1 = TheParser->currentToken(Ctxt);
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
            
            auto Tok2 = TheParser->currentToken(Ctxt);
            Tok2 = TheParser->eatAll(Tok2, Ctxt, ArgsTest2);
            
            auto wasCloser = false;
            
            NodePtr Operand;
            bool possible;
            if (Tok2.Tok.isPossibleBeginningOfExpression()) {
                Operand = TheParser->parse(Tok2, Ctxt);
                possible = true;
            } else {
                Operand = infixParselets[Tok2.Tok.value()]->handleNotPossible(Tok2, Tok1, Ctxt, &wasCloser);
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
    
    Ctxt.InsideGroup = 0x1;
    
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
        
        auto Tok = TheParser->currentToken(Ctxt);
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
            
            auto group = NodePtr(new GroupMissingCloserNeedsReparseNode(Op, std::move(Args)));
            
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
            Operand = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, Tok, Ctxt, &wasCloser);
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


//
// Something like  a ~f~ b
//
NodePtr TildeParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    auto FirstTilde = TokIn;
    
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto FirstTok = TheParser->currentToken(Ctxt);
    FirstTok = TheParser->eatAll(FirstTok, Ctxt, ArgsTest1);
    
    if (!FirstTok.Tok.isPossibleBeginningOfExpression()) {
        
        //
        // Something like  a ~&
        //
        
        bool wasCloser;
        
        auto NotPossible = infixParselets[FirstTok.Tok.value()]->handleNotPossible(FirstTok, FirstTilde, Ctxt, &wasCloser);
        
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
    
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    auto tildeParselet = infixParselets[TOKEN_TILDE.value()];
    tildeParselet->setPrecedence(PRECEDENCE_LOWEST);
    
    auto Middle = TheParser->parse(FirstTok, Ctxt);
    
    Ctxt.Prec = PRECEDENCE_TILDE;
    tildeParselet->setPrecedence(PRECEDENCE_TILDE);
    
    LeafSeq ArgsTest2;
    
    auto Tok1 = TheParser->currentToken(Ctxt);
    Tok1 = TheParser->eatAll(Tok1, Ctxt, ArgsTest2);
    
    if (Tok1.Tok != TOKEN_TILDE) {
        
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
    
    auto Tok2 = TheParser->currentToken(Ctxt);
    Tok2 = TheParser->eatAll(Tok2, Ctxt, ArgsTest3);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok2.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok2, Ctxt);
    } else {
        Right = infixParselets[Tok2.Tok.value()]->handleNotPossible(Tok2, Tok1, Ctxt, &wasCloser);
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
    
    return NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
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
    
    auto Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
    } else {
        Right = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    if (!wasCloser) {
        Args.appendIfNonEmpty(std::move(ArgsTest1));
    }
    Args.append(std::move(Right));
    
    auto Pat = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
    
    LeafSeq ArgsTest2;
    
    Tok = TheParser->currentToken(Ctxt);
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
    
    auto Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest);
    
    auto wasCloser = false;
    
    NodePtr Right;
    if (Tok.Tok.isPossibleBeginningOfExpression()) {
        Right = TheParser->parse(Tok, Ctxt);
    } else {
        Right = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
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
    
    bool implicitTimes;
    Ctxt.Prec = getPrecedence(Ctxt, &implicitTimes);
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
    
    if (!Tok.Tok.isPossibleBeginningOfExpression()) {
        
        bool wasCloser;
        
        auto NotPossible = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
        
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
    
    Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest2);
    
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            NodeSeq Args(1 + 1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(std::move(Middle));
            Args.appendIfNonEmpty(std::move(ArgsTest2));
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            auto equalParselet = infixParselets[Tok.Tok.value()];
            
            auto N = equalParselet->parse(std::move(Args), Tok, Ctxt);
            
            return N;
        }
        case TOKEN_COLONEQUAL.value(): {
            
            NodeSeq Args2(1 + 1);
            Args2.append(std::move(Middle));
            Args2.appendIfNonEmpty(std::move(ArgsTest2));
            
            auto colonEqualParselet = infixParselets[Tok.Tok.value()];
            
            auto N = colonEqualParselet->parse(std::move(Args2), Tok, Ctxt);
            
            auto& C = N->getChildrenDestructive();
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(NodePtr(new NodeSeqNode(std::move(C))));
            
            return NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
        }
        case TOKEN_EQUALDOT.value(): {
            
            NodeSeq Args2(1 + 1);
            Args2.append(std::move(Middle));
            Args2.appendIfNonEmpty(std::move(ArgsTest2));
            
            auto equalDotParselet = infixParselets[Tok.Tok.value()];
            
            auto N = equalDotParselet->parse(std::move(Args2), Tok, Ctxt);
            
            auto& C = N->getChildrenDestructive();
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(NodePtr(new NodeSeqNode(std::move(C))));
            
            return NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
        }
        case TOKEN_ENDOFFILE.value(): {
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(ArgsTest1));
            Args.append(std::move(Middle));
            
            auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDSET, std::move(Args)));
            
            return Error;
        }
        default: {
            
            //
            // Anything other than:
            // a /: b = c
            // a /: b := c
            // a /: b =.
            // a /: b <EOF>
            //
            
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
    }
}


//
// Something like  \( x \)
//
NodePtr LinearSyntaxOpenParenParselet::parse(Token firstTok, ParserContext Ctxt) const {
    
    auto Opener = firstTok;
    
    Ctxt.InsideGroup = 0x1;
    
    TheParser->nextToken(firstTok);
    
    auto Tok = TheParser->currentToken(Ctxt);
    
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
            
            auto group = NodePtr(new GroupMissingCloserNeedsReparseNode(SYMBOL_CODEPARSER_GROUPLINEARSYNTAXPAREN, std::move(Args)));
            
            return group;
        }
        if (Tok.Tok == TOKEN_LINEARSYNTAX_CLOSEPAREN) {
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.append(NodePtr(new LeafNode(Tok)));
            
            TheParser->nextToken(Tok);
            
            return NodePtr(new GroupNode(SYMBOL_CODEPARSER_GROUPLINEARSYNTAXPAREN, std::move(Args)));
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
            
            Tok = TheParser->currentToken(Ctxt);
            
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
            
            Tok = TheParser->currentToken(Ctxt);
        }
        
    } // while
}


//
// a /: b = c  and  a /: b = .  are handled here
//
NodePtr EqualParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    bool implicitTimes;
    Ctxt.Prec = getPrecedence(Ctxt, &implicitTimes);
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest;
    
    auto Tok = TheParser->currentToken(Ctxt);
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
        Right = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
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

//
// Something like  a =.
//
// a /: b =.  is also handled here
//
NodePtr EqualDotParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    bool implicitTimes;
    Ctxt.Prec = getPrecedence(Ctxt, &implicitTimes);
    
    TheParser->nextToken(TokIn);
    
    NodeSeq Args(1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
        return NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
    }
    
    return NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
}

IntegralParselet::IntegralParselet() : Op1(SYMBOL_INTEGRAL), Op2(SYMBOL_INTEGRATE) {}

//
// Something like  \[Integral] f \[DifferentialD] x
//
NodePtr IntegralParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence(Ctxt);
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq ArgsTest1;
    
    auto Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest1);
    
    if (!Tok.Tok.isPossibleBeginningOfExpression() ||
        (prefixParselets[Tok.Tok.value()]->getPrecedence(Ctxt) < PRECEDENCE_CLASS_INTEGRATIONOPERATORS)) {
        
        bool wasCloser;
        
        auto NotPossible = infixParselets[Tok.Tok.value()]->handleNotPossible(Tok, TokIn, Ctxt, &wasCloser);
        
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
    
    Tok = TheParser->currentToken(Ctxt);
    Tok = TheParser->eatAll(Tok, Ctxt, ArgsTest2);
    
    if (!Tok.Tok.isDifferentialD()) {
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(ArgsTest1));
        Args.append(std::move(operand));
        
        return NodePtr(new PrefixNode(Op1, std::move(Args)));
    }
    
    auto differentialDparselet = prefixParselets[Tok.Tok.value()];
    
    auto variable = differentialDparselet->parse(Tok, Ctxt);
    
    NodeSeq Args(1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(ArgsTest1));
    Args.append(std::move(operand));
    Args.appendIfNonEmpty(std::move(ArgsTest2));
    Args.append(std::move(variable));
    
    return NodePtr(new PrefixBinaryNode(Op2, std::move(Args)));
}


InfixOperatorWithTrailingParselet::InfixOperatorWithTrailingParselet(TokenEnum Tok, Precedence precedence) : precedence(precedence), Op(InfixOperatorToSymbol(Tok)) {}

NodePtr InfixOperatorWithTrailingParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto lastOperatorToken = TokIn;
    
    bool implicitTimes;
    Ctxt.Prec = getPrecedence(Ctxt, &implicitTimes);
    
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
        
        auto Tok1 = TheParser->currentToken(Ctxt);
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
            
            auto Tok2 = TheParser->currentToken(Ctxt);
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
        
        auto Tok1 = TheParser->currentToken(Ctxt);
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


DifferentialDParselet::DifferentialDParselet() {}

Precedence DifferentialDParselet::getPrecedence(ParserContext Ctxt, bool *implicitTimes) const {
    
    if ((Ctxt.Flag & PARSER_INSIDE_INTEGRAL) == PARSER_INSIDE_INTEGRAL) {
        
        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    *implicitTimes = true;
    
    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

NodePtr DifferentialDParselet::parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    assert(false);
    return nullptr;
}

Associativity DifferentialDParselet::getAssociativity() const {
    assert(false);
    return ASSOCIATIVITY_ASSERTFALSE;
}


