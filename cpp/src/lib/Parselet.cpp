
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h"


NodePtr LeafParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    auto Left = NodePtr(new LeafNode(TokIn));
    
    return TheParser->infixLoop(std::move(Left), Ctxt);
}


NodePtr PrefixErrorParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isError());
    
    TheParser->nextToken(TokIn);
    
    NodePtr Error;
    if (TokIn.Tok.isUnterminated()) {
        Error = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(TokIn));
    } else {
        Error = NodePtr(new ErrorNode(TokIn));
    }
    
    return Error;
}


NodePtr PrefixCloserParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isCloser());
        
    //
    // Inside some other parselet that is not GroupParselet
    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    // Will be replaced later, so do not need to provide bufAndLen or source
    //
    
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(), Source());
    
    return NodePtr(new ExpectedOperandErrorNode(createdToken));
}


NodePtr PrefixToplevelCloserParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isCloser());
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    TheParser->nextToken(TokIn);
    
    auto Error = NodePtr(new ErrorNode(Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.BufLen, TokIn.Src)));
    
    return Error;
}


NodePtr PrefixEndOfFileParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    //
    // Something like  a+<EOF>
    //
    // Will be replaced later, so do not need to provide bufAndLen or source
    //
    
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(), Source());
    
    return NodePtr(new ExpectedOperandErrorNode(createdToken));
}


NodePtr PrefixUnsupportedTokenParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    auto createdToken = Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.BufLen, TokIn.Src);
    
    return NodePtr(new ErrorNode(createdToken));
}


NodePtr PrefixCommaParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_FAKE_IMPLICITNULL
    //
    if (Ctxt.Prec == PRECEDENCE_LOWEST) {
        
        auto createdToken = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(TokIn.BufLen.buffer), Source(TokIn.Src.Start));
        
        auto Left = NodePtr(new LeafNode(createdToken));
        
        return TheParser->infixLoop(std::move(Left), Ctxt);
        
    } else {
        
        auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.buffer), Source(TokIn.Src.Start));
        
        auto Left = NodePtr(new ErrorNode(createdToken));
        
        return Left;
    }
}


NodePtr PrefixUnhandledParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    assert(!TokIn.Tok.isPossibleBeginning() && "handle at call site");
    
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON | PARSER_INSIDE_TILDE);
    
    auto NotPossible = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.buffer), Source(TokIn.Src.Start))));
    
    auto I = infixParselets[TokIn.Tok.value()];
    
    auto TokenPrecedence = I->getPrecedence(Ctxt);
    
    //
    // if (Ctxt.Prec > TokenPrecedence)
    //   goto prefixUnhandledParseletRet;
    // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
    //   goto prefixUnhandledParseletRet;
    //
    if ((Ctxt.Prec | 0x1) > TokenPrecedence) {
        
        //
        // Something like  a + | 2
        //
        // Make sure that the error leaf is with the + and not the |
        //
        
        return NotPossible;
    }
    
    //
    // Handle something like  f[@2]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Operator node.
    //
    // Do not take next token
    //
    
    NodeSeq LeftSeq(1);
    LeftSeq.append(std::move(NotPossible));
    
    auto NotPossible2 = infixParselets[TokIn.Tok.value()]->parse(std::move(LeftSeq), TokIn, Ctxt);
    
    return NotPossible2;
}


NodePtr InfixToplevelNewlineParselet::parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    assert(false);
    return nullptr;
}


NodePtr SymbolParselet::parse(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
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
            break;
        default: {
            
            {
                LeafSeq Trivia1;
                
                Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, TOPLEVEL, Trivia1);
                
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
                        Args.appendIfNonEmpty(std::move(Trivia1));
                        
                        Sym = infixParselets[TOKEN_COLON.value()]->parse(std::move(Args), Tok, Ctxt);
                    }
                }
            }
            
            return TheParser->infixLoop(std::move(Sym), Ctxt);
        }
    }
    
    assert(false);
    return nullptr;
}

NodePtr SymbolParselet::parseContextSensitive(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    return Sym;
}


NodePtr PrefixOperatorParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    TheParser->nextToken(TokIn);
    
    NodePtr Left;
    {
        LeafSeq Trivia1;
        
        auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        auto Operand = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
        
        if (Operand->isExpectedOperandError()) {
            
            //
            // Reattach the ExpectedOperand Error to the operator for a better experience
            //
            
            auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(std::move(ProperExpectedOperandError));
            Left = NodePtr(new PrefixNode(Op, std::move(Args)));
            
        } else {
            
            NodeSeq Args(1 + 1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(std::move(Operand));
            Left = NodePtr(new PrefixNode(Op, std::move(Args)));
        }
    }
    
    return TheParser->infixLoop(std::move(Left), CtxtIn);
}


NodePtr InfixImplicitTimesParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    assert(false);
    return nullptr;
}


Precedence InfixImplicitTimesParselet::getPrecedence(ParserContext Ctxt) const {
    assert(false);
    return PRECEDENCE_ASSERTFALSE;
}


Token InfixImplicitTimesParselet::processImplicitTimes(Token TokIn, ParserContext Ctxt) const {
    
    //
    // BufAndLen and Src will be filled in properly later
    //
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(), Source());
}


NodePtr InfixAssertFalseParselet::parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    assert(false);
    return nullptr;
}


NodePtr BinaryOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    Ctxt.Prec = getPrecedence(Ctxt);
    
    TheParser->nextToken(TokIn);
    
    NodePtr L;
    {
        LeafSeq Trivia1;
    
        auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
        auto Right = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
        
        if (Right->isExpectedOperandError()) {

            //
            // Reattach the ExpectedOperand Error to the operator for a better experience
            //
            
            auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
            
            NodeSeq Args(1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(std::move(ProperExpectedOperandError));
            L = NodePtr(new BinaryNode(Op, std::move(Args)));
            
        } else {
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(std::move(Right));
            L = NodePtr(new BinaryNode(Op, std::move(Args)));
        }
    }
    
    return TheParser->infixLoop(std::move(L), Ctxt);
}


NodePtr InfixOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    Ctxt.Prec = getPrecedence(Ctxt);
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    Token OperandLastToken;
    {
        LeafSeq Trivia2;
        
        auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        auto Operand = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
        
        OperandLastToken = Operand->lastToken();
        
        if (Operand->isExpectedOperandError()) {
            
            //
            // Reattach the ExpectedOperand Error to the operator for a better experience
            //
            
            auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
            
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(std::move(ProperExpectedOperandError));
            
        } else {
            
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia2));
            Args.append(std::move(Operand));
        }
    }
    
    while (true) {

#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        {
            LeafSeq Trivia1;
            
            Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
            
            auto I = infixParselets[Tok1.Tok.value()];
            
            Tok1 = I->processImplicitTimes(Tok1, Ctxt);
            I = infixParselets[Tok1.Tok.value()];
            
            //
            // Cannot just compare tokens
            //
            // May be something like  a * b c \[Times] d
            //
            // and we want only a single Infix node created
            //
            if (I->getOp() != Op) {
                
                //
                // Tok.Tok != TokIn.Tok, so break
                //
                
                break;
            }
            
            if (Tok1.Tok == TOKEN_FAKE_IMPLICITTIMES) {
                
                //
                // Reattach the ImplicitTimes to the operand for a better experience
                //
                
                Tok1 = Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(OperandLastToken.BufLen.end), Source(OperandLastToken.Src.End));
                
                Args.append(NodePtr(new LeafNode(Tok1)));
                
            } else {
                
                TheParser->nextToken(Tok1);
                
                Args.appendIfNonEmpty(std::move(Trivia1));
                Args.append(NodePtr(new LeafNode(Tok1)));
            }
        }
        
        
        LeafSeq Trivia2;
        
        auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        auto Operand = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
        
        OperandLastToken = Operand->lastToken();
        
        if (Operand->isExpectedOperandError()) {
            
            //
            // Reattach the ExpectedOperand Error to the operator for a better experience
            //
            
            auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(Tok1.BufLen.end), Source(Tok1.Src.End))));
            
            Args.append(std::move(ProperExpectedOperandError));
            
            continue;
        }
        
        //
        // Do not reserve inside loop
        // Allow default resizing strategy, which is hopefully exponential
        //
        Args.appendIfNonEmpty(std::move(Trivia2));
        Args.append(std::move(Operand));
        
    } // while
    
    auto L = NodePtr(new InfixNode(Op, std::move(Args)));
    
    return TheParser->infixLoop(std::move(L), Ctxt);
}


NodePtr PostfixOperatorParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    NodeSeq Args(1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    auto L = NodePtr(new PostfixNode(Op, std::move(Args)));
    
    return TheParser->infixLoop(std::move(L), Ctxt);
}


NodePtr GroupParselet::parse(Token firstTok, ParserContext CtxtIn) const {
    
    auto OpenerT = firstTok;
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(firstTok);
    
    Ctxt.Closr = Closr;
    
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON | PARSER_INSIDE_TILDE);
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(OpenerT)));
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    // ADDENDUM: Actually, there may be more than 1 "good" node
    // e.g. {1\\2}
    //
    
    NodePtr group;
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq Trivia1;
        
        auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        if (TokenToCloser(Tok.Tok) == Closr) {
            
            //
            // Everything is good
            //
            
            TheParser->nextToken(Tok);
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            group = NodePtr(new GroupNode(Op, std::move(Args)));
            
            break;
        }
        
        if (Tok.Tok.isCloser()) {
            
            //
            // some other closer
            //
            // Something like  { ( }  or  { ) }
            //
            // Must choose which one to parse correctly.
            //
            // There are pros and cons with either choice here.
            //
            // But it is important to note that either choice here results in strictly better behavior than the FrontEnd choice to parse neither  { ( }  nor  { ) }  correctly.
            //
            // The FrontEnd parses  { ( }  as  RowBox[{RowBox[{"{", "("}], "}"}]  but it would be better to parse as  RowBox[{"{", someErrorThing["("], "}"}]
            // The FrontEnd parses  { ) }  as  RowBox[{RowBox[{"{", ")"}], "}"}]  but it would be better to parse as  RowBox[{"{", someErrorThing[")"], "}"}]
            //
            auto arbitraryChoiceToBubbleBadCloserUpTheStack = true;
            
            if (arbitraryChoiceToBubbleBadCloserUpTheStack) {
                
                //
                // Do not consume the bad closer now
                // Bubble it up the stack
                //
                // This allows  { ( }  to be parsed as expected
                //
                // But also makes  { ) }  get parsed as MissingCloser[ { ] UnexpectedCloser[ ) ] UnexpectedCloser[ } ]
                //
                
                group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
                
                break;
                
            } else {
                
                //
                // Consume the bad closer now
                //
                // This allows  { ) }  to be parsed as expected
                //
                // But also makes  { ( }  get parsed as MissingCloser[ {, MissingCloser[ (, UnexpectedCloser[ } ] ] ]
                //
                
                //
                // Allow PrefixCloserParselet to handle the error
                //
                Ctxt.Closr = CLOSER_OPEN;
                
                auto Error = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
                
                Ctxt.Closr = Closr;
                
                //
                // Always append here
                //
                Args.appendIfNonEmpty(std::move(Trivia1));
                Args.append(std::move(Error));
                
                continue;
            }
        }
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   { a EOF
            //
            
            group = NodePtr(new GroupMissingCloserNeedsReparseNode(Op, std::move(Args)));
            
            break;
        }
        
        //
        // Handle the expression
        //
        
        auto Operand = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
        
        //
        // Do not reserve inside loop
        // Allow default resizing strategy, which is hopefully exponential
        //
        
        //
        // Always append here
        //
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(std::move(Operand));
        
    } // while
    
    return TheParser->infixLoop(std::move(group), CtxtIn);
}


NodePtr CallParselet::parse(NodeSeq Head, Token TokIn, ParserContext CtxtIn) const {
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    
    auto Right = GP->parse(TokIn, Ctxt);
    
    NodeSeq Args(1);
    Args.append(std::move(Right));
    
    auto L = NodePtr(new CallNode(std::move(Head), std::move(Args)));
    
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr TildeParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    auto FirstTilde = TokIn;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    auto FirstTok = TheParser->currentToken(Ctxt, TOPLEVEL);
    FirstTok = TheParser->eatTrivia(FirstTok, Ctxt, TOPLEVEL, Trivia1);
    
    Ctxt.Flag |= PARSER_INSIDE_TILDE;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    
    auto Middle = prefixParselets[FirstTok.Tok.value()]->parse(FirstTok, Ctxt);
    
    if (Middle->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(FirstTilde.BufLen.end), Source(FirstTilde.Src.End))));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        Args.append(std::move(ProperExpectedOperandError));
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
        
        return Error;
    }
    
    LeafSeq Trivia2;
    
    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, Ctxt, TOPLEVEL, Trivia2);
    
    if (Tok1.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(std::move(Middle));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
        
        return Error;
    }
    
    LeafSeq Trivia3;
    
    //
    // Reset back to "outside" precedence
    //
    Ctxt.Prec = PRECEDENCE_TILDE;
    Ctxt.Flag &= (~PARSER_INSIDE_TILDE);
    
    TheParser->nextToken(Tok1);
    
    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia3);
    
    auto Right = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
    
    if (Right->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        // Structurally correct, so return TernaryNode
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(FirstTilde.BufLen.end), Source(FirstTilde.Src.End))));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(FirstTilde)));
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(std::move(Middle));
        Args.appendIfNonEmpty(std::move(Trivia2));
        Args.append(NodePtr(new LeafNode(Tok1)));
        Args.appendIfNonEmpty(std::move(Trivia3));
        Args.append(std::move(ProperExpectedOperandError));
        auto Error = NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
        
        return Error;
    }
    
    NodeSeq Args(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(FirstTilde)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Middle));
    Args.appendIfNonEmpty(std::move(Trivia2));
    Args.append(NodePtr(new LeafNode(Tok1)));
    Args.appendIfNonEmpty(std::move(Trivia3));
    Args.append(std::move(Right));
    
    auto L = NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr ColonParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    assert((CtxtIn.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Flag |= PARSER_INSIDE_COLON;
    
    TheParser->nextToken(TokIn);
    
    NodePtr Pat;
    {
        LeafSeq Trivia1;
        
        auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        auto Right = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
        
        if (Right->isExpectedOperandError()) {
            
            //
            // Reattach the ExpectedOperand Error to the operator for a better experience
            //
            
            auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
            
            NodeSeq Args(1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(std::move(ProperExpectedOperandError));
            auto Error = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
            
            return Error;
        }
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(std::move(Right));
        
        Pat = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
        
        LeafSeq Trivia2;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, TOPLEVEL, Trivia2);
        
        if (Tok.Tok == TOKEN_COLON) {
            
            Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
            
            NodeSeq PatSeq(1 + 1);
            PatSeq.append(std::move(Pat));
            PatSeq.appendIfNonEmpty(std::move(Trivia2));
            
            auto L = parseContextSensitive(std::move(PatSeq), Tok, Ctxt);
            return L;
        }
    }
    
    return TheParser->infixLoop(std::move(Pat), CtxtIn);
}


NodePtr ColonParselet::parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    auto Right = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    if (Right->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(std::move(ProperExpectedOperandError));
        auto Error = NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
        
        return Error;
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Right));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
    return TheParser->infixLoop(std::move(L), Ctxt);
}


NodePtr SlashColonParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_SLASHCOLON;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    auto Middle = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    if (Middle->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(std::move(ProperExpectedOperandError));
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDSET, std::move(Args)));
        
        return Error;
    }
    
    LeafSeq Trivia2;
    
    Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia2);
    
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            
            NodeSeq Args(1 + 1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(std::move(Middle));
            Args.appendIfNonEmpty(std::move(Trivia2));
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            auto N = infixParselets[TOKEN_EQUAL.value()]->parse(std::move(Args), Tok, Ctxt);
            
            return N;
        }
        case TOKEN_COLONEQUAL.value(): {
            
            NodeSeq Args(1 + 1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(std::move(Middle));
            Args.appendIfNonEmpty(std::move(Trivia2));
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            auto N = infixParselets[TOKEN_COLONEQUAL.value()]->parse(std::move(Args), Tok, Ctxt);
            
            return N;
        }
        case TOKEN_ENDOFFILE.value(): {
            
            NodeSeq Args(1 + 1 + 1 + 1);
            Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia1));
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
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(std::move(Middle));
            Args.appendIfNonEmpty(std::move(Trivia2));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_EXPECTEDSET, std::move(Args)));
            
            return Error;
        }
    }
}


NodePtr LinearSyntaxOpenParenParselet::parse(Token firstTok, ParserContext CtxtIn) const {
    
    auto Opener = firstTok;
    
    TheParser->nextToken(firstTok);
    
    auto Ctxt = CtxtIn;
    Ctxt.Closr = CLOSER_LINEARSYNTAX_CLOSEPAREN;
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(Opener)));
    
    NodePtr group;
    
    while (true) {
    
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   \( a EOF
            //
            
            group = NodePtr(new GroupMissingCloserNeedsReparseNode(SYMBOL_CODEPARSER_GROUPLINEARSYNTAXPAREN, std::move(Args)));
            
            break;
        }
        if (Tok.Tok == TOKEN_LINEARSYNTAX_CLOSEPAREN) {
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.append(NodePtr(new LeafNode(Tok)));
            
            TheParser->nextToken(Tok);
            
            group = NodePtr(new GroupNode(SYMBOL_CODEPARSER_GROUPLINEARSYNTAXPAREN, std::move(Args)));
            
            break;
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
            
            Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
            
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
            
            Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        }
        
    } // while
    
    return TheParser->infixLoop(std::move(group), CtxtIn);
}


NodePtr EqualParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
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
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(NodePtr(new LeafNode(Tok)));
        
        if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            
            auto L = NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
            return TheParser->infixLoop(std::move(L), CtxtIn);
        }
        
        auto L = NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
        return TheParser->infixLoop(std::move(L), CtxtIn);
    }
    
    auto wasInsideSlashColon = ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON);
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto Right = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    if (Right->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(std::move(ProperExpectedOperandError));
        
        if (wasInsideSlashColon) {
            auto Error = NodePtr(new TernaryNode(SYMBOL_TAGSET, std::move(Args)));
            return Error;
        }
        
        auto Error = NodePtr(new BinaryNode(SYMBOL_SET, std::move(Args)));
        return Error;
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Right));
    
    if (wasInsideSlashColon) {
        auto L = NodePtr(new TernaryNode(SYMBOL_TAGSET, std::move(Args)));
        return TheParser->infixLoop(std::move(L), CtxtIn);
    }
    
    auto L = NodePtr(new BinaryNode(SYMBOL_SET, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr ColonEqualParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    auto wasInsideSlashColon = ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON);
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto Right = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    if (Right->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(std::move(ProperExpectedOperandError));
        
        if (wasInsideSlashColon) {
            auto Error = NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
            return Error;
        }
        
        auto Error = NodePtr(new BinaryNode(SYMBOL_SETDELAYED, std::move(Args)));
        return Error;
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Right));
    
    if (wasInsideSlashColon) {
        auto L = NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
        return TheParser->infixLoop(std::move(L), CtxtIn);
    }
    
    auto L = NodePtr(new BinaryNode(SYMBOL_SETDELAYED, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr IntegralParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    auto operand = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    if (operand->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
        
        NodeSeq Args(1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(std::move(ProperExpectedOperandError));
        
        auto Error = NodePtr(new PrefixNode(SYMBOL_INTEGRAL, std::move(Args)));
        return Error;
    }
    
    Ctxt.Flag &= ~(PARSER_INSIDE_INTEGRAL);
    
    LeafSeq Trivia2;
    
    Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia2);
    
    if (!Tok.Tok.isDifferentialD()) {
        
        NodeSeq Args(1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(std::move(operand));
        
        auto L = NodePtr(new PrefixNode(SYMBOL_INTEGRAL, std::move(Args)));
        return TheParser->infixLoop(std::move(L), CtxtIn);
    }

    auto variable = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    if (variable->isExpectedOperandError()) {
        
        //
        // Reattach the ExpectedOperand Error to the operator for a better experience
        //
        
        auto ProperExpectedOperandError = NodePtr(new ExpectedOperandErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
        
        NodeSeq Args(1 + 1 + 1 + 1);
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(std::move(operand));
        Args.append(std::move(ProperExpectedOperandError));
        
        auto Error = NodePtr(new PrefixNode(SYMBOL_INTEGRATE, std::move(Args)));
        return Error;
    }
    
    NodeSeq Args(1 + 1 + 1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(operand));
    Args.appendIfNonEmpty(std::move(Trivia2));
    Args.append(std::move(variable));
    
    auto L = NodePtr(new PrefixBinaryNode(SYMBOL_INTEGRATE, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr CommaParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto lastOperatorToken = TokIn;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    NodePtr L;
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    {
        LeafSeq Trivia2;
        
        auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        if (infixParselets[Tok2.Tok.value()]->getOp() == SYMBOL_CODEPARSER_COMMA) {
            
            //
            // Something like  a,,
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
            
            lastOperatorToken = Tok2;
            
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
        } else {
            
            auto Operand = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
            
            if (Operand->isExpectedOperandError()) {
                
                //
                // Convert the ExpectedOperand Error to ImplicitNull and reattach to the operator for a better experience
                //
                
                auto ProperImplicitNull = NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(TokIn.BufLen.end), Source(TokIn.Src.End))));
                
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.append(std::move(ProperImplicitNull));
                
            } else {
                
                Args.append(NodePtr(new LeafNode(TokIn)));
                Args.appendIfNonEmpty(std::move(Trivia2));
                Args.append(std::move(Operand));
            }
        }
    }
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq Trivia1;
        
        auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a,b\[InvisibleComma]c
        //
        // and we want only a single Infix node created
        //
        if (infixParselets[Tok1.Tok.value()]->getOp() != SYMBOL_CODEPARSER_COMMA) {
            
            L = NodePtr(new InfixNode(SYMBOL_CODEPARSER_COMMA, std::move(Args)));
            
            break;
        }
            
        lastOperatorToken = Tok1;
        
        //
        // Something like  a,b
        //
        
        TheParser->nextToken(Tok1);
        
        LeafSeq Trivia2;
        
        auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        if (infixParselets[Tok2.Tok.value()]->getOp() == SYMBOL_CODEPARSER_COMMA) {
            
            //
            // Something like  a,,
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
            
            lastOperatorToken = Tok2;
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
            continue;
        }
            
        auto Operand = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
        
        if (Operand->isExpectedOperandError()) {
            
            //
            // Convert the ExpectedOperand Error to ImplicitNull and reattach to the operator for a better experience
            //
            
            auto ProperImplicitNull = NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(Tok1.BufLen.end), Source(Tok1.Src.End))));
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            Args.append(std::move(ProperImplicitNull));
            
            continue;
        }
        
        //
        // Do not reserve inside loop
        // Allow default resizing strategy, which is hopefully exponential
        //
        Args.appendIfNonEmpty(std::move(Trivia1));
        Args.append(NodePtr(new LeafNode(Tok1)));
        Args.appendIfNonEmpty(std::move(Trivia2));
        Args.append(std::move(Operand));
        
    } // while
    
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr SemiParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    auto lastOperatorToken = TokIn;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    NodePtr L;
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    {
        LeafSeq Trivia2;
        
        auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        if (Tok2.Tok == TOKEN_SEMI) {
            
            //
            // Something like  a; ;
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
            
            lastOperatorToken = Tok2;
            
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
        } else if (Tok2.Tok.isPossibleBeginning()) {
            
            auto operand = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
            
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.appendIfNonEmpty(std::move(Trivia2));
            Args.append(std::move(operand));
            
        } else {
            
            //
            // Not beginning of an expression
            //
            // For example:  a;&
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
            
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
            L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
            
            goto SemiParseletExit;
        }
    }
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq Trivia1;
        
        auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
        
        if (Tok1.Tok != TOKEN_SEMI) {
            
            L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
            
            break;
        }
        
        lastOperatorToken = Tok1;
        
        //
        // Something like  a;b
        //
        
        TheParser->nextToken(Tok1);
        
        LeafSeq Trivia2;
        
        auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        if (Tok2.Tok == TOKEN_SEMI) {
            
            //
            // Something like  a; ;
            //
            
            auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, BufferAndLength(lastOperatorToken.BufLen.end), Source(lastOperatorToken.Src.End));
            
            lastOperatorToken = Tok2;
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
        } else if (Tok2.Tok.isPossibleBeginning()) {
            
            auto operand = prefixParselets[Tok2.Tok.value()]->parse(Tok2, Ctxt);
            
            //
            // Do not reserve inside loop
            // Allow default resizing strategy, which is hopefully exponential
            //
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            Args.appendIfNonEmpty(std::move(Trivia2));
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
            Args.appendIfNonEmpty(std::move(Trivia1));
            Args.append(NodePtr(new LeafNode(Tok1)));
            Args.append(NodePtr(new LeafNode(Implicit)));
            
            L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
            
            break;
        }
        
    } // while
    
SemiParseletExit:
    
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr ColonColonParselet::parse(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    NodeSeq Args(1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    
    //
    // Not used here because of the special rules for tokenizing after ::
    //
    //auto Ctxt = CtxtIn;
    //Ctxt.Prec = PRECEDENCE_COLONCOLON;
    //Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok2 = TheParser->currentToken_stringifyAsSymbolSegment();
    
    TheParser->nextToken(Tok2);
    
    NodePtr Operand;
    if (Tok2.Tok.isError()) {
        if (Tok2.Tok.isUnterminated()) {
            Operand = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(Tok2));
        } else {
            Operand = NodePtr(new ErrorNode(Tok2));
        }
    } else {
        assert(Tok2.Tok == TOKEN_STRING);
        Operand = NodePtr(new LeafNode(Tok2));
    }
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.append(std::move(Operand));
    
    while (true) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        
        auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        
        if (Tok1.Tok != TOKEN_COLONCOLON) {
            break;
        }
        
        TheParser->nextToken(Tok1);
    
        //
        // Special tokenization, so must do parsing here
        //
    
        auto Tok2 = TheParser->currentToken_stringifyAsSymbolSegment();
    
        TheParser->nextToken(Tok2);
    
        NodePtr Operand;
        if (Tok2.Tok.isError()) {
            if (Tok2.Tok.isUnterminated()) {
                Operand = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(Tok2));
            } else {
                Operand = NodePtr(new ErrorNode(Tok2));
            }
        } else {
            assert(Tok2.Tok == TOKEN_STRING);
            Operand = NodePtr(new LeafNode(Tok2));
        }
        
        //
        // Do not reserve inside loop
        // Allow default resizing strategy, which is hopefully exponential
        //
        Args.append(NodePtr(new LeafNode(Tok1)));
        Args.append(std::move(Operand));
        
    } // while
    
    auto L = NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
    return TheParser->infixLoop(std::move(L), Ctxt);
}


NodePtr GreaterGreaterParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_GREATERGREATER;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok = TheParser->currentToken_stringifyAsFile();
    Tok = TheParser->eatTrivia_stringifyAsFile(Tok, Ctxt, Trivia1);
    
    TheParser->nextToken(Tok);
    
    NodePtr Operand;
    if (Tok.Tok.isError()) {
        if (Tok.Tok.isUnterminated()) {
            Operand = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(Tok));
        } else {
            Operand = NodePtr(new ErrorNode(Tok));
        }
    } else {
        assert(Tok.Tok == TOKEN_STRING);
        Operand = NodePtr(new LeafNode(Tok));
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Operand));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_PUT, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr GreaterGreaterGreaterParselet::parse(NodeSeq Left, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_GREATERGREATERGREATER;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok = TheParser->currentToken_stringifyAsFile();
    Tok = TheParser->eatTrivia_stringifyAsFile(Tok, Ctxt, Trivia1);
    
    TheParser->nextToken(Tok);
    
    NodePtr Operand;
    if (Tok.Tok.isError()) {
        if (Tok.Tok.isUnterminated()) {
            Operand = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(Tok));
        } else {
            Operand = NodePtr(new ErrorNode(Tok));
        }
    } else {
        assert(Tok.Tok == TOKEN_STRING);
        Operand = NodePtr(new LeafNode(Tok));
    }
    
    NodeSeq Args(1 + 1 + 1 + 1);
    Args.append(NodePtr(new NodeSeqNode(std::move(Left))));
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Operand));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_PUTAPPEND, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


NodePtr LessLessParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LESSLESS;
    
    TheParser->nextToken(TokIn);
    
    LeafSeq Trivia1;
    
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok = TheParser->currentToken_stringifyAsFile();
    Tok = TheParser->eatTrivia_stringifyAsFile(Tok, Ctxt, Trivia1);
    
    TheParser->nextToken(Tok);
    
    NodePtr Operand;
    if (Tok.Tok.isError()) {
        if (Tok.Tok.isUnterminated()) {
            Operand = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(Tok));
        } else {
            Operand = NodePtr(new ErrorNode(Tok));
        }
    } else {
        assert(Tok.Tok == TOKEN_STRING);
        Operand = NodePtr(new LeafNode(Tok));
    }
    
    NodeSeq Args(1 + 1 + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendIfNonEmpty(std::move(Trivia1));
    Args.append(std::move(Operand));
    
    auto L = NodePtr(new PrefixNode(SYMBOL_GET, std::move(Args)));
    return TheParser->infixLoop(std::move(L), CtxtIn);
}


Precedence InfixDifferentialDParselet::getPrecedence(ParserContext Ctxt) const {
    
    if ((Ctxt.Flag & PARSER_INSIDE_INTEGRAL) == PARSER_INSIDE_INTEGRAL) {

        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //

        return PRECEDENCE_LOWEST;
    }

    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

Token InfixDifferentialDParselet::processImplicitTimes(Token TokIn, ParserContext Ctxt) const {
    
    if ((Ctxt.Flag & PARSER_INSIDE_INTEGRAL) == PARSER_INSIDE_INTEGRAL) {
        
        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //
        
        return TokIn;
    }
    
    //
    // BufAndLen and Src will be filled in properly later
    //
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(), Source());
}


NodePtr HashParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(TokIn);
    
    NodePtr Slot;
    
    auto Tok = TheParser->currentToken(Ctxt, INSIDE_SLOT);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value():
        case TOKEN_STRING.value(): {
            
            TheParser->nextToken(Tok);
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            Slot = NodePtr(new CompoundNode(SYMBOL_SLOT, std::move(Args)));
        }
            break;
        default: {
            
            Slot = NodePtr(new LeafNode(TokIn));
        }
            break;
    }
    
    return TheParser->infixLoop(std::move(Slot), CtxtIn);
}


NodePtr HashHashParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(TokIn);
    
    NodePtr SlotSequence;
    
    auto Tok = TheParser->currentToken(Ctxt, INSIDE_SLOTSEQUENCE);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            TheParser->nextToken(Tok);
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            SlotSequence = NodePtr(new CompoundNode(SYMBOL_SLOTSEQUENCE, std::move(Args)));
        }
            break;
        default: {
            
            SlotSequence = NodePtr(new LeafNode(TokIn));
        }
            break;
    }
    
    return TheParser->infixLoop(std::move(SlotSequence), CtxtIn);
}


NodePtr PercentParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(TokIn);
    
    NodePtr Out;
    
    auto Tok = TheParser->currentToken(Ctxt, INSIDE_OUT);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            TheParser->nextToken(Tok);
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            Out = NodePtr(new CompoundNode(SYMBOL_OUT, std::move(Args)));
        }
            break;
        default: {
            
            Out = NodePtr(new LeafNode(TokIn));
        }
            break;
    }
    
    return TheParser->infixLoop(std::move(Out), CtxtIn);
}

NodePtr PercentPercentParselet::parse(Token TokIn, ParserContext CtxtIn) const {
    
    TheParser->nextToken(TokIn);
    
    auto Out = NodePtr(new LeafNode(TokIn));
    
    return TheParser->infixLoop(std::move(Out), CtxtIn);
}
