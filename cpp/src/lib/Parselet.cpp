
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h" // for infixParselets, etc.
#include "Symbol.h"


NodePtr LeafParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    auto Left = NodePtr(new LeafNode(TokIn));
    
    return TheParser->parseLoop(std::move(Left), Ctxt);
}


NodePtr PrefixErrorParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
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


NodePtr PrefixCloserParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isCloser());
        
    //
    // Inside some other parselet that is not GroupParselet
    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    return Error;
}


NodePtr PrefixToplevelCloserParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isCloser());
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    TheParser->nextToken(TokIn);
    
    auto Error = NodePtr(new ErrorNode(Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.BufLen, TokIn.Src)));
    
    return Error;
}


NodePtr PrefixEndOfFileParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    //
    // Something like  a+<EOF>
    //
    
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    return Error;
}


NodePtr PrefixUnsupportedTokenParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    auto createdToken = Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.BufLen, TokIn.Src);
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    return Error;
}


NodePtr PrefixCommaParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_ERROR_PREFIXIMPLICITNULL
    //
    if (Ctxt.Prec == PRECEDENCE_LOWEST) {
        
        auto createdToken = Token(TOKEN_ERROR_PREFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
        auto Left = NodePtr(new ErrorNode(createdToken));
        
        return TheParser->parseLoop(std::move(Left), Ctxt);
    }
        
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    auto Left = NodePtr(new ErrorNode(createdToken));
    
    return Left;
}


NodePtr PrefixUnhandledParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    assert(!TokIn.Tok.isPossibleBeginning() && "handle at call site");
    
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON | PARSER_INSIDE_TILDE);
    
    auto NotPossible = NodePtr(new ErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start)));
    
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
    
    return infixParselets[TokIn.Tok.value()]->parseInfix(std::move(LeftSeq), TokIn, Ctxt);
}


NodePtr InfixToplevelNewlineParselet::parseInfix(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    
    assert(false);
    
    return nullptr;
}


NodePtr SymbolParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
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
            
            return contextSensitiveUnder1Parselet->parseInfixContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnder2Parselet->parseInfixContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnder3Parselet->parseInfixContextSensitive(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_UNDERDOT.value(): {
            
            NodeSeq Args(1);
            Args.append(std::move(Sym));
            
            return contextSensitiveUnderDotParselet->parseInfixContextSensitive(std::move(Args), Tok, Ctxt);
        }
        default: {
            
            TriviaSeq Trivia1;
            
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
                    
                    NodeSeq Args(1 + Trivia1.size());
                    Args.append(std::move(Sym));
                    Args.appendSeq(std::move(Trivia1));
                    
                    auto Sym = infixParselets[TOKEN_COLON.value()]->parseInfix(std::move(Args), Tok, Ctxt);
                        
                    return TheParser->parseLoop(std::move(Sym), Ctxt);
                }
                    
                Trivia1.reset();
                
                return TheParser->parseLoop(std::move(Sym), Ctxt);
            }
                
            Trivia1.reset();
            
            return TheParser->parseLoop(std::move(Sym), Ctxt);
        }
    }
    
    assert(false);
    
    return nullptr;
}

NodePtr SymbolParselet::parsePrefixContextSensitive(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    return Sym;
}


const SymbolPtr& InfixParselet::getOp() const {
    return SYMBOL_CODEPARSER_INTERNALINVALID;
}


NodePtr PrefixOperatorParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    auto Operand = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(std::move(Args), std::move(Operand), CtxtIn);
}

NodePtr PrefixOperatorParselet::parse1(NodeSeq Args, NodePtr Operand, ParserContext CtxtIn) const {
    
    Args.append(std::move(Operand));
    
    auto Left = NodePtr(new PrefixNode(Op, std::move(Args)));
    
    return TheParser->parseLoop(std::move(Left), CtxtIn);
}


NodePtr InfixImplicitTimesParselet::parseInfix(NodeSeq Left, Token TokIn, ParserContext Ctxt) const {
    
    assert(false);
    
    return nullptr;
}


Precedence InfixImplicitTimesParselet::getPrecedence(ParserContext Ctxt) const {
    
    assert(false && "The last token may not have been added to InfixParselets");
    
    return PRECEDENCE_ASSERTFALSE;
}


Token InfixImplicitTimesParselet::processImplicitTimes(Token TokIn, ParserContext Ctxt) const {
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
}


NodePtr InfixAssertFalseParselet::parseInfix(NodeSeq Left, Token firstTok, ParserContext Ctxt) const {
    
    assert(false);
    
    return nullptr;
}


NodePtr BinaryOperatorParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;

    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    auto Right = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
        
    return parse1(std::move(Args), std::move(Right), CtxtIn);
}

NodePtr BinaryOperatorParselet::parse1(NodeSeq Args, NodePtr Right, ParserContext CtxtIn) const {
    
    Args.append(std::move(Right));
    
    auto L = NodePtr(new BinaryNode(Op, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr InfixOperatorParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    TriviaSeq Trivia2;
    
    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    auto Operand = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
    return parse1(std::move(Args), std::move(Operand), CtxtIn);
}

NodePtr InfixOperatorParselet::parse1(NodeSeq Args, NodePtr Operand, ParserContext CtxtIn) const {
    
    auto OperandLastToken = Operand->lastToken();
    
    Args.append(std::move(Operand));
    
    return parseLoop(std::move(Args), OperandLastToken, CtxtIn);
}

NodePtr InfixOperatorParselet::parseLoop(NodeSeq Args, Token OperandLastToken, ParserContext CtxtIn) const {
    
    while (true) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);

    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);

    TriviaSeq Trivia1;

    Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
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
        
        auto L = NodePtr(new InfixNode(Op, std::move(Args)));
        
        return TheParser->parseLoop(std::move(L), CtxtIn);
    }
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    
    if (Tok1.Tok != TOKEN_FAKE_IMPLICITTIMES) {

        TheParser->nextToken(Tok1);
    }

    TriviaSeq Trivia2;

    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    auto Operand = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
    OperandLastToken = Operand->lastToken();

    Args.append(std::move(Operand));
    
    continue;
    
    } // while (true)
}


NodePtr PostfixOperatorParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    auto L = NodePtr(new PostfixNode(Op, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), Ctxt);
}


GroupParselet::GroupParselet(TokenEnum Opener, const SymbolPtr& Op) : Op(Op), Closr(GroupOpenerToCloser(Opener)) {}

NodePtr GroupParselet::parsePrefix(Token firstTok, ParserContext CtxtIn) const {
    
    auto OpenerT = firstTok;
    
    TheParser->nextToken(firstTok);
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(OpenerT)));
    
    return parseLoop(std::move(Args), CtxtIn);
}
 
NodePtr GroupParselet::parseLoop(NodeSeq Args, ParserContext CtxtIn) const {
    
    while (true) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto Ctxt = CtxtIn;
    Ctxt.Closr = Closr;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON | PARSER_INSIDE_TILDE);
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    // ADDENDUM: Actually, there may be more than 1 "good" node
    // e.g. {1\\2}
    //

    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    if (TokenToCloser(Tok.Tok) == Closr) {
        
        //
        // Everything is good
        //

        TheParser->nextToken(Tok);
        
        Args.append(NodePtr(new LeafNode(Tok)));

        auto group = NodePtr(new GroupNode(Op, std::move(Args)));

        return TheParser->parseLoop(std::move(group), CtxtIn);
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

            auto group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
            
            return TheParser->parseLoop(std::move(group), CtxtIn);
        }

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
        
        auto Error = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
        //
        // Always append here
        //
        Args.append(std::move(Error));
        
        continue;
    }

    if (Tok.Tok == TOKEN_ENDOFFILE) {

        //
        // Handle something like   { a EOF
        //

        auto group = NodePtr(new UnterminatedGroupNeedsReparseNode(Op, std::move(Args)));

        return TheParser->parseLoop(std::move(group), CtxtIn);
    }

    //
    // Handle the expression
    //
    
    auto Operand = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    //
    // Always append here
    //
    Args.append(std::move(Operand));
    
    continue;
        
    } // while (true)
}


NodePtr CallParselet::parseInfix(NodeSeq Head, Token TokIn, ParserContext CtxtIn) const {
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    
    auto Right = GP->parsePrefix(TokIn, Ctxt);
    
    return parse1(std::move(Head), std::move(Right), CtxtIn);
}

NodePtr CallParselet::parse1(NodeSeq Head, NodePtr Right, ParserContext CtxtIn) const {
    
    NodeSeq Args(1);
    Args.append(std::move(Right));
    
    auto L = NodePtr(new CallNode(std::move(Head), std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr TildeParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto FirstTok = TheParser->currentToken(Ctxt, TOPLEVEL);
    FirstTok = TheParser->eatTrivia(FirstTok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    Ctxt.Flag |= PARSER_INSIDE_TILDE;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    
    auto Middle = prefixParselets[FirstTok.Tok.value()]->parsePrefix(FirstTok, Ctxt);
    
    return parse1(std::move(Args), std::move(Middle), CtxtIn);
}

NodePtr TildeParselet::parse1(NodeSeq Args, NodePtr Middle, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LOWEST;
    Ctxt.Flag |= PARSER_INSIDE_TILDE;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    
    Args.append(std::move(Middle));
        
    TriviaSeq Trivia2;
    
    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
        
    if (Tok1.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
        
        return Error;
    }
    
    TriviaSeq Trivia3;
    
    //
    // Reset back to "outside" precedence
    //
    Ctxt.Prec = PRECEDENCE_TILDE;
    Ctxt.Flag &= (~PARSER_INSIDE_TILDE);
    
    Args.append(NodePtr(new LeafNode(Tok1)));
        
    TheParser->nextToken(Tok1);
    
    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia3);
    
    Args.appendSeq(std::move(Trivia3));
        
    auto Right = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
    return parse2(std::move(Args), std::move(Right), CtxtIn);
}

NodePtr TildeParselet::parse2(NodeSeq Args, NodePtr Right, ParserContext CtxtIn) const {
    
    Args.append(std::move(Right));
    
    auto L = NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr ColonParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    assert((CtxtIn.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Flag |= PARSER_INSIDE_COLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    auto Right = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(std::move(Args), std::move(Right), CtxtIn);
}

NodePtr ColonParselet::parse1(NodeSeq Args, NodePtr Right, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Flag |= PARSER_INSIDE_COLON;
    
    Args.append(std::move(Right));
    
    auto Pat = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
    
    TriviaSeq Trivia2;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, TOPLEVEL, Trivia2);
    
    if (Tok.Tok == TOKEN_COLON) {
        
        Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
        
        NodeSeq PatSeq(1 + Trivia2.size());
        PatSeq.append(std::move(Pat));
        PatSeq.appendSeq(std::move(Trivia2));
        
        return parseInfixContextSensitive(std::move(PatSeq), Tok, Ctxt);
    }
    
    Trivia2.reset();
    
    return TheParser->parseLoop(std::move(Pat), CtxtIn);
}

NodePtr ColonParselet::parseInfixContextSensitive(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert((CtxtIn.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    auto Right = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
        
    return parse2(std::move(Args), std::move(Right), CtxtIn);
}

NodePtr ColonParselet::parse2(NodeSeq Args, NodePtr Right, ParserContext CtxtIn) const {
    
    Args.append(std::move(Right));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr SlashColonParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_SLASHCOLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    auto Middle = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(std::move(Args), std::move(Middle), CtxtIn);
}

NodePtr SlashColonParselet::parse1(NodeSeq Args, NodePtr Middle, ParserContext Ctxt) const {
    
    Args.append(std::move(Middle));
        
    TriviaSeq Trivia2;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
        
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            return infixParselets[TOKEN_EQUAL.value()]->parseInfix(std::move(Args), Tok, Ctxt);
        }
        case TOKEN_COLONEQUAL.value(): {
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            return infixParselets[TOKEN_COLONEQUAL.value()]->parseInfix(std::move(Args), Tok, Ctxt);
        }
        default: {
            
            //
            // Anything other than:
            // a /: b = c
            // a /: b := c
            // a /: b =.
            //
            
            auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSET, std::move(Args)));
            
            return Error;
        }
    }
}


EqualParselet::EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, SYMBOL_SET) {}

NodePtr EqualParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        TheParser->nextToken(Tok);
        
        Args.append(NodePtr(new LeafNode(Tok)));
        
        NodePtr L;
        
        if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            
            L = NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
            
        } else {
            
            L = NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
        }
        
        return TheParser->parseLoop(std::move(L), CtxtIn);
    }
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto Right = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(std::move(Args), std::move(Right), CtxtIn);
}

NodePtr EqualParselet::parse1(NodeSeq Args, NodePtr Right, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    auto wasInsideSlashColon = ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON);
    
    Args.append(std::move(Right));
    
    NodePtr L;
    
    if (wasInsideSlashColon) {
        
        L = NodePtr(new TernaryNode(SYMBOL_TAGSET, std::move(Args)));
        
    } else {
        
        L = NodePtr(new BinaryNode(SYMBOL_SET, std::move(Args)));
    }
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


ColonEqualParselet::ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}

NodePtr ColonEqualParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto Right = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(std::move(Args), std::move(Right), CtxtIn);
}

NodePtr ColonEqualParselet::parse1(NodeSeq Args, NodePtr Right, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    auto wasInsideSlashColon = ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON);
    
    Args.append(std::move(Right));
    
    NodePtr L;
    
    if (wasInsideSlashColon) {
        
        L = NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
        
    } else {
        
        L = NodePtr(new BinaryNode(SYMBOL_SETDELAYED, std::move(Args)));
    }
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr IntegralParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    
    NodeSeq Args(1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    auto operand = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(std::move(Args), std::move(operand), CtxtIn);
}

NodePtr IntegralParselet::parse1(NodeSeq Args, NodePtr operand, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    Ctxt.Flag &= ~(PARSER_INSIDE_INTEGRAL);
    
    Args.append(std::move(operand));
        
    TriviaSeq Trivia2;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    if (!Tok.Tok.isDifferentialD()) {
        
        auto L = NodePtr(new PrefixNode(SYMBOL_INTEGRAL, std::move(Args)));
        
        return TheParser->parseLoop(std::move(L), CtxtIn);
    }
        
    auto variable = prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
        
    return parse2(std::move(Args), std::move(variable), CtxtIn);
}

NodePtr IntegralParselet::parse2(NodeSeq Args, NodePtr variable, ParserContext CtxtIn) const {
    
    Args.append(std::move(variable));
    
    auto L = NodePtr(new PrefixBinaryNode(SYMBOL_INTEGRATE, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr CommaParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    TriviaSeq Trivia2;
    
    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    //
    // Cannot just compare tokens
    //
    // May be something like  a,b\[InvisibleComma]c
    //
    // and we want only a single Infix node created
    //
    if (infixParselets[Tok2.Tok.value()]->getOp() == SYMBOL_CODEPARSER_COMMA) {
        
        //
        // Something like  a,,
        //
        
        auto Implicit = NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
        Args.append(std::move(Implicit));
        
        return parseLoop(std::move(Args), CtxtIn);
    }
    
    auto Operand = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
    return parse1(std::move(Args), std::move(Operand), CtxtIn);
}

NodePtr CommaParselet::parse1(NodeSeq Args, NodePtr Operand, ParserContext CtxtIn) const {
    
    if (Operand->isExpectedOperandError()) {
        
        //
        // Something like  f[1,]
        //
        
        //
        // Convert the ExpectedOperand Error to ImplicitNull
        //
        
        auto Tok2 = Operand->lastToken();
        
        auto Implicit = NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
        Args.append(std::move(Implicit));
                
        return parseLoop(std::move(Args), CtxtIn);
    }
    
    Args.append(std::move(Operand));
    
    return parseLoop(std::move(Args), CtxtIn);
}

NodePtr CommaParselet::parseLoop(NodeSeq Args, ParserContext CtxtIn) const {
    
    while (true) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);

    TriviaSeq Trivia1;

    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    //
    // Cannot just compare tokens
    //
    // May be something like  a,b\[InvisibleComma]c
    //
    // and we want only a single Infix node created
    //
    if (infixParselets[Tok1.Tok.value()]->getOp() != SYMBOL_CODEPARSER_COMMA) {
        
        auto L = NodePtr(new InfixNode(SYMBOL_CODEPARSER_COMMA, std::move(Args)));
        
        return TheParser->parseLoop(std::move(L), CtxtIn);
    }

    //
    // Something like  a,b
    //
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    
    TheParser->nextToken(Tok1);

    TriviaSeq Trivia2;

    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    //
    // Cannot just compare tokens
    //
    // May be something like  a,b\[InvisibleComma]c
    //
    // and we want only a single Infix node created
    //
    if (infixParselets[Tok2.Tok.value()]->getOp() == SYMBOL_CODEPARSER_COMMA) {

        //
        // Something like  a,,
        //

        auto Implicit = NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
        Args.append(std::move(Implicit));
        
        continue;
    }
    
    auto Operand = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
    
    if (Operand->isExpectedOperandError()) {
        
        //
        // Something like  f[1,2,]
        //

        //
        // Convert the ExpectedOperand Error to ImplicitNull
        //
        
        auto Tok2 = Operand->lastToken();
        
        auto Implicit = NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
        Args.append(std::move(Implicit));
        
        continue;
    }
    
    Args.append(std::move(Operand));
    
    continue;
        
    } // while (true)
}

const SymbolPtr& CommaParselet::getOp() const {
    return SYMBOL_CODEPARSER_COMMA;
}


NodePtr SemiParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    TriviaSeq Trivia2;
    
    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    if (Tok2.Tok == TOKEN_SEMI) {
        
        //
        // Something like  a; ;
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
        
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        return parseLoop(std::move(Args), CtxtIn);
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        auto operand = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
            
        return parse1(std::move(Args), std::move(operand), CtxtIn);
    }
        
    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
    
    Args.append(NodePtr(new LeafNode(Implicit)));
    
    auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}

NodePtr SemiParselet::parse1(NodeSeq Args, NodePtr operand, ParserContext CtxtIn) const {
    
    Args.append(std::move(operand));
    
    return parseLoop(std::move(Args), CtxtIn);
}

NodePtr SemiParselet::parseLoop(NodeSeq Args, ParserContext CtxtIn) const {
    
    while (true) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);

    TriviaSeq Trivia1;

    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    if (Tok1.Tok != TOKEN_SEMI) {

        auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));

        return TheParser->parseLoop(std::move(L), CtxtIn);
    }

    //
    // Something like  a;b
    //

    TheParser->nextToken(Tok1);

    TriviaSeq Trivia2;

    auto Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    
    if (Tok2.Tok == TOKEN_SEMI) {

        //
        // Something like  a; ;
        //

        auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
        
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        continue;
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        auto operand = prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
        Args.append(std::move(operand));
        
        continue;
    }

    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //

    auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
    
    Args.append(NodePtr(new LeafNode(Implicit)));

    auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));

    return TheParser->parseLoop(std::move(L), CtxtIn);
    
    } // while (true)
}


NodePtr ColonColonParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext Ctxt) const {
    
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
    
    auto Tok2 = TheParser->currentToken_stringifyAsTag();
    
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
    
    return parseLoop(std::move(Args), Ctxt);
}

NodePtr ColonColonParselet::parseLoop(NodeSeq Args, ParserContext Ctxt) const {
    
    while (true) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);

    if (Tok1.Tok != TOKEN_COLONCOLON) {

        auto L = NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
        
        return TheParser->parseLoop(std::move(L), Ctxt);
    }

    TheParser->nextToken(Tok1);

    //
    // Special tokenization, so must do parsing here
    //

    auto Tok2 = TheParser->currentToken_stringifyAsTag();

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
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    Args.append(std::move(Operand));
    
    continue;
    
    } // while (true)
}


NodePtr GreaterGreaterParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_GREATERGREATER;
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
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
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendSeq(std::move(Trivia1));
    Args.append(std::move(Operand));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_PUT, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr GreaterGreaterGreaterParselet::parseInfix(NodeSeq Args, Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_GREATERGREATERGREATER;
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
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
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendSeq(std::move(Trivia1));
    Args.append(std::move(Operand));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_PUTAPPEND, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
}


NodePtr LessLessParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LESSLESS;
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
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
    
    NodeSeq Args(1 + Trivia1.size() + 1);
    Args.append(NodePtr(new LeafNode(TokIn)));
    Args.appendSeq(std::move(Trivia1));
    Args.append(std::move(Operand));
    
    auto L = NodePtr(new PrefixNode(SYMBOL_GET, std::move(Args)));
    
    return TheParser->parseLoop(std::move(L), CtxtIn);
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
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
}


NodePtr HashParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(CtxtIn, INSIDE_SLOT);
    
    NodePtr Slot;
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value():
        case TOKEN_STRING.value(): {
            
            TheParser->nextToken(Tok);
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            Slot = NodePtr(new CompoundNode(SYMBOL_SLOT, std::move(Args)));
            
            break;
        }
        default: {
            
            Slot = NodePtr(new LeafNode(TokIn));
            
            break;
        }
    }
    
    return TheParser->parseLoop(std::move(Slot), CtxtIn);
}


NodePtr HashHashParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(CtxtIn, INSIDE_SLOTSEQUENCE);
    
    NodePtr SlotSequence;
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            TheParser->nextToken(Tok);
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            SlotSequence = NodePtr(new CompoundNode(SYMBOL_SLOTSEQUENCE, std::move(Args)));
            
            break;
        }
        default: {
            
            SlotSequence = NodePtr(new LeafNode(TokIn));
            
            break;
        }
    }
    
    return TheParser->parseLoop(std::move(SlotSequence), CtxtIn);
}


NodePtr PercentParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(CtxtIn, INSIDE_OUT);
    
    NodePtr Out;
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            TheParser->nextToken(Tok);
            
            NodeSeq Args(1 + 1);
            Args.append(NodePtr(new LeafNode(TokIn)));
            Args.append(NodePtr(new LeafNode(Tok)));
            
            Out = NodePtr(new CompoundNode(SYMBOL_OUT, std::move(Args)));
            
            break;
        }
        default: {
            
            Out = NodePtr(new LeafNode(TokIn));
            
            break;
        }
    }
    
    return TheParser->parseLoop(std::move(Out), CtxtIn);
}

NodePtr PercentPercentParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    TheParser->nextToken(TokIn);

    auto Out = NodePtr(new LeafNode(TokIn));
    
    return TheParser->parseLoop(std::move(Out), CtxtIn);
}
