
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h" // for infixParselets, etc.
#include "Symbol.h"


void LeafParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    auto Left = NodePtr(new LeafNode(TokIn));
    
    TheParser->pushNode(std::move(Left));
    
    return TheParser->parseLoop(Ctxt);
}


void PrefixErrorParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isError());
    
    TheParser->nextToken(TokIn);
    
    NodePtr Error;
    
    if (TokIn.Tok.isUnterminated()) {
        
        Error = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(TokIn));
        
    } else {
        
        Error = NodePtr(new ErrorNode(TokIn));
    }
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


void PrefixCloserParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isCloser());
        
    //
    // Inside some other parselet that is not GroupParselet
    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    
    Token createdToken;
    
    if (Ctxt.Prec == PRECEDENCE_COMMA) {
        
        createdToken = Token(TOKEN_ERROR_INFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    }
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


void PrefixToplevelCloserParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok.isCloser());
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    TheParser->nextToken(TokIn);
    
    auto Error = NodePtr(new ErrorNode(Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.BufLen, TokIn.Src)));
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


void PrefixEndOfFileParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    //
    // Something like  a+<EOF>
    //
    
    Token createdToken;
    
    if (Ctxt.Prec == PRECEDENCE_COMMA) {
            
        createdToken = Token(TOKEN_ERROR_INFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    }
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


void PrefixUnsupportedTokenParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    TheParser->nextToken(TokIn);
    
    auto createdToken = Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.BufLen, TokIn.Src);
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


void PrefixCommaParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_ERROR_PREFIXIMPLICITNULL
    //
    if (Ctxt.Prec == PRECEDENCE_LOWEST) {
        
        auto createdToken = Token(TOKEN_ERROR_PREFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
        auto Left = NodePtr(new ErrorNode(createdToken));
        
        TheParser->pushNode(std::move(Left));
        
        return TheParser->parseLoop(Ctxt);
    }
        
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    auto Left = NodePtr(new ErrorNode(createdToken));
    
    TheParser->pushNode(std::move(Left));
    
    return;
}


void PrefixUnhandledParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
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
        
        TheParser->pushNode(std::move(NotPossible));
        
        return;
    }
    
    //
    // Handle something like  f[@2]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Operator node.
    //
    // Do not take next token
    //
    
    auto& LeftSeq = TheParser->pushArgs();
    LeftSeq.append(std::move(NotPossible));
    
//    MUSTTAIL
    return infixParselets[TokIn.Tok.value()]->parseInfix(TokIn, Ctxt);
}


void InfixToplevelNewlineParselet::parseInfix(Token firstTok, ParserContext Ctxt) const {
    
    assert(false);
    
    return;
}


void SymbolParselet::parsePrefix(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            auto& Args = TheParser->pushArgs();
            Args.append(std::move(Sym));
            
//            MUSTTAIL
            return contextSensitiveUnder1Parselet->parseInfixContextSensitive(Tok, Ctxt);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            auto& Args = TheParser->pushArgs();
            Args.append(std::move(Sym));
            
//            MUSTTAIL
            return contextSensitiveUnder2Parselet->parseInfixContextSensitive(Tok, Ctxt);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            auto& Args = TheParser->pushArgs();
            Args.append(std::move(Sym));
            
//            MUSTTAIL
            return contextSensitiveUnder3Parselet->parseInfixContextSensitive(Tok, Ctxt);
        }
        case TOKEN_UNDERDOT.value(): {
            
            auto& Args = TheParser->pushArgs();
            Args.append(std::move(Sym));
            
//            MUSTTAIL
            return contextSensitiveUnderDotParselet->parseInfixContextSensitive(Tok, Ctxt);
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
                    
                    auto& Args = TheParser->pushArgs();
                    Args.append(std::move(Sym));
                    Args.appendSeq(std::move(Trivia1));
                    
                    infixParselets[TOKEN_COLON.value()]->parseInfix(Tok, Ctxt);
                        
                    return TheParser->parseLoop(Ctxt);
                }
                    
                Trivia1.reset();
                
                TheParser->pushNode(std::move(Sym));
                
                return TheParser->parseLoop(Ctxt);
            }
                
            Trivia1.reset();
            
            TheParser->pushNode(std::move(Sym));
            
            return TheParser->parseLoop(Ctxt);
        }
    }
    
    assert(false);
    
    return;
}

void SymbolParselet::parsePrefixContextSensitive(Token TokIn, ParserContext Ctxt) const {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    TheParser->pushNode(std::move(Sym));
    
    return;
}


const SymbolPtr& InfixParselet::getOp() const {
    return SYMBOL_CODEPARSER_INTERNALINVALID;
}


void PrefixOperatorParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    auto& Args = TheParser->pushArgs();
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(CtxtIn);
}

void PrefixOperatorParselet::parse1(ParserContext CtxtIn) const {
    
    auto Operand = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Operand));
    
    auto Left = NodePtr(new PrefixNode(Op, std::move(Args)));
    
    TheParser->pushNode(std::move(Left));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void InfixImplicitTimesParselet::parseInfix(Token TokIn, ParserContext Ctxt) const {
    
    assert(false);
    
    return;
}


Precedence InfixImplicitTimesParselet::getPrecedence(ParserContext Ctxt) const {
    
    assert(false && "The last token may not have been added to InfixParselets");
    
    return PRECEDENCE_ASSERTFALSE;
}


Token InfixImplicitTimesParselet::processImplicitTimes(Token TokIn, ParserContext Ctxt) const {
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
}


void InfixAssertFalseParselet::parseInfix(Token firstTok, ParserContext Ctxt) const {
    
    assert(false);
    
    return;
}


void BinaryOperatorParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;

    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
        
    return parse1(CtxtIn);
}

void BinaryOperatorParselet::parse1(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Right));
    
    auto L = NodePtr(new BinaryNode(Op, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void InfixOperatorParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
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
    
    prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
    return parse1(CtxtIn);
}

void InfixOperatorParselet::parse1(ParserContext CtxtIn) const {
    
    auto Operand = TheParser->popNode();
    
    auto& Args = TheParser->peekArgs();
    
    auto OperandLastToken = Operand->lastToken();
    
    Args.append(std::move(Operand));
    
    return parseLoop(OperandLastToken, CtxtIn);
}

void InfixOperatorParselet::parseLoop(Token OperandLastToken, ParserContext CtxtIn) const {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);

    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);

    {
        TriviaSeq Trivia1;

        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
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
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
        
        return TheParser->parseLoop(CtxtIn);
    }
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    
    if (Tok1.Tok != TOKEN_FAKE_IMPLICITTIMES) {

        TheParser->nextToken(Tok1);
    }
    
    Token Tok2;
    {
        TriviaSeq Trivia2;

        Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
    
    Token OperandLastToken2;
    {
        auto Operand = TheParser->popNode();
        
        OperandLastToken2 = Operand->lastToken();

        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return parseLoop(OperandLastToken2, CtxtIn);
}


void PostfixOperatorParselet::parseInfix(Token TokIn, ParserContext Ctxt) const {
    
    auto Args = TheParser->popArgs();
    
    TheParser->nextToken(TokIn);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    auto L = NodePtr(new PostfixNode(Op, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
    return TheParser->parseLoop(Ctxt);
}


GroupParselet::GroupParselet(TokenEnum Opener, const SymbolPtr& Op) : Op(Op), Closr(GroupOpenerToCloser(Opener)) {}

void GroupParselet::parsePrefix(Token firstTok, ParserContext CtxtIn) const {
    
    auto OpenerT = firstTok;
    
    TheParser->nextToken(firstTok);
    
    auto& Args = TheParser->pushArgs();
    Args.append(NodePtr(new LeafNode(OpenerT)));
    
    return parseLoop(CtxtIn);
}
 
void GroupParselet::parseLoop(ParserContext CtxtIn) const {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
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
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    if (TokenToCloser(Tok.Tok) == Closr) {
        
        //
        // Everything is good
        //
        
        TheParser->nextToken(Tok);
        
        Args.append(NodePtr(new LeafNode(Tok)));
        
        auto group = NodePtr(new GroupNode(Op, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(group));
        
//        MUSTTAIL
        return TheParser->parseLoop(CtxtIn);
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
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(group));
            
//            MUSTTAIL
            return TheParser->parseLoop(CtxtIn);
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
        
        prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
        
        {
            auto Error = TheParser->popNode();
            
            //
            // Always append here
            //
            Args.append(std::move(Error));
        }
        
        MUSTTAIL
        return parseLoop(CtxtIn);
    }

    if (Tok.Tok == TOKEN_ENDOFFILE) {

        //
        // Handle something like   { a EOF
        //
        
        auto group = NodePtr(new UnterminatedGroupNeedsReparseNode(Op, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(group));
        
//        MUSTTAIL
        return TheParser->parseLoop(CtxtIn);
    }

    //
    // Handle the expression
    //
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    {
        auto Operand = TheParser->popNode();
        
        //
        // Always append here
        //
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return parseLoop(CtxtIn);
}


void CallParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    
    GP->parsePrefix(TokIn, Ctxt);
    
    return parse1(CtxtIn);
}

void CallParselet::parse1(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Head = TheParser->popArgs();
    
    NodeSeq Args(1);
    Args.append(std::move(Right));
    
    auto L = NodePtr(new CallNode(std::move(Head), std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void TildeParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
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
    
    prefixParselets[FirstTok.Tok.value()]->parsePrefix(FirstTok, Ctxt);
    
    return parse1(CtxtIn);
}

void TildeParselet::parse1(ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LOWEST;
    Ctxt.Flag |= PARSER_INSIDE_TILDE;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    
    {
        auto Middle = TheParser->popNode();
        
        Args.append(std::move(Middle));
    }
    
    Token Tok1;
    {
        TriviaSeq Trivia2;
        
        Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok1 = TheParser->eatTrivia(Tok1, Ctxt, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
        
    if (Tok1.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Error));
        
        return;
    }
    
    Token Tok2;
    {
        TriviaSeq Trivia3;

        //
        // Reset back to "outside" precedence
        //
        Ctxt.Prec = PRECEDENCE_TILDE;
        Ctxt.Flag &= (~PARSER_INSIDE_TILDE);

        Args.append(NodePtr(new LeafNode(Tok1)));
            
        TheParser->nextToken(Tok1);

        Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia3);

        Args.appendSeq(std::move(Trivia3));
    }
        
    prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
    
    MUSTTAIL
    return parse2(CtxtIn);
}

void TildeParselet::parse2(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Right));
    
    auto L = NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void ColonParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    assert((CtxtIn.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Flag |= PARSER_INSIDE_COLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(CtxtIn);
}

void ColonParselet::parse1(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
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
        
        auto& PatSeq = TheParser->pushArgs();
        PatSeq.append(std::move(Pat));
        PatSeq.appendSeq(std::move(Trivia2));
        
        return parseInfixContextSensitive(Tok, Ctxt);
    }
    
    Trivia2.reset();
    
    TheParser->pushNode(std::move(Pat));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}

void ColonParselet::parseInfixContextSensitive(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
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
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
        
    return parse2(CtxtIn);
}

void ColonParselet::parse2(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Right));
    
    auto L = NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void SlashColonParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_SLASHCOLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(CtxtIn);
}

void SlashColonParselet::parse1(ParserContext Ctxt) const {
    
    auto Middle = TheParser->popNode();
    
    auto& Args = TheParser->peekArgs();
    
    Args.append(std::move(Middle));
        
    TriviaSeq Trivia2;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia2);
    
    Args.appendSeq(std::move(Trivia2));
        
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            return infixParselets[TOKEN_EQUAL.value()]->parseInfix(Tok, Ctxt);
        }
        case TOKEN_COLONEQUAL.value(): {
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            return infixParselets[TOKEN_COLONEQUAL.value()]->parseInfix(Tok, Ctxt);
        }
        default: {
            
            //
            // Anything other than:
            // a /: b = c
            // a /: b := c
            // a /: b =.
            //
            
            auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSET, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(Error));
            
            return;
        }
    }
}


EqualParselet::EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, SYMBOL_SET) {}

void EqualParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
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
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
        
        return TheParser->parseLoop(CtxtIn);
    }
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(CtxtIn);
}

void EqualParselet::parse1(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
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
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


ColonEqualParselet::ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}

void ColonEqualParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(CtxtIn);
}

void ColonEqualParselet::parse1(ParserContext CtxtIn) const {
    
    auto Right = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
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
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void IntegralParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    
    auto& Args = TheParser->pushArgs();
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    Args.appendSeq(std::move(Trivia1));
    
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
    return parse1(CtxtIn);
}

void IntegralParselet::parse1(ParserContext CtxtIn) const {
    
    auto operand = TheParser->popNode();
    
    auto& Args = TheParser->peekArgs();
    
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
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
        
        return TheParser->parseLoop(CtxtIn);
    }
        
    prefixParselets[Tok.Tok.value()]->parsePrefix(Tok, Ctxt);
    
//    MUSTTAIL
    return parse2(CtxtIn);
}

void IntegralParselet::parse2(ParserContext CtxtIn) const {
    
    auto variable = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(variable));
    
    auto L = NodePtr(new PrefixBinaryNode(SYMBOL_INTEGRATE, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void CommaParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
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
        
        return parseLoop(CtxtIn);
    }
    
    prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
    return parse1(CtxtIn);
}

void CommaParselet::parse1(ParserContext CtxtIn) const {
    
    {
        auto Operand = TheParser->popNode();
        
        auto& Args = TheParser->peekArgs();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return parseLoop(CtxtIn);
}

void CommaParselet::parseLoop(ParserContext CtxtIn) const {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);
    
    Token Tok1;
    {
        TriviaSeq Trivia1;

        Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    //
    // Cannot just compare tokens
    //
    // May be something like  a,b\[InvisibleComma]c
    //
    // and we want only a single Infix node created
    //
    if (infixParselets[Tok1.Tok.value()]->getOp() != SYMBOL_CODEPARSER_COMMA) {
        
        auto L = NodePtr(new InfixNode(SYMBOL_CODEPARSER_COMMA, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
        
//        MUSTTAIL
        return TheParser->parseLoop(CtxtIn);
    }

    //
    // Something like  a,b
    //
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    
    TheParser->nextToken(Tok1);
    
    Token Tok2;
    {
        TriviaSeq Trivia2;

        Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
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
        
        {
            auto Implicit = NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
            
            Args.append(std::move(Implicit));
        }
        
        MUSTTAIL
        return parseLoop(CtxtIn);
    }
    
    prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
    
    {
        auto Operand = TheParser->popNode();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return parseLoop(CtxtIn);
}

const SymbolPtr& CommaParselet::getOp() const {
    return SYMBOL_CODEPARSER_COMMA;
}


void SemiParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto& Args = TheParser->peekArgs();
    
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
        
        return parseLoop(CtxtIn);
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
            
        return parse1(CtxtIn);
    }
        
    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //
    
    auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
    
    Args.append(NodePtr(new LeafNode(Implicit)));
    
    auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
    
    TheParser->popArgs();
    
    TheParser->pushNode(std::move(L));
    
    return TheParser->parseLoop(CtxtIn);
}

void SemiParselet::parse1(ParserContext CtxtIn) const {
    
    {
        auto operand = TheParser->popNode();
        
        auto& Args = TheParser->peekArgs();
        
        Args.append(std::move(operand));
    }
    
    MUSTTAIL
    return parseLoop(CtxtIn);
}

void SemiParselet::parseLoop(ParserContext CtxtIn) const {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence(Ctxt);

    Token Tok1;
    {
        TriviaSeq Trivia1;

        Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    if (Tok1.Tok != TOKEN_SEMI) {
        
        auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
        
//        MUSTTAIL
        return TheParser->parseLoop(CtxtIn);
    }

    //
    // Something like  a;b
    //

    TheParser->nextToken(Tok1);
    
    Token Tok2;
    {
        TriviaSeq Trivia2;

        Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    Args.append(NodePtr(new LeafNode(Tok1)));
    
    if (Tok2.Tok == TOKEN_SEMI) {

        //
        // Something like  a; ;
        //

        auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
        
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        MUSTTAIL
        return parseLoop(CtxtIn);
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        prefixParselets[Tok2.Tok.value()]->parsePrefix(Tok2, Ctxt);
        
        {
            auto operand = TheParser->popNode();
            
            Args.append(std::move(operand));
        }
        
        MUSTTAIL
        return parseLoop(CtxtIn);
    }

    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //

    auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
    
    Args.append(NodePtr(new LeafNode(Implicit)));

    auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}


void ColonColonParselet::parseInfix(Token TokIn, ParserContext Ctxt) const {
    
    auto& Args = TheParser->peekArgs();
    
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
    
    return parseLoop(Ctxt);
}

void ColonColonParselet::parseLoop(ParserContext Ctxt) const {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);

    if (Tok1.Tok != TOKEN_COLONCOLON) {
        
        auto L = NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
        
//        MUSTTAIL
        return TheParser->parseLoop(Ctxt);
    }

    TheParser->nextToken(Tok1);

    //
    // Special tokenization, so must do parsing here
    //

    auto Tok2 = TheParser->currentToken_stringifyAsTag();

    TheParser->nextToken(Tok2);

    {
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
    }
    
    MUSTTAIL
    return parseLoop(Ctxt);
}


void GreaterGreaterParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Args = TheParser->popArgs();
    
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
    
    TheParser->pushNode(std::move(L));
    
    return TheParser->parseLoop(CtxtIn);
}


void GreaterGreaterGreaterParselet::parseInfix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Args = TheParser->popArgs();
    
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
    
    TheParser->pushNode(std::move(L));
    
    return TheParser->parseLoop(CtxtIn);
}


void LessLessParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
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
    
    TheParser->pushNode(std::move(L));
    
    return TheParser->parseLoop(CtxtIn);
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


void HashParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
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
    
    TheParser->pushNode(std::move(Slot));
    
    return TheParser->parseLoop(CtxtIn);
}


void HashHashParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
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
    
    TheParser->pushNode(std::move(SlotSequence));
    
    return TheParser->parseLoop(CtxtIn);
}


void PercentParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
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
    
    TheParser->pushNode(std::move(Out));
    
    return TheParser->parseLoop(CtxtIn);
}

void PercentPercentParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    TheParser->nextToken(TokIn);

    auto Out = NodePtr(new LeafNode(TokIn));
    
    TheParser->pushNode(std::move(Out));
    
    return TheParser->parseLoop(CtxtIn);
}
