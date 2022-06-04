
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h" // for infixParselets, etc.
#include "Symbol.h"


ParseFunction LeafParselet::parsePrefix() const {
    return LeafParselet_parsePrefix;
}

void LeafParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    TheParser->nextToken(TokIn);
    
    auto Left = NodePtr(new LeafNode(TokIn));
    
    TheParser->pushNode(std::move(Left));
    
    return Parser_parseLoop(nullptr, Ctxt);
}


ParseFunction PrefixErrorParselet::parsePrefix() const {
    return PrefixErrorParselet_parsePrefix;
}

void PrefixErrorParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
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


ParseFunction PrefixCloserParselet::parsePrefix() const {
    return PrefixCloserParselet_parsePrefix;
}

void PrefixCloserParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
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


ParseFunction PrefixToplevelCloserParselet::parsePrefix() const {
    return PrefixToplevelCloserParselet_parsePrefix;
}

void PrefixToplevelCloserParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    assert(TokIn.Tok.isCloser());
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    TheParser->nextToken(TokIn);
    
    auto Error = NodePtr(new ErrorNode(Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.BufLen, TokIn.Src)));
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


ParseFunction PrefixEndOfFileParselet::parsePrefix() const {
    return PrefixEndOfFileParselet_parsePrefix;
}

void PrefixEndOfFileParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
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


ParseFunction PrefixUnsupportedTokenParselet::parsePrefix() const {
    return PrefixUnsupportedTokenParselet_parsePrefix;
}

void PrefixUnsupportedTokenParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    TheParser->nextToken(TokIn);
    
    auto createdToken = Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.BufLen, TokIn.Src);
    
    auto Error = NodePtr(new ErrorNode(createdToken));
    
    TheParser->pushNode(std::move(Error));
    
    return;
}


ParseFunction PrefixCommaParselet::parsePrefix() const {
    return PrefixCommaParselet_parsePrefix;
}

void PrefixCommaParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_ERROR_PREFIXIMPLICITNULL
    //
    if (Ctxt.Prec == PRECEDENCE_LOWEST) {
        
        auto createdToken = Token(TOKEN_ERROR_PREFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
        auto Left = NodePtr(new ErrorNode(createdToken));
        
        TheParser->pushNode(std::move(Left));
        
        return Parser_parseLoop(nullptr, Ctxt);
    }
        
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    auto Left = NodePtr(new ErrorNode(createdToken));
    
    TheParser->pushNode(std::move(Left));
    
    return;
}


ParseFunction PrefixUnhandledParselet::parsePrefix() const {
    return PrefixUnhandledParselet_parsePrefix;
}

void PrefixUnhandledParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    assert(!TokIn.Tok.isPossibleBeginning() && "handle at call site");
    
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON | PARSER_INSIDE_TILDE);
    
    {
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
    }

    auto P2 = infixParselets[TokIn.Tok.value()];
    
    MUSTTAIL
    return (P2->parseInfix())(P2, TokIn, Ctxt);
}


ParseFunction InfixToplevelNewlineParselet::parseInfix() const {
    return InfixToplevelNewlineParselet_parseInfix;
}

void InfixToplevelNewlineParselet_parseInfix(ParseletPtr P, Token firstTok, ParserContext Ctxt) {
    
    assert(false);
    
    return;
}


ParseFunction SymbolParselet::parsePrefix() const {
    return SymbolParselet_parsePrefix;
}

void SymbolParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            {
                auto Sym = NodePtr(new LeafNode(TokIn));
                
                auto& Args = TheParser->pushArgs();
                Args.append(std::move(Sym));
            }
            
            MUSTTAIL
            return (under1Parselet->parseInfixContextSensitive())(under1Parselet, Tok, Ctxt);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            {
                auto Sym = NodePtr(new LeafNode(TokIn));
                
                auto& Args = TheParser->pushArgs();
                Args.append(std::move(Sym));
            }
            
            MUSTTAIL
            return (under2Parselet->parseInfixContextSensitive())(under2Parselet, Tok, Ctxt);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            {
                auto Sym = NodePtr(new LeafNode(TokIn));
                
                auto& Args = TheParser->pushArgs();
                Args.append(std::move(Sym));
            }
            
            MUSTTAIL
            return (under3Parselet->parseInfixContextSensitive())(under3Parselet, Tok, Ctxt);
        }
        case TOKEN_UNDERDOT.value(): {
            
            {
                auto Sym = NodePtr(new LeafNode(TokIn));
                
                auto& Args = TheParser->pushArgs();
                Args.append(std::move(Sym));
            }
            
            MUSTTAIL
            return (underDotParselet->parseInfixContextSensitive())(underDotParselet, Tok, Ctxt);
        }
        default: {
            
            {
                auto Sym = NodePtr(new LeafNode(TokIn));
                
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
                
                if (Tok.Tok != TOKEN_COLON) {
                    
                    Trivia1.reset();
                    
                    TheParser->pushNode(std::move(Sym));
                    
                    return Parser_parseLoop(nullptr, Ctxt);
                }
                
                if ((Ctxt.Flag & PARSER_INSIDE_COLON) == PARSER_INSIDE_COLON) {
                    
                    Trivia1.reset();
                    
                    TheParser->pushNode(std::move(Sym));
                    
                    return Parser_parseLoop(nullptr, Ctxt);
                }
                
                auto& Args = TheParser->pushArgs();
                Args.append(std::move(Sym));
                Args.appendSeq(std::move(Trivia1));
            }
            
            MUSTTAIL
            return ColonParselet_parseInfix(colonParselet, Tok, Ctxt);
        }
    }
}


ParseFunction SymbolParselet::parsePrefixContextSensitive() const {
    return SymbolParselet_parsePrefixContextSensitive;
}

void SymbolParselet_parsePrefixContextSensitive(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    TheParser->pushNode(std::move(Sym));
    
    return;
}


const SymbolPtr& InfixParselet::getOp() const {
    return SYMBOL_CODEPARSER_INTERNALINVALID;
}


ParseFunction PrefixOperatorParselet::parsePrefix() const {
    return PrefixOperatorParselet_parsePrefix;
}

void PrefixOperatorParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<PrefixOperatorParselet *>(P)->getPrecedence(Ctxt);
    
    auto& Args = TheParser->pushArgs();
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return PrefixOperatorParselet_parse1(P, CtxtIn);
}

void PrefixOperatorParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Operand = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(std::move(Operand));
        
        auto& Op = dynamic_cast<PrefixOperatorParselet *>(P)->getOp();
        
        auto Left = NodePtr(new PrefixNode(Op, std::move(Args)));
        
        TheParser->pushNode(std::move(Left));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction InfixImplicitTimesParselet::parseInfix() const {
    return InfixImplicitTimesParselet_parseInfix;
}

void InfixImplicitTimesParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
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


ParseFunction InfixAssertFalseParselet::parseInfix() const {
    return InfixAssertFalseParselet_parseInfix;
}

void InfixAssertFalseParselet_parseInfix(ParseletPtr P, Token firstTok, ParserContext Ctxt) {
    
    assert(false);
    
    return;
}


ParseFunction BinaryOperatorParselet::parseInfix() const {
    return BinaryOperatorParselet_parseInfix;
}

void BinaryOperatorParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<BinaryOperatorParselet *>(P)->getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;

        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return BinaryOperatorParselet_parse1(P, CtxtIn);
}

void BinaryOperatorParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(std::move(Right));
        
        auto& Op = dynamic_cast<BinaryOperatorParselet *>(P)->getOp();
        
        auto L = NodePtr(new BinaryNode(Op, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction InfixOperatorParselet::parseInfix() const {
    return InfixOperatorParselet_parseInfix;
}

void InfixOperatorParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<InfixOperatorParselet *>(P)->getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    Token Tok2;
    {
        TriviaSeq Trivia2;
        
        Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2, Ctxt);
    
    return InfixOperatorParselet_parse1(P, CtxtIn);
}

void InfixOperatorParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Operand = TheParser->popNode();
        
        auto& Args = TheParser->peekArgs();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return InfixOperatorParselet_parseLoop(P, CtxtIn);
}

void InfixOperatorParselet_parseLoop(ParseletPtr P, ParserContext CtxtIn) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<InfixOperatorParselet *>(P)->getPrecedence(Ctxt);

    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);

    {
        TriviaSeq Trivia1;

        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto I = infixParselets[Tok1.Tok.value()];

    Tok1 = I->processImplicitTimes(Tok1, Ctxt);
    I = infixParselets[Tok1.Tok.value()];
    
    auto& Op = dynamic_cast<InfixOperatorParselet *>(P)->getOp();
    
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
        
        {
            auto L = NodePtr(new InfixNode(Op, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseLoop(nullptr, CtxtIn);
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
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2, Ctxt);
    
    {
        auto Operand = TheParser->popNode();

        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return InfixOperatorParselet_parseLoop(P, CtxtIn);
}


ParseFunction PostfixOperatorParselet::parseInfix() const {
    return PostfixOperatorParselet_parseInfix;
}

void PostfixOperatorParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    auto Args = TheParser->popArgs();
    
    TheParser->nextToken(TokIn);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    auto& Op = dynamic_cast<PostfixOperatorParselet *>(P)->getOp();
    
    auto L = NodePtr(new PostfixNode(Op, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
    return Parser_parseLoop(nullptr, Ctxt);
}


GroupParselet::GroupParselet(TokenEnum Opener, const SymbolPtr& Op) : Op(Op), Closr(GroupOpenerToCloser(Opener)) {}

ParseFunction GroupParselet::parsePrefix() const {
    return GroupParselet_parsePrefix;
}

void GroupParselet_parsePrefix(ParseletPtr P, Token OpenerT, ParserContext CtxtIn) {
    
    TheParser->nextToken(OpenerT);
    
    auto& Args = TheParser->pushArgs();
    Args.append(NodePtr(new LeafNode(OpenerT)));
    
    return GroupParselet_parseLoop(P, CtxtIn);
}
 
void GroupParselet_parseLoop(ParseletPtr P, ParserContext CtxtIn) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    
    auto Closr = dynamic_cast<GroupParselet *>(P)->getCloser();
    
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
        
        auto& Op = dynamic_cast<GroupParselet *>(P)->getOp();
        
        {
            auto group = NodePtr(new GroupNode(Op, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(group));
        }
        
        MUSTTAIL
        return Parser_parseLoop(nullptr, CtxtIn);
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
            
            auto& Op = dynamic_cast<GroupParselet *>(P)->getOp();
            
            {
                auto group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
                
                TheParser->popArgs();
                
                TheParser->pushNode(std::move(group));
            }
            
            MUSTTAIL
            return Parser_parseLoop(nullptr, CtxtIn);
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
        
        auto P2 = prefixParselets[Tok.Tok.value()];
        
        (P2->parsePrefix())(P2, Tok, Ctxt);
        
        {
            auto Error = TheParser->popNode();
            
            //
            // Always append here
            //
            Args.append(std::move(Error));
        }
        
        MUSTTAIL
        return GroupParselet_parseLoop(P, CtxtIn);
    }

    if (Tok.Tok == TOKEN_ENDOFFILE) {

        //
        // Handle something like   { a EOF
        //
        
        auto& Op = dynamic_cast<GroupParselet *>(P)->getOp();
        
        {
            auto group = NodePtr(new UnterminatedGroupNeedsReparseNode(Op, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(group));
        }
        
        MUSTTAIL
        return Parser_parseLoop(nullptr, CtxtIn);
    }

    //
    // Handle the expression
    //
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    {
        auto Operand = TheParser->popNode();
        
        //
        // Always append here
        //
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return GroupParselet_parseLoop(P, CtxtIn);
}


ParseFunction CallParselet::parseInfix() const {
    return CallParselet_parseInfix;
}

void CallParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    
    const auto& GP = dynamic_cast<CallParselet *>(P)->getGP();
    
    (GP->parsePrefix())(GP, TokIn, Ctxt);
    
    return CallParselet_parse1(P, CtxtIn);
}

void CallParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Head = TheParser->popArgs();
        
        NodeSeq Args(1);
        Args.append(std::move(Right));
        
        auto L = NodePtr(new CallNode(std::move(Head), std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction TildeParselet::parseInfix() const {
    return TildeParselet_parseInfix;
}

void TildeParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token FirstTok;
    {
        TriviaSeq Trivia1;
        
        FirstTok = TheParser->currentToken(Ctxt, TOPLEVEL);
        FirstTok = TheParser->eatTrivia(FirstTok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    Ctxt.Flag |= PARSER_INSIDE_TILDE;
    //
    // FIXME: clear other flags here also?
    //
    Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
    
    auto P2 = prefixParselets[FirstTok.Tok.value()];
    
    (P2->parsePrefix())(P2, FirstTok, Ctxt);
    
    return TildeParselet_parse1(P, CtxtIn);
}

void TildeParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
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
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2, Ctxt);
    
    MUSTTAIL
    return TildeParselet_parse2(P, CtxtIn);
}

void TildeParselet_parse2(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(std::move(Right));
        
        auto L = NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction ColonParselet::parseInfix() const {
    return ColonParselet_parseInfix;
}

void ColonParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    assert((CtxtIn.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON);
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Flag |= PARSER_INSIDE_COLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return ColonParselet_parse1(P, CtxtIn);
}

void ColonParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
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
        
        if (Tok.Tok != TOKEN_COLON) {
            
            Trivia2.reset();
            
            TheParser->pushNode(std::move(Pat));
            
        } else {
            
            Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
            
            auto& PatSeq = TheParser->pushArgs();
            PatSeq.append(std::move(Pat));
            PatSeq.appendSeq(std::move(Trivia2));
            
            ColonParselet_parseInfixContextSensitive(P, Tok, Ctxt);
        }
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction ColonParselet::parseInfixContextSensitive() const {
    return ColonParselet_parseInfixContextSensitive;
}

void ColonParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return ColonParselet_parse2(P, CtxtIn);
}

void ColonParselet_parse2(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(std::move(Right));
        
        auto L = NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction SlashColonParselet::parseInfix() const {
    return SlashColonParselet_parseInfix;
}

void SlashColonParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_SLASHCOLON;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return SlashColonParselet_parse1(P, CtxtIn);
}

void SlashColonParselet_parse1(ParseletPtr P, ParserContext Ctxt) {
    
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
            
            return EqualParselet_parseInfix(equalParselet, Tok, Ctxt);
        }
        case TOKEN_COLONEQUAL.value(): {
            
            Ctxt.Flag |= PARSER_INSIDE_SLASHCOLON;
            
            return ColonEqualParselet_parseInfix(colonEqualParselet, Tok, Ctxt);
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

ParseFunction EqualParselet::parseInfix() const {
    return EqualParselet_parseInfix;
}

void EqualParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
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
        
        return Parser_parseLoop(nullptr, CtxtIn);
    }
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return EqualParselet_parse1(P, CtxtIn);
}

void EqualParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        auto Ctxt = CtxtIn;
        Ctxt.Prec = PRECEDENCE_EQUAL;
        
        Args.append(std::move(Right));
        
        NodePtr L;
        
        if ((Ctxt.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            
            L = NodePtr(new TernaryNode(SYMBOL_TAGSET, std::move(Args)));
            
        } else {
            
            L = NodePtr(new BinaryNode(SYMBOL_SET, std::move(Args)));
        }
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ColonEqualParselet::ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}

ParseFunction ColonEqualParselet::parseInfix() const {
    return ColonEqualParselet_parseInfix;
}

void ColonEqualParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_EQUAL;
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    Ctxt.Flag &= ~(PARSER_INSIDE_SLASHCOLON);
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return ColonEqualParselet_parse1(P, CtxtIn);
}

void ColonEqualParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(std::move(Right));
        
        NodePtr L;
        
        if ((CtxtIn.Flag & PARSER_INSIDE_SLASHCOLON) == PARSER_INSIDE_SLASHCOLON) {
            
            L = NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
            
        } else {
            
            L = NodePtr(new BinaryNode(SYMBOL_SETDELAYED, std::move(Args)));
        }
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction IntegralParselet::parsePrefix() const {
    return IntegralParselet_parsePrefix;
}

void IntegralParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    Ctxt.Flag |= PARSER_INSIDE_INTEGRAL;
    
    auto& Args = TheParser->pushArgs();
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    return IntegralParselet_parse1(P, CtxtIn);
}

void IntegralParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    
    {
        auto operand = TheParser->popNode();
        
        Args.append(std::move(operand));
    }
    
    Token Tok;
    {
        TriviaSeq Trivia2;

        Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, Ctxt, TOPLEVEL, Trivia2);

        Args.appendSeq(std::move(Trivia2));
    }
    
    if (!Tok.Tok.isDifferentialD()) {
        
        {
            auto L = NodePtr(new PrefixNode(SYMBOL_INTEGRAL, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseLoop(nullptr, CtxtIn);
    }
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok, Ctxt);
    
    MUSTTAIL
    return IntegralParselet_parse2(P, CtxtIn);
}

void IntegralParselet_parse2(ParseletPtr P, ParserContext CtxtIn) {
    
    auto variable = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(variable));
    
    auto L = NodePtr(new PrefixBinaryNode(SYMBOL_INTEGRATE, std::move(Args)));
    
    TheParser->pushNode(std::move(L));
    
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction CommaParselet::parseInfix() const {
    return CommaParselet_parseInfix;
}

void CommaParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<CommaParselet *>(P)->getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
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
        
        return CommaParselet_parseLoop(P, CtxtIn);
    }
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2, Ctxt);
    
    return CommaParselet_parse1(P, CtxtIn);
}

void CommaParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto Operand = TheParser->popNode();
        
        auto& Args = TheParser->peekArgs();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return CommaParselet_parseLoop(P, CtxtIn);
}

void CommaParselet_parseLoop(ParseletPtr P, ParserContext CtxtIn) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<CommaParselet *>(P)->getPrecedence(Ctxt);
    
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
        
        {
            auto L = NodePtr(new InfixNode(SYMBOL_CODEPARSER_COMMA, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseLoop(nullptr, CtxtIn);
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
        return CommaParselet_parseLoop(P, CtxtIn);
    }
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2, Ctxt);
    
    {
        auto Operand = TheParser->popNode();
        
        Args.append(std::move(Operand));
    }
    
    MUSTTAIL
    return CommaParselet_parseLoop(P, CtxtIn);
}

const SymbolPtr& CommaParselet::getOp() const {
    return SYMBOL_CODEPARSER_COMMA;
}


ParseFunction SemiParselet::parseInfix() const {
    return SemiParselet_parseInfix;
}

void SemiParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = dynamic_cast<SemiParselet *>(P)->getPrecedence(Ctxt);
    
    Args.append(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    Token Tok2;
    {
        TriviaSeq Trivia2;
        
        Tok2 = TheParser->currentToken(Ctxt, TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, Ctxt, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (Tok2.Tok == TOKEN_SEMI) {
        
        //
        // Something like  a; ;
        //
        
        auto Implicit = Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start);
        
        Args.append(NodePtr(new LeafNode(Implicit)));
        
        return SemiParselet_parseLoop(P, CtxtIn);
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        (P2->parsePrefix())(P2, Tok2, Ctxt);
        
        return SemiParselet_parse1(P, CtxtIn);
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
}

void SemiParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    {
        auto operand = TheParser->popNode();
        
        auto& Args = TheParser->peekArgs();
        
        Args.append(std::move(operand));
    }
    
    MUSTTAIL
    return SemiParselet_parseLoop(P, CtxtIn);
}

void SemiParselet_parseLoop(ParseletPtr P, ParserContext CtxtIn) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    {
        auto& Args = TheParser->peekArgs();
        
        auto Ctxt = CtxtIn;
        Ctxt.Prec = dynamic_cast<SemiParselet *>(P)->getPrecedence(Ctxt);

        Token Tok1;
        {
            TriviaSeq Trivia1;

            Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);
            Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, Ctxt, TOPLEVEL, Trivia1);
            
            Args.appendSeq(std::move(Trivia1));
        }
        
        if (Tok1.Tok != TOKEN_SEMI) {
            
            {
                auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
                
                TheParser->popArgs();
                
                TheParser->pushNode(std::move(L));
            }
            
            MUSTTAIL
            return Parser_parseLoop(nullptr, CtxtIn);
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
            return SemiParselet_parseLoop(P, CtxtIn);
        }
        
        if (Tok2.Tok.isPossibleBeginning()) {
            
            auto P2 = prefixParselets[Tok2.Tok.value()];
            
            (P2->parsePrefix())(P2, Tok2, Ctxt);
            
            {
                auto operand = TheParser->popNode();
                
                Args.append(std::move(operand));
            }
            
            MUSTTAIL
            return SemiParselet_parseLoop(P, CtxtIn);
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
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction ColonColonParselet::parseInfix() const {
    return ColonColonParselet_parseInfix;
}

void ColonColonParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
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
        
        Args.append(NodePtr(new LeafNode(TokIn)));
        Args.append(std::move(Operand));
    }
    
    return ColonColonParselet_parseLoop(P, Ctxt);
}

void ColonColonParselet_parseLoop(ParseletPtr P, ParserContext Ctxt) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    auto Tok1 = TheParser->currentToken(Ctxt, TOPLEVEL);

    if (Tok1.Tok != TOKEN_COLONCOLON) {
        
        {
            auto L = NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseLoop(nullptr, Ctxt);
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
    return ColonColonParselet_parseLoop(P, Ctxt);
}


ParseFunction GreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterParselet_parseInfix;
}

void GreaterGreaterParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction GreaterGreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterGreaterParselet_parseInfix;
}

void GreaterGreaterGreaterParselet_parseInfix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction LessLessParselet::parsePrefix() const {
    return LessLessParselet_parsePrefix;
}

void LessLessParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
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


ParseFunction HashParselet::parsePrefix() const {
    return HashParselet_parsePrefix;
}

void HashParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction HashHashParselet::parsePrefix() const {
    return HashHashParselet_parsePrefix;
}

void HashHashParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction PercentParselet::parsePrefix() const {
    return PercentParselet_parsePrefix;
}

void PercentParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
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
    
    return Parser_parseLoop(nullptr, CtxtIn);
}


ParseFunction PercentPercentParselet::parsePrefix() const {
    return PercentPercentParselet_parsePrefix;
}

void PercentPercentParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    TheParser->nextToken(TokIn);

    auto Out = NodePtr(new LeafNode(TokIn));
    
    TheParser->pushNode(std::move(Out));
    
    return Parser_parseLoop(nullptr, CtxtIn);
}
