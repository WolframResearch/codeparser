
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h" // for infixParselets, etc.
#include "Symbol.h"


ParseFunction LeafParselet::parsePrefix() const {
    return LeafParselet_parsePrefix;
}

void LeafParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        auto Left = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Left));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction PrefixErrorParselet::parsePrefix() const {
    return PrefixErrorParselet_parsePrefix;
}

void PrefixErrorParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(TokIn.Tok.isError());
    
    TheParser->nextToken(TokIn);
    
    {
        NodePtr Error;
        
        if (TokIn.Tok.isUnterminated()) {
            
            Error = NodePtr(new UnterminatedTokenErrorNeedsReparseNode(TokIn));
            
        } else {
            
            Error = NodePtr(new ErrorNode(TokIn));
        }
        
        TheParser->pushNode(std::move(Error));
    }
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Token());
}


ParseFunction PrefixCloserParselet::parsePrefix() const {
    return PrefixCloserParselet_parsePrefix;
}

void PrefixCloserParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(TokIn.Tok.isCloser());
        
    //
    // Inside some other parselet that is not GroupParselet
    //
    //
    
    Token createdToken;
    
    if (TheParser->topPrecedence() == PRECEDENCE_COMMA) {
        
        createdToken = Token(TOKEN_ERROR_INFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    }
    
    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    TheParser->currentToken(TOPLEVEL);
    
    {
        auto Error = NodePtr(new ErrorNode(createdToken));
        
        TheParser->pushNode(std::move(Error));
    }
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Token());
}


ParseFunction PrefixToplevelCloserParselet::parsePrefix() const {
    return PrefixToplevelCloserParselet_parsePrefix;
}

void PrefixToplevelCloserParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(TokIn.Tok.isCloser());
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    TheParser->nextToken(TokIn);
    
    auto Error = NodePtr(new ErrorNode(Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.BufLen, TokIn.Src)));
    
    TheParser->pushNode(std::move(Error));
    
    // no call needed here
    return;
}


ParseFunction PrefixEndOfFileParselet::parsePrefix() const {
    return PrefixEndOfFileParselet_parsePrefix;
}

void PrefixEndOfFileParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // Something like  a+<EOF>
    //
    
    Token createdToken;
    
    if (TheParser->topPrecedence() == PRECEDENCE_COMMA) {
            
        createdToken = Token(TOKEN_ERROR_INFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    }
    
    {
        auto Error = NodePtr(new ErrorNode(createdToken));
        
        TheParser->pushNode(std::move(Error));
    }
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Token());
}


ParseFunction PrefixUnsupportedTokenParselet::parsePrefix() const {
    return PrefixUnsupportedTokenParselet_parsePrefix;
}

void PrefixUnsupportedTokenParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    TheParser->nextToken(TokIn);
    
    auto createdToken = Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.BufLen, TokIn.Src);
    
    {
        auto Error = NodePtr(new ErrorNode(createdToken));
        
        TheParser->pushNode(std::move(Error));
    }
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Token());
}


ParseFunction PrefixCommaParselet::parsePrefix() const {
    return PrefixCommaParselet_parsePrefix;
}

void PrefixCommaParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_ERROR_PREFIXIMPLICITNULL
    //
    if (TheParser->topPrecedence() == PRECEDENCE_LOWEST) {
        
        auto createdToken = Token(TOKEN_ERROR_PREFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
        {
            auto Left = NodePtr(new ErrorNode(createdToken));
            
            TheParser->pushNode(std::move(Left));
        }
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Token());
    }
        
    auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    
    {
        auto Left = NodePtr(new ErrorNode(createdToken));
        
        TheParser->pushNode(std::move(Left));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction PrefixUnhandledParselet::parsePrefix() const {
    return PrefixUnhandledParselet_parsePrefix;
}

void PrefixUnhandledParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(!TokIn.Tok.isPossibleBeginning() && "handle at call site");
    
    {
        {
            auto NotPossible = NodePtr(new ErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start)));
            
            TheParser->pushNode(std::move(NotPossible));
        }
        
        //
        // Do not take next token
        //
        TheParser->currentToken(TOPLEVEL);
        
        auto I = infixParselets[TokIn.Tok.value()];
        
        auto TokenPrecedence = I->getPrecedence();
        
        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   goto prefixUnhandledParseletRet;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   goto prefixUnhandledParseletRet;
        //
        if ((TheParser->topPrecedence() | 0x1) > TokenPrecedence) {
            
            //
            // Something like  a + | 2
            //
            // Make sure that the error leaf is with the + and not the |
            //
            
            MUSTTAIL
            return Parser_tryContinue(nullptr, Token());
        }
        
        //
        // Handle something like  f[@2]
        //
        // We want to make EXPECTEDOPERAND the first arg of the Operator node.
        //
        
        TheParser->pushArgs(nullptr, nullptr);
        
        TheParser->shift();
    }

    auto P2 = infixParselets[TokIn.Tok.value()];
    
    MUSTTAIL
    return (P2->parseInfix())(P2, TokIn);
}


ParseFunction InfixToplevelNewlineParselet::parseInfix() const {
    return InfixToplevelNewlineParselet_parseInfix;
}

void InfixToplevelNewlineParselet_parseInfix(ParseletPtr P, Token firstTok) {
    
    assert(false);
    
    return;
}


ParseFunction SymbolParselet::parsePrefix() const {
    return SymbolParselet_parsePrefix;
}

void SymbolParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        auto Sym = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Sym));
    }
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            //
            // Something like  a_
            //
            
            TheParser->pushArgs(nullptr, nullptr);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(under1Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_parsePatternBlank(under1Parselet, Tok);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            //
            // Something like  a__
            //
            
            TheParser->pushArgs(nullptr, nullptr);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(under2Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_parsePatternBlank(under2Parselet, Tok);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            //
            // Something like  a___
            //
            
            TheParser->pushArgs(nullptr, nullptr);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(under3Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_parsePatternBlank(under3Parselet, Tok);
        }
        case TOKEN_UNDERDOT.value(): {
            
            //
            // Something like  a_.
            //
            
            TheParser->pushArgs(nullptr, nullptr);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderDotParselet_parseInfixContextSensitive(underDotParselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_parsePatternOptionalDefault(underDotParselet, Tok);
        }
        default: {
            
            //
            // Something like  a
            //
            
            MUSTTAIL
            return Parser_parseClimb(nullptr, Token());
        }
    }
}

void SymbolParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    //
    // Something like  _b
    //                  ^
    //
    
    //
    // We know we are already in the middle of parsing _
    //
    // Just push this symbol
    //
    
    auto Sym = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    TheParser->pushNode(std::move(Sym));
    
    // no call needed here
    return;
}

void SymbolParselet_parsePatternBlank(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        auto& PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
        
        auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Pat));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void SymbolParselet_parsePatternOptionalDefault(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Pat));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


const Symbol& InfixParselet::getOp() const {
    return SYMBOL_CODEPARSER_INTERNALINVALID;
}


ParseFunction PrefixOperatorParselet::parsePrefix() const {
    return PrefixOperatorParselet_parsePrefix;
}

void PrefixOperatorParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    auto& Args = TheParser->pushArgs(nullptr, nullptr);
    
    TheParser->shift();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(dynamic_cast<PrefixOperatorParselet *>(P)->getPrecedence());
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = PrefixOperatorParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void PrefixOperatorParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto& Op = dynamic_cast<PrefixOperatorParselet *>(P)->getOp();
        
        auto Left = NodePtr(new PrefixNode(Op, std::move(Args)));
        
        TheParser->pushNode(std::move(Left));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction InfixImplicitTimesParselet::parseInfix() const {
    return InfixImplicitTimesParselet_parseInfix;
}

void InfixImplicitTimesParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    assert(false);
    
    return;
}


Precedence InfixImplicitTimesParselet::getPrecedence() const {
    
    assert(false && "The last token may not have been added to InfixParselets");
    
    return PRECEDENCE_ASSERTFALSE;
}


Token InfixImplicitTimesParselet::processImplicitTimes(Token TokIn) const {
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.BufLen.buffer, TokIn.Src.Start);
}


ParseFunction InfixAssertFalseParselet::parseInfix() const {
    return InfixAssertFalseParselet_parseInfix;
}

void InfixAssertFalseParselet_parseInfix(ParseletPtr P, Token firstTok) {
    
    assert(false);
    
    return;
}


ParseFunction BinaryOperatorParselet::parseInfix() const {
    return BinaryOperatorParselet_parseInfix;
}

void BinaryOperatorParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    Token Tok;
    {
        TriviaSeq Trivia1;

        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(dynamic_cast<BinaryOperatorParselet *>(P)->getPrecedence());
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = BinaryOperatorParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void BinaryOperatorParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto& Op = dynamic_cast<BinaryOperatorParselet *>(P)->getOp();
        
        auto L = NodePtr(new BinaryNode(Op, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction InfixOperatorParselet::parseInfix() const {
    return InfixOperatorParselet_parseInfix;
}

void InfixOperatorParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    Token Tok2;
    {
        TriviaSeq Trivia2;
        
        Tok2 = TheParser->currentToken(TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    TheParser->pushPrecedence(dynamic_cast<InfixOperatorParselet *>(P)->getPrecedence());
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = InfixOperatorParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
}

void InfixOperatorParselet_parse1(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    MUSTTAIL
    return InfixOperatorParselet_parseLoop(P, Ignored);
}

void InfixOperatorParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();

    Token Tok1;

    {
        TriviaSeq Trivia1;
        
        Tok1 = TheParser->currentToken(TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    auto I = infixParselets[Tok1.Tok.value()];

    Tok1 = I->processImplicitTimes(Tok1);
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
        
        TheParser->popPrecedence();
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
    }
    
    TheParser->pushNode(NodePtr(new LeafNode(Tok1)));
    
    TheParser->nextToken(Tok1);
    
    TheParser->shift();
    
    Token Tok2;
    {
        TriviaSeq Trivia2;

        Tok2 = TheParser->currentToken(TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
}


ParseFunction PostfixOperatorParselet::parseInfix() const {
    return PostfixOperatorParselet_parseInfix;
}

void PostfixOperatorParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    {
        TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
        
        TheParser->nextToken(TokIn);
        
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto& Op = dynamic_cast<PostfixOperatorParselet *>(P)->getOp();
        
        auto L = NodePtr(new PostfixNode(Op, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


GroupParselet::GroupParselet(TokenEnum Opener, const Symbol& Op) : Op(Op), Closr(GroupOpenerToCloser(Opener)) {}

ParseFunction GroupParselet::parsePrefix() const {
    return GroupParselet_parsePrefix;
}

void GroupParselet_parsePrefix(ParseletPtr P, Token OpenerT) {
    
    TheParser->pushNode(NodePtr(new LeafNode(OpenerT)));
    
    TheParser->nextToken(OpenerT);
    
    TheParser->pushPrecedence(PRECEDENCE_LOWEST);
    TheParser->pushGroup(GroupOpenerToCloser(OpenerT.Tok));
    
    auto& Args = TheParser->pushArgs(nullptr, nullptr);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = GroupParselet_parse1;
    Args.P = P;
    
    MUSTTAIL
    return GroupParselet_parse1(P, Token());
}

void GroupParselet_parse1(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    MUSTTAIL
    return GroupParselet_parseLoop(P, Ignored);
}

void GroupParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto Closr = dynamic_cast<GroupParselet *>(P)->getCloser();
    
    auto& Args = TheParser->peekArgs();
    
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
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    if (TokenToCloser(Tok.Tok) == Closr) {
        
        //
        // Everything is good
        //
        
        TheParser->pushNode(NodePtr(new LeafNode(Tok)));
        
        TheParser->nextToken(Tok);
        
        TheParser->shift();
        
        auto& Op = dynamic_cast<GroupParselet *>(P)->getOp();
        
        {
            auto group = NodePtr(new GroupNode(Op, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(group));
        }
        
        TheParser->popPrecedence();
        TheParser->popGroup();
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
    }
    
    if (Tok.Tok.isCloser()) {

        //
        // some other closer
        //
        
        if (TheParser->checkGroup(TokenToCloser(Tok.Tok))) {
            
            //
            // Something like  { ( }
            //                     ^
            //
            
            //
            // Do not consume the bad closer now
            //
            
            auto& Op = dynamic_cast<GroupParselet *>(P)->getOp();
            
            {
                auto group = NodePtr(new GroupMissingCloserNode(Op, std::move(Args)));
                
                TheParser->popArgs();
                
                TheParser->pushNode(std::move(group));
            }
            
            TheParser->popPrecedence();
            TheParser->popGroup();
            
            MUSTTAIL
            return Parser_parseClimb(nullptr, Ignored);
        }
            
        //
        // Something like  { ) }
        //                   ^
        //
        
        PrefixToplevelCloserParselet_parsePrefix(prefixToplevelCloserParselet, Tok);
        
        MUSTTAIL
        return GroupParselet_parse1(P, Ignored);
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
        
        TheParser->popPrecedence();
        TheParser->popGroup();
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
    }

    //
    // Handle the expression
    //
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}


ParseFunction CallParselet::parseInfix() const {
    return CallParselet_parseInfix;
}

void CallParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    TheParser->pushPrecedence(PRECEDENCE_HIGHEST);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = CallParselet_parse1;
    Args.P = P;
    
    const auto& GP = dynamic_cast<CallParselet *>(P)->getGP();
    
    MUSTTAIL
    return (GP->parsePrefix())(GP, TokIn);
}

void CallParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        auto Right = TheParser->popNode();
        
        auto Head = TheParser->popArgs();
        
        NodeSeq Args;
        Args.append(std::move(Right));
        
        auto L = NodePtr(new CallNode(std::move(Head), std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction TildeParselet::parseInfix() const {
    return TildeParselet_parseInfix;
}

Precedence TildeParselet::getPrecedence() const {
    
    if (TheParser->getArgsStackSize() == 0) {
        return PRECEDENCE_TILDE;
    }
    
    auto& Args = TheParser->peekArgs();
    
    auto alreadyInsideTilde = Args.checkTilde();
    
    if (alreadyInsideTilde) {
        return PRECEDENCE_LOWEST;
    }
    
    return PRECEDENCE_TILDE;
}

void TildeParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    Token FirstTok;
    {
        TriviaSeq Trivia1;
        
        FirstTok = TheParser->currentToken(TOPLEVEL);
        FirstTok = TheParser->eatTrivia(FirstTok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(PRECEDENCE_LOWEST);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = TildeParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[FirstTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, FirstTok);
}

void TildeParselet_parse1(ParseletPtr P, Token Ignored) {
    
    auto& Args = TheParser->peekArgs();
    
    TheParser->shift();
    
    Token Tok1;
    {
        TriviaSeq Trivia2;
        
        Tok1 = TheParser->currentToken(TOPLEVEL);
        Tok1 = TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
        
    if (Tok1.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        TheParser->popPrecedence();
        
        {
            auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDTILDE, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(Error));
        }
        
        MUSTTAIL
        return Parser_tryContinue(nullptr, Ignored);
    }
    
    TheParser->pushNode(NodePtr(new LeafNode(Tok1)));
    
    TheParser->nextToken(Tok1);
    
    TheParser->shift();
    
    Token Tok2;
    {
        TriviaSeq Trivia3;

        Tok2 = TheParser->currentToken(TOPLEVEL);
        Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL, Trivia3);

        Args.appendSeq(std::move(Trivia3));
    }
    
    //
    // Reset back to "outside" precedence
    //
    TheParser->popPrecedence();
    TheParser->pushPrecedence(PRECEDENCE_TILDE);
    
    assert(Args.F == TildeParselet_parse1);
    assert(Args.P == P);
    Args.F = TildeParselet_parse2;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
}

void TildeParselet_parse2(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction ColonParselet::parseInfix() const {
    return ColonParselet_parseInfix;
}

Precedence ColonParselet::getPrecedence() const {
    
    if (TheParser->checkPatternPrecedence()) {
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
    
    return PRECEDENCE_HIGHEST;
}

void ColonParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    auto colonLHS = Args.checkColonLHS();
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    switch (colonLHS) {
        case COLONLHS_PATTERN: {
            
            TheParser->pushPrecedence(PRECEDENCE_FAKE_PATTERNCOLON);
            
            assert(Args.F == nullptr);
            assert(Args.P == nullptr);
            Args.F = ColonParselet_parsePattern;
            Args.P = P;
            
            auto P2 = prefixParselets[Tok.Tok.value()];
            
            MUSTTAIL
            return (P2->parsePrefix())(P2, Tok);
        }
        case COLONLHS_OPTIONAL: {

            TheParser->pushPrecedence(PRECEDENCE_FAKE_OPTIONALCOLON);
            
            assert(Args.F == nullptr);
            assert(Args.P == nullptr);
            Args.F = ColonParselet_parseOptional;
            Args.P = P;
            
            auto P2 = prefixParselets[Tok.Tok.value()];

            MUSTTAIL
            return (P2->parsePrefix())(P2, Tok);
        }
        case COLONLHS_ERROR: {
            
            TheParser->pushPrecedence(PRECEDENCE_FAKE_PATTERNCOLON);
            
            assert(Args.F == nullptr);
            assert(Args.P == nullptr);
            Args.F = ColonParselet_parseError;
            Args.P = P;
            
            auto P2 = prefixParselets[Tok.Tok.value()];
            
            MUSTTAIL
            return (P2->parsePrefix())(P2, Tok);
        }
        default: {
            assert(false);
            break;
        }
    }
}

void ColonParselet_parsePattern(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new BinaryNode(SYMBOL_PATTERN, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void ColonParselet_parseError(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSYMBOL, std::move(Args)));
        
        TheParser->pushNode(std::move(Error));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void ColonParselet_parseOptional(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new BinaryNode(SYMBOL_OPTIONAL, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction SlashColonParselet::parseInfix() const {
    return SlashColonParselet_parseInfix;
}

void SlashColonParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->peekArgs();
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(PRECEDENCE_SLASHCOLON);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = SlashColonParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void SlashColonParselet_parse1(ParseletPtr P, Token Ignored) {
    
    Token Tok;
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        TriviaSeq Trivia2;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    TheParser->popPrecedence();
    
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            
            MUSTTAIL
            return EqualParselet_parseInfixContextSensitive(equalParselet, Tok);
        }
        case TOKEN_COLONEQUAL.value(): {
            
            MUSTTAIL
            return ColonEqualParselet_parseInfixContextSensitive(colonEqualParselet, Tok);
        }
        default: {
            
            //
            // Anything other than:
            // a /: b = c
            // a /: b := c
            // a /: b =.
            //
            
            {
                auto Args = TheParser->popArgs();
                
                auto Error = NodePtr(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSET, std::move(Args)));
                
                TheParser->pushNode(std::move(Error));
            }
            
            MUSTTAIL
            return Parser_parseClimb(nullptr, Ignored);
        }
    }
}


EqualParselet::EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, SYMBOL_SET) {}

ParseFunction EqualParselet::parseInfix() const {
    return EqualParselet_parseInfix;
}

void EqualParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        TheParser->pushNode(NodePtr(new LeafNode(Tok)));
        
        TheParser->nextToken(Tok);
        
        TheParser->shift();
        
        {
            auto L = NodePtr(new BinaryNode(SYMBOL_UNSET, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Token());
    }
    
    TheParser->pushPrecedence(PRECEDENCE_EQUAL);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = EqualParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void EqualParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        TheParser->pushNode(NodePtr(new LeafNode(Tok)));
        
        TheParser->nextToken(Tok);
        
        TheParser->shift();
        
        {
            auto L = NodePtr(new TernaryNode(SYMBOL_TAGUNSET, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Token());
    }
    
    TheParser->pushPrecedence(PRECEDENCE_EQUAL);
    
    assert(Args.F == SlashColonParselet_parse1);
//    assert(Args.P == nullptr);
    Args.F = EqualParselet_parse2;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void EqualParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new BinaryNode(SYMBOL_SET, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void EqualParselet_parse2(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new TernaryNode(SYMBOL_TAGSET, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ColonEqualParselet::ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}

ParseFunction ColonEqualParselet::parseInfix() const {
    return ColonEqualParselet_parseInfix;
}

void ColonEqualParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(PRECEDENCE_COLONEQUAL);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = ColonEqualParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void ColonEqualParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(PRECEDENCE_COLONEQUAL);
    
    assert(Args.F == SlashColonParselet_parse1);
//    assert(Args.P == nullptr);
    Args.F = ColonEqualParselet_parse2;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void ColonEqualParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new BinaryNode(SYMBOL_SETDELAYED, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void ColonEqualParselet_parse2(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new TernaryNode(SYMBOL_TAGSETDELAYED, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction IntegralParselet::parsePrefix() const {
    return IntegralParselet_parsePrefix;
}

void IntegralParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    auto& Args = TheParser->pushArgs(nullptr, nullptr);
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    Token Tok;
    {
        TriviaSeq Trivia1;
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    TheParser->pushPrecedence(PRECEDENCE_CLASS_INTEGRATIONOPERATORS);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = IntegralParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void IntegralParselet_parse1(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok;
    {
        TriviaSeq Trivia2;

        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia2);

        Args.appendSeq(std::move(Trivia2));
    }
    
    if (!(Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.Tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD)) {
        
        {
            auto L = NodePtr(new PrefixNode(SYMBOL_INTEGRAL, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        TheParser->popPrecedence();
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
    }
    
    assert(Args.F == IntegralParselet_parse1);
    assert(Args.P == P);
    Args.F = IntegralParselet_parse2;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void IntegralParselet_parse2(ParseletPtr P, Token Ignored) {
    
    {
        TheParser->shift();
        
        auto Args = TheParser->popArgs();
        
        auto L = NodePtr(new PrefixBinaryNode(SYMBOL_INTEGRATE, std::move(Args)));
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction CommaParselet::parseInfix() const {
    return CommaParselet_parseInfix;
}

void CommaParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    Token Tok2;
    {
        TriviaSeq Trivia2;
        
        Tok2 = TheParser->currentToken(TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (Tok2.Tok == TOKEN_COMMA || Tok2.Tok == TOKEN_LONGNAME_INVISIBLECOMMA) {
        
        //
        // Something like  a,,
        //
        
        TheParser->pushNode(NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start))));
        
        TheParser->pushPrecedence(PRECEDENCE_COMMA);
        
        assert(Args.F == nullptr);
        assert(Args.P == nullptr);
        Args.F = CommaParselet_parse1;
        Args.P = P;
        
        MUSTTAIL
        return CommaParselet_parse1(P, Token());
    }
    
    TheParser->pushPrecedence(PRECEDENCE_COMMA);
    
    assert(Args.F == nullptr);
    assert(Args.P == nullptr);
    Args.F = CommaParselet_parse1;
    Args.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
}

void CommaParselet_parse1(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    MUSTTAIL
    return CommaParselet_parseLoop(P, Ignored);
}

void CommaParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok1;
    {
        TriviaSeq Trivia1;

        Tok1 = TheParser->currentToken(TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, TOPLEVEL, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
    }
    
    if (!(Tok1.Tok == TOKEN_COMMA || Tok1.Tok == TOKEN_LONGNAME_INVISIBLECOMMA)) {
        
        {
            auto L = NodePtr(new InfixNode(SYMBOL_CODEPARSER_COMMA, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        TheParser->popPrecedence();
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
    }

    //
    // Something like  a,b
    //
    
    TheParser->pushNode(NodePtr(new LeafNode(Tok1)));
    
    TheParser->nextToken(Tok1);
    
    TheParser->shift();
    
    Token Tok2;
    {
        TriviaSeq Trivia2;

        Tok2 = TheParser->currentToken(TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (Tok2.Tok == TOKEN_COMMA || Tok2.Tok == TOKEN_LONGNAME_INVISIBLECOMMA) {

        //
        // Something like  a,,
        //
        
        TheParser->pushNode(NodePtr(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start))));
        
        MUSTTAIL
        return CommaParselet_parse1(P, Ignored);
    }
    
//    assert(Args.F == nullptr);
//    assert(Args.P == nullptr);
//    Args.F = CommaParselet_parse1;
//    Args.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
}

const Symbol& CommaParselet::getOp() const {
    return SYMBOL_CODEPARSER_COMMA;
}


ParseFunction SemiParselet::parseInfix() const {
    return SemiParselet_parseInfix;
}

void SemiParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
    auto& Args = TheParser->peekArgs();
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    Token Tok2;
    {
        TriviaSeq Trivia2;
        
        Tok2 = TheParser->currentToken(TOPLEVEL);
        Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (Tok2.Tok == TOKEN_SEMI) {
        
        //
        // Something like  a; ;
        //
        
        TheParser->pushNode(NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start))));
        
        TheParser->pushPrecedence(PRECEDENCE_SEMI);
        
        assert(Args.F == nullptr);
        assert(Args.P == nullptr);
        Args.F = SemiParselet_parse1;
        Args.P = P;
        
        MUSTTAIL
        return SemiParselet_parse1(P, Token());
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        //
        // Something like  a;+2
        //
        
        TheParser->pushPrecedence(PRECEDENCE_SEMI);
        
        assert(Args.F == nullptr);
        assert(Args.P == nullptr);
        Args.F = SemiParselet_parse1;
        Args.P = P;
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        MUSTTAIL
        return (P2->parsePrefix())(P2, Tok2);
    }
        
    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //
    
    TheParser->pushNode(NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start))));
    
    TheParser->shift();
    
    {
        auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}

void SemiParselet_parse1(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    MUSTTAIL
    return SemiParselet_parseLoop(P, Ignored);
}

void SemiParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    {
        auto& Args = TheParser->peekArgs();

        Token Tok1;
        {
            TriviaSeq Trivia1;

            Tok1 = TheParser->currentToken(TOPLEVEL);
            Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, TOPLEVEL, Trivia1);
            
            Args.appendSeq(std::move(Trivia1));
        }
        
        if (Tok1.Tok != TOKEN_SEMI) {
            
            {
                auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
                
                TheParser->popArgs();
                
                TheParser->pushNode(std::move(L));
            }
            
            TheParser->popPrecedence();
            
            MUSTTAIL
            return Parser_parseClimb(nullptr, Ignored);
        }

        //
        // Something like  a;b
        //
        
        TheParser->pushNode(NodePtr(new LeafNode(Tok1)));
        
        TheParser->nextToken(Tok1);
        
        TheParser->shift();
        
        Token Tok2;
        {
            TriviaSeq Trivia2;

            Tok2 = TheParser->currentToken(TOPLEVEL);
            Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, TOPLEVEL, Trivia2);
            
            Args.appendSeq(std::move(Trivia2));
        }
        
        if (Tok2.Tok == TOKEN_SEMI) {

            //
            // Something like  a; ;
            //
            
            TheParser->pushNode(NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start))));
            
            TheParser->shift();
            
            MUSTTAIL
            return SemiParselet_parseLoop(P, Ignored);
        }
        
        if (Tok2.Tok.isPossibleBeginning()) {
            
//            assert(Args.F == nullptr);
//            assert(Args.P == nullptr);
//            Args.F = SemiParselet_parse1;
//            Args.P = P;
            
            auto P2 = prefixParselets[Tok2.Tok.value()];
            
            MUSTTAIL
            return (P2->parsePrefix())(P2, Tok2);
        }

        //
        // Not beginning of an expression
        //
        // For example:  a;&
        //
        
        TheParser->pushNode(NodePtr(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start))));
        
        TheParser->shift();
        
        auto L = NodePtr(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
    }
    
    TheParser->popPrecedence();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction ColonColonParselet::parseInfix() const {
    return ColonColonParselet_parseInfix;
}

void ColonColonParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    TheParser->shift();
    
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
        
        TheParser->pushNode(std::move(Operand));
        
        TheParser->shift();
    }
    
    MUSTTAIL
    return ColonColonParselet_parseLoop(P, Token());
}

void ColonColonParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Args = TheParser->peekArgs();
    
    Token Tok1;
    {
        TriviaSeq Trivia2;
        
        Tok1 = TheParser->currentToken(TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, TOPLEVEL, Trivia2);
        
        Args.appendSeq(std::move(Trivia2));
    }
    
    if (Tok1.Tok != TOKEN_COLONCOLON) {
        
        {
            auto L = NodePtr(new InfixNode(SYMBOL_MESSAGENAME, std::move(Args)));
            
            TheParser->popArgs();
            
            TheParser->pushNode(std::move(L));
        }
        
        MUSTTAIL
        return Parser_parseClimb(nullptr, Ignored);
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
        
        TheParser->pushNode(NodePtr(new LeafNode(Tok1)));
        
        TheParser->shift();
        
        TheParser->pushNode(std::move(Operand));
        
        TheParser->shift();
    }
    
    MUSTTAIL
    return ColonColonParselet_parseLoop(P, Ignored);
}


ParseFunction GreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterParselet_parseInfix;
}

void GreaterGreaterParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
        
        TheParser->nextToken(TokIn);
        
        TheParser->shift();
        
        TriviaSeq Trivia1;
        
        //
        // Special tokenization, so must do parsing here
        //
        
        auto Tok = TheParser->currentToken_stringifyAsFile();
        Tok = TheParser->eatTrivia_stringifyAsFile(Tok, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
        
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
        
        TheParser->pushNode(std::move(Operand));
        
        TheParser->shift();
        
        auto L = NodePtr(new BinaryNode(SYMBOL_PUT, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction GreaterGreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterGreaterParselet_parseInfix;
}

void GreaterGreaterGreaterParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    {
        TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
        
        TheParser->nextToken(TokIn);
        
        TheParser->shift();
        
        auto& Args = TheParser->peekArgs();
        
        TriviaSeq Trivia1;
        
        //
        // Special tokenization, so must do parsing here
        //
        
        auto Tok = TheParser->currentToken_stringifyAsFile();
        Tok = TheParser->eatTrivia_stringifyAsFile(Tok, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
        
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
        
        TheParser->pushNode(std::move(Operand));
        
        TheParser->shift();
        
        auto L = NodePtr(new BinaryNode(SYMBOL_PUTAPPEND, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction LessLessParselet::parsePrefix() const {
    return LessLessParselet_parsePrefix;
}

void LessLessParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        auto& Args = TheParser->pushArgs(nullptr, nullptr);
        
        TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
        
        TheParser->nextToken(TokIn);
        
        TheParser->shift();
        
        TriviaSeq Trivia1;
        
        //
        // Special tokenization, so must do parsing here
        //
        
        auto Tok = TheParser->currentToken_stringifyAsFile();
        Tok = TheParser->eatTrivia_stringifyAsFile(Tok, Trivia1);
        
        Args.appendSeq(std::move(Trivia1));
        
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
        
        TheParser->pushNode(std::move(Operand));
        
        TheParser->shift();
        
        auto L = NodePtr(new PrefixNode(SYMBOL_GET, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(L));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


Precedence InfixDifferentialDParselet::getPrecedence() const {
    
    if (TheParser->topPrecedence() == PRECEDENCE_CLASS_INTEGRATIONOPERATORS) {

        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //

        return PRECEDENCE_LOWEST;
    }

    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

Token InfixDifferentialDParselet::processImplicitTimes(Token TokIn) const {
    
    if (TheParser->topPrecedence() == PRECEDENCE_CLASS_INTEGRATIONOPERATORS) {

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

void HashParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        TheParser->nextToken(TokIn);
        
        auto Tok = TheParser->currentToken(INSIDE_SLOT);
        
        NodePtr Slot;
        
        switch (Tok.Tok.value()) {
            case TOKEN_INTEGER.value():
            case TOKEN_STRING.value(): {
                
                TheParser->nextToken(Tok);
                
                auto& Args = TheParser->pushArgs(nullptr, nullptr);
                
                TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
                
                TheParser->shift();
                
                TheParser->pushNode(NodePtr(new LeafNode(Tok)));
                
                TheParser->shift();
                
                Slot = NodePtr(new CompoundNode(SYMBOL_SLOT, std::move(Args)));
                
                TheParser->popArgs();
                
                break;
            }
            default: {
                
                Slot = NodePtr(new LeafNode(TokIn));
                
                break;
            }
        }
        
        TheParser->pushNode(std::move(Slot));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction HashHashParselet::parsePrefix() const {
    return HashHashParselet_parsePrefix;
}

void HashHashParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        TheParser->nextToken(TokIn);
        
        auto Tok = TheParser->currentToken(INSIDE_SLOTSEQUENCE);
        
        NodePtr SlotSequence;
        
        switch (Tok.Tok.value()) {
            case TOKEN_INTEGER.value(): {
                
                TheParser->nextToken(Tok);
                
                auto& Args = TheParser->pushArgs(nullptr, nullptr);
                
                TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
                
                TheParser->shift();
                
                TheParser->pushNode(NodePtr(new LeafNode(Tok)));
                
                TheParser->shift();
                
                SlotSequence = NodePtr(new CompoundNode(SYMBOL_SLOTSEQUENCE, std::move(Args)));
                
                TheParser->popArgs();
                
                break;
            }
            default: {
                
                SlotSequence = NodePtr(new LeafNode(TokIn));
                
                break;
            }
        }
        
        TheParser->pushNode(std::move(SlotSequence));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction PercentParselet::parsePrefix() const {
    return PercentParselet_parsePrefix;
}

void PercentParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        TheParser->nextToken(TokIn);
        
        auto Tok = TheParser->currentToken(INSIDE_OUT);
        
        NodePtr Out;
        
        switch (Tok.Tok.value()) {
            case TOKEN_INTEGER.value(): {
                
                TheParser->nextToken(Tok);
                
                auto& Args = TheParser->pushArgs(nullptr, nullptr);
                
                TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
                
                TheParser->shift();
                
                TheParser->pushNode(NodePtr(new LeafNode(Tok)));
                
                TheParser->shift();
                
                Out = NodePtr(new CompoundNode(SYMBOL_OUT, std::move(Args)));
                
                TheParser->popArgs();
                
                break;
            }
            default: {
                
                Out = NodePtr(new LeafNode(TokIn));
                
                break;
            }
        }
        
        TheParser->pushNode(std::move(Out));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}


ParseFunction PercentPercentParselet::parsePrefix() const {
    return PercentPercentParselet_parsePrefix;
}

void PercentPercentParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        TheParser->nextToken(TokIn);

        auto Out = NodePtr(new LeafNode(TokIn));
        
        TheParser->pushNode(std::move(Out));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}
