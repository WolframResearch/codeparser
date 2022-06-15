
#include "Parselet.h"

#include "API.h" // for ParserSession
#include "ParseletRegistration.h" // for infixParselets, etc.
#include "Symbol.h"
#include "Parser.h"


Token InfixParselet::processImplicitTimes(Token TokIn) const {
    return TokIn;
}

Symbol InfixParselet::getOp() const {
    return SYMBOL_CODEPARSER_INTERNALINVALID;
}


ParseFunction LeafParselet::parsePrefix() const {
    return LeafParselet_reduceLeaf;
}

void LeafParselet_reduceLeaf(ParseletPtr P, Token TokIn) {
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, TokIn/*ignored*/);
}


ParseFunction PrefixErrorParselet::parsePrefix() const {
    return PrefixErrorParselet_parsePrefix;
}

void PrefixErrorParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(TokIn.Tok.isError());
    
    if (TokIn.Tok.isUnterminated()) {
        
        TheParser->pushNode(new UnterminatedTokenErrorNeedsReparseNode(TokIn));
        
    } else {
        
        TheParser->pushNode(new ErrorNode(TokIn));
    }
    
    TheParser->nextToken(TokIn);
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, TokIn/*ignored*/);
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
    
    TheParser->pushNode(new ErrorNode(createdToken));
    
    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    TheParser->currentToken(TOPLEVEL);
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, TokIn/*ignored*/);
}


ParseFunction PrefixToplevelCloserParselet::parsePrefix() const {
    return PrefixToplevelCloserParselet_parsePrefix;
}

void PrefixToplevelCloserParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(TokIn.Tok.isCloser());
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    TheParser->pushNode(new ErrorNode(Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.BufLen, TokIn.Src)));
    
    TheParser->nextToken(TokIn);
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, TokIn/*ignored*/);
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
    
    TheParser->pushNode(new ErrorNode(createdToken));
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, TokIn/*ignored*/);
}


ParseFunction PrefixUnsupportedTokenParselet::parsePrefix() const {
    return PrefixUnsupportedTokenParselet_parsePrefix;
}

void PrefixUnsupportedTokenParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushNode(new ErrorNode(Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.BufLen, TokIn.Src)));
    
    TheParser->nextToken(TokIn);
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, TokIn/*ignored*/);
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
    
    Token createdToken;
    
    if (TheParser->topPrecedence() == PRECEDENCE_LOWEST) {
        
        createdToken = Token(TOKEN_ERROR_PREFIXIMPLICITNULL, TokIn.BufLen.buffer, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start);
    }
    
    TheParser->pushNode(new ErrorNode(createdToken));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, TokIn/*ignored*/);
}


ParseFunction PrefixUnhandledParselet::parsePrefix() const {
    return PrefixUnhandledParselet_parsePrefix;
}

void PrefixUnhandledParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(!TokIn.Tok.isPossibleBeginning() && "handle at call site");
    
    TheParser->pushNode(new ErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.BufLen.buffer, TokIn.Src.Start)));
    
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
        return Parser_tryContinue(nullptr, TokIn/*ignored*/);
    }
    
    //
    // Handle something like  f[@2]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Operator node.
    //
    
    TheParser->pushContextV(TokenPrecedence);
    
    TheParser->shift();

    auto P2 = infixParselets[TokIn.Tok.value()];
    
    MUSTTAIL
    return (P2->parseInfix())(P2, TokIn);
}


Precedence InfixToplevelNewlineParselet::getPrecedence() const {
    //
    // Do not do Implicit Times across top-level newlines
    //
    return PRECEDENCE_LOWEST;
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
    
    //
    // Something like  x  or x_
    //
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            //
            // Something like  a_
            //
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(under1Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternBlank(under1Parselet, Tok);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            //
            // Something like  a__
            //
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(under2Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternBlank(under2Parselet, Tok);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            //
            // Something like  a___
            //
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(under3Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternBlank(under3Parselet, Tok);
        }
        case TOKEN_UNDERDOT.value(): {
            
            //
            // Something like  a_.
            //
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderDotParselet_parseInfixContextSensitive(underDotParselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternOptionalDefault(underDotParselet, Tok);
        }
        default: {
            
            //
            // Something like  a
            //
            
            MUSTTAIL
            return Parser_parseClimb(nullptr, TokIn/*ignored*/);
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
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    // no call needed here
    return;
}

void SymbolParselet_reducePatternBlank(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<UnderParselet *>(P));
    
    TheParser->shift();
    
    auto PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
    
    TheParser->pushNode(new CompoundNode(PBOp, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void SymbolParselet_reducePatternOptionalDefault(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


PrefixOperatorParselet::PrefixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence PrefixOperatorParselet::getPrecedence() const {
    return precedence;
}

Symbol PrefixOperatorParselet::getOp() const {
    return Op;
}

ParseFunction PrefixOperatorParselet::parsePrefix() const {
    return PrefixOperatorParselet_parsePrefix;
}

void PrefixOperatorParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    assert(dynamic_cast<PrefixOperatorParselet *>(P));
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    auto& Ctxt = TheParser->pushContext(dynamic_cast<PrefixOperatorParselet *>(P)->getPrecedence());
    
    TheParser->shift();
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = PrefixOperatorParselet_reducePrefixOperator;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void PrefixOperatorParselet_reducePrefixOperator(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<PrefixOperatorParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<PrefixOperatorParselet *>(P)->getOp();
    
    TheParser->pushNode(new PrefixNode(Op, TheParser->popContext()));
    
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


Precedence InfixAssertFalseParselet::getPrecedence() const {
    return PRECEDENCE_LOWEST;
}

ParseFunction InfixAssertFalseParselet::parseInfix() const {
    return InfixAssertFalseParselet_parseInfix;
}

void InfixAssertFalseParselet_parseInfix(ParseletPtr P, Token firstTok) {
    
    assert(false);
    
    return;
}


BinaryOperatorParselet::BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence BinaryOperatorParselet::getPrecedence() const {
    return precedence;
}

Symbol BinaryOperatorParselet::getOp() const {
    return Op;
}

ParseFunction BinaryOperatorParselet::parseInfix() const {
    return BinaryOperatorParselet_parseInfix;
}

void BinaryOperatorParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    assert(dynamic_cast<BinaryOperatorParselet *>(P));
    
    TheParser->appendLeafArgAndNext(TokIn);

    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = BinaryOperatorParselet_reduceBinaryOperator;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void BinaryOperatorParselet_reduceBinaryOperator(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<BinaryOperatorParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<BinaryOperatorParselet *>(P)->getOp();
    
    TheParser->pushNode(new BinaryNode(Op, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


InfixOperatorParselet::InfixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence InfixOperatorParselet::getPrecedence() const {
    return precedence;
}

Symbol InfixOperatorParselet::getOp() const {
    return Op;
}

ParseFunction InfixOperatorParselet::parseInfix() const {
    return InfixOperatorParselet_parseInfix;
}

void InfixOperatorParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    assert(dynamic_cast<InfixOperatorParselet *>(P));
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = Parser_identity;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2);
    
    return InfixOperatorParselet_parseLoop(P, TokIn/*ignored*/);
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = InfixOperatorParselet_parseLoop;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void InfixOperatorParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<InfixOperatorParselet *>(P));
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    auto Tok1 = TheParser->currentToken(TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia1);
    
    auto I = infixParselets[Tok1.Tok.value()];

    Tok1 = I->processImplicitTimes(Tok1);
    
    if (Tok1.Tok == TOKEN_FAKE_IMPLICITTIMES) {
        
        //
        // implicit Times should not cross toplevel newlines
        //
        // so reset and try again
        //
        
        Trivia1.reset();
        
        Tok1 = TheParser->currentToken(TOPLEVEL);
        Tok1 = TheParser->eatTriviaButNotToplevelNewlines(Tok1, TOPLEVEL, Trivia1);
        
        I = infixParselets[Tok1.Tok.value()];
        
        Tok1 = I->processImplicitTimes(Tok1);
    }
    
    I = infixParselets[Tok1.Tok.value()];
    
    auto Op = dynamic_cast<InfixOperatorParselet *>(P)->getOp();
    
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
        
        Trivia1.reset();
        
        MUSTTAIL
        return InfixOperatorParselet_reduceInfixOperator(P, Ignored);
    }
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    TheParser->appendLeafArgAndNext(Tok1);

    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL, Trivia1);
    
    TheParser->appendArgs(Trivia1);
    
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
        
    (P2->parsePrefix())(P2, Tok2);
    
    } // while (true)
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == InfixOperatorParselet_parseLoop);
    assert(Ctxt.P == P);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void InfixOperatorParselet_reduceInfixOperator(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<InfixOperatorParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<InfixOperatorParselet *>(P)->getOp();
    
    TheParser->pushNode(new InfixNode(Op, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


PostfixOperatorParselet::PostfixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence PostfixOperatorParselet::getPrecedence() const {
    return precedence;
}

Symbol PostfixOperatorParselet::getOp() const {
    return Op;
}

ParseFunction PostfixOperatorParselet::parseInfix() const {
    return PostfixOperatorParselet_parseInfix;
}

void PostfixOperatorParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    MUSTTAIL
    return PostfixOperatorParselet_reducePostfixOperator(P, TokIn/*Ignored*/);
}

void PostfixOperatorParselet_reducePostfixOperator(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<PostfixOperatorParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<PostfixOperatorParselet *>(P)->getOp();
    
    TheParser->pushNode(new PostfixNode(Op, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


GroupParselet::GroupParselet(TokenEnum Opener, Symbol Op) : Op(Op), Closr(GroupOpenerToCloser(Opener)) {}

Symbol GroupParselet::getOp() const {
    return Op;
}

Closer GroupParselet::getCloser() const {
    return Closr;
}

ParseFunction GroupParselet::parsePrefix() const {
    return GroupParselet_parsePrefix;
}

void GroupParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    TheParser->pushGroup(GroupOpenerToCloser(TokIn.Tok));
    
    auto& Ctxt = TheParser->pushContext(PRECEDENCE_LOWEST);
    
#if !USE_MUSTTAIL
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = Parser_identity;
    
    return GroupParselet_parseLoop(P, TokIn/*Ignored*/);
#else
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = GroupParselet_parseLoop;
    Ctxt.P = P;
    
    MUSTTAIL
    return GroupParselet_parseLoop(P, TokIn/*Ignored*/);
#endif // !USE_MUSTTAIL
}

void GroupParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<GroupParselet *>(P));
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    // ADDENDUM: Actually, there may be more than 1 "good" node
    // e.g. {1\\2}
    //
    
    auto Closr = dynamic_cast<GroupParselet *>(P)->getCloser();
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
    
    if (TokenToCloser(Tok.Tok) == Closr) {
        
        //
        // Everything is good
        //
        
        TheParser->shift();
        
        TheParser->appendArgs(Trivia1);
        
        TheParser->pushLeafNodeAndNext(Tok);
        
        MUSTTAIL
        return GroupParselet_reduceGroup(P, Ignored);
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
            
            Trivia1.reset();
            
            MUSTTAIL
            return GroupParselet_reduceMissingCloser(P, Ignored);
        }
            
        //
        // Something like  { ) }
        //                   ^
        //
        
        TheParser->shift();
        
        TheParser->appendArgs(Trivia1);
        
#if !USE_MUSTTAIL
        PrefixToplevelCloserParselet_parsePrefix(prefixToplevelCloserParselet, Tok);
        
        continue;
#else
        MUSTTAIL
        return PrefixToplevelCloserParselet_parsePrefix(prefixToplevelCloserParselet, Tok);
#endif
    }

    if (Tok.Tok == TOKEN_ENDOFFILE) {

        //
        // Handle something like   { a EOF
        //
        
        Trivia1.reset();
        
        MUSTTAIL
        return GroupParselet_reduceUnterminatedGroup(P, Ignored);
    }

    //
    // Handle the expression
    //
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok.Tok.value()];
        
    (P2->parsePrefix())(P2, Tok);
    
    } // while (true)
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == GroupParselet_parseLoop);
    assert(Ctxt.P == P);
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
#endif // !USE_MUSTTAIL
}

void GroupParselet_reduceGroup(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<GroupParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<GroupParselet *>(P)->getOp();
    
    TheParser->pushNode(new GroupNode(Op, TheParser->popContext()));
    
    TheParser->popGroup();
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void GroupParselet_reduceMissingCloser(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<GroupParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<GroupParselet *>(P)->getOp();
    
    TheParser->pushNode(new GroupMissingCloserNode(Op, TheParser->popContext()));
    
    TheParser->popGroup();
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Ignored);
}

void GroupParselet_reduceUnterminatedGroup(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<GroupParselet *>(P));
    
    TheParser->shift();
    
    auto Op = dynamic_cast<GroupParselet *>(P)->getOp();
    
    TheParser->pushNode(new UnterminatedGroupNeedsReparseNode(Op, TheParser->popContext()));
    
    TheParser->popGroup();
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Ignored);
}


CallParselet::CallParselet(PrefixParseletPtr GP) : GP(std::move(GP)) {}

PrefixParseletPtr CallParselet::getGP() const {
    return GP;
}

Precedence CallParselet::getPrecedence() const {
    return PRECEDENCE_CALL;
}

ParseFunction CallParselet::parseInfix() const {
    return CallParselet_parseInfix;
}

void CallParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    assert(dynamic_cast<CallParselet *>(P));
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    TheParser->setPrecedence(PRECEDENCE_HIGHEST);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = CallParselet_reduceCall;
    Ctxt.P = P;
    
    auto GP = dynamic_cast<CallParselet *>(P)->getGP();
    
    MUSTTAIL
    return (GP->parsePrefix())(GP, TokIn);
}

void CallParselet_reduceCall(ParseletPtr P, Token Ignored) {
    
    TheParser->pushNode(new CallNode(TheParser->popContext(), TheParser->popNode()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction TildeParselet::parseInfix() const {
    return TildeParselet_parseInfix;
}

Precedence TildeParselet::getPrecedence() const {
    
    if (TheParser->checkTilde()) {
        return PRECEDENCE_LOWEST;
    }
    
    return PRECEDENCE_TILDE;
}

void TildeParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    //
    // Something like  a ~f~ b
    //
    // It'd be weird if this were an "infix operator"
    //
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto FirstTok = TheParser->currentToken(TOPLEVEL);
    FirstTok = TheParser->eatTrivia(FirstTok, TOPLEVEL);
    
    TheParser->setPrecedence(PRECEDENCE_LOWEST);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = TildeParselet_parse1;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[FirstTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, FirstTok);
}

void TildeParselet_parse1(ParseletPtr P, Token Ignored) {
    
    Token Tok1;
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    Tok1 = TheParser->currentToken(TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia1);
    
    if (Tok1.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        Trivia1.reset();
        
        MUSTTAIL
        return TildeParselet_reduceError(P, Ignored);
    }
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    TheParser->appendLeafArgAndNext(Tok1);

    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL, Trivia1);
    
    TheParser->appendArgs(Trivia1);
    
    //
    // Reset back to "outside" precedence
    //
    
    TheParser->setPrecedence(PRECEDENCE_TILDE);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == TildeParselet_parse1);
    assert(Ctxt.P == P);
    Ctxt.F = TildeParselet_reduceTilde;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
}

void TildeParselet_reduceTilde(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void TildeParselet_reduceError(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDTILDE, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_tryContinue(nullptr, Ignored);
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
    
    //
    // Something like  symbol:object  or  pattern:optional
    //
    
    auto colonLHS = TheParser->checkColonLHS();
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    switch (colonLHS) {
        case COLONLHS_PATTERN: {
            
            TheParser->setPrecedence(PRECEDENCE_FAKE_PATTERNCOLON);
            
            auto& Ctxt = TheParser->topContext();
            assert(Ctxt.F == nullptr);
            assert(Ctxt.P == nullptr);
            Ctxt.F = ColonParselet_reducePattern;
            Ctxt.P = P;
            
            auto P2 = prefixParselets[Tok.Tok.value()];
            
            MUSTTAIL
            return (P2->parsePrefix())(P2, Tok);
        }
        case COLONLHS_OPTIONAL: {
            
            TheParser->setPrecedence(PRECEDENCE_FAKE_OPTIONALCOLON);
            
            auto& Ctxt = TheParser->topContext();
            assert(Ctxt.F == nullptr);
            assert(Ctxt.P == nullptr);
            Ctxt.F = ColonParselet_reduceOptional;
            Ctxt.P = P;
            
            auto P2 = prefixParselets[Tok.Tok.value()];

            MUSTTAIL
            return (P2->parsePrefix())(P2, Tok);
        }
        case COLONLHS_ERROR: {
            
            TheParser->setPrecedence(PRECEDENCE_FAKE_PATTERNCOLON);
            
            auto& Ctxt = TheParser->topContext();
            assert(Ctxt.F == nullptr);
            assert(Ctxt.P == nullptr);
            Ctxt.F = ColonParselet_reduceError;
            Ctxt.P = P;
            
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

void ColonParselet_reducePattern(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_PATTERN, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void ColonParselet_reduceError(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSYMBOL, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void ColonParselet_reduceOptional(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_OPTIONAL, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


Precedence SlashColonParselet::getPrecedence() const {
    return PRECEDENCE_SLASHCOLON;
}

ParseFunction SlashColonParselet::parseInfix() const {
    return SlashColonParselet_parseInfix;
}

void SlashColonParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    //
    // a /: b := c  is handled here
    //
    
    //
    // Something like  a /: b = c
    //
    // a   /:   b   =   c
    // ^~~~~ Args at the start
    //       ^~~ Trivia1
    //           ^~~ Trivia2
    //
    //
    // It'd be weird if this were an "infix operator"
    //
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = SlashColonParselet_parse1;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void SlashColonParselet_parse1(ParseletPtr P, Token Ignored) {
    
    Token Tok;
    
    {
        auto& Trivia1 = TheParser->getTrivia1();
        
        Tok = TheParser->currentToken(TOPLEVEL);
        Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
        
        switch (Tok.Tok.value()) {
            case TOKEN_EQUAL.value(): {
                
                TheParser->shift();
                
                TheParser->appendArgs(Trivia1);
                
                break;
            }
            case TOKEN_COLONEQUAL.value(): {
                
                TheParser->shift();
                
                TheParser->appendArgs(Trivia1);
                
                break;
            }
            default: {
                
                Trivia1.reset();
                
                break;
            }
        }
    }
    
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            
            TheParser->setPrecedence(PRECEDENCE_EQUAL);
            
            MUSTTAIL
            return EqualParselet_parseInfixTag(equalParselet, Tok);
        }
        case TOKEN_COLONEQUAL.value(): {
            
            TheParser->setPrecedence(PRECEDENCE_COLONEQUAL);
            
            MUSTTAIL
            return ColonEqualParselet_parseInfixTag(colonEqualParselet, Tok);
        }
        default: {
            
            //
            // Anything other than:
            // a /: b = c
            // a /: b := c
            // a /: b =.
            //
            
            MUSTTAIL
            return SlashColonParselet_reduceError(P, Ignored);
        }
    }
}

void SlashColonParselet_reduceError(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSET, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


EqualParselet::EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, SYMBOL_SET) {}

ParseFunction EqualParselet::parseInfix() const {
    return EqualParselet_parseInfix;
}

void EqualParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        TheParser->pushLeafNodeAndNext(Tok);
        
        MUSTTAIL
        return EqualParselet_reduceUnset(P, TokIn/*Ignored*/);
    }
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = EqualParselet_reduceSet;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void EqualParselet_parseInfixTag(ParseletPtr P, Token TokIn) {
    
    //
    // a /: b = c  and  a /: b = .  are handled here
    //
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        TheParser->pushLeafNodeAndNext(Tok);
        
        MUSTTAIL
        return EqualParselet_reduceTagUnset(P, TokIn/*Ignored*/);
    }
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == SlashColonParselet_parse1);
    assert(Ctxt.P == slashColonParselet);
    Ctxt.F = EqualParselet_reduceTagSet;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void EqualParselet_reduceSet(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_SET, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void EqualParselet_reduceUnset(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_UNSET, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void EqualParselet_reduceTagSet(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new TernaryNode(SYMBOL_TAGSET, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void EqualParselet_reduceTagUnset(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new TernaryNode(SYMBOL_TAGUNSET, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ColonEqualParselet::ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}

ParseFunction ColonEqualParselet::parseInfix() const {
    return ColonEqualParselet_parseInfix;
}

void ColonEqualParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = ColonEqualParselet_reduceSetDelayed;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void ColonEqualParselet_parseInfixTag(ParseletPtr P, Token TokIn) {
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == SlashColonParselet_parse1);
    assert(Ctxt.P == slashColonParselet);
    Ctxt.F = ColonEqualParselet_reduceTagSetDelayed;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void ColonEqualParselet_reduceSetDelayed(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_SETDELAYED, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void ColonEqualParselet_reduceTagSetDelayed(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new TernaryNode(SYMBOL_TAGSETDELAYED, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction IntegralParselet::parsePrefix() const {
    return IntegralParselet_parsePrefix;
}

void IntegralParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // Something like  \[Integral] f \[DifferentialD] x
    //
    
    auto& Ctxt = TheParser->pushContext(PRECEDENCE_CLASS_INTEGRATIONOPERATORS);
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL);
    
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = IntegralParselet_parse1;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void IntegralParselet_parse1(ParseletPtr P, Token Ignored) {
    
    auto& Trivia1 = TheParser->getTrivia1();

    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTrivia(Tok, TOPLEVEL, Trivia1);
    
    if (!(Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.Tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD)) {
        
        Trivia1.reset();
        
        MUSTTAIL
        return IntegralParselet_reduceIntegral(P, Ignored);
    }
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == IntegralParselet_parse1);
    assert(Ctxt.P == P);
    Ctxt.F = IntegralParselet_reduceIntegrate;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
}

void IntegralParselet_reduceIntegrate(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new PrefixBinaryNode(SYMBOL_INTEGRATE, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void IntegralParselet_reduceIntegral(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new PrefixNode(SYMBOL_INTEGRAL, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


Precedence CommaParselet::getPrecedence() const {
    return PRECEDENCE_COMMA;
}

ParseFunction CommaParselet::parseInfix() const {
    return CommaParselet_parseInfix;
}

void CommaParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL);
    
    if (Tok2.Tok == TOKEN_COMMA || Tok2.Tok == TOKEN_LONGNAME_INVISIBLECOMMA) {
        
        //
        // Something like  a,,
        //
        
        TheParser->pushNode(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
#if !USE_MUSTTAIL
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == nullptr);
        assert(Ctxt.P == nullptr);
        Ctxt.F = Parser_identity;
        
        return CommaParselet_parseLoop(P, TokIn/*ignored*/);
#else
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == nullptr);
        assert(Ctxt.P == nullptr);
        Ctxt.F = CommaParselet_parseLoop;
        Ctxt.P = P;
        
        MUSTTAIL
        return CommaParselet_parseLoop(P, TokIn/*ignored*/);
#endif // !USE_MUSTTAIL
    }
    
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = Parser_identity;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(P2, Tok2);
    
    return CommaParselet_parseLoop(P, TokIn/*ignored*/);
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == nullptr);
    assert(Ctxt.P == nullptr);
    Ctxt.F = CommaParselet_parseLoop;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void CommaParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Trivia1 = TheParser->getTrivia1();

    auto Tok1 = TheParser->currentToken(TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia1);
    
    if (!(Tok1.Tok == TOKEN_COMMA || Tok1.Tok == TOKEN_LONGNAME_INVISIBLECOMMA)) {
        
        Trivia1.reset();
        
        MUSTTAIL
        return CommaParselet_reduceComma(P, Ignored);
    }

    //
    // Something like  a,b
    //
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    TheParser->appendLeafArgAndNext(Tok1);

    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    Tok2 = TheParser->eatTrivia(Tok2, TOPLEVEL, Trivia1);
    
    TheParser->appendArgs(Trivia1);
    
    if (Tok2.Tok == TOKEN_COMMA || Tok2.Tok == TOKEN_LONGNAME_INVISIBLECOMMA) {

        //
        // Something like  a,,
        //
        
        TheParser->pushNode(new ErrorNode(Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
#if !USE_MUSTTAIL
        continue;
#else
        MUSTTAIL
        return CommaParselet_parseLoop(P, Ignored);
#endif // !USE_MUSTTAIL
    }
        
#if !USE_MUSTTAIL
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
        
    (P2->parsePrefix())(P2, Tok2);
        
    } // while (true)
#else
    auto& Ctxt = TheParser->topContext();
    assert(Ctxt.F == CommaParselet_parseLoop);
    assert(Ctxt.P == P);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void CommaParselet_reduceComma(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new InfixNode(SYMBOL_CODEPARSER_COMMA, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

Symbol CommaParselet::getOp() const {
    return SYMBOL_CODEPARSER_COMMA;
}


Precedence SemiParselet::getPrecedence() const {
    return PRECEDENCE_SEMI;
}

ParseFunction SemiParselet::parseInfix() const {
    return SemiParselet_parseInfix;
}

void SemiParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    //
    // CompoundExpression should not cross toplevel newlines
    //
    Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, TOPLEVEL);
    
    if (Tok2.Tok == TOKEN_SEMI) {
        
        //
        // Something like  a; ;
        //
        
        TheParser->pushNode(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
        //
        // nextToken() is not needed after an implicit token
        //
        
#if !USE_MUSTTAIL
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == nullptr);
        assert(Ctxt.P == nullptr);
        
        Ctxt.F = Parser_identity;
        
        return SemiParselet_parseLoop(P, TokIn/*ignored*/);
#else
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == nullptr);
        assert(Ctxt.P == nullptr);
        
        Ctxt.F = SemiParselet_parseLoop;
        Ctxt.P = P;
        
        MUSTTAIL
        return SemiParselet_parseLoop(P, TokIn/*ignored*/);
#endif // !USE_MUSTTAIL
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        //
        // Something like  a;+2
        //
        
#if !USE_MUSTTAIL
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == nullptr);
        assert(Ctxt.P == nullptr);
        Ctxt.F = Parser_identity;
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        (P2->parsePrefix())(P2, Tok2);
        
        return SemiParselet_parseLoop(P, TokIn/*ignored*/);
#else
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == nullptr);
        assert(Ctxt.P == nullptr);
        Ctxt.F = SemiParselet_parseLoop;
        Ctxt.P = P;
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        MUSTTAIL
        return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
    }
    
    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //
    
    TheParser->pushNode(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
    
    //
    // nextToken() is not needed after an implicit token
    //
    
    MUSTTAIL
    return SemiParselet_reduceCompoundExpression(P, TokIn/*Ignored*/);
}

void SemiParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    auto Tok1 = TheParser->currentToken(TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia1);
    
    if (Tok1.Tok != TOKEN_SEMI) {
        
        //
        // Something like  a;b
        //
        
        Trivia1.reset();
        
        MUSTTAIL
        return SemiParselet_reduceCompoundExpression(P, Ignored);
    }
    
    //
    // Something like  a;b
    //
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    TheParser->appendLeafArgAndNext(Tok1);

    auto Tok2 = TheParser->currentToken(TOPLEVEL);
    //
    // CompoundExpression should not cross toplevel newlines
    //
    Tok2 = TheParser->eatTriviaButNotToplevelNewlines(Tok2, TOPLEVEL, Trivia1);
    
    TheParser->appendArgs(Trivia1);
    
    if (Tok2.Tok == TOKEN_SEMI) {

        //
        // Something like  a;b; ;
        //
        
        TheParser->pushNode(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
        
        //
        // nextToken() is not needed after an implicit token
        //
        
#if !USE_MUSTTAIL
        continue;
#else
        MUSTTAIL
        return SemiParselet_parseLoop(P, Ignored);
#endif // !USE_MUSTTAIL
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        //
        // Something like  a;b;+2
        //
        
#if !USE_MUSTTAIL
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == Parser_identity);
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        (P2->parsePrefix())(P2, Tok2);
        
        continue;
#else
        auto& Ctxt = TheParser->topContext();
        assert(Ctxt.F == SemiParselet_parseLoop);
        assert(Ctxt.P == P);
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        MUSTTAIL
        return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
    }

    //
    // Not beginning of an expression
    //
    // For example:  a;b;&
    //
    
    TheParser->pushNode(new LeafNode(Token(TOKEN_FAKE_IMPLICITNULL, Tok2.BufLen.buffer, Tok2.Src.Start)));
    
    //
    // nextToken() is not needed after an implicit token
    //
    
    MUSTTAIL
    return SemiParselet_reduceCompoundExpression(P, Ignored);
        
#if !USE_MUSTTAIL
    } // while (true)
#endif // !USE_MUSTTAIL
}

void SemiParselet_reduceCompoundExpression(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new InfixNode(SYMBOL_COMPOUNDEXPRESSION, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


Precedence ColonColonParselet::getPrecedence() const {
    return PRECEDENCE_COLONCOLON;
}

ParseFunction ColonColonParselet::parseInfix() const {
    return ColonColonParselet_parseInfix;
}

void ColonColonParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    //
    // a::b
    //
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok2 = TheParser->currentToken_stringifyAsTag();
    
    if (Tok2.Tok.isError()) {
        
        if (Tok2.Tok.isUnterminated()) {
            
            TheParser->pushNode(new UnterminatedTokenErrorNeedsReparseNode(Tok2));
            
            TheParser->nextToken(Tok2);
            
        } else {
            
            TheParser->pushNode(new ErrorNode(Tok2));
            
            TheParser->nextToken(Tok2);
        }
        
    } else {
        
        assert(Tok2.Tok == TOKEN_STRING);
        
        TheParser->pushLeafNodeAndNext(Tok2);
    }
    
    MUSTTAIL
    return ColonColonParselet_parseLoop(P, TokIn/*ignored*/);
}

void ColonColonParselet_parseLoop(ParseletPtr P, Token Ignored) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
    //
    // Check isAbort() inside loops
    //
    HANDLE_ABORT;
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    auto Tok1 = TheParser->currentToken(TOPLEVEL);
    Tok1 = TheParser->eatTrivia(Tok1, TOPLEVEL, Trivia1);
    
    if (Tok1.Tok != TOKEN_COLONCOLON) {
        
        Trivia1.reset();
        
        MUSTTAIL
        return ColonColonParselet_reduceMessageName(P, Ignored);
    }
    
    TheParser->shift();
    
    TheParser->appendArgs(Trivia1);
    
    TheParser->appendLeafArgAndNext(Tok1);
    
    //
    // Special tokenization, so must do parsing here
    //

    auto Tok2 = TheParser->currentToken_stringifyAsTag();

    if (Tok2.Tok.isError()) {

        if (Tok2.Tok.isUnterminated()) {
            
            TheParser->pushNode(new UnterminatedTokenErrorNeedsReparseNode(Tok2));
            
            TheParser->nextToken(Tok2);
            
        } else {
            
            TheParser->pushNode(new ErrorNode(Tok2));
            
            TheParser->nextToken(Tok2);
        }

    } else {

        assert(Tok2.Tok == TOKEN_STRING);
        
        TheParser->pushLeafNodeAndNext(Tok2);
    }
    
#if !USE_MUSTTAIL
    } // while (true)
#else
    MUSTTAIL
    return ColonColonParselet_parseLoop(P, Ignored);
#endif // !USE_MUSTTAIL
}

void ColonColonParselet_reduceMessageName(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new InfixNode(SYMBOL_MESSAGENAME, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


Precedence GreaterGreaterParselet::getPrecedence() const {
    return PRECEDENCE_GREATERGREATER;
}

ParseFunction GreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterParselet_parseInfix;
}

void GreaterGreaterParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    //
    // a>>b
    //
    
    //
    // Special tokenization, so must do parsing here
    //
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken_stringifyAsFile();
    Tok = TheParser->eatTrivia_stringifyAsFile(Tok);
    
    if (Tok.Tok.isError()) {
        
        if (Tok.Tok.isUnterminated()) {
            
            TheParser->pushNode(new UnterminatedTokenErrorNeedsReparseNode(Tok));
            
            TheParser->nextToken(Tok);
            
        } else {
            
            TheParser->pushNode(new ErrorNode(Tok));
            
            TheParser->nextToken(Tok);
        }
        
    } else {
        
        assert(Tok.Tok == TOKEN_STRING);
        
        TheParser->pushLeafNodeAndNext(Tok);
    }
    
    MUSTTAIL
    return GreaterGreaterParselet_reducePut(P, TokIn/*Ignored*/);
}

void GreaterGreaterParselet_reducePut(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_PUT, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


Precedence GreaterGreaterGreaterParselet::getPrecedence() const {
    return PRECEDENCE_GREATERGREATERGREATER;
}

ParseFunction GreaterGreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterGreaterParselet_parseInfix;
}

void GreaterGreaterGreaterParselet_parseInfix(ParseletPtr P, Token TokIn) {
    
    //
    // a>>>b
    //
    
    //
    // Special tokenization, so must do parsing here
    //
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken_stringifyAsFile();
    Tok = TheParser->eatTrivia_stringifyAsFile(Tok);
    
    if (Tok.Tok.isError()) {
        
        if (Tok.Tok.isUnterminated()) {
            
            TheParser->pushNode(new UnterminatedTokenErrorNeedsReparseNode(Tok));
            
            TheParser->nextToken(Tok);
            
        } else {
            
            TheParser->pushNode(new ErrorNode(Tok));
            
            TheParser->nextToken(Tok);
        }
            
    } else {
        
        assert(Tok.Tok == TOKEN_STRING);
        
        TheParser->pushLeafNodeAndNext(Tok);
    }
    
    MUSTTAIL
    return GreaterGreaterGreaterParselet_reducePutAppend(P, TokIn/*Ignored*/);
}

void GreaterGreaterGreaterParselet_reducePutAppend(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new BinaryNode(SYMBOL_PUTAPPEND, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction LessLessParselet::parsePrefix() const {
    return LessLessParselet_parsePrefix;
}

void LessLessParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // <<a
    //
    
    //
    // Special tokenization, so must do parsing here
    //
    
    TheParser->pushContextV(PRECEDENCE_HIGHEST);
    
    TheParser->appendLeafArgAndNext(TokIn);
    
    auto Tok = TheParser->currentToken_stringifyAsFile();
    Tok = TheParser->eatTrivia_stringifyAsFile(Tok);
    
    if (Tok.Tok.isError()) {
        
        if (Tok.Tok.isUnterminated()) {
            
            TheParser->pushNode(new UnterminatedTokenErrorNeedsReparseNode(Tok));
            
            TheParser->nextToken(Tok);
            
        } else {
            
            TheParser->pushNode(new ErrorNode(Tok));
            
            TheParser->nextToken(Tok);
        }
        
    } else {
            
        assert(Tok.Tok == TOKEN_STRING);
        
        TheParser->pushLeafNodeAndNext(Tok);
    }
    
    MUSTTAIL
    return LessLessParselet_reduceGet(P, TokIn/*Ignored*/);
}

void LessLessParselet_reduceGet(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new PrefixNode(SYMBOL_GET, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
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
    
    //
    // Something like  #  or  #1  or  #abc  or  #"abc"
    //
    // From Slot documentation:
    //
    // In the form #name, the characters in name can be any combination of alphanumeric characters not beginning with digits.
    //
    //
    // A slot that starts with a digit goes down one path
    // And a slot that starts with a letter goes down another path
    //
    // Make sure e.g.  #1a is not parsed as SlotNode["#1a"]
    //
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(INSIDE_SLOT);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value():
        case TOKEN_STRING.value(): {
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            TheParser->pushLeafNodeAndNext(Tok);
            
            MUSTTAIL
            return HashParselet_reduceSlot(P, TokIn/*Ignored*/);
        }
        default: {
            MUSTTAIL
            return Parser_parseClimb(nullptr, TokIn/*ignored*/);
        }
    }
}

void HashParselet_reduceSlot(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new CompoundNode(SYMBOL_SLOT, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction HashHashParselet::parsePrefix() const {
    return HashHashParselet_parsePrefix;
}

void HashHashParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // Something like  ##  or  ##1
    //
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(INSIDE_SLOTSEQUENCE);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            TheParser->pushLeafNodeAndNext(Tok);
            
            MUSTTAIL
            return HashHashParselet_reduceSlotSequence(P, TokIn/*ignored*/);
        }
        default: {
            
            MUSTTAIL
            return Parser_parseClimb(nullptr, TokIn/*ignored*/);
        }
    }
    
    
}

void HashHashParselet_reduceSlotSequence(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new CompoundNode(SYMBOL_SLOTSEQUENCE, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}


ParseFunction PercentParselet::parsePrefix() const {
    return PercentParselet_parsePrefix;
}

void PercentParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // Something like  %  or  %1
    //
    
    TheParser->pushLeafNodeAndNext(TokIn);
    
    auto Tok = TheParser->currentToken(INSIDE_OUT);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            TheParser->pushContextV(PRECEDENCE_HIGHEST);
            
            TheParser->shift();
            
            TheParser->pushLeafNodeAndNext(Tok);
            
            MUSTTAIL
            return PercentParselet_reduceOut(P, TokIn/*ignored*/);
        }
        default: {
            MUSTTAIL
            return Parser_parseClimb(nullptr, TokIn/*ignored*/);
        }
    }
}

void PercentParselet_reduceOut(ParseletPtr P, Token Ignored) {
    
    TheParser->shift();
    
    TheParser->pushNode(new CompoundNode(SYMBOL_OUT, TheParser->popContext()));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}
