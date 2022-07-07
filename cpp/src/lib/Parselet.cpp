
#include "Parselet.h"

#include "ParseletRegistration.h" // for infixParselets, etc.
#include "Symbol.h"
#include "Parser.h"
#include "ParserSession.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


Token InfixParselet::processImplicitTimes(ParserSessionPtr session, Token TokIn) const {
    return TokIn;
}

Symbol InfixParselet::getOp() const {
    return SYMBOL_CODEPARSER_INTERNALINVALID;
}


ParseFunction LeafParselet::parsePrefix() const {
    return LeafParselet_reduceLeaf;
}

void LeafParselet_reduceLeaf(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    Parser_pushLeafAndNext(session, TokIn);
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixErrorParselet::parsePrefix() const {
    return PrefixErrorParselet_parsePrefix;
}

void PrefixErrorParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    assert(TokIn.Tok.isError());
    
    Parser_pushLeafAndNext(session, TokIn);
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixCloserParselet::parsePrefix() const {
    return PrefixCloserParselet_parsePrefix;
}

void PrefixCloserParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    assert(TokIn.Tok.isCloser());
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    //
    // Inside some other parselet that is not GroupParselet
    //
    
    Token createdToken;
    
    if (Parser_topPrecedence(session) == PRECEDENCE_COMMA) {
        
        createdToken = Token(TOKEN_ERROR_INFIXIMPLICITNULL, TokIn.Buf, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.Buf, TokIn.Src.Start);
    }
    
    Parser_pushLeaf(session, createdToken);
    
    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    Parser_currentToken(session, TOPLEVEL);
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixToplevelCloserParselet::parsePrefix() const {
    return PrefixToplevelCloserParselet_parsePrefix;
}

void PrefixToplevelCloserParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    assert(TokIn.Tok.isCloser());
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    //
    // if we are at the top, then make sure to take the token and report it
    //
    
    Parser_pushLeaf(session, Token(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.bufLen(), TokIn.Src));
    
    Parser_nextToken(session, TokIn);
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixEndOfFileParselet::parsePrefix() const {
    return PrefixEndOfFileParselet_parsePrefix;
}

void PrefixEndOfFileParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  a+<EOF>
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Token createdToken;
    
    if (Parser_topPrecedence(session) == PRECEDENCE_COMMA) {
            
        createdToken = Token(TOKEN_ERROR_INFIXIMPLICITNULL, TokIn.Buf, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.Buf, TokIn.Src.Start);
    }
    
    Parser_pushLeaf(session, createdToken);
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixUnsupportedTokenParselet::parsePrefix() const {
    return PrefixUnsupportedTokenParselet_parsePrefix;
}

void PrefixUnsupportedTokenParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeaf(session, Token(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.bufLen(), TokIn.Src));
    
    Parser_nextToken(session, TokIn);
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixCommaParselet::parsePrefix() const {
    return PrefixCommaParselet_parsePrefix;
}

void PrefixCommaParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_ERROR_PREFIXIMPLICITNULL
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Token createdToken;
    
    if (Parser_topPrecedence(session) == PRECEDENCE_LOWEST) {
        
        createdToken = Token(TOKEN_ERROR_PREFIXIMPLICITNULL, TokIn.Buf, TokIn.Src.Start);
        
    } else {
        
        createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.Buf, TokIn.Src.Start);
    }
    
    Parser_pushLeaf(session, createdToken);
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}


ParseFunction PrefixUnhandledParselet::parsePrefix() const {
    return PrefixUnhandledParselet_parsePrefix;
}

void PrefixUnhandledParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    assert(!TokIn.Tok.isPossibleBeginning() && "handle at call site");
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeaf(session, Token(TOKEN_ERROR_EXPECTEDOPERAND, TokIn.Buf, TokIn.Src.Start));
    
    //
    // Do not take next token
    //
    Parser_currentToken(session, TOPLEVEL);
    
    auto I = infixParselets[TokIn.Tok.value()];
    
    auto TokenPrecedence = I->getPrecedence(session);
    
    //
    // if (Ctxt.Prec > TokenPrecedence)
    //   goto prefixUnhandledParseletRet;
    // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
    //   goto prefixUnhandledParseletRet;
    //
    if ((Parser_topPrecedence(session) | 0x1) > TokenPrecedence) {
        
        //
        // Something like  a + | 2
        //
        // Make sure that the error leaf is with the + and not the |
        //
        
        MUSTTAIL
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
    
    //
    // Handle something like  f[@2]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Operator node.
    //
    
    Parser_pushContext(session, TokenPrecedence);

    auto P2 = infixParselets[TokIn.Tok.value()];
    
    MUSTTAIL
    return (P2->parseInfix())(session, P2, TokIn);
}


Precedence InfixToplevelNewlineParselet::getPrecedence(ParserSessionPtr session) const {
    //
    // Do not do Implicit Times across top-level newlines
    //
    return PRECEDENCE_LOWEST;
}

ParseFunction InfixToplevelNewlineParselet::parseInfix() const {
    return InfixToplevelNewlineParselet_parseInfix;
}

void InfixToplevelNewlineParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token firstTok) {
    
    assert(false);
    
    return;
}


ParseFunction SymbolParselet::parsePrefix() const {
    return SymbolParselet_parsePrefix;
}

void SymbolParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  x  or x_
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    switch (Tok.Tok.value()) {
        case TOKEN_UNDER.value(): {
            
            //
            // Something like  a_
            //
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(session, under1Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternBlank(session, under1Parselet, Tok/*ignored*/);
        }
        case TOKEN_UNDERUNDER.value(): {
            
            //
            // Something like  a__
            //
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(session, under2Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternBlank(session, under2Parselet, Tok/*ignored*/);
        }
        case TOKEN_UNDERUNDERUNDER.value(): {
            
            //
            // Something like  a___
            //
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderParselet_parseInfixContextSensitive(session, under3Parselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternBlank(session, under3Parselet, Tok/*ignored*/);
        }
        case TOKEN_UNDERDOT.value(): {
            
            //
            // Something like  a_.
            //
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            //
            // Context-sensitive and OK to build stack
            //
            
            UnderDotParselet_parseInfixContextSensitive(session, underDotParselet, Tok);
            
            MUSTTAIL
            return SymbolParselet_reducePatternOptionalDefault(session, underDotParselet, Tok/*ignored*/);
        }
    } // switch
    
    //
    // Something like  a
    //
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}

void SymbolParselet_parseInfixContextSensitive(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  _b
    //                  ^
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return;
    }
#endif // CHECK_ABORT
    
    //
    // We know we are already in the middle of parsing _
    //
    // Just push this symbol
    //
    
    Parser_pushLeafAndNext(session, TokIn);
    
    // no call needed here
    return;
}

void SymbolParselet_reducePatternBlank(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<UnderParselet *>(P));
    
    auto PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
    
    Parser_pushNode(session, new CompoundNode(PBOp, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
}

void SymbolParselet_reducePatternOptionalDefault(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
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

void PrefixOperatorParselet_parsePrefix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    assert(P);
    assert(dynamic_cast<PrefixOperatorParselet *>(P));
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto& Ctxt = Parser_pushContext(session, dynamic_cast<PrefixOperatorParselet *>(P)->getPrecedence());
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = PrefixOperatorParselet_reducePrefixOperator;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void PrefixOperatorParselet_reducePrefixOperator(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<PrefixOperatorParselet *>(P));
    
    auto Op = dynamic_cast<PrefixOperatorParselet *>(P)->getOp();
    
    Parser_pushNode(session, new PrefixNode(Op, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
}


ParseFunction InfixImplicitTimesParselet::parseInfix() const {
    return InfixImplicitTimesParselet_parseInfix;
}

void InfixImplicitTimesParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    assert(false);
    
    return;
}


Precedence InfixImplicitTimesParselet::getPrecedence(ParserSessionPtr session) const {
    
    assert(false && "The last token may not have been added to InfixParselets");
    
    return PRECEDENCE_ASSERTFALSE;
}


Token InfixImplicitTimesParselet::processImplicitTimes(ParserSessionPtr session, Token TokIn) const {
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.Buf, TokIn.Src.Start);
}


Precedence InfixAssertFalseParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_LOWEST;
}

ParseFunction InfixAssertFalseParselet::parseInfix() const {
    return InfixAssertFalseParselet_parseInfix;
}

void InfixAssertFalseParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token firstTok) {
    
    assert(false);
    
    return;
}


BinaryOperatorParselet::BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence BinaryOperatorParselet::getPrecedence(ParserSessionPtr session) const {
    return precedence;
}

Symbol BinaryOperatorParselet::getOp() const {
    return Op;
}

ParseFunction BinaryOperatorParselet::parseInfix() const {
    return BinaryOperatorParselet_parseInfix;
}

void BinaryOperatorParselet_parseInfix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    assert(P);
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);

    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = BinaryOperatorParselet_reduceBinaryOperator;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void BinaryOperatorParselet_reduceBinaryOperator(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<BinaryOperatorParselet *>(P));
    
    auto Op = dynamic_cast<BinaryOperatorParselet *>(P)->getOp();
    
    Parser_pushNode(session, new BinaryNode(Op, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
}


InfixOperatorParselet::InfixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence InfixOperatorParselet::getPrecedence(ParserSessionPtr session) const {
    return precedence;
}

Symbol InfixOperatorParselet::getOp() const {
    return Op;
}

ParseFunction InfixOperatorParselet::parseInfix() const {
    return InfixOperatorParselet_parseInfix;
}

void InfixOperatorParselet_parseInfix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    assert(P);
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = Parser_identity;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(session, P2, Tok2);
    
    return InfixOperatorParselet_parseLoop(session, P, TokIn/*ignored*/);
#else
    auto& Ctxt = Parser_topContext();
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = InfixOperatorParselet_parseLoop;
    Ctxt.P = P;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void InfixOperatorParselet_parseLoop(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<InfixOperatorParselet *>(P));
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, Ignored);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto Tok1 = Parser_currentToken(session, TOPLEVEL);
        
        Parser_eatTrivia(session, Tok1, TOPLEVEL, Trivia1);
    
    auto I = infixParselets[Tok1.Tok.value()];
    
    auto Op = dynamic_cast<InfixOperatorParselet *>(P)->getOp();
    
    //
    // Cannot just compare tokens
    //
    // May be something like  a && b \[And] c
    //
    // and && and \[And] are different parselets
    //
    // and we want only a single Infix node created
    //
    // FIXME: only create a single parselet for all of the same operators, e.g., && and \[And]
    //
    // then just compare parselets directly here
    //
    if (I->getOp() != Op) {
        
        //
        // Tok.Tok != TokIn.Tok, so break
        //
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return InfixOperatorParselet_reduceInfixOperator(session, P, Ignored);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeafAndNext(session, Tok1);

    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
        
    (P2->parsePrefix())(session, P2, Tok2);
    
    } // while (true)
#else
    auto& Ctxt = Parser_topContext();
    assert(Ctxt.F == InfixOperatorParselet_parseLoop);
    assert(Ctxt.P == P);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void InfixOperatorParselet_reduceInfixOperator(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<InfixOperatorParselet *>(P));
    
    auto Op = dynamic_cast<InfixOperatorParselet *>(P)->getOp();
    
    Parser_pushNode(session, new InfixNode(Op, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
}


PostfixOperatorParselet::PostfixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op) : precedence(precedence), Op(Op) {}

Precedence PostfixOperatorParselet::getPrecedence(ParserSessionPtr session) const {
    return precedence;
}

Symbol PostfixOperatorParselet::getOp() const {
    return Op;
}

ParseFunction PostfixOperatorParselet::parseInfix() const {
    return PostfixOperatorParselet_parseInfix;
}

void PostfixOperatorParselet_parseInfix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    assert(P);
    
    Parser_pushLeafAndNext(session, TokIn);
    
    MUSTTAIL
    return PostfixOperatorParselet_reducePostfixOperator(session, P, TokIn/*Ignored*/);
}

void PostfixOperatorParselet_reducePostfixOperator(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<PostfixOperatorParselet *>(P));
    
    auto Op = dynamic_cast<PostfixOperatorParselet *>(P)->getOp();
    
    Parser_pushNode(session, new PostfixNode(Op, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
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

void GroupParselet_parsePrefix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    assert(P);
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    Parser_pushGroup(session, GroupOpenerToCloser(TokIn.Tok));
    
    auto& Ctxt = Parser_pushContext(session, PRECEDENCE_LOWEST);
    
#if !USE_MUSTTAIL
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = Parser_identity;
    
    return GroupParselet_parseLoop(session, P, TokIn/*Ignored*/);
#else
    assert(!Ctxt.F);
    assert(!Ctxt.P);
    Ctxt.F = GroupParselet_parseLoop;
    Ctxt.P = P;
    
    MUSTTAIL
    return GroupParselet_parseLoop(P, TokIn/*Ignored*/);
#endif // !USE_MUSTTAIL
}

void GroupParselet_parseLoop(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<GroupParselet *>(P));
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_popGroup(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, Ignored);
    }
#endif // CHECK_ABORT
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    // ADDENDUM: Actually, there may be more than 1 "good" node
    // e.g. {1\\2}
    //
    
    auto Closr = dynamic_cast<GroupParselet *>(P)->getCloser();
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL, Trivia1);
    
    if (TokenToCloser(Tok.Tok) == Closr) {
        
        //
        // Everything is good
        //
        
        Parser_pushTriviaSeq(session, Trivia1);
        
        Parser_pushLeafAndNext(session, Tok);
        
        MUSTTAIL
        return GroupParselet_reduceGroup(session, P, Ignored);
    }
    
    if (Tok.Tok.isCloser()) {

        //
        // some other closer
        //
        
        if (Parser_checkGroup(session, TokenToCloser(Tok.Tok))) {
            
            //
            // Something like  { ( }
            //                     ^
            //
            
            //
            // Do not consume the bad closer now
            //
            
            Trivia1.reset(session);
            
            MUSTTAIL
            return GroupParselet_reduceMissingCloser(session, P, Ignored);
        }
            
        //
        // Something like  { ) }
        //                   ^
        //
        
        Parser_pushTriviaSeq(session, Trivia1);
        
#if !USE_MUSTTAIL
        PrefixToplevelCloserParselet_parsePrefix(session, prefixToplevelCloserParselet, Tok);
        
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
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return GroupParselet_reduceUnterminatedGroup(session, P, Ignored);
    }

    //
    // Handle the expression
    //
    
    Parser_pushTriviaSeq(session, Trivia1);
    
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok.Tok.value()];
        
    (P2->parsePrefix())(session, P2, Tok);
    
    } // while (true)
#else
    auto& Ctxt = Parser_topContext();
    assert(Ctxt.F == GroupParselet_parseLoop);
    assert(Ctxt.P == P);
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok);
#endif // !USE_MUSTTAIL
}

void GroupParselet_reduceGroup(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<GroupParselet *>(P));
    
    auto Op = dynamic_cast<GroupParselet *>(P)->getOp();
    
    Parser_pushNode(session, new GroupNode(Op, Parser_popContext(session)));
    
    Parser_popGroup(session);
    
    MUSTTAIL
    return Parser_parseClimb(session, P/*ignored*/, Ignored);
}

void GroupParselet_reduceMissingCloser(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<GroupParselet *>(P));
    
    auto Op = dynamic_cast<GroupParselet *>(P)->getOp();
    
    Parser_pushNode(session, new GroupMissingCloserNode(Op, Parser_popContext(session)));
    
    Parser_popGroup(session);
    
    MUSTTAIL
    return Parser_tryContinue(session, P/*ignored*/, Ignored);
}

void GroupParselet_reduceUnterminatedGroup(ParserSessionPtr session, ParseletPtr P, Token Ignored) {
    
    assert(P);
    assert(dynamic_cast<GroupParselet *>(P));
    
    auto Op = dynamic_cast<GroupParselet *>(P)->getOp();
    
    Parser_pushNode(session, new UnterminatedGroupNeedsReparseNode(Op, Parser_popContext(session)));
    
    Parser_popGroup(session);
    
    MUSTTAIL
    return Parser_tryContinue(session, P/*ignored*/, Ignored);
}


CallParselet::CallParselet(PrefixParseletPtr GP) : GP(GP) {}

PrefixParseletPtr CallParselet::getGP() const {
    return GP;
}

Precedence CallParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_CALL;
}

ParseFunction CallParselet::parseInfix() const {
    return CallParselet_parseInfix;
}

void CallParselet_parseInfix(ParserSessionPtr session, ParseletPtr P, Token TokIn) {
    
    assert(P);
    assert(dynamic_cast<CallParselet *>(P));
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, P/*ignored*/, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = CallParselet_reduceCall;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    
    auto GP = dynamic_cast<CallParselet *>(P)->getGP();
    
    MUSTTAIL
    return (GP->parsePrefix())(session, GP, TokIn);
}

void CallParselet_reduceCall(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    {
        auto Body = Parser_popNode(session);
        
        Parser_pushNode(session, new CallNode(Parser_popContext(session), std::move(Body)));
    }
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


ParseFunction TildeParselet::parseInfix() const {
    return TildeParselet_parseInfix;
}

Precedence TildeParselet::getPrecedence(ParserSessionPtr session) const {
    
    if (Parser_checkTilde(session)) {
        return PRECEDENCE_LOWEST;
    }
    
    return PRECEDENCE_TILDE;
}

void TildeParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  a ~f~ b
    //
    // It'd be weird if this were an "infix operator"
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto FirstTok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, FirstTok, TOPLEVEL);
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = TildeParselet_parse1;
    Ctxt.Prec = PRECEDENCE_LOWEST;
    
    auto P2 = prefixParselets[FirstTok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, FirstTok);
}

void TildeParselet_parse1(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto Tok1 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok1, TOPLEVEL, Trivia1);
    
    if (Tok1.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return TildeParselet_reduceError(session, Ignored, Ignored2);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeafAndNext(session, Tok1);

    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
    //
    // Reset back to "outside" precedence
    //
    
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == TildeParselet_parse1);
    Ctxt.F = TildeParselet_reduceTilde;
    Ctxt.Prec = PRECEDENCE_TILDE;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok2);
}

void TildeParselet_reduceTilde(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new TernaryNode(SYMBOL_CODEPARSER_TERNARYTILDE, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void TildeParselet_reduceError(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDTILDE, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, Ignored2);
}


ParseFunction ColonParselet::parseInfix() const {
    return ColonParselet_parseInfix;
}

Precedence ColonParselet::getPrecedence(ParserSessionPtr session) const {
    
    if (Parser_checkPatternPrecedence(session)) {
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
    
    return PRECEDENCE_HIGHEST;
}

void ColonParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  symbol:object  or  pattern:optional
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    auto colonLHS = Parser_checkColonLHS(session);
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    switch (colonLHS) {
        case COLONLHS_PATTERN: {
            
            auto& Ctxt = Parser_topContext(session);
            assert(!Ctxt.F);
            Ctxt.F = ColonParselet_reducePattern;
            Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
            
            auto P2 = prefixParselets[Tok.Tok.value()];
            
            MUSTTAIL
            return (P2->parsePrefix())(session, P2, Tok);
        }
        case COLONLHS_OPTIONAL: {
            
            auto& Ctxt = Parser_topContext(session);
            assert(!Ctxt.F);
            Ctxt.F = ColonParselet_reduceOptional;
            Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
            
            auto P2 = prefixParselets[Tok.Tok.value()];

            MUSTTAIL
            return (P2->parsePrefix())(session, P2, Tok);
        }
        case COLONLHS_ERROR: {
            
            auto& Ctxt = Parser_topContext(session);
            assert(!Ctxt.F);
            Ctxt.F = ColonParselet_reduceError;
            Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
            
            auto P2 = prefixParselets[Tok.Tok.value()];
            
            MUSTTAIL
            return (P2->parsePrefix())(session, P2, Tok);
        }
        default: {
            
            assert(false);
        }
    }
}

void ColonParselet_reducePattern(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_PATTERN, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void ColonParselet_reduceError(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSYMBOL, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void ColonParselet_reduceOptional(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_OPTIONAL, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence SlashColonParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_SLASHCOLON;
}

ParseFunction SlashColonParselet::parseInfix() const {
    return SlashColonParselet_parseInfix;
}

void SlashColonParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
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
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = SlashColonParselet_parse1;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void SlashColonParselet_parse1(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL, Trivia1);
    
    switch (Tok.Tok.value()) {
        case TOKEN_EQUAL.value(): {
            
            Parser_pushTriviaSeq(session, Trivia1);
            
            Parser_setPrecedence(session, PRECEDENCE_EQUAL);
            
            MUSTTAIL
            return EqualParselet_parseInfixTag(session, equalParselet, Tok);
        }
        case TOKEN_COLONEQUAL.value(): {
            
            Parser_pushTriviaSeq(session, Trivia1);
            
            Parser_setPrecedence(session, PRECEDENCE_COLONEQUAL);
            
            MUSTTAIL
            return ColonEqualParselet_parseInfixTag(session, colonEqualParselet, Tok);
        }
    } // switch
    
    Trivia1.reset(session);
    
    //
    // Anything other than:
    // a /: b = c
    // a /: b := c
    // a /: b =.
    //
    
    MUSTTAIL
    return SlashColonParselet_reduceError(session, Ignored, Ignored2);
}

void SlashColonParselet_reduceError(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new SyntaxErrorNode(SYMBOL_SYNTAXERROR_EXPECTEDSET, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


EqualParselet::EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, SYMBOL_SET) {}

ParseFunction EqualParselet::parseInfix() const {
    return EqualParselet_parseInfix;
}

void EqualParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        Parser_pushLeafAndNext(session, Tok);
        
        MUSTTAIL
        return EqualParselet_reduceUnset(session, Ignored, TokIn/*Ignored*/);
    }
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = EqualParselet_reduceSet;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void EqualParselet_parseInfixTag(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // a /: b = c  and  a /: b = .  are handled here
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_DOT) {
        
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        Parser_pushLeafAndNext(session, Tok);
        
        MUSTTAIL
        return EqualParselet_reduceTagUnset(session, Ignored, TokIn/*Ignored*/);
    }
    
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == SlashColonParselet_parse1);
    Ctxt.F = EqualParselet_reduceTagSet;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void EqualParselet_reduceSet(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_SET, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void EqualParselet_reduceUnset(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_UNSET, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void EqualParselet_reduceTagSet(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new TernaryNode(SYMBOL_TAGSET, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void EqualParselet_reduceTagUnset(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new TernaryNode(SYMBOL_TAGUNSET, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


ColonEqualParselet::ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}

ParseFunction ColonEqualParselet::parseInfix() const {
    return ColonEqualParselet_parseInfix;
}

void ColonEqualParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = ColonEqualParselet_reduceSetDelayed;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void ColonEqualParselet_parseInfixTag(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == SlashColonParselet_parse1);
    Ctxt.F = ColonEqualParselet_reduceTagSetDelayed;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void ColonEqualParselet_reduceSetDelayed(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_SETDELAYED, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void ColonEqualParselet_reduceTagSetDelayed(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new TernaryNode(SYMBOL_TAGSETDELAYED, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


ParseFunction IntegralParselet::parsePrefix() const {
    return IntegralParselet_parsePrefix;
}

void IntegralParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  \[Integral] f \[DifferentialD] x
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto& Ctxt = Parser_pushContext(session, PRECEDENCE_CLASS_INTEGRATIONOPERATORS);
    
    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL);
    
    assert(!Ctxt.F);
    Ctxt.F = IntegralParselet_parse1;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void IntegralParselet_parse1(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);

    auto Tok = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok, TOPLEVEL, Trivia1);
    
    if (!(Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.Tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD)) {
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return IntegralParselet_reduceIntegral(session, Ignored, Ignored2);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == IntegralParselet_parse1);
    Ctxt.F = IntegralParselet_reduceIntegrate;
    
    auto P2 = prefixParselets[Tok.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(session, P2, Tok);
}

void IntegralParselet_reduceIntegrate(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new PrefixBinaryNode(SYMBOL_INTEGRATE, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}

void IntegralParselet_reduceIntegral(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new PrefixNode(SYMBOL_INTEGRAL, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence CommaParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_COMMA;
}

ParseFunction CommaParselet::parseInfix() const {
    return CommaParselet_parseInfix;
}

void CommaParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
    if (Tok2.Tok == TOKEN_COMMA || Tok2.Tok == TOKEN_LONGNAME_INVISIBLECOMMA) {
        
        //
        // Something like  a,,
        //
        
        Parser_pushLeaf(session, Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.Buf, Tok2.Src.Start));
        
#if !USE_MUSTTAIL
        auto& Ctxt = Parser_topContext(session);
        assert(!Ctxt.F);
        Ctxt.F = Parser_identity;
        
        return CommaParselet_parseLoop(session, Ignored, TokIn/*ignored*/);
#else
        auto& Ctxt = Parser_topContext();
        assert(!Ctxt.F);
        Ctxt.F = CommaParselet_parseLoop;
        
        MUSTTAIL
        return CommaParselet_parseLoop(Ignored, TokIn/*ignored*/);
#endif // !USE_MUSTTAIL
    }
    
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(!Ctxt.F);
    Ctxt.F = Parser_identity;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    (P2->parsePrefix())(session, P2, Tok2);
    
    return CommaParselet_parseLoop(session, Ignored, TokIn/*ignored*/);
#else
    auto& Ctxt = Parser_topContext();
    assert(!Ctxt.F);
    Ctxt.F = CommaParselet_parseLoop;
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void CommaParselet_parseLoop(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);

    auto Tok1 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok1, TOPLEVEL, Trivia1);
    
    if (!(Tok1.Tok == TOKEN_COMMA || Tok1.Tok == TOKEN_LONGNAME_INVISIBLECOMMA)) {
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return CommaParselet_reduceComma(session, Ignored, Ignored2);
    }

    //
    // Something like  a,b
    //
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeafAndNext(session, Tok1);

    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok2, TOPLEVEL);
    
    if (Tok2.Tok == TOKEN_COMMA || Tok2.Tok == TOKEN_LONGNAME_INVISIBLECOMMA) {

        //
        // Something like  a,,
        //
        
        Parser_pushLeaf(session, Token(TOKEN_ERROR_INFIXIMPLICITNULL, Tok2.Buf, Tok2.Src.Start));
        
#if !USE_MUSTTAIL
        continue;
#else
        MUSTTAIL
        return CommaParselet_parseLoop(Ignored, Ignored2);
#endif // !USE_MUSTTAIL
    }
        
#if !USE_MUSTTAIL
    auto& Ctxt = Parser_topContext(session);
    assert(Ctxt.F == Parser_identity);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
        
    (P2->parsePrefix())(session, P2, Tok2);
        
    } // while (true)
#else
    auto& Ctxt = Parser_topContext();
    assert(Ctxt.F == CommaParselet_parseLoop);
    
    auto P2 = prefixParselets[Tok2.Tok.value()];
    
    MUSTTAIL
    return (P2->parsePrefix())(P2, Tok2);
#endif // !USE_MUSTTAIL
}

void CommaParselet_reduceComma(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new InfixNode(SYMBOL_CODEPARSER_COMMA, Parser_popContext(session)));
    
    //
    // was:
    //
//    MUSTTAIL
//    return Parser_parseClimb(Ignored, Ignored2);
    
    //
    // but take advantage of fact that Comma has lowest operator precedence and there is nothing after a,b,c that will continue that expression
    //
    // so call Parser_tryContinue directly
    //
    
    MUSTTAIL
    return Parser_tryContinue(session, Ignored, Ignored2);
}


Precedence SemiParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_SEMI;
}

ParseFunction SemiParselet::parseInfix() const {
    return SemiParselet_parseInfix;
}

void SemiParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    
    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    //
    // CompoundExpression should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, Tok2, TOPLEVEL);
    
    if (Tok2.Tok == TOKEN_SEMI) {
        
        //
        // Something like  a; ;
        //
        
        Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITNULL, Tok2.Buf, Tok2.Src.Start));
        
        //
        // nextToken() is not needed after an implicit token
        //
        
#if !USE_MUSTTAIL
        auto& Ctxt = Parser_topContext(session);
        assert(!Ctxt.F);
        Ctxt.F = Parser_identity;
        
        return SemiParselet_parseLoop(session, Ignored, TokIn/*ignored*/);
#else
        auto& Ctxt = Parser_topContext();
        assert(!Ctxt.F);
        Ctxt.F = SemiParselet_parseLoop;
        
        MUSTTAIL
        return SemiParselet_parseLoop(Ignored, TokIn/*ignored*/);
#endif // !USE_MUSTTAIL
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        //
        // Something like  a;+2
        //
        
#if !USE_MUSTTAIL
        auto& Ctxt = Parser_topContext(session);
        assert(!Ctxt.F);
        Ctxt.F = Parser_identity;
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        (P2->parsePrefix())(session, P2, Tok2);
        
        return SemiParselet_parseLoop(session, Ignored, TokIn/*ignored*/);
#else
        auto& Ctxt = Parser_topContext();
        assert(!Ctxt.F);
        Ctxt.F = SemiParselet_parseLoop;
        
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
    
    Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITNULL, Tok2.Buf, Tok2.Src.Start));
    
    //
    // nextToken() is not needed after an implicit token
    //
    
    MUSTTAIL
    return SemiParselet_reduceCompoundExpression(session, Ignored, TokIn/*Ignored*/);
}

void SemiParselet_parseLoop(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto Tok1 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok1, TOPLEVEL, Trivia1);
    
    if (Tok1.Tok != TOKEN_SEMI) {
        
        //
        // Something like  a;b
        //
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return SemiParselet_reduceCompoundExpression(session, Ignored, Ignored2);
    }
    
    //
    // Something like  a;b
    //
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeafAndNext(session, Tok1);

    auto Tok2 = Parser_currentToken(session, TOPLEVEL);
    
    //
    // CompoundExpression should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, Tok2, TOPLEVEL);
    
    if (Tok2.Tok == TOKEN_SEMI) {

        //
        // Something like  a;b; ;
        //
        
        Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITNULL, Tok2.Buf, Tok2.Src.Start));
        
        //
        // nextToken() is not needed after an implicit token
        //
        
#if !USE_MUSTTAIL
        continue;
#else
        MUSTTAIL
        return SemiParselet_parseLoop(Ignored, Ignored2);
#endif // !USE_MUSTTAIL
    }
    
    if (Tok2.Tok.isPossibleBeginning()) {
        
        //
        // Something like  a;b;+2
        //
        
#if !USE_MUSTTAIL
        auto& Ctxt = Parser_topContext(session);
        assert(Ctxt.F == Parser_identity);
        
        auto P2 = prefixParselets[Tok2.Tok.value()];
        
        (P2->parsePrefix())(session, P2, Tok2);
        
        continue;
#else
        auto& Ctxt = Parser_topContext();
        assert(Ctxt.F == SemiParselet_parseLoop);
        
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
    
    Parser_pushLeaf(session, Token(TOKEN_FAKE_IMPLICITNULL, Tok2.Buf, Tok2.Src.Start));
    
    //
    // nextToken() is not needed after an implicit token
    //
    
    MUSTTAIL
    return SemiParselet_reduceCompoundExpression(session, Ignored, Ignored2);
        
#if !USE_MUSTTAIL
    } // while (true)
#endif // !USE_MUSTTAIL
}

void SemiParselet_reduceCompoundExpression(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new InfixNode(SYMBOL_COMPOUNDEXPRESSION, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence ColonColonParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_COLONCOLON;
}

ParseFunction ColonColonParselet::parseInfix() const {
    return ColonColonParselet_parseInfix;
}

void ColonColonParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // a::b
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    //
    // Special tokenization, so must do parsing here
    //
    
    auto Tok2 = Parser_currentToken_stringifyAsTag(session);
    
    Parser_pushLeafAndNext(session, Tok2);
    
    MUSTTAIL
    return ColonColonParselet_parseLoop(session, Ignored, TokIn/*ignored*/);
}

void ColonColonParselet_parseLoop(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
#if !USE_MUSTTAIL
    while (true) {
#endif // !USE_MUSTTAIL
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popNode(session);
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto Tok1 = Parser_currentToken(session, TOPLEVEL);
    
    Parser_eatTrivia(session, Tok1, TOPLEVEL, Trivia1);
    
    if (Tok1.Tok != TOKEN_COLONCOLON) {
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return ColonColonParselet_reduceMessageName(session, Ignored, Ignored2);
    }
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    Parser_pushLeafAndNext(session, Tok1);
    
    //
    // Special tokenization, so must do parsing here
    //

    auto Tok2 = Parser_currentToken_stringifyAsTag(session);

    Parser_pushLeafAndNext(session, Tok2);
    
#if !USE_MUSTTAIL
    } // while (true)
#else
    MUSTTAIL
    return ColonColonParselet_parseLoop(Ignored, Ignored2);
#endif // !USE_MUSTTAIL
}

void ColonColonParselet_reduceMessageName(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new InfixNode(SYMBOL_MESSAGENAME, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence GreaterGreaterParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_GREATERGREATER;
}

ParseFunction GreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterParselet_parseInfix;
}

void GreaterGreaterParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // a>>b
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    //
    // Special tokenization, so must do parsing here
    //
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken_stringifyAsFile(session);
    
    Parser_eatTrivia_stringifyAsFile(session, Tok);
    
    Parser_pushLeafAndNext(session, Tok);
    
    MUSTTAIL
    return GreaterGreaterParselet_reducePut(session, Ignored, TokIn/*Ignored*/);
}

void GreaterGreaterParselet_reducePut(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_PUT, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence GreaterGreaterGreaterParselet::getPrecedence(ParserSessionPtr session) const {
    return PRECEDENCE_GREATERGREATERGREATER;
}

ParseFunction GreaterGreaterGreaterParselet::parseInfix() const {
    return GreaterGreaterGreaterParselet_parseInfix;
}

void GreaterGreaterGreaterParselet_parseInfix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // a>>>b
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_popContext(session);
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    //
    // Special tokenization, so must do parsing here
    //
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken_stringifyAsFile(session);
    
    Parser_eatTrivia_stringifyAsFile(session, Tok);
    
    Parser_pushLeafAndNext(session, Tok);
    
    MUSTTAIL
    return GreaterGreaterGreaterParselet_reducePutAppend(session, Ignored, TokIn/*Ignored*/);
}

void GreaterGreaterGreaterParselet_reducePutAppend(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new BinaryNode(SYMBOL_PUTAPPEND, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


ParseFunction LessLessParselet::parsePrefix() const {
    return LessLessParselet_parsePrefix;
}

void LessLessParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // <<a
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    //
    // Special tokenization, so must do parsing here
    //
    
    Parser_pushLeafAndNext(session, TokIn);
    
    Parser_pushContext(session, PRECEDENCE_HIGHEST);
    
    auto Tok = Parser_currentToken_stringifyAsFile(session);
    
    Parser_eatTrivia_stringifyAsFile(session, Tok);
    
    Parser_pushLeafAndNext(session, Tok);
    
    MUSTTAIL
    return LessLessParselet_reduceGet(session, Ignored, TokIn/*Ignored*/);
}

void LessLessParselet_reduceGet(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new PrefixNode(SYMBOL_GET, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


Precedence InfixDifferentialDParselet::getPrecedence(ParserSessionPtr session) const {
    
    if (Parser_topPrecedence(session) == PRECEDENCE_CLASS_INTEGRATIONOPERATORS) {

        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //

        return PRECEDENCE_LOWEST;
    }

    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

Token InfixDifferentialDParselet::processImplicitTimes(ParserSessionPtr session, Token TokIn) const {
    
    if (Parser_topPrecedence(session) == PRECEDENCE_CLASS_INTEGRATIONOPERATORS) {

        //
        // Inside \[Integral], so \[DifferentialD] is treated specially
        //

        return TokIn;
    }
    
    return Token(TOKEN_FAKE_IMPLICITTIMES, TokIn.Buf, TokIn.Src.Start);
}

ParseFunction InfixDifferentialDParselet::parseInfix() const {
    
    assert(false);
    
    return nullptr;
}


ParseFunction HashParselet::parsePrefix() const {
    return HashParselet_parsePrefix;
}

void HashParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
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
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, INSIDE_SLOT);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value():
        case TOKEN_STRING.value(): {
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            Parser_pushLeafAndNext(session, Tok);
            
            MUSTTAIL
            return HashParselet_reduceSlot(session, Ignored, TokIn/*Ignored*/);
        }
    }
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}

void HashParselet_reduceSlot(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new CompoundNode(SYMBOL_SLOT, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


ParseFunction HashHashParselet::parsePrefix() const {
    return HashHashParselet_parsePrefix;
}

void HashHashParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  ##  or  ##1
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, INSIDE_SLOTSEQUENCE);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            Parser_pushLeafAndNext(session, Tok);
            
            MUSTTAIL
            return HashHashParselet_reduceSlotSequence(session, Ignored, TokIn/*ignored*/);
        }
    }
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}

void HashHashParselet_reduceSlotSequence(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new CompoundNode(SYMBOL_SLOTSEQUENCE, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}


ParseFunction PercentParselet::parsePrefix() const {
    return PercentParselet_parsePrefix;
}

void PercentParselet_parsePrefix(ParserSessionPtr session, ParseletPtr Ignored, Token TokIn) {
    
    //
    // Something like  %  or  %1
    //
    
#if CHECK_ABORT
    if (session->abortQ()) {
        Parser_pushNode(session, new AbortNode());
        return Parser_tryContinue(session, Ignored, TokIn/*ignored*/);
    }
#endif // CHECK_ABORT
    
    Parser_pushLeafAndNext(session, TokIn);
    
    auto Tok = Parser_currentToken(session, INSIDE_OUT);
    
    switch (Tok.Tok.value()) {
        case TOKEN_INTEGER.value(): {
            
            Parser_pushContext(session, PRECEDENCE_HIGHEST);
            
            Parser_pushLeafAndNext(session, Tok);
            
            MUSTTAIL
            return PercentParselet_reduceOut(session, Ignored, TokIn/*ignored*/);
        }
    }
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, TokIn/*ignored*/);
}

void PercentParselet_reduceOut(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    Parser_pushNode(session, new CompoundNode(SYMBOL_OUT, Parser_popContext(session)));
    
    MUSTTAIL
    return Parser_parseClimb(session, Ignored, Ignored2);
}
