
#include "Parser.h"

#include "ParserSession.h"
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
#include "ParseletRegistration.h"
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


Context::Context(size_t Index, Precedence Prec) : F(), P(), Index(Index), Prec(Prec) {}


void Parser_handleFirstLine(ParserSessionPtr session) {
    
    auto firstLineBehavior = session->firstLineBehavior;
    
    switch (firstLineBehavior) {
        case FIRSTLINEBEHAVIOR_NOTSCRIPT: {
            return;
        }
        case FIRSTLINEBEHAVIOR_CHECK: {
            
            //
            // Handle the optional #! shebang
            //
            
            auto peek = Parser_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_HASH) {
                
                // not #!
                
                //
                // reset
                //
                session->buffer = peek.Buf;
                session->SrcLoc = peek.Src.Start;
                
                return;
            }
            
            Parser_nextToken(session, peek);
            
            peek = Parser_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_BANG) {
                
                // not #!
                
                //
                // reset
                //
                session->buffer = peek.Buf;
                session->SrcLoc = peek.Src.Start;
                
                return;
            }
            
            //
            // Definitely a shebang
            //
            
            Parser_nextToken(session, peek);
            
            while (true) {
                
#if CHECK_ABORT
                if (session->abortQ()) {
                    break;
                }
#endif // CHECK_ABORT
                
                auto peek = Parser_currentToken(session, TOPLEVEL);
                
                if (peek.Tok == TOKEN_ENDOFFILE) {
                    break;
                }
                
                if (peek.Tok == TOKEN_TOPLEVELNEWLINE) {
                    
                    Parser_nextToken(session, peek);
                    
                    break;
                }
                
                Parser_nextToken(session, peek);
                
            } // while (true)
            
            //
            // TODO: if anyone ever asks, then consider providing the shebang as a token
            // but only after BIGCODEMERGE!!
            //
            
            break;
        }
        case FIRSTLINEBEHAVIOR_SCRIPT: {
            
            //
            // Handle the #! shebang
            //
            
            auto peek = Parser_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_HASH) {
                
                //
                // TODO: add to Issues
                //

                return;
            }
            
            Parser_nextToken(session, peek);
            
            peek = Parser_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_BANG) {
                
                //
                // TODO: add to Issues
                //

                return;
            }
            
            Parser_nextToken(session, peek);
            
            while (true) {
                
#if CHECK_ABORT
                if (session->abortQ()) {
                    break;
                }
#endif // CHECK_ABORT
                
                auto peek = Parser_currentToken(session, TOPLEVEL);
                
                if (peek.Tok == TOKEN_ENDOFFILE) {
                    break;
                }
                
                if (peek.Tok == TOKEN_TOPLEVELNEWLINE) {
                    
                    Parser_nextToken(session, peek);
                    
                    break;
                }
                
                Parser_nextToken(session, peek);
                
            } // while (true)
            
            break;
        }
    }
}

void Parser_nextToken(ParserSessionPtr session, Token Tok) {
    Tokenizer_nextToken(session, Tok);
}

Token Parser_nextToken0(ParserSessionPtr session, NextPolicy policy) {
    
    auto insideGroup = !session->GroupStack.empty();
    
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    
    return Tokenizer_nextToken0(session, policy & ~(returnInternalNewlineMask));
}

Token Parser_currentToken(ParserSessionPtr session, NextPolicy policy) {
    
    auto insideGroup = !session->GroupStack.empty();
    
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    
    return Tokenizer_currentToken(session, policy & ~(returnInternalNewlineMask));
}


Token Parser_currentToken_stringifyAsTag(ParserSessionPtr session) {
    return Tokenizer_currentToken_stringifyAsTag(session);
}

Token Parser_currentToken_stringifyAsFile(ParserSessionPtr session) {
    return Tokenizer_currentToken_stringifyAsFile(session);
}

void Parser_parseClimb(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    //
    // Check isAbort() inside loops
    //
#if CHECK_ABORT
    if (session->abortQ()) {
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = Parser_getTrivia1(session);
    
    auto token = Parser_currentToken(session, TOPLEVEL);
    
    //
    // not in the middle of parsing anything, so toplevel newlines will delimit
    //
    Parser_eatTriviaButNotToplevelNewlines(session, token, TOPLEVEL, Trivia1);
    
    auto I = infixParselets[token.Tok.value()];
        
    token = I->processImplicitTimes(session, token);
        
    I = infixParselets[token.Tok.value()];
    
    auto TokenPrecedence = I->getPrecedence(session);
    
    //
    // if (Ctxt.Prec > TokenPrecedence)
    //   break;
    // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
    //   break;
    //
    
    if ((Parser_topPrecedence(session) | 0x1) > TokenPrecedence) {
        
        Trivia1.reset(session);
        
        MUSTTAIL
        return Parser_tryContinue(session, Ignored, Ignored2);
    }
    
    Parser_pushContext(session, TokenPrecedence);
    
    Parser_pushTriviaSeq(session, Trivia1);
    
    MUSTTAIL
    return (I->parseInfix())(session, I, token);
}

void Parser_tryContinue(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2) {
    
    if (Parser_isContextStackEmpty(session)) {
        
        // no call needed here
        return;
    }

    auto& Ctxt = Parser_topContext(session);

    auto F = Ctxt.F;
    auto P = Ctxt.P;

    assert(F);
    
    MUSTTAIL
    return F(session, P, Ignored2);
}

void Parser_identity(ParserSessionPtr session, ParseletPtr P, Token firstTok) {
    return;
}

void Parser_eatTrivia(ParserSessionPtr session, Token& T, NextPolicy policy) {
    
    while (T.Tok.isTrivia()) {
        
        session->NodeStack.push_back(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken(session, policy);
    }
}

void Parser_eatTrivia(ParserSessionPtr session, Token& T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        Args.push(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken(session, policy);
    }
}

void Parser_eatTrivia_stringifyAsFile(ParserSessionPtr session, Token& T) {
    
    while (T.Tok.isTrivia()) {
        
        session->NodeStack.push_back(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken_stringifyAsFile(session);
    }
}

void Parser_eatTrivia_stringifyAsFile(ParserSessionPtr session, Token& T, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        Args.push(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken_stringifyAsFile(session);
    }
}

void Parser_eatTriviaButNotToplevelNewlines(ParserSessionPtr session, Token& T, NextPolicy policy) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        session->NodeStack.push_back(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken(session, policy);
    }
}

void Parser_eatTriviaButNotToplevelNewlines(ParserSessionPtr session, Token& T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        Args.push(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken(session, policy);
    }
}

void Parser_eatTriviaButNotToplevelNewlines_stringifyAsFile(ParserSessionPtr session, Token& T, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        Args.push(T);
        
        Parser_nextToken(session, T);
        
        T = Parser_currentToken_stringifyAsFile(session);
    }
}

Context& Parser_pushContext(ParserSessionPtr session, Precedence Prec) {
    
    assert(!session->NodeStack.empty());
    
    session->ContextStack.emplace_back(session->NodeStack.size() - 1, Prec);
    
    return session->ContextStack.back();
}

NodeSeq Parser_popContext(ParserSessionPtr session) {
    
    assert(!session->ContextStack.empty());
    
    //
    // get the top Context
    //
    
    auto Ctxt = session->ContextStack.back();
    
    session->ContextStack.pop_back();
    
    //
    // Copy args from back of NodeStack to ArgsTmp
    //
    
    NodeSeq ArgsTmp{session->NodeStack.begin() + Ctxt.Index, session->NodeStack.end()};
    
    //
    // forget about the moved args
    //
    
    session->NodeStack.resize(Ctxt.Index);
    
    //
    // return ArgsTmp
    //
    
    return ArgsTmp;
}

bool Parser_isContextStackEmpty(ParserSessionPtr session) {
    return session->ContextStack.empty();
}

Context& Parser_topContext(ParserSessionPtr session) {
    
    assert(!session->ContextStack.empty());
    
    return session->ContextStack.back();
}

Precedence Parser_topPrecedence(ParserSessionPtr session) {
    
    if (session->ContextStack.empty()) {
        return PRECEDENCE_LOWEST;
    }
    
    return session->ContextStack.back().Prec;
}

void Parser_setPrecedence(ParserSessionPtr session, Precedence Prec) {
    
    assert(!session->ContextStack.empty());
    
    auto& Ctxt = session->ContextStack.back();
    
    Ctxt.Prec = Prec;
}


void Parser_pushLeaf(ParserSessionPtr session, Token T) {
    session->NodeStack.push_back(T);
}

void Parser_pushLeafAndNext(ParserSessionPtr session, Token Tok) {
    
    session->NodeStack.push_back(Tok);
    
    Tokenizer_nextToken(session, Tok);
}

void Parser_pushTriviaSeq(ParserSessionPtr session, TriviaSeq& Seq) {
    
    //
    // Move all trivia from Seq to back of ArgsStack
    //
    
    std::move(Seq.vec.begin(), Seq.vec.end(), std::back_inserter(session->NodeStack));
    
    //
    // Forget about Seq
    //
    
    Seq.vec.clear();
}

void Parser_pushNode(ParserSessionPtr session, Node *N) {
    session->NodeStack.push_back(N);
}

bool Parser_isNodeStackEmpty(ParserSessionPtr session) {
    return session->NodeStack.empty();
}

NodeVariant& Parser_topNode(ParserSessionPtr session) {
    
    assert(!session->NodeStack.empty());
    
    return session->NodeStack.back();
}

NodeVariant Parser_popNode(ParserSessionPtr session) {
    
    assert(!session->NodeStack.empty());
    
    auto top = session->NodeStack.back();
    
    session->NodeStack.pop_back();
    
    return top;
}

void Parser_pushGroup(ParserSessionPtr session, Closer Closr) {
    session->GroupStack.push_back(Closr);
}

void Parser_popGroup(ParserSessionPtr session) {
    
    assert(!session->GroupStack.empty());
    
    session->GroupStack.pop_back();
}

size_t Parser_getGroupDepth(ParserSessionPtr session) {
    return session->GroupStack.size();
}

bool Parser_checkGroup(ParserSessionPtr session, Closer Closr) {
    
    for (auto rit = session->GroupStack.rbegin(); rit != session->GroupStack.rend(); rit++) {
        if (*rit == Closr) {
            return true;
        }
    }
    
    return false;
}


bool Parser_checkPatternPrecedence(ParserSessionPtr session) {
    
    for (auto rit = session->ContextStack.rbegin(); rit != session->ContextStack.rend(); rit++) {

        auto& Ctxt = *rit;

        auto Prec = Ctxt.Prec;
        
        if (Prec > PRECEDENCE_FAKE_PATTERNCOLON) {
            continue;
        }
        
        if (Prec < PRECEDENCE_FAKE_PATTERNCOLON) {
            return false;
        }
        
        assert(Prec == PRECEDENCE_FAKE_PATTERNCOLON);
        
        return true;
    }
    
    return false;
}

ColonLHS Parser_checkColonLHS(ParserSessionPtr session) {

    //
    // work backwards, looking for a symbol or something that is a pattern
    //
    
    //
    // skip any trivia
    //
    
    auto Ctxt = session->ContextStack.back();
    
    int i;
    for (i = static_cast<int>(session->NodeStack.size())-1; i >= static_cast<int>(Ctxt.Index); i--) {

        auto& N = session->NodeStack[i];

        if (std::holds_alternative<Token>(N)) {
            
            auto& Tok = std::get<Token>(N);

            if (Tok.Tok.isTrivia()) {
                continue;
            }

            break;
        }

        break;
    }

    if (i == static_cast<int>(Ctxt.Index) - 1) {
        
        assert(false);
        
        return COLONLHS_NONE;
    }

    auto& N = session->NodeStack[i];
    
    if (std::holds_alternative<NodePtr>(N)) {
        
        auto& P = std::get<NodePtr>(N);
        
        if (auto B = dynamic_cast<BinaryNode *>(P)) {
            
            //
            // Something like  a:b:c
            //                  ^ Pattern
            //                    ^ Optional
            //
            
            auto Op = B->getOp();

            if (Op == SYMBOL_PATTERN) {
                return COLONLHS_OPTIONAL;
            }

            return COLONLHS_ERROR;
        }

        if (auto C = dynamic_cast<CompoundNode *>(P)) {
            
            //
            // Something like  a_:b
            //                   ^ Optional
            //
            
            auto Op = C->getOp();

            switch (Op.getId()) {
                case SYMBOL_CODEPARSER_PATTERNBLANK.getId():
                case SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE.getId():
                case SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE.getId():
                case SYMBOL_BLANK.getId():
                case SYMBOL_BLANKSEQUENCE.getId():
                case SYMBOL_BLANKNULLSEQUENCE.getId(): {
                    return COLONLHS_OPTIONAL;
                }
            }

            return COLONLHS_ERROR;
        }
        
    } else {
        
        assert(std::holds_alternative<Token>(N));
        
        auto& Tok = std::get<Token>(N);

        switch (Tok.Tok.value()) {
            case TOKEN_SYMBOL.value(): {
                
                //
                // Something like  a:b
                //                  ^ Pattern
                //
                
                return COLONLHS_PATTERN;
            }
            case TOKEN_UNDER.value():
            case TOKEN_UNDERUNDER.value():
            case TOKEN_UNDERUNDERUNDER.value(): {
                
                //
                // Something like  _:b
                //                  ^ Optional
                //
                
                return COLONLHS_OPTIONAL;
            }
            case TOKEN_COLON.value(): {
                assert(false && "Fix at call site");
                return COLONLHS_ERROR;
            }
        }
        
        if (Tok.Tok.isError()) {
            
            //
            // allow errors to be on LHS of :
            //
            // This is a bit confusing. The thinking is that since there is already an error, then we do not need to introduce another error.
            //
            return COLONLHS_PATTERN;
        }
        
        return COLONLHS_ERROR;
    }

    return COLONLHS_ERROR;
}

bool Parser_checkTilde(ParserSessionPtr session) {

    //
    // work backwards, looking for ~
    //
    
    if (session->ContextStack.empty()) {
        return false;
    }
    
    auto Ctxt = session->ContextStack.back();
    
    int i = static_cast<int>(session->NodeStack.size()) - 1;
    
    //
    // skip past top
    //
    i--;
    
    //
    // skip any trivia
    //
    for (; i >= static_cast<int>(Ctxt.Index); i--) {

        auto& N = session->NodeStack[i];

        if (std::holds_alternative<Token>(N)) {

            auto& Tok = std::get<Token>(N);

            if (Tok.Tok.isTrivia()) {
                continue;
            }

            break;
        }

        break;
    }

    if (i == static_cast<int>(Ctxt.Index)-1) {
        
        assert(false);
        
        return false;
    }
    
    auto& N = session->NodeStack[i];

    if (std::holds_alternative<Token>(N)) {
        
        auto& Tok = std::get<Token>(N);

        if (Tok.Tok == TOKEN_TILDE) {
            return true;
        }
    }

    return false;
}

bool Parser_checkSpan(ParserSessionPtr session) {
    
    assert(!session->NodeStack.empty());
    
    auto& N = session->NodeStack.back();
    
    if (std::holds_alternative<NodePtr>(N)) {
        
        auto& NN = std::get<NodePtr>(N);
        
        if (auto B = dynamic_cast<BinaryNode *>(NN)) {
            
            auto Op = B->getOp();
            
            if (Op == SYMBOL_SPAN) {
                
                return true;
            }
            
            //
            // there is a BinaryNode, but it is not a Span
            //
            
            return false;
        }
        
        if (auto T = dynamic_cast<TernaryNode *>(NN)) {
            
            auto Op = T->getOp();
            
            if (Op == SYMBOL_SPAN) {
                
                return true;
            }
            
            //
            // there is a TernaryNode, but it is not a Span
            //
            
            return false;
        }
    }
    
    return false;
}

TriviaSeq& Parser_getTrivia1(ParserSessionPtr session) {
    return session->trivia1;
}

TriviaSeq& Parser_getTrivia2(ParserSessionPtr session) {
    return session->trivia2;
}

bool Parser_isQuiescent(ParserSessionPtr session) {
    
    assert(session->NodeStack.empty());
    assert(session->ContextStack.empty());
    assert(session->GroupStack.empty());
    assert(session->trivia1.empty());
    assert(session->trivia2.empty());
    
    return true;
}
