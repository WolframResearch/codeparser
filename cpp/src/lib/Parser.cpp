
#include "Parser.h"

#include "ParserSession.h"
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
#include "ParseletRegistration.h"
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer
#include "SymbolRegistration.h"

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


bool Parser_isContextStackEmpty(ParserSessionPtr session);


Context::Context(size_t Index, Precedence Prec) : F(), P(), Index(Index), Prec(Prec) {}


void Parser_handleFirstLine(ParserSessionPtr session) {
    
    auto firstLineBehavior = session->opts.firstLineBehavior;
    
    switch (firstLineBehavior) {
        case FIRSTLINEBEHAVIOR_NOTSCRIPT: {
            return;
        }
        case FIRSTLINEBEHAVIOR_CHECK: {
            
            //
            // Handle the optional #! shebang
            //
            
            auto peek = Tokenizer_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_HASH) {
                
                // not #!
                
                //
                // reset
                //
                session->buffer = peek.Buf;
                session->SrcLoc = peek.Src.Start;
                
                return;
            }
            
            peek.skip(session);
            
            peek = Tokenizer_currentToken(session, TOPLEVEL);
            
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
            
            peek.skip(session);
            
            while (true) {
                
#if CHECK_ABORT
                if (session->abortQ()) {
                    break;
                }
#endif // CHECK_ABORT
                
                auto peek = Tokenizer_currentToken(session, TOPLEVEL);
                
                if (peek.Tok == TOKEN_ENDOFFILE) {
                    break;
                }
                
                if (peek.Tok == TOKEN_TOPLEVELNEWLINE) {
                    
                    peek.skip(session);
                    
                    break;
                }
                
                peek.skip(session);
                
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
            
            auto peek = Tokenizer_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_HASH) {
                
                //
                // TODO: add to Issues
                //

                return;
            }
            
            peek.skip(session);
            
            peek = Tokenizer_currentToken(session, TOPLEVEL);
            
            if (peek.Tok != TOKEN_BANG) {
                
                //
                // TODO: add to Issues
                //

                return;
            }
            
            peek.skip(session);
            
            while (true) {
                
#if CHECK_ABORT
                if (session->abortQ()) {
                    break;
                }
#endif // CHECK_ABORT
                
                auto peek = Tokenizer_currentToken(session, TOPLEVEL);
                
                if (peek.Tok == TOKEN_ENDOFFILE) {
                    break;
                }
                
                if (peek.Tok == TOKEN_TOPLEVELNEWLINE) {
                    
                    peek.skip(session);
                    
                    break;
                }
                
                peek.skip(session);
                
            } // while (true)
            
            break;
        }
    }
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
    
    auto& Trivia1 = session->trivia1;
    
    auto token = Tokenizer_currentToken(session, TOPLEVEL);
    
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
        
        T.skip(session);
        
        T = Tokenizer_currentToken(session, policy);
    }
}

void Parser_eatTrivia(ParserSessionPtr session, Token& T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        Args.push(T);
        
        T.skip(session);
        
        T = Tokenizer_currentToken(session, policy);
    }
}

void Parser_eatTrivia_stringifyAsFile(ParserSessionPtr session, Token& T) {
    
    while (T.Tok.isTrivia()) {
        
        session->NodeStack.push_back(T);
        
        T.skip(session);
        
        T = Tokenizer_currentToken_stringifyAsFile(session);
    }
}

void Parser_eatTriviaButNotToplevelNewlines(ParserSessionPtr session, Token& T, NextPolicy policy) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        session->NodeStack.push_back(T);
        
        T.skip(session);
        
        T = Tokenizer_currentToken(session, policy);
    }
}

void Parser_eatTriviaButNotToplevelNewlines(ParserSessionPtr session, Token& T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        Args.push(T);
        
        T.skip(session);
        
        T = Tokenizer_currentToken(session, policy);
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
    
    Tok.skip(session);
}

void Parser_pushTriviaSeq(ParserSessionPtr session, TriviaSeq& Seq) {
    
    //
    // Move all trivia from Seq to back of ArgsStack
    //
    
    std::move(Seq.begin(), Seq.end(), std::back_inserter(session->NodeStack));
    
    //
    // Forget about Seq
    //
    
    Seq.clear();
}

void Parser_pushNode(ParserSessionPtr session, Node *N) {
    session->NodeStack.push_back(N);
}

NodeVariant Parser_popNode(ParserSessionPtr session) {
    
    assert(!session->NodeStack.empty());
    
    auto top = session->NodeStack.back();
    
    session->NodeStack.pop_back();
    
    return top;
}

NodeVariant& Parser_topNode(ParserSessionPtr session) {
    
    assert(!session->NodeStack.empty());
    
    return session->NodeStack.back();
}

void Parser_pushGroup(ParserSessionPtr session, Closer Closr) {
    session->GroupStack.push_back(Closr);
}

void Parser_popGroup(ParserSessionPtr session) {
    
    assert(!session->GroupStack.empty());
    
    session->GroupStack.pop_back();
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
        
        if (auto B = dynamic_cast<const BinaryNode *>(P)) {
            
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

        if (auto C = dynamic_cast<const CompoundNode *>(P)) {
            
            //
            // Something like  a_:b
            //                   ^ Optional
            //
            
            auto Op = C->getOp();

            switch (Op.Id) {
                case SYMBOL_CODEPARSER_PATTERNBLANK.Id:
                case SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE.Id:
                case SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE.Id:
                case SYMBOL_BLANK.Id:
                case SYMBOL_BLANKSEQUENCE.Id:
                case SYMBOL_BLANKNULLSEQUENCE.Id: {
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
        
        if (auto B = dynamic_cast<const BinaryNode *>(NN)) {
            
            auto Op = B->getOp();
            
            if (Op == SYMBOL_SPAN) {
                
                return true;
            }
            
            //
            // there is a BinaryNode, but it is not a Span
            //
            
            return false;
        }
        
        if (auto T = dynamic_cast<const TernaryNode *>(NN)) {
            
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

bool Parser_isQuiescent(ParserSessionPtr session) {
    
    assert(session->NodeStack.empty());
    assert(session->ContextStack.empty());
    assert(session->GroupStack.empty());
    assert(session->trivia1.empty());
    assert(session->trivia2.empty());
    
    return true;
}
