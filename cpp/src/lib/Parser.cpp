
#include "Parser.h"

#include "ParserSession.h"
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
#include "ParseletRegistration.h"
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer

#include <algorithm>

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


Context::Context(size_t Index, Precedence Prec) : F(), P(), Index(Index), Prec(Prec) {}


Parser::Parser() : ArgsStack(), ContextStack(), NodeStack(), GroupStack(), trivia1(), trivia2() {}

void Parser::init() {
    
    handleFirstLine(TheParserSession->firstLineBehavior);
}

void Parser::handleFirstLine(FirstLineBehavior firstLineBehavior) {
    
    switch (firstLineBehavior) {
        case FIRSTLINEBEHAVIOR_NOTSCRIPT: {
            
            return;
        }
        case FIRSTLINEBEHAVIOR_CHECK: {
            
            //
            // Handle the optional #! shebang
            //
            
            auto peek = currentToken(TOPLEVEL);
            
            if (peek.Tok != TOKEN_HASH) {
                
                // not #!
                
                //
                // reset
                //
                TheByteBuffer->buffer = peek.BufLen.buffer;
                TheByteDecoder->SrcLoc = peek.Src.Start;
                
                return;
            }
            
            nextToken(peek);
            
            peek = currentToken(TOPLEVEL);
            
            if (peek.Tok != TOKEN_BANG) {
                
                // not #!
                
                //
                // reset
                //
                TheByteBuffer->buffer = peek.BufLen.buffer;
                TheByteDecoder->SrcLoc = peek.Src.Start;
                
                return;
            }
            
            //
            // Definitely a shebang
            //
            
            nextToken(peek);
            
            while (true) {
                
#if CHECK_ABORT
                if (TheParserSession->isAbort()) {
                    
                    break;
                }
#endif // CHECK_ABORT
                
                auto peek = currentToken(TOPLEVEL);
                
                if (peek.Tok == TOKEN_ENDOFFILE) {
                    
                    break;
                }
                
                if (peek.Tok == TOKEN_TOPLEVELNEWLINE) {
                    
                    nextToken(peek);
                    
                    break;
                }
                
                nextToken(peek);
                
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
            
            auto peek = currentToken(TOPLEVEL);
            
            if (peek.Tok != TOKEN_HASH) {
                
                //
                // TODO: add to Issues
                //

                return;
            }
            
            nextToken(peek);
            
            peek = currentToken(TOPLEVEL);
            
            if (peek.Tok != TOKEN_BANG) {
                
                //
                // TODO: add to Issues
                //

                return;
            }
            
            nextToken(peek);
            
            while (true) {
                
#if CHECK_ABORT
                if (TheParserSession->isAbort()) {
                    
                    break;
                }
#endif // CHECK_ABORT
                
                auto peek = currentToken(TOPLEVEL);
                
                if (peek.Tok == TOKEN_ENDOFFILE) {
                    
                    break;
                }
                
                if (peek.Tok == TOKEN_TOPLEVELNEWLINE) {
                    
                    nextToken(peek);
                    
                    break;
                }
                
                nextToken(peek);
                
            } // while (true)
            
            break;
        }
    }
}


void Parser::deinit() {
    
}

void Parser::nextToken(Token Tok) {
    TheTokenizer->nextToken(Tok);
}


Token Parser::nextToken0(NextPolicy policy) {
    
    auto insideGroup = !GroupStack.empty();
    
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    auto Tok = TheTokenizer->nextToken0(policy & ~(returnInternalNewlineMask));
    
    return Tok;
}

Token Parser::currentToken(NextPolicy policy) const {
    
    auto insideGroup = !GroupStack.empty();
    
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    auto Tok = TheTokenizer->currentToken(policy & ~(returnInternalNewlineMask));
    
    return Tok;
}


Token Parser::currentToken_stringifyAsTag() const {
    return TheTokenizer->currentToken_stringifyAsTag();
}

Token Parser::currentToken_stringifyAsFile() const {
    return TheTokenizer->currentToken_stringifyAsFile();
}

void Parser_parseClimb(ParseletPtr Ignored, Token Ignored2) {
    
    //
    // Check isAbort() inside loops
    //
#if CHECK_ABORT
    if (TheParserSession->isAbort()) {
        return Parser_tryContinue(Ignored, Ignored2);
    }
#endif // CHECK_ABORT
    
    auto& Trivia1 = TheParser->getTrivia1();
    
    auto token = TheParser->currentToken(TOPLEVEL);
    
    //
    // not in the middle of parsing anything, so toplevel newlines will delimit
    //
    TheParser->eatTriviaButNotToplevelNewlines(token, TOPLEVEL, Trivia1);
    
    auto I = infixParselets[token.Tok.value()];
    
    token = I->processImplicitTimes(token);
    
    I = infixParselets[token.Tok.value()];
    
    auto TokenPrecedence = I->getPrecedence();
    
    //
    // if (Ctxt.Prec > TokenPrecedence)
    //   break;
    // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
    //   break;
    //
    
    if ((TheParser->topPrecedence() | 0x1) > TokenPrecedence) {
        
        Trivia1.reset();
        
        MUSTTAIL
        return Parser_tryContinue(Ignored, Ignored2);
    }
    
    TheParser->pushContextAndShift(TokenPrecedence);
    
    TheParser->appendArgs(Trivia1);
    
    MUSTTAIL
    return (I->parseInfix())(I, token);
}

void Parser_tryContinue(ParseletPtr Ignored, Token Ignored2) {
    
    if (TheParser->getContextStackSize() > 0) {

        auto& Ctxt = TheParser->topContext();

        auto F = Ctxt.F;
        auto P = Ctxt.P;

        assert(F);
        
        MUSTTAIL
        return F(P, Ignored2);
    }
    
    // no call needed here
    return;
}

void Parser_identity(ParseletPtr P, Token firstTok) {
    return;
}

void Parser::eatTrivia(Token& T, NextPolicy policy) {
    
    while (T.Tok.isTrivia()) {
        
        ArgsStack.emplace_back(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken(policy);
    }
}

void Parser::eatTrivia(Token& T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        Args.append(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken(policy);
    }
}

void Parser::eatTrivia_stringifyAsFile(Token& T) {
    
    while (T.Tok.isTrivia()) {
        
        ArgsStack.emplace_back(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken_stringifyAsFile();
    }
}

void Parser::eatTrivia_stringifyAsFile(Token& T, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        Args.append(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken_stringifyAsFile();
    }
}

void Parser::eatTriviaButNotToplevelNewlines(Token& T, NextPolicy policy) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        ArgsStack.emplace_back(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken(policy);
    }
}

void Parser::eatTriviaButNotToplevelNewlines(Token& T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        Args.append(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken(policy);
    }
}

void Parser::eatTriviaButNotToplevelNewlines_stringifyAsFile(Token& T, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        Args.append(new LeafNode(T));
        
        nextToken(T);
        
        T = currentToken_stringifyAsFile();
    }
}

void Parser::shift() {
    ArgsStack.push_back(popNode());
}

Context& Parser::pushContext(Precedence Prec) {
    
    ContextStack.emplace_back(ArgsStack.size(), Prec);
    
    return ContextStack.back();
}

void Parser::pushContextV(Precedence Prec) {
    ContextStack.emplace_back(ArgsStack.size(), Prec);
}

void Parser::pushContextAndShift(Precedence Prec) {
    
    ContextStack.emplace_back(ArgsStack.size(), Prec);
    
    ArgsStack.push_back(popNode());
}

NodeSeq Parser::popContext() {
    
    assert(!ContextStack.empty());
    
    //
    // get the top Context
    //
    
    auto Ctxt = ContextStack.back();
    
    ContextStack.pop_back();
    
    //
    // How many args to take?
    //
    
    auto Count = ArgsStack.size() - Ctxt.Index;
    
    NodeSeq ArgsTmp(Count);
    
    //
    // Move that many Args from back of ArgsStack to ArgsTmp
    //
    
    std::move(ArgsStack.begin() + Ctxt.Index, ArgsStack.begin() + ArgsStack.size(), std::back_inserter(ArgsTmp.vec));
    
    //
    // forget about the moved Args
    //
    
    ArgsStack.resize(Ctxt.Index);

    return ArgsTmp;
}

Context& Parser::topContext() {
    
    assert(!ContextStack.empty());
    
    return ContextStack.back();
}

void Parser::appendArg(Node *N) {
    ArgsStack.emplace_back(N);
}

void Parser::appendArgs(TriviaSeq& Seq) {
    
    //
    // Move all trivia from Seq to back of ArgsStack
    //
    
    std::move(Seq.vec.begin(), Seq.vec.end(), std::back_inserter(ArgsStack));
    
    //
    // Forget about Seq
    //
    
    Seq.vec.clear();
}

size_t Parser::getArgsStackSize() const {
    return ArgsStack.size();
}

size_t Parser::getContextStackSize() const {
    return ContextStack.size();
}

void Parser::pushNode(Node *N) {
    NodeStack.emplace_back(N);
}

NodePtr& Parser::topNode() {
    
    assert(!NodeStack.empty());
    
    return NodeStack.back();
}

NodePtr Parser::popNode() {
    
    assert(!NodeStack.empty());
    
    auto top = std::move(NodeStack.back());
    
    NodeStack.pop_back();
    
    return top;
}

size_t Parser::getNodeStackSize() const {
    return NodeStack.size();
}

void Parser::pushGroup(Closer Closr) {
    GroupStack.push_back(Closr);
}

void Parser::popGroup() {
    
    assert(!GroupStack.empty());
    
    GroupStack.pop_back();
}

size_t Parser::getGroupDepth() const {
    return GroupStack.size();
}

bool Parser::checkGroup(Closer Closr) const {
    
    for (auto rit = GroupStack.rbegin(); rit != GroupStack.rend(); rit++) {
        
        if (*rit == Closr) {
            return true;
        }
    }
    
    return false;
}

Precedence Parser::topPrecedence() {
    if (ContextStack.empty()) {
        return PRECEDENCE_LOWEST;
    }
    return ContextStack.back().Prec;
}

void Parser::setPrecedence(Precedence Prec) {
    assert(!ContextStack.empty());
    auto& Ctxt = ContextStack.back();
    Ctxt.Prec = Prec;
}

bool Parser::checkPatternPrecedence() const {
    
    for (auto rit = ContextStack.rbegin(); rit != ContextStack.rend(); rit++) {

        auto& Ctxt = *rit;

        auto Prec = Ctxt.Prec;
        
        if (Prec > PRECEDENCE_FAKE_PATTERNCOLON) {
            continue;
        }
        
        if (Prec == PRECEDENCE_FAKE_PATTERNCOLON) {
            return true;
        }

        if (Prec < PRECEDENCE_FAKE_PATTERNCOLON) {
            return false;
        }
    }
    
    return false;
}

ColonLHS Parser::checkColonLHS() const {

    //
    // work backwards, looking for a symbol or something that is a pattern
    //
    
    if (ArgsStack.empty()) {
        return COLONLHS_NONE;
    }
    
    //
    // skip any trivia
    //
    auto Ctxt = ContextStack.back();
    int i;
    for (i = static_cast<int>(ArgsStack.size())-1; i >= static_cast<int>(Ctxt.Index); i--) {

        auto& N = ArgsStack[i];

        if (auto L = dynamic_cast<LeafNode *>(N.get())) {

            auto Tok = L->getToken();

            if (Tok.Tok.isTrivia()) {
                continue;
            }

            break;
        }

        break;
    }

    if (i == static_cast<int>(Ctxt.Index)-1) {
        assert(false);
        return COLONLHS_NONE;
    }

    auto& N = ArgsStack[i];

    if (auto B = dynamic_cast<BinaryNode *>(N.get())) {

        auto Op = B->getOp();

        if (Op == SYMBOL_PATTERN) {
            return COLONLHS_OPTIONAL;
        }

        return COLONLHS_ERROR;
    }

    if (auto C = dynamic_cast<CompoundNode *>(N.get())) {

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

    if (auto L = dynamic_cast<LeafNode *>(N.get())) {

        auto Tok = L->getToken();

        switch (Tok.Tok.value()) {
            case TOKEN_SYMBOL.value(): {
                return COLONLHS_PATTERN;
            }
            case TOKEN_UNDER.value():
            case TOKEN_UNDERUNDER.value():
            case TOKEN_UNDERUNDERUNDER.value(): {
                return COLONLHS_OPTIONAL;
            }
            case TOKEN_COLON.value(): {
                assert(false && "Fix at call site");
            }
            default: {
                return COLONLHS_ERROR;
            }
        }
    }

    if (dynamic_cast<ErrorNode *>(N.get())) {

        //
        // allow errors to be on LHS of :
        //
        // This is a bit confusing. The thinking is that since there is already an error, then we do not need to introduce another error.
        //
        return COLONLHS_PATTERN;
    }

    return COLONLHS_ERROR;
}

bool Parser::checkTilde() const {

    //
    // work backwards, looking for ~
    //

    if (ArgsStack.empty()) {
        return false;
    }
    
    //
    // skip any trivia
    //
    auto Ctxt = ContextStack.back();
    int i;
    for (i = static_cast<int>(ArgsStack.size())-1; i >= static_cast<int>(Ctxt.Index); i--) {

        auto& N = ArgsStack[i];

        if (auto L = dynamic_cast<LeafNode *>(N.get())) {

            auto Tok = L->getToken();

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

    auto& N = ArgsStack[i];

    if (auto L = dynamic_cast<LeafNode *>(N.get())) {

        auto Tok = L->getToken();

        if (Tok.Tok == TOKEN_TILDE) {
            return true;
        }
    }

    return false;
}


TriviaSeq& Parser::getTrivia1() {
    return trivia1;
}

TriviaSeq& Parser::getTrivia2() {
    return trivia2;
}

void Parser::pushLeafNodeAndNext(Token Tok) {
    
    NodeStack.emplace_back(new LeafNode(Tok));
    
    TheTokenizer->nextToken(Tok);
}

void Parser::appendLeafArgAndNext(Token Tok) {
    
    ArgsStack.emplace_back(new LeafNode(Tok));
    
    TheTokenizer->nextToken(Tok);
}

bool Parser::isQuiescent() const {
    
    assert(ArgsStack.empty());
    assert(ContextStack.empty());
    assert(NodeStack.empty());
    assert(GroupStack.empty());
    assert(trivia1.empty());
    assert(trivia2.empty());
    
    return true;
}

ParserPtr TheParser = nullptr;
