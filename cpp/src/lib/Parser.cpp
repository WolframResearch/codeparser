
#include "Parser.h"

#include "API.h" // for TheParserSession
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
#include "ParseletRegistration.h"
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer


Parser::Parser() : ArgsStack(), NodeStack(), PrecedenceStack(), GroupStack() {}

void Parser::init() {
    
    clearPrecedenceStack();
    pushPrecedence(PRECEDENCE_LOWEST);
    
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
                
        #if !NABORT
                if (TheParserSession->isAbort()) {
                    
                    break;
                }
        #endif // !NABORT
                
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
                
        #if !NABORT
                if (TheParserSession->isAbort()) {
                    
                    break;
                }
        #endif // !NABORT
                
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
    
    auto insideGroup = getGroupDepth() > 0;
    
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
    
    auto insideGroup = getGroupDepth() > 0;
    
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
    HANDLE_ABORT;
    
    Precedence TokenPrecedence;
    InfixParseletPtr I;
    Token token;
    {
        TriviaSeq Trivia1;
        
        token = TheParser->currentToken(TOPLEVEL);
        token = TheParser->eatTriviaButNotToplevelNewlines(token, TOPLEVEL, Trivia1);
        
        I = infixParselets[token.Tok.value()];
        
        token = I->processImplicitTimes(token);
        I = infixParselets[token.Tok.value()];
        
        TokenPrecedence = I->getPrecedence();
        
        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   break;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   break;
        //
        if ((TheParser->topPrecedence() | 0x1) > TokenPrecedence) {
                
            Trivia1.reset();
            
            return;
        }
        
        auto& LeftSeq = TheParser->pushArgs();
        
        TheParser->shift();
        
        LeftSeq.appendSeq(std::move(Trivia1));
    }
    
    MUSTTAIL
    return (I->parseInfix())(I, token);
}

Token Parser::eatTrivia(Token T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken(policy);
    }
    
    return T;
}

Token Parser::eatTrivia_stringifyAsFile(Token T, TriviaSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken_stringifyAsFile();
    }
    
    return T;
}

Token Parser::eatTriviaButNotToplevelNewlines(Token T, NextPolicy policy, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken(policy);
    }
    
    return T;
}

Token Parser::eatTriviaButNotToplevelNewlines_stringifyAsFile(Token T, TriviaSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken_stringifyAsFile();
    }
    
    return T;
}

void Parser::shift() {
    auto Operand = TheParser->popNode();
    auto& Args = ArgsStack.back();
    Args.append(std::move(Operand));
}

NodeSeq& Parser::pushArgs() {
    ArgsStack.push_back(NodeSeq());
    return ArgsStack.back();
}

NodeSeq Parser::popArgs() {
    assert(!ArgsStack.empty());
    auto top = std::move(ArgsStack.back());
    ArgsStack.pop_back();
    return top;
}

NodeSeq& Parser::peekArgs() {
    assert(!ArgsStack.empty());
    return ArgsStack.back();
}

size_t Parser::getArgsStackSize() const {
    return ArgsStack.size();
}

void Parser::pushNode(NodePtr N) {
    NodeStack.push_back(std::move(N));
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

Precedence& Parser::topPrecedence() {
    assert(!PrecedenceStack.empty());
    return PrecedenceStack.back();
}

Precedence& Parser::pushPrecedence(Precedence Prec) {
    PrecedenceStack.push_back(Prec);
    return PrecedenceStack.back();
}

void Parser::popPrecedence() {
    assert(!PrecedenceStack.empty());
    PrecedenceStack.pop_back();
}

size_t Parser::getPrecedenceStackSize() const {
    return PrecedenceStack.size();
}

void Parser::clearPrecedenceStack() {
    while (!PrecedenceStack.empty()) {
        PrecedenceStack.pop_back();
    }
}

bool Parser::checkPatternPrecedence() const {
    
    for (auto rit = PrecedenceStack.rbegin(); rit != PrecedenceStack.rend(); rit++) {
        
        auto Prec = *rit;
        
        if (Prec == PRECEDENCE_FAKE_PATTERNCOLON) {
            return true;
        }
        
        //
        // reset by a group
        //
        if (Prec == PRECEDENCE_LOWEST) {
            return false;
        }
    }
    
    return false;
}

ParserPtr TheParser = nullptr;
