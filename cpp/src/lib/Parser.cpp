
#include "Parser.h"

#include "API.h" // for TheParserSession
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
#include "ParseletRegistration.h"

Parser::Parser() : tokenQueue(), Issues() {}

Parser::~Parser() {}

void Parser::init() {
    
    Issues.clear();
}

void Parser::deinit() {
    
    tokenQueue.clear();
    Issues.clear();
}

void Parser::nextToken(Token Tok) {
    
    //
    // handle the queue before anything else
    //
    // We do not know anything about how many Tokens should be read
    //
    if (!tokenQueue.empty()) {
        
        assert(Tok == tokenQueue[0]);
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return;
    }
    
    TheTokenizer->nextToken(Tok);
}


void Parser::nextToken_stringifySymbol() {
    
    //
    // handle the queue before anything else
    //
    // We do not know anything about how many Tokens should be read
    //
    if (!tokenQueue.empty()) {
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return;
    }
    
    TheTokenizer->nextToken_stringifySymbol();
}

void Parser::nextToken_stringifyFile() {
    
    //
    // handle the queue before anything else
    //
    // We do not know anything about how many Tokens should be read
    //
    if (!tokenQueue.empty()) {
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return;
    }
    
    TheTokenizer->nextToken_stringifyFile();
}

Token Parser::nextToken0(ParserContext Ctxt) {
    
    //
    // handle the queue before anything else
    //
    // We do not know anything about how many Tokens should be read
    //
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        // erase first
        tokenQueue.erase(tokenQueue.begin());
        
        return Tok;
    }
    
    auto insideGroup = (Ctxt.Closr != CLOSER_OPEN);
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    auto Tok = TheTokenizer->nextToken0(TOPLEVEL & ~(returnInternalNewlineMask));
    
    return Tok;
}

Token Parser::currentToken(ParserContext Ctxt) const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    auto insideGroup = (Ctxt.Closr != CLOSER_OPEN);
    //
    // if insideGroup:
    //   returnInternalNewlineMask is 0b100
    // else:
    //   returnInternalNewlineMask is 0b000
    //
    auto returnInternalNewlineMask = static_cast<uint8_t>(insideGroup) << 2;
    auto Tok = TheTokenizer->currentToken(TOPLEVEL & ~(returnInternalNewlineMask));
    
    return Tok;
}


Token Parser::currentToken_stringifySymbol() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken_stringifySymbol();
}

Token Parser::currentToken_stringifyFile() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken_stringifyFile();
}

void Parser::prepend(Token Tok) {
        
    tokenQueue.insert(tokenQueue.begin(), Tok);
}


void Parser::prependInReverse(std::vector<LeafNodePtr>& V) {
    
    if (V.empty()) {
        return;
    }
    
    auto i = V.rbegin();
    for (; i != V.rend(); ++i ) {
        
        auto& T = (*i)->getToken();
        
        tokenQueue.insert(tokenQueue.begin(), T);
    }
}

#if !NISSUES
std::vector<IssuePtr>& Parser::getIssues() {
    return Issues;
}

//
// Only to be used by Parselets
//
void Parser::addIssue(IssuePtr I) {
    Issues.push_back(std::move(I));
}
#endif // !NISSUES


NodePtr Parser::infixLoop(NodePtr Left, ParserContext Ctxt) {
    
    while (true) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest;
        
        auto token = currentToken(Ctxt);
        token = eatTriviaButNotToplevelNewlines(token, Ctxt, ArgsTest);
        
        auto I = infixParselets[token.Tok.value()];
        
        token = I->procesImplicitTimes(token);
        I = infixParselets[token.Tok.value()];
        
        auto TokenPrecedence = I->getPrecedence(Ctxt);
        
        //
        // if (Ctxt.Prec > TokenPrecedence)
        //   break;
        // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
        //   break;
        //
        if ((Ctxt.Prec | 0x1) > TokenPrecedence) {
            break;
        }
        
        NodeSeq LeftSeq(1 + 1);
        LeftSeq.append(std::move(Left));
        LeftSeq.appendIfNonEmpty(std::move(ArgsTest));
        
        auto Ctxt2 = Ctxt;
        Ctxt2.Prec = TokenPrecedence;
        
        Left = I->parse(std::move(LeftSeq), token, Ctxt2);
        
    } // while
    
    return Left;
}

Token Parser::eatTrivia(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken(Ctxt);
    }
    
    return T;
}

Token Parser::eatTrivia_stringifyFile(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken_stringifyFile();
    }
    
    return T;
}

Token Parser::eatTriviaButNotToplevelNewlines(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken(Ctxt);
    }
    
    return T;
}

Token Parser::eatTriviaButNotToplevelNewlines_stringifyFile(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTriviaButNotToplevelNewline()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken_stringifyFile();
    }
    
    return T;
}

ParserPtr TheParser = nullptr;
