
#include "Parser.h"

#include "API.h" // for TheParserSession
#include "Parselet.h" // for SymbolParselet, UnderParselet, etc.
#include "Tokenizer.h" // for Tokenizer
#include "ParseletRegistration.h"

#include <algorithm> // for generate with GCC and MSVC

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


#if STARTOFLINE

void Parser::nextToken_stringifyLine() {
    
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
    
    TheTokenizer->nextToken_stringifyLine();
}

#endif // STARTOFLINE


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
    
    //
    // Treat Ctxt.InsideGroup as a single bit and or with TOPLEVEL to set the RETURN_INTERNALNEWLINE NextCharacterPolicy bit
    //
    return TheTokenizer->nextToken0(TOPLEVEL | Ctxt.InsideGroup);
}

Token Parser::currentToken(ParserContext Ctxt) const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    //
    // Treat Ctxt.InsideGroup as a single bit and or with TOPLEVEL to set the RETURN_INTERNALNEWLINE NextCharacterPolicy bit
    //
    return TheTokenizer->currentToken(TOPLEVEL | Ctxt.InsideGroup);
}


#if STARTOFLINE

Token Parser::currentToken_stringifyLine() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken_stringifyLine();
}

#endif // STARTOFLINE


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


NodePtr Parser::parse(Token token, ParserContext Ctxt) {

#if !NABORT
    if (TheParserSession->isAbort()) {
        
        return TheParserSession->handleAbort();
    }
#endif // !NABORT
    
    assert(token.Tok != TOKEN_UNKNOWN);
    assert(!token.Tok.isTrivia() && "Must handle at the call site");
    assert(token.Tok != TOKEN_ENDOFFILE && "Must handle at the call site");
    assert(token.Tok.isPossibleBeginningOfExpression() && "Must handle at the call site");
    
    //
    // Prefix start
    //
    
    auto P = prefixParselets[token.Tok.value()];
    
    auto Left = P->parse(token, Ctxt);
    
    
    //
    // Infix loop
    //
    
    while (true) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbort();
        }
#endif // !NABORT
        
        LeafSeq ArgsTest;
        
        auto token = currentToken(Ctxt);
        token = Parser::eatAndPreserveToplevelNewlines(token, Ctxt, ArgsTest);
        
        auto I = infixParselets[token.Tok.value()];
        
        bool implicitTimes;
        auto TokenPrecedence = I->getPrecedence(Ctxt, &implicitTimes);
        
        if (implicitTimes) {
            
            token = Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(token.BufLen.buffer), Source(token.Src.Start));
            
            I = infixParselets[TOKEN_FAKE_IMPLICITTIMES.value()];
            
            tokenQueue.insert(tokenQueue.begin(), token);
        }
        
        if (Ctxt.Prec > TokenPrecedence) {
            break;
        }
        if (Ctxt.Prec == TokenPrecedence) {
            auto TokenAssociativity = I->getAssociativity();
            if (TokenAssociativity != ASSOCIATIVITY_RIGHT) {
                break;
            }
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

Token Parser::eatAll(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
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

Token Parser::eatAll_stringifyFile(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
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

Token Parser::eatAndPreserveToplevelNewlines(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        switch (T.Tok.value()) {
            case TOKEN_TOPLEVELNEWLINE.value(): {
                
                return T;
            }
            //
            // Fall through
            //
            case TOKEN_INTERNALNEWLINE.value():
            case TOKEN_WHITESPACE.value():
            case TOKEN_COMMENT.value():
            case TOKEN_LINECONTINUATION.value(): {
                
                Args.append(LeafNodePtr(new LeafNode(std::move(T))));
                
                nextToken(T);
                
                T = currentToken(Ctxt);
            }
                break;
            default:
                return T;
        }
    }
}

Token Parser::eatAndPreserveToplevelNewlines_stringifyFile(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        switch (T.Tok.value()) {
            case TOKEN_TOPLEVELNEWLINE.value(): {
                    
                return T;
            }
            //
            // Fall through
            //
            case TOKEN_INTERNALNEWLINE.value():
            case TOKEN_WHITESPACE.value():
            case TOKEN_COMMENT.value():
            case TOKEN_LINECONTINUATION.value(): {
                
                Args.append(LeafNodePtr(new LeafNode(std::move(T))));
                
                nextToken(T);
                
                T = currentToken_stringifyFile();
            }
                break;
            default:
                return T;
        }
    }
}

ParserPtr TheParser = nullptr;
