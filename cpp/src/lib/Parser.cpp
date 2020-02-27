
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

Token Parser::nextToken0() {
    
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
    
    return TheTokenizer->nextToken0(TOPLEVEL);
}

Token Parser::currentToken() const {
    
    if (!tokenQueue.empty()) {
        
        auto Tok = tokenQueue[0];
        
        return Tok;
    }
    
    return TheTokenizer->currentToken(TOPLEVEL);
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


Precedence Parser::getPrefixTokenPrecedence(Token& TokIn, ParserContext Ctxt) const {
    
    assert(TokIn.Tok != TOKEN_UNKNOWN);
    assert(TokIn.Tok != TOKEN_WHITESPACE);
    // allow top-level newlines
    assert(TokIn.Tok != TOKEN_NEWLINE || !Ctxt.InsideGroup);
    assert(TokIn.Tok != TOKEN_COMMENT);
    assert(TokIn.Tok != TOKEN_LINECONTINUATION);
    
    if (TokIn.Tok.isError()) {
        
        return PRECEDENCE_LOWEST;
    }
    
    if (TokIn.Tok == TOKEN_ENDOFFILE) {
        
        return PRECEDENCE_LOWEST;
    }
    
    //
    // TODO: review when closers have their own parselets
    //
    if (TokIn.Tok.isCloser()) {
        
        return PRECEDENCE_LOWEST;
    }
        
    auto& P = prefixParselets[TokIn.Tok.value()];
    
    //
    // There is an ambiguity with tokens that are both prefix and infix, e.g.
    // +  -  ;;  !  ++  --  !!  \[Minus]  \[MinusPlus]  \[PlusMinus]  \[CircleTimes]  \[Coproduct]
    //
    // Given the input  ;;;;
    // when parsing the second  ;;  , we could get here because ;; is registered as infix
    // But this particular ;; is a new expression, it is not actually infix
    //
    // Given the input  1+2
    // when parsing the +, make sure to treat it as infix and NOT prefix
    //
    // Solution is to handle infix parselets where needed, i.e., SemiSemiParselet
    //
    
    return P->getPrecedence();
}

Precedence Parser::getInfixTokenPrecedence(Token& TokIn, ParserContext Ctxt, bool *implicitTimes) const {
    
    assert(TokIn.Tok != TOKEN_UNKNOWN);
    assert(TokIn.Tok != TOKEN_WHITESPACE);
    // allow top-level newlines
    assert(TokIn.Tok != TOKEN_NEWLINE || !Ctxt.InsideGroup);
    assert(TokIn.Tok != TOKEN_COMMENT);
    assert(TokIn.Tok != TOKEN_LINECONTINUATION);
    
    if (TokIn.Tok.isError()) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    if (TokIn.Tok == TOKEN_ENDOFFILE) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    //
    // TODO: review when closers have their own parselets
    //
    if (TokIn.Tok.isCloser()) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    auto& Infix = infixParselets[TokIn.Tok.value()];
    
    if (Infix != nullptr) {
        
        *implicitTimes = false;
        
        return Infix->getPrecedence();
    }
    
    if (TokIn.Tok.isDifferentialD()) {
        
        if ((Ctxt.Flag & PARSER_INSIDE_INTEGRAL) == PARSER_INSIDE_INTEGRAL) {
            
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //
            
            *implicitTimes = false;
            
            return PRECEDENCE_LOWEST;
        }
    }
    
    //
    // Literals or unhandled
    //
    
    //
    // Do not do Implicit Times across lines
    //
    if (TokIn.Tok == TOKEN_NEWLINE && !Ctxt.InsideGroup) {
        
        *implicitTimes = false;
        
        return PRECEDENCE_LOWEST;
    }
    
    *implicitTimes = true;
    
    return PRECEDENCE_FAKE_IMPLICITTIMES;
}

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
    
    auto& P = findPrefixParselet(token.Tok);
    
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
        
        auto token = currentToken();
        token = Parser::eatAndPreserveToplevelNewlines(token, Ctxt, ArgsTest);
        
        bool implicitTimes;
        
        auto TokenPrecedence = getInfixTokenPrecedence(token, Ctxt, &implicitTimes);
        
        if (implicitTimes) {
            
            token = Token(TOKEN_FAKE_IMPLICITTIMES, BufferAndLength(token.BufLen.buffer), Source(token.Src.Start));
            
            tokenQueue.insert(tokenQueue.begin(), token);
        }
        
        if (Ctxt.Prec > TokenPrecedence) {
            break;
        }
        if (Ctxt.Prec == TokenPrecedence) {            
            auto& I = infixParselets[token.Tok.value()];
            if (I == nullptr) {
                break;
            }
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
    
        auto& I = findInfixParselet(token.Tok);
    
        Left = I->parse(std::move(LeftSeq), token, Ctxt2);
        
    } // while
    
    return Left;
}

NodePtr Parser::handleNotPossible(Token& tokenBad, Token& tokenAnchor, ParserContext CtxtIn, bool *wasCloser) {
    
    //
    // It is possible that possibleBeginningOfExpression could get here
    //
    // For example: \[Integral]!b
    // !b is possible beginning of expression, but ! has lower precedence than \[Integral],
    // so
    //
    //

    if (tokenBad.Tok.isPossibleBeginningOfExpression()) {

        auto operand = parse(tokenBad, CtxtIn);

        if (wasCloser != nullptr) {
            *wasCloser = false;
        }

        return operand;
    }
    
    auto& I = infixParselets[tokenBad.Tok.value()];
    if (I != nullptr) {
        
        //
        // Handle something like  f[,1]
        //
        // We want to make EXPECTEDOPERAND the first arg of the Comma node.
        //
        // Do not take next token
        //
        // Important to not duplicate token's Str here, it may also appear later
        //
        // Also, invent Source
        //
        
        auto NotPossible = NodePtr(new ErrorNode(Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.buffer), Source(tokenAnchor.Src.Start))));
        
        NodeSeq LeftSeq(1);
        LeftSeq.append(std::move(NotPossible));
        
        auto Ctxt = CtxtIn;
        //
        // FIXME: clear other flags here also?
        //
        Ctxt.Flag &= ~(PARSER_INSIDE_COLON);
        
        if (wasCloser != nullptr) {
            *wasCloser = false;
        }
        
        return I->parse(std::move(LeftSeq), tokenBad, Ctxt);
    }
    
    if (tokenBad.Tok.isCloser()) {
        
        if (TokenToCloser(tokenBad.Tok) == CtxtIn.Closr) {
            //
            // Handle the special cases of:
            // { + }
            // { a + }
            // { a @ }
            // We are here parsing the operators, but we don't want to descend and treat the } as the problem
            //
            
            //
            // Do not take next token
            //
            
            auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.end), Source(tokenAnchor.Src.End));
            
            if (wasCloser != nullptr) {
                *wasCloser = true;
            }
            
            return NodePtr(new ErrorNode(createdToken));
        }
        
        //
        // Handle  { a ) }
        // which ends up being  MissingCloser[ { a ) ]   UnexpectedCloser[ } ]
        //
        
        nextToken(tokenBad);
        
        NodeSeq Args(1);
        Args.append(NodePtr(new LeafNode(tokenBad)));
        
        auto Error = NodePtr(new SyntaxErrorNode(SYNTAXERROR_UNEXPECTEDCLOSER, std::move(Args)));
        
        if (wasCloser != nullptr) {
            *wasCloser = true;
        }
        
        return Error;
    }
    
    if (tokenBad.Tok == TOKEN_ENDOFFILE) {
        
        auto createdToken = Token(TOKEN_ERROR_EXPECTEDOPERAND, BufferAndLength(tokenAnchor.BufLen.end), Source(tokenAnchor.Src.End));
        
        if (wasCloser != nullptr) {
            *wasCloser = true;
        }
        
        return NodePtr(new ErrorNode(createdToken));
    }
    
    assert(tokenBad.Tok.isError());
        
    nextToken(tokenBad);
    
    //
    // If there is a Token error, then use that specific error
    //
    
    if (wasCloser != nullptr) {
        *wasCloser = false;
    }
    
    return NodePtr(new ErrorNode(tokenBad));
}

Token Parser::eatAll(Token T, ParserContext Ctxt, LeafSeq& Args) {
    
    while (T.Tok.isTrivia()) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        Args.append(LeafNodePtr(new LeafNode(T)));
        
        nextToken(T);
        
        T = currentToken();
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
            case TOKEN_NEWLINE.value(): {
                
                if (!Ctxt.InsideGroup) {
                    
                    return T;
                }
            }
            //
            // Fall through
            //
            case TOKEN_WHITESPACE.value():
            case TOKEN_COMMENT.value():
            case TOKEN_LINECONTINUATION.value(): {
                
                Args.append(LeafNodePtr(new LeafNode(std::move(T))));
                
                nextToken(T);
                
                T = currentToken();
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
            case TOKEN_NEWLINE.value(): {
                
                if (!Ctxt.InsideGroup) {
                    
                    return T;
                }
            }
            //
            // Fall through
            //
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
