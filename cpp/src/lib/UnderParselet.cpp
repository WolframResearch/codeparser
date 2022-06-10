
#include "Parselet.h"
#include "ParseletRegistration.h" // for symbolParselet
#include "Symbol.h"


ParseFunction UnderParselet::parsePrefix() const {
    return UnderParselet_parsePrefix;
}

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Under));
    }
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  _b
        //
        
        TheParser->pushArgs(nullptr, nullptr);
        
        TheParser->shift();
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_parseBlank(P, Token());
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        TheParser->pushArgs(nullptr, nullptr);
        
        TheParser->shift();
        
        TheParser->nextToken(Tok);
        
        {
            auto Error = NodePtr(new ErrorNode(Tok));
            
            TheParser->pushNode(std::move(Error));
        }
        
        MUSTTAIL
        return UnderParselet_parseBlank(P, Token());
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}

void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    {
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Under));
    }
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  a_b
        //
        
        TheParser->pushArgs(nullptr, nullptr);
        
        TheParser->shift();
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_parseBlankContextSensitive(P, Token());
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  a_b`
        //
        // It's nice to include the error inside of the blank
        //
        
        TheParser->pushArgs(nullptr, nullptr);
        
        TheParser->shift();
        
        TheParser->nextToken(Tok);
        
        {
            auto Error = NodePtr(new ErrorNode(Tok));
            
            TheParser->pushNode(std::move(Error));
        }
    
        MUSTTAIL
        return UnderParselet_parseBlankContextSensitive(P, Token());
    }
    
    // no call needed here
    return;
}

void UnderParselet_parseBlank(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
        
        auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Blank));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

void UnderParselet_parseBlankContextSensitive(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
        
        auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Blank));
    }
    
    // no call needed here
    return;
}


ParseFunction UnderDotParselet::parsePrefix() const {
    return UnderDotParselet_parsePrefix;
}

void UnderDotParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        auto UnderDot = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(UnderDot));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}

void UnderDotParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    {
        auto UnderDot = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(UnderDot));
    }
    
    // no call needed here
    return;
}
