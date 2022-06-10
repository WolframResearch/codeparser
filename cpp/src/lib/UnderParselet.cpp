
#include "Parselet.h"
#include "ParseletRegistration.h" // for contextSensitiveSymbolParselet
#include "Symbol.h"

void UnderParselet_parse0(ParseletPtr P, Token Ignored) {
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        TheParser->pushArgs();
        
        TheParser->shift();
        
//        xxx;
        SymbolParselet_parsePrefixContextSensitive(symbolParselet, Tok);
        
        MUSTTAIL
        return UnderParselet_parse2(P, Ignored);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        TheParser->pushArgs();
        
        TheParser->shift();
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        auto P2 = prefixParselets[Tok.Tok.value()];
        
//        xxx;
        (P2->parsePrefix())(P2, Tok);
        
        MUSTTAIL
        return UnderParselet_parse2(P, Ignored);
    }
    
    return;
}

ParseFunction UnderParselet::parsePrefix() const {
    return UnderParselet_parsePrefix;
}

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    {
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Under));
    }
    
//    xxx;
    UnderParselet_parse0(P, Token());
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Token());
}

void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    {
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Under));
    }
    
//    xxx;
    UnderParselet_parse0(P, Token());
    
    MUSTTAIL
    return UnderParselet_parse4(P, Token());
}

void UnderParselet_parse2(ParseletPtr P, Token Ignored) {
    
    auto& Args = TheParser->peekArgs();
    
    TheParser->shift();
    
    auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->popArgs();
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse4(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        auto& PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
        
        auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Pat));
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
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
    
    MUSTTAIL
    return UnderDotParselet_parse1(P, Token());
}

void UnderDotParselet_parse1(ParseletPtr P, Token Ignored) {
    
    {
        auto& Args = TheParser->peekArgs();
        
        TheParser->shift();
        
        auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
        
        TheParser->popArgs();
        
        TheParser->pushNode(std::move(Pat));
    }
        
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}
