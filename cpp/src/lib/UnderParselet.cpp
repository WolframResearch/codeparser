
#include "Parselet.h"
#include "ParseletRegistration.h" // for symbolParselet
#include "Symbol.h"
#include "Parser.h"


Symbol UnderParselet::getBOp() const {
    return BOp;
}

Symbol UnderParselet::getPBOp() const {
    return PBOp;
}

ParseFunction UnderParselet::parsePrefix() const {
    return UnderParselet_parsePrefix;
}

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  _b
        //
        
        TheParser->pushContextV(PRECEDENCE_HIGHEST);
        
        TheParser->shift();
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlank(P, TokIn/*ignored*/);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        TheParser->pushContextV(PRECEDENCE_HIGHEST);
        
        TheParser->shift();
        
        TheParser->nextToken(Tok);
        
        TheParser->pushNode(NodePtr(new ErrorNode(Tok)));
        
        MUSTTAIL
        return UnderParselet_reduceBlank(P, TokIn/*ignored*/);
    }
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, TokIn/*ignored*/);
}

void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    //
    // infix
    //
    // Something like  a_b
    //
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        //
        // Something like  a_b
        //
        
        TheParser->pushContextV(PRECEDENCE_HIGHEST);
        
        TheParser->shift();
        
        //
        // Context-sensitive and OK to build stack
        //
        
        SymbolParselet_parseInfixContextSensitive(symbolParselet, Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(P, TokIn/*ignored*/);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  a_b`
        //
        // It's nice to include the error inside of the blank
        //
        
        TheParser->pushContextV(PRECEDENCE_HIGHEST);
        
        TheParser->shift();
        
        TheParser->pushNode(NodePtr(new ErrorNode(Tok)));
        
        TheParser->nextToken(Tok);
    
        MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(P, TokIn/*ignored*/);
    }
    
    // no call needed here
    return;
}

void UnderParselet_reduceBlank(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<UnderParselet *>(P));
    
    TheParser->shift();
    
    auto BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    TheParser->pushNode(NodePtr(new CompoundNode(BOp, TheParser->popContext())));
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}

//
// Called from other parselets
//
void UnderParselet_reduceBlankContextSensitive(ParseletPtr P, Token Ignored) {
    
    assert(dynamic_cast<UnderParselet *>(P));
    
    TheParser->shift();
    
    auto BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    TheParser->pushNode(NodePtr(new CompoundNode(BOp, TheParser->popContext())));
    
    // no call needed here
    return;
}


ParseFunction UnderDotParselet::parsePrefix() const {
    return UnderDotParselet_parsePrefix;
}

void UnderDotParselet_parsePrefix(ParseletPtr P, Token TokIn) {
    
    //
    // prefix
    //
    // Something like  _.
    //
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    MUSTTAIL
    return Parser_parseClimb(nullptr, TokIn/*ignored*/);
}


//
// Called from other parselets
//
void UnderDotParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn) {
    
    //
    // infix
    //
    // Something like  a_.
    
    TheParser->pushNode(NodePtr(new LeafNode(TokIn)));
    
    TheParser->nextToken(TokIn);
    
    // no call needed here
    return;
}
