
#include "Parselet.h"
#include "ParseletRegistration.h" // for contextSensitiveSymbolParselet
#include "Symbol.h"

NodePtr UnderParselet::parse0(Token TokIn, ParserContext Ctxt, Continuation k) const {
    
    auto Under = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    NodePtr Blank;
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        return contextSensitiveSymbolParselet->parsePrefixContextSensitive(Tok, Ctxt, [&](NodePtr Sym2) {
        
        NodeSeq Args(1 + 1);
        Args.append(std::move(Under));
        Args.append(std::move(Sym2));
        
        Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
        
        return k(std::move(Blank));
        });
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        auto parselet = prefixParselets[Tok.Tok.value()];
        
        return parselet->parsePrefix(Tok, Ctxt, [&](NodePtr ErrorSym2) {
        
        NodeSeq Args(1 + 1);
        Args.append(std::move(Under));
        Args.append(std::move(ErrorSym2));
        
        Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
        
        return k(std::move(Blank));
        });
    }
        
    Blank = std::move(Under);
    
    return k(std::move(Blank));
}

NodePtr UnderParselet::parse1(NodePtr Blank, Token Tok, ParserContext Ctxt, Continuation k) const {
    
    TriviaSeq Trivia1;
    
    Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, Ctxt, TOPLEVEL, Trivia1);
    
    //
    // For something like _:\"\"  when parsing _
    // ColonFlag == false
    // the : here is Optional, and so we want to go parse with ColonParselet's parseContextSensitive method
    //
    // For something like a:_:\"\"  when parsing _
    // ColonFlag == true
    // make sure to not parse the second : here
    // We are already inside ColonParselet from the first :, and so ColonParselet will also handle the second :
    //
    if (Tok.Tok == TOKEN_COLON) {
        
        if ((Ctxt.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
            
            NodeSeq BlankSeq(1 + Trivia1.size());
            BlankSeq.append(std::move(Blank));
            BlankSeq.appendSeq(std::move(Trivia1));
            
            return contextSensitiveColonParselet->parseInfixContextSensitive(std::move(BlankSeq), Tok, Ctxt, [&](NodePtr Blank) {
            
            return TheParser->parseLoop(std::move(Blank), Ctxt, k);
            });
        }
            
        Trivia1.reset();
        
        return TheParser->parseLoop(std::move(Blank), Ctxt, k);
    }
        
    Trivia1.reset();
    
    return TheParser->parseLoop(std::move(Blank), Ctxt, k);
}

NodePtr UnderParselet::parsePrefix(Token TokIn, ParserContext Ctxt, Continuation k) const {
    
    return parse0(TokIn, Ctxt, [&](NodePtr Blank) {
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    return parse1(std::move(Blank), Tok, Ctxt, k);
    });
}

NodePtr UnderParselet::parseInfixContextSensitive(NodeSeq Args, Token TokIn, ParserContext Ctxt, Continuation k) const {
    
    return parse0(TokIn, Ctxt, [&](NodePtr Blank) {
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    return parse1(std::move(Pat), Tok, Ctxt, k);
    });
}


NodePtr UnderDotParselet::parse0(Token TokIn, ParserContext Ctxt, Continuation k) const {
    
    auto UnderDot = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    return k(std::move(UnderDot));
}

NodePtr UnderDotParselet::parsePrefix(Token TokIn, ParserContext Ctxt, Continuation k) const {
    
    return parse0(TokIn, Ctxt, [&](NodePtr Blank) {
    
    TheParser->nextToken(TokIn);
    
    return TheParser->parseLoop(std::move(Blank), Ctxt, k);
    });
}

NodePtr UnderDotParselet::parseInfixContextSensitive(NodeSeq Args, Token TokIn, ParserContext Ctxt, Continuation k) const {
    
    return parse0(TokIn, Ctxt, [&](NodePtr Blank) {
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
    
    return TheParser->parseLoop(std::move(Pat), Ctxt, k);
    });
}
