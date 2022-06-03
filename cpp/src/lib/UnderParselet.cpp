
#include "Parselet.h"
#include "ParseletRegistration.h" // for contextSensitiveSymbolParselet
#include "Symbol.h"

NodePtr UnderParselet::parse0(Token TokIn, ParserContext Ctxt) const {
    
    auto Under = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& Args = TheParser->pushArgs();
        Args.append(std::move(Under));
        
        auto Sym2 = contextSensitiveSymbolParselet->parsePrefixContextSensitive(Tok, Ctxt);
            
        return parse2(std::move(Sym2), Ctxt);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        auto& Args = TheParser->pushArgs();
        Args.append(std::move(Under));
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        auto parselet = prefixParselets[Tok.Tok.value()];
        
        auto ErrorSym2 = parselet->parsePrefix(Tok, Ctxt);
            
        return parse3(std::move(ErrorSym2), Ctxt);
    }
        
    auto Blank = std::move(Under);
    
    return Blank;
}

NodePtr UnderParselet::parse1(NodePtr Blank, Token Tok, ParserContext CtxtIn) const {
    
    TriviaSeq Trivia1;
    
    Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, CtxtIn, TOPLEVEL, Trivia1);
    
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
        
        if ((CtxtIn.Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
            
            auto& BlankSeq = TheParser->pushArgs();
            BlankSeq.append(std::move(Blank));
            BlankSeq.appendSeq(std::move(Trivia1));
            
            auto Blank = contextSensitiveColonParselet->parseInfixContextSensitive(Tok, CtxtIn);
                
            return TheParser->parseLoop(std::move(Blank), CtxtIn);
        }
            
        Trivia1.reset();
        
        return TheParser->parseLoop(std::move(Blank), CtxtIn);
    }
        
    Trivia1.reset();
    
    return TheParser->parseLoop(std::move(Blank), CtxtIn);
}

NodePtr UnderParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Blank = parse0(TokIn, CtxtIn);
        
    auto Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);
    
    return parse1(std::move(Blank), Tok, CtxtIn);
}

NodePtr UnderParselet::parseInfixContextSensitive(Token TokIn, ParserContext CtxtIn) const {
    
    auto Blank = parse0(TokIn, CtxtIn);
        
    return parse4(std::move(Blank), CtxtIn);
}

NodePtr UnderParselet::parse2(NodePtr Sym2, ParserContext CtxtIn) const {
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Sym2));
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    return Blank;
}

NodePtr UnderParselet::parse3(NodePtr ErrorSym2, ParserContext CtxtIn) const {
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(ErrorSym2));
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    return Blank;
}

NodePtr UnderParselet::parse4(NodePtr Blank, ParserContext CtxtIn) const {
    
    auto Args = TheParser->popArgs();
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));
    
    auto Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);
    
    return parse1(std::move(Pat), Tok, CtxtIn);
}


NodePtr UnderDotParselet::parse0(Token TokIn, ParserContext Ctxt) const {
    
    auto UnderDot = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    return UnderDot;
}

NodePtr UnderDotParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    auto Blank = parse0(TokIn, CtxtIn);
        
    return TheParser->parseLoop(std::move(Blank), CtxtIn);
}

NodePtr UnderDotParselet::parseInfixContextSensitive(Token TokIn, ParserContext CtxtIn) const {
    
    auto Blank = parse0(TokIn, CtxtIn);
        
    return parse1(std::move(Blank), CtxtIn);
}

NodePtr UnderDotParselet::parse1(NodePtr Blank, ParserContext CtxtIn) const {
    
    auto Args = TheParser->popArgs();
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
    
    return TheParser->parseLoop(std::move(Pat), CtxtIn);
}
