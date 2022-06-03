
#include "Parselet.h"
#include "ParseletRegistration.h" // for contextSensitiveSymbolParselet
#include "Symbol.h"

void UnderParselet::parse0(Token TokIn, ParserContext Ctxt) const {
    
    auto Under = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& Args = TheParser->pushArgs();
        Args.append(std::move(Under));
        
        contextSensitiveSymbolParselet->parsePrefixContextSensitive(Tok, Ctxt);
            
        return parse2(Ctxt);
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
        
        parselet->parsePrefix(Tok, Ctxt);
            
        return parse3(Ctxt);
    }
        
    auto Blank = std::move(Under);
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet::parse1(Token Tok, ParserContext CtxtIn) const {
    
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
            
            auto Blank = TheParser->popNode();
            
            auto& BlankSeq = TheParser->pushArgs();
            BlankSeq.append(std::move(Blank));
            BlankSeq.appendSeq(std::move(Trivia1));
            
            contextSensitiveColonParselet->parseInfixContextSensitive(Tok, CtxtIn);
                
            return TheParser->parseLoop(CtxtIn);
        }
            
        Trivia1.reset();
        
        return TheParser->parseLoop(CtxtIn);
    }
        
    Trivia1.reset();
    
    return TheParser->parseLoop(CtxtIn);
}

void UnderParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    parse0(TokIn, CtxtIn);
        
    auto Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);
    
    return parse1(Tok, CtxtIn);
}

void UnderParselet::parseInfixContextSensitive(Token TokIn, ParserContext CtxtIn) const {
    
    parse0(TokIn, CtxtIn);
        
    return parse4(CtxtIn);
}

void UnderParselet::parse2(ParserContext CtxtIn) const {
    
    auto Sym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Sym2));
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet::parse3(ParserContext CtxtIn) const {
    
    auto ErrorSym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(ErrorSym2));
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet::parse4(ParserContext CtxtIn) const {
    
    auto Blank = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));
    
    auto Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);
    
    TheParser->pushNode(std::move(Pat));
    
    return parse1(Tok, CtxtIn);
}


void UnderDotParselet::parse0(Token TokIn, ParserContext Ctxt) const {
    
    auto UnderDot = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    TheParser->pushNode(std::move(UnderDot));
    
    return;
}

void UnderDotParselet::parsePrefix(Token TokIn, ParserContext CtxtIn) const {
    
    parse0(TokIn, CtxtIn);
        
    return TheParser->parseLoop(CtxtIn);
}

void UnderDotParselet::parseInfixContextSensitive(Token TokIn, ParserContext CtxtIn) const {
    
    parse0(TokIn, CtxtIn);
        
    return parse1(CtxtIn);
}

void UnderDotParselet::parse1(ParserContext CtxtIn) const {
    
    auto Blank = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
    
    TheParser->pushNode(std::move(Pat));
    
//    MUSTTAIL
    return TheParser->parseLoop(CtxtIn);
}
