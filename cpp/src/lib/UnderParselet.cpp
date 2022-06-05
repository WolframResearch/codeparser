
#include "Parselet.h"
#include "ParseletRegistration.h" // for contextSensitiveSymbolParselet
#include "Symbol.h"

void UnderParselet_parse0(ParseletPtr P, Token Ignored, ParserContext Ctxt) {
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        {
            auto Under = TheParser->popNode();
            
            auto& Args = TheParser->pushArgs();
            
            Args.append(std::move(Under));
        }
        
//        xxx;
        SymbolParselet_parsePrefixContextSensitive(symbolParselet, Tok, Ctxt);
        
        MUSTTAIL
        return UnderParselet_parse2(P, Ignored, Ctxt);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        {
            auto Under = TheParser->popNode();
            
            auto& Args = TheParser->pushArgs();
            
            Args.append(std::move(Under));
        }
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        auto P2 = prefixParselets[Tok.Tok.value()];
        
//        xxx;
        (P2->parsePrefix())(P2, Tok, Ctxt);
        
        MUSTTAIL
        return UnderParselet_parse3(P, Ignored, Ctxt);
    }
    
//    auto Under = TheParser->popNode();
//
//    auto Blank = std::move(Under);
//
//    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse1(ParseletPtr P, Token Ignored, ParserContext CtxtIn) {
    
    TriviaSeq Trivia1;
    
    auto Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);
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
            
            return ColonParselet_parseInfixContextSensitive(colonParselet, Tok, CtxtIn);
        }
            
        Trivia1.reset();
        
//        MUSTTAIL probably not doable
        return Parser_parseLoop(nullptr, Ignored, CtxtIn);
    }
        
    Trivia1.reset();
    
//    MUSTTAIL probably not doable
    return Parser_parseLoop(nullptr, Ignored, CtxtIn);
}

ParseFunction UnderParselet::parsePrefix() const {
    return UnderParselet_parsePrefix;
}

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    {
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Under));
    }
    
//    xxx;
    UnderParselet_parse0(P, Token(), CtxtIn);
    
    MUSTTAIL
    return UnderParselet_parse1(P, Token(), CtxtIn);
}

ParseFunction UnderParselet::parseInfixContextSensitive() const {
    return UnderParselet_parseInfixContextSensitive;
}

void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    {
        auto Under = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(Under));
    }
    
//    xxx;
    UnderParselet_parse0(P, Token(), CtxtIn);
    
    MUSTTAIL
    return UnderParselet_parse4(P, Token(), CtxtIn);
}

void UnderParselet_parse2(ParseletPtr P, Token Ignored, ParserContext CtxtIn) {
    
    auto Sym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Sym2));
    
    auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse3(ParseletPtr P, Token Ignored, ParserContext CtxtIn) {
    
    auto ErrorSym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(ErrorSym2));
    
    auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse4(ParseletPtr P, Token Ignored, ParserContext CtxtIn) {
    
    {
        auto Blank = TheParser->popNode();

        auto Args = TheParser->popArgs();

        Args.append(NodePtr(std::move(Blank)));
        
        auto& PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
        
        auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));

        TheParser->pushNode(std::move(Pat));
    }
    
    MUSTTAIL
    return UnderParselet_parse1(P, Ignored, CtxtIn);
}


ParseFunction UnderDotParselet::parsePrefix() const {
    return UnderDotParselet_parsePrefix;
}

void UnderDotParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    {
        auto UnderDot = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(UnderDot));
    }
    
    MUSTTAIL
    return Parser_parseLoop(nullptr, Token(), CtxtIn);
}

ParseFunction UnderDotParselet::parseInfixContextSensitive() const {
    return UnderDotParselet_parseInfixContextSensitive;
}

void UnderDotParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    {
        auto UnderDot = NodePtr(new LeafNode(TokIn));
        
        TheParser->nextToken(TokIn);
        
        TheParser->pushNode(std::move(UnderDot));
    }
    
    MUSTTAIL
    return UnderDotParselet_parse1(P, Token(), CtxtIn);
}

void UnderDotParselet_parse1(ParseletPtr P, Token Ignored, ParserContext CtxtIn) {
    
    {
        auto Blank = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(NodePtr(std::move(Blank)));
        
        auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
        
        TheParser->pushNode(std::move(Pat));
    }
        
    MUSTTAIL
    return Parser_parseLoop(nullptr, Ignored, CtxtIn);
}
