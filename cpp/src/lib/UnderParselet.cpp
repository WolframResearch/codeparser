
#include "Parselet.h"
#include "ParseletRegistration.h" // for contextSensitiveSymbolParselet
#include "Symbol.h"

void UnderParselet_parse0(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    auto Under = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    auto Tok = TheParser->currentToken(Ctxt, TOPLEVEL);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& Args = TheParser->pushArgs();
        Args.append(std::move(Under));
        
        SymbolParselet_parsePrefixContextSensitive(contextSensitiveSymbolParselet, Tok, Ctxt);
        
//        MUSTTAIL
        return UnderParselet_parse2(P, Ctxt);
    }
    
    if (Tok.Tok == TOKEN_ERROR_EXPECTEDLETTERLIKE) {
        
        auto& Args = TheParser->pushArgs();
        Args.append(std::move(Under));
        
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //
        
        auto P2 = prefixParselets[Tok.Tok.value()];
        
        (P2->parsePrefix())(P2, Tok, Ctxt);
        
//        MUSTTAIL
        return UnderParselet_parse3(P, Ctxt);
    }
        
    auto Blank = std::move(Under);
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse1(ParseletPtr P, Token Tok, ParserContext CtxtIn) {
    
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
            
            return ColonParselet_parseInfixContextSensitive(contextSensitiveColonParselet, Tok, CtxtIn);
        }
            
        Trivia1.reset();
        
        return Parser_parseLoop(CtxtIn);
    }
        
    Trivia1.reset();
    
    return Parser_parseLoop(CtxtIn);
}

ParseFunction UnderParselet::parsePrefix() const {
    return UnderParselet_parsePrefix;
}

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    UnderParselet_parse0(P, TokIn, CtxtIn);
        
    auto Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);
    
    MUSTTAIL
    return UnderParselet_parse1(P, Tok, CtxtIn);
}

ParseFunction UnderParselet::parseInfixContextSensitive() const {
    return UnderParselet_parseInfixContextSensitive;
}

void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    UnderParselet_parse0(P, TokIn, CtxtIn);
    
//    MUSTTAIL
    return UnderParselet_parse4(P, CtxtIn);
}

void UnderParselet_parse2(ParseletPtr P, ParserContext CtxtIn) {
    
    auto Sym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Sym2));
    
    auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse3(ParseletPtr P, ParserContext CtxtIn) {
    
    auto ErrorSym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(ErrorSym2));
    
    auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse4(ParseletPtr P, ParserContext CtxtIn) {
    
    Token Tok;
    {
        auto Blank = TheParser->popNode();

        auto Args = TheParser->popArgs();

        Args.append(NodePtr(std::move(Blank)));
        
        auto& PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
        
        auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));

        Tok = TheParser->currentToken(CtxtIn, TOPLEVEL);

        TheParser->pushNode(std::move(Pat));
    }
    
//    MUSTTAIL
    return UnderParselet_parse1(P, Tok, CtxtIn);
}


void UnderDotParselet_parse0(ParseletPtr P, Token TokIn, ParserContext Ctxt) {
    
    auto UnderDot = NodePtr(new LeafNode(TokIn));
    
    TheParser->nextToken(TokIn);
    
    TheParser->pushNode(std::move(UnderDot));
    
    return;
}

ParseFunction UnderDotParselet::parsePrefix() const {
    return UnderDotParselet_parsePrefix;
}

void UnderDotParselet_parsePrefix(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    UnderDotParselet_parse0(P, TokIn, CtxtIn);
        
    return Parser_parseLoop(CtxtIn);
}

ParseFunction UnderDotParselet::parseInfixContextSensitive() const {
    return UnderDotParselet_parseInfixContextSensitive;
}

void UnderDotParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn, ParserContext CtxtIn) {
    
    UnderDotParselet_parse0(P, TokIn, CtxtIn);
    
//    MUSTTAIL
    return UnderDotParselet_parse1(P, CtxtIn);
}

void UnderDotParselet_parse1(ParseletPtr P, ParserContext CtxtIn) {
    
    auto Blank = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(NodePtr(std::move(Blank)));
    
    auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
    
    TheParser->pushNode(std::move(Pat));
    
//    MUSTTAIL
    return Parser_parseLoop(CtxtIn);
}
