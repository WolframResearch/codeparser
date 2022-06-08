
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

void UnderParselet_parse1(ParseletPtr P, Token Ignored) {

    TriviaSeq Trivia1;

    auto Tok = TheParser->currentToken(TOPLEVEL);
    Tok = TheParser->eatTriviaButNotToplevelNewlines(Tok, TOPLEVEL, Trivia1);

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

        if ((TheParser->topContext().Flag & PARSER_INSIDE_COLON) != PARSER_INSIDE_COLON) {
            
            auto& BlankSeq = TheParser->pushArgs();
            
            TheParser->shift();
            
            BlankSeq.appendSeq(std::move(Trivia1));
            
//            MUSTTAIL probably not doable
            return ColonParselet_parseInfixContextSensitive(colonParselet, Tok);
        }

        Trivia1.reset();

//        MUSTTAIL probably not doable
        return Parser_parseClimb(nullptr, Ignored);
    }

    Trivia1.reset();

//    MUSTTAIL probably not doable
    return Parser_parseClimb(nullptr, Ignored);
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
    return UnderParselet_parse1(P, Token());
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
    
    auto Sym2 = TheParser->popNode();
    
    auto Args = TheParser->popArgs();
    
    Args.append(std::move(Sym2));
    
    auto& BOp = dynamic_cast<UnderParselet *>(P)->getBOp();
    
    auto Blank = NodePtr(new CompoundNode(BOp, std::move(Args)));
    
    TheParser->pushNode(std::move(Blank));
    
    return;
}

void UnderParselet_parse4(ParseletPtr P, Token Ignored) {
    
    {
        auto Blank = TheParser->popNode();

        auto Args = TheParser->popArgs();

        Args.append(NodePtr(std::move(Blank)));
        
        auto& PBOp = dynamic_cast<UnderParselet *>(P)->getPBOp();
        
        auto Pat = NodePtr(new CompoundNode(PBOp, std::move(Args)));

        TheParser->pushNode(std::move(Pat));
    }
    
    MUSTTAIL
    return UnderParselet_parse1(P, Ignored);
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
        auto Blank = TheParser->popNode();
        
        auto Args = TheParser->popArgs();
        
        Args.append(NodePtr(std::move(Blank)));
        
        auto Pat = NodePtr(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));
        
        TheParser->pushNode(std::move(Pat));
    }
        
    MUSTTAIL
    return Parser_parseClimb(nullptr, Ignored);
}
