
#include "Parselet.h"

#include "Utils.h"

//
// Symbol parselet
//

//
// parsing x in _x
//
// we know it can only be a symbol
//
// Called from other parselets
//
NodePtr SymbolParselet::parseContextSensitive(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    return std::make_shared<LeafNode>(TokIn);
}

//
// something like  x  or x_
//
NodePtr SymbolParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Sym = std::make_shared<LeafNode>(TokIn);
    
    NodeSeq Args;
    
    Args.push_back(Sym);
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    if (Tok.Tok == TOKEN_UNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
        
        Ctxt.UnderCount = UNDER_1;
        
        return underParselet->parseContextSensitive(Args, Ctxt);
    }
    if (Tok.Tok == TOKEN_UNDERUNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
        
        Ctxt.UnderCount = UNDER_2;
        
        return underParselet->parseContextSensitive(Args, Ctxt);
    }
    if (Tok.Tok == TOKEN_UNDERUNDERUNDER) {
        
        auto& underParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
        
        Ctxt.UnderCount = UNDER_3;
        
        return underParselet->parseContextSensitive(Args, Ctxt);
    }
    if (Tok.Tok == TOKEN_UNDERDOT) {
        
        Args.push_back(std::make_shared<LeafNode>(Tok));
        
        TheParser->nextToken(Ctxt);
        
        return std::make_shared<OptionalDefaultPatternNode>(Args.getVector());
    }
    
    NodeSeq ArgsTest;
    
    Tok = Utils::eatAll(Tok, Ctxt, ArgsTest);
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    // It is necessary to go to colonParselet->parse here (even though it seems non-contextSensitive)
    // because in e.g.,   a_*b:f[]    the b is the last node in the Times expression and needs to bind with :f[]
    // Parsing a_*b completely, and then parsing :f[] would be wrong.
    //
    if (!Ctxt.ColonFlag) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            Args.push_back(ArgsTest);
            
            auto& colonParselet = TheParser->findInfixParselet(Tok.Tok);
            
            return colonParselet->parse(Args, Ctxt);
        }
    }
    
    assert(TheParser->getTokenQueue().empty());
    for (auto A : ArgsTest.getVector()) {
        if (auto ALeaf = std::dynamic_pointer_cast<const LeafNode>(A)) {
            TheParser->append(ALeaf->getToken());
            continue;
        }
        
        assert(false);
    }
    
    return Sym;
}


//
// Base Operators parselets
//

NodePtr PrefixOperatorParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(TokIn, Tok, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto operand = TheParser->parse(Ctxt);
    
    Args.push_back(operand);
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok), Args.getVector());
}

NodePtr BinaryOperatorParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {

    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.push_back(Right);
    
    if (getAssociativity() == ASSOCIATIVITY_NONASSOCIATIVE) {
        
        if (auto BinLeft = std::dynamic_pointer_cast<const BinaryNode>(Left.main())) {
            
            if (BinLeft->getSymbol() == BinaryOperatorToSymbol(TokIn.Tok)) {
                
                return std::make_shared<SyntaxErrorNode>(SYNTAXERROR_NONASSOCIATIVE, Args.getVector());
            }
        }
    }
    
    return std::make_shared<BinaryNode>(BinaryOperatorToSymbol(TokIn.Tok), Args.getVector());
}

NodePtr InfixOperatorParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {

    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();

    auto lastOperatorToken = TokIn;
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {

            auto Aborted = std::make_shared<LeafNode>(Token(TOKEN_ERROR_ABORTED, "", Source()));

            return Aborted;
        }
        
        
        auto Tok = TheParser->currentToken();
        
        Tok = Utils::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
        
        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if (isInfixOperator(Tok.Tok) &&
            InfixOperatorToSymbol(Tok.Tok) == InfixOperatorToSymbol(TokIn.Tok)) {
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            lastOperatorToken = Tok;
            
            if (allowTrailing) {
                
                //
                // Something like  a;b  or  a,b
                //
                
                Tok = TheParser->nextToken(Ctxt);
                
                Tok = Utils::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
                
                if (isInfixOperator(Tok.Tok) &&
                    InfixOperatorToSymbol(Tok.Tok) == InfixOperatorToSymbol(TokIn.Tok)) {
                    
                    //
                    // Something like  a; ;
                    //
                    
                    lastOperatorToken = Tok;
                    
                    Args.push_back(std::make_shared<LeafNode>(Token(TOKEN_FAKE_NULL, "", Source(lastOperatorToken.Span.lines.start))));
                    
                } else if (TheParser->isPossibleBeginningOfExpression(Tok, Ctxt)) {
                    
                    auto operand = TheParser->parse(Ctxt);
                    
                    Args.push_back(operand);
                    
                } else {
                    
                    //
                    // Not beginning of an expression
                    //
                    // For example:  a;&
                    //
                    
                    Args.push_back(std::make_shared<LeafNode>(Token(TOKEN_FAKE_NULL, "", Source(lastOperatorToken.Span.lines.end))));
                    
                    break;
                }
                
            } else {
                
                Tok = TheParser->nextToken(Ctxt);
                
                Tok = Utils::eatAll(Tok, Ctxt, Args);
                
                auto operand = TheParser->parse(Ctxt);
                
                Args.push_back(operand);
            }
            
        } else {
            
            //
            // Tok.Tok != TokIn.Tok, so break
            //
            
            break;
        }
        
    } // while
    
    return std::make_shared<InfixNode>(InfixOperatorToSymbol(TokIn.Tok), Args.getVector());
}

NodePtr PostfixOperatorParselet::parse(NodeSeq Operand, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    Args.push_back(Operand);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    Utils::differentLineWarning(Operand, TokIn, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    return std::make_shared<PostfixNode>(PostfixOperatorToSymbol(TokIn.Tok), Args.getVector());
}




//
// Group parselets
//

NodePtr GroupParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto Opener = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(Opener));
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    auto& Op = GroupOpenerToSymbol(Opener.Tok);
    
    auto CloserTok = GroupOpenerToCloser(Opener.Tok);
    Ctxt.Closer = CloserTok;
    
    //
    // There will only be 1 "good" node (either a LeafNode or a CommaNode)
    // But there might be multiple error nodes
    //
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {

            auto Aborted = std::make_shared<LeafNode>(Token(TOKEN_ERROR_ABORTED, "", Source()));

            return Aborted;
        }
        
        
        auto Tok = TheParser->currentToken();
        
        Tok = Utils::eatAll(Tok, Ctxt, Args);
        
        if (Tok.Tok == CloserTok) {
            
            //
            // Everything is good
            //
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            
            TheParser->nextToken(Ctxt2);
            
            auto group = std::make_shared<GroupNode>(Op, Args.getVector());
            
            return group;
        }
        if (isCloser(Tok.Tok)) {
            
            //
            // some other closer
            //
            // e.g.,   { ( a }  or  { a ) }
            //
            
            auto MissingOpener = GroupCloserToOpener(Tok.Tok);
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            auto& MissingOpenerSymbol = GroupOpenerToSymbol(MissingOpener);
            
            auto group = std::make_shared<GroupMissingOpenerNode>(MissingOpenerSymbol, Args.getVector());
            
            return group;
        }
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   { a EOF
            //
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            auto& Op = GroupOpenerToSymbol(Opener.Tok);
            
            auto group = std::make_shared<GroupMissingCloserNode>(Op, Args.getVector());
            
            return group;
        }
        
        //
        // Handle the expression
        //
        
        Tok = Utils::eatAll(Tok, Ctxt, Args);
        
        auto Ctxt2 = Ctxt;
        Ctxt2.ColonFlag = false;
        Ctxt2.Prec = PRECEDENCE_LOWEST;
        Ctxt2.Assoc = ASSOCIATIVITY_NONE;
        Ctxt2.UnderCount = UNDER_UNKNOWN;
        
        auto operand = TheParser->parse(Ctxt2);
        
        Args.push_back(operand);
    }
}


//
// Call parselets
//

NodePtr CallParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto TokIn = TheParser->currentToken();

    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.push_back(Right);
    
    //
    // Only warn if  Symbol [1]
    //
    // Stuff like   # & [1]  occurs a lot and it is stylistic
    //
    auto LeftLast = Left.last();
    while (auto LeftLastCall = std::dynamic_pointer_cast<const CallNode>(LeftLast)) {
        auto Children = LeftLastCall->getChildren();
        auto L = Children[Children.size()-1];
        LeftLast = L;
    }
    if (auto LeftLastLeaf = std::dynamic_pointer_cast<const LeafNode>(LeftLast)) {
        
        auto T = LeftLastLeaf->getToken();
        
        Utils::notContiguousWarning(T, TokIn);
    }
    
    assert(std::dynamic_pointer_cast<const GroupNode>(Right) ||
           std::dynamic_pointer_cast<const GroupMissingCloserNode>(Right) ||
           std::dynamic_pointer_cast<const GroupMissingOpenerNode>(Right));
    
    return std::make_shared<CallNode>(Left.getVector(), Args.getVector());
}


//
// StartOfLine
//

NodePtr StartOfLineParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.StringifyCurrentLine = true;
        
    TheParser->nextToken(Ctxt);
    
    Ctxt.StringifyCurrentLine = false;
    
    //
    // Cannot use TheParser->findPrefixParselet(TOKEN_STRING) here because TOKEN_ERROR_EMPTYSTRING
    // may be returned, and we have to handle that also
    //
    // So just use general parse
    //
    
    auto Operand = TheParser->parse(Ctxt);
    
    Args.push_back(Operand);
    
    return std::make_shared<StartOfLineNode>(StartOfLineOperatorToSymbol(TokIn.Tok), Args.getVector());
}






//
// Special parselets
//

//
// prefix
//
// Something like  _a
//
NodePtr UnderParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->findContextSensitivePrefixParselet(Tok.Tok);
        
        auto Sym2 = symbolParselet->parseContextSensitive(Ctxt);
        
        Args.push_back(Sym2);
    }
    
    std::shared_ptr<const Node> BlankTmp = nullptr;
    switch (TokIn.Tok) {
        case TOKEN_UNDER:
            BlankTmp = std::make_shared<BlankNode>(Args.getVector());
            break;
        case TOKEN_UNDERUNDER:
            BlankTmp = std::make_shared<BlankSequenceNode>(Args.getVector());
            break;
        case TOKEN_UNDERUNDERUNDER:
            BlankTmp = std::make_shared<BlankNullSequenceNode>(Args.getVector());
            break;
        default:
            assert(false);
            break;
    }
    NodePtr Blank = BlankTmp;
    
    
    Tok = TheParser->currentToken();
    
    Tok = Utils::eatAndPreserveToplevelNewlines(Tok, CtxtIn, Args);
    
    //
    // For something like _:""  when parsing _
    // ColonFlag == false
    // the : here is Optional, and so we want to go parse with ColonParselet's parseContextSensitive method
    //
    // For something like a:_:""  when parsing _
    // ColonFlag == true
    // make sure to not parse the second : here
    // We are already inside ColonParselet from the first :, and so ColonParselet will also handle the second :
    //
    if (!Ctxt.ColonFlag) {
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto& colonParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);
            
            NodeSeq BlankSeq;
            BlankSeq.push_back(Blank);
            return colonParselet->parseContextSensitive(BlankSeq, Ctxt);
        }
    }
    
    return Blank;
}

//
// infix
//
// Something like  a_b
//
// Called from other parselets
//
NodePtr UnderParselet::parseContextSensitive(NodeSeq Left, ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto UnderCount = CtxtIn.UnderCount;
    
    auto Ctxt = CtxtIn;
    Ctxt.UnderCount = UNDER_UNKNOWN;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto& symbolParselet = TheParser->findContextSensitivePrefixParselet(Tok.Tok);
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);
        
        Args.push_back(Right);
    }
    
    std::shared_ptr<const Node> PatTmp = nullptr;
    switch (UnderCount) {
        case UNDER_1:
            PatTmp = std::make_shared<PatternBlankNode>(Args.getVector());
            break;
        case UNDER_2:
            PatTmp = std::make_shared<PatternBlankSequenceNode>(Args.getVector());
            break;
        case UNDER_3:
            PatTmp = std::make_shared<PatternBlankNullSequenceNode>(Args.getVector());
            break;
        default:
            assert(false);
            break;
    }
    NodePtr Pat = PatTmp;
    
    Tok = TheParser->currentToken();
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    //
    // For something like a:b_c:d when parsing _
    // ColonFlag == true
    //
    if (!Ctxt.ColonFlag) {
        
        if (Tok.Tok == TOKEN_COLON) {

            auto& colonParselet = TheParser->findContextSensitiveInfixParselet(Tok.Tok);

            NodeSeq PatSeq;
            PatSeq.push_back(Pat);
            return colonParselet->parseContextSensitive(PatSeq, Ctxt);
        }
    }
    
    return Pat;
}

//
// Something like  a ~f~ b
//
NodePtr TildeParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {

    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto FirstTilde = TheParser->currentToken();

    Args.push_back(std::make_shared<LeafNode>(FirstTilde));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(FirstTilde, Tok, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto FirstTok = Tok;
    
    auto Middle = TheParser->parse(Ctxt);

    Args.push_back(Middle);
    
    Tok = TheParser->currentToken();
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(FirstTok, Tok, SYNTAXISSUESEVERITY_FORMATTING);
    
    if (Tok.Tok == TOKEN_TILDE) {
        
        Args.push_back(std::make_shared<LeafNode>(Tok));
        
        Tok = TheParser->nextToken(Ctxt);
        
        Tok = Utils::eatAll(Tok, Ctxt, Args);
        
        auto Right = TheParser->parse(Ctxt);
        
        Args.push_back(Right);
        
        return std::make_shared<TernaryNode>(SYMBOL_TERNARYTILDE, Args.getVector());
    }
        
    //
    // Something like   a ~f b
    //
    // Proceed with parsing as if the second ~ were present
    //
    
    if (Tok.Tok == TOKEN_ENDOFFILE) {
        
        //
        // If EndOfFile, don't bother trying to parse
        //
        
        Args.push_back(std::make_shared<LeafNode>(Tok));
        
        auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDTILDE, Args.getVector());
        
        return Error;
    }
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.push_back(Right);
    
    auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDTILDE, Args.getVector());
    
    return Error;
}



//
// Something like  symbol:object
//
// when parsing a in a:b  then ColonFlag is false
// when parsing b in a:b  then ColonFlag is true
//
NodePtr ColonParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {
    
    assert(!CtxtIn.ColonFlag);
    
    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.ColonFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.push_back(Right);
    
    if (auto LeftLeaf = std::dynamic_pointer_cast<const LeafNode>(Left.main())) {
        
        if (LeftLeaf->getToken().Tok == TOKEN_SYMBOL) {
            
            auto Pat = std::make_shared<BinaryNode>(SYMBOL_PATTERN, Args.getVector());
            
            NodeSeq PatSeq;
            PatSeq.push_back(Pat);
            
            auto Tok = TheParser->currentToken();
            
            Tok = Utils::eatAndPreserveToplevelNewlines(Tok, CtxtIn, PatSeq);
            
            if (Tok.Tok == TOKEN_COLON) {
                
                Ctxt.ColonFlag = false;
                
                return parseContextSensitive(PatSeq, Ctxt);
            }
            
            return Pat;
        }
    }
    
    auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDSYMBOL, Args.getVector());
    
    return Error;
}

//
// Something like  pattern:optional
//
// Called from other parselets
//
NodePtr ColonParselet::parseContextSensitive(NodeSeq Left, ParserContext CtxtIn) const {

    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert(!CtxtIn.ColonFlag);
    
    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.push_back(Right);
    
    return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Args.getVector());
}

//
// Something like  a /: b = c
//
NodePtr SlashColonParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {

    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    auto Middle = TheParser->parse(Ctxt);
    
    if (auto BinaryMiddle = std::dynamic_pointer_cast<const BinaryNode>(Middle)) {
        
        if (BinaryMiddle->getSymbol() == SYMBOL_SET) {
            
            auto MiddleChildren = Middle->getChildren();
            
            Args.push_back(MiddleChildren);
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSET, Args.getVector());
        }
        if (BinaryMiddle->getSymbol() == SYMBOL_SETDELAYED) {
            
            auto MiddleChildren = Middle->getChildren();
            
            Args.push_back(MiddleChildren);
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSETDELAYED, Args.getVector());
        }
        if (BinaryMiddle->getSymbol() == SYMBOL_UNSET) {
            
            auto MiddleChildren = Middle->getChildren();
            
            Args.push_back(MiddleChildren);
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGUNSET, Args.getVector());
        }
    }

    //
    // Anything other than:
    // a /: b = c
    // a /: b := c
    // a /: b =.
    //
    
    Args.push_back(Middle);
    
    auto Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDSET, Args.getVector());
    
    return Error;
}



//
// Something like  \( x \)
//
NodePtr LinearSyntaxOpenParenParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto Opener = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    Ctxt.LinearSyntaxFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    auto CloserTok = TOKEN_LINEARSYNTAX_CLOSEPAREN;
    Ctxt.Closer = CloserTok;
    
    Args.push_back(std::make_shared<LeafNode>(Opener));
    
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            auto Aborted = std::make_shared<LeafNode>(Token(TOKEN_ERROR_ABORTED, "", Source()));
            
            return Aborted;
        }
        
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   \( a EOF
            //
            
            auto group = std::make_shared<GroupMissingCloserNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, Args.getVector());
            
            return group;
        }
        if (Tok.Tok == CloserTok) {
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            
            TheParser->nextToken(Ctxt2);
            
            break;
        }
        
        //
        // Do not check for other closers here
        //
        // As long as \( \) parses by just doing tokenization, then cannot reliably test for other closers
        //
        // e.g., \( ( \) is completely valid syntax
        //
        
        if (Tok.Tok == TOKEN_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse(Ctxt);
            
            if (auto SubOpenParen = std::dynamic_pointer_cast<const GroupNode>(Sub)) {
                
                assert(SubOpenParen->getOperator() == SYMBOL_GROUPLINEARSYNTAXPAREN);
                
                Args.push_back(SubOpenParen);
                
                Tok = TheParser->currentToken();
                
            } else if (auto SubOpenParen = std::dynamic_pointer_cast<const GroupMissingCloserNode>(Sub)) {
                
                assert(SubOpenParen->getOperator() == SYMBOL_GROUPLINEARSYNTAXPAREN);
                
                Args.push_back(SubOpenParen);
                
                Tok = TheParser->currentToken();
                
            } else {
                assert(false);
            }
            
        } else {
            
            //
            // COMMENT, WHITESPACE, and NEWLINE are handled here
            //
            
            Args.push_back(std::make_shared<LeafNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt);
        }

    } // while
    
    return std::make_shared<GroupNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, Args.getVector());
}

//
// Something like  a =.
//
NodePtr EqualParselet::parse(NodeSeq Left, ParserContext CtxtIn) const {

    NodeSeq Args;
    
    Args.push_back(Left);
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    if (Tok.Tok == TOKEN_DOT) {

        //
        // Something like a =  .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //
        
        Utils::notContiguousWarning(TokIn, Tok);
        
        TheParser->nextToken(Ctxt);
        
        auto Empty = std::make_shared<LeafNode>(Tok);
        
        Args.push_back(std::make_shared<LeafNode>(Tok));
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET, Args.getVector());
    }
    
    auto Right = TheParser->parse(Ctxt);
    
    Args.push_back(Right);
    
    return std::make_shared<BinaryNode>(SYMBOL_SET, Args.getVector());
}


//
// Something like  \[Integral] f \[DifferentialD] x
//
NodePtr IntegralParselet::parse(ParserContext CtxtIn) const {
    
    NodeSeq Args;
    
    auto TokIn = TheParser->currentToken();
    
    Args.push_back(std::make_shared<LeafNode>(TokIn));
    
    auto Ctxt = CtxtIn;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.IntegralFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt);
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    Utils::differentLineWarning(TokIn, TokIn, SYNTAXISSUESEVERITY_FORMATTING);
    
    auto operand = TheParser->parse(Ctxt);
    
    Args.push_back(operand);
    
    Ctxt.IntegralFlag = false;
    
    Tok = TheParser->currentToken();
    
    Tok = Utils::eatAll(Tok, Ctxt, Args);
    
    if (Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD) {
        
        auto variable = TheParser->parse(Ctxt);
        
        Args.push_back(variable);
        
        return std::make_shared<PrefixBinaryNode>(PrefixBinaryOperatorToSymbol(TokIn.Tok), Args.getVector());
    }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok), Args.getVector());
}




//
// Error handling and Cleanup
//

NodePtr ExpectedPossibleExpressionErrorParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt);
    
    if (isError(TokIn.Tok)) {
        
        //
        // If there is a Token error, then use that specific error
        //
        
        auto SyntaxErrorEnum = TokenErrorToSyntaxError(TokIn.Tok);
        
        auto Error = std::make_shared<SyntaxErrorNode>(SyntaxErrorEnum, std::vector<NodePtr> { std::make_shared<LeafNode>(TokIn) });
        
        return Error;
    }
        
    //
    // If NOT a Token error, then just use a generic error
    //
    
    auto SyntaxErrorEnum = SYNTAXERROR_EXPECTEDPOSSIBLEEXPRESSION;
    
    auto Error = std::make_shared<SyntaxErrorNode>(SyntaxErrorEnum, std::vector<NodePtr> { std::make_shared<LeafNode>(TokIn) });
    
    return Error;
}

