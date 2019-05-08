
#include "Parselet.h"

//
// Literal parselets
//

//
// parsing x in _x
//
// we know it can only be a symbol
//
std::shared_ptr<Node> SymbolParselet::parseContextSensitive(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
}

std::shared_ptr<Node> IntegerParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<IntegerNode>(TokIn.Str, TokIn.Span);
}

std::shared_ptr<Node> RealParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<RealNode>(TokIn.Str, TokIn.Span);
}

std::shared_ptr<Node> StringParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();

    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<StringNode>(TokIn.Str, TokIn.Span);
}

std::shared_ptr<Node> HashParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SlotNode>(TokIn.Str, TokIn.Span);
}

std::shared_ptr<Node> HashHashParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SlotSequenceNode>(TokIn.Str, TokIn.Span);
}

std::shared_ptr<Node> PercentParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OutNode>(TokIn.Str, TokIn.Span);
}

//
// prefix
//
std::shared_ptr<Node> UnderDotParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OptionalDefaultNode>(TokIn.Span);
}




//
// Special Literal parselets
//

std::shared_ptr<Node> SymbolParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_EVERYTHING);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    if (Tok.Tok == TOKEN_UNDER) {
        
        auto Sym = std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
        
        auto underParselet = dynamic_cast<UnderParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
        
        //
        // Set UnderCount
        //
        Ctxt.UnderCount = UNDER_1;
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
        
    } else if (Tok.Tok == TOKEN_UNDERUNDER) {
        
        auto Sym = std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
        
        auto underParselet = dynamic_cast<UnderParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
        
        //
        // Set UnderCount
        //
        Ctxt.UnderCount = UNDER_2;
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
        
    } else if (Tok.Tok == TOKEN_UNDERUNDERUNDER) {
        
        auto Sym = std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
        
        auto underParselet = dynamic_cast<UnderParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
        
        //
        // Set UnderCount
        //
        Ctxt.UnderCount = UNDER_3;
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
        
    } else if (Tok.Tok == TOKEN_UNDERDOT) {
        
        auto Sym = std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
        
        auto underParselet = dynamic_cast<UnderDotParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
    }
    
    if (Tok.Tok == TOKEN_COMMENT) {
        while (Tok.Tok == TOKEN_COMMENT) {
            Tok = TheParser->nextToken(Ctxt, PRESERVE_EVERYTHING);
        }
    }
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    // It is necessary to go to colonParselet->parse here (even though it seems non-contextSensitive)
    // because in e.g.,   a_*b:f[]    the b is the last node in the Times expression and needs to bind with :f[]
    // Parsing a_*b completely, and then parsing :f[] would be wrong.
    //
    if (!Ctxt.ColonFlag) {
        
        Tok = TheParser->tryNextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
        
        auto Sym = std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
            
            return colonParselet->parse(Sym, Ctxt);
        }
        
        return Sym;
    }
    
    TheParser->tryNextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    auto Sym = std::make_shared<SymbolNode>(TokIn.Str, TokIn.Span);
    
    return Sym;
}




//
// Base Operators parselets
//

std::shared_ptr<Node> PrefixOperatorParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);

    auto operand = TheParser->parse(Ctxt);
    
    if (Tok.Tok != TOKEN_ENDOFFILE && TokIn.Span.lines.end.Line != operand->getSourceSpan().lines.start.Line) {

        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``" + TokIn.Str + "`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(TokIn.Span.lines.start, operand->getSourceSpan().lines.end));
    
        TheParser->addIssue(Issue);
    }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok),
        std::make_shared<TokenNode>(TokIn),
        operand);
}

std::shared_ptr<Node> BinaryOperatorParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto Right = TheParser->parse(Ctxt);
    
    if (getAssociativity() == ASSOCIATIVITY_NONASSOCIATIVE) {
        
        if (auto BinLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
            
            if (BinLeft->getSymbol() == BinaryOperatorToSymbol(TokIn.Tok)) {
                
                std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_NONASSOCIATIVE,
                                                                                std::vector<std::shared_ptr<Node>> {
                                                                                    Left,
                                                                                    std::make_shared<TokenNode>(TokIn),
                                                                                    Right });
                
                return Error;
            }
        }
    }
    
    return std::make_shared<BinaryNode>(BinaryOperatorToSymbol(TokIn.Tok),
                                        Left,
                                        std::make_shared<TokenNode>(TokIn),
                                        Right);
}

std::shared_ptr<Node> InfixOperatorParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();

    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    auto breadth = 1;
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            return nullptr;
        }
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached.\nConsider breaking up into smaller expressions."), SYNTAXISSUESEVERITY_FORMATTING, Span);
        
            TheParser->addIssue(Issue);
        }

        auto Tok = TheParser->currentToken();
        
        if (Tok.Tok == TokIn.Tok) {
            
            Args.push_back(std::make_shared<TokenNode>(Tok));
            
            TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
            
            auto operand = TheParser->parse(Ctxt);
            
            Args.push_back(operand);
            
        } else {
            break;
        }

        breadth++;
    } // while
    
    return std::make_shared<InfixNode>(InfixOperatorToSymbol(TokIn.Tok), Args);
}

std::shared_ptr<Node> PostfixOperatorParselet::parse(std::shared_ptr<Node> Operand, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);

    if (Tok.Tok != TOKEN_ENDOFFILE && Operand->getSourceSpan().lines.end.Line != TokIn.Span.lines.start.Line) {

        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``" + TokIn.Str + "`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(Operand->getSourceSpan().lines.start, TokIn.Span.lines.end));
    
        TheParser->addIssue(Issue);
    }
    
    return std::make_shared<PostfixNode>(PostfixOperatorToSymbol(TokIn.Tok),
                                         Operand,
                                         std::make_shared<TokenNode>(TokIn));
}




//
// Group parselets
//

std::shared_ptr<Node> GroupParselet::parse(ParserContext CtxtIn) const {
    
    auto Opener = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    Ctxt.OperatorDepth++;

    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);

    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(std::make_shared<TokenNode>(Opener));
    
    auto Op = GroupOpenerToSymbol(Opener.Tok);
    
    auto CloserTok = GroupOpenerToCloser(Opener.Tok);
    Ctxt.Closer = CloserTok;
    
    auto breadth = 0;
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            return nullptr;
        }
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached.\nConsider breaking up into smaller expressions."), SYNTAXISSUESEVERITY_FORMATTING, Span);
        
            TheParser->addIssue(Issue);
        }

        auto Tok = TheParser->currentToken();
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   { a EOF
            //
            
            Args.push_back(std::make_shared<TokenNode>(Tok));
            
            auto Op = GroupOpenerToSymbol(Opener.Tok);
            
            auto group = std::make_shared<GroupMissingCloserNode>(Op, Args);
            
            return group;
            
        } else if (Tok.Tok == CloserTok) {
            
            Args.push_back(std::make_shared<TokenNode>(Tok));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            Ctxt2.OperatorDepth--;
            
            TheParser->nextToken(Ctxt2, PRESERVE_TOPLEVEL_NEWLINES);
    
            break;
            
        } else if (isCloser(Tok.Tok)) {
            
            //
            // some other closer
            //
            // e.g.,   { ( a }  or  { a ) }
            //
            
            auto MissingOpener = GroupCloserToOpener(Tok.Tok);

            Args.push_back(std::make_shared<TokenNode>(Tok));

            auto MissingOpenerSymbol = GroupOpenerToSymbol(MissingOpener);

            auto group = std::make_shared<GroupMissingOpenerNode>(MissingOpenerSymbol, Args);

            return group;
            
        } else if (Tok.Tok == TOKEN_COMMA) {
            
            //
            // Reporting of commas, e.g., {1,,2} is done later
            //
            
            TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);

            auto CommaNode = std::make_shared<TokenNode>(Tok);
            
            Args.push_back(CommaNode);
            
        } else {
            
            //
            // Handle the expression
            //

            auto Ctxt2 = Ctxt;
            Ctxt2.ColonFlag = false;
            Ctxt2.LinearSyntaxFlag = false;
            Ctxt2.Prec = PRECEDENCE_LOWEST;
            Ctxt2.Assoc = ASSOCIATIVITY_NONE;
            Ctxt2.UnderCount = UNDER_UNKNOWN;
            
            auto operand = TheParser->parse(Ctxt2);
            
            if (auto GroupMissingOpener = std::dynamic_pointer_cast<GroupMissingOpenerNode>(operand)) {
                
                if (GroupMissingOpener->getOperator() == Op) {
                    
                    TheSourceManager->getIssues();
                }
                
            }
            
            Args.push_back(operand);
        }
        
        breadth++;

    } // while
    
    auto group = std::make_shared<GroupNode>(Op, Args);
    
    return group;
}


//
// Call parselets
//

CallParselet::CallParselet() : groupParselet(std::make_shared<GroupParselet>()) {}

std::shared_ptr<Node> CallParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();

    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = PRECEDENCE_HIGHEST;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Right = TheParser->parse(Ctxt);
    
    if (!isContiguous(Left->getSourceSpan(), Right->getSourceSpan())) {
        
        //
        // Only warn if symbol[1]
        //
        // Stuff like   # & [1]  occurs a lot and it is stylistic
        //
        if (std::dynamic_pointer_cast<SymbolNode>(Left) ||
            std::dynamic_pointer_cast<CallNode>(Left)) {
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_NOTCONTIGUOUS, "Head and brackets are not contiguous", SYNTAXISSUESEVERITY_FORMATTING, Source(Left->getSourceSpan().lines.end, Right->getSourceSpan().lines.start));
            
            TheParser->addIssue(Issue);
        }
    }
    
    assert(std::dynamic_pointer_cast<GroupNode>(Right) ||
           std::dynamic_pointer_cast<GroupMissingCloserNode>(Right) ||
           std::dynamic_pointer_cast<GroupMissingOpenerNode>(Right));
    
    return std::make_shared<CallNode>(Left, Right);
}


//
// StartOfLine
//

std::shared_ptr<Node> StartOfLineParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.InformationFlag = true;
        
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    Ctxt.InformationFlag = false;
    
    //
    // Cannot use TheParser->findPrefixParselet(TOKEN_STRING) here because TOKEN_ERROR_EMPTYSTRING
    // may be returned, and we have to handle that also
    //
    // So just use general parse
    //
    
    auto Operand = TheParser->parse(Ctxt);
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok),
                                        std::make_shared<TokenNode>(TokIn),
                                        Operand);
}






//
// Special parselets
//

//
// prefix
//
// Something like  _a
//
std::shared_ptr<Node> UnderParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Sym2 = nullptr;
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
        
        Sym2 = symbolParselet->parseContextSensitive(Ctxt);
    }
    
    //
    // Tok could be COMMENT here
    //
    
    Tok = TheParser->tryNextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
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
        
        std::shared_ptr<Node> Blank = nullptr;
        switch (TokIn.Tok) {
            case TOKEN_UNDER:
                Blank = std::make_shared<BlankNode>(std::make_shared<TokenNode>(TokIn), Sym2);
                break;
            case TOKEN_UNDERUNDER:
                Blank = std::make_shared<BlankSequenceNode>(std::make_shared<TokenNode>(TokIn), Sym2);
                break;
            case TOKEN_UNDERUNDERUNDER:
                Blank = std::make_shared<BlankNullSequenceNode>(std::make_shared<TokenNode>(TokIn), Sym2);
                break;
            default:
                assert(false);
                break;
        }
        
        if (Tok.Tok == TOKEN_COLON) {
            
            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
            
            return colonParselet->parseContextSensitive(Blank, Ctxt);
        }
        
        return Blank;
    }
    
    std::shared_ptr<Node> Blank = nullptr;
    switch (TokIn.Tok) {
        case TOKEN_UNDER:
            Blank = std::make_shared<BlankNode>(std::make_shared<TokenNode>(TokIn), Sym2);
            break;
        case TOKEN_UNDERUNDER:
            Blank = std::make_shared<BlankSequenceNode>(std::make_shared<TokenNode>(TokIn), Sym2);
            break;
        case TOKEN_UNDERUNDERUNDER:
            Blank = std::make_shared<BlankNullSequenceNode>(std::make_shared<TokenNode>(TokIn), Sym2);
            break;
        default:
            assert(false);
            break;
    }
    
    return Blank;
}

//
// infix
//
// Something like  a_b
//
std::shared_ptr<Node> UnderParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto UnderCount = CtxtIn.UnderCount;
    
    auto Ctxt = CtxtIn;
    Ctxt.UnderCount = UNDER_UNKNOWN;
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Right = nullptr;
    if (Tok.Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));
        
        Right = symbolParselet->parseContextSensitive(Ctxt);
    }
    
    //
    // Tok could be COMMENT here
    //
    
    Tok = TheParser->tryNextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    //
    // For something like a:b_c:d when parsing _
    // ColonFlag == true
    //
    if (!Ctxt.ColonFlag) {
        
        std::shared_ptr<Node> Pat = nullptr;
        switch (UnderCount) {
            case UNDER_1:
                Pat = std::make_shared<PatternBlankNode>(Left, std::make_shared<TokenNode>(TokIn), Right);
                break;
            case UNDER_2:
                Pat = std::make_shared<PatternBlankSequenceNode>(Left, std::make_shared<TokenNode>(TokIn), Right);
                break;
            case UNDER_3:
                Pat = std::make_shared<PatternBlankNullSequenceNode>(Left, std::make_shared<TokenNode>(TokIn), Right);
                break;
            default:
                assert(false);
                break;
        }
        
        if (Tok.Tok == TOKEN_COLON) {

            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok.Tok));

            return colonParselet->parseContextSensitive(Pat, Ctxt);
        }
        
        return Pat;
    }
    
    std::shared_ptr<Node> Pat = nullptr;
    switch (UnderCount) {
        case UNDER_1:
            Pat = std::make_shared<PatternBlankNode>(Left, std::make_shared<TokenNode>(TokIn), Right);
            break;
        case UNDER_2:
            Pat = std::make_shared<PatternBlankSequenceNode>(Left, std::make_shared<TokenNode>(TokIn), Right);
            break;
        case UNDER_3:
            Pat = std::make_shared<PatternBlankNullSequenceNode>(Left, std::make_shared<TokenNode>(TokIn), Right);
            break;
        default:
            assert(false);
            break;
    }
    
    return Pat;
}


//
// postfix
//
// Something like  a_.
//
std::shared_ptr<Node> UnderDotParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OptionalDefaultPatternNode>(Left, std::make_shared<TokenNode>(TokIn));
}


//
// Something like  a;b
//
std::shared_ptr<Node> SemiParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();

    auto lastSpan = TokIn.Span;

    auto operand = Left;

    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);

    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    Args.push_back(std::make_shared<TokenNode>(TokIn));
    
    auto eatTheNextSemi = false;
    auto lastWasSemi = true;

    if (TokIn.Tok != TOKEN_ENDOFFILE && operand->getSourceSpan().lines.end.Line != lastSpan.lines.start.Line) {
        
        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``" + TokIn.Str + "`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(operand->getSourceSpan().lines.end, lastSpan.lines.start));
        
        TheParser->addIssue(Issue);
    }
    
    auto breadth = 1;
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            return nullptr;
        }
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached.\nConsider breaking up into smaller expressions."), SYNTAXISSUESEVERITY_FORMATTING, Span);
        
            TheParser->addIssue(Issue);
        }
        
        auto Tok = TheParser->currentToken();

        if (Tok.Tok == TOKEN_NEWLINE) {
            
            if (lastWasSemi) {

                auto Empty = std::make_shared<InternalNullNode>(lastSpan);

                Args.push_back(Empty);
            }

            break;

        } else if (Tok.Tok == TOKEN_SEMI) {
            
            //
            // something like a; ; parses as CompoundExpression[a, Null, Null]
            //
            
            if (!eatTheNextSemi) {

                auto Empty = std::make_shared<InternalNullNode>(lastSpan);
                
                Args.push_back(Empty);
            }
            
            eatTheNextSemi = false;
            lastWasSemi = true;
            lastSpan = Tok.Span;

            if (Tok.Tok != TOKEN_ENDOFFILE && operand->getSourceSpan().lines.end.Line != lastSpan.lines.start.Line) {
                
                //
                // Skip DifferentLine issues if ENDOFFILE
                //
                
                auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``" + Tok.Str + "`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(operand->getSourceSpan().lines.end, lastSpan.lines.start));
            
                TheParser->addIssue(Issue);
            }
            
            Args.push_back(std::make_shared<TokenNode>(Tok));
            
            TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
            
        } else if (!TheParser->isPossibleBeginningOfExpression(Tok, Ctxt)) {
            
            //
            // A InfixNode[CompoundExpression, ...] may or may not end with a ;
            // Need to test the next token to decide
            //
            
            if (lastWasSemi) {
                
                auto Null = std::make_shared<InternalNullNode>(lastSpan);
                
                Args.push_back(Null);
            }
            
            break;
            
        } else {
            
            //
            // Parse the expression
            //

            //
            // TODO: No error checking needed inside TheParser->parse()
            //
            assert(TheParser->isPossibleBeginningOfExpression(Tok, Ctxt));
            
            operand = TheParser->parse(Ctxt);

            Args.push_back(operand);
            
            eatTheNextSemi = true;
            lastWasSemi = false;
        }

        breadth++;

    } // while
    
    return std::make_shared<InfixNode>(SYMBOL_COMPOUNDEXPRESSION, Args);
}

//
// prefix
//
// Something like  ;;a
//
std::shared_ptr<Node> SemiSemiParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok, Ctxt)) {
        
        //
        // TODO: No error checking needed inside TheParser->parse()
        //
        assert(TheParser->isPossibleBeginningOfExpression(Tok, Ctxt));
        
        auto operand = TheParser->parse(Ctxt);

        if (Tok.Tok != TOKEN_ENDOFFILE && TokIn.Span.lines.end.Line != operand->getSourceSpan().lines.start.Line) {
            
            //
            // Skip DifferentLine issues if ENDOFFILE
            //
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``;;`` is not on same line as operand", SYNTAXISSUESEVERITY_WARNING, Source(TokIn.Span.lines.start, operand->getSourceSpan().lines.end));
        
            TheParser->addIssue(Issue);
        }
        
        if (auto BinOp = std::dynamic_pointer_cast<BinaryNode>(operand)) {
            
            if (BinOp->getSymbol() == SYMBOL_SPAN) {
                
                auto SpanOpSource = BinOp->getSourceSpan();
                auto SpanOpLeft = BinOp->getLeft();
                auto SpanOp = BinOp->getOperator();
                auto SpanOpRight = BinOp->getRight();
                
                if (auto SpanOpRightEmpty = std::dynamic_pointer_cast<InternalAllNode>(SpanOpRight)) {
                    
                    //
                    // This is ;;;; or ;;a;; and is not a single Span expression
                    //
                    
                    std::shared_ptr<Node> NewLeft;
                    if (std::dynamic_pointer_cast<InternalOneNode>(SpanOpLeft)) {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN,
                            std::make_shared<InternalOneNode>(TokIn.Span),
                            std::make_shared<TokenNode>(TokIn),
                            std::make_shared<InternalAllNode>(TokIn.Span));
                    } else {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN,
                            std::make_shared<InternalOneNode>(TokIn.Span),
                            std::make_shared<TokenNode>(TokIn),
                            SpanOpLeft);
                    }
                    
                    auto NewRight = std::make_shared<BinaryNode>(SYMBOL_SPAN,
                        std::make_shared<InternalOneNode>(SpanOpSource),
                        SpanOp,
                        SpanOpRightEmpty);
                    
                    
                    auto ImplicitTimes = std::make_shared<InfixNode>(SYMBOL_IMPLICITTIMES, std::vector<std::shared_ptr<Node>>{
                        NewLeft,
                        std::make_shared<TokenNode>(Token(TOKEN_FAKE_IMPLICITTIMES, "", NewRight->getSourceSpan())),
                        NewRight });
                    
                    auto Issue = SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMESSPAN, "Implicit ``Times`` between 2 ``Span``s", SYNTAXISSUESEVERITY_WARNING, ImplicitTimes->getSourceSpan());
                    
                    TheParser->addIssue(Issue);
                    
                    return ImplicitTimes;
                }
                
                //
                // We know it is ternary Span
                //
                
                if (std::dynamic_pointer_cast<InternalOneNode>(SpanOpLeft)) {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN,
                        std::make_shared<InternalOneNode>(TokIn.Span),
                        std::make_shared<TokenNode>(TokIn),
                        std::make_shared<InternalAllNode>(TokIn.Span),
                        std::make_shared<TokenNode>(TokIn),
                        SpanOpRight);
                } else {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN,
                        std::make_shared<InternalOneNode>(TokIn.Span),
                        std::make_shared<TokenNode>(TokIn),
                        SpanOpLeft,
                        std::make_shared<TokenNode>(TokIn),
                        SpanOpRight);
                }
            }
        }
        
        return std::make_shared<BinaryNode>(SYMBOL_SPAN,
            std::make_shared<InternalOneNode>(TokIn.Span),
            std::make_shared<TokenNode>(TokIn),
            operand);
    }
    
    //
    // Not possible beginning of expression
    //
    
    return std::make_shared<BinaryNode>(SYMBOL_SPAN,
        std::make_shared<InternalOneNode>(TokIn.Span),
        std::make_shared<TokenNode>(TokIn),
        std::make_shared<InternalAllNode>(TokIn.Span));
}

//
// infix
//
// Something like  a;;b
//
std::shared_ptr<Node> SemiSemiParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    if (TokIn.Tok != TOKEN_ENDOFFILE && Left->getSourceSpan().lines.end.Line != TokIn.Span.lines.start.Line) {

        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``;;`` is not on same line as operand", SYNTAXISSUESEVERITY_WARNING, Source(Left->getSourceSpan().lines.start, TokIn.Span.lines.end));
    
        TheParser->addIssue(Issue);
    }
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok, Ctxt)) {
        
        //
        // TODO: No error checking needed inside TheParser->parse()
        //
        assert(TheParser->isPossibleBeginningOfExpression(Tok, Ctxt));
        
        auto Right = TheParser->parse(Ctxt);
        
        if (Tok.Tok != TOKEN_ENDOFFILE && TokIn.Span.lines.end.Line != Right->getSourceSpan().lines.start.Line) {
            
            //
            // Skip DifferentLine issues if ENDOFFILE
            //
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``;;`` is not on same line as operand", SYNTAXISSUESEVERITY_WARNING, Source(TokIn.Span.lines.start, Right->getSourceSpan().lines.end));
        
            TheParser->addIssue(Issue);
        }
        
        if (auto BinLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
            
            if (BinLeft->getSymbol() == SYMBOL_SPAN) {
                
                auto LeftSpanLeft = BinLeft->getLeft();
                auto LeftSpanRight = BinLeft->getRight();
                
                return std::make_shared<TernaryNode>(SYMBOL_SPAN,
                    LeftSpanLeft,
                    std::make_shared<TokenNode>(TokIn),
                    LeftSpanRight,
                    std::make_shared<TokenNode>(TokIn),
                    Right);
            }
        }
        
        if (auto BinRight = std::dynamic_pointer_cast<BinaryNode>(Right)) {
            
            if (BinRight->getSymbol() == SYMBOL_SPAN) {
                
                auto RightSpanSource = BinRight->getSourceSpan();
                auto RightSpanLeft = BinRight->getLeft();
                auto RightSpanRight = BinRight->getRight();
                
                if (auto RightSpanRightEmpty = std::dynamic_pointer_cast<InternalAllNode>(RightSpanRight)) {
                    
                    //
                    // This is a;;;; or a;;b;; and is not a single Span expression
                    //
                    
                    std::shared_ptr<Node> NewLeft;
                    if (std::dynamic_pointer_cast<InternalOneNode>(RightSpanLeft)) {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN,
                            Left,
                            std::make_shared<TokenNode>(TokIn),
                            std::make_shared<InternalAllNode>(RightSpanSource));
                    } else {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN,
                            Left,
                            std::make_shared<TokenNode>(TokIn),
                            RightSpanLeft);
                    }
                    
                    auto NewRight = std::make_shared<BinaryNode>(SYMBOL_SPAN,
                        std::make_shared<InternalOneNode>(RightSpanSource),
                        std::make_shared<TokenNode>(TokIn),
                        std::make_shared<InternalAllNode>(RightSpanSource));
                    
                    auto ImplicitTimes = std::make_shared<InfixNode>(SYMBOL_IMPLICITTIMES, std::vector<std::shared_ptr<Node>>{
                        NewLeft,
                        std::make_shared<TokenNode>(Token(TOKEN_FAKE_IMPLICITTIMES, "", NewRight->getSourceSpan())),
                        NewRight });
                    
                    auto Issue = SyntaxIssue(SYNTAXISSUETAG_IMPLICITTIMESSPAN, "Implicit ``Times`` between 2 ``Span``s", SYNTAXISSUESEVERITY_WARNING, ImplicitTimes->getSourceSpan());
                    
                    TheParser->addIssue(Issue);
                    
                    return ImplicitTimes;
                }
                
                
                //
                // We know it is ternary Span
                //
                
                if (std::dynamic_pointer_cast<InternalOneNode>(RightSpanLeft)) {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN,
                        Left,
                        std::make_shared<TokenNode>(TokIn),
                        std::make_shared<InternalAllNode>(RightSpanSource),
                        std::make_shared<TokenNode>(TokIn),
                        RightSpanRight);
                } else {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN,
                        Left,
                        std::make_shared<TokenNode>(TokIn),
                        RightSpanLeft,
                        std::make_shared<TokenNode>(TokIn),
                        RightSpanRight);
                }
            }
        }
        
        return std::make_shared<BinaryNode>(SYMBOL_SPAN,
                                            Left,
                                            std::make_shared<TokenNode>(TokIn),
                                            Right);
    }
    
    //
    // Not possible beginning of expression
    //
    
    return std::make_shared<BinaryNode>(SYMBOL_SPAN,
                                        Left,
                                        std::make_shared<TokenNode>(TokIn),
                                        std::make_shared<InternalAllNode>(TokIn.Span));
}



//
// Something  a ~f~ b
//
std::shared_ptr<Node> TildeParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    auto FirstTilde = TheParser->currentToken();

    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    auto Tok = TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto Middle = TheParser->parse(Ctxt);

    auto SecondTilde = TheParser->currentToken();
    
    if (Tok.Tok != TOKEN_ENDOFFILE && FirstTilde.Span.lines.end.Line != Middle->getSourceSpan().lines.start.Line) {
        
        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``~`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(FirstTilde.Span.lines.start, Middle->getSourceSpan().lines.end));
    
        TheParser->addIssue(Issue);
    }

    if (SecondTilde.Tok != TOKEN_ENDOFFILE && Middle->getSourceSpan().lines.end.Line != SecondTilde.Span.lines.start.Line) {
        
        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``~`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(Middle->getSourceSpan().lines.start, SecondTilde.Span.lines.end));
        
        TheParser->addIssue(Issue);
    }
    
    
    if (SecondTilde.Tok != TOKEN_TILDE) {
        
        //
        // Something like   a ~f b
        //
        // Proceed with parsing as if the second ~ were present
        //
        
        TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
        
        auto Right = TheParser->parse(Ctxt);
        
        std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDTILDE,
                                                 std::vector<std::shared_ptr<Node>> {
                                                     Left,
                                                     std::make_shared<TokenNode>(FirstTilde),
                                                     Middle,
                                                     std::make_shared<TokenNode>(SecondTilde) });
        
        return Error;
    }
    
    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto Right = TheParser->parse(Ctxt);
    
    return std::make_shared<TernaryNode>(SYMBOL_TERNARYTILDE,
        Left,
        std::make_shared<TokenNode>(FirstTilde),
        Middle,
        std::make_shared<TokenNode>(SecondTilde),
        Right);
}



//
// Something like  symbol:object
//
// when parsing a in a:b  then ColonFlag is false
// when parsing b in a:b  then ColonFlag is true
//
std::shared_ptr<Node> ColonParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {
    
    assert(!CtxtIn.ColonFlag);
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = PRECEDENCE_FAKE_PATTERNCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.ColonFlag = true;
    
    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto Right = TheParser->parse(Ctxt);
    
    if (!std::dynamic_pointer_cast<SymbolNode>(Left)) {
        
        std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDSYMBOL, std::vector<std::shared_ptr<Node>> {
            Left,
            std::make_shared<TokenNode>(TokIn),
            Right });
        
        return Error;
    }
    
    //
    // Replace with special : token for symbol:object
    //
    auto PatternColon = Token(TOKEN_FAKE_PATTERNCOLON, TokIn.Str, TokIn.Span);
    
    auto Pat = std::make_shared<BinaryNode>(SYMBOL_PATTERN,
                                            Left,
                                            std::make_shared<TokenNode>(PatternColon),
                                            Right);
    
    auto Tok = TheParser->currentToken();

    if (Tok.Tok == TOKEN_COLON) {

        Ctxt.ColonFlag = false;
        return parseContextSensitive(Pat, Ctxt);
    }

    return Pat;
}

//
// Something like  pattern:optional
//
std::shared_ptr<Node> ColonParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    assert(!CtxtIn.ColonFlag);

    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    
    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto Right = TheParser->parse(Ctxt);
    
    //
    // Replace with special : token for pattern:optional
    //
    auto OptionalColon = Token(TOKEN_FAKE_OPTIONALCOLON, TokIn.Str, TokIn.Span);
    
    return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL,
                                        Left,
                                        std::make_shared<TokenNode>(OptionalColon),
                                        Right);
}


//
// Something like  a /: b = c
//
std::shared_ptr<Node> SlashColonParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto Middle = TheParser->parse(Ctxt);
    
    if (auto BinaryMiddle = std::dynamic_pointer_cast<BinaryNode>(Middle)) {
        
        if (BinaryMiddle->getSymbol() == SYMBOL_SET) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSET,
                Left,
                std::make_shared<TokenNode>(TokIn),
                BinaryMiddle->getLeft(),
                BinaryMiddle->getOperator(),
                BinaryMiddle->getRight());
            
        } else if (BinaryMiddle->getSymbol() == SYMBOL_SETDELAYED) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSETDELAYED,
                Left,
                std::make_shared<TokenNode>(TokIn),
                BinaryMiddle->getLeft(),
                BinaryMiddle->getOperator(),
                BinaryMiddle->getRight());
            
        } else if (BinaryMiddle->getSymbol() == SYMBOL_UNSET) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGUNSET,
                Left,
                std::make_shared<TokenNode>(TokIn),
                BinaryMiddle->getLeft(),
                BinaryMiddle->getOperator(),
                BinaryMiddle->getRight());
        }
    }

    //
    // Anything other than:
    // a /: b = c
    // a /: b := c
    // a /: b =.
    //

    std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SYNTAXERROR_EXPECTEDSET, std::vector<std::shared_ptr<Node>> {
        Left,
        std::make_shared<TokenNode>(TokIn),
        Middle });
    
    return Error;
}



//
// Something like  \( x \)
//
std::shared_ptr<Node> LinearSyntaxOpenParenParselet::parse(ParserContext CtxtIn) const {
    
    auto Opener = TheParser->currentToken();
    
    std::vector<std::shared_ptr<Node>> Tokens;
    
    auto Ctxt = CtxtIn;
    Ctxt.GroupDepth++;
    Ctxt.OperatorDepth++;
    Ctxt.LinearSyntaxFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt, PRESERVE_EVERYTHING);
    
    auto CloserTok = TOKEN_LINEARSYNTAX_CLOSEPAREN;
    Ctxt.Closer = CloserTok;
    
    Tokens.push_back(std::make_shared<TokenNode>(Opener));
    
    auto breadth = 0;
    while (true) {
        
        //
        // Check isAbort() inside loops
        //
        if (TheParser->isAbort()) {
            
            return nullptr;
        }
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached.\nConsider breaking up into smaller expressions."), SYNTAXISSUESEVERITY_FORMATTING, Span);
        
            TheParser->addIssue(Issue);
        }

        if (Tok.Tok == TOKEN_ENDOFFILE) {
            
            //
            // Handle something like   \( a EOF
            //
            
            auto group = std::make_shared<GroupMissingCloserNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, Tokens);
            
            return group;
            
        } else if (Tok.Tok == TOKEN_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse(Ctxt);
            
            if (auto SubOpenParen = std::dynamic_pointer_cast<GroupNode>(Sub)) {
                
                if (SubOpenParen->getOperator() == SYMBOL_GROUPLINEARSYNTAXPAREN) {
                    
                    Tokens.push_back(SubOpenParen);
                    
                    Tok = TheParser->currentToken();
                    
                } else {
                    assert(false);
                }
                
            } else if (auto SubOpenParen = std::dynamic_pointer_cast<GroupMissingCloserNode>(Sub)) {
                
                if (SubOpenParen->getOperator() == SYMBOL_GROUPLINEARSYNTAXPAREN) {
                    
                    Tokens.push_back(SubOpenParen);
                    
                    Tok = TheParser->currentToken();
                    
                } else {
                    assert(false);
                }
                
            } else {
                assert(false);
            }
            
        } else if (Tok.Tok == CloserTok) {
            
            Tokens.push_back(std::make_shared<TokenNode>(Tok));
            
            auto Ctxt2 = Ctxt;
            Ctxt2.GroupDepth--;
            Ctxt2.OperatorDepth--;
            
            TheParser->nextToken(Ctxt2, PRESERVE_TOPLEVEL_NEWLINES);
            
            break;
            
        }
        
        //
        // Do not check for other closers here
        //
        // As long as \( \) parses by just doing tokenization, then cannot reliably test for other closers
        //
        // e.g., \( ( \) is completely valid syntax
        //
//        else if () {
//
//            some other closer;
//
//        }
        
        else {
            
            //
            // Comments are handled here
            //
            
            Tokens.push_back(std::make_shared<TokenNode>(Tok));
            
            Tok = TheParser->nextToken(Ctxt, PRESERVE_EVERYTHING);
        }

        breadth++;

    } // while
    
    return std::make_shared<GroupNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, Tokens);
}

//
// Something like  a = .
//
std::shared_ptr<Node> EqualParselet::parse(std::shared_ptr<Node> Left, ParserContext CtxtIn) const {

    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = getAssociativity();
    
    auto Tok = TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    if (Tok.Tok == TOKEN_DOT) {

        if (!isContiguous(TokIn.Span, Tok.Span)) {

            //
            // Something like a =  .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //

            auto Issue = SyntaxIssue(SYNTAXISSUETAG_NOTCONTIGUOUS, std::string("``=`` and ``.`` are not contiguous"), SYNTAXISSUESEVERITY_FORMATTING, Source(TokIn.Span.lines.start, Tok.Span.lines.end));
        
             TheParser->addIssue(Issue);
        }
        
        TheParser->nextToken(Ctxt, PRESERVE_TOPLEVEL_NEWLINES);
        
        auto Empty = std::make_shared<TokenNode>(Tok);
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET,
                                            Left,
                                            std::make_shared<TokenNode>(TokIn),
                                            Empty);
    }
    
    auto Right = TheParser->parse(Ctxt);
    
    return std::make_shared<BinaryNode>(SYMBOL_SET,
                                        Left,
                                        std::make_shared<TokenNode>(TokIn),
                                        Right);
}


//
// Something like  \[Integral] f \[DifferentialD] x
//
std::shared_ptr<Node> IntegralParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    Ctxt.OperatorDepth++;
    Ctxt.Prec = getPrecedence();
    Ctxt.Assoc = ASSOCIATIVITY_NONE;
    Ctxt.IntegralFlag = true;
    
    auto Tok = TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    auto operand = TheParser->parse(Ctxt);
    
    if (Tok.Tok != TOKEN_ENDOFFILE && TokIn.Span.lines.end.Line != operand->getSourceSpan().lines.start.Line) {
        
        //
        // Skip DifferentLine issues if ENDOFFILE
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_DIFFERENTLINE, "``" + TokIn.Str + "`` is not on same line as operand", SYNTAXISSUESEVERITY_FORMATTING, Source(TokIn.Span.lines.start, operand->getSourceSpan().lines.end));
        
        TheParser->addIssue(Issue);
    }
    
    Ctxt.IntegralFlag = false;
    
    Tok = TheParser->currentToken();
    
    if (Tok.Tok == TOKEN_LONGNAME_DIFFERENTIALD) {
        
        auto variable = TheParser->parse(Ctxt);
        
        return std::make_shared<PrefixBinaryNode>(PrefixBinaryOperatorToSymbol(TokIn.Tok),
                                                  std::make_shared<TokenNode>(TokIn),
                                                  operand,
                                                  variable);
    }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn.Tok),
                                        std::make_shared<TokenNode>(TokIn),
                                        operand);
    
}




//
// Error handling and Cleanup
//

std::shared_ptr<Node> ExpectedPossibleExpressionErrorParselet::parse(ParserContext CtxtIn) const {
    
    auto TokIn = TheParser->currentToken();
    
    auto Ctxt = CtxtIn;
    
    TheParser->nextToken(Ctxt, DISCARD_EVERYTHING);
    
    if (isError(TokIn.Tok)) {
        
        //
        // If there is a Token error, then use that specific error
        //
        
        auto SyntaxErrorEnum = TokenErrorToSyntaxError(TokIn.Tok);
        
        std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SyntaxErrorEnum, std::vector<std::shared_ptr<Node>> { std::make_shared<TokenNode>(TokIn) });
        
        return Error;
        
    } else {
        
        //
        // If NOT a Token error, then just use a generic error
        //
        
        auto SyntaxErrorEnum = SYNTAXERROR_EXPECTEDPOSSIBLEEXPRESSION;
        
        std::shared_ptr<Node> Error = std::make_shared<SyntaxErrorNode>(SyntaxErrorEnum, std::vector<std::shared_ptr<Node>> { std::make_shared<TokenNode>(TokIn) });
        
        return Error;
    }
}

