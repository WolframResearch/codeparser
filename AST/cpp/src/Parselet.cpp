
#include "Parselet.h"

#include "Parser.h"
#include "Utils.h"
#include "Symbol.h"
#include "Node.h"
#include "SyntaxIssue.h"

#include <cassert>
#include <iostream>
#include <vector>
#include <iterator>

//
// Atom parselets
//

std::shared_ptr<Node> SymbolParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto SymbolSpan = TheSourceManager->getTokenSpan();
    
    auto Sym = std::make_shared<SymbolNode>(Str, SymbolSpan, Issues);
    
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    //
    // if we are here, then we know that Sym could bind to _
    //
    
    if (Tok == TOKEN_OPERATOR_UNDER) {
        
        auto underParselet = dynamic_cast<UnderParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
        
    } else if (Tok == TOKEN_OPERATOR_UNDERUNDER) {
        
        auto underParselet = dynamic_cast<UnderUnderParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
        
    } else if (Tok == TOKEN_OPERATOR_UNDERUNDERUNDER) {
        
        auto underParselet = dynamic_cast<UnderUnderUnderParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
        
    } else if (Tok == TOKEN_OPERATOR_UNDERDOT) {
        
        auto underParselet = dynamic_cast<UnderDotParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        return underParselet->parseContextSensitive(Sym, Ctxt);
    }
    
    //
    // when parsing a in a:b  then InsideColonParselet is false
    // when parsing b in a:b  then InsideColonParselet is true
    //
    if (!Ctxt.InsideColonParselet) {

        Tok = TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        if (Tok == TOKEN_OPERATOR_COLON) {

            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));

            return colonParselet->parse(Sym, Ctxt);
        }

        return Sym;
    }
    
    TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return Sym;
}

//
// parsing a in _a
//
// we know it can only be a symbol
//
std::shared_ptr<Node> SymbolParselet::parseContextSensitive(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SymbolNode>(Str, Span, Issues);
}

std::shared_ptr<Node> NumberParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<NumberNode>(Str, Span, Issues);
}

std::shared_ptr<Node> StringParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Span = TheSourceManager->getTokenSpan();

    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<StringNode>(Str, Span, Issues);
}

std::shared_ptr<Node> HashParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SlotNode>(Str, Span);
}

std::shared_ptr<Node> HashHashParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SlotSequenceNode>(Str, Span);
}

std::shared_ptr<Node> PercentParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OutNode>(Str, Span);
}




//
// Base Operators parselets
//

std::shared_ptr<Node> PrefixOperatorParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto TokIn = TheParser->currentToken();
    
    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);

    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto operand = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Span.end.Line != operand->getSourceSpan().start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn), Span, operand);
}

std::shared_ptr<Node> BinaryOperatorParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto TokIn = TheParser->currentToken();
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);

    auto recalculatedPrecedence = static_cast<precedence_t>(prec - (isRight() ? 1 : 0));
    
    // auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto Right = TheParser->parse({Ctxt.Depth, recalculatedPrecedence, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    // Too noisy
    // if (Span.end.Line != Right->getSourceSpan().start.Line) {
        
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<BinaryNode>(BinaryOperatorToSymbol(TokIn), Left, Right, Issues);
}

std::shared_ptr<Node> InfixOperatorParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    auto Str = TheParser->getString();

    auto TokIn = TheParser->currentToken();
    
    auto Issues = TheParser->getIssues();

    // auto Span = TheSourceManager->getTokenSpan();
    
    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }
    
    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);

    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    auto breadth = 1;
    while (true) {
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(TAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached. Consider breaking up into smaller expressions."), SEVERITY_REMARK, Span);
        
            Issues.push_back(Issue);
        }

        auto Tok = TheParser->currentToken();
        
        if (Tok == TokIn) {
            
            auto Str = TheParser->getString();
            
            // auto Span = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
            
            // Too noisy
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
                
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }
            
            Args.push_back(operand);
            
        } else {
            break;
        }

        breadth++;
    } // while
    
    return std::make_shared<InfixNode>(InfixOperatorToSymbol(TokIn), Args, Issues);
}

std::shared_ptr<Node> PostfixOperatorParselet::parse(std::shared_ptr<Node> Operand, ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto TokIn = TheParser->currentToken();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    // Too noisy
    // if (Operand->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Operand->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<PostfixNode>(PostfixOperatorToSymbol(TokIn), Span, 0, Operand, Issues);
}




//
// Group parselets
//

std::shared_ptr<Node> GroupParselet::parse(ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    TheParser->incrementGroupDepth();
    
    auto OpenerSpan = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    std::vector<std::shared_ptr<Node>> Args;
    
    auto Closer = GroupOpenerToCloser(Opener);
    
    SourceSpan CloserSpan;
    
    auto Issues = TheParser->getIssues();

    auto breadth = 0;
    while (true) {
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(TAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached. Consider breaking up into smaller expressions."), SEVERITY_REMARK, Span);
        
            Issues.push_back(Issue);
        }

        auto Tok = TheParser->currentToken();
        
        if (Tok == TOKEN_EOF) {
            
            //
            // Handle something like { a EOF
            //
            
            auto EOFSpan = TheSourceManager->getTokenSpan();
            
            auto GroupSymbol = GroupOpenerToSymbol(Opener);
            auto GroupPair = SymbolToGroupPair(GroupSymbol);
            auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Missing group closer: " + GroupPair.second, SEVERITY_FATAL, SourceSpan{OpenerSpan.start, EOFSpan.end});
                
            Issues.push_back(Issue);


            auto group = std::make_shared<GroupNode>(GroupOpenerToMissingCloserSymbol(Opener), OpenerSpan, EOFSpan, Args, Issues);
            
            return group;
            
        } else if (Tok == Closer) {
            
            // Clear String
            TheParser->getString();
            
            CloserSpan = TheSourceManager->getTokenSpan();
            
            TheParser->decrementGroupDepth();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
            break;
            
        } else if (Tok == TOKEN_OPERATOR_COMMA) {
            
            //
            // Reporting of commas, e.g., {1,,2} is done later
            //
            
            auto Str = TheParser->getString();
            
            auto Span = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken();

            auto CommaNode = std::make_shared<InternalTokenNode>(Str, Span);
            
            Args.push_back(CommaNode);
            
        } else {
            
            // Handle the expression
            
            auto operand = TheParser->parse({Ctxt.Depth, PRECEDENCE_LOWEST, false});
            
            Args.push_back(operand);
        }
        
        breadth++;

    } // while
    
    auto group = std::make_shared<GroupNode>(GroupOpenerToSymbol(Opener), OpenerSpan, CloserSpan, Args, Issues);
    
    return group;
}


//
// Call parselets
//

std::shared_ptr<Node> CallParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    auto TokIn = TheParser->currentToken();

    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
//    auto prec = getPrecedence();
//    assert(prec != PRECEDENCE_UNUSED);

    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //
    auto prec = PRECEDENCE_HIGHEST;
    
    auto Right = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Left->getSourceSpan().end.Line != Right->getSourceSpan().start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, "Head and brackets are not on same line", SEVERITY_REMARK,(SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    if (auto GroupExpr = std::dynamic_pointer_cast<GroupNode>(Right)) {
            
        if (GroupExpr->getOp() == GroupOpenerToSymbol(Opener)) {
            
            return std::make_shared<CallNode>(Left, GroupExpr, Issues);
            
        } else if (GroupExpr->getOp() == GroupOpenerToMissingCloserSymbol(Opener)) {
            
            //
            // Something like a[ or a[1,2
            //
            
            // assert(GroupExpr->getOp() == SYMBOL_GROUPMISSINGCLOSERSQUARE);
            
            return std::make_shared<CallMissingCloserNode>(Left, GroupExpr, Issues);
        }
    }
        
    return std::make_shared<SyntaxErrorNode>(TokIn, std::vector<std::shared_ptr<Node>> { Right }, Issues);
}



//
// Special parselets
//

// prefix
std::shared_ptr<Node> UnderParselet::parse(ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Blank;
    if (Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);

        Blank = std::make_shared<BlankNode>(Right, SourceSpan{Span.start, Right->getSourceSpan().end});
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        Blank = std::make_shared<BlankNode>(Span);
    }

    //
    // For something like a:_:""  when parsing _, make sure to not parse the second : here
    //
    if (!Ctxt.InsideColonParselet) {
        
        Tok = TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        if (Tok == TOKEN_OPERATOR_COLON) {
            
            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));
            
            return colonParselet->parseContextSensitive(Blank, Ctxt);
        }
    }
    
    return Blank;
}

// infix
std::shared_ptr<Node> UnderParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Pat;
    if (Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);

        Pat = std::make_shared<PatternBlankNode>(Left, Right, SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end});
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        Pat = std::make_shared<PatternBlankNode>(Left, SourceSpan{Left->getSourceSpan().start, Span.end});
    }

    Tok = TheParser->currentToken();

    if (Tok == TOKEN_OPERATOR_COLON) {

        auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));

        return colonParselet->parseContextSensitive(Pat, Ctxt);
    }

    return Pat;
}

// prefix
std::shared_ptr<Node> UnderUnderParselet::parse(ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Blank;
    if (Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);

        Blank = std::make_shared<BlankSequenceNode>(Right, SourceSpan{Span.start, Right->getSourceSpan().end});
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        Blank = std::make_shared<BlankSequenceNode>(Span);
    }

    //
    // For something like a:__:""  when parsing __, make sure to not parse the second : here
    //
    if (!Ctxt.InsideColonParselet) {
        
        Tok = TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        if (Tok == TOKEN_OPERATOR_COLON) {
            
            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));
            
            return colonParselet->parseContextSensitive(Blank, Ctxt);
        }
    }

    return Blank;
}

// infix
std::shared_ptr<Node> UnderUnderParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Pat;
    if (Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);
        
        Pat = std::make_shared<PatternBlankSequenceNode>(Left, Right, SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end});
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        Pat = std::make_shared<PatternBlankSequenceNode>(Left, SourceSpan{Left->getSourceSpan().start, Span.end});
    }

    Tok = TheParser->currentToken();

    if (Tok == TOKEN_OPERATOR_COLON) {

        auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));

        return colonParselet->parseContextSensitive(Pat, Ctxt);
    }

    return Pat;
}

// prefix
std::shared_ptr<Node> UnderUnderUnderParselet::parse(ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Blank;
    if (Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);

        Blank = std::make_shared<BlankNullSequenceNode>(Right, SourceSpan{Span.start, Right->getSourceSpan().end});
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        Blank = std::make_shared<BlankNullSequenceNode>(Span);
    }

    //
    // For something like a:___:""  when parsing ___, make sure to not parse the second : here
    //
    if (!Ctxt.InsideColonParselet) {
        
        Tok = TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        if (Tok == TOKEN_OPERATOR_COLON) {
            
            auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));
            
            return colonParselet->parseContextSensitive(Blank, Ctxt);
        }
    }

    return Blank;
}

// infix
std::shared_ptr<Node> UnderUnderUnderParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Pat;
    if (Tok == TOKEN_SYMBOL) {
        
        auto symbolParselet = dynamic_cast<SymbolParselet*>(TheParser->findContextSensitiveParselet(Tok));
        
        auto Right = symbolParselet->parseContextSensitive(Ctxt);
        
        Pat = std::make_shared<PatternBlankNullSequenceNode>(Left, Right, SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end});
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        Pat = std::make_shared<PatternBlankNullSequenceNode>(Left, SourceSpan{Left->getSourceSpan().start, Span.end});
    }

    Tok = TheParser->currentToken();

    if (Tok == TOKEN_OPERATOR_COLON) {

        auto colonParselet = dynamic_cast<ColonParselet*>(TheParser->findContextSensitiveParselet(Tok));

        return colonParselet->parseContextSensitive(Pat, Ctxt);
    }

    return Pat;
}

// prefix
std::shared_ptr<Node> UnderDotParselet::parse(ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OptionalDefaultNode>(Span);
}

// infix
std::shared_ptr<Node> UnderDotParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OptionalDefaultPatternNode>(Left, SourceSpan{Left->getSourceSpan().start, Span.end});
}


std::shared_ptr<Node> SemiParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    auto lastSpan = Span;
    
    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);

    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    auto eatTheNextSemi = false;
    auto lastWasSemi = true;

    auto breadth = 1;
    while (true) {
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(TAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached. Consider breaking up into smaller expressions."), SEVERITY_REMARK, Span);
        
            Issues.push_back(Issue);
        }

        Tok = TheParser->currentToken();
        
        Span = TheSourceManager->getTokenSpan();

        if (Tok == TOKEN_NEWLINE) {
            
            if (lastWasSemi) {
                
                Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalNullNode>(Span);
                
                Args.push_back(Empty);
            }
            
            break;

        } else if (Tok == TOKEN_OPERATOR_SEMI) {
            
            //
            // something like a; ; parses as CompoundExpression[a, Null, Null]
            //
            
            if (!eatTheNextSemi) {
                
                Span = TheSourceManager->getTokenSpan();

                auto Empty = std::make_shared<InternalNullNode>(Span);
                
                Args.push_back(Empty);
            }
            
            // Clear String
            TheParser->getString();
            
            eatTheNextSemi = false;
            lastWasSemi = true;
            lastSpan = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);

        } else if (!TheParser->isPossibleBeginningOfExpression(Tok)) {
            
            if (lastWasSemi) {
                
                auto Empty = std::make_shared<InternalNullNode>(lastSpan);
                
                Args.push_back(Empty);
            }
            
            break;
            
        } else {
            
            auto operand = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
            
            // Too noisy
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
                
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }

            Args.push_back(operand);
            
            eatTheNextSemi = true;
            lastWasSemi = false;
        }

        breadth++;

    } // while
    
    return std::make_shared<InfixNode>(SYMBOL_COMPOUNDEXPRESSION, Args, Issues);
}


// prefix
std::shared_ptr<Node> SemiSemiParselet::parse(ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto PrefixSpan = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);
    auto recalculatedPrecedence = static_cast<precedence_t>(prec - (isRight() ? 1 : 0));
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok)) {
        
        auto operand = TheParser->parse({Ctxt.Depth, recalculatedPrecedence, Ctxt.InsideColonParselet});
        
        // Too noisy
        // if (PrefixSpan.end.Line != operand->getSourceSpan().start.Line) {
                
        //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{PrefixSpan.start, operand->getSourceSpan().end}));
        
        //     Issues.push_back(Issue);
        // }

        if (auto BinOp = std::dynamic_pointer_cast<BinaryNode>(operand)) {
            
            if (BinOp->getOp() == SYMBOL_SPAN) {
                
                auto SpanOpSource = BinOp->getSourceSpan();
                auto SpanOpLeft = BinOp->getLeft();
                auto SpanOpRight = BinOp->getRight();
                
                if (auto SpanOpRightEmpty = std::dynamic_pointer_cast<InternalAllNode>(SpanOpRight)) {
                    
                    //
                    // This is ;;;; or ;;a;; and is not a single Span expression
                    //
                    
                    std::shared_ptr<Node> NewLeft;
                    if (std::dynamic_pointer_cast<InternalOneNode>(SpanOpLeft)) {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(PrefixSpan), std::make_shared<InternalAllNode>(PrefixSpan), std::vector<SyntaxIssue>());
                    } else {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(PrefixSpan), SpanOpLeft, std::vector<SyntaxIssue>());
                    }
                    
                    auto NewRight = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(SpanOpSource), std::make_shared<InternalAllNode>(SpanOpSource), std::vector<SyntaxIssue>());
                    
                    return std::make_shared<InfixNode>(SYMBOL_IMPLICITTIMES, std::vector<std::shared_ptr<Node>>{NewLeft, NewRight}, Issues);
                }
                
                if (std::dynamic_pointer_cast<InternalOneNode>(SpanOpLeft)) {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(PrefixSpan), std::make_shared<InternalAllNode>(PrefixSpan), SpanOpRight, Issues);
                } else {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(PrefixSpan), SpanOpLeft, SpanOpRight, Issues);
                }
            }
        }
        
        return std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(PrefixSpan), operand, Issues);
    }
        
    return std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(PrefixSpan), std::make_shared<InternalAllNode>(PrefixSpan), Issues);
}

// infix
std::shared_ptr<Node> SemiSemiParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);
    auto recalculatedPrecedence = static_cast<precedence_t>(prec - (isRight() ? 1 : 0));
    
    auto InfixSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok)) {
        
        auto Right = TheParser->parse({Ctxt.Depth, recalculatedPrecedence, Ctxt.InsideColonParselet});
        
        // Too noisy
        // if (InfixSpan.end.Line != Right->getSourceSpan().start.Line) {
                
        //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{InfixSpan.start, Right->getSourceSpan().end}));
        
        //     Issues.push_back(Issue);
        // }

        if (auto BinLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
            
            if (BinLeft->getOp() == SYMBOL_SPAN) {
                
                auto LeftSpanLeft = BinLeft->getLeft();
                auto LeftSpanRight = BinLeft->getRight();
                
                return std::make_shared<TernaryNode>(SYMBOL_SPAN, LeftSpanLeft, LeftSpanRight, Right, Issues);
            }
        }
        
        if (auto BinRight = std::dynamic_pointer_cast<BinaryNode>(Right)) {
            
            if (BinRight->getOp() == SYMBOL_SPAN) {
                
                auto RightSpanSource = BinRight->getSourceSpan();
                auto RightSpanLeft = BinRight->getLeft();
                auto RightSpanRight = BinRight->getRight();
                
                if (auto RightSpanRightEmpty = std::dynamic_pointer_cast<InternalAllNode>(RightSpanRight)) {
                    
                    //
                    // This is a;;;; or a;;b;; and is not a single Span expression
                    //
                    
                    std::shared_ptr<Node> NewLeft;
                    if (std::dynamic_pointer_cast<InternalOneNode>(RightSpanLeft)) {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, std::make_shared<InternalAllNode>(RightSpanSource), std::vector<SyntaxIssue>());
                    } else {
                        NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, RightSpanLeft, std::vector<SyntaxIssue>());
                    }
                    
                    auto NewRight = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalOneNode>(RightSpanSource), std::make_shared<InternalAllNode>(RightSpanSource), std::vector<SyntaxIssue>());
                    
                    return std::make_shared<InfixNode>(SYMBOL_IMPLICITTIMES, std::vector<std::shared_ptr<Node>>{NewLeft, NewRight}, Issues);
                }
                
                if (std::dynamic_pointer_cast<InternalOneNode>(RightSpanLeft)) {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN, Left, std::make_shared<InternalAllNode>(RightSpanSource), RightSpanRight, Issues);
                } else {
                    return std::make_shared<TernaryNode>(SYMBOL_SPAN, Left, RightSpanLeft, RightSpanRight, Issues);
                }
            }
        }
        
        return std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, Right, Issues);
    }
    
    return std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, std::make_shared<InternalAllNode>(InfixSpan), Issues);
}




std::shared_ptr<Node> TildeParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto TildeStr = TheParser->getString();
    
    auto FirstTildeSpan = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != FirstTildeSpan.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, TildeStr + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, FirstTildeSpan.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);

    TheParser->nextToken();
    
    auto Middle = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
    
    // auto MiddleSpan = Middle->getSourceSpan();

    auto Tok = TheParser->currentToken();
    
    auto Str = TheParser->getString();
    
    auto SecondTildeSpan = TheSourceManager->getTokenSpan();

    // Too noisy
    // if (Middle->getSourceSpan().end.Line != SecondTildeSpan.start.Line) {
                
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Middle->getSourceSpan().start, SecondTildeSpan.end}));
    
    //     Issues.push_back(Issue);
    // }

    TheParser->nextToken();
    
    if (Tok != TOKEN_OPERATOR_TILDE) {
        
        auto Issues = TheParser->getIssues();
        
        return std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_EXPECTEDTILDE,
            std::vector<std::shared_ptr<Node>> { Left, std::make_shared<InternalTokenNode>(TildeStr, FirstTildeSpan),
                Middle, std::make_shared<InternalTokenNode>(Str, SecondTildeSpan) }, Issues);
    }
    
    auto Right = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});

    // Too noisy
    // if (SecondTildeSpan.end.Line != Right->getSourceSpan().start.Line) {
                
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{SecondTildeSpan.start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<TernaryNode>(SYMBOL_TERNARYTILDE, Left, Middle, Right, Issues);
}



//
// symbol:object
//
std::shared_ptr<Node> ColonParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
    TheParser->nextToken();
    
    auto prec = PRECEDENCE_FAKE_PATTERNCOLON;
    
    auto Right = TheParser->parse({Ctxt.Depth, prec, true});
    
    if (!std::dynamic_pointer_cast<SymbolNode>(Left)) {
        
        return std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_EXPECTEDSYMBOL, std::vector<std::shared_ptr<Node>> { Left, Right }, Issues);
    }
    
    // Too noisy
    // if (Span.end.Line != Right->getSourceSpan().start.Line) {
            
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }

    auto Pat = std::make_shared<BinaryNode>(SYMBOL_PATTERN, Left, Right, Issues);
    
    auto Tok = TheParser->currentToken();

    if (Tok == TOKEN_OPERATOR_COLON) {

        return parseContextSensitive(Pat, Ctxt);
    }

    return Pat;
}

//
// pattern:optional
//
std::shared_ptr<Node> ColonParselet::parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    assert(!Ctxt.InsideColonParselet);

    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    TheParser->nextToken();
    
    auto prec = PRECEDENCE_FAKE_OPTIONALCOLON;
    
    auto Right = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Span.end.Line != Right->getSourceSpan().start.Line) {
            
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }

    return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Left, Right, Issues);
}



std::shared_ptr<Node> SlashColonParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);
    auto recalculatedPrecedence = static_cast<precedence_t>(prec - 1);
    
    TheParser->nextToken();
    
    auto Middle = TheParser->parse({Ctxt.Depth, recalculatedPrecedence, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Span.end.Line != Middle->getSourceSpan().start.Line) {
                
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Middle->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }

    if (auto BinaryMiddle = std::dynamic_pointer_cast<BinaryNode>(Middle)) {
        
        if (BinaryMiddle->getOp() == SYMBOL_SET) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSET, Left, BinaryMiddle->getLeft(), BinaryMiddle->getRight(), Issues);
            
        } else if (BinaryMiddle->getOp() == SYMBOL_SETDELAYED) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSETDELAYED, Left, BinaryMiddle->getLeft(), BinaryMiddle->getRight(), Issues);
            
        } else if (BinaryMiddle->getOp() == SYMBOL_UNSET) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGUNSET, Left, BinaryMiddle->getLeft(), BinaryMiddle->getRight(), Issues);
        }
    }
    
    return std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_MISMATCHEDSLASHCOLON, std::vector<std::shared_ptr<Node>> { Middle }, Issues);
}




std::shared_ptr<Node> LinearSyntaxOpenParenParselet::parse(ParserContext Ctxt) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
    std::vector<std::shared_ptr<Node>> Tokens;
    
    auto OpenerSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    SourceSpan CloserSpan;
    
    auto breadth = 0;
    while (true) {
        
        if (breadth == MAX_EXPRESSION_BREADTH) {

            auto Span = TheSourceManager->getTokenSpan();
        
            auto Issue = SyntaxIssue(TAG_MAXEXPRESSIONBREADTH, std::string("Max expression breadth reached. Consider breaking up into smaller expressions."), SEVERITY_REMARK, Span);
        
            Issues.push_back(Issue);
        }

        if (Tok == TOKEN_EOF) {
            
            //
            // Handle something like \( a EOF
            //
            
            auto EOFSpan = TheSourceManager->getTokenSpan();
            
            auto group = std::make_shared<GroupNode>(SYMBOL_GROUPMISSINGCLOSERLINEARSYNTAXPAREN, OpenerSpan, EOFSpan, Tokens, std::vector<SyntaxIssue>());
            
            return group;
            
        } else if (Tok == TOKEN_OPERATOR_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse(Ctxt);
            
            if (auto SubOpenParen = std::dynamic_pointer_cast<GroupNode>(Sub)) {
                
                if (SubOpenParen->getOp() == SYMBOL_GROUPLINEARSYNTAXPAREN ||
                    SubOpenParen->getOp() == SYMBOL_GROUPMISSINGCLOSERLINEARSYNTAXPAREN) {
                    
                    Tokens.push_back(SubOpenParen);
                    
                    Tok = TheParser->currentToken();
                    
                } else {
                    assert(false);
                }
                
            } else {
                assert(false);
            }
            
        } else if (Tok == TOKEN_OPERATOR_LINEARSYNTAX_CLOSEPAREN) {
            
            // Clear String
            TheParser->getString();
            
            CloserSpan = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
            
            break;
            
        } else {
            
            auto Str = TheParser->getString();
            
            auto Tmp = TheParser->getIssues();
            
            auto Span = TheSourceManager->getTokenSpan();
            
            Tokens.push_back(std::make_shared<InternalTokenNode>(Str, Span));
            
            std::copy(Tmp.begin(), Tmp.end(), std::back_inserter(Issues));
            
            Tok = TheParser->nextToken();
        }

        breadth++;

    } // while
    
    return std::make_shared<GroupNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, OpenerSpan, CloserSpan, Tokens, Issues);
}


std::shared_ptr<Node> MessageNameParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);

    TheParser->nextToken();
    
    auto Middle = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Span.end.Line != Middle->getSourceSpan().start.Line) {
                
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Middle->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }

    auto Tok = TheParser->currentToken();
    
    if (Tok == TOKEN_OPERATOR_COLONCOLON) {
        
        // Clear String
        TheParser->getString();
        
        TheParser->nextToken();
        
        auto Right = TheParser->parse({Ctxt.Depth, prec, Ctxt.InsideColonParselet});
        
        // Too noisy
        // if (Span.end.Line != Right->getSourceSpan().start.Line) {
                
        //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
        
        //     Issues.push_back(Issue);
        // }

        return std::make_shared<TernaryNode>(SYMBOL_MESSAGENAME, Left, Middle, Right, Issues);
    }

    return std::make_shared<BinaryNode>(SYMBOL_MESSAGENAME, Left, Middle, Issues);
}

std::shared_ptr<Node> EqualParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();
    assert(prec != PRECEDENCE_UNUSED);
    auto recalculatedPrecedence = static_cast<precedence_t>(prec - (isRight() ? 1 : 0));
    
    auto EqualSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    if (Tok == TOKEN_OPERATOR_DOT) {
        
        auto DotSpan = TheSourceManager->getTokenSpan();

        if (!isContiguous(EqualSpan, DotSpan)) {

            //
            // Something like a =  .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //

             auto Issue = SyntaxIssue(TAG_NOTCONTIGUOUS, std::string("= and . are not contiguous"), SEVERITY_REMARK, (SourceSpan{EqualSpan.start, DotSpan.end}));
        
             Issues.push_back(Issue);
         }

        // Clear String
        TheParser->getString();
        
        TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        auto Empty = std::make_shared<InternalDotNode>(DotSpan);
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET, Left, Empty, Issues);
    }
    
    auto Right = TheParser->parse({Ctxt.Depth, recalculatedPrecedence, Ctxt.InsideColonParselet});
    
    // Too noisy
    // if (Span.end.Line != Right->getSourceSpan().start.Line) {
                
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<BinaryNode>(SYMBOL_SET, Left, Right, Issues);
}







//
// Error handling and Cleanup
//

std::shared_ptr<Node> ErrorParselet::parse(ParserContext Ctxt) {
    
    auto TokIn = TheParser->currentToken();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Issues = TheParser->getIssues();
    
    auto Str = TheParser->getString();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SyntaxErrorNode>(TokIn, std::vector<std::shared_ptr<Node>> { std::make_shared<InternalTokenNode>(Str, Span) }, Issues);
}

std::shared_ptr<Node> CleanupRestParselet::parse(std::shared_ptr<Node> Left, ParserContext Ctxt) {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    auto Issues = TheParser->getIssues();
    
    std::vector<std::shared_ptr<Node>> Tokens;
    
    Tokens.push_back(Left);
    Tokens.push_back(std::make_shared<InternalTokenNode>(Str, Span));
    
    //
    // do not keep track of breadth here, not a big deal
    //
    while (true) {
        
        if (Tok == TOKEN_EOF) {
            
            break;
            
        } else {
            
            Str = TheParser->getString();
            
            Span = TheSourceManager->getTokenSpan();
            
            Tokens.push_back(std::make_shared<InternalTokenNode>(Str, Span));
            
            auto Tmp = TheParser->getIssues();
            
            std::copy(Tmp.begin(), Tmp.end(), std::back_inserter(Issues));
            
            Tok = TheParser->nextToken();
        }
    } // while

    auto group = std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_REST, Tokens, Issues);

    return group;
}
