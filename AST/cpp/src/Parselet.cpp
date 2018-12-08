
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
// Atom and Atom-like parselets
//

std::shared_ptr<Node> SymbolParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto SymbolSpan = TheSourceManager->getTokenSpan();
    
    auto Sym = std::make_shared<SymbolNode>(Str, SymbolSpan, Issues);
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    if (Tok == TOKEN_OPERATOR_UNDER) {
        
        auto underParselet = new UnderParselet();
        
        auto under = underParselet->parse();
        
        auto Under = std::dynamic_pointer_cast<BlankNode>(under);
        
        auto Sym2 = Under->getSym2();
        
        return std::make_shared<PatternBlankNode>(Sym, Sym2, SourceSpan{SymbolSpan.start, Under->getSourceSpan().end});
        
    } else if (Tok == TOKEN_OPERATOR_UNDERUNDER) {
        
        auto underParselet = new UnderUnderParselet();
        
        auto under = underParselet->parse();
        
        auto Under = std::dynamic_pointer_cast<BlankSequenceNode>(under);
        
        auto Sym2 = Under->getSym2();
        
        return std::make_shared<PatternBlankSequenceNode>(Sym, Sym2, SourceSpan{SymbolSpan.start, Under->getSourceSpan().end});
        
    } else if (Tok == TOKEN_OPERATOR_UNDERUNDERUNDER) {
        
        auto underParselet = new UnderUnderUnderParselet();
        
        auto under = underParselet->parse();
        
        auto Under = std::dynamic_pointer_cast<BlankNullSequenceNode>(under);
        
        auto Sym2 = Under->getSym2();
        
        return std::make_shared<PatternBlankNullSequenceNode>(Sym, Sym2, SourceSpan{SymbolSpan.start, Under->getSourceSpan().end});
        
    } else if (Tok == TOKEN_OPERATOR_UNDERDOT) {
        
        auto UnderSpan = TheSourceManager->getTokenSpan();
        
        auto underParselet = new UnderDotParselet();
        
        underParselet->parse();
        
        return std::make_shared<OptionalDefaultNode>(Sym, SourceSpan{SymbolSpan.start, UnderSpan.end});
    }

    TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
    return Sym;
}

std::shared_ptr<Node> NumberParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<NumberNode>(Str, Span);
}

std::shared_ptr<Node> StringParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Span = TheSourceManager->getTokenSpan();

    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<StringNode>(Str, Span, Issues);
}

std::shared_ptr<Node> UnderParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto prec = getPrecedence();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Right;
    if (Tok == TOKEN_SYMBOL) {
        
        Right = TheParser->parse(prec);
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        Right = std::make_shared<InternalEmptyNode>(Span);
    }
    
    return std::make_shared<BlankNode>(Right, SourceSpan{Span.start, Right->getSourceSpan().end});
}

std::shared_ptr<Node> UnderUnderParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto prec = getPrecedence();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Right;
    if (Tok == TOKEN_SYMBOL) {
        
        Right = TheParser->parse(prec);
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        Right = std::make_shared<InternalEmptyNode>(Span);
    }
    
    return std::make_shared<BlankSequenceNode>(Right, SourceSpan{Span.start, Right->getSourceSpan().end});
}

std::shared_ptr<Node> UnderUnderUnderParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto prec = getPrecedence();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_EVERYTHING);
    
    std::shared_ptr<Node> Right;
    if (Tok == TOKEN_SYMBOL) {
        
        Right = TheParser->parse(prec);
        
    } else {
        
        TheParser->tryNextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        Right = std::make_shared<InternalEmptyNode>(Span);
    }
    
    return std::make_shared<BlankNullSequenceNode>(Right, SourceSpan{Span.start, Right->getSourceSpan().end});
}

std::shared_ptr<Node> UnderDotParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OptionalDefaultNode>(std::make_shared<InternalEmptyNode>(Span), Span);
}

std::shared_ptr<Node> HashParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SlotNode>(Str, Span);
}

std::shared_ptr<Node> HashHashParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SlotSequenceNode>(Str, Span);
}

std::shared_ptr<Node> PercentParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<OutNode>(Str, Span);
}




//
// Base Operators parselets
//

std::shared_ptr<Node> PrefixOperatorParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto TokIn = TheParser->currentToken();
    
    auto prec = getPrecedence();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto operand = TheParser->parse(prec);
    
    // Too noisy
    // if (Span.end.Line != operand->getSourceSpan().start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn), Span, operand);
}

std::shared_ptr<Node> BinaryOperatorParselet::parse(std::shared_ptr<Node> Left) {

    auto TokIn = TheParser->currentToken();
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto precedence = getPrecedence();
    if (precedence == PRECEDENCE_UNUSED) {
        assert(false);
    }
    auto recalculatedPrecedence = static_cast<precedence_t>(precedence - (isRight() ? 1 : 0));
    
    // auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto Right = TheParser->parse(recalculatedPrecedence);
    
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

std::shared_ptr<Node> InfixOperatorParselet::parse(std::shared_ptr<Node> Left) {
    
    auto Str = TheParser->getString();

    auto TokIn = TheParser->currentToken();
    
    auto Issues = TheParser->getIssues();

    // auto Span = TheSourceManager->getTokenSpan();
    
    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }
    
    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    while (true) {
        
        auto Tok = TheParser->currentToken();
        
        if (Tok == TokIn) {
            
            auto Str = TheParser->getString();
            
            // auto Span = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            // Too noisy
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
                
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }
            
            Args.push_back(operand);
            
        } else {
            break;
        }
    }
    
    return std::make_shared<InfixNode>(InfixOperatorToSymbol(TokIn), Args, Issues);
}

std::shared_ptr<Node> PostfixOperatorParselet::parse(std::shared_ptr<Node> Operand) {
    
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

std::shared_ptr<Node> GroupParselet::parse() {
    ParserScoper Scoper;
    
    // Clear String
    TheParser->getString();
    
    TheParser->setInsideColon1(false);
    TheParser->incrementGroupDepth();
    
    auto OpenerSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    std::vector<std::shared_ptr<Node>> Args;
    
    auto Closer = GroupOpenerToCloser(Opener);
    
    if (Tok == Closer) {
        
        // Clear String
        TheParser->getString();
        
        auto CloserSpan = TheSourceManager->getTokenSpan();
        
        TheParser->decrementGroupDepth();
        
        TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        auto group = std::make_shared<GroupNode>(GroupOpenerToSymbol(Opener), OpenerSpan, CloserSpan, Args, std::vector<SyntaxIssue>());
        
        return group;
    }
    
    bool eatTheNextComma = false;
    bool lastWasComma = false;
    
    SourceSpan CloserSpan;
    
    std::vector<SyntaxIssue> Issues;

    while (true) {
        
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
    
            if (lastWasComma) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Comma encountered with no adjacent expression. The expression will be treated as Null", SEVERITY_ERROR, Span);
                
                Issues.push_back(Issue);

                auto NullNode = std::make_shared<SymbolNode>(SYMBOL_NULL.name(), Span, std::vector<SyntaxIssue>());
                
                Args.push_back(NullNode);
            }
            
            // Clear String
            TheParser->getString();
            
            CloserSpan = TheSourceManager->getTokenSpan();
            
            TheParser->decrementGroupDepth();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
            break;
            
        } else if (Tok == TOKEN_OPERATOR_COMMA) {
            
            if (!eatTheNextComma) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Issue = SyntaxIssue(TAG_SYNTAXERROR, "Comma encountered with no adjacent expression. The expression will be treated as Null", SEVERITY_ERROR, Span);
                
                Issues.push_back(Issue);

                auto NullNode = std::make_shared<SymbolNode>(SYMBOL_NULL.name(), Span, std::vector<SyntaxIssue>());
                
                Args.push_back(NullNode);
            }
            
            // Clear String
            TheParser->getString();
            
            TheParser->nextToken();
            
            eatTheNextComma = false;
            lastWasComma = true;
            
        } else {
            
            // Handle the expression
            
            auto operand = TheParser->parse(PRECEDENCE_LOWEST);
            
            Args.push_back(operand);
            
            eatTheNextComma = true;
            lastWasComma = false;
        }
        
    } // while
    
    auto group = std::make_shared<GroupNode>(GroupOpenerToSymbol(Opener), OpenerSpan, CloserSpan, Args, Issues);
    
    return group;
}

std::shared_ptr<Node> OpenSquareCallParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Right = TheParser->parse(PRECEDENCE_HIGHEST);
    
    // Too noisy
    // if (Left->getSourceSpan().end.Line != Right->getSourceSpan().start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, "Head and brackets are not on same line", SEVERITY_REMARK,(SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    if (auto GroupExpr = std::dynamic_pointer_cast<GroupNode>(Right)) {
        
        if (GroupExpr->getOp() == SYMBOL_GROUPSQUARE ||
            GroupExpr->getOp() == SYMBOL_GROUPMISSINGCLOSERSQUARE) {
            
            auto Args = GroupExpr->getArgs();
            
            if (Args.size() == 1) {
                
                // check if Part
                auto Arg = Args[0];
                
                if (auto GroupArg = std::dynamic_pointer_cast<GroupNode>(Arg)) {
                    if (GroupArg->getOp() == SYMBOL_GROUPSQUARE) {
                        
                        auto GroupArgIssues = GroupArg->getIssues();

                        std::copy(GroupArgIssues.begin(), GroupArgIssues.end(), std::back_inserter(Issues));

                        auto Outer = GroupExpr->getSourceSpan();
                        
                        auto Inner = GroupArg->getSourceSpan();
                        
                        if (!isContiguous(Outer.start, Inner.start)) {
                            
                            auto Issue = SyntaxIssue(TAG_NOTCONTIGUOUS, "Part brackets [[ are not contiguous", SEVERITY_REMARK, (SourceSpan{Outer.start, Inner.start}));
                            
                            Issues.push_back(Issue);
                        }
                        if (!isContiguous(Inner.end, Outer.end)) {

                            auto Issue = SyntaxIssue(TAG_NOTCONTIGUOUS, "Part brackets ]] are not contiguous", SEVERITY_REMARK, (SourceSpan{Inner.end, Outer.end}));
                        
                            Issues.push_back(Issue);
                        }
                        
                        return std::make_shared<PartNode>(Left, GroupExpr->getOpenerTokSpan(), GroupExpr->getCloserTokSpan(), GroupArg->getArgs(), Issues);
                    }
                }
            }
            
            if (GroupExpr->getOp() == SYMBOL_GROUPSQUARE) {
                
                return std::make_shared<CallNode>(Left, GroupExpr->getOpenerTokSpan(), GroupExpr->getCloserTokSpan(), GroupExpr->getArgs(), GroupExpr->getIssues());
                
            } else {
                
                //
                // Something like a[ or a[1,2
                //
                
                assert(GroupExpr->getOp() == SYMBOL_GROUPMISSINGCLOSERSQUARE);
                
                return std::make_shared<CallMissingCloserNode>(Left, GroupExpr->getOpenerTokSpan(), GroupExpr->getArgs(), GroupExpr->getIssues());
            }
        }
    }
        
    return std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_INTERNAL, std::vector<std::shared_ptr<Node>> { Right }, Issues);
}

std::shared_ptr<Node> LeftDoubleBracketCallParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Right = TheParser->parse(PRECEDENCE_HIGHEST);

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Right->getSourceSpan().start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, "Head and double brackets are not on same line", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end}));
        
    //     Issues.push_back(Issue);
    // }
    
    if (auto GroupExpr = std::dynamic_pointer_cast<GroupNode>(Right)) {
        
        if (GroupExpr->getOp() == SYMBOL_GROUPDOUBLEBRACKET) {
            
            auto GroupExprIssues = GroupExpr->getIssues();

            std::copy(GroupExprIssues.begin(), GroupExprIssues.end(), std::back_inserter(Issues)); 

            return std::make_shared<PartNode>(Left, GroupExpr->getOpenerTokSpan(), GroupExpr->getCloserTokSpan(), GroupExpr->getArgs(), Issues);
            
        }
        
        assert(false && "Unhandled Group Type");
        return nullptr;
        
    }
        
    assert(false && "Unhandled Group Type");
    return nullptr;
}



//
// Special parselets
//

std::shared_ptr<Node> InfixPlusParselet::parse(std::shared_ptr<Node> Left) {
    
    auto Str = TheParser->getString();

    auto Issues = TheParser->getIssues();

    // auto Span = TheSourceManager->getTokenSpan();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    while (true) {
        
        auto Tok = TheParser->currentToken();
        
        auto Str = TheParser->getString();

        // auto Span = TheSourceManager->getTokenSpan();

        if (Tok == TOKEN_OPERATOR_PLUS) {
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            // Too noisy
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
        
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }

            Args.push_back(operand);
            
        } else if (Tok == TOKEN_OPERATOR_MINUS) {
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            // Too noisy
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
        
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }

            auto Loc = operand->getSourceSpan();
            
            auto minus = std::make_shared<InternalMinusNode>(operand, Loc);
            
            Args.push_back(minus);
            
        } else {
            break;
        }
    }
    
    return std::make_shared<InfixNode>(SYMBOL_PLUS, Args, Issues);
}

std::shared_ptr<Node> SemiParselet::parse(std::shared_ptr<Node> Left) {

    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    bool eatTheNextSemi = false;
    bool lastWasSemi = true;
    
    while (true) {
        
        Tok = TheParser->currentToken();
        
        Span = TheSourceManager->getTokenSpan();

        if (Tok == TOKEN_NEWLINE) {
            
            if (lastWasSemi) {
                
                Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalEmptyNode>(Span);
                
                Args.push_back(Empty);
            }
            
            break;

        } else if (Tok == TOKEN_OPERATOR_SEMI) {
            
            //
            // something like a; ; parses as CompoundExpression[a, Null, Null]
            //
            
            if (!eatTheNextSemi) {
                
                Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalEmptyNode>(Span);
                
                Args.push_back(Empty);
            }
            
            // Clear String
            TheParser->getString();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
            
            eatTheNextSemi = false;
            lastWasSemi = true;
            
        } else if (!TheParser->isPossibleBeginningOfExpression(Tok)) {
            
            if (lastWasSemi) {
                
                Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalEmptyNode>(Span);
                
                Args.push_back(Empty);
            }
            
            break;
            
        } else {
           
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            // Too noisy
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
                
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, operand->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }

            Args.push_back(operand);
            
            eatTheNextSemi = true;
            lastWasSemi = false;
        }
    }
    
    return std::make_shared<InfixNode>(SYMBOL_COMPOUNDEXPRESSION, Args, Issues);
}


// prefix
std::shared_ptr<Node> SemiSemiParselet::parse() {
    
    auto Str = TheParser->getString();
    
    auto PrefixSpan = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok)) {
        
        auto operand = TheParser->parse(recalculatedPrecedence);
        
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
                
                if (auto SpanOpRightEmpty = std::dynamic_pointer_cast<InternalEmptyNode>(SpanOpRight)) {
                    
                    //
                    // This is ;;;; or ;;a;; and is not a single MultiSpan expression
                    //
                    
                    auto NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalEmptyNode>(PrefixSpan), SpanOpLeft, std::vector<SyntaxIssue>());
                    
                    auto NewRight = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalEmptyNode>(SpanOpSource), std::make_shared<InternalEmptyNode>(SpanOpSource), std::vector<SyntaxIssue>());
                    
                    return std::make_shared<BinaryNode>(SYMBOL_INFIXIMPLICITTIMES, NewLeft, NewRight, Issues);
                }
                    
                return std::make_shared<TernaryNode>(SYMBOL_SPAN, std::make_shared<InternalEmptyNode>(PrefixSpan), SpanOpLeft, SpanOpRight, Issues);
            }
        }
        
        return std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalEmptyNode>(PrefixSpan), operand, Issues);
    }
        
    return std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalEmptyNode>(PrefixSpan), std::make_shared<InternalEmptyNode>(PrefixSpan), Issues);
}

// infix
std::shared_ptr<Node> SemiSemiParselet::parse(std::shared_ptr<Node> Left) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto InfixSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok)) {
        
        auto Right = TheParser->parse(recalculatedPrecedence);
        
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
                
                if (auto RightSpanRightEmpty = std::dynamic_pointer_cast<InternalEmptyNode>(RightSpanRight)) {
                    
                    //
                    // This is a;;;; or a;;b;; and is not a single MultiSpan expression
                    //
                    
                    auto NewLeft = std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, RightSpanLeft, std::vector<SyntaxIssue>());
                    
                    auto NewRight = std::make_shared<BinaryNode>(SYMBOL_SPAN, std::make_shared<InternalEmptyNode>(RightSpanSource), std::make_shared<InternalEmptyNode>(RightSpanSource), std::vector<SyntaxIssue>());
                    
                    return std::make_shared<BinaryNode>(SYMBOL_INFIXIMPLICITTIMES, NewLeft, NewRight, Issues);
                }
                    
                return std::make_shared<TernaryNode>(SYMBOL_SPAN, Left, RightSpanLeft, RightSpanRight, Issues);
            }
        }
        
        return std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, Right, Issues);
    }
    
    return std::make_shared<BinaryNode>(SYMBOL_SPAN, Left, std::make_shared<InternalEmptyNode>(InfixSpan), Issues);
}




std::shared_ptr<Node> TildeParselet::parse(std::shared_ptr<Node> Left) {

    auto TildeStr = TheParser->getString();
    
    auto FirstTildeSpan = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != FirstTildeSpan.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, TildeStr + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, FirstTildeSpan.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();

    TheParser->nextToken();
    
    auto Middle = TheParser->parse(prec);
    
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
    
    auto Right = TheParser->parse(prec);

    // Too noisy
    // if (SecondTildeSpan.end.Line != Right->getSourceSpan().start.Line) {
                
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{SecondTildeSpan.start, Right->getSourceSpan().end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<TernaryNode>(SYMBOL_TERNARYTILDE, Left, Middle, Right, Issues);
}



precedence_t ColonParselet::getPrecedence() {
    assert(false && "Should call getColonPrecedence");
    return PRECEDENCE_UNUSED;
}

precedence_t ColonParselet::getColonPrecedence(std::shared_ptr<Node> Left) {
    
    if (TheParser->isInsideColon1()) {
        if (std::dynamic_pointer_cast<SymbolNode>(Left)) {
            return PRECEDENCE_FAKE_PATTERNCOLON;
        }
        
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
    
    if (auto BinaryLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
        if (BinaryLeft->getOp() == SYMBOL_PATTERN) {
            return PRECEDENCE_FAKE_OPTIONALCOLON;
        }
    }
    
    return PRECEDENCE_CONTEXT_SENSITIVE;
}

std::shared_ptr<Node> ColonParselet::parse(std::shared_ptr<Node> Left) {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Issues = TheParser->getIssues();
    
    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    TheParser->nextToken();
    
    if (std::dynamic_pointer_cast<SymbolNode>(Left)) {
        
        assert(!TheParser->isInsideColon1());
        
        TheParser->setInsideColon1(true);
        
        auto prec = getColonPrecedence(Left);
        
        auto Right = TheParser->parse(prec);
        
        TheParser->setInsideColon1(false);
        
        // Too noisy
        // if (Span.end.Line != Right->getSourceSpan().start.Line) {
                
        //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
        
        //     Issues.push_back(Issue);
        // }

        return std::make_shared<BinaryNode>(SYMBOL_PATTERN, Left, Right, Issues);
        
    } else if (std::dynamic_pointer_cast<PatternBlankNode>(Left) ||
        std::dynamic_pointer_cast<PatternBlankSequenceNode>(Left) ||
        std::dynamic_pointer_cast<PatternBlankNullSequenceNode>(Left) ||
        std::dynamic_pointer_cast<BlankNode>(Left) ||
        std::dynamic_pointer_cast<BlankSequenceNode>(Left) ||
        std::dynamic_pointer_cast<BlankNullSequenceNode>(Left)) {
        
        auto prec = getColonPrecedence(Left);
        auto Right = TheParser->parse(prec);
        
        // Too noisy
        // if (Span.end.Line != Right->getSourceSpan().start.Line) {
                
        //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
        
        //     Issues.push_back(Issue);
        // }

        return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Left, Right, Issues);
        
    } else if (auto BinaryLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
        
        if (BinaryLeft->getOp() == SYMBOL_PATTERN) {
            
            auto prec = getColonPrecedence(Left);
            auto Right = TheParser->parse(prec);
            
            // Too noisy
            // if (Span.end.Line != Right->getSourceSpan().start.Line) {
                
            //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
            
            //     Issues.push_back(Issue);
            // }

            return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Left, Right, Issues);
        }  
    }
    
    return std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_EXPECTEDSYMBOLORPATTERN, std::vector<std::shared_ptr<Node>> { Left, std::make_shared<InternalTokenNode>(Str, Span) }, Issues);
}

std::shared_ptr<Node> SlashColonParselet::parse(std::shared_ptr<Node> Left) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - 1);
    
    TheParser->nextToken();
    
    auto Middle = TheParser->parse(recalculatedPrecedence);
    
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




std::shared_ptr<Node> LinearSyntaxOpenParenParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
    std::vector<std::shared_ptr<Node>> Tokens;
    
    auto OpenerSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    SourceSpan CloserSpan;
    
    while (true) {
        
        if (Tok == TOKEN_EOF) {
            
            //
            // Handle something like \( a EOF
            //
            
            auto EOFSpan = TheSourceManager->getTokenSpan();
            
            auto group = std::make_shared<GroupNode>(SYMBOL_GROUPMISSINGCLOSERLINEARSYNTAXPAREN, OpenerSpan, EOFSpan, Tokens, std::vector<SyntaxIssue>());
            
            return group;
            
        } else if (Tok == TOKEN_OPERATOR_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse();
            
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
    }
    
    return std::make_shared<GroupNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, OpenerSpan, CloserSpan, Tokens, Issues);
}



std::shared_ptr<Node> TickParselet::parse(std::shared_ptr<Node> Operand) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    // Too noisy
    // if (Operand->getSourceSpan().end.Line != Span.start.Line) {
    
    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Operand->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<PostfixNode>(SYMBOL_DERIVATIVE, Span, Str.size(), Operand, Issues);
}


std::shared_ptr<Node> MessageNameParselet::parse(std::shared_ptr<Node> Left) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto prec = getPrecedence();

    TheParser->nextToken();
    
    auto Middle = TheParser->parse(prec);
    
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
        
        auto Right = TheParser->parse(prec);
        
        // Too noisy
        // if (Span.end.Line != Right->getSourceSpan().start.Line) {
                
        //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.start, Right->getSourceSpan().end}));
        
        //     Issues.push_back(Issue);
        // }

        return std::make_shared<TernaryNode>(SYMBOL_MESSAGENAME, Left, Middle, Right, Issues);
    }

    return std::make_shared<BinaryNode>(SYMBOL_MESSAGENAME, Left, Middle, Issues);
}

std::shared_ptr<Node> EqualParselet::parse(std::shared_ptr<Node> Left) {

    auto Str = TheParser->getString();
    
    // auto Span = TheSourceManager->getTokenSpan();

    auto Issues = TheParser->getIssues();

    // Too noisy
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TAG_DIFFERENTLINE, Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().start, Span.end}));
    
    //     Issues.push_back(Issue);
    // }

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    // auto EqualSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    if (Tok == TOKEN_OPERATOR_DOT) {
        
        //
        // May be this: a = .
        // Need to parse as a =.
        //
        
        auto DotSpan = TheSourceManager->getTokenSpan();
        
        // Too noisy
        // if (!isContiguous(EqualSpan, DotSpan)) {

        //     auto Issue = SyntaxIssue(TAG_NOTCONTIGUOUS, std::string("= and . are not contiguous"), SEVERITY_REMARK, (SourceSpan{EqualSpan.start, DotSpan.end}));
        
        //     Issues.push_back(Issue);
        // }

        // Clear String
        TheParser->getString();
        
        TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        auto Empty = std::make_shared<InternalEmptyNode>(DotSpan);
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET, Left, Empty, Issues);
    }
    
    auto Right = TheParser->parse(recalculatedPrecedence);
    
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

std::shared_ptr<Node> ErrorParselet::parse() {
    
    auto TokIn = TheParser->currentToken();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Issues = TheParser->getIssues();
    
    auto Str = TheParser->getString();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    return std::make_shared<SyntaxErrorNode>(TokIn, std::vector<std::shared_ptr<Node>> { std::make_shared<InternalTokenNode>(Str, Span) }, Issues);
}

std::shared_ptr<Node> CleanupRestParselet::parse(std::shared_ptr<Node> Left) {
    
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    auto Issues = TheParser->getIssues();
    
    std::vector<std::shared_ptr<Node>> Tokens;
    
    Tokens.push_back(Left);
    Tokens.push_back(std::make_shared<InternalTokenNode>(Str, Span));
    
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
    }

    auto group = std::make_shared<SyntaxErrorNode>(TOKEN_ERROR_REST, Tokens, Issues);

    return group;
}





