
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
    
    if (Tok == OPERATOR_UNDER) {
        
        auto underParselet = new UnderParselet();
        
        auto under = underParselet->parse();
        
        auto Under = std::dynamic_pointer_cast<BlankNode>(under);
        
        auto Sym2 = Under->getSym2();
        
        return std::make_shared<PatternBlankNode>(Sym, Sym2, SourceSpan{SymbolSpan.start, Under->getSourceSpan().end});
        
    } else if (Tok == OPERATOR_UNDERUNDER) {
        
        auto underParselet = new UnderUnderParselet();
        
        auto under = underParselet->parse();
        
        auto Under = std::dynamic_pointer_cast<BlankSequenceNode>(under);
        
        auto Sym2 = Under->getSym2();
        
        return std::make_shared<PatternBlankSequenceNode>(Sym, Sym2, SourceSpan{SymbolSpan.start, Under->getSourceSpan().end});
        
    } else if (Tok == OPERATOR_UNDERUNDERUNDER) {
        
        auto underParselet = new UnderUnderUnderParselet();
        
        auto under = underParselet->parse();
        
        auto Under = std::dynamic_pointer_cast<BlankNullSequenceNode>(under);
        
        auto Sym2 = Under->getSym2();
        
        return std::make_shared<PatternBlankNullSequenceNode>(Sym, Sym2, SourceSpan{SymbolSpan.start, Under->getSourceSpan().end});
        
    } else if (Tok == OPERATOR_UNDERDOT) {
        
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

std::shared_ptr<Node> ErrorParselet::parse() {
    
    auto TokIn = TheParser->currentToken();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Issues = TheParser->getIssues();
    
    switch (TokIn) {
        case ERROR_UNHANDLEDCHARACTER:
        case ERROR_UNHANDLEDLINEARSYNTAX:
        case ERROR_EXPONENT:
        case ERROR_EMPTYSTRING: {
            
            auto Str = TheParser->getString();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
            
            return std::make_shared<SyntaxErrorNode>(TokIn, std::vector<std::shared_ptr<Node>> { std::make_shared<InternalTokenNode>(Str, Span) }, Issues);
        }
        case TOKEN_EOF: {
            
            auto Str = TheParser->getString();
            
            return std::make_shared<SyntaxErrorNode>(ERROR_UNEXPECTEDEOF, std::vector<std::shared_ptr<Node>> { std::make_shared<InternalTokenNode>(Str, Span) }, Issues);
        }
        default: {
            //
            // Everything else
            //
            
            auto Str = TheParser->getString();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
            
            return std::make_shared<SyntaxErrorNode>(ERROR_UNEXPECTEDTOKEN, std::vector<std::shared_ptr<Node>> { std::make_shared<InternalTokenNode>(Str, Span) }, Issues);
        }
    }
}

std::shared_ptr<Node> CleanupRestParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    auto Str = TheParser->getString();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
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
            
            Tok = TheParser->nextToken();
        }
    }

    auto group = std::make_shared<SyntaxErrorNode>(ERROR_REST, Tokens, std::vector<SyntaxIssue>());

    return group;
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
    
    // Clear String
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto TokIn = TheParser->currentToken();
    
    auto prec = getPrecedence();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto operand = TheParser->parse(prec);
    
    if (Span.end.Line != operand->getSourceSpan().start.Line) {

        auto Issue = SyntaxIssue(Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.end, operand->getSourceSpan().start}));
    
        Issues.push_back(Issue);
    }
    
    return std::make_shared<PrefixNode>(PrefixOperatorToSymbol(TokIn), Span, operand);
}

std::shared_ptr<Node> BinaryOperatorParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto TokIn = TheParser->currentToken();
    
    auto precedence = getPrecedence();
    if (precedence == PRECEDENCE_UNUSED) {
        assert(false);
    }
    auto recalculatedPrecedence = static_cast<precedence_t>(precedence - (isRight() ? 1 : 0));
    
    // auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto Right = TheParser->parse(recalculatedPrecedence);
    
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TokenToString(TokIn) + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().end, Span.start}));
    
    //     Issues.push_back(Issue);
    // }

    // if (Span.end.Line != Right->getSourceSpan().start.Line) {
        
    //     auto Issue = SyntaxIssue(TokenToString(TokIn) + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.end, Right->getSourceSpan().start}));
    
    //     Issues.push_back(Issue);
    // }
    
    return std::make_shared<BinaryNode>(InfixOperatorToSymbol(TokIn), Left, Right, Issues);
}

std::shared_ptr<Node> InfixOperatorParselet::parse(std::shared_ptr<Node> Left) {
    
    auto TokIn = TheParser->currentToken();
    
    auto Issues = TheParser->getIssues();

    auto Span = TheSourceManager->getTokenSpan();
    
    // if (Left->getSourceSpan().end.Line != Span.start.Line) {

    //     auto Issue = SyntaxIssue(TokenToString(TokIn) + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().end, Span.start}));
    
    //     Issues.push_back(Issue);
    // }
    
    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    while (true) {
        
        auto Tok = TheParser->currentToken();
        
        if (Tok == TokIn) {
            
            // Clear String
            TheParser->getString();
            
            Span = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            // if (Span.end.Line != operand->getSourceSpan().start.Line) {
                
            //     auto Issue = SyntaxIssue(TokenToString(TokIn) + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Span.end, operand->getSourceSpan().start}));
            
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
    
    // Clear String
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto TokIn = TheParser->currentToken();
    
    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (Operand->getSourceSpan().end.Line != Span.start.Line) {

        auto Issue = SyntaxIssue(Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Operand->getSourceSpan().end, Span.start}));
    
        Issues.push_back(Issue);
    }
    
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
//    TheParser->disableGroupMissingOpenerParselet(Opener);
    
    auto OpenerSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    std::vector<std::shared_ptr<Node>> Args;
    
    auto Closer = GroupOpenerToCloser(Opener);
    
    if (Tok == Closer) {
        
        // Clear String
        TheParser->getString();
        
        auto CloserSpan = TheSourceManager->getTokenSpan();
        
        TheParser->decrementGroupDepth();
//        TheParser->enableGroupMissingOpenerParselet(Opener);
        
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
            auto Issue = SyntaxIssue("Missing group closer: " + GroupPair.second, SEVERITY_FATAL, SourceSpan{OpenerSpan.start, EOFSpan.end});
                
            Issues.push_back(Issue);


            auto group = std::make_shared<GroupNode>(GroupOpenerToMissingCloserSymbol(Opener), OpenerSpan, EOFSpan, Args, Issues);
            
            return group;
            
        } else if (Tok == Closer) {
    
            if (lastWasComma) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Issue = SyntaxIssue("Comma encountered with no adjacent expression. The expression will be treated as Null", SEVERITY_ERROR, Span);
                
                Issues.push_back(Issue);

                auto NullNode = std::make_shared<SymbolNode>(SYMBOL_NULL->name(), Span, std::vector<SyntaxIssue>());
                
                Args.push_back(NullNode);
            }
            
            // Clear String
            TheParser->getString();
            
            CloserSpan = TheSourceManager->getTokenSpan();
            
            TheParser->decrementGroupDepth();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
            break;
            
        } else if (Tok == OPERATOR_COMMA) {
            
            if (!eatTheNextComma) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Issue = SyntaxIssue("Comma encountered with no adjacent expression. The expression will be treated as Null", SEVERITY_ERROR, Span);
                
                Issues.push_back(Issue);

                auto NullNode = std::make_shared<SymbolNode>(SYMBOL_NULL->name(), Span, std::vector<SyntaxIssue>());
                
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
            
            // if (auto ErrorOperand = std::dynamic_pointer_cast<SyntaxErrorNode>(operand)) {
                
            //     return operand;
            // }
            
            Args.push_back(operand);
            
            eatTheNextComma = true;
            lastWasComma = false;
        }
        
    } // while
    
    auto group = std::make_shared<GroupNode>(GroupOpenerToSymbol(Opener), OpenerSpan, CloserSpan, Args, Issues);
    
    return group;
}

std::shared_ptr<Node> GroupMissingOpenerParselet::parse(std::shared_ptr<Node> Left) {
    
    ParserScoper Scoper;
    
    // Clear String
    TheParser->getString();
    
    TheParser->setInsideColon1(false);
    TheParser->incrementGroupDepth();
    
    auto CloserSpan = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    std::vector<SyntaxIssue> Issues;
    
    auto LeftSpan = Left->getSourceSpan();
    
    auto GroupSymbol = GroupCloserToSymbol(Closer);
    auto GroupPair = SymbolToGroupPair(GroupSymbol);
    auto Issue = SyntaxIssue("Missing group opener: " + GroupPair.first, SEVERITY_FATAL, SourceSpan{LeftSpan.start, CloserSpan.end});
    
    Issues.push_back(Issue);
    
    
    auto group = std::make_shared<GroupNode>(GroupCloserToMissingOpenerSymbol(Closer), SourceSpan{LeftSpan.start, LeftSpan.start}, CloserSpan, std::vector<std::shared_ptr<Node>> { Left }, Issues);
    
    return group;
}

std::shared_ptr<Node> OpenSquareCallParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Right = TheParser->parse(PRECEDENCE_HIGHEST);
    
    if (std::dynamic_pointer_cast<SyntaxErrorNode>(Right)) {
        return Right;
    }
    
    if (Left->getSourceSpan().end.Line != Right->getSourceSpan().start.Line) {

        auto Issue = SyntaxIssue("Head and brackets are not on same line", SEVERITY_REMARK,(SourceSpan{Left->getSourceSpan().end, Right->getSourceSpan().start}));
    
        Issues.push_back(Issue);
    }
    
    if (auto GroupExpr = std::dynamic_pointer_cast<GroupNode>(Right)) {
        
        if (*GroupExpr->getOp() == *SYMBOL_GROUPSQUARE) {
            
            auto Args = GroupExpr->getArgs();
            
            if (Args.size() == 1) {
                
                // check if Part
                auto Arg = Args[0];
                
                if (auto GroupArg = std::dynamic_pointer_cast<GroupNode>(Arg)) {
                    if (*GroupArg->getOp() == *SYMBOL_GROUPSQUARE) {
                        
                        auto GroupArgIssues = GroupArg->getIssues();

                        std::copy(GroupArgIssues.begin(), GroupArgIssues.end(), std::back_inserter(Issues));

                        auto Outer = GroupExpr->getSourceSpan();
                        
                        auto Inner = GroupArg->getSourceSpan();
                        
                        if (!isContiguous(Outer.start, Inner.start)) {
                            
                            auto Issue = SyntaxIssue("Brackets for Part are not contiguous", SEVERITY_REMARK, (SourceSpan{Outer.start, Inner.start}));
                            
                            Issues.push_back(Issue);
                        }
                        if (!isContiguous(Inner.end, Outer.end)) {

                            auto Issue = SyntaxIssue("Brackets for Part are not contiguous", SEVERITY_REMARK, (SourceSpan{Inner.end, Outer.end}));
                        
                            Issues.push_back(Issue);
                        }
                        
                        return std::make_shared<PartNode>(Left, GroupExpr->getOpenerTokSpan(), GroupExpr->getCloserTokSpan(), GroupArg->getArgs(), Issues);
                    }
                }
            }
            
            return std::make_shared<CallNode>(Left, GroupExpr->getOpenerTokSpan(), GroupExpr->getCloserTokSpan(), GroupExpr->getArgs(), GroupExpr->getIssues());
            
        }
        
        //
        // Something like GroupMissingCloseSquare
        //
        return GroupExpr;
        
    }
        
    return std::make_shared<SyntaxErrorNode>(ERROR_INTERNAL, std::vector<std::shared_ptr<Node>> { Right }, Issues);
}

std::shared_ptr<Node> LeftDoubleBracketCallParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Right = TheParser->parse(PRECEDENCE_HIGHEST);
    
    if (std::dynamic_pointer_cast<SyntaxErrorNode>(Right)) {
        return Right;
    }

    if (Left->getSourceSpan().end.Line != Right->getSourceSpan().start.Line) {

        auto Issue = SyntaxIssue("Head and double brackets are not on same line", SEVERITY_REMARK, (SourceSpan{Left->getSourceSpan().end, Right->getSourceSpan().start}));
        
        Issues.push_back(Issue);
    }
    
    if (auto GroupExpr = std::dynamic_pointer_cast<GroupNode>(Right)) {
        
        if (*GroupExpr->getOp() == *SYMBOL_GROUPDOUBLEBRACKET) {
            
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
    
    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    while (true) {
        
        auto Tok = TheParser->currentToken();
        
        if (Tok == OPERATOR_PLUS) {
            
            // Clear String
            TheParser->getString();
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            Args.push_back(operand);
            
        } else if (Tok == OPERATOR_MINUS) {
            
            // Clear String
            TheParser->getString();
            
            TheParser->nextToken();
            
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            auto Loc = operand->getSourceSpan();
            
            auto minus = std::make_shared<InternalMinusNode>(operand, Loc);
            
            Args.push_back(minus);
            
        } else {
            break;
        }
    }
    
    return std::make_shared<InfixNode>(SYMBOL_PLUS, Args, Issues);
}

std::shared_ptr<Node> SemicolonParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    
    std::vector<std::shared_ptr<Node>> Args;
    
    Args.push_back(Left);
    
    bool eatTheNextSemiColon = false;
    bool lastWasSemiColon = true;
    
    while (true) {
        
        Tok = TheParser->currentToken();
        
        if (Tok == TOKEN_NEWLINE) {
            
            if (lastWasSemiColon) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalEmptyNode>(Span);
                
                Args.push_back(Empty);
            }
            
            break;

        } else if (Tok == OPERATOR_SEMICOLON) {
            
            //
            // something like a; ; parses as CompoundExpression[a, Null, Null]
            //
            
            if (!eatTheNextSemiColon) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalEmptyNode>(Span);
                
                Args.push_back(Empty);
            }
            
            // Clear String
            TheParser->getString();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
            
            eatTheNextSemiColon = false;
            lastWasSemiColon = true;
            
        } else if (!TheParser->isPossibleBeginningOfExpression(Tok)) {
            
            if (lastWasSemiColon) {
                
                auto Span = TheSourceManager->getTokenSpan();
                
                auto Empty = std::make_shared<InternalEmptyNode>(Span);
                
                Args.push_back(Empty);
            }
            
            break;
            
        } else {
           
            auto operand = TheParser->parse(recalculatedPrecedence);
            
            if (std::dynamic_pointer_cast<SyntaxErrorNode>(operand)) {
                return operand;
            }
            
            Args.push_back(operand);
            
            eatTheNextSemiColon = true;
            lastWasSemiColon = false;
        }
    }
    
    return std::make_shared<InfixNode>(SYMBOL_COMPOUNDEXPRESSION, Args, Issues);
}


// prefix
std::shared_ptr<Node> SemicolonSemicolonParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto PrefixSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok)) {
        
        auto operand = TheParser->parse(recalculatedPrecedence);
        
        if (std::dynamic_pointer_cast<SyntaxErrorNode>(operand)) {
            return operand;
        }
        
        if (auto BinOp = std::dynamic_pointer_cast<BinaryNode>(operand)) {
            
            if (*BinOp->getOp() == *SYMBOL_SPAN) {
                
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
std::shared_ptr<Node> SemicolonSemicolonParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto InfixSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (TheParser->isPossibleBeginningOfExpression(Tok)) {
        
        auto Right = TheParser->parse(recalculatedPrecedence);
        
        if (std::dynamic_pointer_cast<SyntaxErrorNode>(Right)) {
            return Right;
        }
        
        if (auto BinLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
            
            if (*BinLeft->getOp() == *SYMBOL_SPAN) {
                
                auto LeftSpanLeft = BinLeft->getLeft();
                auto LeftSpanRight = BinLeft->getRight();
                
                return std::make_shared<TernaryNode>(SYMBOL_SPAN, LeftSpanLeft, LeftSpanRight, Right, Issues);
            }
        }
        
        if (auto BinRight = std::dynamic_pointer_cast<BinaryNode>(Right)) {
            
            if (*BinRight->getOp() == *SYMBOL_SPAN) {
                
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
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto prec = getPrecedence();
    
    // auto FirstTildeSpan = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken();
    
    auto Middle = TheParser->parse(prec);
    
    if (std::dynamic_pointer_cast<SyntaxErrorNode>(Middle)) {
        return Middle;
    }
    
    // auto MiddleSpan = Middle->getSourceSpan();

    auto Tok = TheParser->currentToken();
    
    auto Str = TheParser->getString();
    
    auto SecondTildeSpan = TheSourceManager->getTokenSpan();

    TheParser->nextToken();
    
    if (Tok != OPERATOR_TILDE) {
        return std::make_shared<SyntaxErrorNode>(ERROR_EXPECTEDTILDE, std::vector<std::shared_ptr<Node>> { std::make_shared<InternalTokenNode>(Str, SecondTildeSpan) }, Issues);
    }
    
    auto Right = TheParser->parse(prec);
    
    if (std::dynamic_pointer_cast<SyntaxErrorNode>(Right)) {
        return Right;
    }
    
    // if (!isContiguous(FirstTildeSpan.end, MiddleSpan.start)) {
                            
    //     auto Issue = SyntaxIssue("~ and Expr are not contiguous", SEVERITY_REMARK, (SourceSpan{FirstTildeSpan.end, MiddleSpan.start}));
        
    //     Issues.push_back(Issue);
    // }
    // if (!isContiguous(MiddleSpan.end, SecondTildeSpan.start)) {
                            
    //     auto Issue = SyntaxIssue("Expr and ~ are not contiguous", SEVERITY_REMARK, (SourceSpan{MiddleSpan.end, SecondTildeSpan.start}));
        
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
            return PRECEDENCE_PATTERNCOLON;
        }
        
        return PRECEDENCE_OPTIONALCOLON;
    }
    
    if (auto BinaryLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
        if (*BinaryLeft->getOp() == *SYMBOL_PATTERN) {
            return PRECEDENCE_OPTIONALCOLON;
        }
    }
    
    return PRECEDENCE_CONTEXT_SENSITIVE;
}

std::shared_ptr<Node> ColonParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();
    
    TheParser->nextToken();
    
    if (std::dynamic_pointer_cast<SymbolNode>(Left)) {
        
        assert(!TheParser->isInsideColon1());
        
        TheParser->setInsideColon1(true);
        
        auto prec = getColonPrecedence(Left);
        
        auto Right = TheParser->parse(prec);
        
        TheParser->setInsideColon1(false);
        
        return std::make_shared<BinaryNode>(SYMBOL_PATTERN, Left, Right, Issues);
        
    } else if (std::dynamic_pointer_cast<PatternBlankNode>(Left) ||
        std::dynamic_pointer_cast<PatternBlankSequenceNode>(Left) ||
        std::dynamic_pointer_cast<PatternBlankNullSequenceNode>(Left) ||
        std::dynamic_pointer_cast<BlankNode>(Left) ||
        std::dynamic_pointer_cast<BlankSequenceNode>(Left) ||
        std::dynamic_pointer_cast<BlankNullSequenceNode>(Left)) {
        
        auto prec = getColonPrecedence(Left);
        auto Right = TheParser->parse(prec);
        
        return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Left, Right, Issues);
        
    } else if (auto BinaryLeft = std::dynamic_pointer_cast<BinaryNode>(Left)) {
        
        if (*BinaryLeft->getOp() == *SYMBOL_PATTERN) {
            
            auto prec = getColonPrecedence(Left);
            auto Right = TheParser->parse(prec);
            
            return std::make_shared<BinaryNode>(SYMBOL_OPTIONAL, Left, Right, Issues);
        }  
    }
    
    return std::make_shared<SyntaxErrorNode>(ERROR_EXPECTEDSYMBOLORPATTERN, std::vector<std::shared_ptr<Node>> { Left }, Issues);
}

std::shared_ptr<Node> SlashColonParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - 1);
    
    TheParser->nextToken();
    
    auto Middle = TheParser->parse(recalculatedPrecedence);
    
    if (std::dynamic_pointer_cast<SyntaxErrorNode>(Middle)) {
        return Middle;
    }
    
    if (auto BinaryMiddle = std::dynamic_pointer_cast<BinaryNode>(Middle)) {
        
        if (*BinaryMiddle->getOp() == *SYMBOL_SET) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSET, Left, BinaryMiddle->getLeft(), BinaryMiddle->getRight(), Issues);
            
        } else if (*BinaryMiddle->getOp() == *SYMBOL_SETDELAYED) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGSETDELAYED, Left, BinaryMiddle->getLeft(), BinaryMiddle->getRight(), Issues);
            
        } else if (*BinaryMiddle->getOp() == *SYMBOL_UNSET) {
            
            return std::make_shared<TernaryNode>(SYMBOL_TAGUNSET, Left, BinaryMiddle->getLeft(), BinaryMiddle->getRight(), Issues);
        }
    }
    
    return std::make_shared<SyntaxErrorNode>(ERROR_MISMATCHEDSLASHCOLON, std::vector<std::shared_ptr<Node>> { Middle }, Issues);
}




std::shared_ptr<Node> LinearSyntaxOpenParenParselet::parse() {
    
    // Clear String
    TheParser->getString();
    
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
            
        } else if (Tok == OPERATOR_LINEARSYNTAX_OPENPAREN) {
            
            auto Sub = this->parse();
            
            if (std::dynamic_pointer_cast<SyntaxErrorNode>(Sub)) {
                return Sub;
            }
            
            if (auto SubOpenParen = std::dynamic_pointer_cast<GroupNode>(Sub)) {
                
                if (*SubOpenParen->getOp() == *SYMBOL_GROUPLINEARSYNTAXPAREN) {
                    
                    Tokens.push_back(SubOpenParen);
                    
                    Tok = TheParser->currentToken();
                    
                } else {
                    assert(false);
                }
                
            } else {
                assert(false);
            }
            
        } else if (Tok == OPERATOR_LINEARSYNTAX_CLOSEPAREN) {
            
            // Clear String
            TheParser->getString();
            
            CloserSpan = TheSourceManager->getTokenSpan();
            
            TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
            
            break;
            
        } else {
            
            auto Str = TheParser->getString();
            
            auto Span = TheSourceManager->getTokenSpan();
            
            Tokens.push_back(std::make_shared<InternalTokenNode>(Str, Span));
            
            Tok = TheParser->nextToken();
        }
    }
    
    return std::make_shared<GroupNode>(SYMBOL_GROUPLINEARSYNTAXPAREN, OpenerSpan, CloserSpan, Tokens, std::vector<SyntaxIssue>());
}



std::shared_ptr<Node> TickParselet::parse(std::shared_ptr<Node> Operand) {
    
    auto Str = TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto Span = TheSourceManager->getTokenSpan();
    
    TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
    
    if (Operand->getSourceSpan().end.Line != Span.start.Line) {
    
        auto Issue = SyntaxIssue(Str + " is not on same line as operand", SEVERITY_REMARK, (SourceSpan{Operand->getSourceSpan().end, Span.start}));
    
        Issues.push_back(Issue);
    }
    
    return std::make_shared<PostfixNode>(SYMBOL_DERIVATIVE, Span, Str.size(), Operand, Issues);
}


std::shared_ptr<Node> MessageNameParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto prec = getPrecedence();
    
    TheParser->nextToken();
    
    auto Middle = TheParser->parse(prec);
    
    auto Tok = TheParser->currentToken();
    
    if (Tok == OPERATOR_COLONCOLON) {
        
        // Clear String
        TheParser->getString();
        
        TheParser->nextToken();
        
        auto Right = TheParser->parse(prec);
        
        return std::make_shared<TernaryNode>(SYMBOL_MESSAGENAME, Left, Middle, Right, Issues);
    }

    return std::make_shared<BinaryNode>(SYMBOL_MESSAGENAME, Left, Middle, Issues);
}

std::shared_ptr<Node> EqualParselet::parse(std::shared_ptr<Node> Left) {
    
    // Clear String
    TheParser->getString();
    
    auto Issues = TheParser->getIssues();

    auto recalculatedPrecedence = static_cast<precedence_t>(getPrecedence() - (isRight() ? 1 : 0));
    
    auto EqualSpan = TheSourceManager->getTokenSpan();
    
    auto Tok = TheParser->nextToken();
    
    if (Tok == OPERATOR_DOT) {
        
        //
        // May be this: a = .
        // Need to parse as a =.
        //
        
        auto DotSpan = TheSourceManager->getTokenSpan();
        
        if (!isContiguous(EqualSpan, DotSpan)) {

            auto Issue = SyntaxIssue(std::string("= and . are not contiguous"), SEVERITY_REMARK, (SourceSpan{EqualSpan.start, DotSpan.end}));
        
            Issues.push_back(Issue);
        }
        
        // Clear String
        TheParser->getString();
        
        TheParser->nextToken(POLICY_PRESERVE_TOPLEVEL_NEWLINES);
        
        auto Empty = std::make_shared<InternalEmptyNode>(DotSpan);
        
        return std::make_shared<BinaryNode>(SYMBOL_UNSET, Left, Empty, Issues);
    }
    
    auto Right = TheParser->parse(recalculatedPrecedence);
    
    if (std::dynamic_pointer_cast<SyntaxErrorNode>(Right)) {
        return Right;
    }
    
    return std::make_shared<BinaryNode>(SYMBOL_SET, Left, Right, Issues);
}
