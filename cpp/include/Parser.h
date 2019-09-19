
#pragma once

#include "Node.h"
#include "Source.h"
#include "Precedence.h"

#include <vector>
#include <chrono>
#include <array>
#include <cstddef>
#include <functional> // for function with GCC and MSVC
#include <deque>

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
class StartOfLineParselet;
class CleanupParselet;
class GroupParselet;
class Parselet;

//
// How many _ are currently being parsed?
//
enum UnderEnum {
    UNDER_UNKNOWN,
    UNDER_1,
    UNDER_2,
    UNDER_3
};

struct ParserContext {
    
    //
    // Each time a GroupNode (or LinearSyntaxOpenParenNode) is entered, then GroupDepth increments
    // This is used for detecting whether we are parsing at top-level.
    //
    size_t GroupDepth;
    
    //
    // Precedence of the current operator being parsed
    //
    Precedence Prec;
    
    //
    // Associativity of the current operator being parsed
    //
    Associativity Assoc;
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    bool ColonFlag;
    
    //
    // Inside of linear syntax \( \)  ?
    //
    bool LinearSyntaxFlag;
    
    //
    //
    //
    bool StringifyCurrentLine;
    
    //
    //
    //
    bool IntegralFlag;
    
    //
    // The Closer of the innermost Group being parsed
    //
    TokenEnum Closer;
    
    //
    // When parsing  _  or __  or ___  , the implementation is the same, so just keep track of which one is being parsed
    //
    UnderEnum UnderCount;
    
    //
    //
    //
    bool AllowTrailing;
    
    ParserContext() : GroupDepth(0), Prec(PRECEDENCE_LOWEST), Assoc(ASSOCIATIVITY_NONE), ColonFlag(false), LinearSyntaxFlag(false), StringifyCurrentLine(false), IntegralFlag(false), Closer(TOKEN_UNKNOWN), UnderCount(UNDER_UNKNOWN), AllowTrailing(false) {}
    
    ParserContext(size_t GroupDepth, Precedence Prec, Associativity Assoc, bool ColonFlag, bool LinearSyntaxFlag, bool StringifyCurrentLine, bool IntegralFlag, TokenEnum Closer, UnderEnum UnderCount, bool AllowTrailing) : GroupDepth(GroupDepth), Prec(Prec), Assoc(Assoc), ColonFlag(ColonFlag), LinearSyntaxFlag(LinearSyntaxFlag), StringifyCurrentLine(StringifyCurrentLine), IntegralFlag(IntegralFlag), Closer(Closer), UnderCount(UnderCount), AllowTrailing(AllowTrailing) {}
    
    size_t getGroupDepth() {
        return GroupDepth;
    }
    
};

class Parser {
private:
    
    std::array<std::unique_ptr<const PrefixParselet>, TOKEN_COUNT> prefixParselets;
    std::array<std::unique_ptr<const InfixParselet>, TOKEN_COUNT> infixParselets;
    std::array<std::unique_ptr<const StartOfLineParselet>, TOKEN_COUNT> startOfLineParselets;
    std::array<std::unique_ptr<const ContextSensitivePrefixParselet>, TOKEN_COUNT> contextSensitivePrefixParselets;
    std::array<std::unique_ptr<const ContextSensitiveInfixParselet>, TOKEN_COUNT> contextSensitiveInfixParselets;
    
    std::deque<Token> tokenQueue;
    
    std::vector<SyntaxIssue> Issues;
    std::chrono::microseconds totalTimeMicros;
    
    std::function<bool ()> currentAbortQ;
    
    
    void registerPrefixParselet(TokenEnum, std::unique_ptr<const PrefixParselet> );
    
    void registerInfixParselet(TokenEnum, std::unique_ptr<const InfixParselet> );
    
    void registerStartOfLineParselet(TokenEnum, std::unique_ptr<const StartOfLineParselet> );
    
    void registerContextSensitivePrefixParselet(TokenEnum, std::unique_ptr<const ContextSensitivePrefixParselet> );
    
    void registerContextSensitiveInfixParselet(TokenEnum, std::unique_ptr<const ContextSensitiveInfixParselet> );
    
    
    
    NodePtr parse0(std::unique_ptr<NodeSeq> Left, Precedence, ParserContext Ctxt);
    
    void nextToken0(ParserContext Ctxt);
    
    Precedence getCurrentTokenPrecedence(Token& current, ParserContext Ctxt);
    
public:
    Parser();
    
    void init(std::function<bool ()> AbortQ, const std::deque<Token>& queued);
    
    void deinit();
    
    Token nextToken(ParserContext Ctxt);
    
    Token currentToken() const;
    
    void prepend(const Token& current);
    
    void prependInReverse(std::unique_ptr<LeafSeq>);
    
    
    std::vector<SyntaxIssue> getIssues() const;
    
    //    std::vector<Metadata> getMetadatas() const;
    
    
    void addIssue(SyntaxIssue);
    
    NodePtr parse(ParserContext Ctxt);
    
    bool isPossibleBeginningOfExpression(const Token& Tok, ParserContext Ctxt) const;
    
    const std::unique_ptr<const InfixParselet>& findInfixParselet(TokenEnum Tok) const;
    
    const std::unique_ptr<const ContextSensitivePrefixParselet>& findContextSensitivePrefixParselet(TokenEnum Tok) const;
    
    const std::unique_ptr<const ContextSensitiveInfixParselet>& findContextSensitiveInfixParselet(TokenEnum Tok) const;
    
    
    bool isAbort() const;
    
    ~Parser();
    
    static const Token eatAll(const Token& Tok, ParserContext Ctxt, std::unique_ptr<NodeSeq>&);
    static const Token eatAll(const Token& Tok, ParserContext Ctxt, std::unique_ptr<LeafSeq>&);
    
    static const Token eatAndPreserveToplevelNewlines(const Token& Tok, ParserContext Ctxt, std::unique_ptr<NodeSeq>&);
    static const Token eatAndPreserveToplevelNewlines(const Token& Tok, ParserContext Ctxt, std::unique_ptr<LeafSeq>&);
};

extern Parser *TheParser;

