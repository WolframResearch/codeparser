
#pragma once

#include "Node.h"
#include "Source.h"
#include "Precedence.h"

#include <vector>
#include <array>
#include <cstddef>
#include <functional> // for function with GCC and MSVC
#include <deque>
#include <memory> // for unique_ptr

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
class StartOfLineParselet;
class GroupParselet;
class Parselet;
class ExpectedPossibleExpressionErrorParselet;

enum Associativity {
    ASSOCIATIVITY_NONE,
    ASSOCIATIVITY_LEFT,
    ASSOCIATIVITY_RIGHT,
    ASSOCIATIVITY_NONASSOCIATIVE,
};

//
// How many _ are currently being parsed?
//
enum UnderEnum {
    UNDER_UNKNOWN,
    UNDER_1,
    UNDER_2,
    UNDER_3
};

enum ParserContextFlagBits : uint8_t {
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    PARSER_COLON = 0x01,
    
    //
    // Inside of linear syntax \( \)  ?
    //
    PARSER_LINEARSYNTAX = 0x02,
    
    //
    //
    //
    PARSER_STRINGIFY_CURRENT_LINE = 0x04,
    
    //
    //
    //
    PARSER_INTEGRAL = 0x08,
    
    //
    //
    //
    PARSER_PARSED_SYMBOL = 0x10,
    
    //
    //
    //
    PARSER_INSIDE_SLASHCOLON = 0x20,
};

class ParserContextFlag {
    uint8_t val;
public:
    constexpr ParserContextFlag() : val() {}
    constexpr ParserContextFlag(uint8_t val) : val(val) {}
    
    ParserContextFlagBits operator&(const ParserContextFlagBits bits) const {
        return static_cast<ParserContextFlagBits>(val & bits);
    }
    
    ParserContextFlagBits operator|(const ParserContextFlagBits bits) const {
        return static_cast<ParserContextFlagBits>(val | bits);
    }
    
    void operator|=(const ParserContextFlagBits bits) {
        val |= bits;
    }
    
    void clear(const ParserContextFlagBits bits) {
        val &= ~bits;
    }
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
    // The Closer of the innermost Group being parsed
    //
    TokenEnum Closer;
    
    //
    // When parsing  _  or __  or ___  , the implementation is the same, so just keep track of which one is being parsed
    //
    UnderEnum UnderCount;
    
    ParserContextFlag Flag;
    
    ParserContext() : GroupDepth(0), Prec(PRECEDENCE_LOWEST), Assoc(ASSOCIATIVITY_NONE), Closer(TOKEN_UNKNOWN), UnderCount(UNDER_UNKNOWN), Flag() {}
    
    size_t getGroupDepth() {
        return GroupDepth;
    }
    
};

class Parser {
private:
    
    std::array<std::unique_ptr<PrefixParselet>, TOKEN_COUNT> prefixParselets;
    std::array<std::unique_ptr<InfixParselet>, TOKEN_COUNT> infixParselets;
    std::array<std::unique_ptr<StartOfLineParselet>, TOKEN_COUNT> startOfLineParselets;
    std::array<std::unique_ptr<ContextSensitivePrefixParselet>, TOKEN_COUNT> contextSensitivePrefixParselets;
    std::array<std::unique_ptr<ContextSensitiveInfixParselet>, TOKEN_COUNT> contextSensitiveInfixParselets;
    
    std::unique_ptr<ExpectedPossibleExpressionErrorParselet> expectedPossibleExpressionErrorParselet;
    
    std::deque<Token> tokenQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    std::function<bool ()> currentAbortQ;
    
    bool implicitTimesEnabled;
    
    
    void registerPrefixParselet(TokenEnum, std::unique_ptr<PrefixParselet> );
    
    void registerInfixParselet(TokenEnum, std::unique_ptr<InfixParselet> );
    
    void registerStartOfLineParselet(TokenEnum, std::unique_ptr<StartOfLineParselet> );
    
    void registerContextSensitivePrefixParselet(TokenEnum, std::unique_ptr<ContextSensitivePrefixParselet> );
    
    void registerContextSensitiveInfixParselet(TokenEnum, std::unique_ptr<ContextSensitiveInfixParselet> );
    
    
    
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
    
    
    void addIssue(SyntaxIssue);
    
    NodePtr parse(ParserContext Ctxt);
    
    bool isPossibleBeginningOfExpression(const Token& Tok, ParserContext Ctxt) const;
    
    
    const std::unique_ptr<PrefixParselet>& findPrefixParselet(TokenEnum Tok) const;
    
    const std::unique_ptr<InfixParselet>& findInfixParselet(TokenEnum Tok) const;
    
    const std::unique_ptr<ContextSensitivePrefixParselet>& findContextSensitivePrefixParselet(TokenEnum Tok) const;
    
    const std::unique_ptr<ContextSensitiveInfixParselet>& findContextSensitiveInfixParselet(TokenEnum Tok) const;
    
    bool isAbort() const;
    
    ~Parser();
    
    static const Token eatAll(const Token& Tok, ParserContext Ctxt, std::unique_ptr<NodeSeq>&);
    static const Token eatAll(const Token& Tok, ParserContext Ctxt, std::unique_ptr<LeafSeq>&);
    
    static const Token eatAndPreserveToplevelNewlines(const Token& Tok, ParserContext Ctxt, std::unique_ptr<NodeSeq>&);
    static const Token eatAndPreserveToplevelNewlines(const Token& Tok, ParserContext Ctxt, std::unique_ptr<LeafSeq>&);
};

extern std::unique_ptr<Parser> TheParser;

