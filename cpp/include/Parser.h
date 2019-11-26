
#pragma once

#include "Node.h"
#include "Source.h"
#include "Precedence.h"

#include <vector>
#include <array>
#include <cstddef>
#include <deque>
#include <memory> // for unique_ptr

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
class PrefixParselet;
#if STARTOFLINE
class StartOfLineParselet;
class StartOfFileParselet;
#endif // STARTOFLINE
class GroupParselet;
class Parselet;
class Parser;

using PrefixParseletPtr = std::unique_ptr<PrefixParselet>;
using InfixParseletPtr = std::unique_ptr<InfixParselet>;
using ContextSensitivePrefixParseletPtr = std::unique_ptr<ContextSensitivePrefixParselet>;
using ContextSensitiveInfixParseletPtr = std::unique_ptr<ContextSensitiveInfixParselet>;
using ParserPtr = std::unique_ptr<Parser>;


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
    //
    //
    PARSER_INTEGRAL = 0x02,
    
    //
    //
    //
    PARSER_PARSED_SYMBOL = 0x04,
    
    //
    //
    //
    PARSER_INSIDE_SLASHCOLON = 0x08,
};

using ParserContextFlag = uint8_t;

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
    
    std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets;
    std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets;
#if STARTOFLINE
    std::array<StartOfLineParseletPtr>, TOKEN_COUNT> startOfLineParselets;
    std::array<StartOfFileParseletPtr, TOKEN_COUNT> startOfFileParselets;
#endif // STARTOFLINE
    std::array<ContextSensitivePrefixParseletPtr, TOKEN_COUNT.value()> contextSensitivePrefixParselets;
    std::array<ContextSensitiveInfixParseletPtr, TOKEN_COUNT.value()> contextSensitiveInfixParselets;
    
    std::deque<Token> tokenQueue;
    
    std::vector<IssuePtr> Issues;
    
    
    void registerPrefixParselet(TokenEnum T, PrefixParseletPtr );
    
    void registerInfixParselet(TokenEnum T, InfixParseletPtr );

#if STARTOFLINE
    void registerStartOfLineParselet(TokenEnum, StartOfLineParseletPtr );
    
    void registerStartOfFileParselet(TokenEnum, StartOfFileParseletPtr );
#endif // STARTOFLINE
    
    void registerContextSensitivePrefixParselet(TokenEnum T, ContextSensitivePrefixParseletPtr );
    
    void registerContextSensitiveInfixParselet(TokenEnum T, ContextSensitiveInfixParseletPtr );
    
public:
    Parser();
    
    void init();
    
    void deinit();
    
    void nextToken();
    
#if STARTOFLINE
    void nextToken_stringifyLine();
#endif // STARTOFLINE
    
    void nextToken_stringifySymbol();
    void nextToken_stringifyFile();
    
    Token nextToken0();
    
    Token currentToken() const;

#if STARTOFLINE
    Token currentToken_stringifyLine() const;
#endif // STARTOFLINE
    
    Token currentToken_stringifySymbol() const;
    Token currentToken_stringifyFile() const;
    
    void prependInReverse(std::vector<LeafNodePtr>& );
    
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();

    void addIssue(IssuePtr);
#endif // !NISSUES
    
    NodePtr parse(Token firstTok, ParserContext Ctxt);
    
    NodePtr handleNotPossible(Token& tokenBad, Token& tokenAnchor, ParserContext Ctxt, bool *wasCloser);
    
    Precedence getTokenPrecedence(Token& current, ParserContext Ctxt, bool considerPrefix, bool *implicitTimes) const;
    
    
    const PrefixParseletPtr& findPrefixParselet(TokenEnum T) const;
    
    const InfixParseletPtr& findInfixParselet(TokenEnum T) const;
    
    const ContextSensitivePrefixParseletPtr& findContextSensitivePrefixParselet(TokenEnum T) const;
    
    const ContextSensitiveInfixParseletPtr& findContextSensitiveInfixParselet(TokenEnum T) const;
    
    ~Parser();

    Token eatAll(ParserContext Ctxt, LeafSeq&);
    Token eatAll_stringifyFile(ParserContext Ctxt, LeafSeq&);
    Token eatAndPreserveToplevelNewlines(ParserContext Ctxt, LeafSeq&);
    Token eatAndPreserveToplevelNewlines_stringifyFile(ParserContext Ctxt, LeafSeq&);
};

extern ParserPtr TheParser;

