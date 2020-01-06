
#pragma once

#include "Node.h" // for LeafNodePtr, etc.
#include "Source.h" // for IssuePtr
#include "Token.h" // for Token
#include "Precedence.h" // for Precedence
#include "TokenEnum.h" // for TokenEnum

#include <vector>
#include <array>
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
    uint16_t GroupDepth;
    
    //
    //
    //
    uint16_t StackDepth;
    
    //
    // The Closer of the innermost Group being parsed
    //
    TokenEnum Closer;
    
    //
    // Precedence of the current operator being parsed
    //
    Precedence Prec;
    
    //
    // When parsing  _  or __  or ___  , the implementation is the same, so just keep track of which one is being parsed
    //
    UnderEnum UnderCount : 2;
    
    ParserContextFlag Flag : 4;
    
    ParserContext() : GroupDepth(0), StackDepth(0), Closer(TOKEN_UNKNOWN), Prec(PRECEDENCE_LOWEST), UnderCount(UNDER_UNKNOWN), Flag() {}
};

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert(sizeof(ParserContext) == 8, "Check your assumptions");
#endif

class Parser {
private:
    
    std::array<PrefixParseletPtr, TOKEN_COUNT.value()> prefixParselets;
    std::array<InfixParseletPtr, TOKEN_COUNT.value()> infixParselets;
#if STARTOFLINE
    std::array<StartOfLineParseletPtr>, TOKEN_COUNT> startOfLineParselets;
    std::array<StartOfFileParseletPtr, TOKEN_COUNT> startOfFileParselets;
#endif // STARTOFLINE
    
    ContextSensitivePrefixParseletPtr contextSensitiveSymbolParselet;
    ContextSensitiveInfixParseletPtr contextSensitiveUnderParselet;
    ContextSensitiveInfixParseletPtr contextSensitiveColonParselet;
    
    std::deque<Token> tokenQueue;
    
    std::vector<IssuePtr> Issues;
    
    
    void registerPrefixParselet(size_t i, PrefixParseletPtr );
    
    void registerInfixParselet(size_t i, InfixParseletPtr );

#if STARTOFLINE
    void registerStartOfLineParselet(size_t i, StartOfLineParseletPtr );
    
    void registerStartOfFileParselet(size_t i, StartOfFileParseletPtr );
#endif // STARTOFLINE
    
public:
    Parser();
    
    void init();
    
    void deinit();
    
    void nextToken(Token Tok);
    
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
    
    
    Precedence getTokenPrecedence(Token& current, ParserContext Ctxt) const;
    Precedence getInfixTokenPrecedence(Token& current, ParserContext Ctxt, bool *implicitTimes) const;
    
    
    const PrefixParseletPtr& findPrefixParselet(TokenEnum T) const;
    
    const InfixParseletPtr& findInfixParselet(TokenEnum T) const;
    
    
    const ContextSensitivePrefixParseletPtr& getContextSensitiveSymbolParselet() const;
    const ContextSensitiveInfixParseletPtr& getContextSensitiveUnderParselet() const;
    const ContextSensitiveInfixParseletPtr& getContextSensitiveColonParselet() const;
    
    ~Parser();

    Token eatAll(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatAll_stringifyFile(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatAndPreserveToplevelNewlines(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatAndPreserveToplevelNewlines_stringifyFile(Token firstTok, ParserContext Ctxt, LeafSeq&);
};

extern ParserPtr TheParser;

