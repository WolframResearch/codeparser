
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

class Parser;

using ParserPtr = std::unique_ptr<Parser>;


enum Associativity {
    ASSOCIATIVITY_NONE,
    ASSOCIATIVITY_LEFT,
    ASSOCIATIVITY_RIGHT,
    ASSOCIATIVITY_NONASSOCIATIVE,
};

enum ParserContextFlagBits : uint8_t {
    //
    // when parsing a in a:b  then PARSER_INSIDE_COLON bit is 0
    // when parsing b in a:b  then PARSER_INSIDE_COLON bit is 1
    //
    PARSER_INSIDE_COLON = 0x01,
    
    //
    //
    //
    PARSER_INSIDE_INTEGRAL = 0x02,
    
    //
    //
    //
    PARSER_INSIDE_SLASHCOLON = 0x04,
};

using ParserContextFlag = uint8_t;

struct ParserContext {
    
    //
    // Precedence of the current operator being parsed
    //
    Precedence Prec;
    
    //
    // The Closer of the innermost Group being parsed
    //
    Closer Closr : 4;
    
    ParserContextFlag Flag : 3;
    
    //
    //
    //
    bool InsideGroup : 1;
    
    ParserContext() : Prec(), Closr(), Flag(), InsideGroup() {}
};

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert(sizeof(ParserContext) == 2, "Check your assumptions");
#endif

class Parser {
private:
    
    std::deque<Token> tokenQueue;
    
    std::vector<IssuePtr> Issues;
    
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
    
    ~Parser();

    Token eatAll(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatAll_stringifyFile(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatAndPreserveToplevelNewlines(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatAndPreserveToplevelNewlines_stringifyFile(Token firstTok, ParserContext Ctxt, LeafSeq&);
};

extern ParserPtr TheParser;

