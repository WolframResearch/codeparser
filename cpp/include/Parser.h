
#pragma once

#include "Node.h" // for LeafNodePtr, etc.
#include "Source.h" // for IssuePtr
#include "Token.h" // for Token
#include "Precedence.h" // for Precedence
#include "TokenEnum.h" // for TokenEnum

#include <vector>
#include <deque>
#include <memory> // for unique_ptr

class Parser;

using ParserPtr = std::unique_ptr<Parser>;


enum Associativity {
    ASSOCIATIVITY_NONRIGHT,
    ASSOCIATIVITY_RIGHT,
};

//
// The ParserContextFlagBits tend to contain context-sensitive bits for parsing
//
// Generally the parser is a Pratt parser with 1 token of look-ahead, except in these few cases.
//
enum ParserContextFlagBits : uint8_t {
    //
    // when parsing a in a:b  then PARSER_INSIDE_COLON bit is 0
    // when parsing b in a:b  then PARSER_INSIDE_COLON bit is 1
    //
    PARSER_INSIDE_COLON = 0x01,
    
    //
    // Needs to detect \[Differential] while parsing
    //
    PARSER_INSIDE_INTEGRAL = 0x02,
    
    //
    // Needs to detect the = or := or =. while parsing
    //
    PARSER_INSIDE_SLASHCOLON = 0x04,
    
    //
    // Needs to detect the second ~ while parsing
    //
    PARSER_INSIDE_TILDE = 0x08,
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
    
    ParserContextFlag Flag : 4;
    
    ParserContext() : Prec(PRECEDENCE_LOWEST), Closr(), Flag() {}
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
    
    void nextToken_stringifySymbol();
    void nextToken_stringifyFile();
    
    Token nextToken0(ParserContext Ctxt);
    Token nextToken0(ParserContext Ctxt, NextPolicy policy);
    
    Token currentToken(ParserContext Ctxt) const;
    Token currentToken(ParserContext Ctxt, NextPolicy policy) const;
    
    Token currentToken_stringifySymbol() const;
    Token currentToken_stringifyFile() const;
    
    void prepend(Token );
    
    void prependInReverse(std::vector<LeafNodePtr>& );
    
    
#if !NISSUES
    std::vector<IssuePtr>& getIssues();

    void addIssue(IssuePtr);
#endif // !NISSUES
    
    NodePtr infixLoop(NodePtr Left, ParserContext Ctxt);
    
    ~Parser();

    Token eatTrivia(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatTrivia_stringifyFile(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatTriviaButNotToplevelNewlines(Token firstTok, ParserContext Ctxt, LeafSeq&);
    Token eatTriviaButNotToplevelNewlines_stringifyFile(Token firstTok, ParserContext Ctxt, LeafSeq&);
    
    Token eatLineContinuations(Token firstTok, ParserContext Ctxt, LeafSeq&);
};

extern ParserPtr TheParser;

