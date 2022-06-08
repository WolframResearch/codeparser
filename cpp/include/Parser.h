
#pragma once

#include "Node.h" // for LeafSeq
#include "Token.h" // for Token
#include "Precedence.h" // for Precedence
#include "TokenEnum.h" // for TokenEnum
#include "API.h" // for FirstLineBehavior

#include <set>
#include <deque>
#include <memory> // for unique_ptr
#include <vector>

class Parser;
class Parselet;

using ParserPtr = std::unique_ptr<Parser>;
using ParseletPtr = Parselet *;


//
//
//
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
    // Needs to detect the second ~ while parsing
    //
    PARSER_INSIDE_TILDE = 0x04,
    
    // UNUSED = 0x08
};

using ParserContextFlag = uint8_t;

//
//
//
struct ParserContext {
    
    //
    // Precedence of the current operator being parsed
    //
    Precedence Prec;
    
    ParserContextFlag Flag : 4;
    
    ParserContext() : Prec(PRECEDENCE_LOWEST), Flag() {}
    
    ParserContext(Precedence Prec) : Prec(Prec), Flag() {}
};

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert(sizeof(ParserContext) == 2, "Check your assumptions");
#endif // __clang__

//
//
//
class Parser {
private:
    
    std::vector<NodeSeq> ArgsStack;
    std::vector<NodePtr> NodeStack;
    std::vector<ParserContext> ContextStack;
    std::vector<Closer> GroupStack;
    
    void handleFirstLine(FirstLineBehavior firstLineBehavior);
    
public:
    
    Parser();
    
    void init();
    
    void deinit();
    
    void nextToken(Token Tok);
    
    Token nextToken0(NextPolicy policy);
    
    Token currentToken(NextPolicy policy) const;
    
    Token currentToken_stringifyAsTag() const;
    Token currentToken_stringifyAsFile() const;

    Token eatTrivia(Token firstTok, NextPolicy policy, TriviaSeq& Args);
    Token eatTrivia_stringifyAsFile(Token firstTok, TriviaSeq& Args);
    Token eatTriviaButNotToplevelNewlines(Token firstTok, NextPolicy policy, TriviaSeq& Args);
    Token eatTriviaButNotToplevelNewlines_stringifyAsFile(Token firstTok, TriviaSeq& Args);
    
    void shift();
    
    NodeSeq& pushArgs();
    NodeSeq popArgs();
    NodeSeq& peekArgs();
    size_t getArgsStackSize() const;
    
    void pushNode(NodePtr N);
    NodePtr popNode();
    size_t getNodeStackSize() const;
    
    void pushGroup(Closer Closr);
    void popGroup();
    size_t getGroupDepth() const;
    bool checkGroup(Closer Closr) const;
    
    ParserContext& topContext();
    ParserContext& pushFreshContext();
    ParserContext& pushInheritedContext(Precedence Prec);
    void popContext();
    size_t getContextStackSize() const;
    void clearContextStack();
};

void Parser_parseClimb(ParseletPtr Ignored, Token Ignored2);


extern ParserPtr TheParser;
