
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
typedef void (*ParseFunction)(ParseletPtr, Token firstTok);

//
//
//
enum Associativity {
    ASSOCIATIVITY_NONRIGHT,
    ASSOCIATIVITY_RIGHT,
};

//
//
//
class Parser {
private:
    
    std::vector<NodeSeq> ArgsStack;
    std::vector<NodePtr> NodeStack;
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
    
    NodeSeq& pushArgs(Precedence Prec);
    NodeSeq popArgs();
    NodeSeq& peekArgs();
    size_t getArgsStackSize() const;
    
    NodePtr& topNode();
    void pushNode(NodePtr N);
    NodePtr popNode();
    size_t getNodeStackSize() const;
    
    void pushGroup(Closer Closr);
    void popGroup();
    size_t getGroupDepth() const;
    bool checkGroup(Closer Closr) const;
    
    Precedence topPrecedence();
    void setPrecedence(Precedence Prec);
    
    bool checkPatternPrecedence() const;
};

void Parser_parseClimb(ParseletPtr Ignored, Token Ignored2);
void Parser_tryContinue(ParseletPtr Ignored, Token Ignored2);


extern ParserPtr TheParser;
