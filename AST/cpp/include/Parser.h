
#pragma once

#include "Node.h"
#include "Precedence.h"

#include <map>

class PrefixParselet;
class InfixParselet;
class PostfixParselet;
class CleanupParselet;
class GroupParselet;
class Parselet;


enum NextTokenPolicy {
    POLICY_DEFAULT,
    POLICY_PRESERVE_TOPLEVEL_NEWLINES,
    POLICY_PRESERVE_EVERYTHING
};

class Parser {
private:
    
    int groupDepth;
    bool insideColon;

    std::map<Token, PrefixParselet *> mPrefixParselets;
    std::map<Token, InfixParselet *> mInfixParselets;
    std::map<Token, PostfixParselet *> mPostfixParselets;
    std::map<Token, CleanupParselet *> mCleanupParselets;
    
    std::vector<std::pair<Token, std::string>> tokenQueue;
    bool currentCached;
    Token _currentToken;
    std::string _currentTokenString;

    void registerTokenType(Token, Parselet *);
    void registerPrefixTokenType(Token);
    
    precedence_t getCurrentTokenPrecedence(Token current, std::shared_ptr<Node> Left);

    std::shared_ptr<Node> cleanup(std::shared_ptr<Node>);
    
public:
    Parser();
    
    void init();
    
    Token nextToken(NextTokenPolicy policy = POLICY_DEFAULT);
    
    Token tryNextToken(NextTokenPolicy policy = POLICY_DEFAULT);
    
    Token currentToken();
    void setCurrentToken(Token current, std::string Str);

    std::string getString();
    
    std::vector<SyntaxIssue> getIssues();

    std::shared_ptr<Node> parseTopLevel();
    
    std::shared_ptr<Node> parse(precedence_t Precedence);

    bool isInsideColon1() {
        return insideColon;
    }
    
    void setInsideColon1(bool b) {
        insideColon = b;
    }
    
    void decrementGroupDepth() {
        groupDepth--;
    }
    
    void incrementGroupDepth() {
        groupDepth++;
    }

    bool isPossibleBeginningOfExpression(Token Tok);
};

extern Parser *TheParser;

class ParserScoper {
    bool b;
    
public:
    ParserScoper() {
        b = TheParser->isInsideColon1();
    }
    
    ~ParserScoper() {
        TheParser->setInsideColon1(b);
    }
};

