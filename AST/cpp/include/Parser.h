
#pragma once

#include "Node.h"
#include "Precedence.h"

#include <map>

class PrefixParselet;
class InfixParselet;
class PostfixParselet;
class ContextSensitiveParselet;
class CleanupParselet;
class GroupParselet;
class Parselet;


enum NextTokenPolicy {
    POLICY_DEFAULT,
    POLICY_PRESERVE_TOPLEVEL_NEWLINES,
    POLICY_PRESERVE_EVERYTHING
};

struct ParserContext {
    size_t Depth;
    precedence_t Precedence;
    bool InsideColonParselet;
};

class Parser {
private:
    
    int groupDepth;
    bool currentCached;
    Token _currentToken;
    std::string _currentTokenString;

    std::map<Token, PrefixParselet *> mPrefixParselets;
    std::map<Token, InfixParselet *> mInfixParselets;
    std::map<Token, PostfixParselet *> mPostfixParselets;
    std::map<Token, ContextSensitiveParselet *> mContextSensitiveParselets;
    
    std::vector<std::pair<Token, std::string>> tokenQueue;

    std::vector<SyntaxIssue> Issues;
    
    void registerTokenType(Token, Parselet *);
    void registerPrefixTokenType(Token);
    
    precedence_t getCurrentTokenPrecedence(Token current, ParserContext Ctxt);

    std::shared_ptr<Node> cleanup(std::shared_ptr<Node>, ParserContext Ctxt);
    
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
    
    std::shared_ptr<Node> parse(ParserContext Ctxt);
    
    void decrementGroupDepth() {
        groupDepth--;
    }
    
    void incrementGroupDepth() {
        groupDepth++;
    }

    bool isPossibleBeginningOfExpression(Token Tok);

    ContextSensitiveParselet* findContextSensitiveParselet(Token Tok);
};

extern Parser *TheParser;
