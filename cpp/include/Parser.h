
#pragma once

#include "Node.h"
#include "Source.h"
#include "Precedence.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <functional>

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class ContextSensitiveParselet;
class CleanupParselet;
class GroupParselet;
class Parselet;


//
// These bits are set in a policy to flag which tokens to preserve
//
enum NextTokenPolicyBits {
    
    PRESERVE_TOPLEVEL_NEWLINES = 0x01,
    
    PRESERVE_OTHER_NEWLINES = 0x02,
    
    PRESERVE_WHITESPACE = 0x04,
    
    PRESERVE_TOPLEVEL_COMMENTS = 0x08,
    
    PRESERVE_OTHER_COMMENTS = 0x10
};

typedef int NextTokenPolicy;

const NextTokenPolicy DISCARD_EVERYTHING = 0;

const NextTokenPolicy PRESERVE_EVERYTHING = ~DISCARD_EVERYTHING;

struct ParserContext {
    size_t GroupDepth;
    size_t OperatorDepth;
    precedence_t Precedence;
    bool ColonFlag;
    bool LinearSyntaxFlag;
    
    bool isGroupTopLevel() {
        return GroupDepth == 0;
    }
    
    bool isOperatorTopLevel() {
        return OperatorDepth == 0;
    }
};

class Parser {
private:
    
    bool currentCached;
    Token _currentToken;
    std::string _currentTokenString;

    std::unordered_map<Token, PrefixParselet *> prefixParselets;
    std::unordered_map<Token, InfixParselet *> infixParselets;
    std::unordered_map<Token, CallParselet *> callParselets;
    std::unordered_map<Token, PostfixParselet *> postfixParselets;
    std::unordered_map<Token, ContextSensitiveParselet *> contextSensitiveParselets;
    std::unordered_set<Parselet *> parselets;
    
    std::vector<std::pair<Token, std::string>> tokenQueue;

    std::vector<SyntaxIssue> Issues;
    std::vector<Comment> Comments;
    
    std::function<bool ()> currentAbortQ;

    void registerTokenType(Token, Parselet *);
    
    precedence_t getCurrentTokenPrecedence(Token current, ParserContext Ctxt);

    std::shared_ptr<Node> cleanup(std::shared_ptr<Node>, ParserContext Ctxt);
    
public:
    Parser();
    
    void init(std::function<bool ()> AbortQ);
    
    void deinit();

    Token nextToken(ParserContext Ctxt, NextTokenPolicy policy);
    
    Token tryNextToken(ParserContext Ctxt, NextTokenPolicy policy);
    
    Token currentToken();
    void setCurrentToken(Token current, std::string Str);

    std::string getTokenString();
    
    std::vector<SyntaxIssue> getIssues();

    void addIssue(SyntaxIssue);
    
    void addComment(Comment);
    
    std::vector<Comment> getComments();

    std::shared_ptr<Node> parseTopLevel();
    
    std::shared_ptr<Node> parse(ParserContext Ctxt);

    bool isPossibleBeginningOfExpression(Token Tok);
    
    ContextSensitiveParselet* findContextSensitiveParselet(Token Tok);
    
    bool isAbort();
    
    ~Parser();
};

extern Parser *TheParser;
