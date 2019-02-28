
#pragma once

#include "Node.h"
#include "SyntaxIssue.h"
#include "Precedence.h"

#include <map>
#include <set>
#include <vector>

class PrefixParselet;
class InfixParselet;
class PostfixParselet;
class ContextSensitiveParselet;
class CleanupParselet;
class GroupParselet;
class Parselet;


enum NextTokenPolicy {
    //
    // DISCARD_TOPLEVEL_NEWLINES:
    // discard top-level newlines, discard other newline, discard whitespace, keep track of comments but don't return them
    //
    NEXTTOKEN_DISCARD_TOPLEVEL_NEWLINES,
    
    //
    // PRESERVE_TOPLEVEL_NEWLINES:
    // preserve top-level newlines, discard other newlines, discard whitespace, keep track of comments but don't return them
    //
    NEXTTOKEN_PRESERVE_TOPLEVEL_NEWLINES,
    
    //
    // PRESERVE_EVERYTHING
    // return newlines, return whitespace, keep track of comments AND ALSO return them
    //
    // Note: Whoever uses PRESERVE_EVERYTHING also needs to own any Comments that are read in
    //
    NEXTTOKEN_PRESERVE_EVERYTHING,
    
    //
    // PRESERVE_EVERYTHING_AND_DONT_RETURN_COMMENTS
    // return newlines, return whitespace, keep track of comments but don't return them
    //
    NEXTTOKEN_PRESERVE_EVERYTHING_AND_DONT_RETURN_COMMENTS
};

struct ParserContext {
    size_t GroupDepth;
    size_t OperatorDepth;
    precedence_t Precedence;
    bool ColonFlag1;
    
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

    std::map<Token, PrefixParselet *> prefixParselets;
    std::map<Token, InfixParselet *> infixParselets;
    std::map<Token, PostfixParselet *> postfixParselets;
    std::map<Token, ContextSensitiveParselet *> contextSensitiveParselets;
    std::set<Parselet *> parselets;
    
    std::vector<std::pair<Token, std::string>> tokenQueue;

    std::vector<SyntaxIssue> Issues;
    std::vector<Comment> Comments;
    
    void registerTokenType(Token, Parselet *);
    void registerPrefixTokenType(Token);
    
    precedence_t getCurrentTokenPrecedence(Token current, ParserContext Ctxt);

    std::shared_ptr<Node> cleanup(std::shared_ptr<Node>, ParserContext Ctxt);
    
public:
    Parser();
    
    void init();
    
    void deinit();

    Token nextToken(ParserContext Ctxt, NextTokenPolicy policy);
    
    Token tryNextToken(ParserContext Ctxt, NextTokenPolicy policy);
    
    Token currentToken();
    void setCurrentToken(Token current, std::string Str);

    std::string getString();
    
    std::vector<SyntaxIssue> getIssues();

    void addIssue(SyntaxIssue);
    
    void addComment(Comment);
    
    std::vector<Comment> getComments();

    std::shared_ptr<Node> parseTopLevel();
    
    std::shared_ptr<Node> parse(ParserContext Ctxt);

    bool isPossibleBeginningOfExpression(Token Tok);

    ContextSensitiveParselet* findContextSensitiveParselet(Token Tok);

    ~Parser();
};

extern Parser *TheParser;
