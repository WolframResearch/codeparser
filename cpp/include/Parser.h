
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
class StartOfLineParselet;
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
    
    //
    // If using PRESERVE_COMMENTS, then COMMENT tokens may be returned and must be handled by the caller
    //
    PRESERVE_COMMENTS = 0x08,
    
};

typedef int NextTokenPolicy;

const NextTokenPolicy DISCARD_EVERYTHING = 0;

const NextTokenPolicy PRESERVE_EVERYTHING = ~DISCARD_EVERYTHING;


//
// How many _ are currently being parsed?
//
enum UnderEnum {
    UNDER_UNKNOWN,
    UNDER_1,
    UNDER_2,
    UNDER_3
};

struct ParserContext {
    
    //
    // Each time a GroupNode (or LinearSyntaxOpenParenNode) is entered, then GroupDepth increments
    //
    size_t GroupDepth;
    
    //
    // Each time any PrefixNode, PostfixNode, BinaryNode, InfixNode,
    //    GroupNode, special parselets like EqualParselet, etc.   is entered, then OperatorDepth increment
    //
    size_t OperatorDepth;
    
    //
    // Precedence of the current operator being parsed
    //
    Precedence Prec;
    
    //
    // Associativity of the current operator being parsed
    //
    Associativity Assoc;
    
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    bool ColonFlag;
    
    //
    // Inside of linear syntax \( \)  ?
    //
    bool LinearSyntaxFlag;
    
    //
    //
    //
    bool InformationFlag;
    
    //
    //
    //
    bool IntegralFlag;
    
    //
    // The Closer of the innermost Group being parsed
    //
    TokenEnum Closer;
    
    //
    // When parsing  _  or __  or ___  , the implementation is the same, so just keep track of which one is being parsed
    //
    UnderEnum UnderCount;
    
    ParserContext() : GroupDepth(0), OperatorDepth(0), Prec(PRECEDENCE_LOWEST), Assoc(ASSOCIATIVITY_NONE), ColonFlag(false), LinearSyntaxFlag(false), InformationFlag(false), IntegralFlag(false), Closer(TOKEN_UNKNOWN), UnderCount(UNDER_UNKNOWN) {}

    ParserContext(size_t GroupDepth, size_t OperatorDepth, Precedence Prec, Associativity Assoc, bool ColonFlag, bool LinearSyntaxFlag, bool InformationFlag, bool IntegralFlag, TokenEnum Closer, UnderEnum UnderCount) : GroupDepth(GroupDepth), OperatorDepth(OperatorDepth), Prec(Prec), Assoc(Assoc), ColonFlag(ColonFlag), LinearSyntaxFlag(LinearSyntaxFlag), InformationFlag(InformationFlag), IntegralFlag(IntegralFlag), Closer(Closer), UnderCount(UnderCount) {}
    
    bool isGroupTopLevel() const {
        return GroupDepth == 0;
    }
    
    bool isOperatorTopLevel() const {
        return OperatorDepth == 0;
    }
};

class Parser {
private:

    std::unordered_map<TokenEnum, PrefixParselet *> prefixParselets;
    std::unordered_map<TokenEnum, InfixParselet *> infixParselets;
    std::unordered_map<TokenEnum, CallParselet *> callParselets;
    std::unordered_map<TokenEnum, PostfixParselet *> postfixParselets;
    std::unordered_map<TokenEnum, ContextSensitiveParselet *> contextSensitiveParselets;
    std::unordered_map<TokenEnum, StartOfLineParselet *> startOfLineParselets;
    std::unordered_set<Parselet *> parselets;
    
    std::vector<Token> tokenQueue;

    std::vector<SyntaxIssue> Issues;
    std::vector<Token> Comments;
    
    std::function<bool ()> currentAbortQ;

    void registerTokenType(TokenEnum, Parselet *);
    
    Precedence getCurrentTokenPrecedence(TokenEnum current, ParserContext Ctxt);
    
public:
    Parser();
    
    void init(std::function<bool ()> AbortQ, std::vector<Token> queued);
    
    void deinit();

    Token nextToken0(ParserContext Ctxt);
    
    Token nextToken(ParserContext Ctxt, NextTokenPolicy policy);
    
    Token tryNextToken(ParserContext Ctxt, NextTokenPolicy policy);
    
    Token currentToken() const;
    
    void setCurrentToken(Token current);
    
    std::vector<SyntaxIssue> getIssues() const;

    void addIssue(SyntaxIssue);
    
    std::vector<Token> getComments() const;
    
    std::shared_ptr<Node> parse(ParserContext Ctxt);

    bool isPossibleBeginningOfExpression(Token Tok, ParserContext Ctxt) const;
    
    ContextSensitiveParselet* findContextSensitiveParselet(TokenEnum Tok) const;
    
    bool isAbort() const;
    
    ~Parser();
};

extern Parser *TheParser;
