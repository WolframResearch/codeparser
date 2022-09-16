
#pragma once

#include "Source.h" // for Source
#include "Token.h" // for Token
#include "API.h" // for UnsafeCharacterEncodingFlag
#include "Symbol.h"
#include "Issue.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <vector>
#include <set>
#include <ostream>
#include <cstddef> // for size_t
#include <variant>

class Node;
class ParserSession;

using NodePtr = const Node *;
using ParserSessionPtr = ParserSession *;

using NodeVariant = std::variant<NodePtr, struct Token>;

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
// Used mainly for collecting trivia that has been eaten
//
class TriviaSeq {
private:
    
    std::vector<Token> vec;
    
public:
    
    TriviaSeq();
    
    std::vector<Token>::iterator begin();
    std::vector<Token>::iterator end();
    
    void reset(ParserSessionPtr session);
    
    bool empty() const;
    
    void push(Token N);
    
    void clear();
};


//
// A sequence of Nodes
//
// When parsing  a(**)+b  we actually want to keep track of the comment.
// But the comment does not affect the parsing: a(**) is still 1 "thing" to the parser
//
// So pass around a structure that contains all of the nodes from the left, including comments and whitespace.
//
class NodeSeq {
private:
    
    std::vector<NodeVariant> vec;
    
public:
    
    NodeSeq();
    NodeSeq(std::vector<NodeVariant> vec);
    NodeSeq(std::vector<NodeVariant>::iterator Begin, std::vector<NodeVariant>::iterator End);
    
    void release();
    
    bool empty() const;
    
    void push(NodeVariant&& N);
    
    void clear();
    
    size_t size() const;
    
    const NodeVariant& operator[] (size_t index) const;
    
    const NodeVariant& first() const;
    
    const NodeVariant& last() const;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const;
    
    bool syntaxQ() const;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};



//
// An expression representing a node in the syntax tree
//
class Node {
public:
    
    virtual ~Node();
    
    virtual void release();
    
    virtual void print(ParserSessionPtr session, std::ostream& s) const = 0;
    
    virtual Source getSource() const = 0;
    
#if USE_MATHLINK
    virtual void put(ParserSessionPtr session, MLINK callLink) const = 0;
#endif // USE_MATHLINK
    
    virtual bool syntaxQ() const = 0;
    
#if USE_EXPR_LIB
    virtual expr toExpr(ParserSessionPtr session) const = 0;
#endif // USE_EXPR_LIB
};

//
// Any kind of prefix, postfix, binary, or infix operator
//
class OperatorNode : public Node {
private:
    
    const Symbol Op;
    const Symbol MakeSym;
    NodeSeq Children;
    Source Src;
    
public:
    
    OperatorNode(Symbol Op, Symbol MakeSym, NodeSeq&& Children);
    
    void release() override;
    
    Source getSource() const override;
    
    bool syntaxQ() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
    
    Symbol getOp() const;
};

//
//
//
class AbortNode : public Node {
public:
    
    AbortNode();
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
    Source getSource() const override;
    
    bool syntaxQ() const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
// PrefixNode
//
// -a
//
class PrefixNode : public OperatorNode {
public:
    
    PrefixNode(Symbol Op, NodeSeq&& Args);
};

//
// BinaryNode
//
// a @ b
//
class BinaryNode : public OperatorNode {
public:
    
    BinaryNode(Symbol Op, NodeSeq&& Args);
};

//
// InfixNode
//
// a + b + c
//
class InfixNode : public OperatorNode {
public:
    
    InfixNode(Symbol Op, NodeSeq&& Args);
};

//
// TernaryNode
//
// a /: b = c
//
class TernaryNode : public OperatorNode {
public:
    
    TernaryNode(Symbol Op, NodeSeq&& Args);
};

//
// PostfixNode
//
// a!
//
class PostfixNode : public OperatorNode {
public:
    
    PostfixNode(Symbol Op, NodeSeq&& Args);
};

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
class PrefixBinaryNode : public OperatorNode {
public:
    
    PrefixBinaryNode(Symbol Op, NodeSeq&& Args);
};

//
// CallNode
//
// f[x]
//
class CallNode : public Node {
private:
    
    NodeSeq Head;
    const NodeVariant Body;
    Source Src;
    
public:
    
    CallNode(NodeSeq&& Head, NodeVariant&& Body);
    
    void release() override;
    
    Source getSource() const override;
    
    virtual bool syntaxQ() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
// GroupNode
//
// {x}
//
class GroupNode : public OperatorNode {
public:
    
    GroupNode(Symbol Op, NodeSeq&& Args);
};

//
// Any "compound" of tokens:
//
// a_
// _b
// a_.
// #a
// #abc
// ##2
// %2
//
class CompoundNode : public OperatorNode {
public:
    
    CompoundNode(Symbol Op, NodeSeq&& Args);
};

//
// SyntaxErrorNode
//
// A syntax error that contains structure.
//
class SyntaxErrorNode : public Node {
private:
    
    const Symbol Err;
    NodeSeq Children;
    Source Src;
    
public:
    
    SyntaxErrorNode(Symbol Err, NodeSeq&& Children);
    
    void release() override;
    
    Source getSource() const override;
    
    bool syntaxQ() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
// GroupMissingCloserNode
//
// {]
//
class GroupMissingCloserNode : public OperatorNode {
public:
    
    GroupMissingCloserNode(Symbol Op, NodeSeq&& Args);
    
    bool syntaxQ() const override;
};

//
// UnterminatedGroupNeedsReparseNode
//
// {
//
class UnterminatedGroupNeedsReparseNode : public OperatorNode {
public:
    
    UnterminatedGroupNeedsReparseNode(Symbol Op, NodeSeq&& Args);
    
    bool syntaxQ() const override;
};

//
//
//
class CollectedExpressionsNode : public Node {
private:
    
    NodeSeq Exprs;
    
public:
    
    CollectedExpressionsNode(NodeSeq&& Exprs);
    
    void release() override;
    
    Source getSource() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
    bool syntaxQ() const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class CollectedIssuesNode : public Node {
private:
    
    const IssuePtrVector Issues;
    
public:
    
    CollectedIssuesNode(IssuePtrVector Issues);
    
    void release() override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
    bool syntaxQ() const override;
    
    Source getSource() const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class CollectedSourceLocationsNode : public Node {
private:
    
    const std::set<SourceLocation> SourceLocs;
    
public:
    
    CollectedSourceLocationsNode(std::set<SourceLocation> SourceLocs);
    
    Source getSource() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
    bool syntaxQ() const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class MissingBecauseUnsafeCharacterEncodingNode : public Node {
private:
    
    const UnsafeCharacterEncodingFlag flag;
    
public:
    
    MissingBecauseUnsafeCharacterEncodingNode(UnsafeCharacterEncodingFlag flag);
    
    Source getSource() const override;
    
    bool syntaxQ() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class SafeStringNode : public Node {
private:
    
    const Buffer Buf;
    const size_t Len;
    
public:

    SafeStringNode(Buffer Buf, size_t Len);
    
    Source getSource() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
    bool syntaxQ() const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class NodeContainer : public Node {
private:
    
    NodeSeq Nodes;
    
public:
    
    NodeContainer(NodeSeq&& Nodes);
    
    void release() override;
    
    Source getSource() const override;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(ParserSessionPtr session, std::ostream& s) const override;
    
    bool syntaxQ() const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};
