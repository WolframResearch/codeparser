
#pragma once

#include "Source.h" // for Source
#include "Token.h" // for Token
#include "API.h" // for UnsafeCharacterEncodingFlag
#include "Precedence.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <vector>
#include <set>
#include <memory> // for unique_ptr
#include <ostream>
#include <cstddef> // for size_t

class Node;
class LeafNode;
class Parselet;

using NodePtr = std::unique_ptr<Node>;
using LeafNodePtr = std::unique_ptr<LeafNode>;
using ParseletPtr = Parselet *;
typedef void (*ParseFunction)(ParseletPtr, Token firstTok);

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
// Used mainly for collecting trivia that has been eaten
//
class TriviaSeq {
private:
    
    std::vector<LeafNodePtr> vec;
    
public:
    
    TriviaSeq();
    
    void reset();
    
    bool empty() const;
    
    void append(LeafNode *N);
    
    
    friend class Parser;
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
    
    std::vector<NodePtr> vec;
    
public:
    
    NodeSeq(size_t Size);
    
    bool empty() const;
    
    const NodePtr& first() const;
    
    const NodePtr& last() const;
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    bool check() const;
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
    
    
    friend class Parser;
};



//
// An expression representing a node in the syntax tree
//
class Node {
public:
    
    virtual ~Node();
    
    virtual void print(std::ostream& s) const = 0;

    virtual Source getSource() const = 0;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual bool check() const = 0;
    
#if USE_EXPR_LIB
    virtual expr toExpr() const = 0;
#endif // USE_EXPR_LIB
};

//
// Any kind of prefix, postfix, binary, or infix operator
//
class OperatorNode : public Node {
private:
    
    const Symbol Op;
    const Symbol MakeSym;
    const NodeSeq Children;
    Source Src;
    
public:
    
    OperatorNode(Symbol Op, Symbol MakeSym, NodeSeq Children);
    
    Source getSource() const override;
    
    bool check() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
    
    Symbol getOp() const;
};

//
// Leaf
//
// These are Symbols, Strings, Integers, Reals, Rationals.
//
class LeafNode : public Node {
private:
    
    const Token Tok;
    
public:

    LeafNode(Token Tok);
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override;
    
    Token getToken() const;
    
    bool check() const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
// These are syntax errors similar to LeafNode
//
class ErrorNode : public Node {
private:
    
    const Token Tok;
    
public:
    
    ErrorNode(Token Tok);
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Token getToken() const;
    
    Source getSource() const override;
    
    bool check() const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

class UnterminatedTokenErrorNeedsReparseNode : public Node {
private:
    
    const Token Tok;
    
public:
    
    UnterminatedTokenErrorNeedsReparseNode(Token Tok);
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override;
    
    bool check() const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

#if !NABORT
class AbortNode : public Node {
public:
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override;
    
    bool check() const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};
#endif // !NABORT

//
// PrefixNode
//
// -a
//
class PrefixNode : public OperatorNode {
public:
    
    PrefixNode(Symbol Op, NodeSeq Args);
};

//
// BinaryNode
//
// a @ b
//
class BinaryNode : public OperatorNode {
public:
    
    BinaryNode(Symbol Op, NodeSeq Args);
};

//
// InfixNode
//
// a + b + c
//
class InfixNode : public OperatorNode {
public:
    
    InfixNode(Symbol Op, NodeSeq Args);
};

//
// TernaryNode
//
// a /: b = c
//
class TernaryNode : public OperatorNode {
public:
    
    TernaryNode(Symbol Op, NodeSeq Args);
};

//
// PostfixNode
//
// a!
//
class PostfixNode : public OperatorNode {
public:
    
    PostfixNode(Symbol Op, NodeSeq Args);
};

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
class PrefixBinaryNode : public OperatorNode {
public:
    
    PrefixBinaryNode(Symbol Op, NodeSeq Args);
};

//
// CallNode
//
// f[x]
//
class CallNode : public Node {
private:
    
    const NodeSeq Head;
    const NodePtr Body;
    Source Src;
    
public:
    
    CallNode(NodeSeq Head, NodePtr Body);
    
    Source getSource() const override;
    
    virtual bool check() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
// GroupNode
//
// {x}
//
class GroupNode : public OperatorNode {
public:
    
    GroupNode(Symbol Op, NodeSeq Args);
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
    
    CompoundNode(Symbol Op, NodeSeq Args);
};

//
// SyntaxErrorNode
//
// A syntax error that contains structure.
//
class SyntaxErrorNode : public Node {
private:
    
    const Symbol Err;
    const NodeSeq Children;
    Source Src;
    
public:
    
    SyntaxErrorNode(Symbol Err, NodeSeq Children);
    
    Source getSource() const override;
    
    bool check() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
// GroupMissingCloserNode
//
// {]
//
class GroupMissingCloserNode : public OperatorNode {
public:
    
    GroupMissingCloserNode(Symbol Op, NodeSeq Args);
    
    bool check() const override;
};

//
// UnterminatedGroupNeedsReparseNode
//
// {
//
class UnterminatedGroupNeedsReparseNode : public OperatorNode {
public:
    
    UnterminatedGroupNeedsReparseNode(Symbol Op, NodeSeq Args);
    
    bool check() const override;
};

//
//
//
class CollectedExpressionsNode : public Node {
private:
    
    const std::vector<NodePtr> Exprs;
    
public:
    
    CollectedExpressionsNode(std::vector<NodePtr> Exprs);
    
    Source getSource() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
//
//
class CollectedIssuesNode : public Node {
private:
    
    const IssuePtrSet Issues;
    
public:
    
    CollectedIssuesNode(IssuePtrSet Issues);
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override;
    
    Source getSource() const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
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
    
    bool check() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
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
    
    bool check() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
//
//
class SafeStringNode : public Node {
private:
    
    const BufferAndLength bufAndLen;
    
public:

    SafeStringNode(BufferAndLength bufAndLen);
    
    Source getSource() const override;

    bool check() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;

#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};
