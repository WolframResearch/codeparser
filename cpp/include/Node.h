
#pragma once

#include "Source.h" // for Source
#include "Token.h" // for Token
#include "API.h" // for UnsafeCharacterEncodingFlag

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
class Symbol;
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
    
    TriviaSeq() : vec() {}
    
    TriviaSeq(TriviaSeq&& other) : vec(std::move(other.vec)) {}
    
    void reset();
    
    bool empty() const;
    
    size_t size() const;
    
    const Node *first() const;
    const Node *last() const;
    
    void append(LeafNodePtr N);
    
    
    friend class NodeSeq;
};


enum ColonLHS {
    COLONLHS_NONE,
    COLONLHS_PATTERN,
    COLONLHS_OPTIONAL,
    COLONLHS_ERROR
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
    
    ParseFunction F;
    
    ParseletPtr P;
    
    NodeSeq() : vec() {}
    
//    NodeSeq(size_t i) : vec() {
//        vec.reserve(i);
//    }
    
    NodeSeq(ParseFunction F, ParseletPtr P) : vec(), F(F), P(P) {}
    
    bool empty() const;
    
    size_t size() const;
    
    void append(NodePtr N);
    
    void appendSeq(NodeSeq Seq);
    
    void appendSeq(TriviaSeq Seq);
    
    const Node *first() const;
    const Node *last() const;
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    bool check() const;
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
    
    ColonLHS checkColonLHS() const;
    
    bool checkTilde() const;
};

//
// An expression representing a node in the syntax tree
//
class Node {
protected:
    
    const NodeSeq Children;
    
public:

    Node() : Children() {}
    Node(NodeSeq Children);
    
    virtual void print(std::ostream& s) const = 0;

    virtual Source getSource() const;
    
    virtual size_t size() const;
    
    virtual const Node *first() const;
    virtual const Node *last() const;
    
    virtual Token lastToken() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual bool check() const;
    
#if USE_EXPR_LIB
    virtual expr toExpr() const = 0;
#endif // USE_EXPR_LIB
    
    virtual ~Node() {}
};

//
// Any kind of prefix, postfix, binary, or infix operator
//
class OperatorNode : public Node {
private:
    
    const Symbol& Op;
    const Symbol& MakeSym;
    
public:
    
    OperatorNode(const Symbol& Op, const Symbol& MakeSym, NodeSeq Args) : Node(std::move(Args)), Op(Op), MakeSym(MakeSym) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
    
    const Symbol& getOp() const {
        return Op;
    }
};

//
// Leaf
//
// These are Symbols, Strings, Integers, Reals, Rationals.
//
class LeafNode : public Node {
protected:
    
    const Token Tok;
    
public:

    LeafNode(const Token& Tok) : Node(), Tok(Tok) {}

    LeafNode(const Token&& Tok) : Node(), Tok(std::move(Tok)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override {
        return Tok.Src;
    }

    const Token getToken() const {
        return Tok;
    }
    
    Token lastToken() const override {
        return Tok;
    }
    
    bool check() const override {
        return true;
    }
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
// Used for actual back-tracking that is sometimes needed
//
class ResettableLeafNode : public LeafNode {
private:
    
public:

    ResettableLeafNode(const Token& Tok) : LeafNode(Tok) {}
    
    ResettableLeafNode(ResettableLeafNode&& other) : LeafNode(std::move(other.Tok)) {}
    
    void reset();
};

//
// These are syntax errors similar to LeafNode
//
class ErrorNode : public Node {
private:
    
    const Token Tok;
    
public:
    
    ErrorNode(const Token& Tok) : Node(), Tok(Tok) {
        assert(Tok.Tok.isError());
        assert(!Tok.Tok.isUnterminated());
    }
    
    ErrorNode(const Token&& Tok) : Node(), Tok(std::move(Tok)) {
        assert(Tok.Tok.isError());
        assert(!Tok.Tok.isUnterminated());
    }
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    const Token getToken() const {
        return Tok;
    }
    
    Source getSource() const override {
        return Tok.Src;
    }
    
    Token lastToken() const override {
        return Tok;
    }
    
    bool check() const override {
        return false;
    }
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

class UnterminatedTokenErrorNeedsReparseNode : public Node {
private:
    
    const Token Tok;
    
public:
    
    UnterminatedTokenErrorNeedsReparseNode(const Token& Tok) : Node(), Tok(Tok) {
        assert(Tok.Tok.isUnterminated());
    }
    
    UnterminatedTokenErrorNeedsReparseNode(const Token&& Tok) : Node(), Tok(std::move(Tok)) {
        assert(Tok.Tok.isUnterminated());
    }
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override {
        return Tok.Src;
    }
    
    Token lastToken() const override {
        return Tok;
    }
    
    bool check() const override {
        return false;
    }
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
// PrefixNode
//
// -a
//
class PrefixNode : public OperatorNode {
public:
    
    PrefixNode(const Symbol& Op, NodeSeq Args);
};

//
// BinaryNode
//
// a @ b
//
class BinaryNode : public OperatorNode {
public:
    
    BinaryNode(const Symbol& Op, NodeSeq Args);
};

//
// InfixNode
//
// a + b + c
//
class InfixNode : public OperatorNode {
public:
    
    InfixNode(const Symbol& Op, NodeSeq Args);
};

//
// TernaryNode
//
// a /: b = c
//
class TernaryNode : public OperatorNode {
public:
    
    TernaryNode(const Symbol& Op, NodeSeq Args);
};

//
// TernaryNode
//
// a!
//
class PostfixNode : public OperatorNode {
public:
    
    PostfixNode(const Symbol& Op, NodeSeq Args);
};

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
class PrefixBinaryNode : public OperatorNode {
public:
    
    PrefixBinaryNode(const Symbol& Op, NodeSeq Args);
};

//
// CallNode
//
// f[x]
//
class CallNode : public Node {
private:
    
    const NodeSeq Head;
    
public:
    
    CallNode(NodeSeq Head, NodeSeq Body) : Node(std::move(Body)), Head(std::move(Head)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override;
    
    virtual bool check() const override;
    
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
    
    GroupNode(const Symbol& Op, NodeSeq Args);
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
    
    CompoundNode(const Symbol& Op, NodeSeq Args);
};

//
// SyntaxErrorNode
//
// A syntax error that contains structure.
//
class SyntaxErrorNode : public Node {
private:
    
    const Symbol& Err;
    
public:
    
    SyntaxErrorNode(const Symbol& Err, NodeSeq Args) : Node(std::move(Args)), Err(Err) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override {
        return false;
    }
    
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
    
    GroupMissingCloserNode(const Symbol& Op, NodeSeq Args);
    
    bool check() const override {
        return false;
    }
};

//
// UnterminatedGroupNeedsReparseNode
//
// {
//
class UnterminatedGroupNeedsReparseNode : public OperatorNode {
public:
    
    UnterminatedGroupNeedsReparseNode(const Symbol& Op, NodeSeq Args);
    
    bool check() const override {
        return false;
    }
};

//
//
//
class CollectedExpressionsNode : public Node {
private:
    
    const std::vector<NodePtr> Exprs;
    
public:
    
    CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Node(), Exprs(std::move(Exprs)) {}
    
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
    
    CollectedIssuesNode(IssuePtrSet Issues) : Node(), Issues(std::move(Issues)) {}
    
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
class CollectedSourceLocationsNode : public Node {
private:
    
    const std::set<SourceLocation> SourceLocs;
    
public:
    
    CollectedSourceLocationsNode(std::set<SourceLocation> SourceLocs) : Node(), SourceLocs(std::move(SourceLocs)) {}
    
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
    
    MissingBecauseUnsafeCharacterEncodingNode(UnsafeCharacterEncodingFlag flag) : Node(), flag(flag) {}
    
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

    SafeStringNode(BufferAndLength bufAndLen) : Node(), bufAndLen(bufAndLen) {}

#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;

#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};
