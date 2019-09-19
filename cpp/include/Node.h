
#pragma once

#include "Source.h"

#include "mathlink.h"

#include <vector>
#include <memory>

class Symbol;

// MSVC: error C2338: The C++ Standard forbids containers of const elements because allocator<const T> is ill-formed.
using SymbolPtr = std::unique_ptr<Symbol>;

class Node;
class LeafNode;

using NodePtr = std::unique_ptr<Node>;
using LeafNodePtr = std::unique_ptr<LeafNode>;

class LeafSeq {
    
    std::unique_ptr<std::vector<LeafNodePtr>> vec;
    
public:
    
    LeafSeq() : vec(std::unique_ptr<std::vector<LeafNodePtr>>(new std::vector<LeafNodePtr>)) {}
    
    bool empty() const;
    
    size_t size() const;
    
    void reserve(size_t i);
    
    void append(LeafNodePtr );
    
    std::vector<LeafNodePtr> *getVectorDestructive() {
        return vec.release();
    }
};

//
// A sequence of Nodes
//
// When parsing  a(**)+b  we actually want to keep track of the comment.
// But the comment does not affect the parsing: a(**) is still 1 "thing" to the parser
//
// So pass around a structure that contains all of the nodes from the left, including comments and whitespace.
//
// However, we also know that there is a single, actual node that we care about. So remember with an index
// for fast access.
//
class NodeSeq {
    
    std::unique_ptr<std::vector<NodePtr>> vec;
    
    void append(std::unique_ptr<std::vector<NodePtr>> );
    
public:
    
    NodeSeq() : vec(std::unique_ptr<std::vector<NodePtr>>(new std::vector<NodePtr>)) {}
    
    bool empty() const;
    
    size_t size() const;
    
    void reserve(size_t i);
    
    void append(NodePtr );
    
    void append(std::unique_ptr<NodeSeq> );
    
    void append(std::unique_ptr<LeafSeq> );
    
    NodePtr& first() const;
    NodePtr& last() const;
    
    void put(MLINK ) const;
    
    const std::unique_ptr<std::vector<NodePtr>>& getVector() const {
        return vec;
    }
};

//
// An expression representing a node in the syntax tree
//
class Node {
    std::unique_ptr<NodeSeq> Children;
public:
    
    //
    // Tag + Children + 4 Source Ints
    //
    static const size_t NODE_LENGTH = 6;

    Node() : Children() {}
    Node(std::unique_ptr<NodeSeq> Children) : Children(std::move(Children)) {}

    virtual void put(MLINK mlp) const = 0;

    virtual Source getSourceSpan() const;

    void putChildren(MLINK mlp) const;

    const std::unique_ptr<NodeSeq>& getChildren() const {
        return Children;
    }
    
    NodeSeq *getChildrenDestructive() {
        return Children.release();
    }
    
    virtual const Token lastToken() const {
        auto& L = Children->last();
        return L->lastToken();
    }
    
    virtual ~Node() {}
};

class LeafNode;

//
// Literal nodes
//

class LeafNode : public Node {
    const Token Tok;
public:

    LeafNode(Token& Tok) : Node(), Tok(Tok) {}

    void put(MLINK mlp) const override;

    Source getSourceSpan() const override {
        return Tok.Span;
    }

    const Token getToken() const {
        return Tok;
    }
    
    const Token lastToken() const override {
        return Tok;
    }
};





//
// Base operator nodes
//

class PrefixNode : public Node {
    SymbolPtr& Op;
public:
    PrefixNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
};

class BinaryNode : public Node {
    SymbolPtr& Op;
public:
    BinaryNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getSymbol() const {
        return Op;
    }
};

class InfixNode : public Node {
    SymbolPtr& Op;
public:
    InfixNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
};


class TernaryNode : public Node {
    SymbolPtr& Op;
public:
    TernaryNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
};

class PostfixNode : public Node {
    SymbolPtr& Op;
public:
    PostfixNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};

class PrefixBinaryNode : public Node {
    SymbolPtr& Op;
public:
    PrefixBinaryNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
};





//
// Call nodes
//

class CallNode : public Node {
    std::unique_ptr<NodeSeq> Head;
public:
    CallNode(std::unique_ptr<NodeSeq> Head, std::unique_ptr<NodeSeq> Body) : Node(std::move(Body)), Head(std::move(Head)) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override;
};




//
// Group nodes
//

class GroupNode : public Node {
    SymbolPtr& Op;
public:
    GroupNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};




//
// Special nodes
//

class StartOfLineNode : public Node {
    SymbolPtr& Op;
public:
    StartOfLineNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
};


class BlankNode : public Node {
public:
    BlankNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};

class BlankSequenceNode : public Node {
public:
    BlankSequenceNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};

class BlankNullSequenceNode : public Node {
public:
    BlankNullSequenceNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankNode : public Node {
public:
    PatternBlankNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankSequenceNode : public Node {
public:
    PatternBlankSequenceNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankNullSequenceNode : public Node {
public:
    PatternBlankNullSequenceNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};

//
// Operand should always be a OptionalDefaultNode
//
class OptionalDefaultPatternNode : public Node {
public:
    OptionalDefaultPatternNode(std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)) {}
    
    void put(MLINK mlp) const override;
};


//
// Error nodes
//


class SyntaxErrorNode : public Node {
    const SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Err(Err) {}
    
    void put(MLINK mlp) const override;
};

class GroupMissingCloserNode : public Node {
    SymbolPtr& Op;
public:
    GroupMissingCloserNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};

class GroupMissingOpenerNode : public Node {
    SymbolPtr& Op;
public:
    GroupMissingOpenerNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};




//
// Collection nodes
//


class CollectedExpressionsNode : public Node {
    std::vector<NodePtr> Exprs;
public:
    CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Node(), Exprs(std::move(Exprs)) {}
    
    void put(MLINK mlp) const override;
};

class CollectedSyntaxIssuesNode : public Node {
    std::vector<SyntaxIssue> Issues;
public:
    CollectedSyntaxIssuesNode(std::vector<SyntaxIssue> Issues) : Node(), Issues(std::move(Issues)) {}
    
    void put(MLINK mlp) const override;
};

class CollectedMetadatasNode : public Node {
    std::vector<Metadata> Metadatas;
public:
    CollectedMetadatasNode(std::vector<Metadata> Metadatas) : Node(), Metadatas(std::move(Metadatas)) {}
    
    void put(MLINK mlp) const override;
};





