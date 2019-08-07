
#pragma once

#include "Source.h"

#include "mathlink.h"

#include <vector>
#include <memory>

class Symbol;

using SymbolPtr = std::unique_ptr<const Symbol>;

class Node;

using NodePtr = const std::shared_ptr<const Node>;

//
// An expression representing a node in the syntax tree
//
class Node {
    const std::vector<NodePtr> Children;
public:
    
    //
    // Tag + Children + 4 Source Ints
    //
    static const size_t NODE_LENGTH = 6;
    
    Node(const std::vector<NodePtr>& Children) : Children(Children) {}
    
    const std::vector<NodePtr> getChildren() const {
        return Children;
    }
    
    virtual void put(MLINK mlp) const = 0;
    
    virtual Source getSourceSpan() const;
    
    void putChildren(MLINK mlp) const;
    
    virtual ~Node() {}
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
    
    std::vector<NodePtr> vector;
    
public:
    
    NodeSeq() : vector() {}
    
    bool empty() const;
    
    size_t size() const;
    
    void reserve(size_t i);
    
    void append(NodePtr& );
    
    void append(NodeSeq& );
    
    void append(std::vector<NodePtr>& );
    
    NodePtr main() const;
    
    NodePtr last() const;
    
    const std::vector<NodePtr> getVector() const {
        return vector;
    }
    
    void clear();
};




//
// Literal nodes
//

class LeafNode : public Node {
    const Token Tok;
public:
    
    LeafNode(Token& Tok) : Node({}), Tok(Tok) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override {
        return Tok.Span;
    }
    
    const Token getToken() const {
        return Tok;
    }
};





//
// Base operator nodes
//

class PrefixNode : public Node {
    SymbolPtr& Op;
public:
    PrefixNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
};

class BinaryNode : public Node {
    SymbolPtr& Op;
public:
    BinaryNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getSymbol() const {
        return Op;
    }
};

class InfixNode : public Node {
    SymbolPtr& Op;
public:
    InfixNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
};


class TernaryNode : public Node {
    SymbolPtr& Op;
public:
    TernaryNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
};

class PostfixNode : public Node {
    SymbolPtr& Op;
public:
    PostfixNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};

class PrefixBinaryNode : public Node {
    SymbolPtr& Op;
public:
    PrefixBinaryNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
};





//
// Call nodes
//

class CallNode : public Node {
    const std::vector<NodePtr> Head;
public:
    CallNode(std::vector<NodePtr> Head, const std::vector<NodePtr>& Body) : Node(Body), Head(Head) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override;
};




//
// Group nodes
//

class GroupNode : public Node {
    SymbolPtr& Op;
public:
    GroupNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
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
    StartOfLineNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
};


class BlankNode : public Node {
public:
    BlankNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class BlankSequenceNode : public Node {
public:
    BlankSequenceNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class BlankNullSequenceNode : public Node {
public:
    BlankNullSequenceNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankNode : public Node {
public:
    PatternBlankNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankSequenceNode : public Node {
public:
    PatternBlankSequenceNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankNullSequenceNode : public Node {
public:
    PatternBlankNullSequenceNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

//
// Operand should always be a OptionalDefaultNode
//
class OptionalDefaultPatternNode : public Node {
public:
    OptionalDefaultPatternNode(const std::vector<NodePtr>& Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};


//
// Error nodes
//


class SyntaxErrorNode : public Node {
    const SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, const std::vector<NodePtr>& Args) : Node(Args), Err(Err) {}
    
    void put(MLINK mlp) const override;
};

class GroupMissingCloserNode : public Node {
    SymbolPtr& Op;
public:
    GroupMissingCloserNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};

class GroupMissingOpenerNode : public Node {
    SymbolPtr& Op;
public:
    GroupMissingOpenerNode(SymbolPtr& Op, const std::vector<NodePtr>& Args) : Node(Args), Op(Op) {}
    
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
    CollectedExpressionsNode(const std::vector<NodePtr>& Exprs) : Node({}), Exprs(Exprs) {}
    
    void put(MLINK mlp) const override;
};

class CollectedSyntaxIssuesNode : public Node {
    std::vector<SyntaxIssue> Issues;
public:
    CollectedSyntaxIssuesNode(const std::vector<SyntaxIssue>& Issues) : Node({}), Issues(Issues) {}
    
    void put(MLINK mlp) const override;
};

class CollectedMetadatasNode : public Node {
    std::vector<Metadata> Metadatas;
public:
    CollectedMetadatasNode(const std::vector<Metadata>& Metadatas) : Node({}), Metadatas(Metadatas) {}
    
    void put(MLINK mlp) const override;
};




