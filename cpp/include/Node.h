
#pragma once

#include "Source.h"

#include <string>
#include <vector>
#include <memory>


class Node;

//
// Cannot be const std::shared_ptr<const Node>
// 
//
using NodePtr = std::shared_ptr<const Node>;

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
    
    void push_back(NodePtr );
    
    void push_back(NodeSeq );
    
    void push_back(std::vector<NodePtr> );
    
    NodePtr main() const;
    
    NodePtr last() const;
    
    const std::vector<NodePtr> getVector() const {
        return vector;
    }
    
    void clear();
};



//
// An expression representing a node in the syntax tree
//
class Node {
    const std::vector<NodePtr> Children;
public:
    
    Node(std::vector<NodePtr> Children) : Children(Children) {}

    const std::vector<NodePtr> getChildren() const {
        return Children;
    }

    virtual void put(MLINK mlp) const = 0;
    
    virtual Source getSourceSpan() const;

    void putChildren(MLINK mlp) const;
    
    virtual ~Node() {}
};




//
// Literal nodes
//

class LeafNode : public Node {
    const Token Tok;
public:
    LeafNode(Token Tok) : Node({}), Tok(Tok) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override {
        return Tok.Span;
    }
    
    Token getToken() const {
        return Tok;
    }
};





//
// Base operator nodes
//

class PrefixNode : public Node {
    const Symbol* Op;
public:
    PrefixNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    PrefixNode(const PrefixNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getOperand() const {
//        return getChildren()[1];
//    }
};

class BinaryNode : public Node {
    const Symbol* Op;
public:
    BinaryNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}

//    BinaryNode(const BinaryNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getLeft() const {
//        return getChildren()[0];
//    }
    
//    NodePtr getOperator() const {
//        return getChildren()[1];
//    }
    
//    NodePtr getRight() const {
//        return getChildren()[2];
//    }
    
    const Symbol* getSymbol() const {
        return Op;
    }
};

class InfixNode : public Node {
    const Symbol* Op;
public:
    InfixNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    InfixNode(const InfixNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
};


class TernaryNode : public Node {
    const Symbol* Op;
public:
    TernaryNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    TernaryNode(const TernaryNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getLeft() const {
//        return getChildren()[0];
//    }
    
//    NodePtr getMiddle() const {
//        return getChildren()[2];
//    }
    
//    NodePtr getRight() const {
//        return getChildren()[4];
//    }
};

class PostfixNode : public Node {
    const Symbol* Op;
public:
    PostfixNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    PostfixNode(const PostfixNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getOperand() const {
//        return getChildren()[0];
//    }
    
    const Symbol* getOperator() const {
        return Op;
    }
};

class PrefixBinaryNode : public Node {
    const Symbol* Op;
public:
    PrefixBinaryNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    PrefixBinaryNode(const PrefixBinaryNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
};





//
// Call nodes
//

class CallNode : public Node {
    const std::vector<NodePtr> Head;
public:
    CallNode(std::vector<NodePtr> Head, std::vector<NodePtr> Body) : Node(Body), Head(Head) {}
    
    void put(MLINK mlp) const override;

    Source getSourceSpan() const override;
};




//
// Group nodes
//

class GroupNode : public Node {
    const Symbol* Op;
public:
    GroupNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    GroupNode(const GroupNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
    const Symbol* getOperator() const {
        return Op;
    }
};




//
// Special nodes
//

class StartOfLineNode : public Node {
    const Symbol* Op;
public:
    StartOfLineNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
    void put(MLINK mlp) const override;
};


class BlankNode : public Node {
public:
    BlankNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class BlankSequenceNode : public Node {
public:
    BlankSequenceNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class BlankNullSequenceNode : public Node {
public:
    BlankNullSequenceNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
};

class PatternBlankNode : public Node {
public:
    PatternBlankNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getSym1() const {
//        return getChildren()[0];
//    }
};

class PatternBlankSequenceNode : public Node {
public:
    PatternBlankSequenceNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getSym1() const {
//        return getChildren()[0];
//    }
};

class PatternBlankNullSequenceNode : public Node {
public:
    PatternBlankNullSequenceNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getSym1() const {
//        return getChildren()[0];
//    }
};

//
// Operand should always be a OptionalDefaultNode
//
class OptionalDefaultPatternNode : public Node {
public:
    OptionalDefaultPatternNode(std::vector<NodePtr> Args) : Node(Args) {}
    
    void put(MLINK mlp) const override;
    
//    NodePtr getSym1() const {
//        return getChildren()[0];
//    }
};


//
// Error nodes
//


class SyntaxErrorNode : public Node {
    const SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, std::vector<NodePtr> Args) : Node(Args), Err(Err) {}
    
    void put(MLINK mlp) const override;
};

class GroupMissingCloserNode : public Node {
    const Symbol* Op;
public:
    GroupMissingCloserNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    GroupMissingCloserNode(const GroupMissingCloserNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
    const Symbol* getOperator() const {
        return Op;
    }
};

class GroupMissingOpenerNode : public Node {
    const Symbol* Op;
public:
    GroupMissingOpenerNode(const Symbol* Op, std::vector<NodePtr> Args) : Node(Args), Op(Op) {}
    
//    GroupMissingOpenerNode(const GroupMissingOpenerNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    void put(MLINK mlp) const override;
    
    const Symbol* getOperator() const {
        return Op;
    }
};




//
// Collection nodes
//


class CollectedExpressionsNode : public Node {
    std::vector<NodePtr> Exprs;
public:
    CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Node({}), Exprs(Exprs) {}
    
    void put(MLINK mlp) const override;
};

class CollectedSyntaxIssuesNode : public Node {
    std::vector<SyntaxIssue> Issues;
public:
    CollectedSyntaxIssuesNode(std::vector<SyntaxIssue> Issues) : Node({}), Issues(Issues) {}
    
    void put(MLINK mlp) const override;
};

class CollectedMetadatasNode : public Node {
    std::vector<Metadata> Metadatas;
public:
    CollectedMetadatasNode(std::vector<Metadata> Metadatas) : Node({}), Metadatas(Metadatas) {}
    
    void put(MLINK mlp) const override;
};




