
#pragma once

#include "Source.h"
#include "Symbol.h"
#include "Token.h"

#include "mathlink.h"

#include <vector>
#include <memory> // for unique_ptr

// MSVC: error C2338: The C++ Standard forbids containers of const elements because allocator<const T> is ill-formed.
using SymbolPtr = std::unique_ptr<Symbol>;

class Node;
class LeafNode;
class NodeSeqNode;

using NodePtr = std::unique_ptr<Node>;
using LeafNodePtr = std::unique_ptr<LeafNode>;
using NodeSeqNodePtr = std::unique_ptr<NodeSeqNode>;

class LeafSeq {
    std::vector<LeafNodePtr> vec;
public:
    bool moved;
    
    LeafSeq() : vec(), moved(false) {}
    
    LeafSeq(LeafSeq&& other) : vec(std::move(other.vec)), moved(false) {
        other.moved = true;
        
    }
    
    ~LeafSeq();
    
    bool empty() const;
    
    size_t size() const;
    
    const Node* first() const;
    const Node* last() const;
    
    void append(LeafNodePtr );
    
    std::vector<LeafNodePtr>& getVectorDestructive() {
        moved = true;
        return vec;
    }
    
    void put0(MLINK ) const;
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
    
    std::vector<NodePtr> vec;
    
public:
    
    NodeSeq() : vec() {}
    
    bool empty() const;
    
    size_t size() const;
    
    void reserve(size_t i);
    
    void append(NodePtr );
    
    void appendIfNonEmpty(LeafSeq );
    
    const Node* first() const;
    const Node* last() const;
    
    void put(MLINK ) const;
    
    void put0(MLINK ) const;
};

//
// An expression representing a node in the syntax tree
//
class Node {
protected:
    NodeSeq Children;
public:

    Node() : Children() {}
    Node(NodeSeq Children);

    virtual void put(MLINK mlp) const = 0;

    virtual Source getSource() const;
    
    virtual bool isTrivia() const;
    
    virtual bool isError() const;
    
    virtual size_t size() const;
    
    virtual const Node* first() const;
    virtual const Node* last() const;
    
    void putChildren(MLINK mlp) const;

    const NodeSeq& getChildrenSafe() const {
        return Children;
    }
    
    NodeSeq& getChildrenDestructive() {
        return Children;
    }
    
    virtual const Token lastToken() const {
        auto L = Children.last();
        return L->lastToken();
    }
    
    virtual ~Node() {}
};

class LeafSeqNode : public Node {
    LeafSeq Children;
public:
    LeafSeqNode(LeafSeq Children) : Children(std::move(Children)) {}
    
    size_t size() const override;
    
    const Node* first() const override;
    const Node* last() const override;
    
    void put(MLINK mlp) const override;
    
};

class NodeSeqNode : public Node {
public:
    NodeSeqNode(NodeSeq Children) : Node(std::move(Children)) {}
    
    size_t size() const override;
    
    const Node* first() const override;
    const Node* last() const override;
    
    void put(MLINK mlp) const override;
    
};

class OperatorNode : public Node {
    SymbolPtr& Op;
    SymbolPtr& MakeSym;
public:
    OperatorNode(SymbolPtr& Op, SymbolPtr& MakeSym, NodeSeq Args) : Node(std::move(Args)), Op(Op), MakeSym(MakeSym) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};


class LeafNode : public Node {
    const Token Tok;
public:

    LeafNode(Token& Tok) : Node(), Tok(Tok) {}

    LeafNode(Token&& Tok) : Node(), Tok(std::move(Tok)) {}
    
    void put(MLINK mlp) const override;
    
    bool isTrivia() const override;
    
    Source getSource() const override {
        return Tok.Src;
    }

    const Token getToken() const {
        return Tok;
    }
    
    const Token lastToken() const override {
        return Tok;
    }
};


class PrefixNode : public OperatorNode {
public:
    PrefixNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEPREFIXNODE, std::move(Args)) {}
};

class BinaryNode : public OperatorNode {
public:
    BinaryNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEBINARYNODE, std::move(Args)) {}
};

class InfixNode : public OperatorNode {
public:
    InfixNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEINFIXNODE, std::move(Args)) {}
};


class TernaryNode : public OperatorNode {
public:
    TernaryNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKETERNARYNODE, std::move(Args)) {}
};

class PostfixNode : public OperatorNode {
public:
    PostfixNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEPOSTFIXNODE, std::move(Args)) {}
};

class PrefixBinaryNode : public OperatorNode {
public:
    PrefixBinaryNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEPREFIXBINARYNODE, std::move(Args)) {}
};


class CallNode : public Node {
    NodeSeq Head;
public:
    CallNode(NodeSeq Head, NodeSeq Body) : Node(std::move(Body)), Head(std::move(Head)) {}
    
    void put(MLINK mlp) const override;
    
    Source getSource() const override;
};


class GroupNode : public OperatorNode {
public:
    GroupNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPNODE, std::move(Args)) {}
};


class StartOfLineNode : public OperatorNode {
public:
    StartOfLineNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKESTARTOFLINENODE, std::move(Args)) {}
};


class BlankNode : public OperatorNode {
public:
    BlankNode(NodeSeq Args) : OperatorNode(SYMBOL_BLANK, SYMBOL_AST_LIBRARY_MAKEBLANKNODE, std::move(Args)) {}
};

class BlankSequenceNode : public OperatorNode {
public:
    BlankSequenceNode(NodeSeq Args) : OperatorNode(SYMBOL_BLANKSEQUENCE, SYMBOL_AST_LIBRARY_MAKEBLANKSEQUENCENODE, std::move(Args)) {}
};

class BlankNullSequenceNode : public OperatorNode {
public:
    BlankNullSequenceNode(NodeSeq Args) : OperatorNode(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_AST_LIBRARY_MAKEBLANKNULLSEQUENCENODE, std::move(Args)) {}
};

class PatternBlankNode : public OperatorNode {
public:
    PatternBlankNode(NodeSeq Args) : OperatorNode(SYMBOL_AST_PATTERNBLANK, SYMBOL_AST_LIBRARY_MAKEPATTERNBLANKNODE, std::move(Args)) {}
};

class PatternBlankSequenceNode : public OperatorNode {
public:
    PatternBlankSequenceNode(NodeSeq Args) : OperatorNode(SYMBOL_AST_PATTERNBLANKSEQUENCE, SYMBOL_AST_LIBRARY_MAKEPATTERNBLANKSEQUENCENODE, std::move(Args)) {}
};

class PatternBlankNullSequenceNode : public OperatorNode {
public:
    PatternBlankNullSequenceNode(NodeSeq Args) : OperatorNode(SYMBOL_AST_PATTERNBLANKNULLSEQUENCE, SYMBOL_AST_LIBRARY_MAKEPATTERNBLANKNULLSEQUENCENODE, std::move(Args)) {}
};


class OptionalDefaultPatternNode : public OperatorNode {
public:
    OptionalDefaultPatternNode(NodeSeq Args) : OperatorNode(SYMBOL_AST_OPTIONALDEFAULTPATTERN, SYMBOL_AST_LIBRARY_MAKEOPTIONALDEFAULTPATTERNNODE, std::move(Args)) {}
};


class SyntaxErrorNode : public Node {
    const SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, NodeSeq Args) : Node(std::move(Args)), Err(Err) {}
    
    void put(MLINK mlp) const override;
    
    virtual bool isError() const override;
};

class GroupMissingCloserNode : public OperatorNode {
public:
    GroupMissingCloserNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPMISSINGCLOSERNODE, std::move(Args)) {}
};

class GroupMissingOpenerNode : public OperatorNode {
public:
    GroupMissingOpenerNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPMISSINGOPENERNODE, std::move(Args)) {}
};


class CollectedExpressionsNode : public Node {
    std::vector<NodePtr> Exprs;
public:
    CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Node(), Exprs(std::move(Exprs)) {}
    
    void put(MLINK mlp) const override;
};

class CollectedIssuesNode : public Node {
    std::vector<std::unique_ptr<Issue>> Issues;
public:
    CollectedIssuesNode(std::vector<std::unique_ptr<Issue>> Issues) : Node(), Issues(std::move(Issues)) {}
    
    void put(MLINK mlp) const override;
};

