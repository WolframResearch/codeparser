
#pragma once

#include "Source.h"
#include "Symbol.h"

#include "mathlink.h"

#include <vector>
#include <memory>

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


class OperatorNode : public Node {
    SymbolPtr& Op;
    SymbolPtr& MakeSym;
public:
    OperatorNode(SymbolPtr& Op, SymbolPtr& MakeSym, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Op(Op), MakeSym(MakeSym) {}
    
    void put(MLINK mlp) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};


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


class PrefixNode : public OperatorNode {
public:
    PrefixNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEPREFIXNODE, std::move(Args)) {}
};

class BinaryNode : public OperatorNode {
public:
    BinaryNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEBINARYNODE, std::move(Args)) {}
};

class InfixNode : public OperatorNode {
public:
    InfixNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEINFIXNODE, std::move(Args)) {}
};


class TernaryNode : public OperatorNode {
public:
    TernaryNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKETERNARYNODE, std::move(Args)) {}
};

class PostfixNode : public OperatorNode {
public:
    PostfixNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEPOSTFIXNODE, std::move(Args)) {}
};

class PrefixBinaryNode : public OperatorNode {
public:
    PrefixBinaryNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEPREFIXBINARYNODE, std::move(Args)) {}
};


class CallNode : public Node {
    std::unique_ptr<NodeSeq> Head;
public:
    CallNode(std::unique_ptr<NodeSeq> Head, std::unique_ptr<NodeSeq> Body) : Node(std::move(Body)), Head(std::move(Head)) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override;
};


class GroupNode : public OperatorNode {
public:
    GroupNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPNODE, std::move(Args)) {}
};


class StartOfLineNode : public OperatorNode {
public:
    StartOfLineNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKESTARTOFLINENODE, std::move(Args)) {}
};


class BlankNode : public OperatorNode {
public:
    BlankNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_BLANK, SYMBOL_AST_LIBRARY_MAKEBLANKNODE, std::move(Args)) {}
};

class BlankSequenceNode : public OperatorNode {
public:
    BlankSequenceNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_BLANKSEQUENCE, SYMBOL_AST_LIBRARY_MAKEBLANKSEQUENCENODE, std::move(Args)) {}
};

class BlankNullSequenceNode : public OperatorNode {
public:
    BlankNullSequenceNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_BLANKNULLSEQUENCE, SYMBOL_AST_LIBRARY_MAKEBLANKNULLSEQUENCENODE, std::move(Args)) {}
};

class PatternBlankNode : public OperatorNode {
public:
    PatternBlankNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_AST_PATTERNBLANK, SYMBOL_AST_LIBRARY_MAKEPATTERNBLANKNODE, std::move(Args)) {}
};

class PatternBlankSequenceNode : public OperatorNode {
public:
    PatternBlankSequenceNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_AST_PATTERNBLANKSEQUENCE, SYMBOL_AST_LIBRARY_MAKEPATTERNBLANKSEQUENCENODE, std::move(Args)) {}
};

class PatternBlankNullSequenceNode : public OperatorNode {
public:
    PatternBlankNullSequenceNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_AST_PATTERNBLANKNULLSEQUENCE, SYMBOL_AST_LIBRARY_MAKEPATTERNBLANKNULLSEQUENCENODE, std::move(Args)) {}
};


class OptionalDefaultPatternNode : public OperatorNode {
public:
    OptionalDefaultPatternNode(std::unique_ptr<NodeSeq> Args) : OperatorNode(SYMBOL_AST_OPTIONALDEFAULTPATTERN, SYMBOL_AST_LIBRARY_MAKEOPTIONALDEFAULTPATTERNNODE, std::move(Args)) {}
};


class SyntaxErrorNode : public Node {
    const SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, std::unique_ptr<NodeSeq> Args) : Node(std::move(Args)), Err(Err) {}
    
    void put(MLINK mlp) const override;
};

class GroupMissingCloserNode : public OperatorNode {
public:
    GroupMissingCloserNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPMISSINGCLOSERNODE, std::move(Args)) {}
};

class GroupMissingOpenerNode : public OperatorNode {
public:
    GroupMissingOpenerNode(SymbolPtr& Op, std::unique_ptr<NodeSeq> Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPMISSINGOPENERNODE, std::move(Args)) {}
};


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

