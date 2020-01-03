
#pragma once

#include "Source.h" // for Source
#include "Symbol.h" // for SymbolPtr
#include "Token.h" // for Token

#include <vector>
#include <memory> // for unique_ptr
#include <ostream>

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
    
#if USE_MATHLINK
    void put0(MLINK ) const;
#endif // USE_MATHLINK
    
    void print0(std::ostream& s) const;
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
    
#if USE_MATHLINK
    void put(MLINK ) const;
    
    void put0(MLINK ) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s ) const;
    
    void print0(std::ostream& s ) const;
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
    
    virtual void print(std::ostream&) const = 0;

    virtual Source getSource() const;
    
    virtual bool isTrivia() const;
    
    virtual bool isError() const;
    
    virtual bool isEmpty() const;
    
    virtual size_t size() const;
    
    virtual const Node* first() const;
    virtual const Node* last() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
    
    void putChildren(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void printChildren(std::ostream& s) const;

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
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};

class NodeSeqNode : public Node {
public:
    NodeSeqNode(NodeSeq Children) : Node(std::move(Children)) {}
    
    size_t size() const override;
    
    const Node* first() const override;
    const Node* last() const override;
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};

class OperatorNode : public Node {
    SymbolPtr& Op;
    SymbolPtr& MakeSym;
public:
    OperatorNode(SymbolPtr& Op, SymbolPtr& MakeSym, NodeSeq Args) : Node(std::move(Args)), Op(Op), MakeSym(MakeSym) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
    
    SymbolPtr& getOperator() const {
        return Op;
    }
};


class LeafNode : public Node {
    const Token Tok;
public:

    LeafNode(Token& Tok) : Node(), Tok(Tok) {}

    LeafNode(Token&& Tok) : Node(), Tok(std::move(Tok)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
    
    bool isTrivia() const override;
    
    bool isEmpty() const override;
    
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
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
    
    Source getSource() const override;
};


class GroupNode : public OperatorNode {
public:
    GroupNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPNODE, std::move(Args)) {}
};


#if STARTOFLINE
class StartOfLineNode : public OperatorNode {
public:
    StartOfLineNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKESTARTOFLINENODE, std::move(Args)) {}
};

class StartOfFileNode : public OperatorNode {
public:
    StartOfFileNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKESTARTOFFILENODE, std::move(Args)) {}
};
#endif // STARTOFLINE


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
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
    
    virtual bool isError() const override;
};


class GroupMissingCloserNode : public OperatorNode {
public:
    GroupMissingCloserNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_AST_LIBRARY_MAKEGROUPMISSINGCLOSERNODE, std::move(Args)) {}
};


class CollectedExpressionsNode : public Node {
    std::vector<NodePtr> Exprs;
public:
    CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Node(), Exprs(std::move(Exprs)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};

class CollectedIssuesNode : public Node {
    std::vector<IssuePtr> Issues;
public:
    CollectedIssuesNode(std::vector<IssuePtr> Issues) : Node(), Issues(std::move(Issues)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};

class ListNode : public Node {
    std::vector<NodePtr> N;
public:
    ListNode(std::vector<NodePtr> N) : Node(), N(std::move(N)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};

class SourceCharacterNode : public Node {
    const SourceCharacter Char;
public:
    
    SourceCharacterNode(SourceCharacter& Char) : Node(), Char(Char) {}
    
    SourceCharacterNode(SourceCharacter&& Char) : Node(), Char(std::move(Char)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};

class SafeStringNode : public Node {
    std::vector<unsigned char> safeBytes;
public:
    
    SafeStringNode(std::vector<unsigned char>& safeBytes) : Node(), safeBytes(safeBytes) {}
    
    SafeStringNode(std::vector<unsigned char>&& safeBytes) : Node(), safeBytes(std::move(safeBytes)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const override;
};


