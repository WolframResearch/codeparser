
#pragma once

#include "Source.h"

#include <string>
#include <vector>
#include <memory>

//
// An expression representing a node in the syntax tree
//
class Node {
    std::vector<std::shared_ptr<Node>> Children;
public:
    Node(std::vector<std::shared_ptr<Node>> Children) : Children(Children) {}

    std::vector<std::shared_ptr<Node>> getChildren() const {
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

class LiteralNode : public Node {
public:
    std::string Str;
    Source Span;
    
    LiteralNode(std::string Str, Source Span) : Node({}), Str(Str), Span(Span) {}
    
    Source getSourceSpan() const override {
        return Span;
    }
};

class SymbolNode : public LiteralNode {
public:
    SymbolNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class StringNode : public LiteralNode {
public:
    StringNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class IntegerNode : public LiteralNode {
public:
    IntegerNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class RealNode : public LiteralNode {
public:
    RealNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class SlotNode : public LiteralNode {
public:
    SlotNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class SlotSequenceNode : public LiteralNode {
public:
    SlotSequenceNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class OutNode : public LiteralNode {
public:
    OutNode(std::string Str, Source Span) : LiteralNode(Str, Span) {}
    
    void put(MLINK mlp) const override;
};

class OptionalDefaultNode : public LiteralNode {
public:
    OptionalDefaultNode(Source Span) : LiteralNode("_.", Span) {}
    
    void put(MLINK mlp) const override;
};

class TokenNode : public LiteralNode {
    Token Tok;
public:
    TokenNode(Token Tok) : LiteralNode(Tok.Str, Tok.Span), Tok(Tok) {}
    
    void put(MLINK mlp) const override;
};

//
// InternalNullNode is internal to InfixNode[CompoundExpression, ...]
//
class InternalNullNode : public LiteralNode {
public:
    InternalNullNode(Source Span) : LiteralNode("", Span) {}
    
    void put(MLINK mlp) const override;
};

//
// InternalOneNode is internal to BinaryNode[Span, ...]
//
class InternalOneNode : public LiteralNode {
public:
    InternalOneNode(Source Span) : LiteralNode("", Span) {}
    
    void put(MLINK mlp) const override;
};

//
// InternalAllNode is internal to BinaryNode[Span, ...]
//
class InternalAllNode : public LiteralNode {
public:
    InternalAllNode(Source Span) : LiteralNode("", Span) {}
    
    void put(MLINK mlp) const override;
};





//
// Base operator nodes
//

class PrefixNode : public Node {
    const Symbol* Op;
public:
    PrefixNode(const Symbol* Op, std::shared_ptr<Node> Rator, std::shared_ptr<Node> Rand) : Node({Rator, Rand}), Op(Op) {}
    
    PrefixNode(const PrefixNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    PrefixNode& operator=(const PrefixNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
              
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getOperand() {
        return getChildren()[1];
    }
};

class BinaryNode : public Node {
    const Symbol* Op;
public:
    BinaryNode(const Symbol* Op, std::shared_ptr<Node> Left, std::shared_ptr<Node> Rator, std::shared_ptr<Node> Right) : Node({Left, Rator, Right}), Op(Op) {}

    BinaryNode(const BinaryNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    BinaryNode& operator=(const BinaryNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getLeft() {
        return getChildren()[0];
    }
    
    std::shared_ptr<Node> getOperator() {
        return getChildren()[1];
    }
    
    std::shared_ptr<Node> getRight() {
        return getChildren()[2];
    }
    
    const Symbol* getSymbol() {
        return Op;
    }
};

class InfixNode : public Node {
    const Symbol* Op;
public:
    InfixNode(const Symbol* Op, std::vector<std::shared_ptr<Node>> Args) : Node(Args), Op(Op) {}
    
    InfixNode(const InfixNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    InfixNode& operator=(const InfixNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
};


class TernaryNode : public Node {
    const Symbol* Op;
public:
    TernaryNode(const Symbol* Op, std::shared_ptr<Node> Left, std::shared_ptr<Node> Rator0, std::shared_ptr<Node> Middle, std::shared_ptr<Node> Rator1, std::shared_ptr<Node> Right) : Node({Left, Rator0, Middle, Rator1, Right}), Op(Op) {}
    
    TernaryNode(const TernaryNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    TernaryNode& operator=(const TernaryNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getLeft() {
        return getChildren()[0];
    }
    
    std::shared_ptr<Node> getMiddle() {
        return getChildren()[2];
    }
    
    std::shared_ptr<Node> getRight() {
        return getChildren()[4];
    }
};

class PostfixNode : public Node {
    const Symbol* Op;
public:
    PostfixNode(const Symbol* Op, std::shared_ptr<Node> Operand, std::shared_ptr<Node> Rator) : Node({Operand, Rator}), Op(Op) {}
    
    PostfixNode(const PostfixNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    PostfixNode& operator=(const PostfixNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getOperand() {
        return getChildren()[0];
    }
    
    const Symbol* getOperator() {
        return Op;
    }
};

class PrefixBinaryNode : public Node {
    const Symbol* Op;
public:
    PrefixBinaryNode(const Symbol* Op, std::shared_ptr<Node> Rator, std::shared_ptr<Node> Rand1, std::shared_ptr<Node> Rand2) : Node({Rator, Rand1, Rand2}), Op(Op) {}
    
    PrefixBinaryNode(const PrefixBinaryNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    PrefixBinaryNode& operator=(const PrefixBinaryNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
};





//
// Call nodes
//

class CallNode : public Node {
    std::shared_ptr<Node> Head;
public:
    CallNode(std::shared_ptr<Node> Head, std::shared_ptr<Node> Body) : Node({Body}), Head(Head) {}
    
    void put(MLINK mlp) const override;

    Source getSourceSpan() const override;
};




//
// Group nodes
//

class GroupNode : public Node {
    const Symbol* Op;
public:
    GroupNode(const Symbol* Op, std::vector<std::shared_ptr<Node>> Args) : Node(Args), Op(Op) {}
    
    GroupNode(const GroupNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    GroupNode& operator=(const GroupNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;

    std::string internalInputform();
    
    const Symbol* getOperator() {
        return Op;
    }
};




//
// Special nodes
//


class BlankNode : public Node {
public:
    BlankNode(std::shared_ptr<Node> Operand, std::shared_ptr<Node> Sym2) : Node((Sym2 == nullptr) ? std::vector<std::shared_ptr<Node>>{ Operand } : std::vector<std::shared_ptr<Node>>{ Operand, Sym2 }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym2() {
        return getChildren()[0];
    }
};

class BlankSequenceNode : public Node {
public:
    BlankSequenceNode(std::shared_ptr<Node> Operand, std::shared_ptr<Node> Sym2) : Node((Sym2 == nullptr) ? std::vector<std::shared_ptr<Node>>{ Operand } : std::vector<std::shared_ptr<Node>>{ Operand, Sym2 }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym2() {
        return getChildren()[0];
    }
};

class BlankNullSequenceNode : public Node {
public:
    BlankNullSequenceNode(std::shared_ptr<Node> Operand, std::shared_ptr<Node> Sym2) : Node((Sym2 == nullptr) ? std::vector<std::shared_ptr<Node>>{ Operand } : std::vector<std::shared_ptr<Node>>{ Operand, Sym2 }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym2() {
        return getChildren()[0];
    }
};

class PatternBlankNode : public Node {
public:
    PatternBlankNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Operand, std::shared_ptr<Node> Sym2) : Node((Sym2 == nullptr) ? std::vector<std::shared_ptr<Node>>{ Sym1, Operand } : std::vector<std::shared_ptr<Node>>{ Sym1, Operand, Sym2 }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym1() {
        return getChildren()[0];
    }
    
    std::shared_ptr<Node> getSym2() {
        return getChildren()[1];
    }
};

class PatternBlankSequenceNode : public Node {
public:
    PatternBlankSequenceNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Operand, std::shared_ptr<Node> Sym2) : Node((Sym2 == nullptr) ? std::vector<std::shared_ptr<Node>>{ Sym1, Operand } : std::vector<std::shared_ptr<Node>>{ Sym1, Operand, Sym2 }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym1() {
        return getChildren()[0];
    }
    
    std::shared_ptr<Node> getSym2() {
        return getChildren()[1];
    }
};

class PatternBlankNullSequenceNode : public Node {
public:
    PatternBlankNullSequenceNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Operand, std::shared_ptr<Node> Sym2) : Node((Sym2 == nullptr) ? std::vector<std::shared_ptr<Node>>{ Sym1, Operand } : std::vector<std::shared_ptr<Node>>{ Sym1, Operand, Sym2 }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym1() {
        return getChildren()[0];
    }
    
    std::shared_ptr<Node> getSym2() {
        return getChildren()[1];
    }
};

//
// Operand should always be a OptionalDefaultNode
//
class OptionalDefaultPatternNode : public Node {
public:
    OptionalDefaultPatternNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Operand) : Node({ Sym1, Operand }) {}
    
    void put(MLINK mlp) const override;
    
    std::shared_ptr<Node> getSym1() {
        return getChildren()[0];
    }
};


//
// Error nodes
//


class SyntaxErrorNode : public Node {
    SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, std::vector<std::shared_ptr<Node>> Args) : Node(Args), Err(Err) {}
    
    void put(MLINK mlp) const override;
};

class GroupMissingCloserNode : public Node {
    const Symbol* Op;
public:
    GroupMissingCloserNode(const Symbol* Op, std::vector<std::shared_ptr<Node>> Args) : Node(Args), Op(Op) {}
    
    GroupMissingCloserNode(const GroupMissingCloserNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    GroupMissingCloserNode& operator=(const GroupMissingCloserNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
    
    std::string internalInputform();
    
    const Symbol* getOperator() {
        return Op;
    }
};

class GroupMissingOpenerNode : public Node {
    const Symbol* Op;
public:
    GroupMissingOpenerNode(const Symbol* Op, std::vector<std::shared_ptr<Node>> Args) : Node(Args), Op(Op) {}
    
    GroupMissingOpenerNode(const GroupMissingOpenerNode& N) : Node(N.getChildren()), Op(N.Op) {}
    
    GroupMissingOpenerNode& operator=(const GroupMissingOpenerNode& N) {
        if (this != &N) {
            Node::operator=(N);
            Op = N.Op;
        }
        return *this;
    }
    
    void put(MLINK mlp) const override;
    
    std::string internalInputform();
    
    const Symbol* getOperator() {
        return Op;
    }
};




//
// Aggregate nodes
//


class CollectedExpressionsNode : public Node {
    std::vector<std::shared_ptr<Node>> Exprs;
public:
    CollectedExpressionsNode(std::vector<std::shared_ptr<Node>> Exprs) : Node({}), Exprs(Exprs) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override {
        return Source();
    }
};


class CollectedCommentsNode : public Node {
    std::vector<Token> Comments;
public:
    CollectedCommentsNode(std::vector<Token> Comments) : Node({}), Comments(Comments) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override {
        return Source();
    }
};


class CollectedSyntaxIssuesNode : public Node {
    std::vector<SyntaxIssue> Issues;
public:
    CollectedSyntaxIssuesNode(std::vector<SyntaxIssue> Issues) : Node({}), Issues(Issues) {}
    
    void put(MLINK mlp) const override;
    
    Source getSourceSpan() const override {
        return Source();
    }
};




