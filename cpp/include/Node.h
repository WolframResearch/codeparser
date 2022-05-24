
#pragma once

#include "Source.h" // for Source
#include "Symbol.h" // for SymbolPtr
#include "Token.h" // for Token

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

using NodePtr = std::unique_ptr<Node>;
using LeafNodePtr = std::unique_ptr<LeafNode>;


enum UnsafeCharacterEncodingFlag {
    UNSAFECHARACTERENCODING_OK = 0,
    UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE = 1,
    UNSAFECHARACTERENCODING_STRAYSURROGATE = 2,
    UNSAFECHARACTERENCODING_BOM = 3,
};


//
// Used mainly for collecting trivia that has been eaten
//
class LeafSeq {
public:
    
    std::vector<LeafNodePtr> vec;
    
    bool moved;
    
    LeafSeq() : vec(), moved(false) {}
    
    LeafSeq(LeafSeq&& other) : vec(std::move(other.vec)), moved(false) {
        other.moved = true;
    }
    
    ~LeafSeq();
    
    bool empty() const;
    
    size_t size() const;
    
    const Node *first() const;
    const Node *last() const;
    
    void append(LeafNodePtr N);
    
#if USE_MATHLINK
    void put0(MLINK mlp) const;
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
    NodeSeq(size_t i) : vec() {
        vec.reserve(i);
    }
    
    bool empty() const;
    
    size_t size() const;
    
    void append(NodePtr N);
    
    void appendIfNonEmpty(LeafSeq );
    
    const Node *first() const;
    const Node *last() const;
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
    
    void put0(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    void print0(std::ostream& s) const;
    
    bool check() const;
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
    
    virtual void print(std::ostream& s) const = 0;

    virtual Source getSource() const;
    
    virtual size_t size() const;
    
    virtual const Node *first() const;
    virtual const Node *last() const;
    
    virtual Token lastToken() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
    
    void putChildren(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void printChildren(std::ostream& s) const;
    
    virtual bool isExpectedOperandError() const {
        return false;
    }
    
    virtual bool check() const;
    
    virtual ~Node() {}
};

//
// Any kind of prefix, postfix, binary, or infix operator
//
class OperatorNode : public Node {
    SymbolPtr& Op;
    SymbolPtr& MakeSym;
public:
    OperatorNode(SymbolPtr& Op, SymbolPtr& MakeSym, NodeSeq Args) : Node(std::move(Args)), Op(Op), MakeSym(MakeSym) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

//
// Leaf
//
// These are Symbols, Strings, Integers, Reals, Rationals.
//
class LeafNode : public Node {
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
};

//
// These are syntax errors similar to LeafNode
//
class ErrorNode : public Node {
protected:
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
    
    bool isExpectedOperandError() const override {
        return Tok.Tok == TOKEN_ERROR_EXPECTEDOPERAND;
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
};

class UnterminatedTokenErrorNeedsReparseNode : public Node {
protected:
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
};

//
// PrefixNode
//
// -a
//
class PrefixNode : public OperatorNode {
public:
    PrefixNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEPREFIXNODE, std::move(Args)) {}
};

//
// BinaryNode
//
// a @ b
//
class BinaryNode : public OperatorNode {
public:
    BinaryNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEBINARYNODE, std::move(Args)) {}
};

//
// InfixNode
//
// a + b + c
//
class InfixNode : public OperatorNode {
public:
    InfixNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEINFIXNODE, std::move(Args)) {}
};

//
// TernaryNode
//
// a /: b = c
//
class TernaryNode : public OperatorNode {
public:
    TernaryNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKETERNARYNODE, std::move(Args)) {}
};

//
// TernaryNode
//
// a!
//
class PostfixNode : public OperatorNode {
public:
    PostfixNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEPOSTFIXNODE, std::move(Args)) {}
};

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
class PrefixBinaryNode : public OperatorNode {
public:
    PrefixBinaryNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEPREFIXBINARYNODE, std::move(Args)) {}
};

//
// CallNode
//
// f[x]
//
class CallNode : public Node {
    NodeSeq Head;
public:
    CallNode(NodeSeq Head, NodeSeq Body) : Node(std::move(Body)), Head(std::move(Head)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    Source getSource() const override;
    
    virtual bool check() const override;
};

//
// GroupNode
//
// {x}
//
class GroupNode : public OperatorNode {
public:
    GroupNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEGROUPNODE, std::move(Args)) {}
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
    CompoundNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKECOMPOUNDNODE, std::move(Args)) {}
};

//
// SyntaxErrorNode
//
// A syntax error that contains structure.
//
class SyntaxErrorNode : public Node {
    const SyntaxError Err;
public:
    SyntaxErrorNode(SyntaxError Err, NodeSeq Args) : Node(std::move(Args)), Err(Err) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override {
        return false;
    }
};

//
// GroupMissingCloserNode
//
// {]
//
class GroupMissingCloserNode : public OperatorNode {
public:
    GroupMissingCloserNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEGROUPMISSINGCLOSERNODE, std::move(Args)) {}
    
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
    UnterminatedGroupNeedsReparseNode(SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_LIBRARY_MAKEUNTERMINATEDGROUPNEEDSREPARSENODE, std::move(Args)) {}
    
    bool check() const override {
        return false;
    }
};

//
//
//
class CollectedExpressionsNode : public Node {
    std::vector<NodePtr> Exprs;
public:
    CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Node(), Exprs(std::move(Exprs)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override;
};

//
//
//
class CollectedIssuesNode : public Node {
    IssuePtrSet Issues;
public:
    CollectedIssuesNode(IssuePtrSet Issues) : Node(), Issues(std::move(Issues)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override;
};

//
//
//
class CollectedSourceLocationsNode : public Node {
    std::set<SourceLocation> SourceLocs;
public:
    CollectedSourceLocationsNode(std::set<SourceLocation> SourceLocs) : Node(), SourceLocs(std::move(SourceLocs)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;

};

//
//
//
class MissingBecauseUnsafeCharacterEncodingNode : public Node {
    UnsafeCharacterEncodingFlag flag;
public:
    MissingBecauseUnsafeCharacterEncodingNode(UnsafeCharacterEncodingFlag flag) : Node(), flag(flag) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

//
//
//
class SafeStringNode : public Node {
    BufferAndLength bufAndLen;
public:
    
    SafeStringNode(BufferAndLength bufAndLen) : Node(), bufAndLen(bufAndLen) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};
