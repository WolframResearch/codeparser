
#pragma once

#include "SyntaxIssue.h"
#include "SourceManager.h"

#include <string>
#include <vector>
#include <memory>

class Node;

class Node {
    std::vector<std::shared_ptr<Node>> Args;
    std::vector<SyntaxIssue> Issues;
    std::vector<Comment> Comments;
public:
    Node(std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Args(Args), Issues(Issues), Comments(Comments) {}

    std::vector<std::shared_ptr<Node>> getArgs() {
        return Args;
    }

    std::vector<SyntaxIssue> getIssues() {
        return Issues;
    }
    
    std::vector<Comment> getComments() {
        return Comments;
    }

    virtual void put(MLINK mlp) = 0;
    
    virtual SourceSpan getSourceSpan() = 0;

    void putASTArgs(MLINK mlp);

    void putSyntaxIssues(MLINK mlp);
    void putComments(MLINK mlp);
    
    virtual ~Node() {}
};




//
// Atom and Atom-like nodes
//

class SymbolNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    SymbolNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Str(Str), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class StringNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    
    StringNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Str(Str), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class NumberNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    
    NumberNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Str(Str), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class SlotNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    SlotNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Str(Str), Span(Span) {};
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};


class SlotSequenceNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    SlotSequenceNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Str(Str), Span(Span) {};
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class OutNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    OutNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Str(Str), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};




//
// Base operator expressions
//

class PrefixNode : public Node {
    const Symbol& Op;
    SourceSpan TokSpan;
public:
    PrefixNode(const Symbol& Op, SourceSpan TokSpan, std::shared_ptr<Node> Operand, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Operand}, Issues, Comments), Op(Op), TokSpan(TokSpan) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
    
    std::shared_ptr<Node> getOperand() {
        return getArgs()[0];
    }
};

class BinaryNode : public Node {
    const Symbol& Op;
public:
    BinaryNode(const Symbol& Op, std::shared_ptr<Node> Left, std::shared_ptr<Node> Right, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Left, Right}, Issues, Comments), Op(Op) {}

    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
    
    std::shared_ptr<Node> getLeft() {
        return getArgs()[0];
    }
    
    std::shared_ptr<Node> getRight() {
        return getArgs()[1];
    }
    
    const Symbol& getOp() {
        return Op;
    }
};

class InfixNode : public Node {
    const Symbol& Op;
public:
    InfixNode(const Symbol& Op, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node(Args, Issues, Comments), Op(Op) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
};


class TernaryNode : public Node {
    const Symbol& Op;
public:
    TernaryNode(const Symbol& Op, std::shared_ptr<Node> Left, std::shared_ptr<Node> Middle, std::shared_ptr<Node> Right, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Left, Middle, Right}, Issues, Comments), Op(Op) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
    
    std::shared_ptr<Node> getLeft() {
        return getArgs()[0];
    }
    
    std::shared_ptr<Node> getMiddle() {
        return getArgs()[1];
    }
    
    std::shared_ptr<Node> getRight() {
        return getArgs()[2];
    }
};

class PostfixNode : public Node {
    const Symbol& Op;
    SourceSpan TokSpan;
public:
    PostfixNode(const Symbol& Op, SourceSpan TokSpan, std::shared_ptr<Node> Operand, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Operand}, Issues, Comments), Op(Op), TokSpan(TokSpan) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
    
    std::shared_ptr<Node> getOperand() {
        return getArgs()[0];
    }
};




//
// Group expressions
//

class GroupNode;

class CallNode : public Node {
    std::shared_ptr<Node> Head;
public:
    CallNode(std::shared_ptr<Node> Head, std::shared_ptr<Node> Body, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Body}, Issues, Comments), Head(Head) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
};

class CallMissingCloserNode : public Node {
    std::shared_ptr<Node> Head;
public:
    CallMissingCloserNode(std::shared_ptr<Node> Head, std::shared_ptr<Node> Body, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Body}, Issues, Comments), Head(Head) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
};

class GroupNode : public Node {
    const Symbol& Op;
    SourceSpan OpenerTokSpan;
    SourceSpan CloserTokSpan;
public:
    
    GroupNode(const Symbol& Op, SourceSpan OpenerTokSpan, SourceSpan CloserTokSpan, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node(Args, Issues, Comments), Op(Op), OpenerTokSpan(OpenerTokSpan), CloserTokSpan(CloserTokSpan) {}
    
    void put(MLINK mlp) override;

    std::string internalInputform();
    
    SourceSpan getSourceSpan() override;
    
    const Symbol& getOp() {
        return Op;
    }
    
    SourceSpan getOpenerTokSpan() {
        return OpenerTokSpan;
    }
    
    SourceSpan getCloserTokSpan() {
        return CloserTokSpan;
    }
};




class BlankNode : public Node {
    SourceSpan Span;
public:
    BlankNode(SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Span(Span) {}
    BlankNode(std::shared_ptr<Node> Sym2, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym2}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym2() {
        return getArgs()[0];
    }
};

class BlankSequenceNode : public Node {
    SourceSpan Span;
public:
    BlankSequenceNode(SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Span(Span) {}
    BlankSequenceNode(std::shared_ptr<Node> Sym2, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym2}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym2() {
        return getArgs()[0];
    }
};

class BlankNullSequenceNode : public Node {
    SourceSpan Span;
public:
    BlankNullSequenceNode(SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Span(Span) {}
    BlankNullSequenceNode(std::shared_ptr<Node> Sym2, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym2}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym2() {
        return getArgs()[0];
    }
};

class OptionalDefaultNode : public Node {
    SourceSpan Span;
public:
    OptionalDefaultNode(SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class PatternBlankNode : public Node {
    SourceSpan Span;
public:
    PatternBlankNode(std::shared_ptr<Node> Sym1, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1}, Issues, Comments), Span(Span) {}
    PatternBlankNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Sym2, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1, Sym2}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym1() {
        return getArgs()[0];
    }
    
    std::shared_ptr<Node> getSym2() {
        return getArgs()[1];
    }
};

class PatternBlankSequenceNode : public Node {
    SourceSpan Span;
public:
    PatternBlankSequenceNode(std::shared_ptr<Node> Sym1, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1}, Issues, Comments), Span(Span) {}
    PatternBlankSequenceNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Sym2, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1, Sym2}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym1() {
        return getArgs()[0];
    }
    
    std::shared_ptr<Node> getSym2() {
        return getArgs()[1];
    }
};

class PatternBlankNullSequenceNode : public Node {
    SourceSpan Span;
public:
    PatternBlankNullSequenceNode(std::shared_ptr<Node> Sym1, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1}, Issues, Comments), Span(Span) {}
    PatternBlankNullSequenceNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Sym2, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1, Sym2}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym1() {
        return getArgs()[0];
    }
    
    std::shared_ptr<Node> getSym2() {
        return getArgs()[1];
    }
};

class OptionalDefaultPatternNode : public Node {
    SourceSpan Span;
public:
    OptionalDefaultPatternNode(std::shared_ptr<Node> Sym1, SourceSpan Span, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node({Sym1}, Issues, Comments), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym1() {
        return getArgs()[0];
    }
};




//
// InternalNullNode is internal to InfixNode[CompoundExpression, ...]
//
class InternalNullNode : public Node {
    SourceSpan Span;
public:
    InternalNullNode(SourceSpan Span) : Node({}, {}, {}), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

//
// InternalOneNode is internal to BinaryNode[Span, ...]
//
class InternalOneNode : public Node {
    SourceSpan Span;
public:
    InternalOneNode(SourceSpan Span) : Node({}, {}, {}), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

//
// InternalAllNode is internal to BinaryNode[Span, ...]
//
class InternalAllNode : public Node {
    SourceSpan Span;
public:
    InternalAllNode(SourceSpan Span) : Node({}, {}, {}), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

//
// InternalDotNode is internal to BinaryNode[Unset, ...]
//
class InternalDotNode : public Node {
    SourceSpan Span;
public:
    InternalDotNode(SourceSpan Span) : Node({}, {}, {}), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class TokenNode : public Node {
    Token Tok;
    std::string Str;
    SourceSpan Span;
public:
    TokenNode(Token Tok, std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues) : Node({}, Issues, {}), Tok(Tok), Str(Str), Span(Span) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override {
        return Span;
    }
};







class CommentNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    CommentNode(std::string Str, SourceSpan Span) : Node({}, {}, {}), Str(Str), Span(Span) {}
    
    void put(MLINK mlp) override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};



class SyntaxErrorNode : public Node {
    Token Tok;
public:
    SyntaxErrorNode(Token Tok, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues, std::vector<Comment> Comments) : Node(Args, Issues, Comments), Tok(Tok) {}
    
    void put(MLINK mlp) override;

    SourceSpan getSourceSpan() override;
};


