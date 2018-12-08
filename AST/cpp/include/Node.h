
#pragma once

#include "Utils.h"
#include "SyntaxIssue.h"

#include <string>
#include <vector>
#include <memory>

class Node;

class Node {
    std::vector<std::shared_ptr<Node>> Args;
public:
    Node(std::vector<std::shared_ptr<Node>> Args) : Args(Args) {}

    std::vector<std::shared_ptr<Node>> getArgs() {
        return Args;
    }

    virtual std::string string() = 0;
    
    virtual std::string inputform() = 0;
    
    virtual SourceSpan getSourceSpan() = 0;
    
    std::string ASTArgsString();
    
    virtual ~Node() {}
};




//
// Atom and Atom-like nodes
//

class SymbolNode : public Node {
    std::string Str;
    SourceSpan Span;
    std::vector<SyntaxIssue> Issues;
public:
    SymbolNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues) : Node({}), Str(Str), Span(Span), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class StringNode : public Node {
    std::string Str;
    SourceSpan Span;
    std::vector<SyntaxIssue> Issues;
public:
    
    StringNode(std::string Str, SourceSpan Span, std::vector<SyntaxIssue> Issues) : Node({}), Str(Str), Span(Span), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class NumberNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    
    NumberNode(std::string Str, SourceSpan Span) : Node({}), Str(Str), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class SyntaxErrorNode : public Node {
    Token Tok;
    std::vector<SyntaxIssue> Issues;
public:
    SyntaxErrorNode(Token Tok, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues) : Node(Args), Tok(Tok), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
};

//
// InternalEmptyNode is for representing the absence of an expression.
// e.g., ;; would be represented as empty;;empty
// e.g., a /: b =. is represented as a /: b =. empty
// this helps stay consistent about binary and ternary operators
//
class InternalEmptyNode : public Node {
    SourceSpan Span;
public:
    InternalEmptyNode(SourceSpan Span) : Node({}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class BlankNode : public Node {
    SourceSpan Span;
public:
    BlankNode(std::shared_ptr<Node> Sym2, SourceSpan Span) : Node({Sym2}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    BlankSequenceNode(std::shared_ptr<Node> Sym2, SourceSpan Span) : Node({Sym2}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    BlankNullSequenceNode(std::shared_ptr<Node> Sym2, SourceSpan Span) : Node({Sym2}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    OptionalDefaultNode(std::shared_ptr<Node> Sym1, SourceSpan Span) : Node({Sym1}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
    
    std::shared_ptr<Node> getSym1() {
        return getArgs()[0];
    }
};

class PatternBlankNode : public Node {
    SourceSpan Span;
public:
    PatternBlankNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Sym2, SourceSpan Span) : Node({Sym1, Sym2}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    PatternBlankSequenceNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Sym2, SourceSpan Span) : Node({Sym1, Sym2}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    PatternBlankNullSequenceNode(std::shared_ptr<Node> Sym1, std::shared_ptr<Node> Sym2, SourceSpan Span) : Node({Sym1, Sym2}), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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

class SlotNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    SlotNode(std::string Str, SourceSpan Span) : Node({}), Str(Str), Span(Span) {};
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};


class SlotSequenceNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    SlotSequenceNode(std::string Str, SourceSpan Span) : Node({}), Str(Str), Span(Span) {};
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};

class OutNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    OutNode(std::string Str, SourceSpan Span) : Node({}), Str(Str), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    PrefixNode(const Symbol& Op, SourceSpan TokSpan, std::shared_ptr<Node> Operand) : Node({Operand}), Op(Op), TokSpan(TokSpan) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
    
    std::shared_ptr<Node> getOperand() {
        return getArgs()[0];
    }
};

class BinaryNode : public Node {
    const Symbol& Op;
    std::vector<SyntaxIssue> Issues;
public:
    BinaryNode(const Symbol& Op, std::shared_ptr<Node> Left, std::shared_ptr<Node> Right, std::vector<SyntaxIssue> Issues) : Node({Left, Right}), Op(Op), Issues(Issues) {}

    std::string string() override;
    
    std::string inputform() override;
    
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
    std::vector<SyntaxIssue> Issues;
public:
    InfixNode(const Symbol& Op, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues) : Node(Args), Op(Op), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
};


class TernaryNode : public Node {
    const Symbol& Op;
    std::vector<SyntaxIssue> Issues;
public:
    TernaryNode(const Symbol& Op, std::shared_ptr<Node> Left, std::shared_ptr<Node> Middle, std::shared_ptr<Node> Right, std::vector<SyntaxIssue> Issues) : Node({Left, Middle, Right}), Op(Op), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    int DerivativeOrder;
    std::vector<SyntaxIssue> Issues;
public:
    PostfixNode(const Symbol& Op, SourceSpan TokSpan, int DerivativeOrder, std::shared_ptr<Node> Operand, std::vector<SyntaxIssue> Issues) : Node({Operand}), Op(Op), TokSpan(TokSpan), DerivativeOrder(DerivativeOrder), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
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
    SourceSpan OpenerTokSpan;
    SourceSpan CloserTokSpan;
    std::vector<SyntaxIssue> Issues;
public:
    CallNode(std::shared_ptr<Node> Head, SourceSpan OpenerTokSpan, SourceSpan CloserTokSpan, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues) : Node(Args), Head(Head), OpenerTokSpan(OpenerTokSpan), CloserTokSpan(CloserTokSpan), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
};

class CallMissingCloserNode : public Node {
    std::shared_ptr<Node> Head;
    SourceSpan OpenerTokSpan;
    std::vector<SyntaxIssue> Issues;
public:
    CallMissingCloserNode(std::shared_ptr<Node> Head, SourceSpan OpenerTokSpan, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues) : Node(Args), Head(Head), OpenerTokSpan(OpenerTokSpan), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
};

class PartNode : public Node {
    std::shared_ptr<Node> Head;
    SourceSpan OpenerTokSpan;
    SourceSpan CloserTokSpan;
    std::vector<SyntaxIssue> Issues;
public:
    PartNode(std::shared_ptr<Node> Head, SourceSpan OpenerTokSpan, SourceSpan CloserTokSpan, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues) : Node(Args), Head(Head), OpenerTokSpan(OpenerTokSpan), CloserTokSpan(CloserTokSpan), Issues(Issues) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
};

class GroupNode : public Node {
    const Symbol& Op;
    SourceSpan OpenerTokSpan;
    SourceSpan CloserTokSpan;
    std::vector<SyntaxIssue> Issues;
public:
    
    GroupNode(const Symbol& Op, SourceSpan OpenerTokSpan, SourceSpan CloserTokSpan, std::vector<std::shared_ptr<Node>> Args, std::vector<SyntaxIssue> Issues) : Node(Args), Op(Op), OpenerTokSpan(OpenerTokSpan), CloserTokSpan(CloserTokSpan), Issues(Issues) {}
    
    std::string string() override;
    
    // std::string internalString();
    
    std::string inputform() override;
    
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

    std::vector<SyntaxIssue> getIssues() {
        return Issues;
    }
};





//
// InternalMinusNode is internal to AdditionNode
//
class InternalMinusNode : public Node {
public:
    InternalMinusNode(std::shared_ptr<Node> Operand, SourceSpan Loc) : Node({Operand}) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
    
    std::shared_ptr<Node> getOperand() {
        return getArgs()[0];
    }
};

//
// InternalTokenNode is internal to LinearSyntax Paren GroupNodes
//
class InternalTokenNode : public Node {
    std::string Str;
    SourceSpan Span;
public:
    InternalTokenNode(std::string Str, SourceSpan Span) : Node({}), Str(Str), Span(Span) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override {
        return Span;
    }
};




class FileNode : public Node {
public:
    
    FileNode(std::vector<std::shared_ptr<Node>> Args) : Node(Args) {}
    
    std::string string() override;
    
    std::string inputform() override;
    
    SourceSpan getSourceSpan() override;
};



