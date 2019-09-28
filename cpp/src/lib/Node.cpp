
#include "Node.h"

#include "Parser.h"
#include "Symbol.h"

void NodeSeq::append(NodePtr N) {
    vec.push_back(std::move(N));
}

void NodeSeq::append(NodeSeq Args) {
    append(std::move(Args.vec));
}

void NodeSeq::append(LeafSeq Args) {
    auto& V = Args.getVectorDestructive();
    for (auto& N : V) {
        vec.push_back(std::move(N));
    }
}

void NodeSeq::append(std::vector<NodePtr> V) {
    for (auto& N : V) {
        vec.push_back(std::move(N));
    }
}

bool NodeSeq::empty() const {
    return vec.empty();
}

size_t NodeSeq::size() const {
    return vec.size();
}

void NodeSeq::reserve(size_t i) {
    vec.reserve(i);
}

const NodePtr& NodeSeq::first() const {
    return vec.at(0);
}

const NodePtr& NodeSeq::last() const {
    return vec.at(vec.size()-1);
}

void NodeSeq::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(vec.size()));
    
    for (auto& C : vec) {
        C->put(mlp);
    }
}


LeafSeq::~LeafSeq() {
    
    if (!moved) {
        TheParser->prependInReverse(vec);
    }
}

void LeafSeq::append(LeafNodePtr N) {
    vec.push_back(std::move(N));
}

bool LeafSeq::empty() const {
    return vec.empty();
}

size_t LeafSeq::size() const {
    return vec.size();
}


Node::Node(NodeSeq ChildrenIn) : Children(std::move(ChildrenIn)) {
    //
    // These are very useful asserts to help find problems with trivia
    //
    assert(!Children.first()->isTrivia());
    assert(!Children.last()->isTrivia());
}

bool Node::isTrivia() const {
    return false;
}

bool Node::isError() const {
    return false;
}

Source Node::getSource() const {
    
    assert(!Children.empty());
        
    const auto& First = Children.first();
    const auto& Last = Children.last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}

void Node::putChildren(MLINK mlp) const {
    
    Children.put(mlp);
}


void OperatorNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, MakeSym->name(), static_cast<int>(2 + Src.count()));
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    getSource().put(mlp);
}


void LeafNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2 + Tok.Src.count()));
    
    MLPutSymbol(mlp, TokenToSymbol(Tok.Tok)->name());
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tok.Str.c_str()), static_cast<int>(Tok.Str.size()));
    
    Tok.Src.put(mlp);
}

bool LeafNode::isTrivia() const {
    return Tok.isTrivia();
}

void CallNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKECALLNODE->name(), static_cast<int>(2 + Src.count()));
    
    Head.put(mlp);
    
    putChildren(mlp);
    
    Src.put(mlp);
}

Source CallNode::getSource() const {
    
    const auto& First = Head.first();
    
    const auto& Children = getChildrenSafe();
    const auto& Last = Children.last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}


void SyntaxErrorNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKESYNTAXERRORNODE->name(), static_cast<int>(2 + Src.count()));
    
    MLPutSymbol(mlp, SyntaxErrorToString(Err).c_str());
    
    putChildren(mlp);
    
    Src.put(mlp);
}

bool SyntaxErrorNode::isError() const {
    return true;
}

void CollectedExpressionsNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Exprs.size()));
    
    for (auto& E : Exprs) {
        E->put(mlp);
    }
}

void CollectedSyntaxIssuesNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Issues.size()));
    
    for (auto& I : Issues) {
        I.put(mlp);
    }
}

