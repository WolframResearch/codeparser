
#include "Node.h"

#include "Symbol.h"

void NodeSeq::append(NodePtr N) {
    vec->push_back(std::move(N));
}

void NodeSeq::append(std::unique_ptr<NodeSeq> Args) {
    append(std::move(Args->vec));
}

void NodeSeq::append(std::unique_ptr<LeafSeq> Args) {
    auto V = Args->getVectorDestructive();
    for (auto& N : *V) {
        vec->push_back(std::move(N));
    }
    delete V;
}

void NodeSeq::append(std::unique_ptr<std::vector<NodePtr>> V) {
    for (auto& N : *V) {
        vec->push_back(std::move(N));
    }
}

bool NodeSeq::empty() const {
    return vec->empty();
}

size_t NodeSeq::size() const {
    return vec->size();
}

void NodeSeq::reserve(size_t i) {
    vec->reserve(i);
}

NodePtr& NodeSeq::first() const {
    return vec->at(0);
}

NodePtr& NodeSeq::last() const {
    return vec->at(vec->size()-1);
}

void NodeSeq::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(vec->size()));
    
    for (auto& C : *vec) {
        C->put(mlp);
    }
}

void LeafSeq::append(LeafNodePtr N) {
    vec->push_back(std::move(N));
}

bool LeafSeq::empty() const {
    return vec->empty();
}

size_t LeafSeq::size() const {
    return vec->size();
}


Source Node::getSource() const {
    
    assert(!Children->empty());
        
    const auto& First = Children->first();
    const auto& Last = Children->last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}

void Node::putChildren(MLINK mlp) const {
    
    Children->put(mlp);
}


void OperatorNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, MakeSym->name(), 2 + Src.count());
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    getSource().put(mlp);
}


void LeafNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), 2 + Tok.Src.count());
    
    MLPutSymbol(mlp, TokenToSymbol(Tok.Tok)->name());
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tok.Str.c_str()), static_cast<int>(Tok.Str.size()));
    
    Tok.Src.put(mlp);
}


void CallNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKECALLNODE->name(), 2 + Src.count());
    
    Head->put(mlp);
    
    putChildren(mlp);
    
    Src.put(mlp);
}

Source CallNode::getSource() const {
    
    const auto& First = Head->first();
    
    const auto& Children = getChildren();
    const auto& Last = Children->last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}


void SyntaxErrorNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKESYNTAXERRORNODE->name(), 2 + Src.count());
    
    MLPutSymbol(mlp, SyntaxErrorToString(Err).c_str());
    
    putChildren(mlp);
    
    Src.put(mlp);
}


void CollectedExpressionsNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Exprs.size()));
    
    for (auto& E : Exprs) {
        E->put(mlp);
    }
}

void CollectedSyntaxIssuesNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Issues.size()));
    
    for (auto I : Issues) {
        I.put(mlp);
    }
}

