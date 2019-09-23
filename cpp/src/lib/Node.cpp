
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


Source Node::getSourceSpan() const {
    
    if (!Children->empty()) {
        
        const auto& First = Children->first();
        const auto& Last = Children->last();
        
        return Source(First->getSourceSpan().lines.start, Last->getSourceSpan().lines.end);
    }
    
    return Source();
}

void Node::putChildren(MLINK mlp) const {
    
    Children->put(mlp);
}


void OperatorNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, MakeSym->name(), NODE_LENGTH);
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    getSourceSpan().putLineCols(mlp);
}


void LeafNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), NODE_LENGTH);
    
    MLPutSymbol(mlp, TokenToSymbol(Tok.Tok)->name());
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tok.Str.c_str()), static_cast<int>(Tok.Str.size()));
    
    Tok.Span.putLineCols(mlp);
}


void CallNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKECALLNODE->name(), NODE_LENGTH);
    
    Head->put(mlp);
    
    putChildren(mlp);
    
    getSourceSpan().putLineCols(mlp);
}

Source CallNode::getSourceSpan() const {
    
    const auto& FirstHead = Head->first();
    
    const auto& Children = getChildren();
    const auto& LastChild = Children->last();
    
    return Source(FirstHead->getSourceSpan().lines.start, LastChild->getSourceSpan().lines.end);
}


void SyntaxErrorNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKESYNTAXERRORNODE->name(), NODE_LENGTH);
    
    MLPutSymbol(mlp, SyntaxErrorToString(Err).c_str());
    
    putChildren(mlp);
    
    getSourceSpan().putLineCols(mlp);
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

