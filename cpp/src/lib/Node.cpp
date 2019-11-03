
#include "Node.h"

#include "Parser.h"
#include "Symbol.h"

#include <numeric> // for accumulate

void NodeSeq::append(NodePtr N) {
    vec.push_back(std::move(N));
}

void NodeSeq::appendIfNonEmpty(LeafSeq L) {
    if (!L.empty()) {
        append(NodePtr(new LeafSeqNode(std::move(L))));
    }
}

bool NodeSeq::empty() const {
    return vec.empty();
}

size_t NodeSeq::size() const {
    
    auto accum = std::accumulate(vec.begin(), vec.end(), static_cast<size_t>(0), [](size_t a, const NodePtr& b){ return a + b->size(); });
    
    return accum;
}

void NodeSeq::reserve(size_t i) {
    vec.reserve(i);
}

const Node* NodeSeq::first() const {
    
    auto F = vec.at(0).get();
    
    auto FF = F->first();
    
    return FF;
}

const Node* NodeSeq::last() const {
    auto L = vec.at(vec.size()-1).get();
    
    auto LL = L->last();
    
    return LL;
}

void NodeSeq::put(MLINK mlp) const {
    
    auto s = size();
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(s));
    
    put0(mlp);
}

void NodeSeq::print(std::ostream& s) const {
    
    s << SYMBOL_LIST->name() << "[";
    
    print0(s);
    
    s << "]";
}

void NodeSeq::put0(MLINK mlp) const {
    
    for (auto& C : vec) {
        C->put(mlp);
    }
}

void NodeSeq::print0(std::ostream& s) const {
    
    for (auto& C : vec) {
        C->print(s);
        s << ", ";
    }
}

void LeafSeq::put0(MLINK mlp) const {
    
    for (auto& C : vec) {
        C->put(mlp);
    }
}

void LeafSeq::print0(std::ostream& s) const {
    
    for (auto& C : vec) {
        C->print(s);
        s << ", ";
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
    
    auto accum = std::accumulate(vec.begin(), vec.end(), static_cast<size_t>(0), [](size_t a, const LeafNodePtr& b){ return a + b->size(); });
    
    return accum;
}

const Node* LeafSeq::first() const {
    
    auto F = vec.at(0).get();
    
    auto FF = F->first();
    
    return FF;
}

const Node* LeafSeq::last() const {
    auto L = vec.at(vec.size()-1).get();
    
    auto LL = L->last();
    
    return LL;
}


Node::Node(NodeSeq ChildrenIn) : Children(std::move(ChildrenIn)) {
#ifndef NDEBUG
    //
    // These are very useful asserts to help find problems with trivia
    //
    
    //
    // There may be trivia after the Node that we care about, so we cannot test the last
    // But we can test the first
    //

    auto F = Children.first();
//    auto L = Children.last();
    
    assert(!F->isTrivia());
//    assert(!L->isTrivia());
#endif
}

bool Node::isTrivia() const {
    return false;
}

bool Node::isError() const {
    return false;
}

Source Node::getSource() const {
    
    assert(!Children.empty());
    
    auto First = Children.first();
    auto Last = Children.last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}

size_t Node::size() const {
    return 1;
}

const Node* Node::first() const {
    return this;
}

const Node* Node::last() const {
    return this;
}

void Node::putChildren(MLINK mlp) const {
    
    Children.put(mlp);
}

void Node::printChildren(std::ostream& s) const {
    
    Children.print(s);
}

void LeafSeqNode::put(MLINK mlp) const {
    
    Children.put0(mlp);
}

void LeafSeqNode::print(std::ostream& s) const {
    
    Children.print0(s);
}

size_t LeafSeqNode::size() const {
    return Children.size();
}

const Node* LeafSeqNode::first() const {
    assert(!Children.empty());
    return Children.first();
}

const Node* LeafSeqNode::last() const {
    assert(!Children.empty());
    return Children.last();
}


size_t NodeSeqNode::size() const {
    return Children.size();
}

const Node* NodeSeqNode::first() const {
    assert(!Children.empty());
    return Children.first();
}

const Node* NodeSeqNode::last() const {
    assert(!Children.empty());
    return Children.last();
}

void NodeSeqNode::put(MLINK mlp) const {
    
    Children.put0(mlp);
}

void NodeSeqNode::print(std::ostream& s) const {
    
    Children.print0(s);
}

void OperatorNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    MLPutFunction(mlp, MakeSym->name(), static_cast<int>(2 + Src.count()));
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    getSource().put(mlp);
}

void OperatorNode::print(std::ostream& s) const {
    
    s << MakeSym->name() << "[";

    s << Op->name();
    s << ", ";
    
    printChildren(s);
    s << ", ";
    
    getSource().print(s);
    
    s << "]";
}

void LeafNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2 + Tok.Src.count()));
    
    MLPutSymbol(mlp, TokenToSymbol(Tok.Tok())->name());
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tok.Str.c_str()), static_cast<int>(Tok.Str.size()));
    
    Tok.Src.put(mlp);
}

void LeafNode::print(std::ostream& s) const {
    
    s << SYMBOL_AST_LIBRARY_MAKELEAFNODE->name() << "[";
    
    s << TokenToSymbol(Tok.Tok())->name();
    s << ", ";
    
    s << Tok.Str;
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
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

void CallNode::print(std::ostream& s) const {
    
    auto Src = getSource();
    
    s << SYMBOL_AST_LIBRARY_MAKECALLNODE->name() << "[";
    
    Head.print(s);
    s << ", ";
    
    printChildren(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
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

void SyntaxErrorNode::print(std::ostream& s) const {
    
    auto Src = getSource();
    
    s << SYMBOL_AST_LIBRARY_MAKESYNTAXERRORNODE->name() << "[";
    
    s << SyntaxErrorToString(Err);
    s << ", ";
    
    printChildren(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << "]";
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

void CollectedExpressionsNode::print(std::ostream& s) const {
    
    s << "List[";
    
    for (auto& E : Exprs) {
        E->print(s);
        s << ", ";
    }
    
    s << "]";
}

void CollectedIssuesNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Issues.size()));
    
    for (auto& I : Issues) {
        I->put(mlp);
    }
}

void CollectedIssuesNode::print(std::ostream& s) const {
    
    s << "List[";
    
    for (auto& I : Issues) {
        I->print(s);
        s << ", ";
    }
    
    s << "]";
}

void ListNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(N.size()));
    
    for (auto& NN : N) {
        NN->put(mlp);
    }
}

void ListNode::print(std::ostream& s) const {
    
    s << "List[";
    
    for (auto& NN : N) {
        NN->print(s);
        s << ", ";
    }
    
    s << "]";
}


