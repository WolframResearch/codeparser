
#include "Node.h"

#include "Parser.h" // for TheParser
#include "ByteEncoder.h" // for ByteEncoder
#include "API.h" // for TheParserSession

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

const Node* NodeSeq::first() const {
    
    auto i = 0;
    
    auto F = vec.at(i).get();
    
    auto FF = F->first();
    
    return FF;
}

const Node* NodeSeq::last() const {
    
    auto i = vec.size()-1;
    
    auto L = vec.at(i).get();
    
    auto LL = L->last();
    
    return LL;
}


void NodeSeq::print(std::ostream& s) const {
    
    s << SYMBOL_LIST->name() << "[";
    
    print0(s);
    
    s << "]";
}

void NodeSeq::print0(std::ostream& s) const {
    
    for (auto& C : vec) {
        C->print(s);
        s << ", ";
    }
}

void LeafSeq::print0(std::ostream& s) const {
    
    for (auto& C : vec) {
        C->print(s);
        s << ", ";
    }
}

expr LeafSeq::toExpr0() const {
    
    xx;
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

    // TODO: enable again after whitespace work
    // "whitespace work" is refactoring of how eatAll() and appendIfEmpty() is done through the code
    
//    auto F = Children.first();
//    auto L = Children.last();
//
//    assert(!F->isTrivia());
//    assert(!L->isTrivia());
#endif
}

bool Node::isTrivia() const {
    return false;
}

bool Node::isEmpty() const {
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

void Node::printChildren(std::ostream& s) const {
    
    Children.print(s);
}

void LeafSeqNode::print(std::ostream& s) const {
    
    Children.print0(s);
}

expr LeafSeqNode::toExpr() const {
    
    return Children.toExpr0();
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

void NodeSeqNode::print(std::ostream& s) const {
    
    Children.print0(s);
}

expr NodeSeqNode::toExpr() const {
    assert(false);
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

expr OperatorNode::toExpr() const {
    assert(false);
}


void LeafNode::print(std::ostream& s) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        auto& Sym = TokenToSymbol(Tok.Tok);
        
        s << SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name() << "[";
        
        s << Sym->name();
        s << ", ";
        
        if (!Tok.Tok.isEmpty()) {
            
            Tok.BufLen.printUTF8String(s);
        }
        
        s << ", ";
        
        Tok.Src.print(s);
        
        s << "]";
        
        return;
    }
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name() << "[";
    
    s << Sym->name();
    s << ", ";
    
    if (!Tok.Tok.isEmpty()) {
        
        Tok.BufLen.printUTF8String(s);
    }
    
    s << "]";
}

expr LeafNode::toExpr() const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        auto head = Expr_LookupSymbol(SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name());
        
        auto e = Expr_BuildExpression(head, static_cast<int>(2 + 1));
        
        auto& Sym = TokenToSymbol(Tok.Tok);
        
        auto SymExpr = Expr_LookupSymbol(Sym->name());
        Expr_Insert(e, 1, SymExpr);

        auto TokBufLenExpr = Expr_FromUTF8String(Tok.BufLen.buffer, Tok.BufLen.length());
        Expr_Insert(e, 2, TokBufLenExpr);
        Expr_Release(TokBufLenExpr);
        
        auto SrcExpr = Tok.Src.toExpr();
        Expr_Insert(e, 3, SrcExpr);
        Expr_Release(SrcExpr);
        
        return e;
    }
    
    auto head = Expr_LookupSymbol(SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name());
    
    auto e = Expr_BuildExpression(head, static_cast<int>(2));
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Expr_LookupSymbol(Sym->name());
    Expr_Insert(e, 1, SymExpr);
    
    auto TokBufLenExpr = Expr_FromUTF8String(Tok.BufLen.buffer, Tok.BufLen.length());
    Expr_Insert(e, 2, TokBufLenExpr);
    Expr_Release(TokBufLenExpr);
    
    return e;
}

bool LeafNode::isTrivia() const {
    return Tok.Tok.isTrivia();
}

bool LeafNode::isEmpty() const {
    return Tok.Tok.isEmpty();
}

void ErrorNode::print(std::ostream& s) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        auto& Sym = TokenToSymbol(Tok.Tok);
        
        s << SYMBOL_CODEPARSER_LIBRARY_MAKEERRORNODE->name() << "[";
        
        s << Sym->name();
        s << ", ";
        
        if (!Tok.Tok.isEmpty()) {
            
            Tok.BufLen.printUTF8String(s);
        }
        
        s << ", ";
        
        Tok.Src.print(s);
        
        s << "]";
        
        return;
    }
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEERRORNODE->name() << "[";
    
    s << Sym->name();
    s << ", ";
    
    if (!Tok.Tok.isEmpty()) {
        
        Tok.BufLen.printUTF8String(s);
    }
    
    s << "]";
}

expr ErrorNode::toExpr() const {
    assert(false);
}

bool ErrorNode::isTrivia() const {
    return Tok.Tok.isTrivia();
}

bool ErrorNode::isEmpty() const {
    return Tok.Tok.isEmpty();
}


void CallNode::print(std::ostream& s) const {
    
    auto Src = getSource();
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKECALLNODE->name() << "[";
    
    Head.print(s);
    s << ", ";
    
    printChildren(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}

expr CallNode::toExpr() const {
    assert(false);
}

Source CallNode::getSource() const {
    
    const auto& First = Head.first();
    
    const auto& Children = getChildrenSafe();
    const auto& Last = Children.last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}


void SyntaxErrorNode::print(std::ostream& s) const {
    
    auto Src = getSource();
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKESYNTAXERRORNODE->name() << "[";
    
    s << SyntaxErrorToString(Err);
    s << ", ";
    
    printChildren(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << "]";
}

expr SyntaxErrorNode::toExpr() const {
    assert(false);
}

void CollectedExpressionsNode::print(std::ostream& s) const {
    
    s << "List[";
    
    for (auto& E : Exprs) {
        E->print(s);
        s << ", ";
    }
    
    s << "]";
}

expr CollectedExpressionsNode::toExpr() const {
    
    auto head = Expr_LookupSymbol(SYMBOL_LIST->name());
    
    auto e = Expr_BuildExpression(head, static_cast<int>(Exprs.size()));
    
    for (size_t i = 0; i < Exprs.size(); i++) {
        auto NN = Exprs[i];
        auto NExpr = NN->toExpr();
        Expr_Insert(e, i + 1, NExpr);
        Expr_Release(NExpr);
    }
    
    return e;
}

void CollectedIssuesNode::print(std::ostream& s) const {
    
    s << "List[";
    
    for (auto& I : Issues) {
        I->print(s);
        s << ", ";
    }
    
    s << "]";
}

expr CollectedIssuesNode::toExpr() const {
    
    auto head = Expr_LookupSymbol(SYMBOL_LIST->name());
    
    auto e = Expr_BuildExpression(head, static_cast<int>(Issues.size()));
    
    for (size_t i = 0; i < Issues.size(); i++) {
        auto& NN = Issues[i];
        auto NExpr = NN->toExpr();
        Expr_Insert(e, i + 1, NExpr);
        Expr_Release(NExpr);
    }
    
    return e;
}

void ListNode::print(std::ostream& s) const {
    
    s << "List[";
    
    for (auto& NN : N) {
        NN->print(s);
        s << ", ";
    }
    
    s << "]";
}

expr ListNode::toExpr() const {
    
    auto head = Expr_LookupSymbol(SYMBOL_LIST->name());
    
    auto e = Expr_BuildExpression(head, static_cast<int>(N.size()));
    
    for (size_t i = 0; i < N.size(); i++) {
        auto NN = N[i];
        auto NExpr = NN->toExpr();
        Expr_Insert(e, i + 1, NExpr);
        Expr_Release(NExpr);
    }
    
    return e;
}

void SourceCharacterNode::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKESOURCECHARACTERNODE->name() << "[";
    
    s << SYMBOL_CODEPARSER_SOURCECHARACTER->name() << ", ";
    
    s << Char;
    
    s << "]\n";
}

expr SourceCharacterNode::toExpr() const {
    assert(false);
}

void SafeStringNode::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKESAFESTRINGNODE->name() << "[";
    
    s << "<<safe string that I'm too lazy to print>>";
    
    s << "]\n";
}

expr SafeStringNode::toExpr() const {
    assert(false);
}



#if USE_MATHLINK

void NodeSeq::put(MLINK mlp) const {
    
    auto s = size();
    
    if(!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(s))) {
        assert(false);
    }
    
    put0(mlp);
}

void NodeSeq::put0(MLINK mlp) const {
    
    for (auto& C : vec) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            TheParserSession->handleAbort();
            return;
        }
#endif // !NABORT
        
        C->put(mlp);
    }
}

void LeafSeq::put0(MLINK mlp) const {
    
    for (auto& C : vec) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            TheParserSession->handleAbort();
            return;
        }
#endif // !NABORT
        
        C->put(mlp);
    }
}

void Node::putChildren(MLINK mlp) const {
    
    Children.put(mlp);
}

void LeafSeqNode::put(MLINK mlp) const {
    
    Children.put0(mlp);
}

void NodeSeqNode::put(MLINK mlp) const {
    
    Children.put0(mlp);
}

void OperatorNode::put(MLINK mlp) const {

    if(!MLPutFunction(mlp, MakeSym->name(), static_cast<int>(2 + 4))) {
        assert(false);
    }
    
    if(!MLPutSymbol(mlp, Op->name())) {
        assert(false);
    }
    
    putChildren(mlp);
    
    getSource().put(mlp);
}

void LeafNode::put(MLINK mlp) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {

        if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2 + 4))) {
            assert(false);
        }

        auto& Sym = TokenToSymbol(Tok.Tok);

        if (!MLPutSymbol(mlp, Sym->name())) {
            assert(false);
        }

        Tok.BufLen.putUTF8String(mlp);

        Tok.Src.put(mlp);

        return;
    }

    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2))) {
        assert(false);
    }

    auto& Sym = TokenToSymbol(Tok.Tok);

    if (!MLPutSymbol(mlp, Sym->name())) {
        assert(false);
    }

    Tok.BufLen.putUTF8String(mlp);
}

void ErrorNode::put(MLINK mlp) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEERRORNODE->name(), static_cast<int>(2 + 4))) {
            assert(false);
        }
        
        auto& Sym = TokenToSymbol(Tok.Tok);
        
        if (!MLPutSymbol(mlp, Sym->name())) {
            assert(false);
        }
        
        Tok.BufLen.putUTF8String(mlp);
        
        Tok.Src.put(mlp);
        
        return;
    }
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEERRORNODE->name(), static_cast<int>(2))) {
        assert(false);
    }
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    if (!MLPutSymbol(mlp, Sym->name())) {
        assert(false);
    }
    
    Tok.BufLen.putUTF8String(mlp);
}


void CallNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKECALLNODE->name(), static_cast<int>(2 + 4))) {
        assert(false);
    }
    
    Head.put(mlp);
    
    putChildren(mlp);
    
    Src.put(mlp);
}

void SyntaxErrorNode::put(MLINK mlp) const {
    
    auto Src = getSource();
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKESYNTAXERRORNODE->name(), static_cast<int>(2 + 4))) {
        assert(false);
    }
    
    if (!MLPutSymbol(mlp, SyntaxErrorToString(Err).c_str())) {
        assert(false);
    }
    
    putChildren(mlp);
    
    Src.put(mlp);
}

void CollectedExpressionsNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Exprs.size()))) {
        assert(false);
    }
    
    for (auto& E : Exprs) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            TheParserSession->handleAbort();
            return;
        }
#endif // !NABORT
        
        E->put(mlp);
    }
}

void CollectedIssuesNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Issues.size()))) {
        assert(false);
    }
    
    for (auto& I : Issues) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            TheParserSession->handleAbort();
            return;
        }
#endif // !NABORT
        
        I->put(mlp);
    }
}

void ListNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(N.size()))) {
        assert(false);
    }
    
    for (auto& NN : N) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            TheParserSession->handleAbort();
            return;
        }
#endif // !NABORT
        
        NN->put(mlp);
    }
}

void SourceCharacterNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKESOURCECHARACTERNODE->name(), static_cast<int>(2))) {
        assert(false);
    }
    
    if (!MLPutSymbol(mlp, SYMBOL_CODEPARSER_SOURCECHARACTER->name())) {
        assert(false);
    }
    
    auto val = Char.to_point();
    
    auto S = ByteEncoder::size(val);
        
    std::array<unsigned char, 4> Arr;
    ByteEncoderState state;
    
    ByteEncoder::encodeBytes(Arr, val, &state);
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Arr.data()), static_cast<int>(S))) {
        assert(false);
    }
}

void SafeStringNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKESAFESTRINGNODE->name(), static_cast<int>(1))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(safeBytes.data()), static_cast<int>(safeBytes.size()))) {
        assert(false);
    }
}

#endif // USE_MATHLINK

