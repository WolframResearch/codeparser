
#include "Node.h"

#include "Parser.h" // for TheParser
#include "ByteEncoder.h" // for ByteEncoder
#include "API.h" // for TheParserSession
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "Symbol.h"
#include "MyString.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#include <numeric> // for accumulate


void NodeSeq::append(NodePtr N) {
    vec.push_back(std::move(N));
}

void NodeSeq::appendSeq(TriviaSeq Seq) {
    
    for (auto& T : Seq.vec) {
        vec.push_back(std::move(T));
    }

    Seq.moved = true;
}

void NodeSeq::appendSeq(NodeSeq Seq) {
    
    for (auto& N : Seq.vec) {
        vec.push_back(std::move(N));
    }
}

bool NodeSeq::empty() const {
    return vec.empty();
}

size_t NodeSeq::size() const {
    
    auto accum = std::accumulate(vec.begin(), vec.end(), static_cast<size_t>(0), [](size_t a, const NodePtr& b){ return a + b->size(); });
    
    return accum;
}

const Node *NodeSeq::first() const {
    
    auto i = 0;
    
    auto F = vec.at(i).get();
    
    auto FF = F->first();
    
    return FF;
}

const Node *NodeSeq::last() const {
    
    auto i = vec.size()-1;
    
    auto L = vec.at(i).get();
    
    auto LL = L->last();
    
    return LL;
}

void NodeSeq::print(std::ostream& s) const {
    
    SYMBOL_LIST->print(s);
    s << "[";
    
    for (auto& C : vec) {
        C->print(s);
        s << ", ";
    }
    
    s << "]";
}

bool NodeSeq::check() const {
    
    auto accum = std::accumulate(vec.begin(), vec.end(), true, [](bool a, const NodePtr& b){ return a && b->check(); });
    
    return accum;
}


TriviaSeq::~TriviaSeq() {
    
    if (moved) {
        return;
    }
    
    //
    // This sequence is NOT moved, so destructing this sequence should have the effect of putting it
    // in the front of the "queue" to be read again
    //
    // Just need to reset the global buffer to the buffer of the first token in the sequence
    //
    
    if (vec.empty()) {
        return;
    }
    
    auto& First = vec[0];
    
    auto T = First->getToken();
    
    TheByteBuffer->buffer = T.BufLen.buffer;
    TheByteDecoder->SrcLoc = T.Src.Start;
}

void TriviaSeq::append(LeafNodePtr N) {
    vec.push_back(std::move(N));
}

bool TriviaSeq::empty() const {
    return vec.empty();
}

size_t TriviaSeq::size() const {
    return vec.size();
}

const Node *TriviaSeq::first() const {
    
    auto F = vec.at(0).get();
    
    auto FF = F->first();
    
    return FF;
}

const Node *TriviaSeq::last() const {
    
    auto L = vec.at(vec.size()-1).get();
    
    auto LL = L->last();
    
    return LL;
}


Node::Node(NodeSeq ChildrenIn) : Children(std::move(ChildrenIn)) {}

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

const Node *Node::first() const {
    return this;
}

const Node *Node::last() const {
    return this;
}

Token Node::lastToken() const {
    
    assert(!Children.empty());
    
    auto Last = Children.last();
    
    return Last->lastToken();
}

bool Node::check() const {
    return Children.check();
}


void OperatorNode::print(std::ostream& s) const {
    
    MakeSym->print(s);
    s << "[";
    
    Op->print(s);
    s << ", ";
    
    Children.print(s);
    s << ", ";
    
    getSource().print(s);
    
    s << "]";
}


void LeafNode::print(std::ostream& s) const {
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    SYMBOL_CODEPARSER_LEAFNODE->print(s);
    s << "[";
    
    s << Sym->name();
    s << ", ";
    
    Tok.BufLen.print(s);
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
}


ScopedLeafNode::~ScopedLeafNode() {
    
    if (moved) {
        return;
    }
    
    //
    // This node is NOT moved, so destructing this node should have the effect of putting it
    // in the front of the "queue" to be read again
    //
    // Just need to reset the global buffer to the buffer of the token
    //
    
    TheByteBuffer->buffer = Tok.BufLen.buffer;
    TheByteDecoder->SrcLoc = Tok.Src.Start;
}


void ErrorNode::print(std::ostream& s) const {
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    SYMBOL_CODEPARSER_ERRORNODE->print(s);
    s << "[";
    
    s << Sym->name();
    s << ", ";
    
    Tok.BufLen.print(s);
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
}


void UnterminatedTokenErrorNeedsReparseNode::print(std::ostream& s) const {
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE->print(s);
    s << "[";
    
    s << Sym->name();
    s << ", ";
    
    Tok.BufLen.print(s);
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
}


PrefixNode::PrefixNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXNODE, std::move(Args)) {}

BinaryNode::BinaryNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_BINARYNODE, std::move(Args)) {}

InfixNode::InfixNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_INFIXNODE, std::move(Args)) {}

TernaryNode::TernaryNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_TERNARYNODE, std::move(Args)) {}

PostfixNode::PostfixNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_POSTFIXNODE, std::move(Args)) {}

PrefixBinaryNode::PrefixBinaryNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXBINARYNODE, std::move(Args)) {}

GroupNode::GroupNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPNODE, std::move(Args)) {}

CompoundNode::CompoundNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_COMPOUNDNODE, std::move(Args)) {}

GroupMissingCloserNode::GroupMissingCloserNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE, std::move(Args)) {}

UnterminatedGroupNeedsReparseNode::UnterminatedGroupNeedsReparseNode(const SymbolPtr& Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE, std::move(Args)) {}


void CallNode::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CALLNODE->print(s);
    s << "[";
    
    Head.print(s);
    s << ", ";
    
    Children.print(s);
    s << ", ";
    
    getSource().print(s);
    
    s << "]";
}

Source CallNode::getSource() const {
    
    const auto& First = Head.first();
    const auto& Last = Children.last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}

bool CallNode::check() const {
    return Children.check() && Head.check();
}


void SyntaxErrorNode::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_SYNTAXERRORNODE->print(s);
    s << "[";
    
    s << Err->name();
    s << ", ";
        
    Children.print(s);
    s << ", ";
    
    getSource().print(s);
    
    s << "]";
}


void CollectedExpressionsNode::print(std::ostream& s) const {
    
    SYMBOL_LIST->print(s);
    s << "[";
    
    for (auto& E : Exprs) {
        E->print(s);
        s << ", ";
    }
    
    s << "]";
}

bool CollectedExpressionsNode::check() const {
    
    auto accum = std::accumulate(Exprs.begin(), Exprs.end(), true, [](bool a, const NodePtr& b){ return a && b->check(); });
    
    return accum;
}


void CollectedIssuesNode::print(std::ostream& s) const {
    
    SYMBOL_LIST->print(s);
    s << "[";
    
    for (auto& I : Issues) {
        I->print(s);
        s << ", ";
    }
    
    s << "]";
}

bool CollectedIssuesNode::check() const {
    
    auto accum = std::accumulate(Issues.begin(), Issues.end(), true, [](bool a, const IssuePtr& b){ return a && b->check(); });
    
    return accum;
}


void CollectedSourceLocationsNode::print(std::ostream& s) const {
    
    SYMBOL_LIST->print(s);
    s << "[";
    
    for (auto& L : SourceLocs) {
        L.print(s);
        s << ", ";
    }
    
    s << "]";
}


const MyStringPtr& unsafeCharacterEncodingReason(UnsafeCharacterEncodingFlag flag) {
    
    switch (flag) {
        case UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE:
            return STRING_UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE;
        case UNSAFECHARACTERENCODING_STRAYSURROGATE:
            return STRING_UNSAFECHARACTERENCODING_STRAYSURROGATE;
        case UNSAFECHARACTERENCODING_BOM:
            return STRING_UNSAFECHARACTERENCODING_BOM;
        default:
            assert(false);
            return STRING_UNSAFECHARACTERENCODING_UNKNOWN;
    }
}

void MissingBecauseUnsafeCharacterEncodingNode::print(std::ostream& s) const {
    
    auto& reason = unsafeCharacterEncodingReason(flag);
    
    SYMBOL_MISSING->print(s);
    s << "[";
    
    reason->print(s);
    
    s << "]";
}


void SafeStringNode::print(std::ostream& s) const {
    bufAndLen.print(s);
}



#if USE_MATHLINK
void NodeSeq::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(vec.size()))) {
        assert(false);
    }
    
    for (auto& C : vec) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            return;
        }
#endif // !NABORT
        
        C->put(mlp);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void OperatorNode::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, MakeSym->name(), 3)) {
        assert(false);
    }
    
    Op->put(mlp);
    
    Children.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1)) {
        assert(false);
    }
    
    getSource().put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void LeafNode::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LEAFNODE->name(), 3)) {
        assert(false);
    }

    auto& Sym = TokenToSymbol(Tok.Tok);

    Sym->put(mlp);

    Tok.BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1)) {
        assert(false);
    }
    
    Tok.Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void ErrorNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_ERRORNODE->name(), 3)) {
        assert(false);
    }
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    Sym->put(mlp);
    
    Tok.BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1)) {
        assert(false);
    }
    
    Tok.Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void UnterminatedTokenErrorNeedsReparseNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE->name(), 3)) {
        assert(false);
    }
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    Sym->put(mlp);
    
    Tok.BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1)) {
        assert(false);
    }
    
    Tok.Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CallNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_CALLNODE->name(), 3)) {
        assert(false);
    }
        
    Head.put(mlp);
    
    Children.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1)) {
        assert(false);
    }
    
    getSource().put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void SyntaxErrorNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_SYNTAXERRORNODE->name(), 3)) {
        assert(false);
    }
    
    Err->put(mlp);
    
    Children.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1)) {
        assert(false);
    }
    
    getSource().put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
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
#endif // USE_MATHLINK


#if USE_MATHLINK
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
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedSourceLocationsNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(SourceLocs.size()))) {
        assert(false);
    }
    
    for (auto& L : SourceLocs) {
        
#if !NABORT
        //
        // Check isAbort() inside loops
        //
        if (TheParserSession->isAbort()) {
            
            TheParserSession->handleAbort();
            return;
        }
#endif // !NABORT
            
        L.put(mlp);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void MissingBecauseUnsafeCharacterEncodingNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_MISSING->name(), 1)) {
        assert(false);
    }
    
    auto& reason = unsafeCharacterEncodingReason(flag);
    
    reason->put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void SafeStringNode::put(MLINK mlp) const {
    bufAndLen.put(mlp);
}
#endif // USE_MATHLINK



#if USE_EXPR_LIB
expr NodeSeq::toExpr() const {
    
    auto head = SYMBOL_LIST->toExpr();
    
    auto e = Expr_BuildExprA(head, static_cast<int>(vec.size()));
    
    for (size_t i = 0; i < vec.size(); i++) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbortExpr();
        }
#endif // !NABORT
        
        auto& C = vec[i];
        auto CExpr = C->toExpr();
        Expr_InsertA(e, i + 1, CExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr OperatorNode::toExpr() const {
    
    auto head = MakeSym->toExpr();
        
    auto e = Expr_BuildExprA(head, 3);
    
    auto OpExpr = Op->toExpr();
    Expr_InsertA(e, 0 + 1, OpExpr);
        
    auto ChildrenExpr = Children.toExpr();
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION->toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = getSource().toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr LeafNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_LEAFNODE->toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Sym->toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = Tok.BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION->toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Tok.Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr ErrorNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_ERRORNODE->toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Sym->toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = Tok.BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION->toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Tok.Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr UnterminatedTokenErrorNeedsReparseNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE->toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto& Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Sym->toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = Tok.BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION->toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Tok.Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CallNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_CALLNODE->toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto HeadExpr = Head.toExpr();
    Expr_InsertA(e, 0 + 1, HeadExpr);
        
    auto ChildrenExpr = Children.toExpr();
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION->toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = getSource().toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr SyntaxErrorNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_SYNTAXERRORNODE->toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto SymExpr = Err->toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto ChildrenExpr = Children.toExpr();
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION->toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = getSource().toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedExpressionsNode::toExpr() const {
    
    auto head = SYMBOL_LIST->toExpr();
    
    auto e = Expr_BuildExprA(head, static_cast<int>(Exprs.size()));
    
    for (size_t i = 0; i < Exprs.size(); i++) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbortExpr();
        }
#endif // !NABORT
        
        auto& NN = Exprs[i];
        auto NExpr = NN->toExpr();
        Expr_InsertA(e, i + 1, NExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedIssuesNode::toExpr() const {
    
    auto head = SYMBOL_LIST->toExpr();
    
    auto e = Expr_BuildExprA(head, static_cast<int>(Issues.size()));
    
    int i = 0;
    for (auto& I : Issues) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbortExpr();
        }
#endif // !NABORT
        
        auto IExpr = I->toExpr();
        Expr_InsertA(e, i + 1, IExpr);
        i++;
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedSourceLocationsNode::toExpr() const {
    
    auto head = SYMBOL_LIST->toExpr();
            
    auto e = Expr_BuildExprA(head, static_cast<int>(SourceLocs.size()));
    
    int i = 0;
    for (auto& L : SourceLocs) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            
            return TheParserSession->handleAbortExpr();
        }
#endif // !NABORT
        
        auto LExpr = L.toExpr();
        Expr_InsertA(e, i + 1, LExpr);
        i++;
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr MissingBecauseUnsafeCharacterEncodingNode::toExpr() const {
    
    auto head = SYMBOL_MISSING->toExpr();
    
    auto e = Expr_BuildExprA(head, 1);
    
    auto& reason = unsafeCharacterEncodingReason(flag);
    
    auto StrExpr = reason->toExpr();
    Expr_InsertA(e, 0 + 1, StrExpr);
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr SafeStringNode::toExpr() const {
    
    auto e = bufAndLen.toExpr();
    
    return e;
}
#endif // USE_EXPR_LIB
