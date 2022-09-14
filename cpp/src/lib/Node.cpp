
#include "Node.h"

#include "SymbolRegistration.h"
#include "MyStringRegistration.h"
#include "ParserSession.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <limits> // for numeric_limits


using MNodePtr = Node *;


struct SyntaxQVisitor {
    
    bool operator()(const NodePtr& N) { return N->syntaxQ(); }
    
    bool operator()(const Token& L) { return L.syntaxQ(); }
};

struct GetSourceVisitor {
    
    Source operator()(const NodePtr& N) { return N->getSource(); }
    
    Source operator()(const Token& L) { return L.Src; }
};

struct PrintVisitor {
    
    ParserSessionPtr session;
    std::ostream& s;
    
    PrintVisitor(ParserSessionPtr session, std::ostream& s) : session(session), s(s) {};
    
    void operator()(const NodePtr& N) { return N->print(session, s); }
    
    void operator()(const Token& L) { return L.print(session, s); }
};

struct ReleaseVisitor {
    
    void operator()(const NodePtr& N) { const_cast<MNodePtr>(N)->release(); delete N; }
    
    void operator()(const Token& L) {}
};

#if USE_EXPR_LIB
struct ToExprVisitor {
    
    ParserSessionPtr session;
    
    ToExprVisitor(ParserSessionPtr session) : session(session) {};
    
    expr operator()(const NodePtr& N) { return N->toExpr(session); }
    
    expr operator()(const Token& L) { return L.toExpr(session); }
};
#endif // USE_EXPR_LIB

#if USE_MATHLINK
struct PutVisitor {
    
    ParserSessionPtr session;
    MLINK callLink;
    
    PutVisitor(ParserSessionPtr session, MLINK callLink) : session(session), callLink(callLink) {};
    
    void operator()(const NodePtr& N) { N->put(session, callLink); }
    
    void operator()(const Token& L) { L.put(session, callLink); }
};
#endif // USE_MATHLINK


NodeSeq::NodeSeq() : vec() {}

NodeSeq::NodeSeq(std::vector<NodeVariant> vec) : vec(vec) {}

NodeSeq::NodeSeq(std::vector<NodeVariant>::iterator Begin, std::vector<NodeVariant>::iterator End) : vec(Begin, End) {}

void NodeSeq::release() {
    for (auto& N : vec) {
        std::visit(ReleaseVisitor{}, N);
    }
}

void NodeSeq::push(NodeVariant&& N) {
    vec.push_back(N);
}

void NodeSeq::clear() {
    vec.clear();
}

bool NodeSeq::empty() const {
    return vec.empty();
}

size_t NodeSeq::size() const {
    return vec.size();
}

const NodeVariant& NodeSeq::operator[](size_t index) const {
    return vec[index];
}

const NodeVariant& NodeSeq::first() const {

    assert(!vec.empty());
    
    return vec.front();
}

const NodeVariant& NodeSeq::last() const {

    assert(!vec.empty());
    
    return vec.back();
}

void NodeSeq::print(ParserSessionPtr session, std::ostream& s) const {
    
    SYMBOL_LIST.print(session, s);
    s << "[";
    
    for (auto& C : vec) {
        std::visit(PrintVisitor{session, s}, C);
        s << ", ";
    }
    
    s << "]";
}

bool NodeSeq::syntaxQ() const {
    
    for (auto& C : vec) {
        if (!std::visit(SyntaxQVisitor{}, C)) {
            return false;
        }
    }
    
    return true;
}


TriviaSeq::TriviaSeq() : vec() {}

std::vector<Token>::iterator TriviaSeq::begin() {
    return vec.begin();
}

std::vector<Token>::iterator TriviaSeq::end() {
    return vec.end();
}

void TriviaSeq::reset(ParserSessionPtr session) {
    
    //
    // Just need to reset the global buffer to the buffer of the first token in the sequence
    //
    
    if (vec.empty()) {
        return;
    }
    
    auto& T = vec[0];
    
    session->buffer = T.Buf;
    session->SrcLoc = T.Src.Start;
    
    vec.clear();
}

void TriviaSeq::push(Token N) {
    vec.push_back(N);
}

bool TriviaSeq::empty() const {
    return vec.empty();
}

void TriviaSeq::clear() {
    vec.clear();
}


void Node::release() {}

Node::~Node() {}


OperatorNode::OperatorNode(Symbol Op, Symbol MakeSym, NodeSeq&& ChildrenIn) : Op(Op), MakeSym(MakeSym), Children(std::move(ChildrenIn)), Src(std::visit(GetSourceVisitor{}, Children.first()), std::visit(GetSourceVisitor{}, Children.last())) {
    assert(!Children.empty());
}

void OperatorNode::release() {
    Children.release();
}

Symbol OperatorNode::getOp() const {
    return Op;
}

Source OperatorNode::getSource() const {
    return Src;
}

bool OperatorNode::syntaxQ() const {
    return Children.syntaxQ();
}

void OperatorNode::print(ParserSessionPtr session, std::ostream& s) const {
    
    MakeSym.print(session, s);
    s << "[";
    
    Op.print(session, s);
    s << ", ";
    
    Children.print(session, s);
    s << ", ";
    
    Src.print(session, s);
    
    s << "]";
}


AbortNode::AbortNode() {
    
#if DIAGNOSTICS
    Node_AbortNodeCount++;
#endif // DIAGNOSTICS
}

void AbortNode::print(ParserSessionPtr session, std::ostream& s) const {
    SYMBOL__ABORTED.print(session, s);
}

Source AbortNode::getSource() const {
    return Source(SourceLocation(std::numeric_limits<uint32_t>::max(), std::numeric_limits<uint32_t>::max()));
}

bool AbortNode::syntaxQ() const {
    return false;
}


bool GroupMissingCloserNode::syntaxQ() const {
    return false;
}


bool UnterminatedGroupNeedsReparseNode::syntaxQ() const {
    return false;
}


PrefixNode::PrefixNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_PrefixNodeCount++;
#endif // DIAGNOSTICS
}

BinaryNode::BinaryNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_BINARYNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_BinaryNodeCount++;
#endif // DIAGNOSTICS
}

InfixNode::InfixNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_INFIXNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_InfixNodeCount++;
#endif // DIAGNOSTICS
}

TernaryNode::TernaryNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_TERNARYNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_TernaryNodeCount++;
#endif // DIAGNOSTICS
}

PostfixNode::PostfixNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_POSTFIXNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_PostfixNodeCount++;
#endif // DIAGNOSTICS
}

PrefixBinaryNode::PrefixBinaryNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXBINARYNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_PrefixBinaryNodeCount++;
#endif // DIAGNOSTICS
}

GroupNode::GroupNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_GroupNodeCount++;
#endif // DIAGNOSTICS
}

CompoundNode::CompoundNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_COMPOUNDNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_CompoundNodeCount++;
#endif // DIAGNOSTICS
}

GroupMissingCloserNode::GroupMissingCloserNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_GroupMissingCloserNodeCount++;
#endif // DIAGNOSTICS
}

UnterminatedGroupNeedsReparseNode::UnterminatedGroupNeedsReparseNode(Symbol Op, NodeSeq&& Args) : OperatorNode(Op, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_UnterminatedGroupNeedsReparseNodeCount++;
#endif // DIAGNOSTICS
}


CallNode::CallNode(NodeSeq&& HeadIn, NodeVariant&& BodyIn) : Head(std::move(HeadIn)), Body(std::move(BodyIn)), Src(std::visit(GetSourceVisitor{}, Head.first()), std::visit(GetSourceVisitor{}, Body)) {
    
    assert(!Head.empty());
    
#if DIAGNOSTICS
    Node_CallNodeCount++;
#endif // DIAGNOSTICS
}

void CallNode::release() {
    
    Head.release();

    std::visit(ReleaseVisitor{}, Body);
}

Source CallNode::getSource() const {
    return Src;
}

void CallNode::print(ParserSessionPtr session, std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CALLNODE.print(session, s);
    s << "[";
    
    Head.print(session, s);
    s << ", ";
    
    std::visit(PrintVisitor{session, s}, Body);
    s << ", ";
    
    Src.print(session, s);
    
    s << "]";
}

bool CallNode::syntaxQ() const {
    return Head.syntaxQ() && std::visit(SyntaxQVisitor{}, Body);
}


SyntaxErrorNode::SyntaxErrorNode(Symbol Err, NodeSeq&& ChildrenIn) : Err(Err), Children(std::move(ChildrenIn)), Src(std::visit(GetSourceVisitor{}, Children.first()), std::visit(GetSourceVisitor{}, Children.last())) {
    
    assert(!Children.empty());

#if DIAGNOSTICS
    Node_SyntaxErrorNodeCount++;
#endif // DIAGNOSTICS
}

void SyntaxErrorNode::release() {
    Children.release();
}

bool SyntaxErrorNode::syntaxQ() const {
    return false;
}

Source SyntaxErrorNode::getSource() const {
    return Src;
}

void SyntaxErrorNode::print(ParserSessionPtr session, std::ostream& s) const {
    
    SYMBOL_CODEPARSER_SYNTAXERRORNODE.print(session, s);
    s << "[";
    
    s << Err.Name;
    s << ", ";
    
    Children.print(session, s);
    s << ", ";
    
    Src.print(session, s);
    
    s << "]";
}


CollectedExpressionsNode::CollectedExpressionsNode(NodeSeq&& Exprs) : Exprs(Exprs) {}

void CollectedExpressionsNode::release() {
    Exprs.release();
}

void CollectedExpressionsNode::print(ParserSessionPtr session, std::ostream& s) const {
    Exprs.print(session, s);
}

bool CollectedExpressionsNode::syntaxQ() const {
    return Exprs.syntaxQ();
}

Source CollectedExpressionsNode::getSource() const {

    assert(false);
    
    return Source();
}


CollectedIssuesNode::CollectedIssuesNode(IssuePtrVector Issues) : Issues(Issues) {}

void CollectedIssuesNode::release() {
    for (auto& I : Issues) {
        delete I;
    }
}

void CollectedIssuesNode::print(ParserSessionPtr session, std::ostream& s) const {
    
    SYMBOL_LIST.print(session, s);
    s << "[";
    
    for (auto& I : Issues) {
        I->print(session, s);
        s << ", ";
    }
    
    s << "]";
}

bool CollectedIssuesNode::syntaxQ() const {
    
    for (auto& I : Issues) {
        if (!I->syntaxQ()) {
            return false;
        }
    }
    
    return true;
}

Source CollectedIssuesNode::getSource() const {
    
    assert(false);
    
    return Source();
}


CollectedSourceLocationsNode::CollectedSourceLocationsNode(std::set<SourceLocation> SourceLocs) : SourceLocs(SourceLocs) {}

Source CollectedSourceLocationsNode::getSource() const {
    
    assert(false);
    
    return Source();
}

bool CollectedSourceLocationsNode::syntaxQ() const {
    return true;
}

void CollectedSourceLocationsNode::print(ParserSessionPtr session, std::ostream& s) const {
    
    SYMBOL_LIST.print(session, s);
    s << "[";
    
    for (auto& L : SourceLocs) {
        L.print(session, s);
        s << ", ";
    }
    
    s << "]";
}


MyString unsafeCharacterEncodingReason(UnsafeCharacterEncodingFlag flag) {
    
    switch (flag) {
        case UNSAFECHARACTERENCODING_OK: {
            
            assert(false);
            
            return STRING_UNSAFECHARACTERENCODING_UNKNOWN;
        }
        case UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE: {
            return STRING_UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE;
        }
        case UNSAFECHARACTERENCODING_STRAYSURROGATE: {
            return STRING_UNSAFECHARACTERENCODING_STRAYSURROGATE;
        }
        case UNSAFECHARACTERENCODING_BOM: {
            return STRING_UNSAFECHARACTERENCODING_BOM;
        }
    }
    
    assert(false);
    
    return STRING_UNSAFECHARACTERENCODING_UNKNOWN;
}

MissingBecauseUnsafeCharacterEncodingNode::MissingBecauseUnsafeCharacterEncodingNode(UnsafeCharacterEncodingFlag flag) : flag(flag) {}

Source MissingBecauseUnsafeCharacterEncodingNode::getSource() const {
    
    assert(false);
    
    return Source();
}

bool MissingBecauseUnsafeCharacterEncodingNode::syntaxQ() const {
    return false;
}

void MissingBecauseUnsafeCharacterEncodingNode::print(ParserSessionPtr session, std::ostream& s) const {
    
    auto reason = unsafeCharacterEncodingReason(flag);
    
    SYMBOL_MISSING.print(session, s);
    s << "[";
    
    reason.print(session, s);
    
    s << "]";
}


SafeStringNode::SafeStringNode(Buffer Buf, size_t Len) : Buf(Buf), Len(Len) {}

Source SafeStringNode::getSource() const {
    
    assert(false);
    
    return Source();
}

bool SafeStringNode::syntaxQ() const {
    return true;
}

void SafeStringNode::print(ParserSessionPtr session, std::ostream& s) const {
    s.write(reinterpret_cast<const char *>(Buf), Len);
}


NodeContainer::NodeContainer(NodeSeq&& Nodes) : Nodes(Nodes) {}

Source NodeContainer::getSource() const {
    
    assert(false);
    
    return Source();
}

void NodeContainer::release() {
    Nodes.release();
}

void NodeContainer::print(ParserSessionPtr session, std::ostream& s) const {
    Nodes.print(session, s);
}

bool NodeContainer::syntaxQ() const {
    return Nodes.syntaxQ();
}


#if USE_MATHLINK
void NodeSeq::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_LIST.Name, static_cast<int>(vec.size()))) {
        assert(false);
    }
    
    for (auto& C : vec) {
        
#if CHECK_ABORT
        if (session->abortQ()) {
            SYMBOL__ABORTED.put(session, callLink);
            continue;
        }
#endif // CHECK_ABORT
        
        std::visit(PutVisitor{session, callLink}, C);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void OperatorNode::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, MakeSym.Name, 3)) {
        assert(false);
    }
    
    Op.put(session, callLink);
    
    Children.put(session, callLink);
    
    if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 1)) {
        assert(false);
    }
    
    Src.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void AbortNode::put(ParserSessionPtr session, MLINK callLink) const {
    SYMBOL__ABORTED.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CallNode::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_CALLNODE.Name, 3)) {
        assert(false);
    }
        
    Head.put(session, callLink);
    
    std::visit(PutVisitor{session, callLink}, Body);
    
    if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 1)) {
        assert(false);
    }
    
    Src.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void SyntaxErrorNode::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_SYNTAXERRORNODE.Name, 3)) {
        assert(false);
    }
    
    Err.put(session, callLink);
    
    Children.put(session, callLink);
    
    if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 1)) {
        assert(false);
    }
    
    Src.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedExpressionsNode::put(ParserSessionPtr session, MLINK callLink) const {
    Exprs.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedIssuesNode::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_LIST.Name, static_cast<int>(Issues.size()))) {
        assert(false);
    }
    
    for (auto& I : Issues) {
        I->put(session, callLink);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedSourceLocationsNode::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_LIST.Name, static_cast<int>(SourceLocs.size()))) {
        assert(false);
    }
    
    for (auto& L : SourceLocs) {
        L.put(session, callLink);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void MissingBecauseUnsafeCharacterEncodingNode::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_MISSING.Name, 1)) {
        assert(false);
    }
    
    auto reason = unsafeCharacterEncodingReason(flag);
    
    reason.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void SafeStringNode::put(ParserSessionPtr session, MLINK callLink) const {
    if (!MLPutUTF8String(callLink, Buf, static_cast<int>(Len))) {
        assert(false);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void NodeContainer::put(ParserSessionPtr session, MLINK callLink) const {
    Nodes.put(session, callLink);
}
#endif // USE_MATHLINK


#if USE_EXPR_LIB
expr NodeSeq::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_LIST.toExpr(session);

    auto e = Expr_BuildExprA(head, static_cast<int>(vec.size()));

    for (size_t i = 0; i < vec.size(); i++) {

#if CHECK_ABORT
        if (session->abortQ()) {
            Expr_InsertA(e, i + 1, SYMBOL__ABORTED.toExpr(session));
            continue;
        }
#endif // CHECK_ABORT

        auto& C = vec[i];

        auto CExpr = std::visit(ToExprVisitor{session}, C);

        Expr_InsertA(e, i + 1, CExpr);
    }

    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr OperatorNode::toExpr(ParserSessionPtr session) const {
    
    auto head = MakeSym.toExpr(session);
        
    auto e = Expr_BuildExprA(head, 3);
    
    auto OpExpr = Op.toExpr(session);
    Expr_InsertA(e, 0 + 1, OpExpr);
        
    auto ChildrenExpr = Children.toExpr(session);
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr AbortNode::toExpr(ParserSessionPtr session) const {
    
    return SYMBOL__ABORTED.toExpr(session);
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CallNode::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_CODEPARSER_CALLNODE.toExpr(session);
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto HeadExpr = Head.toExpr(session);
    Expr_InsertA(e, 0 + 1, HeadExpr);
        
    auto BodyExpr = std::visit(ToExprVisitor{session}, Body);
    Expr_InsertA(e, 1 + 1, BodyExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr SyntaxErrorNode::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_CODEPARSER_SYNTAXERRORNODE.toExpr(session);
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto SymExpr = Err.toExpr(session);
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto ChildrenExpr = Children.toExpr(session);
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedExpressionsNode::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_LIST.toExpr(session);
    
    auto e = Expr_BuildExprA(head, static_cast<int>(Exprs.size()));
    
    for (size_t i = 0; i < Exprs.size(); i++) {
        
        auto& NN = Exprs[i];
        
        auto NExpr = std::visit(ToExprVisitor{session}, NN);
        
        Expr_InsertA(e, i + 1, NExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedIssuesNode::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_LIST.toExpr(session);
    
    auto e = Expr_BuildExprA(head, static_cast<int>(Issues.size()));
    
    int i = 0;
    for (auto& I : Issues) {
        
        auto IExpr = I->toExpr(session);
        
        Expr_InsertA(e, i + 1, IExpr);
        
        i++;
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedSourceLocationsNode::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_LIST.toExpr(session);
            
    auto e = Expr_BuildExprA(head, static_cast<int>(SourceLocs.size()));
    
    int i = 0;
    for (auto& L : SourceLocs) {
        
        auto LExpr = L.toExpr(session);
        
        Expr_InsertA(e, i + 1, LExpr);
        
        i++;
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr MissingBecauseUnsafeCharacterEncodingNode::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_MISSING.toExpr(session);
    
    auto e = Expr_BuildExprA(head, 1);
    
    auto reason = unsafeCharacterEncodingReason(flag);
    
    auto StrExpr = reason.toExpr(session);
    Expr_InsertA(e, 0 + 1, StrExpr);
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr SafeStringNode::toExpr(ParserSessionPtr session) const {
    return Expr_UTF8BytesToStringExpr(Buf, static_cast<int>(Len));
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr NodeContainer::toExpr(ParserSessionPtr session) const {
    return Nodes.toExpr(session);
}
#endif // USE_EXPR_LIB
