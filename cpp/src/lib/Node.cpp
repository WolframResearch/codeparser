
#include "Node.h"

#include "Parser.h" // for TheParser
#include "ByteEncoder.h" // for ByteEncoder
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "SymbolRegistration.h"
#include "MyStringRegistration.h"
#include "ParserSession.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <limits>


struct CheckVisitor {
    
    bool operator()(const NodePtr& N) { return N->check(); }
    
    bool operator()(const Token& L) { return L.check(); }
};

struct GetSourceVisitor {
    
    Source operator()(const NodePtr& N) { return N->getSource(); }
    
    Source operator()(const Token& L) { return L.Src; }
};

struct PrintVisitor {
    
    std::ostream& s;
    
    PrintVisitor(std::ostream& s) : s(s) {};
    
    void operator()(const NodePtr& N) { return N->print(s); }
    
    void operator()(const Token& L) { return L.print(s); }
};

struct ReleaseVisitor {
    
    void operator()(const NodePtr& N) { N->release(); delete N; }
    
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

void NodeSeq::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
    s << "[";
    
    for (auto& C : vec) {
        std::visit(PrintVisitor{s}, C);
        s << ", ";
    }
    
    s << "]";
}

bool NodeSeq::check() const {
    
    for (auto& C : vec) {
        if (!std::visit(CheckVisitor{}, C)) {
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

bool Node::check() const {
    return true;
}

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

bool OperatorNode::check() const {
    return Children.check();
}

void OperatorNode::print(std::ostream& s) const {
    
    MakeSym.print(s);
    s << "[";
    
    Op.print(s);
    s << ", ";
    
    Children.print(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}


AbortNode::AbortNode() {
    
#if DIAGNOSTICS
    Node_AbortNodeCount++;
#endif // DIAGNOSTICS
}

void AbortNode::print(std::ostream& s) const {
    SYMBOL__ABORTED.print(s);
}

Source AbortNode::getSource() const {
    return Source(SourceLocation(std::numeric_limits<uint32_t>::max(), std::numeric_limits<uint32_t>::max()));
}

bool AbortNode::check() const {
    return false;
}


bool GroupMissingCloserNode::check() const {
    return false;
}


bool UnterminatedGroupNeedsReparseNode::check() const {
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

void CallNode::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CALLNODE.print(s);
    s << "[";
    
    Head.print(s);
    s << ", ";
    
    std::visit(PrintVisitor{s}, Body);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}

bool CallNode::check() const {
    return Head.check() && std::visit(CheckVisitor{}, Body);
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

bool SyntaxErrorNode::check() const {
    return false;
}

Source SyntaxErrorNode::getSource() const {
    return Src;
}

void SyntaxErrorNode::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_SYNTAXERRORNODE.print(s);
    s << "[";
    
    s << Err.Name;
    s << ", ";
        
    Children.print(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}


CollectedExpressionsNode::CollectedExpressionsNode(NodeSeq&& Exprs) : Exprs(Exprs) {}

void CollectedExpressionsNode::release() {
    Exprs.release();
}

void CollectedExpressionsNode::print(std::ostream& s) const {
    Exprs.print(s);
}

bool CollectedExpressionsNode::check() const {
    return Exprs.check();
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

void CollectedIssuesNode::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
    s << "[";
    
    for (auto& I : Issues) {
        I->print(s);
        s << ", ";
    }
    
    s << "]";
}

bool CollectedIssuesNode::check() const {
    
    for (auto& I : Issues) {
        if (!I->check()) {
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

void CollectedSourceLocationsNode::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
    s << "[";
    
    for (auto& L : SourceLocs) {
        L.print(s);
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

bool MissingBecauseUnsafeCharacterEncodingNode::check() const {
    return false;
}

void MissingBecauseUnsafeCharacterEncodingNode::print(std::ostream& s) const {
    
    auto reason = unsafeCharacterEncodingReason(flag);
    
    SYMBOL_MISSING.print(s);
    s << "[";
    
    reason.print(s);
    
    s << "]";
}


SafeStringNode::SafeStringNode(BufferAndLength bufAndLen) : bufAndLen(bufAndLen) {}

Source SafeStringNode::getSource() const {
    
    assert(false);
    
    return Source();
}

void SafeStringNode::print(std::ostream& s) const {
    bufAndLen.print(s);
}


NodeContainer::NodeContainer(NodeSeq&& Nodes) : Nodes(Nodes) {}

Source NodeContainer::getSource() const {
    
    assert(false);
    
    return Source();
}

void NodeContainer::release() {
    Nodes.release();
}

void NodeContainer::print(std::ostream& s) const {
    Nodes.print(s);
}

bool NodeContainer::check() const {
    return Nodes.check();
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
    bufAndLen.put(session, callLink);
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
    return bufAndLen.toExpr(session);
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr NodeContainer::toExpr(ParserSessionPtr session) const {
    return Nodes.toExpr(session);
}
#endif // USE_EXPR_LIB
