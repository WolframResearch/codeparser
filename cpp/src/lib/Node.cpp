
#include "Node.h"

#include "Parser.h" // for TheParser
#include "ByteEncoder.h" // for ByteEncoder
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "Symbol.h"
#include "MyString.h"
#include "ParserSession.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <numeric> // for accumulate
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

#if USE_EXPR_LIB
struct ToExprVisitor {
    
    expr operator()(const NodePtr& N) { return N->toExpr(); }
    
    expr operator()(const Token& L) { return L.toExpr(); }
};
#endif // USE_EXPR_LIB

#if USE_MATHLINK
struct PutVisitor {
    
    MLINK mlp;
    
    PutVisitor(MLINK mlp) : mlp(mlp) {};
    
    void operator()(const NodePtr& N) { return N->put(mlp); }
    
    void operator()(const Token& L) { return L.put(mlp); }
};
#endif // USE_MATHLINK


NodeSeq::NodeSeq(size_t Size) : vec() {
    vec.reserve(Size);
}

bool NodeSeq::empty() const {
    return vec.empty();
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
    
    auto accum = std::accumulate(vec.begin(), vec.end(), true, [](bool a, const NodeVariant& b){ return a && std::visit(CheckVisitor{}, b); });

    return accum;
}


TriviaSeq::TriviaSeq() : vec() {}

void TriviaSeq::reset() {
    
    //
    // Just need to reset the global buffer to the buffer of the first token in the sequence
    //
    
    if (vec.empty()) {
        return;
    }
    
    auto& T = vec[0];
    
    TheByteBuffer->buffer = T.BufLen.buffer;
    TheByteDecoder->SrcLoc = T.Src.Start;
    
    vec.clear();
}

void TriviaSeq::append(Token N) {
    vec.push_back(N);
}

bool TriviaSeq::empty() const {
    return vec.empty();
}


Node::~Node() {}


OperatorNode::OperatorNode(Symbol Op, Symbol MakeSym, NodeSeq ChildrenIn) : Op(Op), MakeSym(MakeSym), Children(std::move(ChildrenIn)), Src(std::visit(GetSourceVisitor{}, Children.first()), std::visit(GetSourceVisitor{}, Children.last())) {
    
    assert(!Children.empty());
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


PrefixNode::PrefixNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_PrefixNodeCount++;
#endif // DIAGNOSTICS
}

BinaryNode::BinaryNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_BINARYNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_BinaryNodeCount++;
#endif // DIAGNOSTICS
}

InfixNode::InfixNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_INFIXNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_InfixNodeCount++;
#endif // DIAGNOSTICS
}

TernaryNode::TernaryNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_TERNARYNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_TernaryNodeCount++;
#endif // DIAGNOSTICS
}

PostfixNode::PostfixNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_POSTFIXNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_PostfixNodeCount++;
#endif // DIAGNOSTICS
}

PrefixBinaryNode::PrefixBinaryNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXBINARYNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_PrefixBinaryNodeCount++;
#endif // DIAGNOSTICS
}

GroupNode::GroupNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_GroupNodeCount++;
#endif // DIAGNOSTICS
}

CompoundNode::CompoundNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_COMPOUNDNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_CompoundNodeCount++;
#endif // DIAGNOSTICS
}

GroupMissingCloserNode::GroupMissingCloserNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_GroupMissingCloserNodeCount++;
#endif // DIAGNOSTICS
}

UnterminatedGroupNeedsReparseNode::UnterminatedGroupNeedsReparseNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE, std::move(Args)) {
    
#if DIAGNOSTICS
    Node_UnterminatedGroupNeedsReparseNodeCount++;
#endif // DIAGNOSTICS
}


CallNode::CallNode(NodeSeq HeadIn, NodeVariant BodyIn) : Head(std::move(HeadIn)), Body(std::move(BodyIn)), Src(std::visit(GetSourceVisitor{}, Head.first()), std::visit(GetSourceVisitor{}, Body)) {
    
    assert(!Head.empty());
    
#if DIAGNOSTICS
    Node_CallNodeCount++;
#endif // DIAGNOSTICS
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


SyntaxErrorNode::SyntaxErrorNode(Symbol Err, NodeSeq ChildrenIn) : Err(Err), Children(std::move(ChildrenIn)), Src(std::visit(GetSourceVisitor{}, Children.first()), std::visit(GetSourceVisitor{}, Children.last())) {
    
    assert(!Children.empty());

#if DIAGNOSTICS
    Node_SyntaxErrorNodeCount++;
#endif // DIAGNOSTICS
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
    
    s << Err.name();
    s << ", ";
        
    Children.print(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}


CollectedExpressionsNode::CollectedExpressionsNode(std::vector<NodeVariant> Exprs) : Exprs(std::move(Exprs)) {}

void CollectedExpressionsNode::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
    s << "[";
    
    for (auto& E : Exprs) {
        
        std::visit(PrintVisitor{s}, E);
        
        s << ", ";
    }
    
    s << "]";
}

bool CollectedExpressionsNode::check() const {
    
    auto accum = std::accumulate(Exprs.begin(), Exprs.end(), true, [](bool a, const NodeVariant& b){ return a && std::visit(CheckVisitor{}, b); });
    
    return accum;
}

Source CollectedExpressionsNode::getSource() const {

    assert(false);
    
    return Source();
}


CollectedIssuesNode::CollectedIssuesNode(IssuePtrSet Issues) : Issues(std::move(Issues)) {}

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
    
    auto accum = std::accumulate(Issues.begin(), Issues.end(), true, [](bool a, const IssuePtr& b){ return a && b->check(); });
    
    return accum;
}

Source CollectedIssuesNode::getSource() const {
    
    assert(false);
    
    return Source();
}


CollectedSourceLocationsNode::CollectedSourceLocationsNode(std::set<SourceLocation> SourceLocs) : SourceLocs(std::move(SourceLocs)) {}

bool CollectedSourceLocationsNode::check() const {
    return true;
}

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
        case UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE: {
            return STRING_UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE;
        }
        case UNSAFECHARACTERENCODING_STRAYSURROGATE: {
            return STRING_UNSAFECHARACTERENCODING_STRAYSURROGATE;
        }
        case UNSAFECHARACTERENCODING_BOM: {
            return STRING_UNSAFECHARACTERENCODING_BOM;
        }
        default: {
            assert(false);
            return STRING_UNSAFECHARACTERENCODING_UNKNOWN;
        }
    }
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

bool SafeStringNode::check() const {
    return true;
}

void SafeStringNode::print(std::ostream& s) const {
    bufAndLen.print(s);
}


NodeContainer::NodeContainer(std::vector<NodeVariant> N) : N(std::move(N)) {}

void NodeContainer::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
    s << "[";
    
    for (auto& NN : N) {
        
        std::visit(PrintVisitor{s}, NN);
        
        s << ", ";
    }
    
    s << "]";
}

bool NodeContainer::check() const {
    
    auto accum = std::accumulate(N.begin(), N.end(), true, [](bool a, const NodeVariant& b){ return a && std::visit(CheckVisitor{}, b); });
    
    return accum;
}


#if USE_MATHLINK
void NodeSeq::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(vec.size()))) {
        assert(false);
    }
    
    for (auto& C : vec) {
        
#if CHECK_ABORT
        if (TheParserSession->isAbort()) {
            SYMBOL__ABORTED.put(mlp);
            continue;
        }
#endif // CHECK_ABORT
        
        std::visit(PutVisitor{mlp}, C);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void OperatorNode::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, MakeSym.name(), 3)) {
        assert(false);
    }
    
    Op.put(mlp);
    
    Children.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void AbortNode::put(MLINK mlp) const {
    
    SYMBOL__ABORTED.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CallNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_CALLNODE.name(), 3)) {
        assert(false);
    }
        
    Head.put(mlp);
    
    std::visit(PutVisitor{mlp}, Body);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void SyntaxErrorNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_SYNTAXERRORNODE.name(), 3)) {
        assert(false);
    }
    
    Err.put(mlp);
    
    Children.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedExpressionsNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(Exprs.size()))) {
        assert(false);
    }
    
    for (auto& E : Exprs) {
        
        std::visit(PutVisitor{mlp}, E);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedIssuesNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(Issues.size()))) {
        assert(false);
    }
    
    for (auto& I : Issues) {
        
        I->put(mlp);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CollectedSourceLocationsNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(SourceLocs.size()))) {
        assert(false);
    }
    
    for (auto& L : SourceLocs) {
            
        L.put(mlp);
    }
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void MissingBecauseUnsafeCharacterEncodingNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_MISSING.name(), 1)) {
        assert(false);
    }
    
    auto reason = unsafeCharacterEncodingReason(flag);
    
    reason.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void SafeStringNode::put(MLINK mlp) const {
    bufAndLen.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void NodeContainer::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(N.size()))) {
        assert(false);
    }
    
    for (auto& NN : N) {
        
#if CHECK_ABORT
        if (TheParserSession->isAbort()) {
            SYMBOL__ABORTED.put(mlp);
            continue;
        }
#endif // CHECK_ABORT
        
        std::visit(PutVisitor{mlp}, NN);
    }
}
#endif // USE_MATHLINK


#if USE_EXPR_LIB
expr NodeSeq::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
    
    auto e = Expr_BuildExprA(head, static_cast<int>(vec.size()));
    
    for (size_t i = 0; i < vec.size(); i++) {
        
#if CHECK_ABORT
        if (TheParserSession->isAbort()) {
            Expr_InsertA(e, i + 1, SYMBOL__ABORTED.toExpr());
            continue;
        }
#endif // CHECK_ABORT
        
        auto& C = vec[i];
        auto CExpr = std::visit(ToExprVisitor{}, C);
        Expr_InsertA(e, i + 1, CExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr OperatorNode::toExpr() const {
    
    auto head = MakeSym.toExpr();
        
    auto e = Expr_BuildExprA(head, 3);
    
    auto OpExpr = Op.toExpr();
    Expr_InsertA(e, 0 + 1, OpExpr);
        
    auto ChildrenExpr = Children.toExpr();
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr AbortNode::toExpr() const {
    
    auto e = SYMBOL__ABORTED.toExpr();
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CallNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_CALLNODE.toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto HeadExpr = Head.toExpr();
    Expr_InsertA(e, 0 + 1, HeadExpr);
        
    auto BodyExpr = std::visit(ToExprVisitor{}, Body);
    Expr_InsertA(e, 1 + 1, BodyExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr SyntaxErrorNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_SYNTAXERRORNODE.toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto SymExpr = Err.toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto ChildrenExpr = Children.toExpr();
    Expr_InsertA(e, 1 + 1, ChildrenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedExpressionsNode::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
    
    auto e = Expr_BuildExprA(head, static_cast<int>(Exprs.size()));
    
    for (size_t i = 0; i < Exprs.size(); i++) {
        
        auto& NN = Exprs[i];
        auto NExpr = std::visit(ToExprVisitor{}, NN);
        Expr_InsertA(e, i + 1, NExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedIssuesNode::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
    
    auto e = Expr_BuildExprA(head, static_cast<int>(Issues.size()));
    
    int i = 0;
    for (auto& I : Issues) {
        
        auto IExpr = I->toExpr();
        Expr_InsertA(e, i + 1, IExpr);
        i++;
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr CollectedSourceLocationsNode::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
            
    auto e = Expr_BuildExprA(head, static_cast<int>(SourceLocs.size()));
    
    int i = 0;
    for (auto& L : SourceLocs) {
        
        auto LExpr = L.toExpr();
        Expr_InsertA(e, i + 1, LExpr);
        i++;
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
expr MissingBecauseUnsafeCharacterEncodingNode::toExpr() const {
    
    auto head = SYMBOL_MISSING.toExpr();
    
    auto e = Expr_BuildExprA(head, 1);
    
    auto reason = unsafeCharacterEncodingReason(flag);
    
    auto StrExpr = reason.toExpr();
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


#if USE_EXPR_LIB
expr NodeContainer::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
        
    auto e = Expr_BuildExprA(head, static_cast<int>(N.size()));
    
    for (size_t i = 0; i < N.size(); i++) {
        
        //
        // Check isAbort() inside loops
        //
#if CHECK_ABORT
        if (TheParserSession->isAbort()) {
            Expr_InsertA(e, i + 1, SYMBOL__ABORTED.toExpr());
            continue;
        }
#endif // CHECK_ABORT
        
        auto& NN = N[i];
        auto NExpr = std::visit(ToExprVisitor{}, NN);
        Expr_InsertA(e, i + 1, NExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB
