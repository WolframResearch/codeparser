
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


NodeSeq::NodeSeq(size_t Size) : vec() {
    vec.reserve(Size);
}

bool NodeSeq::empty() const {
    return vec.empty();
}

const NodePtr& NodeSeq::first() const {

    assert(!vec.empty());
    
    return vec.front();
}

const NodePtr& NodeSeq::last() const {

    assert(!vec.empty());
    
    return vec.back();
}

void NodeSeq::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
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


TriviaSeq::TriviaSeq() : vec() {
    vec.reserve(2);
}

void TriviaSeq::reset() {
    
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
    
    vec.clear();
}

void TriviaSeq::append(LeafNodePtr N) {
    vec.push_back(std::move(N));
}

bool TriviaSeq::empty() const {
    return vec.empty();
}


Node::~Node() {}


OperatorNode::OperatorNode(Symbol Op, Symbol MakeSym, NodeSeq ChildrenIn) : Op(Op), MakeSym(MakeSym), Children(std::move(ChildrenIn)) {
    
    assert(!Children.empty());
    
    auto& First = Children.first();
    auto& Last = Children.last();

    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();

    Src = Source(FirstSrc, LastSrc);
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


LeafNode::LeafNode(Token Tok) : Tok(Tok) {}

Source LeafNode::getSource() const {
    return Tok.Src;
}

Token LeafNode::getToken() const {
    return Tok;
}

bool LeafNode::check() const {
    return true;
}

void LeafNode::print(std::ostream& s) const {
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    SYMBOL_CODEPARSER_LEAFNODE.print(s);
    s << "[";
    
    s << Sym.name();
    s << ", ";
    
    Tok.BufLen.print(s);
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
}


ErrorNode::ErrorNode(Token Tok) : Tok(Tok) {
    assert(Tok.Tok.isError());
    assert(!Tok.Tok.isUnterminated());
}

Token ErrorNode::getToken() const {
    return Tok;
}

Source ErrorNode::getSource() const {
    return Tok.Src;
}

bool ErrorNode::check() const {
    return false;
}

void ErrorNode::print(std::ostream& s) const {
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    SYMBOL_CODEPARSER_ERRORNODE.print(s);
    s << "[";
    
    s << Sym.name();
    s << ", ";
    
    Tok.BufLen.print(s);
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
}


UnterminatedTokenErrorNeedsReparseNode::UnterminatedTokenErrorNeedsReparseNode(Token Tok) : Tok(Tok) {
    assert(Tok.Tok.isUnterminated());
}

Source UnterminatedTokenErrorNeedsReparseNode::getSource() const {
    return Tok.Src;
}

bool UnterminatedTokenErrorNeedsReparseNode::check() const {
    return false;
}

void UnterminatedTokenErrorNeedsReparseNode::print(std::ostream& s) const {
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.print(s);
    s << "[";
    
    s << Sym.name();
    s << ", ";
    
    Tok.BufLen.print(s);
    s << ", ";
    
    Tok.Src.print(s);
    
    s << "]";
}


bool GroupMissingCloserNode::check() const {
    return false;
}


bool UnterminatedGroupNeedsReparseNode::check() const {
    return false;
}


PrefixNode::PrefixNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXNODE, std::move(Args)) {}

BinaryNode::BinaryNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_BINARYNODE, std::move(Args)) {}

InfixNode::InfixNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_INFIXNODE, std::move(Args)) {}

TernaryNode::TernaryNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_TERNARYNODE, std::move(Args)) {}

PostfixNode::PostfixNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_POSTFIXNODE, std::move(Args)) {}

PrefixBinaryNode::PrefixBinaryNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_PREFIXBINARYNODE, std::move(Args)) {}

GroupNode::GroupNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPNODE, std::move(Args)) {}

CompoundNode::CompoundNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_COMPOUNDNODE, std::move(Args)) {}

GroupMissingCloserNode::GroupMissingCloserNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE, std::move(Args)) {}

UnterminatedGroupNeedsReparseNode::UnterminatedGroupNeedsReparseNode(Symbol Op, NodeSeq Args) : OperatorNode(Op, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE, std::move(Args)) {}


CallNode::CallNode(NodeSeq HeadIn, NodePtr BodyIn) : Head(std::move(HeadIn)), Body(std::move(BodyIn)) {
    
    assert(!Head.empty());
    
    const auto& First = Head.first();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Body->getSource();
    
    Src = Source(FirstSrc, LastSrc);
}

Source CallNode::getSource() const {
    return Src;
}

void CallNode::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CALLNODE.print(s);
    s << "[";
    
    Head.print(s);
    s << ", ";
    
    Body->print(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}

bool CallNode::check() const {
    return Head.check() && Body->check();
}


SyntaxErrorNode::SyntaxErrorNode(Symbol Err, NodeSeq ChildrenIn) : Err(Err), Children(std::move(ChildrenIn)) {
    
    assert(!Children.empty());

    auto& First = Children.first();
    auto& Last = Children.last();

    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();

    Src = Source(FirstSrc, LastSrc);
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


CollectedExpressionsNode::CollectedExpressionsNode(std::vector<NodePtr> Exprs) : Exprs(std::move(Exprs)) {}

void CollectedExpressionsNode::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
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



#if USE_MATHLINK
void NodeSeq::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(vec.size()))) {
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
void LeafNode::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LEAFNODE.name(), 3)) {
        assert(false);
    }

    auto Sym = TokenToSymbol(Tok.Tok);

    Sym.put(mlp);

    Tok.BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Tok.Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void ErrorNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_ERRORNODE.name(), 3)) {
        assert(false);
    }
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    Sym.put(mlp);
    
    Tok.BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Tok.Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void UnterminatedTokenErrorNeedsReparseNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.name(), 3)) {
        assert(false);
    }
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    Sym.put(mlp);
    
    Tok.BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Tok.Src.put(mlp);
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void CallNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_CALLNODE.name(), 3)) {
        assert(false);
    }
        
    Head.put(mlp);
    
    Body->put(mlp);
    
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
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(Issues.size()))) {
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
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(SourceLocs.size()))) {
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



#if USE_EXPR_LIB
expr NodeSeq::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
    
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
expr LeafNode::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_LEAFNODE.toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Sym.toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = Tok.BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
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
    
    auto head = SYMBOL_CODEPARSER_ERRORNODE.toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Sym.toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = Tok.BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
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
    
    auto head = SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto Sym = TokenToSymbol(Tok.Tok);
    
    auto SymExpr = Sym.toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = Tok.BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
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
    
    auto head = SYMBOL_CODEPARSER_CALLNODE.toExpr();
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto HeadExpr = Head.toExpr();
    Expr_InsertA(e, 0 + 1, HeadExpr);
        
    auto BodyExpr = Body->toExpr();
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
    
    auto head = SYMBOL_LIST.toExpr();
    
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
    
    auto head = SYMBOL_LIST.toExpr();
            
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
