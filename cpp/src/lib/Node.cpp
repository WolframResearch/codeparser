
#include "Node.h"

#include "Parser.h" // for TheParser
#include "ByteEncoder.h" // for ByteEncoder
#include "API.h" // for TheParserSession
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer

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

bool NodeSeq::check() const {
    
    auto accum = std::accumulate(vec.begin(), vec.end(), true, [](bool a, const NodePtr& b){ return a && b->check(); });
    
    return accum;
}


void LeafSeq::print0(std::ostream& s) const {
    
    for (auto& C : vec) {
        C->print(s);
        s << ", ";
    }
}

LeafSeq::~LeafSeq() {
    
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

const Node* Node::first() const {
    return this;
}

const Node* Node::last() const {
    return this;
}

Token Node::lastToken() const {
    
    assert(!Children.empty());
    
    auto Last = Children.last();
    
    return Last->lastToken();
}

void Node::printChildren(std::ostream& s) const {
    
    Children.print(s);
}

bool Node::check() const {
    return Children.check();
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

void NodeSeqNode::print(std::ostream& s) const {
    
    Children.print0(s);
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


void UnterminatedTokenErrorNeedsReparseNode::print(std::ostream& s) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        auto& Sym = TokenToSymbol(Tok.Tok);
        
        s << SYMBOL_CODEPARSER_LIBRARY_MAKEUNTERMINATEDTOKENERRORNEEDSREPARSENODE->name() << "[";
        
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
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEUNTERMINATEDTOKENERRORNEEDSREPARSENODE->name() << "[";
    
    s << Sym->name();
    s << ", ";
    
    if (!Tok.Tok.isEmpty()) {
        
        Tok.BufLen.printUTF8String(s);
    }
    
    s << "]";
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

Source CallNode::getSource() const {
    
    const auto& First = Head.first();
    
    const auto& Children = getChildrenSafe();
    const auto& Last = Children.last();
    
    auto FirstSrc = First->getSource();
    auto LastSrc = Last->getSource();
    
    return Source(FirstSrc, LastSrc);
}

bool CallNode::check() const {
    return Children.check() && Head.check();
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

void CollectedExpressionsNode::print(std::ostream& s) const {
    
    s << "List[";
    
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
    
    s << "List[";
    
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
    
    s << "List[";
    
    for (auto& L : SourceLocs) {
        L.print(s);
        s << ", ";
    }
    
    s << "]";
}


void MissingBecauseUnsafeCharacterEncodingNode::print(std::ostream& s) const {
    switch (flag) {
        case UNSAFECHARACTERENODING_INCOMPLETEUTF8SEQUENCE:
            s << "Missing[\"UnsafeCharacterEncoding_IncompleteUTF8Sequence\"]";
            break;
        case UNSAFECHARACTERENODING_STRAYSURROGATE:
            s << "Missing[\"UnsafeCharacterEncoding_StraySurrogate\"]";
            break;
        case UNSAFECHARACTERENODING_BOM:
            s << "Missing[\"UnsafeCharacterEncoding_BOM\"]";
            break;
        default:
            assert(false);
            s << "Missing[\"UnsafeCharacterEncoding_UNKNOWN\"]";
            break;
    }
}


void SourceCharacterNode::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKESOURCECHARACTERNODE->name() << "[";
    
    s << SYMBOL_CODEPARSER_SOURCECHARACTER->name() << ", ";
    
    s << Char;
    
    s << "]\n";
}

void SafeStringNode::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKESAFESTRINGNODE->name() << "[";
    
    bufAndLen.printUTF8String(s);
    
    s << "]\n";
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

void UnterminatedTokenErrorNeedsReparseNode::put(MLINK mlp) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEUNTERMINATEDTOKENERRORNEEDSREPARSENODE->name(), static_cast<int>(2 + 4))) {
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
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEUNTERMINATEDTOKENERRORNEEDSREPARSENODE->name(), static_cast<int>(2))) {
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
            
        L.putStructured(mlp);
    }
}

void MissingBecauseUnsafeCharacterEncodingNode::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_MISSING->name(), 1)) {
        assert(false);
    }
    
    std::string reason;
    
    switch (flag) {
        case UNSAFECHARACTERENODING_INCOMPLETEUTF8SEQUENCE:
            reason = "UnsafeCharacterEncoding_IncompleteUTF8Sequence";
            break;
        case UNSAFECHARACTERENODING_STRAYSURROGATE:
            reason = "UnsafeCharacterEncoding_StraySurrogate";
            break;
        case UNSAFECHARACTERENODING_BOM:
            reason = "UnsafeCharacterEncoding_BOM";
            break;
        default:
            assert(false);
            reason = "UnsafeCharacterEncoding_UNKNOWN";
            break;
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(reason.c_str()), static_cast<int>(reason.size()))) {
        assert(false);
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
    
    if (!MLPutUTF8String(mlp, bufAndLen.buffer, static_cast<int>(bufAndLen.length()))) {
        assert(false);
    }
}

#endif // USE_MATHLINK
