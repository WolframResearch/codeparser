
#include "Node.h"



void NodeSeq::push_back(NodePtr N) {
    
    vector.push_back(N);
}

void NodeSeq::push_back(NodeSeq Args) {
    push_back(Args.vector);
}

void NodeSeq::push_back(std::vector<NodePtr> V) {
    
    for (auto N : V) {
        push_back(N);
    }
}

NodePtr NodeSeq::main() const {
    
    for (auto N : vector) {
        
        if (auto NLeaf = std::dynamic_pointer_cast<const LeafNode>(N)) {
            
            auto Tok = NLeaf->getToken();
            if (Tok.Tok == TOKEN_COMMENT) {
                continue;
            }
            if (Tok.Tok == TOKEN_WHITESPACE) {
                continue;
            }
            if (Tok.Tok == TOKEN_NEWLINE) {
                continue;
            }
            
            return N;
        }
        
        return N;
    }
    
    assert(false);
    
    return nullptr;
}

NodePtr NodeSeq::last() const {
    
    assert(!vector.empty());
    
    auto i = vector.end();
    while (i != vector.begin()) {
        
        --i;
        
        auto N = *i;
        
        if (auto NLeaf = std::dynamic_pointer_cast<const LeafNode>(N)) {
            
            auto Tok = NLeaf->getToken();
            if (Tok.Tok == TOKEN_COMMENT) {
                continue;
            }
            if (Tok.Tok == TOKEN_WHITESPACE) {
                continue;
            }
            if (Tok.Tok == TOKEN_NEWLINE) {
                continue;
            }
        }
        
        return N;
    }
    
    return nullptr;
}

void NodeSeq::clear() {
    vector.clear();
}


Source Node::getSourceSpan() const {
    
    auto Args = getChildren();
    
    if (!Args.empty()) {
        
        auto First = Args[0];
        auto Last = Args[Args.size()-1];
        
        return Source(First->getSourceSpan().lines.start, Last->getSourceSpan().lines.end);
    }
    
    return Source();
}

void Node::putChildren(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Children.size()));

    for (auto C : Children) {
        C->put(mlp);
    }
}

//
// Literal nodes
//

void LeafNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_LEAFNODE->name(), 3);

    MLPutSymbol(mlp, TokenToSymbol(Tok.Tok)->name());

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tok.Str.c_str()), static_cast<int>(Tok.Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Tok.Span.putSourceRule(mlp);
}




//
// Base operator nodes
//

void PrefixNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_PREFIXNODE->name(), 3);

    MLPutSymbol(mlp, Op->name());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void BinaryNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_BINARYNODE->name(), 3);

    MLPutSymbol(mlp, Op->name());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void InfixNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_INFIXNODE->name(), 3);

    MLPutSymbol(mlp, Op->name());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void TernaryNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_TERNARYNODE->name(), 3);

    MLPutSymbol(mlp, Op->name());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void PostfixNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_POSTFIXNODE->name(), 3);

    MLPutSymbol(mlp, Op->name());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}




//
// CallNodes
//

void CallNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_CALLNODE->name(), 3);

    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Head.size()));
    
    for (auto H : Head) {
        H->put(mlp);
    }
    
    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

Source CallNode::getSourceSpan() const {

    auto FirstHead = Head[0];
    
    auto Children = getChildren();
    auto LastChild = Children[Children.size()-1];

    return Source(FirstHead->getSourceSpan().lines.start, LastChild->getSourceSpan().lines.end);
}



//
// GroupNode
//

void GroupNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_GROUPNODE->name(), 3);

    MLPutSymbol(mlp, Op->name());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}


//
// Special nodes
//

void StartOfLineNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_STARTOFLINENODE->name(), 3);
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );
    
    getSourceSpan().putSourceRule(mlp);
}

void BlankNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_BLANKNODE->name(), 3);

    SYMBOL_BLANK->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void BlankSequenceNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_BLANKSEQUENCENODE->name(), 3);

    SYMBOL_BLANKSEQUENCE->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void BlankNullSequenceNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_BLANKNULLSEQUENCENODE->name(), 3);

    SYMBOL_BLANKNULLSEQUENCE->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void PatternBlankNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKNODE->name(), 3);

    SYMBOL_PATTERNBLANK->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void PatternBlankSequenceNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKSEQUENCENODE->name(), 3);

    SYMBOL_PATTERNBLANKSEQUENCE->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void PatternBlankNullSequenceNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKNULLSEQUENCENODE->name(), 3);

    SYMBOL_PATTERNBLANKNULLSEQUENCE->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void OptionalDefaultPatternNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_OPTIONALDEFAULTPATTERNNODE->name(), 3);

    SYMBOL_OPTIONALDEFAULTPATTERN->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}


//
// Error nodes
//

void SyntaxErrorNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_SYNTAXERRORNODE->name(), 3);

    MLPutSymbol(mlp, SyntaxErrorToString(Err).c_str());

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

void GroupMissingCloserNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_GROUPMISSINGCLOSERNODE->name(), 3);
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );
    
    getSourceSpan().putSourceRule(mlp);
}

void GroupMissingOpenerNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_GROUPMISSINGOPENERNODE->name(), 3);
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );
    
    getSourceSpan().putSourceRule(mlp);
}


void PrefixBinaryNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_PREFIXBINARYNODE->name(), 3);
    
    MLPutSymbol(mlp, Op->name());
    
    putChildren(mlp);
    
    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );
    
    getSourceSpan().putSourceRule(mlp);
}



//
// Collection nodes
//

void CollectedExpressionsNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Exprs.size()));
    
    for (auto E : Exprs) {
        E->put(mlp);
    }
}

void CollectedSyntaxIssuesNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Issues.size()));
    
    for (auto I : Issues) {
        I.put(mlp);
    }
}

void CollectedMetadatasNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Metadatas.size()));
    
    for (auto M : Metadatas) {
        M.put(mlp);
    }
}







