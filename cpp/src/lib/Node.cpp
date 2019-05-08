
#include "Node.h"

Source Node::getSourceSpan() const {
    
    auto Args = getChildren();
    
    if (!Args.empty()) {
        auto First = Args[0];
        auto Last = Args[Args.size()-1];
        return Source(First->getSourceSpan().lines.start, Last->getSourceSpan().lines.end);
    } else {
        return Source({0, 0}, {0, 0});
    }
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

void SymbolNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_SYMBOLNODE->name(), 3);

    SYMBOL_SYMBOL->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void StringNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_STRINGNODE->name(), 3);
    
    SYMBOL_STRING->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void IntegerNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_INTEGERNODE->name(), 3);

    SYMBOL_INTEGER->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void RealNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_REALNODE->name(), 3);

    SYMBOL_REAL->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void SlotNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_SLOTNODE->name(), 3);

    SYMBOL_SLOT->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void SlotSequenceNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_SLOTSEQUENCENODE->name(), 3);

    SYMBOL_SLOTSEQUENCE->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void OutNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_OUTNODE->name(), 3);

    SYMBOL_OUT->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void OptionalDefaultNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_OPTIONALDEFAULTNODE->name(), 3);

    SYMBOL_OPTIONALDEFAULT->put(mlp);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void TokenNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_TOKENNODE->name(), 3);

    MLPutSymbol(mlp, TokenToString(Tok.Tok).c_str());

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tok.Str.c_str()), static_cast<int>(Tok.Str.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void InternalAllNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_INTERNALALLNODE->name(), 3);

    SYMBOL_ALL->put(mlp);

    MLPutUTF8String(mlp, nullptr, 0);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void InternalNullNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_INTERNALNULLNODE->name(), 3);

    SYMBOL_NULL->put(mlp);

    MLPutUTF8String(mlp, nullptr, 0);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
}

void InternalOneNode::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_INTERNALONENODE->name(), 3);

    MLPutInteger(mlp, 1);

    MLPutUTF8String(mlp, nullptr, 0);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    Span.putSourceRule(mlp);
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

    Head->put(mlp);

    putChildren(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1 );

    getSourceSpan().putSourceRule(mlp);
}

Source CallNode::getSourceSpan() const {

    auto Body = getChildren()[0];

    return Source(Head->getSourceSpan().lines.start, Body->getSourceSpan().lines.end);
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
// Aggregate nodes
//

void CollectedExpressionsNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Exprs.size()));
    
    for (auto E : Exprs) {
        E->put(mlp);
    }
}

void CollectedCommentsNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Comments.size()));
    
    for (auto C : Comments) {
        C.putComment(mlp);
    }
}

void CollectedSyntaxIssuesNode::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(Issues.size()));
    
    for (auto I : Issues) {
        I.put(mlp);
    }
}






