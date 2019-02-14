
#include "Node.h"

void Node::putASTArgs(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_LIST.name().c_str(), Args.size());

    for (auto A : Args) {
        A->put(mlp);
    }
}

void Node::putSyntaxIssues(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_RULE.name().c_str(), 2);

    SYMBOL_SYNTAXISSUES.put(mlp);

    MLPutFunction(mlp, SYMBOL_LIST.name().c_str(), Issues.size());

    for (auto I : Issues) {
        I.put(mlp);
    }
}

//
// Atom and Atom-like expressions
//

void SymbolNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SYMBOLNODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void StringNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_STRINGNODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void NumberNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_NUMBERNODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void SlotNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SLOTNODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void SlotSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SLOTSEQUENCENODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void OutNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_OUTNODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

//
// Base operator expressions
//

void PrefixNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PREFIXNODE.name().c_str(), 3);

    MLPutSymbol(mlp, Op.name().c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan PrefixNode::getSourceSpan() {

    auto Operand = getOperand();

    return SourceSpan{TokSpan.start, Operand->getSourceSpan().end};
}

void BinaryNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_BINARYNODE.name().c_str(), 3);

    MLPutSymbol(mlp, Op.name().c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan BinaryNode::getSourceSpan() {

    auto Left = getLeft();
    auto Right = getRight();

    return SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end};
}

void InfixNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INFIXNODE.name().c_str(), 3);

    MLPutSymbol(mlp, Op.name().c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan InfixNode::getSourceSpan() {

    auto Args = getArgs();
    
    if (!Args.empty()) {
        auto First = Args[0];
        auto Last = Args[Args.size()-1];
        return SourceSpan{First->getSourceSpan().start, Last->getSourceSpan().end};
    } else {
        return SourceSpan{{0, 0}, {0, 0}};
    }
}

void TernaryNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_TERNARYNODE.name().c_str(), 3);

    MLPutSymbol(mlp, Op.name().c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan TernaryNode::getSourceSpan() {

    auto Left = getLeft();
    auto Right = getRight();

    return SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end};
}

void PostfixNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_POSTFIXNODE.name().c_str(), 3);

    MLPutSymbol(mlp, Op.name().c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan PostfixNode::getSourceSpan() {

    auto Operand = getOperand();

    return SourceSpan{Operand->getSourceSpan().start, TokSpan.end};
}



//
// CallNodes
//

void CallNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_CALLNODE.name().c_str(), 3);

    Head->put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan CallNode::getSourceSpan() {

    auto Body = std::dynamic_pointer_cast<GroupNode>(getArgs()[0]);

    return SourceSpan{Head->getSourceSpan().start, Body->getSourceSpan().end};
}

void CallMissingCloserNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_CALLMISSINGCLOSERNODE.name().c_str(), 3);

    Head->put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan CallMissingCloserNode::getSourceSpan() {

    auto Body = std::dynamic_pointer_cast<GroupNode>(getArgs()[0]);
    
    return SourceSpan{Head->getSourceSpan().start, Body->getSourceSpan().end };
}



//
// GroupNode
//

void GroupNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_GROUPNODE.name().c_str(), 3);

    MLPutSymbol(mlp, Op.name().c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan GroupNode::getSourceSpan() {
    return SourceSpan{OpenerTokSpan.start, CloserTokSpan.end};
}


//
// Special expressions
//

void BlankNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_BLANKNODE.name().c_str(), 3);

    SYMBOL_BLANK.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void BlankSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_BLANKSEQUENCENODE.name().c_str(), 3);

    SYMBOL_BLANKSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void BlankNullSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_BLANKNULLSEQUENCENODE.name().c_str(), 3);

    SYMBOL_BLANKNULLSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void OptionalDefaultNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_OPTIONALDEFAULTNODE.name().c_str(), 3);

    SYMBOL_OPTIONALDEFAULT.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void PatternBlankNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKNODE.name().c_str(), 3);

    SYMBOL_PATTERNBLANK.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void PatternBlankSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKSEQUENCENODE.name().c_str(), 3);

    SYMBOL_PATTERNBLANKSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void PatternBlankNullSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKNULLSEQUENCENODE.name().c_str(), 3);

    SYMBOL_PATTERNBLANKNULLSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void OptionalDefaultPatternNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_OPTIONALDEFAULTPATTERNNODE.name().c_str(), 3);

    SYMBOL_OPTIONALDEFAULTPATTERN.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalTokenNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALTOKENNODE.name().c_str(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalAllNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALALLNODE.name().c_str(), 3);

    SYMBOL_ALL.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalDotNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALDOTNODE.name().c_str(), 3);

    SYMBOL_DOT.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalNullNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALNULLNODE.name().c_str(), 3);

    SYMBOL_NULL.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalOneNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALONENODE.name().c_str(), 3);

    MLPutInteger(mlp, 1);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}


void SyntaxErrorNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SYNTAXERRORNODE.name().c_str(), 3);

    MLPutSymbol(mlp, TokenToString(Tok).c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name().c_str(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

SourceSpan SyntaxErrorNode::getSourceSpan() {

    auto Args = getArgs();
    
    if (!Args.empty()) {
        auto First = Args[0];
        auto Last = Args[Args.size()-1];
        return SourceSpan{First->getSourceSpan().start, Last->getSourceSpan().end};
    } else {
        return SourceSpan{{0, 0}, {0, 0}};
    }
}


