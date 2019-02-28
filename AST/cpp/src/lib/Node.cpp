
#include "Node.h"

void Node::putASTArgs(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_LIST.name(), Args.size());

    for (auto A : Args) {
        A->put(mlp);
    }
}

void Node::putSyntaxIssues(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_RULE.name(), 2);

    SYMBOL_SYNTAXISSUES.put(mlp);

    MLPutFunction(mlp, SYMBOL_LIST.name(), Issues.size());

    for (auto I : Issues) {
        I.put(mlp);
    }
}

//
// Atom and Atom-like expressions
//

void SymbolNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SYMBOLNODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void StringNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_STRINGNODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void NumberNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_NUMBERNODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void SlotNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SLOTNODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void SlotSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SLOTSEQUENCENODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void OutNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_OUTNODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_PREFIXNODE.name(), 3);

    MLPutSymbol(mlp, Op.name());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_BINARYNODE.name(), 3);

    MLPutSymbol(mlp, Op.name());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_INFIXNODE.name(), 3);

    MLPutSymbol(mlp, Op.name());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_TERNARYNODE.name(), 3);

    MLPutSymbol(mlp, Op.name());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_POSTFIXNODE.name(), 3);

    MLPutSymbol(mlp, Op.name());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_CALLNODE.name(), 3);

    Head->put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_CALLMISSINGCLOSERNODE.name(), 3);

    Head->put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_GROUPNODE.name(), 3);

    MLPutSymbol(mlp, Op.name());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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

    MLPutFunction(mlp, SYMBOL_BLANKNODE.name(), 3);

    SYMBOL_BLANK.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void BlankSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_BLANKSEQUENCENODE.name(), 3);

    SYMBOL_BLANKSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void BlankNullSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_BLANKNULLSEQUENCENODE.name(), 3);

    SYMBOL_BLANKNULLSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void OptionalDefaultNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_OPTIONALDEFAULTNODE.name(), 3);

    SYMBOL_OPTIONALDEFAULT.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void PatternBlankNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKNODE.name(), 3);

    SYMBOL_PATTERNBLANK.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void PatternBlankSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKSEQUENCENODE.name(), 3);

    SYMBOL_PATTERNBLANKSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void PatternBlankNullSequenceNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_PATTERNBLANKNULLSEQUENCENODE.name(), 3);

    SYMBOL_PATTERNBLANKNULLSEQUENCE.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void OptionalDefaultPatternNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_OPTIONALDEFAULTPATTERNNODE.name(), 3);

    SYMBOL_OPTIONALDEFAULTPATTERN.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalTokenNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALTOKENNODE.name(), 3);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), Str.size());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalAllNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALALLNODE.name(), 3);

    SYMBOL_ALL.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalDotNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALDOTNODE.name(), 3);

    SYMBOL_DOT.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalNullNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALNULLNODE.name(), 3);

    SYMBOL_NULL.put(mlp);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}

void InternalOneNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_INTERNALONENODE.name(), 3);

    MLPutInteger(mlp, 1);

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

    getSourceSpan().putSourceRule(mlp);

    if (!Issues.empty()) {
        putSyntaxIssues(mlp);
    }
}


void SyntaxErrorNode::put(MLINK mlp) {

    auto Issues = getIssues();

    MLPutFunction(mlp, SYMBOL_SYNTAXERRORNODE.name(), 3);

    MLPutSymbol(mlp, TokenToString(Tok).c_str());

    putASTArgs(mlp);

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1 + ((!Issues.empty()) ? 1 : 0));

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


