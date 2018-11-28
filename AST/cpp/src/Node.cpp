
#include "Node.h"

#include "Utils.h"
#include "Symbol.h"
#include "ToInputFormString.h"

#include <cassert>
#include <iostream>
#include <memory>

std::string Node::ASTArgsString() {
    std::ostringstream ss;
    ss << "{";
    if (!Args.empty()) {
        auto I = Args.begin();
        auto LastIt = Args.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I)->string();
            ss << ", ";
        }
        ss << (*I)->string();
    }
    ss << "}";
    return ss.str();
}

//
// Atom and Atom-like expressions
//

std::string SymbolNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_SYMBOLNODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string StringNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_STRINGNODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string NumberNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_NUMBERNODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string SyntaxErrorNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_SYNTAXERRORNODE.name();
    ss << "[";
    ss << TokenToString(Tok);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
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

std::string BlankNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_BLANKNODE.name();
    ss << "[";
    ss << SYMBOL_BLANK.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string BlankSequenceNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_BLANKSEQUENCENODE.name();
    ss << "[";
    ss << SYMBOL_BLANKSEQUENCE.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string BlankNullSequenceNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_BLANKNULLSEQUENCENODE.name();
    ss << "[";
    ss << SYMBOL_BLANKNULLSEQUENCE.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string PatternBlankNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_PATTERNBLANKNODE.name();
    ss << "[";
    ss << SYMBOL_PATTERNBLANK.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string PatternBlankSequenceNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_PATTERNBLANKSEQUENCENODE.name();
    ss << "[";
    ss << SYMBOL_PATTERNBLANKSEQUENCE.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string PatternBlankNullSequenceNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_PATTERNBLANKNULLSEQUENCENODE.name();
    ss << "[";
    ss << SYMBOL_PATTERNBLANKNULLSEQUENCE.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string OptionalDefaultNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_OPTIONALDEFAULTNODE.name();
    ss << "[";
    ss << SYMBOL_OPTIONALDEFAULT.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string SlotNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_SLOTNODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string SlotSequenceNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_SLOTSEQUENCENODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string OutNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_OUTNODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string InternalEmptyNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_INTERNALEMPTYNODE.name();
    ss << "[";
    ss << SYMBOL_INTERNALEMPTY.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}


//
// Base operator expressions
//

std::string PrefixNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_PREFIXNODE.name();
    ss << "[";
    ss << Op.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan PrefixNode::getSourceSpan() {

    auto Operand = getOperand();

    return SourceSpan{TokSpan.start, Operand->getSourceSpan().end};
}

std::string BinaryNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_BINARYNODE.name();
    ss << "[";
    ss << Op.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan BinaryNode::getSourceSpan() {

    auto Left = getLeft();
    auto Right = getRight();

    return SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end};
}

std::string InfixNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_INFIXNODE.name();
    ss << "[";
    ss << Op.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
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

std::string TernaryNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_TERNARYNODE.name();
    ss << "[";
    ss << Op.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan TernaryNode::getSourceSpan() {

    auto Left = getLeft();
    auto Right = getRight();

    return SourceSpan{Left->getSourceSpan().start, Right->getSourceSpan().end};
}

std::string PostfixNode::string() {

    std::ostringstream ss;
    ss << SYMBOL_POSTFIXNODE.name();
    ss << "[";
    ss << Op.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (Op == SYMBOL_DERIVATIVE) {
        ss << ", ";
        ss << SYMBOL_DERIVATIVEORDER.name();
        ss << "->";
        ss << DerivativeOrder;
    }
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan PostfixNode::getSourceSpan() {

    auto Operand = getOperand();

    return SourceSpan{Operand->getSourceSpan().start, TokSpan.end};
}



//
// Group Expressions
//

std::string CallNode::string() {

    auto Args = getArgs();

    std::ostringstream ss;
    auto ArgsGroup = std::make_shared<GroupNode>(SYMBOL_GROUPSQUARE, OpenerTokSpan, CloserTokSpan, Args, std::vector<SyntaxIssue>());
    ss << SYMBOL_CALLNODE.name();
    ss << "[";
    ss << Head->string();
    ss << ", ";
    ss << ArgsGroup->ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan CallNode::getSourceSpan() {

    auto Args = getArgs();

    auto ArgsGroup = std::make_shared<GroupNode>(SYMBOL_GROUPSQUARE, OpenerTokSpan, CloserTokSpan, Args, std::vector<SyntaxIssue>());
    return SourceSpan{Head->getSourceSpan().start, ArgsGroup->getSourceSpan().end};
}

std::string PartNode::string() {

    auto Args = getArgs();

    std::ostringstream ss;
    auto ArgsGroup = std::make_shared<GroupNode>(SYMBOL_GROUPDOUBLEBRACKET, OpenerTokSpan, CloserTokSpan, Args, std::vector<SyntaxIssue>());
    ss << SYMBOL_PARTNODE.name();
    ss << "[";
    ss << Head->string();
    ss << ", ";
    ss << ArgsGroup->ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan PartNode::getSourceSpan() {

    auto Args = getArgs();

    auto ArgsGroup = std::make_shared<GroupNode>(SYMBOL_GROUPDOUBLEBRACKET, OpenerTokSpan, CloserTokSpan, Args, std::vector<SyntaxIssue>());
    return SourceSpan{Head->getSourceSpan().start, ArgsGroup->getSourceSpan().end};
}

std::string GroupNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_GROUPNODE.name();
    ss << "[";
    ss << Op.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    if (!Issues.empty()) {
        ss << ", ";
        ss << SYMBOL_SYNTAXISSUES.name();
        ss << "->{";
        auto I = Issues.begin();
        auto LastIt = Issues.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I).string();
            ss << ", ";
        }
        ss << (*I).string();
        ss << "}";
    }
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan GroupNode::getSourceSpan() {
    return SourceSpan{OpenerTokSpan.start, CloserTokSpan.end};
}


//
// Special expressions
//

std::string InternalMinusNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_INTERNALMINUSNODE.name();
    ss << "[";
    ss << SYMBOL_INTERNALMINUS.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}

SourceSpan InternalMinusNode::getSourceSpan() {
    
    auto Operand = getOperand();

    return Operand->getSourceSpan();
}

std::string InternalTokenNode::string() {
    std::ostringstream ss;
    ss << SYMBOL_INTERNALTOKENNODE.name();
    ss << "[";
    ss << stringEscape(Str);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}


std::string FileNode::string() {

    auto Args = getArgs();

    std::ostringstream ss;
    ss << SYMBOL_FILENODE.name();
    ss << "[";
    ss << SYMBOL_FILE.name();
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << ASTSourceString(getSourceSpan());
    ss << "|>";
    ss << "]";
    return ss.str();
}


SourceSpan FileNode::getSourceSpan() {

    auto Args = getArgs();

    if (!Args.empty()) {
        auto First = Args[0];
        auto Last = Args[Args.size()-1];
        return SourceSpan{First->getSourceSpan().start, Last->getSourceSpan().end};
    } else {
        return SourceSpan{{0, 0}, {0, 0}};
    }
}


