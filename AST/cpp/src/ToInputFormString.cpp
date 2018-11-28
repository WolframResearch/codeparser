
#include "Node.h"

#include <cassert>

std::string SymbolNode::inputform() {
    return Str;
}

std::string StringNode::inputform() {
    return Str;
}

std::string NumberNode::inputform() {
    return Str;
}

std::string SyntaxErrorNode::inputform() {
    std::ostringstream ss;
    ss << SYMBOL_SYNTAXERRORNODE.name();
    ss << "[";
    ss << TokenToString(Tok);
    ss << ", ";
    ss << ASTArgsString();
    ss << ", <|";
    ss << "|>";
    ss << "]";
    return ss.str();
}

std::string BlankNode::inputform() {
    
    auto Sym2 = getSym2();

    return "_" + Sym2->inputform();
}

std::string BlankSequenceNode::inputform() {

    auto Sym2 = getSym2();

    return "__" + Sym2->inputform();
}

std::string BlankNullSequenceNode::inputform() {

    auto Sym2 = getSym2();

    return "___" + Sym2->inputform();
}

std::string PatternBlankNode::inputform() {

    auto Sym1 = getSym1();
    auto Sym2 = getSym2();

    return Sym1->inputform()+"_"+Sym2->inputform();
}

std::string PatternBlankSequenceNode::inputform() {

    auto Sym1 = getSym1();
    auto Sym2 = getSym2();

    return Sym1->inputform()+"__"+Sym2->inputform();
}

std::string PatternBlankNullSequenceNode::inputform() {

    auto Sym1 = getSym1();
    auto Sym2 = getSym2();

    return Sym1->inputform()+"___"+Sym2->inputform();
}

std::string OptionalDefaultNode::inputform() {

    auto Sym1 = getSym1();

    return Sym1->inputform() + "_.";
}

std::string SlotNode::inputform() {
    return Str;
}

std::string SlotSequenceNode::inputform() {
    return Str;
}

std::string OutNode::inputform() {
    return Str;
}

std::string InternalEmptyNode::inputform() {
    return "";
}

std::string PrefixNode::inputform() {

    auto Operand = getOperand();

    return SymbolToPrefixOperatorString(Op) + Operand->inputform();
}

std::string BinaryNode::inputform() {

    auto Left = getLeft();
    auto Right = getRight();

    std::ostringstream s;

    s << Left->inputform();

    if (Op == SYMBOL_MESSAGENAME) {
        //
        // So also remove the space between a and :: in a::b.
        // This is just a nicety for balance.
        //
        ;
    } else {
        s << " ";
    }

    s << SymbolToInfixOperatorString(Op);

    if (Op == SYMBOL_MESSAGENAME) {
        //
        // a::b needs to have no space between :: and b
        //
        ;
    } else {
        s << " ";
    }

    s << Right->inputform();

    return s.str();
}

std::string InfixNode::inputform() {

    auto Args = getArgs();

    std::ostringstream s;
    if (Op == SYMBOL_PLUS) {
        if (!Args.empty()) {
            auto I = Args.begin();
            s << (*I)->inputform();
            I++;
            auto LastIt = Args.end();
            LastIt--;
            for (; I < LastIt; I++) {
                if (std::dynamic_pointer_cast<InternalMinusNode>(*I)) {
                    s << " ";
                    s << SymbolToInfixOperatorString(SYMBOL_MINUS);
                    s << " ";
                    s << (*I)->inputform();
                } else {
                    s << " ";
                    s << SymbolToInfixOperatorString(SYMBOL_PLUS);
                    s << " ";
                    s << (*I)->inputform();
                }
            }
            s << (*I)->inputform();
        }
    } else if (Op == SYMBOL_INFIXIMPLICITTIMES) {
        if (!Args.empty()) {
            auto I = Args.begin();
            auto LastIt = Args.end();
            LastIt--;
            for (; I < LastIt; I++) {
                s << (*I)->inputform();
                //
                // No need for 3 spaces when 1 will do
                //
                s << SymbolToInfixOperatorString(SYMBOL_INFIXIMPLICITTIMES);
                //
                // No need for 3 spaces when 1 will do
                //
            }
            s << (*I)->inputform();
        }
    } else {
        if (!Args.empty()) {
            auto I = Args.begin();
            auto LastIt = Args.end();
            LastIt--;
            for (; I < LastIt; I++) {
                s << (*I)->inputform();
                s << " ";
                s << SymbolToInfixOperatorString(Op);
                s << " ";
            }
            s << (*I)->inputform();
        }
    }
    return s.str();
}

std::string TernaryNode::inputform() {

    auto p = SymbolToTernaryOperatorPair(Op);
    
    auto Left = getLeft();
    auto Middle = getMiddle();
    auto Right = getRight();

    std::ostringstream s;

    s << Left->inputform();

    s << SymbolToInfixOperatorString(p.first);

    s << Middle->inputform();

    s << SymbolToInfixOperatorString(p.second);

    s << Right->inputform();

    return s.str();
}

std::string PostfixNode::inputform() {

    auto Operand = getOperand();

    std::ostringstream s;

    s << Operand->inputform();
    
    if (Op == SYMBOL_DERIVATIVE) {
        
        for (int i = 0; i < DerivativeOrder; i++) {
            s << SymbolToPostfixOperatorString(SYMBOL_DERIVATIVE);
        }
        
    } else {
        
        if (Op == SYMBOL_REPEATED ||
            Op == SYMBOL_REPEATEDNULL ||
            Op == SYMBOL_REPEATEDNULL) {
            
            s << " ";
        }
        
        s << SymbolToPostfixOperatorString(Op);
        
        if (Op == SYMBOL_COMPOUNDEXPRESSION ||
            Op == SYMBOL_REPEATED ||
            Op == SYMBOL_REPEATEDNULL) {
            
            //
            // prevent a; ; from printing as a;;
            //
            
            s << " ";
        }
        
    }

    return s.str();
}

std::string CallNode::inputform() {

    auto Args = getArgs();

    std::ostringstream ss;
    ss << Head->inputform();
    auto ArgsGroup = std::make_shared<GroupNode>(SYMBOL_GROUPSQUARE, OpenerTokSpan, CloserTokSpan, Args, std::vector<SyntaxIssue>());
    ss << ArgsGroup->inputform();
    return ss.str();
}

std::string PartNode::inputform() {

    auto Args = getArgs();

    std::ostringstream ss;
    ss << Head->inputform();
    auto ArgsGroup = std::make_shared<GroupNode>(SYMBOL_GROUPDOUBLEBRACKET, OpenerTokSpan, CloserTokSpan, Args, std::vector<SyntaxIssue>());
    ss << ArgsGroup->inputform();
    return ss.str();
}

std::string GroupNode::internalInputform() {

    auto Args = getArgs();

    std::ostringstream ss;
    if (Op == SYMBOL_GROUPLINEARSYNTAXPAREN) {
        if (!Args.empty()) {
            auto I = Args.begin();
            auto LastIt = Args.end();
            LastIt--;
            for (; I < LastIt; I++) {
                ss << (*I)->inputform();
                ss << "";
            }
            
            ss << (*I)->inputform();
        }
    } else {
        if (!Args.empty()) {
            auto I = Args.begin();
            auto LastIt = Args.end();
            LastIt--;
            for (; I < LastIt; I++) {
                ss << (*I)->inputform();
                ss << ", ";
            }
            ss << (*I)->inputform();
        }
    }
    return ss.str();
}

std::string GroupNode::inputform() {

    std::ostringstream str;

    auto p = SymbolToGroupPair(Op);

    str << p.first;

    str << internalInputform();

    str << p.second;

    return str.str();
}



//
// Special expressions
//

std::string InternalMinusNode::inputform() {

    auto Operand = getOperand();

    return Operand->inputform();
}

std::string InternalTokenNode::inputform() {
    return Str;
}

std::string FileNode::inputform() {

    auto Args = getArgs();

    std::ostringstream ss;
    if (!Args.empty()) {
        auto I = Args.begin();
        auto LastIt = Args.end();
        LastIt--;
        for (; I < LastIt; I++) {
            ss << (*I)->inputform();
            ss << "\n\n";
        }
        
        ss << (*I)->inputform();
    }
    return ss.str();
}




