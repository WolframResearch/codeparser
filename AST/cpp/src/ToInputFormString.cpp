
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


std::string SlotNode::inputform() {
    return Str;
}

std::string SlotSequenceNode::inputform() {
    return Str;
}

std::string OutNode::inputform() {
    return Str;
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

    s << SymbolToBinaryOperatorString(Op);

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

    return s.str();
}

std::string TernaryNode::inputform() {

    auto p = SymbolToTernaryOperatorPair(Op);
    
    auto Left = getLeft();
    auto Middle = getMiddle();
    auto Right = getRight();

    std::ostringstream s;

    s << Left->inputform();

    s << SymbolToTernaryOperatorString(p.first);

    s << Middle->inputform();

    s << SymbolToTernaryOperatorString(p.second);

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

    auto Body = std::dynamic_pointer_cast<GroupNode>(getArgs()[0]);

    std::ostringstream ss;
    ss << Head->inputform();
    ss << Body->inputform();
    return ss.str();
}

std::string CallMissingCloserNode::inputform() {
    
    auto Body = std::dynamic_pointer_cast<GroupNode>(getArgs()[0]);
    
    std::ostringstream ss;
    ss << Head->inputform();
    ss << Body->inputform();
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

std::string OptionalDefaultNode::inputform() {

    return "_.";
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

std::string OptionalDefaultPatternNode::inputform() {

    auto Sym1 = getSym1();

    return Sym1->inputform() + "_.";
}



std::string InternalTokenNode::inputform() {
    return Str;
}


std::string InternalAllNode::inputform() {
    return "";
}

std::string InternalDotNode::inputform() {
    return "";
}

std::string InternalNullNode::inputform() {
    return "";
}

std::string InternalOneNode::inputform() {
    return "";
}

std::string InternalMinusNode::inputform() {
    
    auto Op = getOperand();

    return Op->inputform();
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
