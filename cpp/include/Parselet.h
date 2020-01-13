
#pragma once

#include "Parser.h" // for ParserContext
#include "Token.h" // for Token

#include <memory> // for unique_ptr


//
// Classes that derive from Parselet are responsible for parsing specific kinds of syntax
//
class Parselet {
public:
    
    virtual ~Parselet() {}
};

class PrefixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as NUD method in the literature
    //
    virtual NodePtr parse(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual ~PrefixParselet() {}
};

class InfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual Associativity getAssociativity() const = 0;
    
    virtual ~InfixParselet() {}
};

class BinaryParselet : virtual public InfixParselet {
public:
    
    virtual ~BinaryParselet() {}
};

class CallParselet : public InfixParselet {
    PrefixParseletPtr GP;
public:
    CallParselet(PrefixParseletPtr GP);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_CALL;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};

class ContextSensitivePrefixParselet : virtual public Parselet {
public:
    
    virtual NodePtr parseContextSensitive(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitivePrefixParselet() {}
};

class ContextSensitiveInfixParselet : virtual public Parselet {
public:
    
    virtual NodePtr parseContextSensitive(NodeSeq Left, Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitiveInfixParselet() {}
};

#if STARTOFLINE
class StartOfLineParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as NUD method in the literature
    //
    NodePtr parse(ParserContext Ctxt) const;
};

class StartOfFileParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as NUD method in the literature
    //
    NodePtr parse(ParserContext Ctxt) const;
};
#endif // STARTOFLINE


class LeafParselet : public PrefixParselet {
public:
    LeafParselet() {}
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};




class PrefixOperatorParselet : public PrefixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    PrefixOperatorParselet(TokenEnum Tok, Precedence precedence);
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
};

class BinaryOperatorParselet : public BinaryParselet {
    Precedence precedence;
    Associativity assoc;
    SymbolPtr& Op;
public:
    BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Associativity assoc);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return assoc;
    }
};

class InfixOperatorParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    InfixOperatorParselet(TokenEnum Tok, Precedence precedence);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};

class PostfixOperatorParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    PostfixOperatorParselet(TokenEnum Tok, Precedence precedence);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};



class GroupParselet : public PrefixParselet {
    SymbolPtr& Op;
    Closer Closr;
public:
    
    GroupParselet(TokenEnum Opener);
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};



//
// Special parselets
//

class SymbolParselet : public PrefixParselet, public ContextSensitivePrefixParselet {
public:
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SYMBOL;
    }
};

class UnderParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
    size_t count;
public:
    UnderParselet(size_t count);
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_UNDER;
    }
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class InfixOperatorWithTrailingParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    
    InfixOperatorWithTrailingParselet(TokenEnum Tok, Precedence precedence);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
    
    NodePtr parse0(NodeSeq Left, Token firstTok, ParserContext Ctxt) const;
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SEMISEMI;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};


// It'd be weird if this were an "infix operator"
class TildeParselet : public BinaryParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_TILDE;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_LEFT;
    }
};

class ColonParselet : public BinaryParselet, public ContextSensitiveInfixParselet {
public:
    
    ColonParselet() {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_LEFT;
    }
    
};

// It'd be weird if this were an "infix operator"
class SlashColonParselet : public BinaryParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SLASHCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_RIGHT;
    }
};

class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet(TokenEnum Tok) : BinaryOperatorParselet(Tok, PRECEDENCE_EQUAL, ASSOCIATIVITY_RIGHT) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
};

class IntegralParselet : public PrefixParselet {
    SymbolPtr& Op1;
    SymbolPtr& Op2;
public:
    
    IntegralParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    }
};

class InequalityParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_CLASS_INEQUALITY;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};

// Gather all \[VectorGreater] \[VectorLess] \[VectorGreaterEqual] \[VectorLessEqual] into a single node
class VectorInequalityParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_CLASS_VECTORINEQUALITY;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};

class ColonColonParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_COLONCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONE;
    }
};

class GreaterGreaterParselet : public BinaryParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_GREATERGREATER;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_LEFT;
    }
};

class GreaterGreaterGreaterParselet : public BinaryParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_GREATERGREATERGREATER;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_LEFT;
    }
};

class LessLessParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_LESSLESS;
    }
};
