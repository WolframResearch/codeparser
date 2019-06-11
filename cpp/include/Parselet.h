
#pragma once

#include "Parser.h"
#include "Precedence.h"
#include "Token.h"

#include <memory>


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
    virtual NodePtr parse(ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual ~PrefixParselet() {}
};

class InfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual NodePtr parse(NodeSeq Left, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual ~InfixParselet() {}
};

class BinaryParselet : virtual public InfixParselet {
public:
    virtual Associativity getAssociativity() const = 0;
    
    virtual ~BinaryParselet() {}
};

class CallParselet : public InfixParselet {
public:
    CallParselet() {}
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_CALL;
    }
};

class ContextSensitivePrefixParselet : virtual public Parselet {
public:

    virtual NodePtr parseContextSensitive(ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitivePrefixParselet() {}
};

class ContextSensitiveInfixParselet : virtual public Parselet {
public:
    
    virtual NodePtr parseContextSensitive(NodeSeq Left, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitiveInfixParselet() {}
};

class StartOfLineParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as NUD method in the literature
    //
    NodePtr parse(ParserContext Ctxt) const;
};




class PrefixOperatorParselet : public PrefixParselet {
    Precedence precedence;
public:
    PrefixOperatorParselet(Precedence precedence) : precedence(precedence) {}
    
    NodePtr parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
};

class BinaryOperatorParselet : public BinaryParselet {
    Precedence precedence;
    Associativity assoc;
public:
    BinaryOperatorParselet(Precedence precedence, Associativity assoc) : precedence(precedence), assoc(assoc) {}
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return assoc;
    }
};

class InfixOperatorParselet : public InfixParselet {
    Precedence precedence;
    bool allowTrailing;
public:
    InfixOperatorParselet(Precedence precedence, bool allowTrailing = false) : precedence(precedence), allowTrailing(allowTrailing) {}
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
};

class PostfixOperatorParselet : public InfixParselet {
    Precedence precedence;
public:
    PostfixOperatorParselet(Precedence precedence) : precedence(precedence) {}

    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;

    Precedence getPrecedence() const override {
        return precedence;
    }
};



class GroupParselet : public PrefixParselet {
public:
    NodePtr parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};



//
// Special parselets
//

class SymbolParselet : public PrefixParselet, public ContextSensitivePrefixParselet {
public:
    NodePtr parse(ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SYMBOL;
    }
};

class UnderParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
public:
    NodePtr parse(ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(NodeSeq Left, ParserContext Ctxt) const override;

    Precedence getPrecedence() const override {
        return PRECEDENCE_UNDER;
    }
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
    
    NodePtr parse0(NodeSeq Left, ParserContext Ctxt) const;
public:
    
    NodePtr parse(ParserContext Ctxt) const override;
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SEMISEMI;
    }
};


// It'd be weird if this were an "infix operator"
class TildeParselet : public BinaryParselet {
public:
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
    
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
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(NodeSeq Left, ParserContext Ctxt) const override;

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
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;

    Precedence getPrecedence() const override {
        return PRECEDENCE_SLASHCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_RIGHT;
    }
};

class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    NodePtr parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet() : BinaryOperatorParselet(PRECEDENCE_EQUAL, ASSOCIATIVITY_RIGHT) {}
    
    NodePtr parse(NodeSeq Left, ParserContext Ctxt) const override;
};

class IntegralParselet : public PrefixParselet {
public:
    NodePtr parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_LONGNAME_INTEGRAL;
    }
};




//
// Error handling and cleanup
//

class ExpectedPossibleExpressionErrorParselet : public PrefixParselet {
public:
    NodePtr parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};
