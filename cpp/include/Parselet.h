
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
    virtual std::shared_ptr<Node> parse(ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual ~PrefixParselet() {}
};

class InfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual ~InfixParselet() {}
};

class BinaryParselet : virtual public InfixParselet {
public:
    virtual Associativity getAssociativity() const = 0;
    
    virtual ~BinaryParselet() {}
};

class CallParselet : public Parselet {
    std::shared_ptr<GroupParselet> groupParselet;
public:
    CallParselet();
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const;
    
    Precedence getPrecedence() const {
        return PRECEDENCE_CALL;
    }
    
    GroupParselet *getGroupParselet();
};

class PostfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual ~PostfixParselet() {}
};

class ContextSensitiveParselet : virtual public Parselet {
public:
    
    virtual ~ContextSensitiveParselet() {}
};

class StartOfLineParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as NUD method in the literature
    //
    std::shared_ptr<Node> parse(ParserContext Ctxt) const;
};






//
// Atom and Atom-like parselets
//

class SymbolParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    std::shared_ptr<Node> parseContextSensitive(ParserContext Ctxt) const;

    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class StringParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class IntegerParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class RealParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class HashParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class HashHashParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class PercentParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt)  const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};



//
// Base Operator parselets
//

class PrefixOperatorParselet : public PrefixParselet {
    Precedence precedence;
public:
    PrefixOperatorParselet(Precedence precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
};

class BinaryOperatorParselet : public BinaryParselet {
    Precedence precedence;
    Associativity assoc;
public:
    BinaryOperatorParselet(Precedence precedence, Associativity assoc) : precedence(precedence), assoc(assoc) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return assoc;
    }
};

class InfixOperatorParselet : public InfixParselet {
    Precedence precedence;
public:
    InfixOperatorParselet(Precedence precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
};

class PostfixOperatorParselet : public PostfixParselet {
    Precedence precedence;
public:
    PostfixOperatorParselet(Precedence precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
};




//
// Group parselets
//

class GroupParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};



//
// Special parselets
//

class UnderParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) const;

    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class UnderDotParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) const;

    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};


class SemiParselet : public InfixParselet, PostfixParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SEMI;
    }
};


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public BinaryParselet {
public:
    
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SEMISEMI;
    }
    
    //
    // Making it right-associative makes parsing easier
    //
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_RIGHT;
    }
};


// It'd be weird if this were an "infix operator"
class TildeParselet : public BinaryParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_TILDE;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_LEFT;
    }
};

class ColonParselet : public BinaryParselet, public ContextSensitiveParselet {
public:
    
    ColonParselet() {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt) const;

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
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;

    Precedence getPrecedence() const override {
        return PRECEDENCE_SLASHCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_RIGHT;
    }
};

class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};

class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet() : BinaryOperatorParselet(PRECEDENCE_EQUAL, ASSOCIATIVITY_RIGHT) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) const override;
};

class IntegralParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_LONGNAME_INTEGRAL;
    }
};




//
// Error handling and cleanup
//

class ExpectedPossibleExpressionErrorParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_HIGHEST;
    }
};
