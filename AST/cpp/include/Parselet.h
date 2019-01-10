
#pragma once

#include "Precedence.h"
#include "Token.h"
#include "Parser.h"

#include <memory>

class Parser;
class Node;


class Parselet {
public:
    virtual ~Parselet() {}
};

class PrefixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as NUD method in the literature
    //
    virtual std::shared_ptr<Node> parse(ParserContext Ctxt) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~PrefixParselet() {}
};

class InfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~InfixParselet() {}
};

class BinaryParselet : virtual public InfixParselet {
public:
    virtual bool isRight() = 0;
    
    virtual ~BinaryParselet() {}
};

class PostfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~PostfixParselet() {}
};

class ContextSensitiveParselet : virtual public Parselet {
public:
    
    virtual ~ContextSensitiveParselet() {}
};




//
// Atom and Atom-like parselets
//

class SymbolParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    std::shared_ptr<Node> parseContextSensitive(ParserContext Ctxt);

    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class StringParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class NumberParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class HashParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class HashHashParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class PercentParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};



//
// Base Operator parselets
//

class PrefixOperatorParselet : public PrefixParselet {
    precedence_t precedence;
public:
    PrefixOperatorParselet(precedence_t precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
};

class BinaryOperatorParselet : public BinaryParselet {
    precedence_t precedence;
    bool right;
public:
    BinaryOperatorParselet(precedence_t precedence, bool right) : precedence(precedence), right(right) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
    
    bool isRight() override {
        return right;
    }
};

class InfixOperatorParselet : public InfixParselet {
    precedence_t precedence;
public:
    InfixOperatorParselet(precedence_t precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
};

class PostfixOperatorParselet : public PostfixParselet {
    precedence_t precedence;
public:
    PostfixOperatorParselet(precedence_t precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
};




//
// Call parselets
//

class CallParselet : public BinaryParselet {
    Token Opener;
public:
    CallParselet(Token Opener) : Opener(Opener) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_CALL;
    }
    
    bool isRight() override {
        return false;
    }
};


//
// Group parselets
//

class GroupParselet : public PrefixParselet {
    Token Opener;
public:
    GroupParselet(Token Opener) : Opener(Opener) {}
    
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};




//
// Special parselets
//

class UnderParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt);

    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class UnderUnderParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt);

    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class UnderUnderUnderParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt);

    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class UnderDotParselet : public PrefixParselet, public ContextSensitiveParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt);

    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};


class SemiParselet : public InfixParselet, PostfixParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_SEMI;
    }
};


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public BinaryParselet {
public:
    
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_SEMISEMI;
    }
    
    //
    // Making it right-associative makes parsing easier
    //
    bool isRight() override {
        return true;
    }
};


// It'd be weird if this were an "infix operator"
class TildeParselet : public BinaryParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_TILDE;
    }
    
    bool isRight() override {
        return false;
    }
};

class ColonParselet : public BinaryParselet, public ContextSensitiveParselet {
public:
    
    ColonParselet() {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
    std::shared_ptr<Node> parseContextSensitive(std::shared_ptr<Node> Left, ParserContext Ctxt);

    precedence_t getPrecedence() override {
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
    
    bool isRight() override {
        return false;
    }
    
};

// It'd be weird if this were an "infix operator"
class SlashColonParselet : public BinaryParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;

    precedence_t getPrecedence() override {
        return PRECEDENCE_SLASHCOLON;
    }
    
    bool isRight() override {
        return false;
    }
};

class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class TickParselet : public PostfixOperatorParselet {
public:
    TickParselet() : PostfixOperatorParselet(PRECEDENCE_TICK) {}
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Operand, ParserContext Ctxt) override;
};

class MessageNameParselet : public BinaryOperatorParselet {
public:
    MessageNameParselet() : BinaryOperatorParselet(PRECEDENCE_COLONCOLON, false) {}
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
};

class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet() : BinaryOperatorParselet(PRECEDENCE_EQUAL, true) {}
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
};






//
// Error handling and cleanup
//

class CleanupParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~CleanupParselet() {}
};

class ErrorParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse(ParserContext Ctxt) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class CleanupRestParselet : public CleanupParselet {
public:
   CleanupRestParselet() {}
   
   std::shared_ptr<Node> parse(std::shared_ptr<Node> Left, ParserContext Ctxt) override;
   
   precedence_t getPrecedence() override {
       return PRECEDENCE_HIGHEST;
   }
};
