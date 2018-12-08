
#pragma once

#include "Precedence.h"
#include "Token.h"

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
    virtual std::shared_ptr<Node> parse() = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~PrefixParselet() {}
};

class InfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual bool isRight() = 0;
    
    virtual ~InfixParselet() {}
};

class PostfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~PostfixParselet() {}
};


//
// Atom and Atom-like parselets
//

class SymbolParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class StringParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class NumberParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class UnderParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_CONTEXT_SENSITIVE;
    }
};

class UnderUnderParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_CONTEXT_SENSITIVE;
    }
};

class UnderUnderUnderParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_CONTEXT_SENSITIVE;
    }
};

class UnderDotParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_CONTEXT_SENSITIVE;
    }
};

class HashParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class HashHashParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class PercentParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
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
    
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
};

class BinaryOperatorParselet : public InfixParselet {
    precedence_t precedence;
    bool right;
public:
    BinaryOperatorParselet(precedence_t precedence, bool right) : precedence(precedence), right(right) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
    
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
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
    
    bool isRight() override {
        return false;
    }
};

class PostfixOperatorParselet : public PostfixParselet {
    precedence_t precedence;
public:
    PostfixOperatorParselet(precedence_t precedence) : precedence(precedence) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
    
    precedence_t getPrecedence() override {
        return precedence;
    }
};




//
// Call parselets
//

class OpenSquareCallParselet : public InfixParselet {
public:
    OpenSquareCallParselet() {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_CALL;
    }
    
    bool isRight() override {
        return false;
    }
};

class LeftDoubleBracketCallParselet : public InfixParselet {
public:
    LeftDoubleBracketCallParselet() {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
    
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
    
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};




//
// Special parselets
//

class InfixPlusParselet : public InfixParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_INFIX_PLUS;
    }
    
    bool isRight() override {
        return false;
    }
};


class SemiParselet : public InfixParselet, PostfixParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_SEMI;
    }
    
    bool isRight() override {
        return false;
    }
};


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
public:
    
    std::shared_ptr<Node> parse() override;
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;
    
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
class TildeParselet : public InfixParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_TILDE;
    }
    
    bool isRight() override {
        return false;
    }
};

class ColonParselet : public BinaryOperatorParselet {
public:
    
    ColonParselet() : BinaryOperatorParselet(PRECEDENCE_UNUSED, false) {}
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
    
    precedence_t getPrecedence() override;
    
    precedence_t getColonPrecedence(std::shared_ptr<Node> Left);
    
};

// It'd be weird if this were an "infix operator"
class SlashColonParselet : public InfixParselet {
public:
    
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;

    precedence_t getPrecedence() override {
        return PRECEDENCE_SLASHCOLON;
    }
    
    bool isRight() override {
        return false;
    }
};

class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class TickParselet : public PostfixOperatorParselet {
public:
    TickParselet() : PostfixOperatorParselet(PRECEDENCE_TICK) {}
    std::shared_ptr<Node> parse(std::shared_ptr<Node> Operand) override;
};

class MessageNameParselet : public BinaryOperatorParselet {
public:
    MessageNameParselet() : BinaryOperatorParselet(PRECEDENCE_COLONCOLON, false) {}
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;
};

class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet() : BinaryOperatorParselet(PRECEDENCE_EQUAL, true) {}
    std::shared_ptr<Node> parse(std::shared_ptr<Node> left) override;
};






//
// Error handling and cleanup
//

class CleanupParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) = 0;
    
    virtual precedence_t getPrecedence() = 0;
    
    virtual ~CleanupParselet() {}
};

class ErrorParselet : public PrefixParselet {
public:
    std::shared_ptr<Node> parse() override;
    
    precedence_t getPrecedence() override {
        return PRECEDENCE_HIGHEST;
    }
};

class CleanupRestParselet : public CleanupParselet {
public:
   CleanupRestParselet() {}
   
   std::shared_ptr<Node> parse(std::shared_ptr<Node> Left) override;
   
   precedence_t getPrecedence() override {
       return PRECEDENCE_HIGHEST;
   }
};







