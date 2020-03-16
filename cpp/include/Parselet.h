
#pragma once

#include "Parser.h" // for ParserContext
#include "Token.h" // for Token

#include <memory> // for unique_ptr

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
#if STARTOFLINE
class StartOfLineParselet;
class StartOfFileParselet;
#endif // STARTOFLINE
class GroupParselet;
class Parselet;
class Parser;

using PrefixParseletPtr = PrefixParselet *;
using InfixParseletPtr = InfixParselet *;
using ContextSensitivePrefixParseletPtr = ContextSensitivePrefixParselet *;
using ContextSensitiveInfixParseletPtr = ContextSensitiveInfixParselet *;

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
    
    //
    // There is an ambiguity with tokens that are both prefix and infix, e.g.
    // +  -  ;;  !  ++  --  !!  \[Minus]  \[MinusPlus]  \[PlusMinus]  \[CircleTimes]  \[Coproduct]
    //
    // Given the input  ;;;;
    // when parsing the second  ;;  , we could get here because ;; is registered as infix
    // But this particular ;; is a new expression, it is not actually infix
    //
    // Given the input  1+2
    // when parsing the +, make sure to treat it as infix and NOT prefix
    //
    // Solution is to handle infix parselets where needed, i.e., SemiSemiParselet
    //
    virtual Precedence getPrecedence(ParserContext Ctxt) const = 0;
    
    virtual ~PrefixParselet() {}
};

class InfixParselet : virtual public Parselet {
public:
    //
    // Commonly referred to as LED method in the literature
    //
    virtual NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const = 0;
    
    virtual void setPrecedence(Precedence) {}
    
    virtual Associativity getAssociativity() const = 0;
    
    virtual ~InfixParselet() {}
};

class CallParselet : public InfixParselet {
    PrefixParseletPtr GP;
public:
    CallParselet(PrefixParseletPtr GP);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_CALL;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
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
    Precedence precedence;
public:
    LeafParselet(Precedence precedence);
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixAssertFalseParselet : public PrefixParselet {
public:
    PrefixAssertFalseParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixEndOfFileParselet : public PrefixParselet {
public:
    PrefixEndOfFileParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixErrorParselet : public PrefixParselet {
public:
    PrefixErrorParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixCloserParselet : public PrefixParselet {
public:
    PrefixCloserParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixUnsupportedTokenParselet : public PrefixParselet {
public:
    PrefixUnsupportedTokenParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixUnhandledParselet : public PrefixParselet {
public:
    PrefixUnhandledParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

class PrefixOperatorParselet : public PrefixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    PrefixOperatorParselet(TokenEnum Tok, Precedence precedence);
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
};

class InfixImplicitTimesParselet : public InfixParselet {
public:
    InfixImplicitTimesParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixAssertFalseParselet : public InfixParselet {
public:
    InfixAssertFalseParselet();

    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;

    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixEndOfFileParselet : public InfixParselet {
public:
    InfixEndOfFileParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixErrorParselet : public InfixParselet {
public:
    InfixErrorParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixUnsupportedTokenParselet : public InfixParselet {
public:
    InfixUnsupportedTokenParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixCloserParselet : public InfixParselet {
public:
    InfixCloserParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixDifferentialDParselet : public InfixParselet {
public:
    InfixDifferentialDParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};

class InfixToplevelNewlineParselet : public InfixParselet {
public:
    InfixToplevelNewlineParselet();
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override;
    
    Associativity getAssociativity() const override;
};



class BinaryOperatorParselet : public InfixParselet {
    Precedence precedence;
    Associativity assoc;
    SymbolPtr& Op;
public:
    BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Associativity assoc);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
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
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};

class PostfixOperatorParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    PostfixOperatorParselet(TokenEnum Tok, Precedence precedence);
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};



class GroupParselet : public PrefixParselet {
    SymbolPtr& Op;
    Closer Closr;
public:
    
    GroupParselet(TokenEnum Opener);
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
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
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_SYMBOL;
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
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return precedence;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
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
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_SEMISEMI;
    }
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_SEMISEMI;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};


// It'd be weird if this were an "infix operator"
class TildeParselet : public InfixParselet {
public:
    
    TildeParselet() {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        
        *implicitTimes = false;
        
        if ((Ctxt.Flag & PARSER_INSIDE_TILDE) == PARSER_INSIDE_TILDE) {
            return PRECEDENCE_LOWEST;
        }
        
        return PRECEDENCE_TILDE;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};

class ColonParselet : public InfixParselet, public ContextSensitiveInfixParselet {
public:
    
    ColonParselet() {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
    
};

// It'd be weird if this were an "infix operator"
class SlashColonParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_SLASHCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_RIGHT;
    }
};

class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_HIGHEST;
    }
};

class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, ASSOCIATIVITY_RIGHT) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
};

//
// This is not really a binary operator, but keep it consistent with EqualParselet, which handles  a = .
//
class EqualDotParselet : public BinaryOperatorParselet {
public:
    EqualDotParselet() : BinaryOperatorParselet(TOKEN_EQUALDOT, PRECEDENCE_EQUAL, ASSOCIATIVITY_RIGHT) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
};

class IntegralParselet : public PrefixParselet {
    SymbolPtr& Op1;
    SymbolPtr& Op2;
public:
    
    IntegralParselet();
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_CLASS_INTEGRATIONOPERATORS;
    }
};

class ColonColonParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_COLONCOLON;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};

class GreaterGreaterParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_GREATERGREATER;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};

class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt, bool *implicitTimes) const override {
        *implicitTimes = false;
        return PRECEDENCE_GREATERGREATERGREATER;
    }
    
    Associativity getAssociativity() const override {
        return ASSOCIATIVITY_NONRIGHT;
    }
};

class LessLessParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_LESSLESS;
    }
};
