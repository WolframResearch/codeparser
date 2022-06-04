
#pragma once

#include "Parser.h" // for ParserContext
#include "Token.h" // for Token

#include <memory> // for unique_ptr
#include <functional>

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class ContextSensitivePrefixParselet;
class ContextSensitiveInfixParselet;
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

//
//
//
class PrefixParselet : virtual public Parselet {
public:
    
    //
    // Commonly referred to as NUD method in the literature
    //
    virtual void parsePrefix(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~PrefixParselet() {}
};

//
//
//
class InfixParselet : virtual public Parselet {
public:
    
    //
    // Commonly referred to as LED method in the literature
    //
    virtual void parseInfix(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence(ParserContext Ctxt) const = 0;
    
    virtual const SymbolPtr& getOp() const;
    
    virtual Token processImplicitTimes(Token TokIn, ParserContext Ctxt) const {
        return TokIn;
    }
    
    virtual ~InfixParselet() {}
};

//
//
//
class CallParselet : public InfixParselet {
private:
    
    const PrefixParseletPtr GP;
    
    void parse1(ParserContext CtxtIn) const;
    
public:
    
    CallParselet(PrefixParseletPtr GP) : GP(std::move(GP)) {}
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_CALL;
    }
};

//
//
//
class ContextSensitivePrefixParselet : virtual public Parselet {
public:
    
    virtual void parsePrefixContextSensitive(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitivePrefixParselet() {}
};

//
//
//
class ContextSensitiveInfixParselet : virtual public Parselet {
public:
    
    virtual void parseInfixContextSensitive(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitiveInfixParselet() {}
};

//
//
//
class LeafParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixEndOfFileParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixErrorParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixCloserParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixToplevelCloserParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixUnsupportedTokenParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// The comma infix operator can have leading commas with no operand
//
class PrefixCommaParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// All other unhandleds are handled here
//
class PrefixUnhandledParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixOperatorParselet : public PrefixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
    void parse1(ParserContext CtxtIn) const;
    
public:
    
    PrefixOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
    
    virtual Precedence getPrecedence(ParserContext Ctxt) const {
        return precedence;
    }
};

//
//
//
class InfixImplicitTimesParselet : public InfixParselet {
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
    
    Token processImplicitTimes(Token TokIn, ParserContext Ctxt) const override;
};

//
//
//
class InfixAssertFalseParselet : public InfixParselet {
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_LOWEST;
    }
};

//
// InfixDifferentialDParselet only exists to properly supply precedence, depending on context
//
class InfixDifferentialDParselet : public InfixImplicitTimesParselet {
public:
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
    
    Token processImplicitTimes(Token TokIn, ParserContext Ctxt) const override;
};

//
//
//
class InfixToplevelNewlineParselet : public InfixParselet {
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        //
        // Do not do Implicit Times across top-level newlines
        //
        return PRECEDENCE_LOWEST;
    }
};

//
//
//
class BinaryOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
    void parse1(ParserContext CtxtIn) const;
    void parse2(ParserContext CtxtIn) const;
    
public:
    
    BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    const SymbolPtr& getOp() const override {
        return Op;
    }
};

//
//
//
class InfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
    void parse1(ParserContext CtxtIn) const;
    void parseLoop(ParserContext Ctxt) const;
    
public:
    
    InfixOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    const SymbolPtr& getOp() const override {
        return Op;
    }
};

//
//
//
class PostfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
    void parse1(ParserContext CtxtIn) const;
    void parse2(ParserContext CtxtIn) const;
    
public:
    
    PostfixOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    const SymbolPtr& getOp() const override {
        return Op;
    }
};

//
//
//
class GroupParselet : public PrefixParselet {
private:
    
    const SymbolPtr& Op;
    const Closer Closr;
    
    void parseLoop(ParserContext CtxtIn) const;
    
public:
    
    GroupParselet(TokenEnum Opener, const SymbolPtr& Op);
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};


//
// Special parselets
//

//
// something like  x  or x_
//
class SymbolParselet : public PrefixParselet, public ContextSensitivePrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
    
    void parsePrefixContextSensitive(Token firstTok, ParserContext Ctxt) const override;
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class CommaParselet : public InfixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    void parseLoop(ParserContext Ctxt) const;
    
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_COMMA;
    }
    
    const SymbolPtr& getOp() const override;
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiParselet : public InfixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    void parseLoop(ParserContext Ctxt) const;
    
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_SEMI;
    }
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
private:
    
    //
    // infix
    //
    // Something like  a;;b
    //
    // Parses a single complete Span
    //
    void parse0(Token firstTok, ParserContext Ctxt) const;
    void parse1(ParserContext CtxtIn) const;
    void parse3(ParserContext CtxtIn) const;
    void parse4(ParserContext CtxtIn) const;
    void parse5(ParserContext CtxtIn) const;
    void parseLoop(ParserContext Ctxt) const;
    
public:
    
    //
    // prefix
    //
    // Parses a run of multiple Span expressions
    //
    // A run is anything like  ;;;;x;;y;;;;
    //
    // Multiple Span expressions are ImplicitTimes together
    //
    // Must also handle  ;;!b  where there is an implicit Times, but only a single Span
    //
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
    
    //
    // infix
    //
    // Parses a run of multiple Span expressions
    //
    // A run is anything like  a;;;;x;;y;;;;
    //
    // Multiple Span expressions are ImplicitTimes together
    //
    // Must also handle  a;;!b  where there is an implicit Times, but only a single Span
    //
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_SEMISEMI;
    }
};

//
// Something like  a ~f~ b
//
// It'd be weird if this were an "infix operator"
//
class TildeParselet : public InfixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    void parse2(ParserContext CtxtIn) const;
    
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        
        if ((Ctxt.Flag & PARSER_INSIDE_TILDE) == PARSER_INSIDE_TILDE) {
            return PRECEDENCE_LOWEST;
        }
        
        return PRECEDENCE_TILDE;
    }
};

//
// Something like  symbol:object  or  pattern:optional
//
class ColonParselet : public InfixParselet, public ContextSensitiveInfixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    void parse2(ParserContext CtxtIn) const;
    
public:
    
    //
    // Something like  symbol:object
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    //
    // Something like  pattern:optional
    //
    // Called from other parselets
    //
    void parseInfixContextSensitive(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_FAKE_OPTIONALCOLON;
    }
};

//
// Something like  a /: b = c
//
// a   /:   b   =   c
// ^~~~~ Args at the start
//       ^~~ Trivia1
//           ^~~ Trivia2
//
//
// It'd be weird if this were an "infix operator"
//
class SlashColonParselet : public InfixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_SLASHCOLON;
    }
};

//
// a /: b = c  and  a /: b = .  are handled here
//
class EqualParselet : public BinaryOperatorParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    
public:
    
    EqualParselet();
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
};

//
// a /: b := c  is handled here
//
class ColonEqualParselet : public BinaryOperatorParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    
public:
    
    ColonEqualParselet();
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
};


//
// Something like  \[Integral] f \[DifferentialD] x
//
class IntegralParselet : public PrefixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    void parse2(ParserContext CtxtIn) const;
    
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// a::b
//
class ColonColonParselet : public InfixParselet {
private:
    
    void parse1(ParserContext CtxtIn) const;
    void parseLoop(ParserContext Ctxt) const;
    
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_COLONCOLON;
    }
};

//
// a>>b
//
class GreaterGreaterParselet : public InfixParselet {
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_GREATERGREATER;
    }
};

//
// a>>>b
//
class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    void parseInfix(Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_GREATERGREATERGREATER;
    }
};

//
// <<a
//
class LessLessParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  #  or  #1  or  #abc  or  #"abc"
//
// From Slot documentation:
//
// In the form #name, the characters in name can be any combination of alphanumeric characters not beginning with digits.
//
//
// A slot that starts with a digit goes down one path
// And a slot that starts with a letter goes down another path
//
// Make sure e.g.  #1a is not parsed as SlotNode["#1a"]
//
class HashParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  ##  or  ##1
//
class HashHashParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  %  or  %1
//
class PercentParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  %%  or  %%%
//
class PercentPercentParselet : public PrefixParselet {
public:
    
    void parsePrefix(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class UnderParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
private:
    
    const SymbolPtr& BOp;
    const SymbolPtr& PBOp;
    
    void parse0(Token TokIn, ParserContext Ctxt) const;
    void parse1(Token Tok, ParserContext Ctxt) const;
    void parse2(ParserContext CtxtIn) const;
    void parse3(ParserContext CtxtIn) const;
    void parse4(ParserContext CtxtIn) const;
    
public:
    
    UnderParselet(const SymbolPtr& BOp, const SymbolPtr& PBOp) : BOp(BOp), PBOp(PBOp) {}
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    void parsePrefix(Token TokIn, ParserContext Ctxt) const override;
    
    //
    // infix
    //
    // Something like  a_b
    //
    // Called from other parselets
    //
    void parseInfixContextSensitive(Token TokIn, ParserContext Ctxt) const override;
};

//
//
//
class UnderDotParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
private:
    
    void parse0(Token TokIn, ParserContext Ctxt) const;
    void parse1(ParserContext CtxtIn) const;
    void parse2(ParserContext CtxtIn) const;
    
    
public:
    
    //
    // prefix
    //
    // Something like  _.
    //
    void parsePrefix(Token TokIn, ParserContext Ctxt) const override;
    
    //
    // infix
    //
    // Something like  a_.
    //
    // Called from other parselets
    //
    void parseInfixContextSensitive(Token TokIn, ParserContext Ctxt) const override;
};
