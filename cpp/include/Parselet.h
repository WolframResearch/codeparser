
#pragma once

#include "Parser.h" // for ParserContext
#include "Token.h" // for Token

#include <memory> // for unique_ptr

class PrefixParselet;
class InfixParselet;
class CallParselet;
class PostfixParselet;
class GroupParselet;
class Parselet;
class Parser;

using ParseletPtr = Parselet *;
using PrefixParseletPtr = PrefixParselet *;
using InfixParseletPtr = InfixParselet *;
typedef void (*ParseFunction)(ParseletPtr, Token firstTok);

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
    virtual ParseFunction parsePrefix() const = 0;
    
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
    virtual ParseFunction parseInfix() const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual const SymbolPtr& getOp() const;
    
    virtual Token processImplicitTimes(Token TokIn) const {
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
    
public:
    
    CallParselet(PrefixParseletPtr GP) : GP(std::move(GP)) {}
    
    PrefixParseletPtr getGP() const {
        return GP;
    }
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_CALL;
    }
};

void CallParselet_parse1(ParseletPtr P, Token Ignored);
void CallParselet_parseInfix(ParseletPtr P, Token firstTok);

//
//
//
class LeafParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void LeafParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class PrefixEndOfFileParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixEndOfFileParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class PrefixErrorParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixErrorParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class PrefixCloserParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixCloserParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class PrefixToplevelCloserParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixToplevelCloserParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class PrefixUnsupportedTokenParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixUnsupportedTokenParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
// The comma infix operator can have leading commas with no operand
//
class PrefixCommaParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixCommaParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
// All other unhandleds are handled here
//
class PrefixUnhandledParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixUnhandledParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class PrefixOperatorParselet : public PrefixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
public:
    
    PrefixOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    ParseFunction parsePrefix() const override;
    
    virtual Precedence getPrecedence() const {
        return precedence;
    }
    
    const SymbolPtr& getOp() const {
        return Op;
    }
};

void PrefixOperatorParselet_parse1(ParseletPtr P, Token Ignored);
void PrefixOperatorParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class InfixImplicitTimesParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
    
    Token processImplicitTimes(Token TokIn) const override;
};

void InfixImplicitTimesParselet_parseInfix(ParseletPtr P, Token firstTok);


//
//
//
class InfixAssertFalseParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_LOWEST;
    }
};

void InfixAssertFalseParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// InfixDifferentialDParselet only exists to properly supply precedence, depending on context
//
class InfixDifferentialDParselet : public InfixImplicitTimesParselet {
public:
    
    Precedence getPrecedence() const override;
    
    Token processImplicitTimes(Token TokIn) const override;
};

//
//
//
class InfixToplevelNewlineParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        //
        // Do not do Implicit Times across top-level newlines
        //
        return PRECEDENCE_LOWEST;
    }
};

void InfixToplevelNewlineParselet_parseInfix(ParseletPtr P, Token firstTok);


//
//
//
class BinaryOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
public:
    
    BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    const SymbolPtr& getOp() const override {
        return Op;
    }
};

void BinaryOperatorParselet_parse1(ParseletPtr P, Token Ignored);
void BinaryOperatorParselet_parse2(ParseletPtr P, Token Ignored);
void BinaryOperatorParselet_parseInfix(ParseletPtr P, Token firstTok);


//
//
//
class InfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
public:
    
    InfixOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    const SymbolPtr& getOp() const override {
        return Op;
    }
};

void InfixOperatorParselet_parse1(ParseletPtr P, Token Ignored);
void InfixOperatorParselet_parseLoop(ParseletPtr P, Token Ignored);
void InfixOperatorParselet_parseInfix(ParseletPtr P, Token firstTok);


//
//
//
class PostfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const SymbolPtr& Op;
    
public:
    
    PostfixOperatorParselet(TokenEnum Tok, Precedence precedence, const SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return precedence;
    }
    
    const SymbolPtr& getOp() const override {
        return Op;
    }
};

void PostfixOperatorParselet_parse1(ParseletPtr P, Token Ignored);
void PostfixOperatorParselet_parse2(ParseletPtr P, Token Ignored);
void PostfixOperatorParselet_parseInfix(ParseletPtr P, Token firstTok);


//
//
//
class GroupParselet : public PrefixParselet {
private:
    
    const SymbolPtr& Op;
    const Closer Closr;
    
public:
    
    GroupParselet(TokenEnum Opener, const SymbolPtr& Op);
    
    const SymbolPtr& getOp() const {
        return Op;
    }
    
    Closer getCloser() const {
        return Closr;
    }
    
    ParseFunction parsePrefix() const override;
};

void GroupParselet_parse1(ParseletPtr P, Token Ignored);
void GroupParselet_parseLoop(ParseletPtr P, Token Ignored);
void GroupParselet_parsePrefix(ParseletPtr P, Token firstTok);



//
// Special parselets
//

//
// something like  x  or x_
//
class SymbolParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void SymbolParselet_parsePrefix(ParseletPtr P, Token firstTok);
void SymbolParselet_parsePrefixContextSensitive(ParseletPtr P, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class CommaParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_COMMA;
    }
    
    const SymbolPtr& getOp() const override;
};

void CommaParselet_parse1(ParseletPtr P, Token Ignored);
void CommaParselet_parseLoop(ParseletPtr P, Token Ignored);
void CommaParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SEMI;
    }
};

void SemiParselet_parse1(ParseletPtr P, Token Ignored);
void SemiParselet_parseLoop(ParseletPtr P, Token Ignored);
void SemiParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
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
    ParseFunction parsePrefix() const override;
    
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
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SEMISEMI;
    }
};

void SemiSemiParselet_parse0(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parse1(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parse3(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parse4(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parse6(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parse7(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parseLoop(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parsePrefix(ParseletPtr P, Token firstTok);
void SemiSemiParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// Something like  a ~f~ b
//
// It'd be weird if this were an "infix operator"
//
class TildeParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void TildeParselet_parse1(ParseletPtr P, Token Ignored);
void TildeParselet_parse2(ParseletPtr P, Token Ignored);
void TildeParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// Something like  symbol:object  or  pattern:optional
//
class ColonParselet : public InfixParselet {
public:
    
    //
    // Something like  symbol:object
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void ColonParselet_parsePattern(ParseletPtr P, Token Ignored);
void ColonParselet_parseError(ParseletPtr P, Token Ignored);
void ColonParselet_parseOptional(ParseletPtr P, Token Ignored);
void ColonParselet_parseInfix(ParseletPtr P, Token firstTok);


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
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_SLASHCOLON;
    }
};

void SlashColonParselet_parse1(ParseletPtr P, Token Ignored);
void SlashColonParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// a /: b = c  and  a /: b = .  are handled here
//
class EqualParselet : public BinaryOperatorParselet {
public:
    
    EqualParselet();
    
    ParseFunction parseInfix() const override;
};

void EqualParselet_parse1(ParseletPtr P, Token Ignored);
void EqualParselet_parse2(ParseletPtr P, Token Ignored);
void EqualParselet_parseInfix(ParseletPtr P, Token firstTok);
void EqualParselet_parseInfixContextSensitive(ParseletPtr P, Token Ignored);


//
// a /: b := c  is handled here
//
class ColonEqualParselet : public BinaryOperatorParselet {
public:
    
    ColonEqualParselet();
    
    ParseFunction parseInfix() const override;
};

void ColonEqualParselet_parse1(ParseletPtr P, Token Ignored);
void ColonEqualParselet_parse2(ParseletPtr P, Token Ignored);
void ColonEqualParselet_parseInfix(ParseletPtr P, Token firstTok);
void ColonEqualParselet_parseInfixContextSensitive(ParseletPtr P, Token firstTok);



//
// Something like  \[Integral] f \[DifferentialD] x
//
class IntegralParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void IntegralParselet_parse1(ParseletPtr P, Token Ignored);
void IntegralParselet_parse2(ParseletPtr P, Token Ignored);
void IntegralParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
// a::b
//
class ColonColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_COLONCOLON;
    }
};

void ColonColonParselet_parse1(ParseletPtr P, Token Ignored);
void ColonColonParselet_parseLoop(ParseletPtr P, Token Ignored);
void ColonColonParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// a>>b
//
class GreaterGreaterParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_GREATERGREATER;
    }
};

void GreaterGreaterParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// a>>>b
//
class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override {
        return PRECEDENCE_GREATERGREATERGREATER;
    }
};

void GreaterGreaterGreaterParselet_parseInfix(ParseletPtr P, Token firstTok);


//
// <<a
//
class LessLessParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void LessLessParselet_parsePrefix(ParseletPtr P, Token firstTok);


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
    
    ParseFunction parsePrefix() const override;
};

void HashParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
// Something like  ##  or  ##1
//
class HashHashParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void HashHashParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
// Something like  %  or  %1
//
class PercentParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PercentParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
// Something like  %%  or  %%%
//
class PercentPercentParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PercentPercentParselet_parsePrefix(ParseletPtr P, Token firstTok);


//
//
//
class UnderParselet : public PrefixParselet {
private:
    
    const SymbolPtr& BOp;
    const SymbolPtr& PBOp;
    
public:
    
    UnderParselet(const SymbolPtr& BOp, const SymbolPtr& PBOp) : BOp(BOp), PBOp(PBOp) {}
    
    const SymbolPtr& getBOp() const {
        return BOp;
    }
    
    const SymbolPtr& getPBOp() const {
        return PBOp;
    }
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    ParseFunction parsePrefix() const override;
};

void UnderParselet_parse0(ParseletPtr P, Token Ignored);
void UnderParselet_parse2(ParseletPtr P, Token Ignored);
void UnderParselet_parse4(ParseletPtr P, Token Ignored);
void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn);
//
// infix
//
// Something like  a_b
//
// Called from other parselets
//
void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn);


//
//
//
class UnderDotParselet : public PrefixParselet {
public:
    
    //
    // prefix
    //
    // Something like  _.
    //
    ParseFunction parsePrefix() const override;
};

void UnderDotParselet_parse1(ParseletPtr P, Token Ignored);
void UnderDotParselet_parse2(ParseletPtr P, Token Ignored);
void UnderDotParselet_parsePrefix(ParseletPtr P, Token TokIn);
//
// infix
//
// Something like  a_.
//
// Called from other parselets
//
void UnderDotParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn);
