
#pragma once

#include "Token.h" // for Token
#include "Precedence.h"

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


class PrefixParselet : virtual public Parselet {
public:
    
    virtual ParseFunction parsePrefix() const = 0;
    
    virtual ~PrefixParselet() {}
};


class InfixParselet : virtual public Parselet {
public:
    
    virtual ParseFunction parseInfix() const = 0;
    
    virtual Precedence getPrecedence() const = 0;
    
    virtual Symbol getOp() const;
    
    virtual Token processImplicitTimes(Token TokIn) const;
    
    virtual ~InfixParselet() {}
};


class CallParselet : public InfixParselet {
private:
    
    const PrefixParseletPtr GP;
    
public:
    
    CallParselet(PrefixParseletPtr GP);
    
    PrefixParseletPtr getGP() const;
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void CallParselet_parseInfix(ParseletPtr P, Token firstTok);
void CallParselet_reduceCall(ParseletPtr P, Token Ignored);


class LeafParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void LeafParselet_parsePrefix(ParseletPtr P, Token firstTok);


class PrefixEndOfFileParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixEndOfFileParselet_parsePrefix(ParseletPtr P, Token firstTok);


class PrefixErrorParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixErrorParselet_parsePrefix(ParseletPtr P, Token firstTok);


class PrefixCloserParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixCloserParselet_parsePrefix(ParseletPtr P, Token firstTok);


class PrefixToplevelCloserParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixToplevelCloserParselet_parsePrefix(ParseletPtr P, Token firstTok);


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


class PrefixOperatorParselet : public PrefixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    PrefixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op);
    
    ParseFunction parsePrefix() const override;
    
    Precedence getPrecedence() const;
    
    Symbol getOp() const;
};

void PrefixOperatorParselet_parsePrefix(ParseletPtr P, Token firstTok);
void PrefixOperatorParselet_reducePrefixOperator(ParseletPtr P, Token Ignored);


class InfixImplicitTimesParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
    
    Token processImplicitTimes(Token TokIn) const override;
};

void InfixImplicitTimesParselet_parseInfix(ParseletPtr P, Token firstTok);


class InfixAssertFalseParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
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


class InfixToplevelNewlineParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void InfixToplevelNewlineParselet_parseInfix(ParseletPtr P, Token firstTok);


class BinaryOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op);
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
    
    Symbol getOp() const override;
};

void BinaryOperatorParselet_parseInfix(ParseletPtr P, Token firstTok);
void BinaryOperatorParselet_reduceBinaryOperator(ParseletPtr P, Token Ignored);


class InfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    InfixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op);
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
    
    Symbol getOp() const override;
};

void InfixOperatorParselet_parseInfix(ParseletPtr P, Token firstTok);
void InfixOperatorParselet_parseLoop(ParseletPtr P, Token Ignored);
void InfixOperatorParselet_reduceInfixOperator(ParseletPtr P, Token firstTok);


class PostfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    PostfixOperatorParselet(TokenEnum Tok, Precedence precedence, Symbol Op);
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
    
    Symbol getOp() const override;
};

void PostfixOperatorParselet_parseInfix(ParseletPtr P, Token firstTok);
void PostfixOperatorParselet_reducePostfixOperator(ParseletPtr P, Token firstTok);


class GroupParselet : public PrefixParselet {
private:
    
    const Symbol Op;
    const Closer Closr;
    
public:
    
    GroupParselet(TokenEnum Opener, Symbol Op);
    
    Symbol getOp() const;
    
    Closer getCloser() const;
    
    ParseFunction parsePrefix() const override;
};

void GroupParselet_parsePrefix(ParseletPtr P, Token firstTok);
void GroupParselet_parseLoop(ParseletPtr P, Token Ignored);
void GroupParselet_reduceGroup(ParseletPtr P, Token firstTok);
void GroupParselet_reduceMissingCloser(ParseletPtr P, Token Ignored);
void GroupParselet_reduceUnterminatedGroup(ParseletPtr P, Token Ignored);


class SymbolParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void SymbolParselet_parsePrefix(ParseletPtr P, Token firstTok);
void SymbolParselet_parseInfixContextSensitive(ParseletPtr P, Token firstTok);
void SymbolParselet_reducePatternBlank(ParseletPtr P, Token firstTok);
void SymbolParselet_reducePatternOptionalDefault(ParseletPtr P, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class CommaParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
    
    Symbol getOp() const override;
};

void CommaParselet_parseInfix(ParseletPtr P, Token firstTok);
void CommaParselet_parseLoop(ParseletPtr P, Token Ignored);
void CommaParselet_reduceComma(ParseletPtr P, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void SemiParselet_parseInfix(ParseletPtr P, Token firstTok);
void SemiParselet_parse1(ParseletPtr P, Token Ignored);
void SemiParselet_parseLoop(ParseletPtr P, Token Ignored);
void SemiParselet_reduceCompoundExpression(ParseletPtr P, Token Ignored);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
public:
    
    ParseFunction parsePrefix() const override;
    
    ParseFunction parseInfix() const override;
    
    Token processImplicitTimes(Token TokIn) const override;
    
    Precedence getPrecedence() const override;
};

void SemiSemiParselet_parsePrefix(ParseletPtr P, Token firstTok);
void SemiSemiParselet_parseInfix(ParseletPtr P, Token firstTok);
void SemiSemiParselet_parse1(ParseletPtr P, Token Ignored);
void SemiSemiParselet_parse2(ParseletPtr P, Token Ignored);
void SemiSemiParselet_reduceBinary(ParseletPtr P, Token Ignored);
void SemiSemiParselet_reduceTernary(ParseletPtr P, Token Ignored);


class TildeParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void TildeParselet_parseInfix(ParseletPtr P, Token firstTok);
void TildeParselet_parse1(ParseletPtr P, Token Ignored);
void TildeParselet_reduceTilde(ParseletPtr P, Token Ignored);
void TildeParselet_reduceError(ParseletPtr P, Token Ignored);


class ColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void ColonParselet_parseInfix(ParseletPtr P, Token firstTok);
void ColonParselet_reducePattern(ParseletPtr P, Token Ignored);
void ColonParselet_reduceError(ParseletPtr P, Token Ignored);
void ColonParselet_reduceOptional(ParseletPtr P, Token Ignored);


class SlashColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void SlashColonParselet_parseInfix(ParseletPtr P, Token firstTok);
void SlashColonParselet_parse1(ParseletPtr P, Token Ignored);
void SlashColonParselet_reduceError(ParseletPtr P, Token Ignored);


class EqualParselet : public BinaryOperatorParselet {
public:
    
    EqualParselet();
    
    ParseFunction parseInfix() const override;
};

void EqualParselet_parseInfix(ParseletPtr P, Token firstTok);
void EqualParselet_parseInfixTag(ParseletPtr P, Token TokIn);
void EqualParselet_reduceSet(ParseletPtr P, Token Ignored);
void EqualParselet_reduceTagSet(ParseletPtr P, Token Ignored);
void EqualParselet_reduceUnset(ParseletPtr P, Token Ignored);
void EqualParselet_reduceTagUnset(ParseletPtr P, Token Ignored);


class ColonEqualParselet : public BinaryOperatorParselet {
public:
    
    ColonEqualParselet();
    
    ParseFunction parseInfix() const override;
};

void ColonEqualParselet_parseInfix(ParseletPtr P, Token firstTok);
void ColonEqualParselet_parseInfixTag(ParseletPtr P, Token firstTok);
void ColonEqualParselet_reduceSetDelayed(ParseletPtr P, Token Ignored);
void ColonEqualParselet_reduceTagSetDelayed(ParseletPtr P, Token Ignored);


class IntegralParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void IntegralParselet_parsePrefix(ParseletPtr P, Token firstTok);
void IntegralParselet_parse1(ParseletPtr P, Token Ignored);
void IntegralParselet_reduceIntegrate(ParseletPtr P, Token Ignored);
void IntegralParselet_reduceIntegral(ParseletPtr P, Token Ignored);


class ColonColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void ColonColonParselet_parseInfix(ParseletPtr P, Token firstTok);
void ColonColonParselet_parseLoop(ParseletPtr P, Token Ignored);
void ColonColonParselet_reduceMessageName(ParseletPtr P, Token Ignored);


class GreaterGreaterParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void GreaterGreaterParselet_parseInfix(ParseletPtr P, Token firstTok);
void GreaterGreaterParselet_reducePut(ParseletPtr P, Token TokIn);


class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence() const override;
};

void GreaterGreaterGreaterParselet_parseInfix(ParseletPtr P, Token firstTok);
void GreaterGreaterGreaterParselet_reducePutAppend(ParseletPtr P, Token Ignored);


class LessLessParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void LessLessParselet_parsePrefix(ParseletPtr P, Token firstTok);
void LessLessParselet_reduceGet(ParseletPtr P, Token Ignored);


class HashParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void HashParselet_parsePrefix(ParseletPtr P, Token firstTok);
void HashParselet_reduceSlot(ParseletPtr P, Token Ignored);


class HashHashParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void HashHashParselet_parsePrefix(ParseletPtr P, Token firstTok);
void HashHashParselet_reduceSlotSequence(ParseletPtr P, Token Ignored);


class PercentParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PercentParselet_parsePrefix(ParseletPtr P, Token firstTok);
void PercentParselet_reduceOut(ParseletPtr P, Token Ignored);


class UnderParselet : public PrefixParselet {
private:
    
    const Symbol BOp;
    const Symbol PBOp;
    
public:
    
    UnderParselet(Symbol BOp, Symbol PBOp) : BOp(BOp), PBOp(PBOp) {}
    
    Symbol getBOp() const;
    
    Symbol getPBOp() const;
    
    ParseFunction parsePrefix() const override;
};

void UnderParselet_parsePrefix(ParseletPtr P, Token TokIn);
void UnderParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn);
void UnderParselet_reduceBlank(ParseletPtr P, Token Ignored);
void UnderParselet_reduceBlankContextSensitive(ParseletPtr P, Token Ignored);


class UnderDotParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void UnderDotParselet_parsePrefix(ParseletPtr P, Token TokIn);
void UnderDotParselet_parseInfixContextSensitive(ParseletPtr P, Token TokIn);
