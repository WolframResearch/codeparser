
#pragma once

#include "Token.h" // for Token
#include "Precedence.h"

class PrefixParselet;
class Parselet;
class ParserSession;

using ParseletPtr = Parselet *;
using PrefixParseletPtr = PrefixParselet *;
using ParserSessionPtr = ParserSession *;
typedef void (*ParseFunction)(ParserSessionPtr parser, ParseletPtr parselet, Token firstTok);

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
    
    virtual Precedence getPrecedence(ParserSessionPtr session) const = 0;
    
    virtual Symbol getOp() const;
    
    virtual Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const;
    
    virtual ~InfixParselet() {}
};


class CallParselet : public InfixParselet {
private:
    
    const PrefixParseletPtr GP;
    
public:
    
    CallParselet(PrefixParseletPtr GP);
    
    PrefixParseletPtr getGP() const;
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void CallParselet_parseInfix(ParserSessionPtr session, ParseletPtr P, Token firstTok);
void CallParselet_reduceCall(ParserSessionPtr session, ParseletPtr P, Token Ignored);


class LeafParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void LeafParselet_reduceLeaf(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class PrefixEndOfFileParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixEndOfFileParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class PrefixErrorParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixErrorParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class PrefixCloserParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixCloserParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class PrefixToplevelCloserParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixToplevelCloserParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class PrefixUnsupportedTokenParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixUnsupportedTokenParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


//
// The comma infix operator can have leading commas with no operand
//
class PrefixCommaParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixCommaParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


//
// All other unhandleds are handled here
//
class PrefixUnhandledParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PrefixUnhandledParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class PrefixOperatorParselet : public PrefixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    PrefixOperatorParselet(Precedence precedence, Symbol Op);
    
    ParseFunction parsePrefix() const override;
    
    Precedence getPrecedence() const;
    
    Symbol getOp() const;
};

void PrefixOperatorParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void PrefixOperatorParselet_reducePrefixOperator(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class InfixImplicitTimesParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const override;
};

void InfixImplicitTimesParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class InfixAssertFalseParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void InfixAssertFalseParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


//
// InfixDifferentialDParselet only exists to properly supply precedence, depending on context
//
class InfixDifferentialDParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const override;
};


class InfixToplevelNewlineParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void InfixToplevelNewlineParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


class BinaryOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    BinaryOperatorParselet(Precedence precedence, Symbol Op);
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

void BinaryOperatorParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void BinaryOperatorParselet_reduceBinaryOperator(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class InfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    InfixOperatorParselet(Precedence precedence, Symbol Op);
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

void InfixOperatorParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void InfixOperatorParselet_parseLoop(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void InfixOperatorParselet_reduceInfixOperator(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


//
//
//
class TimesParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

void TimesParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void TimesParselet_parseLoop(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void TimesParselet_reduceTimes(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class PostfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    PostfixOperatorParselet(Precedence precedence, Symbol Op);
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

void PostfixOperatorParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void PostfixOperatorParselet_reducePostfixOperator(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


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

void GroupParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void GroupParselet_parseLoop(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void GroupParselet_reduceGroup(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void GroupParselet_reduceMissingCloser(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void GroupParselet_reduceUnterminatedGroup(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class SymbolParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void SymbolParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SymbolParselet_parseInfixContextSensitive(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SymbolParselet_reducePatternBlank(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SymbolParselet_reducePatternOptionalDefault(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class CommaParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void CommaParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void CommaParselet_parseLoop(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void CommaParselet_reduceComma(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void SemiParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SemiParselet_parse1(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void SemiParselet_parseLoop(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void SemiParselet_reduceCompoundExpression(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
public:
    
    ParseFunction parsePrefix() const override;
    
    ParseFunction parseInfix() const override;
    
    Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void SemiSemiParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SemiSemiParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SemiSemiParselet_parse1(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void SemiSemiParselet_parse2(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void SemiSemiParselet_reduceBinary(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void SemiSemiParselet_reduceTernary(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class TildeParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void TildeParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void TildeParselet_parse1(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void TildeParselet_reduceTilde(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void TildeParselet_reduceError(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class ColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void ColonParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void ColonParselet_reducePattern(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void ColonParselet_reduceError(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void ColonParselet_reduceOptional(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class SlashColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void SlashColonParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void SlashColonParselet_parse1(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void SlashColonParselet_reduceError(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class EqualParselet : public BinaryOperatorParselet {
public:
    
    EqualParselet();
    
    ParseFunction parseInfix() const override;
};

void EqualParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void EqualParselet_parseInfixTag(ParserSessionPtr session, ParseletPtr parselet, Token TokIn);
void EqualParselet_reduceSet(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void EqualParselet_reduceTagSet(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void EqualParselet_reduceUnset(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void EqualParselet_reduceTagUnset(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class ColonEqualParselet : public BinaryOperatorParselet {
public:
    
    ColonEqualParselet();
    
    ParseFunction parseInfix() const override;
};

void ColonEqualParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void ColonEqualParselet_parseInfixTag(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void ColonEqualParselet_reduceSetDelayed(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void ColonEqualParselet_reduceTagSetDelayed(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class IntegralParselet : public PrefixParselet {
private:
    
    const Symbol Op1;
    const Symbol Op2;
    
public:
    
    IntegralParselet(Symbol Op1, Symbol Op2);
    
    Symbol getOp1() const;
    
    Symbol getOp2() const;
    
    ParseFunction parsePrefix() const override;
};

void IntegralParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void IntegralParselet_parse1(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void IntegralParselet_reduceIntegrate(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void IntegralParselet_reduceIntegral(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class ColonColonParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void ColonColonParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void ColonColonParselet_parseLoop(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void ColonColonParselet_reduceMessageName(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class GreaterGreaterParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void GreaterGreaterParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void GreaterGreaterParselet_reducePut(ParserSessionPtr session, ParseletPtr parselet, Token TokIn);


class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    ParseFunction parseInfix() const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

void GreaterGreaterGreaterParselet_parseInfix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void GreaterGreaterGreaterParselet_reducePutAppend(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class LessLessParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void LessLessParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void LessLessParselet_reduceGet(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class HashParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void HashParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void HashParselet_reduceSlot(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class HashHashParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void HashHashParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void HashHashParselet_reduceSlotSequence(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class PercentParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void PercentParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token firstTok);
void PercentParselet_reduceOut(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


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

void UnderParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token TokIn);
void UnderParselet_parseInfixContextSensitive(ParserSessionPtr session, ParseletPtr parselet, Token TokIn);
void UnderParselet_reduceBlank(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);
void UnderParselet_reduceBlankContextSensitive(ParserSessionPtr session, ParseletPtr parselet, Token Ignored);


class UnderDotParselet : public PrefixParselet {
public:
    
    ParseFunction parsePrefix() const override;
};

void UnderDotParselet_parsePrefix(ParserSessionPtr session, ParseletPtr parselet, Token TokIn);
void UnderDotParselet_parseInfixContextSensitive(ParserSessionPtr session, ParseletPtr parselet, Token TokIn);
