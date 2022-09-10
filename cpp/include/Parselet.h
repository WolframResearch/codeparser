
#pragma once

#include "Token.h" // for Token
#include "Precedence.h"
#include "Symbol.h"

class PrefixParselet;
class Parselet;
class ParserSession;

using ParseletPtr = Parselet *;
using PrefixParseletPtr = PrefixParselet *;
using ParserSessionPtr = ParserSession *;
using ParseFunction = void(ParserSessionPtr parser, ParseletPtr parselet, Token firstTok);
using ParseFunctionPtr = ParseFunction *;

//
// Classes that derive from Parselet are responsible for parsing specific kinds of syntax
//
class Parselet {
public:
    
    virtual ~Parselet() {}
};


class PrefixParselet : virtual public Parselet {
public:
    
    const ParseFunctionPtr parsePrefix;
    
    PrefixParselet(ParseFunctionPtr parsePrefix);
    
    virtual ~PrefixParselet() {}
};


class InfixParselet : virtual public Parselet {
public:
    
    const ParseFunctionPtr parseInfix;
    
    InfixParselet(ParseFunctionPtr parseInfix);
    
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
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction CallParselet_parseInfix;
ParseFunction CallParselet_reduceCall;


class LeafParselet : public PrefixParselet {
public:
    
    LeafParselet();
};

ParseFunction LeafParselet_reduceLeaf;


class PrefixEndOfFileParselet : public PrefixParselet {
public:
    
    PrefixEndOfFileParselet();
};

ParseFunction PrefixEndOfFileParselet_parsePrefix;


class PrefixErrorParselet : public PrefixParselet {
public:
    
    PrefixErrorParselet();
};

ParseFunction PrefixErrorParselet_parsePrefix;


class PrefixCloserParselet : public PrefixParselet {
public:
    
    PrefixCloserParselet();
};

ParseFunction PrefixCloserParselet_parsePrefix;


class PrefixToplevelCloserParselet : public PrefixParselet {
public:
    
    PrefixToplevelCloserParselet();
};

ParseFunction PrefixToplevelCloserParselet_parsePrefix;


class PrefixUnsupportedTokenParselet : public PrefixParselet {
public:
    
    PrefixUnsupportedTokenParselet();
};

ParseFunction PrefixUnsupportedTokenParselet_parsePrefix;


//
// The comma infix operator can have leading commas with no operand
//
class PrefixCommaParselet : public PrefixParselet {
public:
    
    PrefixCommaParselet();
};

ParseFunction PrefixCommaParselet_parsePrefix;


//
// All other unhandleds are handled here
//
class PrefixUnhandledParselet : public PrefixParselet {
public:
    
    PrefixUnhandledParselet();
};

ParseFunction PrefixUnhandledParselet_parsePrefix;


class PrefixOperatorParselet : public PrefixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    PrefixOperatorParselet(Precedence precedence, Symbol Op);
    
    Precedence getPrecedence() const;
    
    Symbol getOp() const;
};

ParseFunction PrefixOperatorParselet_parsePrefix;
ParseFunction PrefixOperatorParselet_reducePrefixOperator;


class InfixImplicitTimesParselet : public InfixParselet {
public:
    
    InfixImplicitTimesParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const override;
};


class InfixAssertFalseParselet : public InfixParselet {
public:
    
    InfixAssertFalseParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction InfixAssertFalseParselet_parseInfix;


//
// InfixDifferentialDParselet only exists to properly supply precedence, depending on context
//
class InfixDifferentialDParselet : public InfixParselet {
public:
    
    InfixDifferentialDParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const override;
};


class InfixToplevelNewlineParselet : public InfixParselet {
public:
    
    InfixToplevelNewlineParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};


class BinaryOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    BinaryOperatorParselet(Precedence precedence, Symbol Op);
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

ParseFunction BinaryOperatorParselet_parseInfix;
ParseFunction BinaryOperatorParselet_reduceBinaryOperator;


class InfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    InfixOperatorParselet(Precedence precedence, Symbol Op);
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

ParseFunction InfixOperatorParselet_parseInfix;
ParseFunction InfixOperatorParselet_parseLoop;
ParseFunction InfixOperatorParselet_reduceInfixOperator;


class TimesParselet : public InfixParselet {
public:
    
    TimesParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

ParseFunction TimesParselet_parseInfix;
ParseFunction TimesParselet_parseLoop;
ParseFunction TimesParselet_reduceTimes;


class PostfixOperatorParselet : public InfixParselet {
private:
    
    const Precedence precedence;
    const Symbol Op;
    
public:
    
    PostfixOperatorParselet(Precedence precedence, Symbol Op);
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

ParseFunction PostfixOperatorParselet_parseInfix;
ParseFunction PostfixOperatorParselet_reducePostfixOperator;


class GroupParselet : public PrefixParselet {
private:
    
    const Symbol Op;
    const Closer Closr;
    
public:
    
    GroupParselet(TokenEnum Opener, Symbol Op);
    
    Symbol getOp() const;
    
    Closer getCloser() const;
};

ParseFunction GroupParselet_parsePrefix;
ParseFunction GroupParselet_parseLoop;
ParseFunction GroupParselet_reduceGroup;
ParseFunction GroupParselet_reduceMissingCloser;
ParseFunction GroupParselet_reduceUnterminatedGroup;


class SymbolParselet : public PrefixParselet {
public:
    
    SymbolParselet();
};

ParseFunction SymbolParselet_parsePrefix;
ParseFunction SymbolParselet_parseInfixContextSensitive;
ParseFunction SymbolParselet_reducePatternBlank;
ParseFunction SymbolParselet_reducePatternOptionalDefault;


class CommaParselet : public InfixParselet {
public:
    
    CommaParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction CommaParselet_parseInfix;
ParseFunction CommaParselet_parseLoop;
ParseFunction CommaParselet_reduceComma;


class SemiParselet : public InfixParselet {
public:
    
    SemiParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction SemiParselet_parseInfix;
ParseFunction SemiParselet_parse1;
ParseFunction SemiParselet_parseLoop;
ParseFunction SemiParselet_reduceCompoundExpression;


class SemiSemiParselet : public PrefixParselet, public InfixParselet {
public:
    
    SemiSemiParselet();
    
    Token processImplicitTimes(ParserSessionPtr session, Token TokIn) const override;
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction SemiSemiParselet_parsePrefix;
ParseFunction SemiSemiParselet_parseInfix;
ParseFunction SemiSemiParselet_parse1;
ParseFunction SemiSemiParselet_parse2;
ParseFunction SemiSemiParselet_reduceBinary;
ParseFunction SemiSemiParselet_reduceTernary;


class TildeParselet : public InfixParselet {
public:
    
    TildeParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction TildeParselet_parseInfix;
ParseFunction TildeParselet_parse1;
ParseFunction TildeParselet_reduceTilde;
ParseFunction TildeParselet_reduceError;


class ColonParselet : public InfixParselet {
public:
    
    ColonParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction ColonParselet_parseInfix;
ParseFunction ColonParselet_reducePattern;
ParseFunction ColonParselet_reduceError;
ParseFunction ColonParselet_reduceOptional;


class SlashColonParselet : public InfixParselet {
public:
    
    SlashColonParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction SlashColonParselet_parseInfix;
ParseFunction SlashColonParselet_parse1;
ParseFunction SlashColonParselet_reduceError;


class EqualParselet : public InfixParselet {
public:
    
    EqualParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

ParseFunction EqualParselet_parseInfix;
ParseFunction EqualParselet_parseInfixTag;
ParseFunction EqualParselet_reduceSet;
ParseFunction EqualParselet_reduceTagSet;
ParseFunction EqualParselet_reduceUnset;
ParseFunction EqualParselet_reduceTagUnset;


class ColonEqualParselet : public InfixParselet {
public:
    
    ColonEqualParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
    
    Symbol getOp() const override;
};

ParseFunction ColonEqualParselet_parseInfix;
ParseFunction ColonEqualParselet_parseInfixTag;
ParseFunction ColonEqualParselet_reduceSetDelayed;
ParseFunction ColonEqualParselet_reduceTagSetDelayed;


class IntegralParselet : public PrefixParselet {
private:
    
    const Symbol Op1;
    const Symbol Op2;
    
public:
    
    IntegralParselet(Symbol Op1, Symbol Op2);
    
    Symbol getOp1() const;
    
    Symbol getOp2() const;
};

ParseFunction IntegralParselet_parsePrefix;
ParseFunction IntegralParselet_parse1;
ParseFunction IntegralParselet_reduceIntegrate;
ParseFunction IntegralParselet_reduceIntegral;


class ColonColonParselet : public InfixParselet {
public:
    
    ColonColonParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction ColonColonParselet_parseInfix;
ParseFunction ColonColonParselet_parseLoop;
ParseFunction ColonColonParselet_reduceMessageName;


class GreaterGreaterParselet : public InfixParselet {
public:
    
    GreaterGreaterParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction GreaterGreaterParselet_parseInfix;
ParseFunction GreaterGreaterParselet_reducePut;


class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    GreaterGreaterGreaterParselet();
    
    Precedence getPrecedence(ParserSessionPtr session) const override;
};

ParseFunction GreaterGreaterGreaterParselet_parseInfix;
ParseFunction GreaterGreaterGreaterParselet_reducePutAppend;


class LessLessParselet : public PrefixParselet {
public:
    
    LessLessParselet();
};

ParseFunction LessLessParselet_parsePrefix;
ParseFunction LessLessParselet_reduceGet;


class HashParselet : public PrefixParselet {
public:
    
    HashParselet();
};

ParseFunction HashParselet_parsePrefix;
ParseFunction HashParselet_reduceSlot;


class HashHashParselet : public PrefixParselet {
public:
    
    HashHashParselet();
};

ParseFunction HashHashParselet_parsePrefix;
ParseFunction HashHashParselet_reduceSlotSequence;


class PercentParselet : public PrefixParselet {
public:
    
    PercentParselet();
};

ParseFunction PercentParselet_parsePrefix;
ParseFunction PercentParselet_reduceOut;


class UnderParselet : public PrefixParselet {
private:
    
    const Symbol BOp;
    const Symbol PBOp;
    
public:
    
    UnderParselet(Symbol BOp, Symbol PBOp);
    
    Symbol getBOp() const;
    
    Symbol getPBOp() const;
};

ParseFunction UnderParselet_parsePrefix;
ParseFunction UnderParselet_parseInfixContextSensitive;
ParseFunction UnderParselet_reduceBlank;
ParseFunction UnderParselet_reduceBlankContextSensitive;


class UnderDotParselet : public PrefixParselet {
public:
    
    UnderDotParselet();
};

ParseFunction UnderDotParselet_parsePrefix;
ParseFunction UnderDotParselet_parseInfixContextSensitive;
