
#pragma once

#include "Parser.h" // for ParserContext
#include "Token.h" // for Token
#include "Symbol.h"

#include <memory> // for unique_ptr

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
    virtual NodePtr parse(Token firstTok, ParserContext Ctxt) const = 0;
    
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
    virtual NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual Precedence getPrecedence(ParserContext Ctxt) const = 0;
    
    virtual SymbolPtr& getOp() const {
        return SYMBOL_CODEPARSER_INTERNALINVALID;
    }
    
    virtual Token procesImplicitTimes(Token TokIn) const {
        return TokIn;
    }
    
    virtual ~InfixParselet() {}
};

//
//
//
class CallParselet : public InfixParselet {
    PrefixParseletPtr GP;
public:
    CallParselet(PrefixParseletPtr GP) : GP(std::move(GP)) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_CALL;
    }
};

//
//
//
class ContextSensitivePrefixParselet : virtual public Parselet {
public:
    
    virtual NodePtr parseContextSensitive(Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitivePrefixParselet() {}
};

//
//
//
class ContextSensitiveInfixParselet : virtual public Parselet {
public:
    
    virtual NodePtr parseContextSensitive(NodeSeq Left, Token firstTok, ParserContext Ctxt) const = 0;
    
    virtual ~ContextSensitiveInfixParselet() {}
};

//
//
//
class LeafParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixEndOfFileParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixErrorParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixCloserParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixUnsupportedTokenParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixUnhandledParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class PrefixOperatorParselet : public PrefixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    PrefixOperatorParselet(TokenEnum Tok, Precedence precedence, SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    virtual Precedence getPrecedence(ParserContext Ctxt) const {
        return precedence;
    }
};

//
//
//
class InfixImplicitTimesParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
    
    Token procesImplicitTimes(Token TokIn) const override;
};

//
//
//
class InfixEndOfFileParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_LOWEST;
    }
};

//
//
//
class InfixErrorParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_LOWEST;
    }
};

//
//
//
class InfixUnsupportedTokenParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_LOWEST;
    }
};

//
//
//
class InfixCloserParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_LOWEST;
    }
};

//
// InfixDifferentialDParselet only exists to properly supply precedence, depending on context
//
class InfixDifferentialDParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override;
};

//
//
//
class InfixToplevelNewlineParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
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
    Precedence precedence;
    SymbolPtr& Op;
public:
    BinaryOperatorParselet(TokenEnum Tok, Precedence precedence, SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    SymbolPtr& getOp() const override {
        return Op;
    }
};

//
//
//
class InfixOperatorParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    InfixOperatorParselet(TokenEnum Tok, Precedence precedence, SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    SymbolPtr& getOp() const override {
        return Op;
    }
};

//
//
//
class PostfixOperatorParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    PostfixOperatorParselet(TokenEnum Tok, Precedence precedence, SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    SymbolPtr& getOp() const override {
        return Op;
    }
};

//
//
//
class GroupParselet : public PrefixParselet {
    SymbolPtr& Op;
    Closer Closr;
public:
    GroupParselet(TokenEnum Opener, SymbolPtr& Op) : Op(Op), Closr(GroupOpenerToCloser(Opener)) {}
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};


//
// Special parselets
//

//
// something like  x  or x_
//
class SymbolParselet : public PrefixParselet, public ContextSensitivePrefixParselet {
public:
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
    NodePtr parseContextSensitive(Token firstTok, ParserContext Ctxt) const override;
};

//
// xxx
//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class InfixOperatorWithTrailingParselet : public InfixParselet {
    Precedence precedence;
    SymbolPtr& Op;
public:
    InfixOperatorWithTrailingParselet(TokenEnum Tok, Precedence precedence, SymbolPtr& Op) : precedence(precedence), Op(Op) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return precedence;
    }
    
    SymbolPtr& getOp() const override {
        return Op;
    }
};

//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
class SemiSemiParselet : public PrefixParselet, public InfixParselet {
    
    //
    // infix
    //
    // Something like  a;;b
    //
    // Parses a single complete Span
    //
    NodePtr parse0(NodeSeq Left, Token firstTok, ParserContext Ctxt) const;
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
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
    
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
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
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
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
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
public:
    
    //
    // Something like  symbol:object
    //
    // when parsing a in a:b  then ColonFlag is false
    // when parsing b in a:b  then ColonFlag is true
    //
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    //
    // Something like  pattern:optional
    //
    // Called from other parselets
    //
    NodePtr parseContextSensitive(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
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
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_SLASHCOLON;
    }
};

//
// Something like  \( x \)
//
class LinearSyntaxOpenParenParselet : public PrefixParselet {
public:
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
// a /: b = c  and  a /: b = .  are handled here
//
class EqualParselet : public BinaryOperatorParselet {
public:
    EqualParselet() : BinaryOperatorParselet(TOKEN_EQUAL, PRECEDENCE_EQUAL, SYMBOL_SET) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
};

//
// a /: b := c  is handled here
//
class ColonEqualParselet : public BinaryOperatorParselet {
public:
    ColonEqualParselet() : BinaryOperatorParselet(TOKEN_COLONEQUAL, PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED) {}
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
};


//
// Something like  \[Integral] f \[DifferentialD] x
//
class IntegralParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
// a::b
//
class ColonColonParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_COLONCOLON;
    }
};

//
// a>>b
//
class GreaterGreaterParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_GREATERGREATER;
    }
};

//
// a>>>b
//
class GreaterGreaterGreaterParselet : public InfixParselet {
public:
    
    NodePtr parse(NodeSeq Left, Token firstTok, ParserContext Ctxt) const override;
    
    Precedence getPrecedence(ParserContext Ctxt) const override {
        return PRECEDENCE_GREATERGREATERGREATER;
    }
};

//
// <<a
//
class LessLessParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
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
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  ##  or  ##1
//
class HashHashParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  %  or  %1
//
class PercentParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
// Something like  %%  or  %%%
//
class PercentPercentParselet : public PrefixParselet {
public:
    
    NodePtr parse(Token firstTok, ParserContext Ctxt) const override;
};

//
//
//
class UnderParselet : public PrefixParselet, public ContextSensitiveInfixParselet {
    SymbolPtr& BOp;
    SymbolPtr& PBOp;
    
    NodePtr parse0(Token TokIn, ParserContext Ctxt) const;
    
    NodePtr parse1(NodePtr Blank, Token Tok, ParserContext Ctxt) const;
    
public:
    
    UnderParselet(SymbolPtr& BOp, SymbolPtr& PBOp) : BOp(BOp), PBOp(PBOp) {}
    
    //
    // prefix
    //
    // Something like  _  or  _a
    //
    NodePtr parse(Token TokIn, ParserContext Ctxt) const override;
    
    //
    // infix
    //
    // Something like  a_b
    //
    // Called from other parselets
    //
    NodePtr parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const override;
};

//
//
//
class UnderDotParselet : public PrefixParselet, public ContextSensitiveInfixParselet {

    NodePtr parse0(Token TokIn, ParserContext Ctxt) const;
    
public:
    
    //
    // prefix
    //
    // Something like  _.
    //
    NodePtr parse(Token TokIn, ParserContext Ctxt) const override;
    
    //
    // infix
    //
    // Something like  a_.
    //
    // Called from other parselets
    //
    NodePtr parseContextSensitive(NodeSeq Left, Token TokIn, ParserContext Ctxt) const override;
};
