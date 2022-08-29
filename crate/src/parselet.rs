use std::any::Any;

use crate::{
    node::{
        BinaryNode, CallNode, CompoundNode, GroupMissingCloserNode, GroupNode, InfixNode,
        PostfixNode, PrefixNode, SyntaxErrorNode, TernaryNode, UnterminatedGroupNeedsReparseNode,
    },
    panic_if_aborted,
    parselet_registration::{infixParselets, prefixParselets, *},
    parser::{
        ColonLHS, Parser_checkColonLHS, Parser_checkGroup, Parser_checkPatternPrecedence,
        Parser_checkTilde, Parser_eatTrivia, Parser_eatTriviaButNotToplevelNewlines,
        Parser_eatTrivia_2, Parser_eatTrivia_stringifyAsFile, Parser_eatTrivia_transparent,
        Parser_identity, Parser_parseClimb, Parser_popContext, Parser_popGroup, Parser_popNode,
        Parser_pushContext, Parser_pushContext_transparent, Parser_pushGroup, Parser_pushLeaf,
        Parser_pushLeafAndNext, Parser_pushNode, Parser_pushTriviaSeq, Parser_setPrecedence,
        Parser_topContext, Parser_topPrecedence, Parser_tryContinue,
    },
    parser_session::ParserSession,
    precedence::{Precedence, *},
    source::Source,
    source::*,
    symbol::Symbol,
    symbol_registration::*,
    token::Token,
    token_enum::{Closer, GroupOpenerToCloser, TokenToCloser},
    token_enum_registration::TokenEnum::{self, *},
    tokenizer::{
        Tokenizer_currentToken, Tokenizer_currentToken_stringifyAsFile,
        Tokenizer_currentToken_stringifyAsTag,
    },
    under_parselet::{
        UnderDotParselet_parseInfixContextSensitive, UnderParselet_parseInfixContextSensitive,
    },
};

pub(crate) type ParseletPtr = &'static dyn Parselet;
pub(crate) type PrefixParseletPtr = &'static dyn PrefixParselet;
pub(crate) type InfixParseletPtr = &'static dyn InfixParselet;

// pub(crate) type ParseFunction = fn(parser: &mut ParserSession, parselet: ParseletPtr, firstTok: Token);
pub(crate) type ParseFunction<T = ParseletPtr> =
    for<'i> fn(session: &mut ParserSession<'i>, parselet: T, firstTok: Token);

//
/// Classes that derive from Parselet are responsible for parsing specific kinds of syntax
//
pub(crate) trait Parselet: Any + std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
}

//======================================
// Parselet categories
//======================================

pub(crate) trait PrefixParselet: Parselet {
    // fn parsePrefix(&self) -> ParseFunction<PrefixParseletPtr>;
    fn parsePrefix(&self) -> ParseFunction;
}


pub(crate) trait InfixParselet: Parselet {
    // PRE_COMMIT: Replace with an associated constant;
    // fn parseInfix(&self) -> ParseFunction<InfixParseletPtr>;
    fn parseInfix(&self) -> ParseFunction;

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence;

    fn getOp(&self) -> Symbol {
        return SYMBOL_CODEPARSER_INTERNALINVALID;
    }

    fn processImplicitTimes(&self, _session: &mut ParserSession, tok_in: Token) -> Token {
        return tok_in;
    }
}

//--------------------------------------
// Access parselet for token
//--------------------------------------

/// Get the [`PrefixParselet`] implementation associated with this token.
pub(crate) fn prefix_parselet(tok: TokenEnum) -> PrefixParseletPtr {
    let index = usize::from(tok.value());

    prefixParselets[index]
}

//======================================
// Parselet types
//======================================

macro_rules! impl_Parselet {
    ($($name:ident),* $(,)?) => {
        $(
            impl Parselet for $name {
                fn as_any(&self) -> &dyn Any {
                    self
                }
            }
        )*
    };
}

impl_Parselet!(
    LeafParselet,
    CallParselet,
    PrefixEndOfFileParselet,
    UnderParselet,
    PrefixToplevelCloserParselet,
    BinaryOperatorParselet,
    GroupParselet,
    PrefixOperatorParselet,
    InfixOperatorParselet,
    PostfixOperatorParselet,
    EqualParselet,
    ColonEqualParselet,
    UnderDotParselet,
    PrefixAssertFalseParselet,
    SymbolParselet,
    PrefixUnhandledParselet,
    InfixImplicitTimesParselet,
    CommaParselet,
    TildeParselet,
    InfixAssertFalseParselet,
    InfixDifferentialDParselet,
    TimesParselet,
    SlashColonParselet,
    GreaterGreaterParselet,
    GreaterGreaterGreaterParselet,
    ColonParselet,
    ColonColonParselet,
    PrefixCloserParselet,
    PrefixErrorParselet,
    PrefixCommaParselet,
    SemiParselet,
    SemiSemiParselet,
    InfixToplevelNewlineParselet,
    HashParselet,
    HashHashParselet,
    PercentParselet,
    PrefixUnsupportedTokenParselet,
    IntegralParselet,
    LessLessParselet,
);

#[derive(Debug)]
pub(crate) struct CallParselet /*: public InfixParselet*/ {
    GP: PrefixParseletPtr,
    // CallParselet(PrefixParseletPtr GP);

    // PrefixParseletPtr getGP() const;

    // ParseFunction parseInfix() const override;

    // Precedence getPrecedence(session: &mut ParserSession) const override;
}

#[derive(Debug)]
pub(crate) struct LeafParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PrefixEndOfFileParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PrefixErrorParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PrefixCloserParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PrefixToplevelCloserParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PrefixUnsupportedTokenParselet /* : PrefixParselet */ {}


//
// The comma infix operator can have leading commas with no operand
//
#[derive(Debug)]
pub(crate) struct PrefixCommaParselet /* : PrefixParselet */ {}


//
// All other unhandleds are handled here
//
#[derive(Debug)]
pub(crate) struct PrefixUnhandledParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PrefixOperatorParselet /* : PrefixParselet */ {
    precedence: Precedence,
    Op: Symbol,
}

#[derive(Debug)]
pub(crate) struct PrefixAssertFalseParselet /* : InfixParselet */ {}

#[derive(Debug)]
pub(crate) struct InfixImplicitTimesParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct InfixAssertFalseParselet /* : InfixParselet */ {}


//
// InfixDifferentialDParselet only exists to properly supply precedence, depending on context
//
#[derive(Debug)]
pub(crate) struct InfixDifferentialDParselet /* : InfixParselet */ {}



#[derive(Debug)]
pub(crate) struct InfixToplevelNewlineParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct BinaryOperatorParselet /* : InfixParselet */ {
    // private:
    precedence: Precedence,
    Op: Symbol,
}


#[derive(Debug)]
pub(crate) struct InfixOperatorParselet /* : InfixParselet */ {
    // private:
    precedence: Precedence,
    Op: Symbol,
}


//
//
//
#[derive(Debug)]
pub(crate) struct TimesParselet /* : InfixParselet */ {}



#[derive(Debug)]
pub(crate) struct PostfixOperatorParselet /* : InfixParselet */ {
    // private:
    precedence: Precedence,
    Op: Symbol,
}


#[derive(Debug)]
pub(crate) struct GroupParselet /* : PrefixParselet */ {
    // private:
    Op: Symbol,
    closer: Closer,
    // public:

    //     GroupParselet(Opener: TokenEnum, Op: Symbol);

    //     Closer getCloser() const;
}


#[derive(Debug)]
pub(crate) struct SymbolParselet /* : PrefixParselet */ {}


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
#[derive(Debug)]
pub(crate) struct CommaParselet /* : InfixParselet */ {}


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
#[derive(Debug)]
pub(crate) struct SemiParselet /* : InfixParselet */ {}


//
// Deliberately not extending PrefixOperatorParselet and InfixOperatorParselet because I don't feel like bothering with
// multiple inheritance
//
#[derive(Debug)]
pub(crate) struct SemiSemiParselet /* : public PrefixParselet, public InfixParselet */ {}



#[derive(Debug)]
pub(crate) struct TildeParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct ColonParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct SlashColonParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct EqualParselet /* : BinaryOperatorParselet */ {
    // EqualParselet();
    op: BinaryOperatorParselet,
}


#[derive(Debug)]
pub(crate) struct ColonEqualParselet /* : BinaryOperatorParselet */ {
    // ColonEqualParselet();
    op: BinaryOperatorParselet,
}


#[derive(Debug)]
pub(crate) struct IntegralParselet /* : PrefixParselet */ {
    pub(crate) Op1: Symbol,
    pub(crate) Op2: Symbol,
}



#[derive(Debug)]
pub(crate) struct ColonColonParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct GreaterGreaterParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct GreaterGreaterGreaterParselet /* : InfixParselet */ {}


#[derive(Debug)]
pub(crate) struct LessLessParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct HashParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct HashHashParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct PercentParselet /* : PrefixParselet */ {}


#[derive(Debug)]
pub(crate) struct UnderParselet /* : PrefixParselet */ {
    pub BOp: Symbol,
    pub PBOp: Symbol,
}


#[derive(Debug)]
pub(crate) struct UnderDotParselet /* : PrefixParselet */ {}



//======================================
// LeafParselet
//======================================

impl PrefixParselet for LeafParselet {
    fn parsePrefix(&self) -> ParseFunction {
        LeafParselet_reduceLeaf
    }
}

fn LeafParselet_reduceLeaf(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    Parser_pushLeafAndNext(session, TokIn);

    // MUSTTAIL
    return Parser_parseClimb(session, TokIn /*ignored*/);
}

//======================================
// PrefixErrorParselet
//======================================

impl PrefixParselet for PrefixErrorParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixErrorParselet_parsePrefix
    }
}

fn PrefixErrorParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    assert!(TokIn.tok.isError());

    Parser_pushLeafAndNext(session, TokIn);

    // MUSTTAIL
    return Parser_tryContinue(session, TokIn /*ignored*/);
}

//======================================
// PrefixCloserParselet
//======================================

impl PrefixParselet for PrefixCloserParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixCloserParselet_parsePrefix
    }
}

fn PrefixCloserParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    assert!(TokIn.tok.isCloser());

    panic_if_aborted!();


    //
    // Inside some other parselet that is not GroupParselet
    //

    let createdToken: Token;

    if Parser_topPrecedence(session) == PRECEDENCE_COMMA {
        createdToken = Token::new2(
            TOKEN_ERROR_INFIXIMPLICITNULL,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    } else {
        createdToken = Token::new2(
            TOKEN_ERROR_EXPECTEDOPERAND,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    }

    Parser_pushLeaf(session, createdToken);

    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    // MUSTTAIL
    return Parser_tryContinue(session, TokIn /*ignored*/);
}

//======================================
// PrefixToplevelCloserParselet
//======================================

impl PrefixParselet for PrefixToplevelCloserParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixToplevelCloserParselet_parsePrefix
    }
}

pub(crate) fn PrefixToplevelCloserParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    assert!(TokIn.tok.isCloser());

    panic_if_aborted!();


    //
    // if we are at the top, then make sure to take the token and report it
    //

    Parser_pushLeaf(
        session,
        Token::new2(TOKEN_ERROR_UNEXPECTEDCLOSER, TokIn.span, TokIn.src),
    );

    TokIn.skip(&mut session.tokenizer);

    // MUSTTAIL
    return Parser_tryContinue(session, TokIn /*ignored*/);
}

//======================================
// PrefixEndOfFileParselet
//======================================

impl PrefixParselet for PrefixEndOfFileParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixEndOfFileParselet_parsePrefix
    }
}

fn PrefixEndOfFileParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    //
    // Something like  a+<EOF>
    //

    panic_if_aborted!();


    let createdToken: Token;

    if Parser_topPrecedence(session) == PRECEDENCE_COMMA {
        createdToken = Token::new2(
            TOKEN_ERROR_INFIXIMPLICITNULL,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    } else {
        createdToken = Token::new2(
            TOKEN_ERROR_EXPECTEDOPERAND,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    }

    Parser_pushLeaf(session, createdToken);

    // MUSTTAIL
    return Parser_tryContinue(session, TokIn /*ignored*/);
}

//======================================
// PrefixUnsupportedParselet
//======================================

impl PrefixParselet for PrefixUnsupportedTokenParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixUnsupportedTokenParselet_parsePrefix
    }
}

fn PrefixUnsupportedTokenParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    panic_if_aborted!();


    Parser_pushLeaf(
        session,
        Token::new2(TOKEN_ERROR_UNSUPPORTEDTOKEN, TokIn.span, TokIn.src),
    );

    TokIn.skip(&mut session.tokenizer);

    // MUSTTAIL
    return Parser_tryContinue(session, TokIn /*ignored*/);
}

//======================================
// PrefixCommaParselet
//======================================

impl PrefixParselet for PrefixCommaParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixCommaParselet_parsePrefix
    }
}

fn PrefixCommaParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    //
    // if the input is  f[a@,2]  then we want to return TOKEN_ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TOKEN_ERROR_PREFIXIMPLICITNULL
    //

    panic_if_aborted!();


    let createdToken: Token;

    if Parser_topPrecedence(session) == PRECEDENCE_LOWEST {
        createdToken = Token::new2(
            TOKEN_ERROR_PREFIXIMPLICITNULL,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    } else {
        createdToken = Token::new2(
            TOKEN_ERROR_EXPECTEDOPERAND,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    }

    Parser_pushLeaf(session, createdToken);

    // MUSTTAIL
    return Parser_parseClimb(session, TokIn /*ignored*/);
}

//======================================
// PrefixUnhandledParselet
//======================================

impl PrefixParselet for PrefixUnhandledParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixUnhandledParselet_parsePrefix
    }
}

fn PrefixUnhandledParselet_parsePrefix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    assert!(!TokIn.tok.isPossibleBeginning(), "handle at call site");

    panic_if_aborted!();


    Parser_pushLeaf(
        session,
        Token::new2(
            TOKEN_ERROR_EXPECTEDOPERAND,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        ),
    );

    //
    // Do not take next token
    //
    Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    let I = infixParselets[usize::from(TokIn.tok.value())];

    let TokenPrecedence = I.getPrecedence(session);

    //
    // if (Ctxt.prec > TokenPrecedence)
    //   goto prefixUnhandledParseletRet;
    // else if (Ctxt.prec == TokenPrecedence && Ctxt.prec.Associativity is NonRight)
    //   goto prefixUnhandledParseletRet;
    //
    if (Parser_topPrecedence(session) | 0x1) > TokenPrecedence {
        //
        // Something like  a + | 2
        //
        // Make sure that the error leaf is with the + and not the |
        //

        // MUSTTAIL
        return Parser_tryContinue(session, TokIn /*ignored*/);
    }

    //
    // Handle something like  f[@2]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Operator node.
    //

    Parser_pushContext(session, TokenPrecedence);

    let P2 = infixParselets[usize::from(TokIn.tok.value())];

    // MUSTTAIL
    return (P2.parseInfix())(session, P2, TokIn);
}

//======================================
// InfixToplevelNewlineParselet
//======================================

impl InfixParselet for InfixToplevelNewlineParselet {
    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        //
        // Do not do Implicit Times across top-level newlines
        //
        return PRECEDENCE_LOWEST;
    }

    fn parseInfix(&self) -> ParseFunction {
        assert!(false);

        return InfixToplevelNewlineParselet_parseInfix;
    }
}


fn InfixToplevelNewlineParselet_parseInfix(
    _: &mut ParserSession,
    _: ParseletPtr,
    _firstTok: Token,
) {
    assert!(false);

    return;
}

//======================================
// SymbolParselet
//======================================

impl PrefixParselet for SymbolParselet {
    fn parsePrefix(&self) -> ParseFunction {
        SymbolParselet_parsePrefix
    }
}

fn SymbolParselet_parsePrefix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // Something like  x  or x_
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // if we are here, then we know that Sym could bind to _
    //

    match Tok.tok {
        TOKEN_UNDER => {
            //
            // Something like  a_
            //

            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderParselet_parseInfixContextSensitive(session, &under1Parselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternBlank(
                session,
                &under1Parselet,
                Tok, /*ignored*/
            );
        },
        TOKEN_UNDERUNDER => {
            //
            // Something like  a__
            //

            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderParselet_parseInfixContextSensitive(session, &under2Parselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternBlank(
                session,
                &under2Parselet,
                Tok, /*ignored*/
            );
        },
        TOKEN_UNDERUNDERUNDER => {
            //
            // Something like  a___
            //

            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderParselet_parseInfixContextSensitive(session, &under3Parselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternBlank(
                session,
                &under3Parselet,
                Tok, /*ignored*/
            );
        },
        TOKEN_UNDERDOT => {
            //
            // Something like  a_.
            //

            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderDotParselet_parseInfixContextSensitive(session, &underDotParselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternOptionalDefault(
                session,
                &underDotParselet,
                Tok, /*ignored*/
            );
        },
        _ => (),
    } // switch

    //
    // Something like  a
    //

    // MUSTTAIL
    return Parser_parseClimb(session, TokIn /*ignored*/);
}

pub(crate) fn SymbolParselet_parseInfixContextSensitive(
    session: &mut ParserSession,
    _: ParseletPtr,
    TokIn: Token,
) {
    //
    // Something like  _b
    //                  ^
    //

    panic_if_aborted!();


    //
    // We know we are already in the middle of parsing _
    //
    // Just push this symbol
    //

    Parser_pushLeafAndNext(session, TokIn);

    // no call needed here
    return;
}

fn SymbolParselet_reducePatternBlank(session: &mut ParserSession, P: ParseletPtr, ignored: Token) {
    let P = P
        .as_any()
        .downcast_ref::<UnderParselet>()
        .expect("unable to downcast to UnderParselet");

    let PBOp = P.PBOp;

    let node = CompoundNode::new(PBOp, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored);
}

fn SymbolParselet_reducePatternOptionalDefault(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = CompoundNode::new(
        SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT,
        Parser_popContext(session),
    );
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// PrefixOperatorParselet
//======================================

impl PrefixOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: Symbol) -> Self {
        PrefixOperatorParselet { precedence, Op }
    }

    fn getPrecedence(&self) -> Precedence {
        return self.precedence;
    }

    fn getOp(&self) -> Symbol {
        self.Op
    }
}

impl PrefixParselet for PrefixOperatorParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PrefixOperatorParselet_parsePrefix
    }
}

fn PrefixOperatorParselet_parsePrefix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    let P = P
        .as_any()
        .downcast_ref::<PrefixOperatorParselet>()
        .expect("unable to downcast to PrefixOperatorParselet");

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let ref mut Ctxt = Parser_pushContext_transparent(
        &mut session.NodeStack,
        &mut session.ContextStack,
        P.getPrecedence(),
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia_transparent(
        &mut session.NodeStack,
        &mut session.tokenizer,
        &mut Tok,
        TOPLEVEL,
    );

    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(PrefixOperatorParselet_reducePrefixOperator);
    Ctxt.p = Some(P);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn PrefixOperatorParselet_reducePrefixOperator(
    session: &mut ParserSession,
    P: ParseletPtr,
    ignored: Token,
) {
    let P = P
        .as_any()
        .downcast_ref::<PrefixOperatorParselet>()
        .expect("unable to downcast to PrefixOperatorParselet");

    let Op = P.getOp();

    let node = PrefixNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored);
}

//======================================
// InfixImplicitTimesParselet
//======================================

impl InfixParselet for InfixImplicitTimesParselet {
    fn parseInfix(&self) -> ParseFunction {
        assert!(false);

        return InfixImplicitTimesParselet_parseInfix;
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        panic!("The last token may not have been added to InfixParselets");
    }


    fn processImplicitTimes(&self, _session: &mut ParserSession, TokIn: Token) -> Token {
        return Token::new2(
            TOKEN_FAKE_IMPLICITTIMES,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    }
}

fn InfixImplicitTimesParselet_parseInfix(_session: &mut ParserSession, _: ParseletPtr, _: Token) {
    assert!(false);

    return;
}

//======================================
// PrefixAssertFalseParselet
//======================================

impl PrefixParselet for PrefixAssertFalseParselet {
    // fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
    //     PRECEDENCE_LOWEST
    // }

    fn parsePrefix(&self) -> ParseFunction {
        assert!(false);

        return PrefixAssertFalseParselet_parseInfix;
    }
}

fn PrefixAssertFalseParselet_parseInfix(
    _session: &mut ParserSession,
    _: ParseletPtr,
    _firstTok: Token,
) {
    assert!(false);

    return;
}


//======================================
// InfixAssertFalseParselet
//======================================

impl InfixParselet for InfixAssertFalseParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_LOWEST
    }

    fn parseInfix(&self) -> ParseFunction {
        assert!(false);

        return InfixAssertFalseParselet_parseInfix;
    }
}

fn InfixAssertFalseParselet_parseInfix(
    _session: &mut ParserSession,
    _: ParseletPtr,
    _firstTok: Token,
) {
    assert!(false);

    return;
}

//======================================
// BinaryOperatorParselet
//======================================

impl BinaryOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: Symbol) -> Self {
        BinaryOperatorParselet { precedence, Op }
    }
}

impl InfixParselet for BinaryOperatorParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        self.precedence
    }

    fn getOp(&self) -> Symbol {
        self.Op
    }

    fn parseInfix(&self) -> ParseFunction {
        return BinaryOperatorParselet_parseInfix;
    }
}


fn BinaryOperatorParselet_parseInfix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(BinaryOperatorParselet_reduceBinaryOperator);
    Ctxt.p = Some(P);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn BinaryOperatorParselet_reduceBinaryOperator(
    session: &mut ParserSession,
    P: ParseletPtr,
    ignored: Token,
) {
    let P = P
        .as_any()
        .downcast_ref::<BinaryOperatorParselet>()
        .expect("unable to downcast to BinaryOperatorParselet");

    let Op = P.getOp();

    let node = BinaryNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored);
}

//======================================
// InfixOperatorParselet
//======================================

impl InfixOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: Symbol) -> Self {
        Self { precedence, Op }
    }
}

impl InfixParselet for InfixOperatorParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        self.precedence
    }

    fn getOp(&self) -> Symbol {
        self.Op
    }

    fn parseInfix(&self) -> ParseFunction {
        return InfixOperatorParselet_parseInfix;
    }
}

fn InfixOperatorParselet_parseInfix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

    // #if !USE_MUSTTAIL
    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(Parser_identity);

    let P2 = prefix_parselet(Tok2.tok);

    (P2.parsePrefix())(session, P2, Tok2);

    return InfixOperatorParselet_parseLoop(session, P, TokIn /*ignored*/);
    // #else
    //     let ref mut Ctxt = Parser_topContext(session);
    //     assert!(Ctxt.f.is_none());
    //     assert!(Ctxt.p.is_none());
    //     Ctxt.f = Some(InfixOperatorParselet_parseLoop);
    //     Ctxt.p = P;

    //     let P2 = prefixParselets[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn InfixOperatorParselet_parseLoop(session: &mut ParserSession, P: ParseletPtr, ignored: Token) {
    let P = P
        .as_any()
        .downcast_ref::<InfixOperatorParselet>()
        .expect("unable to downcast to InfixOperatorParselet");

    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        let I = infixParselets[usize::from(Tok1.tok.value())];

        let Op = P.getOp();

        //
        // Cannot just compare tokens
        //
        // May be something like  a && b \[And] c
        //
        // and && and \[And] are different parselets
        //
        // and we want only a single Infix node created
        //
        // FIXME: only create a single parselet for all of the same operators, e.g., && and \[And]
        //
        // then just compare parselets directly here
        //
        if I.getOp() != Op {
            //
            // Tok.tok != TokIn.tok, so break
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return InfixOperatorParselet_reduceInfixOperator(session, P, ignored);
        }

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        Parser_pushLeafAndNext(session, Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        let P2 = prefix_parselet(Tok2.tok);

        (P2.parsePrefix())(session, P2, Tok2);
    } // loop
      // #else
      //     let ref mut Ctxt = Parser_topContext(session);
      //     assert!(Ctxt.f == InfixOperatorParselet_parseLoop);
      //     assert!(Ctxt.p == P);

    //     let P2 = prefixParselets[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn InfixOperatorParselet_reduceInfixOperator(
    session: &mut ParserSession,
    P: ParseletPtr,
    ignored: Token,
) {
    let P = P
        .as_any()
        .downcast_ref::<InfixOperatorParselet>()
        .expect("unable to downcast to InfixOperatorParselet");

    let Op = P.getOp();

    let node = InfixNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored);
}

//======================================
// PostfixOperatorParselet
//======================================

impl PostfixOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: Symbol) -> Self {
        PostfixOperatorParselet { precedence, Op }
    }
}

impl InfixParselet for PostfixOperatorParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        self.precedence
    }

    fn getOp(&self) -> Symbol {
        self.Op
    }

    fn parseInfix(&self) -> ParseFunction {
        return PostfixOperatorParselet_parseInfix;
    }
}


fn PostfixOperatorParselet_parseInfix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    Parser_pushLeafAndNext(session, TokIn);

    // MUSTTAIL
    return PostfixOperatorParselet_reducePostfixOperator(session, P, TokIn /*ignored*/);
}

fn PostfixOperatorParselet_reducePostfixOperator(
    session: &mut ParserSession,
    P: ParseletPtr,
    ignored: Token,
) {
    let P = P
        .as_any()
        .downcast_ref::<PostfixOperatorParselet>()
        .expect("unable to downcast to PostfixOperatorParselet");

    let Op = P.getOp();

    let node = PostfixNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored);
}

//======================================
// GroupParselet
//======================================

impl GroupParselet {
    pub(crate) const fn new(Opener: TokenEnum, Op: Symbol) -> Self {
        Self {
            Op,
            closer: GroupOpenerToCloser(Opener),
        }
    }

    fn getOp(&self) -> Symbol {
        self.Op
    }

    fn getCloser(&self) -> Closer {
        return self.closer;
    }
}

impl PrefixParselet for GroupParselet {
    fn parsePrefix(&self) -> ParseFunction {
        GroupParselet_parsePrefix
    }
}

fn GroupParselet_parsePrefix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    Parser_pushGroup(session, GroupOpenerToCloser(TokIn.tok));

    let ref mut Ctxt = Parser_pushContext(session, PRECEDENCE_LOWEST);

    // #if !USE_MUSTTAIL
    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(Parser_identity);

    return GroupParselet_parseLoop(session, P, TokIn /*ignored*/);
    // #else
    //     assert!(Ctxt.f.is_none());
    //     assert!(Ctxt.p.is_none());
    //     Ctxt.f = Some(GroupParselet_parseLoop);
    //     Ctxt.p = P;

    //     // MUSTTAIL
    //     return GroupParselet_parseLoop(session, P, TokIn/*ignored*/);
    // #endif // !USE_MUSTTAIL
}

fn GroupParselet_parseLoop(session: &mut ParserSession, P: ParseletPtr, ignored: Token) {
    let P = P
        .as_any()
        .downcast_ref::<GroupParselet>()
        .expect("unable to downcast to GroupParselet");

    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        //
        // There will only be 1 "good" node (either a LeafNode or a CommaNode)
        // But there might be multiple error nodes
        //
        // ADDENDUM: Actually, there may be more than 1 "good" node
        // e.g. {1\\2}
        //

        let Closr = P.getCloser();

        let Trivia1 = session.trivia1.clone();

        let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia_2(session, &mut Tok, TOPLEVEL, &mut Trivia1.borrow_mut());

        if TokenToCloser(Tok.tok) == Closr {
            //
            // Everything is good
            //

            Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

            Parser_pushLeafAndNext(session, Tok);

            // MUSTTAIL
            return GroupParselet_reduceGroup(session, P, ignored);
        }

        if Tok.tok.isCloser() {
            //
            // some other closer
            //

            if Parser_checkGroup(session, TokenToCloser(Tok.tok)) {
                //
                // Something like  { ( }
                //                     ^
                //

                //
                // Do not consume the bad closer now
                //

                Trivia1.borrow_mut().reset(&mut session.tokenizer);

                // MUSTTAIl
                return GroupParselet_reduceMissingCloser(session, P, ignored);
            }

            //
            // Something like  { ) }
            //                   ^
            //

            Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

            // #if !USE_MUSTTAIL
            PrefixToplevelCloserParselet_parsePrefix(session, &prefixToplevelCloserParselet, Tok);

            continue;
            // #else
            //         // MUSTTAIL
            //         return PrefixToplevelCloserParselet_parsePrefix(session, prefixToplevelCloserParselet, Tok);
            // #endif
        }

        if Tok.tok == TOKEN_ENDOFFILE {
            //
            // Handle something like   { a EOF
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return GroupParselet_reduceUnterminatedGroup(session, P, ignored);
        }

        //
        // Handle the expression
        //

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        let P2 = prefix_parselet(Tok.tok);

        (P2.parsePrefix())(session, P2, Tok);
    } // loop
      // #else
      //     let ref mut Ctxt = Parser_topContext(session);
      //     assert!(Ctxt.f == GroupParselet_parseLoop);
      //     assert!(Ctxt.p == P);

    //     let P2 = prefix_parselet(Tok.tok);

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok);
    // #endif // !USE_MUSTTAIL
}

fn GroupParselet_reduceGroup(session: &mut ParserSession, P: ParseletPtr, ignored: Token) {
    let P = P
        .as_any()
        .downcast_ref::<GroupParselet>()
        .expect("unable to downcast to GroupParselet");

    let Op = P.getOp();

    let node = GroupNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    Parser_popGroup(session);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored);
}

fn GroupParselet_reduceMissingCloser(session: &mut ParserSession, P: ParseletPtr, ignored: Token) {
    let P = P
        .as_any()
        .downcast_ref::<GroupParselet>()
        .expect("unable to downcast to GroupParselet");

    let Op = P.getOp();

    let node = GroupMissingCloserNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    Parser_popGroup(session);

    // MUSTTAIL
    return Parser_tryContinue(session, ignored);
}

fn GroupParselet_reduceUnterminatedGroup(
    session: &mut ParserSession,
    P: ParseletPtr,
    ignored: Token,
) {
    let P = P
        .as_any()
        .downcast_ref::<GroupParselet>()
        .expect("unable to downcast to GroupParselet");

    let Op = P.getOp();

    let node = UnterminatedGroupNeedsReparseNode::new(Op, Parser_popContext(session));
    Parser_pushNode(session, node);

    Parser_popGroup(session);

    // MUSTTAIL
    return Parser_tryContinue(session, ignored);
}

//======================================
// CallParselet
//======================================

impl CallParselet {
    pub(crate) const fn new(GP: PrefixParseletPtr) -> Self {
        Self { GP }
    }

    fn getGP(&self) -> PrefixParseletPtr {
        return self.GP;
    }
}

impl InfixParselet for CallParselet {
    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        PRECEDENCE_CALL
    }

    fn parseInfix(&self) -> ParseFunction {
        return CallParselet_parseInfix;
    }
}


fn CallParselet_parseInfix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    let P = P
        .as_any()
        .downcast_ref::<CallParselet>()
        .expect("unable to downcast to CallParselet");

    panic_if_aborted!();


    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(CallParselet_reduceCall);
    Ctxt.prec = PRECEDENCE_HIGHEST;

    let GP = P.getGP();

    // MUSTTAIL
    return (GP.parsePrefix())(session, GP, TokIn);
}

fn CallParselet_reduceCall(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    {
        let Body = Parser_popNode(session);

        let node = CallNode::new(Parser_popContext(session), Body);
        Parser_pushNode(session, node);
    }

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// TildeParselet
//======================================

impl InfixParselet for TildeParselet {
    fn parseInfix(&self) -> ParseFunction {
        return TildeParselet_parseInfix;
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        if Parser_checkTilde(session) {
            return PRECEDENCE_LOWEST;
        }

        return PRECEDENCE_TILDE;
    }
}

fn TildeParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // Something like  a ~f~ b
    //
    // It'd be weird if this were an "infix operator"
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut FirstTok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut FirstTok, TOPLEVEL);

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(TildeParselet_parse1);
    Ctxt.prec = PRECEDENCE_LOWEST;

    let P2 = prefix_parselet(FirstTok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, FirstTok);
}

fn TildeParselet_parse1(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

    if Tok1.tok != TOKEN_TILDE {
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //

        Trivia1.borrow_mut().reset(&mut session.tokenizer);

        // MUSTTAIL
        return TildeParselet_reduceError(session, ignored, ignored2);
    }

    Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

    Parser_pushLeafAndNext(session, Tok1);

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

    //
    // Reset back to "outside" precedence
    //

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.unwrap() as usize == TildeParselet_parse1 as usize);
    Ctxt.f = Some(TildeParselet_reduceTilde);
    Ctxt.prec = PRECEDENCE_TILDE;

    let P2 = prefix_parselet(Tok2.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok2);
}

fn TildeParselet_reduceTilde(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = TernaryNode::new(SYMBOL_CODEPARSER_TERNARYTILDE, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn TildeParselet_reduceError(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = SyntaxErrorNode::new(SYMBOL_SYNTAXERROR_EXPECTEDTILDE, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_tryContinue(session, ignored2);
}

//======================================
// ColonParselet
//======================================

impl InfixParselet for ColonParselet {
    fn parseInfix(&self) -> ParseFunction {
        return ColonParselet_parseInfix;
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        if Parser_checkPatternPrecedence(session) {
            return PRECEDENCE_FAKE_OPTIONALCOLON;
        }

        return PRECEDENCE_HIGHEST;
    }
}

fn ColonParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // Something like  symbol:object  or  pattern:optional
    //

    panic_if_aborted!();


    let colonLHS = Parser_checkColonLHS(session);

    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    match colonLHS {
        ColonLHS::Pattern => {
            let ref mut Ctxt = Parser_topContext(session);
            assert!(Ctxt.f.is_none());
            Ctxt.f = Some(ColonParselet_reducePattern);
            Ctxt.prec = PRECEDENCE_FAKE_PATTERNCOLON;

            let P2 = prefix_parselet(Tok.tok);

            // MUSTTAIl
            return (P2.parsePrefix())(session, P2, Tok);
        },
        ColonLHS::Optional => {
            let ref mut Ctxt = Parser_topContext(session);
            assert!(Ctxt.f.is_none());
            Ctxt.f = Some(ColonParselet_reduceOptional);
            Ctxt.prec = PRECEDENCE_FAKE_OPTIONALCOLON;

            let P2 = prefix_parselet(Tok.tok);

            // MUSTTAIl
            return (P2.parsePrefix())(session, P2, Tok);
        },
        ColonLHS::Error => {
            let ref mut Ctxt = Parser_topContext(session);
            assert!(Ctxt.f.is_none());
            Ctxt.f = Some(ColonParselet_reduceError);
            Ctxt.prec = PRECEDENCE_FAKE_PATTERNCOLON;

            let P2 = prefix_parselet(Tok.tok);

            // MUSTTAIl
            return (P2.parsePrefix())(session, P2, Tok);
        },
    }
}

fn ColonParselet_reducePattern(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = BinaryNode::new(SYMBOL_PATTERN, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn ColonParselet_reduceError(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = SyntaxErrorNode::new(
        SYMBOL_SYNTAXERROR_EXPECTEDSYMBOL,
        Parser_popContext(session),
    );
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn ColonParselet_reduceOptional(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = BinaryNode::new(SYMBOL_OPTIONAL, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// SlashColonParselet
//======================================

impl InfixParselet for SlashColonParselet {
    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        PRECEDENCE_SLASHCOLON
    }

    fn parseInfix(&self) -> ParseFunction {
        return SlashColonParselet_parseInfix;
    }
}


fn SlashColonParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // a /: b := c  is handled here
    //

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

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(SlashColonParselet_parse1);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn SlashColonParselet_parse1(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia_2(session, &mut Tok, TOPLEVEL, &mut Trivia1.borrow_mut());

    match Tok.tok {
        TOKEN_EQUAL => {
            Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

            Parser_setPrecedence(session, PRECEDENCE_EQUAL);

            // MUSTTAIl
            return EqualParselet_parseInfixTag(session, &equalParselet, Tok);
        },
        TOKEN_COLONEQUAL => {
            Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

            Parser_setPrecedence(session, PRECEDENCE_COLONEQUAL);

            // MUSTTAIl
            return ColonEqualParselet_parseInfixTag(session, &colonEqualParselet, Tok);
        },
        _ => (),
    } // switch

    Trivia1.borrow_mut().reset(&mut session.tokenizer);

    //
    // Anything other than:
    // a /: b = c
    // a /: b := c
    // a /: b =.
    //

    // MUSTTAIL
    return SlashColonParselet_reduceError(session, ignored, ignored2);
}

fn SlashColonParselet_reduceError(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = SyntaxErrorNode::new(SYMBOL_SYNTAXERROR_EXPECTEDSET, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// EqualParselet
//======================================

impl EqualParselet {
    pub(crate) const fn new() -> Self {
        Self {
            op: BinaryOperatorParselet::new(PRECEDENCE_EQUAL, SYMBOL_SET),
        }
    }
}

impl InfixParselet for EqualParselet {
    fn parseInfix(&self) -> ParseFunction {
        return EqualParselet_parseInfix;
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        self.op.getPrecedence(session)
    }
}

fn EqualParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    if Tok.tok == TOKEN_DOT {
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //

        Parser_pushLeafAndNext(session, Tok);

        // MUSTTAIL
        return EqualParselet_reduceUnset(session, ignored, TokIn /*ignored*/);
    }

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(EqualParselet_reduceSet);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn EqualParselet_parseInfixTag(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // a /: b = c  and  a /: b = .  are handled here
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    if Tok.tok == TOKEN_DOT {
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //

        Parser_pushLeafAndNext(session, Tok);

        // MUSTTAIL
        return EqualParselet_reduceTagUnset(session, ignored, TokIn /*ignored*/);
    }

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
    Ctxt.f = Some(EqualParselet_reduceTagSet);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn EqualParselet_reduceSet(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = BinaryNode::new(SYMBOL_SET, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn EqualParselet_reduceUnset(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = BinaryNode::new(SYMBOL_UNSET, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn EqualParselet_reduceTagSet(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = TernaryNode::new(SYMBOL_TAGSET, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn EqualParselet_reduceTagUnset(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = TernaryNode::new(SYMBOL_TAGUNSET, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// ColonEqualParselet
//======================================

impl ColonEqualParselet {
    pub(crate) const fn new() -> Self {
        ColonEqualParselet {
            op: BinaryOperatorParselet::new(PRECEDENCE_COLONEQUAL, SYMBOL_SETDELAYED),
        }
    }
}

impl InfixParselet for ColonEqualParselet {
    fn parseInfix(&self) -> ParseFunction {
        return ColonEqualParselet_parseInfix;
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        self.op.getPrecedence(session)
    }
}



fn ColonEqualParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(ColonEqualParselet_reduceSetDelayed);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn ColonEqualParselet_parseInfixTag(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok, TOPLEVEL);

    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
    Ctxt.f = Some(ColonEqualParselet_reduceTagSetDelayed);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return (P2.parsePrefix())(session, P2, Tok);
}

fn ColonEqualParselet_reduceSetDelayed(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = BinaryNode::new(SYMBOL_SETDELAYED, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

fn ColonEqualParselet_reduceTagSetDelayed(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = TernaryNode::new(SYMBOL_TAGSETDELAYED, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// CommaParselet
//======================================

impl InfixParselet for CommaParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_COMMA
    }

    fn parseInfix(&self) -> ParseFunction {
        return CommaParselet_parseInfix;
    }
}


fn CommaParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

    if Tok2.tok == TOKEN_COMMA || Tok2.tok == TOKEN_LONGNAME_INVISIBLECOMMA {
        //
        // Something like  a,,
        //

        Parser_pushLeaf(
            session,
            Token::new2(
                TOKEN_ERROR_INFIXIMPLICITNULL,
                Tok2.span,
                Source::from_location(Tok2.src.start),
            ),
        );

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(Parser_identity);

        return CommaParselet_parseLoop(session, ignored, TokIn /*ignored*/);
        // #else
        //         let ref mut Ctxt = Parser_topContext(session);
        //         assert!(Ctxt.f.is_none());
        //         Ctxt.f = Some(CommaParselet_parseLoop);

        //         // MUSTTAIL
        //         return CommaParselet_parseLoop(session, ignored, TokIn/*ignored*/);
        // #endif // !USE_MUSTTAIL
    }

    // #if !USE_MUSTTAIL
    let ref mut Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(Parser_identity);

    let P2 = prefix_parselet(Tok2.tok);

    (P2.parsePrefix())(session, P2, Tok2);

    return CommaParselet_parseLoop(session, ignored, TokIn /*ignored*/);
    // #else
    //     let ref mut Ctxt = Parser_topContext(session);
    //     assert!(Ctxt.f.is_none());
    //     Ctxt.f = Some(CommaParselet_parseLoop);

    //     let P2 = prefixParselets[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn CommaParselet_parseLoop(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        if !(Tok1.tok == TOKEN_COMMA || Tok1.tok == TOKEN_LONGNAME_INVISIBLECOMMA) {
            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return CommaParselet_reduceComma(session, ignored, ignored2);
        }

        //
        // Something like  a,b
        //

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        Parser_pushLeafAndNext(session, Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

        if Tok2.tok == TOKEN_COMMA || Tok2.tok == TOKEN_LONGNAME_INVISIBLECOMMA {
            //
            // Something like  a,,
            //

            Parser_pushLeaf(
                session,
                Token::new2(
                    TOKEN_ERROR_INFIXIMPLICITNULL,
                    Tok2.span,
                    Source::from_location(Tok2.src.start),
                ),
            );

            // #if !USE_MUSTTAIL
            continue;
            // #else
            //         // MUSTTAIL
            //         return CommaParselet_parseLoop(session, ignored, ignored2);
            // #endif // !USE_MUSTTAIL
        }

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        let P2 = prefix_parselet(Tok2.tok);

        (P2.parsePrefix())(session, P2, Tok2);
    } // loop
      // #else
      //     let ref mut Ctxt = Parser_topContext(session);
      //     assert!(Ctxt.f == CommaParselet_parseLoop);

    //     let P2 = prefixParselets[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn CommaParselet_reduceComma(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = InfixNode::new(SYMBOL_CODEPARSER_COMMA, Parser_popContext(session));
    Parser_pushNode(session, node);

    //
    // was:
    //
    //    MUSTTAIL
    //    return Parser_parseClimb(ignored, ignored2);

    //
    // but take advantage of fact that Comma has lowest operator precedence and there is nothing after a,b,c that will continue that expression
    //
    // so call Parser_tryContinue directly
    //

    // MUSTTAIL
    return Parser_tryContinue(session, ignored2);
}

//======================================
// SemiParselet
//======================================

impl InfixParselet for SemiParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_SEMI
    }

    fn parseInfix(&self) -> ParseFunction {
        return SemiParselet_parseInfix;
    }
}


fn SemiParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // CompoundExpression should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, &mut Tok2, TOPLEVEL);

    if Tok2.tok == TOKEN_SEMI {
        //
        // Something like  a; ;
        //

        Parser_pushLeaf(
            session,
            Token::new2(
                TOKEN_FAKE_IMPLICITNULL,
                Tok2.span,
                Source::from_location(Tok2.src.start),
            ),
        );

        //
        // nextToken() is not needed after an implicit token
        //

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(Parser_identity);

        return SemiParselet_parseLoop(session, ignored, TokIn /*ignored*/);
        // #else
        //         let ref mut Ctxt = Parser_topContext(session);
        //         assert!(Ctxt.f.is_none());
        //         Ctxt.f = Some(SemiParselet_parseLoop);

        //         // MUSTTAIL
        //         return SemiParselet_parseLoop(session, ignored, TokIn/*ignored*/);
        // #endif // !USE_MUSTTAIL
    }

    if Tok2.tok.isPossibleBeginning() {
        //
        // Something like  a;+2
        //

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(Parser_identity);

        let P2 = prefix_parselet(Tok2.tok);

        (P2.parsePrefix())(session, P2, Tok2);

        return SemiParselet_parseLoop(session, ignored, TokIn /*ignored*/);
        // #else
        //         let ref mut Ctxt = Parser_topContext(session);
        //         assert!(Ctxt.f.is_none());
        //         Ctxt.f = Some(SemiParselet_parseLoop);

        //         let P2 = prefixParselets[Tok2.tok.value()];

        //         // MUSTTAIL
        //         return (P2.parsePrefix())(session, P2, Tok2);
        // #endif // !USE_MUSTTAIL
    }

    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //

    Parser_pushLeaf(
        session,
        Token::new2(
            TOKEN_FAKE_IMPLICITNULL,
            Tok2.span,
            Source::from_location(Tok2.src.start),
        ),
    );

    //
    // nextToken() is not needed after an implicit token
    //

    // MUSTTAIL
    return SemiParselet_reduceCompoundExpression(session, ignored, TokIn /*ignored*/);
}

fn SemiParselet_parseLoop(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        if Tok1.tok != TOKEN_SEMI {
            //
            // Something like  a;b
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiParselet_reduceCompoundExpression(session, ignored, ignored2);
        }

        //
        // Something like  a;b
        //

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        Parser_pushLeafAndNext(session, Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        //
        // CompoundExpression should not cross toplevel newlines
        //
        Parser_eatTriviaButNotToplevelNewlines(session, &mut Tok2, TOPLEVEL);

        if Tok2.tok == TOKEN_SEMI {
            //
            // Something like  a;b; ;
            //

            Parser_pushLeaf(
                session,
                Token::new2(
                    TOKEN_FAKE_IMPLICITNULL,
                    Tok2.span,
                    Source::from_location(Tok2.src.start),
                ),
            );

            //
            // nextToken() is not needed after an implicit token
            //

            // #if !USE_MUSTTAIL
            continue;
            // #else
            //         // MUSTTAIL
            //         return SemiParselet_parseLoop(session, ignored, ignored2);
            // #endif // !USE_MUSTTAIL
        }

        if Tok2.tok.isPossibleBeginning() {
            //
            // Something like  a;b;+2
            //

            // #if !USE_MUSTTAIL
            let ref mut Ctxt = Parser_topContext(session);
            assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

            let P2 = prefix_parselet(Tok2.tok);

            (P2.parsePrefix())(session, P2, Tok2);

            continue;
            // #else
            //         let ref mut Ctxt = Parser_topContext(session);
            //         assert!(Ctxt.f == SemiParselet_parseLoop);

            //         let P2 = prefixParselets[Tok2.tok.value()];

            //         // MUSTTAIL
            //         return (P2.parsePrefix())(session, P2, Tok2);
            // #endif // !USE_MUSTTAIL
        }

        //
        // Not beginning of an expression
        //
        // For example:  a;b;&
        //

        Parser_pushLeaf(
            session,
            Token::new2(
                TOKEN_FAKE_IMPLICITNULL,
                Tok2.span,
                Source::from_location(Tok2.src.start),
            ),
        );

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiParselet_reduceCompoundExpression(session, ignored, ignored2);

        // #if !USE_MUSTTAIL
    } // loop
      // #endif // !USE_MUSTTAIL
}

fn SemiParselet_reduceCompoundExpression(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = InfixNode::new(SYMBOL_COMPOUNDEXPRESSION, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// ColonColonParselet
//======================================

impl InfixParselet for ColonColonParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_COLONCOLON
    }

    fn parseInfix(&self) -> ParseFunction {
        return ColonColonParselet_parseInfix;
    }
}


fn ColonColonParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // a::b
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    //
    // Special tokenization, so must do parsing here
    //

    let Tok2 = Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

    Parser_pushLeafAndNext(session, Tok2);

    // MUSTTAIL
    return ColonColonParselet_parseLoop(session, ignored, TokIn /*ignored*/);
}

fn ColonColonParselet_parseLoop(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        if Tok1.tok != TOKEN_COLONCOLON {
            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return ColonColonParselet_reduceMessageName(session, ignored, ignored2);
        }

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        Parser_pushLeafAndNext(session, Tok1);

        //
        // Special tokenization, so must do parsing here
        //

        let Tok2 = Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

        Parser_pushLeafAndNext(session, Tok2);

        // #if !USE_MUSTTAIL
    } // loop
      // #else
      //     // MUSTTAIL
      //     return ColonColonParselet_parseLoop(session, ignored, ignored2);
      // #endif // !USE_MUSTTAIL
}

fn ColonColonParselet_reduceMessageName(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = InfixNode::new(SYMBOL_MESSAGENAME, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// GreaterGreaterParselet
//======================================

impl InfixParselet for GreaterGreaterParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_GREATERGREATER
    }

    fn parseInfix(&self) -> ParseFunction {
        return GreaterGreaterParselet_parseInfix;
    }
}

fn GreaterGreaterParselet_parseInfix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    //
    // a>>b
    //

    panic_if_aborted!();


    //
    // Special tokenization, so must do parsing here
    //

    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);

    Parser_eatTrivia_stringifyAsFile(session, &mut Tok);

    Parser_pushLeafAndNext(session, Tok);

    // MUSTTAIL
    return GreaterGreaterParselet_reducePut(session, ignored, TokIn /*ignored*/);
}

fn GreaterGreaterParselet_reducePut(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = BinaryNode::new(SYMBOL_PUT, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// GreaterGreaterGreaterParselet
//======================================

impl InfixParselet for GreaterGreaterGreaterParselet {
    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_GREATERGREATERGREATER
    }

    fn parseInfix(&self) -> ParseFunction {
        return GreaterGreaterGreaterParselet_parseInfix;
    }
}

fn GreaterGreaterGreaterParselet_parseInfix(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    TokIn: Token,
) {
    //
    // a>>>b
    //

    panic_if_aborted!();


    //
    // Special tokenization, so must do parsing here
    //

    Parser_pushLeafAndNext(session, TokIn);

    let mut Tok = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);

    Parser_eatTrivia_stringifyAsFile(session, &mut Tok);

    Parser_pushLeafAndNext(session, Tok);

    // MUSTTAIL
    return GreaterGreaterGreaterParselet_reducePutAppend(session, ignored, TokIn /*ignored*/);
}

fn GreaterGreaterGreaterParselet_reducePutAppend(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = BinaryNode::new(SYMBOL_PUTAPPEND, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// LessLessParselet
//======================================

impl PrefixParselet for LessLessParselet {
    fn parsePrefix(&self) -> ParseFunction {
        LessLessParselet_parsePrefix
    }
}

fn LessLessParselet_parsePrefix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // <<a
    //

    panic_if_aborted!();


    //
    // Special tokenization, so must do parsing here
    //

    Parser_pushLeafAndNext(session, TokIn);

    Parser_pushContext(session, PRECEDENCE_HIGHEST);

    let mut Tok = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);

    Parser_eatTrivia_stringifyAsFile(session, &mut Tok);

    Parser_pushLeafAndNext(session, Tok);

    // MUSTTAIL
    return LessLessParselet_reduceGet(session, ignored, TokIn /*ignored*/);
}

fn LessLessParselet_reduceGet(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = PrefixNode::new(SYMBOL_GET, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// HashParselet
//======================================

impl PrefixParselet for HashParselet {
    fn parsePrefix(&self) -> ParseFunction {
        HashParselet_parsePrefix
    }
}

fn HashParselet_parsePrefix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
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

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, INSIDE_SLOT);

    match Tok.tok {
        TOKEN_INTEGER | TOKEN_STRING => {
            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            Parser_pushLeafAndNext(session, Tok);

            // MUSTTAIl
            return HashParselet_reduceSlot(session, ignored, TokIn /*ignored*/);
        },
        _ => (),
    }

    // MUSTTAIL
    return Parser_parseClimb(session, TokIn /*ignored*/);
}

fn HashParselet_reduceSlot(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = CompoundNode::new(SYMBOL_SLOT, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// HashHashParselet
//======================================

impl PrefixParselet for HashHashParselet {
    fn parsePrefix(&self) -> ParseFunction {
        HashHashParselet_parsePrefix
    }
}

fn HashHashParselet_parsePrefix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // Something like  ##  or  ##1
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, INSIDE_SLOTSEQUENCE);

    match Tok.tok {
        TOKEN_INTEGER => {
            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            Parser_pushLeafAndNext(session, Tok);

            // MUSTTAIl
            return HashHashParselet_reduceSlotSequence(session, ignored, TokIn /*ignored*/);
        },
        _ => (),
    }

    // MUSTTAIL
    return Parser_parseClimb(session, TokIn /*ignored*/);
}

fn HashHashParselet_reduceSlotSequence(
    session: &mut ParserSession,
    ignored: ParseletPtr,
    ignored2: Token,
) {
    let node = CompoundNode::new(SYMBOL_SLOTSEQUENCE, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}

//======================================
// PercentParselet
//======================================

impl PrefixParselet for PercentParselet {
    fn parsePrefix(&self) -> ParseFunction {
        PercentParselet_parsePrefix
    }
}

fn PercentParselet_parsePrefix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // Something like  %  or  %1
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, INSIDE_OUT);

    match Tok.tok {
        TOKEN_INTEGER => {
            Parser_pushContext(session, PRECEDENCE_HIGHEST);

            Parser_pushLeafAndNext(session, Tok);

            // MUSTTAIl
            return PercentParselet_reduceOut(session, ignored, TokIn /*ignored*/);
        },
        _ => (),
    }

    // MUSTTAIL
    return Parser_parseClimb(session, TokIn /*ignored*/);
}

fn PercentParselet_reduceOut(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = CompoundNode::new(SYMBOL_OUT, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored2);
}
