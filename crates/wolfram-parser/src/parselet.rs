mod integral_parselet;
mod semi_semi_parselet;
mod times_parselet;
mod under_parselet;


use std::any::Any;

use crate::{
    cst::{
        BinaryNode, CallBody, CallNode, CompoundNode, CompoundOperator, GroupMissingCloserNode,
        GroupNode, GroupOperator, InfixNode, InfixOperator, OperatorNode, PostfixNode,
        PrefixBinaryOperator, PrefixNode, SyntaxErrorKind, SyntaxErrorNode, TernaryNode,
        UnterminatedGroupNeedsReparseNode,
    },
    panic_if_aborted,
    parselet_registration::{INFIX_PARSELETS, PREFIX_PARSELETS, *},
    parser::{ColonLHS, Parser_identity},
    parser_session::ParserSession,
    precedence::{Precedence, *},
    source::*,
    token::{Token, TokenKind, TokenRef},
    token_enum::{Closer, GroupOpenerToCloser, TokenToCloser},
    tokenizer::{
        Tokenizer_currentToken, Tokenizer_currentToken_stringifyAsFile,
        Tokenizer_currentToken_stringifyAsTag,
    },
};

use self::under_parselet::{
    UnderDotParselet_parseInfixContextSensitive, UnderParselet_parseInfixContextSensitive,
};

pub(crate) type ParseletPtr = &'static dyn Parselet;
pub(crate) type PrefixParseletPtr = &'static dyn PrefixParselet;
pub(crate) type InfixParseletPtr = &'static dyn InfixParselet;

pub(crate) type ParseFunction = for<'i> fn(session: &mut ParserSession<'i>, parselet: ParseletPtr);

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
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>);
}


pub(crate) trait InfixParselet: Parselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>);

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence;

    fn getOp(&self) -> InfixParseletOperator {
        // TODO: Make this sentinel value unnecessary?
        return InfixParseletOperator::Infix(InfixOperator::CodeParser_InternalInvalid);
    }

    fn processImplicitTimes<'i>(
        &self,
        _session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        return tok_in;
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum InfixParseletOperator {
    Prefix(PrefixOperator),
    Infix(InfixOperator),
    Postfix(PostfixOperator),
    Binary(BinaryOperator),
}

impl From<InfixOperator> for InfixParseletOperator {
    fn from(op: InfixOperator) -> Self {
        Self::Infix(op)
    }
}

impl From<PrefixOperator> for InfixParseletOperator {
    fn from(op: PrefixOperator) -> Self {
        Self::Prefix(op)
    }
}

impl From<BinaryOperator> for InfixParseletOperator {
    fn from(op: BinaryOperator) -> Self {
        Self::Binary(op)
    }
}

impl InfixParseletOperator {
    fn unwrap_op(self) -> InfixOperator {
        match self {
            InfixParseletOperator::Infix(op) => op,
            InfixParseletOperator::Prefix(_)
            | InfixParseletOperator::Postfix(_)
            | InfixParseletOperator::Binary(_) => {
                panic!("expected Infix operator, got: {self:?}")
            },
        }
    }

    fn unwrap_postfix_op(self) -> PostfixOperator {
        match self {
            InfixParseletOperator::Prefix(_)
            | InfixParseletOperator::Infix(_)
            | InfixParseletOperator::Binary(_) => {
                panic!("expected Postfix operator, got: {self:?}")
            },
            InfixParseletOperator::Postfix(op) => op,
        }
    }
}

//--------------------------------------
// Access parselet for token
//--------------------------------------

/// Get the [`PrefixParselet`] implementation associated with this token.
pub(crate) fn prefix_parselet(tok: TokenKind) -> PrefixParseletPtr {
    let index = usize::from(tok.value());

    PREFIX_PARSELETS[index]
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
    Op: PrefixOperator,
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
    precedence: Precedence,
    Op: BinaryOperator,
}


#[derive(Debug)]
pub(crate) struct InfixOperatorParselet /* : InfixParselet */ {
    precedence: Precedence,
    Op: InfixOperator,
}


//
//
//
#[derive(Debug)]
pub(crate) struct TimesParselet /* : InfixParselet */ {}



#[derive(Debug)]
pub(crate) struct PostfixOperatorParselet /* : InfixParselet */ {
    precedence: Precedence,
    Op: PostfixOperator,
}


#[derive(Debug)]
pub(crate) struct GroupParselet /* : PrefixParselet */ {
    Op: GroupOperator,
    closer: Closer,
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
    pub(crate) Op1: PrefixBinaryOperator,
    pub(crate) Op2: PrefixOperator,
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
    /// Operator used if this underscore pattern does not bind a named pattern
    pub BOp: CompoundOperator,

    /// Operator used if this underscore pattern does bind a named pattern
    pub PBOp: CompoundOperator,
}


#[derive(Debug)]
pub(crate) struct UnderDotParselet /* : PrefixParselet */ {}



//======================================
// LeafParselet
//======================================

impl PrefixParselet for LeafParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        LeafParselet_reduceLeaf(session, token)
    }
}

fn LeafParselet_reduceLeaf<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    session.push_leaf_and_next(TokIn);

    // MUSTTAIL
    return session.parse_climb();
}

//======================================
// PrefixErrorParselet
//======================================

impl PrefixParselet for PrefixErrorParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixErrorParselet_parsePrefix(session, token)
    }
}

fn PrefixErrorParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    assert!(TokIn.tok.isError());

    session.push_leaf_and_next(TokIn);

    // MUSTTAIL
    return session.try_continue();
}

//======================================
// PrefixCloserParselet
//======================================

impl PrefixParselet for PrefixCloserParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixCloserParselet_parsePrefix(session, token)
    }
}

fn PrefixCloserParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    assert!(TokIn.tok.isCloser());

    panic_if_aborted!();


    //
    // Inside some other parselet that is not GroupParselet
    //

    let createdToken: TokenRef;

    if session.top_precedence() == PRECEDENCE_COMMA {
        createdToken = Token::error_at_start(TokenKind::Error_InfixImplicitNull, TokIn);
    } else {
        createdToken = Token::error_at_start(TokenKind::Error_ExpectedOperand, TokIn);
    }

    session.push_leaf(createdToken);

    //
    // Do not take the closer.
    // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
    //
    Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    // MUSTTAIL
    return session.try_continue();
}

//======================================
// PrefixToplevelCloserParselet
//======================================

impl PrefixParselet for PrefixToplevelCloserParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixToplevelCloserParselet_parsePrefix(session, token)
    }
}

pub(crate) fn PrefixToplevelCloserParselet_parsePrefix<'i>(
    session: &mut ParserSession<'i>,
    TokIn: TokenRef<'i>,
) {
    assert!(TokIn.tok.isCloser());

    panic_if_aborted!();


    //
    // if we are at the top, then make sure to take the token and report it
    //

    session.push_leaf(Token::error_at(TokenKind::Error_UnexpectedCloser, TokIn));

    TokIn.skip(&mut session.tokenizer);

    // MUSTTAIL
    return session.try_continue();
}

//======================================
// PrefixEndOfFileParselet
//======================================

impl PrefixParselet for PrefixEndOfFileParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixEndOfFileParselet_parsePrefix(session, token)
    }
}

fn PrefixEndOfFileParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // Something like  a+<EOF>
    //

    panic_if_aborted!();


    let createdToken: TokenRef;

    if session.top_precedence() == PRECEDENCE_COMMA {
        createdToken = Token::error_at_start(TokenKind::Error_InfixImplicitNull, TokIn);
    } else {
        createdToken = Token::error_at_start(TokenKind::Error_ExpectedOperand, TokIn);
    }

    session.push_leaf(createdToken);

    // MUSTTAIL
    return session.try_continue();
}

//======================================
// PrefixUnsupportedParselet
//======================================

impl PrefixParselet for PrefixUnsupportedTokenParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixUnsupportedTokenParselet_parsePrefix(session, token)
    }
}

fn PrefixUnsupportedTokenParselet_parsePrefix<'i>(
    session: &mut ParserSession<'i>,
    TokIn: TokenRef<'i>,
) {
    panic_if_aborted!();


    session.push_leaf(Token::error_at(TokenKind::Error_UnsupportedToken, TokIn));

    TokIn.skip(&mut session.tokenizer);

    // MUSTTAIL
    return session.try_continue();
}

//======================================
// PrefixCommaParselet
//======================================

impl PrefixParselet for PrefixCommaParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixCommaParselet_parsePrefix(session, token)
    }
}

fn PrefixCommaParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // if the input is  f[a@,2]  then we want to return TokenKind::ERROR_EXPECTEDOPERAND
    //
    // if the input is  f[,2]  then we want to return TokenKind::ERROR_PREFIXIMPLICITNULL
    //

    panic_if_aborted!();


    let createdToken: TokenRef;

    if session.top_precedence() == PRECEDENCE_LOWEST {
        createdToken = Token::error_at_start(TokenKind::Error_PrefixImplicitNull, TokIn);
    } else {
        createdToken = Token::error_at_start(TokenKind::Error_ExpectedOperand, TokIn);
    }

    session.push_leaf(createdToken);

    // MUSTTAIL
    return session.parse_climb();
}

//======================================
// PrefixUnhandledParselet
//======================================

impl PrefixParselet for PrefixUnhandledParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixUnhandledParselet_parsePrefix(session, token)
    }
}

fn PrefixUnhandledParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    assert!(!TokIn.tok.isPossibleBeginning(), "handle at call site");

    panic_if_aborted!();


    session.push_leaf(Token::error_at_start(
        TokenKind::Error_ExpectedOperand,
        TokIn,
    ));

    //
    // Do not take next token
    //
    Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    let I = INFIX_PARSELETS[usize::from(TokIn.tok.value())];

    let TokenPrecedence = I.getPrecedence(session);

    //
    // if (Ctxt.prec > TokenPrecedence)
    //   goto prefixUnhandledParseletRet;
    // else if (Ctxt.prec == TokenPrecedence && Ctxt.prec.Associativity is NonRight)
    //   goto prefixUnhandledParseletRet;
    //
    if (session.top_precedence() | 0x1) > TokenPrecedence {
        //
        // Something like  a + | 2
        //
        // Make sure that the error leaf is with the + and not the |
        //

        // MUSTTAIL
        return session.try_continue();
    }

    //
    // Handle something like  f[@2]
    //
    // We want to make EXPECTEDOPERAND the first arg of the Operator node.
    //

    session.push_context(TokenPrecedence);

    let P2 = INFIX_PARSELETS[usize::from(TokIn.tok.value())];

    // MUSTTAIL
    return P2.parse_infix(session, TokIn);
}

//======================================
// InfixToplevelNewlineParselet
//======================================

impl InfixParselet for InfixToplevelNewlineParselet {
    fn parse_infix<'i>(&'static self, _session: &mut ParserSession<'i>, _token: TokenRef<'i>) {
        assert!(false);
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        //
        // Do not do Implicit Times across top-level newlines
        //
        return PRECEDENCE_LOWEST;
    }
}


//======================================
// SymbolParselet
//======================================

impl PrefixParselet for SymbolParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        SymbolParselet_parsePrefix(session, token)
    }
}

fn SymbolParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // Something like  x  or x_
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // if we are here, then we know that Sym could bind to _
    //

    match Tok.tok {
        TokenKind::Under => {
            //
            // Something like  a_
            //

            session.push_context(PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderParselet_parseInfixContextSensitive(session, &under1Parselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternBlank(session, &under1Parselet);
        },
        TokenKind::UnderUnder => {
            //
            // Something like  a__
            //

            session.push_context(PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderParselet_parseInfixContextSensitive(session, &under2Parselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternBlank(session, &under2Parselet);
        },
        TokenKind::UnderUnderUnder => {
            //
            // Something like  a___
            //

            session.push_context(PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderParselet_parseInfixContextSensitive(session, &under3Parselet, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternBlank(session, &under3Parselet);
        },
        TokenKind::UnderDot => {
            //
            // Something like  a_.
            //

            session.push_context(PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            UnderDotParselet_parseInfixContextSensitive(session, Tok);

            // MUSTTAIl
            return SymbolParselet_reducePatternOptionalDefault(session);
        },
        _ => (),
    } // switch

    //
    // Something like  a
    //

    // MUSTTAIL
    return session.parse_climb();
}

pub(crate) fn SymbolParselet_parseInfixContextSensitive<'i>(
    session: &mut ParserSession<'i>,
    TokIn: TokenRef<'i>,
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

    session.push_leaf_and_next(TokIn);

    // no call needed here
    return;
}

fn SymbolParselet_reducePatternBlank(session: &mut ParserSession, P: &UnderParselet) {
    session.reduce_and_climb(|ctx| CompoundNode::new(P.PBOp, ctx))
}

fn SymbolParselet_reducePatternOptionalDefault(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| {
        CompoundNode::new(CompoundOperator::CodeParser_PatternOptionalDefault, ctx)
    })
}

//======================================
// PrefixOperatorParselet
//======================================

impl PrefixOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: PrefixOperator) -> Self {
        PrefixOperatorParselet { precedence, Op }
    }

    fn getPrecedence(&self) -> Precedence {
        return self.precedence;
    }

    fn getOp(&self) -> PrefixOperator {
        self.Op
    }
}

impl PrefixParselet for PrefixOperatorParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PrefixOperatorParselet_parsePrefix(session, self, token)
    }
}

fn PrefixOperatorParselet_parsePrefix<'i>(
    session: &mut ParserSession<'i>,
    P: &'static PrefixOperatorParselet,
    TokIn: TokenRef<'i>,
) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let ref mut Ctxt = ParserSession::push_context_transparent(
        &mut session.NodeStack,
        &mut session.ContextStack,
        P.getPrecedence(),
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    ParserSession::eat_trivia_transparent(
        &mut session.NodeStack,
        &mut session.tokenizer,
        &mut Tok,
        TOPLEVEL,
    );

    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(PrefixOperatorParselet_reducePrefixOperator);
    Ctxt.p = Some(P);

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn PrefixOperatorParselet_reducePrefixOperator(session: &mut ParserSession, P: ParseletPtr) {
    let P = P
        .as_any()
        .downcast_ref::<PrefixOperatorParselet>()
        .expect("unable to downcast to PrefixOperatorParselet");

    session.reduce_and_climb(|ctx| PrefixNode::new(P.getOp(), ctx))
}

//======================================
// InfixImplicitTimesParselet
//======================================

impl InfixParselet for InfixImplicitTimesParselet {
    fn parse_infix<'i>(&'static self, _session: &mut ParserSession<'i>, _token: TokenRef<'i>) {
        assert!(false);
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        panic!("The last token may not have been added to InfixParselets");
    }


    fn processImplicitTimes<'i>(
        &self,
        _session: &mut ParserSession<'i>,
        TokIn: TokenRef<'i>,
    ) -> TokenRef<'i> {
        return Token::error_at_start(TokenKind::Fake_ImplicitTimes, TokIn);
    }
}

//======================================
// PrefixAssertFalseParselet
//======================================

impl PrefixParselet for PrefixAssertFalseParselet {
    // fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
    //     PRECEDENCE_LOWEST
    // }

    fn parse_prefix<'i>(&'static self, _session: &mut ParserSession<'i>, _token: TokenRef<'i>) {
        assert!(false);
    }
}


//======================================
// InfixAssertFalseParselet
//======================================

impl InfixParselet for InfixAssertFalseParselet {
    fn parse_infix<'i>(&'static self, _session: &mut ParserSession<'i>, _token: TokenRef<'i>) {
        assert!(false)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_LOWEST
    }
}

//======================================
// BinaryOperatorParselet
//======================================

impl BinaryOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: BinaryOperator) -> Self {
        BinaryOperatorParselet { precedence, Op }
    }
}

impl InfixParselet for BinaryOperatorParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        BinaryOperatorParselet_parseInfix(session, self, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        self.precedence
    }

    fn getOp(&self) -> InfixParseletOperator {
        self.Op.into()
    }
}


fn BinaryOperatorParselet_parseInfix<'i>(
    session: &mut ParserSession<'i>,
    P: &'static BinaryOperatorParselet,
    TokIn: TokenRef<'i>,
) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(BinaryOperatorParselet_reduceBinaryOperator);
    Ctxt.p = Some(P);

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn BinaryOperatorParselet_reduceBinaryOperator(session: &mut ParserSession, P: ParseletPtr) {
    let P = P
        .as_any()
        .downcast_ref::<BinaryOperatorParselet>()
        .expect("unable to downcast to BinaryOperatorParselet");

    session.reduce_and_climb(|ctx| BinaryNode::new(P.Op, ctx))
}

//======================================
// InfixOperatorParselet
//======================================

impl InfixOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: InfixOperator) -> Self {
        Self { precedence, Op }
    }
}

impl InfixParselet for InfixOperatorParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        return InfixOperatorParselet_parseInfix(session, self, token);
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        self.precedence
    }

    fn getOp(&self) -> InfixParseletOperator {
        self.Op.into()
    }
}

fn InfixOperatorParselet_parseInfix<'i>(
    session: &mut ParserSession<'i>,
    P: &InfixOperatorParselet,
    TokIn: TokenRef<'i>,
) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok2, TOPLEVEL);

    // #if !USE_MUSTTAIL
    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(Parser_identity);

    session.parse_prefix(Tok2);

    return InfixOperatorParselet_parseLoop(session, P);
    // #else
    //     let ref mut Ctxt = session.top_context();
    //     assert!(Ctxt.f.is_none());
    //     assert!(Ctxt.p.is_none());
    //     Ctxt.f = Some(InfixOperatorParselet_parseLoop);
    //     Ctxt.p = P;

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn InfixOperatorParselet_parseLoop(session: &mut ParserSession, P: &InfixOperatorParselet) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia_2(&mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        let I = INFIX_PARSELETS[usize::from(Tok1.tok.value())];

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
            return InfixOperatorParselet_reduceInfixOperator(session, P);
        }

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        session.push_leaf_and_next(Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia(&mut Tok2, TOPLEVEL);

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = session.top_context();
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        session.parse_prefix(Tok2);
    } // loop
      // #else
      //     let ref mut Ctxt = session.top_context();
      //     assert!(Ctxt.f == InfixOperatorParselet_parseLoop);
      //     assert!(Ctxt.p == P);

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn InfixOperatorParselet_reduceInfixOperator(
    session: &mut ParserSession,
    P: &InfixOperatorParselet,
) {
    let Op = P.getOp().unwrap_op();

    session.reduce_and_climb(|ctx| InfixNode::new(Op, ctx))
}

//======================================
// PostfixOperatorParselet
//======================================

impl PostfixOperatorParselet {
    pub(crate) const fn new(precedence: Precedence, Op: PostfixOperator) -> Self {
        PostfixOperatorParselet { precedence, Op }
    }
}

impl InfixParselet for PostfixOperatorParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PostfixOperatorParselet_parseInfix(session, self, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        self.precedence
    }

    fn getOp(&self) -> InfixParseletOperator {
        InfixParseletOperator::Postfix(self.Op)
    }
}


fn PostfixOperatorParselet_parseInfix<'i>(
    session: &mut ParserSession<'i>,
    P: &PostfixOperatorParselet,
    TokIn: TokenRef<'i>,
) {
    session.push_leaf_and_next(TokIn);

    // MUSTTAIL
    return PostfixOperatorParselet_reducePostfixOperator(session, P);
}

fn PostfixOperatorParselet_reducePostfixOperator(
    session: &mut ParserSession,
    P: &PostfixOperatorParselet,
) {
    let Op = P.getOp().unwrap_postfix_op();

    session.reduce_and_climb(|ctx| PostfixNode::new(Op, ctx))
}

//======================================
// GroupParselet
//======================================

impl GroupParselet {
    pub(crate) const fn new(Opener: TokenKind, Op: GroupOperator) -> Self {
        Self {
            Op,
            closer: GroupOpenerToCloser(Opener),
        }
    }

    fn getOp(&self) -> GroupOperator {
        self.Op
    }

    fn getCloser(&self) -> Closer {
        return self.closer;
    }
}

impl PrefixParselet for GroupParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        GroupParselet_parsePrefix(session, self, token)
    }
}

fn GroupParselet_parsePrefix<'i>(
    session: &mut ParserSession<'i>,
    P: &GroupParselet,
    TokIn: TokenRef<'i>,
) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    session.push_group(GroupOpenerToCloser(TokIn.tok));

    let ref mut Ctxt = session.push_context(PRECEDENCE_LOWEST);

    // #if !USE_MUSTTAIL
    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(Parser_identity);

    return GroupParselet_parseLoop(session, P);
    // #else
    //     assert!(Ctxt.f.is_none());
    //     assert!(Ctxt.p.is_none());
    //     Ctxt.f = Some(GroupParselet_parseLoop);
    //     Ctxt.p = P;

    //     // MUSTTAIL
    //     return GroupParselet_parseLoop(session, P, TokIn/*ignored*/);
    // #endif // !USE_MUSTTAIL
}

fn GroupParselet_parseLoop(session: &mut ParserSession, P: &GroupParselet) {
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

        session.eat_trivia_2(&mut Tok, TOPLEVEL, &mut Trivia1.borrow_mut());

        if TokenToCloser(Tok.tok) == Closr {
            //
            // Everything is good
            //

            session.push_trivia_seq(&mut Trivia1.borrow_mut());

            session.push_leaf_and_next(Tok);

            // MUSTTAIL
            return GroupParselet_reduceGroup(session, P);
        }

        if Tok.tok.isCloser() {
            //
            // some other closer
            //

            if session.check_group(TokenToCloser(Tok.tok)) {
                //
                // Something like  { ( }
                //                     ^
                //

                //
                // Do not consume the bad closer now
                //

                Trivia1.borrow_mut().reset(&mut session.tokenizer);

                // MUSTTAIl
                return GroupParselet_reduceMissingCloser(session, P);
            }

            //
            // Something like  { ) }
            //                   ^
            //

            session.push_trivia_seq(&mut Trivia1.borrow_mut());

            // #if !USE_MUSTTAIL
            PrefixToplevelCloserParselet_parsePrefix(session, Tok);

            continue;
            // #else
            //         // MUSTTAIL
            //         return PrefixToplevelCloserParselet_parsePrefix(session, prefixToplevelCloserParselet, Tok);
            // #endif
        }

        if Tok.tok == TokenKind::EndOfFile {
            //
            // Handle something like   { a EOF
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return GroupParselet_reduceUnterminatedGroup(session, P);
        }

        //
        // Handle the expression
        //

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = session.top_context();
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        session.parse_prefix(Tok);
    } // loop
      // #else
      //     let ref mut Ctxt = session.top_context();
      //     assert!(Ctxt.f == GroupParselet_parseLoop);
      //     assert!(Ctxt.p == P);

    //     // MUSTTAIL
    //     return session.parse_prefix(Tok);
    // #endif // !USE_MUSTTAIL
}

fn GroupParselet_reduceGroup(session: &mut ParserSession, P: &GroupParselet) {
    let Op = P.getOp();

    let node = GroupNode::new(Op, session.pop_context());
    session.push_node(node);

    session.pop_group();

    // MUSTTAIL
    return session.parse_climb();
}

fn GroupParselet_reduceMissingCloser(session: &mut ParserSession, P: &GroupParselet) {
    let Op = P.getOp();

    let node = GroupMissingCloserNode::new(Op, session.pop_context());
    session.push_node(node);

    session.pop_group();

    // MUSTTAIL
    return session.try_continue();
}

fn GroupParselet_reduceUnterminatedGroup(session: &mut ParserSession, P: &GroupParselet) {
    let Op = P.getOp();

    let node = UnterminatedGroupNeedsReparseNode::new(Op, session.pop_context());

    // The input MUST be valid UTF-8, because we only reduce an *unterminated*
    // group node if we've read an EOF (which is how we know it must be
    // unterminated: we've read all the input).
    let input = std::str::from_utf8(session.input())
        .expect("cannot reparse unterminated group node: input is not valid UTF-8");

    let node = crate::error::reparseUnterminatedGroupNode(
        node,
        input,
        session.tokenizer.tabWidth as usize,
    );

    session.push_node(node);

    session.pop_group();

    // MUSTTAIL
    return session.try_continue();
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
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        CallParselet_parseInfix(session, self, token)
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        PRECEDENCE_CALL
    }
}


fn CallParselet_parseInfix<'i>(
    session: &mut ParserSession<'i>,
    P: &CallParselet,
    TokIn: TokenRef<'i>,
) {
    panic_if_aborted!();


    //
    // if we used PRECEDENCE_CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
    //

    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| CallParselet_reduceCall(s));
    Ctxt.prec = PRECEDENCE_HIGHEST;

    let GP = P.getGP();

    // MUSTTAIL
    return GP.parse_prefix(session, TokIn);
}

fn CallParselet_reduceCall(session: &mut ParserSession) {
    let body = session.pop_node();

    let body: CallBody<_> = match body {
            crate::cst::CstNode::Group(group) => {
                let GroupNode(OperatorNode { op, children, src}) = group;

                let op = op.try_to_call_operator().expect("expected call group to be a valid CallOperator");

                let group = GroupNode(OperatorNode {
                    op, children, src
                });

                CallBody::Group(group)
            },
            crate::cst::CstNode::GroupMissingCloser(group) => {
                let GroupMissingCloserNode(OperatorNode { op, children, src}) = group;

                let op = op.try_to_call_operator().expect("expected call group to be a valid CallOperator");

                let group = GroupMissingCloserNode(OperatorNode {
                    op, children, src
                });

                CallBody::GroupMissingCloser(group)
            },
            other => panic!(
                "expected CallParselet body to reduce to a Group or GroupMissingCloser node; got: {:#?}",
                other
            ),
        };

    session.reduce_and_climb(|ctx| CallNode::concrete(ctx, body))
}

//======================================
// TildeParselet
//======================================

impl InfixParselet for TildeParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        TildeParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        if session.check_tilde() {
            return PRECEDENCE_LOWEST;
        }

        return PRECEDENCE_TILDE;
    }
}

fn TildeParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // Something like  a ~f~ b
    //
    // It'd be weird if this were an "infix operator"
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let mut FirstTok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut FirstTok, TOPLEVEL);

    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| TildeParselet_parse1(s));
    Ctxt.prec = PRECEDENCE_LOWEST;

    return session.parse_prefix(FirstTok);
}

fn TildeParselet_parse1(session: &mut ParserSession) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia_2(&mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

    if Tok1.tok != TokenKind::Tilde {
        //
        // Something like   a ~f b
        //
        // Not structurally correct, so return SyntaxErrorNode
        //

        Trivia1.borrow_mut().reset(&mut session.tokenizer);

        // MUSTTAIL
        return TildeParselet_reduceError(session);
    }

    session.push_trivia_seq(&mut Trivia1.borrow_mut());

    session.push_leaf_and_next(Tok1);

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok2, TOPLEVEL);

    //
    // Reset back to "outside" precedence
    //

    let ref mut Ctxt = session.top_context();
    // TODO: Figure out how to express this logic and re-enable this assertion.
    // assert!(Ctxt.f.unwrap() as usize == TildeParselet_parse1 as usize);
    Ctxt.f = Some(|s, _| TildeParselet_reduceTilde(s));
    Ctxt.prec = PRECEDENCE_TILDE;

    return session.parse_prefix(Tok2);
}

fn TildeParselet_reduceTilde(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| TernaryNode::new(TernaryOperator::CodeParser_TernaryTilde, ctx))
}

fn TildeParselet_reduceError(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| SyntaxErrorNode::new(SyntaxErrorKind::ExpectedTilde, ctx))
}

//======================================
// ColonParselet
//======================================

impl InfixParselet for ColonParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        ColonParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        if session.check_pattern_precedence() {
            return PRECEDENCE_FAKE_OPTIONALCOLON;
        }

        return PRECEDENCE_HIGHEST;
    }
}

fn ColonParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // Something like  symbol:object  or  pattern:optional
    //

    panic_if_aborted!();


    let colonLHS = session.check_colon_lhs();

    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    match colonLHS {
        ColonLHS::Pattern => {
            let ref mut Ctxt = session.top_context();
            assert!(Ctxt.f.is_none());
            Ctxt.f = Some(|s, _| ColonParselet_reducePattern(s));
            Ctxt.prec = PRECEDENCE_FAKE_PATTERNCOLON;

            return session.parse_prefix(Tok);
        },
        ColonLHS::Optional => {
            let ref mut Ctxt = session.top_context();
            assert!(Ctxt.f.is_none());
            Ctxt.f = Some(|s, _| ColonParselet_reduceOptional(s));
            Ctxt.prec = PRECEDENCE_FAKE_OPTIONALCOLON;

            // MUSTTAIl
            return session.parse_prefix(Tok);
        },
        ColonLHS::Error => {
            let ref mut Ctxt = session.top_context();
            assert!(Ctxt.f.is_none());
            Ctxt.f = Some(|s, _| ColonParselet_reduceError(s));
            Ctxt.prec = PRECEDENCE_FAKE_PATTERNCOLON;

            // MUSTTAIl
            return session.parse_prefix(Tok);
        },
    }
}

fn ColonParselet_reducePattern(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Pattern, ctx))
}

fn ColonParselet_reduceError(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| SyntaxErrorNode::new(SyntaxErrorKind::ExpectedSymbol, ctx))
}

fn ColonParselet_reduceOptional(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Optional, ctx))
}

//======================================
// SlashColonParselet
//======================================

impl InfixParselet for SlashColonParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        SlashColonParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        PRECEDENCE_SLASHCOLON
    }
}


fn SlashColonParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
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


    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| SlashColonParselet_parse1(s));

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn SlashColonParselet_parse1(session: &mut ParserSession) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia_2(&mut Tok, TOPLEVEL, &mut Trivia1.borrow_mut());

    match Tok.tok {
        TokenKind::Equal => {
            session.push_trivia_seq(&mut Trivia1.borrow_mut());

            session.set_precedence(PRECEDENCE_EQUAL);

            // MUSTTAIl
            return EqualParselet_parseInfixTag(session, Tok);
        },
        TokenKind::ColonEqual => {
            session.push_trivia_seq(&mut Trivia1.borrow_mut());

            session.set_precedence(PRECEDENCE_COLONEQUAL);

            // MUSTTAIl
            return ColonEqualParselet_parseInfixTag(session, Tok);
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
    return SlashColonParselet_reduceError(session);
}

fn SlashColonParselet_reduceError(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| SyntaxErrorNode::new(SyntaxErrorKind::ExpectedSet, ctx))
}

//======================================
// EqualParselet
//======================================

impl EqualParselet {
    pub(crate) const fn new() -> Self {
        Self {
            op: BinaryOperatorParselet::new(PRECEDENCE_EQUAL, BinaryOperator::Set),
        }
    }
}

impl InfixParselet for EqualParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        EqualParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        self.op.getPrecedence(session)
    }
}

fn EqualParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    if Tok.tok == TokenKind::Dot {
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //

        session.push_leaf_and_next(Tok);

        // MUSTTAIL
        return EqualParselet_reduceUnset(session);
    }

    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| EqualParselet_reduceSet(s));

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn EqualParselet_parseInfixTag<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // a /: b = c  and  a /: b = .  are handled here
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    if Tok.tok == TokenKind::Dot {
        //
        // Something like a = .
        //
        // tutorial/OperatorInputForms
        // Spaces to Avoid
        //

        session.push_leaf_and_next(Tok);

        // MUSTTAIL
        return EqualParselet_reduceTagUnset(session);
    }

    let ref mut Ctxt = session.top_context();
    // TODO: Figure out how to express this logic and re-enable this assertion.
    // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
    Ctxt.f = Some(|s, _| EqualParselet_reduceTagSet(s));

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn EqualParselet_reduceSet(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Set, ctx))
}

fn EqualParselet_reduceUnset(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Unset, ctx))
}

fn EqualParselet_reduceTagSet(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| TernaryNode::new(TernaryOperator::TagSet, ctx))
}

fn EqualParselet_reduceTagUnset(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| TernaryNode::new(TernaryOperator::TagUnset, ctx))
}

//======================================
// ColonEqualParselet
//======================================

impl ColonEqualParselet {
    pub(crate) const fn new() -> Self {
        ColonEqualParselet {
            op: BinaryOperatorParselet::new(PRECEDENCE_COLONEQUAL, BinaryOperator::SetDelayed),
        }
    }
}

impl InfixParselet for ColonEqualParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        ColonEqualParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        self.op.getPrecedence(session)
    }
}



fn ColonEqualParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| ColonEqualParselet_reduceSetDelayed(s));

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn ColonEqualParselet_parseInfixTag<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok, TOPLEVEL);

    let ref mut Ctxt = session.top_context();
    // TODO: Figure out how to express this logic and re-enable this assertion.
    // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
    Ctxt.f = Some(|s, _| ColonEqualParselet_reduceTagSetDelayed(s));

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn ColonEqualParselet_reduceSetDelayed(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::SetDelayed, ctx))
}

fn ColonEqualParselet_reduceTagSetDelayed(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| TernaryNode::new(TernaryOperator::TagSetDelayed, ctx))
}

//======================================
// CommaParselet
//======================================

impl InfixParselet for CommaParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        CommaParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_COMMA
    }
}


fn CommaParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok2, TOPLEVEL);

    if Tok2.tok == TokenKind::Comma || Tok2.tok == TokenKind::LongName_InvisibleComma {
        //
        // Something like  a,,
        //

        session.push_leaf(Token::error_at_start(
            TokenKind::Error_InfixImplicitNull,
            Tok2,
        ));

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = session.top_context();
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(Parser_identity);

        return CommaParselet_parseLoop(session);
        // #else
        //         let ref mut Ctxt = session.top_context();
        //         assert!(Ctxt.f.is_none());
        //         Ctxt.f = Some(CommaParselet_parseLoop);

        //         // MUSTTAIL
        //         return CommaParselet_parseLoop(session, ignored, TokIn/*ignored*/);
        // #endif // !USE_MUSTTAIL
    }

    // #if !USE_MUSTTAIL
    let ref mut Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(Parser_identity);

    session.parse_prefix(Tok2);

    return CommaParselet_parseLoop(session);
    // #else
    //     let ref mut Ctxt = session.top_context();
    //     assert!(Ctxt.f.is_none());
    //     Ctxt.f = Some(CommaParselet_parseLoop);

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn CommaParselet_parseLoop(session: &mut ParserSession) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia_2(&mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        if !(Tok1.tok == TokenKind::Comma || Tok1.tok == TokenKind::LongName_InvisibleComma) {
            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return CommaParselet_reduceComma(session);
        }

        //
        // Something like  a,b
        //

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        session.push_leaf_and_next(Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia(&mut Tok2, TOPLEVEL);

        if Tok2.tok == TokenKind::Comma || Tok2.tok == TokenKind::LongName_InvisibleComma {
            //
            // Something like  a,,
            //

            session.push_leaf(Token::error_at_start(
                TokenKind::Error_InfixImplicitNull,
                Tok2,
            ));

            // #if !USE_MUSTTAIL
            continue;
            // #else
            //         // MUSTTAIL
            //         return CommaParselet_parseLoop(session, ignored, ignored2);
            // #endif // !USE_MUSTTAIL
        }

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = session.top_context();
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        session.parse_prefix(Tok2);
    } // loop
      // #else
      //     let ref mut Ctxt = session.top_context();
      //     assert!(Ctxt.f == CommaParselet_parseLoop);

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn CommaParselet_reduceComma(session: &mut ParserSession) {
    let node = InfixNode::new(InfixOperator::CodeParser_Comma, session.pop_context());
    session.push_node(node);

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
    return session.try_continue();
}

//======================================
// SemiParselet
//======================================

impl InfixParselet for SemiParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        SemiParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_SEMI
    }
}


fn SemiParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // CompoundExpression should not cross toplevel newlines
    //
    session.eat_trivia_but_not_toplevel_newlines(&mut Tok2, TOPLEVEL);

    if Tok2.tok == TokenKind::Semi {
        //
        // Something like  a; ;
        //

        session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitNull, Tok2));

        //
        // nextToken() is not needed after an implicit token
        //

        // #if !USE_MUSTTAIL
        let ref mut Ctxt = session.top_context();
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(Parser_identity);

        return SemiParselet_parseLoop(session);
        // #else
        //         let ref mut Ctxt = session.top_context();
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
        let ref mut Ctxt = session.top_context();
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(Parser_identity);

        session.parse_prefix(Tok2);

        return SemiParselet_parseLoop(session);
        // #else
        //         let ref mut Ctxt = session.top_context();
        //         assert!(Ctxt.f.is_none());
        //         Ctxt.f = Some(SemiParselet_parseLoop);

        //         let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

        //         // MUSTTAIL
        //         return P2.parse_prefix(session, Tok2);
        // #endif // !USE_MUSTTAIL
    }

    //
    // Not beginning of an expression
    //
    // For example:  a;&
    //

    session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitNull, Tok2));

    //
    // nextToken() is not needed after an implicit token
    //

    // MUSTTAIL
    return SemiParselet_reduceCompoundExpression(session);
}

fn SemiParselet_parseLoop(session: &mut ParserSession) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia_2(&mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        if Tok1.tok != TokenKind::Semi {
            //
            // Something like  a;b
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiParselet_reduceCompoundExpression(session);
        }

        //
        // Something like  a;b
        //

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        session.push_leaf_and_next(Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        //
        // CompoundExpression should not cross toplevel newlines
        //
        session.eat_trivia_but_not_toplevel_newlines(&mut Tok2, TOPLEVEL);

        if Tok2.tok == TokenKind::Semi {
            //
            // Something like  a;b; ;
            //

            session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitNull, Tok2));

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
            let ref mut Ctxt = session.top_context();
            assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

            session.parse_prefix(Tok2);

            continue;
            // #else
            //         let ref mut Ctxt = session.top_context();
            //         assert!(Ctxt.f == SemiParselet_parseLoop);

            //         let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

            //         // MUSTTAIL
            //         return P2.parse_prefix(session, Tok2);
            // #endif // !USE_MUSTTAIL
        }

        //
        // Not beginning of an expression
        //
        // For example:  a;b;&
        //

        session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitNull, Tok2));

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiParselet_reduceCompoundExpression(session);

        // #if !USE_MUSTTAIL
    } // loop
      // #endif // !USE_MUSTTAIL
}

fn SemiParselet_reduceCompoundExpression(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| InfixNode::new(InfixOperator::CompoundExpression, ctx))
}

//======================================
// ColonColonParselet
//======================================

impl InfixParselet for ColonColonParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        ColonColonParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_COLONCOLON
    }
}


fn ColonColonParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // a::b
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //
    //
    // Special tokenization, so must do parsing here
    //

    let Tok2 = Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

    session.push_leaf_and_next(Tok2);

    // MUSTTAIL
    return ColonColonParselet_parseLoop(session);
}

fn ColonColonParselet_parseLoop(session: &mut ParserSession) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia_2(&mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        if Tok1.tok != TokenKind::ColonColon {
            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return ColonColonParselet_reduceMessageName(session);
        }

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        session.push_leaf_and_next(Tok1);

        //
        // Special tokenization, so must do parsing here
        //

        let Tok2 = Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

        session.push_leaf_and_next(Tok2);

        // #if !USE_MUSTTAIL
    } // loop
      // #else
      //     // MUSTTAIL
      //     return ColonColonParselet_parseLoop(session, ignored, ignored2);
      // #endif // !USE_MUSTTAIL
}

fn ColonColonParselet_reduceMessageName(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| InfixNode::new(InfixOperator::MessageName, ctx))
}

//======================================
// GreaterGreaterParselet
//======================================

impl InfixParselet for GreaterGreaterParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        GreaterGreaterParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_GREATERGREATER
    }
}

fn GreaterGreaterParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // a>>b
    //

    panic_if_aborted!();


    //
    // Special tokenization, so must do parsing here
    //

    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);

    session.eat_trivia_stringify_as_file(&mut Tok);

    session.push_leaf_and_next(Tok);

    // MUSTTAIL
    return GreaterGreaterParselet_reducePut(session);
}

fn GreaterGreaterParselet_reducePut(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Put, ctx))
}

//======================================
// GreaterGreaterGreaterParselet
//======================================

impl InfixParselet for GreaterGreaterGreaterParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        GreaterGreaterGreaterParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _session: &mut ParserSession) -> Precedence {
        PRECEDENCE_GREATERGREATERGREATER
    }
}

fn GreaterGreaterGreaterParselet_parseInfix<'i>(
    session: &mut ParserSession<'i>,
    TokIn: TokenRef<'i>,
) {
    //
    // a>>>b
    //

    panic_if_aborted!();


    //
    // Special tokenization, so must do parsing here
    //

    session.push_leaf_and_next(TokIn);

    let mut Tok = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);

    session.eat_trivia_stringify_as_file(&mut Tok);

    session.push_leaf_and_next(Tok);

    // MUSTTAIL
    return GreaterGreaterGreaterParselet_reducePutAppend(session);
}

fn GreaterGreaterGreaterParselet_reducePutAppend(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::PutAppend, ctx))
}

//======================================
// LessLessParselet
//======================================

impl PrefixParselet for LessLessParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        LessLessParselet_parsePrefix(session, token)
    }
}

fn LessLessParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // <<a
    //

    panic_if_aborted!();


    //
    // Special tokenization, so must do parsing here
    //

    session.push_leaf_and_next(TokIn);

    session.push_context(PRECEDENCE_HIGHEST);

    let mut Tok = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);

    session.eat_trivia_stringify_as_file(&mut Tok);

    session.push_leaf_and_next(Tok);

    // MUSTTAIL
    return LessLessParselet_reduceGet(session);
}

fn LessLessParselet_reduceGet(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| PrefixNode::new(PrefixOperator::Get, ctx))
}

//======================================
// HashParselet
//======================================

impl PrefixParselet for HashParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        HashParselet_parsePrefix(session, token)
    }
}

fn HashParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
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


    session.push_leaf_and_next(TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, INSIDE_SLOT);

    match Tok.tok {
        TokenKind::Integer | TokenKind::String => {
            session.push_context(PRECEDENCE_HIGHEST);

            session.push_leaf_and_next(Tok);

            // MUSTTAIl
            return HashParselet_reduceSlot(session);
        },
        _ => (),
    }

    // MUSTTAIL
    return session.parse_climb();
}

fn HashParselet_reduceSlot(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| CompoundNode::new(CompoundOperator::Slot, ctx))
}

//======================================
// HashHashParselet
//======================================

impl PrefixParselet for HashHashParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        HashHashParselet_parsePrefix(session, token)
    }
}

fn HashHashParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // Something like  ##  or  ##1
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, INSIDE_SLOTSEQUENCE);

    match Tok.tok {
        TokenKind::Integer => {
            session.push_context(PRECEDENCE_HIGHEST);

            session.push_leaf_and_next(Tok);

            // MUSTTAIl
            return HashHashParselet_reduceSlotSequence(session);
        },
        _ => (),
    }

    // MUSTTAIL
    return session.parse_climb();
}

fn HashHashParselet_reduceSlotSequence(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| CompoundNode::new(CompoundOperator::SlotSequence, ctx))
}

//======================================
// PercentParselet
//======================================

impl PrefixParselet for PercentParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        PercentParselet_parsePrefix(session, token)
    }
}

fn PercentParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // Something like  %  or  %1
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, INSIDE_OUT);

    match Tok.tok {
        TokenKind::Integer => {
            session.push_context(PRECEDENCE_HIGHEST);

            session.push_leaf_and_next(Tok);

            // MUSTTAIl
            return PercentParselet_reduceOut(session);
        },
        _ => (),
    }

    // MUSTTAIL
    return session.parse_climb();
}

fn PercentParselet_reduceOut(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| CompoundNode::new(CompoundOperator::Out, ctx))
}
