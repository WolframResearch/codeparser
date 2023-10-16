mod integral_parselet;
mod semi_semi_parselet;
mod times_parselet;
mod under_parselet;


use std::any::Any;

use crate::{
    cst::{
        BinaryNode, BinaryOperator, CallBody, CallNode, CompoundNode,
        CompoundOperator, GroupMissingCloserNode, GroupNode, GroupOperator,
        InfixNode, InfixOperator, OperatorNode, PostfixNode, PostfixOperator,
        PrefixBinaryOperator, PrefixNode, PrefixOperator, SyntaxErrorKind,
        SyntaxErrorNode, TernaryNode, TernaryOperator,
        UnterminatedGroupNeedsReparseNode,
    },
    panic_if_aborted,
    parse::token_parselets::{under1Parselet, under2Parselet, under3Parselet},
    parse::{ColonLHS, ParserSession},
    precedence::Precedence,
    source::*,
    tokenize::{
        token_kind::{Closer, GroupOpenerToCloser, TokenToCloser},
        tokenizer::Tokenizer_currentToken_stringifyAsTag,
        Token, TokenKind, TokenRef,
    },
};

pub(crate) type ParseletPtr = &'static dyn Parselet;

pub(crate) type ParseFunction =
    for<'i> fn(session: &mut ParserSession<'i>, parselet: ParseletPtr);

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
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        token: TokenRef<'i>,
    );
}


pub(crate) trait InfixParselet: Parselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        token: TokenRef<'i>,
    );

    fn getPrecedence(&self, session: &mut ParserSession) -> Option<Precedence>;

    fn getOp(&self) -> InfixParseletOperator {
        // TODO: Make this sentinel value unnecessary?
        return InfixParseletOperator::Infix(
            InfixOperator::CodeParser_InternalInvalid,
        );
    }

    /// Should always return either `tok_in` or a new
    /// [`TokenKind::Fake_ImplicitTimes`] token.
    fn process_implicit_times<'i>(
        &self,
        _session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        return tok_in;
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum InfixParseletOperator {
    Infix(InfixOperator),
    Postfix(PostfixOperator),
    Binary(BinaryOperator),
}

impl From<InfixOperator> for InfixParseletOperator {
    fn from(op: InfixOperator) -> Self {
        Self::Infix(op)
    }
}

impl From<BinaryOperator> for InfixParseletOperator {
    fn from(op: BinaryOperator) -> Self {
        Self::Binary(op)
    }
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
    GP: &'static GroupParselet,
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
pub(crate) struct SemiSemiParselet /* : public PrefixParselet, public InfixParselet */
{}



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
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return session.parse_climb();
    }
}

//======================================
// PrefixErrorParselet
//======================================

impl PrefixParselet for PrefixErrorParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        assert!(tok_in.tok.isError());

        session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// PrefixCloserParselet
//======================================

impl PrefixParselet for PrefixCloserParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        assert!(tok_in.tok.isCloser());

        panic_if_aborted!();


        //
        // Inside some other parselet that is not GroupParselet
        //

        let kind = if session.top_precedence() == Precedence::COMMA {
            TokenKind::Error_InfixImplicitNull
        } else {
            TokenKind::Error_ExpectedOperand
        };

        session.push_leaf(Token::at_start(kind, tok_in));

        //
        // Do not take the closer.
        // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
        //
        // TODO(cleanup): This call does nothing? Add test and remove.
        let _ = session.tokenizer.peek_token();

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// PrefixToplevelCloserParselet
//======================================

impl PrefixParselet for PrefixToplevelCloserParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        assert!(tok_in.tok.isCloser());

        panic_if_aborted!();


        //
        // if we are at the top, then make sure to take the token and report it
        //

        session.push_leaf(Token::at(TokenKind::Error_UnexpectedCloser, tok_in));

        tok_in.skip(&mut session.tokenizer);

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// PrefixEndOfFileParselet
//======================================

impl PrefixParselet for PrefixEndOfFileParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  a+<EOF>
        //

        panic_if_aborted!();


        let kind = if session.top_precedence() == Precedence::COMMA {
            TokenKind::Error_InfixImplicitNull
        } else {
            TokenKind::Error_ExpectedOperand
        };

        session.push_leaf(Token::at_start(kind, tok_in));

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// PrefixUnsupportedParselet
//======================================

impl PrefixParselet for PrefixUnsupportedTokenParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf(Token::at(TokenKind::Error_UnsupportedToken, tok_in));

        tok_in.skip(&mut session.tokenizer);

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// PrefixCommaParselet
//======================================

impl PrefixParselet for PrefixCommaParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // if the input is  f[a@,2]  then we want to return TokenKind::ERROR_EXPECTEDOPERAND
        //
        // if the input is  f[,2]  then we want to return TokenKind::ERROR_PREFIXIMPLICITNULL
        //

        panic_if_aborted!();


        let kind = if session.top_precedence() == None {
            TokenKind::Error_PrefixImplicitNull
        } else {
            TokenKind::Error_ExpectedOperand
        };

        session.push_leaf(Token::at_start(kind, tok_in));

        // MUSTTAIL
        return session.parse_climb();
    }
}

//======================================
// PrefixUnhandledParselet
//======================================

impl PrefixParselet for PrefixUnhandledParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        assert!(!tok_in.tok.isPossibleBeginning(), "handle at call site");

        panic_if_aborted!();


        session.push_leaf(Token::at_start(
            TokenKind::Error_ExpectedOperand,
            tok_in,
        ));

        //
        // Do not take next token
        //
        // TODO(cleanup): This call does nothing? Add test and remove.
        let _ = session.tokenizer.peek_token();

        let TokenPrecedence =
            tok_in.tok.infix_parselet().getPrecedence(session);

        //
        // if (Ctxt.prec > TokenPrecedence)
        //   goto prefixUnhandledParseletRet;
        // else if (Ctxt.prec == TokenPrecedence && Ctxt.prec.Associativity is NonRight)
        //   goto prefixUnhandledParseletRet;
        //
        if Precedence::greater(session.top_precedence(), TokenPrecedence) {
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

        // MUSTTAIL
        return session.parse_infix(tok_in);
    }
}

//======================================
// InfixToplevelNewlineParselet
//======================================

impl InfixParselet for InfixToplevelNewlineParselet {
    fn parse_infix<'i>(
        &'static self,
        _session: &mut ParserSession<'i>,
        _token: TokenRef<'i>,
    ) {
        assert!(false);
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Option<Precedence> {
        //
        // Do not do Implicit Times across top-level newlines
        //
        return None;
    }
}


//======================================
// SymbolParselet
//======================================

impl PrefixParselet for SymbolParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  x  or x_
        //

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.tokenizer.peek_token();

        //
        // if we are here, then we know that Sym could bind to _
        //

        match tok.tok {
            TokenKind::Under => {
                //
                // Something like  a_
                //

                session.push_context(Precedence::HIGHEST);

                //
                // Context-sensitive and OK to build stack
                //

                under1Parselet.parse_infix_context_sensitive(session, tok);

                // MUSTTAIl
                return SymbolParselet::reducePatternBlank(
                    session,
                    &under1Parselet,
                );
            },
            TokenKind::UnderUnder => {
                //
                // Something like  a__
                //

                session.push_context(Precedence::HIGHEST);

                //
                // Context-sensitive and OK to build stack
                //

                under2Parselet.parse_infix_context_sensitive(session, tok);

                // MUSTTAIl
                return SymbolParselet::reducePatternBlank(
                    session,
                    &under2Parselet,
                );
            },
            TokenKind::UnderUnderUnder => {
                //
                // Something like  a___
                //

                session.push_context(Precedence::HIGHEST);

                //
                // Context-sensitive and OK to build stack
                //

                under3Parselet.parse_infix_context_sensitive(session, tok);

                // MUSTTAIl
                return SymbolParselet::reducePatternBlank(
                    session,
                    &under3Parselet,
                );
            },
            TokenKind::UnderDot => {
                //
                // Something like  a_.
                //

                session.push_context(Precedence::HIGHEST);

                //
                // Context-sensitive and OK to build stack
                //

                //
                // infix
                //
                // Something like  a_.

                session.push_leaf_and_next(tok);

                // MUSTTAIl
                return SymbolParselet::reducePatternOptionalDefault(session);
            },
            _ => (),
        } // switch

        //
        // Something like  a
        //

        // MUSTTAIL
        return session.parse_climb();
    }
}

impl SymbolParselet {
    fn reducePatternBlank(session: &mut ParserSession, P: &UnderParselet) {
        session.reduce_and_climb(|ctx| CompoundNode::new(P.PBOp, ctx))
    }

    fn reducePatternOptionalDefault(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            CompoundNode::new(
                CompoundOperator::CodeParser_PatternOptionalDefault,
                ctx,
            )
        })
    }
}

//======================================
// PrefixOperatorParselet
//======================================

impl PrefixOperatorParselet {
    pub(crate) const fn new(
        precedence: Precedence,
        Op: PrefixOperator,
    ) -> Self {
        PrefixOperatorParselet { precedence, Op }
    }

    fn getPrecedence(&self) -> Option<Precedence> {
        return Some(self.precedence);
    }
}

impl PrefixParselet for PrefixOperatorParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();

        session.push_leaf_and_next(tok_in);

        let ctxt = session.push_context(self.getPrecedence());

        ctxt.init_callback(
            PrefixOperatorParselet::reducePrefixOperator,
            Some(self),
        );

        let tok = session.current_token_eat_trivia();

        // MUSTTAIL
        return session.parse_prefix(tok);
    }
}

impl PrefixOperatorParselet {
    fn reducePrefixOperator(session: &mut ParserSession, P: ParseletPtr) {
        let P = P
            .as_any()
            .downcast_ref::<PrefixOperatorParselet>()
            .expect("unable to downcast to PrefixOperatorParselet");

        session.reduce_and_climb(|ctx| PrefixNode::new(P.Op, ctx))
    }
}

//======================================
// InfixImplicitTimesParselet
//======================================

impl InfixParselet for InfixImplicitTimesParselet {
    fn parse_infix<'i>(
        &'static self,
        _session: &mut ParserSession<'i>,
        _token: TokenRef<'i>,
    ) {
        assert!(false);
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        panic!("The last token may not have been added to InfixParselets");
    }


    fn process_implicit_times<'i>(
        &self,
        _session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        return Token::at_start(TokenKind::Fake_ImplicitTimes, tok_in);
    }
}

//======================================
// PrefixAssertFalseParselet
//======================================

impl PrefixParselet for PrefixAssertFalseParselet {
    // fn getPrecedence(&self, session: &mut ParserSession) -> Option<Precedence> {
    //     None
    // }

    fn parse_prefix<'i>(
        &'static self,
        _session: &mut ParserSession<'i>,
        _token: TokenRef<'i>,
    ) {
        assert!(false);
    }
}


//======================================
// InfixAssertFalseParselet
//======================================

impl InfixParselet for InfixAssertFalseParselet {
    fn parse_infix<'i>(
        &'static self,
        _session: &mut ParserSession<'i>,
        _token: TokenRef<'i>,
    ) {
        assert!(false)
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        None
    }
}

//======================================
// BinaryOperatorParselet
//======================================

impl BinaryOperatorParselet {
    pub(crate) const fn new(
        precedence: Precedence,
        Op: BinaryOperator,
    ) -> Self {
        BinaryOperatorParselet { precedence, Op }
    }
}

impl InfixParselet for BinaryOperatorParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();

        ctxt.init_callback(
            BinaryOperatorParselet::reduceBinaryOperator,
            Some(self),
        );

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(self.precedence)
    }

    fn getOp(&self) -> InfixParseletOperator {
        self.Op.into()
    }
}

impl BinaryOperatorParselet {
    fn reduceBinaryOperator(session: &mut ParserSession, P: ParseletPtr) {
        let P = P
            .as_any()
            .downcast_ref::<BinaryOperatorParselet>()
            .expect("unable to downcast to BinaryOperatorParselet");

        session.reduce_and_climb(|ctx| BinaryNode::new(P.Op, ctx))
    }
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
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let tok2 = session.current_token_eat_trivia();

        // #if !USE_MUSTTAIL
        let ctxt = session.top_context();
        ctxt.init_identity();

        session.parse_prefix(tok2);

        return self.parse_loop(session);
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

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(self.precedence)
    }

    fn getOp(&self) -> InfixParseletOperator {
        self.Op.into()
    }
}

impl InfixOperatorParselet {
    fn parse_loop(&self, session: &mut ParserSession) {
        // #if !USE_MUSTTAIL
        loop {
            // #endif // !USE_MUSTTAIL

            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token_eat_trivia_into();

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
            if tok1.tok.infix_parselet().getOp() != self.getOp() {
                //
                // Tok.tok != tok_in.tok, so break
                //

                trivia1.reset(&mut session.tokenizer);

                // MUSTTAIL
                return session
                    .reduce_and_climb(|ctx| InfixNode::new(self.Op, ctx));
            }

            session.push_trivia_seq(trivia1);

            session.push_leaf_and_next(tok1);

            let Tok2 = session.current_token_eat_trivia();

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

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
}

//======================================
// PostfixOperatorParselet
//======================================

impl PostfixOperatorParselet {
    pub(crate) const fn new(
        precedence: Precedence,
        Op: PostfixOperator,
    ) -> Self {
        PostfixOperatorParselet { precedence, Op }
    }
}

impl InfixParselet for PostfixOperatorParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return session.reduce_and_climb(|ctx| PostfixNode::new(self.Op, ctx));
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(self.precedence)
    }

    fn getOp(&self) -> InfixParseletOperator {
        InfixParseletOperator::Postfix(self.Op)
    }
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
}

impl PrefixParselet for GroupParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        session.push_group(GroupOpenerToCloser(tok_in.tok));

        let ctxt = session.push_context(None);
        ctxt.init_identity();

        // #if !USE_MUSTTAIL

        return self.parse_loop(session);
        // #else
        //     assert!(Ctxt.f.is_none());
        //     assert!(Ctxt.p.is_none());
        //     Ctxt.f = Some(GroupParselet_parseLoop);
        //     Ctxt.p = P;

        //     // MUSTTAIL
        //     return GroupParselet_parseLoop(session, P, tok_in/*ignored*/);
        // #endif // !USE_MUSTTAIL
    }
}

impl GroupParselet {
    fn parse_loop(&self, session: &mut ParserSession) {
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

            let (trivia1, tok) = session.current_token_eat_trivia_into();

            if TokenToCloser(tok.tok) == self.closer {
                //
                // Everything is good
                //

                session.push_trivia_seq(trivia1);

                session.push_leaf_and_next(tok);

                // MUSTTAIL
                return self.reduce_group(session);
            }

            if tok.tok.isCloser() {
                //
                // some other closer
                //

                if session.check_group(TokenToCloser(tok.tok)) {
                    //
                    // Something like  { ( }
                    //                     ^
                    //

                    //
                    // Do not consume the bad closer now
                    //

                    trivia1.reset(&mut session.tokenizer);

                    // MUSTTAIl
                    return self.reduce_missing_closer(session);
                }

                //
                // Something like  { ) }
                //                   ^
                //

                session.push_trivia_seq(trivia1);

                // #if !USE_MUSTTAIL
                (PrefixToplevelCloserParselet {}).parse_prefix(session, tok);

                continue;
                // #else
                //         // MUSTTAIL
                //         return PrefixToplevelCloserParselet_parsePrefix(session, prefixToplevelCloserParselet, Tok);
                // #endif
            }

            if tok.tok == TokenKind::EndOfFile {
                //
                // Handle something like   { a EOF
                //

                session.push_trivia_seq(trivia1);

                // MUSTTAIL
                return self.reduce_unterminated_group(session);
            }

            //
            // Handle the expression
            //

            session.push_trivia_seq(trivia1);

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

            session.parse_prefix(tok);
        } // loop
          // #else
          //     let ref mut Ctxt = session.top_context();
          //     assert!(Ctxt.f == GroupParselet_parseLoop);
          //     assert!(Ctxt.p == P);

        //     // MUSTTAIL
        //     return session.parse_prefix(Tok);
        // #endif // !USE_MUSTTAIL
    }

    fn reduce_group(&self, session: &mut ParserSession) {
        let op = self.Op;

        session.pop_group();

        session.reduce_and_climb(|ctx| GroupNode::new(op, ctx))
    }

    fn reduce_missing_closer(&self, session: &mut ParserSession) {
        let op = self.Op;

        session.pop_group();

        session.reduce(|ctx| GroupMissingCloserNode::new(op, ctx));

        // MUSTTAIL
        return session.try_continue();
    }

    fn reduce_unterminated_group(&self, session: &mut ParserSession) {
        let op = self.Op;

        // The input MUST be valid UTF-8, because we only reduce an *unterminated*
        // group node if we've read an EOF (which is how we know it must be
        // unterminated: we've read all the input).
        let input = std::str::from_utf8(session.input()).expect(
            "cannot reparse unterminated group node: input is not valid UTF-8",
        );

        let tab_width = session.tokenizer.tabWidth as usize;

        session.pop_group();

        session.reduce(|ctx| {
            let node = UnterminatedGroupNeedsReparseNode::new(op, ctx);

            crate::error::reparse_unterminated_group_node(
                node, input, tab_width,
            )
        });

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// CallParselet
//======================================

impl CallParselet {
    pub(crate) const fn new(GP: &'static GroupParselet) -> Self {
        Self { GP }
    }

    fn getGP(&self) -> &'static GroupParselet {
        return self.GP;
    }
}

impl InfixParselet for CallParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        //
        // if we used Precedence::CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
        //

        let ctxt = session.top_context();
        ctxt.init_callback(|s, _| CallParselet::reduce_call(s), None);
        ctxt.set_precedence(Precedence::HIGHEST);

        let GP = self.getGP();

        // MUSTTAIL
        return GP.parse_prefix(session, tok_in);
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Option<Precedence> {
        Some(Precedence::CALL)
    }
}

impl CallParselet {
    fn reduce_call(session: &mut ParserSession) {
        let body = session.pop_node();

        let body: CallBody<_> = match body {
            crate::cst::Cst::Group(group) => {
                let GroupNode(OperatorNode { op, children, src}) = group;

                let op = op.try_to_call_operator().expect("expected call group to be a valid CallOperator");

                let group = GroupNode(OperatorNode {
                    op, children, src
                });

                CallBody::Group(group)
            },
            crate::cst::Cst::GroupMissingCloser(group) => {
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
}

//======================================
// TildeParselet
//======================================

impl InfixParselet for TildeParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  a ~f~ b
        //                   ^
        //
        // It'd be weird if this were an "infix operator"
        //

        panic_if_aborted!();

        debug_assert_eq!(tok_in.tok, TokenKind::Tilde);


        session.push_leaf_and_next(tok_in);

        let first_tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        ctxt.init_callback(|s, _| TildeParselet::parse1(s), None);
        ctxt.set_precedence(None);

        return session.parse_prefix(first_tok);
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Option<Precedence> {
        if session.top_non_trivia_node_is_tilde() {
            return None;
        }

        return Some(Precedence::TILDE);
    }
}

impl TildeParselet {
    fn parse1(session: &mut ParserSession) {
        panic_if_aborted!();


        let (trivia1, tok1) = session.current_token_eat_trivia_into();

        if tok1.tok != TokenKind::Tilde {
            //
            // Something like   a ~f b
            //
            // Not structurally correct, so return SyntaxErrorNode
            //

            trivia1.reset(&mut session.tokenizer);

            // MUSTTAIL
            return TildeParselet::reduce_error(session);
        }

        session.push_trivia_seq(trivia1);

        session.push_leaf_and_next(tok1);

        let tok2 = session.current_token_eat_trivia();

        //
        // Reset back to "outside" precedence
        //

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == TildeParselet_parse1 as usize);
        ctxt.set_callback(|s, _| TildeParselet::reduce_tilde(s));
        ctxt.set_precedence(Precedence::TILDE);

        return session.parse_prefix(tok2);
    }

    fn reduce_tilde(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            TernaryNode::new(TernaryOperator::CodeParser_TernaryTilde, ctx)
        })
    }

    fn reduce_error(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            SyntaxErrorNode::new(SyntaxErrorKind::ExpectedTilde, ctx)
        })
    }
}

//======================================
// ColonParselet
//======================================

impl InfixParselet for ColonParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  symbol:object  or  pattern:optional
        //

        panic_if_aborted!();


        let colonLHS = session.check_colon_lhs();

        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        match colonLHS {
            ColonLHS::Pattern => {
                let ctxt = session.top_context();
                ctxt.init_callback(
                    |s, _| ColonParselet::reduce_pattern(s),
                    None,
                );
                ctxt.set_precedence(Precedence::FAKE_PATTERNCOLON);

                return session.parse_prefix(tok);
            },
            ColonLHS::Optional => {
                let ctxt = session.top_context();
                ctxt.init_callback(
                    |s, _| ColonParselet::reduce_optional(s),
                    None,
                );
                ctxt.set_precedence(Precedence::FAKE_OPTIONALCOLON);

                // MUSTTAIl
                return session.parse_prefix(tok);
            },
            ColonLHS::Error => {
                let ctxt = session.top_context();
                ctxt.init_callback(|s, _| ColonParselet::reduce_error(s), None);
                ctxt.set_precedence(Precedence::FAKE_PATTERNCOLON);

                // MUSTTAIl
                return session.parse_prefix(tok);
            },
        }
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Option<Precedence> {
        if session.check_pattern_precedence() {
            return Some(Precedence::FAKE_OPTIONALCOLON);
        }

        return Some(Precedence::HIGHEST);
    }
}

impl ColonParselet {
    fn reduce_pattern(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            BinaryNode::new(BinaryOperator::Pattern, ctx)
        })
    }

    fn reduce_error(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            SyntaxErrorNode::new(SyntaxErrorKind::ExpectedSymbol, ctx)
        })
    }

    fn reduce_optional(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            BinaryNode::new(BinaryOperator::Optional, ctx)
        })
    }
}

//======================================
// SlashColonParselet
//======================================

impl InfixParselet for SlashColonParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
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


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        ctxt.init_callback(|s, _| SlashColonParselet::parse1(s), None);

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Option<Precedence> {
        Some(Precedence::SLASHCOLON)
    }
}

impl SlashColonParselet {
    fn parse1(session: &mut ParserSession) {
        panic_if_aborted!();


        let (trivia1, tok) = session.current_token_eat_trivia_into();

        match tok.tok {
            TokenKind::Equal => {
                session.push_trivia_seq(trivia1);

                session.set_precedence(Precedence::EQUAL);

                // MUSTTAIl
                return EqualParselet::parse_infix_tag(session, tok);
            },
            TokenKind::ColonEqual => {
                session.push_trivia_seq(trivia1);

                session.set_precedence(Precedence::COLONEQUAL);

                // MUSTTAIl
                return ColonEqualParselet::parse_infix_tag(session, tok);
            },
            _ => (),
        } // switch

        trivia1.reset(&mut session.tokenizer);

        //
        // Anything other than:
        // a /: b = c
        // a /: b := c
        // a /: b =.
        //

        // MUSTTAIL
        return SlashColonParselet::reduce_error(session);
    }

    fn reduce_error(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            SyntaxErrorNode::new(SyntaxErrorKind::ExpectedSet, ctx)
        })
    }
}

//======================================
// EqualParselet
//======================================

impl EqualParselet {
    pub(crate) const fn new() -> Self {
        Self {
            op: BinaryOperatorParselet::new(
                Precedence::EQUAL,
                BinaryOperator::Set,
            ),
        }
    }
}

impl InfixParselet for EqualParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        if tok.tok == TokenKind::Dot {
            //
            // Something like a = .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //

            session.push_leaf_and_next(tok);

            // MUSTTAIL
            return EqualParselet::reduce_Unset(session);
        }

        let ctxt = session.top_context();
        ctxt.init_callback(|s, _| EqualParselet::reduce_Set(s), None);

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Option<Precedence> {
        self.op.getPrecedence(session)
    }
}

impl EqualParselet {
    fn parse_infix_tag<'i>(
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // a /: b = c  and  a /: b = .  are handled here
        //

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        if tok.tok == TokenKind::Dot {
            //
            // Something like a = .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //

            session.push_leaf_and_next(tok);

            // MUSTTAIL
            return EqualParselet::reduce_TagUnset(session);
        }

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
        ctxt.set_callback(|s, _| EqualParselet::reduce_TagSet(s));

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn reduce_Set(session: &mut ParserSession) {
        session
            .reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Set, ctx))
    }

    fn reduce_Unset(session: &mut ParserSession) {
        session
            .reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Unset, ctx))
    }

    fn reduce_TagSet(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            TernaryNode::new(TernaryOperator::TagSet, ctx)
        })
    }

    fn reduce_TagUnset(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            TernaryNode::new(TernaryOperator::TagUnset, ctx)
        })
    }
}

//======================================
// ColonEqualParselet
//======================================

impl ColonEqualParselet {
    pub(crate) const fn new() -> Self {
        ColonEqualParselet {
            op: BinaryOperatorParselet::new(
                Precedence::COLONEQUAL,
                BinaryOperator::SetDelayed,
            ),
        }
    }
}

impl InfixParselet for ColonEqualParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        ctxt.init_callback(
            |s, _| ColonEqualParselet::reduce_SetDelayed(s),
            None,
        );

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Option<Precedence> {
        self.op.getPrecedence(session)
    }
}


impl ColonEqualParselet {
    fn parse_infix_tag<'i>(
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
        ctxt.set_callback(|s, _| ColonEqualParselet::reduce_TagSetDelayed(s));

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn reduce_SetDelayed(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            BinaryNode::new(BinaryOperator::SetDelayed, ctx)
        })
    }

    fn reduce_TagSetDelayed(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            TernaryNode::new(TernaryOperator::TagSetDelayed, ctx)
        })
    }
}

//======================================
// CommaParselet
//======================================

impl InfixParselet for CommaParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let tok2 = session.current_token_eat_trivia();

        if tok2.tok == TokenKind::Comma
            || tok2.tok == TokenKind::LongName_InvisibleComma
        {
            //
            // Something like  a,,
            //

            session.push_leaf(Token::at_start(
                TokenKind::Error_InfixImplicitNull,
                tok2,
            ));

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            ctxt.init_identity();

            return CommaParselet::parse_loop(session);
            // #else
            //         let ref mut Ctxt = session.top_context();
            //         assert!(Ctxt.f.is_none());
            //         Ctxt.f = Some(CommaParselet_parseLoop);

            //         // MUSTTAIL
            //         return CommaParselet_parseLoop(session, ignored, tok_in/*ignored*/);
            // #endif // !USE_MUSTTAIL
        }

        // #if !USE_MUSTTAIL
        let ctxt = session.top_context();
        ctxt.init_identity();

        session.parse_prefix(tok2);

        return CommaParselet::parse_loop(session);
        // #else
        //     let ref mut Ctxt = session.top_context();
        //     assert!(Ctxt.f.is_none());
        //     Ctxt.f = Some(CommaParselet_parseLoop);

        //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

        //     // MUSTTAIL
        //     return P2.parse_prefix(session, Tok2);
        // #endif // !USE_MUSTTAIL
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(Precedence::COMMA)
    }
}

impl CommaParselet {
    fn parse_loop(session: &mut ParserSession) {
        // #if !USE_MUSTTAIL
        loop {
            // #endif // !USE_MUSTTAIL

            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token_eat_trivia_into();

            if !(tok1.tok == TokenKind::Comma
                || tok1.tok == TokenKind::LongName_InvisibleComma)
            {
                trivia1.reset(&mut session.tokenizer);

                // MUSTTAIL
                return CommaParselet::reduce_comma(session);
            }

            //
            // Something like  a,b
            //

            session.push_trivia_seq(trivia1);

            session.push_leaf_and_next(tok1);

            let tok2 = session.current_token_eat_trivia();

            if tok2.tok == TokenKind::Comma
                || tok2.tok == TokenKind::LongName_InvisibleComma
            {
                //
                // Something like  a,,
                //

                session.push_leaf(Token::at_start(
                    TokenKind::Error_InfixImplicitNull,
                    tok2,
                ));

                // #if !USE_MUSTTAIL
                continue;
                // #else
                //         // MUSTTAIL
                //         return CommaParselet_parseLoop(session, ignored, ignored2);
                // #endif // !USE_MUSTTAIL
            }

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

            session.parse_prefix(tok2);
        } // loop
          // #else
          //     let ref mut Ctxt = session.top_context();
          //     assert!(Ctxt.f == CommaParselet_parseLoop);

        //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

        //     // MUSTTAIL
        //     return P2.parse_prefix(session, Tok2);
        // #endif // !USE_MUSTTAIL
    }

    fn reduce_comma(session: &mut ParserSession) {
        session
            .reduce(|ctx| InfixNode::new(InfixOperator::CodeParser_Comma, ctx));

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
}

//======================================
// SemiParselet
//======================================

impl InfixParselet for SemiParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        //
        // CompoundExpression should not cross toplevel newlines
        //
        let tok2 = session.current_token_eat_trivia_but_not_toplevel_newlines();

        if tok2.tok == TokenKind::Semi {
            //
            // Something like  a; ;
            //

            session
                .push_leaf(Token::at_start(TokenKind::Fake_ImplicitNull, tok2));

            //
            // nextToken() is not needed after an implicit token
            //

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            ctxt.init_identity();

            return SemiParselet::parse_loop(session);
            // #else
            //         let ref mut Ctxt = session.top_context();
            //         assert!(Ctxt.f.is_none());
            //         Ctxt.f = Some(SemiParselet_parseLoop);

            //         // MUSTTAIL
            //         return SemiParselet_parseLoop(session, ignored, tok_in/*ignored*/);
            // #endif // !USE_MUSTTAIL
        }

        if tok2.tok.isPossibleBeginning() {
            //
            // Something like  a;+2
            //

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            ctxt.init_identity();

            session.parse_prefix(tok2);

            return SemiParselet::parse_loop(session);
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

        session.push_leaf(Token::at_start(TokenKind::Fake_ImplicitNull, tok2));

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiParselet::reduce_CompoundExpression(session);
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(Precedence::SEMI)
    }
}

impl SemiParselet {
    fn parse_loop(session: &mut ParserSession) {
        // #if !USE_MUSTTAIL
        loop {
            // #endif // !USE_MUSTTAIL

            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token_eat_trivia_into();

            if tok1.tok != TokenKind::Semi {
                //
                // Something like  a;b
                //

                trivia1.reset(&mut session.tokenizer);

                // MUSTTAIL
                return SemiParselet::reduce_CompoundExpression(session);
            }

            //
            // Something like  a;b
            //

            session.push_trivia_seq(trivia1);

            session.push_leaf_and_next(tok1);

            //
            // CompoundExpression should not cross toplevel newlines
            //
            let tok2 =
                session.current_token_eat_trivia_but_not_toplevel_newlines();

            if tok2.tok == TokenKind::Semi {
                //
                // Something like  a;b; ;
                //

                session.push_leaf(Token::at_start(
                    TokenKind::Fake_ImplicitNull,
                    tok2,
                ));

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

            if tok2.tok.isPossibleBeginning() {
                //
                // Something like  a;b;+2
                //

                // #if !USE_MUSTTAIL
                let ctxt = session.top_context();
                assert!(ctxt.is_identity());

                session.parse_prefix(tok2);

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

            session
                .push_leaf(Token::at_start(TokenKind::Fake_ImplicitNull, tok2));

            //
            // nextToken() is not needed after an implicit token
            //

            // MUSTTAIL
            return SemiParselet::reduce_CompoundExpression(session);

            // #if !USE_MUSTTAIL
        } // loop
          // #endif // !USE_MUSTTAIL
    }

    fn reduce_CompoundExpression(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            InfixNode::new(InfixOperator::CompoundExpression, ctx)
        })
    }
}

//======================================
// ColonColonParselet
//======================================

impl InfixParselet for ColonColonParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // a::b
        //

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //
        //
        // Special tokenization, so must do parsing here
        //

        let Tok2 =
            Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

        session.push_leaf_and_next(Tok2);

        // MUSTTAIL
        return ColonColonParselet::parse_loop(session);
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(Precedence::COLONCOLON)
    }
}

impl ColonColonParselet {
    fn parse_loop(session: &mut ParserSession) {
        // #if !USE_MUSTTAIL
        loop {
            // #endif // !USE_MUSTTAIL

            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token_eat_trivia_into();

            if tok1.tok != TokenKind::ColonColon {
                trivia1.reset(&mut session.tokenizer);

                // MUSTTAIL
                return session.reduce_and_climb(|ctx| {
                    InfixNode::new(InfixOperator::MessageName, ctx)
                });
            }

            session.push_trivia_seq(trivia1);

            session.push_leaf_and_next(tok1);

            //
            // Special tokenization, so must do parsing here
            //

            let Tok2 =
                Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

            session.push_leaf_and_next(Tok2);

            // #if !USE_MUSTTAIL
        } // loop
          // #else
          //     // MUSTTAIL
          //     return ColonColonParselet_parseLoop(session, ignored, ignored2);
          // #endif // !USE_MUSTTAIL
    }
}

//======================================
// GreaterGreaterParselet
//======================================

impl InfixParselet for GreaterGreaterParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // a>>b
        //

        panic_if_aborted!();


        //
        // Special tokenization, so must do parsing here
        //

        session.push_leaf_and_next(tok_in);

        let token = session.current_token_stringify_as_file_eat_trivia();

        session.push_leaf_and_next(token);

        // MUSTTAIL
        return session
            .reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Put, ctx));
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(Precedence::GREATERGREATER)
    }
}

//======================================
// GreaterGreaterGreaterParselet
//======================================

impl InfixParselet for GreaterGreaterGreaterParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // a>>>b
        //

        panic_if_aborted!();


        //
        // Special tokenization, so must do parsing here
        //

        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_stringify_as_file_eat_trivia();

        session.push_leaf_and_next(tok);

        // MUSTTAIL
        return session.reduce_and_climb(|ctx| {
            BinaryNode::new(BinaryOperator::PutAppend, ctx)
        });
    }

    fn getPrecedence(
        &self,
        _session: &mut ParserSession,
    ) -> Option<Precedence> {
        Some(Precedence::GREATERGREATERGREATER)
    }
}

//======================================
// LessLessParselet
//======================================

impl PrefixParselet for LessLessParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // <<a
        //

        panic_if_aborted!();


        //
        // Special tokenization, so must do parsing here
        //

        tok_in.skip(&mut session.tokenizer);

        let (trivia, tok) = session.current_syntax_token_stringify_as_file();

        tok.skip(&mut session.tokenizer);

        // MUSTTAIL
        return session.push_and_climb(PrefixNode::new2(
            PrefixOperator::Get,
            tok_in,
            trivia,
            tok,
        ));
    }
}

//======================================
// HashParselet
//======================================

impl PrefixParselet for HashParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
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

        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token_with(INSIDE_SLOT);

        if matches!(tok.tok, TokenKind::Integer | TokenKind::String) {
            tok.skip(&mut session.tokenizer);

            return session.push_and_climb(CompoundNode::new2(
                CompoundOperator::Slot,
                tok_in,
                tok,
            ));
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}

//======================================
// HashHashParselet
//======================================

impl PrefixParselet for HashHashParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  ##  or  ##1
        //

        panic_if_aborted!();


        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token_with(INSIDE_SLOTSEQUENCE);

        if tok.tok == TokenKind::Integer {
            tok.skip(&mut session.tokenizer);

            // MUSTTAIl
            return session.push_and_climb(CompoundNode::new2(
                CompoundOperator::SlotSequence,
                tok_in,
                tok,
            ));
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}

//======================================
// PercentParselet
//======================================

impl PrefixParselet for PercentParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  %  or  %1
        //

        panic_if_aborted!();


        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token_with(INSIDE_OUT);

        if tok.tok == TokenKind::Integer {
            tok.skip(&mut session.tokenizer);

            // MUSTTAIl
            return session.push_and_climb(CompoundNode::new2(
                CompoundOperator::Out,
                tok_in,
                tok,
            ));
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}
