mod integral_parselet;
mod semi_semi_parselet;
mod times_parselet;
mod under_parselet;


use std::any::Any;

use crate::{
    cst::{
        BinaryOperator, CompoundOperator, GroupOperator, InfixOperator,
        PostfixOperator, PrefixBinaryOperator, PrefixOperator, SyntaxErrorKind,
        TernaryOperator,
    },
    panic_if_aborted,
    parse::token_parselets::{under1Parselet, under2Parselet, under3Parselet},
    parse::{ColonLHS, ParseBuilder, ParserSession},
    precedence::Precedence,
    source::*,
    tokenize::{
        token_kind::{Closer, GroupOpenerToCloser, TokenToCloser},
        tokenizer::Tokenizer_currentToken_stringifyAsTag,
        Token, TokenKind, TokenRef,
    },
};

//
/// Classes that derive from Parselet are responsible for parsing specific kinds of syntax
//
// PRECOMMIT: Remove Any?
pub(crate) trait Parselet: std::fmt::Debug {
    // fn as_any(&self) -> &dyn Any;
}

//======================================
// Parselet categories
//======================================

// PRECOMMIT: Reduce visibility?
pub(crate) trait PrefixParselet<'i, B>: Parselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        token: TokenRef<'i>,
    );
}


pub(crate) trait InfixParselet<'i, B: 'i>: Parselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        token: TokenRef<'i>,
    );

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence>;

    fn getOp(&self) -> InfixParseletOperator {
        // TODO: Make this sentinel value unnecessary?
        return InfixParseletOperator::Infix(
            InfixOperator::CodeParser_InternalInvalid,
        );
    }

    /// Should always return either `tok_in` or a new
    /// [`TokenKind::Fake_ImplicitTimes`] token.
    fn process_implicit_times(
        &self,
        _session: &mut ParserSession<'i, B>,
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
                // PRECOMMIT
                // fn as_any(&self) -> &dyn Any {
                //     self
                // }
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
    GP: GroupParselet,
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


#[derive(Debug, Copy, Clone)]
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for LeafParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixErrorParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixCloserParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixToplevelCloserParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixEndOfFileParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixUnsupportedTokenParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixCommaParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // if the input is  f[a@,2]  then return ERROR_EXPECTEDOPERAND (TID:231016/3)
        //
        // if the input is  f[,2]  then ERROR_PREFIXIMPLICITNULL (TID:231016/4)
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixUnhandledParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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
            session.infix_parselet(tok_in.tok).getPrecedence(session);

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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for InfixToplevelNewlineParselet
{
    fn parse_infix(
        &self,
        _session: &mut ParserSession<'i, B>,
        _token: TokenRef<'i>,
    ) {
        assert!(false);
    }

    fn getPrecedence(&self, _: &ParserSession<'i, B>) -> Option<Precedence> {
        //
        // Do not do Implicit Times across top-level newlines
        //
        return None;
    }
}


//======================================
// SymbolParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for SymbolParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like  x  or x_
        //

        panic_if_aborted!();


        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token();

        //
        // if we are here, then we know that Sym could bind to _
        //

        match tok.tok {
            TokenKind::Under => {
                //
                // Something like  a_
                //

                let under = under1Parselet
                    .get_parse_infix_context_sensitive(session, tok);

                // MUSTTAIl
                session.builder.push_compound_pattern_blank(
                    under1Parselet.PBOp,
                    tok_in,
                    under,
                );

                return session.parse_climb();
            },
            TokenKind::UnderUnder => {
                //
                // Something like  a__
                //

                let under = under2Parselet
                    .get_parse_infix_context_sensitive(session, tok);

                session.builder.push_compound_pattern_blank(
                    under2Parselet.PBOp,
                    tok_in,
                    under,
                );

                // MUSTTAIl
                return session.parse_climb();
            },
            TokenKind::UnderUnderUnder => {
                //
                // Something like  a___
                //

                let under = under3Parselet
                    .get_parse_infix_context_sensitive(session, tok);

                session.builder.push_compound_pattern_blank(
                    under3Parselet.PBOp,
                    tok_in,
                    under,
                );

                // MUSTTAIl
                return session.parse_climb();
            },
            TokenKind::UnderDot => {
                //
                // infix
                //
                // Something like  a_.

                tok.skip(&mut session.tokenizer);

                // MUSTTAIl

                session.builder.push_compound_pattern_optional(
                    CompoundOperator::CodeParser_PatternOptionalDefault,
                    tok_in,
                    tok,
                );

                return session.parse_climb();
            },
            _ => (),
        }

        //
        // Something like  a
        //

        // MUSTTAIL
        return session.push_and_climb(tok_in);
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixOperatorParselet
{
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();

        session.push_leaf_and_next(tok_in);

        let ctxt = session.push_context(self.getPrecedence());

        let op = self.Op;
        ctxt.init_callback_with_state(
            move |session: &mut ParserSession<'i, B>| {
                session.reduce_prefix(op);
                session.parse_climb();
            },
        );

        let tok = session.current_token_eat_trivia();

        // MUSTTAIL
        return session.parse_prefix(tok);
    }
}

//======================================
// InfixImplicitTimesParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for InfixImplicitTimesParselet
{
    fn parse_infix(
        &self,
        _session: &mut ParserSession<'i, B>,
        _token: TokenRef<'i>,
    ) {
        assert!(false);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        panic!("The last token may not have been added to InfixParselets");
    }


    fn process_implicit_times(
        &self,
        _session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        return Token::at_start(TokenKind::Fake_ImplicitTimes, tok_in);
    }
}

//======================================
// PrefixAssertFalseParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B>
    for PrefixAssertFalseParselet
{
    // fn getPrecedence(&self, session: &mut ParserSession<'i, B>) -> Option<Precedence> {
    //     None
    // }

    fn parse_prefix(
        &self,
        _session: &mut ParserSession<'i, B>,
        _token: TokenRef<'i>,
    ) {
        assert!(false);
    }
}


//======================================
// InfixAssertFalseParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for InfixAssertFalseParselet
{
    fn parse_infix(
        &self,
        _session: &mut ParserSession<'i, B>,
        _token: TokenRef<'i>,
    ) {
        assert!(false)
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for BinaryOperatorParselet
{
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();

        let op = self.Op;
        ctxt.init_callback_with_state(move |session| {
            session.reduce_binary(op);
            session.parse_climb();
        });

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(self.precedence)
    }

    fn getOp(&self) -> InfixParseletOperator {
        self.Op.into()
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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for InfixOperatorParselet
{
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let tok2 = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        ctxt.init_identity();

        session.parse_prefix(tok2);

        return self.parse_loop(session);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(self.precedence)
    }

    fn getOp(&self) -> InfixParseletOperator {
        self.Op.into()
    }
}

impl InfixOperatorParselet {
    fn parse_loop<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        loop {
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
            // if tok1.tok.infix_parselet().getOp() != self.getOp() {

            let PRECOMMIT = session.infix_parselet(tok1.tok);

            if PRECOMMIT.getOp() != <Self as InfixParselet<'i, B>>::getOp(self)
            {
                //
                // Tok.tok != tok_in.tok, so break
                //

                trivia1.reset(&mut session.tokenizer);

                session.reduce_infix(self.Op);

                // MUSTTAIL
                return session.parse_climb();
            }

            session.push_trivia_seq(trivia1);

            session.push_leaf_and_next(tok1);

            let Tok2 = session.current_token_eat_trivia();

            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

            session.parse_prefix(Tok2);
        } // loop
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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for PostfixOperatorParselet
{
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        session.skip(tok_in);

        session.reduce_postfix(self.Op, tok_in);

        // MUSTTAIL
        return session.parse_climb();
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for GroupParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        session.push_group(GroupOpenerToCloser(tok_in.tok));

        let ctxt = session.push_context(None);
        ctxt.init_identity();

        return self.parse_loop(session);
    }
}

impl GroupParselet {
    fn parse_loop<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        loop {
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

                (PrefixToplevelCloserParselet {}).parse_prefix(session, tok);

                continue;
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

            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

            session.parse_prefix(tok);
        } // loop
    }

    fn reduce_group<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        let op = self.Op;

        session.pop_group();

        session.reduce_group(op);

        session.parse_climb();
    }

    fn reduce_missing_closer<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        let op = self.Op;

        session.pop_group();

        session.reduce_group_missing_closer(op);

        // MUSTTAIL
        return session.try_continue();
    }

    fn reduce_unterminated_group<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        let op = self.Op;

        // The input MUST be valid UTF-8, because we only reduce an *unterminated*
        // group node if we've read an EOF (which is how we know it must be
        // unterminated: we've read all the input).
        let input = std::str::from_utf8(session.input()).expect(
            "cannot reparse unterminated group node: input is not valid UTF-8",
        );

        let tab_width = session.tokenizer.tab_width as usize;

        session.pop_group();

        session.reduce_unterminated_group(op, input, tab_width);

        // MUSTTAIL
        return session.try_continue();
    }
}

//======================================
// CallParselet
//======================================

impl CallParselet {
    pub(crate) const fn new(GP: GroupParselet) -> Self {
        Self { GP }
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for CallParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        //
        // if we used Precedence::CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
        //

        let ctxt = session.top_context();
        ctxt.init_callback(CallParselet::reduce_call);
        ctxt.set_precedence(Precedence::HIGHEST);

        // MUSTTAIL
        return self.GP.parse_prefix(session, tok_in);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::CALL)
    }
}

impl CallParselet {
    fn reduce_call<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_call();

        session.parse_climb();
    }
}

//======================================
// TildeParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for TildeParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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
        ctxt.init_callback(|s| TildeParselet::parse1(s));
        ctxt.set_precedence(None);

        return session.parse_prefix(first_tok);
    }

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        if session.top_non_trivia_node_is_tilde() {
            return None;
        }

        return Some(Precedence::TILDE);
    }
}

impl TildeParselet {
    fn parse1<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        panic_if_aborted!();


        let (trivia1, tok1) = session.current_token_eat_trivia_into();

        if tok1.tok != TokenKind::Tilde {
            //
            // Something like   a ~f b
            //
            // Not structurally correct, so return SyntaxErrorNode
            //

            trivia1.reset(&mut session.tokenizer);

            session.reduce_syntax_error(SyntaxErrorKind::ExpectedTilde);

            // MUSTTAIL
            return session.parse_climb();
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
        ctxt.set_callback(|s| TildeParselet::reduce_tilde(s));
        ctxt.set_precedence(Precedence::TILDE);

        return session.parse_prefix(tok2);
    }

    fn reduce_tilde<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_ternary(TernaryOperator::CodeParser_TernaryTilde);

        session.parse_climb();
    }
}

//======================================
// ColonParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for ColonParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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
                ctxt.init_callback(|session| {
                    session.reduce_binary(BinaryOperator::Pattern);

                    session.parse_climb();
                });
                ctxt.set_precedence(Precedence::FAKE_PATTERNCOLON);

                return session.parse_prefix(tok);
            },
            ColonLHS::Optional => {
                let ctxt = session.top_context();
                ctxt.init_callback(|session| {
                    session.reduce_binary(BinaryOperator::Optional);

                    session.parse_climb();
                });
                ctxt.set_precedence(Precedence::FAKE_OPTIONALCOLON);

                // MUSTTAIl
                return session.parse_prefix(tok);
            },
            ColonLHS::Error => {
                let ctxt = session.top_context();
                ctxt.init_callback(|session| {
                    session
                        .reduce_syntax_error(SyntaxErrorKind::ExpectedSymbol);

                    session.parse_climb();
                });
                ctxt.set_precedence(Precedence::FAKE_PATTERNCOLON);

                // MUSTTAIl
                return session.parse_prefix(tok);
            },
        }
    }

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        if session.check_pattern_precedence() {
            return Some(Precedence::FAKE_OPTIONALCOLON);
        }

        return Some(Precedence::HIGHEST);
    }
}

//======================================
// SlashColonParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for SlashColonParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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
        ctxt.init_callback(|s| SlashColonParselet::parse1(s));

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::SLASHCOLON)
    }
}

impl SlashColonParselet {
    fn parse1<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
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

        session.reduce_syntax_error(SyntaxErrorKind::ExpectedSet);

        // MUSTTAIL
        return session.parse_climb();
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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for EqualParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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
        ctxt.init_callback(|s| EqualParselet::reduce_Set(s));

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        self.op.getPrecedence(session)
    }
}

impl EqualParselet {
    fn parse_infix_tag<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // a /: b = c  and  a /: b = .  are handled here
        //

        panic_if_aborted!();


        session.skip(tok_in);

        let (trivia, tok) = session.current_token_eat_trivia_into();

        if tok.tok == TokenKind::Dot {
            //
            // Something like a = .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //
            // TID:231105/1: Typical TagUnset ("=.")
            // TID:231105/2: TagUnset with interior trivia ("= .")

            session.skip(tok);

            session.reduce_ternary_tag_unset(
                TernaryOperator::TagUnset,
                tok_in,
                trivia,
                tok,
            );

            // MUSTTAIL
            return session.parse_climb();
        }

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
        ctxt.set_callback_with_state(move |session| {
            // TID:231105/3
            session.reduce_ternary_tag_set(
                TernaryOperator::TagSet,
                tok_in,
                trivia,
            );

            session.parse_climb();
        });

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn reduce_Set<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_binary(BinaryOperator::Set);

        session.parse_climb();
    }

    fn reduce_Unset<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_binary(BinaryOperator::Unset);

        session.parse_climb();
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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for ColonEqualParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        ctxt.init_callback(|s| ColonEqualParselet::reduce_SetDelayed(s));

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        self.op.getPrecedence(session)
    }
}


impl ColonEqualParselet {
    fn parse_infix_tag<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.current_token_eat_trivia();

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
        ctxt.set_callback(|s| ColonEqualParselet::reduce_TagSetDelayed(s));

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn reduce_SetDelayed<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_binary(BinaryOperator::SetDelayed);

        session.parse_climb();
    }

    fn reduce_TagSetDelayed<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_ternary(TernaryOperator::TagSetDelayed);

        session.parse_climb();
    }
}

//======================================
// CommaParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for CommaParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let tok2 = session.current_token_eat_trivia();

        if matches!(
            tok2.tok,
            TokenKind::Comma | TokenKind::LongName_InvisibleComma
        ) {
            //
            // Something like  a,,
            //

            session.push_leaf(Token::at_start(
                TokenKind::Error_InfixImplicitNull,
                tok2,
            ));

            let ctxt = session.top_context();
            ctxt.init_identity();

            return CommaParselet::parse_loop(session);
        }

        let ctxt = session.top_context();
        ctxt.init_identity();

        session.parse_prefix(tok2);

        return CommaParselet::parse_loop(session);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::COMMA)
    }
}

impl CommaParselet {
    fn parse_loop<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        loop {
            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token_eat_trivia_into();

            if !matches!(
                tok1.tok,
                TokenKind::Comma | TokenKind::LongName_InvisibleComma
            ) {
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

            if matches!(
                tok2.tok,
                TokenKind::Comma | TokenKind::LongName_InvisibleComma
            ) {
                //
                // Something like  a,,
                //

                session.push_leaf(Token::at_start(
                    TokenKind::Error_InfixImplicitNull,
                    tok2,
                ));

                continue;
            }

            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

            session.parse_prefix(tok2);
        } // loop
    }

    fn reduce_comma<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_infix(InfixOperator::CodeParser_Comma);

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

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for SemiParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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

            let ctxt = session.top_context();
            ctxt.init_identity();

            return SemiParselet::parse_loop(session);
        }

        if tok2.tok.isPossibleBeginning() {
            //
            // Something like  a;+2
            //

            let ctxt = session.top_context();
            ctxt.init_identity();

            session.parse_prefix(tok2);

            return SemiParselet::parse_loop(session);
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
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::SEMI)
    }
}

impl SemiParselet {
    fn parse_loop<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        loop {
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

                continue;
            }

            if tok2.tok.isPossibleBeginning() {
                //
                // Something like  a;b;+2
                //

                let ctxt = session.top_context();
                assert!(ctxt.is_identity());

                session.parse_prefix(tok2);

                continue;
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
        } // loop
    }

    fn reduce_CompoundExpression<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_infix(InfixOperator::CompoundExpression);

        session.parse_climb();
    }
}

//======================================
// ColonColonParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for ColonColonParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::COLONCOLON)
    }
}

impl ColonColonParselet {
    fn parse_loop<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
    ) {
        loop {
            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token_eat_trivia_into();

            if tok1.tok != TokenKind::ColonColon {
                trivia1.reset(&mut session.tokenizer);

                session.reduce_infix(InfixOperator::MessageName);

                // MUSTTAIL
                return session.parse_climb();
            }

            session.push_trivia_seq(trivia1);

            session.push_leaf_and_next(tok1);

            //
            // Special tokenization, so must do parsing here
            //

            let Tok2 =
                Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

            session.push_leaf_and_next(Tok2);
        } // loop
    }
}

//======================================
// GreaterGreaterParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for GreaterGreaterParselet
{
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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

        session.reduce_binary(BinaryOperator::Put);

        // MUSTTAIL
        return session.parse_climb();
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::GREATERGREATER)
    }
}

//======================================
// GreaterGreaterGreaterParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for GreaterGreaterGreaterParselet
{
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
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

        session.reduce_binary(BinaryOperator::PutAppend);

        // MUSTTAIL
        return session.parse_climb();
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::GREATERGREATERGREATER)
    }
}

//======================================
// LessLessParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for LessLessParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

        session.builder.push_prefix_get(
            PrefixOperator::Get,
            tok_in,
            trivia,
            tok,
        );

        // MUSTTAIL
        return session.parse_climb();
    }
}

//======================================
// HashParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for HashParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

            session.builder.push_compound_slot(
                CompoundOperator::Slot,
                tok_in,
                tok,
            );

            return session.parse_climb();
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}

//======================================
// HashHashParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for HashHashParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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
            session.builder.push_compound_slot(
                CompoundOperator::SlotSequence,
                tok_in,
                tok,
            );

            return session.parse_climb();
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}

//======================================
// PercentParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for PercentParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
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

            session.builder.push_compound_out(
                CompoundOperator::Out,
                tok_in,
                tok,
            );

            // MUSTTAIl
            return session.parse_climb();
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}
