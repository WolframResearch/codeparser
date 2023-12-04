mod integral_parselet;
mod semi_semi_parselet;
mod times_parselet;
mod under_parselet;


use crate::{
    panic_if_aborted,
    parse::{
        operators::{
            BinaryOperator, CompoundOperator, GroupOperator, InfixOperator,
            PostfixOperator, PrefixBinaryOperator, PrefixOperator,
            TernaryOperator,
        },
        token_parselets::{under1Parselet, under2Parselet, under3Parselet},
        ColonLHS, ParseBuilder, ParserSession, SyntaxErrorData,
    },
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
pub(crate) trait Parselet: std::fmt::Debug {}

//======================================
// Parselet categories
//======================================

pub(crate) trait PrefixParselet<'i, B: ParseBuilder<'i> + 'i>:
    Parselet
{
    #[must_use]
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        token: TokenRef<'i>,
    ) -> B::Node;
}


pub(crate) trait InfixParselet<'i, B: ParseBuilder<'i> + 'i>:
    Parselet
{
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        // The previous node that was just parsed.
        node: B::Node,
        // The trivia between the previous node and infix `token`.
        trivia1: B::TriviaHandle,
        token: TokenRef<'i>,
    ) -> B::Node;

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
            impl Parselet for $name {}
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
    ) -> B::Node {
        let node = session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return session.parse_climb(node);
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
    ) -> B::Node {
        assert!(tok_in.tok.isError());

        let node = session.push_leaf_and_next(tok_in);

        return node;
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
    ) -> B::Node {
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

        let node = session.push_leaf(Token::at_start(kind, tok_in));

        //
        // Do not take the closer.
        // Delay taking the closer until necessary. This allows  { 1 + }  to be parsed as a GroupNode
        //
        // TODO(cleanup): This call does nothing? Add test and remove.
        let _ = session.tokenizer.peek_token();

        return node;
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
    ) -> B::Node {
        assert!(tok_in.tok.isCloser());

        panic_if_aborted!();


        //
        // if we are at the top, then make sure to take the token and report it
        //

        let node = session
            .push_leaf(Token::at(TokenKind::Error_UnexpectedCloser, tok_in));

        tok_in.skip(&mut session.tokenizer);

        return node;
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
    ) -> B::Node {
        //
        // Something like  a+<EOF>
        //

        panic_if_aborted!();


        let kind = if session.top_precedence() == Precedence::COMMA {
            TokenKind::Error_InfixImplicitNull
        } else {
            TokenKind::Error_ExpectedOperand
        };

        let node = session.push_leaf(Token::at_start(kind, tok_in));

        return node;
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
    ) -> B::Node {
        panic_if_aborted!();


        let node = session
            .push_leaf(Token::at(TokenKind::Error_UnsupportedToken, tok_in));

        tok_in.skip(&mut session.tokenizer);

        return node;
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
    ) -> B::Node {
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

        let node = session.push_leaf(Token::at_start(kind, tok_in));

        // MUSTTAIL
        return session.parse_climb(node);
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
    ) -> B::Node {
        assert!(!tok_in.tok.isPossibleBeginning(), "handle at call site");

        panic_if_aborted!();


        let node = session.push_leaf(Token::at_start(
            TokenKind::Error_ExpectedOperand,
            tok_in,
        ));

        //
        // Do not take next token
        //
        // TODO(cleanup): This call does nothing? Add test and remove.
        let _ = session.tokenizer.peek_token();

        let TokenPrecedence = B::with_infix_parselet(tok_in.tok, |parselet| {
            parselet.getPrecedence(session)
        });

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

            return node;
        }

        //
        // Handle something like  f[@2]
        //
        // We want to make EXPECTEDOPERAND the first arg of the Operator node.
        //

        session.push_context(TokenPrecedence);

        // MUSTTAIL
        return session.parse_infix(node, B::empty_trivia(), tok_in);
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
        _node: B::Node,
        _trivia: B::TriviaHandle,
        _token: TokenRef<'i>,
    ) -> B::Node {
        panic!("invalid infix toplevel newline parselet")
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
    ) -> B::Node {
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
                let node = session.builder.push_compound_pattern_blank(
                    under1Parselet.PBOp,
                    tok_in,
                    under,
                );

                return session.parse_climb(node);
            },
            TokenKind::UnderUnder => {
                //
                // Something like  a__
                //

                let under = under2Parselet
                    .get_parse_infix_context_sensitive(session, tok);

                let node = session.builder.push_compound_pattern_blank(
                    under2Parselet.PBOp,
                    tok_in,
                    under,
                );

                // MUSTTAIl
                return session.parse_climb(node);
            },
            TokenKind::UnderUnderUnder => {
                //
                // Something like  a___
                //

                let under = under3Parselet
                    .get_parse_infix_context_sensitive(session, tok);

                let node = session.builder.push_compound_pattern_blank(
                    under3Parselet.PBOp,
                    tok_in,
                    under,
                );

                // MUSTTAIl
                return session.parse_climb(node);
            },
            TokenKind::UnderDot => {
                //
                // infix
                //
                // Something like  a_.

                tok.skip(&mut session.tokenizer);

                // MUSTTAIl

                let node = session.builder.push_compound_pattern_optional(
                    CompoundOperator::CodeParser_PatternOptionalDefault,
                    tok_in,
                    tok,
                );

                return session.parse_climb(node);
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
    ) -> B::Node {
        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let _ = session.push_context(self.getPrecedence());

        let (trivia, tok) = session.current_token_eat_trivia();


        let operand = session.parse_prefix(tok);

        let node = session.reduce_prefix(self.Op, tok_in, trivia, operand);

        return session.parse_climb(node);
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
        _node: B::Node,
        _trivia: B::TriviaHandle,
        _token: TokenRef<'i>,
    ) -> B::Node {
        panic!("invalid infix implicit times parselet")
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
    ) -> B::Node {
        panic!("invalid prefix parselet")
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
        _node: B::Node,
        _trivia: B::TriviaHandle,
        _token: TokenRef<'i>,
    ) -> B::Node {
        panic!("unexpected infix assert false parselet")
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, tok) = session.current_token_eat_trivia();

        // MUSTTAIL
        let rhs_node = session.parse_prefix(tok);

        let node = session.reduce_binary(
            self.Op, lhs_node, trivia1, tok_in, trivia2, rhs_node,
        );

        return session.parse_climb(node);
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
        first_operand: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let mut infix_state = session.begin_infix(self.Op, first_operand);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let (trivia2, tok2) = session.current_token_eat_trivia();

        let second_operand = session.parse_prefix(tok2);

        session.builder.infix_add(
            &mut infix_state,
            trivia1,
            tok_in,
            trivia2,
            second_operand,
        );

        return self.parse_loop(session, infix_state);
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
        mut infix_state: B::InfixParseState,
    ) -> B::Node {
        loop {
            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token();

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

            let tok1_op =
                B::with_infix_parselet(tok1.tok, |parselet| parselet.getOp());

            if tok1_op != <Self as InfixParselet<'i, B>>::getOp(self) {
                //
                // Tok.tok != tok_in.tok, so break
                //

                session.trivia_reset(trivia1);

                let node = session.reduce_infix(infix_state);

                // MUSTTAIL
                return session.parse_climb(node);
            }

            let (trivia1, tok1) = session.commit_syntax_and_next(trivia1, tok1);

            let (trivia2, tok2) = session.current_token_eat_trivia();

            let operand = session.parse_prefix(tok2);

            session.builder.infix_add(
                &mut infix_state,
                trivia1,
                tok1,
                trivia2,
                operand,
            );
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
        finished: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        let tok_in = session.push_syntax_and_next(tok_in);

        let node = session.reduce_postfix(self.Op, finished, trivia1, tok_in);

        // MUSTTAIL
        return session.parse_climb(node);
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
    ) -> B::Node {
        panic_if_aborted!();

        session.push_group(GroupOpenerToCloser(tok_in.tok));


        //------------------------------
        // Parse loop
        //------------------------------

        let tok_in = session.push_syntax_and_next(tok_in);

        let mut group_state = session.builder.begin_group(tok_in);

        let _ = session.push_context(None);

        loop {
            panic_if_aborted!();


            //
            // There will only be 1 "good" node (either a LeafNode or a CommaNode)
            // But there might be multiple error nodes
            //
            // ADDENDUM: Actually, there may be more than 1 "good" node
            // e.g. {1\\2}
            //

            let (trivia1, tok) = session.current_token();

            if TokenToCloser(tok.tok) == self.closer {
                //
                // Everything is good
                //

                let (trivia1, tok) =
                    session.commit_syntax_and_next(trivia1, tok);

                let node =
                    session.reduce_group(self.Op, group_state, trivia1, tok);

                // MUSTTAIL
                return session.parse_climb(node);
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

                    session.trivia_reset(trivia1);

                    let node = session
                        .reduce_group_missing_closer(self.Op, group_state);

                    return node;
                }

                //
                // Something like  { ) }
                //                   ^
                //

                let trivia1 = session.builder.push_trivia_seq(trivia1);

                let node = (PrefixToplevelCloserParselet {})
                    .parse_prefix(session, tok);

                session.builder.group_add(&mut group_state, trivia1, node);

                continue;
            }

            let trivia1 = session.builder.push_trivia_seq(trivia1);

            if tok.tok == TokenKind::EndOfFile {
                //
                // Handle something like   { a EOF
                //

                // MUSTTAIL
                let node = session.reduce_unterminated_group(
                    self.Op,
                    group_state,
                    trivia1,
                );

                return node;
            }

            //
            // Handle the expression
            //

            let node = session.parse_prefix(tok);

            session.builder.group_add(&mut group_state, trivia1, node);
        } // loop
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
        head: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();


        //
        // if we used Precedence::CALL here, then e.g., a[]?b should technically parse as   a <call> []?b
        //

        let ctxt = session.top_context();
        ctxt.set_precedence(Precedence::HIGHEST);

        let group_node = self.GP.parse_prefix(session, tok_in);

        let node = session.reduce_call(head, trivia1, group_node);

        return session.parse_climb(node);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::CALL)
    }
}

//======================================
// TildeParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for TildeParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // Something like  a ~f~ b
        //                   ^
        //
        // It'd be weird if this were an "infix operator"
        //

        panic_if_aborted!();

        debug_assert_eq!(tok_in.tok, TokenKind::Tilde);

        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, first_tok) = session.current_token_eat_trivia();

        let middle_node = session.parse_prefix(first_tok);

        let (trivia3, tok1) = session.current_token();

        if tok1.tok != TokenKind::Tilde {
            //
            // Something like   a ~f b
            //
            // Not structurally correct, so return SyntaxErrorNode
            //

            session.trivia_reset(trivia3);

            let node =
                session.reduce_syntax_error(SyntaxErrorData::ExpectedTilde {
                    lhs_node,
                    trivia1,
                    first_op_token: tok_in,
                    trivia2,
                    middle_node,
                });

            // MUSTTAIL
            return session.parse_climb(node);
        }

        let (trivia3, tok1) = session.commit_syntax_and_next(trivia3, tok1);

        let (trivia4, tok2) = session.current_token_eat_trivia();

        let rhs_node = session.parse_prefix(tok2);

        let node = session.reduce_ternary(
            TernaryOperator::CodeParser_TernaryTilde,
            lhs_node,
            trivia1,
            tok_in,
            trivia2,
            middle_node,
            trivia3,
            tok1,
            trivia4,
            rhs_node,
        );

        return session.parse_climb(node);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        return Some(Precedence::TILDE);
    }
}

//======================================
// ColonParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for ColonParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // Something like  symbol:object  or  pattern:optional
        //

        panic_if_aborted!();


        let colonLHS = session.check_colon_lhs(&lhs_node);

        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, tok) = session.current_token_eat_trivia();

        match colonLHS {
            ColonLHS::Pattern => {
                session
                    .top_context()
                    .set_precedence(Precedence::FAKE_PATTERNCOLON);

                let rhs_node = session.parse_prefix(tok);

                let node = session.reduce_binary(
                    BinaryOperator::Pattern,
                    lhs_node,
                    trivia1,
                    tok_in,
                    trivia2,
                    rhs_node,
                );

                return session.parse_climb(node);
            },
            ColonLHS::Optional => {
                session
                    .top_context()
                    .set_precedence(Precedence::FAKE_OPTIONALCOLON);

                let rhs_node = session.parse_prefix(tok);

                let node = session.reduce_binary(
                    BinaryOperator::Optional,
                    lhs_node,
                    trivia1,
                    tok_in,
                    trivia2,
                    rhs_node,
                );

                return session.parse_climb(node);
            },
            ColonLHS::Error => {
                session
                    .top_context()
                    .set_precedence(Precedence::FAKE_PATTERNCOLON);

                let rhs_node = session.parse_prefix(tok);

                let node = session.reduce_syntax_error(
                    SyntaxErrorData::ExpectedSymbol {
                        lhs_node,
                        trivia1,
                        tok_in,
                        trivia2,
                        rhs_node,
                    },
                );

                return session.parse_climb(node);
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
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


        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, tok) = session.current_token_eat_trivia();

        let middle_node = session.parse_prefix(tok);

        let (trivia3, tok) = session.current_token();

        match tok.tok {
            TokenKind::Equal => {
                session.set_precedence(Precedence::EQUAL);

                let trivia3 = session.builder.push_trivia_seq(trivia3);

                let tok = session.push_syntax_and_next(tok);

                // MUSTTAIl
                return EqualParselet::parse_infix_tag(
                    session,
                    lhs_node,
                    trivia1,
                    tok_in,
                    trivia2,
                    middle_node,
                    trivia3,
                    tok,
                );
            },
            TokenKind::ColonEqual => {
                session.set_precedence(Precedence::COLONEQUAL);

                let trivia3 = session.builder.push_trivia_seq(trivia3);

                let tok = session.push_syntax_and_next(tok);

                // MUSTTAIl
                return ColonEqualParselet::parse_infix_tag(
                    session,
                    lhs_node,
                    trivia1,
                    tok_in,
                    trivia2,
                    middle_node,
                    trivia3,
                    tok,
                );
            },
            _ => (),
        } // switch

        session.trivia_reset(trivia3);

        //
        // Anything other than:
        // a /: b = c
        // a /: b := c
        // a /: b =.
        //

        let node = session.reduce_syntax_error(SyntaxErrorData::ExpectedSet);

        // MUSTTAIL
        return session.parse_climb(node);
    }

    fn getPrecedence(
        &self,
        _session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        Some(Precedence::SLASHCOLON)
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();


        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, tok) = session.current_token_eat_trivia();

        if tok.tok == TokenKind::Dot {
            //
            // Something like a = .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //

            let tok = session.push_syntax_and_next(tok);

            let node = session.reduce_binary_unset(
                BinaryOperator::Unset,
                lhs_node,
                trivia1,
                tok_in,
                trivia2,
                tok,
            );

            // MUSTTAIL
            return session.parse_climb(node);
        }

        let rhs_node = session.parse_prefix(tok);

        let node = session.reduce_binary(
            BinaryOperator::Set,
            lhs_node,
            trivia1,
            tok_in,
            trivia2,
            rhs_node,
        );

        return session.parse_climb(node);
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        first_op_token: B::SyntaxTokenNode,
        trivia2: B::TriviaHandle,
        middle_node: B::Node,
        trivia3: B::TriviaHandle,
        second_op_token: B::SyntaxTokenNode,
    ) -> B::Node {
        //
        // a /: b = c  and  a /: b = .  are handled here
        //

        panic_if_aborted!();

        // debug_assert!(tok_in.tok == TokenKind::Equal);

        let (trivia4, tok) = session.current_token_eat_trivia();

        if tok.tok == TokenKind::Dot {
            //
            // Something like a = .
            //
            // tutorial/OperatorInputForms
            // Spaces to Avoid
            //
            // TID:231105/1: Typical TagUnset ("=.")
            // TID:231105/2: TagUnset with interior trivia ("= .")

            let tok = session.push_syntax_and_next(tok);

            let node = session.reduce_ternary_tag_unset(
                TernaryOperator::TagUnset,
                lhs_node,
                trivia1,
                first_op_token,
                trivia2,
                middle_node,
                trivia3,
                second_op_token,
                trivia4,
                tok,
            );

            // MUSTTAIL
            return session.parse_climb(node);
        }

        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SlashColonParselet_parse1 as usize);
        let rhs_node = session.parse_prefix(tok);

        // TID:231105/3
        let node = session.reduce_ternary(
            TernaryOperator::TagSet,
            lhs_node,
            trivia1,
            first_op_token,
            trivia2,
            middle_node,
            trivia3,
            second_op_token,
            trivia4,
            rhs_node,
        );

        return session.parse_climb(node);
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, tok) = session.current_token_eat_trivia();

        let rhs_node = session.parse_prefix(tok);

        let node = session.reduce_binary(
            BinaryOperator::SetDelayed,
            lhs_node,
            trivia1,
            tok_in,
            trivia2,
            rhs_node,
        );

        return session.parse_climb(node);
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        first_op_token: B::SyntaxTokenNode,
        trivia2: B::TriviaHandle,
        middle_node: B::Node,
        trivia3: B::TriviaHandle,
        second_op_token: B::SyntaxTokenNode,
    ) -> B::Node {
        panic_if_aborted!();

        // debug_assert!(tok_in.tok == TokenKind::ColonEqual);

        let (trivia4, tok) = session.current_token_eat_trivia();

        let rhs_node = session.parse_prefix(tok);

        let node = session.reduce_ternary(
            TernaryOperator::TagSetDelayed,
            lhs_node,
            trivia1,
            first_op_token,
            trivia2,
            middle_node,
            trivia3,
            second_op_token,
            trivia4,
            rhs_node,
        );

        return session.parse_climb(node);
    }
}

//======================================
// CommaParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for CommaParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        first_operand: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        debug_assert_eq!(tok_in.tok, TokenKind::Comma);

        let tok_in = session.push_syntax_and_next(tok_in);

        let mut infix_state =
            session.begin_infix(InfixOperator::CodeParser_Comma, first_operand);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let (trivia2, tok2) = session.current_token_eat_trivia();

        let second_operand = if matches!(
            tok2.tok,
            TokenKind::Comma | TokenKind::LongName_InvisibleComma
        ) {
            //
            // Something like  a,,
            //

            session.push_leaf(Token::at_start(
                TokenKind::Error_InfixImplicitNull,
                tok2,
            ))
        } else {
            session.parse_prefix(tok2)
        };

        session.builder.infix_add(
            &mut infix_state,
            trivia1,
            tok_in,
            trivia2,
            second_operand,
        );

        //
        // Start the loop
        //

        return CommaParselet::parse_loop(session, infix_state);
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
        mut infix_state: B::InfixParseState,
    ) -> B::Node {
        loop {
            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token();

            if !matches!(
                tok1.tok,
                TokenKind::Comma | TokenKind::LongName_InvisibleComma
            ) {
                session.trivia_reset(trivia1);

                let node = session.reduce_infix(infix_state);

                // MUSTTAIL
                return CommaParselet::reduce_comma(session, node);
            }

            //
            // Something like  a,b
            //

            let (trivia1, tok1) = session.commit_syntax_and_next(trivia1, tok1);

            let (trivia2, tok2) = session.current_token_eat_trivia();

            let operand = if matches!(
                tok2.tok,
                TokenKind::Comma | TokenKind::LongName_InvisibleComma
            ) {
                //
                // Something like  a,,
                //

                session.push_leaf(Token::at_start(
                    TokenKind::Error_InfixImplicitNull,
                    tok2,
                ))
            } else {
                session.parse_prefix(tok2)
            };

            session.builder.infix_add(
                &mut infix_state,
                trivia1,
                tok1,
                trivia2,
                operand,
            )
        } // loop
    }

    fn reduce_comma<'i, B: ParseBuilder<'i> + 'i>(
        _session: &mut ParserSession<'i, B>,
        node: B::Node,
    ) -> B::Node {
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

        return node;
    }
}

//======================================
// SemiParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for SemiParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        first_operand: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let mut infix_state = session
            .begin_infix(InfixOperator::CompoundExpression, first_operand);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        //
        // CompoundExpression should not cross toplevel newlines
        //
        let (trivia2, tok2) =
            session.current_token_eat_trivia_but_not_toplevel_newlines();

        if tok2.tok == TokenKind::Semi {
            //
            // Something like  a; ;
            //

            let second_operand = session
                .push_leaf(Token::at_start(TokenKind::Fake_ImplicitNull, tok2));

            //
            // nextToken() is not needed after an implicit token
            //

            session.builder.infix_add(
                &mut infix_state,
                trivia1,
                tok_in,
                trivia2,
                second_operand,
            );

            return SemiParselet::parse_loop(session, infix_state);
        }

        if tok2.tok.isPossibleBeginning() {
            //
            // Something like  a;+2
            //

            let second_operand = session.parse_prefix(tok2);

            session.builder.infix_add(
                &mut infix_state,
                trivia1,
                tok_in,
                trivia2,
                second_operand,
            );

            return SemiParselet::parse_loop(session, infix_state);
        }

        //
        // Not beginning of an expression
        //
        // For example:  a;&
        //

        let second_operand = session
            .push_leaf(Token::at_start(TokenKind::Fake_ImplicitNull, tok2));

        session.builder.infix_add(
            &mut infix_state,
            trivia1,
            tok_in,
            trivia2,
            second_operand,
        );

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiParselet::reduce_CompoundExpression(session, infix_state);
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
        mut infix_state: B::InfixParseState,
    ) -> B::Node {
        loop {
            panic_if_aborted!();


            let (trivia1, tok1) = session.current_token();

            if tok1.tok != TokenKind::Semi {
                //
                // Something like  a;b
                //

                session.trivia_reset(trivia1);

                // MUSTTAIL
                return SemiParselet::reduce_CompoundExpression(
                    session,
                    infix_state,
                );
            }

            //
            // Something like  a;b
            //

            let (trivia1, tok1) = session.commit_syntax_and_next(trivia1, tok1);

            //
            // CompoundExpression should not cross toplevel newlines
            //
            let (trivia2, tok2) =
                session.current_token_eat_trivia_but_not_toplevel_newlines();

            if tok2.tok == TokenKind::Semi {
                //
                // Something like  a;b; ;
                //

                let operand = session.push_leaf(Token::at_start(
                    TokenKind::Fake_ImplicitNull,
                    tok2,
                ));

                session.builder.infix_add(
                    &mut infix_state,
                    trivia1,
                    tok1,
                    trivia2,
                    operand,
                );

                //
                // nextToken() is not needed after an implicit token
                //

                continue;
            }

            if tok2.tok.isPossibleBeginning() {
                //
                // Something like  a;b;+2
                //

                let operand = session.parse_prefix(tok2);

                session.builder.infix_add(
                    &mut infix_state,
                    trivia1,
                    tok1,
                    trivia2,
                    operand,
                );

                continue;
            }

            //
            // Not beginning of an expression
            //
            // For example:  a;b;&
            //

            let operand = session
                .push_leaf(Token::at_start(TokenKind::Fake_ImplicitNull, tok2));

            session.builder.infix_add(
                &mut infix_state,
                trivia1,
                tok1,
                trivia2,
                operand,
            );

            //
            // nextToken() is not needed after an implicit token
            //

            // MUSTTAIL
            return SemiParselet::reduce_CompoundExpression(
                session,
                infix_state,
            );
        } // loop
    }

    fn reduce_CompoundExpression<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        infix_state: B::InfixParseState,
    ) -> B::Node {
        let node = session.reduce_infix(infix_state);

        return session.parse_climb(node);
    }
}

//======================================
// ColonColonParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for ColonColonParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        head_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // a::b
        //

        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let mut infix_state =
            session.begin_infix(InfixOperator::MessageName, head_node);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //
        //
        // Special tokenization, so must do parsing here
        //

        let Tok2 =
            Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

        let second_operand = session.push_leaf_and_next(Tok2);

        session.builder.infix_add(
            &mut infix_state,
            trivia1,
            tok_in,
            // TODO: Document, stringify as tag doesn't allow leading whitespace
            //       E.g. "a:: b" is an error.
            B::empty_trivia(),
            second_operand,
        );

        // MUSTTAIL
        return ColonColonParselet::parse_loop(session, infix_state);
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
        mut infix_state: B::InfixParseState,
    ) -> B::Node {
        loop {
            panic_if_aborted!();

            let (trivia1, tok1) = session.current_token();

            if tok1.tok != TokenKind::ColonColon {
                session.trivia_reset(trivia1);

                let node = session.reduce_infix(infix_state);

                // MUSTTAIL
                return session.parse_climb(node);
            }

            let (trivia1, tok1) = session.commit_syntax_and_next(trivia1, tok1);

            //
            // Special tokenization, so must do parsing here
            //

            let Tok2 =
                Tokenizer_currentToken_stringifyAsTag(&mut session.tokenizer);

            let operand = session.push_leaf_and_next(Tok2);

            session.builder.infix_add(
                &mut infix_state,
                trivia1,
                tok1,
                B::empty_trivia(),
                operand,
            );
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // a>>b
        //

        panic_if_aborted!();


        //
        // Special tokenization, so must do parsing here
        //

        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, token) = session.current_syntax_token_stringify_as_file();

        let rhs_node = session.push_leaf_and_next(token);

        let node = session.reduce_binary(
            BinaryOperator::Put,
            lhs_node,
            trivia1,
            tok_in,
            trivia2,
            rhs_node,
        );

        // MUSTTAIL
        return session.parse_climb(node);
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
        lhs_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // a>>>b
        //

        panic_if_aborted!();


        //
        // Special tokenization, so must do parsing here
        //

        let tok_in = session.push_syntax_and_next(tok_in);

        let (trivia2, tok) = session.current_syntax_token_stringify_as_file();

        let rhs_node = session.push_leaf_and_next(tok);

        let node = session.reduce_binary(
            BinaryOperator::PutAppend,
            lhs_node,
            trivia1,
            tok_in,
            trivia2,
            rhs_node,
        );

        // MUSTTAIL
        return session.parse_climb(node);
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
    ) -> B::Node {
        //
        // <<a
        //

        panic_if_aborted!();


        //
        // Special tokenization, so must do parsing here
        //

        let tok_in = session.push_syntax_and_next(tok_in);

        session.push_context(Precedence::HIGHEST);

        let (trivia, tok) = session.current_syntax_token_stringify_as_file();

        // Special operand token, not processed into a Node in the normal way
        // since this is supposed to be a String token.
        let _ = session.push_leaf_and_next(tok);

        let node =
            session.reduce_prefix_get(PrefixOperator::Get, tok_in, trivia, tok);

        // MUSTTAIL
        return session.parse_climb(node);
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
    ) -> B::Node {
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

            let node = session.builder.push_compound_slot(
                CompoundOperator::Slot,
                tok_in,
                tok,
            );

            return session.parse_climb(node);
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
    ) -> B::Node {
        //
        // Something like  ##  or  ##1
        //

        panic_if_aborted!();


        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token_with(INSIDE_SLOTSEQUENCE);

        if tok.tok == TokenKind::Integer {
            tok.skip(&mut session.tokenizer);

            // MUSTTAIl
            let node = session.builder.push_compound_slot(
                CompoundOperator::SlotSequence,
                tok_in,
                tok,
            );

            return session.parse_climb(node);
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
    ) -> B::Node {
        //
        // Something like  %  or  %1
        //

        panic_if_aborted!();


        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token_with(INSIDE_OUT);

        if tok.tok == TokenKind::Integer {
            tok.skip(&mut session.tokenizer);

            let node = session.builder.push_compound_out(
                CompoundOperator::Out,
                tok_in,
                tok,
            );

            // MUSTTAIl
            return session.parse_climb(node);
        }

        // MUSTTAIL
        return session.push_and_climb(tok_in);
    }
}
