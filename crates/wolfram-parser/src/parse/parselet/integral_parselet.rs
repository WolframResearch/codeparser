use crate::{
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{Token, TokenKind, TokenRef},
};

impl IntegralParselet {
    pub(crate) const fn new(
        Op1: PrefixBinaryOperator,
        Op2: PrefixOperator,
    ) -> Self {
        IntegralParselet { Op1, Op2 }
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for IntegralParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // Something like "\[Integral] f \[DifferentialD] x" (TID:231113/1)
        //

        panic_if_aborted!();

        session.push_leaf_and_next(tok_in);

        let self_ = *self;
        let ctxt = session.push_context(Precedence::CLASS_INTEGRATIONOPERATORS);
        ctxt.init_callback_with_state(move |session| {
            IntegralParselet::parse1(&self_, session)
        });

        let Tok = session.current_token_eat_trivia();

        if Tok.tok == TokenKind::LongName_DifferentialD
            || Tok.tok == TokenKind::LongName_CapitalDifferentialD
        {
            //
            // TID:231113/2: "\[Integral] \[DifferentialD] x"
            //

            session
                .push_leaf(Token::at_start(TokenKind::Fake_ImplicitOne, Tok));

            return IntegralParselet::parse1(self, session);
        }

        // MUSTTAIL
        return session.parse_prefix(Tok);
    }
}

impl IntegralParselet {
    fn parse1<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        panic_if_aborted!();


        let (trivia1, tok) = session.current_token_eat_trivia_into();

        if !(tok.tok == TokenKind::LongName_DifferentialD
            || tok.tok == TokenKind::LongName_CapitalDifferentialD)
        {
            trivia1.reset(&mut session.tokenizer);

            //
            // TID:231113/3: "\[Integral] f"
            //

            // MUSTTAIL
            return IntegralParselet::reduceIntegral(self, session);
        }

        session.push_trivia_seq(trivia1);

        let self_ = *self;
        let ctxt = session.top_context();
        ctxt.set_callback_with_state(move |session| {
            IntegralParselet::reduceIntegrate(&self_, session)
        });

        // MUSTTAIL
        return session.parse_prefix(tok);
    }

    fn reduceIntegrate<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_prefix_binary(self.Op1);

        session.parse_climb();
    }

    fn reduceIntegral<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
    ) {
        session.reduce_prefix(self.Op2);

        session.parse_climb();
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for InfixDifferentialDParselet
{
    fn parse_infix(
        &self,
        _session: &mut ParserSession<'i, B>,
        _token: TokenRef,
    ) {
        panic!("illegal call to InfixDifferentialDParselet::parse_infix()")
    }

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        if session.top_precedence() == Precedence::CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return None;
        }

        return Some(Precedence::FAKE_IMPLICITTIMES);
    }

    fn process_implicit_times(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        if session.top_precedence() == Precedence::CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return tok_in;
        }

        return Token::at_start(TokenKind::Fake_ImplicitTimes, tok_in);
    }
}
