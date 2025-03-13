use crate::{
    cst::{PrefixBinaryNode, PrefixNode},
    panic_if_aborted,
    parselet::*,
    parser::ParserSession,
    precedence::*,
    token::{Token, TokenKind, TokenRef},
};

impl IntegralParselet {
    pub(crate) const fn new(Op1: PrefixBinaryOperator, Op2: PrefixOperator) -> Self {
        IntegralParselet { Op1, Op2 }
    }
}

impl PrefixParselet for IntegralParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        IntegralParselet_parsePrefix(session, self, token)
    }
}

fn IntegralParselet_parsePrefix<'i>(
    session: &mut ParserSession<'i>,
    P: ParseletPtr,
    TokIn: TokenRef<'i>,
) {
    //
    // Something like  \[Integral] f \[DifferentialD] x
    //

    panic_if_aborted!();

    session.push_leaf_and_next(TokIn);

    let Ctxt = session.push_context(PRECEDENCE_CLASS_INTEGRATIONOPERATORS);

    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(IntegralParselet_parse1);
    Ctxt.p = Some(P);

    let Tok = session.current_token_eat_trivia();

    if Tok.tok == TokenKind::LongName_DifferentialD
        || Tok.tok == TokenKind::LongName_CapitalDifferentialD
    {
        //
        // \[Integral] \[DifferentialD] x
        //

        session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitOne, Tok));

        return IntegralParselet_parse1(session, P);
    }

    // MUSTTAIL
    return session.parse_prefix(Tok);
}

fn IntegralParselet_parse1(session: &mut ParserSession, P: ParseletPtr) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let tok = session.current_token_eat_trivia_into(&mut Trivia1.borrow_mut());

    if !(tok.tok == TokenKind::LongName_DifferentialD
        || tok.tok == TokenKind::LongName_CapitalDifferentialD)
    {
        Trivia1.borrow_mut().reset(&mut session.tokenizer);

        // MUSTTAIL
        return IntegralParselet_reduceIntegral(session, P);
    }

    session.push_trivia_seq(&mut Trivia1.borrow_mut());

    let Ctxt = session.top_context();
    Ctxt.f = Some(IntegralParselet_reduceIntegrate);
    Ctxt.p = Some(P);

    // MUSTTAIL
    return session.parse_prefix(tok);
}

fn IntegralParselet_reduceIntegrate(session: &mut ParserSession, P: ParseletPtr) {
    let P: &IntegralParselet = P
        .as_any()
        .downcast_ref::<IntegralParselet>()
        .expect("unable to downcast to IntegralParselet");

    session.reduce_and_climb(|ctx| PrefixBinaryNode::new(P.Op1, ctx))
}

fn IntegralParselet_reduceIntegral(session: &mut ParserSession, P: ParseletPtr) {
    let P = P
        .as_any()
        .downcast_ref::<IntegralParselet>()
        .expect("unable to downcast to IntegralParselet");

    session.reduce_and_climb(|ctx| PrefixNode::new(P.Op2, ctx))
}

impl InfixParselet for InfixDifferentialDParselet {
    fn parse_infix(&'static self, _session: &mut ParserSession, _token: TokenRef) {
        panic!("illegal call to InfixDifferentialDParselet::parse_infix()")
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        if session.top_precedence() == PRECEDENCE_CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return PRECEDENCE_LOWEST;
        }

        return PRECEDENCE_FAKE_IMPLICITTIMES;
    }

    fn processImplicitTimes<'i>(
        &self,
        session: &mut ParserSession<'i>,
        TokIn: TokenRef<'i>,
    ) -> TokenRef<'i> {
        if session.top_precedence() == PRECEDENCE_CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return TokIn;
        }

        return Token::error_at_start(TokenKind::Fake_ImplicitTimes, TokIn);
    }
}
