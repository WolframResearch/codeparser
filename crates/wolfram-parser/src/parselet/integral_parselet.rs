use crate::{
    cst::{PrefixBinaryNode, PrefixNode},
    panic_if_aborted,
    parselet::*,
    parser_session::ParserSession,
    precedence::*,
    source::TOPLEVEL,
    token::{Token, TokenKind, TokenRef},
    tokenizer::Tokenizer_currentToken,
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

    let Ctxt = ParserSession::push_context_transparent(
        &mut session.NodeStack,
        &mut session.ContextStack,
        PRECEDENCE_CLASS_INTEGRATIONOPERATORS,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    ParserSession::eat_trivia_transparent(
        &mut session.NodeStack,
        &mut session.tokenizer,
        &mut Tok,
        TOPLEVEL,
    );

    if Tok.tok == TokenKind::LongName_DifferentialD
        || Tok.tok == TokenKind::LongName_CapitalDifferentialD
    {
        //
        // \[Integral] \[DifferentialD] x
        //

        session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitOne, Tok));

        return IntegralParselet_parse1(session, P);
    }

    assert!(Ctxt.f.is_none());
    assert!(Ctxt.p.is_none());
    Ctxt.f = Some(IntegralParselet_parse1);
    Ctxt.p = Some(P);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return P2.parse_prefix(session, Tok);
}

fn IntegralParselet_parse1(session: &mut ParserSession, P: ParseletPtr) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia_2(&mut Tok, TOPLEVEL, &mut Trivia1.borrow_mut());

    if !(Tok.tok == TokenKind::LongName_DifferentialD
        || Tok.tok == TokenKind::LongName_CapitalDifferentialD)
    {
        Trivia1.borrow_mut().reset(&mut session.tokenizer);

        // MUSTTAIL
        return IntegralParselet_reduceIntegral(session, P);
    }

    session.push_trivia_seq(&mut Trivia1.borrow_mut());

    let Ctxt = session.top_context();
    Ctxt.f = Some(IntegralParselet_reduceIntegrate);
    Ctxt.p = Some(P);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return P2.parse_prefix(session, Tok);
}

fn IntegralParselet_reduceIntegrate(session: &mut ParserSession, P: ParseletPtr) {
    let P: &IntegralParselet = P
        .as_any()
        .downcast_ref::<IntegralParselet>()
        .expect("unable to downcast to IntegralParselet");

    let node = PrefixBinaryNode::new(P.Op1, session.pop_context());
    session.push_node(node);

    // MUSTTAIL
    return session.parse_climb();
}

fn IntegralParselet_reduceIntegral(session: &mut ParserSession, P: ParseletPtr) {
    let P = P
        .as_any()
        .downcast_ref::<IntegralParselet>()
        .expect("unable to downcast to IntegralParselet");

    let node = PrefixNode::new(P.Op2, session.pop_context());
    session.push_node(node);

    // MUSTTAIL
    return session.parse_climb();
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
