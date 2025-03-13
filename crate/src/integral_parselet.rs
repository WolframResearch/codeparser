use crate::{
    node::{PrefixBinaryNode, PrefixNode},
    panic_if_aborted,
    parselet::*,
    parser::{
        Parser_eatTrivia_2, Parser_eatTrivia_transparent, Parser_parseClimb, Parser_popContext,
        Parser_pushContext_transparent, Parser_pushLeaf, Parser_pushLeafAndNext, Parser_pushNode,
        Parser_pushTriviaSeq, Parser_topContext, Parser_topPrecedence,
    },
    parser_session::ParserSession,
    precedence::*,
    source::{Source, TOPLEVEL},
    symbol::Symbol,
    token::Token,
    token_enum_registration::TokenEnum::*,
    tokenizer::Tokenizer_currentToken,
};

impl IntegralParselet {
    pub(crate) const fn new(Op1: Symbol, Op2: Symbol) -> Self {
        IntegralParselet { Op1, Op2 }
    }
}

impl PrefixParselet for IntegralParselet {
    fn parse_prefix(&'static self, session: &mut ParserSession, token: Token) {
        IntegralParselet_parsePrefix(session, self, token)
    }
}

fn IntegralParselet_parsePrefix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    //
    // Something like  \[Integral] f \[DifferentialD] x
    //

    panic_if_aborted!();

    Parser_pushLeafAndNext(session, TokIn);

    let Ctxt = Parser_pushContext_transparent(
        &mut session.NodeStack,
        &mut session.ContextStack,
        PRECEDENCE_CLASS_INTEGRATIONOPERATORS,
    );

    let mut Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia_transparent(
        &mut session.NodeStack,
        &mut session.tokenizer,
        &mut Tok,
        TOPLEVEL,
    );

    if Tok.tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD {
        //
        // \[Integral] \[DifferentialD] x
        //

        Parser_pushLeaf(
            session,
            Token::new2(
                TOKEN_FAKE_IMPLICITONE,
                Tok.span,
                Source::from_location(Tok.src.start),
            ),
        );

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

    Parser_eatTrivia_2(session, &mut Tok, TOPLEVEL, &mut Trivia1.borrow_mut());

    if !(Tok.tok == TOKEN_LONGNAME_DIFFERENTIALD || Tok.tok == TOKEN_LONGNAME_CAPITALDIFFERENTIALD)
    {
        Trivia1.borrow_mut().reset(&mut session.tokenizer);

        // MUSTTAIL
        return IntegralParselet_reduceIntegral(session, P);
    }

    Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

    let Ctxt = Parser_topContext(session);
    Ctxt.f = Some(IntegralParselet_reduceIntegrate);
    Ctxt.p = Some(P);

    let P2 = prefix_parselet(Tok.tok);

    // MUSTTAIL
    return P2.parse_prefix(session, Tok);
}

fn IntegralParselet_reduceIntegrate(session: &mut ParserSession, P: ParseletPtr) {
    let P = P
        .as_any()
        .downcast_ref::<IntegralParselet>()
        .expect("unable to downcast to IntegralParselet");

    let Op1 = P.Op1;

    let node = PrefixBinaryNode::new(Op1, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session);
}

fn IntegralParselet_reduceIntegral(session: &mut ParserSession, P: ParseletPtr) {
    let P = P
        .as_any()
        .downcast_ref::<IntegralParselet>()
        .expect("unable to downcast to IntegralParselet");

    let Op2 = P.Op2;

    let node = PrefixNode::new(Op2, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session);
}

impl InfixParselet for InfixDifferentialDParselet {
    fn parse_infix(&'static self, _session: &mut ParserSession, _token: Token) {
        panic!("illegal call to InfixDifferentialDParselet::parse_infix()")
    }

    fn getPrecedence(&self, session: &mut ParserSession) -> Precedence {
        if Parser_topPrecedence(session) == PRECEDENCE_CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return PRECEDENCE_LOWEST;
        }

        return PRECEDENCE_FAKE_IMPLICITTIMES;
    }

    fn processImplicitTimes(&self, session: &mut ParserSession, TokIn: Token) -> Token {
        if Parser_topPrecedence(session) == PRECEDENCE_CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return TokIn;
        }

        return Token::new2(
            TOKEN_FAKE_IMPLICITTIMES,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        );
    }
}
