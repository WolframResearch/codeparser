use crate::{
    cst::InfixNode,
    panic_if_aborted,
    parselet::*,
    parselet_registration::*,
    parser::ParserSession,
    precedence::*,
    source::TOPLEVEL,
    token::{TokenKind, TokenRef},
    tokenizer::Tokenizer_currentToken,
};


impl InfixParselet for TimesParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        TimesParselet_parseInfix(session, token)
    }

    fn getOp(&self) -> InfixParseletOperator {
        return InfixOperator::Times.into();
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        return PRECEDENCE_STAR;
    }
}

fn TimesParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();

    session.push_leaf_and_next(TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    session.eat_trivia(&mut Tok2);

    // #if !USE_MUSTTAIL
    let Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(Parser_identity);

    session.parse_prefix(Tok2);

    return TimesParselet_parseLoop(session);
    // #else
    //     auto& Ctxt = session.top_context();
    //     assert!(!Ctxt.F);
    //     Ctxt.F = TimesParselet_parseLoop;

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn TimesParselet_parseLoop(session: &mut ParserSession) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia_2(&mut Tok1, &mut Trivia1.borrow_mut());

        let mut I: &dyn InfixParselet = INFIX_PARSELETS[usize::from(Tok1.tok.value())];

        Tok1 = I.processImplicitTimes(session, Tok1);

        if Tok1.tok == TokenKind::Fake_ImplicitTimes {
            //
            // implicit Times should not cross toplevel newlines
            //
            // so reset and try again
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

            session.eat_trivia_but_not_toplevel_newlines_2(&mut Tok1, &mut Trivia1.borrow_mut());

            I = INFIX_PARSELETS[usize::from(Tok1.tok.value())];

            Tok1 = I.processImplicitTimes(session, Tok1);
        }

        I = INFIX_PARSELETS[usize::from(Tok1.tok.value())];

        //
        // Cannot just compare tokens
        //
        // May be something like  a * b c \[Times] d
        //
        // and we want only a single Infix node created
        //
        if I.as_any().type_id() != timesParselet.as_any().type_id() {
            //
            // Tok.tok != TokIn.tok, so break
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return TimesParselet_reduceTimes(session);
        }

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        session.push_leaf_and_next(Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        session.eat_trivia(&mut Tok2);

        // #if !USE_MUSTTAIL
        let Ctxt = session.top_context();
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        session.parse_prefix(Tok2);
    } // while (true)
      // #else
      //     auto& Ctxt = session.top_context(;
      //     assert!(Ctxt.F == TimesParselet_parseLoop);

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn TimesParselet_reduceTimes(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| InfixNode::new(InfixOperator::Times, ctx))
}
