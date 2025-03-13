use crate::{
    cst::InfixNode,
    panic_if_aborted,
    parselet::*,
    parselet_registration::*,
    parser::ParserSession,
    precedence::*,
    token::{TokenKind, TokenRef},
};


impl InfixParselet for TimesParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, tok_in: TokenRef<'i>) {
        panic_if_aborted!();

        session.push_leaf_and_next(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let tok2 = session.current_token_eat_trivia();

        // #if !USE_MUSTTAIL
        let ctxt = session.top_context();
        ctxt.init_callback(Parser_identity, None);

        session.parse_prefix(tok2);

        return TimesParselet::parse_loop(session);
        // #else
        //     auto& Ctxt = session.top_context();
        //     assert!(!Ctxt.F);
        //     Ctxt.F = TimesParselet_parseLoop;

        //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

        //     // MUSTTAIL
        //     return P2.parse_prefix(session, Tok2);
        // #endif // !USE_MUSTTAIL
    }

    fn getOp(&self) -> InfixParseletOperator {
        return InfixOperator::Times.into();
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        return PRECEDENCE_STAR;
    }
}

impl TimesParselet {
    fn parse_loop(session: &mut ParserSession) {
        // #if !USE_MUSTTAIL
        loop {
            // #endif // !USE_MUSTTAIL

            panic_if_aborted!();


            let Trivia1 = session.trivia1.clone();

            let mut tok1 = session.current_token_eat_trivia_into(&mut Trivia1.borrow_mut());

            let mut I: &dyn InfixParselet = INFIX_PARSELETS[usize::from(tok1.tok.value())];

            tok1 = I.processImplicitTimes(session, tok1);

            if tok1.tok == TokenKind::Fake_ImplicitTimes {
                //
                // implicit Times should not cross toplevel newlines
                //
                // so reset and try again
                //

                Trivia1.borrow_mut().reset(&mut session.tokenizer);

                tok1 = session.current_token_eat_trivia_but_not_toplevel_newlines_into(
                    &mut Trivia1.borrow_mut(),
                );

                I = INFIX_PARSELETS[usize::from(tok1.tok.value())];

                tok1 = I.processImplicitTimes(session, tok1);
            }

            I = INFIX_PARSELETS[usize::from(tok1.tok.value())];

            //
            // Cannot just compare tokens
            //
            // May be something like  a * b c \[Times] d
            //
            // and we want only a single Infix node created
            //
            if I.as_any().type_id() != timesParselet.as_any().type_id() {
                //
                // Tok.tok != tok_in.tok, so break
                //

                Trivia1.borrow_mut().reset(&mut session.tokenizer);

                // MUSTTAIL
                return TimesParselet::reduce_Times(session);
            }

            session.push_trivia_seq(&mut Trivia1.borrow_mut());

            session.push_leaf_and_next(tok1);

            let Tok2 = session.current_token_eat_trivia();

            // #if !USE_MUSTTAIL
            let ctxt = session.top_context();
            assert!(ctxt.is_identity());

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

    fn reduce_Times(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| InfixNode::new(InfixOperator::Times, ctx))
    }
}
