use crate::{
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{TokenKind, TokenRef},
};


impl InfixParselet for TimesParselet {
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

        let ctxt = session.top_context();
        ctxt.init_identity();

        session.parse_prefix(tok2);

        return TimesParselet::parse_loop(session);
    }

    fn getOp(&self) -> InfixParseletOperator {
        return InfixOperator::Times.into();
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Option<Precedence> {
        return Some(Precedence::STAR);
    }
}

impl TimesParselet {
    fn parse_loop(session: &mut ParserSession) {
        loop {
            panic_if_aborted!();


            let (mut trivia1, mut tok1) =
                session.current_token_eat_trivia_into();

            tok1 = session.do_process_implicit_times(tok1);

            if tok1.tok == TokenKind::Fake_ImplicitTimes {
                //
                // implicit Times should not cross toplevel newlines
                //
                // so reset and try again
                //

                trivia1.reset(&mut session.tokenizer);

                (trivia1, tok1) = session
                    .current_token_eat_trivia_but_not_toplevel_newlines_into();

                tok1 = session.do_process_implicit_times(tok1)
            }

            //
            // Cannot just compare tokens
            //
            // May be something like  a * b c \[Times] d
            //
            // and we want only a single Infix node created
            //
            if tok1.tok.infix_parselet().getOp() != (TimesParselet {}).getOp() {
                //
                // Tok.tok != tok_in.tok, so break
                //

                trivia1.reset(&mut session.tokenizer);

                session.reduce_infix(InfixOperator::Times);

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
