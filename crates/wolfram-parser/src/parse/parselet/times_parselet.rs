use crate::{
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{TokenKind, TokenRef},
};


impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for TimesParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        first_node: B::Node,
        trivia1: B::TriviaHandle,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let mut infix_state =
            session.begin_infix(InfixOperator::Times, first_node);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let (trivia2, tok2) = session.current_token_eat_trivia();

        let second_node = session.parse_prefix(tok2);

        session.builder.infix_add(
            &mut infix_state,
            trivia1,
            tok_in,
            trivia2,
            second_node,
        );

        return TimesParselet::parse_loop(session, infix_state);
    }

    fn getOp(&self) -> InfixParseletOperator {
        return InfixOperator::Times.into();
    }

    fn getPrecedence(&self, _: &ParserSession<'i, B>) -> Option<Precedence> {
        return Some(Precedence::STAR);
    }
}

impl TimesParselet {
    fn parse_loop<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        mut infix_state: B::InfixParseState,
    ) -> B::Node {
        loop {
            panic_if_aborted!();


            let (mut trivia1, mut tok1) = session.current_token();

            tok1 = session.do_process_implicit_times(tok1);

            if tok1.tok == TokenKind::Fake_ImplicitTimes {
                //
                // implicit Times should not cross toplevel newlines
                //
                // so reset and try again
                //

                session.trivia_reset(trivia1);

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

            let tok1_op =
                B::with_infix_parselet(tok1.tok, |parselet| parselet.getOp());

            if tok1_op
                != <TimesParselet as InfixParselet<B>>::getOp(&TimesParselet {})
            {
                //
                // Tok.tok != tok_in.tok, so break
                //

                session.trivia_reset(trivia1);

                let node = session.reduce_infix(infix_state);

                // MUSTTAIL
                return session.parse_climb(node);
            }

            let (trivia1, tok1) = session.commit_syntax_and_next(trivia1, tok1);

            let (trivia2, Tok2) = session.current_token_eat_trivia();

            let operand = session.parse_prefix(Tok2);

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
