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
        trivia1: TriviaSeqRef<'i>,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        let mut infix_builder =
            session.begin_infix(InfixOperator::Times, first_node);

        session.skip(tok_in);

        //
        // Unroll 1 iteration of the loop because we know that tok_in has already been read
        //

        let (trivia2, tok2) = session.current_token_eat_trivia_into();

        let second_node = session.parse_prefix(tok2);

        infix_builder.add(trivia1, tok_in, trivia2, second_node);

        return TimesParselet::parse_loop(session, infix_builder);
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
        mut infix_builder: B::InfixParseBuilder,
    ) -> B::Node {
        loop {
            panic_if_aborted!();


            let (mut trivia1, mut tok1) =
                session.current_token_eat_trivia_into();

            tok1 = session
                .do_process_implicit_times(infix_builder.last_node(), tok1);

            if tok1.tok == TokenKind::Fake_ImplicitTimes {
                //
                // implicit Times should not cross toplevel newlines
                //
                // so reset and try again
                //

                trivia1.reset(&mut session.tokenizer);

                (trivia1, tok1) = session
                    .current_token_eat_trivia_but_not_toplevel_newlines_into();

                tok1 = session
                    .do_process_implicit_times(infix_builder.last_node(), tok1)
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

                trivia1.reset(&mut session.tokenizer);

                let node = session.reduce_infix(infix_builder);

                // MUSTTAIL
                return session.parse_climb(node);
            }

            session.skip(tok1);

            let (trivia2, Tok2) = session.current_token_eat_trivia_into();

            let operand = session.parse_prefix(Tok2);

            infix_builder.add(trivia1, tok1, trivia2, operand);
        } // loop
    }
}
