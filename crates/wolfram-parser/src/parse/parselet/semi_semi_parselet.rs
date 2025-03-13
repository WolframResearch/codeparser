use crate::{
    cst::{BinaryNode, TernaryNode},
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{Token, TokenKind, TokenRef},
};

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

impl InfixParselet for SemiSemiParselet {
    fn parse_infix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return SemiSemiParselet::parse1(session);
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Option<Precedence> {
        return Some(Precedence::SEMISEMI);
    }

    fn process_implicit_times<'i>(
        &self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        //
        // SemiSemi was already parsed with look-ahead with the assumption that implicit Times will be handled correctly
        //

        if session.top_node_is_span() {
            return Token::at_start(TokenKind::Fake_ImplicitTimes, tok_in);
        }

        return tok_in;
    }
}

impl PrefixParselet for SemiSemiParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();

        debug_assert_eq!(tok_in.tok, TokenKind::SemiSemi);

        session.push_leaf(Token::at_start(TokenKind::Fake_ImplicitOne, tok_in));

        session.push_context(Precedence::SEMISEMI);

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return self.parse_infix(session, tok_in);
    }
}

impl SemiSemiParselet {
    fn parse1(session: &mut ParserSession) {
        panic_if_aborted!();

        //
        // Span should not cross toplevel newlines
        //
        let SecondTok =
            session.current_token_eat_trivia_but_not_toplevel_newlines();

        //
        // a;;
        //  ^~tok_in
        //

        if !SecondTok.tok.isPossibleBeginning() {
            //
            // a;;&
            //    ^SecondTok
            //

            session.push_leaf(Token::at_start(
                TokenKind::Fake_ImplicitAll,
                SecondTok,
            ));

            //
            // nextToken() is not needed after an implicit token
            //

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(session);
        }

        if SecondTok.tok != TokenKind::SemiSemi {
            //
            // a;;b
            //    ^SecondTok
            //

            let ctxt = session.top_context();
            ctxt.init_callback(|s| SemiSemiParselet::parse2(s));

            // MUSTTAIL
            return session.parse_prefix(SecondTok);
        }

        //
        // a;;;;
        //    ^~SecondTok
        //

        session
            .push_leaf(Token::at_start(TokenKind::Fake_ImplicitAll, SecondTok));

        SecondTok.skip(&mut session.tokenizer);

        //
        // Span should not cross toplevel newlines
        //
        let (trivia1, ThirdTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        if !ThirdTok.tok.isPossibleBeginning()
            || ThirdTok.tok == TokenKind::SemiSemi
        {
            //
            // a;;;;&
            //      ^ThirdTok
            //

            //
            // a;;;;;;
            //      ^~ThirdTok
            //

            trivia1.reset(&mut session.tokenizer);
            SecondTok.reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(session);
        }

        //
        // a;;;;b
        //      ^ThirdTok
        //

        session.push_leaf(SecondTok);

        //
        // nextToken() already handled above
        //

        session.push_trivia_seq(trivia1);

        let ctxt = session.top_context();
        ctxt.init_callback(|s| SemiSemiParselet::reduce_ternary(s));

        // MUSTTAIL
        return session.parse_prefix(ThirdTok);
    }

    fn parse2(session: &mut ParserSession) {
        panic_if_aborted!();


        //
        // Span should not cross toplevel newlines
        //
        let (trivia1, ThirdTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        if !ThirdTok.tok.isPossibleBeginning()
            || ThirdTok.tok != TokenKind::SemiSemi
        {
            //
            // a;;b&
            //     ^ThirdTok
            //

            //
            // \[Integral];;x\[DifferentialD]x
            //               ^~~~~~~~~~~~~~~~ThirdTok
            //

            trivia1.reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(session);
        }

        //
        // a;;b;;
        //     ^~ThirdTok
        //

        ThirdTok.skip(&mut session.tokenizer);

        //
        // Span should not cross toplevel newlines
        //
        let (trivia2, FourthTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        if !FourthTok.tok.isPossibleBeginning()
            || FourthTok.tok == TokenKind::SemiSemi
        {
            //
            // a;;b;;&
            //       ^FourthTok
            //

            //
            // a;;b;;;;
            //       ^~FourthTok
            //

            trivia2.reset(&mut session.tokenizer);
            ThirdTok.reset(&mut session.tokenizer);
            trivia1.reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(session);
        }

        //
        // a;;b;;c
        //       ^FourthTok
        //

        session.push_trivia_seq(trivia1);

        session.push_leaf(ThirdTok);

        //
        // nextToken() already handled above
        //

        session.push_trivia_seq(trivia2);

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SemiSemiParselet_parse2 as usize);
        ctxt.set_callback(|s| SemiSemiParselet::reduce_ternary(s));

        // MUSTTAIL
        return session.parse_prefix(FourthTok);
    }

    fn reduce_binary(session: &mut ParserSession) {
        session
            .reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Span, ctx))
    }

    fn reduce_ternary(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| {
            TernaryNode::new(TernaryOperator::Span, ctx)
        })
    }
}
