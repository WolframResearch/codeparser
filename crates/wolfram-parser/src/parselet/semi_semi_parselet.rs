use crate::{
    cst::{BinaryNode, TernaryNode},
    panic_if_aborted,
    parselet::*,
    parser::ParserSession,
    precedence::Precedence,
    token::{Token, TokenKind, TokenRef},
};

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

impl InfixParselet for SemiSemiParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, tok_in: TokenRef<'i>) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return SemiSemiParselet::parse1(session);
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Option<Precedence> {
        return Some(Precedence::SEMISEMI);
    }

    fn processImplicitTimes<'i>(
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
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, tok_in: TokenRef<'i>) {
        panic_if_aborted!();

        debug_assert_eq!(tok_in.tok, TokenKind::SemiSemi);

        session.push_leaf(Token::at_start(TokenKind::Fake_ImplicitOne, tok_in));

        session.push_context(Precedence::SEMISEMI);

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiSemiParselet {}.parse_infix(session, tok_in);
    }
}

impl SemiSemiParselet {
    fn parse1(session: &mut ParserSession) {
        panic_if_aborted!();

        //
        // Span should not cross toplevel newlines
        //
        let SecondTok = session.current_token_eat_trivia_but_not_toplevel_newlines();

        //
        // a;;
        //  ^~tok_in
        //

        if !SecondTok.tok.isPossibleBeginning() {
            //
            // a;;&
            //    ^SecondTok
            //

            session.push_leaf(Token::at_start(TokenKind::Fake_ImplicitAll, SecondTok));

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
            ctxt.init_callback(|s, _| SemiSemiParselet::parse2(s), None);

            // MUSTTAIL
            return session.parse_prefix(SecondTok);
        }

        //
        // a;;;;
        //    ^~SecondTok
        //

        session.push_leaf(Token::at_start(TokenKind::Fake_ImplicitAll, SecondTok));

        SecondTok.skip(&mut session.tokenizer);

        let Trivia1 = session.trivia1.clone();

        //
        // Span should not cross toplevel newlines
        //
        let ThirdTok = session
            .current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia1.borrow_mut());

        if !ThirdTok.tok.isPossibleBeginning() || ThirdTok.tok == TokenKind::SemiSemi {
            //
            // a;;;;&
            //      ^ThirdTok
            //

            //
            // a;;;;;;
            //      ^~ThirdTok
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);
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

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        let ctxt = session.top_context();
        ctxt.init_callback(|s, _| SemiSemiParselet::reduce_ternary(s), None);

        // MUSTTAIL
        return session.parse_prefix(ThirdTok);
    }

    fn parse2(session: &mut ParserSession) {
        panic_if_aborted!();


        let Trivia1 = session.trivia1.clone();

        //
        // Span should not cross toplevel newlines
        //
        let ThirdTok = session
            .current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia1.borrow_mut());

        if !ThirdTok.tok.isPossibleBeginning() || ThirdTok.tok != TokenKind::SemiSemi {
            //
            // a;;b&
            //     ^ThirdTok
            //

            //
            // \[Integral];;x\[DifferentialD]x
            //               ^~~~~~~~~~~~~~~~ThirdTok
            //

            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(session);
        }

        //
        // a;;b;;
        //     ^~ThirdTok
        //

        ThirdTok.skip(&mut session.tokenizer);

        let Trivia2 = session.trivia2.clone();

        //
        // Span should not cross toplevel newlines
        //
        let FourthTok = session
            .current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia2.borrow_mut());

        if !FourthTok.tok.isPossibleBeginning() || FourthTok.tok == TokenKind::SemiSemi {
            //
            // a;;b;;&
            //       ^FourthTok
            //

            //
            // a;;b;;;;
            //       ^~FourthTok
            //

            Trivia2.borrow_mut().reset(&mut session.tokenizer);
            ThirdTok.reset(&mut session.tokenizer);
            Trivia1.borrow_mut().reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(session);
        }

        //
        // a;;b;;c
        //       ^FourthTok
        //

        session.push_trivia_seq(&mut Trivia1.borrow_mut());

        session.push_leaf(ThirdTok);

        //
        // nextToken() already handled above
        //

        session.push_trivia_seq(&mut Trivia2.borrow_mut());

        let ctxt = session.top_context();
        // TODO: Figure out how to express this logic and re-enable this assertion.
        // assert!(Ctxt.f.unwrap() as usize == SemiSemiParselet_parse2 as usize);
        ctxt.set_callback(|s, _| SemiSemiParselet::reduce_ternary(s));

        // MUSTTAIL
        return session.parse_prefix(FourthTok);
    }

    fn reduce_binary(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Span, ctx))
    }

    fn reduce_ternary(session: &mut ParserSession) {
        session.reduce_and_climb(|ctx| TernaryNode::new(TernaryOperator::Span, ctx))
    }
}
