use crate::{
    cst::{BinaryNode, TernaryNode},
    panic_if_aborted,
    parselet::*,
    parser::ParserSession,
    precedence::*,
    token::{Token, TokenKind, TokenRef},
};

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

impl InfixParselet for SemiSemiParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        SemiSemiParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        return PRECEDENCE_SEMISEMI;
    }

    fn processImplicitTimes<'i>(
        &self,
        session: &mut ParserSession<'i>,
        TokIn: TokenRef<'i>,
    ) -> TokenRef<'i> {
        //
        // SemiSemi was already parsed with look-ahead with the assumption that implicit Times will be handled correctly
        //

        if session.top_node_is_span() {
            return Token::error_at_start(TokenKind::Fake_ImplicitTimes, TokIn);
        }

        return TokIn;
    }
}

impl PrefixParselet for SemiSemiParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        SemiSemiParselet_parsePrefix(session, token)
    }
}

fn SemiSemiParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf(Token::error_at_start(TokenKind::Fake_ImplicitOne, TokIn));

    session.push_context(PRECEDENCE_SEMISEMI);

    //
    // nextToken() is not needed after an implicit token
    //

    // MUSTTAIL
    return SemiSemiParselet_parseInfix(session, TokIn);
}

fn SemiSemiParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    // MUSTTAIL
    return SemiSemiParselet_parse1(session);
}

fn SemiSemiParselet_parse1(session: &mut ParserSession) {
    panic_if_aborted!();

    //
    // Span should not cross toplevel newlines
    //
    let SecondTok = session.current_token_eat_trivia_but_not_toplevel_newlines();

    //
    // a;;
    //  ^~TokIn
    //

    if !SecondTok.tok.isPossibleBeginning() {
        //
        // a;;&
        //    ^SecondTok
        //

        session.push_leaf(Token::error_at_start(
            TokenKind::Fake_ImplicitAll,
            SecondTok,
        ));

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiSemiParselet_reduceBinary(session);
    }

    if SecondTok.tok != TokenKind::SemiSemi {
        //
        // a;;b
        //    ^SecondTok
        //

        let Ctxt = session.top_context();
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(|s, _| SemiSemiParselet_parse2(s));

        // MUSTTAIL
        return session.parse_prefix(SecondTok);
    }

    //
    // a;;;;
    //    ^~SecondTok
    //

    session.push_leaf(Token::error_at_start(
        TokenKind::Fake_ImplicitAll,
        SecondTok,
    ));

    SecondTok.skip(&mut session.tokenizer);

    let Trivia1 = session.trivia1.clone();

    //
    // Span should not cross toplevel newlines
    //
    let ThirdTok =
        session.current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia1.borrow_mut());

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
        return SemiSemiParselet_reduceBinary(session);
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

    let Ctxt = session.top_context();
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| SemiSemiParselet_reduceTernary(s));

    // MUSTTAIL
    return session.parse_prefix(ThirdTok);
}

fn SemiSemiParselet_parse2(session: &mut ParserSession) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    //
    // Span should not cross toplevel newlines
    //
    let ThirdTok =
        session.current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia1.borrow_mut());

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
        return SemiSemiParselet_reduceBinary(session);
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
    let FourthTok =
        session.current_token_eat_trivia_but_not_toplevel_newlines_into(&mut Trivia2.borrow_mut());

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
        return SemiSemiParselet_reduceBinary(session);
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

    let Ctxt = session.top_context();

    // TODO: Figure out how to express this logic and re-enable this assertion.
    // assert!(Ctxt.f.unwrap() as usize == SemiSemiParselet_parse2 as usize);
    Ctxt.f = Some(|s, _| SemiSemiParselet_reduceTernary(s));

    // MUSTTAIL
    return session.parse_prefix(FourthTok);
}

fn SemiSemiParselet_reduceBinary(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| BinaryNode::new(BinaryOperator::Span, ctx))
}

fn SemiSemiParselet_reduceTernary(session: &mut ParserSession) {
    session.reduce_and_climb(|ctx| TernaryNode::new(TernaryOperator::Span, ctx))
}
