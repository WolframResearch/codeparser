use crate::{
    node::{BinaryNode, TernaryNode},
    panic_if_aborted,
    parselet::*,
    parser::{
        Parser_checkSpan, Parser_eatTriviaButNotToplevelNewlines,
        Parser_eatTriviaButNotToplevelNewlines_2, Parser_parseClimb, Parser_popContext,
        Parser_pushContext, Parser_pushLeaf, Parser_pushLeafAndNext, Parser_pushNode,
        Parser_pushTriviaSeq, Parser_topContext,
    },
    parser_session::ParserSession,
    precedence::*,
    source::Source,
    source::TOPLEVEL,
    symbol_registration::SYMBOL_SPAN,
    token::Token,
    token_enum_registration::TokenEnum::*,
    tokenizer::Tokenizer_currentToken,
};

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

impl InfixParselet for SemiSemiParselet {
    fn parse_infix(&'static self, session: &mut ParserSession, token: Token) {
        SemiSemiParselet_parseInfix(session, token)
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        return PRECEDENCE_SEMISEMI;
    }

    fn processImplicitTimes(&self, session: &mut ParserSession, TokIn: Token) -> Token {
        //
        // SemiSemi was already parsed with look-ahead with the assumption that implicit Times will be handled correctly
        //

        if Parser_checkSpan(session) {
            return Token::new2(
                TOKEN_FAKE_IMPLICITTIMES,
                TokIn.span,
                Source::from_location(TokIn.src.start),
            );
        }

        return TokIn;
    }
}

impl PrefixParselet for SemiSemiParselet {
    fn parse_prefix(&'static self, session: &mut ParserSession, token: Token) {
        SemiSemiParselet_parsePrefix(session, token)
    }
}

fn SemiSemiParselet_parsePrefix(session: &mut ParserSession, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeaf(
        session,
        Token::new2(
            TOKEN_FAKE_IMPLICITONE,
            TokIn.span,
            Source::from_location(TokIn.src.start),
        ),
    );

    Parser_pushContext(session, PRECEDENCE_SEMISEMI);

    //
    // nextToken() is not needed after an implicit token
    //

    // MUSTTAIL
    return SemiSemiParselet_parseInfix(session, TokIn);
}

fn SemiSemiParselet_parseInfix(session: &mut ParserSession, TokIn: Token) {
    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    // MUSTTAIL
    return SemiSemiParselet_parse1(session);
}

fn SemiSemiParselet_parse1(session: &mut ParserSession) {
    panic_if_aborted!();


    let mut SecondTok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines(session, &mut SecondTok, TOPLEVEL);

    //
    // a;;
    //  ^~TokIn
    //

    if !SecondTok.tok.isPossibleBeginning() {
        //
        // a;;&
        //    ^SecondTok
        //

        Parser_pushLeaf(
            session,
            Token::new2(
                TOKEN_FAKE_IMPLICITALL,
                SecondTok.span,
                Source::from_location(SecondTok.src.start),
            ),
        );

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return SemiSemiParselet_reduceBinary(session);
    }

    if SecondTok.tok != TOKEN_SEMISEMI {
        //
        // a;;b
        //    ^SecondTok
        //

        let Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.is_none());
        Ctxt.f = Some(|s, _| SemiSemiParselet_parse2(s));

        let P2 = prefix_parselet(SecondTok.tok);

        // MUSTTAIL
        return P2.parse_prefix(session, SecondTok);
    }

    //
    // a;;;;
    //    ^~SecondTok
    //

    Parser_pushLeaf(
        session,
        Token::new2(
            TOKEN_FAKE_IMPLICITALL,
            SecondTok.span,
            Source::from_location(SecondTok.src.start),
        ),
    );

    SecondTok.skip(&mut session.tokenizer);

    let mut ThirdTok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    let Trivia1 = session.trivia1.clone();

    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines_2(
        session,
        &mut ThirdTok,
        TOPLEVEL,
        &mut Trivia1.borrow_mut(),
    );

    if !ThirdTok.tok.isPossibleBeginning() || ThirdTok.tok == TOKEN_SEMISEMI {
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

    Parser_pushLeaf(session, SecondTok);

    //
    // nextToken() already handled above
    //

    Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

    let Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(|s, _| SemiSemiParselet_reduceTernary(s));

    let P2 = prefix_parselet(ThirdTok.tok);

    // MUSTTAIL
    return P2.parse_prefix(session, ThirdTok);
}

fn SemiSemiParselet_parse2(session: &mut ParserSession) {
    panic_if_aborted!();


    let Trivia1 = session.trivia1.clone();

    let mut ThirdTok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines_2(
        session,
        &mut ThirdTok,
        TOPLEVEL,
        &mut Trivia1.borrow_mut(),
    );

    if !ThirdTok.tok.isPossibleBeginning() || ThirdTok.tok != TOKEN_SEMISEMI {
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

    let mut FourthTok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // Span should not cross toplevel newlines
    //
    Parser_eatTriviaButNotToplevelNewlines_2(
        session,
        &mut FourthTok,
        TOPLEVEL,
        &mut Trivia2.borrow_mut(),
    );

    if !FourthTok.tok.isPossibleBeginning() || FourthTok.tok == TOKEN_SEMISEMI {
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

    Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

    Parser_pushLeaf(session, ThirdTok);

    //
    // nextToken() already handled above
    //

    Parser_pushTriviaSeq(session, &mut Trivia2.borrow_mut());

    let Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.unwrap() as usize == SemiSemiParselet_parse2 as usize);
    Ctxt.f = Some(|s, _| SemiSemiParselet_reduceTernary(s));

    let P2 = prefix_parselet(FourthTok.tok);

    // MUSTTAIL
    return P2.parse_prefix(session, FourthTok);
}

fn SemiSemiParselet_reduceBinary(session: &mut ParserSession) {
    let node = BinaryNode::new(SYMBOL_SPAN, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session);
}

fn SemiSemiParselet_reduceTernary(session: &mut ParserSession) {
    let node = TernaryNode::new(SYMBOL_SPAN, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session);
}
