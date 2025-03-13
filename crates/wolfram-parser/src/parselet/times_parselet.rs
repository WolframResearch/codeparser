use crate::{
    node::{InfixNode, Operator},
    panic_if_aborted,
    parselet::*,
    parselet_registration::*,
    parser::{
        Parser_eatTrivia, Parser_eatTriviaButNotToplevelNewlines_2, Parser_eatTrivia_2,
        Parser_identity, Parser_parseClimb, Parser_popContext, Parser_pushLeafAndNext,
        Parser_pushNode, Parser_pushTriviaSeq, Parser_topContext,
    },
    parser_session::ParserSession,
    precedence::*,
    source::TOPLEVEL,
    token::{TokenKind, TokenRef},
    tokenizer::Tokenizer_currentToken,
};


impl InfixParselet for TimesParselet {
    fn parse_infix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        TimesParselet_parseInfix(session, token)
    }

    fn getOp(&self) -> Operator {
        return Operator::Times;
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        return PRECEDENCE_STAR;
    }
}

fn TimesParselet_parseInfix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    panic_if_aborted!();

    Parser_pushLeafAndNext(session, TokIn);

    //
    // Unroll 1 iteration of the loop because we know that TokIn has already been read
    //

    let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

    // #if !USE_MUSTTAIL
    let Ctxt = Parser_topContext(session);
    assert!(Ctxt.f.is_none());
    Ctxt.f = Some(Parser_identity);

    let P2 = prefix_parselet(Tok2.tok);

    P2.parse_prefix(session, Tok2);

    return TimesParselet_parseLoop(session);
    // #else
    //     auto& Ctxt = Parser_topContext(session);
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

        Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

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

            Parser_eatTriviaButNotToplevelNewlines_2(
                session,
                &mut Tok1,
                TOPLEVEL,
                &mut Trivia1.borrow_mut(),
            );

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

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        Parser_pushLeafAndNext(session, Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

        // #if !USE_MUSTTAIL
        let Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        let P2 = prefix_parselet(Tok2.tok);

        P2.parse_prefix(session, Tok2);
    } // while (true)
      // #else
      //     auto& Ctxt = Parser_topContext(session);
      //     assert!(Ctxt.F == TimesParselet_parseLoop);

    //     let P2 = PREFIX_PARSELETS[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return P2.parse_prefix(session, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn TimesParselet_reduceTimes(session: &mut ParserSession) {
    let node = InfixNode::new(Operator::Times, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session);
}
