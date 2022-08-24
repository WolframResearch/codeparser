use crate::{
    feature,
    node::{AbortNode, InfixNode},
    parselet::*,
    parselet_registration::*,
    parser::{
        Parser_eatTrivia, Parser_eatTriviaButNotToplevelNewlines_2, Parser_eatTrivia_2,
        Parser_identity, Parser_parseClimb, Parser_popContext, Parser_popNode,
        Parser_pushLeafAndNext, Parser_pushNode, Parser_pushTriviaSeq, Parser_topContext,
        Parser_tryContinue,
    },
    parser_session::ParserSession,
    precedence::*,
    source::TOPLEVEL,
    symbol::Symbol,
    symbol_registration::SYMBOL_TIMES,
    token::Token,
    token_enum_registration::TokenEnum::TOKEN_FAKE_IMPLICITTIMES,
    tokenizer::Tokenizer_currentToken,
};


impl InfixParselet for TimesParselet {
    fn getOp(&self) -> Symbol {
        return SYMBOL_TIMES;
    }

    fn getPrecedence(&self, _: &mut ParserSession) -> Precedence {
        return PRECEDENCE_STAR;
    }

    fn parseInfix(&self) -> ParseFunction {
        return TimesParselet_parseInfix;
    }
}

fn TimesParselet_parseInfix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    if feature::CHECK_ABORT && session.abortQ() {
        Parser_popContext(session);
        Parser_pushNode(session, AbortNode::new());
        return Parser_tryContinue(session, ignored, TokIn /*ignored*/);
    }

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

    (P2.parsePrefix())(session, P2, Tok2);

    return TimesParselet_parseLoop(session, ignored, TokIn /*ignored*/);
    // #else
    //     auto& Ctxt = Parser_topContext(session);
    //     assert!(!Ctxt.F);
    //     Ctxt.F = TimesParselet_parseLoop;

    //     let P2 = prefixParselets[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn TimesParselet_parseLoop(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    // #if !USE_MUSTTAIL
    loop {
        // #endif // !USE_MUSTTAIL

        if feature::CHECK_ABORT && session.abortQ() {
            Parser_popNode(session);
            Parser_popContext(session);
            Parser_pushNode(session, AbortNode::new());
            return Parser_tryContinue(session, ignored, ignored2);
        }


        let Trivia1 = session.trivia1.clone();

        let mut Tok1 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia_2(session, &mut Tok1, TOPLEVEL, &mut Trivia1.borrow_mut());

        let mut I: &dyn InfixParselet = infixParselets[usize::from(Tok1.tok.value())];

        Tok1 = I.processImplicitTimes(session, Tok1);

        if Tok1.tok == TOKEN_FAKE_IMPLICITTIMES {
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

            I = infixParselets[usize::from(Tok1.tok.value())];

            Tok1 = I.processImplicitTimes(session, Tok1);
        }

        I = infixParselets[usize::from(Tok1.tok.value())];

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
            return TimesParselet_reduceTimes(session, ignored, ignored2);
        }

        Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

        Parser_pushLeafAndNext(session, Tok1);

        let mut Tok2 = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        Parser_eatTrivia(session, &mut Tok2, TOPLEVEL);

        // #if !USE_MUSTTAIL
        let Ctxt = Parser_topContext(session);
        assert!(Ctxt.f.unwrap() as usize == Parser_identity as usize);

        let P2 = prefix_parselet(Tok2.tok);

        (P2.parsePrefix())(session, P2, Tok2);
    } // while (true)
      // #else
      //     auto& Ctxt = Parser_topContext(session);
      //     assert!(Ctxt.F == TimesParselet_parseLoop);

    //     let P2 = prefixParselets[Tok2.tok.value()];

    //     // MUSTTAIL
    //     return (P2.parsePrefix())(session, P2, Tok2);
    // #endif // !USE_MUSTTAIL
}

fn TimesParselet_reduceTimes(session: &mut ParserSession, ignored: ParseletPtr, ignored2: Token) {
    let node = InfixNode::new(SYMBOL_TIMES, Parser_popContext(session));
    Parser_pushNode(session, node);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored, ignored2);
}