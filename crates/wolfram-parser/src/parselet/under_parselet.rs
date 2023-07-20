use crate::{
    cst::{CompoundNode, CompoundOperator},
    panic_if_aborted,
    parselet::*,
    parser_session::ParserSession,
    precedence::*,
    source::*,
    token::{TokenKind, TokenRef},
    tokenizer::Tokenizer_currentToken,
};

impl UnderParselet {
    pub(crate) const fn new(BOp: CompoundOperator, PBOp: CompoundOperator) -> Self {
        Self { BOp, PBOp }
    }

    fn getBOp(&self) -> CompoundOperator {
        return self.BOp;
    }
}

impl PrefixParselet for UnderParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        UnderParselet_parsePrefix(session, self, token)
    }
}

fn UnderParselet_parsePrefix<'i>(
    session: &mut ParserSession<'i>,
    P: &UnderParselet,
    TokIn: TokenRef<'i>,
) {
    //
    // prefix
    //
    // Something like  _  or  _a
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    if Tok.tok == TokenKind::Symbol {
        //
        // Something like  _b
        //

        session.push_context(PRECEDENCE_HIGHEST);

        //
        // Context-sensitive and OK to build stack
        //

        SymbolParselet_parseInfixContextSensitive(session, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlank(session, P);
    }

    if Tok.tok == TokenKind::Error_ExpectedLetterlike {
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //

        session.push_context(PRECEDENCE_HIGHEST);

        session.push_leaf_and_next(Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlank(session, P);
    }

    // MUSTTAIL
    return session.parse_climb();
}

pub(crate) fn UnderParselet_parseInfixContextSensitive<'i>(
    session: &mut ParserSession<'i>,
    P: &UnderParselet,
    TokIn: TokenRef<'i>,
) {
    //
    // infix
    //
    // Something like  a_b
    //

    // assert!(P);

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    if Tok.tok == TokenKind::Symbol {
        //
        // Something like  a_b
        //

        session.push_context(PRECEDENCE_HIGHEST);

        //
        // Context-sensitive and OK to build stack
        //

        SymbolParselet_parseInfixContextSensitive(session, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P);
    }

    if Tok.tok == TokenKind::Error_ExpectedLetterlike {
        //
        // Something like  a_b`
        //
        // It's nice to include the error inside of the blank
        //

        session.push_context(PRECEDENCE_HIGHEST);

        session.push_leaf_and_next(Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P);
    }

    // no call needed here
    return;
}

fn UnderParselet_reduceBlank(session: &mut ParserSession, P: &UnderParselet) {
    let BOp = P.getBOp();

    session.reduce_and_climb(|ctx| CompoundNode::new(BOp, ctx))
}

//
// Called from other parselets
//
fn UnderParselet_reduceBlankContextSensitive(session: &mut ParserSession, P: &UnderParselet) {
    let BOp = P.getBOp();

    let context = session.pop_context();
    session.push_node(CompoundNode::new(BOp, context));

    // no call needed here
    return;
}

//======================================
// UnderDotParselet
//======================================

impl PrefixParselet for UnderDotParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, token: TokenRef<'i>) {
        UnderDotParselet_parsePrefix(session, token)
    }
}

fn UnderDotParselet_parsePrefix<'i>(session: &mut ParserSession<'i>, TokIn: TokenRef<'i>) {
    //
    // prefix
    //
    // Something like  _.
    //

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    // MUSTTAIL
    return session.parse_climb();
}


//
// Called from other parselets
//
pub(crate) fn UnderDotParselet_parseInfixContextSensitive<'i>(
    session: &mut ParserSession<'i>,
    TokIn: TokenRef<'i>,
) {
    //
    // infix
    //
    // Something like  a_.

    panic_if_aborted!();


    session.push_leaf_and_next(TokIn);

    // no call needed here
    return;
}
