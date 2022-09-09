use crate::{
    node::CompoundNode,
    panic_if_aborted,
    parselet::*,
    parser::{
        Parser_parseClimb, Parser_popContext, Parser_pushContext, Parser_pushLeafAndNext,
        Parser_pushNode,
    },
    parser_session::ParserSession,
    precedence::*,
    source::*,
    symbol::Symbol,
    token::{Token, TokenKind},
    tokenizer::Tokenizer_currentToken,
};

impl UnderParselet {
    pub(crate) const fn new(BOp: Symbol, PBOp: Symbol) -> Self {
        Self { BOp, PBOp }
    }

    fn getBOp(&self) -> Symbol {
        return self.BOp;
    }
}

impl PrefixParselet for UnderParselet {
    fn parse_prefix(&'static self, session: &mut ParserSession, token: Token) {
        UnderParselet_parsePrefix(session, self, token)
    }
}

fn UnderParselet_parsePrefix(session: &mut ParserSession, P: &UnderParselet, TokIn: Token) {
    //
    // prefix
    //
    // Something like  _  or  _a
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    if Tok.tok == TokenKind::Symbol {
        //
        // Something like  _b
        //

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

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

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

        Parser_pushLeafAndNext(session, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlank(session, P);
    }

    // MUSTTAIL
    return Parser_parseClimb(session);
}

pub(crate) fn UnderParselet_parseInfixContextSensitive(
    session: &mut ParserSession,
    P: &UnderParselet,
    TokIn: Token,
) {
    //
    // infix
    //
    // Something like  a_b
    //

    // assert!(P);

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    if Tok.tok == TokenKind::Symbol {
        //
        // Something like  a_b
        //

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

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

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

        Parser_pushLeafAndNext(session, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P);
    }

    // no call needed here
    return;
}

fn UnderParselet_reduceBlank(session: &mut ParserSession, P: &UnderParselet) {
    let BOp = P.getBOp();

    let context = Parser_popContext(session);
    Parser_pushNode(session, CompoundNode::new(BOp, context));

    // MUSTTAIL
    return Parser_parseClimb(session);
}

//
// Called from other parselets
//
fn UnderParselet_reduceBlankContextSensitive(session: &mut ParserSession, P: &UnderParselet) {
    let BOp = P.getBOp();

    let context = Parser_popContext(session);
    Parser_pushNode(session, CompoundNode::new(BOp, context));

    // no call needed here
    return;
}

//======================================
// UnderDotParselet
//======================================

impl PrefixParselet for UnderDotParselet {
    fn parse_prefix(&'static self, session: &mut ParserSession, token: Token) {
        UnderDotParselet_parsePrefix(session, token)
    }
}

fn UnderDotParselet_parsePrefix(session: &mut ParserSession, TokIn: Token) {
    //
    // prefix
    //
    // Something like  _.
    //

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    // MUSTTAIL
    return Parser_parseClimb(session);
}


//
// Called from other parselets
//
pub(crate) fn UnderDotParselet_parseInfixContextSensitive(
    session: &mut ParserSession,
    TokIn: Token,
) {
    //
    // infix
    //
    // Something like  a_.

    panic_if_aborted!();


    Parser_pushLeafAndNext(session, TokIn);

    // no call needed here
    return;
}
