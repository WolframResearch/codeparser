use crate::{
    feature,
    node::{AbortNode, CompoundNode},
    parselet::*,
    parselet_registration::*,
    parser::{
        Parser_parseClimb, Parser_popContext, Parser_pushContext, Parser_pushLeafAndNext,
        Parser_pushNode, Parser_tryContinue,
    },
    parser_session::ParserSession,
    precedence::*,
    source::*,
    symbol::Symbol,
    token::Token,
    token_enum_registration::TokenEnum::*,
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
    fn parsePrefix(&self) -> ParseFunction {
        return UnderParselet_parsePrefix;
    }
}

fn UnderParselet_parsePrefix(session: &mut ParserSession, P: ParseletPtr, TokIn: Token) {
    //
    // prefix
    //
    // Something like  _  or  _a
    //

    if feature::CHECK_ABORT && session.abortQ() {
        Parser_pushNode(session, AbortNode::new());
        return Parser_tryContinue(session, P /*ignored*/, TokIn /*ignored*/);
    }


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    if Tok.tok == TOKEN_SYMBOL {
        //
        // Something like  _b
        //

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

        //
        // Context-sensitive and OK to build stack
        //

        SymbolParselet_parseInfixContextSensitive(session, &symbolParselet, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlank(session, P, TokIn /*ignored*/);
    }

    if Tok.tok == TOKEN_ERROR_EXPECTEDLETTERLIKE {
        //
        // Something like  _a`
        //
        // It's nice to include the error inside of the blank
        //

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

        Parser_pushLeafAndNext(session, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlank(session, P, TokIn /*ignored*/);
    }

    // MUSTTAIL
    return Parser_parseClimb(session, P /*ignored*/, TokIn /*ignored*/);
}

pub(crate) fn UnderParselet_parseInfixContextSensitive(
    session: &mut ParserSession,
    P: ParseletPtr,
    TokIn: Token,
) {
    //
    // infix
    //
    // Something like  a_b
    //

    // assert!(P);

    if feature::CHECK_ABORT && session.abortQ() {
        Parser_pushNode(session, AbortNode::new());
        return;
    }


    Parser_pushLeafAndNext(session, TokIn);

    let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    if Tok.tok == TOKEN_SYMBOL {
        //
        // Something like  a_b
        //

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

        //
        // Context-sensitive and OK to build stack
        //

        SymbolParselet_parseInfixContextSensitive(session, &symbolParselet, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P, TokIn /*ignored*/);
    }

    if Tok.tok == TOKEN_ERROR_EXPECTEDLETTERLIKE {
        //
        // Something like  a_b`
        //
        // It's nice to include the error inside of the blank
        //

        Parser_pushContext(session, PRECEDENCE_HIGHEST);

        Parser_pushLeafAndNext(session, Tok);

        // MUSTTAIL
        return UnderParselet_reduceBlankContextSensitive(session, P, TokIn /*ignored*/);
    }

    // no call needed here
    return;
}

fn UnderParselet_reduceBlank(session: &mut ParserSession, P: ParseletPtr, Ignored: Token) {
    let P = P
        .as_any()
        .downcast_ref::<UnderParselet>()
        .expect("unable to downcast to UnderParselet");

    let BOp = P.getBOp();

    let context = Parser_popContext(session);
    Parser_pushNode(session, CompoundNode::new(BOp, context));

    // MUSTTAIL
    return Parser_parseClimb(session, P /*ignored*/, Ignored);
}

//
// Called from other parselets
//
fn UnderParselet_reduceBlankContextSensitive(
    session: &mut ParserSession,
    P: ParseletPtr,
    _: Token,
) {
    let P = P
        .as_any()
        .downcast_ref::<UnderParselet>()
        .expect("unable to downcast to UnderParselet");

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
    fn parsePrefix(&self) -> ParseFunction {
        return UnderDotParselet_parsePrefix;
    }
}

fn UnderDotParselet_parsePrefix(session: &mut ParserSession, ignored: ParseletPtr, TokIn: Token) {
    //
    // prefix
    //
    // Something like  _.
    //

    if feature::CHECK_ABORT && session.abortQ() {
        Parser_pushNode(session, AbortNode::new());
        return Parser_tryContinue(session, ignored, TokIn /*ignored*/);
    }


    Parser_pushLeafAndNext(session, TokIn);

    // MUSTTAIL
    return Parser_parseClimb(session, ignored, TokIn /*ignored*/);
}


//
// Called from other parselets
//
pub(crate) fn UnderDotParselet_parseInfixContextSensitive(
    session: &mut ParserSession,
    _: ParseletPtr,
    TokIn: Token,
) {
    //
    // infix
    //
    // Something like  a_.

    if feature::CHECK_ABORT && session.abortQ() {
        Parser_pushNode(session, AbortNode::new());
        return;
    }


    Parser_pushLeafAndNext(session, TokIn);

    // no call needed here
    return;
}
