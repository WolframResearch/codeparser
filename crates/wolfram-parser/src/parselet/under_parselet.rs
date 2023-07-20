use crate::{
    cst::{CompoundNode, CompoundOperator},
    panic_if_aborted,
    parselet::*,
    parser::ParserSession,
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
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, tok_in: TokenRef<'i>) {
        //
        // prefix
        //
        // Something like  _  or  _a
        //

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        if Tok.tok == TokenKind::Symbol {
            //
            // Something like  _b
            //

            session.push_context(PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            SymbolParselet::parse_infix_context_sensitive(session, Tok);

            // MUSTTAIL
            return self.reduce_Blank(session);
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
            return self.reduce_Blank(session);
        }

        // MUSTTAIL
        return session.parse_climb();
    }
}

impl UnderParselet {
    pub(crate) fn parse_infix_context_sensitive<'i>(
        &self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // infix
        //
        // Something like  a_b
        //

        // assert!(P);

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let Tok = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

        if Tok.tok == TokenKind::Symbol {
            //
            // Something like  a_b
            //

            session.push_context(PRECEDENCE_HIGHEST);

            //
            // Context-sensitive and OK to build stack
            //

            SymbolParselet::parse_infix_context_sensitive(session, Tok);

            // MUSTTAIL
            return self.reduce_Blank_context_sensitive(session);
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
            return self.reduce_Blank_context_sensitive(session);
        }

        // no call needed here
        return;
    }

    fn reduce_Blank(&self, session: &mut ParserSession) {
        let BOp = self.getBOp();

        session.reduce_and_climb(|ctx| CompoundNode::new(BOp, ctx))
    }

    //
    // Called from other parselets
    //
    fn reduce_Blank_context_sensitive(&self, session: &mut ParserSession) {
        let BOp = self.getBOp();

        let context = session.pop_context();
        session.push_node(CompoundNode::new(BOp, context));

        // no call needed here
        return;
    }
}

//======================================
// UnderDotParselet
//======================================

impl PrefixParselet for UnderDotParselet {
    fn parse_prefix<'i>(&'static self, session: &mut ParserSession<'i>, tok_in: TokenRef<'i>) {
        //
        // prefix
        //
        // Something like  _.
        //

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return session.parse_climb();
    }
}

impl UnderDotParselet {
    //
    // Called from other parselets
    //
    pub(crate) fn parse_infix_context_sensitive<'i>(
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // infix
        //
        // Something like  a_.

        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        // no call needed here
        return;
    }
}
