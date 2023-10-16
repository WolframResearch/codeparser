use crate::{
    cst::{CompoundNode, CompoundOperator},
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{TokenKind, TokenRef},
};

impl UnderParselet {
    pub(crate) const fn new(
        BOp: CompoundOperator,
        PBOp: CompoundOperator,
    ) -> Self {
        Self { BOp, PBOp }
    }
}

impl PrefixParselet for UnderParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        //
        // prefix
        //
        // Something like  _  or  _a
        //

        self.parse_under_context_sensitive(session, tok_in);

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

        self.parse_under_context_sensitive(session, tok_in);
    }

    fn parse_under_context_sensitive<'i>(
        &self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
        panic_if_aborted!();


        session.push_leaf_and_next(tok_in);

        let tok = session.tokenizer.peek_token();

        match tok.tok {
            TokenKind::Symbol => {
                //
                // Something like
                //     prefix:  _b
                //      infix:  a_b
                //

                session.push_context(Precedence::HIGHEST);

                // Context-sensitive infix parse of Symbol token
                //
                // Something like  _b
                //                  ^
                // We know we are already in the middle of parsing _
                //
                // Just push this symbol
                //
                session.push_leaf_and_next(tok);

                session.reduce(|ctx| CompoundNode::new(self.BOp, ctx));
            },

            TokenKind::Error_ExpectedLetterlike => {
                //
                // Something like:
                //     prefix:  _a`   (TID:231016/1)
                //      infix:  a_b`  (TID:231016/2)
                //
                // It's nice to include the error inside of the blank
                //

                session.push_context(Precedence::HIGHEST);

                session.push_leaf_and_next(tok);

                session.reduce(|ctx| CompoundNode::new(self.BOp, ctx));
            },

            _ => (),
        }
    }
}

//======================================
// UnderDotParselet
//======================================

impl PrefixParselet for UnderDotParselet {
    fn parse_prefix<'i>(
        &'static self,
        session: &mut ParserSession<'i>,
        tok_in: TokenRef<'i>,
    ) {
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
