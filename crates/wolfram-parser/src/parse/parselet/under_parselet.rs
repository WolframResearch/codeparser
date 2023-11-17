use crate::{
    panic_if_aborted,
    parse::{
        operators::CompoundOperator, parselet::*, ParserSession, UnderParseData,
    },
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

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for UnderParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // prefix
        //
        // Something like  _  or  _a
        //

        let node = self.get_parse_under_context_sensitive(session, tok_in);

        let node = session.builder.push_compound_blank(node);

        // MUSTTAIL
        return session.parse_climb(node);
    }
}

impl UnderParselet {
    pub(in crate::parse) fn get_parse_infix_context_sensitive<
        'i,
        B: ParseBuilder<'i> + 'i,
    >(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> UnderParseData<'i> {
        //
        // infix
        //
        // Something like  a_b
        //

        self.get_parse_under_context_sensitive(session, tok_in)
    }

    fn get_parse_under_context_sensitive<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> UnderParseData<'i> {
        panic_if_aborted!();

        tok_in.skip(&mut session.tokenizer);

        let tok = session.tokenizer.peek_token();

        match tok.tok {
            TokenKind::Symbol => {
                //
                // Something like
                //     prefix:  _b
                //      infix:  a_b
                //

                // Context-sensitive infix parse of Symbol token
                //
                // Something like  _b
                //                  ^
                // We know we are already in the middle of parsing _
                //
                // Just push this symbol
                //
                tok.skip(&mut session.tokenizer);

                UnderParseData::UnderSymbol {
                    op: self.BOp,
                    under: tok_in,
                    symbol: tok,
                }
            },

            TokenKind::Error_ExpectedLetterlike => {
                //
                // Something like:
                //     prefix:  _a`   (TID:231016/1)
                //      infix:  a_b`  (TID:231016/2)
                //
                // It's nice to include the error inside of the blank
                //

                tok.skip(&mut session.tokenizer);

                UnderParseData::UnderSymbol {
                    op: self.BOp,
                    under: tok_in,
                    symbol: tok,
                }
            },

            _ => UnderParseData::Under(tok_in),
        }
    }
}

//======================================
// UnderDotParselet
//======================================

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for UnderDotParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // prefix
        //
        // Something like  _.
        //

        panic_if_aborted!();


        let node = session.push_leaf_and_next(tok_in);

        // MUSTTAIL
        return session.parse_climb(node);
    }
}
