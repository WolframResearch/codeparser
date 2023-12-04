use crate::{
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{Token, TokenKind, TokenRef},
};

impl IntegralParselet {
    pub(crate) const fn new(
        Op1: PrefixBinaryOperator,
        Op2: PrefixOperator,
    ) -> Self {
        IntegralParselet { Op1, Op2 }
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for IntegralParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        //
        // Something like "\[Integral] f \[DifferentialD] x" (TID:231113/1)
        //

        panic_if_aborted!();

        let tok_in = session.push_syntax_and_next(tok_in);

        let _ = session.push_context(Precedence::CLASS_INTEGRATIONOPERATORS);

        let (trivia1, Tok) = session.current_token_eat_trivia();

        if Tok.tok == TokenKind::LongName_DifferentialD
            || Tok.tok == TokenKind::LongName_CapitalDifferentialD
        {
            //
            // TID:231113/2: "\[Integral] \[DifferentialD] x"
            //

            let node = session
                .push_leaf(Token::at_start(TokenKind::Fake_ImplicitOne, Tok));

            return IntegralParselet::parse1(
                self, session, tok_in, trivia1, node,
            );
        }

        let lhs_expr = session.parse_prefix(Tok);

        // MUSTTAIL
        return IntegralParselet::parse1(
            self, session, tok_in, trivia1, lhs_expr,
        );
    }
}

impl IntegralParselet {
    fn parse1<'i, B: ParseBuilder<'i> + 'i>(
        &self,
        session: &mut ParserSession<'i, B>,
        prefix_op_token: B::SyntaxTokenNode,
        trivia1: B::TriviaHandle,
        first_operand: B::Node,
    ) -> B::Node {
        panic_if_aborted!();


        let (trivia2, tok) = session.current_token();

        if !(tok.tok == TokenKind::LongName_DifferentialD
            || tok.tok == TokenKind::LongName_CapitalDifferentialD)
        {
            session.trivia_reset(trivia2);

            //
            // TID:231113/3: "\[Integral] f"
            //

            let node = session.reduce_prefix(
                self.Op2,
                prefix_op_token,
                trivia1,
                first_operand,
            );

            // MUSTTAIL
            return session.parse_climb(node);
        }

        let trivia2 = session.builder.push_trivia_seq(trivia2);

        // TODO(cleanup):
        // `tok` here is a known prefix operator.
        // Statically check somehow that `second_operand` is a prefix
        // parselet, because we know it is LongName_{Capital}DifferentialD

        // MUSTTAIL
        let second_operand = session.parse_prefix(tok);


        // \[Integral] f \[DifferentialD] x

        let node = session.reduce_prefix_binary(
            self.Op1,
            prefix_op_token,
            trivia1,
            first_operand,
            trivia2,
            second_operand,
        );

        return session.parse_climb(node);
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B>
    for InfixDifferentialDParselet
{
    fn parse_infix(
        &self,
        _session: &mut ParserSession<'i, B>,
        _node: B::Node,
        _trivia1: B::TriviaHandle,
        _token: TokenRef,
    ) -> B::Node {
        panic!("illegal call to InfixDifferentialDParselet::parse_infix()")
    }

    fn getPrecedence(
        &self,
        session: &ParserSession<'i, B>,
    ) -> Option<Precedence> {
        if session.top_precedence() == Precedence::CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return None;
        }

        return Some(Precedence::FAKE_IMPLICITTIMES);
    }

    fn process_implicit_times(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        if session.top_precedence() == Precedence::CLASS_INTEGRATIONOPERATORS {
            //
            // Inside \[Integral], so \[DifferentialD] is treated specially
            //

            return tok_in;
        }

        return Token::at_start(TokenKind::Fake_ImplicitTimes, tok_in);
    }
}
