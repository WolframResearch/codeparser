use crate::{
    panic_if_aborted,
    parse::{parselet::*, ParserSession},
    precedence::Precedence,
    tokenize::{Token, TokenKind, TokenRef},
};

//
// SemiSemiParselet is complicated enough to warrant its own implementation file.
// The syntax for ;; is complicated and has a lot of edge cases.
//

impl<'i, B: ParseBuilder<'i> + 'i> InfixParselet<'i, B> for SemiSemiParselet {
    fn parse_infix(
        &self,
        session: &mut ParserSession<'i, B>,
        lhs_node: B::Node,
        trivia1: TriviaSeqRef<'i>,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        session.skip(tok_in);

        // MUSTTAIL
        return SemiSemiParselet::parse1(session, lhs_node, trivia1, tok_in);
    }

    fn getPrecedence(&self, _: &ParserSession<'i, B>) -> Option<Precedence> {
        return Some(Precedence::SEMISEMI);
    }

    fn process_implicit_times(
        &self,
        session: &mut ParserSession<'i, B>,
        prev_node: &B::Node,
        tok_in: TokenRef<'i>,
    ) -> TokenRef<'i> {
        //
        // SemiSemi was already parsed with look-ahead with the assumption that implicit Times will be handled correctly
        //

        if session.builder.top_node_is_span(prev_node) {
            return Token::at_start(TokenKind::Fake_ImplicitTimes, tok_in);
        }

        return tok_in;
    }
}

impl<'i, B: ParseBuilder<'i> + 'i> PrefixParselet<'i, B> for SemiSemiParselet {
    fn parse_prefix(
        &self,
        session: &mut ParserSession<'i, B>,
        tok_in: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        debug_assert_eq!(tok_in.tok, TokenKind::SemiSemi);

        let first_operand = session
            .push_leaf(Token::at_start(TokenKind::Fake_ImplicitOne, tok_in));

        session.push_context(Precedence::SEMISEMI);

        //
        // nextToken() is not needed after an implicit token
        //

        // MUSTTAIL
        return self.parse_infix(
            session,
            first_operand,
            TriviaSeqRef::new(),
            tok_in,
        );
    }
}

impl SemiSemiParselet {
    fn parse1<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        first_operand: B::Node,
        trivia1: TriviaSeqRef<'i>,
        first_op_token: TokenRef<'i>,
    ) -> B::Node {
        panic_if_aborted!();

        //
        // Span should not cross toplevel newlines
        //
        let (trivia2, SecondTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        //
        // a;;
        //  ^~tok_in
        //

        if !SecondTok.tok.isPossibleBeginning() {
            //
            // a;;&
            //    ^SecondTok
            //

            let second_operand = session.push_leaf(Token::at_start(
                TokenKind::Fake_ImplicitAll,
                SecondTok,
            ));

            //
            // nextToken() is not needed after an implicit token
            //

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(
                session,
                first_operand,
                trivia1,
                first_op_token,
                trivia2,
                second_operand,
            );
        }

        if SecondTok.tok != TokenKind::SemiSemi {
            //
            // a;;b
            //    ^SecondTok
            //

            // MUSTTAIL
            let second_operand = session.parse_prefix(SecondTok);

            return SemiSemiParselet::parse2(
                session,
                first_operand,
                trivia1,
                first_op_token,
                trivia2,
                second_operand,
            );
        }

        //
        // a;;;;
        //    ^~SecondTok
        //

        let second_operand = session
            .push_leaf(Token::at_start(TokenKind::Fake_ImplicitAll, SecondTok));

        SecondTok.skip(&mut session.tokenizer);

        // trivia3 must be empty because with an implicit All 2nd operand
        // whatever would have been in the third trivia position if there was
        // a non-implicit operand actually ends up in trivia2:
        //     a ;; <All> ;; b
        //      1  2     3  4
        //      ------------- trivias
        let trivia3 = TriviaSeqRef::new();

        //
        // Span should not cross toplevel newlines
        //
        let (trivia4, ThirdTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        if !ThirdTok.tok.isPossibleBeginning()
            || ThirdTok.tok == TokenKind::SemiSemi
        {
            //
            // a;;;;&
            //      ^ThirdTok
            //

            //
            // a;;;;;;
            //      ^~ThirdTok
            //

            trivia4.reset(&mut session.tokenizer);
            SecondTok.reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(
                session,
                first_operand,
                trivia1,
                first_op_token,
                trivia2,
                second_operand,
            );
        }

        //
        // a;;;;b
        //      ^ThirdTok
        //

        //
        // nextToken() already handled above
        //

        // MUSTTAIL
        let third_operand = session.parse_prefix(ThirdTok);

        SemiSemiParselet::reduce_ternary(
            session,
            first_operand,
            trivia1,
            first_op_token,
            trivia2,
            second_operand,
            trivia3,
            SecondTok,
            trivia4,
            third_operand,
        )
    }

    fn parse2<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        first_operand: B::Node,
        trivia1: TriviaSeqRef<'i>,
        first_op_token: TokenRef<'i>,
        trivia2: TriviaSeqRef<'i>,
        second_operand: B::Node,
    ) -> B::Node {
        panic_if_aborted!();


        //
        // Span should not cross toplevel newlines
        //
        let (trivia3, ThirdTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        // TODO(cleanup): Can this condition be simplified? It would seem to
        //                imply that ThirdTok is always SemiSemi if it's false,
        //                so why check isPossibleBeginning()?
        if !ThirdTok.tok.isPossibleBeginning()
            || ThirdTok.tok != TokenKind::SemiSemi
        {
            //
            // a;;b&
            //     ^ThirdTok
            //

            //
            // \[Integral];;x\[DifferentialD]x
            //               ^~~~~~~~~~~~~~~~ThirdTok
            //

            trivia3.reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(
                session,
                first_operand,
                trivia1,
                first_op_token,
                trivia2,
                second_operand,
            );
        }

        debug_assert_eq!(ThirdTok.tok, TokenKind::SemiSemi);

        //
        // a;;b;;
        //     ^~ThirdTok
        //

        ThirdTok.skip(&mut session.tokenizer);

        //
        // Span should not cross toplevel newlines
        //
        let (trivia4, FourthTok) =
            session.current_token_eat_trivia_but_not_toplevel_newlines_into();

        if !FourthTok.tok.isPossibleBeginning()
            || FourthTok.tok == TokenKind::SemiSemi
        {
            //
            // a;;b;;&
            //       ^FourthTok
            //

            //
            // a;;b;;;;
            //       ^~FourthTok
            //

            trivia4.reset(&mut session.tokenizer);
            ThirdTok.reset(&mut session.tokenizer);
            trivia3.reset(&mut session.tokenizer);

            // MUSTTAIL
            return SemiSemiParselet::reduce_binary(
                session,
                first_operand,
                trivia1,
                first_op_token,
                trivia2,
                second_operand,
            );
        }

        //
        // a;;b;;c
        //       ^FourthTok
        //

        //
        // nextToken() already handled above
        //

        let third_node = session.parse_prefix(FourthTok);

        return SemiSemiParselet::reduce_ternary(
            session,
            first_operand,
            trivia1,
            first_op_token,
            trivia2,
            second_operand,
            trivia3,
            ThirdTok,
            trivia4,
            third_node,
        );
    }

    //

    fn reduce_binary<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        lhs_node: B::Node,
        trivia1: TriviaSeqRef<'i>,
        op_token: TokenRef<'i>,
        trivia2: TriviaSeqRef<'i>,
        rhs_node: B::Node,
    ) -> B::Node {
        let node = session.reduce_binary(
            BinaryOperator::Span,
            lhs_node,
            trivia1,
            op_token,
            trivia2,
            rhs_node,
        );

        return session.parse_climb(node);
    }

    fn reduce_ternary<'i, B: ParseBuilder<'i> + 'i>(
        session: &mut ParserSession<'i, B>,
        lhs_node: B::Node,
        trivia1: TriviaSeqRef<'i>,
        first_op_token: TokenRef<'i>,
        trivia2: TriviaSeqRef<'i>,
        middle_node: B::Node,
        trivia3: TriviaSeqRef<'i>,
        second_op_token: TokenRef<'i>,
        trivia4: TriviaSeqRef<'i>,
        rhs_node: B::Node,
    ) -> B::Node {
        let node = session.reduce_ternary(
            TernaryOperator::Span,
            lhs_node,
            trivia1,
            first_op_token,
            trivia2,
            middle_node,
            trivia3,
            second_op_token,
            trivia4,
            rhs_node,
        );

        return session.parse_climb(node);
    }
}
