#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    create_parse_result,
    cst::{Cst, CstSeq, TriviaSeq},
    feature,
    parse::{
        parselet::{PrefixParselet, PrefixToplevelCloserParselet},
        ParseBuilder,
    },
    parse_cst::ParseCst,
    quirks::{self, QuirkSettings},
    tokenize::{tokenizer::Tokenizer, TokenKind, TokenRef, TokenStr},
    NodeSeq, ParseOptions, ParseResult,
};


/// A parser session
#[derive(Debug)]
pub(crate) struct ParserSession<'i, B: ParseBuilder<'i> = ParseCst<'i>> {
    pub(crate) tokenizer: Tokenizer<'i>,

    #[cfg(not(test))]
    pub(super) builder: B,

    #[cfg(test)]
    pub(crate) builder: B,

    pub(crate) quirk_settings: QuirkSettings,
}

//
// Used mainly for collecting trivia that has been eaten
//
pub(crate) type TriviaSeqRef<'i> = TriviaSeq<TokenStr<'i>>;

//======================================
// Impls
//======================================

impl<'i> ParserSession<'i> {
    pub fn new(input: &'i [u8], opts: &ParseOptions) -> ParserSession<'i> {
        let ParseOptions {
            first_line_behavior: _,
            src_convention: _,
            encoding_mode: _,
            tab_width: _,
            check_issues: _,
            compute_oob: _,
            quirk_settings,
        } = *opts;

        ParserSession {
            tokenizer: Tokenizer::new(input, opts),
            builder: ParseCst::new(),
            quirk_settings,
        }
    }

    /// Returns the complete input [`Buffer`][crate::source::Buffer].
    pub fn input(&self) -> &'i [u8] {
        self.tokenizer.input
    }

    pub fn concrete_parse_expressions(
        &mut self,
    ) -> ParseResult<CstSeq<TokenStr<'i>>> {
        quirks::set_quirks(self.quirk_settings);

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("enter parseExpressions");
            DiagnosticsMarkTime();
        }

        //
        // Collect all expressions
        //

        let mut exprs: CstSeq<TokenStr<'i>> = NodeSeq::new();

        loop {
            if feature::CHECK_ABORT && crate::abortQ() {
                break;
            }

            let peek: TokenRef = self.tokenizer.peek_token();

            if peek.tok == TokenKind::EndOfFile {
                break;
            }

            if peek.tok.isTrivia() {
                exprs.push(Cst::Token(peek));

                peek.skip(&mut self.tokenizer);

                continue;
            }

            //
            // special top-level handling of stray closers
            //
            if peek.tok.isCloser() {
                (PrefixToplevelCloserParselet {}).parse_prefix(self, peek);

                exprs.push(self.builder.pop_finished_expr());

                assert!(self.is_quiescent());

                continue;
            }

            self.parse_prefix(peek);

            exprs.push(self.builder.pop_finished_expr());

            assert!(self.is_quiescent());
        } // while (true)

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("exit parseExpressions");
            DiagnosticsLogTime();
        }

        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            exprs = crate::error::reparse_unterminated(
                exprs,
                input,
                usize::try_from(self.tokenizer.tab_width).unwrap(),
            );
        }

        return create_parse_result(&self.tokenizer, exprs);
    }

    #[cfg(test)]
    pub(crate) fn fatal_issues(&self) -> &Vec<crate::issue::Issue> {
        &self.tokenizer.fatal_issues
    }

    #[cfg(test)]
    pub(crate) fn non_fatal_issues(&self) -> &Vec<crate::issue::Issue> {
        &self.tokenizer.non_fatal_issues
    }
}

//======================================
// TriviaSeq
//======================================

impl<'i> TriviaSeq<TokenStr<'i>> {
    pub(crate) fn new() -> Self {
        TriviaSeq(Vec::new())
    }

    pub(crate) fn reset(self, session: &mut Tokenizer) {
        let TriviaSeq(vec) = self;

        //
        // Just need to reset the global buffer to the buffer of the first token in the sequence
        //

        if vec.is_empty() {
            return;
        }

        let T = &vec[0];

        T.reset(session);
    }

    pub(crate) fn push(&mut self, token: TokenRef<'i>) {
        let TriviaSeq(vec) = self;
        vec.push(token);
    }
}
