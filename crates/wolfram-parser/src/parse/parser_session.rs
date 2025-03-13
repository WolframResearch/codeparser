#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    create_parse_result,
    cst::{Cst, CstSeq, TriviaSeq},
    feature,
    issue::Issue,
    parse::{
        parselet::{PrefixParselet, PrefixToplevelCloserParselet},
        Context,
    },
    quirks::{self, QuirkSettings},
    tokenize::{
        tokenizer::{
            Tokenizer, TrackedSourceLocations, UnsafeCharacterEncoding,
        },
        TokenKind, TokenRef, TokenStr,
    },
    NodeSeq, ParseOptions,
};


/// A parser session
#[derive(Debug)]
pub(crate) struct ParserSession<'i> {
    pub(crate) tokenizer: Tokenizer<'i>,

    pub(super) NodeStack: NodeStack<'i>,
    pub(super) ContextStack: Vec<Context>,

    pub(crate) quirk_settings: QuirkSettings,
}

pub(crate) type NodeStack<'i> = Vec<Cst<TokenStr<'i>>>;

//
// Used mainly for collecting trivia that has been eaten
//
pub(crate) type TriviaSeqRef<'i> = TriviaSeq<TokenStr<'i>>;

pub struct ParseResult<T> {
    /// Tokens, concrete syntax, or abstract syntax.
    pub syntax: T,

    #[doc(hidden)]
    pub unsafe_character_encoding: Option<UnsafeCharacterEncoding>,

    #[doc(hidden)]
    pub fatal_issues: Vec<Issue>,
    #[doc(hidden)]
    pub non_fatal_issues: Vec<Issue>,

    #[doc(hidden)]
    pub tracked: TrackedSourceLocations,
}

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
            quirk_settings,
        } = *opts;

        ParserSession {
            tokenizer: Tokenizer::new(input, opts),

            NodeStack: Vec::new(),
            ContextStack: Vec::new(),

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

                exprs.push(self.pop_node());

                assert!(self.is_quiescent());

                continue;
            }

            self.parse_prefix(peek);

            exprs.push(self.pop_node());

            assert!(self.is_quiescent());
        } // while (true)

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("exit parseExpressions");
            DiagnosticsLogTime();
        }

        let exprs = self.reparse_unterminated(exprs);

        return create_parse_result(&self.tokenizer, exprs);
    }

    fn reparse_unterminated(
        &self,
        mut nodes: CstSeq<TokenStr<'i>>,
    ) -> CstSeq<TokenStr<'i>> {
        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            nodes = crate::error::reparse_unterminated(
                nodes,
                input,
                usize::try_from(self.tokenizer.tab_width).unwrap(),
            );
        }

        nodes
    }

    #[cfg(test)]
    pub(crate) fn fatalIssues(&self) -> &Vec<Issue> {
        &self.tokenizer.fatalIssues
    }

    #[cfg(test)]
    pub(crate) fn nonFatalIssues(&self) -> &Vec<Issue> {
        &self.tokenizer.nonFatalIssues
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

//======================================
// ParseResult
//======================================

impl<N> ParseResult<NodeSeq<N>> {
    pub fn nodes(&self) -> &[N] {
        let NodeSeq(vec) = &self.syntax;
        vec.as_slice()
    }

    pub fn node_seq(&self) -> &NodeSeq<N> {
        &self.syntax
    }
}
