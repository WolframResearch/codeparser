use std::collections::HashSet;

#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    cst::{Cst, CstSeq, TriviaSeq},
    feature,
    issue::Issue,
    parse::{
        parselet::{PrefixParselet, PrefixToplevelCloserParselet},
        Context, Parser_handleFirstLine,
    },
    quirks::{self, QuirkSettings},
    read::Reader,
    source::TOPLEVEL,
    tokenize::{
        tokenizer::{
            Tokenizer, Tokenizer_nextToken_stringifyAsFile,
            Tokenizer_nextToken_stringifyAsTag, TrackedSourceLocations,
            UnsafeCharacterEncoding,
        },
        Token, TokenKind, TokenRef, TokenStr,
    },
    NodeSeq, ParseOptions, StringifyMode, Tokens,
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
            first_line_behavior,
            src_convention,
            encoding_mode,
            tab_width,
            quirk_settings,
        } = *opts;

        let mut session = ParserSession {
            tokenizer: Tokenizer {
                reader: Reader {
                    input,
                    offset: 0,
                    wasEOF: false,
                    SrcLoc: src_convention.newSourceLocation(),
                    tab_width,

                    encoding_mode,

                    fatalIssues: Vec::new(),
                    nonFatalIssues: Vec::new(),

                    unsafe_character_encoding_flag: None,
                },

                first_line_behavior,

                GroupStack: Vec::new(),

                tracked: TrackedSourceLocations {
                    simple_line_continuations: HashSet::new(),
                    complex_line_continuations: HashSet::new(),
                    embedded_newlines: HashSet::new(),
                    embedded_tabs: HashSet::new(),
                },
            },

            NodeStack: Vec::new(),
            ContextStack: Vec::new(),

            quirk_settings,
        };

        Parser_handleFirstLine(&mut session.tokenizer);

        return session;
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

        return self.create_parse_result(exprs);
    }

    pub fn tokenize(
        &mut self,
    ) -> Result<Tokens<TokenStr<'i>>, UnsafeCharacterEncoding> {
        let mut tokens = Vec::new();

        loop {
            if feature::CHECK_ABORT && crate::abortQ() {
                break;
            }

            let tok = self.tokenizer.peek_token();

            if tok.tok == TokenKind::EndOfFile {
                break;
            }

            tokens.push(tok);

            tok.skip(&mut self.tokenizer);
        } // while (true)

        if let Some(flag) = self.tokenizer.unsafe_character_encoding_flag {
            return Err(flag);
        }

        let tokens = self.reparse_unterminated_tokens(Tokens(tokens));

        return Ok(tokens);
    }

    fn concreteParseLeaf0(
        &mut self,
        mode: StringifyMode,
    ) -> Token<TokenStr<'i>> {
        let token = match mode {
            StringifyMode::Normal => self.tokenizer.next_token(),
            StringifyMode::Tag => {
                Tokenizer_nextToken_stringifyAsTag(&mut self.tokenizer)
            },
            StringifyMode::File => {
                Tokenizer_nextToken_stringifyAsFile(&mut self.tokenizer)
            },
        };

        token
    }

    pub(crate) fn concreteParseLeaf(
        &mut self,
        mode: StringifyMode,
    ) -> ParseResult<NodeSeq<Token<TokenStr<'i>>>> {
        //
        // Collect all expressions
        //

        let mut exprs: NodeSeq<Token<_>> = NodeSeq::new();

        exprs.push(self.concreteParseLeaf0(mode));

        return self.create_parse_result(exprs);
    }

    // TODO(cleanup): What is this used for? Perhaps ultimately this is just
    //                std::str::from_utf8()?
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn safe_string(&mut self) -> Result<&'i str, UnsafeCharacterEncoding> {
        //
        // read all characters, just to set unsafeCharacterEncoding flag if necessary
        //
        loop {
            let char = self.tokenizer.next_source_char(TOPLEVEL);

            if char.isEndOfFile() {
                break;
            }
        } // while (true)

        match self.tokenizer.unsafe_character_encoding_flag {
            None => {
                // let N = SafeStringNode::new(BufferAndLength::new(self.start, self.end - self.start));
                let str = std::str::from_utf8(self.input())
                    .expect("safeString: unable to convert source input into safe string");

                Ok(str)
            },
            Some(flag) => {
                debug_assert!(
                    std::str::from_utf8(self.input()).is_err()
                        || flag == UnsafeCharacterEncoding::BOM
                );

                Err(flag)
            },
        }
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

    fn reparse_unterminated_tokens(
        &self,
        mut tokens: Tokens<TokenStr<'i>>,
    ) -> Tokens<TokenStr<'i>> {
        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            tokens = crate::error::reparse_unterminated_tokens(
                tokens,
                input,
                usize::try_from(self.tokenizer.tab_width).unwrap(),
            );
        }

        tokens
    }

    fn create_parse_result<N>(
        &self,
        nodes: NodeSeq<N>,
    ) -> ParseResult<NodeSeq<N>> {
        let result = ParseResult {
            syntax: nodes,
            unsafe_character_encoding: self
                .tokenizer
                .unsafe_character_encoding_flag,
            fatal_issues: self.fatalIssues().clone(),
            non_fatal_issues: self.nonFatalIssues().clone(),
            tracked: self.tokenizer.tracked.clone(),
        };

        result
    }

    pub(crate) fn fatalIssues(&self) -> &Vec<Issue> {
        &self.tokenizer.fatalIssues
    }

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
