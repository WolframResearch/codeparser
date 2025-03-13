use std::{cell::RefCell, collections::HashSet, rc::Rc};

#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    abstract_::{Abstract, Aggregate},
    ast::AstNode,
    cst::{CstNode, CstNodeSeq},
    feature,
    issue::Issue,
    parselet::{PrefixParselet, PrefixToplevelCloserParselet},
    parser::{Context, Parser_handleFirstLine},
    quirks::{self, QuirkSettings},
    read::{ByteDecoder_nextSourceCharacter, Reader},
    source::{SourceConvention, TOPLEVEL},
    token::{BorrowedTokenInput, Token, TokenKind, TokenRef},
    tokenizer::{
        Tokenizer, Tokenizer_nextToken_stringifyAsFile, Tokenizer_nextToken_stringifyAsTag,
        TrackedSourceLocations, UnsafeCharacterEncoding,
    },
    EncodingMode, FirstLineBehavior, NodeSeq, ParseOptions, StringifyMode, Tokens,
};

/// A parser session
#[derive(Debug)]
pub(crate) struct ParserSession<'i> {
    pub(crate) tokenizer: Tokenizer<'i>,

    pub(super) NodeStack: NodeStack<'i>,
    pub(super) ContextStack: Vec<Context>,

    pub(crate) trivia1: Rc<RefCell<TriviaSeq<'i>>>,
    pub(crate) trivia2: Rc<RefCell<TriviaSeq<'i>>>,

    pub(crate) quirk_settings: QuirkSettings,
}

pub(crate) type NodeStack<'i> = Vec<CstNode<BorrowedTokenInput<'i>>>;

//
// Used mainly for collecting trivia that has been eaten
//
#[derive(Debug)]
pub(crate) struct TriviaSeq<'i> {
    pub vec: Vec<Token<BorrowedTokenInput<'i>>>,
}

pub struct ParseResult<N> {
    /// Tokens or expressions.
    #[doc(hidden)]
    pub nodes: NodeSeq<N>,

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

        ParserSession::new_(
            input,
            src_convention,
            tab_width,
            first_line_behavior,
            encoding_mode,
            quirk_settings,
        )
    }

    fn new_(
        input: &[u8],
        srcConvention: SourceConvention,
        tabWidth: u32,
        firstLineBehavior: FirstLineBehavior,
        encodingMode: EncodingMode,
        quirk_settings: QuirkSettings,
    ) -> ParserSession {
        let mut session = ParserSession {
            tokenizer: Tokenizer {
                reader: Reader {
                    input,
                    offset: 0,
                    wasEOF: false,
                    SrcLoc: srcConvention.newSourceLocation(),
                    tabWidth,

                    encodingMode,

                    fatalIssues: Vec::new(),
                    nonFatalIssues: Vec::new(),

                    unsafe_character_encoding_flag: None,
                },

                firstLineBehavior,

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

            trivia1: Rc::new(RefCell::new(TriviaSeq::new())),
            trivia2: Rc::new(RefCell::new(TriviaSeq::new())),

            quirk_settings,
        };

        Parser_handleFirstLine(&mut session.tokenizer);

        return session;
    }

    /// Returns the complete input [`Buffer`][crate::source::Buffer].
    pub fn input(&self) -> &'i [u8] {
        self.tokenizer.input
    }

    pub fn abstract_parse_expressions(&mut self) -> ParseResult<AstNode> {
        quirks::set_quirks(self.quirk_settings);

        let ParseResult {
            nodes,
            unsafe_character_encoding,
            fatal_issues,
            non_fatal_issues,
            tracked,
        } = self.concrete_parse_expressions();

        let nodes = Aggregate(nodes);
        let nodes = Abstract(nodes);

        ParseResult {
            nodes: NodeSeq(nodes),
            unsafe_character_encoding,
            fatal_issues,
            non_fatal_issues,
            tracked,
        }
    }

    pub fn concrete_parse_expressions(&mut self) -> ParseResult<CstNode<BorrowedTokenInput<'i>>> {
        quirks::set_quirks(self.quirk_settings);

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("enter parseExpressions");
            DiagnosticsMarkTime();
        }

        //
        // Collect all expressions
        //

        let mut exprs: CstNodeSeq<BorrowedTokenInput<'i>> = NodeSeq::new();

        loop {
            if feature::CHECK_ABORT && crate::abortQ() {
                break;
            }

            let peek: TokenRef = self.tokenizer.peek_token();

            if peek.tok == TokenKind::EndOfFile {
                break;
            }

            if peek.tok.isTrivia() {
                exprs.push(CstNode::Token(peek));

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

    pub fn tokenize(&mut self) -> Result<Tokens<BorrowedTokenInput<'i>>, UnsafeCharacterEncoding> {
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

    fn concreteParseLeaf0(&mut self, mode: StringifyMode) -> Token<BorrowedTokenInput<'i>> {
        let token = match mode {
            StringifyMode::Normal => self.tokenizer.next_token(),
            StringifyMode::Tag => Tokenizer_nextToken_stringifyAsTag(&mut self.tokenizer),
            StringifyMode::File => Tokenizer_nextToken_stringifyAsFile(&mut self.tokenizer),
        };

        token
    }

    pub(crate) fn concreteParseLeaf(
        &mut self,
        mode: StringifyMode,
    ) -> ParseResult<Token<BorrowedTokenInput<'i>>> {
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
            let Char = ByteDecoder_nextSourceCharacter(&mut self.tokenizer, TOPLEVEL);

            if Char.isEndOfFile() {
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
        mut nodes: CstNodeSeq<BorrowedTokenInput<'i>>,
    ) -> CstNodeSeq<BorrowedTokenInput<'i>> {
        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            nodes = crate::error::reparse_unterminated(
                nodes,
                input,
                usize::try_from(self.tokenizer.tabWidth).unwrap(),
            );
        }

        nodes
    }

    fn reparse_unterminated_tokens(
        &self,
        mut tokens: Tokens<BorrowedTokenInput<'i>>,
    ) -> Tokens<BorrowedTokenInput<'i>> {
        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            tokens = crate::error::reparse_unterminated_tokens(
                tokens,
                input,
                usize::try_from(self.tokenizer.tabWidth).unwrap(),
            );
        }

        tokens
    }

    fn create_parse_result<N>(&self, nodes: NodeSeq<N>) -> ParseResult<N> {
        let result = ParseResult {
            nodes,
            unsafe_character_encoding: self.tokenizer.unsafe_character_encoding_flag,
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

impl<'i> TriviaSeq<'i> {
    pub(crate) fn new() -> Self {
        TriviaSeq { vec: Vec::new() }
    }

    pub fn reset(&mut self, session: &mut Tokenizer) {
        let TriviaSeq { vec } = self;

        //
        // Just need to reset the global buffer to the buffer of the first token in the sequence
        //

        if vec.is_empty() {
            return;
        }

        let T = &vec[0];

        T.reset(session);

        vec.clear();
    }

    pub fn push(&mut self, token: TokenRef<'i>) {
        self.vec.push(token);
    }

    pub fn is_empty(&self) -> bool {
        return self.vec.is_empty();
    }

    pub fn clear(&mut self) {
        let TriviaSeq { vec } = self;

        vec.clear();
    }
}

//======================================
// ParseResult
//======================================

impl<N> ParseResult<N> {
    pub fn nodes(&self) -> &[N] {
        let NodeSeq(vec) = &self.nodes;
        vec.as_slice()
    }

    pub fn node_seq(&self) -> &NodeSeq<N> {
        &self.nodes
    }
}
