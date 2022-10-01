use std::{cell::RefCell, collections::HashSet, rc::Rc};

#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    byte_decoder::ByteDecoder_nextSourceCharacter,
    feature,
    node::{Node, NodeSeq, TriviaSeq},
    parselet::{prefix_parselet, PrefixToplevelCloserParselet_parsePrefix},
    parser::{Context, Parser_handleFirstLine, Parser_isQuiescent, Parser_popNode},
    source::{Issue, IssuePtrSet, SourceConvention, TOPLEVEL},
    token::{BorrowedTokenInput, TokenKind, TokenRef},
    tokenizer::{
        Tokenizer, Tokenizer_currentToken, Tokenizer_nextToken,
        Tokenizer_nextToken_stringifyAsFile, Tokenizer_nextToken_stringifyAsTag,
        TrackedSourceLocations, UnsafeCharacterEncoding,
    },
    EncodingMode, FirstLineBehavior, StringifyMode, Tokens,
};

/// A parser session
#[derive(Debug)]
pub struct ParserSession<'i> {
    pub(crate) tokenizer: Tokenizer<'i>,

    pub(crate) NodeStack: NodeStack<'i>,
    pub(crate) ContextStack: Vec<Context>,

    pub(crate) trivia1: Rc<RefCell<TriviaSeq<'i>>>,
    pub(crate) trivia2: Rc<RefCell<TriviaSeq<'i>>>,
}

pub(crate) type NodeStack<'i> = Vec<Node<BorrowedTokenInput<'i>>>;

pub struct ParseResult<I> {
    /// Tokens or expressions.
    pub(crate) nodes: NodeSeq<I>,

    pub(crate) unsafe_character_encoding: Option<UnsafeCharacterEncoding>,

    pub(crate) fatal_issues: Vec<Issue>,
    pub(crate) non_fatal_issues: Vec<Issue>,

    pub(crate) tracked: TrackedSourceLocations,
}

//======================================
// Impls
//======================================

impl<'i> ParserSession<'i> {
    pub fn new(
        input: &[u8],
        srcConvention: SourceConvention,
        tabWidth: u32,
        firstLineBehavior: FirstLineBehavior,
        encodingMode: EncodingMode,
    ) -> ParserSession {
        let mut session = ParserSession {
            tokenizer: Tokenizer {
                input,
                offset: 0,
                wasEOF: false,

                tabWidth,
                firstLineBehavior,

                encodingMode,

                srcConvention,
                SrcLoc: srcConvention.newSourceLocation(),

                GroupStack: Vec::new(),

                tracked: TrackedSourceLocations {
                    simple_line_continuations: HashSet::new(),
                    complex_line_continuations: HashSet::new(),
                    embedded_newlines: HashSet::new(),
                    embedded_tabs: HashSet::new(),
                },

                fatalIssues: Vec::new(),
                nonFatalIssues: Vec::new(),

                unsafe_character_encoding_flag: None,
            },

            NodeStack: Vec::new(),
            ContextStack: Vec::new(),

            trivia1: Rc::new(RefCell::new(TriviaSeq::new())),
            trivia2: Rc::new(RefCell::new(TriviaSeq::new())),
        };

        Parser_handleFirstLine(&mut session.tokenizer);

        return session;
    }

    /// Returns the complete input [`Buffer`].
    pub fn input(&self) -> &'i [u8] {
        self.tokenizer.input
    }

    pub fn concrete_parse_expressions(&mut self) -> ParseResult<BorrowedTokenInput<'i>> {
        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("enter parseExpressions");
            DiagnosticsMarkTime();
        }

        //
        // Collect all expressions
        //

        let mut exprs: NodeSeq<BorrowedTokenInput<'i>> = NodeSeq::new();

        loop {
            if feature::CHECK_ABORT && crate::abortQ() {
                break;
            }

            let peek: TokenRef = Tokenizer_currentToken(&mut self.tokenizer, TOPLEVEL);

            if peek.tok == TokenKind::EndOfFile {
                break;
            }

            if peek.tok.isTrivia() {
                exprs.push(Node::Token(peek));

                peek.skip(&mut self.tokenizer);

                continue;
            }

            //
            // special top-level handling of stray closers
            //
            if peek.tok.isCloser() {
                PrefixToplevelCloserParselet_parsePrefix(self, peek);

                exprs.push(Parser_popNode(self));

                assert!(Parser_isQuiescent(self));

                continue;
            }

            let P = prefix_parselet(peek.tok);

            P.parse_prefix(self, peek);

            exprs.push(Parser_popNode(self));

            assert!(Parser_isQuiescent(self));
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

            let Tok = Tokenizer_currentToken(&mut self.tokenizer, TOPLEVEL);

            if Tok.tok == TokenKind::EndOfFile {
                break;
            }

            tokens.push(Tok);

            Tok.skip(&mut self.tokenizer);
        } // while (true)

        if let Some(flag) = self.tokenizer.unsafe_character_encoding_flag {
            return Err(flag);
        }

        let tokens = self.reparse_unterminated_tokens(Tokens(tokens));

        return Ok(tokens);
    }

    fn concreteParseLeaf0(&mut self, mode: StringifyMode) -> Node<BorrowedTokenInput<'i>> {
        let token = match mode {
            StringifyMode::Normal => Tokenizer_nextToken(&mut self.tokenizer, TOPLEVEL),
            StringifyMode::Tag => Tokenizer_nextToken_stringifyAsTag(&mut self.tokenizer),
            StringifyMode::File => Tokenizer_nextToken_stringifyAsFile(&mut self.tokenizer),
        };

        Node::Token(token)
    }

    #[allow(dead_code)]
    pub(crate) fn concreteParseLeaf(
        &mut self,
        mode: StringifyMode,
    ) -> ParseResult<BorrowedTokenInput<'i>> {
        //
        // Collect all expressions
        //

        let mut exprs = NodeSeq::new();

        exprs.push(self.concreteParseLeaf0(mode));

        let exprs = self.reparse_unterminated(exprs);

        return self.create_parse_result(exprs);
    }

    // TODO(cleanup): What is this used for? Perhaps ultimately this is just
    //                std::str::from_utf8()?
    #[allow(dead_code)]
    pub(crate) fn safe_string(&mut self) -> Result<&str, UnsafeCharacterEncoding> {
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
        mut nodes: NodeSeq<BorrowedTokenInput<'i>>,
    ) -> NodeSeq<BorrowedTokenInput<'i>> {
        if let Ok(input) = std::str::from_utf8(self.tokenizer.input) {
            nodes = crate::error::reparse_unterminated(
                nodes,
                input,
                self.tokenizer.srcConvention,
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
                self.tokenizer.srcConvention,
                usize::try_from(self.tokenizer.tabWidth).unwrap(),
            );
        }

        tokens
    }

    fn create_parse_result(
        &self,
        nodes: NodeSeq<BorrowedTokenInput<'i>>,
    ) -> ParseResult<BorrowedTokenInput<'i>> {
        let result = ParseResult {
            nodes,
            unsafe_character_encoding: self.tokenizer.unsafe_character_encoding_flag,
            fatal_issues: self.fatalIssues().clone(),
            non_fatal_issues: self.nonFatalIssues().clone(),
            tracked: self.tokenizer.tracked.clone(),
        };

        result
    }

    pub(crate) fn fatalIssues(&self) -> &IssuePtrSet {
        &self.tokenizer.fatalIssues
    }

    pub(crate) fn nonFatalIssues(&self) -> &IssuePtrSet {
        &self.tokenizer.nonFatalIssues
    }
}

impl<I> ParseResult<I> {
    pub fn nodes(&self) -> &[Node<I>] {
        let NodeSeq(vec) = &self.nodes;
        vec.as_slice()
    }
}
