use std::{cell::RefCell, collections::HashSet, rc::Rc};

#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    byte_decoder::ByteDecoder_nextSourceCharacter,
    feature,
    node::{
        CollectedExpressionsNode, CollectedIssuesNode, MissingBecauseUnsafeCharacterEncodingNode,
        Node, NodeContainer, NodeSeq, SafeStringNode, TriviaSeq,
    },
    parselet::{prefix_parselet, PrefixToplevelCloserParselet_parsePrefix},
    parser::{Context, Parser_handleFirstLine, Parser_isQuiescent, Parser_popNode},
    source::{Issue, IssuePtrSet, SourceConvention, TOPLEVEL},
    token_enum_registration::TokenEnum::TOKEN_ENDOFFILE,
    tokenizer::{
        Tokenizer, Tokenizer_currentToken, Tokenizer_nextToken,
        Tokenizer_nextToken_stringifyAsFile, Tokenizer_nextToken_stringifyAsTag,
        TrackedSourceLocations, UnsafeCharacterEncoding,
    },
    EncodingMode, FirstLineBehavior, StringifyMode,
};

/// A parser session
#[derive(Debug)]
pub struct ParserSession<'i> {
    pub(crate) tokenizer: Tokenizer<'i>,

    pub(crate) NodeStack: Vec<Node>,
    pub(crate) ContextStack: Vec<Context>,

    pub(crate) trivia1: Rc<RefCell<TriviaSeq>>,
    pub(crate) trivia2: Rc<RefCell<TriviaSeq>>,
}

pub(crate) struct ParseResult {
    /// Tokens or expressions.
    nodes: NodeSeq,

    unsafe_character_encoding: Option<UnsafeCharacterEncoding>,

    fatal_issues: Vec<Issue>,
    non_fatal_issues: Vec<Issue>,

    tracked: TrackedSourceLocations,
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

    pub fn parseExpressions(&mut self) -> NodeContainer {
        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("enter parseExpressions");
            DiagnosticsMarkTime();
        }

        //
        // Collect all expressions
        //

        let mut exprs = NodeSeq::new();

        loop {
            if feature::CHECK_ABORT && self.abortQ() {
                break;
            }

            let peek = Tokenizer_currentToken(&mut self.tokenizer, TOPLEVEL);

            if peek.tok == TOKEN_ENDOFFILE {
                break;
            }

            if peek.tok.isTrivia() {
                exprs.push(peek);

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

        return self.create_parse_result(exprs).into_node_container();
    }

    pub fn tokenize(&mut self) -> NodeContainer {
        let mut nodes = NodeSeq::new();

        loop {
            if feature::CHECK_ABORT && self.abortQ() {
                break;
            }

            let Tok = Tokenizer_currentToken(&mut self.tokenizer, TOPLEVEL);

            if Tok.tok == TOKEN_ENDOFFILE {
                break;
            }

            nodes.push(Tok);

            Tok.skip(&mut self.tokenizer);
        } // while (true)

        if let Some(flag) = self.tokenizer.unsafe_character_encoding_flag {
            nodes.clear();

            let N = MissingBecauseUnsafeCharacterEncodingNode::new(flag);

            nodes.push(N);
        }

        return NodeContainer::new(nodes);
    }

    fn concreteParseLeaf0(&mut self, mode: StringifyMode) -> Node {
        let token = match mode {
            StringifyMode::Normal => Tokenizer_nextToken(&mut self.tokenizer, TOPLEVEL),
            StringifyMode::Tag => Tokenizer_nextToken_stringifyAsTag(&mut self.tokenizer),
            StringifyMode::File => Tokenizer_nextToken_stringifyAsFile(&mut self.tokenizer),
        };

        Node::Token(token)
    }

    pub fn concreteParseLeaf(&mut self, mode: StringifyMode) -> NodeContainer {
        //
        // Collect all expressions
        //

        let mut exprs = NodeSeq::new();

        exprs.push(self.concreteParseLeaf0(mode));

        return self.create_parse_result(exprs).into_node_container();
    }

    pub fn safeString(&mut self) -> NodeContainer {
        //
        // read all characters, just to set unsafeCharacterEncoding flag if necessary
        //
        loop {
            let Char = ByteDecoder_nextSourceCharacter(&mut self.tokenizer, TOPLEVEL);

            if Char.isEndOfFile() {
                break;
            }
        } // while (true)

        let node = match self.tokenizer.unsafe_character_encoding_flag {
            None => {
                // let N = SafeStringNode::new(BufferAndLength::new(self.start, self.end - self.start));
                Node::from(SafeStringNode::new(
                    std::str::from_utf8(self.input())
                        .expect("safeString: unable to convert source input into safe string")
                        .to_owned(),
                ))
            },
            Some(flag) => {
                debug_assert!(
                    std::str::from_utf8(self.input()).is_err()
                        || flag == UnsafeCharacterEncoding::BOM
                );

                Node::from(MissingBecauseUnsafeCharacterEncodingNode::new(flag))
            },
        };

        let mut nodes = NodeSeq::new();
        nodes.push(node);

        return NodeContainer::new(nodes);
    }

    fn create_parse_result(&self, nodes: NodeSeq) -> ParseResult {
        let result = ParseResult {
            nodes,
            unsafe_character_encoding: self.tokenizer.unsafe_character_encoding_flag,
            fatal_issues: self.fatalIssues().clone(),
            non_fatal_issues: self.nonFatalIssues().clone(),
            tracked: self.tokenizer.tracked.clone(),
        };

        result
    }

    // TODO(cleanup): This doesn't need to be a method on ParserSession.
    pub(crate) fn abortQ(&self) -> bool {
        crate::abortQ()
    }

    pub(crate) fn fatalIssues(&self) -> &IssuePtrSet {
        &self.tokenizer.fatalIssues
    }

    pub(crate) fn nonFatalIssues(&self) -> &IssuePtrSet {
        &self.tokenizer.nonFatalIssues
    }
}

impl ParseResult {
    fn into_node_container(self) -> NodeContainer {
        let ParseResult {
            nodes: outer_exprs,
            unsafe_character_encoding,
            fatal_issues,
            non_fatal_issues,
            tracked,
        } = self;

        let mut nodes = NodeSeq::new();
        nodes.push(CollectedExpressionsNode::new(outer_exprs));

        if let Some(flag) = unsafe_character_encoding {
            nodes.clear();

            let mut exprs = NodeSeq::new();

            let node = MissingBecauseUnsafeCharacterEncodingNode::new(flag);

            exprs.push(node);

            let Collected = CollectedExpressionsNode::new(exprs);

            nodes.push(Collected);
        }

        //
        // Now handle the out-of-band expressions, i.e., issues and metadata
        //

        //
        // if there are fatal issues, then only send fatal issues
        //
        if !fatal_issues.is_empty() {
            nodes.push(CollectedIssuesNode(fatal_issues));
        } else {
            nodes.push(CollectedIssuesNode(non_fatal_issues));
        }

        for node in tracked.to_nodes() {
            nodes.push(node);
        }

        NodeContainer::new(nodes)
    }
}
