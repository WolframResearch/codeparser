use std::{cell::RefCell, collections::HashSet, rc::Rc};

#[cfg(feature = "USE_MATHLINK")]
use wolfram_library_link::sys::WolframLibraryData;

#[cfg(feature = "DIAGNOSTICS")]
use Diagnostics::*;

use crate::{
    byte_decoder::ByteDecoder_nextSourceCharacter,
    feature,
    node::{
        CollectedExpressionsNode, CollectedIssuesNode, CollectedSourceLocationsNode,
        MissingBecauseUnsafeCharacterEncodingNode, Node, NodeContainer, NodeSeq, SafeStringNode,
        TriviaSeq,
    },
    parselet::{prefix_parselet, PrefixToplevelCloserParselet_parsePrefix},
    parser::{Context, Parser_handleFirstLine, Parser_isQuiescent, Parser_popNode},
    source::{IssuePtrSet, SourceConvention, TOPLEVEL},
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

                unsafeCharacterEncodingFlag: UnsafeCharacterEncoding::Ok,
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

    // /// Return a slice of the remaining input to be parsed.
    // pub(crate) fn buffer(&self) -> &'i [u8] {
    //     if self.offset >= self.input.len() {
    //         panic!("offset ({}) is greater than length of input ({})", self.offset, self.input.len())
    //     }

    //     let (_, rest) = self.input.split_at(self.offset);
    //     rest
    // }

    // /// Return a slice of the remaining input of length `len` as a string.
    // ///
    // /// # Panics
    // ///
    // /// This function will panic if `len` is larger than the remaining input,
    // /// or if the requested slice of inputs is not valid UTF-8.
    // pub(crate) fn buffer_str(&self, len: usize) -> &str {
    //     let data = &self.buffer()[..len];
    //     std::str::from_utf8(data).expect("buffer_str(): unable to parse current input as UTF-8")
    // }

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

        let collected = Node::from(CollectedExpressionsNode::new(exprs));

        let mut nodes = NodeSeq::new();
        nodes.push(collected);

        if self.tokenizer.unsafeCharacterEncodingFlag != UnsafeCharacterEncoding::Ok {
            nodes.clear();

            let mut exprs = NodeSeq::new();

            exprs.push(Node::from(MissingBecauseUnsafeCharacterEncodingNode::new(
                self.tokenizer.unsafeCharacterEncodingFlag,
            )));

            let Collected = CollectedExpressionsNode::new(exprs);

            nodes.push(Collected);
        }

        //
        // Now handle the out-of-band expressions, i.e., issues and metadata
        //

        //
        // if there are fatal issues, then only send fatal issues
        //
        if !self.fatalIssues().is_empty() {
            nodes.push(CollectedIssuesNode(self.fatalIssues().clone()));
        } else {
            nodes.push(CollectedIssuesNode(self.nonFatalIssues().clone()));
        }

        for node in self.tokenizer.tracked.to_nodes() {
            nodes.push(node);
        }

        let C = NodeContainer::new(nodes);

        #[cfg(feature = "DIAGNOSTICS")]
        {
            DiagnosticsLog("exit parseExpressions");
            DiagnosticsLogTime();
        }

        return C;
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

        if self.tokenizer.unsafeCharacterEncodingFlag != UnsafeCharacterEncoding::Ok {
            nodes.clear();

            let N = MissingBecauseUnsafeCharacterEncodingNode::new(
                self.tokenizer.unsafeCharacterEncodingFlag,
            );

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

        let Collected = CollectedExpressionsNode::new(exprs);

        let mut nodes = NodeSeq::new();
        nodes.push(Collected);

        if self.tokenizer.unsafeCharacterEncodingFlag != UnsafeCharacterEncoding::Ok {
            nodes.clear();

            let mut exprs = NodeSeq::new();

            let node = MissingBecauseUnsafeCharacterEncodingNode::new(
                self.tokenizer.unsafeCharacterEncodingFlag,
            );

            exprs.push(node);

            let Collected = CollectedExpressionsNode::new(exprs);

            nodes.push(Collected);
        }

        //
        // Collect all issues from the various components
        //

        //
        // if there are fatal issues, then only send fatal issues
        //
        if !self.fatalIssues().is_empty() {
            nodes.push(CollectedIssuesNode(self.fatalIssues().clone()));
        } else {
            nodes.push(CollectedIssuesNode(self.nonFatalIssues().clone()));
        }

        for node in self.tokenizer.tracked.to_nodes() {
            nodes.push(node);
        }

        return NodeContainer::new(nodes);
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

        let node = match self.tokenizer.unsafeCharacterEncodingFlag {
            UnsafeCharacterEncoding::Ok => {
                // let N = SafeStringNode::new(BufferAndLength::new(self.start, self.end - self.start));
                Node::from(SafeStringNode::new(
                    std::str::from_utf8(self.input())
                        .expect("safeString: unable to convert source input into safe string")
                        .to_owned(),
                ))
            },
            UnsafeCharacterEncoding::IncompleteUTF8Sequence
            | UnsafeCharacterEncoding::StraySurrogate
            | UnsafeCharacterEncoding::BOM => {
                debug_assert!(
                    std::str::from_utf8(self.input()).is_err()
                        || self.tokenizer.unsafeCharacterEncodingFlag
                            == UnsafeCharacterEncoding::BOM
                );

                Node::from(MissingBecauseUnsafeCharacterEncodingNode::new(
                    self.tokenizer.unsafeCharacterEncodingFlag,
                ))
            },
        };

        let mut nodes = NodeSeq::new();
        nodes.push(node);

        return NodeContainer::new(nodes);
    }

    // fn releaseNodeContainer(C: NodeContainerPtr) {
    //     C.release();

    //     delete(C);
    // }

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
