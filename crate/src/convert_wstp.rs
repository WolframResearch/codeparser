use std::collections::HashSet;

use wolfram_library_link::wstp;

use crate::{
    my_string::MyString,
    my_string_registration::*,
    node::{
        BinaryNode, CallNode, CompoundNode, GroupMissingCloserNode, GroupNode, InfixNode, Node,
        NodeSeq, OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode, SyntaxErrorNode,
        TernaryNode, UnterminatedGroupNeedsReparseNode,
    },
    source::{
        BufferAndLength, CodeAction, CodeActionKind, Issue, IssueTag, Severity, Source,
        SourceConvention, SourceLocation,
    },
    symbol::Symbol,
    symbol_registration::*,
    token::{BorrowedTokenInput, Token},
    token_enum_registration::TokenToSymbol,
    ParseResult, UnsafeCharacterEncoding,
};

//======================================
// Token
//======================================

impl<'i> Token<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Token { tok, src, input } = self;

        if tok.isError() {
            if tok.isUnterminated() {
                callLink
                    .put_function(
                        SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.as_str(),
                        3,
                    )
                    .unwrap();
            } else {
                callLink
                    .put_function(SYMBOL_CODEPARSER_ERRORNODE.as_str(), 3)
                    .unwrap();
            }
        } else {
            //
            // These are Symbols, Strings, Integers, Reals, Rationals.
            //

            callLink
                .put_function(SYMBOL_CODEPARSER_LEAFNODE.as_str(), 3)
                .unwrap();
        }

        let sym = TokenToSymbol(*tok);

        Symbol_put(sym, callLink);

        // bufLen().put(callLink);
        // let source: &[u8] = &session.tokenizer.input[span.offset..span.offset + span.len];
        let source: &[u8] = &input.buf.as_bytes();

        let source = std::str::from_utf8(source).expect("token source span is not valid UTF-8");
        callLink.put_str(source).unwrap();

        callLink
            .put_function(SYMBOL_ASSOCIATION.as_str(), 1)
            .unwrap();

        src.put(callLink);
    }
}

//======================================
// Node types
//======================================

impl<'i> Node<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        match self {
            Node::Token(token) => token.put(link),
            Node::Call(node) => node.put(link),
            Node::SyntaxError(node) => node.put(link),
            Node::Infix(InfixNode(op)) => op.put(link, SYMBOL_CODEPARSER_INFIXNODE),
            Node::Prefix(PrefixNode(op)) => op.put(link, SYMBOL_CODEPARSER_PREFIXNODE),
            Node::Postfix(PostfixNode(op)) => op.put(link, SYMBOL_CODEPARSER_POSTFIXNODE),
            Node::Binary(BinaryNode(op)) => op.put(link, SYMBOL_CODEPARSER_BINARYNODE),
            Node::Ternary(TernaryNode(op)) => op.put(link, SYMBOL_CODEPARSER_TERNARYNODE),
            Node::PrefixBinary(PrefixBinaryNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_PREFIXBINARYNODE)
            },
            Node::Compound(CompoundNode(op)) => op.put(link, SYMBOL_CODEPARSER_COMPOUNDNODE),
            Node::Group(GroupNode(op)) => op.put(link, SYMBOL_CODEPARSER_GROUPNODE),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE)
            },
            Node::UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE)
            },
        }
    }
}

impl<'i> NodeSeq<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let NodeSeq(vec) = self;

        callLink
            .put_function(SYMBOL_LIST.as_str(), vec.len())
            .unwrap();

        for C in vec {
            if crate::feature::CHECK_ABORT && crate::abortQ() {
                Symbol_put(SYMBOL__ABORTED, callLink);
                continue;
            }

            C.put(callLink)
        }
    }
}

impl<'i> OperatorNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link, op_head: Symbol) {
        let OperatorNode { op, children, src } = self;

        callLink.put_function(op_head.as_str(), 3).unwrap();

        Symbol_put(op.to_symbol(), callLink);

        children.put(callLink);

        callLink
            .put_function(SYMBOL_ASSOCIATION.as_str(), 1)
            .unwrap();

        src.put(callLink);
    }
}

impl<'i> CallNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CallNode { head, body, src } = self;
        callLink
            .put_function(SYMBOL_CODEPARSER_CALLNODE.as_str(), 3)
            .unwrap();

        head.put(callLink);

        body.put(callLink);

        callLink
            .put_function(SYMBOL_ASSOCIATION.as_str(), 1)
            .unwrap();

        src.put(callLink);
    }
}

impl<'i> SyntaxErrorNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SyntaxErrorNode { err, children, src } = self;

        callLink
            .put_function(SYMBOL_CODEPARSER_SYNTAXERRORNODE.as_str(), 3)
            .unwrap();

        Symbol_put(*err, callLink);

        children.put(callLink);

        callLink
            .put_function(SYMBOL_ASSOCIATION.as_str(), 1)
            .unwrap();

        src.put(callLink);
    }
}

impl UnsafeCharacterEncoding {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        callLink.put_function(SYMBOL_MISSING.as_str(), 1).unwrap();

        let reason = self.reason();

        reason.put(callLink);
    }
}

//======================================
// Source types
//======================================

impl<'i> BufferAndLength<'i> {
    #[allow(dead_code)]
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        callLink.put_str(self.as_str()).unwrap();
    }
}

impl Issue {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Issue {
            make_sym,
            tag,
            msg,
            sev,
            src,
            val,
            actions,
            additional_descriptions,
        } = self;

        callLink.put_function(make_sym.as_str(), 4).unwrap();

        tag.put(callLink);

        callLink.put_str(msg).unwrap();

        sev.put(callLink);

        {
            callLink
                .put_function(
                    SYMBOL_ASSOCIATION.as_str(),
                    2 + (if actions.is_empty() { 0 } else { 1 })
                        + (if additional_descriptions.is_empty() {
                            0
                        } else {
                            1
                        }),
                )
                .unwrap();

            src.put(callLink);

            {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                Symbol_put(SYMBOL_CONFIDENCELEVEL, callLink);

                callLink.put_f64(**val).unwrap();
            }

            if !actions.is_empty() {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_CODEACTIONS, callLink);

                callLink
                    .put_function(SYMBOL_LIST.as_str(), actions.len())
                    .unwrap();

                for A in actions {
                    A.put(callLink);
                }
            }

            if !additional_descriptions.is_empty() {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                STRING_ADDITIONALDESCRIPTIONS.put(callLink);

                callLink
                    .put_function(SYMBOL_LIST.as_str(), additional_descriptions.len())
                    .unwrap();

                for D in additional_descriptions {
                    callLink.put_str(D).unwrap();
                }
            }
        }
    }
}

impl CodeAction {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CodeAction {
            label: Label,
            src: Src,
            kind,
        } = self;

        match kind {
            CodeActionKind::ReplaceText { replacement_text } => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_REPLACETEXT, callLink);

                {
                    callLink
                        .put_function(SYMBOL_ASSOCIATION.as_str(), 2)
                        .unwrap();

                    Src.put(callLink);

                    callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                    STRING_REPLACEMENTTEXT.put(callLink);

                    callLink.put_str(replacement_text).unwrap();
                }
            },
            CodeActionKind::InsertText { insertion_text } => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_INSERTTEXT, callLink);

                {
                    callLink
                        .put_function(SYMBOL_ASSOCIATION.as_str(), 2)
                        .unwrap();

                    Src.put(callLink);

                    callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                    STRING_INSERTIONTEXT.put(callLink);

                    callLink.put_str(insertion_text).unwrap();
                }
            },
            CodeActionKind::DeleteText => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_DELETETEXT, callLink);

                {
                    callLink
                        .put_function(SYMBOL_ASSOCIATION.as_str(), 1)
                        .unwrap();

                    Src.put(callLink);
                }
            },
        }
    }
}

impl SourceLocation {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SourceLocation { first, second } = *self;

        callLink.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

        callLink.put_i64(first.into()).unwrap();

        callLink.put_i64(second.into()).unwrap();
    }
}

impl Source {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Source { start, end } = self;

        callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

        Symbol_put(SYMBOL_CODEPARSER_SOURCE, callLink);

        debug_assert_eq!(start.convention(), end.convention());

        match start.convention() {
            SourceConvention::LineColumn => {
                callLink.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

                start.put(callLink);
                end.put(callLink);
            },
            SourceConvention::CharacterIndex => {
                callLink.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

                callLink.put_i64(start.second.into()).unwrap();

                callLink.put_i64((end.second - 1).into()).unwrap();

                return;
            },
        }
    }
}

fn put_source_locations(link: &mut wstp::Link, source_locs: HashSet<SourceLocation>) {
    link.put_function(SYMBOL_LIST.as_str(), source_locs.len())
        .unwrap();

    for loc in source_locs {
        loc.put(link);
    }
}

//======================================
// Result types
//======================================

impl<'i> ParseResult<BorrowedTokenInput<'i>> {
    pub(crate) fn put(self, link: &mut wstp::Link) {
        let ParseResult {
            nodes: outer_exprs,
            unsafe_character_encoding,
            fatal_issues,
            non_fatal_issues,
            tracked,
        } = self;

        link.put_function(SYMBOL_LIST.as_str(), 6).unwrap();

        // 1.
        // Collected expressions.
        match unsafe_character_encoding {
            None => outer_exprs.put(link),
            Some(flag) => {
                link.put_function(SYMBOL_LIST.as_str(), 1).unwrap();
                flag.put(link);
            },
        };

        //
        // Now handle the out-of-band expressions, i.e., issues and metadata
        //

        // 2.
        {
            //
            // if there are fatal issues, then only send fatal issues
            //
            let issues = if !fatal_issues.is_empty() {
                fatal_issues
            } else {
                non_fatal_issues
            };

            link.put_function(SYMBOL_LIST.as_str(), issues.len())
                .unwrap();

            for issue in issues {
                issue.put(link);
            }
        }

        // 3, 4, 5, 6
        for source_locs in tracked.to_nodes() {
            put_source_locations(link, source_locs);
        }
    }
}

//======================================
// Other
//======================================

impl MyString {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let MyString(val) = self;

        link.put_str(val).unwrap()
    }
}

// Note: This function can't be a method on Symbol because Symbol (currently) is
//       a type alias to a type from wolfram_expr, and its not legal in Rust
//       to implement methods on types that aren't part of the current crate.
pub(crate) fn Symbol_put(self_: Symbol, callLink: &mut wstp::Link) {
    callLink.put_symbol(self_.as_str()).unwrap();
}

impl Severity {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let string: &'static str = self.into();

        link.put_str(string).unwrap();
    }
}

impl IssueTag {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let string: &'static str = self.into();

        link.put_str(string).unwrap();
    }
}
